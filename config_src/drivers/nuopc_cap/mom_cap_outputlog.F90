!> This module contains a set of subroutines that check if MOM restart and history files
!! have been written and closed. This file is specific to UWM operational requirements
!! and configurations (eg specific output frequencys in hours) and may break if used outside
!! the scope of intended use.
!! This module a stub when CESMCOUPLED is defined
module MOM_cap_outputlog

#ifdef CESMCOUPLED
  use ESMF                  , only : ESMF_GridComp, ESMF_Clock, ESMF_SUCCESS
  implicit none; private

  public :: outputlog_init, outputlog_run, outputlog_restart
contains
  subroutine outputlog_init(gcomp, mclock, rc)
    type(ESMF_GridComp)  :: gcomp  !< an ESMF_GridComp object
    type(ESMF_Clock)     :: mclock !< the ESMF_clock for the model
    integer, intent(out) :: rc     !< return code
    rc = ESMF_SUCCESS
  end subroutine outputlog_init
  subroutine outputlog_run(mclock, atStopTime, rc)
    type(ESMF_Clock)              :: mclock     !< the ESMF_clock for the model
    logical, intent(in), optional :: atStopTime !< if true, checks for final output file
    integer, intent(out)          :: rc         !< return code
    rc = ESMF_SUCCESS
  end subroutine outputlog_run
  subroutine outputlog_restart(mclock, num_rest_files, rc)
    type(ESMF_Clock)     :: mclock         !< the ESMF_clock for the model
    integer, intent(in)  :: num_rest_files !< the number of restart files
    integer, intent(out) :: rc             !< return code
    rc = ESMF_SUCCESS
  end subroutine outputlog_restart
#else
  use MOM_error_handler     , only : is_root_pe, MOM_error, FATAL
  use NUOPC                 , only : NUOPC_CompAttributeGet
  use ESMF                  , only : ESMF_GridComp, ESMF_GridCompGet, ESMF_VM, ESMF_VMGet
  use ESMF                  , only : ESMF_Time, ESMF_Clock, ESMF_ClockGet, ESMF_Alarm, ESMF_AlarmSet
  use ESMF                  , only : ESMF_ClockGetAlarm, ESMF_AlarmIsRinging, ESMF_AlarmRingerOff
  use ESMF                  , only : ESMF_ClockGetNextTime, ESMF_TimeGet, ESMF_TimeInterval
  use ESMF                  , only : ESMF_AlarmGet, ESMF_TimeIntervalSet, ESMF_TimeIntervalPrint
  use ESMF                  , only : ESMF_SUCCESS, ESMF_LogWrite, ESMF_LOGMSG_INFO, ESMF_VMBroadCast
  use ESMF                  , only : ESMF_LogSetError, ESMF_LogFoundError, ESMF_LOGERR_PASSTHRU
  use ESMF                  , only : operator(*), operator(+), operator(-), operator(>), operator(==)
  use MOM_cap_methods       , only : ChkErr
  use MOM_cap_time          , only : AlarmInit
  use shr_is_restart_fh_mod , only : log_restart_fh
  use netcdf

  implicit none; private

  public :: outputlog_init, outputlog_run, outputlog_restart

  ! the allowable output frequency for MOM6 history, in hours only
  integer, parameter :: n_freq  = 3
  integer, parameter, dimension(n_freq) :: freq = (/3, 6, 24/)
  ! TODO: check multiple output freq for same run, reqs different  known filename
  ! root for different freqs

  ! the tincrement interval (defined in minutes) is used to construct the output filename
  ! the file name must be set as the mid-point of the averaging period via the diagtable
  ! and the output filename timestrings are given by
  !      T - (interval * 60 * increment + interval/2 * 60 * increment )
  ! where T is the time when the file is closed
  !
  !   00   .   03   .   06   .   09
  !       1:30 = 6 - (3 + 1:30)
  !                4:30 = 9 - (3 + 1:30)
  !
  !   00   .   06   .   12   .   18
  !       03 = 12 - (6 + 3)
  !                 09 = 18 - (6 + 3)
  !
  !   00   .   24   .   48   .   72
  !       12 = 48 - (24 + 12)
  !                 36 = 72 - (24 + 12)
  !
  ! when the model reaches the stop time, any 'pending' output file is closed, and the final
  ! interval output is also closed
  !
  !                   stop
  !  18   .   24   .   30
  !      21 = 30 - (12 + 3)
  !                03 = 30 - (3)
  !
  ! since both the final interval and the next-to-final interval are closed at the stop time,
  ! a different log file name is required for  the final log file, otherwise the next-to-final
  ! log is overwritten
  !
  ! an output file is declared closed when the unlimited dimension in the file is > 0
  ! each closed output file is recorded in a logfile named with the associated forecast hour.

  type(ESMF_VM)           :: vm
  type(ESMF_TimeInterval) :: tincrement
  type(ESMF_Time)         :: lastrestart

  type :: outputlog_type
    character(len=128)      :: alarm_name
    integer                 :: opt_n
    logical                 :: chkfile_nextAdvance
    character(len=1024)     :: filename
    type(ESMF_Alarm)        :: alarm
    type(ESMF_TimeInterval) :: fhoffset
    type(ESMF_TimeInterval) :: filename_fhoffset
    type(ESMF_Time)         :: time_lastrestart
  end type outputlog_type

  type(outputlog_type) :: olog(n_freq)

  integer            :: toffset
  logical            :: debug
  logical            :: existflag
  character(len=256) :: restartdir
  character(len=256) :: outputdir
  character(len=2)   :: output_fh
  character(len=3)   :: chour
  character(len=512) :: msgString
  character(len=*), parameter :: u_FILE_u = &
       __FILE__

contains
  !> Initialize a set of Alarms at the allowed output frequencies
  !!
  !! @param gcomp   an ESMF_GridComp object
  !! @param clock   an ESMF_Clock object
  !! @param rc      return code
  subroutine outputlog_init(gcomp, mclock, rc)

    type(ESMF_GridComp)  :: gcomp  !< ESMF_GridComp object
    type(ESMF_Clock)     :: mclock !< ESMF_clock for the model
    integer, intent(out) :: rc     !< return code

    ! local variables
    type(ESMF_Time)         :: mcurrTime
    type(ESMF_TimeInterval) :: alarmoffset
    logical                 :: isPresent, isSet
    integer                 :: n
    integer                 :: year, month, day, hour
    character(len=256)      :: value
    character(len=256)      :: subname='MOM_cap:(outputlog_init)'
    !----------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompAttributeGet(gcomp, name="mom6_restart_dir", value=value, &
         isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) then
      restartdir = trim(value)
    else
      restartdir = './'
    end if
    if (restartdir(len_trim(restartdir):len_trim(restartdir)) /= '/') then
      restartdir = trim(restartdir)//'/'
    end if
    write(msgString,'(A)')'MOM_cap:MOM6 restart directory = '//trim(restartdir)
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)

    call NUOPC_CompAttributeGet(gcomp, name="mom6_output_dir", value=value, &
         isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) then
      outputdir = trim(value)
    else
      outputdir = './'
    end if
    if (outputdir(len_trim(outputdir):len_trim(outputdir)) /= '/') then
      outputdir = trim(outputdir)//'/'
    end if
    write(msgString,'(A)')'MOM_cap:MOM6 output directory = '//trim(outputdir)
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)

    call NUOPC_CompAttributeGet(gcomp, name="mom6_output_fh", value=value, &
         isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) then
      if (len_trim(value) == 1) then
        output_fh = '0'//trim(value)
      else
        output_fh = trim(value)
      end if
    else
      output_fh = '06'
    end if
    write(msgString,'(A)')'MOM_cap:MOM6 output frequency = '//trim(output_fh)
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)

    debug = .false.
    call NUOPC_CompAttributeGet(gcomp, name="debug_outputlog", value=value, &
         isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) debug=(trim(value)=="true")
    if (debug) call ESMF_LogWrite('MOM_cap:MOM6 output debug ON', ESMF_LOGMSG_INFO)

    call ESMF_ClockGet(mclock, currTime=mcurrTime, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TimeIntervalSet(tincrement, m=1, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! get start hour time offset (ie, fhrot)
    call ESMF_TimeGet(mcurrTime, yy=year, mm=month, dd=day, h=hour, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (mod(hour,6) .ne. 0) then
      toffset = hour - 6
    else
      toffset = 0
    end if
    if (debug .and. is_root_pe()) then
      print '(A,i8)',trim(subname)//' toffset = ',toffset
    end if
    ! initialize
    lastrestart = mcurrTime

    do n = 1,n_freq
      write(chour,'(I2.2,A)')freq(n),'h'
      olog(n)%alarm_name          = 'output_alarm'//trim(chour)
      olog(n)%opt_n               = freq(n)
      olog(n)%chkfile_nextAdvance = .false.
      olog(n)%filename            = ''
      olog(n)%time_lastrestart    = lastrestart
      olog(n)%fhoffset            = 60*freq(n)*tincrement
      olog(n)%filename_fhoffset   = 90*freq(n)*tincrement

      ! the time offset in hours required to ensure the alarm rings at multiples of 6
      if (freq(n) >= 6) then
        alarmoffset = toffset*60*tincrement
      else
        alarmoffset = 0*tincrement
      end if

      call AlarmInit(mclock,                  &
           alarm     = olog(n)%alarm,         &
           option    = 'nhours',              &
           opt_n     = olog(n)%opt_n,         &
           opt_ymd   = -999,                  &
           RefTime   = mcurrTime+alarmoffset, &
           alarmname = olog(n)%alarm_name, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return

      call ESMF_AlarmSet(olog(n)%alarm, clock=mclock, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      write(msgString,'(A)')trim(subname)//' Output alarm '//trim(olog(n)%alarm_name)//' Created & Set'
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
      if (debug .and. is_root_pe()) then
        call ESMF_TimeIntervalPrint(olog(n)%filename_fhoffset, options="string", rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
      end if
    end do
  end subroutine outputlog_init

  !> Use Alarms at the output frequency to determine if output has been completed
  !!
  !! @param clock        an ESMF_Clock object
  !! @param atStopTime   when present, checks for final output file
  !! @param rc           return code
  subroutine outputlog_run(mclock, atStopTime, rc)

    type(ESMF_Clock)              :: mclock     !< the ESMF_clock for the model
    logical, intent(in), optional :: atStopTime !< if true, checks for final output file
    integer, intent(out)          :: rc         !< return code

    ! local variables
    type(ESMF_Time)    :: nextTime, currTime, startTime, prevRing
    logical            :: lstop
    integer            :: n, nlen(1)
    integer            :: filesize
    character(len=40)  :: importexport
    character(len=16)  :: timestr
    character(len=512) :: fname
    character(len=256) :: subname='MOM_cap:(outputlog_run)'
    !----------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    call ESMF_ClockGet(mclock, startTime=startTime, currTime=currTime, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_ClockGetNextTime(mclock, nextTime, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    importexport = get_importexport(currTime, nextTime, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    lstop = .false.
    if (present(atStopTime)) then
      lstop = atStopTime
    end if

    do n = 1,n_freq
      write(chour,'(I2.2,A)')freq(n),'h'
      if (chour(1:2) == output_fh(1:2)) then
        call ESMF_ClockGetAlarm(mclock, alarmname=trim(olog(n)%alarm_name), alarm=olog(n)%alarm, rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        ! when the alarm rings, set file check on next advance and construct the filename
        if (ESMF_AlarmIsRinging(olog(n)%alarm, rc=rc)) then
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
          call ESMF_AlarmRingerOff(olog(n)%alarm, rc=rc )
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
          olog(n)%chkfile_nextAdvance = .true.

          timestr = get_timestr(nextTime-olog(n)%filename_fhoffset, rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
          write(olog(n)%filename,'(A)')trim(outputdir)//'ocn_'//trim(timestr)//'.nc'
          if (debug .and. is_root_pe()) then
             print '(A,L)',trim(subname)//' fname '//trim(olog(n)%filename)//'  '//trim(importexport) &
                  //' checkflag ',olog(n)%chkfile_nextAdvance
          end if
        end if

        if (olog(n)%chkfile_nextAdvance) then
          fname = trim(olog(n)%filename)
          inquire(file=fname, exist=existflag)
          if (existflag) then
            if (is_root_pe()) then
              nlen(1) = get_unlimited_len(trim(fname))
            end if
            call ESMF_VMBroadCast(vm, nlen, 1, 0, rc=rc)
            if (ChkErr(rc,__LINE__,u_FILE_u)) return

            if (nlen(1) > 0) then
              olog(n)%chkfile_nextAdvance = .false.
              olog(n)%time_lastrestart = lastrestart
              if (is_root_pe()) then
                if (toffset > 0) then
                  call log_restart_fh(nextTime-olog(n)%fhoffset, startTime, 'mom6.'//chour, prefixtime=.true., &
                       lastrestart=olog(n)%time_lastrestart, lastoutput=olog(n)%filename, rc=rc)
                  if (ChkErr(rc,__LINE__,u_FILE_u)) return
                else
                  call log_restart_fh(currTime-olog(n)%fhoffset, startTime, 'mom6.'//chour, prefixtime=.true., &
                       lastrestart=olog(n)%time_lastrestart, lastoutput=olog(n)%filename, rc=rc)
                  if (ChkErr(rc,__LINE__,u_FILE_u)) return
                end if
              endif
            end if
          end if ! existflag
        end if

        if (debug .and. is_root_pe()) call debug_info(trim(subname)//'  ',trim(fname), &
             olog(n)%chkfile_nextAdvance, importexport)

        if (lstop) then
          ! use prevRing in place of currTime to allow for stopping between averaging intervals
          ! prevring == currTime if stopping on intervals
          call ESMF_AlarmGet(olog(n)%alarm, prevRingTime=prevring, rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return

          timestr = get_timestr(prevring-30*freq(n)*tincrement, rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
          write(olog(n)%filename,'(A)')trim(outputdir)//'ocn_'//trim(timestr)//'.nc'
          if (debug .and. is_root_pe()) then
            print '(A)',trim(subname)//' fname at lstop '//trim(olog(n)%filename)//'  '//trim(importexport)
          end if

          fname = trim(olog(n)%filename)
          inquire(file=fname, exist=existflag)
          if (existflag) then
            if (is_root_pe()) then
              nlen(1) = get_unlimited_len(fname)
            end if
            call ESMF_VMBroadCast(vm, nlen, 1, 0, rc=rc)
            if (ChkErr(rc,__LINE__,u_FILE_u)) return

            if (nlen(1) > 0) then
              olog(n)%chkfile_nextAdvance = .false.
              olog(n)%time_lastrestart = lastrestart
              if (is_root_pe()) then
                call log_restart_fh(prevring, startTime, 'mom6.'//chour, prefixtime=.true., &
                     lastrestart=olog(n)%time_lastrestart, lastoutput=olog(n)%filename, rc=rc)
                if (ChkErr(rc,__LINE__,u_FILE_u)) return
              end if
            end if
          end if

          if (debug .and. is_root_pe()) call debug_info(trim(subname)//' lstop ',trim(fname), &
              olog(n)%chkfile_nextAdvance, importexport)
        end if ! lstop
      end if ! chour = output_fh
    end do
  end subroutine outputlog_run

  !> Check all restart files to determine if output has been completed
  !!
  !! @param clock          an ESMF_Clock object
  !! @param num_rest_files the number of restart files
  !! @param rc             return code
  subroutine outputlog_restart(mclock, num_rest_files, rc)
    type(ESMF_Clock)     :: mclock         !< the ESMF_clock for the model
    integer, intent(in)  :: num_rest_files !< the number of restart files
    integer, intent(out) :: rc             !< return code

    ! local variables
    type(ESMF_Time)      :: startTime, currTime, nextTime
    integer              :: n, nlen(1)
    integer              :: year, month, day, hour, minute, seconds
    character(len=512)   :: fname
    character(len=15)    :: timestr
    character(len=40)    :: importexport
    logical, allocatable :: allDone(:)
    character(len=8)     :: suffix
    character(len=256)   :: subname='MOM_cap:(outputlog_restart)'
    !----------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    call ESMF_ClockGet(mclock, startTime=startTime, currTime=currTime, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_ClockGetNextTime(mclock, nextTime, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    importexport = get_importexport(currTime, nextTime, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_TimeGet(nextTime, yy=year, mm=month, dd=day, h=hour, m=minute, s=seconds, rc=rc )
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    write(timestr,'(I4.4,2(I2.2),A,3(I2.2))') year, month, day,".", hour, minute, seconds

    allocate(allDone(1:num_rest_files))
    allDone = .false.

    do n = 1,num_rest_files
      if (n == 1) then
        suffix = ''
      else if (n-1 < 10) then
        write(suffix,'("_",I1)') n-1
      else
        write(suffix,'("_",I2)') n-1
      endif
      if (len_trim(suffix) == 0) then
        fname = trim(restartdir)//trim(timestr)//'.MOM.res.nc'
      else
        fname = trim(restartdir)//trim(timestr)//'.MOM.res'//trim(suffix)//'.nc'
      endif

      ! check if file is written
      inquire(file=trim(fname), exist=existflag)
      if (existflag) then
        if (is_root_pe())then
          nlen(1) = get_unlimited_len(trim(fname))
        end if
        call ESMF_VMBroadCast(vm, nlen, 1, 0, rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return

        if (nlen(1) > 0) allDone(n) = .true.
        if (debug .and. is_root_pe()) then
          if (nlen(1) > 0) then
            print '(A)',trim(subname)//' restart '//trim(fname)//'  '//trim(importexport)//' complete'
          else
            print '(A)',trim(subname)//' restart '//trim(fname)//'  '//trim(importexport)//' still 0'
          end if
        end if
      end if
    end do ! num_rest_files

    if (all(allDone) .eqv. .true.) then
      lastrestart = nextTime
      if (is_root_pe()) then
        call log_restart_fh(nextTime, startTime, 'mom6.res', prefixtime=.true., rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
      endif
    end if
  end subroutine outputlog_restart
  !> Return the length of the unlimited dimension
  !!
  !! @param[in]  fname   the file name
  !! @return             unlimited dimension length
  integer function get_unlimited_len(fname) result(unlen)

    character(len=*), intent(in) :: fname

    integer :: ncid, dimid
    !----------------------------------------------------------------------------

    unlen = 0
    call nf90_err(nf90_open(trim(fname), nf90_nowrite, ncid), 'nf90_open: '//trim(fname))
    call nf90_err(nf90_inquire(ncid, unlimiteddimid=dimid), 'inquire unlimiteddimid')
    call nf90_err(nf90_inquire_dimension(ncid, dimid, len=unlen), 'inquire unlimited dimension')
    call nf90_err(nf90_close(ncid), 'close: '//trim(fname))
  end function get_unlimited_len

  !> Convenience function to return a 16-character time string
  !!
  !! @param[in]  MyTime   an ESMF_Time object
  !! @param[out] rc       return code
  !! @return              16-character formatted time string (YYYY_MM_DD_HH_MM)
  character(len=16) function get_timestr(MyTime, rc) result(timestr)

    type(ESMF_Time), intent(in)  :: MyTime
    integer,         intent(out) :: rc

    integer :: year, month, day, hour, minute
    !----------------------------------------------------------------------------
    rc = ESMF_SUCCESS

    call ESMF_TimeGet(MyTime, yy=year, mm=month, dd=day, h=hour, m=minute, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    write(timestr,'(I4.4,4(A,I2.2))')year,'_',month,'_',day,'_',hour,'_',minute
  end function get_timestr

  !> Convenience function to return import/export timestring
  !!
  !! @param[in]  currTime   an ESMF_Time object
  !! @param[in]  nextTime   an ESMF_Time object
  !! @param[out] rc         return code
  !! @return                40-character string
  character(len=40) function get_importexport(currTime, nextTime, rc) result(importexport)

    type(ESMF_Time), intent(in)  :: currTime, nextTime
    integer,         intent(out) :: rc

    character(len=19) :: import_timestr, export_timestr
    !----------------------------------------------------------------------------
    rc = ESMF_SUCCESS

    call ESMF_TimeGet(currTime, timestring=import_timestr, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TimeGet(nextTime, timestring=export_timestr, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    importexport = trim(import_timestr)//'  '//trim(export_timestr)
  end function get_importexport

  !> Write debug info to stdout
  !!
  !! @param[in] tag            an information tag
  !! @param[in] fname          the filename to check
  !! @param[in] chkflag        logical flag for checking next Advance
  !! @param[in] timestring     a timestring
  !! @param [out]rc            return code
  subroutine debug_info(tag,fname,chkflag,timestring)

    character(len=*), intent(in) :: tag
    character(len=*), intent(in) :: fname
    logical,          intent(in) :: chkflag
    character(len=*), intent(in) :: timestring

    integer :: nlen(1), filesize
    !----------------------------------------------------------------------------

    inquire(file=fname, exist=existflag, size=filesize)
    if (existflag) then
      nlen(1) = get_unlimited_len(fname)
      write(msgString,'(A)')tag//'  '//fname//' exists '//timestring
      if (nlen(1) > 0) then
        print '(A,L,i14)',trim(msgString)//' complete, chkflag ',chkflag,filesize
      else
        print '(A,L,i14)',trim(msgString)//' still  0, chkflag ',chkflag,filesize
      end if
    end if
  end subroutine debug_info

  !> Handle netcdf errors
  !!
  !! @param[in]  ierr        the error code
  !! @param[in]  string      the error message
  subroutine nf90_err(ierr, string)

    integer,          intent(in) :: ierr
    character(len=*), intent(in) :: string
    !----------------------------------------------------------------------------

    if (ierr /= nf90_noerr) then
      write(0, '(A)') 'FATAL ERROR: ' // trim(string)// ' : ' // trim(nf90_strerror(ierr))
      ! This fails on WCOSS2 with Intel 19 compiler. See
      ! https://community.intel.com/t5/Intel-Fortran-Compiler/STOP-and-ERROR-STOP-with-variable-stop-codes/m-p/1182521#M149254
      ! When WCOSS2 moves to Intel 2020+, uncomment the next line and remove stop 99
      !stop ierr
      stop 99
    end if
  end subroutine nf90_err
#endif
end module MOM_cap_outputlog
