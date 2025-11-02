!> This module contains a set of subroutines that check if MOM restart
!! and history files have been written and closed. This file is specific
!! to UWM operational requirements and configurations (eg specific output
!! frequencys in hours) and may break if used outside the scope of intended use.
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
  use ESMF                  , only : ESMF_GridComp
  use ESMF                  , only : ESMF_Time, ESMF_Clock, ESMF_ClockGet, ESMF_Alarm, ESMF_AlarmSet
  use ESMF                  , only : ESMF_ClockGetAlarm, ESMF_AlarmIsRinging, ESMF_AlarmRingerOff
  use ESMF                  , only : ESMF_ClockGetNextTime, ESMF_TimeGet, ESMF_TimeInterval
  use ESMF                  , only : ESMF_TimeIntervalSet, ESMF_TimeIntervalPrint, ESMF_TimePrint
  use ESMF                  , only : ESMF_SUCCESS, ESMF_LogWrite, ESMF_LOGMSG_INFO
  use ESMF                  , only : ESMF_LogSetError, ESMF_LogFoundError, ESMF_LOGERR_PASSTHRU
  use ESMF                  , only : operator(*), operator(+), operator(-), operator(>), operator(==)
  use MOM_cap_methods       , only : ChkErr
  use MOM_cap_time          , only : AlarmInit
  use shr_is_restart_fh_mod , only : log_restart_fh
  use netcdf

  implicit none; private

  public :: outputlog_init, outputlog_run, outputlog_restart

  ! the allowable output frequency for MOM6 history, in hours only
  ! TODO: 3hrly output reqs filename with minutes field
  ! TODO: check multiple output freq for same run, reqs different
  ! known filename root for different freqs
  integer, parameter :: n_freq  = 3
  integer, parameter, dimension(n_freq) :: freq = (/3, 6, 24/)

  ! the timeoffset interval is used only to construct the file name.
  ! the file name must be set as the mid-point of the averaging period.
  ! filenames will be given by
  !      T - (interval * offset + interval/2 * offset)
  ! where interval is the averaging interval in minutes
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
  type(ESMF_TimeInterval) :: timeoffset
  type(ESMF_Time)         :: lastrestart

  type :: outputlog_type
    character(len=128)      :: alarm_name
    integer                 :: opt_n
    logical                 :: chkfile_nextAdvance
    character(len=1024)     :: filename
    type(ESMF_Alarm)        :: alarm
    type(ESMF_TimeInterval) :: filename_timeoffset
    type(ESMF_Time)         :: time_lastrestart
  end type outputlog_type

  type(outputlog_type) :: olog(n_freq)

  logical            :: debug
  logical            :: existflag
  character(len=256) :: restartdir
  character(len=256) :: outputdir
  character(len=2)   :: output_fh
  character(len=3)   :: chour
  character(len=*), parameter :: u_FILE_u = &
       __FILE__

contains
  !> Initialize a set of Alarms at the allowed output frequencies
  !!
  !! @param gcomp an ESMF_GridComp object
  !! @param clock an ESMF_Clock object
  !! @param rc    return code
  subroutine outputlog_init(gcomp, mclock, rc)

    type(ESMF_GridComp)  :: gcomp  !< an ESMF_GridComp object
    type(ESMF_Clock)     :: mclock !< the ESMF_clock for the model
    integer, intent(out) :: rc     !< return code

    ! local variables
    type(ESMF_Time)         :: mcurrTime, startTime
    type(ESMF_TimeInterval) :: timestep
    logical                 :: isPresent, isSet
    integer                 :: n
    character(len=256)      :: value
    character(len=256)      :: subname='MOM_cap:(outputlog_init) '
    !----------------------------------------------------------------------------

    rc = ESMF_SUCCESS

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
    call ESMF_LogWrite('MOM_cap:MOM6 restart directory = '//trim(restartdir), ESMF_LOGMSG_INFO)

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
    call ESMF_LogWrite('MOM_cap:MOM6 output directory = '//trim(outputdir), ESMF_LOGMSG_INFO)

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
    call ESMF_LogWrite('MOM_cap:MOM6 output frequency = '//trim(output_fh), ESMF_LOGMSG_INFO)

    debug = .false.
    call NUOPC_CompAttributeGet(gcomp, name="debug_outputlog", value=value, &
         isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) debug=(trim(value)=="true")
    if (debug) call ESMF_LogWrite('MOM_cap:MOM6 output debug ON', ESMF_LOGMSG_INFO)

    call ESMF_ClockGet(mclock, currTime=mcurrTime, startTime=startTime, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TimeIntervalSet(timeoffset, m=1, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    do n = 1,n_freq
      write(chour,'(I2.2,A)')freq(n),'h'
      olog(n)%alarm_name = 'output_alarm'//trim(chour)
      olog(n)%opt_n = freq(n)
      olog(n)%filename_timeoffset = 90*freq(n)*timeoffset
      olog(n)%chkfile_nextAdvance = .false.
      olog(n)%filename = ''
      olog(n)%time_lastrestart = startTime

      call AlarmInit(mclock,         &
           alarm   = olog(n)%alarm,  &
           option  = 'nhours',       &
           opt_n   = olog(n)%opt_n,  &
           opt_ymd = -999,           &
           RefTime = mcurrTime,      &
           alarmname = olog(n)%alarm_name, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return

      call ESMF_AlarmSet(olog(n)%alarm, clock=mclock, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      call ESMF_LogWrite(trim(subname)//" Output alarm "//trim(olog(n)%alarm_name)//" is Created and Set", ESMF_LOGMSG_INFO)
      if (debug .and. is_root_pe()) then
        call ESMF_TimeIntervalPrint(olog(n)%filename_timeoffset, options="string", rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
      end if

    end do

  end subroutine outputlog_init
  !> Use Alarms at the output frequency to determine if output has been
  !! completed
  !!
  !! @param clock      an ESMF_Clock object
  !! @param atStopTime when present, checks for final output file
  !! @param rc         return code
  subroutine outputlog_run(mclock, atStopTime, rc)

    type(ESMF_Clock)              :: mclock     !< the ESMF_clock for the model
    logical, intent(in), optional :: atStopTime !< if true, checks for final output file
    integer, intent(out)          :: rc         !< return code

    ! local variables
    type(ESMF_Time)         :: nextTime, currTime, startTime
    type(ESMF_TimeInterval) :: timeStep
    logical                 :: lstop
    integer                 :: n, ncid, dimid, nlen
    integer                 :: year, month, day, hour, minute
    character(len=256)      :: import_timestr, export_timestr, importexport !debugging only
    character(len=512)      :: fname
    character(len=256)      :: subname='MOM_cap:(outputlog_run) '
    !----------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    call ESMF_ClockGet(mclock, startTime=startTime, currTime=currTime, timeStep=timeStep, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TimeGet(currTime,          timestring=import_timestr, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TimeGet(currTime+timestep, timestring=export_timestr, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    importexport = trim(import_timestr)//'  '//trim(export_timestr)

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
          olog(n)%chkfile_nextAdvance = .true.
          call ESMF_AlarmRingerOff(olog(n)%alarm, rc=rc )
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
          call ESMF_ClockGetNextTime(mclock, nextTime, rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
          !if (freq(n) < 6) then
          call ESMF_TimeGet (nextTime-olog(n)%filename_timeoffset, yy=year, mm=month, dd=day, h=hour, m=minute, rc=rc )
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
          write(olog(n)%filename,'(A,I4.4,4(A,I2.2),A)')trim(outputdir)//'ocn_',year,'_',month,'_',day,'_',hour,'_',minute,'.nc'
          !else
          !  call ESMF_TimeGet (nextTime-olog(n)%filename_timeoffset, yy=year, mm=month, dd=day, h=hour, rc=rc )
          !  if (ChkErr(rc,__LINE__,u_FILE_u)) return
          !  write(olog(n)%filename,'(A,I4.4,3(A,I2.2),A)')trim(outputdir)//'ocn_',year,'_',month,'_',day,'_',hour,'.nc'
          !end if
          if (debug .and. is_root_pe()) then
            print '(A)',trim(subname)//' fname '//trim(olog(n)%filename)//'  '//trim(importexport)
          end if
        end if

        if (olog(n)%chkfile_nextAdvance) then
          fname = trim(olog(n)%filename)
          inquire(file=fname, exist=existflag)
          if (existflag) then
            call nf90_err(nf90_open(fname, nf90_nowrite, ncid), 'nf90_open: '//fname)
            call nf90_err(nf90_inquire(ncid, unlimiteddimid=dimid), 'inquire unlimiteddimid')
            call nf90_err(nf90_inquire_dimension(ncid, dimid, len=nlen), 'inquire unlimited dimension')
            call nf90_err(nf90_close(ncid), 'close: '//fname)
            if (nlen > 0) then
              olog(n)%chkfile_nextAdvance = .false.
              olog(n)%time_lastrestart = lastrestart
              if (is_root_pe()) then
                ! the file check is taking place one advance after the end of the interval *after* the averaging
                ! interval in the file (eg, 06 average file is checked for at hour=12+1), so the time
                ! needed for logging the file completion is one full averaging interval prior to the current time
                call ESMF_ClockGet(mclock, currTime=currTime, rc=rc)
                if (ChkErr(rc,__LINE__,u_FILE_u)) return
                if (olog(n)%time_lastrestart > startTime) then
                  call log_restart_fh(currTime-60*freq(n)*timeoffset, startTime, 'mom6.'//chour, prefixtime=.true., appendtime=olog(n)%time_lastrestart, rc=rc)
                  if (ChkErr(rc,__LINE__,u_FILE_u)) return
                else
                  call log_restart_fh(currTime-60*freq(n)*timeoffset, startTime, 'mom6.'//chour, prefixtime=.true., rc=rc)
                  if (ChkErr(rc,__LINE__,u_FILE_u)) return
                end if
              endif
            end if
          end if ! existflag
        end if

        if (lstop) then
          call ESMF_TimeGet (currTime-30*freq(n)*timeoffset, yy=year, mm=month, dd=day, h=hour, m=minute, rc=rc )
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
          write(olog(n)%filename,'(A,I4.4,4(A,I2.2),A)')trim(outputdir)//'ocn_',year,'_',month,'_',day,'_',hour,'_',minute,'.nc'
          if (debug .and. is_root_pe()) then
            print '(A)',trim(subname)//' fname XX '//trim(olog(n)%filename)//'  '//trim(importexport)
          end if

          fname = trim(olog(n)%filename)
          inquire(file=fname, exist=existflag)
          if (existflag) then
            call nf90_err(nf90_open(fname, nf90_nowrite, ncid), 'nf90_open: '//fname)
            call nf90_err(nf90_inquire(ncid, unlimiteddimid=dimid), 'inquire unlimiteddimid')
            call nf90_err(nf90_inquire_dimension(ncid, dimid, len=nlen), 'inquire unlimited dimension')
            call nf90_err(nf90_close(ncid), 'close: '//fname)
            if (nlen > 0) then
              olog(n)%chkfile_nextAdvance = .false.
              olog(n)%time_lastrestart = lastrestart
              if (is_root_pe()) then
                ! the file check is taking place at the stopTime (==currTime)
                call ESMF_ClockGet(mclock, currTime=currTime, rc=rc)
                if (ChkErr(rc,__LINE__,u_FILE_u)) return
                if (olog(n)%time_lastrestart > startTime) then
                  call log_restart_fh(currTime, startTime, 'mom6.'//chour, prefixtime=.true., appendtime=olog(n)%time_lastrestart, rc=rc)
                  if (ChkErr(rc,__LINE__,u_FILE_u)) return
                else
                  call log_restart_fh(currTime, startTime, 'mom6.'//chour, prefixtime=.true., rc=rc)
                  if (ChkErr(rc,__LINE__,u_FILE_u)) return
                end if
              end if
            end if
          end if
        end if ! lstop

        if (debug) then
          fname = trim(olog(n)%filename)
          inquire(file=fname, exist=existflag)
          if (existflag) then
            call nf90_err(nf90_open(fname, nf90_nowrite, ncid), 'nf90_open: '//fname)
            call nf90_err(nf90_inquire(ncid, unlimiteddimid=dimid), 'inquire unlimiteddimid')
            call nf90_err(nf90_inquire_dimension(ncid, dimid, len=nlen), 'inquire unlimited dimension')
            call nf90_err(nf90_close(ncid), 'close: '//fname)
            if (is_root_pe()) then
              if (nlen > 0) then
                print '(A,L)',trim(subname)//trim(fname)//' exists '//trim(importexport)//' complete ',olog(n)%chkfile_nextAdvance
              else
                print '(A,L)',trim(subname)//trim(fname)//' exists '//trim(importexport)//' still 0 ',olog(n)%chkfile_nextAdvance
              end if
            end if
          end if
        end if

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
    type(ESMF_Time)         :: startTime, currTime, nextTime
    type(ESMF_TimeInterval) :: timestep
    integer                 :: n, ncid, dimid, nlen
    integer                 :: year, month, day, hour, minute, seconds
    character(len=512)      :: fname
    character(len=15)       :: timestr
    character(len=256)      :: import_timestr, export_timestr, importexport !debugging only
    logical, allocatable    :: allDone(:)
    character(len=8)        :: suffix
    character(len=256)      :: subname='MOM_cap:(outputlog_restart) '
    !----------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    call ESMF_ClockGet(mclock, startTime=startTime, currTime=currTime, timeStep=timeStep, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TimeGet(currTime,          timestring=import_timestr, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TimeGet(currTime+timestep, timestring=export_timestr, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    importexport = trim(import_timestr)//'  '//trim(export_timestr)

    call ESMF_ClockGetNextTime(mclock, nextTime, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TimeGet (nextTime, yy=year, mm=month, dd=day, h=hour, m=minute, s=seconds, rc=rc )
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
        fname = trim(restartdir)//trim(timestr)//'.MOM.res_'//trim(suffix)//'nc'
      endif

      ! check if file is written
      inquire(file=fname, exist=existflag)
      if (existflag) then
        call nf90_err(nf90_open(fname, nf90_nowrite, ncid), 'nf90_open: '//fname)
        call nf90_err(nf90_inquire(ncid, unlimiteddimid=dimid), 'inquire unlimiteddimid')
        call nf90_err(nf90_inquire_dimension(ncid, dimid, len=nlen), 'inquire unlimited dimension')
        call nf90_err(nf90_close(ncid), 'close: '//fname)
        if (nlen > 0) allDone(n) = .true.
        if (is_root_pe())print *,'allDone= ',allDone

        if (debug .and. is_root_pe()) then
          if (nlen > 0) then
            print '(A)',trim(subname)//' restart '//trim(fname)//'  '//trim(importexport)//' complete'
          else
            print '(A)',trim(subname)//' restart '//trim(fname)//'  '//trim(importexport)//' still 0'
          end if
        end if
      end if
    end do ! num_rest_files

    if (any(allDone) == .false.) then
      !call MOM_error(FATAL, 'not all Restart files are complete')
    else
      lastrestart = nextTime
      if (is_root_pe()) then
        call log_restart_fh(nextTime, startTime, 'mom6.res', prefixtime=.true., rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
      endif
    endif

  end subroutine outputlog_restart
  !> Handle netcdf errors
  !!
  !! @param[in]  ierr        the error code
  !! @param[in]  string      the error message
  subroutine nf90_err(ierr, string)

    integer ,         intent(in) :: ierr
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
