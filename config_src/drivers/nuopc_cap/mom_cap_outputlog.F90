!> This module contains a set of subroutines that check if MOM
!! history files have been written and closed. This file is
!! specific to UWM operational requirements and configurations
!! (eg specific output frequencys in hours) and may break if
!! used outside the scope of intended use.
!! This module a stub when CESMCOUPLED is defined
module MOM_cap_outputlog

#ifdef CESMCOUPLED
  use ESMF                  , only : ESMF_Clock, ESMF_SUCCESS
  implicit none; private

  public :: outputlog_init, outputlog_run
contains
  subroutine outputlog_init(gcomp, mclock, rc)

    type(ESMF_GridComp)  :: gcomp  !< an ESMF_GridComp object
    type(ESMF_Clock)     :: mclock !< the ESMF_clock for the model
    integer, intent(out) :: rc     !< return code
    rc = ESMF_SUCCESS
  end subroutine outputlog_init
  subroutine outputlog_run(mclock, rc)
    type(ESMF_Clock)     :: mclock !< the ESMF_clock for the model
    integer, intent(out) :: rc     !< return code
    rc = ESMF_SUCCESS
  end subroutine outputlog_run
end module MOM_cap_outputlog
#else
  use MOM_error_handler,      only : is_root_pe
  use ESMF                  , only : ESMF_Time, ESMF_Clock, ESMF_ClockGet, ESMF_Alarm, ESMF_AlarmSet
  use ESMF                  , only : ESMF_ClockGetAlarm, ESMF_AlarmIsRinging, ESMF_AlarmRingerOff
  use ESMF                  , only : ESMF_ClockGetNextTime, ESMF_TimeGet, ESMF_TimeInterval
  use ESMF                  , only : ESMF_TimeIntervalSet
  use ESMF                  , only : ESMF_SUCCESS, ESMF_LogWrite, ESMF_LOGMSG_INFO
  use ESMF                  , only : ESMF_LogSetError, ESMF_LogFoundError, ESMF_LOGERR_PASSTHRU
  use ESMF                  , only : operator(*), operator(+), operator(-)
  use MOM_cap_methods       , only : ChkErr
  use MOM_cap_time          , only : AlarmInit
  use shr_is_restart_fh_mod , only : log_restart_fh
  use netcdf

  implicit none; private

  public :: outputlog_init, outputlog_run

  ! the allowable output frequency for MOM6 history, in hours only
  ! TODO: 3hrly output reqs filename with minutes field
  ! TODO: check multiple output freq for same run, reqs different
  ! known filename root for different freqs
  integer, parameter :: n_freq  = 3
  integer, parameter, dimension(n_freq) :: freq = (/3, 6, 24/)

  ! the timeoffset interval is used only to construct the file name.
  ! filenames will be given by T - (interval * offset + interval/2 * offset)
  ! where interval is the averaging interval in minutes
  !   00   .     06   .   12   .  18
  !        03 = 12 - (6 + 3)
  !                  09 = 18 - (6 + 3)
  ! the timeoffset must be defined in minutes to allow sub-6hrly output
  type(ESMF_TimeInterval) :: timeoffset

  type :: outputlog_type
    character(len=128)      :: alarm_name
    integer                 :: opt_n
    logical                 :: chkfile_nextAdvance
    character(len=1024)     :: filename
    type(ESMF_Alarm)        :: alarm
    type(ESMF_TimeInterval) :: filename_timeoffset
  end type outputlog_type

  type(outputlog_type) :: olog(n_freq)

  character(len=256) :: outputdir
  character(len=2)   :: output_fh
  character(len=3)   :: chour
  character(len=*), parameter :: u_FILE_u = &
       __FILE__

contains

!> Initialize a set of Alarms at the allowed output frequencies
!!
!! @param clock an ESMF_Clock object
!! @param rc return code
  subroutine outputlog_init(gcomp, mclock, rc)

    type(ESMF_GridComp)  :: gcomp  !< an ESMF_GridComp object
    type(ESMF_Clock)     :: mclock !< the ESMF_clock for the model
    integer, intent(out) :: rc     !< return code

    ! local variables
    type(ESMF_Time)         :: mcurrtime
    type(ESMF_TimeInterval) :: timestep
    integer                 :: n
    character(len=256)      :: value
    character(len=256)      :: subname='MOM_cap:(outputlog_init) '
    !--------------------------------

    rc = ESMF_SUCCESS

    call NUOPC_CompAttributeGet(gcomp, name="mom6_outputdir", value=value, &
         isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) then
      outputdir = trim(value)
    else
      outputdir = './'
    end if
    call ESMF_LogWrite('MOM_cap:MOM6 output directory = '//trim(outputdir), ESMF_LOGMSG_INFO)

    call NUOPC_CompAttributeGet(gcomp, name="mom6_output_fh", value=value, &
         isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) then
      write(output_fh, '(a2)')trim(value)
    else
      output_fh = '06'
    end if
    call ESMF_LogWrite('MOM_cap:MOM6 output frequency = '//trim(output_fh), ESMF_LOGMSG_INFO)

    call ESMF_ClockGet(mclock, currTime=mcurrtime, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_TimeIntervalSet(timeoffset, m=1, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    do n = 1,n_freq
      write(chour,'(i2.2,a)')freq(n),'h'
      olog(n)%alarm_name = 'output_alarm'//trim(chour)
      olog(n)%opt_n = freq(n)
      olog(n)%filename_timeoffset = 90*freq(n)*timeoffset
      olog(n)%chkfile_nextAdvance = .false.
      olog(n)%filename = ''

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
      call ESMF_LogWrite(subname//" Output alarm "//trim(olog(n)%alarm_name)//" is Created and Set", ESMF_LOGMSG_INFO)
    end do
  end subroutine outputlog_init
  !> Use Alarms at the output frequency to determine if output has been
  !! completed
  !!
  !! @param clock an ESMF_Clock object
  !! @param rc return code
  subroutine outputlog_run(mclock, rc)

    type(ESMF_Clock)     :: mclock !< the ESMF_clock for the model
    integer, intent(out) :: rc     !< return code

    ! local variables
    type(ESMF_Time)         :: nextTime, currTime, startTime
    type(ESMF_TimeInterval) :: timeStep
    logical                 :: existflag
    integer                 :: n, ncid, dimid, nlen
    integer                 :: year, month, day, hour, minute
    character(256)          :: import_timestr, export_timestr, fname
    character(len=256)      :: subname='MOM_cap:(outputlog_run) '
    !--------------------------------

    rc = ESMF_SUCCESS

    call ESMF_ClockGet(mclock, startTime=startTime, currTime=currTime, timeStep=timeStep, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TimeGet(currTime,          timestring=import_timestr, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TimeGet(currTime+timestep, timestring=export_timestr, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    do n = 1,n_freq
      write(chour,'(i2.2,a)')freq(n),'h'
      if (chour == outputfh(1:2)) then
        call ESMF_ClockGetAlarm(mclock, alarmname=trim(olog(n)%alarm_name), alarm=olog(n)%alarm, rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        if (ESMF_AlarmIsRinging(olog(n)%alarm, rc=rc)) then
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
          olog(n)%chkfile_nextAdvance = .true.
          ! turn off the alarm
          call ESMF_AlarmRingerOff(olog(n)%alarm, rc=rc )
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
          ! set filename
          call ESMF_ClockGetNextTime(mclock, nextTime, rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
          if (freq(n) < 6) then
             call ESMF_TimeGet (nextTime-olog(n)%filename_timeoffset, yy=year, mm=month, dd=day, h=hour, m=minute, rc=rc )
             if (ChkErr(rc,__LINE__,u_FILE_u)) return
             write(olog(n)%filename,'(a,i4.4,4(a,i2.2),a)')trim(outputdir)//'ocn_',year,'_',month,'_',day,'_',hour,'_',minute,'.nc'
          else
             call ESMF_TimeGet (nextTime-olog(n)%filename_timeoffset, yy=year, mm=month, dd=day, h=hour, rc=rc )
             if (ChkErr(rc,__LINE__,u_FILE_u)) return
             write(olog(n)%filename,'(a,i4.4,3(a,i2.2),a)')trim(outputdir)//'ocn_',year,'_',month,'_',day,'_',hour,'.nc'
          end if
          if(is_root_pe())print *,'XX0 '//trim(olog(n)%filename)//'  '//trim(import_timestr)//'  '//trim(export_timestr)
        end if

        if (olog(n)%chkfile_nextAdvance) then
          ! check if file is written
          fname = trim(olog(n)%filename)
          inquire(file=fname, exist=existflag)
          if (existflag) then
            call nf90_err(nf90_open(fname, nf90_nowrite, ncid), 'nf90_open: '//fname)
            call nf90_err(nf90_inquire(ncid, unlimiteddimid=dimid), 'inquire unlimiteddimid')
            call nf90_err(nf90_inquire_dimension(ncid, dimid, len=nlen), 'inquire unlimited dimension')
            call nf90_err(nf90_close(ncid), 'close: '//fname)
            if (nlen > 0) then
              olog(n)%chkfile_nextAdvance = .false.
              if (is_root_pe()) then
                call log_restart_fh(currTime+timestep, startTime, 'mom6.'//chour, prefixtime=.true., rc=rc)
                if (ChkErr(rc,__LINE__,u_FILE_u)) return
              endif
              if(is_root_pe())print *,'XX '//trim(olog(n)%filename)//'  '//trim(import_timestr)//'  '//trim(export_timestr)//' complete'
            else
              if(is_root_pe())print *,'XX '//trim(olog(n)%filename)//'  '//trim(import_timestr)//'  '//trim(export_timestr)//' still 0'
            end if
          end if ! existflag
        end if ! chkfile_nextAdvance
      end if ! chour = output_fh
    end do
#ifdef test

    fname = 'ocn_2011_10_01_10_30.nc'
    inquire(file=fname, exist=existflag)
    if (existflag) then
      !open and inquire unlimdim
      call nf90_err(nf90_open(fname, nf90_nowrite, ncid), 'nf90_open: '//fname)
      call nf90_err(nf90_inquire(ncid, unlimiteddimid=dimid), 'inquire unlimiteddimid')
      call nf90_err(nf90_inquire_dimension(ncid, dimid, len=nlen), 'inquire unlimited dimension')
      call nf90_err(nf90_close(ncid), 'close: '//fname)
      if (nlen > 0) then
        if(is_root_pe())print '(A)','XX '//trim(fname)//' exists  '//trim(import_timestr)//'  '//trim(export_timestr)//' complete'
      else
        if(is_root_pe())print '(A)','XX '//trim(fname)//' exists  '//trim(import_timestr)//'  '//trim(export_timestr)//' still 0'
      end if
    end if

    fname = 'ocn_2011_10_03_01_30.nc'
    inquire(file=fname, exist=existflag)
    if (existflag) then
      !open and inquire unlimdim
      call nf90_err(nf90_open(fname, nf90_nowrite, ncid), 'nf90_open: '//fname)
      call nf90_err(nf90_inquire(ncid, unlimiteddimid=dimid), 'inquire unlimiteddimid')
      call nf90_err(nf90_inquire_dimension(ncid, dimid, len=nlen), 'inquire unlimited dimension')
      call nf90_err(nf90_close(ncid), 'close: '//fname)
      if (nlen > 0) then
        if(is_root_pe())print '(A)','XX '//trim(fname)//' exists  '//trim(import_timestr)//'  '//trim(export_timestr)//' complete'
      else
        if(is_root_pe())print '(A)','XX '//trim(fname)//' exists  '//trim(import_timestr)//'  '//trim(export_timestr)//' still 0'
      end if
    end if
#endif
  end subroutine outputlog_run
  !> Handle netcdf errors
  !!
  !! @param[in]  ierr        the error code
  !! @param[in]  string      the error message
  !!
  subroutine nf90_err(ierr, string)

    integer ,         intent(in) :: ierr
    character(len=*), intent(in) :: string
    !----------------------------------------------------------------------------

    if (ierr /= nf90_noerr) then
      write(0, '(a)') 'FATAL ERROR: ' // trim(string)// ' : ' // trim(nf90_strerror(ierr))
      ! This fails on WCOSS2 with Intel 19 compiler. See
      ! https://community.intel.com/t5/Intel-Fortran-Compiler/STOP-and-ERROR-STOP-with-variable-stop-codes/m-p/1182521#M149254
      ! When WCOSS2 moves to Intel 2020+, uncomment the next line and remove stop 99
      !stop ierr
      stop 99
    end if
  end subroutine nf90_err
end module MOM_cap_outputlog
#endif
