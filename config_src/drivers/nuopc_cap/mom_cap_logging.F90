!> This module contains a set of subroutines that check if MOM diagnostic
!! output files have been written and closed
module MOM_cap_logging

  use MOM_error_handler,      only: is_root_pe
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

  public :: outputAlarms_init, outputAlarms_run

  ! for now, only one type of output we need to log (6hrly)
  integer, parameter :: n_freq  = 1
  integer, parameter, dimension(n_freq) :: freq = (/6/)

  ! for generic cases, it will be convienent to use a standard 30min output
  ! interval type to account for eg 1 or 3 hrly output
  ! for now, use only 1 hour for 6-hrly case
  type(ESMF_TimeInterval) :: outputInterval

  type :: ologfile_type
    character(len=256) :: alarm_name
    integer            :: opt_n
    integer            :: interval_factor     !num of intervals between output files, for a given output freq
    logical            :: chkfile_nextAdvance
    character(len=256) :: filename
    type(ESMF_Alarm)   :: alarm
  end type ologfile_type

  type(ologfile_type) :: olog(n_freq)

  character(len=*), parameter :: u_FILE_u = &
       __FILE__

contains

!> Initialize a set of Alarms at the diagnostic output frequency
!!
!! @param clock an ESMF_Clock object
!! @param rc return code
  subroutine outputAlarms_init(mclock, rc)

    type(ESMF_Clock)     :: mclock !< the ESMF_clock for the model
    integer, intent(out) :: rc     !< return code

    ! local variables
    type(ESMF_Time)         :: mcurrtime
    type(ESMF_TimeInterval) :: timestep
    character(len=3)        :: chour
    integer                 :: n
    character(len=256)      :: subname='MOM_cap:(outputAlarms_init) '
    !--------------------------------

    rc = ESMF_SUCCESS

    call ESMF_ClockGet(mclock, currTime=mcurrtime, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_TimeIntervalSet(outputInterval, h=1, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    do n = 1,n_freq
      write(chour,'(i2.2,a)')freq(n),'h'
      olog(n)%alarm_name = 'output_alarm'//trim(chour)
      olog(n)%opt_n = freq(n)
      ! for 6hr, factor = 6 + 3
      olog(n)%interval_factor = olog(n)%opt_n + olog(n)%opt_n/2
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
  end subroutine outputAlarms_init
  !> Use Alarms at the diagnostic output frequency to determine if output has been
  !! completed
  !!
  !! @param clock an ESMF_Clock object
  !! @param rc return code
  subroutine outputAlarms_run(mclock, rc)

    type(ESMF_Clock)     :: mclock !< the ESMF_clock for the model
    integer, intent(out) :: rc     !< return code

    ! local variables
    type(ESMF_Time)         :: nextTime, mcurrTime
    type(ESMF_TimeInterval) :: timeStep
    logical                 :: existflag
    integer                 :: n, ncid, dimid, nlen
    integer                 :: year, month, day, hour
    character(256)          :: import_timestr, export_timestr, fname
    character(len=256)      :: subname='MOM_cap:(outputAlarms_run) '
    !--------------------------------

    rc = ESMF_SUCCESS

    call ESMF_ClockGet(mclock, currTime=mcurrTime, timeStep=timeStep, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TimeGet(mcurrTime,          timestring=import_timestr, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TimeGet(mcurrTime+timestep, timestring=export_timestr, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    do n = 1,n_freq
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
        call ESMF_TimeGet (nextTime-olog(n)%interval_factor*outputInterval, yy=year, mm=month, dd=day, h=hour, rc=rc )
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        write(olog(n)%filename,'(a,i4.4,3(a,i2.2),a)')'ocn_',year,'_',month,'_',day,'_',hour,'.nc'
        if(is_root_pe())print *,'XX0 '//trim(olog(n)%filename)//'  '//trim(import_timestr)//'  '//trim(export_timestr)
      end if

      if (olog(n)%chkfile_nextAdvance) then
        ! check if file is written
        fname = trim(olog(n)%filename)
        inquire(file=fname, exist=existflag)
        if (existflag) then
          !open and inquire unlimdim
          call nf90_err(nf90_open(fname, nf90_nowrite, ncid), 'nf90_open: '//fname)
          call nf90_err(nf90_inquire(ncid, unlimiteddimid=dimid), 'inquire unlimiteddimid')
          call nf90_err(nf90_inquire_dimension(ncid, dimid, len=nlen), 'inquire unlimited dimension')
          call nf90_err(nf90_close(ncid), 'close: '//fname)
          if (nlen > 0) then
            olog(n)%chkfile_nextAdvance = .false.
            if(is_root_pe())print *,'XX '//trim(olog(n)%filename)//'  '//trim(import_timestr)//'  '//trim(export_timestr)//' complete'
          else
            if(is_root_pe())print *,'XX '//trim(olog(n)%filename)//'  '//trim(import_timestr)//'  '//trim(export_timestr)//' still 0'
          end if
        end if
      end if
    end do
  end subroutine outputAlarms_run
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
end module MOM_cap_logging
