!> This module contains a set of subroutines that check if MOM restart and history files
!! have been written and closed. This file is specific to UWM operational requirements
!! and configurations (eg specific output frequencies in hours) and may break if used outside
!! the scope of intended use.
!! This module is a stub when CESMCOUPLED is defined
module MOM_cap_outputlog

#ifdef CESMCOUPLED
  use ESMF                  , only : ESMF_GridComp, ESMF_Clock, ESMF_SUCCESS
  use MOM_grid              , only : ocean_grid_type
implicit none; private

public :: outputlog_init, outputlog_run, outputlog_restart
contains
subroutine outputlog_init(gcomp, mclock, ocean_grid, rc)
  type(ESMF_GridComp)  :: gcomp                            !< an ESMF_GridComp object
  type(ESMF_Clock)     :: mclock                           !< the ESMF_clock for the model
  type(ocean_grid_type), pointer, intent(in) :: ocean_grid !< the ocean grid
  integer, intent(out) :: rc                               !< return code
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
use MOM_coms_infra        , only : root_pe
use MOM_error_handler     , only : is_root_pe, MOM_error, FATAL
use MOM_get_input         , only : get_MOM_input, directories
use MOM_grid              , only : ocean_grid_type
use mpp_domains_mod       , only : mpp_get_io_domain_layout
use NUOPC                 , only : NUOPC_CompAttributeGet
use ESMF                  , only : ESMF_GridComp, ESMF_GridCompGet, ESMF_VM, ESMF_VMGet
use ESMF                  , only : ESMF_Time, ESMF_Clock, ESMF_ClockGet, ESMF_Alarm, ESMF_AlarmSet
use ESMF                  , only : ESMF_ClockGetAlarm, ESMF_AlarmIsRinging, ESMF_AlarmRingerOff
use ESMF                  , only : ESMF_ClockGetNextTime, ESMF_TimeGet, ESMF_TimeInterval
use ESMF                  , only : ESMF_AlarmGet, ESMF_TimeIntervalSet, ESMF_TimeIntervalPrint
use ESMF                  , only : ESMF_SUCCESS, ESMF_LogWrite, ESMF_LOGMSG_INFO, ESMF_FAILURE
use ESMF                  , only : ESMF_LogSetError, ESMF_LogFoundError, ESMF_LOGERR_PASSTHRU
use ESMF                  , only : operator(*), operator(+), operator(-), operator(>), operator(==)
use MOM_cap_methods       , only : ChkErr
use MOM_cap_time          , only : AlarmInit
use shr_is_restart_fh_mod , only : log_restart_fh
use mom_outputlog_methods , only : get_file_state, file_is_complete, get_unlimited_len
use mom_outputlog_methods , only : get_timestr, get_importexport
use mom_outputlog_methods , only : readnml, debug_info, nf90_err
use mom_outputlog_methods , only : outputlog_config_type, outputlog_state_type
use mpi_f08               , only : MPI_Comm, MPI_INTEGER, MPI_SUCCESS
use netcdf

implicit none; private

public :: outputlog_init, outputlog_run, outputlog_restart

! the allowable output frequency for MOM6 history, in hours only
integer, parameter :: n_freq  = 4
integer, parameter, dimension(n_freq) :: freq = (/1, 3, 6, 24/)
! TODO: for multiple output freq in same run, a different known filename
! root for different freqs needs to be read in, consistent with the diag table

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
! since both the final interval and the next-to-final interval can be closed at the stop time,
! a different log file name is required for the final log file, otherwise the next-to-final
! log is overwritten
!
! Depending on configuration, the output file can have an unlimited dimension >0 at creation time.
! This necessitates checking for an additional criteria using the filesize at creation. An output file
! is declared complete either when the unlimited dimension in the file is >0 or when the unlimited
! dimension is >0 and the filesize is larger than the initial size.

! When a file is determined to be complete, a log file is recorded containing the forecast hour, the valid
! time, the name of the output file and the last completed restart file.

type(outputlog_config_type) :: cf(n_freq)
type(outputlog_state_type)  :: state(n_freq)

type(ESMF_VM)           :: vm
type(ESMF_TimeInterval) :: tincrement
type(ESMF_Time)         :: lastrestart
type(MPI_Comm)          :: mpicomm

integer                 :: toffset
integer                 :: nfiles
logical                 :: debug_onroot
character(len=256)      :: restartdir
character(len=256)      :: outputdir
character(len=256)      :: errmsg
character(len=*), parameter :: u_FILE_u = &
     __FILE__

contains
!> Initialize a set of Alarms at the allowed output frequencies
!!
!! @param      gcomp        an ESMF_GridComp object
!! @param      clock        an ESMF_Clock object
!! @param[in]  ocean_grid   ocean grid
!! @param[out] rc           return code
subroutine outputlog_init(gcomp, mclock, ocean_grid, rc)

  type(ESMF_GridComp)  :: gcomp
  type(ESMF_Clock)     :: mclock
  type(ocean_grid_type), pointer, intent(in) :: ocean_grid
  integer, intent(out) :: rc

  ! local variables
  type(ESMF_Time)         :: mcurrTime
  type(ESMF_TimeInterval) :: alarmoffset
  type(directories)       :: dirs
  logical                 :: debug
  integer                 :: n, int_mpic, io_layout(2)
  integer                 :: year, month, day, hour
  character(len=3)        :: chour
  character(len=256)      :: msgString
  character(len=256)      :: subname='MOM_cap:(outputlog_init)'
  !----------------------------------------------------------------------------

  rc = ESMF_SUCCESS
  call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
  if (ChkErr(rc,__LINE__,u_FILE_u)) return
  call ESMF_VMGet(vm=vm,mpiCommunicator=int_mpic, rc=rc)
  if (ChkErr(rc,__LINE__,u_FILE_u)) return
  mpicomm%mpi_val = int_mpic

  call get_MOM_input(dirs=dirs)
  restartdir = trim(dirs%restart_output_dir)
  outputdir = trim(dirs%output_directory)
  !print *,'XXX restart dirs = '//trim(restartdir)
  !print *,'XXX output dirs = '//trim(outputdir)

  io_layout = mpp_get_io_domain_layout(ocean_grid%Domain%mpp_domain)
  nfiles = io_layout(1) * io_layout(2)

  call ESMF_ClockGet(mclock, currTime=mcurrTime, rc=rc)
  if (ChkErr(rc,__LINE__,u_FILE_u)) return
  call ESMF_TimeIntervalSet(tincrement, m=1, rc=rc)
  if (ChkErr(rc,__LINE__,u_FILE_u)) return

  ! get start hour time offset (ie, fhrot)
  call ESMF_TimeGet(mcurrTime, yy=year, mm=month, dd=day, h=hour, rc=rc)
  if (ChkErr(rc,__LINE__,u_FILE_u)) return
  if (mod(hour,6) /= 0) then
    toffset = hour - 6
  else
    toffset = 0
  endif

  ! initialize
  lastrestart = mcurrTime
  do n = 1,n_freq
    write(chour,'(I2.2,A)')freq(n),'h'
    cf(n)%alarm_name        = 'output_alarm'//trim(chour)
    cf(n)%opt_n             = freq(n)
    cf(n)%requested         = .false.
    cf(n)%timereduce        = ''
    cf(n)%fnameprefix       = ''
    if (nfiles == 1) then
      cf(n)%fnamesuffix     = ''
    else
      cf(n)%fnamesuffix     = '.000'
    endif
    cf(n)%fhoffset          = 0*tincrement
    cf(n)%filename_fhoffset = 0*tincrement

    state(n)%chkfile_nextAdvance = .false.
    state(n)%use_filesize        = .false.
    state(n)%filename            = ''
    state(n)%createsize          = 0
    state(n)%time_lastrestart    = lastrestart

    ! the time offset in hours required to ensure the alarm rings at multiples of 6
    if (freq(n) >= 6) then
      alarmoffset = toffset*60*tincrement
    else
      alarmoffset = 0*tincrement
    endif

    call AlarmInit(mclock,                  &
         alarm     = cf(n)%alarm,           &
         option    = 'nhours',              &
         opt_n     = cf(n)%opt_n,           &
         opt_ymd   = -999,                  &
         RefTime   = mcurrTime+alarmoffset, &
         alarmname = cf(n)%alarm_name, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_AlarmSet(cf(n)%alarm, clock=mclock, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    write(msgString,'(A)')trim(subname)//' Output alarm '//trim(cf(n)%alarm_name)//' Created & Set'
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
  enddo

  call readnml('input.nml', cf, debug, errmsg, rc=rc)
  rc = merge(ESMF_SUCCESS, ESMF_FAILURE, rc == 0)
  if (ChkErr(rc,__LINE__,u_FILE_u)) return
  if (is_root_pe() .and. len_trim(errmsg) > 0) print '(A)',trim(subname)//trim(errmsg)

  debug_onroot = debug .and. is_root_pe()

  do n = 1,n_freq
    if (trim(cf(n)%timereduce) == 'none') then
      cf(n)%filename_fhoffset   = 60*freq(n)*tincrement
    else
      cf(n)%filename_fhoffset   = 90*freq(n)*tincrement
    endif
  enddo

  ! Snapshots and IO_Layout not yet implemented
  if (nfiles > 1) then
    cf(:)%requested = .false.
    if (is_root_pe())print '(A)',trim(subname)//' output logging unavailable when IO_LAYOUT is used '
  endif
  ! do n = 1,n_freq
  !   if (trim(cf(n)%timereduce) == 'none') then
  !     cf(n)%requested = .false.
  !     if (is_root_pe())print '(A)',trim(subname)//' output logging unavailable when Snapshots are requested '
  !   endif
  ! enddo

  if (is_root_pe()) then
    do n = 1,n_freq
      print '(A,i8)',trim(subname)//' toffset = ',toffset
      call ESMF_TimeIntervalPrint(cf(n)%filename_fhoffset, options="string", rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
    enddo

    do n = 1,n_freq
      if (cf(n)%requested) print '(A,i6,A)',trim(subname)//' output requested: freq (hours)= ' &
           ,cf(n)%opt_n,', time_reduction= '//cf(n)%timereduce
    enddo
  endif

end subroutine outputlog_init
!> Write a log file denoting that an output file is complete
!!
!! @param clock        an ESMF_Clock object
!! @param atStopTime   when present, checks for final output file
!! @param rc           return code
subroutine outputlog_run(mclock, atStopTime, rc)
  type(ESMF_Clock)              :: mclock
  logical, intent(in), optional :: atStopTime
  integer, intent(out)          :: rc

  ! local variables
  type(ESMF_Time)    :: nextTime, currTime, startTime, prevRing
  logical            :: lstop
  logical            :: filecomplete
  integer            :: n, nlen, fsize
  character(len=3)   :: chour
  character(len=40)  :: importexport
  character(len=16)  :: timestr
  character(len=256) :: subname='MOM_cap:(outputlog_run)'
  !debug
  character(len=256) :: tmpname
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
  endif
  fsize = nf90_fill_int
  nlen  = nf90_fill_int

  do n = 1,n_freq
    write(chour,'(I2.2,A)')freq(n),'h'
    filecomplete = .false.
    if (cf(n)%requested) then
      call ESMF_ClockGetAlarm(mclock, alarmname=trim(cf(n)%alarm_name), alarm=cf(n)%alarm, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      ! when the alarm rings, set file check on next advance and construct the filename
      if (ESMF_AlarmIsRinging(cf(n)%alarm, rc=rc)) then
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        call ESMF_AlarmRingerOff(cf(n)%alarm, rc=rc )
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        state(n)%chkfile_nextAdvance = .true.

        timestr = get_timestr(nextTime-cf(n)%filename_fhoffset, rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        state(n)%filename = trim(outputdir)//trim(cf(n)%fnameprefix)//trim(timestr)//'.nc' &
             //trim(cf(n)%fnamesuffix)

        call get_file_state(mpicomm, is_root_pe(), root_pe(), state(n)%filename, nlen=nlen, &
             fsize=fsize, rc=rc)
        rc = merge(ESMF_SUCCESS, ESMF_FAILURE, rc == 0)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return

        state(n)%createsize = fsize
        if (nlen == 0) then
          state(n)%use_filesize = .false.
        else
          state(n)%use_filesize = .true.
        endif

        if (debug_onroot then
          print '(A,2(A,L),A,2i16)',trim(subname)//' fname '//trim(state(n)%filename)//'  '      &
               //trim(importexport),' checkflag ',state(n)%chkfile_nextAdvance,' use_filesize ', &
               state(n)%use_filesize, '  ',state(n)%createsize,nlen
        endif
      endif ! ESMF_AlarmIsRinging

        ! tmpname = "MOM6_OUTPUT/ocn_2021_03_22_06_30.nc"
        ! importexport = get_importexport(currTime, nextTime, rc=rc)
        ! call get_file_state(mpicomm, is_root_pe(), root_pe(), trim(tmpname), nlen=nlen, &
        !      fsize=fsize, rc=rc)
        ! rc = merge(ESMF_SUCCESS, ESMF_FAILURE, rc == 0)
        ! if (ChkErr(rc,__LINE__,u_FILE_u)) return
        ! if(is_root_pe()) then
        !   inquire(file=trim(tmpname), exist=existflag)
        !   if (existflag) then
        !     print '(a,i6,i16)','XXX  '//trim(tmpname)//' exist '//trim(importexport)//' ',nlen,fsize
        !   endif
        ! endif

        ! tmpname = "MOM6_OUTPUT/ocn_2021_03_22_07_00.nc"
        ! importexport = get_importexport(currTime, nextTime, rc=rc)
        ! call get_file_state(mpicomm, is_root_pe(), root_pe(), trim(tmpname), nlen=nlen, &
        !      fsize=fsize, rc=rc)
        ! rc = merge(ESMF_SUCCESS, ESMF_FAILURE, rc == 0)
        ! if (ChkErr(rc,__LINE__,u_FILE_u)) return
        ! if(is_root_pe()) then
        !   inquire(file=trim(tmpname), exist=existflag)
        !   if (existflag) then
        !     print '(a,i6,i16)','XXX  '//trim(tmpname)//' exist '//trim(importexport)//' ',nlen,fsize
        !   endif
        ! endif

        ! tmpname = "MOM6_OUTPUT/ocn_2021_03_22_07_30.nc"
        ! importexport = get_importexport(currTime, nextTime, rc=rc)
        ! call get_file_state(mpicomm, is_root_pe(), root_pe(), trim(tmpname), nlen=nlen, &
        !      fsize=fsize, rc=rc)
        ! rc = merge(ESMF_SUCCESS, ESMF_FAILURE, rc == 0)
        ! if (ChkErr(rc,__LINE__,u_FILE_u)) return
        ! if(is_root_pe()) then
        !   inquire(file=trim(tmpname), exist=existflag)
        !   if (existflag) then
        !     print '(a,i6,i16)','XXX  '//trim(tmpname)//' exist '//trim(importexport)//' ',nlen,fsize
        !   endif
        ! endif

        ! tmpname = "MOM6_OUTPUT/ocn_2021_03_22_08_00.nc"
        ! importexport = get_importexport(currTime, nextTime, rc=rc)
        ! call get_file_state(mpicomm, is_root_pe(), root_pe(), trim(tmpname), nlen=nlen, &
        !      fsize=fsize, rc=rc)
        ! rc = merge(ESMF_SUCCESS, ESMF_FAILURE, rc == 0)
        ! if (ChkErr(rc,__LINE__,u_FILE_u)) return
        ! if(is_root_pe()) then
        !   inquire(file=trim(tmpname), exist=existflag)
        !   if (existflag) then
        !     print '(a,i6,i16)','XXX  '//trim(tmpname)//' exist '//trim(importexport)//' ',nlen,fsize
        !   endif
        ! endif

        ! tmpname = "MOM6_OUTPUT/ocn_2021_03_22_08_30.nc"
        ! importexport = get_importexport(currTime, nextTime, rc=rc)
        ! call get_file_state(mpicomm, is_root_pe(), root_pe(), trim(tmpname), nlen=nlen, &
        !      fsize=fsize, rc=rc)
        ! rc = merge(ESMF_SUCCESS, ESMF_FAILURE, rc == 0)
        ! if (ChkErr(rc,__LINE__,u_FILE_u)) return
        ! if(is_root_pe()) then
        !   inquire(file=trim(tmpname), exist=existflag)
        !   if (existflag) then
        !     print '(a,i6,i16)','XXX  '//trim(tmpname)//' exist '//trim(importexport)//' ',nlen,fsize
        !   endif
        ! endif

        ! tmpname = "MOM6_OUTPUT/ocn_2021_03_22_09_00.nc"
        ! importexport = get_importexport(currTime, nextTime, rc=rc)
        ! call get_file_state(mpicomm, is_root_pe(), root_pe(), trim(tmpname), nlen=nlen, &
        !      fsize=fsize, rc=rc)
        ! rc = merge(ESMF_SUCCESS, ESMF_FAILURE, rc == 0)
        ! if (ChkErr(rc,__LINE__,u_FILE_u)) return
        ! if(is_root_pe()) then
        !   inquire(file=trim(tmpname), exist=existflag)
        !   if (existflag) then
        !     print '(a,i6,i16)','XXX  '//trim(tmpname)//' exist '//trim(importexport)//' ',nlen,fsize
        !   endif
        ! endif

        ! tmpname = "MOM6_OUTPUT/ocn_2021_03_22_09_30.nc"
        ! importexport = get_importexport(currTime, nextTime, rc=rc)
        ! call get_file_state(mpicomm, is_root_pe(), root_pe(), trim(tmpname), nlen=nlen, &
        !      fsize=fsize, rc=rc)
        ! rc = merge(ESMF_SUCCESS, ESMF_FAILURE, rc == 0)
        ! if (ChkErr(rc,__LINE__,u_FILE_u)) return
        ! if(is_root_pe()) then
        !   inquire(file=trim(tmpname), exist=existflag)
        !   if (existflag) then
        !     print '(a,i6,i16)','XXX  '//trim(tmpname)//' exist '//trim(importexport)//' ',nlen,fsize
        !   endif
        ! endif

      if (state(n)%chkfile_nextAdvance) then
        filecomplete = file_is_complete(mpicomm, is_root_pe(), root_pe(), state(n)%filename, &
             state(n)%use_filesize, state(n)%createsize, rc)
        rc = merge(ESMF_SUCCESS, ESMF_FAILURE, rc == 0)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return

        if (filecomplete) then
          state(n)%chkfile_nextAdvance = .false.
          state(n)%time_lastrestart = lastrestart
          if (is_root_pe()) then
            call log_restart_fh(currTime-cf(n)%fhoffset, startTime, 'mom6.'//chour, prefixtime=.true., &
                 lastrestart=state(n)%time_lastrestart, lastoutput=state(n)%filename, rc=rc)
            if (ChkErr(rc,__LINE__,u_FILE_u)) return
          endif
        endif
      endif
      if (debug_onroot) call debug_info(trim(subname)//'  ',trim(state(n)%filename), &
           state(n)%chkfile_nextAdvance, state(n)%createsize, importexport)

      if (lstop) then
        ! use prevRing in place of currTime to allow for stopping between averaging intervals
        ! prevring == currTime if stopping on intervals
        call ESMF_AlarmGet(cf(n)%alarm, prevRingTime=prevring, rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return

        timestr = get_timestr(prevring-30*freq(n)*tincrement, rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        state(n)%filename = trim(outputdir)//trim(cf(n)%fnameprefix)//trim(timestr)//'.nc' &
             //trim(cf(n)%fnamesuffix)

        call get_file_state(mpicomm, is_root_pe(), root_pe(), state(n)%filename, nlen=nlen, &
             fsize=fsize, rc=rc)
        rc = merge(ESMF_SUCCESS, ESMF_FAILURE, rc == 0)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return

        filecomplete = file_is_complete(mpicomm, is_root_pe(), root_pe(), state(n)%filename, &
             state(n)%use_filesize, state(n)%createsize, rc)
        rc = merge(ESMF_SUCCESS, ESMF_FAILURE, rc == 0)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return

        if (filecomplete) then
          state(n)%chkfile_nextAdvance = .false.
          state(n)%time_lastrestart = lastrestart
          if (is_root_pe()) then
            call log_restart_fh(prevring, startTime, 'mom6.lstop.'//chour, prefixtime=.true., &
                 lastrestart=state(n)%time_lastrestart, lastoutput=state(n)%filename, rc=rc)
            if (ChkErr(rc,__LINE__,u_FILE_u)) return
          endif
        endif
        if (debug_onroot) call debug_info(trim(subname)//' lstop ',trim(state(n)%filename), &
             state(n)%chkfile_nextAdvance, state(n)%createsize, importexport)
      endif ! lstop
    endif ! output requested
  enddo
end subroutine outputlog_run

!> Check all restart files to determine if output has been completed
!!
!! @param[in]    clock            an ESMF_Clock object
!! @param[in]    num_rest_files   the number of restart files
!! @param[out]   rc               return code
subroutine outputlog_restart(mclock, num_rest_files, rc)
  type(ESMF_Clock)     :: mclock
  integer, intent(in)  :: num_rest_files
  integer, intent(out) :: rc

  ! local variables
  type(ESMF_Time)      :: startTime, currTime, nextTime
  integer              :: n, nlen
  integer              :: year, month, day, hour, minute, seconds
  character(len=256)   :: fname
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
    call get_file_state(mpicomm, is_root_pe(), root_pe(), fname, nlen=nlen, rc=rc)
    rc = merge(ESMF_SUCCESS, ESMF_FAILURE, rc == 0)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    if (nlen > 0) allDone(n) = .true.
    if (debug_onroot) then
      if (nlen > 0) then
        print '(A)',trim(subname)//' restart '//trim(fname)//'  '//trim(importexport)//' complete'
      else
        print '(A)',trim(subname)//' restart '//trim(fname)//'  '//trim(importexport)//' still 0'
      endif
    endif
  enddo ! num_rest_files

  if (all(allDone) .eqv. .true.) then
    lastrestart = nextTime
    if (is_root_pe()) then
      call log_restart_fh(nextTime, startTime, 'mom6.res', prefixtime=.true., rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
    endif
  endif
end subroutine outputlog_restart
#endif
end module MOM_cap_outputlog
