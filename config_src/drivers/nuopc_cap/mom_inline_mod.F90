module mom_inline_mod

  use ESMF             , only: ESMF_GridComp, ESMF_Mesh
  use ESMF             , only: ESMF_Clock, ESMF_Time, ESMF_TimeGet, ESMF_ClockGet
  use ESMF             , only: ESMF_KIND_R8, ESMF_SUCCESS, ESMF_LogFoundError
  use ESMF             , only: ESMF_LOGERR_PASSTHRU, ESMF_LOGMSG_INFO, ESMF_LOGWRITE
  use ESMF             , only: ESMF_MAXSTR
  use dshr_mod         , only: dshr_pio_init
  use dshr_strdata_mod , only: shr_strdata_type, shr_strdata_print
  use dshr_strdata_mod , only: shr_strdata_init_from_inline
  use dshr_strdata_mod , only: shr_strdata_advance
  use dshr_methods_mod , only: dshr_fldbun_getfldptr, dshr_fldbun_Field_diagnose
  use dshr_stream_mod  , only: shr_stream_init_from_esmfconfig
  use MOM_cap_methods  , only: ChkErr

  implicit none
  private

  public mom_inline_init
  public mom_inline_run

  integer :: logunit   ! the logunit on the root task
  ! available streams
  type(shr_strdata_type) :: sdat_lrunoff
  type(shr_strdata_type) :: sdat_frunoff

  character(len=*), parameter :: u_FILE_u =  __FILE__
contains
  !===============================================================================
  subroutine mom_inline_init(gcomp, model_clock, model_mesh, mytask, streamconfigfile, rc)

    ! input/output parameters
    type(ESMF_GridComp)    , intent(in)  :: gcomp
    type(ESMF_Clock)       , intent(in)  :: model_clock
    type(ESMF_Mesh)        , intent(in)  :: model_mesh
    integer                , intent(in)  :: mytask
    character(len=*)       , intent(in)  :: streamconfigfile
    integer                , intent(out) :: rc

    ! stream data from config (xml or esmf), one or more streams
    type(shr_strdata_type) :: sdat

    integer :: id_lrunoff=0
    integer :: id_frunoff=0

    integer :: ns, nv
    integer :: nstreams, nvars

    character(len=*), parameter  :: subname='(mom_inline_init)'
    !----------------------------------------------------------------------

    rc = ESMF_SUCCESS

    if (mytask == 0) then
       open (newunit=logunit, file='log.mom6.cdeps')
    else
       logunit = 6
    end if

#ifndef CESMCOUPLED
    ! CMEPS Init PIO
    call dshr_pio_init(gcomp, sdat, logunit, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    ! read the available stream definitions, each data stream is one or more data_files
    ! which have the same spatial and temporal coordinates
    ! returns sdat%stream, of type shr_stream_streamType
    call shr_stream_init_from_esmfconfig(streamconfigfile, sdat%stream, logunit, &
         sdat%pio_subsystem, sdat%io_type, sdat%io_format, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
#else
    !do cesm stuff...point to shr, use xml
#endif
    ! set the model clock and mesh
    sdat%model_clock = model_clock
    sdat%model_mesh = model_mesh

    nstreams = size(sdat%stream)
    ! locate the individual stream data
    do ns = 1,nstreams
       nvars = sdat%stream(ns)%nvars
       do nv = 1,nvars
          if (trim(sdat%stream(ns)%varlist(nv)%nameinmodel) == 'lrunoff') id_lrunoff = ns
          if (trim(sdat%stream(ns)%varlist(nv)%nameinmodel) == 'frunoff') id_frunoff = ns
       end do
    end do

    if (id_lrunoff /= 0) then
       call initialize_single_stream(sdat, sdat_lrunoff, id_lrunoff, 'lrunoff', 'OCN', mytask, logunit, rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
    end if
    if (id_frunoff /= 0) then
       call initialize_single_stream(sdat, sdat_frunoff, id_frunoff, 'frunoff', 'OCN', mytask, logunit, rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
    end if

  end subroutine mom_inline_init
!===============================================================================

  !call mom_inline_run(clock, isc, iec, jsc, jec, 'lrunoff', ice_ocean_boundary%lrunoff, rc=rc)
  subroutine mom_inline_run(clock, isc, iec, jsc, jec, fldname, output, rc)

    ! input/output variables
    type(ESMF_Clock) ,    intent(in)    :: clock
    integer ,             intent(in)    :: isc                     !< The start i-index of cell centers within
                                                                   !! the computational domain
    integer ,             intent(in)    :: iec                     !< The end i-index of cell centers within the
                                                                   !! computational domain
    integer ,             intent(in)    :: jsc                     !< The start j-index of cell centers within
                                                                   !! the computational domain
    integer ,             intent(in)    :: jec                     !< The end j-index of cell centers within
                                                                   !! the computational domain
    character(len=*) ,    intent(in)    :: fldname                 !! the stream name
    real (ESMF_KIND_R8) , intent(inout) :: output(isc:iec,jsc:jec) !< Output 2D array

    integer ,             intent(out)   :: rc

    ! local variables
    type(ESMF_Time)             :: date
    integer                     :: i,j,n
    integer                     :: year    ! year (0, ...) for nstep+1
    integer                     :: mon     ! month (1, ..., 12) for nstep+1
    integer                     :: day     ! day of month (1, ..., 31) for nstep+1
    integer                     :: sec     ! seconds into current date for nstep+1
    integer                     :: mcdate  ! Current model date (yyyymmdd)
    real(ESMF_KIND_R8), pointer :: dataPtr1d(:)
    !-----------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! Advance sdat stream
    call ESMF_ClockGet( clock, currTime=date, rc=rc )
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TimeGet(date, yy=year, mm=mon, dd=day, s=sec, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    mcdate = year*10000 + mon*100 + day

    if (trim(fldname) == 'lrunoff') then
       call shr_strdata_advance(sdat_lrunoff, ymd=mcdate, tod=sec, logunit=logunit, istr='lrunoff stream',rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return

       ! Get pointer for stream data that is time and spatially interpolated to model time and grid
       call dshr_fldbun_getFldPtr(sdat_lrunoff%pstrm(1)%fldbun_model, trim(fldname), dataPtr1d, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return

       call dshr_fldbun_Field_diagnose(sdat_lrunoff%pstrm(1)%fldbun_model, trim(fldname), rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
    end if
    if (trim(fldname) == 'frunoff') then
       call shr_strdata_advance(sdat_frunoff, ymd=mcdate, tod=sec, logunit=logunit, istr='frunoff stream', rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return

       ! Get pointer for stream data that is time and spatially interpolated to model time and grid
       call dshr_fldbun_getFldPtr(sdat_frunoff%pstrm(1)%fldbun_model, trim(fldname), dataPtr1d, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return

       call dshr_fldbun_Field_diagnose(sdat_frunoff%pstrm(1)%fldbun_model, trim(fldname), rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
    end if

    n = 0
    do j = jsc,jec
       do i = isc,iec
          n = n + 1
          output(i,j)  = dataPtr1d(n)
       end do
    end do

  end subroutine mom_inline_run
  !===============================================================================

  subroutine initialize_single_stream(sdatm, sdats, sid, sdatname, compname, mytask, logunit, rc)

    type(shr_strdata_type), intent(inout) :: sdatm
    type(shr_strdata_type), intent(inout) :: sdats
    integer,                intent(in)    :: sid
    character(len=*),       intent(in)    :: sdatname
    character(len=*),       intent(in)    :: compname
    integer,                intent(in)    :: mytask
    integer,                intent(in)    :: logunit
    integer ,               intent(out)   :: rc

    ! local
    integer :: nfiles, nvars, nf, nv
    character(len=ESMF_MAXSTR), allocatable :: filelist(:)
    character(len=ESMF_MAXSTR), allocatable :: filevars(:,:)

    !-----------------------------------------------------------------------

    rc = ESMF_SUCCESS

    nfiles = sdatm%stream(sid)%nfiles
    nvars = sdatm%stream(sid)%nvars

    allocate(filelist(1:nfiles))
    allocate(filevars(1:nvars,2))

    do nf = 1,nfiles
       filelist(nf) = trim(sdatm%stream(sid)%file(nf)%name)
    end do
    do nv = 1,nvars
       filevars(nv,1) = trim(sdatm%stream(sid)%varlist(nv)%nameinfile)
       filevars(nv,2) = trim(sdatm%stream(sid)%varlist(nv)%nameinmodel)
    end do

    ! Write out info
    if (mytask == 0) then
       write(logunit,'(a)'   ) ' '
       write(logunit,'(a,i8)')  'stream settings:',sid
       do nf = 1,nfiles
          write(logunit,'(a)' )  '  stream file list = '//trim(filelist(nf))
       end do
       do nv = 1,nvars
          write(logunit,'(a)' )  '  stream variable in file= '//trim(filevars(nv,1))//' ; variable in model= '//trim(filevars(nv,2))
          write(logunit,'(a)' )  ' '
       end do
    endif

    ! Set PIO related variables
    sdats%pio_subsystem => sdatm%pio_subsystem
    sdats%io_type = sdatm%io_type
    sdats%io_format = sdatm%io_format

    call shr_strdata_init_from_inline(sdats,                        &
         my_task             = mytask,                              &
         logunit             = logunit,                             &
         compname            = trim(compname),                      &
         model_clock         = sdatm%model_clock,                   &
         model_mesh          = sdatm%model_mesh,                    &
         stream_name         = trim(sdatname),                      &
         stream_meshfile     = trim(sdatm%stream(sid)%meshFile),    &
         stream_filenames    = filelist,                            &
         stream_yearFirst    = sdatm%stream(sid)%yearFirst,         &
         stream_yearLast     = sdatm%stream(sid)%yearLast,          &
         stream_yearAlign    = sdatm%stream(sid)%yearAlign,         &
         stream_fldlistFile  = filevars(:,1),                       &
         stream_fldListModel = filevars(:,2),                       &
         stream_lev_dimname  = trim(sdatm%stream(sid)%lev_dimname), &
         stream_mapalgo      = trim(sdatm%stream(sid)%mapAlgo),     &
         stream_offset       = sdatm%stream(sid)%offset,            &
         stream_taxmode      = trim(sdatm%stream(sid)%taxmode),     &
         stream_dtlimit      = sdatm%stream(sid)%dtlimit,           &
         stream_tintalgo     = trim(sdatm%stream(sid)%tInterpAlgo), &
         stream_src_mask     = sdatm%stream(sid)%src_mask_val,      &
         stream_dst_mask     = sdatm%stream(sid)%dst_mask_val,      &
         rc                  = rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    deallocate(filelist)
    deallocate(filevars)

  end subroutine initialize_single_stream

end module mom_inline_mod
