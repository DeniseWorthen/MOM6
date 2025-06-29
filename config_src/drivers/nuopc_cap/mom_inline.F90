module mom_inline_mod

  use ESMF             , only: ESMF_GridComp, ESMF_Mesh
  use ESMF             , only: ESMF_Clock, ESMF_Time, ESMF_TimeGet, ESMF_ClockGet
  use ESMF             , only: ESMF_KIND_R8, ESMF_SUCCESS, ESMF_LogFoundError
  use ESMF             , only: ESMF_LOGERR_PASSTHRU, ESMF_LOGMSG_INFO, ESMF_LOGWRITE
  use dshr_mod         , only: dshr_pio_init
  use dshr_strdata_mod , only: shr_strdata_type
  use dshr_strdata_mod , only: shr_strdata_init_from_inline
  use dshr_strdata_mod , only: shr_strdata_advance
  use dshr_methods_mod , only: dshr_fldbun_getfldptr
  use dshr_stream_mod  , only: shr_stream_init_from_esmfconfig
  use MOM_cap_methods  , only: ChkErr

  implicit none
  private

  public mom_inline_init
  public mom_inline_run

  type(shr_strdata_type)          :: sdat    ! stream dat
  ! need array to hold dust input

  character(len=*), parameter :: u_FILE_u =  __FILE__
contains

  subroutine mom_inline_init(gcomp, clock, mesh, logunit, streamconfigfile, rc)

    ! input/output parameters
    type(ESMF_GridComp)    , intent(in)  :: gcomp
    type(ESMF_Clock)       , intent(in)  :: clock
    type(ESMF_Mesh)        , intent(in)  :: mesh
    integer                , intent(in)  :: logunit
    character(len=*)       , intent(in)  :: streamconfigfile
    integer                , intent(out) :: rc

    ! CMEPS Init PIO
    call dshr_pio_init(gcomp, sdat, logunit, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    ! Read stream configuration file
    ! TODO: At this point it only suports ESMF config format (XML?)
    !streamfilename = 'stream.config'
    call shr_stream_init_from_esmfconfig(streamconfigfile, sdat%stream, logunit, &
         sdat%pio_subsystem, sdat%io_type, sdat%io_format, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    !         ! Write out info
    !       if (mnproc == 1) then
    !          write(lp,'(a)'   ) ' '
    !          write(lp,'(a,i8)')  'stream dust settings:'
    !          do nf = 1,nfiles
    !             write(tmpchar,'(i0)') nf
    !             write(lp,'(a,a)' )  '  stream_dust_data_filename('//trim(tmpchar)//') = ',trim(stream_filenames(nf)\
    ! )
    !          end do
    !          write(lp,'(a,a)' )  '  stream_dust_mesh_filename = ',trim(stream_dust_mesh_filename)
    !          write(lp,'(a,a,a)') '  stream_dust_varnames      = ',trim(stream_dust_varnames)
    !          write(lp,'(a,i8)')  '  stream_dust_year_first    = ',stream_dust_year_first
    !          write(lp,'(a,i8)')  '  stream_dust_year_last     = ',stream_dust_year_last
    !          write(lp,'(a,i8)')  '  stream_dust_year_align    = ',stream_dust_year_align
    !          write(lp,'(a)'   )  ' '
    !       endif

    !       ! allocate field to hold dust fields
    !       allocate(dust_stream(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,ndust), stat=errstat)
    !       if (errstat /= 0) then
    !          stop 'not enough memory for dust_stream'
    !       end if
    !       dust_stream(:,:,:) = 0.0

  end subroutine mom_inline_init

  subroutine mom_inline_run(clock, logunit, rc)

    ! input/output variables
    type(ESMF_Clock), intent(in)  :: clock
    integer         , intent(in)  :: logunit
    integer         , intent(out) :: rc

    ! local variables
    type(ESMF_Time)     :: date
    integer             :: i,j,n,nfld
    integer             :: jjcpl
    integer             :: year    ! year (0, ...) for nstep+1
    integer             :: mon     ! month (1, ..., 12) for nstep+1
    integer             :: day     ! day of month (1, ..., 31) for nstep+1
    integer             :: sec     ! seconds into current date for nstep+1
    integer             :: mcdate  ! Current model date (yyyymmdd)
    real(ESMF_KIND_R8), pointer   :: dataptr1(:)
    real(ESMF_KIND_R8), parameter :: mval = -1.e12_ESMF_KIND_R8
    real(ESMF_KIND_R8), parameter :: fval = -1.e13_ESMF_KIND_R8
    !-----------------------------------------------------------------------

    ! Advance sdat stream
    call ESMF_ClockGet( clock, currTime=date, rc=rc )
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TimeGet(date, yy=year, mm=mon, dd=day, s=sec, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    mcdate = year*10000 + mon*100 + day

    call shr_strdata_advance(sdat, ymd=mcdate, tod=sec, logunit=logunit, istr='dust', rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    ! ! Set to unreasonable value to catch errors
    ! dust_stream(:,:,:) = 1.e30
    ! do nfld = 1, size(stream_varnames)

    !    ! Get pointer for stream data that is time and spatially interpolated to model time and grid
    !    call dshr_fldbun_getFldPtr(sdat_dust%pstrm(1)%fldbun_model, stream_varnames(nfld), fldptr1=dataptr1, rc=rc)
    !    if (chkerr(rc,__LINE__,u_FILE_u)) return

    !    ! fill internal MOM variable w/ fldptr
    ! end do

  end subroutine mom_inline_run

end module mom_inline_mod
