module mom_wrapper_mod

  use ESMF          , only : ESMF_KIND_I8, ESMF_KIND_R8
#ifdef CESMCOUPLED
  use perf_mod      , only : t_startf, t_stopf, t_barrierf
  use shr_file_mod  , only : shr_file_getlogunit, shr_file_setlogunit
  use shr_log_mod   , only : shr_log_setLogUnit

  implicit none

contains
  ! Define stub routines that do nothing - they are just here to avoid
  ! having cppdefs in the main program
  subroutine ufs_settimer(timevalue)
    real(ESMF_KIND_R8),    intent(inout) :: timevalue
  end subroutine ufs_settimer
  subroutine ufs_logtimer(nunit,esecs,string,time0)
    integer,               intent(in)    :: nunit
    integer(ESMF_KIND_I8), intent(in)    :: esecs
    character(len=*),      intent(in)    :: string
    real(ESMF_KIND_R8),    intent(in)    :: time0
  end subroutine ufs_logtimer
  subroutine ufs_file_setLogUnit(filename,nunit)
    character(len=*),      intent(in)    :: filename
    integer,               intent(out)   :: nunit
  end subroutine ufs_file_setLogUnit
  subroutine ufs_logfhour(date,esecs,hour)
    integer,               intent(in)    :: date
    integer(ESMF_KIND_I8), intent(in)    :: esecs
    real,                  intent(in)    :: hour
  end subroutine ufs_logfhour
#else

  implicit none

  real :: wtime = 0.0
contains
  subroutine ufs_settimer(timevalue)
    real(ESMF_KIND_R8),    intent(inout) :: timevalue
    real(ESMF_KIND_R8)                   :: MPI_Wtime
    timevalue = MPI_Wtime()
  end subroutine ufs_settimer

  subroutine ufs_logtimer(nunit,esecs,string,time0)
    integer,               intent(in)    :: nunit
    integer(ESMF_KIND_I8), intent(in)    :: esecs
    character(len=*),      intent(in)    :: string
    real(ESMF_KIND_R8),    intent(in)    :: time0
    real(ESMF_KIND_R8)                   :: MPI_Wtime, timevalue
    if (time0 > 0.) then
       timevalue = MPI_Wtime()-time0
       write(nunit,'(i10,A,g12.7)')esecs,' MOM '//trim(string)//'  ',timevalue
    end if
  end subroutine ufs_logtimer

  subroutine ufs_file_setLogUnit(filename,nunit)
    character(len=*),      intent(in)    :: filename
    integer,               intent(out)   :: nunit
    open (newunit=nunit, file=trim(filename))
  end subroutine ufs_file_setLogUnit

  subroutine ufs_logfhour(date,esecs,hour)
    integer,               intent(in)    :: date
    integer(ESMF_KIND_I8), intent(in)    :: esecs
    real,                  intent(in)    :: hour
    character(len=80)                    :: filename
    integer                              :: nunit
    write(filename,'(a,i3.3)')'log.mom.f',int(hour)
    open(newunit=nunit,file=trim(filename))
    write(nunit,'(a,f8.2,a,2i10)')'completed MOM fhour = ',hour,' complete ',date,esecs
    close(nunit)
  end subroutine ufs_logfhour

  ! Define stub routines that do nothing - they are just here to avoid
  ! having cppdefs in the main program
  subroutine shr_log_setLogUnit(nunit)
    integer, intent(in) :: nunit
  end subroutine shr_log_setLogUnit
  subroutine shr_file_setLogUnit(nunit)
    integer, intent(in) :: nunit
  end subroutine shr_file_setLogUnit
  subroutine shr_file_getLogUnit(nunit)
    integer, intent(in) :: nunit
  end subroutine shr_file_getLogUnit
  subroutine t_startf(string)
    character(len=*) :: string
  end subroutine t_startf
  subroutine t_stopf(string)
    character(len=*) :: string
  end subroutine t_stopf
  subroutine t_barrierf(string, comm)
    character(len=*) :: string
    integer:: comm
  end subroutine t_barrierf
#endif

end module mom_wrapper_mod
