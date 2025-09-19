module ufs_trace_mod

  implicit none
  public ufs_trace_init
  public ufs_trace
  public ufs_trace_finalize

contains

  subroutine ufs_trace_init()
  end subroutine ufs_trace_init

  subroutine ufs_trace(component, routine, ph)
    character(len=*), intent(in) :: component, routine, ph
  end subroutine ufs_trace

  subroutine ufs_trace_finalize()
  end subroutine ufs_trace_finalize

end module ufs_trace_mod
