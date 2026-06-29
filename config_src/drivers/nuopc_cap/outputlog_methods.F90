! This file is part of MOM6, the Modular Ocean Model version 6.
! See the LICENSE file for licensing information.
! SPDX-License-Identifier: Apache-2.0

!> This module contains a set of subroutines that are required by the UFS
!> outputlog feature

module outputlog_methods

use ESMF,              only : ESMF_Alarm, ESMF_TimeInterval
use ESMF,              only : ESMF_SUCCESS, ESMF_Failure, ESMF_Time, ESMF_TimeGet
use MOM_coms_infra,    only : root_pe
use MOM_error_handler, only : is_root_pe, MOM_error, FATAL
use MOM_cap_methods,   only : ChkErr
use mpi_f08,           only : MPI_Comm, MPI_INTEGER, MPI_SUCCESS
use netcdf

implicit none; private

type :: outputlog_type
  character(len=14)       :: alarm_name
  integer                 :: opt_n
  logical                 :: requested
  character(len=7)        :: type
  character(len=12)       :: fnameroot
  logical                 :: chkfile_nextAdvance
  logical                 :: use_filesize
  character(len=256)      :: filename
  integer                 :: createsize
  type(ESMF_Alarm)        :: alarm
  type(ESMF_TimeInterval) :: fhoffset
  type(ESMF_TimeInterval) :: filename_fhoffset
  type(ESMF_Time)         :: time_lastrestart
end type outputlog_type

character(len=*), parameter :: u_FILE_u = &
     __FILE__

public :: file_is_complete, get_unlimited_len, get_timestr, get_importexport
public :: readnml, debug_info, nf90_err
public :: outputlog_type

contains

!> Read nml options to configure output logging
!!
!! @param[in]     fname    input namelist file
!! @param[inout]  cf       outputlog configuration
!! @param[out]    debug    logical flag to enable debug output
!! @param[out]    errmsg   error message
!! @param[out]    rc       return code
subroutine readnml(fname, cf, debug, errmsg, rc)

  character(len=*),     intent(in)    :: fname
  type(outputlog_type), intent(inout) :: cf(:)
  logical,              intent(out)   :: debug
  character(len=*),     intent(out)   :: errmsg
  integer,              intent(out)   :: rc

  integer :: n, nn, nfreq, iounit, ierr
  logical :: existflag, output_debug

  integer, allocatable :: output_fh(:)
  character(len=7), allocatable :: output_type(:)
  character(len=24), allocatable :: output_rootname(:)

  namelist / MOM_outputlog_nml/ output_fh, output_rootname, output_type, output_debug

  rc = 0
  errmsg = ''
  nfreq = size(cf)
  allocate(output_fh(1:nfreq), source = 0)
  allocate(output_type(1:nfreq), source = cf(1:nfreq)%type)
  allocate(output_rootname(1:nfreq), source = cf(1:nfreq)%fnameroot)
  output_debug = .false.

  inquire(file=trim(fname), exist=existflag)
  if (.not. existflag) then
    write (errmsg, '(a)') 'FATAL ERROR: input file '//trim(fname)//' does not exist'
    rc = -1
    return
  else
    open (action='read', file=trim(fname), iostat=ierr, newunit=iounit)
    read (nml=MOM_outputlog_nml, iostat=ierr, unit=iounit)
    close (iounit)
    if (ierr /= 0) then
      cf(:)%requested = .false.
      write (errmsg, '(a)') ' MOM output logging disabled '
      return
    endif
  endif

  cf%requested = setrequest(cf%opt_n, output_fh, errmsg, ierr)
  if (ierr /= 0) return

  cf%type = settype(cf%opt_n, cf%requested, output_type, errmsg, ierr)
  if (ierr /= 0) return

  cf%fnameroot = setrootname(cf%opt_n, cf%requested, output_rootname, errmsg, ierr)
  if (ierr /= 0) return

end subroutine readnml
!> TODO: doxy
logical function setrequest(validfreqs, requested_fh, errmsg, ierr) result(is_requested)
  integer,          intent(in)  :: validfreqs(:)
  integer,          intent(in)  :: requested_fh(:)
  character(len=*), intent(out) :: errmsg
  integer,          intent(out) :: ierr

  integer :: n, nfreq, reqval
  logical :: is_requested(size(validfreqs))

  ierr = 0
  errmsg = ''
  nfreq = size(validfreqs)
  is_requested = .false.

  do n = 1,nfreq
    reqval = requested_fh(n)
    if (reqval /= 0) then
      if (.not. any(validfreqs == reqval)) then
        ierr = 1
        write(errmsg, '(A, I0)') "MOM_outputlog: Unsupported output frequency requested: ", reqval
        return
      endif
    endif
  enddo

  do n = 1, nfreq
    if (any(requested_fh == validfreqs(n))) then
      is_requested(n) = .true.
    endif
  enddo
end function setrequest
!> TODO: doxy
function settype(validfreqs, requested, output_type, errmsg, ierr) result(requested_types)

  integer,          intent(in)  :: validfreqs(:)
  logical,          intent(in)  :: requested(:)
  character(len=*), intent(in)  :: output_type(:)
  character(len=*), intent(out) :: errmsg
  integer,          intent(out) :: ierr

  character(len=7) :: requested_types(size(validfreqs))

  integer :: n, nfreq
  character(len=7) :: reqval

  nfreq = size(validfreqs)
  ierr = 0
  errmsg = ''
  requested_types = ''

  do n = 1, nfreq
    reqval = trim(adjustl(output_type(n)))
    if (reqval /= '') then
      if (reqval /= 'average' .and. reqval /= 'none') then
        ierr = 1
        errmsg = "MOM_outputlog: Invalid output_type '"// trim(reqval)// "'. Must be exactly 'average' or 'none'"
        return
      endif
    endif
  enddo

  do n = 1, nfreq
    if (requested(n)) then
      reqval = trim(adjustl(output_type(n)))
      if (reqval == 'average') then
        requested_types(n) = 'average'
      elseif (reqval == 'none') then
        requested_types(n) = 'none'
      else
        requested_types(n) = 'average'
      endif
    else
      requested_types(n) = ''
    endif
  enddo
end function settype
!> TODO: doxy
function setrootname(validfreqs, requested, output_rootname, errmsg, ierr) result(rootnames)

  integer,          intent(in)  :: validfreqs(:)
  logical,          intent(in)  :: requested(:)
  character(len=*), intent(in)  :: output_rootname(:)
  character(len=*), intent(out) :: errmsg
  integer,          intent(out) :: ierr

  character(len=12) :: rootnames(size(validfreqs))

  integer :: n, m, nfreq, n_customroots, custom_idx
  character(len=12) :: tmpname

  nfreq = size(validfreqs)
  ierr = 0
  errmsg = ''
  rootnames = ''

  ! 1. Count how many custom non-blank strings the user provided in the namelist
  n_customroots = 0
  do n = 1, nfreq
    if (trim(output_rootname(n)) /= '') then
      n_customroots = n_customroots + 1
    endif
  enddo

  ! 2. Map custom names or construct defaults for active frequencies
  custom_idx = 1
  do n = 1, nfreq
    if (requested(n)) then
      if (custom_idx <= n_customroots) then
        ! Assign user's custom string sequentially to active slots
        rootnames(n) = trim(adjustl(output_rootname(custom_idx)))
        custom_idx = custom_idx + 1
      else
        ! Fallback Default: "ocn_" + 2-digit integer frequency
        write(rootnames(n), '(A, I2.2)') "ocn_", validfreqs(n)
      endif
    else
      ! Inactive slots remain strictly empty strings
      rootnames(n) = ''
    endif
  enddo

  ! --- GUARD: Check for ambiguous (duplicate) filenames among active slots ---
  do n = 1, nfreq
    if (requested(n)) then
      tmpname = rootnames(n)
      do m = n + 1, nfreq
        if (requested(m)) then
          if (tmpname == rootnames(m)) then
            ierr = 1
            errmsg = "MOM_outputlog: Ambiguous fname_root '" // trim(tmpname) // &
                 "'. Multiple active output streams cannot share the same filename."
            return
          endif
        endif
      enddo
    endif
  enddo
end function setrootname

!> Determine if the netcdf output file is complete
!!
!! @param[in]   comm          the MPI communicator
!! @param[in]   fname         the file name
!! @param[in]   chk4size      logical flag for check method in use
!! @param[in]   createsize    the filesize at creation
!! @param[out]  rc            return code
!! @return                    logical flag, true if the file is complete
logical function file_is_complete(comm, fname, chk4size, createsize, rc) result(filecomplete)

  type(MPI_Comm),   intent(in)  :: comm
  character(len=*), intent(in)  :: fname
  logical,          intent(in)  :: chk4size
  integer,          intent(in)  :: createsize
  integer,          intent(out) :: rc

  logical :: existflag
  integer :: nlen(1), fsize(1), ierr
  !----------------------------------------------------------------------------

  rc = 0

  filecomplete = .false.
  nlen(1) = nf90_fill_int
  fsize(1) = nf90_fill_int

  if (is_root_pe()) then
    inquire(file=fname, exist=existflag)
    if (existflag) then
      nlen(1) = get_unlimited_len(trim(fname))
      inquire(file=fname, size=fsize(1))
    endif
  endif

  call MPI_Bcast(nlen, 1, MPI_INTEGER, root_pe(), comm, ierr)
  if (ierr /= MPI_SUCCESS) then
    rc = -1
    return
  endif
  call MPI_Bcast(fsize, 1, MPI_INTEGER, root_pe(), comm, ierr)
  if (ierr /= MPI_SUCCESS) then
    rc = -1
    return
  endif

  if (chk4size) then
    filecomplete = (nlen(1) > 0 .and. fsize(1) > createsize)
  else
    filecomplete = (nlen(1) > 0)
  endif
end function file_is_complete

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
function get_timestr(MyTime, rc) result(timestr)
  type(ESMF_Time), intent(in)  :: MyTime
  integer,         intent(out) :: rc

  character(len=16) :: timestr
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
function get_importexport(currTime, nextTime, rc) result(importexport)

  type(ESMF_Time), intent(in)  :: currTime, nextTime
  integer,         intent(out) :: rc

  character(len=19) :: import_timestr, export_timestr
  character(len=40) :: importexport
  !----------------------------------------------------------------------------

  rc = ESMF_SUCCESS

  call ESMF_TimeGet(currTime, timestring=import_timestr, rc=rc)
  if (ChkErr(rc,__LINE__,u_FILE_u)) return
  call ESMF_TimeGet(nextTime, timestring=export_timestr, rc=rc)
  if (ChkErr(rc,__LINE__,u_FILE_u)) return
  importexport = trim(import_timestr)//'  '//trim(export_timestr)
end function get_importexport

!> Write debug info to stdout, only called on root pe
!!
!! @param[in]    tag            an information tag
!! @param[in]    fname          the filename to check
!! @param[in]    filesize       the filesize at creation time
!! @param[in]    chkflag        logical flag for checking next Advance
!! @param[in]    timestring     a timestring
subroutine debug_info(tag,fname,chkflag,filesize,timestring)
  character(len=*), intent(in) :: tag
  character(len=*), intent(in) :: fname
  integer,          intent(in) :: filesize
  logical,          intent(in) :: chkflag
  character(len=*), intent(in) :: timestring

  logical :: existflag
  integer :: fsize
  character(len=256) :: msgString
  !----------------------------------------------------------------------------

  inquire(file=fname, exist=existflag)
  if (existflag) then
    inquire(file=fname, size=fsize)
    write(msgString,'(A)')tag//'  '//fname//' exists '//timestring
    if (chkflag) then
      print '(A,L,2i16)',trim(msgString)//' not complete, chkflag ',chkflag,filesize,fsize
    else
      print '(A,L,2i16)',trim(msgString)//'     complete, chkflag ',chkflag,filesize,fsize
    endif
  else
    write(msgString,'(A)')tag//'  '//fname//' does not exist '//timestring
    print '(A)',trim(msgString)
  endif
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
    ! This fails on WCOSS2 with Intel 19 compiler. See https://community.intel.com/
    ! Search term "STOP and ERROR STOP with variable stop codes"
    ! When WCOSS2 moves to Intel 2020+, uncomment the next line and remove stop 99
    !stop ierr
    stop 99
  endif
end subroutine nf90_err
end module outputlog_methods
