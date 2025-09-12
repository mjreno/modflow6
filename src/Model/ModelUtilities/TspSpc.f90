!> @brief This module contains the TspSpc Module
!!
!! This module contains the code for reading and storing a
!! generic input file of source and sink concentrations or
!! temperatures.
!<
module TspSpcModule

  use KindModule, only: DP, LGP, I4B
  use ConstantsModule, only: LENPACKAGENAME, LENMODELNAME, &
                             LENMEMPATH, DZERO, LENFTYPE, &
                             LINELENGTH, TABLEFT, TABCENTER, &
                             LENVARNAME
  use SimVariablesModule, only: errmsg
  use SimModule, only: store_error, count_errors, store_error_filename
  use MemoryHelperModule, only: create_mem_path
  use BaseDisModule, only: DisBaseType
  use TableModule, only: TableType, table_cr

  implicit none
  private
  public :: TspSpcType

  character(len=LENFTYPE) :: ftype = 'SPC'
  character(len=LENPACKAGENAME) :: text = 'STRESS PACK COMP'

  !> @brief Derived type for managing SPC input
  !!
  !! This derived type will read and process an SPC input file
  !! and provide concentrations or temperatures to the SSM package
  !! that correspond to an individual GWF stress package.
  !<
  type :: TspSpcType

    character(len=LENMODELNAME) :: name_model = '' !< the name of the model that contains this package
    character(len=LENPACKAGENAME) :: packName = '' !< name of the package
    character(len=LENPACKAGENAME) :: packNameFlow = '' !< name of the corresponding flow package
    character(len=LENVARNAME) :: depvarname = '' !< name of the dependent variable (CONCENTRATION or TEMPERATURE)
    character(len=LENMEMPATH) :: memoryPath = '' !< the location in the memory manager where the variables are stored
    character(len=LENMEMPATH) :: input_mempath = '' !< input context mempath
    character(len=LINELENGTH), pointer :: input_fname => null() !< input file name
    integer(I4B), pointer :: id => null() !< id number for this spc package
    integer(I4B), pointer :: inunit => null() !< unit number for input
    integer(I4B), pointer :: iout => null() !< unit number for output
    integer(I4B), pointer :: maxbound => null() !< length of dblvec
    integer(I4B), pointer :: lastiper => null() !< last loaded value of iper (for checking)
    integer(I4B), pointer :: iprpak => null() !< flag for printing input
    logical(LGP), pointer :: readasarrays => null() !< flag for reading concentrations as an array
    real(DP), dimension(:), pointer, contiguous :: dblvec => null() !< vector of floats read from file
    class(DisBaseType), pointer :: dis => null() !< model discretization object
    type(TableType), pointer :: inputtab => null() !< input table object

  contains

    procedure :: initialize
    procedure :: allocate_scalars
    procedure :: source_options
    procedure :: source_dimensions
    procedure :: allocate_arrays
    procedure :: get_value
    procedure :: spc_rp
    procedure :: spc_rp_list
    procedure :: spc_ad
    procedure :: spc_da
    procedure :: check_flow_package

  end type TspSpcType

contains

  !> @ brief Initialize the SPC type
  !!
  !! Initialize the SPC object by reading options
  !! and dimensions and allocating memory.
  !!
  !<
  subroutine initialize(this, dis, id, iout, name_model, packNameFlow, &
                        dvn, input_mempath)
    ! -- dummy variables
    class(TspSpcType) :: this !<  TspSpcType
    class(DisBaseType), pointer, intent(in) :: dis !<  discretization package
    integer(I4B), intent(in) :: id !<  id number for this spc package
    integer(I4B), intent(in) :: iout !<  unit number for output
    character(len=*), intent(in) :: name_model !<  character string containing model name
    character(len=*), intent(in) :: packNameflow !<  character string containing name of corresponding flow package
    character(len=*), intent(in) :: dvn !<  dependent variable name (CONCENTRATION or TEMPERATURE)
    character(len=*), intent(in) :: input_mempath
    ! -- local
    !
    ! -- construct the memory path
    write (this%packName, '(a, i0)') 'SPC'//'-', id
    this%name_model = name_model
    this%memoryPath = create_mem_path(this%name_model, this%packName)
    this%input_mempath = input_mempath
    !
    ! -- allocate scalar variables
    call this%allocate_scalars()
    !
    ! -- assign member values
    this%id = id
    this%inunit = 1
    this%iout = iout
    this%packNameFlow = packNameFlow
    this%depvarname = dvn
    !
    ! -- set pointers
    this%dis => dis
    !
    ! -- read options
    call this%source_options()
    !
    ! -- read dimensions
    if (this%readasarrays) then
      this%maxbound = this%dis%get_ncpl()
    else
      call this%source_dimensions()
    end if
    !
    ! -- allocate arrays
    call this%allocate_arrays()
  end subroutine initialize

  !> @ brief Allocate package scalars
  !!
  !!  Allocate and initialize package scalars.
  !!
  !<
  subroutine allocate_scalars(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    use MemoryManagerExtModule, only: mem_set_value
    ! -- dummy variables
    class(TspSpcType) :: this !< TspSpcType object
    logical(LGP) :: found
    !
    ! -- allocate scalars in memory manager
    call mem_allocate(this%input_fname, LINELENGTH, 'INPUT_FNAME', &
                      this%memoryPath)
    call mem_allocate(this%id, 'ID', this%memoryPath)
    call mem_allocate(this%inunit, 'INUNIT', this%memoryPath)
    call mem_allocate(this%iout, 'IOUT', this%memoryPath)
    call mem_allocate(this%maxbound, 'MAXBOUND', this%memoryPath)
    call mem_allocate(this%lastiper, 'LASTIPER', this%memoryPath)
    call mem_allocate(this%iprpak, 'IPRPAK', this%memoryPath)
    call mem_allocate(this%readasarrays, 'READASARRAYS', this%memoryPath)
    !
    ! -- initialize
    this%id = 0
    this%inunit = 0
    this%iout = 0
    this%maxbound = 0
    this%lastiper = 0
    this%iprpak = 0
    this%readasarrays = .false.

    call mem_set_value(this%input_fname, 'INPUT_FNAME', &
                       this%input_mempath, found)
  end subroutine allocate_scalars

  !> @ brief Source options for package
  !!
  !!  Source options for this package.
  !!
  !<
  subroutine source_options(this)
    ! -- modules
    use MemoryManagerModule, only: get_isize, mem_setptr
    use MemoryManagerExtModule, only: mem_set_value
    use CharacterStringModule, only: CharacterStringType
    ! -- dummy
    class(TspSpcType) :: this
    ! -- local
    type(CharacterStringType), dimension(:), pointer, &
      contiguous :: ts_fnames
    character(len=LINELENGTH) :: fname
    logical(LGP) :: found_iprpak, found_raa
    integer(I4B) :: isize, n
    ! -- formats
    character(len=*), parameter :: fmtiprpak = &
      &"(4x,'SPC INFORMATION WILL BE PRINTED TO LISTING FILE.')"
    character(len=*), parameter :: fmtreadasarrays = &
      "(4x,'SPC INFORMATION WILL BE READ AS ARRAYS RATHER THAN IN LIST &
      &FORMAT.')"
    character(len=*), parameter :: fmtts = &
      &"(4x, 'TIME-SERIES DATA WILL BE READ FROM FILE: ', a)"
    character(len=*), parameter :: fmttas = &
      &"(4x, 'TIME-ARRAY SERIES DATA WILL BE READ FROM FILE: ', a)"

    ! -- source package input
    call mem_set_value(this%iprpak, 'PRINT_INPUT', this%input_mempath, &
                       found_iprpak)
    call mem_set_value(this%readasarrays, 'READASARRAYS', this%input_mempath, &
                       found_raa)

    ! log options
    write (this%iout, '(1x,a)') 'PROCESSING SPC OPTIONS'

    if (found_iprpak) write (this%iout, fmtiprpak)
    if (found_raa) write (this%iout, fmtreadasarrays)

    call get_isize('TS6_FILENAME', this%input_mempath, isize)
    if (isize > 0) then
      call mem_setptr(ts_fnames, 'TS6_FILENAME', this%input_mempath)
      do n = 1, size(ts_fnames)
        fname = ts_fnames(n)
        write (this%iout, fmtts) trim(fname)
      end do
    end if

    call get_isize('TAS6_FILENAME', this%input_mempath, isize)
    if (isize > 0) then
      call mem_setptr(ts_fnames, 'TAS6_FILENAME', this%input_mempath)
      do n = 1, size(ts_fnames)
        fname = ts_fnames(n)
        write (this%iout, fmttas) trim(fname)
      end do
    end if

    write (this%iout, '(1x,a)') 'END OF SPC OPTIONS'
  end subroutine source_options

  !> @ brief Source dimensions for package
  !!
  !!  Source dimensions for this package.
  !!
  !<
  subroutine source_dimensions(this)
    ! -- modules
    use MemoryManagerExtModule, only: mem_set_value
    use CharacterStringModule, only: CharacterStringType
    ! -- dummy
    class(TspSpcType) :: this
    logical(LGP) :: found_maxbound

    call mem_set_value(this%maxbound, 'MAXBOUND', this%input_mempath, &
                       found_maxbound)

    ! check maxbound
    if (this%maxbound <= 0) then
      write (errmsg, '(a)') 'MAXBOUND must be an integer greater than zero.'
      call store_error(errmsg)
      call store_error_filename(this%input_fname)
    end if

    ! log dimensions
    write (this%iout, '(/1x,a)') 'PROCESSING '//trim(adjustl(text))// &
      ' DIMENSIONS'
    write (this%iout, '(4x,a,i7)') 'MAXBOUND = ', this%maxbound
    write (this%iout, '(1x,a)') 'END OF '//trim(adjustl(text))//' DIMENSIONS'
  end subroutine source_dimensions

  !> @ brief Allocate package arrays
  !!
  !!  Allocate and initialize package arrays.
  !!
  !<
  subroutine allocate_arrays(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate, mem_setptr, mem_checkin
    ! -- dummy variables
    class(TspSpcType) :: this !< TspSpcType object
    ! -- local
    integer(I4B) :: i
    !
    if (this%readasarrays) then
      ! -- set DBLVEC input context pointers
      call mem_setptr(this%dblvec, trim(this%depvarname), this%input_mempath)
      !
      ! -- checkin DBLVEC input context pointers
      call mem_checkin(this%dblvec, 'DBLVEC', this%memoryPath, &
                       trim(this%depvarname), this%input_mempath)
    else
      !
      ! -- allocate array
      call mem_allocate(this%dblvec, this%maxbound, 'DBLVEC', this%memoryPath)
      !
      ! -- initialize dblvec to zero
      do i = 1, this%maxbound
        this%dblvec(i) = DZERO
      end do
    end if
  end subroutine allocate_arrays

  !> @ brief Get the data value from this package
  !!
  !!  Get the floating point value from the dblvec array.
  !!
  !<
  function get_value(this, ientry, nbound_flow) result(value)
    class(TspSpcType) :: this !< TspSpcType object
    integer(I4B), intent(in) :: ientry !< index of the data to return
    integer(I4B), intent(in) :: nbound_flow !< size of bound list in flow package
    real(DP) :: value
    integer(I4B) :: nu
    if (this%readasarrays) then
      ! Special handling for reduced grids and readasarrays
      ! if flow and transport are in the same simulation, then
      ! ientry is a user node number and it corresponds to the
      ! correct position in the dblvec array.  But if flow and
      ! transport are not in the same simulation, then ientry is
      ! a reduced node number, because the list of flows in the
      ! budget file do not include idomain < 1 entries. In this
      ! case, ientry must be converted to a user node number so
      ! that it corresponds to a user array, which includes
      ! idomain < 1 values.
      if (nbound_flow == this%maxbound) then
        ! flow and transport are in the same simulation or there
        ! are no idomain < 1 cells.
        value = this%dblvec(ientry)
      else
        ! This identifies case where flow and transport must be
        ! in a separate simulation, because nbound_flow is not
        ! the same as this%maxbound.  Under these conditions, we
        ! must assume that ientry corresponds to a flow list that
        ! would be of size ncpl if flow and transport were in the
        ! same simulation, but because boundary cells with
        ! idomain < 1 are not written to binary budget file, the
        ! list size is smaller.
        nu = this%dis%get_nodeuser(ientry)
        value = this%dblvec(nu)
      end if
    else
      value = this%dblvec(ientry)
    end if
  end function get_value

  !> @ brief Read and prepare
  !!
  !!  Read and prepare the period data block and fill dblvec
  !!  if the next period block corresponds to this time step.
  !!
  !<
  subroutine spc_rp(this)
    ! -- modules
    use TdisModule, only: kper
    use MemoryManagerModule, only: mem_setptr
    use CharacterStringModule, only: CharacterStringType
    ! -- dummy
    class(TspSpcType), intent(inout) :: this !< TspSpcType object
    ! -- local
    integer(I4B), pointer :: iper
    ! -- formats
    character(len=*), parameter :: fmtlsp = &
      &"(1X,/1X,'REUSING ',A,'S FROM LAST STRESS PERIOD')"
    !
    ! return if package not active
    if (this%inunit == 0) return

    ! set pointer to last and next period loaded
    call mem_setptr(iper, 'IPER', this%input_mempath)

    ! return if rp already executed for iper
    if (iper == this%lastiper) return

    if (iper == kper) then
      if (this%readasarrays) then
        ! no-op
      else
        call this%spc_rp_list()
      end if
      this%lastiper = iper
    else
      ! -- using data from the last stress period
      write (this%iout, fmtlsp) trim(ftype)
    end if

    ! -- write summary of maw well stress period error messages
    if (count_errors() > 0) then
      call store_error_filename(this%input_fname)
    end if
  end subroutine spc_rp

  !> @ brief spc_rp_list
  !!
  !!  Read the stress period data in list format
  !!
  !<
  subroutine spc_rp_list(this)
    ! -- modules
    use TdisModule, only: kper
    use MemoryManagerModule, only: mem_setptr
    use CharacterStringModule, only: CharacterStringType
    ! -- dummy
    class(TspSpcType), intent(inout) :: this !< TspSpcType object
    ! local variables
    integer(I4B), dimension(:), contiguous, &
      pointer :: bndno
    type(CharacterStringType), dimension(:), contiguous, &
      pointer :: bndtype
    real(DP), dimension(:), contiguous, &
      pointer :: bndvalue
    integer(I4B), pointer :: nlist
    character(len=LINELENGTH) :: line, bound
    character(len=LINELENGTH) :: title
    character(len=LINELENGTH) :: tabletext
    integer(I4B) :: n

    ! set input context pointers
    call mem_setptr(nlist, 'NBOUND', this%input_mempath)
    call mem_setptr(bndno, 'BNDNO', this%input_mempath)
    call mem_setptr(bndtype, 'BNDTYPE', this%input_mempath)
    call mem_setptr(bndvalue, 'BNDVALUE', this%input_mempath)

    ! -- setup table for period data
    if (this%iprpak /= 0) then
      !
      ! -- reset the input table object
      title = trim(adjustl(text))//' PACKAGE ('// &
              'SPC'//') DATA FOR PERIOD'
      write (title, '(a,1x,i6)') trim(adjustl(title)), kper
      call table_cr(this%inputtab, ftype, title)
      call this%inputtab%table_df(1, 3, this%iout, finalize=.FALSE.)
      tabletext = 'NUMBER'
      call this%inputtab%initialize_column(tabletext, 10, alignment=TABCENTER)
      tabletext = 'DATA TYPE'
      call this%inputtab%initialize_column(tabletext, 20, alignment=TABLEFT)
      write (tabletext, '(a,1x,i6)') 'VALUE'
      call this%inputtab%initialize_column(tabletext, 15, alignment=TABCENTER)
    end if

    ! allocate and set input
    do n = 1, nlist
      if (bndno(n) < 1 .or. bndno(n) > this%maxbound) then
        write (errmsg, '(2(a,1x),i0,a)') &
          'BNDNO must be greater than 0 and', &
          'less than or equal to ', this%maxbound, '.'
        call store_error(errmsg)
        cycle
      end if

      this%dblvec(bndno(n)) = bndvalue(n)

      ! -- write line to table
      if (this%iprpak /= 0) then
        bound = bndtype(n)
        write (line, '(i0,a,g0)') bndno(n), ' '//trim(bound)//' ', bndvalue(n)
        call this%inputtab%line_to_columns(line)
      end if
    end do

    ! -- finalize the table
    if (this%iprpak /= 0) then
      call this%inputtab%finalize_table()
    end if
  end subroutine spc_rp_list

  !> @ brief Advance
  !!
  !!  Advance time step
  !!
  !<
  subroutine spc_ad(this, nbound_flowpack, budtxt)
    ! -- modules
    ! -- dummy
    class(TspSpcType), intent(inout) :: this !< TspSpcType object
    integer(I4B), intent(in) :: nbound_flowpack
    character(len=*), intent(in) :: budtxt
    ! -- local
    !
    ! -- Check flow package consistency
    call this%check_flow_package(nbound_flowpack, budtxt)
  end subroutine spc_ad

  !> @ brief Deallocate variables
  !!
  !!  Deallocate and nullify package variables.
  !!
  !<
  subroutine spc_da(this)
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy variables
    class(TspSpcType) :: this !< TspSpcType object
    !
    ! -- deallocate arrays in memory manager
    if (this%readasarrays) then
      ! no-op
    else
      call mem_deallocate(this%dblvec)
    end if
    !
    ! -- deallocate scalars in memory manager
    call mem_deallocate(this%input_fname)
    call mem_deallocate(this%id)
    call mem_deallocate(this%inunit)
    call mem_deallocate(this%iout)
    call mem_deallocate(this%maxbound)
    call mem_deallocate(this%lastiper)
    call mem_deallocate(this%iprpak)
    call mem_deallocate(this%readasarrays)
  end subroutine spc_da

  !> @ brief check_flow_package
  !!
  !!  Check to make sure that flow package information is consistent
  !!  with this SPC information.
  !!
  !<
  subroutine check_flow_package(this, nbound_flowpack, budtxt)
    ! -- modules
    ! -- dummy
    class(TspSpcType), intent(inout) :: this !< TspSpcType object
    integer(I4B), intent(in) :: nbound_flowpack
    character(len=*), intent(in) :: budtxt
    ! -- local
    !
    ! -- Check and make sure MAXBOUND is not less than nbound_flowpack
    if (this%maxbound < nbound_flowpack) then
      write (errmsg, '(a, a, a, i0, a, i0, a)') &
            'The SPC Package corresponding to flow package ', &
            trim(this%packNameFlow), &
            ' has MAXBOUND set less than the number of boundaries &
            &active in this package.  Found MAXBOUND equal ', &
            this%maxbound, &
            ' and number of flow boundaries (NBOUND) equal ', &
            nbound_flowpack, &
            '. Increase MAXBOUND in the SPC input file for this package.'
      call store_error(errmsg)
      call store_error_filename(this%input_fname)
    end if
    !
    ! -- If budtxt is RCHA or EVTA, then readasarrays must be used, otherwise
    !    readasarrays cannot be used
    select case (trim(adjustl(budtxt)))
    case ('RCHA')
      if (.not. this%readasarrays) then
        write (errmsg, '(a, a, a)') &
          'Array-based recharge must be used with array-based stress package &
          &concentrations.  GWF Package ', trim(this%packNameFlow), ' is being &
          &used with list-based SPC6 input.  Use array-based SPC6 input instead.'
        call store_error(errmsg)
        call store_error_filename(this%input_fname)
      end if
    case ('EVTA')
      if (.not. this%readasarrays) then
        write (errmsg, '(a, a, a)') &
          'Array-based evapotranspiration must be used with array-based stress &
          &package concentrations.  GWF Package ', trim(this%packNameFlow), &
          &' is being used with list-based SPC6 input.  Use array-based SPC6 &
          &input instead.'
        call store_error(errmsg)
        call store_error_filename(this%input_fname)
      end if
    case default
      if (this%readasarrays) then
        write (errmsg, '(a, a, a)') &
          'List-based packages must be used with list-based stress &
          &package concentrations.  GWF Package ', trim(this%packNameFlow), &
          &' is being used with array-based SPC6 input.  Use list-based SPC6 &
          &input instead.'
        call store_error(errmsg)
        call store_error_filename(this%input_fname)
      end if
    end select
  end subroutine check_flow_package

end module TspSpcModule
