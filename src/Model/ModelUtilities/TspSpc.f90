!> @brief This module contains the TspSpc Module
!!
!! This module contains the code for reading and storing a
!! generic input file of source and sink concentrations or
!! temperatures.
!!
!<
module TspSpcModule

  use KindModule, only: DP, LGP, I4B
  use ConstantsModule, only: LENPACKAGENAME, LENMODELNAME, &
                             LENMEMPATH, DZERO, DNODATA, LENFTYPE, &
                             LINELENGTH, LENVARNAME
  use SimVariablesModule, only: errmsg
  use SimModule, only: store_error, count_errors, store_error_filename
  use MemoryHelperModule, only: create_mem_path
  use MemoryManagerModule, only: mem_setptr, get_isize
  use BaseDisModule, only: DisBaseType
  use TdisModule, only: kper

  implicit none
  private
  public :: TspSpcType

  character(len=LENFTYPE) :: ftype = 'SPC'

  !> @brief Derived type for managing SPC input
  !!
  !! This derived type reads pre-loaded values from the SPC input
  !! memory path and provides concentrations or temperatures to the
  !! SSM package for individual GWF stress packages.
  !<
  type :: TspSpcType

    character(len=LENMODELNAME) :: name_model = '' !< the name of the model that contains this package
    character(len=LENPACKAGENAME) :: packName = '' !< name of the package
    character(len=LENPACKAGENAME) :: packNameFlow = '' !< name of the corresponding flow package
    character(len=LENVARNAME) :: depvarname = '' !< name of the dependent variable (CONCENTRATION or TEMPERATURE)
    character(len=LENMEMPATH) :: memoryPath = '' !< the location in the memory manager where the variables are stored
    character(len=LENMEMPATH) :: input_mempath = '' !< input memory path for SPC data
    character(len=LINELENGTH) :: input_fname = '' !< SPC input file name
    integer(I4B), pointer :: id => null() !< id number for this spc package
    integer(I4B), pointer :: iout => null() !< unit number for output
    integer(I4B), pointer :: maxbound => null() !< length of dblvec
    integer(I4B), pointer :: iprpak => null() !< flag for printing input
    logical(LGP), pointer :: readasarrays => null() !< flag for reading concentrations as an array
    logical(LGP) :: ts_active = .false. !< .true. if time series or time-array series are active
    real(DP), dimension(:), pointer, contiguous :: dblvec => null() !< vector of floats read from file
    class(DisBaseType), pointer :: dis => null() !< model discretization object

  contains

    procedure :: initialize
    procedure :: allocate_scalars
    procedure :: allocate_arrays
    procedure :: get_value
    procedure :: apply_input_values
    procedure :: spc_rp
    procedure :: spc_ad
    procedure :: spc_da
    procedure :: check_flow_package

  end type TspSpcType

contains

  !> @brief Initialize the SPC type
  !!
  !! Initialize the SPC object using input context data.
  !!
  !<
  subroutine initialize(this, dis, id, input_mempath, iout, name_model, &
                        packNameFlow, dvn, readasarrays, input_fname)
    ! -- dummy variables
    class(TspSpcType) :: this !< TspSpcType
    class(DisBaseType), pointer, intent(in) :: dis !< discretization package
    integer(I4B), intent(in) :: id !< id number for this spc package
    character(len=*), intent(in) :: input_mempath !< input memory path
    integer(I4B), intent(in) :: iout !< unit number for output
    character(len=*), intent(in) :: name_model !< model name
    character(len=*), intent(in) :: packNameFlow !< name of corresponding flow package
    character(len=*), intent(in) :: dvn !< dependent variable name (CONCENTRATION or TEMPERATURE)
    logical(LGP), intent(in) :: readasarrays !< .true. for array-based input
    character(len=*), intent(in) :: input_fname !< SPC input file name
    ! -- local
    integer(I4B), pointer :: maxbound_ptr
    integer(I4B) :: isize
    !
    write (this%packName, '(a,i0)') 'SPC-', id
    this%name_model = name_model
    this%memoryPath = create_mem_path(this%name_model, this%packName)
    this%input_mempath = input_mempath
    this%input_fname = input_fname
    !
    call this%allocate_scalars()
    !
    this%id = id
    this%iout = iout
    this%packNameFlow = packNameFlow
    this%depvarname = dvn
    this%readasarrays = readasarrays
    this%dis => dis
    !
    ! -- set maxbound
    if (readasarrays) then
      this%maxbound = dis%get_ncpl()
    else
      call mem_setptr(maxbound_ptr, 'MAXBOUND', input_mempath)
      this%maxbound = maxbound_ptr
    end if
    !
    call this%allocate_arrays()
    !
    ! -- check for active time series
    call get_isize('TS6_FILENAME', input_mempath, isize)
    if (isize > 0) this%ts_active = .true.
    call get_isize('TAS6_FILENAME', input_mempath, isize)
    if (isize > 0) this%ts_active = .true.
    !
    write (this%iout, '(4x,a,a,a,a,a)') 'USING SPC INPUT FILE ', &
      trim(input_fname), ' TO SET ', trim(dvn), &
      'S FOR PACKAGE '//trim(packNameFlow)
  end subroutine initialize

  !> @brief Allocate package scalars
  !<
  subroutine allocate_scalars(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy variables
    class(TspSpcType) :: this !< TspSpcType object
    !
    call mem_allocate(this%id, 'ID', this%memoryPath)
    call mem_allocate(this%iout, 'IOUT', this%memoryPath)
    call mem_allocate(this%maxbound, 'MAXBOUND', this%memoryPath)
    call mem_allocate(this%iprpak, 'IPRPAK', this%memoryPath)
    call mem_allocate(this%readasarrays, 'READASARRAYS', this%memoryPath)
    !
    this%id = 0
    this%iout = 0
    this%maxbound = 0
    this%iprpak = 0
    this%readasarrays = .false.
  end subroutine allocate_scalars

  !> @brief Allocate package arrays
  !<
  subroutine allocate_arrays(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy variables
    class(TspSpcType) :: this !< TspSpcType object
    ! -- local
    integer(I4B) :: i
    !
    call mem_allocate(this%dblvec, this%maxbound, 'DBLVEC', this%memoryPath)
    !
    do i = 1, this%maxbound
      this%dblvec(i) = DZERO
    end do
  end subroutine allocate_arrays

  !> @brief Get the data value from this package
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
      ! -- Special handling for reduced grids and readasarrays.
      ! -- If flow and transport are in the same simulation, ientry is a user
      ! -- node number corresponding to the correct position in dblvec.
      ! -- If they are in separate simulations, ientry is a reduced node number
      ! -- and must be converted to a user node number.
      if (nbound_flow == this%maxbound) then
        value = this%dblvec(ientry)
      else
        nu = this%dis%get_nodeuser(ientry)
        value = this%dblvec(nu)
      end if
    else
      value = this%dblvec(ientry)
    end if
  end function get_value

  !> @brief Apply current input mempath values to dblvec
  !!
  !! For list-based SPC, iterates BNDNO/value rows. For array-based SPCA,
  !! copies the depvarname array directly.
  !!
  !<
  subroutine apply_input_values(this)
    ! -- dummy
    class(TspSpcType), intent(inout) :: this !< TspSpcType object
    ! -- local
    integer(I4B), pointer :: nbound
    integer(I4B), dimension(:), pointer, contiguous :: bndno_arr
    real(DP), dimension(:), pointer, contiguous :: val_arr
    integer(I4B) :: n, isize
    !
    if (this%readasarrays) then
      ! -- array mode: copy depvarname array into dblvec
      call get_isize(trim(this%depvarname), this%input_mempath, isize)
      if (isize > 0) then
        call mem_setptr(val_arr, trim(this%depvarname), this%input_mempath)
        do n = 1, this%maxbound
          this%dblvec(n) = val_arr(n)
        end do
      end if
    else
      ! -- list mode: apply BNDNO-indexed values; DNODATA entries are skipped
      call mem_setptr(nbound, 'NBOUND', this%input_mempath)
      call mem_setptr(bndno_arr, 'BNDNO', this%input_mempath)
      call mem_setptr(val_arr, trim(this%depvarname), this%input_mempath)
      do n = 1, nbound
        if (val_arr(n) /= DNODATA) then
          this%dblvec(bndno_arr(n)) = val_arr(n)
        end if
      end do
    end if
  end subroutine apply_input_values

  !> @brief Read and prepare stress period data
  !!
  !! Copies input period data into dblvec when new data
  !! has been loaded for this stress period.
  !!
  !<
  subroutine spc_rp(this)
    ! -- dummy
    class(TspSpcType), intent(inout) :: this !< TspSpcType object
    ! -- local
    integer(I4B), pointer :: iper
    ! -- formats
    character(len=*), parameter :: fmtlsp = &
      &"(1X,/1X,'REUSING ',A,'S FROM LAST STRESS PERIOD')"
    !
    call mem_setptr(iper, 'IPER', this%input_mempath)
    if (iper /= kper) then
      write (this%iout, fmtlsp) trim(ftype)
      return
    end if
    !
    call this%apply_input_values()
  end subroutine spc_rp

  !> @brief Advance
  !!
  !! Re-applies input values after IDM has advanced time series for the
  !! new time step, then checks flow package consistency.
  !!
  !<
  subroutine spc_ad(this, nbound_flowpack, budtxt)
    ! -- dummy
    class(TspSpcType), intent(inout) :: this !< TspSpcType object
    integer(I4B), intent(in) :: nbound_flowpack
    character(len=*), intent(in) :: budtxt
    ! -- local
    integer(I4B), pointer :: iper
    !
    ! -- Re-apply if time series are active
    if (this%ts_active) then
      call mem_setptr(iper, 'IPER', this%input_mempath)
      if (iper == kper) then
        call this%apply_input_values()
      end if
    end if
    !
    call this%check_flow_package(nbound_flowpack, budtxt)
  end subroutine spc_ad

  !> @brief Deallocate variables
  !<
  subroutine spc_da(this)
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy variables
    class(TspSpcType) :: this !< TspSpcType object
    !
    call mem_deallocate(this%dblvec)
    call mem_deallocate(this%id)
    call mem_deallocate(this%iout)
    call mem_deallocate(this%maxbound)
    call mem_deallocate(this%iprpak)
    call mem_deallocate(this%readasarrays)
  end subroutine spc_da

  !> @brief Check flow package consistency
  !!
  !!  Check that MAXBOUND is not less than nbound_flowpack and that
  !!  readasarrays is consistent with the flow package type.
  !!
  !<
  subroutine check_flow_package(this, nbound_flowpack, budtxt)
    ! -- dummy
    class(TspSpcType), intent(inout) :: this !< TspSpcType object
    integer(I4B), intent(in) :: nbound_flowpack
    character(len=*), intent(in) :: budtxt
    !
    if (this%maxbound < nbound_flowpack) then
      write (errmsg, '(a,a,a,i0,a,i0,a)') &
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
    select case (trim(adjustl(budtxt)))
    case ('RCHA')
      if (.not. this%readasarrays) then
        write (errmsg, '(a,a,a)') &
          'Array-based recharge must be used with array-based stress package &
          &concentrations.  GWF Package ', trim(this%packNameFlow), ' is being &
          &used with list-based SPC6 input.  Use array-based SPC6 input instead.'
        call store_error(errmsg)
        call store_error_filename(this%input_fname)
      end if
    case ('EVTA')
      if (.not. this%readasarrays) then
        write (errmsg, '(a,a,a)') &
          'Array-based evapotranspiration must be used with array-based stress &
          &package concentrations.  GWF Package ', trim(this%packNameFlow), &
          &' is being used with list-based SPC6 input.  Use array-based SPC6 &
          &input instead.'
        call store_error(errmsg)
        call store_error_filename(this%input_fname)
      end if
    case default
      if (this%readasarrays) then
        write (errmsg, '(a,a,a)') &
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
