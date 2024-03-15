!> @brief This module contains the BoundInputContextModule
!!
!! This module contains a type that stores and creates context
!! relevant to stress package inputs.
!!
!<
module BoundInputContextModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: DZERO, IZERO, LINELENGTH, LENAUXNAME, &
                             LENVARNAME, LENBOUNDNAME
  use SimVariablesModule, only: errmsg
  use SimModule, only: store_error, store_error_filename
  use ModflowInputModule, only: ModflowInputType
  use InputDefinitionModule, only: InputParamDefinitionType
  use CharacterStringModule, only: CharacterStringType
  use DynamicPackageParamsModule, only: DynamicPackageParamsType

  implicit none
  private
  public :: BoundInputContextType
  public :: ReadStateVarType

  !> @brief Pointer type for read state variable
  !<
  type ReadStateVarType
    integer, pointer :: invar
  end type ReadStateVarType

  !> @brief derived type for boundary package input context
  !!
  !! This derived type defines input context used by dynamic package loaders.
  !! Some variables (e.g. iprpak) in the type may have already been created
  !! by a static loader whereas others (e.g. nboound) are created by this
  !! type, updated by to dynamic loader, and accessed from the model package.
  !!
  !<
  type :: BoundInputContextType
    integer(I4B), pointer :: naux => null() !< number of auxiliary variables
    integer(I4B), pointer :: maxbound => null() !< max list input records per period
    integer(I4B), pointer :: inamedbound => null() !< are bound names optioned
    integer(I4B), pointer :: iprpak => null() ! print input option
    integer(I4B), pointer :: nbound => null() !< number of bounds in period
    integer(I4B), pointer :: ncpl => null() !< number of cells per layer
    type(CharacterStringType), dimension(:), pointer, &
      contiguous :: auxname_cst => null() !< array of auxiliary names
    type(CharacterStringType), dimension(:), pointer, &
      contiguous :: boundname_cst => null() !< array of bound names
    real(DP), dimension(:, :), pointer, &
      contiguous :: auxvar => null() !< auxiliary variable array
    integer(I4B), dimension(:), pointer, contiguous :: mshape => null() !< model shape
    logical(LGP) :: readasarrays !< grid or list based input
    type(DynamicPackageParamsType) :: package_params
    type(ModflowInputType) :: mf6_input !< description of input
  contains
    procedure :: create
    procedure :: allocate_scalars
    procedure :: allocate_arrays
    procedure :: list_params_create
    procedure :: array_params_create
    procedure :: destroy
    procedure :: rsv_alloc
    procedure :: bound_params
  end type BoundInputContextType

contains

  !> @brief create boundary input context
  !!
  !<
  subroutine create(this, mf6_input, readasarrays)
    ! -- modules
    ! -- dummy
    class(BoundInputContextType) :: this
    type(ModflowInputType), intent(in) :: mf6_input
    logical(LGP), intent(in) :: readasarrays
    !
    this%mf6_input = mf6_input
    this%readasarrays = readasarrays
    !
    ! -- create the dynamic package input context
    call this%allocate_scalars()
    !
    ! --return
    return
  end subroutine create

  !> @brief create boundary input context
  !!
  !<
  subroutine allocate_scalars(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate, mem_setptr, get_isize
    use MemoryManagerExtModule, only: mem_set_value
    ! -- dummy
    class(BoundInputContextType) :: this
    logical(LGP) :: found
    !
    ! -- set pointers to defined scalars
    call mem_setptr(this%naux, 'NAUX', this%mf6_input%mempath)
    !
    ! -- allocate memory managed scalars
    call mem_allocate(this%nbound, 'NBOUND', this%mf6_input%mempath)
    call mem_allocate(this%ncpl, 'NCPL', this%mf6_input%mempath)
    !
    ! -- internally allocate package optional scalars
    allocate (this%maxbound)
    allocate (this%inamedbound)
    allocate (this%iprpak)
    !
    ! -- initialize allocated and internal scalars
    this%nbound = 0
    this%ncpl = 0
    this%maxbound = 0
    this%inamedbound = 0
    this%iprpak = 0
    !
    ! -- update optional scalars
    call mem_set_value(this%inamedbound, 'BOUNDNAMES', this%mf6_input%mempath, &
                       found)
    call mem_set_value(this%maxbound, 'MAXBOUND', this%mf6_input%mempath, found)
    call mem_set_value(this%iprpak, 'IPRPAK', this%mf6_input%mempath, found)
    !
    ! -- set pointer to model shape
    call mem_setptr(this%mshape, 'MODEL_SHAPE', &
                    this%mf6_input%component_mempath)
    !
    ! -- update ncpl from model shape
    if (size(this%mshape) == 2) then
      this%ncpl = this%mshape(2)
    else if (size(this%mshape) == 3) then
      this%ncpl = this%mshape(2) * this%mshape(3)
    end if
    !
    ! -- initialize package params object
    call this%package_params%init(this%mf6_input, this%readasarrays, &
                                  this%naux, this%inamedbound)
    !
    ! -- return
    return
  end subroutine allocate_scalars

  !> @brief allocate_arrays
  !!
  !! allocate bound input context arrays
  !!
  !<
  subroutine allocate_arrays(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate, mem_setptr, get_isize
    use MemoryManagerExtModule, only: mem_set_value
    ! -- dummy
    class(BoundInputContextType) :: this
    integer(I4B), dimension(:, :), pointer, contiguous :: cellid
    ! -- local
    !
    ! -- set auxname_cst and iauxmultcol
    if (this%naux > 0) then
      call mem_setptr(this%auxname_cst, 'AUXILIARY', this%mf6_input%mempath)
    else
      call mem_allocate(this%auxname_cst, LENAUXNAME, 0, &
                        'AUXILIARY', this%mf6_input%mempath)
    end if
    !
    ! -- allocate cellid if this is not list input
    if (this%readasarrays) then
      call mem_allocate(cellid, 0, 0, 'CELLID', this%mf6_input%mempath)
    end if
    !
    ! -- allocate or set pointer to BOUNDNAME
    if (this%inamedbound == 0) then
      call mem_allocate(this%boundname_cst, LENBOUNDNAME, 0, &
                        'BOUNDNAME', this%mf6_input%mempath)
      !
    else
      call mem_setptr(this%boundname_cst, 'BOUNDNAME', this%mf6_input%mempath)
    end if
    !
    ! -- allocate or set pointer to AUXVAR
    if (this%naux == 0) then
      call mem_allocate(this%auxvar, 0, 0, 'AUXVAR', this%mf6_input%mempath)
      !
    else
      call mem_setptr(this%auxvar, 'AUXVAR', this%mf6_input%mempath)
    end if
    !
    ! -- return
    return
  end subroutine allocate_arrays

  subroutine list_params_create(this, params, nparam, input_name)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate, mem_setptr, get_isize
    use InputDefinitionModule, only: InputParamDefinitionType
    use DefinitionSelectModule, only: get_param_definition_type
    use DynamicPackageParamsModule, only: allocate_param_int1d, &
                                          allocate_param_int2d, &
                                          allocate_param_dbl1d, &
                                          allocate_param_dbl2d, &
                                          allocate_param_charstr
    ! -- dummy
    class(BoundInputContextType) :: this
    character(len=*), dimension(:), allocatable, intent(in) :: params
    integer(I4B), intent(in) :: nparam
    character(len=*), intent(in) :: input_name
    ! -- local
    type(InputParamDefinitionType), pointer :: idt
    integer(I4B) :: iparam
    integer(I4B), pointer :: nseg, nseg_1
    integer(I4B) :: nseg1_isize
    !
    ! --
    do iparam = 1, nparam
      idt => get_param_definition_type(this%mf6_input%param_dfns, &
                                       this%mf6_input%component_type, &
                                       this%mf6_input%subcomponent_type, &
                                       'PERIOD', params(iparam), '')
      !
      ! allocate based on dfn datatype
      select case (idt%datatype)
      case ('INTEGER')
        call allocate_param_int1d(this%maxbound, idt%mf6varname, &
                                  this%mf6_input%mempath)
        !
      case ('DOUBLE')
        call allocate_param_dbl1d(this%maxbound, idt%mf6varname, &
                                  this%mf6_input%mempath)
        !
      case ('STRING')
        call allocate_param_charstr(LENBOUNDNAME, this%maxbound, idt%mf6varname, &
                                    this%mf6_input%mempath)
        !
      case ('INTEGER1D')
        if (idt%shape == 'NCELLDIM') then
          call allocate_param_int2d(size(this%mshape), this%maxbound, &
                                    idt%mf6varname, this%mf6_input%mempath)
        else
          errmsg = 'IDM unimplemented. BoundInputContext::list_params_create &
                   &shape='//trim(idt%shape)
          call store_error(errmsg)
          call store_error_filename(input_name)
        end if
        !
      case ('DOUBLE1D')
        if (idt%shape == 'NAUX') then
          call allocate_param_dbl2d(this%naux, this%maxbound, &
                                    idt%mf6varname, this%mf6_input%mempath)
        else if (idt%shape == 'NSEG-1') then
          call mem_setptr(nseg, 'NSEG', this%mf6_input%mempath)
          !
          call get_isize('NSEG_1', this%mf6_input%mempath, nseg1_isize)
          !
          if (nseg1_isize < 0) then
            call mem_allocate(nseg_1, 'NSEG_1', this%mf6_input%mempath)
            nseg_1 = nseg - 1
          else
            call mem_setptr(nseg_1, 'NSEG_1', this%mf6_input%mempath)
          end if
          !
          call allocate_param_dbl2d(nseg_1, this%maxbound, &
                                    idt%mf6varname, this%mf6_input%mempath)
        else
          errmsg = 'IDM unimplemented. BoundInputContext::list_params_create &
                   &tagname='//trim(idt%tagname)
          call store_error(errmsg)
          call store_error_filename(input_name)
        end if
        !
      case default
        errmsg = 'IDM unimplemented. BoundInputContext::list_params_create &
                 &datatype='//trim(idt%datatype)
        call store_error(errmsg)
        call store_error_filename(input_name)
      end select
    end do
    !
    ! -- return
    return
  end subroutine list_params_create

  !> @brief allocate dfn array input period block parameters
  !!
  !! Currently supports numeric (i.e. array based) params
  !!
  !<
  subroutine array_params_create(this, params, nparam, input_name)
    ! -- modules
    use DefinitionSelectModule, only: get_param_definition_type
    use DynamicPackageParamsModule, only: allocate_param_int1d, &
                                          allocate_param_dbl1d, &
                                          allocate_param_dbl2d
    ! -- dummy
    class(BoundInputContextType) :: this
    character(len=*), dimension(:), allocatable, intent(in) :: params
    integer(I4B), intent(in) :: nparam
    character(len=*), intent(in) :: input_name
    ! -- local
    type(InputParamDefinitionType), pointer :: idt
    integer(I4B) :: iparam
    !
    ! -- allocate dfn input params
    do iparam = 1, nparam
      !
      ! -- assign param definition pointer
      idt => get_param_definition_type(this%mf6_input%param_dfns, &
                                       this%mf6_input%component_type, &
                                       this%mf6_input%subcomponent_type, &
                                       'PERIOD', params(iparam), '')
      !
      if (idt%blockname == 'PERIOD') then
        select case (idt%datatype)
        case ('INTEGER1D')
          call allocate_param_int1d(this%ncpl, idt%mf6varname, &
                                    this%mf6_input%mempath)
          !
        case ('DOUBLE1D')
          call allocate_param_dbl1d(this%ncpl, idt%mf6varname, &
                                    this%mf6_input%mempath)
          !
        case ('DOUBLE2D')
          call allocate_param_dbl2d(this%naux, this%ncpl, idt%mf6varname, &
                                    this%mf6_input%mempath)
          !
        case default
          errmsg = 'IDM unimplemented. BoundInputContext::array_params_create &
                   &datatype='//trim(idt%datatype)
          call store_error(errmsg)
          call store_error_filename(input_name)
        end select
      end if
    end do
    !
    ! -- return
    return
  end subroutine array_params_create

  !> @brief destroy boundary input context
  !!
  !<
  subroutine destroy(this)
    ! -- modules
    ! -- dummy
    class(BoundInputContextType) :: this
    !
    ! -- destroy package params object
    call this%package_params%destroy()
    !
    ! -- deallocate
    deallocate (this%maxbound)
    deallocate (this%inamedbound)
    deallocate (this%iprpak)
    !
    ! -- nullify
    nullify (this%naux)
    nullify (this%nbound)
    nullify (this%ncpl)
    nullify (this%maxbound)
    nullify (this%inamedbound)
    nullify (this%iprpak)
    nullify (this%auxname_cst)
    nullify (this%boundname_cst)
    nullify (this%auxvar)
    nullify (this%mshape)
    !
    ! --return
    return
  end subroutine destroy

  !> @brief allocate a read state variable
  !!
  !! Create and set a read state variable, e.g. 'INRECHARGE',
  !! which are updated per iper load as follows:
  !! -1: unset, not in use
  !!  0: not read in most recent period block
  !!  1: numeric input read in most recent period block
  !!  2: time series input read in most recent period block
  !!
  !<
  function rsv_alloc(this, mf6varname) result(varname)
    ! -- modules
    use ConstantsModule, only: LENVARNAME
    use MemoryManagerModule, only: mem_setptr, mem_allocate
    ! -- dummy
    class(BoundInputContextType) :: this
    character(len=*), intent(in) :: mf6varname
    ! -- local
    character(len=LENVARNAME) :: varname
    integer(I4B) :: ilen
    integer(I4B), pointer :: intvar
    character(len=2) :: prefix = 'IN'
    !
    ! -- assign first column as the block number
    ilen = len_trim(mf6varname)
    !
    if (ilen > (LENVARNAME - len(prefix))) then
      varname = prefix//mf6varname(1:(LENVARNAME - len(prefix)))
    else
      varname = prefix//trim(mf6varname)
    end if
    !
    call mem_allocate(intvar, varname, this%mf6_input%mempath)
    intvar = -1
    !
    ! -- return
    return
  end function rsv_alloc

  !> @brief allocate and set input array to filtered param set
  !!
  !<
  subroutine bound_params(this, params, nparam, input_name, create)
    ! -- modules
    ! -- dummy
    class(BoundInputContextType) :: this
    character(len=LINELENGTH), dimension(:), allocatable, &
      intent(inout) :: params
    integer(I4B), intent(inout) :: nparam
    character(len=*), intent(in) :: input_name
    logical(LGP), optional, intent(in) :: create
    logical(LGP) :: allocate_params
    integer(I4B) :: n
    !
    ! -- initialize allocate_params
    allocate_params = .true.
    !
    ! -- override default if provided
    if (present(create)) then
      allocate_params = create
    end if
    !
    if (allocated(params)) deallocate (params)
    !
    nparam = this%package_params%nparam
    !
    allocate (params(nparam))
    !
    do n = 1, nparam
      params(n) = this%package_params%params(n)
    end do
    !
    if (allocate_params) then
      if (this%readasarrays) then
        !
        call this%array_params_create(params, nparam, input_name)
      else
        !
        call this%list_params_create(params, nparam, input_name)
      end if
    end if
    !
    ! -- return
    return
  end subroutine bound_params

end module BoundInputContextModule
