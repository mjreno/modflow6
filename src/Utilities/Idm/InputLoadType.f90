!> @brief This module contains the InputLoadTypeModule
!!
!! This module defines types that support generic IDM
!! static and dynamic input loading.
!!
!<
module InputLoadTypeModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LINELENGTH, LENCOMPONENTNAME, LENMODELNAME, &
                             LENMEMPATH, LENVARNAME, LENFTYPE
  use SimVariablesModule, only: errmsg
  use SimModule, only: store_error, store_error_filename
  use ModflowInputModule, only: ModflowInputType
  use ListModule, only: ListType
  use InputDefinitionModule, only: InputParamDefinitionType
  use NCFileVarsModule, only: NCPackageVarsType

  implicit none
  private
  public :: StaticPkgLoadBaseType
  public :: DynamicPkgLoadBaseType
  public :: ModelDynamicPkgsType
  public :: AddDynamicModelToList, GetDynamicModelFromList
  public :: StaticPkgLoadType, DynamicPkgLoadType
  public :: SubPackageListType
  public :: model_inputs

  !> @brief Subpackage list type
  !!
  !! Ordered list of subpackages belonging to a single IDM package
  !!
  !<
  type :: SubPackageListType
    character(len=LENCOMPONENTNAME), dimension(:), allocatable :: pkgtypes !< subpkg ftypes, e.g. 'TVK6'
    character(len=LENCOMPONENTNAME), dimension(:), allocatable :: component_types !< subpkg components, e.g. 'UTL'
    character(len=LENCOMPONENTNAME), dimension(:), &
      allocatable :: subcomponent_types !< subpkg subcomponents, e.g. 'TVK'
    character(len=LENCOMPONENTNAME), dimension(:), &
      allocatable :: subcomponent_names !< generated subcomponent names, e.g. 'NPF-TVK1'
    character(len=LINELENGTH), dimension(:), allocatable :: filenames !< input file path for each subpackage instance
    character(len=LENCOMPONENTNAME) :: component_type !< model type, e.g. 'GWF'
    character(len=LENCOMPONENTNAME) :: component_name !< model name, e.g. 'MYMODEL'
    integer(I4B) :: pnum !< number of entries in the list
  contains
    procedure :: create => subpkg_create
    procedure :: add => subpkg_add
    procedure :: set_names => subpkg_names
    procedure :: destroy => subpkg_destroy
  end type SubPackageListType

  !> @brief Static loader type
  !!
  !! This type is a base concrete type for a static input loader
  !!
  !<
  type StaticPkgLoadType
    type(ModflowInputType) :: mf6_input !< description of modflow6 input
    type(NCPackageVarsType), pointer :: nc_vars => null()
    character(len=LENCOMPONENTNAME) :: component_name !< name of component
    character(len=LINELENGTH) :: component_input_name !< component input name, e.g. model name file
    character(len=LINELENGTH) :: input_name !< input name, e.g. package *.chd file
    integer(I4B) :: iperblock !< index of period block on block definition list
    type(SubPackageListType) :: subpkg_list !< list of input subpackages
  contains
    procedure :: init => static_init
    procedure :: create_subpkg_list
    procedure :: destroy => static_destroy
  end type StaticPkgLoadType

  !> @brief Base abstract type for static input loader
  !!
  !! IDM sources should extend and implement this type
  !!
  !<
  type, abstract, extends(StaticPkgLoadType) :: StaticPkgLoadBaseType
  contains
    procedure(load_if), deferred :: load
  end type StaticPkgLoadBaseType

  !> @brief Dynamic loader type
  !!
  !! This type is a base concrete type for a dynamic (period) input loader
  !!
  !<
  type :: DynamicPkgLoadType
    type(ModflowInputType) :: mf6_input !< description of modflow6 input
    type(NCPackageVarsType), pointer :: nc_vars => null()
    character(len=LENCOMPONENTNAME) :: component_name !< name of component
    character(len=LINELENGTH) :: component_input_name !< component input name, e.g. model name file
    character(len=LINELENGTH) :: input_name !< input name, e.g. package *.chd file
    character(len=LINELENGTH), dimension(:), allocatable :: param_names !< dynamic param tagnames
    logical(LGP) :: readasarrays !< readasarrays style input package
    logical(LGP) :: readarraygrid !< readarraygrid style input package
    logical(LGP) :: has_setting !< period block contains setting keystring param
    integer(I4B) :: iperblock !< index of period block on block definition list
    integer(I4B) :: iout !< inunit number for logging
    integer(I4B) :: nparam !< number of in scope params
  contains
    procedure :: init => dynamic_init
    procedure :: df => dynamic_df
    procedure :: ad => dynamic_ad
    procedure :: destroy => dynamic_destroy
  end type DynamicPkgLoadType

  !> @brief Base abstract type for dynamic input loader
  !!
  !! IDM sources should extend and implement this type
  !!
  !<
  type, abstract, extends(DynamicPkgLoadType) :: DynamicPkgLoadBaseType
  contains
    procedure(period_load_if), deferred :: rp
  end type DynamicPkgLoadBaseType

  !> @brief load interfaces for source static and dynamic types
  !<
  abstract interface
    function load_if(this, iout) result(dynamic_loader)
      import StaticPkgLoadBaseType, DynamicPkgLoadBaseType, I4B
      class(StaticPkgLoadBaseType), intent(inout) :: this
      integer(I4B), intent(in) :: iout
      class(DynamicPkgLoadBaseType), pointer :: dynamic_loader
    end function load_if
    subroutine period_load_if(this)
      import DynamicPkgLoadBaseType, I4B
      class(DynamicPkgLoadBaseType), intent(inout) :: this
    end subroutine
  end interface

  !> @brief type for storing a dynamic package load list
  !!
  !! This type is used to store a list of package
  !! dynamic load types for a model
  !!
  !<
  type :: ModelDynamicPkgsType
    character(len=LENCOMPONENTNAME) :: modeltype !< type of model
    character(len=LENMODELNAME) :: modelname !< name of model
    character(len=LINELENGTH) :: modelfname !< name of model input file
    type(ListType) :: pkglist !< model package list
    character(len=LINELENGTH) :: nc_fname !< name of model netcdf input
    integer(I4B) :: ncid !< netcdf file handle
    integer(I4B) :: iout
  contains
    procedure :: init => dynamicpkgs_init
    procedure :: add => dynamicpkgs_add
    procedure :: get => dynamicpkgs_get
    procedure :: rp => dynamicpkgs_rp
    procedure :: df => dynamicpkgs_df
    procedure :: ad => dynamicpkgs_ad
    procedure :: size => dynamicpkgs_size
    procedure :: destroy => dynamicpkgs_destroy
  end type ModelDynamicPkgsType

  type(ListType) :: model_inputs

contains

  !> @brief initialize a SubPackageListType object
  !<
  subroutine subpkg_create(this, component_type, component_name)
    class(SubPackageListType) :: this
    character(len=*), intent(in) :: component_type
    character(len=*), intent(in) :: component_name

    ! initialize
    this%pnum = 0
    this%component_type = component_type
    this%component_name = component_name

    ! allocate arrays
    allocate (this%pkgtypes(0))
    allocate (this%component_types(0))
    allocate (this%subcomponent_types(0))
    allocate (this%subcomponent_names(0))
    allocate (this%filenames(0))
  end subroutine subpkg_create

  !> @brief append one subpackage file instance to the list
  !<
  subroutine subpkg_add(this, pkgtype, component_type, subcomponent_type, &
                        filename)
    use ArrayHandlersModule, only: expandarray
    class(SubPackageListType) :: this
    character(len=*), intent(in) :: pkgtype
    character(len=*), intent(in) :: component_type
    character(len=*), intent(in) :: subcomponent_type
    character(len=*), intent(in) :: filename

    ! reallocate
    call expandarray(this%pkgtypes)
    call expandarray(this%component_types)
    call expandarray(this%subcomponent_types)
    call expandarray(this%subcomponent_names)
    call expandarray(this%filenames)

    ! add new package instance
    this%pnum = this%pnum + 1
    this%pkgtypes(this%pnum) = pkgtype
    this%component_types(this%pnum) = component_type
    this%subcomponent_types(this%pnum) = subcomponent_type
    this%subcomponent_names(this%pnum) = ''
    this%filenames(this%pnum) = filename
  end subroutine subpkg_add

  !> @brief Assign subpackage names for unique mempaths
  !!
  !! Assign subpackage names, set and store mempaths for IDM integrated
  !! subpackages.
  !!
  !<
  subroutine subpkg_names(this, ppkg_sctype, ppkg_scname, &
                          ppkg_mempath, modelfname)
    use MemoryHelperModule, only: create_mem_path
    use MemoryManagerModule, only: mem_allocate, mem_setptr
    use ArrayHandlersModule, only: expandarray
    use SimVariablesModule, only: idm_context
    use CharacterStringModule, only: CharacterStringType
    use ModelPackageInputModule, only: multi_package_type
    use IdmDfnSelectorModule, only: idm_multi_package, idm_integrated
    use SourceCommonModule, only: idm_utl_type
    class(SubPackageListType) :: this
    character(len=*), intent(in) :: ppkg_sctype !< parent package subcomponent type
    character(len=*), intent(in) :: ppkg_scname !< parent package subcomponent name
    character(len=*), intent(in) :: ppkg_mempath !< parent package IDM memory path
    character(len=*), intent(in) :: modelfname !< model name file path (for error reporting)
    ! -- locals
    character(len=LINELENGTH), dimension(:), allocatable :: subptypes !< unique pkgtype values
    integer(I4B), dimension(:), allocatable :: nsubptypes !< count of each unique pkgtype
    type(CharacterStringType), dimension(:), &
      pointer, contiguous :: mempaths !< pointer to allocated subpkg mempath array
    type(CharacterStringType), dimension(:), contiguous, &
      pointer :: pnames, ftypes !< pointers to model name file PNAME and FTYPE arrays
    character(len=LINELENGTH), pointer :: input_fname
    character(len=LENVARNAME) :: ppkg_name !< parent package name from NAM PACKAGES PNAME column
    character(len=LENVARNAME) :: ppkg_type !< canonical parent type after alias stripping, e.g. 'NPF'
    character(len=LENVARNAME) :: ppkg_ftype !< parent file-type string, e.g. 'NPF6'
    character(len=LENVARNAME) :: subpkg_mempath !< e.g. 'TVK6_MEMPATH'
    character(len=LENVARNAME) :: subpkg_prefix !< e.g. 'NPF-' or 'WEL1-'
    character(len=LENVARNAME) :: last_subptype !< tracks last pkgtype seen in deduplication pass
    character(len=LENMEMPATH) :: mempath, model_mempath
    integer(I4B) :: ppkg_inst !< instance number of the parent within NAM packages (multi only)
    integer(I4B) :: subpkg_inst !< per-type instance counter within the assign loop
    integer(I4B) :: ntype, n, m
    logical(LGP) :: multi

    ! nothing to do if no subpackages were added
    if (size(this%pkgtypes) == 0) return

    ! UTL packages are leaf nodes: they do not themselves have subpackages
    if (idm_utl_type(this%component_type, ppkg_sctype)) return

    ! initialize
    subpkg_prefix = ''
    allocate (subptypes(0))
    allocate (nsubptypes(0))

    ! resolve definition names to the namefile packages block type name
    select case (ppkg_sctype)
    case ('EVTA', 'RCHA', 'RIVG', 'CHDG', &
          'WELG', 'DRNG', 'GHBG')
      ppkg_type = ppkg_sctype(1:3)
    case default
      ppkg_type = ppkg_sctype
    end select

    ! build the filetype string used to match FTYPE in the NAM packages block
    ppkg_ftype = trim(ppkg_type)//'6'

    ! multi-package parents require an instance number in the prefix to
    ! guarantee subpackage name uniqueness
    if (idm_integrated(this%component_type, ppkg_type)) then
      multi = idm_multi_package(this%component_type, ppkg_type)
    else
      multi = multi_package_type(this%component_type, ppkg_type, ppkg_ftype)
    end if

    ! build subpkg_prefix from the parent package identity.  EXG (exchange)
    ! packages have no model NAM file and don't need a prefix.
    if (this%component_type /= 'EXG') then
      if (multi) then
        ! if multi, identify instance number of this package type in the
        ! namefile packages block and use to set subpackage prefix.
        model_mempath = create_mem_path(this%component_name, 'NAM', idm_context)
        call mem_setptr(pnames, 'PNAME', model_mempath)
        call mem_setptr(ftypes, 'FTYPE', model_mempath)

        ppkg_inst = 0
        do n = 1, size(pnames)
          if (ftypes(n) == ppkg_ftype) then
            ppkg_inst = ppkg_inst + 1
            ppkg_name = pnames(n)
            if (ppkg_name == '') then
              ! unnamed entry: default idm name is '<TYPE>-<N>'
              write (ppkg_name, '(a,i0)') trim(ppkg_type)//'-', ppkg_inst
            end if
            if (ppkg_name == ppkg_scname) then
              ! set the prefix
              write (subpkg_prefix, '(a,i0,a)') trim(ppkg_type), ppkg_inst, '-'
              exit
            end if
          end if
        end do

        if (subpkg_prefix == '') then
          errmsg = &
            'Internal IDM error: subpackage load cannot identify &
            &package "'//trim(ppkg_scname)//'" in model name file &
            &packages block.'
          call store_error(errmsg)
          call store_error_filename(modelfname)
        end if

      else
        ! single-instance parent: prefix is '<TYPE>-', e.g. 'NPF-'
        write (subpkg_prefix, '(2a)') trim(ppkg_type), '-'
      end if
    end if

    ! prepare to allocate mempaths array by counting number of
    ! subpackages there are of each type
    last_subptype = ''
    ntype = 0
    do n = 1, size(this%pkgtypes)
      if (this%pkgtypes(n) /= last_subptype) then
        ntype = ntype + 1
        last_subptype = this%pkgtypes(n)
        call expandarray(subptypes)
        call expandarray(nsubptypes)
        subptypes(ntype) = last_subptype
        nsubptypes(ntype) = 1
      else
        nsubptypes(ntype) = nsubptypes(ntype) + 1
      end if
    end do

    ! allocate mempath arrays for each subpackage type, create and
    ! store the memory paths for package side access.
    do n = 1, size(subptypes)
      subpkg_inst = 0
      subpkg_mempath = trim(subptypes(n))//'_MEMPATH'
      call mem_allocate(mempaths, LENMEMPATH, nsubptypes(n), &
                        subpkg_mempath, ppkg_mempath)
      do m = 1, size(this%pkgtypes)
        if (this%pkgtypes(m) == subptypes(n)) then
          subpkg_inst = subpkg_inst + 1
          ! set the subpackage name
          write (this%subcomponent_names(m), '(a,i0)') &
            trim(subpkg_prefix)//trim(this%subcomponent_types(m)), subpkg_inst
          ! create and set mempath
          mempath = create_mem_path(this%component_name, &
                                    this%subcomponent_names(m), &
                                    idm_context)
          mempaths(subpkg_inst) = mempath
          ! create and set INPUT_FNAME string in each new memory path.
          call mem_allocate(input_fname, LINELENGTH, 'INPUT_FNAME', mempath)
          input_fname = trim(this%filenames(m))
        end if
      end do
    end do

    ! cleanup temporaries
    deallocate (subptypes)
    deallocate (nsubptypes)
  end subroutine subpkg_names

  !> @brief destroy a SubPackageListType object
  !<
  subroutine subpkg_destroy(this)
    class(SubPackageListType) :: this
    ! deallocate arrays
    deallocate (this%pkgtypes)
    deallocate (this%component_types)
    deallocate (this%subcomponent_types)
    deallocate (this%subcomponent_names)
    deallocate (this%filenames)
  end subroutine subpkg_destroy

  !> @brief initialize static package loader
  !!
  !<
  subroutine static_init(this, mf6_input, component_name, component_input_name, &
                         input_name)
    class(StaticPkgLoadType), intent(inout) :: this
    type(ModflowInputType), intent(in) :: mf6_input
    character(len=*), intent(in) :: component_name
    character(len=*), intent(in) :: component_input_name
    character(len=*), intent(in) :: input_name
    integer(I4B) :: iblock

    this%mf6_input = mf6_input
    this%component_name = component_name
    this%component_input_name = component_input_name
    this%input_name = input_name
    this%iperblock = 0

    ! create subpackage list
    call this%subpkg_list%create(this%mf6_input%component_type, &
                                 this%mf6_input%component_name)

    ! identify period block definition
    do iblock = 1, size(mf6_input%block_dfns)
      if (mf6_input%block_dfns(iblock)%blockname == 'PERIOD') then
        this%iperblock = iblock
        exit
      end if
    end do
  end subroutine static_init

  !> @brief create the subpackage list
  !!
  !<
  subroutine create_subpkg_list(this)
    use IdmDfnSelectorModule, only: idm_subpackages, idm_integrated
    use MemoryManagerModule, only: mem_setptr, get_isize
    use ArrayHandlersModule, only: expandarray
    use CharacterStringModule, only: CharacterStringType
    class(StaticPkgLoadType), intent(inout) :: this
    character(len=16), dimension(:), pointer :: subpkgs
    type(CharacterStringType), dimension(:), pointer, &
      contiguous :: fnames
    character(len=LINELENGTH) :: tag, fname, pkgtype
    character(len=LENFTYPE) :: c_type, sc_type
    character(len=16) :: subpkg
    integer(I4B) :: idx, n, m, isize

    ! set pointer to package (idm integrated) subpackage list
    subpkgs => idm_subpackages(this%mf6_input%component_type, &
                               this%mf6_input%subcomponent_type)

    ! check each subpackage type this package supports
    do n = 1, size(subpkgs)
      ! check for input matching this supported subpackage
      subpkg = subpkgs(n)
      idx = index(subpkg, '-')

      if (idx > 0) then
        ! split string in component/subcomponent types
        c_type = subpkg(1:idx - 1)
        sc_type = subpkg(idx + 1:len_trim(subpkg))

        if (idm_integrated(c_type, sc_type)) then
          ! construct FILEIN filename tag
          pkgtype = trim(sc_type)//'6'
          tag = trim(pkgtype)//'_FILENAME'
          call get_isize(tag, this%mf6_input%mempath, isize)
          if (isize > 0) then
            ! add all input files of this type to subpackage type list
            call mem_setptr(fnames, tag, this%mf6_input%mempath)
            do m = 1, size(fnames)
              fname = fnames(m)
              call this%subpkg_list%add(pkgtype, c_type, sc_type, fname)
            end do
          end if
        else
          errmsg = 'Identified subpackage is not IDM integrated. Remove dfn &
                   &subpackage tagline for package "'//trim(subpkg)//'".'
          call store_error(errmsg)
          call store_error_filename(this%input_name)
        end if
      end if
    end do

    ! create subpackage names and use to store mempaths in memory manager
    call this%subpkg_list%set_names(this%mf6_input%subcomponent_type, &
                                    this%mf6_input%subcomponent_name, &
                                    this%mf6_input%mempath, &
                                    this%component_input_name)
  end subroutine create_subpkg_list

  subroutine static_destroy(this)
    class(StaticPkgLoadType), intent(inout) :: this
    call this%subpkg_list%destroy()
    if (associated(this%nc_vars)) then
      call this%nc_vars%destroy()
      deallocate (this%nc_vars)
      nullify (this%nc_vars)
    end if
  end subroutine static_destroy

  !> @brief initialize dynamic package loader
  !!
  !! Any managed memory pointed to from model/package context
  !! must be allocated when dynamic loader is initialized.
  !!
  !<
  subroutine dynamic_init(this, mf6_input, component_name, component_input_name, &
                          input_name, iperblock, iout)
    use SimVariablesModule, only: errmsg
    use InputDefinitionModule, only: InputParamDefinitionType
    use DefinitionSelectModule, only: idt_datatype
    class(DynamicPkgLoadType), intent(inout) :: this
    type(ModflowInputType), intent(in) :: mf6_input
    character(len=*), intent(in) :: component_name
    character(len=*), intent(in) :: component_input_name
    character(len=*), intent(in) :: input_name
    integer(I4B), intent(in) :: iperblock
    integer(I4B), intent(in) :: iout
    type(InputParamDefinitionType), pointer :: idt
    integer(I4B) :: iparam, ilen

    this%mf6_input = mf6_input
    this%component_name = component_name
    this%component_input_name = component_input_name
    this%input_name = input_name
    this%readasarrays = .false.
    this%readarraygrid = .false.
    this%has_setting = .false.
    this%iperblock = iperblock
    this%nparam = 0
    this%iout = iout
    nullify (idt)

    ! throw error and exit if not found
    if (this%iperblock == 0) then
      write (errmsg, '(a,a)') &
        'Programming error. (IDM) PERIOD block not found in '&
        &'dynamic package input block dfns: ', &
        trim(mf6_input%subcomponent_name)
      call store_error(errmsg)
      call store_error_filename(this%input_name)
    end if

    ! set readasarrays and readarraygrid
    if (mf6_input%block_dfns(iperblock)%aggregate) then
      ! no-op, list based input
    else
      do iparam = 1, size(mf6_input%param_dfns)
        idt => mf6_input%param_dfns(iparam)
        if (idt%blockname == 'OPTIONS') then
          select case (idt%tagname)
          case ('READASARRAYS')
            this%readasarrays = .true.
          case ('READARRAYGRID')
            this%readarraygrid = .true.
          case default
            ! no-op
          end select
        end if
      end do
    end if

    ! determine if has setting type
    do iparam = 1, size(mf6_input%param_dfns)
      idt => mf6_input%param_dfns(iparam)
      if (idt%blockname == 'PERIOD') then
        if (idt_datatype(idt) == 'KEYSTRING') then
          ilen = len_trim(idt%tagname)
          if (idt%tagname(ilen - 6:ilen) == 'SETTING') then
            this%has_setting = .true.
          end if
        end if
      end if
    end do
  end subroutine dynamic_init

  !> @brief dynamic package loader define
  !!
  !<
  subroutine dynamic_df(this)
    class(DynamicPkgLoadType), intent(inout) :: this
    ! override in derived type
  end subroutine dynamic_df

  !> @brief dynamic package loader advance
  !!
  !<
  subroutine dynamic_ad(this)
    class(DynamicPkgLoadType), intent(inout) :: this
    ! override in derived type
  end subroutine dynamic_ad

  !> @brief dynamic package loader destroy
  !!
  !<
  subroutine dynamic_destroy(this)
    use MemoryManagerModule, only: mem_deallocate
    use MemoryManagerExtModule, only: memorystore_remove
    use SimVariablesModule, only: idm_context
    class(DynamicPkgLoadType), intent(inout) :: this

    ! clean up netcdf variables structure
    if (associated(this%nc_vars)) then
      call this%nc_vars%destroy()
      deallocate (this%nc_vars)
      nullify (this%nc_vars)
    end if

    ! deallocate package static and dynamic input context
    call memorystore_remove(this%mf6_input%component_name, &
                            this%mf6_input%subcomponent_name, &
                            idm_context)
  end subroutine dynamic_destroy

  !> @brief model dynamic packages init
  !!
  !<
  subroutine dynamicpkgs_init(this, modeltype, modelname, modelfname, nc_fname, &
                              ncid, iout)
    class(ModelDynamicPkgsType), intent(inout) :: this
    character(len=*), intent(in) :: modeltype
    character(len=*), intent(in) :: modelname
    character(len=*), intent(in) :: modelfname
    character(len=*), intent(in) :: nc_fname
    integer(I4B), intent(in) :: ncid
    integer(I4B), intent(in) :: iout
    this%modeltype = modeltype
    this%modelname = modelname
    this%modelfname = modelfname
    this%nc_fname = nc_fname
    this%ncid = ncid
    this%iout = iout
  end subroutine dynamicpkgs_init

  !> @brief add package to model dynamic packages list
  !!
  !<
  subroutine dynamicpkgs_add(this, dynamic_pkg)
    class(ModelDynamicPkgsType), intent(inout) :: this
    class(DynamicPkgLoadBaseType), pointer, intent(inout) :: dynamic_pkg
    class(*), pointer :: obj
    obj => dynamic_pkg
    call this%pkglist%add(obj)
  end subroutine dynamicpkgs_add

  !> @brief retrieve package from model dynamic packages list
  !!
  !<
  function dynamicpkgs_get(this, idx) result(res)
    class(ModelDynamicPkgsType), intent(inout) :: this
    integer(I4B), intent(in) :: idx
    class(DynamicPkgLoadBaseType), pointer :: res
    class(*), pointer :: obj
    nullify (res)
    obj => this%pkglist%GetItem(idx)
    if (associated(obj)) then
      select type (obj)
      class is (DynamicPkgLoadBaseType)
        res => obj
      end select
    end if
  end function dynamicpkgs_get

  !> @brief read and prepare model dynamic packages
  !!
  !<
  subroutine dynamicpkgs_rp(this)
    use IdmLoggerModule, only: idm_log_period_header, idm_log_period_close
    class(ModelDynamicPkgsType), intent(inout) :: this
    class(DynamicPkgLoadBaseType), pointer :: dynamic_pkg
    integer(I4B) :: n
    call idm_log_period_header(this%modelname, this%iout)
    do n = 1, this%pkglist%Count()
      dynamic_pkg => this%get(n)
      call dynamic_pkg%rp()
    end do
    call idm_log_period_close(this%iout)
  end subroutine dynamicpkgs_rp

  !> @brief define model dynamic packages
  !!
  !<
  subroutine dynamicpkgs_df(this)
    class(ModelDynamicPkgsType), intent(inout) :: this
    class(DynamicPkgLoadBaseType), pointer :: dynamic_pkg
    integer(I4B) :: n
    do n = 1, this%pkglist%Count()
      dynamic_pkg => this%get(n)
      call dynamic_pkg%df()
    end do
  end subroutine dynamicpkgs_df

  !> @brief advance model dynamic packages
  !!
  !<
  subroutine dynamicpkgs_ad(this)
    class(ModelDynamicPkgsType), intent(inout) :: this
    class(DynamicPkgLoadBaseType), pointer :: dynamic_pkg
    integer(I4B) :: n
    do n = 1, this%pkglist%Count()
      dynamic_pkg => this%get(n)
      call dynamic_pkg%ad()
    end do
  end subroutine dynamicpkgs_ad

  !> @brief get size of model dynamic packages list
  !!
  !<
  function dynamicpkgs_size(this) result(size)
    class(ModelDynamicPkgsType), intent(inout) :: this
    integer(I4B) :: size
    size = this%pkglist%Count()
  end function dynamicpkgs_size

  !> @brief destroy model dynamic packages object
  !!
  !<
  subroutine dynamicpkgs_destroy(this)
    class(ModelDynamicPkgsType), intent(inout) :: this
    class(DynamicPkgLoadBaseType), pointer :: dynamic_pkg
    integer(I4B) :: n
    ! destroy dynamic loaders
    do n = 1, this%pkglist%Count()
      dynamic_pkg => this%get(n)
      call dynamic_pkg%destroy()
      deallocate (dynamic_pkg)
      nullify (dynamic_pkg)
    end do
    call this%pkglist%Clear()
  end subroutine dynamicpkgs_destroy

  !> @brief add model dynamic packages object to list
  !!
  !<
  subroutine AddDynamicModelToList(list, model_dynamic)
    type(ListType), intent(inout) :: list !< package list
    class(ModelDynamicPkgsType), pointer, intent(inout) :: model_dynamic
    class(*), pointer :: obj
    obj => model_dynamic
    call list%Add(obj)
  end subroutine AddDynamicModelToList

  !> @brief get model dynamic packages object from list
  !!
  !<
  function GetDynamicModelFromList(list, idx) result(res)
    type(ListType), intent(inout) :: list !< spd list
    integer(I4B), intent(in) :: idx !< package number
    class(ModelDynamicPkgsType), pointer :: res
    class(*), pointer :: obj
    ! initialize res
    nullify (res)
    ! get the object from the list
    obj => list%GetItem(idx)
    if (associated(obj)) then
      select type (obj)
      class is (ModelDynamicPkgsType)
        res => obj
      end select
    end if
  end function GetDynamicModelFromList

end module InputLoadTypeModule
