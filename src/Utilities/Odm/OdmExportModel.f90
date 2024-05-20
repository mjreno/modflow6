!> @brief This module contains the OdmExportModelModule
!!
!!
!<
module OdmExportModelModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LINELENGTH, LENMODELNAME, LENCOMPONENTNAME, LENMEMPATH
  use SimModule, only: store_error, store_error_filename
  use SimVariablesModule, only: errmsg
  use ListModule, only: ListType
  use NCExportModelModule, only: NCExportBaseModelType
  use SimVariablesModule, only: iout ! TODO remove

  implicit none
  private
  public :: GetExportModelIdx
  public :: odm_create
  public :: nc_export
  public :: export_models
  public :: ExportModelType

  type(ListType) :: export_models

  type :: ExportModelType
    character(len=LENMODELNAME) :: modelname !< name of model
    character(len=LENCOMPONENTNAME) :: modeltype
    character(len=LINELENGTH) :: modelfname !< name of model input file
    class(NCExportBaseModelType), pointer :: nc_export => null()
    logical(I4B) :: is_nc_export
    logical(I4B) :: ugrid
    integer(I4B) :: disenum
    integer(I4B) :: iout
  contains
    procedure :: init
    procedure :: destroy
  end type ExportModelType

contains

  function nc_export() result(active)
    logical(LGP) :: active
    integer(I4B) :: n
    type(ExportModelType), pointer :: export_model
    active = .false.
    !
    do n = 1, export_models%Count()
      export_model => GetExportModelIdx(n)
      if (export_model%is_nc_export) then
        active = .true.
        exit
      end if
    end do
  end function nc_export

  !> @brief advance package dynamic data for period steps
  !!
  subroutine odm_create()
    use InputLoadTypeModule, only: ModelDynamicPkgsType
    use InputLoadTypeModule, only: model_dynamic_pkgs
    use MemoryManagerModule, only: mem_setptr
    use MemoryManagerExtModule, only: mem_set_value
    use MemoryHelperModule, only: create_mem_path
    use InputLoadTypeModule, only: GetDynamicModelFromList
    use SimVariablesModule, only: idm_context
    type(ModelDynamicPkgsType), pointer :: model_dynamic_input
    type(ExportModelType), pointer :: export_model
    character(len=LENMEMPATH) :: modelnam_mempath, model_mempath
    integer(I4B), pointer :: export, disenum
    character(len=LINELENGTH) :: exportstr
    integer(I4B) :: n
    logical(LGP) :: found
    !
    do n = 1, model_dynamic_pkgs%Count()
      !
      ! -- allocate and initialize
      allocate (export)
      allocate (export_model)
      export = 0
      !
      ! -- set pointer to dynamic input model instance
      model_dynamic_input => GetDynamicModelFromList(model_dynamic_pkgs, n)
      write(iout, '(a)') 'ODM modelname='//trim(model_dynamic_input%modelname)
      !
      ! --set input mempaths
      modelnam_mempath = create_mem_path(component=model_dynamic_input%modelname, &
                                      subcomponent='NAM', context=idm_context)
      model_mempath = create_mem_path(component=model_dynamic_input%modelname, &
                                        context=idm_context)
      ! -- set pointer to dis enum type
      call mem_setptr(disenum, 'DISENUM', model_mempath)
      !
      ! -- update EXPORT_NETCDF value if it was read
      call mem_set_value(export, 'EXPORT_NETCDF', modelnam_mempath, found)
      if (found) then
        write(iout, '(a)') 'ODM int EXPORT_NETCDF FOUND'
      end if
      !
      call export_model%init(model_dynamic_input%modelname, &
                             model_dynamic_input%modeltype, &
                             model_dynamic_input%modelfname, disenum, iout)
      !
      ! -- TODO explain, this is string above is int
      call mem_set_value(exportstr, 'EXPORT_NETCDF', modelnam_mempath, found)
      if (found) then
        write(iout, '(a)') 'ODM EXPORT_NETCDF='//trim(exportstr)
        export_model%is_nc_export = .true.
        if (exportstr == 'UGRID') then
          export_model%ugrid = .true.
        end if
      end if
      if (export > 0) then
        export_model%is_nc_export = .true.
      end if
      !
      ! -- add model to list
      call AddExportModel(export_model)
      !
      ! -- cleanup
      deallocate (export)
    end do
    !
    ! -- return
    return
  end subroutine odm_create

  !> @brief model export init
  !!
  !<
  subroutine init(this, modelname, modeltype, modelfname, disenum, iout)
    class(ExportModelType), intent(inout) :: this
    character(len=*), intent(in) :: modelname
    character(len=*), intent(in) :: modeltype
    character(len=*), intent(in) :: modelfname
    integer(I4B), intent(in) :: disenum
    integer(I4B), intent(in) :: iout
    !
    this%modelname = modelname
    this%modeltype = modeltype
    this%modelfname = modelfname
    this%is_nc_export = .false.
    this%ugrid = .false.
    this%disenum = disenum
    this%iout = iout
    !
    nullify (this%nc_export)
    !
    return
  end subroutine init

  !> @brief destroy model export object
  !!
  !<
  subroutine destroy(this)
    class(ExportModelType), intent(inout) :: this
    !
    if (associated(this%nc_export)) then
      call this%nc_export%destroy()
    end if
    !
    return
  end subroutine destroy

  !> @brief add model export object to list
  !!
  !<
  subroutine AddExportModel(export_model)
    ! -- dummy variables
    type(ExportModelType), pointer, intent(inout) :: export_model
    ! -- local variables
    class(*), pointer :: obj
    !
    obj => export_model
    call export_models%Add(obj)
    !
    ! -- return
    return
  end subroutine AddExportModel

  function GetExportModel(modelname) result(res)
    ! -- dummy variables
    character(len=*), intent(in) :: modelname
    class(ExportModelType), pointer :: res
    ! -- local variables
    class(*), pointer :: obj
    class(ExportModelType), pointer :: export_model
    integer(I4B) :: n
    !
    ! -- initialize res
    res => null()
    !
    ! -- get the object from the list
    do n = 1, export_models%Count()
      obj => export_models%GetItem(n)
      if (associated(obj)) then
        select type (obj)
        class is (ExportModelType)
          export_model => obj
          if (export_model%modelname == modelname) then
            res => obj
            exit
          end if
        end select
      end if
    end do
    !
    ! -- return
    return
  end function GetExportModel

  function GetExportModelIdx(idx) result(res)
    ! -- dummy variables
    integer(I4B), intent(in) :: idx !< package number
    class(ExportModelType), pointer :: res
    ! -- local variables
    class(*), pointer :: obj
    !
    ! -- initialize res
    nullify (res)
    !
    ! -- get the object from the list
    obj => export_models%GetItem(idx)
    if (associated(obj)) then
      select type (obj)
      class is (ExportModelType)
        res => obj
      end select
    end if
  end function GetExportModelIdx

  !> @brief cleanup model export objects
  !!
  !<
  subroutine ExportModelsDestroy()
    ! -- dummy variables
    ! -- local variables
    class(*), pointer :: obj
    class(ExportModelType), pointer :: export_model
    integer(I4B) :: n
    !
    ! -- get the object from the list
    do n = 1, export_models%Count()
      obj => export_models%GetItem(n)
      if (associated(obj)) then
        select type (obj)
        class is (ExportModelType)
          export_model => obj
          call export_model%destroy()
          deallocate (export_model)
          nullify (export_model)
        end select
      end if
    end do
    !
    call export_models%clear()
    !
    ! -- return
    return
  end subroutine ExportModelsDestroy

end module OdmExportModelModule
