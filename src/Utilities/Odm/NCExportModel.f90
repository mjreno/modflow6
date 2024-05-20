!> @brief This module contains the NCExportModelModule
!!
!<
module NCExportModelModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LINELENGTH, LENCOMPONENTNAME, LENMODELNAME, LENFTYPE, &
                             LENMEMPATH, LENBIGLINE, DISUNDEF, DIS, DISU, DISV
  use SimVariablesModule, only: errmsg
  use SimModule, only: store_error, store_error_filename

  implicit none
  private
  public :: NCExportBaseModelType
  public :: NCExportAnnotation
  public :: TestUnusedType

  type :: TestUnusedType
    integer(I4B) :: t
  contains
  end type TestUnusedType

  type :: NCExportAnnotation
    character(len=LINELENGTH) :: title
    character(len=LINELENGTH) :: model
    character(len=LINELENGTH) :: history
    character(len=LINELENGTH) :: source
    character(len=LINELENGTH) :: conventions
    character(len=LINELENGTH) :: xname
    character(len=LINELENGTH) :: xstdname
    character(len=LINELENGTH) :: xlongname
    character(len=LENCOMPONENTNAME) :: pkgtype
    character(len=LENCOMPONENTNAME) :: pkgname
  end type NCExportAnnotation

  type :: NCExportModelType
    character(len=LENMODELNAME) :: modelname !< name of model
    character(len=LENCOMPONENTNAME) :: modeltype
    character(len=LINELENGTH) :: modelfname !< name of model input file
    character(len=LINELENGTH) :: nc_filename
    character(len=LENMEMPATH) :: model_mempath
    character(len=LENMEMPATH) :: inmodel_mempath
    character(len=LENMEMPATH) :: indis_mempath
    type(NCExportAnnotation) :: annotation
    character(len=LINELENGTH) :: gridmap_name = 'projection'
    character(len=LINELENGTH) :: mesh_name = 'mesh'
    character(len=LENMEMPATH) :: ncf6_mempath
    character(len=LENBIGLINE) :: ogc_wkt
    real(DP), dimension(:), pointer, contiguous :: x
    integer(I4B) :: disenum
    integer(I4B) :: ncid
    integer(I4B) :: stepcnt
    integer(I4B) :: iout
  contains
   procedure :: init => export_init
   procedure :: destroy => export_destroy
  end type NCExportModelType

  type, abstract, extends(NCExportModelType) :: NCExportBaseModelType
  contains
   procedure(model_define_if), deferred :: df
   procedure(model_step_if), deferred :: step
  end type NCExportBaseModelType

  abstract interface
    subroutine model_define_if(this)
      import NCExportBaseModelType
      class(NCExportBaseModelType), intent(inout) :: this
    end subroutine
    subroutine model_step_if(this)
      import NCExportBaseModelType
      class(NCExportBaseModelType), intent(inout) :: this
    end subroutine
  end interface

contains

  !> @brief netcdf export dis model init
  !<
  subroutine export_init(this, modelname, modeltype, modelfname, disenum, ugrid, iout)
    use SimVariablesModule, only: idm_context
    use VersionModule, only: VERSION
    use MemoryManagerModule, only: mem_setptr
    use MemoryHelperModule, only: create_mem_path
    use MemoryManagerExtModule, only: mem_set_value
    use GwfNcfInputModule, only: GwfNcfParamFoundType
    class(NCExportModelType), intent(inout) :: this
    character(len=*), intent(in) :: modelname
    character(len=*), intent(in) :: modeltype
    character(len=*), intent(in) :: modelfname
    integer(I4B), intent(in) :: disenum
    logical(LGP), intent(in) :: ugrid
    integer(I4B), intent(in) :: iout
    integer :: values(8)
    character(len=LINELENGTH) :: fullname
    !character(len=LENMEMPATH) :: dis_mempath, dis_inmempath
    !character(len=LENFTYPE) :: disftype
    type(GwfNcfParamFoundType) :: found
    logical(LGP) :: found_mempath
    !
    ! -- initialize
    this%modelname = modelname
    this%modeltype = modeltype
    this%modelfname = modelfname
    this%nc_filename = trim(modelname)//'.nc'
    this%model_mempath = ''
    this%inmodel_mempath = ''
    this%annotation%title = ''
    this%annotation%model = ''
    this%annotation%history = ''
    this%annotation%source = ''
    this%annotation%conventions = ''
    this%annotation%xname = ''
    this%annotation%xstdname = ''
    this%annotation%xlongname = ''
    this%annotation%pkgtype = ''
    this%annotation%pkgname = ''
    this%ogc_wkt = ''
    this%ncf6_mempath = ''
    this%disenum = disenum
    this%ncid = 0
    this%stepcnt = 0
    this%iout = iout
    !
    ! -- set file conventions
    this%annotation%conventions = 'CF-1.8'
    if (ugrid) this%annotation%conventions = trim(this%annotation%conventions)//' UGRID-1.0'
    !
    ! -- set nc info
    ! -- GWF Model type
    if (modeltype == 'GWF') then
      fullname = 'Groundwater Flow'
      this%annotation%title = trim(modelname)//' hydraulic head'
      this%annotation%xname = 'head'
      ! -- https://csdms.colorado.edu/wiki/CSN_Searchable_List-Names
      this%annotation%xstdname = 'soil_water__pressure_head'
      this%annotation%xlongname = 'hydraulic head'
    ! -- SWF Model type
    else if (modeltype == 'SWF') then
      fullname = 'Surface Water Flow'
      ! TODO: add qoutflow (outflow) variable?
      this%annotation%title = trim(modelname)//' stage'
      this%annotation%xname = 'stage'
      ! land_surface_water__depth
      ! land_surface_water_flow__depth
      ! land_surface_water_flow__speed
      this%annotation%xstdname = 'land_surface_water__depth'
      this%annotation%xlongname = 'water stage'
    ! -- GWT Model type
    else if (modeltype == 'GWT') then
      fullname = 'Groundwater Transport'
      this%annotation%title = trim(modelname)//' transport concentration'
      this%annotation%xname = 'concentration'
      this%annotation%xstdname = 'soil_water__diffusivity'
      this%annotation%xlongname = 'mass diffusivity'
    ! -- GWE Model type
    else if (modeltype == 'GWE') then
      fullname = 'Groundwater Energy'
      this%annotation%title = trim(modelname)//' temperature'
      this%annotation%xname = 'temperature'
      this%annotation%xstdname = 'soil_water__diffusivity'
      this%annotation%xlongname = 'heat diffusivity'
    else
      errmsg = trim(modeltype)//' models not supported for NetCDF export.'
      call store_error(errmsg, .true.)
    end if
    !
    ! -- model attribute string
    this%annotation%model = trim(modelname)//': MODFLOW 6 '//trim(fullname)// &
                            ' ('//trim(modeltype)//') model'
    !
    ! -- modflow6 version string
    this%annotation%source = 'MODFLOW 6 '//trim(adjustl(VERSION))
    !
    ! -- create timestamp
    call date_and_time(values=values)
    write (this%annotation%history, '(a,i0,a,i0,a,i0,a,i0,a,i0,a,i0,a,i0)') &
      'first created ', values(1), '/', values(2), '/', values(3), ' ',  &
      values(5), ':',  values(6), ':', values(7), '.', values(8)
    !
    ! --set mempaths
    this%model_mempath = create_mem_path(component=modelname)
    this%inmodel_mempath = create_mem_path(component=modelname, &
                                           context=idm_context)
    if (disenum == DIS) then
      this%indis_mempath = create_mem_path(modelname, 'DIS', idm_context)
    else if (disenum == DISU) then
      this%indis_mempath = create_mem_path(modelname, 'DISU', idm_context)
    else if (disenum == DISV) then
      this%indis_mempath = create_mem_path(modelname, 'DISV', idm_context)
    end if
    !
    ! -- set dependent variable pointer
    call mem_setptr(this%x, 'X', this%model_mempath)
    !
    !NCF6_FILENAME
    call mem_set_value(this%ncf6_mempath, 'NCF6_MEMPATH', this%indis_mempath, found_mempath)
    if (found_mempath) then
      call mem_set_value(this%ogc_wkt, 'OGC_WKT', this%ncf6_mempath, found%ogc_wkt)
    end if
  end subroutine export_init

  subroutine export_destroy(this)
    class(NCExportModelType), intent(inout) :: this
    ! -- override in derived class
  end subroutine export_destroy

end module NCExportModelModule
