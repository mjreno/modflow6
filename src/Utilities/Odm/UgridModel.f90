!> @brief This module contains the UgridModelModule
!!
!<
module UgridModelModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LINELENGTH, LENCOMPONENTNAME, LENMEMPATH
  use SimVariablesModule, only: errmsg
  use SimModule, only: store_error, store_error_filename
  use MemoryManagerModule, only: mem_setptr
  use InputDefinitionModule, only: InputParamDefinitionType
  use CharacterStringModule, only: CharacterStringType
  use NCExportModelModule, only: NCExportBaseModelType
  use netcdf

  implicit none
  private
  public :: nf_verify
  public :: UgridModelNCDimIdType, UgridModelNCVarIdType
  public :: Ugrid2dModelType

  ! NOTE currently for DIS (structured grid)
  type :: UgridModelNCDimIdType
    integer(I4B) :: nmesh_node
    integer(I4B) :: nmesh_face
    integer(I4B) :: max_nmesh_face_nodes
    integer(I4B) :: nlay
    integer(I4B) :: time
    ! TODO remove?
    integer(I4B) :: x
    integer(I4B) :: y
    integer(I4B) :: ncpl
  contains
  end type UgridModelNCDimIdType

  type :: UgridModelNCVarIdType
    integer(I4B) :: mesh
    integer(I4B) :: mesh_node_x
    integer(I4B) :: mesh_node_y
    integer(I4B) :: mesh_face_x
    integer(I4B) :: mesh_face_y
    integer(I4B) :: mesh_face_xbnds
    integer(I4B) :: mesh_face_ybnds
    integer(I4B) :: mesh_face_nodes
    integer(I4B) :: time
    integer(I4B), dimension(:), allocatable  :: dependent
  contains
  end type UgridModelNCVarIdType

  type, abstract, extends(NCExportBaseModelType) :: UgridModelType
    type(UgridModelNCDimIdType) dim_ids
    type(UgridModelNCVarIdType) var_ids
    integer(I4B) :: nlay
  contains
    procedure :: create_global_att
    procedure(nc_array_export_if), deferred :: export_input_array
    procedure :: export_input_arrays
    procedure :: add_pkg_data
    procedure :: create_dep_var
    procedure :: create_gridmap_var
  end type UgridModelType

  abstract interface
    subroutine nc_array_export_if(this, pkgname, mempath, idt)
      import UgridModelType, InputParamDefinitionType, LGP
      class(UgridModelType), intent(inout) :: this
      character(len=*), intent(in) :: pkgname
      character(len=*), intent(in) :: mempath
      type(InputParamDefinitionType), pointer, intent(in) :: idt
    end subroutine
  end interface

  type, abstract, extends(UgridModelType) :: Ugrid2dModelType
  contains
    procedure :: create_mesh
  end type Ugrid2dModelType

contains

  subroutine create_global_att(this)
    class(UgridModelType), intent(inout) :: this
    call nf_verify(nf90_put_att(this%ncid, NF90_GLOBAL, 'title', &
                                this%annotation%title), this%ncid, this%iout)
    call nf_verify(nf90_put_att(this%ncid, NF90_GLOBAL, 'source', &
                                this%annotation%source), this%ncid, this%iout)
    call nf_verify(nf90_put_att(this%ncid, NF90_GLOBAL, 'model', &
                                this%annotation%model), this%ncid, this%iout)
    call nf_verify(nf90_put_att(this%ncid, NF90_GLOBAL, 'history', &
                                this%annotation%history), this%ncid, this%iout)
    call nf_verify(nf90_put_att(this%ncid, NF90_GLOBAL, 'Conventions', &
                                this%annotation%conventions), this%ncid, this%iout)
  end subroutine create_global_att

  subroutine export_input_arrays(this, pkgname, mempath, param_dfns)
    use MemoryManagerModule, only: get_isize
    class(UgridModelType), intent(inout) :: this
    character(len=*), intent(in) :: pkgname
    character(len=*), intent(in) :: mempath
    type(InputParamDefinitionType), dimension(:), pointer, intent(in) :: param_dfns
    type(InputParamDefinitionType), pointer :: idt
    integer(I4B) :: iparam, isize
    !
    do iparam = 1, size(param_dfns)
      ! -- assign param definition pointer
      idt => param_dfns(iparam)
      ! -- for now
      if (idt%blockname == 'GRIDDATA') then
        ! -- veriy variable is allocated
        call get_isize(idt%mf6varname, mempath, isize)
        !
        if (isize > 0) then
          write(this%iout, '(a)') 'ODM export wants to export='//trim(idt%mf6varname)
          call this%export_input_array(pkgname, mempath, idt)
        end if
      end if
    end do
  end subroutine export_input_arrays

  subroutine add_pkg_data(this)
    use MemoryManagerExtModule, only: mem_set_value
    use IdmDfnSelectorModule, only: param_definitions
    use SourceCommonModule, only: idm_subcomponent_type
    class(UgridModelType), intent(inout) :: this
    character(LENCOMPONENTNAME) :: ptype, pname, pkgtype
    type(CharacterStringType), dimension(:), contiguous, &
      pointer :: pkgtypes => null()
    type(CharacterStringType), dimension(:), contiguous, &
      pointer :: pkgnames => null()
    type(CharacterStringType), dimension(:), contiguous, &
      pointer :: mempaths => null()
    type(InputParamDefinitionType), dimension(:), pointer :: param_dfns
    character(len=LENMEMPATH) :: mempath
    integer(I4B) :: n
    integer(I4B), pointer :: export_arrays
    character(len=LENMEMPATH) :: exportstr
    logical(LGP) :: found, clip
    !
    ! -- set pointers to model path package info
    call mem_setptr(pkgtypes, 'PKGTYPES', this%inmodel_mempath)
    call mem_setptr(pkgnames, 'PKGNAMES', this%inmodel_mempath)
    call mem_setptr(mempaths, 'MEMPATHS', this%inmodel_mempath)
    !
    allocate (export_arrays)
    !
    do n = 1, size(mempaths)
      !
      ! -- set export_arrays to default
      export_arrays = 0
      !
      ! -- set package attributes
      mempath = mempaths(n)
      pname = pkgnames(n)
      ptype = pkgtypes(n)
      !
      ! update nc info
      this%annotation%pkgname = pname
      this%annotation%pkgtype = ptype
      !
      ! -- export input arrays
      if (mempath /= '') then
        ! -- update export
        call mem_set_value(export_arrays, 'EXPORT_NC', mempath, found)
        !
        if (export_arrays > 0) then
          pkgtype = idm_subcomponent_type(this%modeltype, ptype)
          param_dfns => param_definitions(this%modeltype, pkgtype)
          ! TODO: use ptype if not multi_package_type?
          call this%export_input_arrays(pname, mempath, param_dfns)
        end if

!        clip = .false.
!        call mem_set_value(exportstr, 'EXPORT_ARRAY_NC', mempath, found)
!        if (found) then
!          if (exportstr == 'CLIP') then
!            clip = .true.
!          end if
!          pkgtype = idm_subcomponent_type(this%modeltype, ptype)
!          param_dfns => param_definitions(this%modeltype, pkgtype)
          ! TODO: use ptype if not multi_package_type?
!          call this%export_input_arrays(pname, mempath, param_dfns, clip)
!        end if
      end if
    end do
    !
    ! -- cleanup
    deallocate (export_arrays)
  end subroutine add_pkg_data

  subroutine create_dep_var(this)
    class(UgridModelType), intent(inout) :: this
    character(len=LINELENGTH) :: varname, longname
    integer(I4B) :: k
    !
    do k = 1, this%nlay
      !
      ! -- set layer variable and longnames
      varname = ''
      longname = ''
      write(varname, '(a,i0)') trim(this%annotation%xname)//'_l', k
      write(longname, '(a,i0)') trim(this%annotation%xlongname)//' layer ', k
      !
      ! -- create the netcdf dependent layer variable
      call nf_verify(nf90_def_var(this%ncid, varname, NF90_DOUBLE, &
                                (/this%dim_ids%nmesh_face, this%dim_ids%time/), &
                                this%var_ids%dependent(k)), this%ncid, this%iout)
      call nf_verify(nf90_put_att(this%ncid, this%var_ids%dependent(k), 'units', 'm'), &
                     this%ncid, this%iout)
      call nf_verify(nf90_put_att(this%ncid, this%var_ids%dependent(k), &
                                  'standard_name', this%annotation%xstdname), &
                                  this%ncid, this%iout)
      call nf_verify(nf90_put_att(this%ncid, this%var_ids%dependent(k), 'long_name', &
                                  longname), this%ncid, this%iout)
      call nf_verify(nf90_put_att(this%ncid, this%var_ids%dependent(k), '_FillValue', &
                                  (/NF90_FILL_DOUBLE/)), this%ncid, this%iout)
      call nf_verify(nf90_put_att(this%ncid, this%var_ids%dependent(k), 'mesh', &
                                  this%mesh_name), this%ncid, this%iout)
      call nf_verify(nf90_put_att(this%ncid, this%var_ids%dependent(k), 'location', &
                                  'face'), this%ncid, this%iout)
      if (this%ogc_wkt /= '') then
        call nf_verify(nf90_put_att(this%ncid, this%var_ids%dependent(k), 'coordinates', &
                                    'mesh_face_x mesh_face_y'), this%ncid, this%iout)
        call nf_verify(nf90_put_att(this%ncid, this%var_ids%dependent(k), 'grid_mapping', &
                                    this%gridmap_name), this%ncid, this%iout)
      end if
    end do
  end subroutine create_dep_var

  subroutine create_gridmap_var(this)
    class(UgridModelType), intent(inout) :: this
    integer(I4B) :: var_id
    !
    ! -- was projection info provided
    if (this%ogc_wkt /= '') then
      !
      ! -- create projection variable
      call nf_verify(nf90_redef(this%ncid), this%ncid, this%iout)
      call nf_verify(nf90_def_var(this%ncid, this%gridmap_name, NF90_INT, var_id), &
                     this%ncid, this%iout)
      ! toDO: is it possible that only cf-convention supported CRS can use
      !       crs_wkt while others can use wkt?  Make sure wkt is generic,
      !       might need to use both
      !call nf_verify(nf90_put_att(this%ncid, var_id, 'crs_wkt', this%ogc_wkt), &
      call nf_verify(nf90_put_att(this%ncid, var_id, 'wkt', this%ogc_wkt), &
                     this%ncid, this%iout)
      call nf_verify(nf90_enddef(this%ncid), this%ncid, this%iout)
      call nf_verify(nf90_put_var(this%ncid, var_id, 1), &
                     this%ncid, this%iout)
    end if
  end subroutine create_gridmap_var

  subroutine create_mesh(this)
    class(Ugrid2dModelType), intent(inout) :: this
    integer(I4B) :: var_id, n
    character(len=LINELENGTH) :: varname, longname
    !
    ! -- mesh variable
    call nf_verify(nf90_def_var(this%ncid, this%mesh_name, NF90_INT, &
                                this%var_ids%mesh), this%ncid, this%iout)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%mesh, 'cf_role', &
                                'mesh_topology'), this%ncid, this%iout)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%mesh, 'long_name', &
                                '2D mesh topology'), this%ncid, this%iout)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%mesh, 'topology_dimension', &
                                2), this%ncid, this%iout)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%mesh, 'face_dimension', &
                                'nmesh_face'), this%ncid, this%iout)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%mesh, 'node_coordinates', &
                                'mesh_node_x mesh_node_y'), this%ncid, this%iout)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%mesh, 'face_coordinates', &
                                'mesh_face_x mesh_face_y'), this%ncid, this%iout)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%mesh, 'face_node_connectivity', &
                                'mesh_face_nodes'), this%ncid, this%iout)

    ! -- node y
    call nf_verify(nf90_def_var(this%ncid, 'mesh_node_y', NF90_DOUBLE, &
                                (/this%dim_ids%nmesh_node/), this%var_ids%mesh_node_y), &
                                this%ncid, this%iout)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%mesh_node_y, 'units', &
                                'm'), this%ncid, this%iout)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%mesh_node_y, 'standard_name', &
                                'projection_y_coordinate'), this%ncid, this%iout)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%mesh_node_y, 'long_name', &
                                'Northing'), this%ncid, this%iout)
    if (this%ogc_wkt /= '') then
      call nf_verify(nf90_put_att(this%ncid, this%var_ids%mesh_node_y, 'grid_mapping', &
                                  this%gridmap_name), this%ncid, this%iout)
    end if

    ! -- face y
    call nf_verify(nf90_def_var(this%ncid, 'mesh_face_y', NF90_DOUBLE, &
                                (/this%dim_ids%nmesh_face/), this%var_ids%mesh_face_y), &
                                this%ncid, this%iout)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%mesh_face_y, 'units', &
                                'm'), this%ncid, this%iout)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%mesh_face_y, 'standard_name', &
                                'projection_y_coordinate'), this%ncid, this%iout)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%mesh_face_y, 'long_name', &
                                'Northing'), this%ncid, this%iout)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%mesh_face_y, 'bounds', &
                                'mesh_face_ybnds'), this%ncid, this%iout)
    if (this%ogc_wkt /= '') then
      call nf_verify(nf90_put_att(this%ncid, this%var_ids%mesh_face_y, 'grid_mapping', &
                                  this%gridmap_name), this%ncid, this%iout)
    end if

    ! -- face y bnds
    call nf_verify(nf90_def_var(this%ncid, 'mesh_face_ybnds', NF90_DOUBLE, &
                                (/this%dim_ids%max_nmesh_face_nodes, this%dim_ids%nmesh_face/), &
                                this%var_ids%mesh_face_ybnds), this%ncid, this%iout)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%mesh_face_ybnds, 'units', &
                                'm'), this%ncid, this%iout)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%mesh_face_ybnds, 'standard_name', &
                                'projection_y_coordinate'), this%ncid, this%iout)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%mesh_face_ybnds, 'long_name', &
                                'Northing'), this%ncid, this%iout)

    ! -- node x
    call nf_verify(nf90_def_var(this%ncid, 'mesh_node_x', NF90_DOUBLE, &
                                (/this%dim_ids%nmesh_node/), this%var_ids%mesh_node_x), &
                                this%ncid, this%iout)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%mesh_node_x, 'units', &
                                'm'), this%ncid, this%iout)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%mesh_node_x, 'standard_name', &
                                'projection_x_coordinate'), this%ncid, this%iout)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%mesh_node_x, 'long_name', &
                                'Easting'), this%ncid, this%iout)
    if (this%ogc_wkt /= '') then
      call nf_verify(nf90_put_att(this%ncid, this%var_ids%mesh_node_x, 'grid_mapping', &
                                  this%gridmap_name), this%ncid, this%iout)
    end if

    ! -- face x
    call nf_verify(nf90_def_var(this%ncid, 'mesh_face_x', NF90_DOUBLE, &
                                (/this%dim_ids%nmesh_face/), this%var_ids%mesh_face_x), &
                                this%ncid, this%iout)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%mesh_face_x, 'units', &
                                'm'), this%ncid, this%iout)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%mesh_face_x, 'standard_name', &
                                'projection_x_coordinate'), this%ncid, this%iout)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%mesh_face_x, 'long_name', &
                                'Easting'), this%ncid, this%iout)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%mesh_face_x, 'bounds', &
                                'mesh_face_xbnds'), this%ncid, this%iout)
    if (this%ogc_wkt /= '') then
      call nf_verify(nf90_put_att(this%ncid, this%var_ids%mesh_face_x, 'grid_mapping', &
                                  this%gridmap_name), this%ncid, this%iout)
    end if

    ! -- face x bnds
    call nf_verify(nf90_def_var(this%ncid, 'mesh_face_xbnds', NF90_DOUBLE, &
                                (/this%dim_ids%max_nmesh_face_nodes, this%dim_ids%nmesh_face/), &
                                this%var_ids%mesh_face_xbnds), this%ncid, this%iout)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%mesh_face_xbnds, 'units', &
                                'm'), this%ncid, this%iout)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%mesh_face_xbnds, 'standard_name', &
                                'projection_x_coordinate'), this%ncid, this%iout)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%mesh_face_xbnds, 'long_name', &
                                'Easting'), this%ncid, this%iout)

    ! -- face nodes
    call nf_verify(nf90_def_var(this%ncid, 'mesh_face_nodes', NF90_INT, &
                                (/this%dim_ids%max_nmesh_face_nodes, this%dim_ids%nmesh_face/), &
                                this%var_ids%mesh_face_nodes), this%ncid, this%iout)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%mesh_face_nodes, 'cf_role', &
                                'face_node_connectivity'), this%ncid, this%iout)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%mesh_face_nodes, 'long_name', &
                                'Vertices bounding cell (counterclockwise)'), this%ncid, this%iout)
    ! -- TODO: saw QGIS access violations and xugrid issues without this
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%mesh_face_nodes, '_FillValue', &
                                (/NF90_FILL_INT/)), this%ncid, this%iout)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%mesh_face_nodes, 'start_index', &
                                1), this%ncid, this%iout)
  end subroutine create_mesh

  subroutine nf_verify(res, ncid, iout)
    integer(I4B), intent(in) :: res
    integer(I4B), intent(in) :: ncid
    integer(I4B), intent(in) :: iout
    ! -- local variables
    character(len=LINELENGTH) :: errstr
    !
    ! -- strings are set for a subset of errors
    !    but the exit status will always be reported
    if (res /= NF90_NOERR) then
      !
      select case (res)

      case (NF90_EINVAL) ! (-36)
        errstr = 'Invalid Argument'
      case (NF90_EPERM) ! (-37)
        errstr = 'Write to read only'
      case (-38) ! (NC_ENOTINDEFINE)
        errstr = 'Operation not allowed in data mode'
      case (-39) ! (NC_EINDEFINE)
        errstr = 'Operation not allowed in define mode'
      case (NF90_EINVALCOORDS) ! (-40)
        errstr = 'Index exceeds dimension bound'
      case (NF90_ENAMEINUSE) ! (-42)
        errstr = 'String match to name in use'
      case (NF90_ENOTATT) ! (-43)
        errstr = 'Attribute not found'
      case (-45) ! (NC_EBADTYPE)
        errstr = 'Not a netcdf data type'
      case (NF90_EBADDIM) ! (-46)
        errstr = 'Invalid dimension id or name'
      case (NF90_ENOTVAR) ! (-49)
        errstr = 'Variable not found'
      case (NF90_ENOTNC) ! (-51)
        errstr = 'Not a netcdf file'
      case (NF90_ECHAR) ! (-56)
        errstr = 'Attempt to convert between text & numbers'
      case (NF90_EEDGE) ! (-57)
        errstr = 'Edge+start exceeds dimension bound'
      case (NF90_ESTRIDE) ! (-58)
        errstr = 'Illegal stride'
      case (NF90_EBADNAME) ! (-59)
        errstr = 'Attribute or variable name contains illegal characters'
      case default
        errstr = ''
      end select
      !
      if (errstr /= '') then
        write (errmsg, '(a,a,a,i0,a,i0,a)') 'NetCDF library error [error="', &
          trim(errstr), '", exit code=', res, ', ncid=', ncid, '].'
      else
        write (errmsg, '(a,i0,a,i0,a)') 'NetCDF library error [exit code=', &
          res, ', ncid=', ncid, '].'
      end if
      !
      call store_error(errmsg, .true.)
    end if
    !
    ! -- return
    return
  end subroutine nf_verify

end module UgridModelModule
