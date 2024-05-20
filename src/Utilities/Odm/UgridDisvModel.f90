!> @brief This module contains the UgridDisvModelModule
!!
!! Export DISV based model NetCDF file based on UGRID layered mesh paradigm
!!
!<
module UgridDisvModelModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LINELENGTH, LENBIGLINE, LENCOMPONENTNAME, &
                             LENMEMPATH
  use SimVariablesModule, only: errmsg
  use SimModule, only: store_error, store_error_filename
  use MemoryManagerModule, only: mem_setptr
  use InputDefinitionModule, only: InputParamDefinitionType
  use CharacterStringModule, only: CharacterStringType
  use UgridModelModule, only: nf_verify, Ugrid2dModelType, &
                              UgridModelNCDimIdType, UgridModelNCVarIdType
  use DisvModule, only: DisvType
  use netcdf

  implicit none
  private
  public :: Ugrid2dDisvModelType

  ! -- UGRID layered mesh (ULM) DISV
  type, extends(Ugrid2dModelType) :: Ugrid2dDisvModelType
    class(DisvType), pointer :: disv => null() !discretization object
  contains
    procedure :: init => dis_export_init
    procedure :: destroy => dis_export_destroy
    procedure :: df
    procedure :: step
    procedure :: export_input_array
    procedure :: create_coord_var
    procedure :: add_mesh_data
  end type Ugrid2dDisvModelType

contains

  !> @brief netcdf export dis model init
  !<
  subroutine dis_export_init(this, modelname, modeltype, modelfname, disenum, ugrid, iout)
    use ArrayHandlersModule, only: expandarray
    class(Ugrid2dDisvModelType), intent(inout) :: this
    character(len=*), intent(in) :: modelname
    character(len=*), intent(in) :: modeltype
    character(len=*), intent(in) :: modelfname
    integer(I4B), intent(in) :: disenum
    logical(LGP), intent(in) :: ugrid
    integer(I4B), intent(in) :: iout
    integer(I4B) :: k, i, j, n
    !
    ! -- initialize base class
    call this%NCExportModelType%init(modelname, modeltype, modelfname, disenum, ugrid, iout)
    !
    ! allocate var_id arrays
    allocate (this%var_ids%dependent(this%disv%nlay))
    !
    ! -- set nlay
    this%nlay = this%disv%nlay
    !
    ! -- create the netcdf file
    !call nf_verify(nf90_create(this%nc_filename, NF90_CLOBBER, this%ncid), &
    !               this%ncid, this%iout)
    ! TODO check if file exists?
    ! TODO: don't create file until sanity checks (START_DATE_TIME, etc) are done
    call nf_verify(nf90_create(this%nc_filename, NF90_NETCDF4, this%ncid), &
    !call nf_verify(nf90_create(this%nc_filename, NF90_64BIT_OFFSET, this%ncid), &
                   this%ncid, this%iout)
  end subroutine dis_export_init

  subroutine dis_export_destroy(this)
    use SimVariablesModule, only: idm_context
    use MemoryManagerExtModule, only: memorylist_remove
    class(Ugrid2dDisvModelType), intent(inout) :: this
    call nf_verify(nf90_close(this%ncid), this%ncid, this%iout)
    !
    ! -- Deallocate idm memory
    if (this%ncf6_mempath /= '') then
      call memorylist_remove(this%modelname, 'NCF', idm_context)
    end if
  end subroutine dis_export_destroy

  subroutine df(this)
    class(Ugrid2dDisvModelType), intent(inout) :: this

    write(this%iout, '(a)') 'Ugrid2dDisvModelType create_global_att()'
    call this%create_global_att()

    write(this%iout, '(a)') 'Ugrid2dDisvModelType create_coord_var()'
    call this%create_coord_var()

    write(this%iout, '(a)') 'Ugrid2dDisvModelType create_mesh()'
    call this%create_mesh()

    write(this%iout, '(a)') 'Ugrid2dDisvModelType create_dep_var()'
    call this%create_dep_var()

    write(this%iout, '(a)') 'Ugrid2dDisvModelType nf90_enddef()'
    call nf_verify(nf90_enddef(this%ncid), this%ncid, this%iout)

    write(this%iout, '(a)') 'Ugrid2dDisvModelType add_mesh_data()'
    call this%add_mesh_data()

    write(this%iout, '(a)') 'Ugrid2dDisvModelType add_pkg_data()'
    call this%add_pkg_data()

    write(this%iout, '(a)') 'Ugrid2dDisvModelType create_gridmap_var()'
    call this%create_gridmap_var()
  end subroutine df

  subroutine step(this)
    use TdisModule, only: totim
    class(Ugrid2dDisvModelType), intent(inout) :: this
    real(DP), dimension(:), pointer, contiguous :: dbl1d
    integer(I4B) :: n, k
    integer(I4B), dimension(2) :: dis_shape
    real(I4B), dimension(:, :), pointer, contiguous :: dbl2d
    !
    ! -- increment step
    this%stepcnt = this%stepcnt + 1
    !
    ! -- add data to dependendent variable
    if (size(this%disv%nodeuser) < &
        size(this%disv%nodereduced)) then
      allocate (dbl1d(size(this%disv%nodereduced)))
      dbl1d = NF90_FILL_DOUBLE
      do n = 1, size(this%disv%nodereduced)
        if (this%disv%nodereduced(n) > 0) then
          dbl1d(n) = this%x(this%disv%nodereduced(n))
        end if
      end do
      !
      dis_shape(1) = this%disv%ncpl
      dis_shape(2) = this%disv%nlay
      allocate (dbl2d(dis_shape(1), dis_shape(2)))
      dbl2d = reshape(dbl1d, dis_shape)
      !
      do k = 1, this%disv%nlay
        call nf_verify(nf90_put_var(this%ncid, &
                                    this%var_ids%dependent(k), dbl2d(:,k), &
                                    start=(/1, this%stepcnt/), &
                                    count=(/(this%disv%ncpl), 1/)), &
                                    this%ncid, this%iout)
      end do

      deallocate (dbl1d)
      deallocate (dbl2d)
    else
      !
      dis_shape(1) = this%disv%ncpl
      dis_shape(2) = this%disv%nlay
      allocate (dbl2d(dis_shape(1), dis_shape(2)))
      dbl2d = reshape(this%x, dis_shape)
      do k = 1, this%disv%nlay
        call nf_verify(nf90_put_var(this%ncid, &
                                  this%var_ids%dependent(k), dbl2d(:, k), &
                                  start=(/1, this%stepcnt/), &
                                  count=(/this%disv%ncpl, 1/)), &
                                  this%ncid, this%iout)
      end do
      deallocate (dbl2d)
    end if
    !
    ! -- write to time coordinate variable
    call nf_verify(nf90_put_var(this%ncid, this%var_ids%time, &
                                totim, start=(/this%stepcnt/)), &
                                this%ncid, this%iout)
  end subroutine step

  subroutine export_input_array(this, pkgname, mempath, idt)
    use InputOutputModule, only: lowcase
    class(Ugrid2dDisvModelType), intent(inout) :: this
    character(len=*), intent(in) :: pkgname
    character(len=*), intent(in) :: mempath
    type(InputParamDefinitionType), pointer, intent(in) :: idt
    integer(I4B), dimension(:), pointer, contiguous :: int1d
    integer(I4B), dimension(:, :), pointer, contiguous :: int2d
    integer(I4B), dimension(:, :, :), pointer, contiguous :: int3d
    real(DP), dimension(:), pointer, contiguous :: dbl1d
    real(DP), dimension(:, :), pointer, contiguous :: dbl2d
    real(DP), dimension(:, :, :), pointer, contiguous :: dbl3d
    character(len=LINELENGTH) :: pname, vname, nc_varname, gridmap
    !
    ! -- set variable name written to file
    pname = pkgname
    vname = idt%tagname
    call lowcase(pname)
    call lowcase(vname)
    nc_varname = trim(pname)//'-'//trim(vname) ! TODO valid nc variable name?
    if (this%ogc_wkt /= '') then
      gridmap = this%gridmap_name
    else
      gridmap = ''
    end if
    !
    select case(idt%datatype)
    case ('INTEGER1D')
      call mem_setptr(int1d, idt%mf6varname, mempath)
      call nc_export_int1d(this%ncid, this%dim_ids, this%var_ids, this%disv, &
                           int1d, nc_varname, pkgname, idt%tagname, gridmap, &
                           idt%shape, this%iout)
    case ('INTEGER2D')
      call mem_setptr(int2d, idt%mf6varname, mempath)
      call nc_export_int2d(this%ncid, this%dim_ids, this%var_ids, this%disv, &
                           int2d, nc_varname, pkgname, idt%tagname, gridmap, &
                           idt%shape, this%iout)
    case ('INTEGER3D')
      errmsg = 'ODM Export int3d export called on DISV grid, tag='//trim(idt%tagname)
      call store_error(errmsg, .true.)
    case ('DOUBLE1D')
      call mem_setptr(dbl1d, idt%mf6varname, mempath)
      call nc_export_dbl1d(this%ncid, this%dim_ids, this%var_ids, this%disv, &
                           dbl1d, nc_varname, pkgname, idt%tagname, gridmap, &
                           idt%shape, this%iout)
    case ('DOUBLE2D')
      call mem_setptr(dbl2d, idt%mf6varname, mempath)
      call nc_export_dbl2d(this%ncid, this%dim_ids, this%var_ids, this%disv, &
                           dbl2d, nc_varname, pkgname, idt%tagname, gridmap, &
                           idt%shape, this%iout)
    case ('DOUBLE3D')
      errmsg = 'ODM Export dbl3d export called on DISV grid, tag='//trim(idt%tagname)
      call store_error(errmsg, .true.)
    case default
      ! -- no-op, no other datatypes exported
    end select
  end subroutine export_input_array

  subroutine create_coord_var(this)
    use TdisModule, only: datetime0
    class(Ugrid2dDisvModelType), intent(inout) :: this
    character(len=LINELENGTH) :: datetime, lenunits
    integer(I4B), dimension(:), contiguous, pointer :: ncvert
    !
    if (datetime0 == '') then
      errmsg = 'TDIS parameter START_DATE_TIME required for NetCDF export.'
      call store_error(errmsg, .true.)
    else
      datetime = 'days since '//trim(datetime0)//' 0:0:0'
    end if

    lenunits = 'm'
    if (this%disv%lenuni == 1) then
      write(this%iout, '(a)') 'ODM WARN transform length in feet'
    else if (this%disv%lenuni == 2) then
      write(this%iout, '(a)') 'ODM lenuni meters'
    else if (this%disv%lenuni == 3) then
      write(this%iout, '(a)') 'ODM WARN transform length in cm'
    else
      write(this%iout, '(a)') 'ODM WARN unknown lenuni'
    end if
    !
    ! -- set pointers to input context
    call mem_setptr(ncvert, 'NCVERT', this%indis_mempath)

    ! -- TODO: fix units
    ! bound dim
    !call nf_verify(nf90_def_dim(this%ncid, 'bnd', 2, this%dim_ids%bnd), &
    !               this%ncid, this%iout)
    !
    ! -- Time
    call nf_verify(nf90_def_dim(this%ncid, 'time', NF90_UNLIMITED, &
                                this%dim_ids%time), this%ncid, this%iout)
    call nf_verify(nf90_def_var(this%ncid, 'time', NF90_DOUBLE, this%dim_ids%time, &
                                this%var_ids%time), this%ncid, this%iout)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%time, 'calendar', &
                                'standard'), this%ncid, this%iout)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%time, 'units', &
                                datetime), this%ncid, this%iout)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%time, 'axis', 'T'), &
                   this%ncid, this%iout)
    !call nf_verify(nf90_put_att(ncid, var_ids%time, 'bounds', 'time_bnds'), ncid, iout)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%time, 'standard_name', &
                                'time'), this%ncid, this%iout)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%time, 'long_name', &
                                'time'), this%ncid, this%iout)
    !
    ! -- mesh
    call nf_verify(nf90_def_dim(this%ncid, 'nmesh_node', this%disv%nvert, &
                                this%dim_ids%nmesh_node), this%ncid, this%iout)
    call nf_verify(nf90_def_dim(this%ncid, 'nmesh_face', this%disv%ncpl, &
                                this%dim_ids%nmesh_face), this%ncid, this%iout)
    call nf_verify(nf90_def_dim(this%ncid, 'max_nmesh_face_nodes', maxval(ncvert), &
                                this%dim_ids%max_nmesh_face_nodes), this%ncid, this%iout)
    !
    ! -- ncpl, nlay
    call nf_verify(nf90_def_dim(this%ncid, 'nlay', this%disv%nlay, &
                                this%dim_ids%nlay), this%ncid, this%iout)
    call nf_verify(nf90_def_dim(this%ncid, 'ncpl', this%disv%ncpl, &
                                this%dim_ids%ncpl), this%ncid, this%iout)
  end subroutine create_coord_var

  subroutine add_mesh_data(this)
    class(Ugrid2dDisvModelType), intent(inout) :: this
    integer(I4B), dimension(:), contiguous, pointer :: icell2d => null()
    integer(I4B), dimension(:), contiguous, pointer :: ncvert => null()
    integer(I4B), dimension(:), contiguous, pointer :: icvert => null()
    real(DP), dimension(:), contiguous, pointer :: cell_x => null()
    real(DP), dimension(:), contiguous, pointer :: cell_y => null()
    real(DP), dimension(:), contiguous, pointer :: vert_x => null()
    real(DP), dimension(:), contiguous, pointer :: vert_y => null()
    integer(I4B), dimension(:), contiguous, pointer :: ip
    integer(I4B) :: n, m, idx, cnt, iv, maxvert
    !integer(I4B), dimension(4) :: verts
    integer(I4B), dimension(:), allocatable :: verts
    real(DP), dimension(:), allocatable :: bnds
    integer(I4B) :: k, i, j, istop
    !
    ! -- set pointers to input context
    ! -- TODO: should the data come from the disv object
    ! -- TODO: does this data ignore xorigin/yorigin?
    call mem_setptr(icell2d, 'ICELL2D', this%indis_mempath)
    call mem_setptr(ncvert, 'NCVERT', this%indis_mempath)
    call mem_setptr(icvert, 'ICVERT', this%indis_mempath)
    call mem_setptr(cell_x, 'XC', this%indis_mempath)
    call mem_setptr(cell_y, 'YC', this%indis_mempath)
    call mem_setptr(vert_x, 'XV', this%indis_mempath)
    call mem_setptr(vert_y, 'YV', this%indis_mempath)


    write(this%iout, '(a)') 'Ugrid2dDisvModelType mesh'
    ! -- mesh
    call nf_verify(nf90_put_var(this%ncid, this%var_ids%mesh, 1), &
                   this%ncid, this%iout)

    ! -- node y and x
    call nf_verify(nf90_put_var(this%ncid, this%var_ids%mesh_node_x, vert_x), &
                   this%ncid, this%iout)
    call nf_verify(nf90_put_var(this%ncid, this%var_ids%mesh_node_y, vert_y), &
                   this%ncid, this%iout)

    ! -- face y and x
    call nf_verify(nf90_put_var(this%ncid, this%var_ids%mesh_face_x, cell_x), &
                   this%ncid, this%iout)
    call nf_verify(nf90_put_var(this%ncid, this%var_ids%mesh_face_y, cell_y), &
                   this%ncid, this%iout)

    write(this%iout, '(a)') 'Ugrid2dDisvModelType face_nodes bound arrays'
    ! populate mesh_face_nodes and bound arrays
    maxvert = maxval(ncvert)
    allocate (verts(maxvert))
    allocate (bnds(maxvert))
    cnt = 0
    do n = 1, size(ncvert)
      verts = NF90_FILL_INT
      idx = cnt + ncvert(n)
      iv = 0
      istop = cnt + 1
      do m = idx, istop, -1
        cnt = cnt + 1
        iv = iv + 1
        verts(iv) = icvert(m)
      end do

      ! -- face nodes
      call nf_verify(nf90_put_var(this%ncid, this%var_ids%mesh_face_nodes, &
                                  verts, start=(/1, n/), &
                                  count=(/maxvert, 1/)), &
                                  this%ncid, this%iout)
      ! -- face y bnds
      bnds = NF90_FILL_DOUBLE
      do m = 1, size(bnds)
        if (verts(m) /= NF90_FILL_INT) then
          bnds(m) = vert_y(verts(m))
        end if
        call nf_verify(nf90_put_var(this%ncid, this%var_ids%mesh_face_ybnds, &
                                    bnds, start=(/1, n/), &
                                    count=(/maxvert, 1/)), &
                                    this%ncid, this%iout)
      end do

      ! -- face x bnds
      bnds = NF90_FILL_DOUBLE
      do m = 1, size(bnds)
        if (verts(m) /= NF90_FILL_INT) then
          bnds(m) = vert_x(verts(m))
        end if
        call nf_verify(nf90_put_var(this%ncid, this%var_ids%mesh_face_xbnds, &
                                    bnds, start=(/1, n/), &
                                    count=(/maxvert, 1/)), &
                                    this%ncid, this%iout)
      end do
    end do
    !
    ! -- cleanup
    deallocate (bnds)
    deallocate (verts)
  end subroutine add_mesh_data

  ! TODO for all export variable naming:
  !     make consistent with mempath naming-
  !     check multi_package_type or use split_mem_path
  !     e.g. now variables can be written like: DISV-1 TOP
  subroutine nc_export_int1d(ncid, dim_ids, var_ids, dis, p_mem, nc_varname, &
                             pkgname, tagname, gridmap_name, shapestr, iout)
    use InputOutputModule, only: lowcase
    integer(I4B), intent(in) :: ncid
    type(UgridModelNCDimIdType), intent(inout) :: dim_ids
    type(UgridModelNCVarIdType), intent(inout) :: var_ids
    type(DisvType), pointer, intent(in) :: dis
    integer(I4B), dimension(:), pointer, contiguous, intent(in) :: p_mem
    character(len=*), intent(in) :: nc_varname
    character(len=*), intent(in) :: pkgname
    character(len=*), intent(in) :: tagname
    character(len=*), intent(in) :: gridmap_name
    character(len=*), intent(in) :: shapestr
    integer(I4B), intent(in) :: iout
    ! -- local
    integer(I4B), dimension(2) :: dis_shape
    integer(I4B), dimension(:, :), pointer, contiguous :: int2d
    integer(I4B) :: axis_sz, k, i, j
    integer(I4B), dimension(:), allocatable :: var_id
    character(len=LINELENGTH) :: longname, varname_l
    !
    if (shapestr == 'NCPL') then
      ! -- set long_name attribute
      !longname = trim(pkgname)//' input array for '//trim(tagname)
      longname = trim(pkgname)//' '//trim(tagname)
      !
      allocate (var_id(1))
      axis_sz = dim_ids%nmesh_face
      !
      ! -- reenter define mode and create variable
      call nf_verify(nf90_redef(ncid), ncid, iout)
      call nf_verify(nf90_def_var(ncid, nc_varname, NF90_INT, &
                                  (/axis_sz/), var_id(1)), &
                                  ncid, iout)
      call nf_verify(nf90_put_att(ncid, var_id(1), '_FillValue', &
                                  (/NF90_FILL_INT/)), ncid, iout)
      call nf_verify(nf90_put_att(ncid, var_id(1), 'long_name', &
                                  longname), ncid, iout)
      if (gridmap_name /= '') then
        call nf_verify(nf90_put_att(ncid, var_id(1), 'coordinates', &
                                    'mesh_face_x mesh_face_y'), ncid, iout)
        call nf_verify(nf90_put_att(ncid, var_id(1), 'grid_mapping', gridmap_name), &
                       ncid, iout)
      end if
      !
      ! -- exit define mode and write data
      call nf_verify(nf90_enddef(ncid), ncid, iout)
      call nf_verify(nf90_put_var(ncid, var_id(1), p_mem), &
                                  ncid, iout)

    else
      allocate (var_id(dis%nlay))
      !
      ! -- allocate temporary 3d and reshape input
      dis_shape(1) = dis%ncpl
      dis_shape(2) = dis%nlay
      allocate (int2d(dis_shape(1), dis_shape(2)))
      int2d = reshape(p_mem, dis_shape)
      !
      ! TODO: don't idomain filter for input right?
      do k = 1, dis%nlay
        do j = 1, dis%ncpl
          if (dis%idomain(j, k) < 1) then
            ! TODO: fix
            !int2d(j, k) = NF90_FILL_INT
          end if
        end do
      end do
      !
      ! -- reenter define mode and create variable
      call nf_verify(nf90_redef(ncid), ncid, iout)
      do k = 1, dis%nlay
        write(varname_l, '(a,i0)') trim(nc_varname)//'_l', k
        write(longname, '(a,i0)') trim(pkgname)//' '//trim(tagname)//' layer ', k
        call nf_verify(nf90_def_var(ncid, varname_l, NF90_INT, &
                                    (/dim_ids%nmesh_face/), var_id(k)), &
                                    ncid, iout)
        call nf_verify(nf90_put_att(ncid, var_id(k), '_FillValue', &
                                    (/NF90_FILL_INT/)), ncid, iout)
        call nf_verify(nf90_put_att(ncid, var_id(k), 'long_name', &
                                    longname), ncid, iout)
        if (gridmap_name /= '') then
          call nf_verify(nf90_put_att(ncid, var_id(k), 'coordinates', &
                                      'mesh_face_x mesh_face_y'), ncid, iout)
          call nf_verify(nf90_put_att(ncid, var_id(k), 'grid_mapping', gridmap_name), &
                         ncid, iout)
        end if
      end do
      !
      ! -- exit define mode and write data
      call nf_verify(nf90_enddef(ncid), ncid, iout)
      do k = 1, dis%nlay
        call nf_verify(nf90_put_var(ncid, var_id(k), int2d(:,k)), ncid, iout)
      end do
      !
      ! -- cleanup
     deallocate (int2d)
     deallocate (var_id)
    end if
  end subroutine nc_export_int1d

  !> @brief Create export file int2d
  !<
  subroutine nc_export_int2d(ncid, dim_ids, var_ids, disv, p_mem, nc_varname, &
                             pkgname, tagname, gridmap_name, shapestr, iout)
    integer(I4B), intent(in) :: ncid
    type(UgridModelNCDimIdType), intent(inout) :: dim_ids
    type(UgridModelNCVarIdType), intent(inout) :: var_ids
    type(DisvType), pointer, intent(in) :: disv
    integer(I4B), dimension(:, :), pointer, contiguous, intent(in) :: p_mem
    character(len=*), intent(in) :: nc_varname
    character(len=*), intent(in) :: pkgname
    character(len=*), intent(in) :: tagname
    character(len=*), intent(in) :: gridmap_name
    character(len=*), intent(in) :: shapestr
    integer(I4B), intent(in) :: iout
    ! -- local
    integer(I4B), dimension(:), allocatable :: var_id
    character(len=LINELENGTH) :: longname, varname_l
    integer(I4B) :: k
    !
    allocate (var_id(disv%nlay))
    !
    ! -- reenter define mode and create variable
    call nf_verify(nf90_redef(ncid), ncid, iout)
    do k = 1, disv%nlay
      write(varname_l, '(a,i0)') trim(nc_varname)//'_l', k
      write(longname, '(a,i0)') trim(pkgname)//' '//trim(tagname)//' layer ', k
      call nf_verify(nf90_def_var(ncid, varname_l, NF90_INT, &
                                  (/dim_ids%nmesh_face/), var_id(k)), &
                                  ncid, iout)
      call nf_verify(nf90_put_att(ncid, var_id(k), '_FillValue', &
                                  (/NF90_FILL_INT/)), ncid, iout)
      call nf_verify(nf90_put_att(ncid, var_id(k), 'long_name', &
                                  longname), ncid, iout)
      if (gridmap_name /= '') then
        call nf_verify(nf90_put_att(ncid, var_id(k), 'coordinates', &
                                    'mesh_face_x mesh_face_y'), ncid, iout)
        call nf_verify(nf90_put_att(ncid, var_id(k), 'grid_mapping', gridmap_name), &
                       ncid, iout)
      end if
    end do
    !
    ! -- exit define mode and write data
    call nf_verify(nf90_enddef(ncid), ncid, iout)
    do k = 1, disv%nlay
      call nf_verify(nf90_put_var(ncid, var_id(k), p_mem(:,k)), ncid, iout)
    end do
    !
    deallocate (var_id)
  end subroutine nc_export_int2d

  subroutine nc_export_dbl1d(ncid, dim_ids, var_ids, dis, p_mem, nc_varname, &
                             pkgname, tagname, gridmap_name, shapestr, iout)
    use InputOutputModule, only: lowcase
    integer(I4B), intent(in) :: ncid
    type(UgridModelNCDimIdType), intent(inout) :: dim_ids
    type(UgridModelNCVarIdType), intent(inout) :: var_ids
    type(DisvType), pointer, intent(in) :: dis
    real(DP), dimension(:), pointer, contiguous, intent(in) :: p_mem
    character(len=*), intent(in) :: nc_varname
    character(len=*), intent(in) :: pkgname
    character(len=*), intent(in) :: tagname
    character(len=*), intent(in) :: gridmap_name
    character(len=*), intent(in) :: shapestr
    integer(I4B), intent(in) :: iout
    ! -- local
    integer(I4B), dimension(2) :: dis_shape
    real(DP), dimension(:, :), pointer, contiguous :: dbl2d
    integer(I4B) :: axis_sz, k, i, j
    integer(I4B), dimension(:), allocatable :: var_id
    character(len=LINELENGTH) :: longname, varname_l
    !
    if (shapestr == 'NCPL') then
      ! -- set long_name attribute
      !longname = trim(pkgname)//' input array for '//trim(tagname)
      longname = trim(pkgname)//' '//trim(tagname)
      !
      allocate (var_id(1))
      axis_sz = dim_ids%nmesh_face
      !
      ! -- reenter define mode and create variable
      call nf_verify(nf90_redef(ncid), ncid, iout)
      call nf_verify(nf90_def_var(ncid, nc_varname, NF90_DOUBLE, &
                                  (/axis_sz/), var_id(1)), &
                                  ncid, iout)
      call nf_verify(nf90_put_att(ncid, var_id(1), '_FillValue', &
                                  (/NF90_FILL_DOUBLE/)), ncid, iout)
      call nf_verify(nf90_put_att(ncid, var_id(1), 'long_name', &
                                  longname), ncid, iout)
      if (gridmap_name /= '') then
        call nf_verify(nf90_put_att(ncid, var_id(1), 'coordinates', &
                                    'mesh_face_x mesh_face_y'), ncid, iout)
        call nf_verify(nf90_put_att(ncid, var_id(1), 'grid_mapping', gridmap_name), &
                       ncid, iout)
      end if
      !
      ! -- exit define mode and write data
      call nf_verify(nf90_enddef(ncid), ncid, iout)
      call nf_verify(nf90_put_var(ncid, var_id(1), p_mem), &
                                  ncid, iout)

    else
      allocate (var_id(dis%nlay))
      !
      ! -- allocate temporary 3d and reshape input
      dis_shape(1) = dis%ncpl
      dis_shape(2) = dis%nlay
      allocate (dbl2d(dis_shape(1), dis_shape(2)))
      dbl2d = reshape(p_mem, dis_shape)
      !
      ! TODO: don't idomain filter for input right?
      do k = 1, dis%nlay
        do j = 1, dis%ncpl
          if (dis%idomain(j, k) < 1) then
            ! TODO: fix
            !dbl2d(j, k) = NF90_FILL_DOUBLE
          end if
        end do
      end do
      !
      ! -- reenter define mode and create variable
      call nf_verify(nf90_redef(ncid), ncid, iout)
      do k = 1, dis%nlay
        write(varname_l, '(a,i0)') trim(nc_varname)//'_l', k
        write(longname, '(a,i0)') trim(pkgname)//' '//trim(tagname)//' layer ', k
        call nf_verify(nf90_def_var(ncid, varname_l, NF90_DOUBLE, &
                                    (/dim_ids%nmesh_face/), var_id(k)), &
                                    ncid, iout)
        call nf_verify(nf90_put_att(ncid, var_id(k), '_FillValue', &
                                    (/NF90_FILL_DOUBLE/)), ncid, iout)
        call nf_verify(nf90_put_att(ncid, var_id(k), 'long_name', &
                                    longname), ncid, iout)
        if (gridmap_name /= '') then
          call nf_verify(nf90_put_att(ncid, var_id(k), 'coordinates', &
                                      'mesh_face_x mesh_face_y'), ncid, iout)
          call nf_verify(nf90_put_att(ncid, var_id(k), 'grid_mapping', gridmap_name), &
                         ncid, iout)
        end if
      end do
      !
      ! -- exit define mode and write data
      call nf_verify(nf90_enddef(ncid), ncid, iout)
      do k = 1, dis%nlay
        call nf_verify(nf90_put_var(ncid, var_id(k), dbl2d(:,k)), ncid, iout)
      end do
      !
      ! -- cleanup
     deallocate (dbl2d)
     deallocate (var_id)
    end if
  end subroutine nc_export_dbl1d

  !> @brief Create export file dbl2d
  !<
  subroutine nc_export_dbl2d(ncid, dim_ids, var_ids, disv, p_mem, nc_varname, &
                             pkgname, tagname, gridmap_name, shapestr, iout)
    integer(I4B), intent(in) :: ncid
    type(UgridModelNCDimIdType), intent(inout) :: dim_ids
    type(UgridModelNCVarIdType), intent(inout) :: var_ids
    type(DisvType), pointer, intent(in) :: disv
    real(DP), dimension(:, :), pointer, contiguous, intent(in) :: p_mem
    character(len=*), intent(in) :: nc_varname
    character(len=*), intent(in) :: pkgname
    character(len=*), intent(in) :: tagname
    character(len=*), intent(in) :: gridmap_name
    character(len=*), intent(in) :: shapestr
    integer(I4B), intent(in) :: iout
    ! -- local
    integer(I4B), dimension(:), allocatable :: var_id
    character(len=LINELENGTH) :: longname, varname_l
    integer(I4B) :: k
    !
    allocate (var_id(disv%nlay))
    !
    ! -- reenter define mode and create variable
    call nf_verify(nf90_redef(ncid), ncid, iout)
    do k = 1, disv%nlay
      write(varname_l, '(a,i0)') trim(nc_varname)//'_l', k
      write(longname, '(a,i0)') trim(pkgname)//' '//trim(tagname)//' layer ', k
      call nf_verify(nf90_def_var(ncid, varname_l, NF90_DOUBLE, &
                                  (/dim_ids%nmesh_face/), var_id(k)), &
                                  ncid, iout)
      call nf_verify(nf90_put_att(ncid, var_id(k), '_FillValue', &
                                  (/NF90_FILL_DOUBLE/)), ncid, iout)
      call nf_verify(nf90_put_att(ncid, var_id(k), 'long_name', &
                                  longname), ncid, iout)
      if (gridmap_name /= '') then
        call nf_verify(nf90_put_att(ncid, var_id(k), 'coordinates', &
                                    'mesh_face_x mesh_face_y'), ncid, iout)
        call nf_verify(nf90_put_att(ncid, var_id(k), 'grid_mapping', gridmap_name), &
                       ncid, iout)
      end if
    end do
    !
    ! -- exit define mode and write data
    call nf_verify(nf90_enddef(ncid), ncid, iout)
    do k = 1, disv%nlay
      call nf_verify(nf90_put_var(ncid, var_id(k), p_mem(:,k)), ncid, iout)
    end do
    !
    deallocate (var_id)
  end subroutine nc_export_dbl2d

end module UgridDisvModelModule
