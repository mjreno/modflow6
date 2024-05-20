!> @brief This module contains the UgridDisModelModule
!!
!! Export DISV based model NetCDF file based on UGRID layered mesh paradigm
!!
!<
module UgridDisModelModule

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
  use DisModule, only: DisType
  use netcdf

  implicit none
  private
  public :: Ugrid2dDisModelType

  ! -- UGRID layered mesh (ULM) DIS
  type, extends(Ugrid2dModelType) :: Ugrid2dDisModelType
    class(DisType), pointer :: dis => null() !discretization object
  contains
    procedure :: init => dis_export_init
    procedure :: destroy => dis_export_destroy
    procedure :: df
    procedure :: step
    procedure :: export_input_array
    procedure :: create_coord_var
    procedure :: add_mesh_data
  end type Ugrid2dDisModelType

contains

  !> @brief netcdf export dis model init
  !<
  subroutine dis_export_init(this, modelname, modeltype, modelfname, disenum, ugrid, iout)
    use ArrayHandlersModule, only: expandarray
    class(Ugrid2dDisModelType), intent(inout) :: this
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
    allocate (this%var_ids%dependent(this%dis%nlay))
    !
    ! -- set nlay
    this%nlay = this%dis%nlay
    !
    ! -- create the netcdf file
    ! TODO: don't create file until sanity checks (START_DATE_TIME, etc) are done
    call nf_verify(nf90_create(this%nc_filename, NF90_CLOBBER, this%ncid), &
                   this%ncid, this%iout)
  end subroutine dis_export_init

  subroutine dis_export_destroy(this)
    use SimVariablesModule, only: idm_context
    use MemoryManagerExtModule, only: memorylist_remove
    class(Ugrid2dDisModelType), intent(inout) :: this
    call nf_verify(nf90_close(this%ncid), this%ncid, this%iout)
    !
    ! -- Deallocate idm memory
    if (this%ncf6_mempath /= '') then
      call memorylist_remove(this%modelname, 'NCF', idm_context)
    end if
  end subroutine dis_export_destroy

  subroutine df(this)
    class(Ugrid2dDisModelType), intent(inout) :: this

    write(this%iout, '(a)') 'Ugrid2dDisModelType create_global_att()'
    call this%create_global_att()

    write(this%iout, '(a)') 'Ugrid2dDisModelType create_coord_var()'
    call this%create_coord_var()

    write(this%iout, '(a)') 'Ugrid2dDisModelType create_mesh()'
    call this%create_mesh()

    write(this%iout, '(a)') 'Ugrid2dDisModelType create_dep_var()'
    call this%create_dep_var()

    write(this%iout, '(a)') 'Ugrid2dDisModelType nf90_enddef()'
    call nf_verify(nf90_enddef(this%ncid), this%ncid, this%iout)

    write(this%iout, '(a)') 'Ugrid2dDisModelType add_mesh_data()'
    call this%add_mesh_data()

    write(this%iout, '(a)') 'Ugrid2dDisModelType add_pkg_data()'
    call this%add_pkg_data()

    write(this%iout, '(a)') 'Ugrid2dDisModelType create_gridmap_var()'
    call this%create_gridmap_var()
  end subroutine df

  subroutine step(this)
    use TdisModule, only: totim
    class(Ugrid2dDisModelType), intent(inout) :: this
    real(DP), dimension(:), pointer, contiguous :: dbl1d
    integer(I4B) :: n, k
    integer(I4B), dimension(2) :: dis_shape
    real(I4B), dimension(:, :), pointer, contiguous :: dbl2d
    !
    ! -- increment step
    this%stepcnt = this%stepcnt + 1
    !
    ! -- add data to dependendent variable
    if (size(this%dis%nodeuser) < &
        size(this%dis%nodereduced)) then
      allocate (dbl1d(size(this%dis%nodereduced)))
      dbl1d = NF90_FILL_DOUBLE
      do n = 1, size(this%dis%nodereduced)
        if (this%dis%nodereduced(n) > 0) then
          dbl1d(n) = this%x(this%dis%nodereduced(n))
        end if
      end do
      !
      dis_shape(1) = this%dis%ncol * this%dis%nrow
      dis_shape(2) = this%dis%nlay
      allocate (dbl2d(dis_shape(1), dis_shape(2)))
      dbl2d = reshape(dbl1d, dis_shape)
      !
      do k = 1, this%dis%nlay
        call nf_verify(nf90_put_var(this%ncid, &
                                    this%var_ids%dependent(k), dbl2d(:,k), &
                                    start=(/1, this%stepcnt/), &
                                    count=(/(this%dis%ncol * this%dis%nrow), 1/)), &
                                    this%ncid, this%iout)
      end do

      deallocate (dbl1d)
      deallocate (dbl2d)
    else
      !
      dis_shape(1) = this%dis%ncol * this%dis%nrow
      dis_shape(2) = this%dis%nlay
      allocate (dbl2d(dis_shape(1), dis_shape(2)))
      dbl2d = reshape(this%x, dis_shape)
      do k = 1, this%dis%nlay
        call nf_verify(nf90_put_var(this%ncid, &
                                    this%var_ids%dependent(k), dbl2d(:, k), &
                                    start=(/1, this%stepcnt/), &
                                    count=(/this%dis%ncol * this%dis%nrow, 1/)), &
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
    class(Ugrid2dDisModelType), intent(inout) :: this
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
      call nc_export_int1d(this%ncid, this%dim_ids, this%var_ids, this%dis, &
                           int1d, nc_varname, pkgname, idt%tagname, gridmap, &
                           idt%shape, this%iout)
    case ('INTEGER2D')
      call mem_setptr(int2d, idt%mf6varname, mempath)
      call nc_export_int2d(this%ncid, this%dim_ids, this%var_ids, this%dis, &
                           int2d, nc_varname, pkgname, idt%tagname, gridmap, &
                           idt%shape, this%iout)
    case ('INTEGER3D')
      call mem_setptr(int3d, idt%mf6varname, mempath)
      call nc_export_int3d(this%ncid, this%dim_ids, this%var_ids, this%dis, &
                           int3d, nc_varname, pkgname, idt%tagname, gridmap, &
                           idt%shape, this%iout)
    case ('DOUBLE1D')
      call mem_setptr(dbl1d, idt%mf6varname, mempath)
      call nc_export_dbl1d(this%ncid, this%dim_ids, this%var_ids, this%dis, &
                           dbl1d, nc_varname, pkgname, idt%tagname, gridmap, &
                           idt%shape, this%iout)
    case ('DOUBLE2D')
      call mem_setptr(dbl2d, idt%mf6varname, mempath)
      call nc_export_dbl2d(this%ncid, this%dim_ids, this%var_ids, this%dis, &
                           dbl2d, nc_varname, pkgname, idt%tagname, gridmap, &
                           idt%shape, this%iout)
    case ('DOUBLE3D')
      call mem_setptr(dbl3d, idt%mf6varname, mempath)
      call nc_export_dbl3d(this%ncid, this%dim_ids, this%var_ids, this%dis, &
                           dbl3d, nc_varname, pkgname, idt%tagname, gridmap, &
                           idt%shape, this%iout)
    case default
      ! -- no-op, no other datatypes exported
    end select
  end subroutine export_input_array

  subroutine create_coord_var(this)
    use TdisModule, only: datetime0
    class(Ugrid2dDisModelType), intent(inout) :: this
    character(len=LINELENGTH) :: datetime, lenunits
    !
    if (datetime0 == '') then
      errmsg = 'TDIS parameter START_DATE_TIME required for NetCDF export.'
      call store_error(errmsg, .true.)
    else
      datetime = 'days since '//trim(datetime0)//' 0:0:0'
    end if

    lenunits = 'm'
    if (this%dis%lenuni == 1) then
      write(this%iout, '(a)') 'ODM WARN transform length in feet'
    else if (this%dis%lenuni == 2) then
      write(this%iout, '(a)') 'ODM lenuni meters'
    else if (this%dis%lenuni == 3) then
      write(this%iout, '(a)') 'ODM WARN transform length in cm'
    else
      write(this%iout, '(a)') 'ODM WARN unknown lenuni'
    end if

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
    call nf_verify(nf90_def_dim(this%ncid, 'nmesh_node', ((this%dis%ncol + 1) * (this%dis%nrow + 1)), &
                                this%dim_ids%nmesh_node), this%ncid, this%iout)
    call nf_verify(nf90_def_dim(this%ncid, 'nmesh_face', (this%dis%ncol * this%dis%nrow), &
                                this%dim_ids%nmesh_face), this%ncid, this%iout)
    call nf_verify(nf90_def_dim(this%ncid, 'max_nmesh_face_nodes', 4, &
                                this%dim_ids%max_nmesh_face_nodes), this%ncid, this%iout)
    !
    ! -- x, y, nlay
    call nf_verify(nf90_def_dim(this%ncid, 'nlay', this%dis%nlay, &
                                this%dim_ids%nlay), this%ncid, this%iout)
    call nf_verify(nf90_def_dim(this%ncid, 'x', this%dis%ncol, &
                                this%dim_ids%x), this%ncid, this%iout)
    call nf_verify(nf90_def_dim(this%ncid, 'y', this%dis%nrow, &
                                this%dim_ids%y), this%ncid, this%iout)
  end subroutine create_coord_var

  subroutine add_mesh_data(this)
    class(Ugrid2dDisModelType), intent(inout) :: this
    integer(I4B), dimension(:), contiguous, pointer :: ip
    integer(I4B) :: n, m, idx, cnt, iv, maxvert
    !integer(I4B), dimension(4) :: verts
    integer(I4B), dimension(:), allocatable :: verts
    real(DP), dimension(:), allocatable :: bnds
    integer(I4B) :: k, i, j, istop, x, y
    real(DP), dimension(:), allocatable :: node_x, node_y
    real(DP), dimension(:), allocatable :: cell_x, cell_y

    ! -- mesh
    call nf_verify(nf90_put_var(this%ncid, this%var_ids%mesh, 1), &
                   this%ncid, this%iout)

    allocate (node_x(((this%dis%ncol + 1) * (this%dis%nrow + 1))))
    allocate (node_y(((this%dis%ncol + 1) * (this%dis%nrow + 1))))
    allocate (cell_x((this%dis%ncol * this%dis%nrow)))
    allocate (cell_y((this%dis%ncol * this%dis%nrow)))

    ! traverse from lower left corner across (x) and down (y)
    ! -- set node_x and node_x arrays
    cnt = 0
    node_x = NF90_FILL_DOUBLE
    node_y = NF90_FILL_DOUBLE
    y = this%dis%yorigin + sum(this%dis%delc)
    do j = this%dis%nrow, 0, -1
      x = this%dis%xorigin
      do i = this%dis%ncol, 0, -1
        cnt = cnt + 1
        node_x(cnt) = x
        node_y(cnt) = y
        if (i > 0) x = x + this%dis%delr(i)
      end do
      if (j > 0)  y = y - this%dis%delc(j)
    end do

    !
    call nf_verify(nf90_put_var(this%ncid, this%var_ids%mesh_node_x, node_x), &
                   this%ncid, this%iout)
    call nf_verify(nf90_put_var(this%ncid, this%var_ids%mesh_node_y, node_y), &
                   this%ncid, this%iout)

    ! traverse from lower left corner across (x) and down (y)
    cnt = 1
    cell_x = NF90_FILL_DOUBLE
    cell_y = NF90_FILL_DOUBLE
    do j = 1, this%dis%nrow
      x = this%dis%xorigin
      y = this%dis%celly(j) + this%dis%yorigin
      do i = 1, this%dis%ncol
        cell_x(cnt) = x
        cell_y(cnt) = y
        cnt = cnt + 1
        x = this%dis%cellx(i) + this%dis%xorigin
      end do
    end do

    !
    call nf_verify(nf90_put_var(this%ncid, this%var_ids%mesh_face_x, cell_x), &
                   this%ncid, this%iout)
    call nf_verify(nf90_put_var(this%ncid, this%var_ids%mesh_face_y, cell_y), &
                   this%ncid, this%iout)

    ! populate mesh_face_nodes and bound arrays
    !maxvert = maxval(ncvert)
    maxvert = 4
    allocate (verts(maxvert))
    allocate (bnds(maxvert))
    cnt = 0
    cnt = 0

    do i = 1, this%dis%nrow
      do j = 1, this%dis%ncol
        cnt = cnt + 1
        verts = NF90_FILL_INT
        verts(1) = cnt + this%dis%ncol + i
        verts(2) = cnt + this%dis%ncol + i + 1
        if (i > 1) then
          verts(3) = cnt + i
          verts(4) = cnt + i - 1
        else
          verts(3) = cnt + 1
          verts(4) = cnt
        end if

        ! -- face nodes
        call nf_verify(nf90_put_var(this%ncid, this%var_ids%mesh_face_nodes, &
                                    verts, start=(/1, cnt/), &
                                    count=(/maxvert, 1/)), &
                                    this%ncid, this%iout)

        ! -- face y bnds
      bnds = NF90_FILL_DOUBLE
      do m = 1, size(bnds)
        if (verts(m) /= NF90_FILL_INT) then
          bnds(m) = node_y(verts(m))
        end if
        call nf_verify(nf90_put_var(this%ncid, this%var_ids%mesh_face_ybnds, &
                                    bnds, start=(/1, cnt/), &
                                    count=(/maxvert, 1/)), &
                                    this%ncid, this%iout)
      end do

      ! -- face x bnds
      bnds = NF90_FILL_DOUBLE
      do m = 1, size(bnds)
        if (verts(m) /= NF90_FILL_INT) then
          bnds(m) = node_x(verts(m))
        end if
        call nf_verify(nf90_put_var(this%ncid, this%var_ids%mesh_face_xbnds, &
                                    bnds, start=(/1, cnt/), &
                                    count=(/maxvert, 1/)), &
                                    this%ncid, this%iout)
      end do
        ! -- face y bnds
!        bnds = NF90_FILL_DOUBLE
!        do m = 1, size(bnds)
!          if (verts(m) /= NF90_FILL_INT) then
!            bnds(m) = node_y(verts(m))
!          end if
!          call nf_verify(nf90_put_var(this%ncid, this%var_ids%mesh_face_ybnds, &
!                                      bnds, start=(/1, cnt/), &
!                                      count=(/maxvert, 1/)), &
!                                      this%ncid, this%iout)
!        end do

        ! -- face x bnds
!        bnds = NF90_FILL_DOUBLE
!        do m = 1, size(bnds)
!          if (verts(m) /= NF90_FILL_INT) then
!            bnds(m) = node_x(verts(m))
!          end if
!          call nf_verify(nf90_put_var(this%ncid, this%var_ids%mesh_face_xbnds, &
!                                      bnds, start=(/1, cnt/), &
!                                      count=(/maxvert, 1/)), &
!                                      this%ncid, this%iout)
!        end do

      end do
    end do
    deallocate (bnds)
    deallocate (verts)
    deallocate (node_x)
    deallocate (node_y)
    deallocate (cell_x)
    deallocate (cell_y)
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
    type(DisType), pointer, intent(in) :: dis
    integer(I4B), dimension(:), pointer, contiguous, intent(in) :: p_mem
    character(len=*), intent(in) :: nc_varname
    character(len=*), intent(in) :: pkgname
    character(len=*), intent(in) :: tagname
    character(len=*), intent(in) :: gridmap_name
    character(len=*), intent(in) :: shapestr
    integer(I4B), intent(in) :: iout
    ! -- local
    integer(I4B), dimension(3) :: dis_shape
    integer(I4B), dimension(1) :: layer_shape
    integer(I4B), dimension(:, :, :), pointer, contiguous :: int3d
    integer(I4B), dimension(:), pointer, contiguous :: int1d
    integer(I4B) :: axis_sz, ncpl, k, i, j
    integer(I4B), dimension(:), allocatable :: var_id
    character(len=LINELENGTH) :: longname, varname_l
    !
    if (shapestr == 'NROW' .or. &
        shapestr == 'NCOL') then
      !
      ! TODO don't write to netcdf file?
      !
      ! -- TODO: shapes other than the grid can't be mapped to the ugrid
      if (shapestr == 'NROW') then
        axis_sz = dim_ids%y
      else
        axis_sz = dim_ids%x
      end if
      ! -- set long_name attribute
      longname = trim(pkgname)//' '//trim(tagname)
      !
      allocate (var_id(1))
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
      ! -- shapes other than the grid can't be mapped to the ugrid
      !
      ! -- exit define mode and write data
      call nf_verify(nf90_enddef(ncid), ncid, iout)
      call nf_verify(nf90_put_var(ncid, var_id(1), p_mem), &
                                  ncid, iout)

    else
      allocate (var_id(dis%nlay))
      !
      ! -- allocate temporary 3d and reshape input
      dis_shape(1) = dis%ncol
      dis_shape(2) = dis%nrow
      dis_shape(3) = dis%nlay
      allocate (int3d(dis_shape(1), dis_shape(2), dis_shape(3)))
      int3d = reshape(p_mem, dis_shape)
      !
      ! TODO?: set idomain cells to fill?  all datatypes
      do k = 1, dis%nlay
        do i = 1, dis%nrow
          do j = 1, dis%ncol
            if (dis%idomain(j, i, k) < 1) then
              ! TODO: fix
              !int3d(j, i, k) = NF90_FILL_INT
            end if
          end do
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
      allocate (int1d(dis%nrow * dis%ncol))
      layer_shape(1) = dis%nrow * dis%ncol
      do k = 1, dis%nlay
        int1d = reshape(int3d(:,:,k), layer_shape)
        call nf_verify(nf90_put_var(ncid, var_id(k), int1d), ncid, iout)
      end do
      !
      ! -- cleanup
      deallocate (int1d)
      deallocate (int3d)
      deallocate (var_id)
    end if
  end subroutine nc_export_int1d

  !> @brief Create export file int2d
  !<
  subroutine nc_export_int2d(ncid, dim_ids, var_ids, dis, p_mem, nc_varname, &
                             pkgname, tagname, gridmap_name, shapestr, iout)
    integer(I4B), intent(in) :: ncid
    type(UgridModelNCDimIdType), intent(inout) :: dim_ids
    type(UgridModelNCVarIdType), intent(inout) :: var_ids
    type(DisType), pointer, intent(in) :: dis
    integer(I4B), dimension(:, :), pointer, contiguous, intent(in) :: p_mem
    character(len=*), intent(in) :: nc_varname
    character(len=*), intent(in) :: pkgname
    character(len=*), intent(in) :: tagname
    character(len=*), intent(in) :: gridmap_name
    character(len=*), intent(in) :: shapestr
    integer(I4B), intent(in) :: iout
    ! -- local
    integer(I4B) :: var_id
    integer(I4B), dimension(:), pointer, contiguous :: int1d
    integer(I4B), dimension(1) :: layer_shape
    character(len=LINELENGTH) :: longname
    !
    ! -- set long_name attribute
    longname = trim(pkgname)//' '//trim(tagname)
    !
    ! -- reenter define mode and create variable
    call nf_verify(nf90_redef(ncid), ncid, iout)
    call nf_verify(nf90_def_var(ncid, nc_varname, NF90_INT, &
                                (/dim_ids%nmesh_face/), var_id), &
                                ncid, iout)
    call nf_verify(nf90_put_att(ncid, var_id, '_FillValue', &
                                (/NF90_FILL_INT/)), ncid, iout)
    call nf_verify(nf90_put_att(ncid, var_id, 'long_name', &
                                  longname), ncid, iout)
    if (gridmap_name /= '') then
      call nf_verify(nf90_put_att(ncid, var_id, 'coordinates', &
                     'mesh_face_x mesh_face_y'), ncid, iout)
      call nf_verify(nf90_put_att(ncid, var_id, 'grid_mapping', gridmap_name), &
                     ncid, iout)
    end if
    !
    ! -- exit define mode and write data
    call nf_verify(nf90_enddef(ncid), ncid, iout)
    allocate (int1d(dis%nrow * dis%ncol))
    layer_shape(1) = dis%nrow * dis%ncol
    int1d = reshape(p_mem, layer_shape)
    call nf_verify(nf90_put_var(ncid, var_id, int1d), ncid, iout)
    deallocate (int1d)
  end subroutine nc_export_int2d

  !> @brief Create export file int3d
  !<
  subroutine nc_export_int3d(ncid, dim_ids, var_ids, dis, p_mem, nc_varname, &
                             pkgname, tagname, gridmap_name, shapestr, iout)
    use InputOutputModule, only: lowcase
    integer(I4B), intent(in) :: ncid
    type(UgridModelNCDimIdType), intent(inout) :: dim_ids
    type(UgridModelNCVarIdType), intent(inout) :: var_ids
    type(DisType), pointer, intent(in) :: dis
    integer(I4B), dimension(:, :, :), pointer, contiguous, intent(in) :: p_mem
    character(len=*), intent(in) :: nc_varname
    character(len=*), intent(in) :: pkgname
    character(len=*), intent(in) :: tagname
    character(len=*), intent(in) :: gridmap_name
    character(len=*), intent(in) :: shapestr
    integer(I4B), intent(in) :: iout
    ! -- local
    !integer(I4B) :: var_id
    !character(len=LINELENGTH) :: longname
    integer(I4B), dimension(:), allocatable :: var_id
    integer(I4B), dimension(:), pointer, contiguous :: int1d
    character(len=LINELENGTH) :: longname, varname_l
    integer(I4B), dimension(1) :: layer_shape
    integer(I4B) :: k
    !
    allocate (var_id(dis%nlay))
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
    allocate (int1d(dis%nrow * dis%ncol))
    layer_shape(1) = dis%nrow * dis%ncol
    do k = 1, dis%nlay
      int1d = reshape(p_mem(:,:,k), layer_shape)
      call nf_verify(nf90_put_var(ncid, var_id(k), int1d), ncid, iout)
    end do
    !
    deallocate (int1d)
    deallocate (var_id)
  end subroutine nc_export_int3d

  subroutine nc_export_dbl1d(ncid, dim_ids, var_ids, dis, p_mem, nc_varname, &
                             pkgname, tagname, gridmap_name, shapestr, iout)
    use InputOutputModule, only: lowcase
    integer(I4B), intent(in) :: ncid
    type(UgridModelNCDimIdType), intent(inout) :: dim_ids
    type(UgridModelNCVarIdType), intent(inout) :: var_ids
    type(DisType), pointer, intent(in) :: dis
    real(DP), dimension(:), pointer, contiguous, intent(in) :: p_mem
    character(len=*), intent(in) :: nc_varname
    character(len=*), intent(in) :: pkgname
    character(len=*), intent(in) :: tagname
    character(len=*), intent(in) :: gridmap_name
    character(len=*), intent(in) :: shapestr
    integer(I4B), intent(in) :: iout
    ! -- local
    integer(I4B), dimension(3) :: dis_shape
    integer(I4B), dimension(1) :: layer_shape
    real(DP), dimension(:, :, :), pointer, contiguous :: dbl3d
    real(DP), dimension(:), pointer, contiguous :: dbl1d
    integer(I4B) :: axis_sz, ncpl, k, i, j
    integer(I4B), dimension(:), allocatable :: var_id
    character(len=LINELENGTH) :: longname, varname_l
    !
    if (shapestr == 'NROW' .or. &
        shapestr == 'NCOL') then
      !
      ! TODO don't write to netcdf file?
      !
      if (shapestr == 'NROW') then
        axis_sz = dim_ids%y
      else
        axis_sz = dim_ids%x
      end if
      ! -- set long_name attribute
      longname = trim(pkgname)//' '//trim(tagname)
      !
      allocate (var_id(1))
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
      ! -- shapes other than the grid can't be mapped to the ugrid
      !
      ! -- exit define mode and write data
      call nf_verify(nf90_enddef(ncid), ncid, iout)
      call nf_verify(nf90_put_var(ncid, var_id(1), p_mem), &
                                  ncid, iout)

    else
      allocate (var_id(dis%nlay))
      !
      ! -- allocate temporary 3d and reshape input
      dis_shape(1) = dis%ncol
      dis_shape(2) = dis%nrow
      dis_shape(3) = dis%nlay
      allocate (dbl3d(dis_shape(1), dis_shape(2), dis_shape(3)))
      dbl3d = reshape(p_mem, dis_shape)
      !
      ! TODO?: set idomain cells to fill?  all datatypes
      ! TODO: not for input right?
      do k = 1, dis%nlay
        do i = 1, dis%nrow
          do j = 1, dis%ncol
            if (dis%idomain(j, i, k) < 1) then
              ! TODO: fix
              !dbl3d(j, i, k) = NF90_FILL_DOUBLE
            end if
          end do
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
      allocate (dbl1d(dis%nrow * dis%ncol))
      layer_shape(1) = dis%nrow * dis%ncol
      do k = 1, dis%nlay
        dbl1d = reshape(dbl3d(:,:,k), layer_shape)
        call nf_verify(nf90_put_var(ncid, var_id(k), dbl1d), ncid, iout)
      end do
      !
      ! -- cleanup
      deallocate (dbl1d)
      deallocate (dbl3d)
      deallocate (var_id)
    end if
  end subroutine nc_export_dbl1d

  !> @brief Create export file dbl2d
  !<
  subroutine nc_export_dbl2d(ncid, dim_ids, var_ids, dis, p_mem, nc_varname, &
                             pkgname, tagname, gridmap_name, shapestr, iout)
    integer(I4B), intent(in) :: ncid
    type(UgridModelNCDimIdType), intent(inout) :: dim_ids
    type(UgridModelNCVarIdType), intent(inout) :: var_ids
    type(DisType), pointer, intent(in) :: dis
    real(DP), dimension(:, :), pointer, contiguous, intent(in) :: p_mem
    character(len=*), intent(in) :: nc_varname
    character(len=*), intent(in) :: pkgname
    character(len=*), intent(in) :: tagname
    character(len=*), intent(in) :: gridmap_name
    character(len=*), intent(in) :: shapestr
    integer(I4B), intent(in) :: iout
    ! -- local
    integer(I4B) :: var_id
    character(len=LINELENGTH) :: longname
    real(DP), dimension(:), pointer, contiguous :: dbl1d
    integer(I4B), dimension(1) :: layer_shape
    !
    ! -- set long_name attribute
    longname = trim(pkgname)//' '//trim(tagname)
    !
    ! -- reenter define mode and create variable
    call nf_verify(nf90_redef(ncid), ncid, iout)
    call nf_verify(nf90_def_var(ncid, nc_varname, NF90_DOUBLE, &
                                  (/dim_ids%nmesh_face/), var_id), &
                                  ncid, iout)
    call nf_verify(nf90_put_att(ncid, var_id, '_FillValue', &
                                (/NF90_FILL_DOUBLE/)), ncid, iout)
    call nf_verify(nf90_put_att(ncid, var_id, 'long_name', &
                                  longname), ncid, iout)
    if (gridmap_name /= '') then
      call nf_verify(nf90_put_att(ncid, var_id, 'coordinates', &
                     'mesh_face_x mesh_face_y'), ncid, iout)
      call nf_verify(nf90_put_att(ncid, var_id, 'grid_mapping', gridmap_name), &
                     ncid, iout)
    end if
    !
    ! -- exit define mode and write data
    call nf_verify(nf90_enddef(ncid), ncid, iout)
    allocate (dbl1d(dis%nrow * dis%ncol))
    layer_shape(1) = dis%nrow * dis%ncol
    dbl1d = reshape(p_mem, layer_shape)
    call nf_verify(nf90_put_var(ncid, var_id, dbl1d), ncid, iout)
    deallocate (dbl1d)
  end subroutine nc_export_dbl2d

  !> @brief Create export file dbl3d
  !<
  subroutine nc_export_dbl3d(ncid, dim_ids, var_ids, dis, p_mem, nc_varname, &
                             pkgname, tagname, gridmap_name, shapestr, iout)
    use InputOutputModule, only: lowcase
    integer(I4B), intent(in) :: ncid
    type(UgridModelNCDimIdType), intent(inout) :: dim_ids
    type(UgridModelNCVarIdType), intent(inout) :: var_ids
    type(DisType), pointer, intent(in) :: dis
    real(DP), dimension(:, :, :), pointer, contiguous, intent(in) :: p_mem
    character(len=*), intent(in) :: nc_varname
    character(len=*), intent(in) :: pkgname
    character(len=*), intent(in) :: tagname
    character(len=*), intent(in) :: gridmap_name
    character(len=*), intent(in) :: shapestr
    integer(I4B), intent(in) :: iout
    ! -- local
    !integer(I4B) :: var_id
    !character(len=LINELENGTH) :: longname
    integer(I4B), dimension(:), allocatable :: var_id
    real(DP), dimension(:), pointer, contiguous :: dbl1d
    character(len=LINELENGTH) :: longname, varname_l
    integer(I4B), dimension(1) :: layer_shape
    integer(I4B) :: k
    !
    allocate (var_id(dis%nlay))
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
    allocate (dbl1d(dis%nrow * dis%ncol))
    layer_shape(1) = dis%nrow * dis%ncol
    do k = 1, dis%nlay
      dbl1d = reshape(p_mem(:,:,k), layer_shape)
      call nf_verify(nf90_put_var(ncid, var_id(k), dbl1d), ncid, iout)
    end do
    !
    deallocate (dbl1d)
    deallocate (var_id)
  end subroutine nc_export_dbl3d

end module UgridDisModelModule
