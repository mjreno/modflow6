!> @brief This module contains the OdmExportModule
!!
!!
!<
module NCExportModule

  use KindModule, only: DP, I4B, LGP
  use SimVariablesModule, only: errmsg
  use ConstantsModule, only: DIS, DISU, DISV
  use SimModule, only: store_error, store_error_filename
  use NumericalModelModule, only: NumericalModelType
  use BaseDisModule, only: DisBaseType
  use DisModule, only: DisType
  use DisvModule, only: DisvType
  use DisuModule, only: DisuType
  use OdmExportModelModule, only: export_models, GetExportModelIdx
  use OdmExportModelModule, only: ExportModelType

  implicit none
  private
  public :: nc_export_init
  public :: nc_export_post_step
  public :: nc_export_destroy
  public :: nc_version

contains

  subroutine create_nc_export(export_model, num_model, ugrid)
    use UgridDisModelModule, only: Ugrid2dDisModelType
    use UgridDisvModelModule, only: Ugrid2dDisvModelType
    type(ExportModelType), pointer, intent(inout) :: export_model
    class(NumericalModelType), pointer, intent(in) :: num_model
    logical(LGP), intent(in) :: ugrid
    class(Ugrid2dDisModelType),  pointer :: ugrid_dis
    class(Ugrid2dDisvModelType), pointer :: ugrid_disv
    class(DisBaseType), pointer :: disbase
    select case (export_model%disenum)
    case (DIS)
      ! -- allocate nc structured grid export object
      if (ugrid) then
        !
        ! -- allocate nc structured grid export object
        allocate (ugrid_dis)
        !
        ! -- set dis base type
        disbase => num_model%dis
        select type (disbase)
          type is (DisType)
            ugrid_dis%dis => disbase
        end select
        !
        ! -- initialize and define
        call ugrid_dis%init(export_model%modelname, export_model%modeltype, &
                            export_model%modelfname, export_model%disenum, &
                            ugrid, export_model%iout)
        call ugrid_dis%df()
        export_model%nc_export => ugrid_dis
      else
        errmsg = 'DIS discretization for NetCDF model export &
                 &only supported as UGRID export. &
                 &Model='//trim(export_model%modelname)//'.'
        call store_error(errmsg)
        call store_error_filename(export_model%modelfname)
      end if
    case (DISV)
      if (ugrid) then
        ! -- allocate nc structured grid export object
        allocate (ugrid_disv)
        !
        ! -- set dis base type
        disbase => num_model%dis
        select type (disbase)
          type is (DisvType)
            ugrid_disv%disv => disbase
        end select
        !
        ! -- initialize and define
        call ugrid_disv%init(export_model%modelname, export_model%modeltype, &
                             export_model%modelfname, export_model%disenum, &
                             ugrid, export_model%iout)
        call ugrid_disv%df()
        export_model%nc_export => ugrid_disv
      else
        errmsg = 'DISV discretization for NetCDF model export &
                 &only supported as UGRID export. &
                 &Model='//trim(export_model%modelname)//'.'
        call store_error(errmsg)
        call store_error_filename(export_model%modelfname)
      end if
    case default
      errmsg = 'Unsupported discretization for NetCDF model export. &
               &Model='//trim(export_model%modelname)//'.'
      call store_error(errmsg)
      call store_error_filename(export_model%modelfname)
    end select
  end subroutine create_nc_export

  subroutine nc_export_init()
    use NumericalModelModule, only: GetNumericalModelFromList
    use ListsModule, only: basemodellist
    integer(I4B) :: n
    type(ExportModelType), pointer :: export_model
    class(NumericalModelType), pointer :: num_model
    integer(I4B) :: im
    !
    do n = 1, export_models%Count()
      export_model => GetExportModelIdx(n)
      if (export_model%is_nc_export) then
        do im = 1, basemodellist%Count()
          num_model => GetNumericalModelFromList(basemodellist, im)
          if (num_model%name == export_model%modelname .and. &
              num_model%macronym == export_model%modeltype) then
            !
            ! -- allocate and initialize nc export model
            call create_nc_export(export_model, num_model, export_model%ugrid)
            exit
            !
          end if
        end do
      end if
    end do
  end subroutine nc_export_init

  subroutine nc_export_post_step()
    integer(I4B) :: n
    class(ExportModelType), pointer :: export_model
    !
    do n = 1, export_models%Count()
      export_model => GetExportModelIdx(n)
      if (associated(export_model%nc_export)) then
        call export_model%nc_export%step()
      end if
    end do
  end subroutine nc_export_post_step

  subroutine nc_export_destroy()
    integer(I4B) :: n
    class(ExportModelType), pointer :: export_model
    !
    do n = 1, export_models%Count()
      export_model => GetExportModelIdx(n)
      if (associated(export_model%nc_export)) then
        call export_model%nc_export%destroy()
        deallocate(export_model%nc_export)
        nullify (export_model%nc_export)
      end if
    end do
  end subroutine nc_export_destroy

  subroutine nc_version()
    use netcdf
    print *, 'NetCDF libversion='//trim(nf90_inq_libvers())
  end subroutine nc_version


end module NCExportModule
