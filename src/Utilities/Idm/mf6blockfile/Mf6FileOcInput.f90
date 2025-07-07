!> @brief This module contains the Mf6FileOcInputModule
!!
!! This module contains the routines for reading STO period block input
!!
!<
module Mf6FileOcInputModule

  use KindModule, only: I4B, DP, LGP
  use ConstantsModule, only: LINELENGTH
  use InputDefinitionModule, only: InputParamDefinitionType
  use MemoryManagerModule, only: mem_setptr, mem_allocate, mem_reallocate
  use ModflowInputModule, only: ModflowInputType
  use AsciiInputLoadTypeModule, only: AsciiDynamicPkgLoadBaseType
  use TdisModule, only: nper, kper, nstp
  use BlockParserModule, only: BlockParserType
  use CharacterStringModule, only: CharacterStringType

  implicit none
  private
  public :: OcInputType

  !> @brief STO package loader
  !!
  !<
  type, extends(AsciiDynamicPkgLoadBaseType) :: OcInputType
    type(CharacterStringType), dimension(:), pointer, contiguous :: ocaction
    type(CharacterStringType), dimension(:), pointer, contiguous :: rtype
    type(CharacterStringType), dimension(:), pointer, contiguous :: ocsetting
  contains
    procedure :: ainit => oc_init
    procedure :: df
    procedure :: rp
    procedure :: destroy
  end type OcInputType

contains

  subroutine oc_init(this, mf6_input, component_name, component_input_name, &
                     input_name, iperblock, parser, iout)
    use MemoryManagerExtModule, only: mem_set_value
    use BlockParserModule, only: BlockParserType
    use LoadMf6FileModule, only: LoadMf6FileType
    class(OcInputType), intent(inout) :: this
    type(ModflowInputType), intent(in) :: mf6_input
    character(len=*), intent(in) :: component_name
    character(len=*), intent(in) :: component_input_name
    character(len=*), intent(in) :: input_name
    integer(I4B), intent(in) :: iperblock
    type(BlockParserType), pointer, intent(inout) :: parser
    integer(I4B), intent(in) :: iout
    type(LoadMf6FileType) :: loader

    ! init loader
    call this%DynamicPkgLoadType%init(mf6_input, component_name, &
                                      component_input_name, input_name, &
                                      iperblock, iout)
    ! initialize static loader
    call loader%load(parser, mf6_input, this%nc_vars, this%input_name, iout)

    call mem_allocate(this%ocaction, LINELENGTH, 0, 'OCACTION', &
                      this%mf6_input%mempath)
    call mem_allocate(this%rtype, LINELENGTH, 0, 'RTYPE', &
                      this%mf6_input%mempath)
    call mem_allocate(this%ocsetting, LINELENGTH, 0, 'OCSETTING', &
                      this%mf6_input%mempath)
  end subroutine oc_init

  subroutine df(this)
    class(OcInputType), intent(inout) :: this
  end subroutine df

  subroutine rp(this, parser)
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: store_error, count_errors, store_error_filename
    use ArrayHandlersModule, only: expandarray
    use MemoryManagerModule, only: mem_deallocate
    class(OcInputType), intent(inout) :: this
    type(BlockParserType), pointer, intent(inout) :: parser
    character(len=LINELENGTH), dimension(:), allocatable :: ocaction
    character(len=LINELENGTH), dimension(:), allocatable :: rtype
    character(len=LINELENGTH), dimension(:), allocatable :: ocsetting
    character(len=LINELENGTH) :: tag
    character(len=:), allocatable :: line
    logical(lGP) :: endofblock
    integer(I4B) :: nline, n

    allocate (ocaction(0))
    allocate (rtype(0))
    allocate (ocsetting(0))
    nline = 0

    do
      ! read next line
      call parser%GetNextLine(endOfBlock)
      if (endOfBlock) exit

      nline = nline + 1

      ! reallocate
      call expandarray(ocaction)
      call expandarray(rtype)
      call expandarray(ocsetting)

      ! read action
      call parser%GetStringCaps(tag)
      ocaction(nline) = tag

      ! read rtype
      call parser%GetStringCaps(tag)
      rtype(nline) = tag

      ! read setting
      call parser%GetRemainingLine(line)
      ocsetting(nline) = line
      deallocate (line)
    end do

    ! reallocate model input package attribute arrays
    call mem_reallocate(this%ocaction, LINELENGTH, nline, 'OCACTION', &
                        this%mf6_input%mempath)
    call mem_reallocate(this%rtype, LINELENGTH, nline, 'RTYPE', &
                        this%mf6_input%mempath)
    call mem_reallocate(this%ocsetting, LINELENGTH, nline, 'OCSETTING', &
                        this%mf6_input%mempath)

    ! load pkinfo
    do n = 1, nline
      this%ocaction(n) = ocaction(n)
      this%rtype(n) = rtype(n)
      this%ocsetting(n) = ocsetting(n)
    end do

    deallocate (ocaction)
    deallocate (rtype)
    deallocate (ocsetting)
  end subroutine rp

  subroutine destroy(this)
    use MemoryManagerModule, only: mem_deallocate
    class(OcInputType), intent(inout) :: this
    call this%DynamicPkgLoadType%destroy()
    call mem_deallocate(this%ocaction)
    call mem_deallocate(this%rtype)
    call mem_deallocate(this%ocsetting)
  end subroutine destroy

end module Mf6FileOcInputModule
