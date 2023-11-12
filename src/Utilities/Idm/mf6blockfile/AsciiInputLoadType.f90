!> @brief This module contains the AsciiInputLoadTypeModule
!!
!! This module defines an abstract type that support generic
!! IDP dynamic input loading for traditional MODFLOW 6 ascii
!! files.
!!
!<
module AsciiInputLoadTypeModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LENVARNAME
  use InputLoadTypeModule, only: DynamicPkgLoadType
  use BlockParserModule, only: BlockParserType
  use BoundInputContextModule, only: BoundInputContextType

  implicit none
  private
  public :: AsciiDynamicPkgLoadBaseType

  !> @brief base abstract type for ascii source dynamic load
  !!
  !<
  type, abstract, extends(DynamicPkgLoadType) :: AsciiDynamicPkgLoadBaseType
    integer(I4B) :: nparam
    character(len=LENVARNAME), dimension(:), &
      allocatable :: param_names !< dynamic param names
    type(BoundInputContextType) :: bndctx
  contains
    procedure(ascii_period_load_if), deferred :: rp
  end type AsciiDynamicPkgLoadBaseType

  abstract interface
    subroutine ascii_period_load_if(this, parser)
      import AsciiDynamicPkgLoadBaseType, BlockParserType
      class(AsciiDynamicPkgLoadBaseType), intent(inout) :: this
      type(BlockParserType), pointer, intent(inout) :: parser !< block parser
    end subroutine
  end interface

end module AsciiInputLoadTypeModule
