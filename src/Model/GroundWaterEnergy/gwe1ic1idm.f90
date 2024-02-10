! ** Do Not Modify! MODFLOW 6 system generated file. **
module GweIcInputModule
  use ConstantsModule, only: LENVARNAME
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public gwe_ic_param_definitions
  public gwe_ic_aggregate_definitions
  public gwe_ic_block_definitions
  public GweIcParamFoundType
  public gwe_ic_multi_package
  public gwe_ic_advanced_package
  public gwe_ic_subpackages

  type GweIcParamFoundType
    logical :: strt = .false.
  end type GweIcParamFoundType

  logical :: gwe_ic_multi_package = .false.
  logical :: gwe_ic_advanced_package = .false.

  character(len=16), parameter :: &
    gwe_ic_subpackages(*) = &
    [ &
    '                ' &
    ]

  type(InputParamDefinitionType), parameter :: &
    gweic_strt = InputParamDefinitionType &
    ( &
    'GWE', & ! component
    'IC', & ! subcomponent
    'GRIDDATA', & ! block
    'STRT', & ! tag name
    'STRT', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .true., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwe_ic_param_definitions(*) = &
    [ &
    gweic_strt &
    ]

  type(InputParamDefinitionType), parameter :: &
    gwe_ic_aggregate_definitions(*) = &
    [ &
    InputParamDefinitionType &
    ( &
    '', & ! component
    '', & ! subcomponent
    '', & ! block
    '', & ! tag name
    '', & ! fortran variable
    '', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    ) &
    ]

  type(InputBlockDefinitionType), parameter :: &
    gwe_ic_block_definitions(*) = &
    [ &
    InputBlockDefinitionType( &
    'OPTIONS', & ! blockname
    .false., & ! required
    .false., & ! aggregate
    .false., & ! block_variable
    .false. & ! timeseries
    ), &
    InputBlockDefinitionType( &
    'GRIDDATA', & ! blockname
    .true., & ! required
    .false., & ! aggregate
    .false., & ! block_variable
    .false. & ! timeseries
    ) &
    ]

end module GweIcInputModule
