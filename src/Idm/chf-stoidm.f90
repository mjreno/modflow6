! ** Do Not Modify! MODFLOW 6 system generated file. **
module ChfStoInputModule
  use ConstantsModule, only: LENVARNAME
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public chf_sto_param_definitions
  public chf_sto_aggregate_definitions
  public chf_sto_block_definitions
  public ChfStoParamFoundType
  public chf_sto_multi_package
  public chf_sto_subpackages

  type ChfStoParamFoundType
    logical :: ipakcb = .false.
    logical :: export_ascii = .false.
    logical :: storage = .false.
  end type ChfStoParamFoundType

  logical :: chf_sto_multi_package = .false.

  character(len=16), parameter :: &
    chf_sto_subpackages(*) = &
    [ &
    '                ' &
    ]

  type(InputParamDefinitionType), parameter :: &
    chfsto_ipakcb = InputParamDefinitionType &
    ( &
    'CHF', & ! component
    'STO', & ! subcomponent
    'OPTIONS', & ! block
    'SAVE_FLOWS', & ! tag name
    'IPAKCB', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    'keyword to save NPF flows', & ! longname
    .false., & ! required
    .false., & ! developmode
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    chfsto_export_ascii = InputParamDefinitionType &
    ( &
    'CHF', & ! component
    'STO', & ! subcomponent
    'OPTIONS', & ! block
    'EXPORT_ARRAY_ASCII', & ! tag name
    'EXPORT_ASCII', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    'export array variables to layered ascii files.', & ! longname
    .false., & ! required
    .false., & ! developmode
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    chfsto_storage = InputParamDefinitionType &
    ( &
    'CHF', & ! component
    'STO', & ! subcomponent
    'PERIOD', & ! block
    'STORAGE', & ! tag name
    'STORAGE', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    'storage type', & ! longname
    .true., & ! required
    .false., & ! developmode
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    chf_sto_param_definitions(*) = &
    [ &
    chfsto_ipakcb, &
    chfsto_export_ascii, &
    chfsto_storage &
    ]

  type(InputParamDefinitionType), parameter :: &
    chfsto_spd = InputParamDefinitionType &
    ( &
    'CHF', & ! component
    'STO', & ! subcomponent
    'PERIOD', & ! block
    'STRESS_PERIOD_DATA', & ! tag name
    'SPD', & ! fortran variable
    'RECARRAY STORAGE', & ! type
    '1', & ! shape
    '', & ! longname
    .false., & ! required
    .false., & ! developmode
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    chf_sto_aggregate_definitions(*) = &
    [ &
    chfsto_spd &
    ]

  type(InputBlockDefinitionType), parameter :: &
    chf_sto_block_definitions(*) = &
    [ &
    InputBlockDefinitionType( &
    'OPTIONS', & ! blockname
    .false., & ! required
    .false., & ! aggregate
    .false. & ! block_variable
    ), &
    InputBlockDefinitionType( &
    'PERIOD', & ! blockname
    .false., & ! required
    .true., & ! aggregate
    .true. & ! block_variable
    ) &
    ]

end module ChfStoInputModule
