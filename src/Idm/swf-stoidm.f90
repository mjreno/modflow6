! ** Do Not Modify! MODFLOW 6 system generated file. **
module SwfStoInputModule
  use ConstantsModule, only: LENVARNAME
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public swf_sto_param_definitions
  public swf_sto_aggregate_definitions
  public swf_sto_block_definitions
  public SwfStoParamFoundType
  public swf_sto_multi_package
  public swf_sto_subpackages

  type SwfStoParamFoundType
    logical :: ipakcb = .false.
    logical :: export_ascii = .false.
    logical :: storage = .false.
  end type SwfStoParamFoundType

  logical :: swf_sto_multi_package = .false.

  character(len=16), parameter :: &
    swf_sto_subpackages(*) = &
    [ &
    '                ' &
    ]

  type(InputParamDefinitionType), parameter :: &
    swfsto_ipakcb = InputParamDefinitionType &
    ( &
    'SWF', & ! component
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
    swfsto_export_ascii = InputParamDefinitionType &
    ( &
    'SWF', & ! component
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
    swfsto_storage = InputParamDefinitionType &
    ( &
    'SWF', & ! component
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
    swf_sto_param_definitions(*) = &
    [ &
    swfsto_ipakcb, &
    swfsto_export_ascii, &
    swfsto_storage &
    ]

  type(InputParamDefinitionType), parameter :: &
    swfsto_spd = InputParamDefinitionType &
    ( &
    'SWF', & ! component
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
    swf_sto_aggregate_definitions(*) = &
    [ &
    swfsto_spd &
    ]

  type(InputBlockDefinitionType), parameter :: &
    swf_sto_block_definitions(*) = &
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

end module SwfStoInputModule
