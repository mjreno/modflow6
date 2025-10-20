! ** Do Not Modify! MODFLOW 6 system generated file. **
module OlfStoInputModule
  use ConstantsModule, only: LENVARNAME
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public olf_sto_param_definitions
  public olf_sto_aggregate_definitions
  public olf_sto_block_definitions
  public OlfStoParamFoundType
  public olf_sto_multi_package
  public olf_sto_subpackages

  type OlfStoParamFoundType
    logical :: ipakcb = .false.
    logical :: export_ascii = .false.
    logical :: storage = .false.
  end type OlfStoParamFoundType

  logical :: olf_sto_multi_package = .false.

  character(len=16), parameter :: &
    olf_sto_subpackages(*) = &
    [ &
    '                ' &
    ]

  type(InputParamDefinitionType), parameter :: &
    olfsto_ipakcb = InputParamDefinitionType &
    ( &
    'OLF', & ! component
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
    olfsto_export_ascii = InputParamDefinitionType &
    ( &
    'OLF', & ! component
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
    olfsto_storage = InputParamDefinitionType &
    ( &
    'OLF', & ! component
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
    olf_sto_param_definitions(*) = &
    [ &
    olfsto_ipakcb, &
    olfsto_export_ascii, &
    olfsto_storage &
    ]

  type(InputParamDefinitionType), parameter :: &
    olfsto_spd = InputParamDefinitionType &
    ( &
    'OLF', & ! component
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
    olf_sto_aggregate_definitions(*) = &
    [ &
    olfsto_spd &
    ]

  type(InputBlockDefinitionType), parameter :: &
    olf_sto_block_definitions(*) = &
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

end module OlfStoInputModule
