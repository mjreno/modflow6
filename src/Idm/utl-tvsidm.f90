! ** Do Not Modify! MODFLOW 6 system generated file. **
module UtlTvsInputModule
  use ConstantsModule, only: LENVARNAME
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public utl_tvs_param_definitions
  public utl_tvs_aggregate_definitions
  public utl_tvs_block_definitions
  public UtlTvsParamFoundType
  public utl_tvs_multi_package
  public utl_tvs_subpackages

  type UtlTvsParamFoundType
    logical :: disable_sto_chg = .false.
    logical :: print_input = .false.
    logical :: ts_filerecord = .false.
    logical :: ts6 = .false.
    logical :: filein = .false.
    logical :: ts6_filename = .false.
    logical :: maxbound = .false.
    logical :: cellid = .false.
    logical :: tvtype = .false.
    logical :: tvvalue = .false.
  end type UtlTvsParamFoundType

  logical :: utl_tvs_multi_package = .false.

  character(len=16), parameter :: &
    utl_tvs_subpackages(*) = &
    [ &
    '                ' &
    ]

  type(InputParamDefinitionType), parameter :: &
    utltvs_disable_sto_chg = InputParamDefinitionType &
    ( &
    'UTL', & ! component
    'TVS', & ! subcomponent
    'OPTIONS', & ! block
    'DISABLE_STORAGE_CHANGE_INTEGRATION', & ! tag name
    'DISABLE_STO_CHG', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    'deactivate storage change integration', & ! longname
    .false., & ! required
    .false., & ! prerelease
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    utltvs_print_input = InputParamDefinitionType &
    ( &
    'UTL', & ! component
    'TVS', & ! subcomponent
    'OPTIONS', & ! block
    'PRINT_INPUT', & ! tag name
    'PRINT_INPUT', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    'print input to listing file', & ! longname
    .false., & ! required
    .false., & ! prerelease
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    utltvs_ts_filerecord = InputParamDefinitionType &
    ( &
    'UTL', & ! component
    'TVS', & ! subcomponent
    'OPTIONS', & ! block
    'TS_FILERECORD', & ! tag name
    'TS_FILERECORD', & ! fortran variable
    'RECORD TS6 FILEIN TS6_FILENAME', & ! type
    '', & ! shape
    '', & ! longname
    .false., & ! required
    .false., & ! prerelease
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    utltvs_ts6 = InputParamDefinitionType &
    ( &
    'UTL', & ! component
    'TVS', & ! subcomponent
    'OPTIONS', & ! block
    'TS6', & ! tag name
    'TS6', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    'head keyword', & ! longname
    .true., & ! required
    .false., & ! prerelease
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    utltvs_filein = InputParamDefinitionType &
    ( &
    'UTL', & ! component
    'TVS', & ! subcomponent
    'OPTIONS', & ! block
    'FILEIN', & ! tag name
    'FILEIN', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    'file keyword', & ! longname
    .true., & ! required
    .false., & ! prerelease
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    utltvs_ts6_filename = InputParamDefinitionType &
    ( &
    'UTL', & ! component
    'TVS', & ! subcomponent
    'OPTIONS', & ! block
    'TS6_FILENAME', & ! tag name
    'TS6_FILENAME', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    'file name of time series information', & ! longname
    .true., & ! required
    .false., & ! prerelease
    .true., & ! multi-record
    .true., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    utltvs_maxbound = InputParamDefinitionType &
    ( &
    'UTL', & ! component
    'TVS', & ! subcomponent
    'DIMENSIONS', & ! block
    'MAXBOUND', & ! tag name
    'MAXBOUND', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    'maximum number of tvs entries', & ! longname
    .false., & ! required
    .false., & ! prerelease
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    utltvs_cellid = InputParamDefinitionType &
    ( &
    'UTL', & ! component
    'TVS', & ! subcomponent
    'PERIOD', & ! block
    'CELLID', & ! tag name
    'CELLID', & ! fortran variable
    'INTEGER1D', & ! type
    'NCELLDIM', & ! shape
    'cell identifier', & ! longname
    .true., & ! required
    .false., & ! prerelease
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    utltvs_tvtype = InputParamDefinitionType &
    ( &
    'UTL', & ! component
    'TVS', & ! subcomponent
    'PERIOD', & ! block
    'TVTYPE', & ! tag name
    'TVTYPE', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    '', & ! longname
    .true., & ! required
    .false., & ! prerelease
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    utltvs_tvvalue = InputParamDefinitionType &
    ( &
    'UTL', & ! component
    'TVS', & ! subcomponent
    'PERIOD', & ! block
    'TVVALUE', & ! tag name
    'TVVALUE', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    'specific storage', & ! longname
    .true., & ! required
    .false., & ! prerelease
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .true. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    utl_tvs_param_definitions(*) = &
    [ &
    utltvs_disable_sto_chg, &
    utltvs_print_input, &
    utltvs_ts_filerecord, &
    utltvs_ts6, &
    utltvs_filein, &
    utltvs_ts6_filename, &
    utltvs_maxbound, &
    utltvs_cellid, &
    utltvs_tvtype, &
    utltvs_tvvalue &
    ]

  type(InputParamDefinitionType), parameter :: &
    utltvs_perioddata = InputParamDefinitionType &
    ( &
    'UTL', & ! component
    'TVS', & ! subcomponent
    'PERIOD', & ! block
    'PERIODDATA', & ! tag name
    'PERIODDATA', & ! fortran variable
    'RECARRAY CELLID TVTYPE TVVALUE', & ! type
    '', & ! shape
    '', & ! longname
    .true., & ! required
    .false., & ! prerelease
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    utl_tvs_aggregate_definitions(*) = &
    [ &
    utltvs_perioddata &
    ]

  type(InputBlockDefinitionType), parameter :: &
    utl_tvs_block_definitions(*) = &
    [ &
    InputBlockDefinitionType( &
    'OPTIONS', & ! blockname
    .false., & ! required
    .false., & ! aggregate
    .false. & ! block_variable
    ), &
    InputBlockDefinitionType( &
    'DIMENSIONS', & ! blockname
    .false., & ! required
    .false., & ! aggregate
    .false. & ! block_variable
    ), &
    InputBlockDefinitionType( &
    'PERIOD', & ! blockname
    .true., & ! required
    .true., & ! aggregate
    .true. & ! block_variable
    ) &
    ]

end module UtlTvsInputModule
