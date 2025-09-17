! ** Do Not Modify! MODFLOW 6 system generated file. **
module UtlTvkInputModule
  use ConstantsModule, only: LENVARNAME
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public utl_tvk_param_definitions
  public utl_tvk_aggregate_definitions
  public utl_tvk_block_definitions
  public UtlTvkParamFoundType
  public utl_tvk_multi_package
  public utl_tvk_subpackages

  type UtlTvkParamFoundType
    logical :: print_input = .false.
    logical :: ts_filerecord = .false.
    logical :: ts6 = .false.
    logical :: filein = .false.
    logical :: ts6_filename = .false.
    logical :: maxbound = .false.
    logical :: cellid = .false.
    logical :: tvtype = .false.
    logical :: tvvalue = .false.
  end type UtlTvkParamFoundType

  logical :: utl_tvk_multi_package = .false.

  character(len=16), parameter :: &
    utl_tvk_subpackages(*) = &
    [ &
    '                ' &
    ]

  type(InputParamDefinitionType), parameter :: &
    utltvk_print_input = InputParamDefinitionType &
    ( &
    'UTL', & ! component
    'TVK', & ! subcomponent
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
    utltvk_ts_filerecord = InputParamDefinitionType &
    ( &
    'UTL', & ! component
    'TVK', & ! subcomponent
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
    utltvk_ts6 = InputParamDefinitionType &
    ( &
    'UTL', & ! component
    'TVK', & ! subcomponent
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
    utltvk_filein = InputParamDefinitionType &
    ( &
    'UTL', & ! component
    'TVK', & ! subcomponent
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
    utltvk_ts6_filename = InputParamDefinitionType &
    ( &
    'UTL', & ! component
    'TVK', & ! subcomponent
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
    utltvk_maxbound = InputParamDefinitionType &
    ( &
    'UTL', & ! component
    'TVK', & ! subcomponent
    'DIMENSIONS', & ! block
    'MAXBOUND', & ! tag name
    'MAXBOUND', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    'maximum number of tvk entries', & ! longname
    .false., & ! required
    .false., & ! prerelease
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    utltvk_cellid = InputParamDefinitionType &
    ( &
    'UTL', & ! component
    'TVK', & ! subcomponent
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
    utltvk_tvtype = InputParamDefinitionType &
    ( &
    'UTL', & ! component
    'TVK', & ! subcomponent
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
    utltvk_tvvalue = InputParamDefinitionType &
    ( &
    'UTL', & ! component
    'TVK', & ! subcomponent
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
    utl_tvk_param_definitions(*) = &
    [ &
    utltvk_print_input, &
    utltvk_ts_filerecord, &
    utltvk_ts6, &
    utltvk_filein, &
    utltvk_ts6_filename, &
    utltvk_maxbound, &
    utltvk_cellid, &
    utltvk_tvtype, &
    utltvk_tvvalue &
    ]

  type(InputParamDefinitionType), parameter :: &
    utltvk_perioddata = InputParamDefinitionType &
    ( &
    'UTL', & ! component
    'TVK', & ! subcomponent
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
    utl_tvk_aggregate_definitions(*) = &
    [ &
    utltvk_perioddata &
    ]

  type(InputBlockDefinitionType), parameter :: &
    utl_tvk_block_definitions(*) = &
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

end module UtlTvkInputModule
