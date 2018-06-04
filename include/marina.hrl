%% macros
-define(CONSISTENCY_ANY, 16#00).
-define(CONSISTENCY_ONE, 16#01).
-define(CONSISTENCY_TWO, 16#02).
-define(CONSISTENCY_THREE, 16#03).
-define(CONSISTENCY_QUORUM, 16#04).
-define(CONSISTENCY_ALL, 16#05).
-define(CONSISTENCY_LOCAL_QUORUM, 16#06).
-define(CONSISTENCY_EACH_QUORUM, 16#07).
-define(CONSISTENCY_SERIAL, 16#08).
-define(CONSISTENCY_LOCAL_SERIAL, 16#09).
-define(CONSISTENCY_LOCAL_ONE, 16#0A).

%% records
-record(buffer, {
    buffered :: iolist(),
    current  :: non_neg_integer(),
    pending  :: non_neg_integer() | undefined
}).

-record(frame, {
    flags  :: 0 | 1,
    stream :: integer(),
    opcode :: non_neg_integer(),
    body   :: iolist() | binary()
}).

-record(column_spec, {
    keyspace  = <<>>      :: binary(),
    table     = <<>>      :: binary(),
    name      = <<>>      :: binary(),
    type      = undefined :: atom() | binary()
}).

-record(result_metadata, {
    columns_count = 0         :: integer(),
    columns       = []        :: list(column_spec()),
    paging_state  = undefined :: binary() | undefined
}).

-record(result, {
    metadata        :: result_metadata(),
    rows_count = 0  :: integer(),
    rows       = [] :: [[binary()]]
}).

%% types
-type buffer() :: #buffer {}.
-type column_spec() :: #column_spec {}.
-type consistency_level() ::
    ?CONSISTENCY_ANY |
    ?CONSISTENCY_ONE |
    ?CONSISTENCY_TWO |
    ?CONSISTENCY_THREE |
    ?CONSISTENCY_QUORUM |
    ?CONSISTENCY_ALL |
    ?CONSISTENCY_LOCAL_QUORUM |
    ?CONSISTENCY_EACH_QUORUM |
    ?CONSISTENCY_SERIAL |
    ?CONSISTENCY_LOCAL_SERIAL |
    ?CONSISTENCY_LOCAL_ONE.

-type error() :: {error, term()}.
-type frame() :: #frame {}.
-type frame_flag() :: 0..1.
-type query() :: binary().
-type query_opts() :: #{
    consistency_level => consistency_level(),
    page_size => pos_integer(),
    paging_state => binary(),
    pid => pid(),
    routing_key => binary(),
    skip_metadata => boolean(),
    timeout => pos_integer(),
    values => values()
}.

-type result() :: #result {}.
-type result_metadata() :: #result_metadata {}.
-type statement_id() :: binary().
-type stream() :: 0..32768.
-type value() :: binary().
-type values() :: [value()].
