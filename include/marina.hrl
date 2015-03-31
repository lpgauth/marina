% application
-define(APP, marina).
-define(SERVER_BASE_NAME, "marina_server_").

% defaults
-define(DEFAULT_FLAGS, 0).
-define(DEFAULT_IP, "127.0.0.1").
-define(DEFAULT_POOL_SIZE, 16).
-define(DEFAULT_PORT, 9042).
-define(DEFAULT_RECONNECT, 5000).
-define(DEFAULT_RECV_TIMEOUT, 1000).
-define(DEFAULT_SEND_TIMEOUT, 20).
-define(DEFAULT_STREAM, 0).
-define(DEFAULT_TIMEOUT, 1000).

% protocol
-define(CQL_VERSION, {<<"CQL_VERSION">>, <<"3.2.0">>}).
-define(MAX_STREAM_ID, 32768).
-define(PROTO_VERSION, 3).

-define(OP_ERROR, 16#00).
-define(OP_STARTUP, 16#01).
-define(OP_READY, 16#02).
-define(OP_AUTHENTICATE, 16#03).
-define(OP_OPTIONS, 16#05).
-define(OP_SUPPORTED, 16#06).
-define(OP_QUERY, 16#07).
-define(OP_RESULT, 16#08).
-define(OP_PREPARE, 16#09).
-define(OP_EXECUTE, 16#0A).
-define(OP_REGISTER, 16#0B).
-define(OP_EVENT, 16#0C).
-define(OP_BATCH, 16#0D).
-define(OP_AUTH_CHALLENGE, 16#0E).
-define(OP_AUTH_RESPONSE, 16#0F).
-define(OP_AUTH_SUCCESS, 16#10).

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
-define(CONSISTENCY_LOCAL_ONE, 16#10).

% records
-record(buffer, {
    buffered :: iolist(),
    current  :: non_neg_integer(),
    pending  :: non_neg_integer() | undefined
}).

-record(frame, {
    flags,
    stream :: integer(),
    opcode :: non_neg_integer(),
    body   :: binary()
}).

-record(column_spec, {
    keyspace  = <<>>      :: binary(),
    table     = <<>>      :: binary(),
    name      = <<>>      :: binary(),
    type      = undefined :: atom() | binary()
}).

-record(result_metadata, {
    columns_count = 0  :: integer(),
    columns       = [] :: list(column_spec())
}).

-record(result, {
    metadata        :: result_metadata(),
    rows_count = 0  :: integer(),
    rows       = [] :: [[binary()]]
}).

% types
-type buffer() :: #buffer {}.
-type column_spec() :: #column_spec {}.
-type frame()  :: #frame {}.
-type result() :: #result {}.
-type result_metadata() :: #result_metadata {}.

-export_type([
   buffer/0,
   frame/0
]).
