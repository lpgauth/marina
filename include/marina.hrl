% application
-define(APP, marina).
-define(CHILD(Mod), {Mod, {Mod, start_link, []}, permanent, 5000, worker, [Mod]}).

% defaults
-define(DEFAULT_IP, "127.0.0.1").
-define(DEFAULT_PORT, 9042).
-define(DEFAULT_RECONNECT, 5000).
-define(DEFAULT_SEND_TIMEOUT, 20).

% protocol
-define(CQL_VERSION, <<"3.0.0">>).
-define(CQL_VERSION_KEY, <<"CQL_VERSION">>).
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
-record(frame, {
    flags,
    stream,
    opcode,
    body
}).
