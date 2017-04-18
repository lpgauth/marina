-include("marina.hrl").

%% macros
-define(APP, marina).
-define(CLIENT, marina_client).
-define(GET_ENV(Key, Default), application:get_env(?APP, Key, Default)).
-define(LOOKUP(Key, List), shackle_utils:lookup(Key, List, undefined)).

%% defaults
-define(DEFAULT_BACKLOG_SIZE, 1024).
-define(DEFAULT_BOOTSTRAP_IPS, [?GET_ENV(ip, ?DEFAULT_IP)]).
-define(DEFAULT_CONNECT_RETRY, 500).
-define(DEFAULT_DATACENTER, undefined).
-define(DEFAULT_FLAGS, 0).
-define(DEFAULT_IP, "127.0.0.1").
-define(DEFAULT_POOL_SIZE, 16).
-define(DEFAULT_POOL_STRATEGY, random).
-define(DEFAULT_PORT, 9042).
-define(DEFAULT_RECONNECT, true).
-define(DEFAULT_RECONNECT_MAX, 120000).
-define(DEFAULT_RECONNECT_MIN, 1500).
-define(DEFAULT_RECV_TIMEOUT, 1000).
-define(DEFAULT_SOCKET_OPTIONS, [
    binary,
    {buffer, 65535},
    {nodelay, true},
    {packet, raw},
    {send_timeout, 50},
    {send_timeout_close, true}
]).
-define(DEFAULT_STREAM, 0).
-define(DEFAULT_TIMEOUT, 1000).

%% protocol
-define(CQL_VERSION, {<<"CQL_VERSION">>, <<"3.2.0">>}).
-define(HEADER_SIZE, 9).
-define(LZ4_COMPRESSION, {<<"COMPRESSION">>, <<"lz4">>}).
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

%% ETS tables
-define(ETS_TABLE_CACHE, marina_cache).
