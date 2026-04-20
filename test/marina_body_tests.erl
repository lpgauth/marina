-module(marina_body_tests).

-include("test.hrl").

topology_change_new_node_test() ->
    Payload = build_event(<<"TOPOLOGY_CHANGE">>, <<"NEW_NODE">>,
        {10, 0, 0, 42}, 9042),
    ?assertEqual(
        {ok, {event, topology_change, new_node, {10, 0, 0, 42}}},
        decode_event_frame(Payload)).

topology_change_removed_node_test() ->
    Payload = build_event(<<"TOPOLOGY_CHANGE">>, <<"REMOVED_NODE">>,
        {192, 168, 1, 1}, 9042),
    ?assertEqual(
        {ok, {event, topology_change, removed_node, {192, 168, 1, 1}}},
        decode_event_frame(Payload)).

status_change_up_test() ->
    Payload = build_event(<<"STATUS_CHANGE">>, <<"UP">>, {10, 0, 0, 1}, 9042),
    ?assertEqual(
        {ok, {event, status_change, up, {10, 0, 0, 1}}},
        decode_event_frame(Payload)).

status_change_down_test() ->
    Payload = build_event(<<"STATUS_CHANGE">>, <<"DOWN">>, {10, 0, 0, 2}, 9042),
    ?assertEqual(
        {ok, {event, status_change, down, {10, 0, 0, 2}}},
        decode_event_frame(Payload)).

unknown_topology_kind_passes_raw_atom_through_test() ->
    %% Future event kinds (e.g. a hypothetical MOVED_RACK) should not
    %% crash the decoder; event_kind/1 falls through to return the
    %% binary.
    Payload = build_event(<<"TOPOLOGY_CHANGE">>, <<"WEIRD">>, {1,1,1,1}, 9042),
    ?assertEqual(
        {ok, {event, topology_change, <<"WEIRD">>, {1, 1, 1, 1}}},
        decode_event_frame(Payload)).

ipv6_inet_decoded_as_8_tuple_test() ->
    Body = <<(encode_string(<<"STATUS_CHANGE">>))/binary,
             (encode_string(<<"UP">>))/binary,
             16, %% ipv6 marker
             16#2001:16, 16#0db8:16, 0:16, 0:16,
             0:16, 0:16, 0:16, 1:16,
             9042:32/signed>>,
    ?assertEqual(
        {ok, {event, status_change, up,
            {16#2001, 16#0db8, 0, 0, 0, 0, 0, 1}}},
        marina_body:decode(#frame {
            flags = 0, stream = -1, opcode = ?OP_EVENT, body = Body
        })).

schema_change_is_passed_through_raw_test() ->
    Body = <<(encode_string(<<"SCHEMA_CHANGE">>))/binary,
             (encode_string(<<"CREATED">>))/binary,
             (encode_string(<<"KEYSPACE">>))/binary,
             (encode_string(<<"test2">>))/binary>>,
    {ok, {event, schema_change, Raw}} = marina_body:decode(#frame {
        flags = 0, stream = -1, opcode = ?OP_EVENT, body = Body
    }),
    %% Raw is everything after the SCHEMA_CHANGE header — shape is
    %% identical to OP_RESULT kind 5, so the caller can route it through
    %% that decoder later.
    ?assertMatch(<<_:16, "CREATED", _:16, "KEYSPACE", _:16, "test2">>, Raw).

flag_preambles_decoded_in_spec_order_test() ->
    %% Spec order inside a response body when multiple flag bits are set:
    %%   [tracing_id (0x02)][warnings (0x08)][custom_payload (0x04)][message]
    %% Build a READY response with all three preambles and assert the
    %% body decoder consumes them in that order — reordering would either
    %% crash on length mismatch or read the wrong bytes as the message.
    TracingUuid = <<1:128>>,
    Warnings = <<(byte_size(<<"slow query">>)):16, "slow query">>,
    CustomPayload = <<>>,
    Body = <<TracingUuid/binary,
             1:16, Warnings/binary,        %% 1 warning string
             0:16, CustomPayload/binary>>, %% empty custom_payload map
    Flags = 16#02 bor 16#04 bor 16#08,
    ?assertEqual({ok, undefined},
        marina_body:decode(#frame {
            flags = Flags,
            stream = 0,
            opcode = ?OP_READY,
            body = Body
        })).

result_schema_change_function_test() ->
    %% CREATE FUNCTION response: target=FUNCTION, options =
    %%   <string keyspace><string name><string list arg_types>
    Body = <<5:32,                               %% kind = SCHEMA_CHANGE
             (encode_string(<<"CREATED">>))/binary,
             (encode_string(<<"FUNCTION">>))/binary,
             (encode_string(<<"test">>))/binary,
             (encode_string(<<"add">>))/binary,
             2:16,
             (encode_string(<<"int">>))/binary,
             (encode_string(<<"int">>))/binary>>,
    ?assertEqual(
        {ok, {<<"CREATED">>, <<"FUNCTION">>,
            {<<"test">>, <<"add">>, [<<"int">>, <<"int">>]}}},
        marina_body:decode(#frame {
            flags = 0, stream = 1, opcode = ?OP_RESULT, body = Body
        })).

result_schema_change_aggregate_test() ->
    Body = <<5:32,
             (encode_string(<<"DROPPED">>))/binary,
             (encode_string(<<"AGGREGATE">>))/binary,
             (encode_string(<<"test">>))/binary,
             (encode_string(<<"mysum">>))/binary,
             1:16,
             (encode_string(<<"bigint">>))/binary>>,
    ?assertEqual(
        {ok, {<<"DROPPED">>, <<"AGGREGATE">>,
            {<<"test">>, <<"mysum">>, [<<"bigint">>]}}},
        marina_body:decode(#frame {
            flags = 0, stream = 1, opcode = ?OP_RESULT, body = Body
        })).

auth_challenge_test() ->
    Token = <<1, 2, 3, 4, 5>>,
    Body = <<(byte_size(Token)):32/signed, Token/binary>>,
    ?assertEqual({ok, {auth_challenge, Token}},
        marina_body:decode(#frame {
            flags = 0, stream = 1, opcode = ?OP_AUTH_CHALLENGE, body = Body
        })).

%% helpers
decode_event_frame(Body) ->
    marina_body:decode(#frame {
        flags = 0, stream = -1, opcode = ?OP_EVENT, body = Body
    }).

build_event(EventType, Kind, {A, B, C, D}, Port) ->
    <<(encode_string(EventType))/binary,
      (encode_string(Kind))/binary,
      4, A, B, C, D, Port:32/signed>>.

encode_string(Bin) ->
    <<(byte_size(Bin)):16/unsigned, Bin/binary>>.
