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
