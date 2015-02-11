-module(marina_types).
-include("marina.hrl").

-export([
    decode_bytes/1,
    decode_inet/1,
    decode_int/1,
    decode_long/1,
    decode_long_string/1,
    decode_short/1,
    decode_short_bytes/1,
    decode_string/1,
    decode_string_list/1,
    decode_string_map/1,
    decode_string_multimap/1,
    decode_uuid/1,
    encode_bytes/1,
    encode_int/1,
    encode_long/1,
    encode_long_string/1,
    encode_short/1,
    encode_short_bytes/1,
    encode_string/1,
    encode_string_list/1,
    encode_string_map/1,
    encode_string_multimap/1
]).

%% public
decode_bytes(Bin) ->
    {Pos, Rest} = decode_int(Bin),
    split_binary(Rest, Pos).

decode_inet(Bin) ->
    <<Pos:8, Rest/binary>> = Bin,
    {IpBytes, Rest2} = split_binary(Rest, Pos),
    {Port, Rest3} = decode_int(Rest2),
    Ip = case Pos of
        4 ->
            <<A1:8, A2:8, A3:8, A4:8>> = IpBytes,
            {A1, A2, A3, A4};
        16 ->
            <<A1:16, A2:16, A3:16, A4:16, A5:16, A6:16, A7:16, A8:16>> = IpBytes,
            {A1, A2, A3, A4, A5, A6, A7, A8}
    end,
    {{Ip, Port}, Rest3}.

decode_int(Bin) ->
    <<Value:32, Rest/binary>> = Bin,
    {Value, Rest}.

decode_long(Bin) ->
    <<Value:64, Rest/binary>> = Bin,
    {Value, Rest}.

decode_long_string(Bin) ->
    {Pos, Rest} = decode_long(Bin),
    split_binary(Rest, Pos).

decode_short(Bin) ->
    <<Value:16, Rest/binary>> = Bin,
    {Value, Rest}.

decode_short_bytes(Bin) ->
    {Pos, Rest} = decode_short(Bin),
    split_binary(Rest, Pos).

decode_string(Bin) ->
    {Pos, Rest} = decode_short(Bin),
    split_binary(Rest, Pos).

decode_string_list(Bin) ->
    {Length, Rest} = decode_short(Bin),
    decode_string_list(Rest, Length, []).

decode_string_map(Bin) ->
    {Length, Rest} = decode_short(Bin),
    decode_string_map(Rest, Length, []).

decode_string_multimap(Bin) ->
    {Length, Rest} = decode_short(Bin),
    decode_string_multimap(Rest, Length, []).

decode_uuid(Bin) ->
    split_binary(Bin, 16).

encode_bytes(Value) ->
    <<(encode_int(size(Value)))/binary, Value/binary>>.

encode_int(Value) ->
    <<Value:32>>.

encode_long(Value) ->
    <<Value:64>>.

encode_long_string(Value) ->
    <<(encode_long(size(Value)))/binary, Value/binary>>.

encode_short(Value) ->
    <<Value:16>>.

encode_short_bytes(Value) ->
    <<(encode_short(size(Value)))/binary, Value/binary>>.

encode_string(Value) ->
    <<(encode_short(size(Value)))/binary, Value/binary>>.

encode_string_list(Values) ->
    EncodedValues = [encode_string(Value) || Value <- Values],
    iolist_to_binary([encode_short(length(Values)), EncodedValues]).

encode_string_map(KeyValues) ->
    encode_string_map(KeyValues, []).

encode_string_multimap(KeyValues) ->
    encode_string_multimap(KeyValues, []).

%% private
decode_string_list(Bin, 0, Acc) ->
    {lists:reverse(Acc), Bin};
decode_string_list(Bin, Length, Acc) ->
    {String, Rest} = decode_string(Bin),
    decode_string_list(Rest, Length - 1, [String | Acc]).

decode_string_map(Bin, 0, Acc) ->
    {lists:reverse(Acc), Bin};
decode_string_map(Bin, Length, Acc) ->
    {Key, Rest} = decode_string(Bin),
    {Value, Rest2} = decode_string(Rest),
    decode_string_map(Rest2, Length - 1, [{Key, Value} | Acc]).

decode_string_multimap(Bin, 0, Acc) ->
    {lists:reverse(Acc), Bin};
decode_string_multimap(Bin, Length, Acc) ->
    {Key, Rest} = decode_string(Bin),
    {Values, Rest2} = decode_string_list(Rest),
    decode_string_multimap(Rest2, Length - 1, [{Key, Values} | Acc]).

encode_string_multimap([], Acc) ->
    iolist_to_binary([encode_short(length(Acc)), lists:reverse(Acc)]);
encode_string_multimap([{Key, Values} | Rest], Acc) ->
    encode_string_multimap(Rest, [[encode_string(Key), encode_string_list(Values)] | Acc]).

encode_string_map([], Acc) ->
    iolist_to_binary([encode_short(length(Acc)), lists:reverse(Acc)]);
encode_string_map([{Key, Value} | Rest], Acc) ->
    encode_string_map(Rest, [[encode_string(Key), encode_string(Value)] | Acc]).
