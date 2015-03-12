-module(marina_types).
-include("marina.hrl").

-export([
    decode_bytes/1,
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
-spec decode_bytes(binary()) -> {binary(), binary()}.
decode_bytes(Bin) ->
    {Pos, Rest} = decode_int(Bin),
    split_binary(Rest, Pos).

-spec decode_int(binary()) -> {integer(), binary()}.
decode_int(<<Value:32, Rest/binary>>) ->
    {Value, Rest}.

-spec decode_long(binary()) -> {integer(), binary()}.
decode_long(<<Value:64, Rest/binary>>) ->
    {Value, Rest}.

-spec decode_long_string(binary()) -> {binary(), binary()}.
decode_long_string(Bin) ->
    {Pos, Rest} = decode_int(Bin),
    split_binary(Rest, Pos).

-spec decode_short(binary()) -> {integer(), binary()}.
decode_short(<<Value:16, Rest/binary>>) ->
    {Value, Rest}.

-spec decode_short_bytes(binary()) -> {binary(), binary()}.
decode_short_bytes(Bin) ->
    {Pos, Rest} = decode_short(Bin),
    split_binary(Rest, Pos).

-spec decode_string(binary()) -> {binary(), binary()}.
decode_string(Bin) ->
    {Pos, Rest} = decode_short(Bin),
    split_binary(Rest, Pos).

-spec decode_string_list(binary()) -> {[binary()], binary()}.
decode_string_list(Bin) ->
    {Length, Rest} = decode_short(Bin),
    decode_string_list(Rest, Length, []).

-spec decode_string_map(binary()) -> {[{binary(), binary()}], binary()}.
decode_string_map(Bin) ->
    {Length, Rest} = decode_short(Bin),
    decode_string_map(Rest, Length, []).

-spec decode_string_multimap(binary()) -> {[{binary(), [binary()]}], binary()}.
decode_string_multimap(Bin) ->
    {Length, Rest} = decode_short(Bin),
    decode_string_multimap(Rest, Length, []).

-spec decode_uuid(binary()) -> {binary(), binary()}.
decode_uuid(Bin) ->
    split_binary(Bin, 16).

-spec encode_bytes(binary()) -> binary().
encode_bytes(Value) ->
    <<(encode_int(size(Value)))/binary, Value/binary>>.

-spec encode_int(integer()) -> binary().
encode_int(Value) ->
    <<Value:32>>.

-spec encode_long(integer()) -> binary().
encode_long(Value) ->
    <<Value:64>>.

-spec encode_long_string(binary()) -> binary().
encode_long_string(Value) ->
    <<(encode_int(size(Value)))/binary, Value/binary>>.

-spec encode_short(integer()) -> binary().
encode_short(Value) ->
    <<Value:16>>.

-spec encode_short_bytes(binary()) -> binary().
encode_short_bytes(Value) ->
    <<(encode_short(size(Value)))/binary, Value/binary>>.

-spec encode_string(binary()) -> binary().
encode_string(Value) ->
    <<(encode_short(size(Value)))/binary, Value/binary>>.

-spec encode_string_list([binary()]) -> binary().
encode_string_list(Values) ->
    EncodedValues = [encode_string(Value) || Value <- Values],
    iolist_to_binary([encode_short(length(Values)), EncodedValues]).

-spec encode_string_map([{binary(), binary()}]) -> binary().
encode_string_map(KeyValues) ->
    encode_string_map(KeyValues, []).

-spec encode_string_multimap([{binary(), [binary()]}]) -> binary().
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
