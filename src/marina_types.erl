-module(marina_types).
-include("marina_internal.hrl").

-compile(inline).
-compile({inline_size, 512}).

-export([
    decode_bytes/1,
    decode_int/1,
    decode_long/1,
    decode_long_string/1,
    decode_long_string_set/1,
    decode_short/1,
    decode_short_bytes/1,
    decode_string/1,
    decode_string_list/1,
    decode_string_map/1,
    decode_string_multimap/1,
    decode_tinyint/1,
    decode_uuid/1,
    encode_boolean/1,
    encode_bytes/1,
    encode_int/1,
    encode_list/1,
    encode_long/1,
    encode_long_string/1,
    encode_short/1,
    encode_short_bytes/1,
    encode_string/1,
    encode_string_list/1,
    encode_string_map/1,
    encode_string_multimap/1,
    encode_tinyint/1
]).

%% public
-spec decode_bytes(binary()) -> {null, binary()} | {binary(), binary()}.

decode_bytes(<<255, 255, 255, 255, Rest/binary>>) ->
    {null, Rest};
decode_bytes(<<Pos:32, Value:Pos/binary, Rest/binary>>) ->
    {Value, Rest}.

-spec decode_int(binary()) -> {integer(), binary()}.

decode_int(<<Value:32, Rest/binary>>) ->
    {Value, Rest}.

-spec decode_long(binary()) -> {integer(), binary()}.

decode_long(<<Value:64, Rest/binary>>) ->
    {Value, Rest}.

-spec decode_long_string(binary()) -> {binary(), binary()}.

decode_long_string(Bin) ->
    decode_bytes(Bin).

-spec decode_long_string_set(binary()) -> {[binary()], binary()}.

decode_long_string_set(<<Length:32, Rest/binary>>) ->
    decode_long_string_set(Rest, Length, []).

-spec decode_short(binary()) -> {integer(), binary()}.

decode_short(<<Value:16, Rest/binary>>) ->
    {Value, Rest}.

-spec decode_short_bytes(binary()) -> {binary(), binary()}.

decode_short_bytes(<<255, 255, Rest/binary>>) ->
    {null, Rest};
decode_short_bytes(<<Pos:16, Value:Pos/binary, Rest/binary>>) ->
    {Value, Rest}.

-spec decode_string(binary()) -> {binary(), binary()}.

decode_string(Bin) ->
    decode_short_bytes(Bin).

-spec decode_string_list(binary()) -> {[binary()], binary()}.

decode_string_list(<<Length:16, Rest/binary>>) ->
    decode_string_list(Rest, Length, []).

-spec decode_string_map(binary()) -> {[{binary(), binary()}], binary()}.

decode_string_map(<<Length:16, Rest/binary>>) ->
    decode_string_map(Rest, Length, []).

-spec decode_string_multimap(binary()) -> {[{binary(), [binary()]}], binary()}.

decode_string_multimap(<<Length:16, Rest/binary>>) ->
    decode_string_multimap(Rest, Length, []).

-spec decode_tinyint(binary()) -> {integer(), binary()}.

decode_tinyint(<<Value:8, Rest/binary>>) ->
    {Value, Rest}.

-spec decode_uuid(binary()) -> {binary(), binary()}.

decode_uuid(<<Value:16/binary, Rest/binary>>) ->
    {Value, Rest}.

-spec encode_boolean(boolean()) -> binary().

encode_boolean(false) -> <<0>>;
encode_boolean(true) -> <<1>>.

-spec encode_bytes(binary()) -> binary().

encode_bytes(null) ->
    <<255, 255, 255, 255>>;
encode_bytes(Value) ->
    <<(encode_int(size(Value)))/binary, Value/binary>>.

-spec encode_int(integer()) -> binary().

encode_int(Value) ->
    <<Value:32>>.

-spec encode_list([binary()]) -> binary().

encode_list(Values) ->
    iolist_to_binary([encode_short(length(Values)), Values]).

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

encode_short_bytes(null) ->
    <<255, 255>>;
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

encode_tinyint(Value) ->
    <<Value:8>>.

%% private
decode_long_string_set(Bin, 0, Acc) ->
    {lists:reverse(Acc), Bin};
decode_long_string_set(Bin, Length, Acc) ->
    {String, Rest} = decode_bytes(Bin),
    decode_long_string_set(Rest, Length - 1, [String | Acc]).

decode_string_list(Bin, 0, Acc) ->
    {lists:reverse(Acc), Bin};
decode_string_list(<<Pos:16, String:Pos/binary, Rest/binary>>, Length, Acc) ->
    decode_string_list(Rest, Length - 1, [String | Acc]).

decode_string_map(Bin, 0, Acc) ->
    {lists:reverse(Acc), Bin};
decode_string_map(<<Pos:16, Key:Pos/binary, Pos2:16, Value:Pos2/binary,
    Rest/binary>>, Length, Acc) ->

    decode_string_map(Rest, Length - 1, [{Key, Value} | Acc]).

decode_string_multimap(Bin, 0, Acc) ->
    {lists:reverse(Acc), Bin};
decode_string_multimap(<<Pos:16, Key:Pos/binary, Rest/binary>>, Length, Acc) ->
    {Values, Rest2} = decode_string_list(Rest),
    decode_string_multimap(Rest2, Length - 1, [{Key, Values} | Acc]).

encode_string_map([], Acc) ->
    iolist_to_binary([encode_short(length(Acc)), lists:reverse(Acc)]);
encode_string_map([{Key, Value} | Rest], Acc) ->
    encode_string_map(Rest, [[encode_string(Key), encode_string(Value)] | Acc]).

encode_string_multimap([], Acc) ->
    encode_string_map([], Acc);
encode_string_multimap([{Key, Values} | Rest], Acc) ->
    encode_string_multimap(Rest, [[encode_string(Key),
    encode_string_list(Values)] | Acc]).
