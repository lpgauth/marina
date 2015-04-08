-module(marina_body).
-include("marina.hrl").

-export([
    decode/1
]).

%% public
-spec decode(frame()) -> {ok, term()} | {error, atom()}.

decode(#frame {opcode = ?OP_READY}) ->
    {ok, undefined};
decode(#frame {
        opcode = ?OP_ERROR,
        body = Body
    }) ->

    {Code, Rest} = marina_types:decode_int(Body),
    {Msg, _Rest2} = marina_types:decode_string(Rest),
    {error, {Code, Msg}};
decode(#frame {
        opcode = ?OP_RESULT,
        body = <<1:32/integer>>
    }) ->

    {ok, undefined};
decode(#frame {
        opcode = ?OP_RESULT,
        body = <<2:32/integer, Rest/binary>>
    }) ->

    {Metadata, Rest2} = decode_result_metadata(Rest),
    {RowsCount, Rest3} = marina_types:decode_int(Rest2),
    ColumnsCount = Metadata#result_metadata.columns_count,
    {Rows, <<>>} = decode_rows(Rest3, RowsCount, ColumnsCount),

    {ok, #result {
        metadata = Metadata,
        rows_count = RowsCount,
        rows = Rows
    }};
decode(#frame {
        opcode = ?OP_RESULT,
        body = <<3:32/integer, Rest/binary>>
    }) ->

    {Keyspace, <<>>} = marina_types:decode_string(Rest),
    {ok, Keyspace};
decode(#frame {
        opcode = ?OP_RESULT,
        body = <<4:32/integer, Rest/binary>>
    }) ->

    {Id, Rest2} = marina_types:decode_short_bytes(Rest),
    {_Metadata, Rest3} = decode_result_metadata(Rest2),
    {_ResultMetadata, <<>>} = decode_result_metadata(Rest3),

    {ok, Id};
decode(#frame {
        opcode = ?OP_RESULT,
        body = <<5:32/integer, Rest/binary>>
    }) ->

    {ChangeType, Rest2} = marina_types:decode_string(Rest),
    {Target, Rest3} = marina_types:decode_string(Rest2),

    Options = case Target of
        <<"KEYSPACE">> ->
            {Option, <<>>} = marina_types:decode_string(Rest3),
            {Option};
        <<"TABLE">> ->
            {Option, Rest4} = marina_types:decode_string(Rest3),
            {Option2, <<>>} = marina_types:decode_string(Rest4),
            {Option, Option2};
        <<"TYPE">> ->
            {Option, Rest4} = marina_types:decode_string(Rest3),
            {Option2, <<>>} = marina_types:decode_string(Rest4),
            {Option, Option2}
    end,

    {ok, {ChangeType, Target, Options}}.

%% private
decode_columns(Bin, Count) ->
    decode_columns(Bin, Count, []).

decode_columns(Rest, 0, Acc) ->
    {lists:reverse(Acc), Rest};
decode_columns(Bin, Count, Acc) ->
    {Column, Rest} = marina_types:decode_bytes(Bin),
    decode_columns(Rest, Count - 1, [Column | Acc]).

decode_columns_metadata(Bin, ColumnsCount, true) ->
    {Keyspace, Rest} = marina_types:decode_string(Bin),
    {Table, Rest2} = marina_types:decode_string(Rest),
    decode_columns_metadata(Rest2, ColumnsCount, {Keyspace, Table}, []);
decode_columns_metadata(Bin, ColumnsCount, false) ->
    decode_columns_metadata(Bin, ColumnsCount, {undefined, undefined}, []).

decode_columns_metadata(Rest, 0, _GlobalTableSpec, Acc) ->
    {lists:reverse(Acc), Rest};
decode_columns_metadata(Bin, Count, {undefined, undefined} = GlobalTableSpec, Acc) ->
    {Keyspace, Bin2} = marina_types:decode_string(Bin),
    {Table, Bin3} = marina_types:decode_string(Bin2),
    {Name, Bin4} = marina_types:decode_string(Bin3),
    {Type, Bin5} = decode_type(Bin4),
    ColumnSpec = #column_spec {
        keyspace = Keyspace,
        table = Table,
        name = Name,
        type = Type
    },
    decode_columns_metadata(Bin5, Count - 1, GlobalTableSpec, [ColumnSpec | Acc]);
decode_columns_metadata(Bin, Count, {Keyspace, Table} = GlobalTableSpec, Acc) ->
    {Name, Bin2} = marina_types:decode_string(Bin),
    {Type, Bin3} = decode_type(Bin2),
    ColumnSpec = #column_spec {
        keyspace = Keyspace,
        table = Table,
        name = Name,
        type = Type
    },
    decode_columns_metadata(Bin3, Count - 1, GlobalTableSpec, [ColumnSpec | Acc]).

decode_elements(Bin, N) ->
    decode_elements(Bin, N, []).

decode_elements(Rest, 0, Acc) ->
    {lists:reverse(Acc), Rest};
decode_elements(Bin, N, Acc) ->
    {Type, Rest} = decode_type(Bin),
    decode_elements(Rest, N - 1, [Type | Acc]).

decode_fields(Bin, N) ->
    decode_fields(Bin, N, []).

decode_fields(Rest, 0, Acc) ->
    {lists:reverse(Acc), Rest};
decode_fields(Bin, N, Acc) ->
    {Name, Rest} = marina_types:decode_string(Bin),
    {Type, Rest2} = decode_type(Rest),
    decode_fields(Rest2, N - 1, [{Name, Type} | Acc]).

decode_result_flags(Flags) ->
    GlobalTableSpec = Flags band 1 == 1,
    HasMorePages = Flags band 2 == 2,
    NoMetaData = Flags band 4 == 4,
    {GlobalTableSpec, HasMorePages, NoMetaData}.

decode_result_paging_state(Bin, true) ->
    {Paging, Rest} = marina_types:decode_bytes(Bin),
    {Paging, Rest};
decode_result_paging_state(Rest, false) ->
    {undefined, Rest}.

decode_result_metadata(<<Flags:32/integer, ColumnsCount:32/integer, Rest/binary>>) ->
    {GlobalTableSpec, HasMorePages, NoMetaData} = decode_result_flags(Flags),
    {PagingState, Rest2} = decode_result_paging_state(Rest, HasMorePages),
    {Columns, Rest3} = case NoMetaData of
        true -> {[], Rest2};
        false ->
            decode_columns_metadata(Rest2, ColumnsCount, GlobalTableSpec)
    end,

    {#result_metadata {
        columns_count = ColumnsCount,
        columns = Columns,
        paging_state = PagingState
    }, Rest3}.

decode_rows(Bin, Count, ColumnsCount) ->
    decode_rows(Bin, Count, ColumnsCount, []).

decode_rows(Rest, 0, _ColumnsCount, Acc) ->
    {lists:reverse(Acc), Rest};
decode_rows(Bin, Count, ColumnsCount, Acc) ->
    {Row, Rest} = decode_columns(Bin, ColumnsCount),
    decode_rows(Rest, Count - 1, ColumnsCount, [Row | Acc]).

decode_type(<<16#0:16, Rest/binary>>) ->
    {Type, Rest2} = marina_types:decode_string(Rest),
    {Type, Rest2};
decode_type(<<16#1:16, Rest/binary>>) ->
    {ascii, Rest};
decode_type(<<16#2:16, Rest/binary>>) ->
    {bigint, Rest};
decode_type(<<16#3:16, Rest/binary>>) ->
    {blob, Rest};
decode_type(<<16#4:16, Rest/binary>>) ->
    {boolean, Rest};
decode_type(<<16#5:16, Rest/binary>>) ->
    {counter, Rest};
decode_type(<<16#6:16, Rest/binary>>) ->
    {decimal, Rest};
decode_type(<<16#7:16, Rest/binary>>) ->
    {double, Rest};
decode_type(<<16#8:16, Rest/binary>>) ->
    {float, Rest};
decode_type(<<16#9:16, Rest/binary>>) ->
    {int, Rest};
decode_type(<<16#B:16, Rest/binary>>) ->
    {timestamp, Rest};
decode_type(<<16#C:16, Rest/binary>>) ->
    {uid, Rest};
decode_type(<<16#D:16, Rest/binary>>) ->
    {varchar, Rest};
decode_type(<<16#E:16, Rest/binary>>) ->
    {varint, Rest};
decode_type(<<16#F:16, Rest/binary>>) ->
    {timeuuid, Rest};
decode_type(<<16#10:16, Rest/binary>>) ->
    {inet, Rest};
decode_type(<<16#20:16, Rest/binary>>) ->
    {Type, Rest2} = decode_type(Rest),
    {{list, Type}, Rest2};
decode_type(<<16#21:16, Rest/binary>>) ->
    {KeyType, Rest2} = decode_type(Rest),
    {ValueType, Rest3} = decode_type(Rest2),
    {{map, KeyType, ValueType}, Rest3};
decode_type(<<16#22:16, Rest/binary >>) ->
    {Type, Rest2} = decode_type(Rest),
    {{set, Type}, Rest2};
decode_type(<<16#30:16, Rest/binary >>) ->
    {Keyspace, Rest2} = marina_types:decode_string(Rest),
    {Name, Rest3} = marina_types:decode_string(Rest2),
    {FieldsCount, Rest4} = marina_types:decode_short(Rest3),
    {Fields, Rest5} = decode_fields(Rest4, FieldsCount),
    {{udt, Keyspace, Name, Fields}, Rest5};
decode_type(<<16#31:16, Rest/binary >>) ->
    {ElementsCount, Rest2} = marina_types:decode_short(Rest),
    {Elements, Rest3} = decode_elements(Rest2, ElementsCount),
    {{tuple, Elements}, Rest3}.
