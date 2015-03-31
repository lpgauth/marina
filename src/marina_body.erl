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

    {_Code, Rest} = marina_types:decode_int(Body),
    {Msg, _Rest2} = marina_types:decode_string(Rest),
    {error, Msg};
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
decode(#frame {}) ->
    {error, unknown_response}.

%% private
decode_columns(Bin, Count) ->
    decode_columns(Bin, Count, []).

decode_columns(Rest, 0, Acc) ->
    {lists:reverse(Acc), Rest};
decode_columns(Bin, Count, Acc) ->
    {Column, Rest} = marina_types:decode_bytes(Bin),
    decode_columns(Rest, Count - 1, [Column | Acc]).

decode_columns_metadata(Bin, Count, GlobalTableSpec) ->
    decode_columns_metadata(Bin, Count, GlobalTableSpec, []).

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

decode_result_metadata(<<Flags:32/integer, ColumnsCount:32/integer, Rest/binary>>) ->
    GlobalTableSpec = Flags band 1,
    HasMorePages = Flags band 2,
    NoMetaData = Flags band 4,

    {_PagingState, Rest2} = case HasMorePages of
        0 -> {undefined, Rest};
        1 -> marina_types:decode_bytes(Rest)
    end,

    {Columns, Rest5} = case NoMetaData of
        0 ->
            case GlobalTableSpec of
                0 ->
                    Keyspace = undefined,
                    Table = undefined,
                    Rest4 = Rest2;
                1 ->
                    {Keyspace, Rest3} = marina_types:decode_string(Rest2),
                    {Table, Rest4} = marina_types:decode_string(Rest3)
            end,
            decode_columns_metadata(Rest4, ColumnsCount, {Keyspace, Table});
        4 ->
            {[], Rest2}
    end,

    {#result_metadata {
        columns_count = ColumnsCount,
        columns = Columns
    }, Rest5}.

decode_rows(Bin, Count, ColumnsCount) ->
    decode_rows(Bin, Count, ColumnsCount, []).

decode_rows(Rest, 0, _ColumnsCount, Acc) ->
    {lists:reverse(Acc), Rest};
decode_rows(Bin, Count, ColumnsCount, Acc) ->
    {Row, Rest} = decode_columns(Bin, ColumnsCount),
    decode_rows(Rest, Count - 1, ColumnsCount, [Row | Acc]).

decode_type(<<0:16, Rest/binary>>) ->
    {Type, Rest2} = marina_types:decode_string(Rest),
    {Type, Rest2};
decode_type(<<1:16, Rest/binary>>) ->
    {ascii, Rest};
decode_type(<<2:16, Rest/binary>>) ->
    {bigint, Rest};
decode_type(<<3:16, Rest/binary>>) ->
    {blob, Rest};
decode_type(<<4:16, Rest/binary>>) ->
    {boolean, Rest};
decode_type(<<5:16, Rest/binary>>) ->
    {counter, Rest};
decode_type(<<6:16, Rest/binary>>) ->
    {decimal, Rest};
decode_type(<<7:16, Rest/binary>>) ->
    {double, Rest};
decode_type(<<8:16, Rest/binary>>) ->
    {float, Rest};
decode_type(<<9:16, Rest/binary>>) ->
    {int, Rest};
decode_type(<<11:16, Rest/binary>>) ->
    {timestamp, Rest};
decode_type(<<12:16, Rest/binary>>) ->
    {uid, Rest};
decode_type(<<13:16, Rest/binary>>) ->
    {varchar, Rest};
decode_type(<<14:16, Rest/binary>>) ->
    {varint, Rest};
decode_type(<<15:16, Rest/binary>>) ->
    {timeuuid, Rest};
decode_type(<<16:16, Rest/binary>>) ->
    {inet, Rest}.
