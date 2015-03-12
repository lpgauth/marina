-module(marina_buffer).
-include("marina.hrl").

-export([
    new/0,
    decode/2
]).

%% public
-spec new() -> buffer().
new() ->
    #buffer {
        buffered = [],
        current = 0,
        pending = undefined
    }.

-spec decode(binary(), buffer()) -> {[frame()], buffer()}.
decode(Data, #buffer {
        buffered = Buffered,
        current = Current,
        pending = Pending
    } = Buffer) when is_integer(Pending)
                andalso Current + size(Data) < Pending ->

    {[], Buffer#buffer {
        buffered = [Data | Buffered],
        current = Current + size(Data)
    }};
decode(Data, #buffer {buffered = Buffered}) ->
    Data2 = list_to_binary(lists:reverse([Data | Buffered])),
    {Rest, Frames} = marina_frame:decode(Data2),

    {Frames, #buffer {
        buffered = [Rest],
        current = size(Rest),
        pending = marina_frame:pending_size(Rest)
    }}.
