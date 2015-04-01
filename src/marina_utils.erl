-module(marina_utils).
-compile(export_all).

%% public
timeout(Timeout, Timestamp) ->
    Diff = timer:now_diff(os:timestamp(), Timestamp) div 1000,
    case Timeout - Diff of
        Timeout2 when Timeout2 < 0 -> 0;
        Timeout2 -> Timeout2
    end.

warning_msg(Format, Data) ->
    error_logger:warning_msg(Format, Data).
