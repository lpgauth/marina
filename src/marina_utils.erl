-module(marina_utils).
-compile(export_all).

%% public
warning_msg(Format, Data) ->
    error_logger:warning_msg(Format, Data).
