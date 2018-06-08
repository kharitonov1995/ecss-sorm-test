%%%-------------------------------------------------------------------
%%% @author Artem Kharitonov
%%% @copyright (C) 2018, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(io_gen_sorm_shell).

-callback start_shell(WorkerRef :: reference() , InitArgs :: term()) -> 
    {ok, ShellPid :: pid()} | {error, Reason :: term()}.
-callback stop_shell(ShellPid :: pid(), Reason :: term()) -> 
    ok | {error, shell_not_found}.
-callback send_message(ShellPid :: pid(), Message :: binary()) -> 
    ok | {error, shell_not_found}. 
