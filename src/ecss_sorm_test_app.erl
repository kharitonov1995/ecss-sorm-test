%%%-------------------------------------------------------------------
%%% @author Artem Kharitonov
%%% @copyright (C) 2018, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ecss_sorm_test_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-record(options, {
    id,
    password,
    ip,
    mode,
    file,
    ex_formatter
}).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case ecss_sorm_test_sup:start_link() of
        {ok, Pid} ->
            try
                Options = get_options(),
                {ok, Ref} = start_sorm(Options),
                run(Ref, Options),
                {ok, Pid}
            catch
                _:Error ->
                    log:warning(error_log, "~p", [{Error, erlang:get_stacktrace()}]),
                    {error, {Error, erlang:get_stacktrace()}}
            end;        
        Error ->
            {error, Error}
    end.

stop(_State) ->
    ok.

start_sorm(#options{id = SormId, password = Password, ip = Ip}) ->
    case ecss_sorm_test_manager:start_sorm(SormId, Password, Ip) of
        {ok, Ref} ->
            {ok, Ref};
        Error ->
            error(Error)
    end.

get_options() ->
    Id = os:getenv("SORM_ID"),
    Mode = os:getenv("MODE"),
    ExFormatter = os:getenv("EXTEND"),
    log:info("ExFormatter: ~p", [ExFormatter]),
    ExFormatter2 = 
    try erlang:list_to_atom(ExFormatter) of
        Result ->
            log:error("!!! EXTEND OK !!!", []),
            Result
    catch 
        _:_ ->
            log:error("!!! EXTEND NOTOK !!! ~p", [{ExFormatter, erlang:get_stacktrace()}]),
            ExFormatter
    end,
    #options{id = erlang:list_to_integer(Id), password = os:getenv("PASSWORD"), ip = os:getenv("IP"), 
             mode = erlang:list_to_atom(Mode), file = os:getenv("FILE"), ex_formatter = ExFormatter2}.

run(Ref, #options{id = SormId, password = Password, mode = script, file = File, ex_formatter = ExFormatter}) ->
    case ecss_sorm_test_manager:load_file(Ref, File) of
        {error, Reason} ->
            error({error, Reason});
        _ ->
            case ecss_sorm_test_manager:start_shell(Ref, [{mode, script}, {id, SormId}, {pass, Password}, {ex_formatter, ExFormatter}]) of
                {ok, _ShellPid} ->
                    ecss_sorm_test_manager:play(Ref);
                Error ->
                    error(Error)
            end
    end;
run(Ref, #options{id = SormId, password = Password, mode = Mode, ex_formatter = ExFormatter}) ->
    InitArgs = [{mode, Mode}, {id, SormId}, {pass, Password}, {ex_formatter, ExFormatter}],
    case ecss_sorm_test_manager:start_shell(Ref, InitArgs) of
        {ok, _ShellPid} ->
            ok;
        Error ->
            error(Error)
    end.