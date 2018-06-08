%%%-------------------------------------------------------------------
%%% @author Artem Kharitonov
%%% @copyright (C) 2018, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ecss_sorm_test_worker).

-include_lib("chronica/include/chronica.hrl").
-include("commands.hrl").
-include("messages.hrl").
-include("sorm_constants.hrl").
-include("ecss_sorm_test.hrl").

-behaviour(gen_tcserver).

%%Callbacks
-export([
    init/1,
    handle_call/4,
    handle_cast/3,
    handle_info/3,
    handle_timeout/6,
    terminate/2,
    code_change/4
]).

%%API
-export([
    set_list_commands/1,
    stop/0,
    start_link/5
]).

-record(start_shell, {
    init_args
}). 

-record(play, {
}).

-record(pause, {
}).

-record(resume, {
}).

-record(stop_play, {
}).

-record(set_list_commands, {
    list_commands
}).


%%====================================================================
%% API
%%====================================================================
-spec set_list_commands(list()) -> ok | {error, reason}.
set_list_commands(ListCommands) ->
    gen_server:call(?MODULE, #set_list_commands{list_commands = ListCommands}).

start_link(SormId, Password, SocketNotification, SocketMessage, Name) ->
    gen_tcserver:start({local, Name}, ?MODULE, [SormId, Password, SocketNotification, SocketMessage], []).

stop() ->
    gen_server:call(?MODULE, #stop{}).

%%====================================================================
%% Callbacks
%%====================================================================

init([SormId, Password, SocketN, SocketMsg]) ->
    TC = timer_container:init([]),
    {_, NameDB} = application:get_env(db_name),

    {_, TableID} = 
    case filelib:is_file(NameDB) of
        false ->
            dets:open_file(abonents, [{file, NameDB}, {type, bag}]);
        _ ->
            dets:open_file(NameDB)
    end,

    S = #s_worker{status = start, sorm_id = SormId, password = erlang:list_to_binary(Password),
                    socket_n = SocketN, socket_msg = SocketMsg, table_id = TableID},

    send_message(S, #get_version{}),

    {ok, S, {timer_container, TC}}.

handle_call(#start_shell{init_args = InitArgs}, _From, TC, S) ->
    try
        setup_shell(InitArgs),
        receive
            {ok, ShellPid} ->
                {reply, {ok, ShellPid}, TC, S#s_worker{shell_pid = ShellPid}};
            {error, Reason2} ->
                log:warning(error_log, "[Start Shell]: ~10000p", [{error, Reason2}]),
                {reply, {error, Reason2}, TC, S}
        end
    catch
        error:Reason ->
            log:warning(error_log, "[Setup Shell]: ~p", [{Reason, erlang:get_stacktrace()}]),
            {reply, {error, Reason}, TC, S} 
    end;

handle_call(#set_list_commands{list_commands = []}, _From, TC, S) ->
    {reply, {error, list_is_empty}, TC, S};

handle_call(#set_list_commands{list_commands = ListCommands}, _From, TC, #s_worker{shell_pid = Pid} = S) ->
    try
        S2 = generate_call_id_state(S#s_worker{call_w_db = maps:new()}, ListCommands), %% ИСПРАВИТЬ!!! обновляется база данных ожидаемых вызовов
        file:write_file(?SAVE_COMMANDS_PATH, erlang:term_to_binary(ListCommands)),

        {reply, ok, TC, S2#s_worker{commands = ListCommands, call_db = maps:new()}}
    catch
        throw:{call_undefined, Path} ->
            log:debug(debug, "!!! CALL UNDEFINED !!!", []),
            Error = ion:format("~s: Field Call in 'test' undefined", [Path]),
            io_sorm_shell:send_message(Pid, {error, Error}),
            {reply, error, TC, S}
    end;

handle_call(#stop{}, _From, TC, S) ->
    worker_driver:play_script(S#s_worker{status = stop}, TC),
    file:delete(?SAVE_COMMANDS_PATH),
    {stop, normal, ok, TC, S};

handle_call(_Message, _From, TC, S) ->
    {reply, ok, TC, S}.

handle_cast(#play{}, TC, S) ->
    {TC2, S2} = 
    try 
        worker_driver:play_script(S#s_worker{status = play, mode = script}, TC)
    catch
        _:Reason ->
            log:warning(error_log, "~p", [{Reason, erlang:get_stacktrace()}]),
            {TC, S}
    end,
    {noreply, TC2, S2};

handle_cast(#repeat{}, TC, S) ->
    repeat(TC, S);

handle_cast(#stop_play{}, TC, S) ->
    {noreply, TC, S};

handle_cast(#pause{}, TC, S) ->
    {noreply, TC, S};

handle_cast(#resume{}, TC, S) ->
    {noreply, TC, S};

handle_cast(#interactive_mode{id = Id, password = Password, command = Command}, TC, #s_worker{shell_pid = Pid} = S) ->
    S2 = S#s_worker{sorm_id = Id, password = Password, mode = interactive},
    try
        {TC2, S3} = worker_driver:try_update_DB(Command, S2, TC),
        send_message(S3, Command),
        {noreply, TC2, S3}
    catch 
        _:Error ->
            log:warning(error_log, "~p~n", [{Error, erlang:get_stacktrace()}]),
            io_sorm_shell:send_message(Pid, {error, Error}),
            {noreply, TC, S2}
    end;

handle_cast(#record_start{}, TC, S) ->
    {noreply, TC, S#s_worker{record_mode = true}};

handle_cast(#record_stop{filename = FileName}, TC, S) ->
    worker_driver:save_commands(FileName, S),
    {noreply, TC, S#s_worker{record_mode = false}};

handle_cast(#record_command{command = Command}, TC, S) ->
    S2 = worker_driver:store_command(Command, S),
    {noreply, TC, S2};

handle_cast({send_message, Message}, TC, #s_worker{shell_pid = Pid} = S) ->
    io_sorm_shell:send_message(Pid, Message),
    {noreply, TC, S};

handle_cast(_Message, TC, S) ->
    {noreply, TC, S}.

handle_info({tcp, _Socket, RcvdData}, TC, #s_worker{socket_msg = _Socket, status = do_call, shell_pid = Pid, buffer = Buffer} = S) ->
    Msg = utils:parse_message(RcvdData),
    try
        {TC3, S4} =
        lists:foldl(
            fun(Msg2, {TC2, S2}) -> 
                io_sorm_shell:send_message(Pid, {msg, Msg2}),
                %log:debug(debug, "MSG DO_CALL: ~p", [Msg2]),
                %log:debug(debug, "MgmCallDB: ~p", [S2#s_worker.mgm_call_db]),
                case worker_driver:check_msg_reply(S2, Msg2) of
                    {play, CallId, S3} ->
                        {_, CallS} = maps:find(CallId, S3#s_worker.call_db),
                        case Buffer of
                            [] ->
                                worker_driver:continue(Msg2, CallS, S3, TC2);
                            _ ->
                                worker_driver:continue_buffer(Msg2, CallS, S3, TC2)
                        end;
                    {_, S3} ->
                        {TC2, S3};
                    _ ->
                        {TC2, S2}
                end
            end,
            {TC, S}, Msg),
        %log:debug(debug, "S444444: ~p", [S4]),
        %log:debug(debug, "TCCCCCCC3333333: ~p", [TC3]),
        {noreply, TC3, S4}
    catch
        throw:{{stop, Reason, TC4, #s_call{call_id = CallId, call = {CallsId, CallId2}} = CallS2}, #s_worker{call_db = CallDB} = S5} ->
            Path = ion:format("~s", [?CALLS_PREFIX(CallsId, CallId2)]),
            io_sorm_shell:send_message(Pid, {info, {call_stop, Reason, Path}}),
            CallDB2 = maps:remove(CallId, CallDB),
            S6 = worker_driver:try_repeat(CallS2, S5, Reason),
            {TC5, S7} = try_play(S6#s_worker{call_db = CallDB2}, TC4),
            {noreply, TC5, S7};
        _:Reason ->
            io_sorm_shell:send_message(Pid, {error, Reason}),
            log:warning(error_log, "~p", [{error, {Reason, erlang:get_stacktrace()}}]),
            {noreply, TC, S}
    end;

handle_info({tcp, _Socket, RcvdData}, TC, #s_worker{status = Status, socket_msg = _Socket, shell_pid = Pid} = S) ->
    Msg = utils:parse_message(RcvdData),
    try
        {TC4, S4} = lists:foldl(
                fun(Msg2, {TC2, S2}) ->
                    case Status of
                        start ->
                            ok;
                        _ ->
                            io_sorm_shell:send_message(Pid, {msg, Msg2})
                    end,
                    log:debug(debug, "MSG: ~p", [Msg2]),

                    {TC3, S3} = worker_driver:try_update_DB(Msg2, S2, TC2),
                    {TC3, worker_driver:check_msg_reply(S3, Msg2)}
                end, {TC, S}, Msg),
        {TC5, S5} = worker_driver:play_script(S4, TC4),
        {noreply, TC5, S5}
    catch
        throw:Error ->
            log:warning(error_log, "{~p, {~100000p~n~100000p}}", [Error, Msg, RcvdData]),
            {noreply, TC, S};
        _:Reason ->
            io_sorm_shell:send_message(Pid, {error, Reason}),
            log:warning(error_log, "~p", [{error, {Reason, erlang:get_stacktrace()}}]),
            {noreply, TC, S}
    end;

handle_info({tcp, _Socket, RcvdData}, TC, #s_worker{status = do_call} = S) ->
    Notifications = utils:parse_notification(RcvdData),
    %log:debug(debug, "N: ~p", [Notifications]),
    % Вынести в отдельную функцию
    Fun =
    fun
        (N, {TC2, #s_worker{call_db = CallDB, shell_pid = Pid} = S2}) ->
            case worker_driver:play_script(N, S2, TC2) of
                {TC3, S3} ->
                    {TC3, S3};
                {stop, Reason, TC3, #s_call{call_id = CallId, call = #call{id = {CallsId, CallId2}}} = CallS} ->
                    Path = ion:format("~s", [?CALLS_PREFIX(CallsId, CallId2)]),
                    io_sorm_shell:send_message(Pid, {info, {call_stop, Reason, Path}}),
                    CallDB2 = maps:remove(CallId, CallDB),
                    S3 = worker_driver:try_repeat(CallS, S2, Reason),
                    %log:debug(debug, "!!! CALL_WAit_DB after repeat: ~p", [S3#s_worker.call_w_db]),
                    %log:debug(debug, "!!! CALL_RES_DB after repeat: ~p", [S3#s_worker.call_res_db]),
                    try_play(S3#s_worker{call_db = CallDB2}, TC3)
            end
    end,

    {ResTC, ResState} = lists:foldl(Fun, {TC, S}, Notifications),
    {noreply, ResTC, ResState};

handle_info({tcp, _Socket, RcvdData}, TC, #s_worker{shell_pid = Pid, table_id = TableId} = S) ->
    Notifications = utils:parse_notification(RcvdData),

    % Fun2 =
    % fun
    %     (Key, _) ->
    %         log:debug(debug, "Keeeey: ~p", [Key])
    % end,

    % log:debug(debug, "N: ~p", [Notifications]),
    % dets:foldl(Fun2, 0, TableId),

    % Вынести в отдельную функцию
    Fun =
    fun
        (#notification{code_msg = _} = N, S2) ->
            Numbers = worker_driver:check_numbers_on_control(TableId, N),
            io_sorm_shell:send_message(Pid, {msg, {N, Numbers}}),
            worker_driver:store_command(N, S2);
        (#t_notification{code_msg = _} = N, S2) ->
            io_sorm_shell:send_message(Pid, {msg, N}),
            worker_driver:store_command(N, S2)
    end,

    S3 = lists:foldl(Fun, S, Notifications),
    {noreply, TC, S3};
handle_info(_Message, TC, S) ->
    {noreply, TC, S}.

handle_timeout(TCID, _TID, _Args, _TTime, TC, #s_worker{status = Status} = S) when Status =:= play; Status =:= stop ->
    TC2 = timer_container:cont_cancel_timers(TCID, TC),
    {noreply, TC2, S};

handle_timeout(_TCID, {CodeMsg, CallId, _Hash}, [Path, {mode, ?OPTIONAL_MODE}], _TTime, TC,
                    #s_worker{call_db = CallDB, shell_pid = Pid} = S) ->
    io_sorm_shell:send_message(Pid, {info, {optional_cmd_timeout, {Path, CodeMsg}}}),
    {TC2, S2} = case maps:find(CallId, CallDB) of
        {ok, CallS} ->
            worker_driver:continue_after_timer(CallS, S, TC);
        error ->
            {TC, S}
    end,
    TC3 = timer_container:timer_ack(TC2),
    {noreply, TC3, S2};

handle_timeout(_TCID, {CodeMsg, _, _} = TID, [Path, _Mode], _TTime, TC, #s_worker{shell_pid = Pid} = S) ->
    io_sorm_shell:send_message(Pid, {info, {timeout_exec_cmd, {Path, CodeMsg}}}),
    TC2 = timer_container:timer_ack(TC),
    {TC3, S2} = call_timer_handle(TID, S, TC2),
    {noreply, TC3, S2};

handle_timeout(TCID, Key, [Path], _TTime, TC, #s_worker{shell_pid = Pid} = S) ->
    io_sorm_shell:send_message(Pid, {info, {call_timeout, Path}}),
    TC2 = timer_container:timer_ack(TC),

    {TC4, S3} =
    case call_timer_handle(Key, S, TC2) of
        {TC3, #s_worker{status = do_call} = S2} ->
            {TC3, S2};
        {TC3, S2} ->
            {timer_container:cont_cancel_timers(TCID, TC3), S2}
    end,

    {noreply, TC4, S3};

handle_timeout(_TCID, _TID, _Args, _TTime, TC, S) ->
    TC2 = timer_container:timer_ack(TC),
    log:debug(debug, "!!! Undefined timer !!!", []),
    {noreply, TC2, S}.

terminate(_Reason, _S) -> 
    ok.
code_change(_OldVersion, TC, S, _Extra) -> 
    {ok, TC, S}.

%%====================================================================
%% Private
%%====================================================================

generate_call_id_state(#s_worker{call_w_db = _} = S, []) ->
    S;

generate_call_id_state(#s_worker{call_w_db = CallDB} = S, [#call{id = Id, commands = [
                           #recv_msg{id = Id2, number_a = NumberA, number_b = NumberB, test = #test{call = CallNum}}|_]} = Call|T]) ->
    SideNumber = utils:get_number_by_side(Call),

    case CallNum of
        undefined ->
            Key = crypto:hash(md5, NumberA ++ NumberB ++ SideNumber),

            case maps:find(Key, CallDB) of
                error ->
                    generate_call_id_state(S#s_worker{call_w_db = maps:put(Key, Call, CallDB)}, T);
                _ ->
                    {CallsId, CallId} = Id,
                    Path = ion:format("~s/~s", [?CALLS_PREFIX(CallsId, CallId), ?RECEIVE_PREFIX(Id2)]),
                    throw({call_undefined, Path})
            end;
        _ ->
            Key = crypto:hash(md5, NumberA ++ NumberB ++ CallNum ++ SideNumber),
            generate_call_id_state(S#s_worker{call_w_db = maps:put(Key, Call, CallDB)}, T)
    end;

generate_call_id_state(#s_worker{call_w_db = _} = S, [_|T]) ->
    generate_call_id_state(S, T).

call_timer_handle({?N_RECEIVE, _, Hash}, #s_worker{call_w_db = CallWaitDB, call_db = CallDB} = S, TC) ->
    case maps:find(Hash, CallWaitDB) of
        {ok, #call_wait{call = Call, call_id = CallId}} ->
            S2 = worker_driver:try_repeat(#s_call{hash = Hash, call = Call}, S, failed),
            TC2 = timer_container:cancel_timer(Hash, TC),
            CallDB2 = maps:remove(CallId, CallDB),

            try_play(S2#s_worker{call_db = CallDB2}, TC2);
        _ ->
            {TC, S}
    end;

call_timer_handle({_CodeMsg, CallId, Hash}, #s_worker{call_db = CallDB} = S, TC) ->
    case maps:find(CallId, CallDB) of
        {ok, #s_call{hash = _} = CallS} ->
            S2 = worker_driver:try_repeat(CallS, S, failed),
            CallDB2 = maps:remove(CallId, CallDB),
            S3 = S2#s_worker{call_db = CallDB2},
            TC2 = timer_container:cancel_timer(Hash, TC),

            case maps:size(CallDB2) of
                0 ->
                    try_play(S3, TC2);
                _ ->
                    worker_driver:play_script(S3, TC2)
            end;
        _ ->
            {TC, S}
    end;

call_timer_handle(Key, #s_worker{call_w_db = CallWaitDB, call_db = CallDB} = S, TC) ->
    case maps:find(Key, CallWaitDB) of
        {ok, #call_wait{call = Call, call_id = CallId}} ->
            CallDB2 = maps:remove(CallId, CallDB),
            S2 = worker_driver:try_repeat(#s_call{hash = Key, call = Call}, S, failed),
            try_play(S2#s_worker{call_db = CallDB2}, TC);
        error ->
            {TC, S}
    end. 

try_play(#s_worker{call_w_db = CallWaitDB, call_db = CallDB} = S, TC) ->
    case lists:keymember(wait, 2, maps:values(CallWaitDB)) of
        false ->
            case maps:size(CallDB) of
                0 ->
                    worker_driver:play_script(S#s_worker{status = play}, TC);
                _ ->
                    {TC, S}
            end;
        _ ->
            worker_driver:play_script(S, TC)
    end.

setup_shell(InitArgs) ->
    %% terminate the current user
    ok = supervisor:terminate_child(kernel_sup, user),
    %% start a new shell (this also starts a new user under the correct group)
    _ = user_drv:start('tty_sl -c -e', {io_sorm_shell, start_shell, [self(), InitArgs]}),
    %% wait until user_drv and user have been registered (max 3 seconds)
    wait_until_user_started(3000),
    try
        %% enable error_logger's tty output
        error_logger:swap_handler(tty),
        %% disable the simple error_logger (which may have been added multiple
        %% times). removes at most the error_logger added by init and the
        %% error_logger added by the tty handler
        remove_error_handler(3)
    catch
        E:R -> % may fail with custom loggers
            log:waning(error_log, "Logger changes failed for ~p:~p (~p)", [E, R, erlang:get_stacktrace()]),
            hope_for_best
    end.

remove_error_handler(0) ->
    log:warning("Unable to remove simple error_logger handler");
remove_error_handler(N) ->
    case gen_event:delete_handler(error_logger, error_logger, []) of
        {error, module_not_found} -> ok;
        {error_logger, _} -> remove_error_handler(N - 1)
    end.

%% Timeout is a period to wait before giving up
wait_until_user_started(0) ->
    log:warning(error_log, "~p", ["Timeout exceeded waiting for `user` to register itself"]),
    erlang:error(timeout);
wait_until_user_started(Timeout) ->
    case whereis(user) of
        %% if user is not yet registered wait a tenth of a second and try again
        undefined -> timer:sleep(100), wait_until_user_started(Timeout - 100);
        _ -> ok
    end.

send_message(S, Message) when is_tuple(Message) ->
    worker_driver:send_cmd_to_smg(S, #mgm_cmd{command = Message});

send_message(S, Message) ->
    worker_driver:send_cmd_to_smg(S, Message).

repeat(TC, #s_worker{shell_pid = ShellPid, sorm_id = SormId, password = Password, 
                        socket_n = SocketN, socket_msg = SocketMsg, table_id = TableID}) ->
    TC2 = timer_container:cont_cancel_timers(default, TC),
    S2 = #s_worker{shell_pid = ShellPid, sorm_id = SormId, password = Password, socket_n = SocketN, 
                        socket_msg = SocketMsg, table_id = TableID},

    case file:read_file(?SAVE_COMMANDS_PATH) of
        {ok, Binary} ->
            ListCommands = erlang:binary_to_term(Binary),
            {TC3, S3} =
            try 
                worker_driver:play_script(S2#s_worker{status = play, mode = script, commands = ListCommands}, TC2)
            catch
                _:Reason ->
                    log:warning(error_log, "~p", [{Reason, erlang:get_stacktrace()}]),
                    {TC2, S2}
            end,

            {noreply, TC3, S3};
        {error, Reason} ->
            io_sorm_shell:send_message(ShellPid, {error, Reason}),
            {noreply, TC, S2}
    end.