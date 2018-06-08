%%%-------------------------------------------------------------------
%%% @author Artem Kharitonov
%%% @copyright (C) 2018, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(worker_driver).

-include_lib("chronica/include/chronica.hrl").
-include_lib("eltex_stdlib/include/ternary_operation.hrl").
-include("commands.hrl").
-include("messages.hrl").
-include("sorm_constants.hrl").
-include("ecss_sorm_test.hrl").

-define(NUMBER_A, number_a).
-define(NUMBER_B, number_b).
-define(APP_NAME, "ecss-sorm-test").

-export([
    play_script/2,
    play_script/3,
    
    check_numbers_on_control/2,
    check_msg_reply/2,
    continue_after_timer/3,
    continue/4,
    continue_buffer/4,
    send_cmd_to_smg/2,
    store_call/2,
    try_repeat/3,
    try_update_DB/3,

    save_commands/2,
    store_command/2
]).

-spec play_script(#s_worker{}, TC :: tuple()) -> {TC :: tuple(), #s_worker{}}.
play_script(#s_worker{status = undefined, mode = interactive} = S, TC) ->
    {TC, S};

play_script(#s_worker{mode = interactive} = S, TC) ->
    {TC, S#s_worker{mode = script}};

play_script(#s_worker{status = play, commands = [], shell_pid = Pid} = S, TC) ->
    io_sorm_shell:send_message(Pid, {info, end_script}),
    {TC, S#s_worker{status = undefined, buffer = []}};

play_script(#s_worker{status = play, commands = [#mgm_cmd{command = Command}|_] = List} = S, TC) ->
    {TC2, S2} = try_update_DB(Command, S, TC),
    send_cmd_to_smg(S2, #mgm_cmd{command = Command}),
    {TC2, S2#s_worker{commands = List, status = wait_ack}};

play_script(#s_worker{status = play, commands = [CallsList|Rest], call_w_db = _CallDB} = S, TC) ->
    {TC2, S2} = start_call(CallsList, S, TC),
    {TC2, S2#s_worker{status = do_call, commands = Rest}};

play_script(#s_worker{status = stop} = S, TC) ->
    {TC, S};

play_script(S, TC) ->
    {TC, S}.

play_script(#notification{code_msg = ?N_RECEIVE} = N, #s_worker{status = do_call, shell_pid = Pid, table_id = TableId} = S, TC) ->
    Numbers = check_numbers_on_control(TableId, N),
    io_sorm_shell:send_message(Pid, {msg, {N, Numbers}}),

    try
        {CallS, S3} = case get_call(N, S) of
            {ok, CallState, S2} ->
                {CallState, S2};
            {_, Reason} ->
                io_sorm_shell:send_message(Pid, {warning, Reason}),
                {undefined, S}
        end,

        call_process(N, CallS, TC, S3)
    catch
        _:Reason2 ->
            log:warning(error_log, "~p", [{Reason2, erlang:get_stacktrace()}]),
            io_sorm_shell:send_message(Pid, {error, Reason2}),
            {TC, S}
    end;

play_script(#notification{call_id = CallId} = N, #s_worker{status = do_call, shell_pid = Pid, buffer = Buffer,
                                                                table_id = TableId, mgm_call_db = MgmCallDB} = S, TC) ->
    FunFilter =
    fun
        ({CallId2, _CodeMsg}, _Value) when CallId =:= CallId2 ->
            true;
        (_Key, _Value) ->
            false
    end,

    Map = maps:filter(FunFilter, MgmCallDB),

    case maps:size(Map) of
        0 -> %% #{}
            CallS =
            case get_call(N, S) of
                {ok, CallState} ->
                    CallState;
                _ ->
                    undefined
            end,
            Numbers = check_numbers_on_control(TableId, N),
            Numbers2 = proplists:get_keys(Numbers),
            io_sorm_shell:send_message(Pid, {msg, {N, Numbers2}}),
            call_process(N, CallS, TC, S); 
        _ ->
            case lists:member({CallId, N}, Buffer) of
                true ->
                    throw({is_member, TC, S});
                _ ->
                    {TC, S#s_worker{buffer = Buffer ++ [{CallId, N}]}}
            end
    end;

play_script(#t_notification{code_msg = _CodeMsg} = N, #s_worker{shell_pid = Pid} = S, TC) ->
    io_sorm_shell:send_message(Pid, {msg, N}),
    {TC, S}.

call_process(N, CallS, TC, #s_worker{shell_pid = _Pid} = S) ->
    case call_worker:process(N, CallS, TC) of
        {ok, Msg, CallS2, TC2} ->
            S2 = store_call(CallS2, S),
            S3 = send_cmd_to_smg(S2, CallS2#s_call.call_id, Msg),
            {TC2, S3};
        {stop, Reason, TC2} ->
            {stop, Reason, TC2, CallS};
        _->
            {TC, S}
    end.

-spec continue(Msg :: #msg{}, CallS :: #s_call{}, S :: #s_worker{}, TC :: tuple()) -> {TC :: tuple(), S2 :: #s_worker{}}.
continue(Msg, CallS, S, TC) ->
    case call_worker:process(Msg, CallS, TC) of
        {ok, Msg2, CallS2, TC2} ->
            S2 = store_call(CallS2, S),
            {TC2, send_cmd_to_smg(S2, CallS2#s_call.call_id, Msg2)};
        _ ->
            {TC, S}
    end.

continue_after_timer(CallS, S, TC) ->
    case call_worker:continue(CallS, TC) of
        {ok, Msg2, CallS2, TC2} ->
            S2 = store_call(CallS2, S),
            {TC2, send_cmd_to_smg(S2, CallS2#s_call.call_id, Msg2)};
        _ ->
            {TC, S}
    end.

continue_buffer(Msg, #s_call{call_id = _CallId} = CallS, #s_worker{buffer = Buffer, call_db = CallDB} = S, TC) ->
        {TC2, S2} = continue(Msg, CallS, S, TC),

        Fun1 =
        fun(CallId2, _, {TC3, #s_worker{call_db = CallDB2} = S3}) ->
            CallS2 = maps:get(CallId2, CallDB2),
            
            case proplists:get_all_values(CallId2, Buffer) of
                [] ->
                    {TC3, S3};
                L ->
                    apply_notification_from_buffer(L, CallS2, TC3, S3)
            end
        end,

        maps:fold(Fun1, {TC2, S2}, CallDB).

check_msg_reply(#s_worker{status = start} = S, #msg{msg_id = MsgCode} = Msg) ->
    StatusMsg = ?_(string:equal(MsgCode, ?MSG_ACK), wait_ack, wait_result),
    case check_msg_reply(S#s_worker{status = StatusMsg}, Msg) of
        #s_worker{status = play} ->
            S#s_worker{status = undefined};
        #s_worker{status = _Other} ->
            S
    end;

check_msg_reply(#s_worker{status = do_call, mgm_call_db = MgmCallDB, call_db = CallDB} = S,
                #msg{msg_id = MsgCode,
                        msg_text = #msg_reply{msg_id = MsgId, flag = _}} = Msg) -> %when MsgId =:= ?C_ATTACH; MsgId =:= ?C_FREE_KSL ->
    MsgId2 = erlang:list_to_integer(MsgId, 16),
    StatusMsg = ?_(string:equal(MsgCode, ?MSG_ACK), wait_ack, wait_result),
    try
        maps:fold(fun(CallId, _, _) -> 
                case maps:get({CallId, MsgId2}, MgmCallDB, error) of
                    #mgm_call{status = StatusMsg} = MgmCall ->
                        throw(MgmCall);
                    _ ->
                        {S#s_worker.status, S}
                end
              end, 0, CallDB)
    catch
        throw:#mgm_call{call_id = CallId, status = Status} = MgmCall ->
            case check_msg_reply(S#s_worker{status = Status}, Msg) of
                #s_worker{status = play} ->
                    {play, CallId, S#s_worker{mgm_call_db = maps:remove({CallId, MsgId2}, MgmCallDB)}};
                #s_worker{status = Status2} ->
                    {Status2, S#s_worker{mgm_call_db = maps:update({CallId, MsgId2}, MgmCall#mgm_call{status = Status2}, MgmCallDB)}}
            end
    end;

check_msg_reply(#s_worker{status = wait_ack, commands = [H | Commands], shell_pid = Pid} = S,
                #msg{msg_id = ?MSG_ACK, msg_text = #msg_reply{msg_id = MsgId, flag = Flag}} = Msg) ->
    validate_test(H, Msg, Pid),

    case Flag of
        ?FLAG_OK -> 
            log:debug(debug, "Command with code ~p accepted", [MsgId]),
            S#s_worker{status = wait_result};
        _   -> 
            log:info("Command with code ~p not accepted; code error: ~p", [MsgId, Flag]),
            S#s_worker{status = play, commands = Commands}
    end;

check_msg_reply(#s_worker{status = wait_result, commands = [H | Commands], shell_pid = Pid} = S,
                #msg{msg_id = ?MSG_EXEC, msg_text = #msg_reply{msg_id = MsgId, flag = Flag}} = Msg) ->
    validate_test(H, Msg, Pid),

    case Flag of
        ?FLAG_OK ->
            log:info("Command with code ~p executed", [MsgId]);
        _   ->
            log:info("Command with code ~p not executed; code error: ~p", [MsgId, Flag])
    end,
    S#s_worker{status = play, commands = Commands};

check_msg_reply(#s_worker{status = wait_result, commands = Commands} = S, #msg{msg_id = ?MSG_TEST, msg_text = _MsgText}) ->
    S#s_worker{status = play, commands = get_rest_commands(Commands)};

check_msg_reply(S, _Msg) ->
    S.

-spec check_numbers_on_control(integer(), #notification{}) -> list().
check_numbers_on_control(TableId, #notification{trunk_group_id = ?UNDEFINED_TRUNK} = N) ->
    ListNumbers = utils:get_numbers_from_notification(N),
    check_numbers_on_control_(TableId, ListNumbers);

check_numbers_on_control(TableId, #notification{trunk_group_id = _} = N) ->
    ListNumbers = utils:get_number_and_trunk_from_notification(N),
    check_numbers_on_control_(TableId, ListNumbers).

check_numbers_on_control_(TableId, ListNumbers) ->
    L = lists:map(
                fun(Key) ->
                    case dets:lookup(TableId, Key) of
                        [] ->
                            undefined;
                        [{Number, Id}] ->
                            {Number, Id};
                        Res when is_list(Res) ->
                            Res;
                        _ ->
                            undefined
                    end
                end, ListNumbers),
    
    try lists:concat([X || X <- L, X =/= undefined]) of
        List ->
            List
    catch
        _:_ -> 
            [X || X <- L, X =/= undefined]
    end.

get_call(#notification{code_msg = ?N_RECEIVE, call_id = <<Call:2/little-unit:8, Type:1/unit:8, Obj:2/unit:8>>, trunk_group_id = Trunk} = N,
             #s_worker{call_w_db = CallWaitDB, table_id = TableId} = S) ->

    [NumberA, NumberB] = utils:get_numbers_from_notification(N),
    [CallStr] = utils:convert_dec_to_hex([Call]),
    
    KeyCallNumber = NumberA ++ NumberB ++ CallStr,
    KeyWithoutCallNumber = NumberA ++ NumberB,
    NumbersControl = check_numbers_on_control(TableId, N),
    Numbers = ?_(Type =:= ?TRUNK_TYPE, [Trunk, NumberA, NumberB], [NumberA, NumberB]),
    
    %log:debug(debug, "CallWaitDB: ~p", [CallWaitDB]),
    %log:debug(debug, "Notification: ~p", [N]),

    FunFilter =
    fun
        ({_, Obj2}) when Obj =:= Obj2, Type =:= ?TRUNK_TYPE ->
            true;
        (_) ->
            false
    end,
    
    FunSearchByKey =
    fun(Key) ->
            lists:foldl(
                    fun(Num, _) ->
                        Key2 = crypto:hash(md5, Key ++ Num),

                        %log:debug(debug, "Key: ~p", [Key]),
                        %log:debug(debug, "Key2: ~p", [Key2]),
                        %log:debug(debug, "NumberA: ~p", [NumberA]),
                        %log:debug(debug, "NumberB: ~p", [NumberB]),

                        case maps:find(Key2, CallWaitDB) of
                            {_, #call_wait{call = _, status = wait} = CallWait} ->
                                %log:debug(debug, "{Num, Obj}: ~p", [{Num, Obj}]),
                                %% Поиск среди номеров на контроле
                                case lists:member({Num, Obj}, NumbersControl) of
                                    false ->
                                        %% Поиск среди транков на контроле
                                        case lists:filter(FunFilter, NumbersControl) of
                                            [] ->
                                                {error, not_found_valid_waiting_call};
                                            _ ->
                                                throw({find, Key2, CallWait})
                                        end;
                                    _ ->
                                        throw({find, Key2, CallWait})
                                end;
                            _ ->
                                {error, not_found_valid_waiting_call}
                        end
                    end, 0, Numbers)
    end,

    try
        %% Поиск по ключам; если нашел то выкидывает throw и возвращает #s_call{}
        %% Сначала пытается найти вызовы которые были заданы с номером вызова
        %% Если не находит, ищет ожидающие вызовы, в которых не задан номер вызова
        %% Если ничего не находит возвращает ошибку 
        FunSearchByKey(KeyCallNumber),
        FunSearchByKey(KeyWithoutCallNumber)
    catch
        throw:{find, Key3, #call_wait{call = Call2}} ->
            CallWait = #call_wait{call = Call2, call_id = N#notification.call_id, status = run},
            {ok, CallSt} = call_worker:init(Call2),
            {ok, CallSt#s_call{call_id = Key3, hash = Key3}, S#s_worker{call_w_db = maps:update(Key3, CallWait, CallWaitDB)}}
    end;

get_call(#notification{code_msg = _CodeMsg, call_id = CallId}, #s_worker{call_db = CallDB}) ->
    case maps:find(CallId, CallDB) of
        {ok, Call} ->
            {ok, Call};
        error ->
            {error, not_found_call}
    end.

send_cmd_to_smg(#s_worker{sorm_id = _SormId, password = _Password, socket_msg = _Socket} = S, _, []) ->
    S;

send_cmd_to_smg(#s_worker{status = do_call} = S, CallId,
                #mgm_cmd{command = #connect_to_call{call_id = undefined} = Command}) ->
    send_cmd_to_smg(S, CallId, #mgm_cmd{command = Command#connect_to_call{call_id = CallId}});

send_cmd_to_smg(#s_worker{status = do_call} = S, CallId, 
                #mgm_cmd{command = #free_ksl{call_id = undefined} = Command}) ->
    send_cmd_to_smg(S, CallId, #mgm_cmd{command = Command#free_ksl{call_id = CallId}});

send_cmd_to_smg(#s_worker{status = do_call, mgm_call_db = MgmCallDB} = S, CallId,
                #mgm_cmd{command = Command} = MgmCmd) ->
    CodeMsg = erlang:element(2, Command),
    send_cmd_to_smg(S, MgmCmd),
    S#s_worker{mgm_call_db = maps:put({CallId, CodeMsg}, #mgm_call{call_id = CallId, code_msg = CodeMsg, status = wait_ack}, MgmCallDB)}.

send_cmd_to_smg(#s_worker{mode = interactive} = S, #mgm_cmd{command = Command}) ->
    Res = apply_command(S, Command),
    send_cmd_to_smg(S, Res);

send_cmd_to_smg(#s_worker{sorm_id = SormId, password = Password, shell_pid = ShellPid} = S, #mgm_cmd{command = Command}) ->
    Res = apply_command(S, Command),
    io_sorm_shell:send_message(ShellPid, {send, {SormId, Password, Command}}),
    send_cmd_to_smg(S, Res);

send_cmd_to_smg(#s_worker{socket_msg = Socket}, Message) ->
    case gen_tcp:send(Socket, Message) of
        ok ->
            log:debug(debug, "Send message: ~p", [Message]);
        Error ->
            throw({gen_tcp_send, Error})
    end.

start_call(#calls{list = []}, S, TC) ->
    {TC, S};

start_call(#calls{list = [#call{id = {CallsId, CallId}, options = Options, commands = [#recv_msg{id = Id, code_msg = Code, 
                                                                        options = Options2, test = #test{call = CallNum},
                                                                        number_a = NumberA, number_b = NumberB}|_]} = Call|T]},
            #s_worker{shell_pid = Pid, call_w_db = CallDB} = S, TC) ->
    SideNumber = utils:get_number_by_side(Call), %% TODO error
    Path2 = ion:format("~s/~s", [?CALLS_PREFIX(CallsId, CallId), ?RECEIVE_PREFIX(Id)]),
    %log:debug(debug, "Call: ~p", [Call]),
    %log:debug(debug, "CallNum: ~p", [CallNum]),
    %log:debug(debug, "SideNumber: ~p", [SideNumber]),
    %log:debug(debug, "!!! start_CALL CallWaitDB: ~p", [CallDB]),
    %log:debug(debug, "!!! start_CALL CallResDB: ~p", [CallResDB]),

    try
        Key =
        case CallNum of
            undefined ->
                Key2 = crypto:hash(md5, NumberA ++ NumberB ++ SideNumber),
                %log:debug(debug, "!!! START_CALL key2: ~p", [Key2]),
                case maps:find(Key2, CallDB) of
                    error ->
                        Key2;
                    _ ->
                        throw(call_undefined)
                end;
            _ ->
                crypto:hash(md5, NumberA ++ NumberB ++ CallNum ++ SideNumber)
        end,
        %log:debug(debug, "!!! START_CALL Key: ~p", [Key]),

        TC2 =
        case Options#options.timeout of
            undefined ->
                TC;
            Timeout ->
                Path = ion:format("~s", [?CALLS_PREFIX(CallsId, CallId)]),
                timer_container:start_timer(Key, Timeout, [Path], TC)
        end,

        log:debug(debug, "set timer cmd #~p", [Code]),
        TC3 =
        case Options2#options.timeout of
            undefined ->
                TC2;
            Timeout2 ->
                timer_container:start_timer({Code, Key, Key}, Timeout2, [Path2, {mode, Options2#options.mode}], TC2)
        end,
        CallDB2 = maps:put(Key, #call_wait{call = Call}, CallDB),
        start_call(#calls{list = T}, S#s_worker{call_w_db = CallDB2}, TC3)
    catch
        throw:call_undefined ->
            Error = ion:format("~s: Field Call in 'test' undefined", [Path2]),
            io_sorm_shell:send_message(Pid, {error, Error}),
            start_call(#calls{list = T}, S, TC)
    end.   

store_call(#s_call{call_id = CallId} = CallS, #s_worker{call_db = #{} = CallDB} = S) ->
    S#s_worker{call_db = maps:put(CallId, CallS, CallDB)};

store_call(#s_call{call_id = CallId} = CallS, #s_worker{call_db = CallDB} = S) ->
    S#s_worker{call_db = maps:update(CallId, CallS, CallDB)}.

try_repeat(#s_call{hash = Hash, call = #call{id = Id}} = CallS, #s_worker{call_w_db = CallWaitDB, call_res_db = CallResDB} = S, Reason) ->
    %log:debug(debug, "!!! try_REPEAT CAAAAAAAAAALDB: ~p", [CallWaitDB]),
    case try_repeat_(CallS, S) of
        {error, CallWait} ->
            CallWaitDB2 = maps:remove(Hash, CallWaitDB),
            CallResDB2 = maps:put({Id, Hash}, CallWait#call_wait{status = Reason}, CallResDB),
            S#s_worker{call_w_db = CallWaitDB2, call_res_db = CallResDB2};
        S2 ->
            S2
    end.

try_repeat_(#s_call{hash = Hash, call = #call{options = #options{repeat = Repeat} = Options}},
           #s_worker{call_w_db = CallWaitDB, commands = Commands} = S) ->
    Repeat2 = Repeat - 1,
    case maps:find(Hash, CallWaitDB) of
        {_, CallWait} ->
            case Repeat2 > 0 of
                true ->
                    CallWaitDB2 = maps:remove(Hash, CallWaitDB),
                    NewCall = (CallWait#call_wait.call)#call{options = Options#options{repeat = Repeat2}},
                    Calls = #calls{list = [NewCall], variables = []},
                    S#s_worker{status = play, commands = [Calls|Commands], call_w_db = CallWaitDB2};
                _ ->
                    {error, CallWait}
            end;
        _ ->
            S
    end.

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

try_update_DB(#wiretap_add{command_id = _} = Cmd, S, TC) ->
    try_update_DB_({insert, Cmd}, S, TC);

try_update_DB(#wiretap_del{command_id = _} = Cmd, S, TC) ->
    try_update_DB_({delete, Cmd}, S, TC);

try_update_DB(#stop_sorm{command_id = _}, S, TC) ->
    try_update_DB_(delete_all, S, TC);

try_update_DB(#msg{msg_id = ?MSG_EXEC, msg_text = #msg_reply{msg_id = MsgId, flag = ?FLAG_OK}}, 
                #s_worker{table_id = TableId, mgm_buffer = Buffer} = S, TC) ->
    MsgId2 = erlang:list_to_integer(MsgId, 16),
    IdWiretapAdd = (#wiretap_add{})#wiretap_add.command_id,
    IdWiretapDel = (#wiretap_del{})#wiretap_del.command_id,
    IdStopSorm = (#stop_sorm{})#stop_sorm.command_id,

    case MsgId2 of
        IdWiretapAdd ->
            [Command | T] = Buffer,
            Key = {insert, Command},
            TC2 = timer_container:cancel_timer(Key, TC),

            update(Command, TableId),
            {TC2, S#s_worker{mgm_buffer = T}};
        IdWiretapDel ->
            [Command | T] = Buffer,
            Key = {delete, Command},
            TC2 = timer_container:cancel_timer(Key, TC),

            update(Command, TableId),
            {TC2, S#s_worker{mgm_buffer = T}};
        IdStopSorm ->
            TC2 = timer_container:cancel_timer(delete_all, TC),
            update(#stop_sorm{}, TableId),
            {TC2, S};
        _ ->
            {TC, S}
    end;

try_update_DB(_Command, S, TC) ->
    {TC, S}.

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

update(#wiretap_add{id = <<Id:2/unit:8>>, number = <<16#FF>>, trunk_group_id = <<TrunkId:16>>}, TableId) ->
    Value = {ion:format("~2.16.0B", [TrunkId]), Id},
    dets:insert(TableId, Value);

update(#wiretap_del{id = <<Id:2/unit:8>>, number = <<16#FF>>, trunk_group_id = <<TrunkId:16>>}, TableId) ->
    Value = {ion:format("~2.16.0B", [TrunkId]), Id},
    dets:delete_object(TableId, Value);

update(#wiretap_add{id = <<Id:2/unit:8>>, number = Number}, TableId) ->
    Value = {Number, Id},
    dets:insert(TableId, Value);

update(#wiretap_del{id = <<Id:2/unit:8>>, number = Number}, TableId) ->
    Value = {Number, Id},
    dets:delete_object(TableId, Value);

update(#stop_sorm{command_id = _}, TableId) ->
    dets:delete_all_objects(TableId).

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

try_update_DB_({insert, Command}, #s_worker{mgm_buffer = Buffer} = S, TC) ->
    {timer_container:start_timer({insert, Command}, 500, [], TC), S#s_worker{mgm_buffer = Buffer ++ [Command]}};

try_update_DB_({delete, Command}, #s_worker{mgm_buffer = Buffer} = S, TC) ->
    {timer_container:start_timer({delete, Command}, 500, [], TC), S#s_worker{mgm_buffer = Buffer ++ [Command]}};

try_update_DB_(delete_all, #s_worker{} = S, TC) ->
    {timer_container:start_timer(delete_all, 500, [], TC), S}.

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

apply_command(#s_worker{sorm_id = SormId, password = Password}, Command) ->
    Fun = element(1, Command),
    erlang:apply(commands, Fun, [SormId, Password, Command]).

store_command(_Text, #s_worker{record_mode = false} = S) -> 
    S;

store_command(#notification{call_id =  <<_:2/binary, Type:1/unit:8, Obj:2/unit:8>> = CallId} = N, 
              #s_worker{table_id = TableId, record_commands = [#calls{list = List} | Rest]} = S) ->
    Command = notification2recv_msg(N),
    
    Calls =
    case proplists:get_value(CallId, List) of
        #call{commands = Commands} = Call ->
            lists:keyreplace(CallId, 1, List, {CallId, Call#call{commands = Commands ++ [Command]}});
        undefined ->
            [NumberA, _NumberB] = 
            case Type of
                16#03 ->
                    utils:get_number_and_trunk_from_notification(N);
                _ ->
                    utils:get_numbers_from_notification(N)
            end,
            Numbers = check_numbers_on_control(TableId, N),

            Call =
            case lists:member({NumberA, Obj}, Numbers) of
                true ->
                    #call{options = #options{}, observer = #observer{side = ?NUMBER_A}, commands = [Command]};
                _ ->
                    #call{options = #options{}, observer = #observer{side = ?NUMBER_B}, commands = [Command]}
            end,
            lists:keystore(CallId, 1, List, {CallId, Call})
    end,

    S#s_worker{record_commands = [#calls{list = Calls} | Rest]};

store_command(#notification{call_id = <<_:2/binary, Type:1/unit:8, Obj:2/unit:8>> = CallId} = N, 
              #s_worker{table_id = TableId, record_commands = List} = S) ->
    Command = notification2recv_msg(N),

    [NumberA, _NumberB] = 
    case Type of
        ?TRUNK_TYPE ->
            utils:get_number_and_trunk_from_notification(N);
        _ ->
            utils:get_numbers_from_notification(N)
    end,

    Numbers = check_numbers_on_control(TableId, N),
    %log:debug(debug, "NUMBERS: ~p", [Numbers]),

    Call =
    case lists:member({NumberA, Obj}, Numbers) of
        true ->
            #call{options = #options{}, observer = #observer{side = ?NUMBER_A}, commands = [Command]};
        _ ->
            #call{options = #options{}, observer = #observer{side = ?NUMBER_B}, commands = [Command]}
    end,
    Calls = #calls{list = [{CallId, Call}]},

    S#s_worker{record_commands = [Calls | List]};

store_command(#t_notification{code_msg = _CodeMsg}, #s_worker{record_commands = _Commands} = S) ->
    S;

store_command("wiretap" ++ _Rest = Text, #s_worker{record_commands = [#calls{list = List} = Calls | Rest]} = S) -> 
    case string:tokens(Text, " ") of
        [_Command, CallId, Ksl] ->
            ResultText = ion:format("wiretap <CallId> ~s", [Ksl]),
            List2 = store_mgm_cmd2call(ResultText, CallId, List),

            S#s_worker{record_commands = [Calls#calls{list = List2} | Rest]};
        _ ->
            S
    end;

store_command("free-ksl" ++ _Rest = Text, #s_worker{record_commands = [#calls{list = List} = Calls | Rest]} = S) -> 
    case string:tokens(Text, " ") of
        [_Command, CallId] ->
            ResultText = ion:format("free-ksl <CallId>", []),
            List2 = store_mgm_cmd2call(ResultText, CallId, List),

            S#s_worker{record_commands = [Calls#calls{list = List2} | Rest]};
        _ ->
            S
    end;

store_command(Text, #s_worker{record_commands = Commands} = S) ->
    S#s_worker{record_commands = [[Text] | Commands]}.

save_commands(FileName, #s_worker{shell_pid = Pid, record_commands = Commands}) ->
    log:debug(debug, "~p", [Commands]),

    F =
    fun
        ({_, #call{commands = CommandLocal} = Call}) ->
            Call#call{commands = CommandLocal};
        (Other) ->
            Other
    end,

    F2 =
    fun
        (#calls{list = Commands3} = Calls) ->
            Calls#calls{list = lists:map(F, Commands3)};
        (Other) ->
            Other
    end, 

    Commands2 = lists:map(F2, Commands),
    CommandsReverse = lists:reverse(Commands2),

    %% Сохранение в файл
    AbsName =
    case filename:dirname(FileName) of
        "." ->
            [Root, Home, User | _] = filename:split(filename:basedir(user_data, ?APP_NAME)),
            PathFolder = filename:join([Root, Home, User, ?APP_NAME]),
            file:make_dir(PathFolder),
            filename:join([PathFolder, FileName]);
        _ ->
            FileName
    end,

    CommandsJsx = json_helper:convert_to_jsx(CommandsReverse),
    CommandsEncode = jsx:encode(CommandsJsx, [{space, 1}, {indent, 4}]),

    case file:open(AbsName, [write]) of
        {ok, File} ->
            file:write(File, CommandsEncode),
            file:close(File);
        {error, Reason} ->
            Message = {error, Reason},
            io_sorm_shell:send_message(Pid, Message),
            log:warning(error_log, "~p", [{error, Reason, erlang:get_stacktrace()}])
    end.

notification2recv_msg(#notification{code_msg = CodeMsg, call_id = CallId, number_a = NumberA, number_b = NumberB, num_ksl_a = KslA, 
                                    num_ksl_b = KslB, mark_priority = Priority, description_service = {_, _, SS}} = N) ->
    [NumberA2, NumberB2] = utils:get_numbers_from_notification(N),

    NumIndicatorA = string:substr(NumberA, 1, 2),
    NumIndicatorB = string:substr(NumberB, 1, 2),

    <<Call:2/little-unit:8, Type:1/unit:8, Obj:2/unit:8>> = CallId,

    [CallStr, TypeStr, ObjStr] = utils:convert_dec_to_hex([Call, Type, Obj]),

    Test = #test{call = CallStr, type = TypeStr, obj = ObjStr, ni_a = NumIndicatorA, ni_b = NumIndicatorB, 
                ksl_a = KslA, ksl_b = KslB, priority = Priority, ss = SS},

    #recv_msg{options = #options{}, number_a = NumberA2, number_b = NumberB2, code_msg = CodeMsg, test = Test}.

-spec store_mgm_cmd2call(Command :: string(), CallId :: string(), Calls :: [{binary(), #call{}}]) -> [{CallId :: binary(), #call{}}].
store_mgm_cmd2call(Command, CallId, Calls) ->
    CallIdInt = erlang:list_to_integer(CallId, 16),
    CallIdBin = <<CallIdInt:40>>,

    case proplists:get_value(CallIdBin, Calls) of
        #call{commands = Commands} = Call ->
            lists:keyreplace(CallIdBin, 1, Calls, {CallIdBin, Call#call{commands = Commands ++ [Command]}});
        _ ->
            Calls
    end.

-spec get_rest_commands(Commands :: list()) -> RestCommads :: list() | [].
get_rest_commands([_|T]) ->
    T;

get_rest_commands([]) ->
    [].

apply_notification_from_buffer(L, CallS, TC, S) ->
    Fun =
    fun
        (N, {#s_call{call_id = CallId}, TC2, #s_worker{buffer = Buffer} = S2}) ->
                        
            case play_script(N, S2, TC2) of
                {TC3, S3} ->
                    S4 = S3#s_worker{buffer = lists:delete({CallId, N}, Buffer)},

                    {ok, CallS2} = get_call(N, S4),
                    {CallS2, TC3, S4};
                {stop, _, _, _} = Stop->
                    throw({Stop, S2})
            end
    end,

    try
        {_, TC2, S2} = lists:foldl(Fun, {CallS, TC, S}, L),
        {TC2, S2}
    catch
        _:{is_member, TC4, S4} ->
            {TC4, S4}
    end.

validate_test(#mgm_cmd{id = Id} = Command, #msg{msg_id = MsgId} = Msg, Pid) ->
    case utils:validate_mgm_test(Command, Msg) of
        {false, Message} ->
            Path =
            case MsgId of
                ?MSG_ACK ->
                    ion:format("[~s].ack", [?MGM_PREFIX(Id)]);
                ?MSG_EXEC ->
                    ion:format("[~s].exec", [?MGM_PREFIX(Id)])
            end,

            io_sorm_shell:send_message(Pid, {test_failed, Path, Message});
        _ ->
            ok
    end.
