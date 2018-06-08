%%%-------------------------------------------------------------------
%%% @author Artem Kharitonov
%%% @copyright (C) 2018, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(call_worker).

-include_lib("chronica/include/chronica.hrl").
-include("commands.hrl").
-include("messages.hrl").
-include("sorm_constants.hrl").
-include("ecss_sorm_test.hrl").

-define(RECORD_TO_LIST(Record),
    fun(Val) ->
        Fields = record_info(fields, Record),
        [_Tag| Values] = tuple_to_list(Val),
        lists:zip(Fields, Values)
    end
).

-export([
    init/1,
    process/3,
    continue/2
]).

init(#call{options = _Options, observer = _Observer, commands = _Commands} = Call) ->
    {ok, #s_call{call = Call}}.

process(_Message, undefined, _TC) ->
    {error, undefined_call};

process(#msg{msg_id = MsgId4, msg_text = #msg_reply{msg_id = MsgId}} = Msg, 
        #s_call{call = #call{id = {CallsId, CallId2}, commands = [#mgm_cmd{id = Id, command = Command} = MgmCmd|_]}} = S, TC) ->
    MsgId2 = erlang:list_to_integer(MsgId, 16),
    MsgId3 = element(2, Command),
    if
        MsgId2 =:= MsgId3 ->
            case utils:validate_mgm_test(MgmCmd, Msg) of
                {false, Message} ->
                    Path = ion:format("~s/~s", [?CALLS_PREFIX(CallsId, CallId2), ?MGM_PREFIX(Id)]),
                    Path2 =
                    case MsgId4 of
                        ?MSG_ACK ->
                            ion:format("[~s].ack", [Path]);
                        ?MSG_EXEC ->
                            ion:format("[~s].exec", [Path])
                    end,

                    gen_server:cast(self(), {send_message, {test_failed, Path2, Message}});
                _ ->
                    ok
            end,
            {S2, NewMsg, TC2} = process_(S, TC),
            {ok, NewMsg, S2, TC2};
        true ->
            {ok, [], S, TC}
    end;

process(#notification{call_id = CallId} = N, #s_call{call_id = CallId2, hash = CallId2} = S, TC) ->
    process(N, S#s_call{call_id = CallId}, TC);

process(#notification{code_msg = CodeMsg} = N, #s_call{call_id = CallId, hash = Hash, call = #call{id = {CallsId, CallId2}, commands = 
                                                        [#recv_msg{code_msg = CodeMsg, options = Options} = RecvMsg|_]}} = S,
        TC) ->
    case check_msg(N, RecvMsg) of
        true ->
            TC3 = cancel_timer({CodeMsg, CallId, Hash}, TC),
            case process_(S, TC3) of
                {stop, Reason, TC2} ->
                    {stop, Reason, TC2};
                {S2, Msg, TC2} ->
                    {ok, Msg, S2, TC2}
            end;
        {false, Path, Message} ->
            Path2 = ion:format("~s/~s", [?CALLS_PREFIX(CallsId, CallId2), Path]),
            
            TC2 =
            case Options#options.mode of
                ?OPTIONAL_MODE ->
                    gen_server:cast(self(), {send_message, {optional_test_failed, Path2, Message}}),
                    cancel_timer({CodeMsg, CallId, Hash}, TC);
                ?REQUIRED_MODE ->
                    gen_server:cast(self(), {send_message, {test_failed, Path2, Message}}),
                    TC
            end,
            
            {ok, [], S, TC2}
    end;

%% Если пришло сообщение с другим номером, и это опциональная команда, генирируем варнинг и пропускаем сообщение
process(#notification{code_msg = _CodeMsg} = N, #s_call{call = #call{commands = [#recv_msg{options =
                                                                        #options{mode = ?OPTIONAL_MODE}} | _]}} = S,
        TC) ->
    %Path = ion:format("~s/~s", [?CALLS_PREFIX(CallsId, CallId2), ?RECEIVE_PREFIX(Id)]),
    %Message = {optional_test_failed, Path, {{msg, CodeMsg2}, {msg, CodeMsg}}},
    %gen_server:cast(self(), {send_message, Message}),

    case process_(S, TC) of
        {stop, Reason, TC2} ->
            {stop, Reason, TC2};
        {S2, [], TC2} ->
            %% Пробуем применить сообщение, которое пришло к следующей команде
            process(N, S2, TC2);
        {S2, Msg, TC2} ->
            %% Следующая команда является mgm_cmd, поэтому пришедшая нотификация является unexpected,
            %% вызываем функцию только для того, чтобы вывести варнинг
            {_, _, S3, TC3} = process(N, S2, TC2),
            {ok, Msg, S3, TC3}
    end;

process(#notification{code_msg = ?N_DISCONNECT}, #s_call{hash = Hash, call_id = CallId, 
                                                            call = #call{commands = [#recv_msg{code_msg = CodeMsg}|_]}}, TC) ->
    % Отмена таймеров, так как вызов завершился
    TC2 = cancel_timer({CodeMsg, CallId, Hash}, TC),
    TC3 = cancel_timer(Hash, TC2),
    {stop, stopped_by_user, TC3};

process(#notification{code_msg = CodeMsg}, #s_call{call_id = CallId, hash = Hash, call = #call{id = {CallsId, CallId2}, commands = 
                                                        [#recv_msg{options = Options, id = Id, code_msg = CodeMsg2}|_]}} = S, TC) ->
%    log:debug(debug, "!!! Unexpected notification !!!", []),
%    log:debug(debug, "N: ~10000p, S: ~10000p", [N, S]),
    Path = ion:format("~s/~s", [?CALLS_PREFIX(CallsId, CallId2), ?RECEIVE_PREFIX(Id)]),

    Message = {{msg, CodeMsg2}, {msg, CodeMsg}},
    TC2 =
    case Options#options.mode of
            ?OPTIONAL_MODE ->
                gen_server:cast(self(), {send_message, {optional_test_failed, Path, Message}}),
                cancel_timer({CodeMsg2, CallId, Hash}, TC);
            ?REQUIRED_MODE ->
                gen_server:cast(self(), {send_message, {test_failed, Path, Message}}),
                TC
        end,

    {ok, [], S, TC2};

process(#notification{code_msg = CodeMsg}, #s_call{call = #call{id = {CallsId, CallId2}}} = S, TC) ->
%    log:debug(debug, "!!! Unexpected notification !!!", []),
%    log:debug(debug, "N: ~10000p, S: ~10000p", [N, S]),
    Path = ion:format("~s", [?CALLS_PREFIX(CallsId, CallId2)]),
    Message = {unexpected, CodeMsg},
    gen_server:cast(self(), {send_message, {optional_test_failed, Path, Message}}),
    {ok, [], S, TC}.

continue(#s_call{call_id = _CallId} = S, TC) ->
    case process_(S, TC) of
        {stop, Reason, TC2} ->
            {stop, Reason, TC2};
        {S2, Msg, TC2} ->
            {ok, Msg, S2, TC2}
    end.

process_(#s_call{hash = Hash, call = #call{commands = [_|[]]}}, TC) ->
    TC2 = cancel_timer(Hash, TC),
    {stop, normal_ok, TC2};

process_(#s_call{call_id = CallId, hash = Hash, call = #call{id = {CallsId, CallId2}, commands = [_ | T]} = Call} = S, TC) ->
    {Msg, TC3} =
    case T of
        [#mgm_cmd{command = Command}|_] ->
            {#mgm_cmd{command = Command}, TC};
        [#recv_msg{id = Id, code_msg = CodeMsg2, options = Options}|_] ->
            log:debug(debug, "set timer cmd #~p for call: ~p", [CodeMsg2, CallId]),
            TC2 = 
            case Options#options.timeout of
                undefined ->
                    TC;
                Timeout ->
                    Path = ion:format("~s/~s", [?CALLS_PREFIX(CallsId, CallId2), ?RECEIVE_PREFIX(Id)]),
                    timer_container:start_timer({CodeMsg2, CallId, Hash}, Timeout, [Path, {mode, Options#options.mode}], TC)
            end,
            {[], TC2}
    end,
    S2 = S#s_call{call = Call#call{commands = T}},
    {S2, Msg, TC3}.

check_msg(N, #recv_msg{id = Id, number_a = NumberA2, number_b = NumberB2} = RecvMsg) ->
    [NumberA, NumberB] = utils:get_numbers_from_notification(N),
    case RecvMsg of
        #recv_msg{number_a = NumberA, number_b = NumberB, test = Test} ->
            ResultList = check_test(N, Test),
            case proplists:get_all_values(false, ResultList) of
                [] ->
                    true;
                FalseList ->
                    Message = FalseList,
                    Path = ion:format("~s/~s", [?RECEIVE_PREFIX(Id), ?TEST_PREFIX]),
                    {false, Path, Message}
            end;
        _ ->
            Message = {{NumberA2, NumberB2}, {NumberA, NumberB}},
            {false, ?RECEIVE_PREFIX(Id), Message}
    end.

check_test(#notification{call_id = CallId, num_ksl_a = KslA, num_ksl_b = KslB, mark_priority = Priority,
                            number_a = NumberA, number_b = NumberB, description_service = {_, _, SS}}, Test) ->
    NumIndicatorA = string:substr(NumberA, 1, 2),
    NumIndicatorB = string:substr(NumberB, 1, 2),

    <<Call:2/little-unit:8, TypeObj:1/unit:8, Obj:2/unit:8>> = CallId,
    [CallStr, TypeStr, ObjStr] = utils:convert_dec_to_hex([Call, TypeObj, Obj]),
    RecvTest = #test{obj = ObjStr, call = CallStr, type = TypeStr, ksl_a = KslA, ksl_b = KslB, 
                        priority = Priority, ni_a = NumIndicatorA, ni_b = NumIndicatorB, ss = SS},

    FunConvert = ?RECORD_TO_LIST(test),
    RecvTestList = FunConvert(RecvTest),
    TestList = FunConvert(Test),

    validate_test(TestList, RecvTestList, []).

validate_test([], [], Res) ->
    Res;

validate_test([{_, undefined} | T], [_ | T2], Res) ->
    validate_test(T, T2, Res);

validate_test([{Key, Value} = Test | T], [{Key, Value} | T2], Res) ->
    Res2 = {true, Test},
    validate_test(T, T2, [Res2 | Res]);

validate_test([Test | T], [Test2 | T2], Res) ->
    Res2 = {false, {Test, Test2}},
    validate_test(T, T2, [Res2 | Res]).

cancel_timer({?N_RECEIVE, _, Hash}, TC) ->
    timer_container:cancel_timer({?N_RECEIVE, Hash, Hash}, TC);

cancel_timer({CodeMsg, CallId, Hash}, TC) ->
    timer_container:cancel_timer({CodeMsg, CallId, Hash}, TC);

cancel_timer(Key, TC) ->
    timer_container:cancel_timer(Key, TC).    