%%%-------------------------------------------------------------------
%%% @author Artem Kharitonov
%%% @copyright (C) 2018, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(json_helper).

-include_lib("chronica/include/chronica.hrl").
-include("ecss_sorm_test.hrl").

-export([
    parse_json/1,
    convert_to_jsx/1
]).

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

parse_json(JsonText) ->
    DecodeJson = jsx:decode(JsonText),
    convert_json(DecodeJson).

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

convert_json(DecodeJson) -> 
    convert_json_local(DecodeJson, []).

convert_json_local([], Commands) ->
    log:debug(debug, "Commands: ~p", [Commands]),
    lists:reverse(Commands);

convert_json_local([Command | T], Commands) when is_binary(Command) ->
    Command2 = erlang:binary_to_list(Command),
    MgmCmd = do_mgm_cmd(Command2),
    MgmCmd2 = MgmCmd#mgm_cmd{id = generate_id_mgm(Commands)},
    convert_json_local(T, [MgmCmd2 | Commands]);

convert_json_local([[{<<"test-mgm">>, ValueList}] | T], Commands) ->
    MgmCmd = test2mgm(ValueList),
    %log:debug(debug, "MgmCmd2: ~p", [MgmCmd2]),
    convert_json_local(T, [MgmCmd#mgm_cmd{id = generate_id_mgm(Commands)} | Commands]);

%% convert_json_local(list([<<calls>>], Commands)) ->  #calls{}
convert_json_local([H | T], Commands) ->
    Commands2 =
    case proplists:get_value(<<"calls">>, H) of
        undefined ->
            Commands;
        Calls ->
            %log:debug(debug, "List CALLS: ~p", [Calls]),
            CallsId = generate_calls_id(Commands),
            Calls2 = lists:concat(Calls),

            VariablesRaw = proplists:get_value(<<"variables">>, Calls2),
            ListCallRaw = proplists:lookup_all(<<"call">>, Calls2),

            Fun = 
            fun({<<"call">>, List}) ->
                lists:flatmap(fun thing_to_list/1, List)
            end,
            ListCallRaw2 = lists:map(Fun, ListCallRaw),

            Variables = get_variables(VariablesRaw, []),

            Variables =/= undefined andalso
                lists:map(
                        fun(#variable{name = Name, value = Value}) ->
                                erlang:put(Name, Value)
                        end, Variables),

            ListCall = convert_calls2records(CallsId, ListCallRaw2, []),

            Variables =/= undefined andalso
                lists:map(
                        fun(#variable{name = Name}) ->
                            erlang:erase(Name)
                        end, Variables),

            CallsRes = #calls{variables = Variables, list = ListCall, id = CallsId},

            [CallsRes | Commands]
    end,
    convert_json_local(T, Commands2).

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

do_mgm_cmd(Command) ->
    case sorm_shell_scan:string(Command) of
        {_, Tokens, _} ->
            case sorm_shell_parse:parse(Tokens) of 
                {ok, {Command2, Args}} ->
                    Id = erlang:get(?SORM_ID),
                    Password = erlang:get(?PASSWORD),
                    [_, _ | Args2] = utils:check_id_and_password(Args, Id, Password),
                    Command3 = utils:do_cmd(Command2, Args2),
                    #mgm_cmd{command = Command3};
                Error ->
                    log:warning(error_log, "{error_parse_json, ~p}", [Error]),
                    error(Error)
            end;
        Error ->
            log:warning(error_log, "{error_scan_json, ~p}", [Error]),
            error(Error)
    end.

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

convert_to_jsx(T) ->
    [convert(E) || E <- T].

convert(#calls{list = Calls}) ->
    Calls2 = lists:map(fun convert/1, Calls),
    [{<<"calls">>, [Calls2]}];

convert(#call{options = Options, observer = #observer{side = Number}, commands = Commands}) ->
    Options2 = convert(Options),
    Observer = {<<"observer">>, convert(Number)},
    Commands2 = lists:map(fun convert/1, Commands),
    List = [[Options2], [Observer]] ++ Commands2,

    {<<"call">>, List};

convert(#options{mode = Mode, timeout = Timeout, repeat = Repeat}) ->
    {<<"options">>, [
        convert({mode, Mode}),
        convert({timeout, trunc(Timeout / 1000)}),
        convert({repeat, Repeat})
    ]};

convert(#test{obj = Obj, ni_a = NumIndicatorA, ni_b = NumIndicatorB, type = Type, 
                ksl_a = KslA, ksl_b = KslB, priority = Priority, ss = SS}) ->
    {<<"test">>, [
        convert({obj, Obj}),
        convert({type, Type}),
        convert({'ni-a', NumIndicatorA}),
        convert({'ni-b', NumIndicatorB}),
        convert({'ksl-a', KslA}),
        convert({'ksl-b', KslB}),
        convert({priority, Priority}),
        convert({ss, SS})
    ]};

convert(#recv_msg{options = Options, number_a = NumberA, number_b = NumberB, code_msg = Msg, test = Test}) ->
    Options2 = convert(Options),
    Test2 = convert(Test),
    NumberAConvert = {<<"number-a">>, convert(NumberA)},
    NumberBConvert = {<<"number-b">>, convert(NumberB)},
    Msg2 = {<<"msg">>, convert(Msg)},
    [{<<"recv">>, [
        Options2,
        NumberAConvert,
        NumberBConvert,
        Test2,
        Msg2
    ]}];

convert([H | _] = L) when is_tuple(H) ->
    lists:map(fun convert/1, L);

convert(T) when is_tuple(T) ->
    erlang:list_to_tuple([convert(E) || E <- erlang:tuple_to_list(T)]);

convert(T) when is_list(T) ->
    erlang:list_to_binary(T);

convert(T) when is_atom(T) ->
    erlang:atom_to_binary(T, unicode);

convert(T) when is_integer(T) ->
    erlang:integer_to_binary(T);

convert(T) when is_binary(T) ->
    T.

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

thing_to_list(X) when is_integer(X) -> integer_to_list(X);
thing_to_list(X) when is_binary(X)  -> [X];
thing_to_list(X) when is_float(X)   -> float_to_list(X);
thing_to_list(X) when is_atom(X)    -> atom_to_list(X);
thing_to_list(X) when is_list(X)    -> X.   %Assumed to be a string

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

convert_calls2records(_, [], Res) ->
    lists:reverse(Res);

convert_calls2records(CallsId, [Call | T], Res) ->
    OptionsRaw = proplists:get_value(<<"options">>, Call),
    Options = get_options(OptionsRaw, #options{timeout = undefined}),
    Call2 = proplists:delete(<<"options">>, Call),

    ObserverRaw = proplists:get_value(<<"observer">>, Call2),
    Observer = #observer{side = erlang:binary_to_atom(ObserverRaw, unicode)},
    Call3 = proplists:delete(<<"observer">>, Call2),

    %log:debug(debug, "REEEEEEEEEEES ~p", [Res]),

    Id = {CallsId, generate_id(Res)},
    Commands = get_commands(Call3, []),

    ResultCall = #call{options = Options, observer = Observer, commands = Commands, id = Id},
    convert_calls2records(CallsId, T, [ResultCall | Res]).

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

get_variables([], Res) ->
    lists:reverse(Res);

get_variables(undefined, _Res) ->
    undefined;

get_variables([ {Name, Value} | T], Res) ->
    NameDecode = erlang:binary_to_atom(Name, unicode),
    ValueDecode = erlang:binary_to_list(Value),

    Variable = #variable{name = NameDecode, value = ValueDecode},
    get_variables(T, [Variable | Res]).

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

get_options([], Res) ->
    Res;

get_options(undefined, Res) ->
    Res;

get_options([ {<<"mode">>, Mode} | T], Res) ->
    get_options(T, Res#options{mode = erlang:binary_to_atom(Mode, unicode)});

get_options([ {<<"timeout">>, Timeout} | T], Res) ->
    get_options(T, Res#options{timeout = erlang:binary_to_integer(Timeout) * 1000});

get_options([ {<<"repeat">>, Repeat} | T], Res) ->
    get_options(T, Res#options{repeat = erlang:binary_to_integer(Repeat)}).

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

get_test([], Res) ->
    Res;

get_test(undefined, Res) ->
    Res;

get_test([ {<<"obj">>, Obj} | T], Res) ->
    get_test(T, Res#test{obj = erlang:binary_to_list(Obj)});

get_test([ {<<"call">>, Call} | T], Res) ->
    get_test(T, Res#test{call = erlang:binary_to_list(Call)});

get_test([ {<<"type">>, Type} | T], Res) ->
    get_test(T, Res#test{type = erlang:binary_to_list(Type)});

get_test([ {<<"ksl-a">>, KslA} | T], Res) ->
    get_test(T, Res#test{ksl_a = erlang:binary_to_list(KslA)});

get_test([ {<<"ksl-b">>, KslB} | T], Res) ->
    get_test(T, Res#test{ksl_b = erlang:binary_to_list(KslB)});

get_test([ {<<"priority">>, Priority} | T], Res) ->
    get_test(T, Res#test{priority = erlang:binary_to_list(Priority)});

get_test([ {<<"ni-a">>, NumIndicator} | T], Res) ->
    get_test(T, Res#test{ni_a = erlang:binary_to_list(NumIndicator)});

get_test([ {<<"ni-b">>, NumIndicator} | T], Res) ->
    get_test(T, Res#test{ni_b = erlang:binary_to_list(NumIndicator)});

get_test([ {<<"ss">>, SS} | T], Res) ->
    get_test(T, Res#test{ss = erlang:binary_to_list(SS)}).

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

get_commands([], Res) ->
    lists:reverse(Res);

get_commands([ {<<"recv">>, RecvRaw} | T], Res) ->
    OptionsRaw = proplists:get_value(<<"options">>, RecvRaw),
    Options = get_options(OptionsRaw, #options{timeout = undefined}),

    NumberA =
    case proplists:get_value(<<"number-a">>, RecvRaw) of
        undefined ->
            error({number_a_undefined});
        NumberARaw ->
            erlang:binary_to_list(NumberARaw)
    end,
    NumberB =
    case proplists:get_value(<<"number-b">>, RecvRaw) of
        undefined ->
            error({number_b_undefined});
        NumberBRaw ->
            erlang:binary_to_list(NumberBRaw)
    end,

    TestRaw = proplists:get_value(<<"test">>, RecvRaw),
    Test = get_test(TestRaw, #test{}),

    MsgRaw = proplists:get_value(<<"msg">>, RecvRaw),
    Msg = erlang:binary_to_list(MsgRaw),

    Id = generate_id_recv(Res),

    RecvMsg = #recv_msg{options = Options, number_a = NumberA, number_b = NumberB, code_msg = Msg, test = Test, id = Id},
    get_commands(T, [RecvMsg | Res]);

get_commands([{<<"test-mgm">>, ValueList} | T], Commands) ->
    MgmCmd = test2mgm(ValueList),
    get_commands(T, [MgmCmd#mgm_cmd{id = generate_id_mgm(Commands)} | Commands]);    

get_commands([Command | T], Res) ->
    Command2 = erlang:binary_to_list(Command),
    Command3 = do_mgm_cmd(Command2),
    get_commands(T, [Command3 | Res]).

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

generate_id([#call{id = {_CallsId, CallId}} | _]) ->
    CallId + 1;

generate_id([]) ->
    1.

generate_id_recv(List) ->
    case lists:keyfind(recv_msg, 1, List) of
        false ->
            1;
        #recv_msg{id = Id} ->
            Id + 1
    end.

generate_id_mgm(List) ->
    case lists:keyfind(mgm_cmd, 1, List) of
        false ->
            1;
        #mgm_cmd{id = undefined} ->
            1;
        #mgm_cmd{id = Id} ->
            Id + 1
    end.

generate_calls_id(List) ->
    case lists:keyfind(calls, 1, List) of
        false ->
            1;
        #calls{id = Id} ->
            Id + 1
    end.

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test2mgm(ValueList) ->
    CommandBin = proplists:get_value(<<"cmd">>, ValueList),
    AckBin = proplists:get_value(<<"ack">>, ValueList),
    ExecBin = proplists:get_value(<<"exec">>, ValueList),

    MgmCmd = do_mgm_cmd(erlang:binary_to_list(CommandBin)),
    TestMgm = #mgm_test{ack = erlang:binary_to_list(AckBin), exec = erlang:binary_to_list(ExecBin)},

    MgmCmd#mgm_cmd{test = TestMgm}.
