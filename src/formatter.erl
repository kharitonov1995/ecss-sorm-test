%%%-------------------------------------------------------------------
%%% @author Artem Kharitonov
%%% @copyright (C) 2018, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(formatter).

-include_lib("chronica/include/chronica.hrl").
-include_lib("eltex_stdlib/include/ternary_operation.hrl").
-include("messages.hrl").
-include("commands.hrl").
-include("sorm_constants.hrl").

-export([
    print_formatter/2,
    print_formatter_expand/2,
    err/1,
    err/2,
    warning/1,
    warning/2
]).

-define(P_ERROR, "[\e[31mERROR\e[m]").
-define(P_WARNING, "[\e[33mWARNING\e[m]").
-define(P_COMMAND(Command), ion:format("\e[1m~s\e[m", [Command])).

-define(HELP_NUMBER, "[-n <Number> | -number <Number> | -t <Trunk> | -trunk <Trunk>]").
-define(HELP_NUMBER_KSL, "<Trukt id> <Trunk number> [<Trukt id 2> <<Trunk number 2>]").
-define(HELP_ID_AND_PASSWORD, "[-sorm-id <SormId> -p | -password <Password>]").

%% Type: send, recv.
print_formatter_expand(Text, Type) ->
    try
        formatter_hex:print_formatter(Text, Type),
        print_formatter(Text, Type, fun utils:build_void_prefix/1)
    catch
        throw:build_hex_send ->
            print_formatter(Text, Type, fun utils:build_void_prefix/1);
        Error:Reason ->
            err(" ~p~n", [Reason]),
            log:warning(error_log, "~p", [{Error, {Reason, erlang:get_stacktrace()}}])
    end.

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

print_formatter({service, Text}, _Type) ->
    print_formatter(Text, undefined, fun utils:build_void_prefix/1);

print_formatter(Text, send) ->
    print_formatter(Text, send, fun utils:build_prefix_send/1);

print_formatter(Text, recv) ->
    print_formatter(Text, recv, fun utils:build_prefix_recv/1).

print_formatter({Id, _Password, Record}, Type, PrefixFun) when is_integer(Id) ->
    print_formatter(Record, Type, PrefixFun);

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

print_formatter(#help{command = Command}, _, _) ->
    case build_string_help(Command) of
        {HelpString, HelpArgs} ->
            Prefix = utils:build_help_prefix(),
            io:format("~s ~n ~s~s~n~n", [HelpString, Prefix, HelpArgs]);
        _ ->
            ok
    end;

print_formatter(Text, undefined, PrefixFun) ->
    Result = build_string_service(Text),
    Prefix = PrefixFun(Text),
    io:format("~s ~s~n~n", [Prefix, Result]);

print_formatter(Text, send, PrefixFun) ->
    Result = build_string_send(Text),
    Prefix = PrefixFun(Text),

    io:format("~s ~s~n~n", [Prefix, Result]);
print_formatter(Text, recv, PrefixFun) ->
    Prefix = PrefixFun(Text),

    case build_string_recv(Text) of
        {list, List} ->
            lists:map(
                fun(Result) -> 
                    io:format("~s ~s~n~n", [Prefix, Result])
                end, List);
        Result ->
            io:format("~s ~s~n~n", [Prefix, Result])
    end.

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

build_string_recv(#msg{msg_id = ?MSG_CRASH, msg_text = #crash{type = Type, code = Code}}) ->
    ion:format("Command end with error. Type: ~s; Code: ~s", [Type, Code]);

build_string_recv(#msg{msg_id = ?MSG_RESTART}) ->
    ion:format("Restart SORM", []);

build_string_recv(#msg{msg_id = ?MSG_INFO_OBJ, msg_text = #info_object{id = Id, type = Type, number_indicator = NumIndicator, 
                                                                       len_number = LenNumber, number = Number, trunk_group_id = TrunkGroupId, 
                                                                       category = Category, ksl = Ksl, mark_priority = MarkPriority, 
                                                                       kit_status = KitStatus}}) ->
    Number2 = get_number(Number),
    ion:format("Info about abonent:" ++ 
               "\n  Id: ~s; Type: ~s; Number indicator: ~s;" ++
               "\n  Len number: ~s; Number: ~s; Trunk group id: ~s;" ++
               "\n  Category: ~s; Ksl: ~s; Mark priority: ~s; Kit status: ~s",
                [Id, Type, NumIndicator, LenNumber, Number2, TrunkGroupId, Category, Ksl, MarkPriority, KitStatus]);

build_string_recv(#msg{msg_id = ?MSG_INFO_KSL, msg_text = InfoKsl}) ->
    F = 
    fun(L) when is_list(L) ->
            error;
       (#info_ksl{ksl = "FF", type = "FF", num_ksl_a = "FF", num_ksl_b = "FF"}) ->
            error;
       (#info_ksl{ksl = Ksl, type = Type, num_ksl_a = NumKslA, num_ksl_b = NumKslB}) ->
            ion:format("Info about ksl: Ksl: ~s; Type: ~s; KSL-A: ~s; KSL-B: ~s", [Ksl, Type, NumKslA, NumKslB])
    end,

    Result = lists:map(F, InfoKsl),

    F2 = 
    fun(error) -> 
            false;
       (_) ->
            true
    end,

    {list, lists:filter(F2, Result)};

build_string_recv(#msg{msg_id = ?MSG_LIST_OF_SS, msg_text = #description_service{number_indicator = NumIndicator, len_number = LenNumber, 
                                                                                 number = Number, number_services = NumberServices, 
                                                                                 list_of_services = ListServices}}) ->
    Number2 = get_number(Number),
    Str = ion:format("List of supplementary service:" ++
                     "\n  Number indicator: ~s; Len number: ~s;" ++
                     "\n  Number: ~s; Number services: ~s;" ++
                     "\n  List Services:\n",
                      [NumIndicator, LenNumber, Number2, NumberServices]),

    Str2 = 
    lists:foldl(
            fun({Code, Code2, Code3}, Result) ->
                TmpStr = ion:format("    - ~s ~s ~s;\n", 
                                    [decode_ss(Code), decode_ss(Code2), decode_ss(Code3)]),
                Result ++ TmpStr
            end, "", ListServices),

    Str ++ Str2;

build_string_recv(#msg{msg_id = ?MSG_CRACK, msg_text = #crack_sorm{code = Code, day_of_month = DayOfMonth, time = {H, M, S}}}) ->
    [DayOfMonth2, H2, M2, S2] = convert_date([DayOfMonth, H, M, S]),
    ion:format("Crack SORM: Code: ~s; Day of month: ~s; Time: ~s:~s:~s.", [Code, DayOfMonth2, H2, M2, S2]);

build_string_recv(#msg{msg_id = ?MSG_ACK, msg_text = #msg_reply{msg_id = MsgId, flag = ?FLAG_OK}}) ->
    ion:format("Command ~s ACK FIN. Flag: ~s", [MsgId, ?FLAG_OK]);

build_string_recv(#msg{msg_id = ?MSG_ACK, msg_text = #msg_reply{msg_id = MsgId, flag = Flag}}) ->
    ion:format("Command ~s ACK FIN. \e[31mFlag: ~s\e[m", [MsgId, Flag]);

build_string_recv(#msg{msg_id = ?MSG_EXEC, msg_text = #msg_reply{msg_id = MsgId, flag = ?FLAG_OK}}) ->
    ion:format("Command ~s executed. Flag: ~s", [MsgId, ?FLAG_OK]);

build_string_recv(#msg{msg_id = ?MSG_EXEC, msg_text = #msg_reply{msg_id = MsgId, flag = Flag}}) ->
    ion:format("Command ~s executed. \e[31mFlag: ~s\e[m", [MsgId, Flag]);

build_string_recv(#msg{msg_id = ?MSG_TEST, msg_text = #reply_test_message{id = Id, status_1 = Status1, status_2 = Status2}}) ->
    log:debug(debug, "!!! RECEIVE TEST MSG !!!", []),
    Status1L = binary:bin_to_list(Status1),
    Status2L = binary:bin_to_list(Status2),

    Fun = 
    fun(X, Res) ->
        Res ++ ion:format("~p", [X])
    end,

    Result = lists:foldl(Fun, "", Status1L),
    Result2 = lists:foldl(Fun, "", Status2L),

    ion:format("Test message: Id: ~s;" ++
               "~n    State N1(bin): ~s" ++
               "~n    State N2(bin): ~s",
                [Id, Result, Result2]);

build_string_recv(#msg{msg_id = ?MSG_INFO_TRUNK, msg_text = #info_trunk{trunk_group_id = TrunkGroupId, string = String}}) ->
    ion:format("Info trunk: Id: ~s; Name: ~s", [TrunkGroupId, String]);

build_string_recv(#msg{msg_id = ?MSG_VERSION, msg_text = #version{string = String}}) ->
    ion:format("Version: ~s", [String]);

build_string_recv({#notification{code_msg = Id, call_id = CallId, tag_selection_number = TagNumber, connection_params = Params, code_ss = SS,
                                number_a = NumberA, number_b = NumberB, trunk_group_id = TrunkId, num_ksl_a = KslA, num_ksl_b = KslB, 
                                date = Date, mark_priority = Priority, code_operation = CodeOperation} = N, Numbers}) ->
    [_DayOfMonth, H, M, S] = convert_date(Date, []),
    
    NumIndicatorA = string:substr(NumberA, 1, 2),
    NumIndicatorB = string:substr(NumberB, 1, 2),
    LenNumberA = string:substr(NumberA, 3, 2),
    LenNumberB = string:substr(NumberB, 3, 2),

    %log:debug(debug, "Numbers: ~p", [Numbers]),

    {NumberA3, NumberB3} =
    case Id of
        ?N_RECEIVE ->
            numbers_on_control(N, Numbers);
        _ ->
            [NumberA2, NumberB2] = utils:get_numbers_from_notification(N),
            {ion:format("Number A: ~s", [NumberA2]), ion:format("Number B: ~s", [NumberB2])}
    end,
    
    <<Call:2/little-unit:8, _, Obj:2/unit:8>> = CallId,
    CallIdInt = binary:decode_unsigned(CallId),
    [CallStr, ObjStr, CallIdStr] = utils:convert_dec_to_hex([Call, Obj, CallIdInt]),
    
    Str = build_string_recv_(N),

    NumberA4 = ?_(NumIndicatorA =:= ?UNDEFINED_NUM_INDICATOR, ion:format("~s; Trunk Id: ~s", [NumberA3, TrunkId]), NumberA3),
    NumberB4 = ?_(NumIndicatorB =:= ?UNDEFINED_NUM_INDICATOR, ion:format("~s; Trunk Id: ~s", [NumberB3, TrunkId]), NumberB3),

    ion:format("Time: ~s:~s:~s;" ++
               "\n  MSG: ~s; Call: ~s; Obj: ~s; Call id: ~s;" ++
               "\n  Indicator selection: ~s; Conn params: ~s; SS: ~s;" ++ 
               "\n  ~s; Number indicator: ~s; Len: ~s;" ++
               "\n  ~s; Number indicator: ~s; Len: ~s;" ++
               "\n  KSL-A: ~s; KSL-B: ~s; Priority: ~s; Code Operation: ~s;" ++ 
               "\n  ~s",
                [H, M, S, Id, CallStr, ObjStr, CallIdStr, TagNumber, Params, SS, NumberA4, NumIndicatorA, 
                 LenNumberA, NumberB4, NumIndicatorB, LenNumberB, KslA, KslB, Priority, CodeOperation, Str]);

build_string_recv(#t_notification{code_msg = Id, test_id = TestId, status_1 = Status1, status_2 = Status2}) ->
    Status1Int = erlang:list_to_integer(Status1),
    Status2Int = erlang:list_to_integer(Status2),

    Str = build_string_recv(#msg{msg_id = ?MSG_TEST, 
                                 msg_text = #reply_test_message{id = TestId, status_1 = <<Status1Int:64>>, status_2 = <<Status2Int:64>>}}),

    ion:format("MSG: ~s;" ++ 
               "\n  ~s", [Id, Str]);

build_string_recv({call_stop, Reason, Path}) ->
    ion:format("Call ~s stopped: ~p", [Path, Reason]);

build_string_recv({call_timeout, PathToCall}) ->
    ion:format("Call timeout ~s", [PathToCall]);

build_string_recv({optional_cmd_timeout, {PathToCommand, CodeMsg}}) ->
    Path = ion:format("[~s].msg: ~s", [PathToCommand, CodeMsg]),
    ion:format("Optional command timeout: ~s, continue call", [Path]);

build_string_recv({timeout_exec_cmd, {PathToCommand, CodeMsg}}) ->
    Path = ion:format("[~s].msg: ~s", [PathToCommand, CodeMsg]),
    ion:format("Require command timeout: ~s, ending call", [Path]);

build_string_recv(end_script) ->
    ion:format("End script", []);

build_string_recv(Mesg_) ->
    log:debug(debug, "!!! UNDEFINED RECV MESSAGE !!!: ~p", [Mesg_]). 

build_string_recv_(#notification{code_msg = ?N_RECEIVE}) ->
    ?STR_RECEIVE_NUMBER;
build_string_recv_(#notification{code_msg = ?N_ANSWER}) ->
    ?STR_ANSWER;
build_string_recv_(#notification{code_msg = ?N_DISCONNECT}) ->
    ?STR_DISCONNECT;
build_string_recv_(#notification{code_msg = ?N_USE_SS, description_service = {Code, Code2, Code3}, additional_code = AdditionalCode}) ->
    Str = ion:format("~s ~s ~s;\n", [decode_ss(Code), decode_ss(Code2), decode_ss(Code3)]),
    ?STR_USE_SS(Str, AdditionalCode);
build_string_recv_(#notification{code_msg = ?N_CONNECT_KSL}) ->
    ?STR_CONNECT_KSL;
build_string_recv_(#notification{code_msg = ?N_FREE_KSL}) ->
    ?STR_FREE_KSL.

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

build_string_send(#start_sorm{command_id = Id}) ->
    ion:format("Start sorm (~2.16.0B).", [Id]);

build_string_send(#stop_sorm{command_id = Id}) ->
    ion:format("Stop sorm (~2.16.0B).", [Id]);

build_string_send(#set_password{command_id = Id}) ->
    ion:format("Set new password (~2.16.0B).", [Id]);

build_string_send(#set_ksl{command_id = Id}) ->
    ion:format("Set KSL (~2.16.0B).", [Id]);

build_string_send(#wiretap_add{command_id = Id}) ->
    ion:format("Wiretap add (~2.16.0B).", [Id]);

build_string_send(#wiretap_del{command_id = Id}) ->
    ion:format("Wiretap del (~2.16.0B).", [Id]);

build_string_send(#free_ksl{command_id = Id}) ->
    ion:format("Free KSL (~2.16.0B).", [Id]);

build_string_send(#kick_ksl{command_id = Id}) ->
    ion:format("Kick KSL (~2.16.0B).", [Id]);

build_string_send(#connect_to_call{command_id = Id}) ->
    ion:format("Wiretap (~2.16.0B).", [Id]);

build_string_send(#get_info{command_id = Id}) ->
    ion:format("Get info (~2.16.0B).", [Id]);

build_string_send(#info_ksl_btw_group{command_id = Id}) ->
    ion:format("Info KSL (~2.16.0B).", [Id]);

build_string_send(#list_ss{command_id = Id}) ->
    ion:format("List supplementary service (~2.16.0B).", [Id]);

build_string_send(#stop_send_message{command_id = Id}) ->
    ion:format("Stop sending message (~2.16.0B).", [Id]);

build_string_send(#test_message{command_id = Id}) ->
    ion:format("Test message (~2.16.0B).", [Id]);

build_string_send(#wiretap_ed{command_id = Id}) ->
    ion:format("Wiretap edit (~2.16.0B).", [Id]);

build_string_send(#info_trunk_group{command_id = Id}) ->
    ion:format("Info trunk group (~2.16.0B).", [Id]);

build_string_send(#get_version{command_id = Id}) ->
    ion:format("Get version (~2.16.0B).", [Id]);

build_string_send(_Message) ->
    log:debug(debug, "!!! UNDEFINED SEND MESSAGE !!!", []),
    log:debug(debug, "_Message: ~p", [_Message]),
    log:debug(debug, "Stack: ~p", [erlang:get_stacktrace()]),
    ok.

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

build_string_help('start-sorm') ->
    HelpString = "Launch SORM",
    HelpArgs = ion:format("~s ~s", [?P_COMMAND("start-sorm"), ?HELP_ID_AND_PASSWORD]),
    {HelpString, HelpArgs};

build_string_help('stop-sorm') ->
    HelpString = "Stopping SORM",
    HelpArgs = ion:format("~s ~s", [?P_COMMAND("stop-sorm"), ?HELP_ID_AND_PASSWORD]),
    {HelpString, HelpArgs};

build_string_help('set-password') ->
    HelpString = "Set new password",
    HelpArgs = ion:format("~s ~s <New password>", [?P_COMMAND("set-password"), ?HELP_ID_AND_PASSWORD]),
    {HelpString, HelpArgs};

build_string_help('set-ksl') ->
    HelpString = "Set KSL",
    HelpArgs = ion:format("~s ~s <KSL> <Type group> ~s", [?P_COMMAND("set-ksl"), ?HELP_ID_AND_PASSWORD, ?HELP_NUMBER_KSL]),
    {HelpString, HelpArgs};

build_string_help('wiretap-add') ->
    HelpString = "Add wiretap abonent",
    HelpArgs = ion:format("~s ~s <Id> <Type obj> <Indicator number> ~s <Category> [<KSL>] <Mark priority>", 
                          [?P_COMMAND("wiretap-add"), ?HELP_ID_AND_PASSWORD, ?HELP_NUMBER]),
    {HelpString, HelpArgs};

build_string_help('wiretap-del') ->
    HelpString = "Del wiretap abonent",
    HelpArgs = ion:format("~s ~s <Id> <Type obj> <Indicator number> ~s", 
                          [?P_COMMAND("wiretap-del"), ?HELP_ID_AND_PASSWORD, ?HELP_NUMBER]),
    {HelpString, HelpArgs};

build_string_help('wiretap') ->
    HelpString = "Wiretap abonent",
    HelpArgs = ion:format("~s ~s <Call id> <KSL>", [?P_COMMAND("wiretap"), ?HELP_ID_AND_PASSWORD]),
    {HelpString, HelpArgs};

build_string_help('free-ksl') ->
    HelpString = "Free KSL",
    HelpArgs = ion:format("~s ~s <Call id> | ~s", [?P_COMMAND("free-ksl"), ?HELP_ID_AND_PASSWORD, ?HELP_NUMBER_KSL]),
    {HelpString, HelpArgs};

build_string_help('kick-ksl') ->
    HelpString = "Kick KSL from group",
    HelpArgs = ion:format("~s ~s <KSL> ~s", [?P_COMMAND("kick-ksl"), ?HELP_ID_AND_PASSWORD, ?HELP_NUMBER_KSL]),
    {HelpString, HelpArgs};

build_string_help('get-info') ->
    HelpString = "Get info about objects",
    HelpArgs = ion:format("~s <Fields>" ++
                          "\n Fields:"  
                          "\n  ~s" ++
                          "\n  [-id <Id>]" ++
                          "\n  [-type <Type> | -T <Type>]" ++
                          "\n  ~s" ++
                          "\n  [-i <Number indicator> | -indicator <Number indicator>]",
                          [?P_COMMAND("get-info"), ?HELP_ID_AND_PASSWORD, ?HELP_NUMBER]),
    {HelpString, HelpArgs};

build_string_help('info-ksl') ->
    HelpString = "Get info between KSL and groups",
    HelpArgs = ion:format("~s <Fields>" ++
                          "\n Fields:"
                          "\n  ~s" ++
                          "\n  [-ksl-a <Trukt id>, <Trunk number> | -k-a <Trukt id>, <Trunk number>]" ++
                          "\n  [-ksl-b <Trukt id>, <Trunk number> | -k-b <Trukt id>, <Trunk number>]" ++
                          "\n  [-ksl <KSL> | -k <KSL>]" ++
                          "\n  [-type <Type> | -T <Type>]",
                          [?P_COMMAND("info-ksl"), ?HELP_ID_AND_PASSWORD]),
    {HelpString, HelpArgs};

build_string_help('list-ss') ->
    HelpString = "Get list of supplementary services",
    HelpArgs = ion:format("~s ~s <Number indicator> -n | -number <Number>", [?P_COMMAND("list-ss"), ?HELP_ID_AND_PASSWORD]),
    {HelpString, HelpArgs};

build_string_help('stop-sending') ->
    HelpString = "Stopping sending messages on the requests about the content of tables",
    HelpArgs = ion:format("~s ~s", [?P_COMMAND("stop-sending"), ?HELP_ID_AND_PASSWORD]),
    {HelpString, HelpArgs};

build_string_help('test') ->
    HelpString = "Testing the pipes transporting data",
    HelpArgs = ion:format("~s ~s <Id test message>", [?P_COMMAND("test"), ?HELP_ID_AND_PASSWORD]),
    {HelpString, HelpArgs};

build_string_help('wiretap-ed') ->
    HelpString = "Editing wiretap abonent",
    HelpArgs = ion:format("~s ~s <Id> <Category> [<KSL>] <Mark priority>", [?P_COMMAND("wiretap-ed"), ?HELP_ID_AND_PASSWORD]),
    {HelpString, HelpArgs};

build_string_help('info-trunk-group') ->
    HelpString = "Get information about conform name trunk group and his id",
    HelpArgs = ion:format("~s ~s [<Trunk group id>]", [?P_COMMAND("info-trunk-group"), ?HELP_ID_AND_PASSWORD]),
    {HelpString, HelpArgs};

build_string_help('get-version') ->
    HelpString = "Get software version of station",
    HelpArgs = ion:format("~s ~s", [?P_COMMAND("get-version"), ?HELP_ID_AND_PASSWORD]),
    {HelpString, HelpArgs};

build_string_help('help') ->
    HelpString = "Show this information",
    HelpArgs = ion:format("~s <Command>", [?P_COMMAND("help")]),
    {HelpString, HelpArgs};

build_string_help('record-start') ->
    HelpString = "Start writing to file",
    HelpArgs = ?P_COMMAND("record-start"),
    {HelpString, HelpArgs};

build_string_help('record-stop') ->
    HelpString = "Stop writing and save",
    HelpArgs = ion:format("~s [<FileName>]", [?P_COMMAND("record-stop")]),
    {HelpString, HelpArgs};

build_string_help('exit') ->
    HelpString = "Exiting the application",
    HelpArgs = ?P_COMMAND("exit"),
    {HelpString, HelpArgs};

build_string_help('repeat') ->
    HelpString = "Repeating uploaded file",
    HelpArgs = ?P_COMMAND("repeat"),
    {HelpString, HelpArgs};

build_string_help([]) ->
    ListCommands = expand_prop:get_role_list(),

    F = 
    fun(Command) ->
        print_formatter(#help{command = erlang:list_to_atom(Command)}, undefined, undefined)
    end,

    lists:map(F, ListCommands);

build_string_help(_Command) ->
    log:debug(debug, "!!! UNDEFINED HELP COMMAND!!! ~p", [_Command]).

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

build_string_service(#record_start{}) ->
    ion:format("Starting recording", []);

build_string_service(#record_stop{filename = FileName}) ->
    ion:format("Saving to file '~s'", [FileName]);

build_string_service(#exit{}) ->
    ion:format("Bye", []);

build_string_service(_Command) ->
    log:debug(debug, "!!! UNDEFINED SERVICE COMMAND!!! ~p", [_Command]).

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

err(Format) ->
    io:format("~s ~s ~s~n", [utils:now_string(), ?P_ERROR, Format]).
err(Format, Args) ->
    io:format("~s ~s ~s~n", [utils:now_string(), ?P_ERROR, ion:format(Format, Args)]).

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

warning(Format) ->
    io:format("~s ~s ~s~n", [utils:now_string(), ?P_WARNING, Format]).
warning(Format, Args) ->
    io:format("~s ~s ~s~n", [utils:now_string(), ?P_WARNING, ion:format(Format, Args)]).

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

get_number(Number) ->
    List = utils:convert_number(Number),
    lists:concat(List).

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

convert_date(List) ->
    lists:map(
        fun([H, H2]) ->
            [H2, H]
        end, List).

convert_date([], Res) ->
    lists:reverse(Res);
convert_date([H, H2 | T], Res) ->
    convert_date(T, [[H2, H] | Res]).

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
numbers_on_control(#notification{call_id = <<_:2/binary, 16#03:1/unit:8, Obj:2/unit:8>>, number_a = NumberA, number_b = NumberB} = N, 
                    List) ->

    NumIndicatorA = string:substr(NumberA, 1, 2),
    NumIndicatorB = string:substr(NumberB, 1, 2),

    [NumberA2, NumberB2] = utils:get_numbers_from_notification(N),

    Fun =
    fun
        ({_, Id}) when NumIndicatorA =:= ?UNDEFINED_NUM_INDICATOR, Obj =:= Id ->
            {num_a, NumberA2, NumberB2};
        ({_, Id}) when NumIndicatorB =:= ?UNDEFINED_NUM_INDICATOR, Obj =:= Id ->
            {num_b, NumberA2, NumberB2};
        ([]) ->
            {NumberA2, NumberB2};
        (_) ->
            false
    end,
        
    numbers_on_control_(Fun, List);

numbers_on_control(#notification{call_id = <<_:3/binary, Obj:2/unit:8>>} = N, List) ->
    [NumberA, NumberB] = utils:get_numbers_from_notification(N),

    Fun =
    fun
        ({Number, Id}) when NumberA =:= Number, Obj =:= Id ->
            {num_a, NumberA, NumberB};
        ({Number, Id}) when NumberB =:= Number, Obj =:= Id ->
            {num_b, NumberA, NumberB};
        ([]) ->
            {NumberA, NumberB};
        (_) ->
            false
    end,

    numbers_on_control_(Fun, List).

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
numbers_on_control_(Fun, []) ->
    err("Numbers on control not found\n"),
    {NumberA, NumberB} = Fun([]),
    {ion:format("Number A: ~s", [NumberA]), ion:format("Number B: ~s", [NumberB])};

numbers_on_control_(Fun, [{_, _} = H | T]) ->
    case Fun(H) of
        {num_a, NumberA, NumberB} ->
            {ion:format("Number A^: ~s", [NumberA]), ion:format("Number B:  ~s", [NumberB])};
        {num_b, NumberA, NumberB} ->
            {ion:format("Number A:  ~s", [NumberA]), ion:format("Number B^: ~s", [NumberB])};
        _ ->
            numbers_on_control_(Fun, T)
    end.

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

decode_ss(?SS_UNDEFINED) ->
    ?SS_UNDEFINED;

decode_ss(?SS_FORWARD_UNCONDITIONAL) ->
    ion:format("~s (~s)", [?SS_FORWARD_UNCONDITIONAL, ?STR_FORWARD_UNCONDITIONAL]);

decode_ss(?SS_FORWARD_SUBS_BUSY) ->
    ion:format("~s (~s)", [?SS_FORWARD_SUBS_BUSY, ?STR_FORWARD_SUBS_BUSY]);

decode_ss(?SS_FORWARD_NO_REPLY) ->
    ion:format("~s (~s)", [?SS_FORWARD_NO_REPLY, ?STR_FORWARD_NO_REPLY]);

decode_ss(?SS_FORWARD_SUBS_NOT_REACH) ->
    ion:format("~s (~s)", [?SS_FORWARD_SUBS_NOT_REACH, ?STR_FORWARD_SUBS_NOT_REACH]);

decode_ss(?SS_ALL_FORWARD) ->
    ion:format("~s (~s)", [?SS_ALL_FORWARD, ?STR_ALL_FORWARD]);

decode_ss(?SS_ALL_CONDITIONAL_FORWARD) ->
    ion:format("~s (~s)", [?SS_ALL_CONDITIONAL_FORWARD, ?STR_ALL_CONDITIONAL_FORWARD]);

decode_ss(?SS_CALL_WAITING) ->
    ion:format("~s (~s)", [?SS_CALL_WAITING, ?STR_CALL_WAITING]);

decode_ss(?SS_CALL_HOLD) ->
    ion:format("~s (~s)", [?SS_CALL_HOLD, ?STR_CALL_HOLD]);

decode_ss(?SS_COMPLETION_CALL) ->
    ion:format("~s (~s)", [?SS_COMPLETION_CALL, ?STR_COMPLETION_CALL]);

decode_ss(?SS_HOLD_END) ->
    ion:format("~s (~s)", [?SS_HOLD_END, ?STR_HOLD_END]);

decode_ss(?SS_THREE_PARTY) ->
    ion:format("~s (~s)", [?SS_THREE_PARTY, ?STR_THREE_PARTY]);

decode_ss(?SS_MULTIPARTY) ->
    ion:format("~s (~s)", [?SS_MULTIPARTY, ?STR_MULTIPARTY]);

decode_ss(?SS_ALL_MULTIPARTY) ->
    ion:format("~s (~s)", [?SS_ALL_MULTIPARTY, ?STR_ALL_MULTIPARTY]);

decode_ss(?SS_CALLS_TRANSFER) ->
    ion:format("~s (~s)", [?SS_CALLS_TRANSFER, ?STR_CALLS_TRANSFER]);

decode_ss(?SS_CALL_PICKUP) ->
    ion:format("~s (~s)", [?SS_CALL_PICKUP, ?STR_CALL_PICKUP]);

decode_ss(?SS_CONSULTATION_CALL) ->
    ion:format("~s (~s)", [?SS_CONSULTATION_CALL, ?STR_CONSULTATION_CALL]).

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++