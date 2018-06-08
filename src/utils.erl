%%%-------------------------------------------------------------------
%%% @author Artem Kharitonov
%%% @copyright (C) 2018, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(utils).

-include_lib("chronica/include/chronica.hrl").
-include("messages.hrl").
-include("sorm_constants.hrl").
-include("ecss_sorm_test.hrl").
-include("commands.hrl").

-define(P_MSG, "[M]").
-define(P_NOTIFICATION, "[E]").
-define(P_INFO, "[I]").

-define(END_COMMAND, '$end').

-export([
    convert_number/1,
    parse_message/1,
    parse_notification/1,
    parse_number/1,
    get_number_by_side/1,
    get_numbers_from_notification/1,
    get_number_and_trunk_from_notification/1,
    convert_dec_to_hex/1,
    convert_trunk/1,
    build_prefix_send/1,
    build_prefix_recv/1,
    build_void_prefix/1,
    build_help_prefix/0,
    now_string/0,
    do_cmd/2,
    check_id_and_password/3,
    validate_mgm_test/2
]).

parse_message(Msg) ->
    parse_message(Msg, []).
parse_message(<<>>, L) ->
    lists:reverse(L);
parse_message(<<Preamble:8, SormId:8, MsgId, LenMsg:8, QuantityMsg:16, NumberMsg:16,
                ReserveA:8, ReserveB:8, Rest/binary>>, List) ->
    L = [
         Preamble, 
         SormId, 
         MsgId, 
         LenMsg, 
         QuantityMsg, 
         NumberMsg, 
         ReserveA, 
         ReserveB, 
         16#FF
         ],
    L2 = convert_dec_to_hex(L),
    Msg = erlang:list_to_tuple([msg|L2]),
    {Msg2, Rest2} = parse_message_(Msg, Rest),
    parse_message(Rest2, [Msg2|List]).

parse_message_(#msg{msg_id = ?MSG_CRASH} = Msg, Bin) ->
    <<TypeCrash:8, CodeCrash:8, Rest/binary>> = Bin,
    L = convert_dec_to_hex([TypeCrash, CodeCrash]),
    MsgCrash = erlang:list_to_tuple([crash|L]),
    {Msg#msg{msg_text = MsgCrash}, Rest};
parse_message_(#msg{msg_id = ?MSG_RESTART} = Msg, Bin) ->
    {Msg, Bin};
parse_message_(#msg{msg_id = ?MSG_INFO_OBJ} = Msg, Bin) ->
    <<Id:16, Type:8, NumIndicator:8, 
      LenNumber:8, Number:72, TrunkGroupId:2/little-unit:8, 
      Category:8, Ksl:8, Mark:8, KitStatus:8, Rest/binary>> = Bin,
    L = convert_dec_to_hex([Id, 
                            Type,
                            NumIndicator, 
                            LenNumber, 
                            Number, 
                            TrunkGroupId, 
                            Category, 
                            Ksl, 
                            Mark, 
                            KitStatus]),
    MsgInfo = erlang:list_to_tuple([info_object|L]),
    {Msg#msg{msg_text = MsgInfo}, Rest};
parse_message_(#msg{msg_id = ?MSG_INFO_KSL} = Msg, <<Bin:45/binary, Rest/binary>>) ->
    Res = parse_message__(Bin, []),
    {Msg#msg{msg_text = Res}, Rest};

parse_message_(#msg{msg_id = ?MSG_LIST_OF_SS} = Msg, Bin) ->
    <<NumIndicator:8, LenNumber:8, Number:72, NumberServices:8, Bin2/binary>> = Bin,
    L = convert_dec_to_hex([NumIndicator, LenNumber, Number, NumberServices, 16#FF]),

    {Bin4, Rest3} =
    case Bin2 of
        <<Bin3:33/binary, Rest2/binary>> ->
            {Bin3, Rest2};
        <<Bin3/binary>> ->
            {Bin3, <<>>}
    end,

    {Rest4, Res} = get_ss_from_message(Bin4, [], NumberServices),

    Res2 =
    case Rest4 of
        <<>> ->
            L ++ [[]];
        _ ->
            L2 = erlang:binary_to_list(Rest4),
            L3 = convert_dec_to_hex(L2),
            L ++ [lists:concat(L3)]
    end, 

    MsgText = erlang:list_to_tuple([description_service|Res2]),
    {Msg#msg{msg_text = MsgText#description_service{list_of_services = Res}}, Rest3};
parse_message_(#msg{msg_id = ?MSG_CRACK} = Msg, Bin) ->
    <<Code:8, DayOfMonth:8, H:8, M:8, S:8, _:40/unit:8, Rest/binary>> = Bin,
    [Code2 | L] = convert_dec_to_hex([Code, DayOfMonth, H, M, S]),
    [DayOfMonth2| Time] = L,
    {Msg#msg{msg_text = #crack_sorm{code = Code2, day_of_month = DayOfMonth2, time = erlang:list_to_tuple(Time)}}, Rest};
parse_message_(#msg{msg_id = MsgId2} = Msg, Bin) when MsgId2 =:= ?MSG_ACK; MsgId2 =:= ?MSG_EXEC->
    <<MsgId:8, Flag:8, Rest/binary>> = Bin,
    L = convert_dec_to_hex([MsgId, Flag]),
    MsgReply = erlang:list_to_tuple([msg_reply|L]),
    {Msg#msg{msg_text = MsgReply}, Rest};
parse_message_(#msg{msg_id = ?MSG_TEST} = Msg, Bin) ->
    <<Id:8, Status1:8, Status2:8, Rest/binary>> = Bin,
    F = fun(Bin2) ->
            << <<B>> || <<B:1>> <= <<Bin2>> >>
        end,
    MsgText = #reply_test_message{id = ion:format("~2.16.0B", [Id]), status_1 = F(Status1), status_2 = F(Status2)},
    {Msg#msg{msg_text = MsgText}, Rest};
parse_message_(#msg{msg_id = ?MSG_INFO_TRUNK} = Msg, Bin) ->
    <<TrunkGroupId:2/little-unit:8, Bin2:43/binary, Rest/binary>> = Bin,
    Res = << <<B>> || <<B:8>> <= Bin2, (B > 31) and (B < 127) >>,

    {Msg#msg{msg_text = #info_trunk{trunk_group_id = ion:format("~2.16.0B", [TrunkGroupId]), string = Res}}, Rest};
parse_message_(#msg{msg_id = ?MSG_VERSION} = Msg, <<Bin:45/binary, Rest/binary>>) ->
    Res = << <<B>> || <<B:8>> <= Bin, (B > 31) and (B < 127) >>,
    UnimportantRes = << <<B>> || <<B:8>> <= Bin, B =:= 255 >>,
    {Msg#msg{msg_text = #version{string = Res, unimportant = UnimportantRes}}, Rest};
parse_message_(#msg{msg_id = _} = Msg, _) ->
    {Msg#msg{msg_text = undefined}, <<>>}.

parse_message__(<<>>, Res) ->
    lists:reverse(Res);

parse_message__(<<Bin:8>>, Res) ->
    log:debug(debug, "~s", [ion:format("~2.16.0B", [Bin])]),
    parse_message__(<<>>, [ ion:format("~2.16.0B", [Bin]) | Res]);
parse_message__(Bin, Res) ->
    <<Ksl:8, Type:8, NumKslA:8, NumKslB:8, Rest/binary>> = Bin,
    L = convert_dec_to_hex([Ksl, Type, NumKslA, NumKslB]),
    MsgInfo = erlang:list_to_tuple([info_ksl|L]),
    parse_message__(Rest, [MsgInfo | Res]).

get_ss_from_message(Rest, Res, 0) ->
    {Rest, lists:reverse(Res)};
get_ss_from_message(Bin, Res, Acc) ->
    <<Byte1:8, Byte2:8, Byte3:8, Rest/binary>> = Bin,
    L = convert_dec_to_hex([Byte1, Byte2, Byte3]),
    L2 = erlang:list_to_tuple(L),
    get_ss_from_message(Rest, [L2 | Res], Acc - 1).

parse_notification(Bin) ->
    parse_notification_(Bin, []).

parse_notification_(<<>>, Res) ->
    lists:reverse(Res);

parse_notification_(<<Preamble:8, SormId:8, CodeMsg:8, LenMsg:8, CallId:5/binary, TagSelectNumber:8, ConnParams:8, CodeSS:8,
                     NumPhoneCallingA:88, NumPhoneCallingB:88, TrunkGroupId:16, NumKslA:8, NumKslB:8, Date:32, MarkPriority:8, 
                     CodeOperation:8, DescriptionService:3/binary, AdditionalCode:8, Rest/binary>>, Res) ->
    {_, [SS]} = get_ss_from_message(DescriptionService, [], 1),

    L = [
         Preamble,
         SormId,
         CodeMsg, 
         LenMsg, 
         16#FF, 
         TagSelectNumber, 
         ConnParams, 
         CodeSS, 
         NumPhoneCallingA, 
         NumPhoneCallingB, 
         TrunkGroupId, 
         NumKslA, 
         NumKslB, 
         Date, 
         MarkPriority, 
         CodeOperation,
         16#FF,
         AdditionalCode
        ],

    L2 = convert_dec_to_hex(L),
    Notification = erlang:list_to_tuple([notification|L2]), 
    N = Notification#notification{call_id = CallId, description_service = SS},

    parse_notification_(Rest, [N | Res]);

parse_notification_(<<Preamble:8, SormId:8, CodeMsg:8, LenMsg:8, Unimportant:64, IdMessage:8, Status1:8, Status2:8, Rest/binary>>, Res) ->
    L = [
         Preamble,
         SormId,
         CodeMsg,
         LenMsg,
         Unimportant,
         IdMessage,
         Status1,
         Status2
        ],

    L2 = convert_dec_to_hex(L),
    N = erlang:list_to_tuple([t_notification|L2]),

    parse_notification_(Rest, [N | Res]).

parse_number([]) ->
    [];
parse_number(<<16#FF>>) ->
    Seq = lists:seq(1, 9),
    L = lists:map(fun(_) ->
                16#FF
              end, Seq),
    erlang:list_to_bitstring(L);
parse_number(Number) ->
    Number2 = convert_number(Number),
    Number3 = [erlang:list_to_integer(X, 16) || X <- Number2],
    BinNumber = binary:list_to_bin(Number3),
    Seq = lists:seq(1, 9 - byte_size(BinNumber)),
    RestNumber = binary:list_to_bin([255 || _ <- Seq]),
    {ok, <<BinNumber/binary, RestNumber/binary>>}.

convert_number([]) ->
    [];
convert_number(L) ->
    convert_number(L, []).
convert_number([$F, $F|_], L) ->
    convert_number([], L);
convert_number([$F, H|_], L) ->
    convert_number([], [[H]|L]);
convert_number([H1, H2|T], L) ->
    convert_number(T, [[H2, H1]|L]);
convert_number([H|T], L) ->
    H2 = erlang:binary_to_list(<<$F, H>>),
    convert_number(T, [H2|L]);
convert_number([], L) ->
    lists:reverse(L).

convert_dec_to_hex([]) ->
    [];
convert_dec_to_hex(L) ->
    convert_dec_to_hex(L, []).
convert_dec_to_hex([H|T], L)->
    H2 = erlang:integer_to_list(H, 16),
    Len = string:len(H2),
    H3 = 
    if
        Len rem 2 =/= 0 ->
            [$0 | H2];
        true ->
            H2
    end,
    convert_dec_to_hex(T, [H3|L]);
convert_dec_to_hex([], L)->
    lists:reverse(L).

convert_trunk(Trunk) when is_integer(Trunk) ->
    <<Trunk:2/little-unit:8>>;
convert_trunk(Trunk) ->
    try erlang:list_to_bitstring(Trunk) of
        Trunk2 ->
            Trunk2
    catch
        error:Error ->
            error(Error)
    end.

get_number_by_side(#call{observer = #observer{side = number_a}, commands = [#recv_msg{number_a = NumberA}|_]}) ->
    NumberA;
get_number_by_side(#call{observer = #observer{side = number_b}, commands = [#recv_msg{number_b = NumberB}|_]}) ->
    NumberB;
get_number_by_side(_) ->
    error.

get_number_and_trunk_from_notification(#notification{number_a = NumberA, number_b = NumberB, trunk_group_id = TrunkId}) ->
    Fun =
    fun(Number) ->
        case string:substr(Number, 1, 2) of
            ?UNDEFINED_NUM_INDICATOR ->
                TrunkId;
            _ ->
                get_number(Number)
        end
    end,

    [Fun(Number) || Number <- [NumberA, NumberB]].

-spec get_numbers_from_notification(#notification{}) -> [string()].
get_numbers_from_notification(#notification{number_a = NumberA, number_b = NumberB}) ->
    lists:map(fun(Number) ->
                get_number(Number)
              end, [NumberA, NumberB]).

get_number([_, _ | Number]) ->
    LenNumber = lists:sublist(Number, 1, 2),
    LenNumber2 = erlang:list_to_integer(LenNumber, 16),
    Number2 = case LenNumber2 rem 2 of
        0 -> lists:sublist(Number, 3, LenNumber2);
        _ -> lists:sublist(Number, 3, LenNumber2 + 1)
    end,
    Number3 = utils:convert_number(Number2),
    lists:append(Number3).    

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

build_prefix_send(Text) ->
    ion:format(" ~s ->", [build_prefix(Text)]).

build_prefix_recv(Text) ->
    ion:format(" ~s <-", [build_prefix(Text)]).

build_prefix(#msg{msg_id = _}) ->
    ion:format("~s ~s", [now_string(), ?P_MSG]);

build_prefix({N, _Numbers}) when is_record(N, notification); is_record(N, t_notification) ->
    ion:format("~s ~s", [now_string(), ?P_NOTIFICATION]);

build_prefix(_) ->
    ion:format("~s ~s", [now_string(), ?P_INFO]).

build_void_prefix(_) -> " ".

build_help_prefix() -> 
    "Usage: ".

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

now_string() ->
   elx_time:now_to_string(erlang:timestamp()).

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

do_cmd(Command, ?END_COMMAND) ->
    do_cmd(Command, []);    
do_cmd(Command, [?END_COMMAND]) ->
    do_cmd(Command, []);
do_cmd('start-sorm', []) ->
    #start_sorm{};
do_cmd('stop-sorm', []) ->
    #stop_sorm{};
do_cmd('set-password', [NewPassword]) ->
    #set_password{new_password = NewPassword};
do_cmd('set-ksl', [Ksl, TypeGroup, TruktId, TrunkNumber]) ->
    NumKsl = <<TruktId:3, TrunkNumber:5>>,
    #set_ksl{ksl = Ksl, type_group = TypeGroup, num_ksl_a = NumKsl, num_ksl_b = NumKsl};
do_cmd('set-ksl', [Ksl, TypeGroup, TruktId, TrunkNumber, TruktId2, TrunkNumber2]) ->
    NumKslA = <<TruktId:3, TrunkNumber:5>>,
    NumKslB = <<TruktId2:3, TrunkNumber2:5>>,
    #set_ksl{ksl = Ksl, type_group = TypeGroup, num_ksl_a = NumKslA, num_ksl_b = NumKslB};
do_cmd('wiretap-add', Args) ->
    wiretap_add_args(Args);
do_cmd('wiretap-del', [Id, Type, NumIndicator, Number]) ->
    Command = 
    case Number of
        {[$n | _], Number2} ->
            #wiretap_del{number = Number2};
        {[$t | _], Trunk} ->
            #wiretap_del{trunk_group_id = utils:convert_trunk(Trunk)};
        _ ->
            error(wrongarg)
    end,
    Command#wiretap_del{id = <<Id:16>>, type = Type, number_indicator = NumIndicator};
do_cmd('wiretap-del', [Id, Type, NumIndicator, {[$n | _], Number}, {[$t | _], TrunkGroupId}]) ->
    #wiretap_del{id = <<Id:16>>, type = Type, number_indicator = NumIndicator, number = Number, trunk_group_id = utils:convert_trunk(TrunkGroupId)};
do_cmd('wiretap', [CallId, Ksl]) -> %% CallId :: integer() | binary().
    log:debug(debug, "!!! Call Id: ~p, KSL: ~p !!!", [CallId, Ksl]),
    CallId2 = 
    if
        is_integer(CallId); is_binary(CallId) ->
            CallId;
        true ->
            undefined
    end,
    #connect_to_call{call_id = CallId2, ksl = Ksl};
do_cmd('free-ksl', [CallId]) ->
    do_cmd('free-ksl', [CallId, 16#FF, 16#FF, 16#FF, 16#FF]);
do_cmd('free-ksl', [TruktId, TrunkNumber]) ->
    do_cmd('free-ksl', [16#FFFFFFFFFF, TruktId, TrunkNumber, 16#FF, 16#FF]);
do_cmd('free-ksl', [TruktId, TrunkNumber, TruktId2, TrunkNumber2]) ->
    do_cmd('free-ksl', [16#FFFFFFFFFF, TruktId, TrunkNumber, TruktId2, TrunkNumber2]);
do_cmd('free-ksl', [CallId, TruktId, TrunkNumber, TruktId2, TrunkNumber2]) ->
    NumKslA = <<TruktId:3, TrunkNumber:5>>,
    NumKslB = <<TruktId2:3, TrunkNumber2:5>>,

    CallId2 = 
    if
        is_integer(CallId); is_binary(CallId) ->
            CallId;
        true ->
            undefined
    end,
    #free_ksl{call_id = CallId2, num_ksl_a = NumKslA, num_ksl_b = NumKslB};
do_cmd('kick-ksl', [Ksl, TruktId, TrunkNumber]) ->
    NumKsl = <<TruktId:3, TrunkNumber:5>>,
    #kick_ksl{ksl = Ksl, num_ksl_a = NumKsl, num_ksl_b = NumKsl};
do_cmd('kick-ksl', [Ksl, TruktId, TrunkNumber, TruktId2, TrunkNumber2]) ->
    NumKslA = <<TruktId:3, TrunkNumber:5>>,
    NumKslB = <<TruktId2:3, TrunkNumber2:5>>,
    #kick_ksl{ksl = Ksl, num_ksl_a = NumKslA, num_ksl_b = NumKslB};
do_cmd('get-info', Args) ->
    get_info_args(Args, #get_info{});
do_cmd('info-ksl', Args) ->
    info_ksl_args(Args, #info_ksl_btw_group{});
do_cmd('list-ss', [NumIndicator, {[$n | _], Number}]) ->
    #list_ss{number_indicator = NumIndicator, number = Number};
do_cmd('stop-sending', []) ->
    #stop_send_message{};
do_cmd('test', [IdMessage]) ->
    #test_message{id_message = IdMessage};
do_cmd('wiretap-ed', [Id, 16#02 = Category, Mark]) ->
    #wiretap_ed{id = <<Id:16>>, category = Category, ksl = 16#FF, mark_priority = Mark};
do_cmd('wiretap-ed', [Id, Category, Ksl, Mark]) ->
    #wiretap_ed{id = <<Id:16>>, category = Category, ksl = Ksl, mark_priority = Mark};
do_cmd('info-trunk-group', [TrunkGroupId]) ->
    #info_trunk_group{trunk_group_id = utils:convert_trunk(TrunkGroupId)};
do_cmd('info-trunk-group', []) ->
    #info_trunk_group{};
do_cmd('get-version', []) ->
    #get_version{};

do_cmd(Command, Args) ->
    case try_do_service_cmd(Command, Args) of
        error ->
            log:debug(debug, "!!! WRONG ARG !!!", []),
            log:debug(debug, "Command: ~p", [Command]),
            log:debug(debug, "Args: ~p", [Args]),
            error(wrongarg);
        Res ->
            {service, Res}
    end.

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

check_id_and_password([{Param, _} = Tuple, {_, _} = Tuple2 | T], _Id, _Password) when Param =:= "sorm-id"; Param =:= "password" ->
    F = 
    fun({"sorm-id", Id2}, {"password", Password2}) ->
        {Id2, Password2};
       ({"password", Password2}, {"sorm-id", Id2}) ->
        {Id2, Password2}
    end,

    %log:debug(debug, "Tuple: ~p", [Tuple]),
    %log:debug(debug, "Tuple2: ~p", [Tuple2]),
    %log:debug(debug, "Id, Password: ~p", [{Id, Password}]),

    {Id3, Password3} = F(Tuple, Tuple2),
    Password4 = erlang:list_to_binary(Password3),

    [erlang:list_to_integer(Id3), Password4 | T];
check_id_and_password(Args, Id, Password) ->
    [Id, Password | Args].

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

wiretap_add_args([Id, Type, NumIndicator, Number, 16#02 = Category, Mark]) ->
    wiretap_add_args([Id, Type, NumIndicator, Number, Category, 16#FF, Mark]);

wiretap_add_args([Id, Type, NumIndicator, {[$n | _], Number}, {"trunk", TrunkGroupId}, 16#02 = Category, Mark]) ->
    wiretap_add_args([Id, Type, NumIndicator, Number, TrunkGroupId, Category, 16#FF, Mark]);

wiretap_add_args([Id, Type, NumIndicator, Number, Category, Ksl, Mark]) ->
    Command = 
    case Number of
        {[$n | _], Number2} ->
            #wiretap_add{number = Number2};
        {[$t | _], Trunk} ->
            #wiretap_add{trunk_group_id = utils:convert_trunk(Trunk)};
        _ ->
            error(wrongarg)
    end,
    Command#wiretap_add{id = <<Id:16>>, type = Type, number_indicator = NumIndicator, category = Category, ksl = Ksl, mark_priority = Mark};

wiretap_add_args([Id, Type, NumIndicator, Number, TrunkGroupId, Category, Ksl, Mark]) ->
    #wiretap_add{id = <<Id:16>>, type = Type, number_indicator = NumIndicator, number = Number, 
                 trunk_group_id = utils:convert_trunk(TrunkGroupId), category = Category, ksl = Ksl, mark_priority = Mark};

wiretap_add_args(_Args) ->
    error(wrongarg).

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

get_info_args([], Res) ->
    Res;
get_info_args([{"id", Id} | T], Res) ->
    get_info_args(T, Res#get_info{id = <<Id:16>>});
get_info_args([{"type", Type} | T], Res) ->
    get_info_args(T, Res#get_info{type = Type});
get_info_args([{"trunk", Trunk} | T], Res) ->
    get_info_args(T, Res#get_info{trunk_group_id = utils:convert_trunk(Trunk)});
get_info_args([{[$i | _], NumIndicator} | T], Res) ->
    get_info_args(T, Res#get_info{number_indicator = NumIndicator});
get_info_args([{[$n | _], Number} | T], Res) ->
    get_info_args(T, Res#get_info{number = Number});
get_info_args(_Args, _Res) ->
    error(wrongarg).

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

info_ksl_args([], Res) ->
    Res;
info_ksl_args([{"type", Type} | T], Res) ->
    info_ksl_args(T, Res#info_ksl_btw_group{type = Type});
info_ksl_args([{"ksl", Ksl} | T], Res) ->
    info_ksl_args(T, Res#info_ksl_btw_group{ksl = Ksl});
info_ksl_args([{"k-a", {TruktId, TrunkNumber}} | T], Res) ->
    info_ksl_args(T, Res#info_ksl_btw_group{num_ksl_a = <<TruktId:3, TrunkNumber:5>>});
info_ksl_args([{"k-b", {TruktId, TrunkNumber}} | T], Res) ->
    info_ksl_args(T, Res#info_ksl_btw_group{num_ksl_b = <<TruktId:3, TrunkNumber:5>>});
info_ksl_args(Args, Res) when is_tuple(Args) ->
    info_ksl_args([Args], Res);
info_ksl_args(_Args, _Res) ->
    error(wrongarg).

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

try_do_service_cmd('help', [Args]) ->
    try_do_service_cmd('help', Args);

try_do_service_cmd('help', Args) ->
    #help{command = Args};

try_do_service_cmd('record-start', _Args) ->
    #record_start{};

try_do_service_cmd('record-stop', []) ->
    FileName = now_string(),
    [Y, M, D, Time] = string:tokens(FileName, "/ "),
    FileNameRes = ion:format("~s.~s.~s ~s", [Y, M, D, Time]),
    #record_stop{filename = FileNameRes};

try_do_service_cmd('record-stop', [FileName]) ->
    #record_stop{filename = FileName};

try_do_service_cmd('repeat', _Args) ->
    #repeat{};

try_do_service_cmd('exit', _Args) ->
    #exit{};

try_do_service_cmd(_Commands, _Args) ->
    error.

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

validate_mgm_test(#mgm_cmd{test = #mgm_test{ack = AckFlag}}, 
                    #msg{msg_id = ?MSG_ACK, msg_text = #msg_reply{flag = AckFlag2}}) when AckFlag =/= AckFlag2 ->
%    Path = ion:format("[mgm@~p].ack", [Id]),
    Message = {{ack_exec, AckFlag}, {ack_exec, AckFlag2}},
%    ion:format("Test ACK:" ++ 
%                         "\n    Wait: ~s, Comming: ~s", [AckFlag2, AckFlag]),
%    io_sorm_shell:send_message(ShellPid, {test_failed, Path, Message});

    {false, Message};

validate_mgm_test(#mgm_cmd{test = #mgm_test{exec = ExecFlag}}, 
                    #msg{msg_id = ?MSG_EXEC, msg_text = #msg_reply{flag = ExecFlag2}}) when ExecFlag =/= ExecFlag2 ->
%    Path = ion:format("[mgm@~p].exec", [Id]),
    Message = {{ack_exec, ExecFlag}, {ack_exec, ExecFlag2}},
%    io_sorm_shell:send_message(ShellPid, {test_failed, Path, Message});

    {false, Message};

validate_mgm_test(_MgmCmd, _Msg) ->
    true.
