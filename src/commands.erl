%%%-------------------------------------------------------------------
%%% @author Artem Kharitonov
%%% @copyright (C) 2018, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(commands).

-include_lib("chronica/include/chronica.hrl").
-include_lib("eltex_stdlib/include/ternary_operation.hrl").
-include("commands.hrl").

-compile(export_all).
-compile(nowarn_export_all).

-define(CMD_N(SormId, CommId, LenMsg, Password), <<16#CC, SormId:8, CommId:8, LenMsg:8, Password/binary>>).

header(SormId, CommId, LenMsg, Password) ->
    <<16#CC, SormId:8, CommId:8, LenMsg:8, Password/binary>>.

-spec start_sorm(pos_integer(), binary(), #start_sorm{}) -> binary().
start_sorm(SormId, Password, #start_sorm{command_id = CommId}) ->
    header(SormId, CommId, 0, Password).

-spec stop_sorm(pos_integer(), binary(), #stop_sorm{}) -> binary().
stop_sorm(SormId, Password, #stop_sorm{command_id = CommId}) ->
    header(SormId, CommId, 0, Password).

-spec set_password(pos_integer(), binary(), #set_password{}) -> binary().
set_password(SormId, Password, #set_password{command_id = CommId, new_password = NewPassword}) ->
    Size = byte_size(NewPassword),

    Header = header(SormId, CommId, Size, Password),
    <<Header/binary, NewPassword/binary>>.

-spec set_ksl(pos_integer(), binary(), #set_ksl{}) -> binary().
set_ksl(SormId, Password, #set_ksl{command_id = CommId, ksl = NumGroupKsl, type_group = TypeGroup,
                                    num_ksl_a = NumKslA, num_ksl_b = NumKslB}) -> % создание КСЛ
    Message = <<NumGroupKsl:8, TypeGroup:8, NumKslA/binary, NumKslB/binary>>,
    Size = byte_size(Message),

    Header = header(SormId, CommId, Size, Password),
    <<Header/binary, Message/binary>>.

-spec wiretap_add(pos_integer(), binary(), #wiretap_add{}) -> binary(). % установить пользователя на контроль
wiretap_add(SormId, Password, #wiretap_add{command_id = CommId, id = Id, type = Type, number_indicator = NumIndicator, 
                                           number = Number, trunk_group_id = TrunkGroupId, category = Category,
                                           ksl = Ksl, mark_priority = MarkPriority}) ->
    Message = case utils:parse_number(Number) of
        {ok, ResultNumber} ->
            LenNumber = string:len(Number),
            <<Id/binary, Type:8, NumIndicator:8, LenNumber:8, ResultNumber/binary, 
              TrunkGroupId/binary, Category:8, Ksl:8, MarkPriority:8>>;
        ResultNumber ->
            <<Id/binary, Type:8, NumIndicator:8, 16#FF, ResultNumber/binary, 
              TrunkGroupId/binary, Category:8, Ksl:8, MarkPriority:8>>
    end,

    Size = byte_size(Message),
    Header = header(SormId, CommId, Size, Password),
    <<Header/binary, Message/binary>>.

-spec wiretap_del(pos_integer(), binary(), #wiretap_del{}) -> binary(). % убрать с контроля
wiretap_del(SormId, Password, #wiretap_del{command_id = CommId, id = Id, type = Type, number_indicator = NumIndicator,
                                           trunk_group_id = TrunkGroupId, number = Number}) ->
    Message = case utils:parse_number(Number) of
        {ok, ResultNumber} ->
            LenNumber = string:len(Number),
            <<Id/binary, Type:8, NumIndicator:8, LenNumber, ResultNumber/binary, TrunkGroupId/binary>>;
        ResultNumber ->
            <<Id/binary, Type:8, NumIndicator:8, 16#FF, ResultNumber/binary, TrunkGroupId/binary>>
    end,

    Size = byte_size(Message),
    Header = header(SormId, CommId, Size, Password),
    <<Header/binary, Message/binary>>.

-spec free_ksl(pos_integer(), binary(), #free_ksl{}) -> binary().
free_ksl(SormId, Password, #free_ksl{command_id = CommId, call_id = CallId, num_ksl_a = NumKslA, num_ksl_b = NumKslB}) ->
    CallId2 = ?_(is_binary(CallId), CallId, <<CallId:40>>),
    Message = <<CallId2/binary, NumKslA/binary, NumKslB/binary>>,
    Size = byte_size(Message),
    Header = header(SormId, CommId, Size, Password),
    <<Header/binary, Message/binary>>.

-spec kick_ksl(pos_integer(), binary(), #kick_ksl{}) -> binary().
kick_ksl(SormId, Password, #kick_ksl{command_id = CommId, ksl = NumGroupKsl, num_ksl_a = NumKslA, num_ksl_b = NumKslB}) ->
    Message = <<NumGroupKsl:8, NumKslA/binary, NumKslB/binary>>,
    Size = byte_size(Message),
    Header = header(SormId, CommId, Size, Password),
    <<Header/binary, Message/binary>>.

-spec connect_to_call(pos_integer(), binary(), #connect_to_call{}) -> binary().
connect_to_call(SormId, Password, #connect_to_call{command_id = CommId, call_id = CallId, ksl = Ksl}) ->
    CallId2 = ?_(is_binary(CallId), CallId, <<CallId:40>>),
    Message = <<CallId2/binary, Ksl:8>>,
    Size = byte_size(Message),
    Header = header(SormId, CommId, Size, Password),
    <<Header/binary, Message/binary>>.

-spec get_info(pos_integer(), binary(), #get_info{}) -> binary().
get_info(SormId, Password, #get_info{command_id = CommId, id = Id, type = Type,
                                    number_indicator = NumIndicator, number = Number, trunk_group_id = TrunkGroupId}) ->
    log:debug(debug, "!!! GET_INFO !!!", []),
    Message = case utils:parse_number(Number) of
        {ok, ResultNumber} ->
            LenNumber = string:len(Number),
            <<Id/binary, Type:8, NumIndicator:8, LenNumber, ResultNumber/binary, TrunkGroupId/binary>>;
        ResultNumber ->
            <<Id/binary, Type:8, NumIndicator:8, 16#FF, ResultNumber/binary, TrunkGroupId/binary>>
    end,
            
    Size = byte_size(Message),            
    Header = header(SormId, CommId, Size, Password),
    <<Header/binary, Message/binary>>.

-spec info_ksl_btw_group(pos_integer(), binary(), #info_ksl_btw_group{}) -> binary().
info_ksl_btw_group(SormId, Password, #info_ksl_btw_group{command_id = CommId, ksl = Ksl,
                                                        type = Type, num_ksl_a = NumKslA, num_ksl_b = NumKslB}) ->
    Message = <<Ksl:8, Type:8, NumKslA/binary, NumKslB/binary>>,
    Size = byte_size(Message),

    Header = header(SormId, CommId, Size, Password),
    <<Header/binary, Message/binary>>.

-spec list_ss(pos_integer(), binary(), #list_ss{}) -> binary().
list_ss(SormId, Password, #list_ss{command_id = CommId, number_indicator = NumIndicator, number = Number}) ->
    Message = case utils:parse_number(Number) of
        {ok, ResultNumber} ->
            LenNumber = string:len(Number),
            <<NumIndicator:8, LenNumber, ResultNumber/binary>>;
        ResultNumber ->
            <<NumIndicator:8, 16#FF, ResultNumber/binary>>
    end,

    Size = byte_size(Message),
    Header = header(SormId, CommId, Size, Password),
    <<Header/binary, Message/binary>>.

-spec stop_send_message(pos_integer(), binary(), #stop_send_message{}) -> binary().
stop_send_message(SormId, Password, #stop_send_message{command_id = CommId}) ->
    header(SormId, CommId, 16#00, Password).

-spec test_message(pos_integer(), binary(), #test_message{}) -> binary().
test_message(SormId, Password, #test_message{command_id = CommId, id_message = NumTest}) ->
    Header = header(SormId, CommId, 16#01, Password),
    <<Header/binary, NumTest:8>>.

-spec wiretap_ed(pos_integer(), binary(), #wiretap_ed{}) -> binary().
wiretap_ed(SormId, Password, #wiretap_ed{command_id = CommId, id = Id, category = Category,
                                                 ksl = Ksl, mark_priority = MarkPriority}) ->
    Message = <<Id/binary, Category:8, Ksl:8, MarkPriority:8>>,
    %log:debug(debug, "!!! COMMANDS_WIRETAP_ED !!!", []),
    %log:debug(debug, "Message ~10000p", [Message]),
    Size = byte_size(Message),

    Header = header(SormId, CommId, Size, Password),
    <<Header/binary, Message/binary>>.

-spec info_trunk_group(pos_integer(), binary(), #info_trunk_group{}) -> binary().
info_trunk_group(SormId, Password, #info_trunk_group{command_id = CommId, trunk_group_id = TrunkGroupId}) ->
    Size = byte_size(TrunkGroupId),

    Header = header(SormId, CommId, Size, Password),
    <<Header/binary, TrunkGroupId/binary>>.

-spec get_version(pos_integer(), binary(), #get_version{}) -> binary().
get_version(SormId, Password, #get_version{command_id = CommId}) ->
    header(SormId, CommId, 0, Password).