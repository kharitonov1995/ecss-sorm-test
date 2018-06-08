%%%-------------------------------------------------------------------
%%% @author Artem Kharitonov
%%% @copyright (C) 2018, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(formatter_hex).

-include_lib("chronica/include/chronica.hrl").
-include("messages.hrl").
-include("sorm_constants.hrl").

-export([print_formatter/2]).

build_hex_string(Message) ->
    Result =
    case msg2list(Message) of
        error ->
            build_hex_send(Message);
        List ->
            List
    end,
    ListOfStr = formatting_hex(Result),
    {_, Result2} = 
    lists:foldl(
        fun(Str, {Acc, Res}) -> 
            Res2 = string:concat(Res, Str),
            Last = lists:last(ListOfStr),
            if 
                Str =:= Last ->
                    {0, string:concat(Res2, "\n")};
                Acc div 8 =:= 1 ->
                    {16, string:concat(Res2, "  ")};
                Acc div 16 =:= 1 ->
                    {8, string:concat(Res2, "\n  ")};
                true ->
                    {Acc, Res}
            end
        end, {8, ""}, ListOfStr),
    Result2.

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

formatting_hex(List) ->
    List2 = lists:reverse(split(List, [])),
    formatting_hex_(List2, [], erlang:length(List2)).

formatting_hex_(List, Res, Len) when Len >= 8 ->
    List2 = lists:sublist(List, 8),
    Res2 = [string:join(List2, " ") | Res],
    List3 = List -- List2,
    formatting_hex_(List3, Res2, erlang:length(List3));

formatting_hex_(_List, Res, 0) ->
    lists:reverse(Res);

formatting_hex_(List, Res, Len) ->
    Temp = lists:seq(1, 8 - Len),
    List2 = ["00" || _ <- Temp],
    Str = string:join(string:concat(List, List2), " "),
    lists:reverse([Str | Res]).

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

print_formatter(Text, send) ->
    Prefix = utils:build_prefix_send(Text),
    Result = build_hex_string(Text),
    io:format("~s~n  ~s~n", [Prefix, Result]);

print_formatter(Text, recv) ->
    Prefix = utils:build_prefix_recv(Text),
    Result = build_hex_string(Text),
    io:format("~s~n  ~s~n", [Prefix, Result]).

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

msg2list(#msg{msg_id = ?MSG_INFO_KSL, msg_text = List} = Msg) ->
    F = 
    fun(L) when is_list(L) ->
        [L];
       (MsgInfo) ->
        [_|List2] = erlang:tuple_to_list(MsgInfo),
        List2
    end,

    List2 = lists:map(F, List),
    Text = lists:concat(List2),

    [_|Header] = erlang:tuple_to_list(Msg),
    Header2 = lists:delete(List, Header),
    
    Header2 ++ Text;

msg2list(#msg{msg_id = ?MSG_TEST, msg_text = #reply_test_message{id = Id, status_1 = <<Status1Int:64>>, 
                                                                 status_2 = <<Status2Int:64>>}} = Msg) ->
    Status1Str = ion:format("~2.16.0B", [Status1Int]),
    Status2Str = ion:format("~2.16.0B", [Status2Int]),

    Msg2 = Msg#msg{msg_text = #reply_test_message{id = Id, status_1 = Status1Str, status_2 = Status2Str}},
    msg2list(Msg2);

msg2list(#msg{msg_text = Text} = Msg) ->
    [_|List] = erlang:tuple_to_list(Msg),
    List2 = lists:delete(Text, List),
    [_|List3] = erlang:tuple_to_list(Text),
    List2 ++ List3;

msg2list({#notification{code_msg = _} = N, _}) ->
    [_|List] = erlang:tuple_to_list(N),
    List;

msg2list(#t_notification{code_msg = _} = N) ->
    [_|List] = erlang:tuple_to_list(N),
    List;

msg2list(List) when is_list(List) ->
    string:tokens(List, " ");

msg2list(_) ->
    error.

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

split([], Res) ->
    Res;

split([[] | T], Res) ->
    split(T, Res);

split([[{_, _, _} | _] = L | T], Res) ->
    Fun =
    fun({Code, Code2, Code3}, Res2) ->
        [Code, Code2, Code3] ++ Res2
    end,

    Res2 = lists:foldl(Fun, Res, L),
    split(T, Res2 ++ Res);

split([{H, M, S} | T], Res) ->
    Res2 = split([H, M, S], []),
    split(T, Res2 ++ Res);

split([H | T], Res) ->
    {L3, Result} =
    try lists:split(2, H) of
        {L, []} ->
            {T, [L | Res]};
        {L, L2} ->
            {[L2 | T], [L | Res]}
    catch
        error:_Error ->
            Res2 = utils:convert_dec_to_hex(erlang:binary_to_list(H)),
            L = lists:reverse(Res2),
            {T, L ++ Res}
    end,
    split(L3, Result).

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

build_hex_send({Id, Password, Record}) ->
    Fun = erlang:element(1, Record),
    Result = erlang:apply(commands, Fun, [Id, Password, Record]),
    utils:convert_dec_to_hex(binary:bin_to_list(Result));

build_hex_send(_) ->
    throw(build_hex_send).

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
