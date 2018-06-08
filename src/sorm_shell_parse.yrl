Nonterminals command args arg arg1 arg2 pass arg_sep.
Terminals key string sep space ',' new_pass.
Rootsymbol command.
Endsymbol '$end'.

command -> key                 : {erlang:element(3, '$1'), []}.
command -> key args            : {erlang:element(3, '$1'), '$2'}.
command -> new_pass pass       : {erlang:element(3, '$1'), '$2'}.

args    -> arg      : ['$1'].
args    -> arg args : merge('$1', '$2').

arg     -> arg1                    : '$1'.
arg     -> arg_sep                 : '$1'.
arg     -> space                   : '$end'.
arg     -> space key               : erlang:element(3, '$2').
arg     -> space new_pass          : erlang:element(3, '$2').
arg     -> space string            : value('$2').

arg_sep -> sep string arg1         : value_with_key('$2', '$3').
arg_sep -> sep string space string : value_with_key('$2', '$4').

arg1    -> space string ',' string : {value('$2'), value('$4')}.

pass    -> arg2      : ['$1'].
pass    -> arg2 pass : merge('$1', '$2').

arg2    -> space                   : '$end'. 
arg2    -> arg_sep                 : '$1'.
arg2    -> space string            : erlang:list_to_bitstring(erlang:element(3, '$2')).

Erlang code.

% тип группы / категория контроля
value({_, _, "combined"}) ->
    16#01;
value({_, _, "separated"}) ->
    16#11;
value({_, _, "statistic"}) ->
    16#02;
% тип объекта
value({_, _, "subscriber"}) ->
    16#01;
value({_, _, "network-full"}) ->
    16#02;
value({_, _, "network-short"}) ->
    16#12;
value({_, _, "trunk"}) ->
    16#03;
value({_, _, "none"}) ->
    16#FF;
% признак номера телефона
value({_, _, "private"}) ->
    16#01;
value({_, _, "local"}) ->
    16#02;
value({_, _, "zone"}) ->
    16#03;
value({_, _, "city"}) ->
    16#04;
value({_, _, "international"}) ->
    16#05;
value({_, _, "emergency"}) ->
    16#06;
%метка приоритета
value({_, _, "high"}) ->
    16#01;
value({_, _, "normal"}) ->
    16#02;

value({_, _, Value}) ->
    try erlang:list_to_integer(Value, 16) of
        ValueInt ->
            ValueInt
    catch
        _:_ ->
            [Value]
    end;
value(Token) ->
    Token.

value_with_key({_, _, "ksl-a"}, {_, _} = Value) ->
    {"k-a", Value};
value_with_key({_, _, "ksl-b"}, {_, _} = Value) ->
    {"k-b", Value};
value_with_key({_, _, "k-a"}, {_, _} = Value) ->
    {"k-a", Value};
value_with_key({_, _, "k-b"}, {_, _} = Value) ->
    {"k-b", Value};
value_with_key({_, _, Key}, {_, _} = Value) ->
    {Key, Value};

value_with_key({_, _, "id" = Key}, Value) ->
    {Key, value(Value)};
value_with_key({_, _, "type" = Key}, Value) -> % тип объекта
    {Key, value(Value)};
value_with_key({_, _, "T"}, Value) -> % тип объекта
    {"type", value(Value)};
value_with_key({_, _, [$p | _]}, {_, _, Value}) ->
    {"password", Value};
value_with_key({_, _, [$k | _]}, Value) ->
    {"ksl", value(Value)};
value_with_key({_, _, [$t | _]}, Value) -> % trunk
    {"trunk", value(Value)};
value_with_key({_, _, [$i | _] = Key}, Value) ->
    {Key, value(Value)};

value_with_key({_, _, Key}, {_, _, Value}) ->
    {Key, Value}.

merge(L, ['$end']) ->
    [L];
merge([_|_] = L, [_|_] = L2) ->
    L ++ L2;
merge(L, [_|_] = L2) ->
    [L | L2];
merge(L, L2) ->
    [L, L2].
