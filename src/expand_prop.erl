%%%-------------------------------------------------------------------
%%% @author Artem Kharitonov
%%% @copyright (C) 2018, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(expand_prop).

-include_lib("pt_scripts/include/pt_str_parser.hrl").
-include_lib("chronica/include/chronica.hrl").

-define(ID, "id").
-define(KSL, "KSL").
-define(CALL_ID, "call id").
-define(ID_TEST, "id test message").
-define(FILENAME, "file name").
-define(TRUKT_ID(N), ion:format("trukt id (~B)", [N])).
-define(NEW_PASSWORD, "new password").
-define(MARK_PRIORITY, "mark priority").
-define(TRUNK_GROUP_ID, "trunk group id").
-define(TRUNK_NUMBER(N), ion:format("trunk number (~B)", [N])).

-define(OBJ_TOKENS, ["subscriber", "network-full", "network-short", "trunk", "none"]).
-define(GROUP_TOKENS, ["combined", "separated"]).
-define(NUMBER_TOKENS, ["-n", "-number", "-t", "-trunk"]).
-define(PRIORITY_TOKENS, ["high", "normal"]).
-define(CATEGORY_TOKENS, ["combined", "separated", "statistic"]).
-define(INFO_KSL_TOKENS, ["-ksl-a", "-k-a", "-ksl-b", "-k-b", "-ksl", "-k", "-T", "-type"]).
-define(GET_INFO_TOKENS, ["-id", "-type", "-T", "-trunk", "-t", "-indicator", "-i", "-n", "-number"]).
-define(INDICATOR_TOKENS, ["private", "local", "zone", "city", "international", "emergency", "none"]).

-export([
    get_role_list/0,
    get_expand_function/1,

    start_sorm_args/1,
    stop_sorm_args/1,
    set_password_args/1,
    set_ksl_args/1,
    wiretap_add_args/1,
    free_ksl_args/1,
    kick_ksl_args/1,
    get_info_args/1,
    info_ksl_args/1,
    list_ss_args/1,
    stop_sending_args/1,
    test_args/1,
    wiretap_ed_args/1,
    info_trunk_group_args/1,
    get_version_args/1,
    error_args/1,
    repeat_args/1
]).

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

get_role_list() ->
    ["start-sorm",
     "stop-sorm",
     "set-password",
     "set-ksl",
     "wiretap-add",
     "wiretap-del",
     "wiretap",
     "free-ksl",
     "kick-ksl",
     "get-info",
     "info-ksl",
     "list-ss",
     "stop-sending",
     "test",
     "wiretap-ed",
     "info-trunk-group",
     "get-version",
     "help",
     "record-start",
     "record-stop",
     "repeat",
     "exit"
    ].

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

get_expand_function("start-sorm") ->
    fun start_sorm_args/1;

get_expand_function("stop-sorm") ->
    fun stop_sorm_args/1;

get_expand_function("set-password") ->
    fun set_password_args/1;

get_expand_function("set-ksl") ->
    fun set_ksl_args/1;

get_expand_function("wiretap-add") ->
    fun wiretap_add_args/1;

get_expand_function("wiretap-del") ->
    fun wiretap_del_args/1;

get_expand_function("wiretap") ->
    fun wiretap_args/1;

get_expand_function("free-ksl") ->
    fun free_ksl_args/1;

get_expand_function("kick-ksl") ->
    fun kick_ksl_args/1;

get_expand_function("get-info") ->
    fun get_info_args/1;

get_expand_function("info-ksl") ->
    fun info_ksl_args/1;

get_expand_function("list-ss") ->
    fun list_ss_args/1;

get_expand_function("stop-sending") ->
    fun stop_sending_args/1;

get_expand_function("test") ->
    fun test_args/1;

get_expand_function("wiretap-ed") ->
    fun wiretap_ed_args/1;

get_expand_function("info-trunk-group") ->
    fun info_trunk_group_args/1;

get_expand_function("get-version") ->
    fun get_version_args/1;

get_expand_function("help") ->
    {service, fun help_args/1};

get_expand_function("record-start") ->
    {service, fun record_start_args/1};

get_expand_function("record-stop") ->
    {service, fun record_stop_args/1};

get_expand_function("exit") ->
    {service, fun exit_args/1};

get_expand_function("repeat") ->
    {service, fun repeat_args/1};

get_expand_function(_) ->
    {service, fun error_args/1}.

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

start_sorm_args(args) ->
    [];

start_sorm_args(_Input) ->
    elx_expanders:end_of_command_expander().

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

stop_sorm_args(args) ->
    [];

stop_sorm_args(_Input) ->
    elx_expanders:end_of_command_expander().

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

set_password_args(args) ->
    [?NEW_PASSWORD];

set_password_args(Input) ->
    string:match(Input, [_P1 = string], [Delim = delim], [_Other = any]),

    case Delim of
        [] ->
            elx_expanders:help_expander("Enter new password: ");
        _ ->
            elx_expanders:end_of_command_expander()
    end.

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

set_ksl_args(args) ->
    ["KSL"];

set_ksl_args(Input) ->
    string:match(Input,
                 [_P1 = string], [D1 = delim], 
                 [ P2 = string], [D2 = delim],
                 [_P3 = string], [D3 = delim], 
                 [_P4 = string], [D4 = delim],
                 [_P5 = string], [D5 = delim],
                 [_P6 = string], [D6 = delim]),
    
    case {D1, D2, D3, D4, D5, D6} of
        {[], [], [], [], [], []} ->
            Str = ion:format("Enter ~s", [?KSL]),
            elx_expanders:help_expander(Str);
        {_, [], [], [], [], []} ->
            elx_expander:expand(P2, ?GROUP_TOKENS, " ");
        {_, _, [], [], [], []} ->
            Str = ion:format("Enter ~s", [?TRUKT_ID(1)]),
            elx_expanders:help_expander(Str);
        {_, _, _, [], [], []} ->
            Str = ion:format("Enter ~s", [?TRUNK_NUMBER(1)]),
            elx_expanders:help_expander(Str);
        {_, _, _, _, [], []} ->
            Str = ion:format("Enter ~s", [?TRUKT_ID(2)]),
            elx_expanders:help_expander(Str);
        {_, _, _, _, _, []} ->
            Str = ion:format("Enter ~s", [?TRUNK_NUMBER(2)]),
            elx_expanders:help_expander(Str);
        {_, _, _, _, _, _} ->
            elx_expanders:end_of_command_expander()
    end.

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

wiretap_add_args(args) ->
    [?ID];

wiretap_add_args(Input) ->
    string:match(Input,
                 [_P1 = string], [D1 = delim], 
                 [ P2 = string], [D2 = delim],
                 [ P3 = string], [D3 = delim], 
                 [ P4 = string], [D4 = delim],
                 [_P5 = string], [D5 = delim],
                 [ P6 = string], [D6 = delim],
                 [ P7 = string], [D7 = delim],
                 [ P8 = string], [D8 = delim]),

    case {D1, D2, D3, D4, D5, D6, D7, D8} of
        {[], [], [], [], [], [], [], []} ->
            Str = ion:format("Enter ~s", [?ID]),
            elx_expanders:help_expander(Str);
        {_, [], [], [], [], [], [], []} ->
            elx_expander:expand(P2, ?OBJ_TOKENS, " ");
        {_, _, [], [], [], [], [], []} ->
            elx_expander:expand(P3, ?INDICATOR_TOKENS, " ");
        {_, _, _, [], [], [], [], []} ->
            elx_expander:expand(P4, ?NUMBER_TOKENS, " ");
        {_, _, _, _, [], [], [], []} when P4 =:= "-n" orelse P4 =:= "-number" ->
            elx_expanders:help_expander("Enter number");
        {_, _, _, _, [], [], [], []} when P4 =:= "-t" orelse P4 =:= "-trunk" ->
            elx_expanders:help_expander("Enter trunk");
        {_, _, _, _, _, [], [], []} ->
            elx_expander:expand(P6, ?CATEGORY_TOKENS, " ");
        {_, _, _, _, _, _, [], []} when P6 =:= "statistic" ->
            elx_expander:expand(P7, ?PRIORITY_TOKENS, " ");
        {_, _, _, _, _, _, [], []} ->
            Str = ion:format("Enter ~s", [?KSL]),
            elx_expanders:help_expander(Str);
        {_, _, _, _, _, _, _, []} when P6 =:= "statistic" ->
            elx_expanders:end_of_command_expander();
        {_, _, _, _, _, _, _, []} ->
            elx_expander:expand(P8, ?PRIORITY_TOKENS, " ");
        {_, _, _, _, _, _, _, _} ->
            elx_expanders:end_of_command_expander()
    end.

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

wiretap_del_args(args) ->
    [?ID];

wiretap_del_args(Input) ->
    string:match(Input,
                 [_P1 = string], [D1 = delim], 
                 [ P2 = string], [D2 = delim],
                 [ P3 = string], [D3 = delim], 
                 [ P4 = string], [D4 = delim],
                 [_P5 = string], [D5 = delim]),

    case {D1, D2, D3, D4, D5} of
        {[], [], [], [], []} ->
            Str = ion:format("Enter ~s", [?ID]),
            elx_expanders:help_expander(Str);
        {_, [], [], [], []} ->
            elx_expander:expand(P2, ?OBJ_TOKENS, " ");
        {_, _, [], [], []} ->
            elx_expander:expand(P3, ?INDICATOR_TOKENS, " ");
        {_, _, _, [], []} ->
            elx_expander:expand(P4, ?NUMBER_TOKENS);
        {_, _, _, _, []} when P4 =:= "-n" orelse P4 =:= "-number" ->
            elx_expanders:help_expander("Enter number");
        {_, _, _, _, []} when P4 =:= "-t" orelse P4 =:= "-trunk" ->
            elx_expanders:help_expander("Enter trunk");
        {_, _, _, _, _} ->
            elx_expanders:end_of_command_expander()
    end.

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

wiretap_args(args) ->
    [?CALL_ID];

wiretap_args(Input) ->
    string:match(Input,
                 [_P1 = string], [D1 = delim], 
                 [_P2 = string], [D2 = delim]),

    case {D1, D2} of
        {[], []} ->
            Str = ion:format("Enter ~s", [?CALL_ID]),
            elx_expanders:help_expander(Str);
        {_, []} ->
            Str = ion:format("Enter ~s", [?KSL]),
            elx_expanders:help_expander(Str);
        {_, _} ->
            elx_expanders:end_of_command_expander()
    end.

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

free_ksl_args(args) ->
    [?CALL_ID, ?TRUKT_ID(1)];

free_ksl_args(Input) ->
    string:match(Input,
                 [_P1 = string], [D1 = delim], 
                 [_P2 = string], [D2 = delim],
                 [_P3 = string], [D3 = delim],
                 [_P4 = string], [D4 = delim]),

    case {D1, D2, D3, D4} of
        {[], [], [], []} ->
            Str = ion:format("Enter ~s or ~s", [?CALL_ID, ?TRUKT_ID(1)]),
            elx_expanders:help_expander(Str);
        {_, [], [], []} ->
            Str = ion:format("Enter ~s", [?TRUNK_NUMBER(1)]),
            elx_expanders:help_expander(Str);
        {_, _, [], []} ->
            Str = ion:format("Enter ~s", [?TRUKT_ID(2)]),
            elx_expanders:help_expander(Str);
        {_, _, _, []} ->
            Str = ion:format("Enter ~s", [?TRUNK_NUMBER(2)]),
            elx_expanders:help_expander(Str);
        {_, _, _, _} ->
            elx_expanders:end_of_command_expander()
    end.

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

kick_ksl_args(args) ->
    [?KSL];

kick_ksl_args(Input) ->
    string:match(Input,
                 [_P1 = string], [D1 = delim], 
                 [_P2 = string], [D2 = delim],
                 [_P3 = string], [D3 = delim],
                 [_P4 = string], [D4 = delim],
                 [_P5 = string], [D5 = delim]),

    case {D1, D2, D3, D4, D5} of
        {[], [], [], [], []} ->
            Str = ion:format("Enter ~s", [?KSL]),
            elx_expanders:help_expander(Str);
        {_, [], [], [], []} ->
            Str = ion:format("Enter ~s", [?TRUKT_ID(1)]),
            elx_expanders:help_expander(Str);
        {_, _, [], [], []} ->
            Str = ion:format("Enter ~s", [?TRUNK_NUMBER(1)]),
            elx_expanders:help_expander(Str);
        {_, _, _, [], []} ->
            Str = ion:format("Enter ~s", [?TRUKT_ID(2)]),
            elx_expanders:help_expander(Str);
        {_, _, _, _, []} ->
            Str = ion:format("Enter ~s", [?TRUNK_NUMBER(2)]),
            elx_expanders:help_expander(Str);
        {_, _, _, _, _} ->
            elx_expanders:end_of_command_expander()
    end.

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

get_info_args(args) ->
    ?GET_INFO_TOKENS;

get_info_args(Input) ->
    get_info_args_local(Input, ?GET_INFO_TOKENS).

get_info_args_local(_Input, []) ->
    elx_expanders:end_of_command_expander();

get_info_args_local(Input, Params) ->
    string:match(Input,
                 [Key = string], [D1 = delim], 
                 [Value = string], [D2 = delim],
                 [Any = any]),

    case {D1, D2} of
        {[], []} ->
            elx_expander:expand(Key, Params, " ");
        {_, []} when Key =:= "-id" ->
            Str = ion:format("Enter ~s", [?ID]),
            elx_expanders:help_expander(Str);
        {_, []} when Key =:= "-T"; Key =:= "-type" ->
            elx_expander:expand(Value, ?OBJ_TOKENS, " ");
        {_, []} when Key =:= "-t"; Key =:= "-trunk" ->
            elx_expanders:help_expander("Enter trunk");
        {_, []} when Key =:= "-i"; Key =:= "-indicator" ->
            elx_expander:expand(Value, ?INDICATOR_TOKENS, " ");
        {_, []} when Key =:= "-n"; Key =:= "-number" ->
            elx_expanders:help_expander("Enter number");
        _ when Key =:= "-id" ->
            get_info_args_local(Any, Params -- ["-id"]);
        _ when Key =:= "-T"; Key =:= "-type" ->
            get_info_args_local(Any, Params -- ["-type", "-T"]);
        _ when Key =:= "-t"; Key =:= "-trunk" ->
            get_info_args_local(Any, Params -- ["-trunk", "-t"]);
        _ when Key =:= "-i"; Key =:= "-indicator" ->
            get_info_args_local(Any, Params -- ["-indicator", "-i"]);
        _ when Key =:= "-n"; Key =:= "-number" ->
            get_info_args_local(Any, Params -- ["-number", "-n"]);
        _ ->
            elx_expanders:help_expander("ERROR: Invalid command")
    end.

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

info_ksl_args(args) ->
    ?INFO_KSL_TOKENS;

info_ksl_args(Input) ->
    info_ksl_args_local(Input, ?INFO_KSL_TOKENS).

info_ksl_args_local(_Input, []) ->
    elx_expanders:end_of_command_expander();

info_ksl_args_local(Input, Params) ->
    string:match(Input,
                 [Key = string], [D1 = delim], 
                 [Value = string], [D2 = delim],
                 [Any = any]),

    case {D1, D2} of
        {[], []} ->
            elx_expander:expand(Key, Params, " ");
        {_, []} when Key =:= "-ksl-a"; Key =:= "-k-a" ->
            Str = ion:format("~s,~s", [?TRUKT_ID(1), ?TRUNK_NUMBER(1)]),
            elx_expanders:help_expander(Str);
        {_, []} when Key =:= "-ksl-b"; Key =:= "-k-b" ->
            Str = ion:format("~s,~s", [?TRUKT_ID(2), ?TRUNK_NUMBER(2)]),
            elx_expanders:help_expander(Str);
        {_, []} when Key =:= "-ksl"; Key =:= "-k" ->
            Str = ion:format("Enter ~s", [?KSL]),
            elx_expanders:help_expander(Str);
        {_, []} when Key =:= "-T"; Key =:= "-type" ->
            elx_expander:expand(Value, ?GROUP_TOKENS, " ");
        _ when Key =:= "-ksl-a"; Key =:= "-k-a" ->
            info_ksl_args_local(Any, Params -- ["-ksl-a", "-k-a"]);
        _ when Key =:= "-ksl-b"; Key =:= "-k-b" ->
            info_ksl_args_local(Any, Params -- ["-ksl-b", "-k-b"]);
        _ when Key =:= "-ksl"; Key =:= "-k" ->
            info_ksl_args_local(Any, Params -- ["-ksl", "-k"]);
        _ when Key =:= "-T"; Key =:= "-type" ->
            info_ksl_args_local(Any, Params -- ["-type", "-T"]);
        _ ->
            elx_expanders:help_expander("ERROR: Invalid command")
    end.

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

list_ss_args(args) ->
    ?INDICATOR_TOKENS;

list_ss_args(Input) ->
    string:match(Input,
                 [ P1 = string], [D1 = delim], 
                 [ P2 = string], [D2 = delim],
                 [_P3 = string], [D3 = delim]),

    case {D1, D2, D3} of
        {[], [], []} ->
            elx_expander:expand(P1, ?INDICATOR_TOKENS, " ");
        {_, [], []} ->
            elx_expander:expand(P2, ?NUMBER_TOKENS -- ["-t", "-trunk"], " ");
        {_, _, []} ->
            elx_expanders:help_expander("Enter number");
        {_, _, _} ->
            elx_expanders:end_of_command_expander()
    end.

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

stop_sending_args(args) ->
    [];

stop_sending_args(_Input) ->
    elx_expanders:end_of_command_expander().

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_args(args) ->
    [?ID_TEST];

test_args(Input) ->
    string:match(Input,
                 [_P1 = string], [D1 = delim]),

    case D1 of
        [] ->
            Str = ion:format("Enter ~s", [?ID_TEST]),
            elx_expanders:help_expander(Str);
        _ ->
            elx_expanders:end_of_command_expander()
    end.

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

wiretap_ed_args(args) ->
    [?ID];

wiretap_ed_args(Input) ->
    string:match(Input,
                 [_P1 = string], [D1 = delim],
                 [ P2 = string], [D2 = delim],
                 [ P3 = string], [D3 = delim],
                 [ P4 = string], [D4 = delim]),

    case {D1, D2, D3, D4} of
        {[], [], [], []} ->
            Str = ion:format("Enter ~s", [?ID]),
            elx_expanders:help_expander(Str);
        {_, [], [], []} ->
            elx_expander:expand(P2, ?CATEGORY_TOKENS, " ");
        {_, _, [], []} when P2 =:= "statistic" ->
            elx_expander:expand(P3, ?PRIORITY_TOKENS, " ");
        {_, _, [], []} ->
            Str = ion:format("Enter ~s", [?KSL]),
            elx_expanders:help_expander(Str);
        {_, _, _, []} when P2 =:= "statistic" ->
            elx_expanders:end_of_command_expander();
        {_, _, _, []} ->
            elx_expander:expand(P4, ?PRIORITY_TOKENS, " ");
        {_, _, _, _} ->
            elx_expanders:end_of_command_expander()
    end.

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

info_trunk_group_args(args) ->
    [?TRUNK_GROUP_ID];

info_trunk_group_args(Input) ->
    string:match(Input,
                 [_P1 = string], [D1 = delim]),

    case D1 of
        [] ->
            Str = ion:format("Enter ~s", [?TRUNK_GROUP_ID]),
            elx_expanders:help_expander(Str);
        _ ->
            elx_expanders:end_of_command_expander()
    end.

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

get_version_args(args) ->
    [];

get_version_args(_Input) ->
    elx_expanders:end_of_command_expander().

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

help_args(args) ->
    get_role_list();

help_args(Input) ->
    string:match(Input,
                 [P1 = string], [D1 = delim]),

    case D1 of
        [] ->
            elx_expander:expand(P1, get_role_list(), " ");
        _ ->
            elx_expanders:end_of_command_expander()
    end.

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

record_start_args(args) ->
    "";

record_start_args(_Input) ->
    elx_expanders:end_of_command_expander().

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

record_stop_args(args) ->
    "";

record_stop_args(Input) ->
    string:match(Input,
                 [_P1 = string], [D1 = delim]),

    case D1 of
        [] ->
            Str = ion:format("Enter ~s", [?FILENAME]),
            elx_expanders:help_expander(Str);
        _ ->
            elx_expanders:end_of_command_expander()
    end.

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

exit_args(args) ->
    "";

exit_args(_Input) ->
    elx_expanders:end_of_command_expander().

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

error_args(args) ->
    "";

error_args(_Input) ->
    Str = ion:format("ERROR: undefined command", []),
    elx_expanders:help_expander(Str).

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

repeat_args(args) ->
    "";

repeat_args(_Input) ->
    elx_expanders:end_of_command_expander().

%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++