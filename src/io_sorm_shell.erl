%%%-------------------------------------------------------------------
%%% @author Artem Kharitonov
%%% @copyright (C) 2018, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(io_sorm_shell).

-behaviour(gen_server).
-behaviour(io_gen_sorm_shell).

-include_lib("pt_scripts/include/pt_str_parser.hrl").
-include_lib("chronica/include/chronica.hrl").
-include("ecss_sorm_test.hrl").
-include("messages.hrl").
-include("commands.hrl").

-define(SORM_ID_PASS, ["-p", "-password", "-sorm-id"]).
-define(CMD_EXIT, "exit").

%Callbacks
-export([
    init/1,
    code_change/3,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,

    start_shell/2,
    send_message/2,
    stop_shell/2
]).

-record(s, {
    reader,
    worker,
    options,
    formatter,
    record_mode = false
}).

-record(options_shell, {
    sorm_id,
    password,
    mode,
    ex_formatter
}).

-record(stop_shell, {
    reason
}).

-record(send_message, {
    message
}).

-record(io_line, {
    text
}).

-spec start_shell(WorkerPid :: pid(), InitArgs :: term()) -> {ok, ShellPid :: pid()} | {error, Reason :: term()}.
start_shell(WorkerPid, InitArgs) ->
    case gen_server:start_link({local, ?MODULE}, ?MODULE, [WorkerPid, InitArgs], []) of
        {ok, Pid} ->
            erlang:send(WorkerPid, {ok, Pid}),
            Pid;
        Error ->
            erlang:send(WorkerPid, Error),
            throw(Error)
    end.
-spec stop_shell(ShellPid :: pid(), Reason :: term()) -> ok | {error, shell_not_found}.
stop_shell(ShellPid, Reason) when is_pid(ShellPid) ->
    case erlang:process_info(ShellPid) of
        undefined -> 
            {error, shell_not_found};
        _ ->
            gen_server:call(ShellPid, #stop_shell{reason = Reason})
    end;
stop_shell(_ShellPid, _Reason) ->
    {error, badarg}.

-spec send_message(ShellPid :: pid(), Message :: term()) -> ok | {error, shell_not_found}.
send_message(ShellPid, Message)  when is_pid(ShellPid) ->
    case erlang:process_info(ShellPid) of
        undefined -> 
            {error, shell_not_found};
        _ ->
            gen_server:cast(ShellPid, #send_message{message = Message})
    end;
send_message(_ShellPid, _Reason) ->
    {error, badarg}.

init([WorkerPid, InitArgs]) ->
    Self = self(),
    ReadPid = spawn_link(fun() -> io:setopts([{expand_fun, fun expand_fun/1}]), read_line(Self) end),

    Options = parse_args(InitArgs, #options_shell{}),
    Formatter = 
    case Options of
        #options_shell{mode = 'shell-hex'} ->
            fun formatter_hex:print_formatter/2;
        #options_shell{ex_formatter = true} ->
            fun formatter:print_formatter_expand/2;
        _ ->
            fun formatter:print_formatter/2
    end,

    {ok, #s{reader = ReadPid, worker = WorkerPid, options = Options, formatter = Formatter}}.

handle_call(#stop_shell{reason = Reason}, _From, S) ->
    log:debug(debug, "~p", [{exit, Reason}]),
    {stop, normal, ok, S};
handle_call(_Message, _From, S) -> {reply, ok, S}.

handle_cast(#send_message{message = {send, Message}}, #s{formatter = Formatter} = S) ->
    try 
        Formatter(Message, send) 
    catch
        _:Reason ->
            formatter:err("~p~n", [Reason]),
            log:warning(error_log, "~p", [{Reason, erlang:get_stacktrace()}])
    end,
    {noreply, S};

handle_cast(#send_message{message = {error, Error}}, S) -> 
    formatter:err("~p~n", [Error]),
    {noreply, S};

handle_cast(#send_message{message = {warning, Warning}}, S) -> 
    formatter:warning("~p~n", [Warning]),
    {noreply, S};

handle_cast(#send_message{message = {test_failed, Path, Message}}, S) ->
    PrintFun = fun formatter:err/2,
    do_test_failed(PrintFun, Path, Message),
    log:debug(debug, "Message: ~p, Path: ~p", [Message, Path]),
    {noreply, S};

handle_cast(#send_message{message = {optional_test_failed, Path, Message}}, S) -> 
    PrintFun = fun formatter:warning/2,
    do_test_failed(PrintFun, Path, Message),
    {noreply, S};

%% Type: msg, info
handle_cast(#send_message{message = {_Type, Message}}, #s{formatter = Formatter} = S) ->
    try 
        Formatter(Message, recv)
    catch
        _:Reason ->
            formatter:err("~p~n", [Reason]),
            log:warning(error_log, "~p", [{Reason, erlang:get_stacktrace()}])
    end,
    {noreply, S};
handle_cast(_Message, S) ->
    {noreply, S}.

handle_info(#io_line{text = []}, S) ->
    {noreply, S};
handle_info(#io_line{text = Text}, S) ->
    S3 =
    try
        Result = parse_io_line(Text, S),
        S2 = 
        case do_io_line(Result, S) of
            #s{record_mode = Mode} ->
                S#s{record_mode = Mode};
            _ ->
                S
        end,

        try_record_command(Result, Text, S2),
        S2
    catch
        Error:Reason ->
            formatter:err("~p~n", [Reason]),
            log:warning(error_log, "~p", [{Error, Reason, erlang:get_stacktrace()}]),
            S
    end,
    {noreply, S3};
handle_info(_Message, S) -> {noreply, S}.

terminate(_Reason, _S) -> ok.
code_change(_OldVersion, S, _Extra) -> {ok, S}.

read_line(ShellPid) ->
    case io:get_line("> ") of
        {error, terminated} ->
            ok;
        Line ->
            Line2 = Line -- "\n",
            erlang:send(ShellPid, #io_line{text = Line2}),
            case Line2 of
                ?CMD_EXIT ++ _ ->
                    ok;
                _ ->
                    read_line(ShellPid)
            end
    end.

parse_args([], Options) ->
    Options;
parse_args([{mode, Mode} | Rest], Options) ->
    parse_args(Rest, Options#options_shell{mode = Mode});
parse_args([{id, Id} | Rest], Options) ->
    parse_args(Rest, Options#options_shell{sorm_id = Id});
parse_args([{pass, Pass} | Rest], Options) ->
    parse_args(Rest, Options#options_shell{password = erlang:list_to_bitstring(Pass)});
parse_args([{ex_formatter, ExFormatter} | Rest], Options) ->
    parse_args(Rest, Options#options_shell{ex_formatter = ExFormatter}).

validate_hex_format([[_,_]|Rest]) ->
    validate_hex_format(Rest);
validate_hex_format([[_]|_]) ->
    throw(badarg);
validate_hex_format([]) ->
    ok.

parse_io_line(Line, #s{formatter = formatter_hex}) ->
    L = string:tokens(Line, " "),
    validate_hex_format(L),
    Result =
    lists:foldl(fun(X, Acc) ->
                    B = 
                    try 
                        erlang:list_to_integer(X)
                    catch
                        error:_Reason ->
                            erlang:list_to_integer(X, 16)
                    end,
                    <<Acc/binary, B>>
                end, <<>>, L),
    {Line, Result};
parse_io_line(Line, #s{formatter = _, options = #options_shell{sorm_id = Id, password = Password}}) ->
    case sorm_shell_scan:string(Line) of
        {_, Tokens, _} ->
            case sorm_shell_parse:parse(Tokens) of 
                {ok, {Command, Args}} ->
                    [Id2, Password2 | Args2] = utils:check_id_and_password(Args, Id, Password),
                    %log:debug(debug, "Command: ~p", [Command]),
                    %log:debug(debug, "Args: ~p", [Args]),
                    %log:debug(debug, "Args2: ~p", [Args2]),
                    case utils:do_cmd(Command, Args2) of
                        {service, Record} ->
                            {service, Record};
                        Record ->
                            {#options_shell{sorm_id = Id2, password = Password2}, Record}
                    end;
                {error, Reason} ->
                    error({error_parse, Reason})
            end;
        {error, Reason} ->
            error({error_scan, Reason})
    end.

expand_fun(InputOrig) ->
    Input = lists:reverse(InputOrig),
    string:match(Input, [Role = string], [Delim1 = delim], [Any = any]),
    
    case Delim1 of
        [] ->
            RoleList = expand_prop:get_role_list(),
            elx_expander:expand(Role, RoleList, " ");
        _ ->
            case expand_prop:get_expand_function(Role) of
                {service, F} ->
                    F(Any);
                F ->
                    expand(Any, F, ?SORM_ID_PASS ++ F(args))
            end
    end.

expand(Input, F, AvailableParams) ->
    string:match(Input, [InputKey = string], [_Other = any]),

    try
        case InputKey of
            [] ->
                elx_expander:expand(InputKey, AvailableParams, " ");
            "-" ++ _ ->
                expand_id_and_pass_args(Input, AvailableParams);
            _ ->
                F(Input)
        end
    catch
        _:{invalid_command, InputRest} ->
           F(InputRest)
    end. 

expand_id_and_pass_args(Input, []) ->
    throw({invalid_command, Input});

expand_id_and_pass_args(Input, AvailableParams) ->
    string:match(Input, [InputKey = string], [Delim1 = delim], [_Value = string], [Delim2 = delim], [Any = any]),

    F =
    fun("-p") ->
        true;
       ("-password") ->
        true;
       ("-sorm-id") ->
        true;
       (_) ->
        false
    end,

    case {Delim1, Delim2} of
        {[], []} ->
            elx_expander:expand(InputKey, AvailableParams, " ");
        {_, []} when InputKey =:= "-p" orelse InputKey =:= "-password" ->
            elx_expanders:help_expander("Enter sorm password");
        {_, []} when InputKey =:= "-sorm-id" ->
            elx_expanders:help_expander("Enter sorm id");
        _ when InputKey =:= "-p" orelse InputKey =:= "-password" ->
            AvailableParams2 = lists:filter(F, AvailableParams),
            expand_id_and_pass_args(Any, AvailableParams2 -- ["-p", "-password"]);
        _ when InputKey =:= "-sorm-id" ->
            AvailableParams2 = lists:filter(F, AvailableParams),
            expand_id_and_pass_args(Any, AvailableParams2 -- [InputKey]);
        _ ->
            throw({invalid_command, Input})
    end.

do_io_line({#options_shell{sorm_id = Id, password = Password}, Record}, #s{formatter = Formatter, worker = WorkerPid,
                                                                     options = #options_shell{ex_formatter = ExFormatter}}) ->
    case ExFormatter of
        true ->
            Formatter({Id, Password, Record}, send);
        _ ->
            Formatter(Record, send)
    end,
    gen_server:cast(WorkerPid, #interactive_mode{id = Id, password = Password, command = Record});

% Была введена служебная команда
do_io_line({service, #record_start{} = Record} = Command, #s{worker = WorkerPid} = S) ->
    gen_server:cast(WorkerPid, Record),
    formatter:print_formatter(Command, undefined),
    S#s{record_mode = true};

do_io_line({service, #record_stop{} = Record} = Command, #s{worker = WorkerPid} = S) ->
    gen_server:cast(WorkerPid, Record),
    formatter:print_formatter(Command, undefined),
    S#s{record_mode = false};

do_io_line({service, #exit{}} = Command, #s{worker = WorkerPid}) ->
    formatter:print_formatter(Command, undefined),
    gen_server:call(WorkerPid, #stop{}),
    erlang:halt();

do_io_line({service, #repeat{}} = Command, #s{worker = WorkerPid}) ->
    formatter:print_formatter(Command, undefined),
    gen_server:cast(WorkerPid, #repeat{});

do_io_line({service, _} = Command, _S) ->
    formatter:print_formatter(Command, undefined);

do_io_line({HexLine, Result}, #s{worker = WorkerPid, formatter = Formatter}) ->
    Formatter(HexLine, send),
    gen_server:cast(WorkerPid, {interactive_mode, Result}).

try_record_command({#options_shell{sorm_id = _, password = _}, _Record}, Text, #s{worker = WorkerPid, record_mode = true}) ->
    gen_server:cast(WorkerPid, #record_command{command = Text});

try_record_command(_RecordService, _Text, _S) ->
    ok.

% {Field, Value} = expect, {Field2, Value2} = recv
-spec do_test_failed(PrintFun :: function(), Path :: string(), {Expect :: tuple(), Recv :: tuple()}) -> ok.
do_test_failed(PrintFun, Path, {{msg, Msg}, {_, Msg2}}) -> 
    Path2 = ion:format("[~s].msg:" ++
                        "\n  expect : ~s" ++ 
                        "\n  receive: ~s", [Path, Msg, Msg2]),
    PrintFun("~s~n", [Path2]);

do_test_failed(PrintFun, Path, {{ack_exec, ExpectMsg}, {_, ReceiveMsg}}) -> 
    Path2 = ion:format("~s:" ++
                        "\n  expect : ~s" ++ 
                        "\n  receive: ~s", [Path, ExpectMsg, ReceiveMsg]),
    PrintFun("~s~n", [Path2]);

do_test_failed(PrintFun, Path, {unexpected, CodeMsg}) ->
    Path2 = ion:format("[~s]:" ++
                            "\n  Unexpected message: ~s ", [Path, CodeMsg]),
    PrintFun("~s~n", [Path2]);

do_test_failed(PrintFun, Path, {{NumberA, NumberB}, {NumberA2, NumberB2}}) ->
    Path2 = ion:format("[~s].numbers:" ++
                            "\n  expect : ~s -> ~s" ++
                            "\n  receive: ~s -> ~s", [Path, NumberA, NumberB, NumberA2, NumberB2]),
    PrintFun("~s~n", [Path2]);

do_test_failed(PrintFun, Path, Errors) when is_list(Errors) ->
    Fun =
    fun({{Field, Expect}, {_, Recv}}) ->
        Path2 = ion:format("[~s].~p:" ++
                            "\n  expect : ~s" ++
                            "\n  receive: ~s", [Path, Field, Expect, Recv]),
        PrintFun("~s~n", [Path2])        
    end,
    lists:map(Fun, Errors);

do_test_failed(_, _, Other) ->
    formatter:err("Undefined test result: ~p~n", [Other]),
    log:debug(debug, "Undefined test result: ~p~n", [Other]).
