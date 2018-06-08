%%%-------------------------------------------------------------------
%%% @author Artem Kharitonov
%%% @copyright (C) 2018, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ecss_sorm_test_manager).

-include_lib("chronica/include/chronica.hrl").
-include("ecss_sorm_test.hrl").
-include("commands.hrl").
-include("messages.hrl").

-behaviour(gen_server).

%%Callbacks
-export([
    init/1,
    start_link/0,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%%API
-export([
    start_sorm/3,
    stop_sorm/1,

    play/1,
    load_file/2,

    start_shell/2
]).

-record(s, {
    clients = maps:new(),
    sorm_id_workers = maps:new(),
    password_workers = maps:new(),
    socket = undefined,
    pid_workers = maps:new(),
    last_num_worker = 0
}).

-record(start_shell, {
    ref_worker,
    init_args
}).

-record(load_file, {
    file_name,
    worker
}).

-record(play, {
    ref_worker
}).

-record(pause, {
    ref_worker
}).

-record(resume, {
    ref_worker
}).

-record(stop_play, {
    ref_worker
}).

-record(start, {
    sorm_id = 1,
    password = "123456",
    ip_addr, 
    pid_client
}).

-record(stop_sorm_worker, {
    sorm_ref
}).

%%====================================================================
%% API
%%====================================================================

start_shell(WorkerRef, InitArgs) ->
    gen_server:call(?MODULE, #start_shell{ref_worker = WorkerRef, init_args = InitArgs}).

-spec load_file(reference(), string()) -> ok | {error, reason}.
load_file(Worker, FileName) ->
    gen_server:call(?MODULE, #load_file{file_name = FileName, worker = Worker}).

-spec play(reference()) -> ok | {error, reason}.
play(RefWorker) ->
    gen_server:call(?MODULE, #play{ref_worker = RefWorker}).    

-spec start_link() -> {ok, pid()} | {error, reason}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
-spec start_sorm(integer(), string(), string()) -> {ok, reference()} | {error, reason}.
start_sorm(SormId, Password, IpAddr) ->
    try 
        case inet_parse:address(IpAddr) of
            {ok, IpAddr2} ->
                gen_server:call(?MODULE, #start{sorm_id = SormId, password = Password, 
                                                ip_addr = IpAddr2, pid_client = self()});
            Error ->
                throw(Error)
        end
    catch
        throw:Reason ->
            Reason
    end.

-spec stop_sorm(reference()) -> ok.
stop_sorm(SormRef) ->
    gen_server:cast(?MODULE, #stop_sorm_worker{sorm_ref = SormRef}).

%%====================================================================
%% Callbacks
%%====================================================================

init([]) ->
    {ok, #s{}}.

handle_call(#start_shell{ref_worker = RefWorker, init_args = InitArgs}, _From,  #s{pid_workers = PidWorkers} = S) ->
    case maps:find(RefWorker, PidWorkers) of
        {ok, PidWorker} ->
            log:debug(debug, "!!! InitArgs !!!: ~10000p", [InitArgs]),
            Result = gen_server:call(PidWorker, {start_shell, InitArgs}),
            {reply, Result, S};
        _ ->
            {reply, {error, worker_not_found}, S}
    end;

handle_call(#play{ref_worker = RefWorker}, _From,  S) ->
    case maps:find(RefWorker, S#s.pid_workers) of
        {ok, PidWorker} ->
            gen_server:cast(PidWorker, {play}),
            {reply, ok, S};
        _ ->
            {reply, {error, worker_not_found}, S}
    end;

handle_call(#stop_play{ref_worker = RefWorker}, _From,  S) ->
    case maps:find(RefWorker, S#s.pid_workers) of
        {ok, PidWorker} ->
            gen_server:cast(PidWorker, {stop_play}),
            {reply, ok, S};
        _ ->
            {reply, {error, worker_not_found}, S}
    end;

handle_call(#pause{ref_worker = RefWorker}, _From,  S) ->
    case maps:find(RefWorker, S#s.pid_workers) of
        {ok, PidWorker} ->
            gen_server:cast(PidWorker, {pause}),
            {reply, ok, S};
        _ ->
            {reply, {error, worker_not_found}, S}
    end;

handle_call(#resume{ref_worker = RefWorker}, _From,  S) ->
    case maps:find(RefWorker, S#s.pid_workers) of
        {ok, PidWorker} ->
            gen_server:cast(PidWorker, {resume}),
            {reply, ok, S};
        _ ->
            {reply, {error, worker_not_found}, S}
    end;

handle_call(#start{sorm_id = SormId, password = Password, ip_addr = IpAddr, pid_client = Pid2}, _From,  #s{clients = Clients} = S) ->
    SocketN = case gen_tcp:connect(IpAddr, ?PORT_NOTIFICATIONS, [binary, {active, true}]) of
        {ok, Socket} ->
            log:debug(debug, "Connection notifications is created", []),
            Socket;
        {error, Reason} ->
            log:warning(error_log, "Create connection notifications is failed: ~p", [Reason]),
            throw({reply, {error, {socket_notification_create, Reason}}, S})
    end,

    SocketMsg = case gen_tcp:connect(IpAddr, ?PORT_MESSAGES, [binary, {active, true}]) of
        {ok, Socket2} ->
            log:debug(debug, "Connection messages is created", []),
            Socket2;
        {error, Reason2} ->
            log:warning(error_log, "Create connection messages is failed: ~p", [Reason2]),
            throw({reply, {error, {socket_message_create, Reason2}}, S})
    end,

    NameWorker = erlang:list_to_atom("worker" ++ erlang:integer_to_list(S#s.last_num_worker + 1)),
    case ecss_sorm_test_worker:start_link(SormId, Password, SocketN, SocketMsg, NameWorker) of
        {ok, Pid} ->
            log:info("Manager: Service is started"),
            Ref = erlang:make_ref(),
            PidWorkers = maps:put(Ref, Pid, S#s.pid_workers),
            Clients2 = maps:put(Pid, Pid2, Clients),
            SormId2 = maps:put(Ref, SormId, S#s.sorm_id_workers),
            Password2 = binary:list_to_bin(Password),
            Password3 = maps:put(Ref, Password2, S#s.password_workers),
            gen_tcp:controlling_process(SocketN, Pid),
            gen_tcp:controlling_process(SocketMsg, Pid),
            {reply, {ok, Ref}, #s{sorm_id_workers = SormId2, password_workers = Password3, clients = Clients2,
                                  pid_workers = PidWorkers, last_num_worker = S#s.last_num_worker + 1}};
        {error, Reason3} ->
            throw({reply, {error, Reason3}, S})
    end;

handle_call(#load_file{file_name = _} = LoadFile, _From, S) ->
    load_file_local(LoadFile, S);

handle_call(_Message, _From, S) -> {reply, ok, S}.

handle_cast(#stop_sorm_worker{sorm_ref = SormRef}, S) ->
    case maps:find(SormRef, S#s.pid_workers) of
        {ok, Pid} ->
            SormId = maps:remove(SormRef, S#s.sorm_id_workers),
            Password = maps:remove(SormRef, S#s.password_workers),
            Workers = maps:remove(SormRef, S#s.pid_workers),
            Clients = maps:remove(Pid, S#s.clients),
            gen_server:call(Pid, {stop}),
            {noreply, S#s{sorm_id_workers = SormId, password_workers = Password, pid_workers = Workers, clients = Clients}};
        _ ->
            {noreply, S}
    end;

handle_cast(_Message, S) ->
    {noreply, S}.

handle_info(_Message, S) -> {noreply, S}.

terminate(_Reason, _S) -> ok.
code_change(_OldVersion, S, _Extra) -> {ok, S}.

%%====================================================================
%% Private
%%====================================================================

load_file_local(#load_file{file_name = FileName, worker = Worker}, 
                    #s{sorm_id_workers = SormIdMap, password_workers = PasswordMap} = S) ->
    try 
        case maps:find(Worker, S#s.pid_workers) of
            {ok, PidWorker} ->
                case file:read_file(FileName) of
                    {ok, JsonText} ->
                        {ok, SormId} = maps:find(Worker, SormIdMap),
                        {ok, Password} = maps:find(Worker, PasswordMap),

                        erlang:put(?SORM_ID, SormId),
                        erlang:put(?PASSWORD, Password),

                        Commands = json_helper:parse_json(JsonText),

                        erlang:erase(?SORM_ID),
                        erlang:erase(?PASSWORD),

                        case gen_server:call(PidWorker, {set_list_commands, Commands}) of
                            ok ->
                                {reply, ok, S};
                            Error ->
                                {reply, Error, S}
                        end;
                    Error2 ->
                        log:warning(error_log, "~p", [Error2]),
                        formatter:err("~p", [Error2]),
                        {reply, ok, S}
                end;
            _ ->
                Error = {error, {worker_not_found, Worker}},
                log:warning(error_log, "~p", [Error]),
                formatter:err("~p", [Error]),
                {reply, ok, S}
        end
    catch
        Error3:Reason ->
            log:warning(error_log, "~p", [{Error3, Reason, erlang:get_stacktrace()}]),
            formatter:err("~p", [{Error3, Reason}]),
            {reply, ok, S}
    end.
