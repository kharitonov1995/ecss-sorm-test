-define(PORT_MESSAGES, 1001).
-define(PORT_NOTIFICATIONS, 1002).
-define(IP_ADDR, {192,168,23,64}).
-define(DEFAULT_MODE, required).
-define(DEFAULT_TIMEOUT, 15000).
-define(DEFAULT_REPEAT, 1).

-define(OPTIONAL_MODE, optional).
-define(REQUIRED_MODE, required).

-define(SORM_ID, sorm_id).
-define(PASSWORD, password).

-define(TEST_PREFIX, ion:format("test", [])).
-define(MGM_PREFIX(Id), ion:format("mgm@~b", [Id])).
-define(RECEIVE_PREFIX(Id), ion:format("receive@~b", [Id])).
-define(CALLS_PREFIX(Id, Id2), ion:format("calls@~b/call@~b", [Id, Id2])).

-define(SAVE_COMMANDS_PATH, "/tmp/ecss-sorm-test.commands").

-record(observer, {
    side
}).

-record(options, {
    mode    = ?DEFAULT_MODE    :: atom(),
    repeat  = ?DEFAULT_REPEAT  :: integer(),
    timeout = ?DEFAULT_TIMEOUT :: integer()
}).

-record(call, {
    id :: integer(),
    options :: #options{},
    observer :: #observer{},
    commands :: list()
}).

-record(call_wait, {
    status = wait :: atom(),
    call :: #call{},
    call_id
}).

-record(s_call, {
    call :: #call{},
    call_id,
    hash,
    status
}).

-record(s_worker, {
    sorm_id,
    password,
    socket_n,
    socket_msg,
    table_id,
    commands = [],
    mgm_buffer = [],
    status,

    mgm_call_db = maps:new(),
    call_db = maps:new(),
    call_w_db = maps:new(),
    call_res_db = maps:new(),
    buffer = [], % буфер нотификаций

    shell_pid,
    mode,

    record_mode = false,
    record_commands = []
}).

-record(mgm_call, {
    call_id,
    code_msg,
    status,
    status_n = false
}).

-record(mgm_test, {
    ack,
    exec
}).

-record(mgm_cmd, {
    id,
    command :: term(),
    test :: #mgm_test{}
}).

-record(variable, {
    name,
    value
}).

-record(calls, {
    id :: integer(),
    variables :: [#variable{}],
    list :: [#call{}]
}).

-record(recv_msg, {
    id :: integer(),
    options :: #options{},
    number_a,
    number_b,
    test,
    code_msg
}).

-record(record_command, {
    command
}).

-record(test, {
    obj,
    call,
    ni_a,
    ni_b,
    type,
    ksl_a,
    ksl_b,
    priority,
    ss
}).

-record(interactive_mode, {
    id,
    password,
    command
}).

-record(stop, {
}).

-record(repeat, {
}).
