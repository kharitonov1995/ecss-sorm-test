-define(RECEIVING_FULL_NUMBER_CODE, 41). 

-record(start_sorm, {
    command_id = 16#01
}).

-record(stop_sorm, {
    command_id = 16#02
}).

-record(set_password, {
    command_id = 16#03,
    new_password = <<"123456">>
}).

-record(set_ksl, {
    command_id = 16#04,
    ksl,
    type_group,
    num_ksl_a,
    num_ksl_b
}).

-record(wiretap_add, {
    command_id = 16#05,
    id,
    type,
    number_indicator, %признак номера телефона
    len_phone_number,
    number = <<16#FF>>,
    trunk_group_id = <<16#FFFF:16>>,
    category,
    ksl = 16#FF,
    mark_priority
}).

-record(wiretap_del, {
    command_id = 16#06,
    id = <<16#01, 16#00>>,
    type = 16#01,
    number_indicator = 16#01, %признак номера телефона
    len_phone_number,
    number = <<16#FF>>,
    trunk_group_id = <<16#FFFF:16>>
}).

-record(connect_to_call, {
    command_id = 16#07,
    call_id = <<16#FFFFFFFFFF:40>>,
    ksl
}).

-record(free_ksl, {
    command_id = 16#08,
    call_id = <<16#FFFFFFFFFF:40>>,
    num_ksl_a = <<16#FF>>,
    num_ksl_b = <<16#FF>>
}).

-record(kick_ksl, {
    command_id = 16#09,
    ksl,
    num_ksl_a,
    num_ksl_b 
}).

-record(get_info, {
    command_id = 16#0A,
    id = <<16#FFFF:16>>,
    type = 16#FF,
    number_indicator = 16#FF,
    number = <<16#FF>>,
    trunk_group_id = <<16#FFFF:16>> %номер входящего пучка
}).

-record(info_ksl_btw_group, {
    command_id = 16#0B,
    ksl = 16#FF,
    type = 16#FF,
    num_ksl_a = <<16#FF>>,
    num_ksl_b = <<16#FF>>
}).

-record(list_ss, {
    command_id = 16#0C,
    number_indicator = 16#01,
    len_phone_number,
    number = <<16#FF>>
}).

-record(stop_send_message, {
    command_id = 16#0D
}).

-record(test_message, {
    command_id = 16#0E,
    id_message = 16#00
}).

-record(wiretap_ed, {
    command_id = 16#0F,
    id = <<16#01, 16#00>>,
    category = 16#02,
    ksl = 16#FF,
    mark_priority = 16#02
}).

-record(info_trunk_group, {
    command_id = 16#10,
    trunk_group_id = <<16#FF, 16#FF>>
}).

-record(get_version, {
    command_id = 16#11
}).

% Cлужебные команды
-record(help, {
    command
}).

-record(record_start, {
}).

-record(record_stop, {
    filename
}).

-record(exit, {
}).