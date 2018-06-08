-record(msg, {
    preamble,
    sorm_id,
    msg_id,
    len_msg,
    quantity_msg,
    number_msg,
    reserve_a,
    reserve_b,
    msg_text
}).

-record(msg_reply, {
    msg_id,
    flag
}).

-record(crash, {
    type,
    code
}).

-record(crack_sorm, {
    code,
    day_of_month,
    time
}).

-record(info_ksl, {
    ksl,
    type,
    num_ksl_a,
    num_ksl_b
}).

-record(info_object, {
    id,
    type,
    number_indicator,
    len_number,
    number,
    trunk_group_id,
    category,
    ksl,
    mark_priority,
    kit_status
}).

-record(description_service, {
    number_indicator,
    len_number,
    number,
    number_services,
    list_of_services,
    unimportant
}).

-record(reply_test_message, {
    id,
    status_1,
    status_2
}).

-record(info_trunk, {
    trunk_group_id,
    string
}).

-record(version, {
    string,
    unimportant
}).

-record(notification, {
    preamble,
    sorm_id,
    code_msg,
    len_msg,
    call_id,
    tag_selection_number,
    connection_params,
    code_ss,
    number_a,
    number_b,
    trunk_group_id,
    num_ksl_a,
    num_ksl_b,
    date,
    mark_priority,
    code_operation,
    description_service,
    additional_code
}).

-record(t_notification, {
    preamble,
    sorm_id,
    code_msg,
    len_msg,
    unimportant,
    test_id,
    status_1,
    status_2
}).