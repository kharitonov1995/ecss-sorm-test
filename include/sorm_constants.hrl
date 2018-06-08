%% Messages
-define(MSG_CRASH, "21").
-define(MSG_RESTART, "22").
-define(MSG_INFO_OBJ, "23").
-define(MSG_INFO_KSL, "24").
-define(MSG_LIST_OF_SS, "25").
-define(MSG_CRACK, "26").
-define(MSG_ACK, "27").
-define(MSG_EXEC, "28").
-define(MSG_TEST, "29").
-define(MSG_INFO_TRUNK, "2A").
-define(MSG_VERSION, "2B").

%% Notifications
-define(N_RECEIVE, "41").
-define(N_ANSWER, "42").
-define(N_DISCONNECT, "43").
-define(N_USE_SS, "44").
-define(N_CONNECT_KSL, "51").
-define(N_FREE_KSL, "52").

-define(STR_RECEIVE_NUMBER, "Receive full number").
-define(STR_ANSWER, "Answer").
-define(STR_DISCONNECT, "Disconnect").
-define(STR_USE_SS(Service, Code), ion:format("Use SS. Description service: ~s  Additional code: ~s", [Service, Code])).
-define(STR_CONNECT_KSL, "Connect KSL").
-define(STR_FREE_KSL, "Free KSL").

%Commands
-define(C_ATTACH, "07").
-define(C_FREE_KSL, "08").

% Other
-define(FLAG_OK, "00").
-define(UNDEFINED_TRUNK, "FFFF").
-define(TRUNK_TYPE, 16#03).
-define(UNDEFINED_NUM_INDICATOR, "FF").

% Supplementary services
-define(SS_FORWARD_UNCONDITIONAL, "21").
-define(SS_FORWARD_SUBS_BUSY, "29").
-define(SS_FORWARD_NO_REPLY, "2A").
-define(SS_FORWARD_SUBS_NOT_REACH, "2B").
-define(SS_ALL_FORWARD, "20").
-define(SS_ALL_CONDITIONAL_FORWARD, "28").
-define(SS_CALL_WAITING, "41").
-define(SS_CALL_HOLD, "42").
-define(SS_COMPLETION_CALL, "43").
-define(SS_HOLD_END, "40").
-define(SS_THREE_PARTY, "52").
-define(SS_MULTIPARTY, "51").
-define(SS_ALL_MULTIPARTY, "50").
-define(SS_CALLS_TRANSFER, "31").
-define(SS_CALL_PICKUP, "32").
-define(SS_CONSULTATION_CALL, "33").
-define(SS_UNDEFINED, "FF").

-define(STR_FORWARD_UNCONDITIONAL, "Call forwarding unconditional").
-define(STR_FORWARD_SUBS_BUSY, "Call forwarding on subscruber busy").
-define(STR_FORWARD_NO_REPLY, "Call forwarding on no reply").
-define(STR_FORWARD_SUBS_NOT_REACH, "Call forwarding on mobil subscriber not reachabl").
-define(STR_ALL_FORWARD, "All forwarding").
-define(STR_ALL_CONDITIONAL_FORWARD, "All conditional forwarding").
-define(STR_CALL_WAITING, "Call waiting").
-define(STR_CALL_HOLD, "Call hold").
-define(STR_COMPLETION_CALL, "Completion of call to busy subscribers").
-define(STR_HOLD_END, "HOLD End").
-define(STR_THREE_PARTY, "3-party").
-define(STR_MULTIPARTY, "Multiparty").
-define(STR_ALL_MULTIPARTY, "All multiparty SS").
-define(STR_CALLS_TRANSFER, "Calls Transfer").
-define(STR_CALL_PICKUP, "Call Pickup").
-define(STR_CONSULTATION_CALL, "Consultation Call").
