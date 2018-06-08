%% leex file structure
 
Definitions.

C   = [a-zA-Z]
N   = [0-9]

Rules.

({C}|{N}|\-|\~|\/|\.)+       :   {token, 
                            case command(TokenChars) of
                                true    -> {key, TokenLine, erlang:list_to_atom(TokenChars)};
                                false   -> {string, TokenLine, TokenChars};
                                pass    -> {new_pass, TokenLine, erlang:list_to_atom(TokenChars)}
                            end}.

(\s+\-)             :   {token, {sep, TokenLine, TokenChars}}.

(\s+)               :   {token, {space, TokenLine, TokenChars}}.

(,|,\s+|\s+,\s+)    :   {token, {',', TokenLine, TokenChars}}.

Erlang code.

command("start-sorm") ->
    true;
command("stop-sorm") ->
    true;
command("set-password") ->
    pass;
command("set-ksl") ->
    true;
command("wiretap-add") ->
    true;
command("wiretap-del") ->
    true;
command("wiretap") ->
    true;
command("free-ksl") ->
    true;
command("kick-ksl") ->
    true;
command("get-info") ->
    true;
command("info-ksl") ->
    true;
command("list-ss") ->
    true;
command("stop-sending") ->
    true;
command("test") ->
    true;
command("wiretap-ed") ->
    true;
command("info-trunk-group") ->
    true;
command("get-version") ->
    true;
command("help") ->
    true;
command("record-start") ->
    true;
command("record-stop") ->
    true;
command("repeat") ->
    true;
command("exit") ->
    true;

command(_) ->
    false.
