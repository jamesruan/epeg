-ifndef(epeg_parser_h).
-define(epeg_parser_h, 1).
-include("epeg.hrl").
-define(TR(A),     fun(State) -> epeg_combinator:c_tr(State, A) end).
-define(EMPTY(),   fun(State) -> epeg_combinator:c_empty(State, []) end).
-define(ANYCHAR(), fun(State) -> epeg_combinator:c_anychar(State, []) end).
-define(CHAR(A),   fun(State) -> epeg_combinator:c_char(State, A) end).
-define(CHARR(A),  fun(State) -> epeg_combinator:c_charrange(State, A) end).
-define(CHARC(A),  fun(State) -> epeg_combinator:c_charclass(State, A) end).
-define(STRING(A), fun(State) -> epeg_combinator:c_string(State, A) end).
-define(SEQ(A),    fun(State) -> epeg_combinator:c_seq(State, A) end).
-define(PAND(A),   fun(State) -> epeg_combinator:c_pred_and(State, A) end).
-define(PNOT(A),   fun(State) -> epeg_combinator:c_pred_not(State, A) end).
-define(ALT(A),    fun(State) -> epeg_combinator:c_alt(State, A) end).
-define(REP(A),    fun(State) -> epeg_combinator:c_rep(State, A) end).
-define(MORE(A),   fun(State) -> epeg_combinator:c_more(State, A) end).
-define(OPT(A),    fun(State) -> epeg_combinator:c_option(State, A) end).
-endif.
