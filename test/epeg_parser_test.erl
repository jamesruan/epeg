-module(epeg_parser_test).
-include_lib("eunit/include/eunit.hrl").
-include("epeg.hrl").
-include("epeg_parser.hrl").

-define(P(S), {1, S, ""}).
-define(M(S), {ok, {_, _, S}}).

-define(S(F), {timeout, 10, {??F, F}}).

-define(SYM(S), epeg_parser:grammar(S)).

setup() ->
	pass.


symbol_test_() ->
	setup(),
	[
	 ?S(fun test_EOF/0)
	,?S(fun test_EOL/0)
	,?S(fun test_Space/0)
	,?S(fun test_Comment/0)
	,?S(fun test_Spacing/0)
	,?S(fun test_DOT/0)
	,?S(fun test_CLOSE/0)
	,?S(fun test_OPEN/0)
	,?S(fun test_PLUS/0)
	,?S(fun test_STAR/0)
	,?S(fun test_QUESTION/0)
	,?S(fun test_NOT/0)
	,?S(fun test_AND/0)
	,?S(fun test_SLASH/0)
	,?S(fun test_LARROW/0)
	,?S(fun test_Char/0)
	,?S(fun test_Range/0)
	,?S(fun test_Class/0)
	,?S(fun test_Literal/0)
	,?S(fun test_Identifier/0)
	,?S(fun test_Primary/0)
	,?S(fun test_Suffix/0)
	,?S(fun test_Prefix/0)
	,?S(fun test_Sequence/0)
	,?S(fun test_Expression/0)
	,?S(fun test_Transformer/0)
	,?S(fun test_Definition/0)
	,?S(fun test_Grammar/0)
	,?S(fun test_bootstrap/0)
	].

test_EOF() ->
	F = ?SYM('_EOF'),
	?M([eof]) = F(?P("")).

test_EOL() ->
	F = ?SYM('_EOL'),
	?M([eol]) = F(?P("\r\n")),
	?M([eol]) = F(?P("\n")),
	?M([eol]) = F(?P("\r")).
	
test_Space() ->
	F = ?SYM('_Space'),
	?M([space]) = F(?P(" ")),
	?M([space]) = F(?P("\t")),
	?M([space]) = F(?P("\r\n")).

test_Comment() ->
	F = ?SYM('_Comment'),
	?M([{'comment', "Comment"}]) = F(?P("%Comment\n")).

test_Spacing() ->
	F = ?SYM('_Spacing'),
	?M([spacing]) = F(?P(" ")),
	?M([spacing]) = F(?P("")),
	?M([spacing]) = F(?P("%Comment\n")).

test_DOT() ->
	F = ?SYM('_DOT'),
	?M([dot]) = F(?P(".")),
	?M([dot]) = F(?P(". ")).

test_CLOSE() ->
	F = ?SYM('_CLOSE'),
	?M([close]) = F(?P(")")),
	?M([close]) = F(?P(") ")).

test_OPEN() ->
	F = ?SYM('_OPEN'),
	?M([open]) = F(?P("(")),
	?M([open]) = F(?P("( ")).

test_PLUS() ->
	F = ?SYM('_PLUS'),
	?M([plus]) = F(?P("+")),
	?M([plus]) = F(?P("+ ")).

test_STAR() ->
	F = ?SYM('_STAR'),
	?M([star]) = F(?P("*")),
	?M([star]) = F(?P("* ")).

test_QUESTION() ->
	F = ?SYM('_QUESTION'),
	?M([question]) = F(?P("?")),
	?M([question]) = F(?P("? ")).

test_NOT() ->
	F = ?SYM('_NOT'),
	?M(['not']) = F(?P("!")),
	?M(['not']) = F(?P("! ")).

test_AND() ->
	F = ?SYM('_AND'),
	?M(['and']) = F(?P("&")),
	?M(['and']) = F(?P("& ")).

test_SLASH() ->
	F = ?SYM('_SLASH'),
	?M([slash]) = F(?P("/")),
	?M([slash]) = F(?P("/ ")).

test_LARROW() ->
	F = ?SYM('_LARROW'),
	?M([larrow]) = F(?P("<-")),
	?M([larrow]) = F(?P("<- ")).

test_Char() ->
	F = ?SYM('_Char'),
	?M([{char, "\r"}]) = F(?P("\\r")),
	?M([{char, "\n"}]) = F(?P("\\n")),
	?M([{char, "\t"}]) = F(?P("\\t")),
	?M([{char, "["}]) = F(?P("\\[")),
	?M([{char, "`"}]) = F(?P("\\`")),
	?M([{char, "]"}]) = F(?P("\\]")),
	?M([{char, "\""}]) = F(?P("\\\"")),
	?M([{char, "'"}]) = F(?P("\\'")),
	?M([{char, "\\"}]) = F(?P("\\\\")),
	?M([{char, "a"}]) = F(?P("a")).

test_Range() ->
	F = ?SYM('_Range'),
	?M([{charr, $a, $z}]) = F(?P("a-z")),
	?M([{char, $a}]) = F(?P("a")).

test_Class() ->
	F = ?SYM('_Class'),
	?M([{charc, "tes"}]) = F(?P("[test]")),
	?M([{charr, $a, $z}]) = F(?P("[a-z]")),
	?M([{class, [{charr, $a, $z},
	    {charr, $A, $Z},
	    {charc, "\r\n\t"}]}]) = F(?P("[\ra-z\nA-Z\t]")).

test_Literal() ->
	F = ?SYM('_Literal'),
	?M([{literal, "te\"st\\"}]) = F(?P("'te\"st\\\\'")),
	?M([{literal, "te'st"}]) = F(?P("\"te'st\"")).

test_Identifier() ->
	F = ?SYM('_Identifier'),
	?M([{identifier, "'t'"}]) = F(?P("t")),
	?M([{identifier, "'t1'"}]) = F(?P("t1")),
	?M([{identifier, "'test'"}]) = F(?P("test")).

test_Primary() ->
	F = ?SYM('_Primary'),
	?M([{primary, {symbol,"'t'"}}]) = F(?P("t")),
	?M([{primary, {symbol,"'t'"}}]) = F(?P("(t)")),
	?M([{primary, {string,"test"}}]) = F(?P("'test'")),
	?M([{primary, {charr, $a, $z}}]) = F(?P("[a-z]")),
	?M([{primary, {charc, "tes"}}]) = F(?P("[test]")),
	?M([{primary, {alt, [{charr, $a, $z}, {charr, $A, $Z}, {charc, "\r\n\t"}]}}]) = F(?P("[\ra-z\nA-Z\t]")),
	?M([{primary, {anychar}}]) = F(?P(".")).

test_Suffix() ->
	F = ?SYM('_Suffix'),
	?M([{suffix, {option, {anychar}}}]) = F(?P(".?")),
	?M([{suffix, {rep, {anychar}}}]) = F(?P(".*")),
	?M([{suffix, {more, {anychar}}}]) = F(?P(".+")),
	?M([{suffix, {anychar}}]) = F(?P(".")).

test_Prefix() ->
	F = ?SYM('_Prefix'),
	?M([{prefix, {p_and, {anychar}}}]) = F(?P("&.")),
	?M([{prefix, {p_not, {anychar}}}]) = F(?P("!.")),
	?M([{prefix, {more, {anychar}}}]) = F(?P(".+")).

test_Sequence() ->
	F = ?SYM('_Sequence'),
	?M([{sequence, {seq, [{anychar}, {anychar}]}}]) = F(?P(". .")),
	?M([{sequence, {more, {anychar}}}]) = F(?P(".+")).

test_Expression() ->
	F = ?SYM('_Expression'),
	?M([{expression, {alt, [{symbol, "'t'"}, {anychar}]}}]) = F(?P("t/.")),
	?M([{expression, {symbol, "'t'"}}]) = F(?P("t")).

test_Transformer()->
	F = ?SYM('_Transformer'),
	?M([{transformer, "fun (_) ->\n\tok end"}]) = F(?P("`fun (_) ->\n\tok end`")).

test_Definition() ->
	F = ?SYM('_Definition'),
	?M([{definition, "'Transformer'", {seq, [{string, "`"}, {rep, {seq, [{p_not, {string ,"`"}}, {anychar} ]}}, {string, "`"}]}, "fun ([\"`\", L, \"`\"]) -> L end"}]) = F(?P("Transformer <- '`' (!'`' .)* '`'\n`fun ([\"\\`\", L, \"\\`\"]) -> L end`\n")).

test_Grammar() ->
	F = ?SYM('_Grammar'),
	?M([{grammar, [{definition, "'Start'", {symbol, "'Transformer'"}}, {definition, "'Transformer'", {seq, [{string, "`"}, {rep, {seq, [{p_not, {string ,"`"}}, {anychar} ]}}, {string, "`"}]}, "fun ([\"`\", L, \"`\"]) -> L end"}]}]) = F(?P("Start <- Transformer\nTransformer <- '`' (!'`' .)* '`'\n`fun ([\"\\`\", L, \"\\`\"]) -> L end`\n")).

test_bootstrap() ->
	{ok, B} = file:read_file("../priv/grammar.epeg"),
	F = ?SYM('_Grammar'),
	?M(R) = F(?P(binary:bin_to_list(B))),
	ok = file:write_file("../priv/test.ast", erlang:list_to_bitstring(io_lib:format("~p", [R]))).
