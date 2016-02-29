-module(epeg_parser).
-include_lib("eunit/include/eunit.hrl").


-include("epeg.hrl").
-include("epeg_parser.hrl").

-export([formal_grammar/0]).


% #Herarchical syntax
% Grammar <- Spacing Definition+ EndOfFile
% Definition <- Identifier LEFTARROW Expression Transformer
% Transformer <- '`' (!'`' .)+ '`' Spacing
% Expression <- Sequence (SLASH Sequence)*
% Sequence <- Prefix+
% Prefix <- (AND / NOT)? Suffix
% Suffix <- Primary (QUESTION / STAR / PLUS)?
% Primary <- Identifier !LEFTARROW
% / OPEN Expression CLOSE
% / Literal / Class / DOT

% # Lexical syntax
% Identifier <- IdentStart IdentCont* Spacing
% IdentStart <- [a-zA-Z_]
% IdentCont <- IdentStart / [0-9]
% Literal <- ['] (!['] Char)* ['] Spacing
% / ["] (!["] Char)* ["] Spacing
% Class <- '[' (!']' Range)* ']' Spacing
% Range <- Char '-' Char / Char
% Char <- '\\' [nrt'"\[\]\\] # support unicode
% / '\\' [0-2][0-7][0-7]
% / '\\' [0-7][0-7]?
% / !'\\' .
% LEFTARROW <- '<-' Spacing
% SLASH <- '/' Spacing
% AND <- '&' Spacing
% NOT <- '!' Spacing
% QUESTION <- '?' Spacing
% STAR <- '*' Spacing
% PLUS <- '+' Spacing
% OPEN <- '(' Spacing
% CLOSE <- ')' Spacing
% DOT <- '.' Spacing
% Spacing <- (Space / Comment)*
% Comment <- '%' (!EndOfLine .)* EndOfLine
% Space <- ' ' / '\t' / EndOfLine
% EndOfLine <- '\r\n' / '\n' / '\r'
% EndOfFile <- !.

formal_grammar() ->
% formal_grammar() matches PEG with combinators and generates a PEG parser.
% double loading to solve mutual dependency problem.
	grammar(), grammar().

grammar() ->
	?SET('_EOF', ?PNOT(?ANYCHAR(), fun matched_EOF/1)),
	?SET('_EOL', ?ALT([?STRING("\r\n"), ?CHAR($\n), ?CHAR($\r)], fun matched_EOL/1)),
	?SET('_Space', ?ALT([?CHAR($\ ), ?CHAR($\t), ?SYM('_EOL')], fun matched_Space/1)),
	?SET('_Comment', ?SEQ([?CHAR($%), ?REP(?THEN(?PNOT(?SYM('_EOL')), ?ANYCHAR())), ?SYM('_EOL')], fun matched_Comment/1)),
	?SET('_Spacing', ?REP(?ALT([?SYM('_Space'), ?SYM('_Comment')]), fun matched_Spacing/1)),
	?SET('_DOT', ?THEN(?CHAR($.), ?SYM('_Spacing'), fun matched_DOT/1)),
	?SET('_CLOSE', ?THEN(?CHAR($)), ?SYM('_Spacing'), fun matched_CLOSE/1)),
	?SET('_OPEN', ?THEN(?CHAR($(), ?SYM('_Spacing'), fun matched_OPEN/1)),
	?SET('_PLUS', ?THEN(?CHAR($+), ?SYM('_Spacing'), fun matched_PLUS/1)),
	?SET('_STAR', ?THEN(?CHAR($*), ?SYM('_Spacing'), fun matched_STAR/1)),
	?SET('_QUESTION', ?THEN(?CHAR($?), ?SYM('_Spacing'), fun matched_QUESTION/1)),
	?SET('_NOT', ?THEN(?CHAR($!), ?SYM('_Spacing'), fun matched_NOT/1)),
	?SET('_AND', ?THEN(?CHAR($&), ?SYM('_Spacing'), fun matched_AND/1)),
	?SET('_SLASH', ?THEN(?CHAR($/), ?SYM('_Spacing'), fun matched_SLASH/1)),
	?SET('_LARROW', ?THEN(?STRING("<-"), ?SYM('_Spacing'), fun matched_LARROW/1)),
	?SET('_Char', ?ALT([
		?THEN(?CHAR($\\), ?CHARC("rnt[]\\\"\'")),
		?THEN(?PNOT(?CHAR($\\)), ?CHARR(0, 16#10ffff))],
		fun matched_Char/1)),
	?SET('_Range', ?ORELSE(
		?SEQ([?SYM('_Char'), ?CHAR($-), ?SYM('_Char')]),
		?SYM('_Char'),
		fun matched_Range/1)),
	?SET('_Class', ?SEQ([?CHAR($[),
		?REP(?THEN(?PNOT(?CHAR($])), ?SYM('_Range'))),
		?CHAR($]), ?SYM('_Spacing')],
		fun matched_Class/1)),
	?SET('_Literal', ?ALT([
		?SEQ([?CHAR($'),
		?REP(?THEN(?PNOT(?CHAR($')), ?SYM('_Char'))),
		?CHAR($'), ?SYM('_Spacing')]),
		?SEQ([?CHAR($"),
		?REP(?THEN(?PNOT(?CHAR($")), ?SYM('_Char'))),
		?CHAR($"), ?SYM('_Spacing')])],
		fun matched_Literal/1)),
	?SET('_IdentStart', ?ALT([?CHARC("_"), ?CHARR($a, $z), ?CHARR($A, $Z)])),
	?SET('_IdentCont', ?ORELSE(?SYM('_IdentStart'), ?CHARR($0, $9))),
	?SET('_Identifier', ?SEQ([?SYM('_IdentStart'), ?REP(?SYM('_IdentCont')), ?SYM('_Spacing')], fun matched_Identifier/1)),
	?SET('_Primary', ?ALT([
		?THEN(?SYM('_Identifier'), ?PNOT(?SYM('_LARROW'))),
		?SEQ([?SYM('_OPEN'), ?SYM('_Expression'),  ?SYM('_CLOSE')]),
		?SYM('_Literal'),
		?SYM('_Class'),
		?SYM('_DOT')], fun matched_Primary/1)),
	?SET('_Suffix', ?THEN(?SYM('_Primary'), ?OPT(?ALT([?SYM('_QUESTION'), ?SYM('_STAR'), ?SYM('_PLUS')])), fun matched_Suffix/1)),
	?SET('_Prefix', ?THEN(?OPT(?ORELSE(?SYM('_AND'), ?SYM('_NOT'))), ?SYM('_Suffix'), fun matched_Prefix/1)),
	?SET('_Sequence', ?MORE(?SYM('_Prefix'), fun matched_Sequence/1)),
	?SET('_Expression', ?THEN(?SYM('_Sequence'), ?REP(?THEN(?SYM('_SLASH'), ?SYM('_Sequence'))), fun matched_Expression/1)).

matched_EOF(_L) ->
	% !.
	eof.

matched_EOL(_L) ->
	eol.
	
matched_Space(_L) ->
	space.

matched_Comment(L) ->
	[ "%", Comment, 'eol'] = L,
	{comment, lists:flatten(Comment)}.

matched_Spacing(_L) ->
	spacing.

matched_DOT(_L) ->
	dot.

matched_CLOSE(_L) ->
	close.

matched_OPEN(_L) ->
	open.

matched_PLUS(_L) ->
	plus.

matched_STAR(_L) ->
	star.

matched_QUESTION(_L) ->
	question.

matched_NOT(_L) ->
	'not'.

matched_AND(_L) ->
	'and'.

matched_SLASH(_L) ->
	slash.

matched_LARROW(_L) ->
	larrow.

matched_Char(L) ->
	R = case L of
	["\\", E]->
		case E of
		"r" -> "\r";
		"n" -> "\n";
		"t" -> "\t";
		"[" -> "[";
		"]" -> "]";
		"\"" -> "\"";
		"'" -> "'";
		"\\" -> "\\"
		end;
	O ->
		O
	end,
	{char, R}.

matched_Range(L) ->
	case L of
	[F, "-", T] ->
		{charr, F, T};
	O ->
		O
	end.

matched_Class(L) ->
	["[", S , "]", spacing] = L,
	RangeStrList = lists:filtermap(fun(E) ->
		case E of
		{charr, {char, [F]}, {char, [T]}} ->
			{true, {charr, {char, [F]}, {char, [T]}}};
		_ -> false
		end end, S),
	CharList = lists:filtermap(fun(E) ->
		case E of
		{char, [C]} ->
			{true, [C]};
		_ -> false
		end end, S),
	CharStr = case CharList of
	[] ->
		[];
	_ ->
		Dedup = lists:foldl(fun (E, Acc) ->
			[C] = E,
			case lists:member(C, Acc) of
			true ->
				Acc;
			false ->
				Acc ++ E
			end end, [], CharList),
		[{charc, Dedup}]
	end,
	[Head| Tail] = RangeStrList ++ CharStr,
	if
	length(Tail) >= 1 ->
		{alt, [Head | Tail]};
	true ->
		Head
	end.

matched_Literal(L) ->
	String = case L of
	["'", S, "'", spacing] ->
		lists:map(fun (E) ->
			{char, [C]} = E,
			C
			end, S);
	["\"", S, "\"", spacing] ->
		lists:map(fun (E) ->
			{char, [C]} = E,
			C
			end, S)
	end,
	{literal, String}.

matched_Identifier(L)->
	FL = lists:flatten(L),
	spacing = lists:last(FL),
	{identifier, lists:droplast(FL)}.

matched_PrimaryClass(L) ->
	case L of
	{charr, {char, [F]}, {char, [T]}} ->
		gen_charrange(F, T);
	{charc, CList} ->
		gen_charclass(CList)
	end.

matched_Primary(L) ->
	G = case L of
	{identifier, Id} ->
		gen_identifier(Id);
	[open, {expression, E}, close] ->
		E;
	{literal, S} ->
		gen_literal(S);
	{charr, {char, [F]}, {char, [T]}} ->
		gen_charrange(F, T);
	{charc, CList} ->
		gen_charclass(CList);
	{alt, R} ->
		PC = lists:map(fun matched_PrimaryClass/1, R),
		gen_alt(PC);
	dot ->
		gen_anychar()
	end,
	{primary, G}.

matched_Suffix(L) ->
	G = case L of
	[{primary, P}, question]->
		lists:concat(["epeg_combinator:c_option(", P, ")"]);
	[{primary, P}, star]->
		lists:concat(["epeg_combinator:c_rep(", P, ")"]);
	[{primary, P}, plus]->
		lists:concat(["epeg_combinator:c_more(", P, ")"]);
	{primary, P}->
		P
	end,
	{suffix, G}.

matched_Prefix(L) ->
	G = case L of
	['and', {suffix, P}]->
		lists:concat(["epeg_combinator:c_pred_and(", P, ")"]);
	['not', {suffix, P}]->
		lists:concat(["epeg_combinator:c_pred_not(", P, ")"]);
	{suffix, P} ->
		P
	end,
	{prefix, G}.

matched_Sequence(L) ->
	G = case L of
	[{prefix, H} | T] ->
		A = lists:foldl(fun({prefix, E}, Acc)->
			Acc ++ ", " ++ E
		end, H, T),
		lists:concat(["epeg_combinator:c_seq([", A, "])"]);
	{prefix, H}->
		H
	end,
	{sequence, G}.

matched_Expression(L) ->
	G = case L of
	[{sequence, H} | T] ->
		if
		T =:= [] ->
			H;
		true ->
			A = lists:foldl(fun([slash, E], Acc)->
				{sequence, SE} = E,
				Acc ++ ", " ++ SE
			end, H, T),
			lists:concat(["epeg_combinator:c_alt([", A, "])"])
		end;
	{sequence, H} ->
		H
	end,
	{expression, G}.

gen_identifier(Id) ->
	lists:concat(["epeg_combinator:c_symbol_get(\"", Id, "\")"]).

gen_literal(S) ->
	lists:concat(["epeg_combinator:c_string(\"", S, "\")"]).

gen_charrange(F, T) ->
	lists:concat(["epeg_combinator:c_charrange(", F, ", ", T, ")"]).

gen_charclass(CList) ->
	lists:concat(["epeg_combinator:c_charclass(\"", CList, "\")"]).

gen_alt(L) ->
	[Head | Tail] = L,
	S = lists:foldl(fun (E, Acc) ->
		Acc ++ ", " ++ E
		end, Head, Tail),
	lists:concat(["epeg_combinator:c_alt([", S, "])"]).

gen_anychar() ->
	"epeg_combinator:c_anychar()".
