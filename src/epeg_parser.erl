-module(epeg_parser).
-compile(nowarn_shadow_vars).
-include_lib("eunit/include/eunit.hrl").


-include("epeg.hrl").
-include("epeg_parser.hrl").

-define(SYM(S), grammar(S)).
-define(SET(S, P), grammar(S)->P).

-export([grammar/1]).


% #Herarchical syntax
% Grammar <- Spacing Definition+ EndOfFile
% Definition <- Identifier LEFTARROW Expression Transformer?
% Transformer <- '\`' (!'\`' Char)* '\`' Spacing
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
% Char <- '\\' [nrt'\`"\[\]\\] # support unicode
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

 ?SET('_EOF',
	?TR({?PNOT(?ANYCHAR()),
	fun matched_EOF/1}))
;?SET('_EOL',
	?TR({?ALT([?STRING("\r\n"), ?CHAR($\n), ?CHAR($\r)]),
	fun matched_EOL/1}))
;?SET('_Space',
	?TR({?ALT([?CHAR($\ ), ?CHAR($\t), ?SYM('_EOL')]),
	fun matched_Space/1}))
;?SET('_Comment',
	?TR({?SEQ([?CHAR($%), ?REP(?SEQ([?PNOT(?SYM('_EOL')), ?ANYCHAR()])), ?SYM('_EOL')]),
	fun matched_Comment/1}))
;?SET('_Spacing',
	?TR({?REP(?ALT([?SYM('_Space'), ?SYM('_Comment')])),
	fun matched_Spacing/1}))
;?SET('_DOT',
	?TR({?SEQ([?CHAR($.), ?SYM('_Spacing')]),
	fun matched_DOT/1}))
;?SET('_CLOSE',
	?TR({?SEQ([?CHAR($)), ?SYM('_Spacing')]),
	fun matched_CLOSE/1}))
;?SET('_OPEN',
	?TR({?SEQ([?CHAR($(), ?SYM('_Spacing')]),
	fun matched_OPEN/1}))
;?SET('_PLUS',
	?TR({?SEQ([?CHAR($+), ?SYM('_Spacing')]),
	fun matched_PLUS/1}))
;?SET('_STAR',
	?TR({?SEQ([?CHAR($*), ?SYM('_Spacing')]),
	fun matched_STAR/1}))
;?SET('_QUESTION',
	?TR({?SEQ([?CHAR($?), ?SYM('_Spacing')]),
	fun matched_QUESTION/1}))
;?SET('_NOT',
	?TR({?SEQ([?CHAR($!), ?SYM('_Spacing')]),
	fun matched_NOT/1}))
;?SET('_AND',
	?TR({?SEQ([?CHAR($&), ?SYM('_Spacing')]),
	fun matched_AND/1}))
;?SET('_SLASH',
	?TR({?SEQ([?CHAR($/), ?SYM('_Spacing')]),
	fun matched_SLASH/1}))
;?SET('_LARROW',
	?TR({?SEQ([?STRING("<-"), ?SYM('_Spacing')]),
	fun matched_LARROW/1}))
;?SET('_Char',
	?TR({?ALT([
		?SEQ([?CHAR($\\), ?CHARC("rnt[`]\\\"\'")]),
		?SEQ([?PNOT(?CHAR($\\)), ?CHARR({0, 16#10ffff})])]),
	fun matched_Char/1}))
;?SET('_Range',
	?TR({?ALT([
		?SEQ([?SYM('_Char'), ?CHAR($-), ?SYM('_Char')]),
		?SYM('_Char')]),
	fun matched_Range/1}))
;?SET('_Class',
	?TR({?SEQ([?CHAR($[),
		?REP(?SEQ([?PNOT(?CHAR($])), ?SYM('_Range')])),
		?CHAR($]), ?SYM('_Spacing')]),
	fun matched_Class/1}))
;?SET('_Literal',
	?TR({?ALT([
		?SEQ([?CHAR($'),
		?REP(?SEQ([?PNOT(?CHAR($')), ?SYM('_Char')])),
		?CHAR($'), ?SYM('_Spacing')]),
		?SEQ([?CHAR($"),
		?REP(?SEQ([?PNOT(?CHAR($")), ?SYM('_Char')])),
		?CHAR($"), ?SYM('_Spacing')])]),
	fun matched_Literal/1}))
;?SET('_IdentStart',
	?ALT([?CHAR($_), ?CHARR({$a, $z}), ?CHARR({$A, $Z})]))
;?SET('_IdentCont',
	?ALT([?SYM('_IdentStart'), ?CHARR({$0, $9})]))
;?SET('_Identifier',
	?TR({?SEQ([
		?SYM('_IdentStart'),
		?REP(?SYM('_IdentCont')),
		?SYM('_Spacing')]),
	fun matched_Identifier/1}))
;?SET('_Primary',
	?TR({?ALT([
		?SEQ([?SYM('_Identifier'), ?PNOT(?SYM('_LARROW'))]),
		?SEQ([?SYM('_OPEN'), ?SYM('_Expression'), ?SYM('_CLOSE')]),
		?SYM('_Literal'),
		?SYM('_Class'),
		?SYM('_DOT')]),
	fun matched_Primary/1}))
;?SET('_Suffix',
	?TR({?SEQ([?SYM('_Primary'),
	?OPT(?ALT([?SYM('_QUESTION'), ?SYM('_STAR'), ?SYM('_PLUS')]))]),
	fun matched_Suffix/1}))
;?SET('_Prefix',
	?TR({?SEQ([?OPT(?ALT([?SYM('_AND'), ?SYM('_NOT')])), ?SYM('_Suffix')]),
	fun matched_Prefix/1}))
;?SET('_Sequence',
	?TR({?MORE(?SYM('_Prefix')),
	fun matched_Sequence/1}))
;?SET('_Expression',
	?TR({?SEQ([?SYM('_Sequence'),
	?REP(?SEQ([?SYM('_SLASH'), ?SYM('_Sequence')]))]),
	fun matched_Expression/1}))
;?SET('_Transformer',
	?TR({?SEQ([?CHAR($`),
		?REP(?SEQ([?PNOT(?CHAR($`)), ?SYM('_Char')])),
		?CHAR($`), ?SYM('_Spacing')]),
	fun matched_Transformer/1}))
;?SET('_Definition', 
	?TR({?SEQ([
		?SYM('_Identifier'),
		?SYM('_LARROW'),
		?SYM('_Expression'),
		?OPT(?SYM('_Transformer'))]),
	fun matched_Definition/1}))
;?SET('_Grammar',
	?TR({?SEQ([
		?SYM('_Spacing'),
		?MORE(?SYM('_Definition')),
		?SYM('_EOF')]),
	fun matched_Grammar/1}))
.

matched_EOF(_L) ->
	% !.
	[eof].

matched_EOL(_L) ->
	[eol].
	
matched_Space(_L) ->
	[space].

matched_Comment(L) ->
	[ "%"| Commenteol] = L,
	eol = lists:last(Commenteol),
	Comment = lists:droplast(Commenteol),
	[{comment, lists:append(Comment)}].

matched_Spacing(_L) ->
	[spacing].

matched_DOT(_L) ->
	[dot].

matched_CLOSE(_L) ->
	[close].

matched_OPEN(_L) ->
	[open].

matched_PLUS(_L) ->
	[plus].

matched_STAR(_L) ->
	[star].

matched_QUESTION(_L) ->
	[question].

matched_NOT(_L) ->
	['not'].

matched_AND(_L) ->
	['and'].

matched_SLASH(_L) ->
	[slash].

matched_LARROW(_L) ->
	[larrow].

matched_Char(L) ->
	R = case L of
	["\\", E]->
		case E of
		"r" -> "\r";
		"n" -> "\n";
		"t" -> "\t";
		"[" -> "[";
		"`" -> "`";
		"]" -> "]";
		"\"" -> "\"";
		"'" -> "'";
		"\\" -> "\\"
		end;
	[O] ->
		O
	end,
	[{char, R}].

matched_Range(L) ->
	case L of
	[{char, [F]}, "-", {char, [T]}] ->
		[{charr, F, T}];
	[{char, [O]}]->
		[{char, O}]
	end.

matched_Class(L) ->
	spacing = lists:last(L),
	["[" | X ] = lists:droplast(L),
	"]" = lists:last(X),
	S = lists:droplast(X),
	RangeStrList = lists:filtermap(fun(E) ->
		case E of
		{charr, F, T} ->
			{true, {charr, F, T}};
		_ -> false
		end end, S),
	CharList = lists:filtermap(fun(E) ->
		case E of
		{char, C} ->
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
		[{class, [Head | Tail]}];
	true ->
		[Head]
	end.

matched_Literal(L) ->
	spacing = lists:last(L),
	[M | X ] = lists:droplast(L),
	M = lists:last(X),
	String = lists:map(fun (E) ->
		{char, [C]} = E,
		C
		end, lists:droplast(X)),
	[{literal, String}].

matched_Identifier(L)->
	spacing = lists:last(L),
	Id = lists:append(lists:droplast(L)),
	[{identifier, "'"++Id++"'"}].

matched_Primary(L) ->
	G = case L of
	[{identifier, Id}] ->
		{symbol, Id};
	[open, {expression, E}, close] ->
		E;
	[{literal, S}] ->
		{string, S};
	[{class, R}] ->
		{alt, R};
	[dot] ->
		{anychar};
	[O] -> O
	end,
	[{primary, G}].

matched_Suffix(L) ->
	G = case L of
	[{primary, P}, question]->
		{option, P};
	[{primary, P}, star]->
		{rep, P};
	[{primary, P}, plus]->
		{more, P};
	[{primary, P}]->
		P
	end,
	[{suffix, G}].

matched_Prefix(L) ->
	G = case L of
	['and', {suffix, P}]->
		{p_and, P};
	['not', {suffix, P}]->
		{p_not, P};
	[{suffix, P}] ->
		P
	end,
	[{prefix, G}].

matched_Sequence(L) ->
	G = case L of
	[{prefix, H}]->
		H;
	[{prefix, H} | T] ->
		A = lists:foldl(fun({prefix, E}, Acc)->
			Acc ++ [E]
		end, [H], T),
		{seq, A}
	end,
	[{sequence, G}].

matched_Expression(L) ->
	G = case L of
	[{sequence, H}] ->
		H;
	[{sequence, H} | T] ->
		A = lists:foldl(fun(E, Acc)->
			case E of
			slash ->
				Acc;
			{sequence, SE} ->
				Acc ++ [SE]
			end
		end, [H], T),
		{alt, A}
	end,
	[{expression, G}].

matched_Transformer(L) ->
	spacing = lists:last(L),
	[M | X ] = lists:droplast(L),
	M = "`" = lists:last(X),
	String = lists:map(fun (E) ->
		{char, [C]} = E,
		C
		end, lists:droplast(X)),
	[{transformer, String}].

matched_Definition(L) ->
	case L of
	[{identifier, Id}, larrow, {expression, E}, {transformer, T}] ->
		[{definition, Id, E, T}];
	[{identifier, Id}, larrow, {expression, E}] ->
		[{definition, Id, E}]
	end.

matched_Grammar(L) ->
	eof = lists:last(L),
	[spacing | Defs ] = lists:droplast(L),
	[{grammar, Defs}].
