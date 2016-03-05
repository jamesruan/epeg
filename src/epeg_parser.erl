-module(epeg_parser).
-compile(nowarn_shadow_vars).
-include_lib("eunit/include/eunit.hrl").

-include("epeg_parser.hrl").

-export([grammar/1]).
-define(SYM(S), grammar(S)).

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

% @doc Return parsers with symbol S.
% @spec grammar(S :: atom()) -> parser()
-spec grammar(S :: atom()) -> parser().
grammar('_EOF') ->
	?TR({?PNOT(?ANYCHAR()),
	fun matched_EOF/1});
grammar('_EOL') ->
	?TR({?ALT([?STRING("\r\n"), ?CHAR($\n), ?CHAR($\r)]),
	fun matched_EOL/1});
grammar('_Space') ->
	?TR({?ALT([?CHAR($\ ), ?CHAR($\t), ?SYM('_EOL')]),
	fun matched_Space/1});
grammar('_Comment') ->
	?TR({?SEQ([?CHAR($%), ?REP(?SEQ([?PNOT(?SYM('_EOL')), ?ANYCHAR()])), ?SYM('_EOL')]),
	fun matched_Comment/1});
grammar('_Spacing') ->
	?TR({?REP(?ALT([?SYM('_Space'), ?SYM('_Comment')])),
	fun matched_Spacing/1});
grammar('_DOT') ->
	?TR({?SEQ([?CHAR($.), ?SYM('_Spacing')]),
	fun matched_DOT/1});
grammar('_CLOSE') ->
	?TR({?SEQ([?CHAR($)), ?SYM('_Spacing')]),
	fun matched_CLOSE/1});
grammar('_OPEN') ->
	?TR({?SEQ([?CHAR($(), ?SYM('_Spacing')]),
	fun matched_OPEN/1});
grammar('_PLUS') ->
	?TR({?SEQ([?CHAR($+), ?SYM('_Spacing')]),
	fun matched_PLUS/1});
grammar('_STAR') ->
	?TR({?SEQ([?CHAR($*), ?SYM('_Spacing')]),
	fun matched_STAR/1});
grammar('_QUESTION') ->
	?TR({?SEQ([?CHAR($?), ?SYM('_Spacing')]),
	fun matched_QUESTION/1});
grammar('_NOT') ->
	?TR({?SEQ([?CHAR($!), ?SYM('_Spacing')]),
	fun matched_NOT/1});
grammar('_AND') ->
	?TR({?SEQ([?CHAR($&), ?SYM('_Spacing')]),
	fun matched_AND/1});
grammar('_SLASH') ->
	?TR({?SEQ([?CHAR($/), ?SYM('_Spacing')]),
	fun matched_SLASH/1});
grammar('_LARROW') ->
	?TR({?SEQ([?STRING("<-"), ?SYM('_Spacing')]),
	fun matched_LARROW/1});
grammar('_Char') ->
	?TR({?ALT([
		?SEQ([?CHAR($\\), ?CHARC("rnt[`]\\\"\'")]),
		?SEQ([?PNOT(?CHAR($\\)), ?CHARR({0, 16#10ffff})])]),
	fun matched_Char/1});
grammar('_Range') ->
	?TR({?ALT([
		?SEQ([?SYM('_Char'), ?CHAR($-), ?SYM('_Char')]),
		?SYM('_Char')]),
	fun matched_Range/1});
grammar('_Class') ->
	?TR({?SEQ([?CHAR($[),
		?REP(?SEQ([?PNOT(?CHAR($])), ?SYM('_Range')])),
		?CHAR($]), ?SYM('_Spacing')]),
	fun matched_Class/1});
grammar('_Literal') ->
	?TR({?ALT([
		?SEQ([?CHAR($'),
		?REP(?SEQ([?PNOT(?CHAR($')), ?SYM('_Char')])),
		?CHAR($'), ?SYM('_Spacing')]),
		?SEQ([?CHAR($"),
		?REP(?SEQ([?PNOT(?CHAR($")), ?SYM('_Char')])),
		?CHAR($"), ?SYM('_Spacing')])]),
	fun matched_Literal/1});
grammar('_IdentStart') ->
	?ALT([?CHAR($_), ?CHARR({$a, $z}), ?CHARR({$A, $Z})]);
grammar('_IdentCont') ->
	?ALT([?SYM('_IdentStart'), ?CHARR({$0, $9})]);
grammar('_Identifier') ->
	?TR({?SEQ([
		?SYM('_IdentStart'),
		?REP(?SYM('_IdentCont')),
		?SYM('_Spacing')]),
	fun matched_Identifier/1});
grammar('_Primary') ->
	?TR({?ALT([
		?SEQ([?SYM('_Identifier'), ?PNOT(?SYM('_LARROW'))]),
		?SEQ([?SYM('_OPEN'), ?SYM('_Expression'),  ?SYM('_CLOSE')]),
		?SYM('_Literal'),
		?SYM('_Class'),
		?SYM('_DOT')]),
	fun matched_Primary/1});
grammar('_Suffix') ->
	?TR({?SEQ([?SYM('_Primary'),
		?OPT(?ALT([?SYM('_QUESTION'), ?SYM('_STAR'), ?SYM('_PLUS')]))]),
	fun matched_Suffix/1});
grammar('_Prefix') ->
	?TR({?SEQ([?OPT(?ALT([?SYM('_AND'), ?SYM('_NOT')])), ?SYM('_Suffix')]),
	fun matched_Prefix/1});
grammar('_Sequence') ->
	?TR({?MORE(?SYM('_Prefix')),
	fun matched_Sequence/1});
grammar('_Expression') ->
	?TR({?SEQ([?SYM('_Sequence'),
		?REP(?SEQ([?SYM('_SLASH'), ?SYM('_Sequence')]))]),
	fun matched_Expression/1});
grammar('_Transformer') ->
	?TR({?SEQ([?CHAR($`),
		?REP(?SEQ([?PNOT(?CHAR($`)), ?SYM('_Char')])),
		?CHAR($`), ?SYM('_Spacing')]),
	fun matched_Transformer/1});
grammar('_Definition') ->
	?TR({?SEQ([
		?SYM('_Identifier'),
		?SYM('_LARROW'),
		?SYM('_Expression'),
		?OPT(?SYM('_Transformer'))]),
	fun matched_Definition/1});
grammar('_Grammar') ->
	?TR({?SEQ([
		?SYM('_Spacing'),
		?MORE(?SYM('_Definition')),
		?SYM('_EOF')]),
	fun matched_Grammar/1}).

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
