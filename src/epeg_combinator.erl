-module(epeg_combinator).

-include("epeg.hrl").

-export([
	c_empty/0, c_empty/1,
	c_anychar/0, c_anychar/1,
	c_char/1, c_char/2,
	c_charrange/2, c_charrange/3,
	c_charclass/1, c_charclass/2,
	c_string/1, c_string/2,
	c_then/2, c_then/3,
	c_seq/1, c_seq/2,
	c_pred_not/1, c_pred_not/2,
	c_pred_and/1, c_pred_and/2,
	c_orelse/2, c_orelse/3,
	c_alt/1, c_alt/2,
	c_rep/1, c_rep/2,
	c_more/1, c_more/2,
	c_option/1, c_option/2,
	c_symbol_put/2, c_symbol_get/1
	 ]).


-spec apply_transform(state(), transformer()) -> state().
apply_transform({I, P, Input}, T) when T =:= undefined ->
	{I, P, Input};
apply_transform({I, P, Input}, T) when is_function(T) ->
	{I, T(P), Input}.


-spec c_empty() -> parser().
c_empty() ->
	c_empty(undefined).

-spec c_empty(transformer()) -> parser().
c_empty(T) ->
	fun ({I, _P, Input}) ->
		{ok, apply_transform({I, [], Input}, T)}
	end.


-spec c_anychar() -> parser().
c_anychar() -> c_anychar(undefined).

-spec c_anychar(transformer()) -> parser().
c_anychar(T) ->
	fun ({I, _, Input}) ->
		case I > length(Input) of
		true ->
			{fail, eof, {I, [], Input}};
		_ ->
			{ok, apply_transform({I+1, [lists:nth(I, Input)], Input}, T)}
		end
	end.


-spec c_char(char()) -> parser().
c_char(C) -> c_char(C, undefined).

-spec c_char(char(), transformer()) -> parser().
c_char(C, T) ->
	fun ({I, _, Input})->
		case I > length(Input) of
		true ->
			{fail, eof, {I, [], Input}};
		_ ->
			case lists:nth(I, Input) of
			C ->
				{ok, apply_transform({I+1, [C], Input}, T)};
			_ ->
				{fail, mismatch, {I, [], Input}}
			end
		end
	end.

-spec c_charrange(char(), char()) -> parser().
c_charrange(F, T) -> c_charrange(F, T, undefined).

-spec c_charrange(char(), char(), transformer()) -> parser().
c_charrange(F, T, Tr) ->
	fun ({I, _, Input})->
		case I > length(Input) of
		true ->
			{fail, eof, {I, [], Input}};
		_ ->
			C = lists:nth(I, Input),
			if
			C >= F , C =< T ->
				{ok, apply_transform({I+1, [C], Input}, Tr)};
			true ->
				{fail, mismatch, {I, [], Input}}
			end
		end
	end.

-spec c_charclass([char()]) -> parser().
c_charclass(L) -> c_charclass(L, undefined).

-spec c_charclass([char()], transformer()) -> parser().
c_charclass(L, Tr) ->
	fun ({I, _, Input})->
		case I > length(Input) of
		true ->
			{fail, eof, {I, [], Input}};
		_ ->
			C = lists:nth(I, Input),
			case lists:member(C, L) of
			true ->
				{ok, apply_transform({I+1, [C], Input}, Tr)};
			_ ->
				{fail, mismatch, {I, [], Input}}
			end
		end
	end.

-spec c_string(string()) -> parser().
c_string(S) -> c_string(S, undefined).

-spec c_string(string(), transformer()) -> parser().
c_string(S, T) ->
	fun ({I, _, Input}) ->
		case lists:sublist(Input, I, length(S)) of
		S ->
			{ok, apply_transform({I+length(S), S, Input}, T)};
		_ ->
			{fail, mismatch, {I, [], Input}}
		end
	end.


-spec c_then(parser(), parser()) -> parser().
c_then(A, B) -> c_then(A, B, undefined).

-spec c_then(parser(), parser(), transformer()) -> parser().
c_then(A, B, T) ->
	fun ({I, _, Input}) ->
		case A({I, [], Input}) of
		{ok, {Ia, Pa, _}} ->
			case B({Ia, Pa, Input}) of
			{ok, {Ib, Pb, _}} ->
				if
				I =:= Ia, Pa =:= "" ->
					% A has not advance nor return
					{ok, apply_transform({Ib, Pb, Input}, T)};
				Ia =:= Ib, Pb =:= "" ->
					% B has not advance nor return
					{ok, apply_transform({Ia, Pa, Input}, T)};
				true ->
					{ok, apply_transform({Ib, [Pa] ++ [Pb], Input}, T)}
				end;
			{fail, Reason, _} ->
				{fail, Reason, {Ia, Pa, Input}}
			end;
		{fail, Reason, _} ->
			{fail, Reason, {I, [], Input}}
		end
	end.


delist(L)->
% [[a], b] -> [a, b]
	[H|T] = L,
	H ++ T.

comp(A, B) ->
	fun (C) ->
		B(A(C))
	end.

-spec c_seq([parser()]) -> parser().
c_seq([H|[A|[]]]) -> c_then(H, A);
c_seq([H|[A|T]]) -> c_seq([c_then(H, A)| T], fun delist/1).

-spec c_seq([parser()], transformer()) -> parser().
c_seq([H|[A|[]]], Tr) -> c_then(H, A, Tr);
c_seq([H|[A|T]], Tr) -> c_seq([c_then(H, A)|T], comp(fun delist/1, Tr)).


-spec c_pred_not(parser()) -> parser().
c_pred_not(A) -> c_pred_not(A, undefined).

-spec c_pred_not(parser(), transformer()) -> parser().
% !
c_pred_not(A, T) ->
	fun ({I, _, Input}) ->
		case A({I, [], Input}) of
		{ok, _State} ->
			{fail, pred_not, {I, [], Input}};
		{fail, _R, _S} ->
			F = c_empty(T),
			F({I, [], Input})
		end
	end.


-spec c_pred_and(parser()) -> parser().
c_pred_and(A) -> c_pred_and(A, undefined).

-spec c_pred_and(parser(), transformer()) -> parser().
% &
c_pred_and(A, T) ->
	fun ({I, _, Input}) ->
		case A({I, [], Input}) of
		{ok, _State} ->
			F = c_empty(T),
			F({I, [], Input});
		{fail, _R, _S} ->
			{fail, pred_not, {I, [], Input}}
		end
	end.


-spec c_orelse(parser(), parser()) -> parser().
c_orelse(A, B) -> c_orelse(A, B, undefined).

-spec c_orelse(parser(), parser(), transformer()) -> parser().
% |
c_orelse(A, B, T) ->
	fun ({I, _, Input}) ->
		case A({I, [], Input}) of
		{ok, State} ->
			{ok, apply_transform(State, T)};
		{fail, _R, _S} ->
			case B({I, [], Input}) of
			{ok, State} ->
				{ok, apply_transform(State, T)};
			{fail, R, _S} ->
				{fail, R, {I, [], Input}}
			end
		end
	end.


-spec c_alt([parser()]) -> parser().
c_alt([H|[A|[]]]) -> c_orelse(H, A);
c_alt([H|[A|T]]) -> c_alt([c_orelse(H, A)| T]).

-spec c_alt([parser()], transformer()) -> parser().
c_alt([H|[A|[]]], Tr) -> c_orelse(H, A, Tr);
c_alt([H|[A|T]], Tr) -> c_alt([c_orelse(H, A)| T], Tr).


-spec c_rep(parser()) -> parser().
% *
c_rep(A) -> c_rep(A, undefined).

-spec c_rep(parser(), transformer()) -> parser().
c_rep(A, T) ->
	fun ({Io, _, Input})->
		R = fun R(St) ->
			case A(St) of
			{ok, {I, P, Input}} ->
				case R({I, [], Input}) of
				{ok, {In, Pn, Input}} ->
					if
					is_list(P), is_list(Pn) ->
						{ok, {In, P ++ Pn, Input}};
					is_list(P) ->
						{ok, {In, P ++ [Pn], Input}};
					is_list(Pn) ->
						{ok, {In, [P] ++ Pn, Input}};
					true ->
						{ok, {In, [P] ++ [Pn], Input}}
					end;
				{fail, _R, Sn} ->
					{ok, Sn}
				end;
			{fail, _R, Sn} ->
				{ok, Sn}
			end
		end,
		% R find all that matched by A.
		case R({Io, [], Input}) of
		{ok, Sa} ->
			{ok, apply_transform(Sa, T)};
		Otherwise ->
			Otherwise
		end
	end.


-spec c_more(parser()) -> parser().
% +
c_more(A) -> c_then(A, c_rep(A), fun(L) ->
	case L of
	[H , T] ->
		if
		T =:= [] ->
			[H];
		true ->
			[H] ++ T
		end;
	H ->
		H
	end
	end).

-spec c_more(parser(), transformer()) -> parser().
c_more(A, Tr) -> c_then(A, c_rep(A), comp(fun(L) ->
	case L of
	[H , T] ->
		if
		T =:= [] ->
			[H];
		true ->
			[H] ++ T
		end;
	H ->
		H
	end
	end, Tr)).


-spec c_option(parser()) -> parser().
c_option(A) -> c_option(A, undefined).

-spec c_option(parser(), transformer()) -> parser().
% ?
c_option(A, T) -> c_orelse(A, c_empty(), T).

-spec c_symbol_put(atom(), parser()) -> parser().
c_symbol_put(S, F) ->
	case get(S) of
	F -> F;
	_ ->
		put(S, F),
		F
	end.

-spec c_symbol_get(atom()) -> parser().
c_symbol_get(S) -> get(S).
