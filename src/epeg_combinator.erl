-module(epeg_combinator).

-include("epeg.hrl").

-export([
	c_tr/2,
	c_empty/0,
	c_anychar/0,
	c_char/1,
	c_charrange/2,
	c_charclass/1,
	c_string/1,
	c_seq/1,
	c_pred_not/1,
	c_pred_and/1,
	c_alt/1,
	c_rep/1,
	c_more/1,
	c_option/1,
	c_symbol_put/2,
	c_symbol_get/1
	]).


-spec c_tr(parser(), transformer()) -> parser().
c_tr(A, T) when is_function(T) ->
	fun (S) ->
		case A(S) of
		{ok, {I, P, Input}} ->
			{ok, {I, T(P), Input}};
		Otherwise ->
			Otherwise
		end
	end.

-spec c_empty() -> parser().
c_empty() ->
	fun ({I, _P, Input}) ->
		{ok, {I, [], Input}}
	end.

-spec c_anychar() -> parser().
c_anychar() ->
	fun ({I, _, Input}) ->
		case I > length(Input) of
		true ->
			{fail, eof, {I, [], Input}};
		_ ->
			{ok, {I+1, [[lists:nth(I, Input)]], Input}}
		end
	end.

-spec c_char(char()) -> parser().
c_char(C) ->
	fun ({I, _, Input})->
		case I > length(Input) of
		true ->
			{fail, eof, {I, [], Input}};
		_ ->
			case lists:nth(I, Input) of
			C ->
				{ok, {I+1, [[C]], Input}};
			_ ->
				{fail, {mismatch, char, [C]}, {I, [], Input}}
			end
		end
	end.

-spec c_charrange(char(), char()) -> parser().
c_charrange(F, T) when F < T ->
	fun ({I, _, Input})->
		case I > length(Input) of
		true ->
			{fail, eof, {I, [], Input}};
		_ ->
			C = lists:nth(I, Input),
			if
			C >= F , C =< T ->
				{ok, {I+1, [[C]], Input}};
			true ->
				{fail, {mismatch, charrange, [F], [T]}, {I, [], Input}}
			end
		end
	end.

-spec c_charclass(string()) -> parser().
c_charclass(L) ->
	fun ({I, _, Input})->
		case I > length(Input) of
		true ->
			{fail, eof, {I, [], Input}};
		_ ->
			C = lists:nth(I, Input),
			case lists:member(C, L) of
			true ->
				{ok, {I+1, [[C]], Input}};
			_ ->
				{fail, {mismatch, charclass, L}, {I, [], Input}}
			end
		end
	end.

-spec c_string(string()) -> parser().
c_string(S) ->
	fun ({I, _, Input}) ->
		case lists:sublist(Input, I, length(S)) of
		S ->
			{ok, {I+length(S), [S], Input}};
		_ ->
			{fail, {mismatch, string, S}, {I, [], Input}}
		end
	end.


-spec c_then(parser(), parser()) -> parser().
%([A], [B]) -> [A, B]
c_then(A, B) ->
	fun ({I, _, Input}) ->
		case A({I, [], Input}) of
		{ok, {Ia, Pa, _}} ->
			case B({Ia, Pa, Input}) of
			{ok, {Ib, Pb, _}} ->
				{ok, {Ib, Pa ++ Pb, Input}};
			{fail, Reason, _} ->
				{fail, Reason, {I, [], Input}}
			end;
		{fail, Reason, _} ->
			{fail, Reason, {I, [], Input}}
		end
	end.

-spec c_seq([parser()]) -> parser().
%([A, B, ...]) -> [A, B, ...]
c_seq([H|[A|[]]]) -> c_then(H, A);
c_seq([H|[A|T]]) -> lists:foldl(fun (E, Acc) ->
	c_then(Acc, E) end
	, c_then(H, A), T).

-spec c_pred_not(parser()) -> parser().
% !
c_pred_not(A) ->
	fun ({I, _, Input}) ->
		case A({I, [], Input}) of
		{ok, _State} ->
			{fail, {mismatch, '!'}, {I, [], Input}};
		{fail, _R, _S} ->
			{ok, {I, [], Input}}
		end
	end.


-spec c_pred_and(parser()) -> parser().
% &
c_pred_and(A) ->
	fun ({I, _, Input}) ->
		case A({I, [], Input}) of
		{ok, _State} ->
			{ok, {I, [], Input}};
		{fail, _R, _S} ->
			{fail, {mismatch, '&'}, {I, [], Input}}
		end
	end.


-spec c_orelse(parser(), parser()) -> parser().
% |
%([A], [B]) -> [A or B]
c_orelse(A, B) ->
	fun ({I, _, Input}) ->
		case A({I, [], Input}) of
		{ok, {Ia, Pa, _}} ->
			{ok, {Ia, Pa, Input}};
		{fail, _R, _S} ->
			case B({I, [], Input}) of
			{ok, {Ib, Pb, _}} ->
				{ok, {Ib, Pb, Input}};
			{fail, R, _S} ->
				{fail, R, {I, [], Input}}
			end
		end
	end.

-spec c_alt([parser()]) -> parser().
%([A, B, ...]) -> [A or B or ...]
c_alt([H|[A|[]]]) -> c_orelse(H, A);
c_alt([H|[A|T]]) ->
	lists:foldl(fun (E, Acc) ->
	c_orelse(Acc, E) end,
	c_orelse(H, A), T).


-spec c_rep(parser()) -> parser().
%([A]) -> [A1, A2, ...]
% *
c_rep(A) ->
	fun R({I, _, Input}) ->
		case A({I, [], Input}) of
		{ok, {Io, Po, _}} ->
			{ok, {In, Pn, _}} = R({Io, [], Input}),
			{ok, {In, Po ++ Pn, Input}};
		{fail, _, _} ->
			{ok, {I, [], Input}}
		end
	end.

-spec c_more(parser()) -> parser().
%([A]) -> [A, A1, ...]
% +
c_more(A) -> c_then(A, c_rep(A)).

-spec c_option(parser()) -> parser().
% ?
c_option(A) -> c_orelse(A, c_empty()).

-ifdef(TRACE).
-spec c_symbol_put(atom(), parser()) -> parser().
c_symbol_put(S, F) ->
	case get(S) of
	F -> F;
	_ ->
		R=fun({I, _, Input}) ->
			?LOG(I), ?LOG(S),
			V =  F({I, [], Input}),
			case V of
			{ok, {_, M, _}} ->
				?LOG({ok, S, M});
			_ ->
				pass
			end,
			V end,
		put(S, R),
		R
	end.
-else.
-spec c_symbol_put(atom(), parser()) -> parser().
c_symbol_put(S, F) ->
	case get(S) of
	F -> F;
	_ ->
		put(S, F),
		F
	end.
-endif.

-spec c_symbol_get(atom()) -> parser().
c_symbol_get(S) -> get(S).
