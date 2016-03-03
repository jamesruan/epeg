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

c_tr(A, T) when is_function(T) ->
	fun (S) ->
		case A(S) of
		{ok, {I, Input, P}} ->
			{ok, {I, Input, T(P)}};
		Otherwise ->
			Otherwise
		end
	end.

c_empty() ->
	fun ({I, Input, _}) ->
		{ok, {I, Input, []}}
	end.

c_anychar() ->
	fun ({I, Input, _}) ->
		case I > length(Input) of
		true ->
			{fail, eof, {I, Input, []}};
		_ ->
			{ok, {I+1, Input, [[lists:nth(I, Input)]]}}
		end
	end.

c_char(C) ->
	fun ({I, Input, _})->
		case I > length(Input) of
		true ->
			{fail, eof, {I, Input, []}};
		_ ->
			case lists:nth(I, Input) of
			C ->
				{ok, {I+1, Input, [[C]]}};
			_ ->
				{fail, {mismatch, char, [C]}, {I, Input, []}}
			end
		end
	end.

c_charrange(F, T) when F < T ->
	fun ({I, Input, _})->
		case I > length(Input) of
		true ->
			{fail, eof, {I, Input, []}};
		_ ->
			C = lists:nth(I, Input),
			if
			C >= F , C =< T ->
				{ok, {I+1, Input, [[C]]}};
			true ->
				{fail, {mismatch, charrange, [F], [T]}, {I, Input, []}}
			end
		end
	end.

c_charclass(L) ->
	fun ({I, Input, _})->
		case I > length(Input) of
		true ->
			{fail, eof, {I, Input, []}};
		_ ->
			C = lists:nth(I, Input),
			case lists:member(C, L) of
			true ->
				{ok, {I+1, Input, [[C]]}};
			_ ->
				{fail, {mismatch, charclass, L}, {I, Input, []}}
			end
		end
	end.

c_string(S) ->
	fun ({I, Input, _}) ->
		case lists:sublist(Input, I, length(S)) of
		S ->
			{ok, {I+length(S), Input, [S]}};
		_ ->
			{fail, {mismatch, string, S}, {I, Input, []}}
		end
	end.


%([A], [B]) -> [A, B]
c_then(A, B) ->
	fun ({I, Input, _}) ->
		case A({I, Input, []}) of
		{ok, {Ia, _, Pa}} ->
			case B({Ia, Input, Pa}) of
			{ok, {Ib, _, Pb}} ->
				{ok, {Ib, Input, Pa ++ Pb}};
			{fail, Reason, _} ->
				{fail, Reason, {I, Input, []}}
			end;
		{fail, Reason, _} ->
			{fail, Reason, {I, Input, []}}
		end
	end.

%([A, B, ...]) -> [A, B, ...]
c_seq([H|[A|[]]]) -> c_then(H, A);
c_seq([H|[A|T]]) -> lists:foldl(fun (E, Acc) ->
	c_then(Acc, E) end
	, c_then(H, A), T).

% !
c_pred_not(A) ->
	fun ({I, Input, _}) ->
		case A({I, Input, []}) of
		{ok, _State} ->
			{fail, {mismatch, '!'}, {I, Input, []}};
		{fail, _R, _S} ->
			{ok, {I, Input, []}}
		end
	end.


% &
c_pred_and(A) ->
	fun ({I, Input, _}) ->
		case A({I, Input, []}) of
		{ok, _State} ->
			{ok, {I, Input, []}};
		{fail, _R, _S} ->
			{fail, {mismatch, '&'}, {I, Input, []}}
		end
	end.


% |
%([A], [B]) -> [A or B]
c_orelse(A, B) ->
	fun ({I, Input, _}) ->
		case A({I, Input, []}) of
		{ok, {Ia, _, Pa}} ->
			{ok, {Ia, Input, Pa}};
		{fail, _R, _S} ->
			case B({I, Input, []}) of
			{ok, {Ib, _, Pb}} ->
				{ok, {Ib, Input, Pb}};
			{fail, R, _S} ->
				{fail, R, {I, Input, []}}
			end
		end
	end.

%([A, B, ...]) -> [A or B or ...]
c_alt([H|[A|[]]]) -> c_orelse(H, A);
c_alt([H|[A|T]]) ->
	lists:foldl(fun (E, Acc) ->
	c_orelse(Acc, E) end,
	c_orelse(H, A), T).


%([A]) -> [A1, A2, ...]
% *
c_rep(A) ->
	fun R({I, Input, _}) ->
		case A({I, Input, []}) of
		{ok, {Io, _, Po}} ->
			{ok, {In, _, Pn}} = R({Io, Input, []}),
			{ok, {In, Input, Po ++ Pn}};
		{fail, _, _} ->
			{ok, {I, Input, []}}
		end
	end.

%([A]) -> [A, A1, ...]
% +
c_more(A) -> c_then(A, c_rep(A)).

% ?
c_option(A) -> c_orelse(A, c_empty()).

-ifdef(TRACE).
c_symbol_put(S, F) ->
	case get(S) of
	F -> F;
	_ ->
		R=fun({I, Input, _}) ->
			?LOG(I), ?LOG(S),
			V =  F({I, Input, []}),
			case V of
			{ok, {_, _, M}} ->
				?LOG({ok, S, M});
			_ ->
				pass
			end,
			V end,
		put(S, R),
		R
	end.
-else.
c_symbol_put(S, F) ->
	case get(S) of
	F -> F;
	_ ->
		put(S, F),
		F
	end.
-endif.

c_symbol_get(S) -> get(S).
