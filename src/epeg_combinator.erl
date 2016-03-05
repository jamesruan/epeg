% @author James Ruan <ruanbeihong@gmail.com>
% @doc This module provides combinators for constructing parsers.
%      Helping macros are defined to generate parsers:
%```
%  ?TR(A)     --> fun(State) -> epeg_combinator:c_tr(State, A) end
%  ?EMPTY()   --> fun(State) -> epeg_combinator:c_empty(State, []) end
%  ?ANYCHAR() --> fun(State) -> epeg_combinator:c_anychar(State, []) end
%  ?CHAR(A)   --> fun(State) -> epeg_combinator:c_char(State, A) end
%  ?CHARR(A)  --> fun(State) -> epeg_combinator:c_charrange(State, A) end
%  ?CHARC(A)  --> fun(State) -> epeg_combinator:c_charclass(State, A) end
%  ?STRING(A) --> fun(State) -> epeg_combinator:c_string(State, A) end
%  ?SEQ(A)    --> fun(State) -> epeg_combinator:c_seq(State, A) end
%  ?PAND(A)   --> fun(State) -> epeg_combinator:c_pred_and(State, A) end
%  ?PNOT(A)   --> fun(State) -> epeg_combinator:c_pred_not(State, A) end
%  ?ALT(A)    --> fun(State) -> epeg_combinator:c_alt(State, A) end
%  ?REP(A)    --> fun(State) -> epeg_combinator:c_rep(State, A) end
%  ?MORE(A)   --> fun(State) -> epeg_combinator:c_more(State, A) end
%  ?OPT(A)    --> fun(State) -> epeg_combinator:c_option(State, A) end
%'''
%      Notice that when using {@link c_tr/2}, the altered results should be warpped
%      in a list.
-module(epeg_combinator).

-include("epeg.hrl").

-export([
	c_tr/2,
	c_empty/2,
	c_anychar/2,
	c_char/2,
	c_charrange/2,
	c_charclass/2,
	c_string/2,
	c_seq/2,
	c_pred_not/2,
	c_pred_and/2,
	c_alt/2,
	c_rep/2,
	c_more/2,
	c_option/2
	]).

% @type index() = pos_integer().
% @type input() = string().
% @type state() = {index(), input(), ([] | {index(), []})}.
% @type transformer() = fun(([]) -> []).
% @type fail_reason() = {mismatch, list()} | eof.
% @type result(S) = {ok, S} | {fail, fail_reason(), S}.
% @type parsed_result() = result(state()).
% @type parser() = fun((state()) -> parsed_result()).

% @doc Translate parsed result form parser P with transformer Tr.
% @spec c_tr(state(), {P :: parser(), Tr :: transformer()}) -> parsed_result()
c_tr(S, {A, T}) when is_function(T) ->
	case A(S) of
	{ok, {I, Input, P}} ->
		{ok, {I, Input, T(P)}};
	Otherwise ->
		Otherwise
	end.

% @doc Lift a state() to parsed_result().
% @spec c_empty(state(), any()) -> parsed_result()
c_empty(S, _) ->
	{ok, S}.

% @doc Parse any character.
% @spec c_anychar(state(), any()) -> parsed_result()
c_anychar({I, Input, _}, _) when I > length(Input) ->
	{fail, eof, {I, Input, []}};
c_anychar({I, Input, _}, _) ->
	{ok, {I+1, Input, [[lists:nth(I, Input)]]}}.

% @doc Parse a character C.
% @spec c_char(state(), C::char()) -> parsed_result()
c_char({I, Input, _}, _) when I > length(Input) ->
	{fail, eof, {I, Input, []}};
c_char({I, Input, _}, C) ->
	case lists:nth(I, Input) of
	C ->
		{ok, {I+1, Input, [[C]]}};
	_ ->
		{fail, {mismatch, [char, [C]]}, {I, Input, []}}
	end.

% @doc Parse a character in range from F to T when F smaller then T.
% @spec c_charrange(state(), {F :: char(), T :: char()}) -> parsed_result()
c_charrange({I, Input, _}, _) when I > length(Input) ->
	{fail, eof, {I, Input, []}};
c_charrange({I, Input, _}, {F, T}) when F < T ->
	C = lists:nth(I, Input),
	if
	C >= F , C =< T ->
		{ok, {I+1, Input, [[C]]}};
	true ->
		{fail, {mismatch, [charrange, [F], [T]]}, {I, Input, []}}
	end.

% @doc Parse a character in a list L.
% @spec c_charclass(state(), L :: string()) -> parsed_result()
c_charclass({I, Input, _}, _) when I > length(Input) ->
	{fail, eof, {I, Input, []}};
c_charclass({I, Input, _}, L) ->
	C = lists:nth(I, Input),
	case lists:member(C, L) of
	true ->
		{ok, {I+1, Input, [[C]]}};
	_ ->
		{fail, {mismatch, [charclass, L]}, {I, Input, []}}
	end.

% @doc Parse a string S.
% @spec c_string(state(), S :: string()) -> parsed_result()
c_string({I, Input, _}, S) when I + length(S)-1 > length(Input) ->
	{fail, eof, {I, Input, []}};
c_string({I, Input, _}, S) ->
	case lists:sublist(Input, I, length(S)) of
	S ->
		{ok, {I+length(S), Input, [S]}};
	_ ->
		{fail, {mismatch, [string, S]}, {I, Input, []}}
	end.

% @doc Parse with a series of parser L, success when ALL of them parsed.
% @spec c_seq(state(), L :: [parser()]) -> parsed_result()
c_seq({I, Input, []}, [H|[]]) -> H({I, Input, []});
c_seq({I, Input, []}, [H|T]) -> c_seq({I, Input, {I, []}}, [H|T]);
c_seq({I, Input, {Bt, Acc}}, [H|[]]) ->
	case H({I, Input, []}) of
	{ok, {Ih, _, Ph}} ->
		{ok, {Ih, Input, Acc ++ Ph}};
	{fail, Reason, _} ->
		{fail, Reason, {Bt, Input, []}}
	end;
c_seq({I, Input, {Bt, Acc}}, [H|T]) ->
	case H({I, Input, []}) of
	{ok, {Ih, _, Ph}} ->
		c_seq({Ih, Input, {Bt, Acc ++ Ph}}, T);
	{fail, Reason, _} ->
		{fail, Reason, {Bt, Input, []}}
	end.

% @doc Parse with parser P but not advance, success when parsing failed.
% @spec c_pred_not(state(), P :: parser()) -> parsed_result()
c_pred_not({I, Input, _}, A) ->
	case A({I, Input, []}) of
	{ok, _State} ->
		{fail, {mismatch, ['!']}, {I, Input, []}};
	{fail, _R, _S} ->
		{ok, {I, Input, []}}
	end.


% @doc Parse with parser P but not advance, success when parsing successed.
% @spec c_pred_and(state(), P :: parser()) -> parsed_result()
c_pred_and({I, Input, _}, A) ->
	case A({I, Input, []}) of
	{ok, _State} ->
		{ok, {I, Input, []}};
	{fail, _R, _S} ->
		{fail, {mismatch, ['!']}, {I, Input, []}}
	end.

% @doc Parse with a series of parser L, success when FIRST of them parsed.
% @spec c_alt(state(), L :: [parser()]) -> parsed_result()
c_alt(S, [H|[]]) -> H(S);
c_alt({I, Input, _}, [H|T]) ->
	case H({I, Input, []}) of
	{ok, {Ih, _, Ph}} ->
		{ok, {Ih, Input, Ph}};
	{fail, _Reason, _} ->
		c_alt({I, Input, []}, T)
	end.

% @doc Parse with parser P repeatively, stop when first failed, never fail.
%      Result contains zero or more parsed results.
% @end
% @spec c_rep(state(), P :: parser()) -> parsed_result()
c_rep({I, Input, Acc}, A) ->
	case A({I, Input, []}) of
	{ok, {Ih, _, Ph}} ->
		c_rep({Ih, Input, Acc ++ Ph}, A);
	{fail, _, _} ->
		{ok, {I, Input, Acc}}
	end.

% @doc Parse with parser P repeatively, stop when first failed, fail when no result.
%      Result contains one or more parsed results.
% @end
% @spec c_more(state(), parser()) -> parsed_result()
c_more(S, A) -> c_seq(S, [A, fun(Sf) -> c_rep(Sf, A) end]).

% @doc Parse with parser P, never fail.
%      Result contains zero or one parsed results.
% @end
% @spec c_option(state(), parser()) -> parsed_result()
c_option(S, A) -> c_alt(S, [A, fun(Sf) -> c_empty(Sf, []) end]).

