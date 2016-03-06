%%% @author James Ruan <ruanbeihong@gmail.com>
%%% @doc This module provides combinators for constructing parsers.
%%%      Helping macros are defined to generate parsers:
%%%```
%%%  ?TR(A)     --> fun(State) -> epeg_combinator:c_tr(State, A) end
%%%  ?EMPTY()   --> fun(State) -> epeg_combinator:c_empty(State, []) end
%%%  ?ANYCHAR() --> fun(State) -> epeg_combinator:c_anychar(State, []) end
%%%  ?CHAR(A)   --> fun(State) -> epeg_combinator:c_char(State, A) end
%%%  ?CHARR(A)  --> fun(State) -> epeg_combinator:c_charrange(State, A) end
%%%  ?CHARC(A)  --> fun(State) -> epeg_combinator:c_charclass(State, A) end
%%%  ?STRING(A) --> fun(State) -> epeg_combinator:c_string(State, A) end
%%%  ?SEQ(A)    --> fun(State) -> epeg_combinator:c_seq(State, A) end
%%%  ?PAND(A)   --> fun(State) -> epeg_combinator:c_pred_and(State, A) end
%%%  ?PNOT(A)   --> fun(State) -> epeg_combinator:c_pred_not(State, A) end
%%%  ?ALT(A)    --> fun(State) -> epeg_combinator:c_alt(State, A) end
%%%  ?REP(A)    --> fun(State) -> epeg_combinator:c_rep(State, A) end
%%%  ?MORE(A)   --> fun(State) -> epeg_combinator:c_more(State, A) end
%%%  ?OPT(A)    --> fun(State) -> epeg_combinator:c_option(State, A) end
%%%'''
%%%      Notice that when using {@link c_tr/2}, the altered results should be warpped
%%%      in a list.
%%%
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
-spec c_tr(#state{}, {parser(), transformer()}) -> parsed_result().
-spec c_empty(#state{}, any()) -> parsed_result().
-spec c_anychar(#state{}, any()) -> parsed_result().
-spec c_char(#state{}, char()) -> parsed_result().
-spec c_charrange(#state{}, {char(), char()}) -> parsed_result().
-spec c_charclass(#state{}, string()) -> parsed_result().
-spec c_string(#state{}, string()) -> parsed_result().
-spec c_seq(#state{}, [parser()]) -> parsed_result().
-spec c_pred_not(#state{}, parser()) -> parsed_result().
-spec c_pred_and(#state{}, parser()) -> parsed_result().
-spec c_alt(#state{}, [parser()]) -> parsed_result().
-spec c_rep(#state{}, parser()) -> parsed_result().
-spec c_more(#state{}, parser()) -> parsed_result().
-spec c_option(#state{}, parser()) -> parsed_result().

% @doc Translate parsed result form parser P with transformer Tr.
% @spec c_tr(#state{}, {P :: parser(), Tr :: transformer()}) -> parsed_result()
c_tr(S, {A, T}) when is_function(T) ->
	case A(S) of
	{ok, #state{index=I, input=Input, parsed=P}} ->
		{ok, #state{index=I, input=Input, parsed=T(P)}};
	Otherwise ->
		Otherwise
	end.

% @doc Lift a #state{} to parsed_result().
% @spec c_empty(#state{}, any()) -> parsed_result()
c_empty(S, _) ->
	{ok, S}.

% @doc Parse any character.
% @spec c_anychar(#state{}, any()) -> parsed_result()
c_anychar(#state{index=I, input=Input}, _) when I > length(Input) ->
	{fail, eof, #state{index=I, input=Input}};
c_anychar(#state{index=I, input=Input}, _) ->
	{ok, #state{index=I+1, input=Input, parsed=[[lists:nth(I, Input)]]}}.

% @doc Parse a character C.
% @spec c_char(#state{}, C::char()) -> parsed_result()
c_char(#state{index=I, input=Input}, _) when I > length(Input) ->
	{fail, eof, #state{index=I, input=Input}};
c_char(#state{index=I, input=Input}, C) ->
	case lists:nth(I, Input) of
	C ->
		{ok, #state{index=I+1, input=Input, parsed=[[C]]}};
	_ ->
		{fail, {mismatch, [char, [C]]}, #state{index=I, input=Input}}
	end.

% @doc Parse a character in range from F to T when F smaller then T.
% @spec c_charrange(#state{}, {F :: char(), T :: char()}) -> parsed_result()
c_charrange(#state{index=I, input=Input}, _) when I > length(Input) ->
	{fail, eof, #state{index=I, input=Input}};
c_charrange(#state{index=I, input=Input}, {F, T}) when F < T ->
	C = lists:nth(I, Input),
	if
	C >= F , C =< T ->
		{ok, #state{index=I+1, input=Input, parsed=[[C]]}};
	true ->
		{fail, {mismatch, [charrange, [F], [T]]}, #state{index=I, input=Input}}
	end.

% @doc Parse a character in a list L.
% @spec c_charclass(#state{}, L :: string()) -> parsed_result()
c_charclass(#state{index=I, input=Input}, _) when I > length(Input) ->
	{fail, eof, #state{index=I, input=Input}};
c_charclass(#state{index=I, input=Input}, L) ->
	C = lists:nth(I, Input),
	case lists:member(C, L) of
	true ->
		{ok, #state{index=I+1, input=Input, parsed=[[C]]}};
	_ ->
		{fail, {mismatch, [charclass, L]}, #state{index=I, input=Input}}
	end.

% @doc Parse a string S.
% @spec c_string(#state{}, S :: string()) -> parsed_result()
c_string(#state{index=I, input=Input}, S) when I + length(S)-1 > length(Input) ->
	{fail, eof, #state{index=I, input=Input}};
c_string(#state{index=I, input=Input}, S) ->
	case lists:sublist(Input, I, length(S)) of
	S ->
		{ok, #state{index=I+length(S), input=Input, parsed=[S]}};
	_ ->
		{fail, {mismatch, [string, S]}, #state{index=I, input=Input}}
	end.

% @doc Parse with a series of parser L, success when ALL of them parsed.
% @spec c_seq(#state{}, L :: [parser()]) -> parsed_result()
c_seq(#state{index=I, input=Input, parsed=[]}, [H|[]]) ->
	H(#state{index=I, input=Input});
c_seq(#state{index=I, input=Input, parsed=[]}, [H|T]) ->
	c_seq(#state{index=I, input=Input, parsed={I, []}}, [H|T]);
c_seq(#state{index=I, input=Input, parsed={Bt, Acc}}, [H|[]]) ->
	case H(#state{index=I, input=Input}) of
	{ok, #state{index=Ih, parsed=Ph}} ->
		{ok, #state{index=Ih, input=Input, parsed=Acc ++ Ph}};
	{fail, Reason, _} ->
		{fail, Reason, #state{index=Bt, input=Input}}
	end;
c_seq(#state{index=I, input=Input, parsed={Bt, Acc}}, [H|T]) ->
	case H(#state{index=I, input=Input}) of
	{ok, #state{index=Ih, parsed=Ph}} ->
		c_seq(#state{index=Ih, input=Input, parsed={Bt, Acc ++ Ph}}, T);
	{fail, Reason, _} ->
		{fail, Reason, #state{index=Bt, input=Input}}
	end.

% @doc Parse with parser P but not advance, success when parsing failed.
% @spec c_pred_not(#state{}, P :: parser()) -> parsed_result()
c_pred_not(#state{index=I, input=Input}, A) ->
	case A(#state{index=I, input=Input}) of
	{ok, _State} ->
		{fail, {mismatch, ['!']}, #state{index=I, input=Input}};
	{fail, _R, _S} ->
		{ok, #state{index=I, input=Input}}
	end.


% @doc Parse with parser P but not advance, success when parsing successed.
% @spec c_pred_and(#state{}, P :: parser()) -> parsed_result()
c_pred_and(#state{index=I, input=Input}, A) ->
	case A(#state{index=I, input=Input}) of
	{ok, _State} ->
		{ok, #state{index=I, input=Input}};
	{fail, _R, _S} ->
		{fail, {mismatch, ['!']}, #state{index=I, input=Input}}
	end.

% @doc Parse with a series of parser L, success when FIRST of them parsed.
% @spec c_alt(#state{}, L :: [parser()]) -> parsed_result()
c_alt(S, [H|[]]) -> H(S);
c_alt(#state{index=I, input=Input}, [H|T]) ->
	case H(#state{index=I, input=Input}) of
	{ok, #state{index=Ih, parsed=Ph}} ->
		{ok, #state{index=Ih, input=Input, parsed=Ph}};
	{fail, _Reason, _} ->
		c_alt(#state{index=I, input=Input}, T)
	end.

% @doc Parse with parser P repeatively, stop when first failed, never fail.
%      Result contains zero or more parsed results.
% @end
% @spec c_rep(#state{}, P :: parser()) -> parsed_result()
c_rep(#state{index=I, input=Input, parsed=Acc}, A) ->
	case A(#state{index=I, input=Input}) of
	{ok, #state{index=Ih, parsed=Ph}} ->
		c_rep(#state{index=Ih, input=Input, parsed=Acc ++ Ph}, A);
	{fail, _, _} ->
		{ok, #state{index=I, input=Input, parsed=Acc}}
	end.

% @doc Parse with parser P repeatively, stop when first failed, fail when no result.
%      Result contains one or more parsed results.
% @end
% @spec c_more(#state{}, parser()) -> parsed_result()
c_more(#state{index=I, input=Input}, A) ->
	c_seq(#state{index=I, input=Input}, [A, fun(Sf) -> c_rep(Sf, A) end]).

% @doc Parse with parser P, never fail.
%      Result contains zero or one parsed results.
% @end
% @spec c_option(#state{}, parser()) -> parsed_result()
c_option(#state{index=I, input=Input}, A) ->
	c_alt(#state{index=I, input=Input}, [A, fun(Sf) -> c_empty(Sf, []) end]).

