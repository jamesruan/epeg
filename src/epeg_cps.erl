% @author James Ruan <ruanbeihong@gmail.com>
% @doc This module provides combinators for constructing CPS parsers.
%      Helping macros are defined to generate CPS parsers:
%```
%  ?TR(A)     --> fun(S, _, K) -> epeg_cps:cps_tr(S, A, K) end
%  ?EMPTY()   --> fun(S, _, K) -> epeg_cps:cps_anychar(S, [], K) end
%  ?ANYCHAR() --> fun(S, _, K) -> epeg_cps:cps_anychar(S, [], K) end
%  ?CHAR(A)   --> fun(S, _, K) -> epeg_cps:cps_char(S, A, K) end
%  ?CHARR(A)  --> fun(S, _, K) -> epeg_cps:cps_charrange(S, A, K) end
%  ?CHARC(A)  --> fun(S, _, K) -> epeg_cps:cps_charclass(S, A, K) end
%  ?STRING(A) --> fun(S, _, K) -> epeg_cps:cps_string(S, A, K) end
%  ?SEQ(A)    --> fun(S, _, K) -> epeg_cps:cps_seq(S, A, K) end
%  ?PAND(A)   --> fun(S, _, K) -> epeg_cps:cps_pred_and(S, A, K) end
%  ?PNOT(A)   --> fun(S, _, K) -> epeg_cps:cps_pred_not(S, A, K) end
%  ?ALT(A)    --> fun(S, _, K) -> epeg_cps:cps_alt(S, A, K) end
%  ?REP(A)    --> fun(S, _, K) -> epeg_cps:cps_rep(S, A, K) end
%  ?MORE(A)   --> fun(S, _, K) -> epeg_cps:cps_more(S, A, K) end
%  ?OPT(A)    --> fun(S, _, K) -> epeg_cps:cps_option(S, A, K) end
%'''
%      Notice that when using {@link cps_tr/3}, the altered results should be warpped
%      in a list.
-module(epeg_cps).
-include("epeg.hrl").

-export([
	 return/1
	,cps_tr/3
	,cps_empty/3
	,cps_anychar/3
	,cps_char/3
	,cps_charrange/3
	,cps_charclass/3
	,cps_string/3
	,cps_seq/3
	,cps_pred_not/3
	,cps_pred_and/3
	,cps_alt/3
	,cps_rep/3
	,cps_more/3
	,cps_option/3
	]).

% @type index() = pos_integer().
% @type input() = string().
% @type state() = {index(), input(), ([] | {index(), []})}.
% @type transformer() = fun(([]) -> []).
% @type fail_reason() = {mismatch, list()} | eof.
% @type result(S) = {ok, S} | {fail, fail_reason(), S}.
% @type parsed_result() = result(state()).
% @type parser_continuation() = fun((parsed_result()) -> parsed_result()).
% @type cps_parser() = fun((state(), any(), parser_continuation()) -> parsed_result()).

% @doc Used as terminator of CPS chain.
return(X) -> X.

% @doc Translate parsed result form parser P with transformer Tr.
% @spec cps_tr(state(), {P :: cps_parser(), T :: transformer()}, parser_continuation()) -> parsed_result()
cps_tr({I, Input, _}, {A, T}, K) when is_function(T) ->
	A({I, Input, []}, [], fun(R) ->
		case R of
		{ok, {Ia, Input, Pa}} ->
			K({ok, {Ia, Input, T(Pa)}});
		_ ->
			K(R)
		end
	end).

% @doc Lift a state() to parsed_result().
% @spec cps_empty(state(), any(), parser_continuation()) -> parsed_result()
cps_empty(S, _, K) ->
	K({ok, S}).

% @doc Parse any character.
% @spec cps_anychar(state(), any(), parser_continuation()) -> parsed_result()
cps_anychar({I, Input, _}, _, K) when I > length(Input) ->
	K({fail, eof, {I, Input, []}});
cps_anychar({I, Input, _}, _, K) ->
	K({ok, {I+1, Input, [[lists:nth(I, Input)]]}}).

% @doc Parse a character C.
% @spec cps_char(state(), C :: char(), parser_continuation()) -> parsed_result()
cps_char({I, Input, _}, _, K) when I > length(Input) ->
	K({fail, eof, {I, Input, []}});
cps_char({I, Input, _}, C, K) ->
	PR = case lists:nth(I, Input) of
	C ->
		{ok, {I+1, Input, [[C]]}};
	_ ->
		{fail, {mismatch, char, [C]}, {I, Input, []}}
	end,
	K(PR).

% @doc Parse a character in range from F to T when F smaller then T.
% @spec cps_charrange(state(), {F :: char(), T :: char()}, parser_continuation()) -> parsed_result()
cps_charrange({I, Input, _}, _, K) when I > length(Input) ->
	K({fail, eof, {I, Input, []}});
cps_charrange({I, Input, _}, {F, T}, K) when F < T->
	C = lists:nth(I, Input),
	PR = if
	C >= F , C =< T ->
		{ok, {I+1, Input, [[C]]}};
	true ->
		{fail, {mismatch, charrange, [F], [T]}, {I, Input, []}}
	end,
	K(PR).

% @doc Parse a character in a list L.
% @spec cps_charclass(state(), string(), parser_continuation()) -> parsed_result()
cps_charclass({I, Input, _}, _, K) when I > length(Input) ->
	K({fail, eof, {I, Input, []}});
cps_charclass({I, Input, _}, L, K) ->
	C = lists:nth(I, Input),
	PR = case lists:member(C, L) of
	true ->
		{ok, {I+1, Input, [[C]]}};
	_ ->
		{fail, {mismatch, charclass, L}, {I, Input, []}}
	end,
	K(PR).

% @doc Parse a string S.
% @spec cps_string(state(), S :: string(), parser_continuation()) -> parsed_result()
cps_string({I, Input, _}, S, K) when I + length(S) -1 > length(Input) ->
	K({fail, eof, {I, Input, []}});
cps_string({I, Input, _}, S, K) ->
	case lists:sublist(Input, I, length(S)) of
	S ->
		K({ok, {I+length(S), Input, [S]}});
	_ ->
		K({fail, {mismatch, string, S}, {I, Input, []}})
	end.

% @doc Parse with a series of parser L, success when ALL of them parsed.
% @spec cps_seq(state(), L :: [cps_parser()], parser_continuation()) -> parsed_result()
cps_seq({I, Input, []}, [H|[]], K) ->  H({I, Input, []}, [], K);
cps_seq({I, Input, []}, [H|T], K) -> cps_seq({I, Input, {I, []}}, [H|T], K);
cps_seq({I, Input, {Bt, Acc}}, [H|[]], K) ->
	H({I, Input, []}, [], fun(Rh) ->
		case Rh of
		{ok, {Ih, _, Ph}} ->
			K({ok, {Ih, Input, Acc ++ Ph}});
		{fail, Reason, _} ->
			K({fail, Reason, {Bt, Input, []}})
		end
	end);
cps_seq({I, Input, {Bt, Acc}}, [H|T], K) ->
	H({I, Input, []}, [], fun(Rh) ->
		case Rh of
		{ok, {Ih, _, Ph}} ->
			cps_seq({Ih, Input, {Bt, Acc ++ Ph}}, T, K);
		{fail, Reason, _} ->
			K({fail, Reason, {Bt, Input, []}})
		end
	end).

% @doc Parse with parser P but not advance, success when parsing failed.
% @spec cps_pred_not(state(), P :: cps_parser(), parser_continuation()) -> parsed_result()
cps_pred_not({I, Input, _}, A, K) ->
	A({I, Input, []}, [], fun(R) ->
		case R of
		{ok, _} ->
			K({fail, {mismatch, '!'}, {I, Input, []}});
		_ ->
			K({ok, {I, Input, []}})
		end
	end).

% @doc Parse with parser P but not advance, success when parsing successed.
% @spec cps_pred_and(state(), P :: cps_parser(), parser_continuation()) -> parsed_result()
cps_pred_and({I, Input, _}, A, K) ->
	A({I, Input, []}, [], fun(R) ->
		case R of
		{ok, _} ->
			K({ok, {I, Input, []}});
		_ ->
			K({fail, {mismatch, '!'}, {I, Input, []}})
		end
	end).

% @doc Parse with a series of parser L, success when FIRST of them parsed.
% @spec cps_alt(state(), L :: [cps_parser()], parser_continuation()) -> parsed_result()
cps_alt(S, [H|[]], K) -> H(S, [], K);
cps_alt(S, [H|T], K) ->
	{I, Input, _} = S,
	H({I, Input, []}, [], fun(Rh) ->
		case Rh of
		{ok, {Ih, _, Ph}} ->
			K({ok, {Ih, Input, Ph}});
		{fail, _Reason, _} ->
			cps_alt({I, Input, []}, T, K)
		end
	end).

% @doc Parse with parser P repeatively, stop when first failed, never fail.
%      Result contains zero or more parsed results.
% @end
% @spec cps_rep(state(), P :: cps_parser(), parser_continuation()) -> parsed_result()
-spec cps_rep(state(), cps_parser(), parser_continuation()) -> parsed_result().
cps_rep({I, Input, Acc}, A, K) ->
	A({I, Input, []}, [], fun(Rh) ->
		case Rh of
		{ok, {Ih, _, Ph}} ->
			cps_rep({Ih, Input, Acc ++ Ph}, A, K);
		{fail, _Reason, _} ->
			K({ok, {I, Input, Acc}})
		end
	end).

% @doc Parse with parser P repeatively, stop when first failed, fail when no result.
%      Result contains one or more parsed results.
% @end
% @spec cps_more(state(), P :: cps_parser(), parser_continuation()) -> parsed_result()
cps_more({I, Input, []}, A, K) ->
	cps_seq({I, Input, []}, [A, fun(S1, _, K1)-> cps_rep(S1, A, K1) end], K).

% @doc Parse with parser P, never fail.
%      Result contains zero or one parsed results.
% @end
% @spec cps_option(state(), P :: cps_parser(), parser_continuation()) -> parsed_result()
cps_option({I, Input, []}, A, K) ->
	cps_alt({I, Input, []}, [A, fun cps_empty/3], K).
