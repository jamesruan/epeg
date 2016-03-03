-module(epeg_cps).
-include("epeg.hrl").

-export([
	cps_tr/3,
	cps_empty/3,
	cps_anychar/3,
	cps_char/3,
	cps_charrange/3,
	cps_charclass/3,
	cps_string/3,
	cps_seq/3
%	c_pred_not/1,
%	c_pred_and/1,
%	c_alt/1,
%	c_rep/1,
%	c_more/1,
%	c_option/1,
%	c_symbol_put/2,
%	c_symbol_get/1
	]).

-type parser_continuation() :: fun((parse_result(), parser_continuation()) -> parse_result()).
-type cps_parser() :: fun((state(), any(), parser_continuation()) -> parse_result()).

-spec cps_tr(state(), {cps_parser(), transformer()}, parser_continuation()) -> parse_result().
cps_tr({I, _, Input}, {A, T}, K) when is_function(T) ->
	A({I, [], Input}, fun(R) ->
		case R of
		{ok, {Ia, Pa, Input}} ->
			K({ok, {Ia, T(Pa), Input}});
		_ ->
			K(R)
		end
	end).

-spec cps_empty(state(), any(), parser_continuation()) -> parse_result().
cps_empty({I, _, Input}, _, K) when I > length(Input) ->
	K({fail, eof, {I, [], Input}});
cps_empty(S, _, K) ->
	K({ok, S}).

-spec cps_anychar(state(), any(), parser_continuation()) -> parse_result().
cps_anychar({I, _, Input}, _, K) ->
	cps_empty({I, [], Input}, [], fun(R) ->
		case R of
		{ok, _} ->
			K({ok, {I+1, [[lists:nth(I, Input)]], Input}});
		_ ->
			K(R)
		end
	end).

-spec cps_char(state(), char(), parser_continuation()) -> parse_result().
cps_char({I, _, Input}, C, K) ->
	cps_empty({I, [], Input}, [], fun(R) ->
		case R of
		{ok, _} ->
			PR = case lists:nth(I, Input) of
			C ->
				{ok, {I+1, [[C]], Input}};
			_ ->
				{fail, {mismatch, char, [C]}, {I, [], Input}}
			end,
			K(PR);
		_ ->
			K(R)
		end
	end).

-spec cps_charrange(state(), {char(), char()}, parser_continuation()) -> parse_result().
cps_charrange({I, _, Input}, {F, T}, K) when F < T->
	cps_empty({I, [], Input}, [], fun(R) ->
		case R of
		{ok, _} ->
			C = lists:nth(I, Input),
			PR = if
			C >= F , C =< T ->
				{ok, {I+1, [[C]], Input}};
			true ->
				{fail, {mismatch, charrange, [F], [T]}, {I, [], Input}}
			end,
			K(PR);
		_ ->
			K(R)
		end
	end).

-spec cps_charclass(state(), string(), parser_continuation()) -> parse_result().
cps_charclass({I, _, Input}, L, K) ->
	cps_empty({I, [], Input}, [], fun(R) ->
		case R of
		{ok, _} ->
			C = lists:nth(I, Input),
			PR = case lists:member(C, L) of
			true ->
				{ok, {I+1, [[C]], Input}};
			_ ->
				{fail, {mismatch, charclass, L}, {I, [], Input}}
			end,
			K(PR);
		_ ->
			K(R)
		end
	end).

-spec cps_string(state(), string(), parser_continuation()) -> parse_result().
cps_string({I, _, Input}, S, K) ->
	cps_empty({I, [], Input}, [], fun(R) ->
		case R of
		{ok, _} ->
			case lists:sublist(Input, I, length(S)) of
			S ->
				K({ok, {I+length(S), [S], Input}});
			_ ->
				K({fail, {mismatch, string, S}, {I, [], Input}})
			end;
		_ ->
			K(R)
		end
	end).

-spec cps_seq(state(), [cps_parser()], parser_continuation()) -> parse_result().
cps_seq(S, [H|[]], K) ->  H(S, [], K);
cps_seq(S, [H|T], K) ->
	{I, Acc, Input} = S,
	H({I, [], Input}, fun(Rh) ->
		case Rh of
		{ok, {Ih, Ph, _}} ->
			cps_seq({Ih, Acc ++ Ph, Input}, T, K);
		{fail, Reason, _} ->
			K({fail, Reason, S})
		end
	end).
