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
%	,c_symbol_put/2
%	,c_symbol_get/1
	]).


-type parser_continuation() :: fun((parse_result()) -> parse_result()).
-type cps_parser() :: fun((state(), any(), parser_continuation()) -> parse_result()).

return(X) -> X.
-spec cps_tr(state(), {cps_parser(), transformer()}, parser_continuation()) -> parse_result().
cps_tr({I, Input, _}, {A, T}, K) when is_function(T) ->
	A({I, Input, []}, [], fun(R) ->
		case R of
		{ok, {Ia, Input, Pa}} ->
			K({ok, {Ia, Input, T(Pa)}});
		_ ->
			K(R)
		end
	end).

-spec cps_empty(state(), any(), parser_continuation()) -> parse_result().
cps_empty(S, _, K) ->
	K({ok, S}).

-spec cps_anychar(state(), any(), parser_continuation()) -> parse_result().
cps_anychar({I, Input, _}, _, K) when I > length(Input) ->
	K({fail, eof, {I, Input, []}});
cps_anychar({I, Input, _}, _, K) ->
	K({ok, {I+1, Input, [[lists:nth(I, Input)]]}}).

-spec cps_char(state(), char(), parser_continuation()) -> parse_result().
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

-spec cps_charrange(state(), {char(), char()}, parser_continuation()) -> parse_result().
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

-spec cps_charclass(state(), string(), parser_continuation()) -> parse_result().
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

-spec cps_string(state(), string(), parser_continuation()) -> parse_result().
cps_string({I, Input, _}, S, K) when I + length(S) -1 > length(Input) ->
	K({fail, eof, {I, Input, []}});
cps_string({I, Input, _}, S, K) ->
	case lists:sublist(Input, I, length(S)) of
	S ->
		K({ok, {I+length(S), Input, [S]}});
	_ ->
		K({fail, {mismatch, string, S}, {I, Input, []}})
	end.

-spec cps_seq(state(), [cps_parser()], parser_continuation()) -> parse_result().
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

-spec cps_pred_not(state(), cps_parser(), parser_continuation()) -> parse_result().
cps_pred_not({I, Input, _}, A, K) ->
	A({I, Input, []}, [], fun(R) ->
		case R of
		{ok, _} ->
			K({fail, {mismatch, '!'}, {I, Input, []}});
		_ ->
			K({ok, {I, Input, []}})
		end
	end).

-spec cps_pred_and(state(), cps_parser(), parser_continuation()) -> parse_result().
cps_pred_and({I, Input, _}, A, K) ->
	A({I, Input, []}, [], fun(R) ->
		case R of
		{ok, _} ->
			K({ok, {I, Input, []}});
		_ ->
			K({fail, {mismatch, '!'}, {I, Input, []}})
		end
	end).
	
-spec cps_alt(state(), [cps_parser()], parser_continuation()) -> parse_result().
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

-spec cps_rep(state(), cps_parser(), parser_continuation()) -> parse_result().
cps_rep({I, Input, Acc}, A, K) ->
	A({I, Input, []}, [], fun(Rh) ->
		case Rh of
		{ok, {Ih, _, Ph}} ->
			cps_rep({Ih, Input, Acc ++ Ph}, A, K);
		{fail, _Reason, _} ->
			K({ok, {I, Input, Acc}})
		end
	end).

-spec cps_more(state(), cps_parser(), parser_continuation()) -> parse_result().
cps_more({I, Input, []}, A, K) ->
	cps_seq({I, Input, []}, [A, fun(S1, _, K1)-> cps_rep(S1, A, K1) end], K).

-spec cps_option(state(), cps_parser(), parser_continuation()) -> parse_result().
cps_option({I, Input, []}, A, K) ->
	cps_alt({I, Input, []}, [A, fun cps_empty/3], K).
