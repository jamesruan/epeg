-ifndef(epeg_h).
-define(epeg_h, 1).

-define(DEBUG, true).
-ifdef(DEBUG).
-ifdef(TEST).
-define(LOG(X), io:fwrite(user, "{~p,~p}: ~p: ~p~n", [?MODULE, ?LINE, ??X, X])).
-else.
-define(LOG(X), io:format(user, "{~p,~p}: ~p: ~p~n", [?MODULE, ?LINE, ??X, X])).
-endif.
-else.
-define(LOG(X), true).
-endif.

-type token() :: list().
%% Token that returned when a symbol is parsed, must be a list
-record(state,
	{index :: pos_integer(),
	 input = [] :: string(),
	 parsed = [] :: token() | {pos_integer(), token()}}).
%% The contains necessariy information for parsing state.
-type transformer() :: fun((token()) -> token()).
%% A function that changes the token from default.
-type result(S) :: {ok, S} | {fail, fail_reason(), S}.
%% A mark for sucess or failure.
-type parsed_result() :: result(#state{}).
-type fail_reason() :: {mismatch, list()} | eof.
%% Reason for parsing failure
-type parser() :: fun((#state{}) -> parsed_result()).
%% Monadic function that parses a token and return its parsed_result.
-endif.
