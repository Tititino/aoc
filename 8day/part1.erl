%% ------------------------------- %%
-module(part1).
-export([final/1]).
%% ------------------------------- %%

%% some utility functions
%% it may be faster to use reverse and head since they're implemented in C
last([]) ->
    nothing;
last([X]) ->
    X;
last([_ | XS]) ->
    last(XS).

words([]) -> [];
words(STR) -> 
    case string:split(STR, " ") of
	[X]      -> [string:trim(X) | []];
	[X | XS] -> [ X | words(XS)]
    end.
%% ---------------------------

nToLen(N) ->
    case N of
        0 -> 6;
        1 -> 2;
        2 -> 5;
        3 -> 5;
        4 -> 4;
        5 -> 5;
        6 -> 6;
        7 -> 3;
        8 -> 7;
        9 -> 6;
        _ -> erlang:error(undefinedNumberException)
    end.

trimStrings([]) -> [];
trimStrings([X | XS]) ->
    SP = words(last(string:split(X, " | "))),
    case length(SP) of
	4 -> SP ++ trimStrings(XS);
       	_ -> erlang:error(badInputException)
    end.

count([], _) -> 0;
count([X | XS], N) ->
    K = nToLen(N),
    if length(X) == K -> 1 + count(XS, N);
       true	          -> count(XS, N)
    end.

countCon(XS, N, PID) ->
    PID ! count(XS, N).

%% first attemps at concurrency
%% is this actually concurrent or it is just some extra useless steps???
countTotal(NUMS) -> K = fun(VAL, N) ->
                                spawn(part1, countCon, [VAL, N, self()]),
                                receive 
									K -> K
		        				end
						end,
	        			K(NUMS, 1) + K(NUMS, 4) + K(NUMS, 7) + K(NUMS, 8).

read(File) ->
    case file:read_line(File) of
        {ok, Data} -> [Data | read(File)];
        eof	       -> [];
        _	       -> erlang:error(fileException)
    end.

final(FileName) ->
    T = file:open(FileName, [read]),
    case T of
        {ok, Device} -> io:format("~p~n", [countTotal(trimStrings(read(Device)))]),
						file:close(Device);
        _            -> erlang:error(fileException)
    end.
