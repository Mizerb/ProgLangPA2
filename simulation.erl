-module(simulation).
-export([run/1]).

run(Filename) ->
    TestName = "node1",
    Data = parser:read(Filename),
    %io:format("~w~n",[Data]),
    %NodeData = lists:keyfind("node1",3, Data),
    %io:format("~w~n", [NodeData]),
    [Left, Center, Right] = getInfo(Data, TestName, length(Data)),
    io:format("~w~n", [Left]),
    io:format("~w~n", [Center]),
    io:format("~w~n", [Right]).




getInfo(Data, Loc, Max) ->
    %this would be so much easier in C
    % I want the damn index of the tuple that matches the name
    Center = lists:keyfind(Loc,3, Data),
    {Index,_} = string:to_integer(string:substr(Loc, 5)),
    if 
        Index == 1 ->
            Left = lists:nth(Max,Data),
            Right = lists:nth(Index+1,Data);
        Index == Max ->
            Right = lists:nth(1, Data),
            Left = lists:nth(Index-1,Data);
        true ->
            Left = lists:nth(Index-1,Data),
            Right = lists:nth(Index+1,Data)
    end,
    [Left, Center, Right].


