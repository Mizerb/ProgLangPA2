-module(simulation).
-export([run/1]).


run(Filename) ->
    Data = parser:read(Filename),
    %io:format("~w~n",[Data]),
    NodeData = lists:keyfind("node1",3, Data),
    io:format("~w~n", [NodeData])

    