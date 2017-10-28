-module(simulation).
-export([run/1]).

run(Filename) ->
    TestName = "node1",
    Data = parser:read(Filename),

    % take the list of nodes and spawn the nodes that I want
    %io:format("~w~n",[Data]),
    %NodeData = lists:keyfind("node1",3, Data),
    %io:format("~w~n", [NodeData]),
    Pids = create_Actors(length(Data)),

    [Left, Center, Right] = getInfo(Data, Pids,TestName, length(Data)).
    %io:format("~w~n", [Left]),
    %io:format("~w~n", [Center]),
    %io:format("~w~n", [Right]).


create_Actors(0) -> [];
create_Actors(N) ->
    Input = {1,1,1,1,1,1,1},
    [spawn(simulation,nodelife,Input) | create_Actors(Data, N-1)];


addPid({ID,IP,Name,Priority,Tolerance}, PID) ->
    {ID, IP, Name, Priority, Tolerance, PID}.

getInfo(Data, Pids, Loc, Max) ->
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


master(Info, Time) ->
    receive
        %Begin a vote
        {voteStart, Time} ->
            io:format("the vote will begin"),
            master(Info, Time);
        {voteWin, From} ->
            From ! {startClock, Time},
            master(Info, Time+1)
    end.



add(Revolters, Peasant, Time) ->
    case lists:member(Peasant, Revolters) of
        false ->
            io:format("ID=~w revolted at t=~w~n", [getID(Peasant),Time]),
            [Peasant | Revolters];
        true ->
            Revolters
    end.


deposeCheck(Leader, Myself, RevCount, Max) ->
    (Leader == Myself) andalso (RevCount >= ((Max+1) div 2) ).

revoltCheck(Leader, Myself, Revolted, Time, Start) ->
    (Leader /= Myself) and (Revolted == false) and ((Time - Start) > getTolerance(Myself)).

getTolerance({_,_,_,_,Tolerance,_}) -> Tolerance.
getID({ID,_,_,_,_,_}) -> ID.
getPid({_,_,_,_,_,PID}) -> PID.

nodelife(Data) ->
    {Left, Center, Right, Master, Total, Living, Revolted} = Data,
    %Left is node to Left, Center is self, Right is node to right, Master
    % is master node, that prints stuff, & Living is if this node has been
    % the leader before
    receive
        {time, Leader, Start, Time, RevCount} ->
            %Check if Leader and Deposition possible
            case deposeCheck(Leader, Center, RevCount, Total) of true->
                writeOut("ID=~w was deposed at t=~w~n",[getID(Center),Time]),
                Master ! {voteStart, Time},
                nodelife({Left, Center, Right, Master, Total, false, false})
            end,
            %Check if this node revolts
            case revoltCheck(Leader, Center, Revolted, Time, Start) of true ->
                writeOut("ID=~w revolted at t=~w~n",[getID(Center),Time]),
                Revolted = true,
                RevCount = RevCount + 1
            end,
            %send along to next node and hit recursion
            getPid(Left) ! {time, Leader, Start, Time + 1, RevCount},
            nodelife({Left, Center, Right, Master, Total, Living, Revolted});
        {voteStart} ->

            nodelife({Left, Center, Right, Master, Total, Living, false});
        {startClock, Time} ->
            writeOut("ID=~w became leader at t=~w~n",[getID(Center),Time]),
            getPid(Left) ! {time, Center, Time, Time+1, 0},
            nodelife({Left, Center, Right, Master, Total, false, false});
        {orient, {Zleft,ZCenter,Zright, ZMaster, ZTotal}}
            nodelife({Zleft, ZCenter, Zright, ZMaster,ZTotal, false,false});
    end.






writeOut(Written, Data) ->
    io:format(Written,Data).