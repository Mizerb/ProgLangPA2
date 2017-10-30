-module(simulation).
-export([run/1,nodelife/8]).

run(Filename) ->
    file:delete("output.txt"),
    Data = parser:read(Filename),

    % take the list of nodes and spawn the nodes that I want
    %io:format("~w~n",[Data]),
    %NodeData = lists:keyfind("node1",3, Data),
    %io:format("~w~n", [NodeData]),
    Pids = create_Actors(length(Data), lists:nth(1,Data)),


    sendInfo(Data, Pids, length(Data), Data, 1),
    holdElection(Pids),
    master(Pids, 0, true,0).
    % holdElection(Data, Pids, length(Data), Data, 1),
    % sleep(3000),
    % getResults(Data, Pids, length(Data), Data, 1).

    %holdElection(Data, Pids, length(Data), Data, 1).

    %io:format("~w~n", [Left]),
    %io:format("~w~n", [Center]),
    %io:format("~w~n", [Right]).



 
dummyArgs(Dummy) -> [0,Dummy,0,0,0,0,0,0].

create_Actors(0,_) -> [];
create_Actors(N,Dummy) ->
    [spawn(?MODULE,nodelife,dummyArgs(Dummy)) | create_Actors( N-1 , Dummy)].

sendInfo(_, _, _, [], _) -> [];
sendInfo(Data,Pids, Max, [Working | Tail],Index) ->
    %Grab left and right depending on Index of item
    if
        Index == 1 ->
            Left = lists:nth(Max,Pids),
            Right = lists:nth(Index+1,Pids);
        Index == Max ->
            Right = lists:nth(1, Pids),
            Left = lists:nth(Index-1,Pids);
        true ->
            Left = lists:nth(Index-1,Pids),
            Right = lists:nth(Index+1,Pids)
    end,
    %writeOut("Initialized node ~w ~n", [lists:nth(Index, Pids)]),
    lists:nth(Index,Pids) ! {information,Left, Working, Right, self(), Max},
    sendInfo(Data,Pids, Max, Tail,Index+1).

holdElection([]) -> [];
holdElection([Working | Tail]) ->
    Working ! {voteStart},
    holdElection(Tail).

% getResults([]) -> ok;
% getResults( [Working | Tail]) ->
%     %Grab left and right depending on Index of item
%     Working ! {voteStop},
%     getResults(Tail).


% getInfo(Data, Pids, Loc, Max) ->
%     %this would be so much easier in C
%     % I want the damn index of the tuple that matches the name
%     Center = lists:keyfind(Loc,3, Data),
%     {Index,_} = string:to_integer(string:substr(Loc, 5)),
%     if
%         Index == 1 ->
%             Left = lists:nth(Max,Data),
%             Right = lists:nth(Index+1,Data);
%         Index == Max ->
%             Right = lists:nth(1, Data),
%             Left = lists:nth(Index-1,Data);
%         true ->
%             Left = lists:nth(Index-1,Data),
%             Right = lists:nth(Index+1,Data)
%     end,
%     [Left, Center, Right].


terminate([]) -> writeOut("End of simulation~n",[]);
terminate([Working | Tail]) ->
    Working ! {dead},
    terminate(Tail).


master(Pids, Time, ElectionSeason, Count) ->
    receive
        %Begin a vote
        {shit}->
            %io:format("Shit~w~n",[0]),
            master(Pids,Time,ElectionSeason, Count);
        {dude,TimeA} ->
            if
                Count >= length(Pids) ->
                    terminate(Pids);
                true->
                    %io:format("the vote will begin~n"),
                    holdElection(Pids),
                    master(Pids, TimeA, true, Count)
            end;
        {voteWin, From} ->
            %writeOut("MASTER ID ~w~n",[self()]),
            if
                ElectionSeason == true ->
                    From ! {startClock, Time},
                    master(Pids,Time+1, false, Count+1);
                true ->
                    master(Pids, Time, false, Count)
            end
    end.



% add(Revolters, Peasant, Time) ->
%     case lists:member(Peasant, Revolters) of
%         false ->
%             io:format("ID=~w revolted at t=~w~n", [getID(Peasant),Time]),
%             [Peasant | Revolters];
%         true ->
%             Revolters
%     end.


deposeCheck(Leader, Myself, RevCount, Max) ->
    (Leader == Myself) and (RevCount >= ((Max+1) div 2) ).

revoltCheck(Leader, Myself, Revolted, Time, Start) ->
    (Leader /= Myself) and (Revolted == false) and ((Time - Start) > getTolerance(Myself)).

getTolerance({_,_,_,_,Tolerance}) -> Tolerance.
getID({ID,_,_,_,_}) -> ID.
%getName({_,_,Name,_,_}) -> Name.
getPriority({_,_,_,Priority,_}) -> Priority.
setPriority({A,B,C,_,D}) -> {A,B,C,0,D}.

% if another actor has a priority greater than me or I was already leader
priorityGreater( MyPriority, SomeonesPriority) ->
	(SomeonesPriority > MyPriority).

priorityLess(Living, MyPriority, SomeonesPriority) ->
	(MyPriority > SomeonesPriority) and (Living == true).

% if an actor gets its own message it is leader
gotMyOwnMessage(Living, MyID, SomeonesID) ->
	(MyID == SomeonesID) and(Living == true).

% left and right are pids. center is the tuple of current node info
nodelife(Left, Center, Right, Master, Total, Living, Revolted, WasLeader) ->

    receive
    	{leftmessage, Sender_ID, Sender_Priority} ->

    		%io:format("Node~w received a left message from node ~w ~n", [self(), Sender_ID]),
            % check if the actor got its own message
    		Own = gotMyOwnMessage(Living, self(), Sender_ID),
            Greater = priorityGreater( getPriority(Center), Sender_Priority),
            Less = priorityLess(Living, getPriority(Center), Sender_Priority),
            if
                Own == true ->
                    Master ! {voteWin, self()},
                    nodelife(Left, Center, Right, Master, Total, Living, false, true);
                Greater == true ->
                    Left ! {leftmessage, Sender_ID, Sender_Priority},
                    nodelife(Left, Center, Right, Master, Total, false, false, WasLeader);
                Less == true ->
                    nodelife(Left, Center, Right, Master, Total, true, false, WasLeader);
                true ->
                    nodelife(Left, Center, Right, Master, Total, Living, false, WasLeader)
            end;


    	{rightmessage, Sender_ID, Sender_Priority} ->

    		Own = gotMyOwnMessage(Living, self(), Sender_ID),
            Greater = priorityGreater( getPriority(Center), Sender_Priority),
            Less = priorityLess(Living, getPriority(Center), Sender_Priority),
            if
                Own == true ->
                    Master ! {voteWin, self()},
                    nodelife(Left, Center, Right, Master, Total, Living, false, true);
                Greater == true ->
                    Right ! {leftmessage, Sender_ID, Sender_Priority},
                    nodelife(Left, Center, Right, Master, Total, false, false, WasLeader);
                Less == true ->
                    nodelife(Left, Center, Right, Master, Total, true, false, WasLeader);
                true ->
                    nodelife(Left, Center, Right, Master, Total, Living, false, WasLeader)
            end;

        {time, Leader, Start, Time, RevCount} ->
            %Check if Leader and Deposition possible
            %writeOut("Time~w~n",[Time]),
            case deposeCheck(Leader, Center, RevCount, Total) of
                true->
                    writeOut("ID=~w was deposed at t=~w~n",[getID(Center),Time]),
                    %writeOut("My Master is ~w~n", [Master]),
                    %Master ! {shit},
                    Master ! {dude,Time+1},
                    nodelife(Left, setPriority(Center), Right, Master, Total, true, false, false);
                _ ->
                    case revoltCheck(Leader, Center, Revolted, Time, Start) of
                        true ->
                            writeOut("ID=~w revolted at t=~w~n",[getID(Center),Time]),
                            Left ! {time, Leader, Start, Time +1, RevCount +1},
                            nodelife(Left,Center,Right, Master,Total,Living,true,WasLeader);
                        _ ->
                            %writeOut("Revolt?ID ~w  ~w   ~w  ~n",[getID(Center),getTolerance(Center), Time-Start]),
                            Left ! {time, Leader, Start, Time + 1, RevCount},
                            nodelife(Left, Center, Right, Master, Total, Living, Revolted, WasLeader)
                    end
            end;
            %Check if this node revolts
        {voteStart} ->
        	%io:format("node ~w received msg to start election.~n", [self()]),

        	Left  ! {leftmessage, self(), getPriority(Center)},
        	Right ! {rightmessage, self(), getPriority(Center)},

        	% initially all nodes are active (Living = true)
        	nodelife(Left, Center, Right, Master, Total, true, Revolted, WasLeader);
        {voteStop} ->
        	%io:format("      ~w: ~w: active: ~w~n", [self(), getPriority(Center), Living]),
        	if
        		% if this node is the current leader, set (WasLeader = true)
        		Living == true ->
        			%io:format("~n~nThe leader is: ~w. Priority: ~w~n~n~n", [getID(Center), getPriority(Center)]),
        			master ! {voteWin, self()},
        			nodelife(Left, Center, Right, Master, Total, Living, Revolted, true);
        		true ->
        			nodelife(Left, Center, Right, Master, Total, Living, Revolted, false)
        	end;

        {startClock, Time} ->
            writeOut("ID=~w became leader at t=~w~n",[getID(Center),Time]),
            Left ! {time, Center, Time, Time+1, 0},
            nodelife(Left, Center, Right, Master, Total, true, false, WasLeader);
        {information, Zleft,ZCenter,Zright, ZMaster, ZTotal} ->
            %writeOut("REAL BOY ~w~n",[ZCenter]),
            nodelife(Zleft, ZCenter, Zright, ZMaster,ZTotal, false,false, false);
        {dead} ->
            exit(0)
    end.


writeOut(Written, Data) ->
    file:write_file("output.txt", io_lib:fwrite(Written,Data), [append]).
    %io:format(Written,Data).
