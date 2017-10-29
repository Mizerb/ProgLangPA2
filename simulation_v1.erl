-module(simulation_v1).
-export([run/1,nodelife/7]).

run(Filename) ->
    TestName = "node1",
    Data = parser:read(Filename),

    % take the list of nodes and spawn the nodes that I want
    %io:format("~w~n",[Data]),
    %NodeData = lists:keyfind("node1",3, Data),
    %io:format("~w~n", [NodeData]),
    Pids = create_Actors(length(Data), lists:nth(1,Data)),

    sendInfo(Data, Pids, length(Data), Data, 1).
    %io:format("~w~n", [Left]),
    %io:format("~w~n", [Center]),
    %io:format("~w~n", [Right]).
    
% recurElection(Data, MaxSender, L) -> recurElection(Data, MaxSender, L,1). 

% recurElection(Data, MaxSender, [], Increment) -> Increment;

% recurElection(Data, MaxSender, [_|T], Increment) ->
%    %io:fwrite("~w~n",[Increment]), 

%    io:format("~nThe local leader is: ~w~n", [electLeader(Data, Increment)]),
%    %MaxSenderPriority = getPriority()
%    %TmpLeader = electLeader(Data, Increment),
  
%    recurElection(Data, MaxSender, T,Increment+1). 
   

% %% the node with the highest priority becomes leader
% %% you can only be leader once
% electLeader(Data, NodeID) ->
% 	%% start with the first node in the list
% 	%% the first node is the center node
% 	%% recursively search down the left node and right node

% 	StartNode = concat("node", integer_to_list(NodeID)),
% 	[Left, Center, Right] = getInfo(Data, StartNode, length(Data)),
% 	CurrentNode = spawn(nodelife, nodelife, [Left]),

% 	%% add a check here if the left and right nodes have already been leader
% 	%% if either node was already leader, set their priority to zero
% 	LP = getPriority(Left),
% 	RP = getPriority(Right),
% 	CP = getPriority(Center),
% 	if
% 		LP > CP  ->
% 			Leader = Left;
% 		RP > CP  ->
% 			Leader = Right;
% 		true	 ->
% 			Leader = Center
% 	end,
	
% 	Leader.

dummyArgs(Dummy) -> [0,Dummy,0,0,0,0,0].

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
    %writeOut("SENDING TO ~w ~n", [lists:nth(Index, Pids)]),
    lists:nth(Index,Pids) ! {information,Left, Working, Right, self(), Max},
    sendInfo(Data,Pids, Max, Tail,Index+1).

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

getTolerance({_,_,_,_,Tolerance}) -> Tolerance.
getID({ID,_,_,_,_}) -> ID.
getName({_,_,Name,_,_}) -> Name.
getPriority({_,_,_,Priority,_}) -> Priority.

% if another actor has a priority greater than me
priorityGreater(MyPriority, SomeonesPriority) ->
	SomeonesPriority > MyPriority.

priorityLess(MyPriority, SomeonesPriority) ->
	SomeonesPriority < MyPriority.

% if an actor gets its own message it is leader
gotMyOwnMessage(MyID, SomeonesID) ->
	MyID == SomeonesID.

nodelife(Left, Center, Right, Master, Total, Living, Revolted) ->
    %Left is node to Left, Center is self, Right is node to right, Master
    % is master node, that prints stuff, & Living is if this node has been
    % the leader before
    writeOut("I'm with stupid~n",[]),
    receive
    	
    	{message, Sender_ID, Sender_Priority} ->
    		% set myself to passive if someone has higher priority than me
    		case priorityGreater(getPriority(Center), Sender_Priority) of true ->
    			Living = false
    		end,
    		% check if the actor got its own message
    		case gotMyOwnMessage(getID(Center), Sender_ID) of true ->
    			Living = true,
    			Master ! {voteStop},
    			io:format("The leader is node~w.~n", [getID(Center)])
    		end,
    		% if my priority is still higher than anyone I have talked to
    		case priorityLess(getPriority(Center), Sender_Priority) of true ->
    			% forward the message to the next node
    			Left ! {message, Sender_ID, Sender_Priority}
    		end;   		


        {time, Leader, Start, Time, RevCount} ->
            %Check if Leader and Deposition possible
            case deposeCheck(Leader, Center, RevCount, Total) of true->
                writeOut("ID=~w was deposed at t=~w~n",[getID(Center),Time]),
                Master ! {voteStart, Time},
                nodelife(Left, Center, Right, Master, Total, false, false)
            end,
            %Check if this node revolts
            case revoltCheck(Leader, Center, Revolted, Time, Start) of true ->
                writeOut("ID=~w revolted at t=~w~n",[getID(Center),Time]),
                Revolted = true,
                RevCount = RevCount + 1
            end,
            %send along to next node and hit recursion
            Left ! {time, Leader, Start, Time + 1, RevCount},
            nodelife(Left, Center, Right, Master, Total, Living, Revolted);
        {voteStart} ->

            nodelife(Left, Center, Right, Master, Total, Living, false);
        {voteStop} ->

            nodelife(Left, Center, Right, Master, Total, Living, false);
        {startClock, Time} ->
            writeOut("ID=~w became leader at t=~w~n",[getID(Center),Time]),
            Left ! {time, Center, Time, Time+1, 0},
            nodelife(Left, Center, Right, Master, Total, false, false);
        {information, Zleft,ZCenter,Zright, ZMaster, ZTotal} ->
            %writeOut("REAL BOY ~w~n",[ZCenter]),
            nodelife(Zleft, ZCenter, Zright, ZMaster,ZTotal, false,false)



    


    end.






writeOut(Written, Data) ->
    io:format(Written,Data).