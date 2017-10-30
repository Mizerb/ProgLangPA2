-module(simulation_v1).
-export([run/1,nodelife/8]).

run(Filename) ->
    %TestName = "node1",
    Data = parser:read(Filename),

    % take the list of nodes and spawn the nodes that I want
    %io:format("~w~n",[Data]),
    %NodeData = lists:keyfind("node1",3, Data),
    %io:format("~w~n", [NodeData]),
    Pids = create_Actors(length(Data), lists:nth(1,Data)),

   
    sendInfo(Data, Pids, length(Data), Data, 1),
    holdElection(Data, Pids, length(Data), Data, 1),
    sleep(3000),
    getResults(Data, Pids, length(Data), Data, 1),
    holdElection(Data, Pids, length(Data), Data, 1),
    sleep(3000),
    getResults(Data, Pids, length(Data), Data, 1).
    
    %holdElection(Data, Pids, length(Data), Data, 1).

    %io:format("~w~n", [Left]),
    %io:format("~w~n", [Center]),
    %io:format("~w~n", [Right]).

sleep(T) ->
	receive
	after T -> ok
	end.

 
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
    writeOut("Initialized node ~w ~n", [lists:nth(Index, Pids)]),
    lists:nth(Index,Pids) ! {information,Left, Working, Right, self(), Max},
    sendInfo(Data,Pids, Max, Tail,Index+1).

holdElection(_, _, _, [], _) -> [];
holdElection([Working | Tail]) ->
    Working ! {voteStart},
    holdElection(Tail).

getResults([]) -> ok;
getResults( [Working | Tail]) ->
    %Grab left and right depending on Index of item
    Working ! {voteStop},
    getResults(Tail).


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

deposeCheck(Leader, Myself, RevCount, Max) ->
    (Leader == Myself) andalso (RevCount >= ((Max+1) div 2) ).

revoltCheck(Leader, Myself, Revolted, Time, Start) ->
    (Leader /= Myself) and (Revolted == false) and ((Time - Start) > getTolerance(Myself)).

getTolerance({_,_,_,_,Tolerance}) -> Tolerance.
getID({ID,_,_,_,_}) -> ID.
getName({_,_,Name,_,_}) -> Name.
getPriority({_,_,_,Priority,_}) -> Priority.

% if another actor has a priority greater than me or I was already leader
priorityGreater(WasLeader, MyPriority, SomeonesPriority) ->
	(SomeonesPriority > MyPriority) or (WasLeader == true).

priorityLess(Living, WasLeader, MyPriority, SomeonesPriority) ->
	(MyPriority > SomeonesPriority) and (WasLeader == false) and (Living == true).

% if an actor gets its own message it is leader
gotMyOwnMessage(Living, MyID, SomeonesID) ->
	(MyID == SomeonesID) and (Living == true).

% left and right are pids. center is the tuple of current node info
nodelife(Left, Center, Right, Master, Total, Living, Revolted, WasLeader) ->
  
    receive
    	{leftmessage, Sender_ID, Sender_Priority} ->

    		io:format("Node~w received a left message from node ~w ~n", [self(), Sender_ID]),

    		% someone has a higher priority than me or if I was already leader
    		case priorityGreater(WasLeader, getPriority(Center), Sender_Priority) of 
    			true ->
    				io:format("I can not be leader. Priority: ~w. ~n", [getPriority(Center)]),
       				
       				% forward the original message to the next node to the left
    				Left ! {leftmessage, Sender_ID, Sender_Priority},
    				
    				% set myself to passive (living = false)
    				nodelife(Left, Center, Right, Master, Total, false, Revolted, WasLeader);
    			_ ->
          			ok
    		end,

    		% my priority is still higher than anyone I have talked to
    		case priorityLess(Living, WasLeader, getPriority(Center), Sender_Priority) of 
    			true ->
	    			% send my own message to the next node
	    			Left ! {leftmessage, self(), getPriority(Center)},

	    			io:format("Node with priority ~w might be leader, ~n", [getPriority(Center)]),

	    			% i am still active (Living = true)
	    			nodelife(Left, Center, Right, Master, Total, true, Revolted, WasLeader);
	    		_ ->																	
	          		ok
    		end,

    		% check if the actor got its own message
    		case gotMyOwnMessage(Living, self(), Sender_ID) of
    			true ->
    				io:format("L BOOOOOOOM: I got my own msg! The leader is node~w.  Priority: ~w~n", [self(), getPriority(Center)]);
    				%nodelife(Left, Center, Right, Master, Total, Living, Revolted, true);
    			_ ->
          			ok
    		end;


    	{rightmessage, Sender_ID, Sender_Priority} ->

    		io:format("Node~w received a right message from node ~w ~n", [self(), Sender_ID]),

    		% someone has a higher priority than me or if I was already leader
    		case priorityGreater(WasLeader, getPriority(Center), Sender_Priority) of 
    			true ->
    				io:format("I can not be leader. Priority: ~w. ~n", [getPriority(Center)]),

    				% forward the original message to the next node to the right
    				Right ! {rightmessage, Sender_ID, Sender_Priority},

    				% set myself to passive (living = false)
    				nodelife(Left, Center, Right, Master, Total, false, Revolted, WasLeader);
    			_ ->
          			ok
    		end,

    		% my priority is still higher than anyone I have talked to
    		case priorityLess(Living, WasLeader, getPriority(Center), Sender_Priority) of
    			true ->
	    			% send my own message to the next node
	    			Right ! {rightmessage, self(), getPriority(Center)},

	    			io:format("Node with priority ~w might be leader, ~n", [getPriority(Center)]),

	    			% i am still active (Living = true)
	    			nodelife(Left, Center, Right, Master, Total, true, Revolted, WasLeader);
	    		_ ->
	          		ok
    		end,

    		% check if the actor got its own message
    		case gotMyOwnMessage(Living, self(), Sender_ID) of
    			true ->
    				io:format("R BOOOOOOOM: I got my own msg! The leader is node~w.  Priority: ~w~n", [self(), getPriority(Center)]);
    				%nodelife(Left, Center, Right, Master, Total, Living, Revolted, true);
    			_ ->
          			ok
    		end;



        {time, Leader, Start, Time, RevCount} ->
            %Check if Leader and Deposition possible
            case deposeCheck(Leader, Center, RevCount, Total) of true->
                writeOut("ID=~w was deposed at t=~w~n",[getID(Center),Time]),
                Master ! {voteStart, Time},
                nodelife(Left, Center, Right, Master, Total, false, false, WasLeader);
                _ ->
                    ok
            end,
            %Check if this node revolts
            case revoltCheck(Leader, Center, Revolted, Time, Start) of true ->
                writeOut("ID=~w revolted at t=~w~n",[getID(Center),Time]),
                Revolted = true,
                RevCount = RevCount + 1
            end,
            %send along to next node and hit recursion
            Left ! {time, Leader, Start, Time + 1, RevCount},
            nodelife(Left, Center, Right, Master, Total, Living, Revolted, WasLeader);
        {voteStart} ->
        	io:format("node ~w received msg to start election.~n", [self()]),

        	Left  ! {leftmessage, self(), getPriority(Center)},
        	Right ! {rightmessage, self(), getPriority(Center)},

        	% initially all nodes are active (Living = true)
        	nodelife(Left, Center, Right, Master, Total, true, Revolted, WasLeader);
        {voteStop} ->
        	io:format("      ~w: ~w: active: ~w~n", [self(), getPriority(Center), Living]),
        	if
        		% if this node is the current leader, set (WasLeader = true)
        		Living == true ->
        			nodelife(Left, Center, Right, Master, Total, Living, Revolted, true);
        		true ->
        			nodelife(Left, Center, Right, Master, Total, Living, Revolted, false)
        	end;
            
        {startClock, Time} ->
            writeOut("ID=~w became leader at t=~w~n",[getID(Center),Time]),
            Left ! {time, Center, Time, Time+1, 0},
            nodelife(Left, Center, Right, Master, Total, false, false, WasLeader);
        {information, Zleft,ZCenter,Zright, ZMaster, ZTotal} ->
            %writeOut("REAL BOY ~w~n",[ZCenter]),
            nodelife(Zleft, ZCenter, Zright, ZMaster,ZTotal, false,false, false);

         {returnPriority} ->
    		getPriority(Center);
    		%io:format("Priority: ~w~n", [getPriority(Center)]);

    	% getting a reply means I can no longer be leader
    	{replyMessage, Sender_ID, Sender_Priority} ->
    		Living = false
    		%io:format("Got a reply. I am passive now.~n", []);



     

    end.






writeOut(Written, Data) ->
    io:format(Written,Data).
