-module(actor).
-export([create/1],[actor/1] ).


actor({Pid,Addresss, Name,Priority,Tolerance}) ->
    receive ->
        {get,A} ->  A ! {A,_,_,_,_};
        {get,B} ->  B ! {_,B,_,_,_};
        {get,C} ->  C ! {_,_,C,_,_};
        {get,D} ->  D ! {_,_,_,D,_};
        {get,E} ->  E ! {_,_,_,_,E};
    .end