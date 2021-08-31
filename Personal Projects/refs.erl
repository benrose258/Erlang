% "References" program

-module(refs).
-compile(export_all).


fact(0) ->
    1;
fact(N) when N>0 ->
    N*fact(N-1).

start() ->
    S = spawn(?MODULE,server,[]),
    spawn(?MODULE,client,[S]).

server() ->
    receive
	{From,req,Ref,N} ->
	    From!{self(),ans,Ref,fact(N)},
	    server()
    end.

client(S) ->
    R1 =  make_ref(),
    S!{self(),req,R1,10},
    R2 = make_ref(),
    S!{self(),req,R2,12},
    receive
	{S,ans,R1,A1} ->
	    receive
		{S,ans,R2,A2} ->
		    io:format("The fact of 10 is ~w and of 12 is ~w~n",[A1,A2])
	    end
    end.
