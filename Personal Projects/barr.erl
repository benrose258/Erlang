% "Barrier" program

-module(barr).
-compile(export_all).


start(N) ->
    B = spawn(?MODULE,coordinator,[N,N,[]]),
    spawn(?MODULE,client1,[B]),
    spawn(?MODULE,client2,[B]).

%%% N is the number of threads that have to reach the barrier before they all fall through
%%% M is the number of threads YET to reach the barrier
%%% L is the list of {PID,Refs} of those threads that have ALREADY reached the barrier
coordinator(N,M,L) ->
    implement.

barrier(B) ->
    R = make_ref(),
    B!{self(),arrived,R},
    receive
	{B,R,ok} ->
	    ok
    end.

client1(B) ->
    io:format("a"),
    barrier(B),
    io:format("1").

client2(B) ->
    io:format("b"),
    barrier(B),
    io:format("2").
