% Classwork for November 3rd and November 5th

-module(sem_Nov_3_2020).
-compile(export_all).



start(Permits) ->
    S = spawn(?MODULE,semaphore,[Permits]),
    %% Spawn client1 and client 2
    todo.


semaphore(0) ->
    todo;
semaphore(N) when N>0 ->
    todo.

acquire(S) ->
  receive
    {S,ok} -> ok
  end.

release(S) ->
  S!{self(),release}.

%% Print ab only after printing cd
% Do so by making semaphores for this code.

client1(S) ->
  acquire(S),
  io:format("a"),
  io:format("b").

client2(S) ->
    io:format("c"),
    io:format("d").
    release(S).
