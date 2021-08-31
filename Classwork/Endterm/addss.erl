% Ben Rose
% I pledge my honor that I have abided by the Stevens Honor System.
% 12/18/2020

% CS 511 Endterm
% Addition Service Server (ADDSS)

-module(addss).
-compile(export_all).

start() ->
  S = spawn(fun server/0),
  [ spawn(?MODULE,client,[S]) || _ <- lists:seq(1,10)].

server() ->
  receive
    {From, Ref, start} -> % receive message from client
      Servlet_PID = spawn(?MODULE, servlet, [0]), % start servlet with the current sum = 0
      From!{Ref, Servlet_PID, servlet_pid}, % send servlet pid to client
      server() % restart server to listen for new requests
  end.

servlet(Current_Sum) ->
  receive
    {From, add, Number} -> % Receive first message
      New_Sum = Current_Sum + Number, % Create the new sum
      servlet(New_Sum); % Recursively update the sum using the servlet function, since adding repeatedly to an existing "variable" is not a function of erlang
    {From, read} -> % Receive second message
      From!{Current_Sum, current_sum}, % send the current sum to the requesting pid
      servlet(Current_Sum); % restart servlet
    {From, done} -> % Receive second message
      exit(normal) % Destroy this servlet.
  end.


client(S) ->%% S is the PID of the server
  Reference_ID = make_ref(), % make a new reference id
  S!{self(), Reference_ID, start}, % start a new servlet
  receive
    {Ref, Servlet_PID, servlet_pid} -> % receive generated servlet pid
      % Test data
      Random_1 = rand:uniform(),
      Random_2 = rand:uniform(),
      Random_3 = rand:uniform(),
      Servlet_PID!{self(), read},
      receive
        {Current_Sum, current_sum} ->
          io:format("Current sum = ~d~n", Current_Sum),
      end,
      Servlet_PID!{self(), Random_1},
      Servlet_PID!{self(), read},
      receive
        {Current_Sum, current_sum} ->
          io:format("Current sum = ~d~n", Current_Sum),
      end,
      Servlet_PID!{self(), Random_2},
      Servlet_PID!{self(), read},
      receive
        {Current_Sum, current_sum} ->
          io:format("Current sum = ~d~n", Current_Sum),
      end,
      Servlet_PID!{self(), Random_3},
      Servlet_PID!{self(), read},
      receive
        {Current_Sum, current_sum} ->
          io:format("Current sum = ~d~n", Current_Sum),
      end
  end.
