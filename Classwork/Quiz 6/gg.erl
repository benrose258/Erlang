% Ben Rose
% I pledge my honor that I have abided by the Stevens Honor System.

% Guessing Game, i.e. Quiz 6

-module(gg).
-compile(export_all).

% Start the program
start() ->
  Server = spawn( fun server /0),
  spawn(?MODULE,client,[Server]).

server() ->
  receive
    {start,Client_PID} ->
      Servlet = spawn(?MODULE,server_loop,[Client_PID,rand:uniform(10)]),
      Client_PID!{self(),ok,Servlet},
      server()
  end.

client(Server) ->
  Server!{start,self()},
  receive
    {Server,ok,Servlet} ->
      Servlet!{done,self()},
      receive
        {Servlet,Int} ->
          io:format("Done: ~p~s~n",[self(),Int])
        end
  end.
