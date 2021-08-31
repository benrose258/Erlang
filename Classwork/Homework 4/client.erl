% Ben Rose
% I pledge my honor that I have abided by the Stevens Honor System.
% 11/16/2020, due 11/29/2020

-module(client).

-author("Ben Rose").

-export([main/1, initial_state/2]).

-include_lib("./defs.hrl").

-spec main(_InitialState) -> _.
-spec listen(_State) -> _.
-spec initial_state(_Nick, _GuiName) -> _InitialClientState.
-spec loop(_State, _Request, _Ref) -> _.
-spec do_join(_State, _Ref, _ChatName) -> _.
-spec do_leave(_State, _Ref, _ChatName) -> _.
-spec do_new_nick(_State, _Ref, _NewNick) -> _.
-spec do_new_incoming_msg(_State, _Ref, _SenderNick, _ChatName, _Message) -> _.

%% Receive messages from GUI and handle them accordingly
%% All handling can be done in loop(...)
main(InitialState) ->
    %% The client tells the server it is connecting with its initial nickname.
    %% This nickname is guaranteed unique system-wide as long as you do not assign a client
    %% the nickname in the form "user[number]" manually such that a new client happens
    %% to generate the same random number as you assigned to your client.
    whereis(server)!{self(), connect, InitialState#cl_st.nick},
    %% if running test suite, tell test suite that client is up
    case whereis(testsuite) of
	undefined -> ok;
	TestSuitePID -> TestSuitePID!{client_up, self()}
    end,
    %% Begins listening
    listen(InitialState).

%% This method handles all incoming messages from either the GUI or the
%% chatrooms that are not directly tied to an ongoing request cycle.
listen(State) ->
    receive
        {request, From, Ref, Request} ->
	    %% the loop method will return a response as well as an updated
	    %% state to pass along to the next cycle
            {Response, NextState} = loop(State, Request, Ref),
	    case Response of
		{dummy_target, Resp} ->
		    io:format("Use this for whatever you would like~n"),
		    From!{result, self(), Ref, {dummy_target, Resp}},
		    listen(NextState);
		%% if shutdown is received, terminate
		shutdown ->
		    ok_shutdown;
		%% if ok_msg_received, then we don't need to reply to sender.
		ok_msg_received ->
		    listen(NextState);
		%% otherwise, reply to sender with response
		_ ->
		    From!{result, self(), Ref, Response},
		    listen(NextState)
	    end
    end.

%% This function just initializes the default state of a client.
%% This should only be used by the GUI. Do not change it, as the
%% GUI code we provide depends on it.
initial_state(Nick, GUIName) ->
    #cl_st { gui = GUIName, nick = Nick, con_ch = maps:new() }.

%% ------------------------------------------
%% loop handles each kind of request from GUI
%% ------------------------------------------
loop(State, Request, Ref) ->
    case Request of
	%% GUI requests to join a chatroom with name ChatName
	{join, ChatName} ->
	    do_join(State, Ref, ChatName);

	%% GUI requests to leave a chatroom with name ChatName
	{leave, ChatName} ->
	    do_leave(State, Ref, ChatName);

	%% GUI requests to send an outgoing message Message to chatroom ChatName
	{outgoing_msg, ChatName, Message} ->
	    do_msg_send(State, Ref, ChatName, Message);

	%% GUI requests the nickname of client
  % Assignment Spec Part 3.4: "/whoami"
	whoami ->
      % Retrieve current nickname
      Nickname = State#cl_st.nick,
      % Create the response
      Response = {result, self(), Ref, Nickname},
      % Send the response to the GUI
      whereis(list_to_atom(State#cl_st.gui))!Response,
      % Return {Response, NextState} to the "listen" function
      {Response, State};
	    % {{dummy_target, dummy_response}, State};

	%% GUI requests to update nickname to Nick
	{nick, Nick} ->
            do_new_nick(State, Ref, Nick);

	%% GUI requesting to quit completely
	quit ->
	    do_quit(State, Ref);

	%% Chatroom with name ChatName has sent an incoming message Message
	%% from sender with nickname SenderNick
	{incoming_msg, SenderNick, ChatName, Message} ->
	    do_new_incoming_msg(State, Ref, SenderNick, ChatName, Message);

	{get_state} ->
	    {{get_state, State}, State};

	%% Somehow reached a state where we have an unhandled request.
	%% Without bugs, this should never be reached.
	_ ->
	    io:format("Client: Unhandled Request: ~w~n", [Request]),
	    {unhandled_request, State}
    end.

%% executes `/join` protocol from client perspective
% If such a chatroom doesn't exist, the server must create one.
% Otherwise, join the chatroom and have the server tell the chatroom that the client wants to join it
% and update the list of chatroom members after joining.
% The chatroom has to tell the client about it, whether it was just created or
% already exists, once the client joins.

% If the client is in the specified chatroom, send an error message.
% Else, continue.
% Client asks to join the specified chatroom, so it requests to do so from the server.
% Once the server gets the request, it checks if the chatroom exists. If it
% doesn't already exist, the server needs to spawn the chatroom.
% Then the server looks up the requesting client nickname from its state record (serv_st)
% Once the chatroom is either created or found, the server needs to tell the chatroom that
% the requesting client is joining.
% Server updates its own record for chatroom registrations to include the new change with the requesting client.
% Actually, for the rest, just refer to the "/join" section of the assignment spec, pages 6 and 7.

% What the client has to do in the list of steps:
% The client has to do steps 2, 3, half of step 8, and 9
do_join(State, Ref, ChatName) ->
    % Step 2 in do_join
    % This will be false if the chatroom hasn't yet been joined. Else it will be true
    Chatroom_Joined = maps:is_key(ChatName,State#cl_st.con_ch),
    if
      % /join Step 2
      Chatroom_Joined == true -> % If we're already in the chatroom
        % Return the following tuple which will be sent to the GUI by the "loop" function
        % {{result, self(), Ref, err}, State};
        % Because we haven't updated the state in this error, we bind UpdatedState to State
        UpdatedState = State,
        % Because we reached an error and have joined this chatroom already,
        % the final map of connected channels will remain the same as it was.

        % Create response to send to the GUI and "listen" function
        Response = {result, self(), Ref, err};

      % /join Step 3
      Chatroom_Joined == false -> % we're not in the chatroom
        % io:format("Proceed, we aren't in the chatroom"),
        % Send the tuple message requested in step 3 to the server, wherever it may be
        % (or, more literally, what PID is associated with the registered name "server")
        whereis(server)!{self(), Ref, join, ChatName},

        % /join Step 8 (second half of it)
        % Await receiving this message from the Chatroom Process
        % with the chatroom's PID, Reference ID for this sequence, "connect",
        % and the chat history.
        receive
          % Upon receving the message
          {ChatroomPID, Ref, connect, History} ->
              % Print message to console (used during debugging)
              % /join Step 9

              % Update the client's map containing the registered chatrooms
              FinalConnectedChannels = maps:put(ChatName, ChatroomPID, State#cl_st.con_ch),

              % Create the Updated State for the client
              UpdatedState = #cl_st{gui = State#cl_st.gui, nick = State#cl_st.nick, con_ch = FinalConnectedChannels},

              % Create response to send to the GUI and "listen" function
              % Response = {result, self(), Ref, NewHistory},
              Response = {result, self(), Ref, History}
        end
    end,

    % Send the response data to the GUI. We have to convert this from a string
    % to an atom using the "list_to_atom" command to satisfy the input requirements
    % for the "whereis" function.
    whereis(list_to_atom(State#cl_st.gui))!Response,

    {Response, UpdatedState}.

% What the client has to do in the list of steps:
% The client has to do steps 2, 3, part of 7, 8, and 9
do_leave(State, Ref, ChatName) ->
    % Create a variable for the GUI's PID so you don't have to keep retyping that line.
    GUI_PID = whereis(list_to_atom(State#cl_st.gui)),
    % Is the client in the list of chatrooms that you're connected to?
    Connected_To_Chatroom = maps:is_key(ChatName, State#cl_st.con_ch),
    if
      % /leave Step 2
      % If we're not connected to the chatroom we want to leave
      Connected_To_Chatroom == false ->
        % Create the updated list of connected chatrooms to be the same as before,
        % since we didn't actually change the variable.
        FinalConnectedChannels = State#cl_st.con_ch,
        % Create the error response
        Response = {result, self(), Ref, err};
      % /leave Step 3
      % If we are already connected to the chatroom
      Connected_To_Chatroom == true ->
        whereis(server)!{self(), Ref, leave, ChatName},
        % /leave Step 7
        % Wait to receive the ack_leave message from the server
        receive
          % Upon receving the message from the server
          % The _Server_PID variable is unused, so the "_" goes before it
          {_Server_PID, Ref, ack_leave} ->
            % /leave Step 8
            % Update connected channels (chatrooms) map by removing
            % the chatroom with the name ChatName from the connected chatrooms
            FinalConnectedChannels = maps:remove(ChatName, State#cl_st.con_ch),
            % Create the Response
            Response = {result, self(), Ref, ok}
        end % End the receive block
    end,

    % /leave Step 2 or 9
    % Send the GUI the response
    GUI_PID!Response,
    % Create the Updated State
    UpdatedState = #cl_st{gui = State#cl_st.gui, nick = State#cl_st.nick, con_ch = FinalConnectedChannels},
    % Return the response and updated state.
    {Response, UpdatedState}.

%% executes `/nick` protocol from client perspective
% Description of "/nick new_nickname" on page 8

% What the client has to do in the list of steps:
% The client has to do steps 2, 3, part of 4 (maybe), part of 7, and 8.
do_new_nick(State, Ref, NewNick) ->
    % Create a variable for the GUI's PID so you don't have to keep retyping that line.
    GUI_PID = whereis(list_to_atom(State#cl_st.gui)),
    Current_Nickname = State#cl_st.nick,
    if
      % /nick Step 2
      NewNick == Current_Nickname ->
        Response = {result, self(), Ref, err_same};
      % /nick Step 3
      NewNick =/= Current_Nickname ->
        whereis(server)!{self(), Ref, nick, NewNick},
        % /nick Step 4 or 7
        receive
          % /nick Step 4
          % Upon receving an error message from the server
          % The _Server_PID variable is unused, so the "_" goes before it
          {_Server_PID, Ref, err_nick_used} ->
            % Make the response an error that the nickname is in use by another client
            Response = {result, self(), Ref, err_nick_used};
          % /nick Step 7
          % Upon receving the success message from the server
          % The _Server_PID variable is unused, so the "_" goes before it
          {_Server_PID, Ref, ok_nick} ->
            % Create the Response
            Response = {result, self(), Ref, ok_nick}
        end % End the receive statement
    end,
    % /nick Step 2, 4, or 8
    GUI_PID!Response,
    % Create the updated state with the new nickname. The reason that I didn't do it inside
    % the "if" block is that the error step, Step 2, says that the current nickname is the
    % same as the new nickname, so creating a state where nick = Current_Nickname is identical
    % to nick = NewNick, since Current_Nickname == NewNick.
    UpdatedState = #cl_st{gui = State#cl_st.gui, nick = NewNick, con_ch = State#cl_st.con_ch},
    % Return the response and updated state.
    {Response, UpdatedState}.

%% executes send message protocol from client perspective
% Description of "Send a Message->Sending client" on page 9

% What the client has to do in the list of steps:
% The sending client has to do steps 2, 3, part of 4, and 5
do_msg_send(State, Ref, ChatName, Message) ->
    % Sending client Step 2
    Chatroom_PID = maps:get(ChatName, State#cl_st.con_ch),
    % Sending client Step 3
    Chatroom_PID!{self(), Ref, message, Message},
    % Sending client Step 4
    receive
      % The _Received_PID_From_Chatroom variable is unused, so the "_" goes before it
      {_Received_PID_From_Chatroom, Ref, ack_msg} ->
        % Create a variable for the GUI's PID so you don't have
        % to keep retyping that line.
        GUI_PID = whereis(list_to_atom(State#cl_st.gui)),
        % Sending client Step 5
        Response = {result, self(), Ref, {msg_sent, State#cl_st.nick}},
        GUI_PID!Response
    end,
    {Response, State}.

%% executes new incoming message protocol from client perspective
% Description of "Send a Message->Receiving client(s)" on pages 9 and 10

% What the client has to do in the list of steps:
% 3, and the explanation for the code below is in the assignment spec on page 10.
% This is already given by default.
do_new_incoming_msg(State, _Ref, CliNick, ChatName, Msg) ->
    %% pass message along to gui
    gen_server:call(list_to_atom(State#cl_st.gui), {msg_to_GUI, ChatName, CliNick, Msg}),
    {ok_msg_received, State}.

%% executes quit protocol from client perspective
% Description of "/quit" on pages 10 and 11

% What the client has to do in the list of steps:
% The client has to do steps 2, part of 4, 5, and 6.
do_quit(State, Ref) ->
    % /quit Step 2
    whereis(server)!{self(), Ref, quit},
    % /quit Step 4
    receive
      % The _Server_PID variable is unused, so the "_" goes before it
      {_Server_PID, Ref, ack_quit} ->
        % /quit Step 5
        % Send the quit message to the GUI
        whereis(list_to_atom(State#cl_st.gui))!{self(), Ref, ack_quit}
    end,
    % /quit Step 6
    % Terminate the process normally.
    exit(normal).
