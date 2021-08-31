% Ben Rose
% I pledge my honor that I have abided by the Stevens Honor System.
% 11/16/2020

-module(chatroom).

-author("Ben Rose").

-include_lib("./defs.hrl").

-export([start_chatroom/1]).

-spec start_chatroom(_ChatName) -> _.
-spec loop(_State) -> _.
-spec do_register(_State, _Ref, _ClientPID, _ClientNick) -> _NewState.
-spec do_unregister(_State, _ClientPID) -> _NewState.
-spec do_update_nick(_State, _ClientPID, _NewNick) -> _NewState.
-spec do_propegate_message(_State, _Ref, _ClientPID, _Message) -> _NewState.

start_chatroom(ChatName) ->
    loop(#chat_st{name = ChatName,
		  registrations = maps:new(), history = []}),
    ok.

loop(State) ->
    NewState =
	receive
	    %% Server tells this chatroom to register a client
	    {_ServerPID, Ref, register, ClientPID, ClientNick} ->
		do_register(State, Ref, ClientPID, ClientNick);
	    %% Server tells this chatroom to unregister a client
	    {_ServerPID, _Ref, unregister, ClientPID} ->
		do_unregister(State, ClientPID);
	    %% Server tells this chatroom to update the nickname for a certain client
	    {_ServerPID, _Ref, update_nick, ClientPID, NewNick} ->
		do_update_nick(State, ClientPID, NewNick);
	    %% Client sends a new message to the chatroom, and the chatroom must
	    %% propegate to other registered clients
	    {ClientPID, Ref, message, Message} ->
		do_propegate_message(State, Ref, ClientPID, Message);
	    {TEST_PID, get_state} ->
		TEST_PID!{get_state, State},
		loop(State)
end,
    loop(NewState).

%% This function should register a new client to this chatroom
% Used in '/join' Step 8
do_register(State, Ref, ClientPID, ClientNick) ->
    % Put the key-value pair ClientPID, ClientNick into the map of registered clients in
    % this chatroom.
    NewRegistrationMap = maps:put(ClientPID, ClientNick, State#chat_st.registrations),

    % (This next block of lines is not in the assignment
    % spec, I've just noticed this in various chat mediums
    % and thought I should include it in this one too. The assignment
    % spec stuff resumes after NewHistory is created.)
    Auto_Generated_I_Joined_Message = "AUTOMATICALLY GENERATED: I joined this chatroom.\n",
    Client_Registration_PIDs = maps:keys(maps:remove(ClientPID, NewRegistrationMap)),
    % Send the automated message about the client joining the chatroom to the
    % clients in the chatroom and announce it to the new client as well.
    send_to_receivers(State, Ref, Client_Registration_PIDs, ClientPID, ClientNick, Auto_Generated_I_Joined_Message),
    % Update the chatroom's history to include the new message that we will send
    % to the chatroom automatically saying that we joined the chatroom.
    NewHistory = State#chat_st.history ++ [{ClientNick, Auto_Generated_I_Joined_Message}],


    % Send the connection message from the Chatroom's PID, passing Ref,
    % stating that the chatroom is connecting, and sending the client the current chat history.
    ClientPID!{self(), Ref, connect, NewHistory},
    % Return the updated chatroom state.
    UpdatedState = #chat_st{name = State#chat_st.name, registrations = NewRegistrationMap, history = NewHistory},
    UpdatedState.

%% This function should unregister a client from this chatroom
% What the client has to do in the list of steps:
% The chatroom has to do step 6.a. for '/leave', which has identical instructions to
% '/quit' step 3.b.
do_unregister(State, ClientPID) ->
    % (This next block of lines is not in the assignment
    % spec, I've just noticed this in various chat mediums
    % and thought I should include it in this one too. The assignment
    % spec stuff resumes after NewHistory is created.)
    Auto_Generated_I_Left_Message = "AUTOMATICALLY GENERATED: I left this chatroom.\n",
    Client_Registration_PIDs = maps:keys(maps:remove(ClientPID, State#chat_st.registrations)),
    % Retrieve the nickname of the client who is leaving for the "I_Left" message
    Nickname_Of_Leaving_Client = maps:get(ClientPID, State#chat_st.registrations),
    % Send the automated message about the client leaving the chatroom to the
    % clients in the chatroom. The reference number isn't used in the
    % end result of the send_to_receivers function, so it can be anything.
    send_to_receivers(State, 0, Client_Registration_PIDs, ClientPID, Nickname_Of_Leaving_Client, Auto_Generated_I_Left_Message),
    % Update the chatroom's history to include the new message that we will send
    % to the chatroom automatically saying that we left the chatroom.
    NewHistory = State#chat_st.history ++ [{Nickname_Of_Leaving_Client, Auto_Generated_I_Left_Message}],

    % /leave Step 6.a., which is identical to /quit Step 3.b.
    % Make the Updated_Registrations map of all Client PIDs in the Chatroom
    Updated_Registrations = maps:remove(ClientPID, State#chat_st.registrations),
    % Create the updated state with the Updated_Registrations for this chatroom
    UpdatedState = #chat_st{name = State#chat_st.name, registrations = Updated_Registrations, history = NewHistory},
    UpdatedState.

%% This function should update the nickname of specified client.

% What the client has to do in the list of steps:
% The chatroom has to do step 6 for '/nick'
do_update_nick(State, ClientPID, NewNick) ->
    % (This next block of lines is not in the assignment
    % spec, I've just noticed this in various chat mediums
    % and thought I should include it in this one too. The assignment
    % spec stuff resumes after NewHistory is created.)
    Partial_Auto_Generated_Nickname_Change_Message = "AUTOMATICALLY GENERATED: I have changed my nickname to '",
    New_Nickname_Plus_Period_And_NewLine = string:concat(NewNick, "'.\n"),
    % Create the final message about nickname changing to send to the clients.
    Final_Auto_Generated_Nickname_Change_Message = string:concat(Partial_Auto_Generated_Nickname_Change_Message, New_Nickname_Plus_Period_And_NewLine),
    Client_Registration_PIDs = maps:keys(maps:remove(ClientPID, State#chat_st.registrations)),
    % Retrieve the current nickname of the client who is changing its nickname
    Old_Nickname_For_Client = maps:get(ClientPID, State#chat_st.registrations),
    % Send the automated message about the client changing its nickname to NewNick
    % to the clients in the chatroom. The reference number isn't used in the
    % end result of the send_to_receivers function, so it can be anything.
    send_to_receivers(State, 0, Client_Registration_PIDs, ClientPID, Old_Nickname_For_Client, Final_Auto_Generated_Nickname_Change_Message),
    NewHistory = State#chat_st.history ++ [{Old_Nickname_For_Client, Final_Auto_Generated_Nickname_Change_Message}],
    % /nick Step 6
    % Create the updated Client Nickname Registration Map with the client with
    % the pid ClientPID having its new nickname NewNick.
    Updated_Registrations_Map = maps:update(ClientPID, NewNick, State#chat_st.registrations),
    % Create the updated state with the client with the pid ClientPID's nickname updated
    UpdatedState = #chat_st{name = State#chat_st.name, registrations = Updated_Registrations_Map, history = NewHistory},
    % Return the updated state
    UpdatedState.

%% This function should update all clients in chatroom with new message
%% (read assignment specs for details)
% Instructions for this function are under Receving clients,
% but the initial message is received from the Sending client
% and to reference the Receving clients section to for information on what
% the Chatroom has to do with the message it receives.

% What the client has to do in the list of steps:
% The chatroom has to do steps 1 and 2 for Receving clients
% and step 4 for Sending client
do_propegate_message(State, Ref, ClientPID, Message) ->
    % Retrieve the Nickname of the Sending client by getting the associated
    % nickname to the Sending client's pid (i.e. ClientPID) to
    % use in the send_to_receivers function for Receving clients Step 1
    % and Receiving clients Step 2
    Sending_Client_Nickname = maps:get(ClientPID, State#chat_st.registrations),
    % Remove the Sending Client from the map of registered clients, as we don't want to
    % send Message to the sender of Message. Then isolate the keys, i.e. the PIDs of all
    % of the registered clients instead of a map of the client PIDs to client
    % nicknames (i.e. State#chat_st.registrations), and store that
    % in the variable Registered_Clients_Minus_Sending_Client. This variable
    % is what we're going to use in send_to_receivers, i.e. Receving clients Step 1.
    Registered_Client_PIDs_Minus_Sending_Client = maps:keys(maps:remove(ClientPID, State#chat_st.registrations)),
    % Receving clients Step 1
    send_to_receivers(State, Ref, Registered_Client_PIDs_Minus_Sending_Client, ClientPID, Sending_Client_Nickname, Message),
    % send_to_receivers(State, Ref, State#chat_st.registrations, ClientPID, Sending_Client_Nickname, Message),
    % Receving clients Step 2
    New_History = State#chat_st.history ++ [{Sending_Client_Nickname, Message}],
    % Update the State with the new chat history
    UpdatedState = #chat_st{name = State#chat_st.name, registrations = State#chat_st.registrations, history = New_History},
    % Sending client Step 4
    ClientPID!{self(), Ref, ack_msg},
    % Return the UpdatedState
    UpdatedState.

% This function is the heart of executing Receving clients Step 1.
% Variable definitions:
% Chatroom_State = The chatroom state
% Reference_ID = The unique reference ID for this sequence of requested instructions
% Client_Registration_PIDs = The PIDs of all currently connected clients (i.e. the values
% of Chatroom_State#chat_st.registrations) except for the PID of the Sending client
% Sending_Client_PID = The Client PID that is sending Message
% to the rest of the clients
% CliNick = The name of the client with the pid Sending_Client_PID
% Message = The message that the client with the pid Sending_Client_PID is sending
% to the other clients in the chatroom.
% Function description:
% This function sends the message Message to all clients in Client_Registrations
% if their pid is not Sending_Client_PID, i.e. if they are not sending the message
% themselves.
send_to_receivers(Chatroom_State, Reference_ID, Client_Registration_PIDs, Sending_Client_PID, CliNick, Message) ->
  case Client_Registration_PIDs of
    [] ->
      % We have finished sending Message to all Receiving clients
      io:format("");
    [Current_Client_PID | Other_Registered_Clients] ->

      % Send CliNick, the chatroom's name, and Message
      % to the client with the pid Current_Client_PID
      Current_Client_PID!{request, self(), Reference_ID, {incoming_msg, CliNick, Chatroom_State#chat_st.name, Message}},
      % Recursively call send_to_receivers over the rest of the registered receivers
      send_to_receivers(Chatroom_State, Reference_ID, Other_Registered_Clients, Sending_Client_PID, CliNick, Message)
    end. % End the case statement and the function itself.
