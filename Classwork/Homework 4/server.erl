% Ben Rose
% I pledge my honor that I have abided by the Stevens Honor System.
% 11/16/2020

-module(server).

-author("Ben Rose").

-export([start_server/0]).

-include_lib("./defs.hrl").

-spec start_server() -> _.
-spec loop(_State) -> _.
-spec do_join(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_leave(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_new_nick(_State, _Ref, _ClientPID, _NewNick) -> _.
-spec do_client_quit(_State, _Ref, _ClientPID) -> _NewState.

start_server() ->
    catch(unregister(server)),
    register(server, self()),
    case whereis(testsuite) of
	undefined -> ok;
	TestSuitePID -> TestSuitePID!{server_up, self()}
    end,
    loop(
      #serv_st{
	 nicks = maps:new(), %% nickname map. client_pid => "nickname"
	 registrations = maps:new(), %% registration map. "chat_name" => [client_pids]
	 chatrooms = maps:new() %% chatroom map. "chat_name" => chat_pid
	}
     ).

loop(State) ->
    receive
	%% initial connection
	{ClientPID, connect, ClientNick} ->
	    NewState =
		#serv_st{
		   nicks = maps:put(ClientPID, ClientNick, State#serv_st.nicks),
		   registrations = State#serv_st.registrations,
		   chatrooms = State#serv_st.chatrooms
		  },
	    loop(NewState);
	%% client requests to join a chat
	{ClientPID, Ref, join, ChatName} ->
	    NewState = do_join(ChatName, ClientPID, Ref, State),
	    loop(NewState);
	%% client requests to join a chat
	{ClientPID, Ref, leave, ChatName} ->
	    NewState = do_leave(ChatName, ClientPID, Ref, State),
	    loop(NewState);
	%% client requests to register a new nickname
	{ClientPID, Ref, nick, NewNick} ->
	    NewState = do_new_nick(State, Ref, ClientPID, NewNick),
	    loop(NewState);
	%% client requests to quit
	{ClientPID, Ref, quit} ->
	    NewState = do_client_quit(State, Ref, ClientPID),
	    loop(NewState);
	{TEST_PID, get_state} ->
	    TEST_PID!{get_state, State},
	    loop(State)
    end.

%% executes '/join' protocol from server perspective
% What steps of the assignment the server executes:
% The server has to do steps 4, 5, 6, and 7
do_join(ChatName, ClientPID, Ref, State) ->
    % /join Step 4
    % Check if the chatroom exists yet
    Chatroom_Exists = maps:is_key(ChatName, State#serv_st.chatrooms),
    if
      Chatroom_Exists == true ->
        % Get the registration map itself
        RegistrationMap = State#serv_st.registrations,
        % Get the chatroom map from the server
        FinalChatroomMap = State#serv_st.chatrooms;
      Chatroom_Exists == false ->
        % If the chatroom doesn't exist yet, spawn a new chatroom with the name ChatName
        Chatroom_PID = spawn(chatroom, start_chatroom, [ChatName]),
        % Then register the name ChatName (in atom form, as required by the register
        % function) to be the PID Chatroom_PID
        % Apparently converting a string to an atom requires the "list_to_atom" function.
        register(list_to_atom(ChatName), Chatroom_PID),
        % Then make it a chatroom with no clients registered in the server's
        % map of every chatroom's registered client PIDs
        RegistrationMap = maps:put(ChatName, [], State#serv_st.registrations),
        % Create final chatroom map to be returned in the state
        FinalChatroomMap = maps:put(ChatName, Chatroom_PID, State#serv_st.chatrooms)
    end,
    % /join Step 5
    % Get the nickname associated with ClientPID
    ClientNick = maps:get(ClientPID, State#serv_st.nicks),
    % /join Step 6
    % Tell the chatroom with the name ChatName (in atom form) that the client is joining it.
    whereis(list_to_atom(ChatName))!{self(), Ref, register, ClientPID, ClientNick},
    % /join Step 7
    % Get the current list of registrations for the chatroom with the name ChatName from
    % the server
    Current_ChatName_Registrations = maps:get(ChatName, RegistrationMap),
    % Update the server's list of clients in the chatroom with the name ChatName by
    % concatenating the ClientPID (in its own singleton list, of course) to the
    % rest of the list of registered PIDs for the chatroom with the name ChatName.
    Updated_ChatName_Registrations = [ClientPID] ++ Current_ChatName_Registrations,
    % Make singleton updated map with the new client PID in it for the chatroom ChatName
    % UpdatedChatNameSingletonMap = maps:from_list([{ChatName, Updated_ChatName_Registrations}]),
    % Update the key-value pair in the map with the chatroom's new list of Client PIDs
    FinalRegistrationMap = maps:update(ChatName, Updated_ChatName_Registrations, RegistrationMap),
    % Return the new Server State
    #serv_st{nicks = State#serv_st.nicks, registrations = FinalRegistrationMap, chatrooms = FinalChatroomMap}.

%% executes '/leave' protocol from server perspective
% What the server has to do in the list of steps:
% The server has to do steps 4, 5, 6, and 7
do_leave(ChatName, ClientPID, Ref, State) ->
    % /leave Step 4
    Chatroom_PID = maps:get(ChatName, State#serv_st.chatrooms),
    % /leave Step 5
    % Retrieve an updated list of registrations for a given chatroom after
    % removing a client from it
    Updated_Registrations = remove_client_from_a_chatroom(ChatName, ClientPID, State#serv_st.registrations),
    % Create the new Server State with the updated chatroom registrations
    UpdatedState = #serv_st{nicks = State#serv_st.nicks, registrations = Updated_Registrations, chatrooms = State#serv_st.chatrooms},
    % /leave Step 6
    Chatroom_PID!{self(), Ref, unregister, ClientPID},
    % /leave Step 7
    ClientPID!{self(), Ref, ack_leave},
    % Return from the Server's do_leave function, i.e. return the UpdatedState
    UpdatedState.

% Remove a client from the Server's local list of registered clients for the chatroom
% with the name ChatName
remove_client_from_a_chatroom(ChatName, ClientPID, Registrations) ->
  % Get the current list of PIDs registered to the chatroom ChatName
  Current_PIDs_Registered_For_A_Chatroom = maps:get(ChatName, Registrations),
  % Create a new list of PIDs registered to the chatroom ChatName after removing
  % (de-concatenating) the ClientPID from the list of Current Client PIDs registered for ChatName
  Updated_PIDs_Registered_For_A_Chatroom = Current_PIDs_Registered_For_A_Chatroom -- [ClientPID],
  % Create a singleton map with the updated PIDs for the chatroom with the name ChatName
  % so that we can overwrite the registered PIDs without trying to update a non-existent
  % entry.
  Single_Chatroom_Updated_Registration = maps:from_list([{ChatName, Updated_PIDs_Registered_For_A_Chatroom}]),
  % Create an updated registration map where, if the map on the right has newer data
  % than the map on the left with the same key, the right map's key-value pair will
  % overwrite its lefthand counterpart's key-value pair. If the map on the left
  % does not contain an entry with an identical key as the right map, all entries in
  % both maps will be merged.
  Updated_Registrations_Map = maps:merge(Registrations, Single_Chatroom_Updated_Registration),

  Updated_Registrations_Map.


%% executes new nickname protocol from server perspective
% What the server has to do in the list of steps:
% The server has to do steps 4, 5, 6, and 7
do_new_nick(State, Ref, ClientPID, NewNick) ->
    % Check if the desired new nickname is currently in use by any of the registered clients
    NewNick_In_Use = lists:member(NewNick, maps:values(State#serv_st.nicks)),
    if
      % /nick Step 4
      % Send the error back to the client and make sure the client receives it
      NewNick_In_Use == true ->
        % Create the error response to send to the client
        Response = {self(), Ref, err_nick_used},
        % Make the potentially updated nickname list equal to the current one.
        Updated_Nickname_List = State#serv_st.nicks;
      % /nick Step 5
      NewNick_In_Use == false ->
        Updated_Nickname_List = maps:update(ClientPID, NewNick, State#serv_st.nicks),
        % /nick Step 6
        chatroom_nickname_updater(State, Ref, maps:keys(State#serv_st.chatrooms), ClientPID, NewNick),
        % Create response to client that we've updated the
        % nicknames in the relevant chatrooms
        Response = {self(), Ref, ok_nick}
    end,
    % Step 4 or 7
    ClientPID!Response,
    UpdatedState = #serv_st{nicks = Updated_Nickname_List, registrations = State#serv_st.registrations, chatrooms = State#serv_st.chatrooms},
    UpdatedState.

% This function is the heart of executing /nick Step 6.
% Variable definitions:
% Server_State = The server state
% Reference_ID = The unique reference ID for this sequence of requested instructions
% Chatroom_Names = The names of all existing chatrooms (i.e. the keys
% of Chatroom_Registrations_Map)
% ClientPID = The ClientPID that we're updating the nickname of
% NewNick = The new nickname for the client with the pid ClientPID
% Function description:
% This function determines if each chatroom contains the client with the pid ClientPID
% and, if so, updates the chatroom's registered nickname for that client with NewNick
chatroom_nickname_updater(Server_State, Reference_ID, Chatroom_Names, ClientPID, NewNick) ->
  case Chatroom_Names of
    [] ->
      % We have finished updating all relevant chatrooms.
      io:format("");
    [Chatroom_Name | Rest_Of_Chatroom_Names] ->
      % We still have at least one chatroom to check.
      % Create a variable holding the map of all existing chatroom registrations in
      % the Server_State
      Chatroom_Registrations_Map = Server_State#serv_st.registrations,
      % Check if the ClientPID is in the list of PIDs in the chatroom with
      % the name Chatroom_Name, i.e. if we need to update Chatroom_Name's
      % nickname list with the new nickname for the client with the pid ClientPID.
      Client_In_This_Chatroom = lists:member(ClientPID, maps:get(Chatroom_Name, Chatroom_Registrations_Map)),
      if
        % If the client is not in the chatroom
        Client_In_This_Chatroom == false ->
          % Recursively call chatroom_nickname_updater over the
          % rest of the Chatroom_Names
          chatroom_nickname_updater(Server_State, Reference_ID, Rest_Of_Chatroom_Names, ClientPID, NewNick);
        % If the client is in the chatroom
        Client_In_This_Chatroom == true ->
          % Look up the PID of the chatroom with the name Chatroom_Name in Server_State
          Chatroom_PID = maps:get(Chatroom_Name, Server_State#serv_st.chatrooms),
          % Send the request to update nickname of the client with the pid ClientPID
          % to be NewNick
          Chatroom_PID!{self(), Reference_ID, update_nick, ClientPID, NewNick},
          % Recursively call chatroom_nickname_updater over the
          % rest of the Chatroom_Names
          chatroom_nickname_updater(Server_State, Reference_ID, Rest_Of_Chatroom_Names, ClientPID, NewNick)
      end % End the if statement
  end. % End the case statement and the function itself

%% executes client quit protocol from server perspective
% What the server has to do in the list of steps:
% The server has to do steps 3.a, 3.b, 3.c, and part of 4.
do_client_quit(State, Ref, ClientPID) ->
    % /quit Step 3.a
    Nicknames_Minus_Quitting_Client = maps:remove(ClientPID, State#serv_st.nicks),
    % /quit Steps 3.b and 3.c
    Updated_Registrations = quit_all_chatrooms(State, Ref, maps:keys(State#serv_st.chatrooms), ClientPID, State#serv_st.registrations),
    % Create the new Server State with the updated chatroom registrations
    UpdatedState = #serv_st{nicks = Nicknames_Minus_Quitting_Client, registrations = Updated_Registrations, chatrooms = State#serv_st.chatrooms},
    % /quit Step 4
    ClientPID!{self(), Ref, ack_quit},
    % Return the updated state
    UpdatedState.

% This function is at the heart of executing '/quit' Steps 3.b and 3.c.
% Variable definitions:
% Server_State = The server state
% Reference_ID = The unique reference ID for this sequence of requested instructions
% Chatroom_Names = The names of all existing chatrooms (i.e. the keys
% of Chatroom_Registrations_Map)
% ClientPID = The ClientPID that we're removing
% Registrations = The current registration map for all chatrooms
% Function description:
% This function determines if each chatroom contains the client with the pid ClientPID
% and, if so, updates the chatroom and server's list of registered clients having deleted
% the client who is quitting from their chatrooms
quit_all_chatrooms(Server_State, Reference_ID, Chatroom_Names, ClientPID, Registrations) ->
  case Chatroom_Names of
  [] ->
    % We have finished updating all relevant chatrooms and servers.
    Registrations;
  [Chatroom_Name | Rest_Of_Chatroom_Names] ->
    % We still have at least one chatroom to check.
    % Check if the ClientPID is in the list of PIDs in the chatroom with
    % the name Chatroom_Name, i.e. if we need to update Chatroom_Name's
    % registered clients to by removing the quitting client and to remove
    % such a client from the corresponding registration map in the server itself
    Client_In_This_Chatroom = lists:member(ClientPID, maps:get(Chatroom_Name, Registrations)),
    if
      % If the client is not in the chatroom
      Client_In_This_Chatroom == false ->
        % Recursively call quit_all_chatrooms over the
        % rest of the Chatroom_Names
        quit_all_chatrooms(Server_State, Reference_ID, Rest_Of_Chatroom_Names, ClientPID, Registrations);
      % If the client is in the chatroom
      Client_In_This_Chatroom == true ->
        % Look up the PID of the chatroom with the name Chatroom_Name in Server_State
        Chatroom_PID = maps:get(Chatroom_Name, Server_State#serv_st.chatrooms),
        % Send the request to unregister the client with the pid ClientPID
        Chatroom_PID!{self(), Reference_ID, unregister, ClientPID},
        % Then update the server registrations for that chatroom to remove the client
        Updated_Server_Registrations = remove_client_from_a_chatroom(Chatroom_Name, ClientPID, Registrations),
        % Recursively call quit_all_chatrooms over the rest of the Chatroom_Names
        % and with the updated server registrations for each chatroom
        quit_all_chatrooms(Server_State, Reference_ID, Rest_Of_Chatroom_Names, ClientPID, Updated_Server_Registrations)
    end % End the if statement
end. % End the case statement and the function itself
