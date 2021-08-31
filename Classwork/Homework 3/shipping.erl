%% Ben Rose
%% Homework 3
%% I pledge my honor that I have abided by the Stevens Honor System.

-module(shipping).
-author("Ben Rose").
-compile(export_all).
-include_lib("./shipping.hrl").


get_ship(Shipping_State, Ship_ID) ->
  if
    % If the specified Shipping ID is at least 1,
    % then it is potentially valid and we can proceed to the helper function
    Ship_ID >= 1 ->
      % Call get_ship_helper of...
      % "Shipping_State": the shipping state variable defined in the top
      % of the get_ship function
      % "#": notify the program that
        % "shipping_state": enter the record shipping_state as defined
      % in the shipping.hrl header file
      % ".ships": access the field 'ships', which is a list, as defined
      % in the record definition of shipping state.
      get_ship_helper(Shipping_State#shipping_state.ships, Ship_ID);
    % Else, the specified Shipping ID is < 1, and is invalid,
    % as Shipping IDs start at 1
    true ->
      error
  end.

% Helper function for get_ship
get_ship_helper(Ships, Ship_ID) ->
  case Ships of
    [] ->
      error;
    % Case with Ship = head of the list and Other_Ships = everything else
    [Ship | Other_Ships] ->
      if
        % If the ship id of the ship we're evaluating is equal to the
        % requested ship id
        Ship#ship.id == Ship_ID ->
          Ship; % Print the ship at the matching Ship_ID
        true -> % Else
          % Do a recursive call to get_ship_helper over the rest of
          % the list (i.e. Other_Ships, defined at the top of this case)
          % with the same Ship_ID to search for.
          get_ship_helper(Other_Ships, Ship_ID)
      end
  end.

% Gets the value of the container whose container id == Container_ID,
% or returns an error if a container with Container_ID doesn't exist.
get_container(Shipping_State, Container_ID) ->
  % Call get_container_helper with the "containers" in the specified shipping state and the
  % desired Container ID, using the period at the end to end the get_container function.
  get_container_helper(Shipping_State#shipping_state.containers, Container_ID).

get_container_helper(Containers, Container_ID) ->
  case Containers of % Initialize "case" statement
    [] -> % If the list is empty (i.e. the Container_ID is not found)
      error; % Print "error".
    [Container | Other_Containers] ->
      if
        Container#container.id == Container_ID -> % If we found the correct container
          Container; % Print the correct container out
        % If the Container's (i.e. head of the list) specified container id != Container_ID
        Container#container.id =/= Container_ID ->
          get_container_helper(Other_Containers, Container_ID); % Recurse over the rest of the containers
        true -> % Else, we've encountered an impossible error.
            % Say that we got such an error.
            io:format("unknown error in get_container")
      end
  end.

% Get the Port whose ID matches the specified "Port_ID"
get_port(Shipping_State, Port_ID) ->
    % Call the get_port_helper function with "Shipping_State"'s ports and the specified "Port_ID"
    get_port_helper(Shipping_State#shipping_state.ports, Port_ID). % Then use a "." to end "get_port"

get_port_helper(Ports, Port_ID) ->
  case Ports of % Initialize "case" statement
    [] -> % If the Ports list is empty (i.e. the Port_ID is not found)
      error; % Print "error".
    [Port | Other_Ports] -> % "Port" is the head of the list, "Other_Ports" is everything else in the list
      if
        Port#port.id == Port_ID -> % If "Port"'s id is equal to "Port_ID"
          Port; % Print out the "Port" that we just located
        Port#port.id =/= Port_ID -> % If "Port"'s id is NOT equal to "Port_ID"
          get_port_helper(Other_Ports, Port_ID) % Call get_port_helper on the rest of the list
      end
  end.

% Returns a list of all occupied docks a=for a given port,
% or the empty list if the port doesn't exist.
get_occupied_docks(Shipping_State, Port_ID) ->
    % The variable "Port" will either be bound to "error" or the Port with the id "Port_ID".
    Port = get_port(Shipping_State, Port_ID),
    if
      Port == error -> % If the port does not exist, then "get_port" bound "error" to "Port"
        []; % Print an empty list, as specified in the Homework 3 Assignment doc.
      Port =/= error -> % If the port exists, i.e. "get_port" found a port with the id "Port_ID"
        get_occupied_docks_helper(Shipping_State#shipping_state.ship_locations, Port_ID)
    end.

% Gets all occupied docks for a given "Port_ID"
% This looks in the "ship_locations" field of a "shipping_state", since that's where
% the ships at certain docks are listed. Their format is:
%% {port, dock, ship}
get_occupied_docks_helper(Ship_Locations, Port_ID) ->
    case Ship_Locations of % Initialize case statement
      [] -> % If the ship_locations is empty, or we've emptied it
        []; % Return (or concatenate) the empty list.
      % "Ship_Location" is the head of the "Ship_Locations" list,
      % "Other_Ship_Locations" is everything else in the "Ship_Locations" list
      [Ship_Location | Other_Ship_Locations] ->
        % Create a list containing the list form of the tuple "Ship_Location"
        % so we can search "Ship_Location"'s elements.
        Ship_Location_List_Form = tuple_to_list(Ship_Location),
        % Breaking the next uncommented statement down:
        % Retrieve the "1"st element in "Ship_Location_List_Form" to check later against "Port_ID"
        First_Element_of_Ship_Location_List_Form = lists:nth(1,Ship_Location_List_Form),
        if
          % If the current ship that we are checking is at the "Port_ID" that we're looking for
          First_Element_of_Ship_Location_List_Form == Port_ID ->
            % add a list with the middle element (i.e. the dock occupied by that ship)
            % to a recursive call on Other_Ship_Locations.
            [lists:nth(2,Ship_Location_List_Form)] ++ get_occupied_docks_helper(Other_Ship_Locations, Port_ID);
          % Else, the current ship that we are checking is not at the "Port_ID" that we're looking for.
          First_Element_of_Ship_Location_List_Form =/= Port_ID ->
            % Call get_occupied_docks_helper over the rest of the list
            get_occupied_docks_helper(Other_Ship_Locations, Port_ID)
        end
    end.

% Gets the location of the ship with the id "Ship_ID", or returns "error" if such a ship doesn't exist
get_ship_location(Shipping_State, Ship_ID) ->
    % Call helper function
    Final_List = get_ship_location_helper(Shipping_State#shipping_state.ship_locations, Ship_ID),
    if % Initialize "if" statement
      Final_List == [] -> % If the helper function returns an empty list
        error; % Print "error"
      Final_List =/= [] -> % If the helper function returns a non-empty list
        list_to_tuple(Final_List) % Make the "list" into a "tuple" and print the result.
    end.

get_ship_location_helper(Ship_Locations, Ship_ID) ->
  case Ship_Locations of % Initialize "case" statement
    [] -> % If the ship_locations is empty, or we've emptied it
      []; % Return (or concatenate) the empty list.
    % "Ship_Location" is the head of the "Ship_Locations" list,
    % "Other_Ship_Locations" is everything else in the "Ship_Locations" list
    [Ship_Location | Other_Ship_Locations] ->
      % Create a list containing the list form of the tuple "Ship_Location"
      % so we can search "Ship_Location"'s elements.
      Ship_Location_List_Form = tuple_to_list(Ship_Location),
      % Breaking the next uncommented statement down:
      % Retrieve the "3"rd element in "Ship_Location_List_Form" to check later against "Ship_ID"
      Third_Element_of_Ship_Location_List_Form = lists:nth(3,Ship_Location_List_Form),
      if % Initialize "if" statement
        % If the current ship that we are checking is the "Ship_ID" that we're looking for
        Third_Element_of_Ship_Location_List_Form == Ship_ID ->
          % Return a list (that will be converted into a tuple later) with
          % the Port_ID (i.e. the first element of the Ship_Location_List_Form)
          % concatenated with the Dock_ID (i.e. the second element of the Ship_Location_List_Form)
          [lists:nth(1,Ship_Location_List_Form)] ++ [lists:nth(2,Ship_Location_List_Form)];
        % Else, the current ship that we are checking is not the "Ship_ID" that we're looking for.
        Third_Element_of_Ship_Location_List_Form =/= Ship_ID ->
           % Call get_ship_location_helper on the rest of the list
          get_ship_location_helper(Other_Ship_Locations, Ship_ID)
      end % End the "if" statement
  end. % End the "case" statement and the program itself

% Find the total weight of the container ids listed in the list "Container_IDs"
% and return that sum, or an error if any container ID does not exist.
get_container_weight(Shipping_State, Container_IDs) ->
    % Helper function to test if all of the container IDs in the "Container_IDs" list are
    % valid containers in the provided "Shipping_State". This function cannot be replaced by
    % the get_container_weight_helper function, since that function uses recursion over integers
    % to determine to total weight of the listed containers, and an integer added to a string
    % would throw an exception, since you can't add a string to the sum of numbers.
    Is_Valid_Container_ID_List = valid_container_id_list(Shipping_State#shipping_state.containers, Container_IDs),
    if % Initialize "if" statement
      % If the "Container_IDs" list contains at least one bad container id
      Is_Valid_Container_ID_List == false ->
        error; % Print "error".
      % If the "Container_IDs" list is a valid list of container id's
      Is_Valid_Container_ID_List == true ->
        % Print out the total weight of all containers whose id's are in "Container_IDs"
        get_container_weight_helper(Shipping_State#shipping_state.containers, Container_IDs)
    end. % End the "if" statement and the function itself.

    % io:format("Implement me!!"),
    % error.

get_container_weight_helper(Containers, Container_IDs) ->
  % Begin recursing over all of the "Container_IDs" and return a list with the weight of each container
  case Container_IDs of
    % If the list is empty, then we've successfully added and should add 0 and stop recursing.
    [] ->
      % [];
      0;
    % Else, the list isn't empty, so add the weight of the head to every other container listed
    % in "Container_IDs"
    [Container_ID | Other_Container_IDs] ->
      % Getting the container in list form so we can access the
      % weight of the container, i.e. the third element of the retrieved tuple,
      % and using the get_container_helper to get the actual container itself.
      Container_List_Form = tuple_to_list(get_container_helper(Containers,Container_ID)),
      % Add the third element in the "container" tuple (i.e. the container's weight)
      % to the total container weights, and again notice the comma at the end of the statement
      % like we explained above.
      % Container_Weight = lists:nth(3,Container_List_Form),
      % [Container_Weight] + get_container_weight_helper(Containers, Other_Container_IDs)
      lists:nth(3,Container_List_Form) + get_container_weight_helper(Containers, Other_Container_IDs)
    end. % End the cases and the function itself.

% Determines if every ID in "Container_IDs" has a corresponding
% container in "Containers", i.e. "Shipping_State#shipping_state.containers".
valid_container_id_list(Containers, Container_IDs) ->
  % Begin iterating over all of the "Container_IDs" and check if they exist in
  % our list of "Containers" provided from the shipping state.
  case Container_IDs of
    % If "Container_IDs" == [], then every container id in the "Container_ID" list was found...
    [] ->
      true; % Return "true", indicating that every container in Container_IDs is valid.
    % Else, check the "Container_ID" at the head of the "Container_Ids" list to
    % see if it's a valid container id.
    [Container_ID | Other_Container_IDs] ->
      % Use the get_container_helper function to see if the id "Container_ID" is valid.
      % This function will either return "error" or the container
      % with the id "Container_ID". However, we aren't interested in what the container actually
      % is, just if it gives "error" or not.
      Is_Valid_Container_ID = get_container_helper(Containers, Container_ID),
      if
        % If we get back an error, meaning that a container with the id "Container_ID"
        % does not exist in "Containers"
        Is_Valid_Container_ID == error ->
          % Then return "false", indicating that the list "Container_IDs" contains
          % at least one invalid container id and is therefore not a valid list of container id's.
          false;
        % If we do not get back "error" from the get_container_helper function,
        % then the id from "Container_IDs" that we're currently evaluating
        % corresponds to some existing container in "Containers".
        Is_Valid_Container_ID =/= error ->
          % So we call valid_container_id_list over the "Other_Container_IDs" in "Container_IDs".
          valid_container_id_list(Containers, Other_Container_IDs)
      end % End the if statement
    end. % End the case statement and the function itself


get_ship_weight(Shipping_State, Ship_ID) ->
    Ship_With_Specified_ID = get_ship(Shipping_State, Ship_ID),
    if % Initialize "if" statement
      % If a ship corresponding to the id "Ship_ID" does not
      % exist (i.e. get_ship would return "error")
      Ship_With_Specified_ID == error ->
        error; % Print "error".
      % Else, get and print the sum of the weights of all the containers on the ship with the id "Ship_ID"
      Ship_With_Specified_ID =/= error ->
        % Call "get_container_weight" with parameters "Shipping_State"
        % and, using "Shipping_State"s "ship_inventory" 'maps' (ship_inventory is a 'maps' that
        % uses a ship's id as the key and the containers on the ship as the corresponding
        % value) get the value associated with the key "Ship_ID",
        % which will be the containers on the ship with the corresponding ID.
        get_container_weight(Shipping_State, maps:get(Ship_ID,Shipping_State#shipping_state.ship_inventory))
    end.

% Has many helper functions, but returns a shipping state loading the
% containers listed in "Container_IDs" onto a provided ship, if possible.
% We need to see if the ship location (i.e. what port the ship is at) is at the proper port, then check the
% location of every container in the list of "Container_IDs" to see if it's the same
% as the port that the ship is at. If even one container is in the wrong place, then
% return an error. Else, check if the number of containers that might be loaded onto the
% ship is not greater than the ((ship's capacity) - (containers already on the ship)). If
% it is, then return error. Else, move all of the containers in the "Container_IDs" to the
% specified ship. This requires reconstructing the port inventory and the ship inventory
% to account for the change in the container locations for both, so helper functions are used for each
% of those things. Then use the current "ships", "containers", "ports", and "ship_locations" that
% are in the current shipping state and the new "ship_inventory" and the new "port_inventory", which
% were created from their helper functions, to complete the new "Shipping_State". Then return the
% new "Shipping_State".
load_ship(Shipping_State, Ship_ID, Container_IDs) ->
    % First, check if "Container_IDs" contains any non-existent container id's.
    Valid_Container_ID_List = valid_container_id_list(Shipping_State#shipping_state.containers, Container_IDs),
    if
      % If at least one of the container id's in the list "Container_IDs" is not a valid container id
      Valid_Container_ID_List == false ->
        error; % Return "error"
      % If all id's in the "Container_IDs" are valid container id's, then continue
      Valid_Container_ID_List == true ->
        % Checks if the containers in the list of containers with the id's listed in "Container_IDs"
        % are all located at ports. "Containers_At_Ports" will return "true" if they are all at ports
        % and false otherwise.
        Containers_At_Ports = containers_are_at_ships_or_ports(Shipping_State#shipping_state.port_inventory,Container_IDs),
        if
          % If either the containers do not exist or are not all at ports
          Containers_At_Ports == false ->
            error; % Return "error", since all containers must exist and be at ports.
          % If all of the containers in the list of id's "Container_IDs" exist and are at ports
          Containers_At_Ports == true ->
            % "Container_Ports" uses the value from the function "container_ports" to
            % determine the lists of all ports that the containers in "Container_IDs" are located.
            Container_Ports = container_ports_or_ships(Shipping_State#shipping_state.port_inventory,Container_IDs),
            % Use the list_length function to determine if the containers are at
            % different ports (i.e. "Number_Of_Container_Ports" > 1),
            % no containers were given in the list of "Container_IDs" (i.e. "Number_Of_Container_Ports" == 0)
            % or all located at the same port (i.e. "Number_Of_Container_Ports" == 1)
            Number_Of_Container_Ports = list_length(Container_Ports),
            if
              % If the listed containers are at different ports (i.e. "Number_Of_Container_Ports" > 1)
              % or no containers were listed in "Container_IDs"
              Number_Of_Container_Ports =/= 1 ->
                % Return "error", since all containers must be at the same port as the ship with the id "Ship_ID"
                error;
              % If all of the containers are at the same port
              Number_Of_Container_Ports == 1 ->
                % Get the specified ship, or an error if the ship does not exist
                Specified_Ship = get_ship(Shipping_State, Ship_ID),
                if
                  % If the ship with the id "Ship_ID" does not exist
                  Specified_Ship == error ->
                    error; % Return "error"
                  % Else, the ship with the id "Ship_ID" exists
                  Specified_Ship =/= error ->
                    % Find the port of the ship, i.e. the first value in "get_ship_location",
                    % by converting the resulting tuple from "get_ship_location" into a list,
                    % then retrieving the first value, i.e. the port of the ship with id "Ship_ID"
                    % Since we did the check if the ship with id "Ship_ID" exists just before this,
                    % checking if a ship with the id "Ship_ID" will not throw an
                    % exception for a non-existant ship
                    Ships_Port = lists:nth(1,tuple_to_list(get_ship_location(Shipping_State, Ship_ID))),
                    % The container port, since we've determined all containers are at the same port
                    % and that the "Container_IDs" list is not empty, is now defined to be checked against
                    % the "Ships_Port", since we've made sure that all of the containers exist and are
                    % at the same port to prevent possible exceptions.
                    Container_Port = lists:nth(1,Container_Ports),
                    if
                      % If the ship is not at the same port as the containers
                      Ships_Port =/= Container_Port ->
                        % Return "error", since the ship and all containers must be at the same port
                        error;
                      % Else, the ship is at the same port as all of the containers.
                      Ships_Port == Container_Port ->
                        % Create a uniquified list of container id's to check if all of the supplied containers
                        % are indeed unique. If not, we have received erroneous input for our "Container_IDs" list
                        Uniquified_Container_List = uniquify(Container_IDs),
                        % The original number of containers to load
                        Number_Of_Containers_To_Load = list_length(Container_IDs),
                        % The length of the uniquified list of containers
                        Length_Of_Uniquified_Container_List = list_length(Uniquified_Container_List),
                        if
                          % If the length of the original "Container_IDs" list is not equal to the
                          % length of the uniquified "Container_IDs" list
                          Number_Of_Containers_To_Load =/= Length_Of_Uniquified_Container_List ->
                            % We have received erroneous input for our "Container_IDs" list and must return an error.
                            error;
                          % If the unique list of "Container_IDs" is equivalent to the original number
                          % of elements in "Container_IDs", then continue
                          Number_Of_Containers_To_Load == Length_Of_Uniquified_Container_List ->

                            % Get the ship that we are planning to load cargo onto
                            % in list form so we can search its elements
                            Specified_Ship_List_Form = tuple_to_list(get_ship(Shipping_State, Ship_ID)),
                            % Get the original list of containers on the ship to check the capacity and add to the list of containers
                            Specified_Ship_Original_Container_List = maps:get(Ship_ID,Shipping_State#shipping_state.ship_inventory),
                            % Length of the containers on the specified ship before loading
                            Length_Of_Original_Ship_Container_List = list_length(Specified_Ship_Original_Container_List),
                            % Maximum capacity of the specified ship, i.e. the fourth element in the value returned by get_ship
                            Ship_Max_Capacity = lists:nth(4,Specified_Ship_List_Form),
                            % Get the remaining capacity of the ship before attempting to load new containers using the
                            % ship's "container_cap" (container capacity)
                            % minus the current number of containers on the ship right now
                            % Capacity_Before_Loading = list_length(lists:duplicate(Ship_Max_Capacity,0)) - Length_Of_Original_Ship_Container_List,
                            Capacity_Before_Loading = Ship_Max_Capacity - Length_Of_Original_Ship_Container_List,
                            % Get what the capacity of the ship that we're loading will be after
                            % we load it with the containers specified in the "Container_IDs"
                            Capacity_After_Loading = Capacity_Before_Loading - Number_Of_Containers_To_Load,
                            % Capacity_After_Loading = Capacity_Before_Loading - list_length(Container_IDs),

                            if
                              % If loading the ships with the requested cargo puts the ship over capacity
                              Capacity_After_Loading < 0 ->
                                error; % Print an error, as a ship cannot be loaded over its capacity
                              % Else, the ship can be loaded.
                              Capacity_After_Loading >= 0 ->
                                % Now the ship can be loaded, because the required conditions to do
                                % so (i.e. all of the "if" statements above), have been satisfied.
                                % The conditions satisfied are:
                                % 1. The list of "Container IDs" supplied all exist in the "Shipping_State"
                                % 2. All of the containers are located at ports, i.e. none of the
                                %    containers listed are on ships
                                % 3. All of the containers are located at the same port
                                % 4. The ship with the id "Ship_ID" exists
                                % 5. The port that the ship is docked at is the same port that
                                %    all of the containers are located
                                % 6. All of the containers provided in "Container_IDs" are unique,
                                %    which is a requirement by the definition of a container
                                % 7. Loading the ship with the containers listed in "Container_IDs"
                                %    won't put the ship over its capacity.

                                % The original inventory for the port that we are removing containers from
                                Specified_Port_Original_Container_List = maps:get(Container_Port,Shipping_State#shipping_state.port_inventory),
                                % the original ship inventory for the id Ship_ID concatenated the new containers
                                Original_Plus_Loaded_Containers = Specified_Ship_Original_Container_List ++ Container_IDs,
                                % the original port inventory for the
                                % id we're removing from -- (i.e. list subtraction) the removed containers
                                Original_Minus_Removed_Containers = Specified_Port_Original_Container_List -- Container_IDs,
                                % New ship inventory = the old ship inventory, but when you encounter
                                % the key "Ship_ID", replace the value with "Original_Plus_Loaded_Containers"
                                % which is the original ship inventory for the id Ship_ID concatenated the new containers
                                New_Ship_Inventory = new_inventory_map(Ship_ID,Original_Plus_Loaded_Containers,Shipping_State#shipping_state.ship_inventory),
                                % "New_Port_Inventory" = the old port inventory, but when you encounter
                                % the key "Container_Port", replace the value
                                % with "Original_Minus_Removed_Containers",
                                % which is the original port
                                % inventory for the id we're removing from -- (i.e. list subtraction) the removed containers
                                New_Port_Inventory = new_inventory_map(Container_Port,Original_Minus_Removed_Containers,Shipping_State#shipping_state.port_inventory),
                                % Create the new "shipping_state", using the original "Shipping_State"s
                                % corresponding values for everything but the "New_Ship_Inventory" and
                                % the "New_Port_Inventory".
                                #shipping_state{ships = Shipping_State#shipping_state.ships, containers = Shipping_State#shipping_state.containers, ports = Shipping_State#shipping_state.ports, ship_locations = Shipping_State#shipping_state.ship_locations, ship_inventory = New_Ship_Inventory, port_inventory = New_Port_Inventory}
                            end % End the 7th "if" statement
                        end % End the 6th "if" statement
                    end % End the 5th "if" statement
                end % End the 4th "if" statement
            end % End the 3rd "if" statement
        end % End the 2nd "if" statement
    end. % End the 1st "if" statement and the function itself

% Returns a modified inventory map where, when encountering the specified "Replacing_Key",
% the original value for that key is replaced by "New_Value", and the rest of the map is left the same.
% This is done by creating a list of tuples and converting them into a map
new_inventory_map(Replacing_Key,New_Value,Inventory_Map) ->
    % Convert the helper function's return value to the new map, and start the
    % helper function's iterator at 1
    maps:from_list(new_inventory_map_helper(Replacing_Key,New_Value,Inventory_Map,1)).

% Helper function for new_inventory_map that will iterate over the entire map, and then create a
% list of tuples that will be converted into key-value pairs in a map in the main function.
new_inventory_map_helper(Replacing_Key,New_Value,Inventory_Map,Iterator) ->
    % Declaring the next iterator value if we decide to use one.
    % This line is only here to avoid duplicate code later on. It can be done
    % every time you want to create a new iterator value, though.
    New_Iterator_Value = Iterator + 1,
    % Detect if the iterator is in the map
    Is_Iterator_In_Map = maps:is_key(Iterator,Inventory_Map),
    if
      % If we have recursively reached outside of the "Inventory_Map"
      Is_Iterator_In_Map == false ->
        % Concatenate the empty list to our final list
        [];
      % If we are still within the "Inventory_Map"
      Is_Iterator_In_Map == true ->
        % The value of the map at the current iterator value

        Inventory_Map_At_Iterator = maps:get(Iterator,Inventory_Map),
        if
          % If we have reached the key-value pair we are going to replace
          Iterator == Replacing_Key ->
            % Add the replacement key-value pair to the final list of tuples
            % instead of the original key-value pair
            [{Replacing_Key,New_Value}] ++ new_inventory_map_helper(Replacing_Key,New_Value,Inventory_Map,New_Iterator_Value);
          % We already know that the Inventory_Map_At_Iterator is not an error by our above statement,
          % so this only needs to check if the Iterator is at a different value than the Replacing_Key,
          % which is what it does.
          Iterator =/= Replacing_Key ->
            % Add the key-value pair "Iterator" and the "Iterator"s corresponding value in the map
            % to the final list to be converted into a map
            [{Iterator,Inventory_Map_At_Iterator}] ++ new_inventory_map_helper(Replacing_Key,New_Value,Inventory_Map,New_Iterator_Value)
        end % End the 2nd "if" statement
    end. % End the 1st "if" statement and the function itself.

% This function is designed to be used in conjunction with the valid_container_id_list function,
% and the valid_container_id_list function is run first to determine if the list of containers
% all exist and the list is not empty. So running this function makes those assumptions.
containers_are_at_ships_or_ports(Map_Inventory,Container_IDs) ->
    case Container_IDs of % Initialize "case" statement for "Container_IDs"
      [] -> % If the list has recursively arrived at an empty list
        true; % Every container is at some port, so return true.
      % Else, the list has at least one element.
      [Container_ID | Other_Container_IDs] ->
        % If the head of the list, i.e. "Container_ID", is at a port, then "Container_Is_At_A_Port_Or_Ship"
        % will equal the port that the container is located at. Otherwise, it will return "error".
        Container_Is_At_A_Port_Or_Ship = container_port_or_ship(Map_Inventory, Container_ID),
        if
          % If the container with id "Container_ID" is not at a port or ship
          Container_Is_At_A_Port_Or_Ship == error ->
            false; % Then not all containers listed in "Container_IDs" are at ports, so return false.
          % Else, search the rest of the list's containers to see if they are all at ports or ships.
          Container_Is_At_A_Port_Or_Ship =/= error ->
             % Recursively call "containers_are_at_ships_or_ports" over the rest of the list
            containers_are_at_ships_or_ports(Map_Inventory,Other_Container_IDs)
        end % End the "if" statement
    end. % End the "case" statement and the function itself

% Finds and returns the Port_ID or Ship_ID that some container is located at.
% If the container is not located at a port or on a ship (depending on the specified Inventory_Map),
% the function returns error.
container_port_or_ship(Inventory_Map,Container_ID) ->
    % Call the "container_in_inventory" function with
    % the iterator set to 1 and return the function's result
    container_in_inventory(Inventory_Map,Container_ID,1).

% Checks if, in a given "Port_Inventory" or a "Ship_Inventory",
% the specified Container_ID exists in one of the ports/ships in
% the supplied "Port_Inventory" or "Ship_Inventory"
container_in_inventory(Inventory_Map,Container_ID,Iterator) ->
    Is_Iterator_In_Map = maps:is_key(Iterator,Inventory_Map),
    if
      % If we have recursively reached outside of the "Inventory_Map"
      Is_Iterator_In_Map == false ->
        % If we have exceeded the possible keys for "Inventory_Map" and have not located the Container_ID
        error; % Return an error, as the Container_ID was not found.
      % If we are still within the "Inventory_Map"
      Is_Iterator_In_Map == true ->
        % "Inventory_Map_At_Iterator" contains the list corresponding to the key "Iterator"
        Inventory_Map_At_Iterator = maps:get(Iterator,Inventory_Map),
        % Checks if the desired "Container_ID" is in the obtained list from the "Inventory_Map" at
        % at the key "Iterator"
        Container_In_Map_At_Iterator = list_contains_value(Container_ID,Inventory_Map_At_Iterator),
        if
          % If the "Container_ID" is found in the "Inventory_Map_At_Iterator" list
          Container_In_Map_At_Iterator == true ->
            % Return "Iterator", which is equal to the Ship_ID or
            % Port_ID that the container with the id "Container_ID" is located on.
            Iterator;
          % Else, the "Container_ID" was not found in "Inventory_Map_At_Iterator"
          Container_In_Map_At_Iterator == false ->
            % Increment the iterator
            New_Iterator = Iterator + 1,
            % Then continue searching the rest of the "Inventory_Map" for the "Container_ID"
            container_in_inventory(Inventory_Map,Container_ID,New_Iterator)
        end % End the 2nd "if" statement
    end. % End the 1st "if" statement and the function itself.

% Returns all of the ports that the containers with the id's specified are located.
% Runs after checks that all containers exist and are at the necessary type of Inventory location.
container_ports_or_ships(Inventory_Map,Container_IDs) ->
    % Obtain the unedited list (i.e. may contain duplicates) of the locations of Containers
    % across all of the ports or ships, depending on the "Inventory_Map"
    List_Of_Ports_For_Container_IDs = container_inventory_list(Inventory_Map,Container_IDs),
    % Uniquify the original list and return the uniquified list.
    uniquify(List_Of_Ports_For_Container_IDs).

% Obtains a list of either Ship_IDs or Port_IDs, depending on the specified "Inventory_Map".
% This runs after checks are completed that all containers exist and are at the necessary type of
% Inventory location.
container_inventory_list(Inventory_Map,Container_IDs) ->
    case Container_IDs of % Initialize "case" statement for "Container_IDs"
      [] -> % If we've iterated over the entire "Container_IDs" list
        []; % Return an empty list
      [Container_ID | Other_Container_IDs] -> % The list "Container_IDs" contains at least one element
        % Add the list form of the Port_ID or Ship_ID to the final list of Port_IDs or Ship_IDs
        [container_port_or_ship(Inventory_Map,Container_ID)] ++ container_inventory_list(Inventory_Map, Other_Container_IDs)
    end. % End the case statement and the function itself

% Determines the length of some list "My_List"
list_length(My_List) ->
  case My_List of % Initialize "case" statement
    [] -> % If the argument passed to "My_List" is an empty list
      0; % Return 0
    % Else, for the first element in the list and the rest of the list
    [_List_Head | Rest_Of_List] ->
      % Add 1 (since we are guarenteed that there is one element of the list (i.e. the "List_Head"))
      % and recursively call "list_length" over the rest of "My_List"
      1 + list_length(Rest_Of_List)
  end. % End the "case" statement and the function itself

% Removes any duplicate elements of some list "My_List" and returns a list without duplicates
uniquify(My_List) ->
    case My_List of % Initialize "case" statement for "My_List"
      [] -> % If "My_List" is empty, then it is certainly unique, since there are no elements to be duplicates
        []; % So return an empty list
      % Else, the list contains at least one element
      [List_Head | Rest_Of_List] ->
        % Variable that determines if "My_List" contains a duplicate of "List_Head"
        Does_List_Contain_The_Value = list_contains_value(List_Head, Rest_Of_List),
        if
          % If "My_List" does not contain a duplicate of the "List_Head"
          Does_List_Contain_The_Value == false ->
            % Then keep the unique list head and "uniquify" the rest of the list
            [List_Head] ++ uniquify(Rest_Of_List);
          % Else, "My_List" contains a duplicate of the "List_Head"
          Does_List_Contain_The_Value == true ->
            % So don't include it in the uniquified list and just "uniquify" the rest of the list
            uniquify(Rest_Of_List)
        end % End the "if" statement
      end. % End the "case" statement and the function itself

% Checks if some value "My_Value" is an element of some list "My_List"
% Returns false of "My_Value" is not in "My_List", true if "My_Value" is in "My_List"
list_contains_value(My_Value, My_List) ->
    case My_List of % Initialize "case" statement for "My_List"
      % If the list contains no elements, then "My_Value" is certainly not in "My_List"
      [] ->
        false; % So we return false, since the value "My_Value" is not in "My_List"
      % Else, the list "My_List" contains at least one element
      [List_Head | Rest_Of_List] ->
      if
        % If "List_Head" is equal to My_Value, then "My_Value" is an element of the list
        List_Head == My_Value ->
          true; % Return true
        % Else, the head of the list is not equal to "My_Value"
        List_Head =/= My_Value ->
          % So call "list_contains_value" over the rest of the list
          list_contains_value(My_Value, Rest_Of_List)
      end % End the "if" statement
    end. % End the case statement and the function itself

% Unload all of the containers on the ship with the id "Ship_ID" onto the port that it is docked at,
% if the port has the capacity to hold all the new containers.
unload_ship_all(Shipping_State, Ship_ID) ->
    % Get the list of containers on the ship with the id "Ship_ID"
    Ships_Container_IDs = maps:get(Ship_ID,Shipping_State#shipping_state.ship_inventory),
    % Then call and return the result of "unload_ship" for all of
    % the containers on the ship with the id "Ship_ID"
    unload_ship(Shipping_State,Ship_ID,Ships_Container_IDs).
    % io:format("Implement me!!"),
    % error.

% Has many helper functions, but returns a shipping state unloading the
% containers listed in "Container_IDs" onto the port where ship with the id "Ship_ID" is docked, if possible.
% We need to see if the location of every container in the list of "Container_IDs" are on the ship.
% If even one container is in the wrong place, then
% return an error. Else, check if the number of containers that might be unloaded onto the
% port is not greater than the ((port's capacity) - (containers already at the port)). If
% it is, then return error. Else, move all of the containers in the "Container_IDs" to the
% port the ship is docked at. This requires reconstructing the port inventory and the ship inventory
% to account for the change in the container locations for both, so helper functions are used for each
% of those things. Then use the current "ships", "containers", "ports", and "ship_locations" that
% are in the current shipping state and the new "ship_inventory" and the new "port_inventory", which
% were created from their helper functions, to complete the new "Shipping_State". Then return the
% new "Shipping_State".
unload_ship(Shipping_State, Ship_ID, Container_IDs) ->
  % First, check if "Container_IDs" contains any non-existent container id's.
  Valid_Container_ID_List = valid_container_id_list(Shipping_State#shipping_state.containers, Container_IDs),
  if
    % If at least one of the container id's in the list "Container_IDs" is not a valid container id
    Valid_Container_ID_List == false ->
      error; % Return "error"
    % If all id's in the "Container_IDs" are valid container id's, then continue
    Valid_Container_ID_List == true ->
      % Checks if the containers in the list of containers with the id's listed in "Container_IDs"
      % are all located on ships. "Containers_On_Ships" will return "true" if they are all at ports
      % and false otherwise.
      Containers_On_Ships = containers_are_at_ships_or_ports(Shipping_State#shipping_state.ship_inventory,Container_IDs),
      if
        % If either the containers do not exist or are not all on ships
        Containers_On_Ships == false ->
          error; % Return "error", since all containers must exist and be at ports.
        % If all of the containers in the list of id's "Container_IDs" exist and are at ports
        Containers_On_Ships == true ->
          % "Container_Ships" uses the value from the function "container_ports_or_ships" to
          % determine the lists of all ships that the containers in "Container_IDs" are located.
          Container_Ships = container_ports_or_ships(Shipping_State#shipping_state.ship_inventory,Container_IDs),
          % Use the list_length function to determine if the containers are on
          % different ships (i.e. "Number_Of_Container_Ships" > 1),
          % no containers were given in the list of "Container_IDs" (i.e. "Number_Of_Container_Ships" == 0)
          % or all located on the same ship (i.e. "Number_Of_Container_Ships" == 1)
          Number_Of_Container_Ships = list_length(Container_Ships),
          if
            % If the listed containers are on different ships (i.e. "Number_Of_Container_Ships" > 1)
            % or no containers were listed in "Container_IDs"
            Number_Of_Container_Ships =/= 1 ->
              % Return "error", since all containers must be at the same port as the ship with the id "Ship_ID"
              error;
            % If all of the containers are on the same ship
            Number_Of_Container_Ships == 1 ->
              % Get the specified ship, or an error if the ship does not exist
              Specified_Ship = get_ship(Shipping_State, Ship_ID),
              if
                % If the ship with the id "Ship_ID" does not exist
                Specified_Ship == error ->
                  error;
                % Else, the ship with the id "Ship_ID" exists
                Specified_Ship =/= error ->
                  % Find the port of the ship, i.e. the first value in "get_ship_location",
                  % by converting the resulting tuple from "get_ship_location" into a list,
                  % then retrieving the first value, i.e. the port of the ship with id "Ship_ID"
                  % Since we did the check if the ship with id "Ship_ID" exists just before this,
                  % checking if a ship with the id "Ship_ID" will not throw an
                  % exception for a non-existant ship
                  Ships_Port = lists:nth(1,tuple_to_list(get_ship_location(Shipping_State, Ship_ID))),
                  % Create a uniquified list of container id's to check if all of the supplied containers
                  % are indeed unique. If not, we have received erroneous input for our "Container_IDs" list
                  Uniquified_Container_List = uniquify(Container_IDs),
                  % The original number of containers to unload
                  Number_Of_Containers_To_Unload = list_length(Container_IDs),
                  % The length of the uniquified list of containers
                  Length_Of_Uniquified_Container_List = list_length(Uniquified_Container_List),
                  if
                    % If the length of the original "Container_IDs" list is not equal to the
                    % length of the uniquified "Container_IDs" list
                    Number_Of_Containers_To_Unload =/= Length_Of_Uniquified_Container_List ->
                      % We have received erroneous input for our "Container_IDs" list and must return an error.
                      error;
                    % If the unique list of "Container_IDs" is equivalent to the original number
                    % of elements in "Container_IDs", then continue
                    Number_Of_Containers_To_Unload == Length_Of_Uniquified_Container_List ->
                      % Get the port that we are planning to load cargo onto
                      % in list form so we can search its elements
                      Specified_Port_List_Form = tuple_to_list(get_port(Shipping_State, Ships_Port)),
                      % Get the original list of containers in the port to check the capacity and add to the list of containers
                      Specified_Port_Original_Container_List = maps:get(Ships_Port,Shipping_State#shipping_state.port_inventory),
                      % Length of the containers in the specified port before unloading
                      Length_Of_Original_Port_Container_List = list_length(Specified_Port_Original_Container_List),
                      % Maximum capacity of the specified port, i.e. the fifth element in the value returned by get_port
                      Port_Max_Capacity = lists:nth(5,Specified_Port_List_Form),
                      % Get the remaining capacity of the port before attempting to unload new containers using the
                      % port's "container_cap" (container capacity)
                      % minus the current number of containers at the port right now
                      Capacity_Before_Unloading = Port_Max_Capacity - Length_Of_Original_Port_Container_List,
                      % Get what the capacity of the port that we're unloading onto will be after
                      % we unload to it with the containers specified in the "Container_IDs"
                      Capacity_After_Unloading = Capacity_Before_Unloading - Number_Of_Containers_To_Unload,
                      if
                        % If unloading onto the port with the requested cargo puts the port over capacity
                        Capacity_After_Unloading < 0 ->
                          error; % Print an error, as a port cannot be unloaded over its capacity
                        % Else, the port can be unloaded onto.
                        Capacity_After_Unloading >= 0 ->
                          % Now the port can be unloaded onto, because the required conditions to do
                          % so (i.e. all of the "if" statements above), have been satisfied.
                          % The conditions satisfied are:
                          % 1. The list of "Container IDs" supplied all exist in the "Shipping_State"
                          % 2. All of the containers are located at ports, i.e. none of the
                          %    containers listed are on ships
                          % 3. All of the containers are located at the same port
                          % 4. The ship with the id "Ship_ID" exists
                          % 5. All of the containers provided in "Container_IDs" are unique,
                          %    which is a requirement by the definition of a container
                          % 6. Unloading the ship with the containers listed in "Container_IDs"
                          %    won't put the port over its capacity.

                          % The original inventory for the port that we are adding containers to
                          Specified_Port_Original_Container_List = maps:get(Ships_Port,Shipping_State#shipping_state.port_inventory),
                          % The original inventory for the ship that we are removing containers from
                          Specified_Ship_Original_Container_List = maps:get(Ship_ID,Shipping_State#shipping_state.ship_inventory),
                          % The original port inventory for the id Ships_Port concatenated the new containers
                          Original_Plus_Unloaded_Containers = Specified_Port_Original_Container_List ++ Container_IDs,
                          % the original ship inventory for the
                          % id we're removing from -- (i.e. list subtraction) the removed containers
                          Original_Minus_Unloaded_Containers = Specified_Ship_Original_Container_List -- Container_IDs,
                          % New port inventory = the old port inventory, but when you encounter
                          % the key "Ships_Port", replace the value with "Original_Plus_Unloaded_Containers"
                          % which is the original port inventory for the id Ships_Port concatenated the new containers
                          New_Port_Inventory = new_inventory_map(Ships_Port,Original_Plus_Unloaded_Containers,Shipping_State#shipping_state.port_inventory),
                          % "New_Ship_Inventory" = the old ship inventory, but when you encounter
                          % the key "Ship_ID", replace the value
                          % with "Original_Minus_Unloaded_Containers",
                          % which is the original ship
                          % inventory for the id we're removing from -- (i.e. list subtraction) the removed containers
                          New_Ship_Inventory = new_inventory_map(Ship_ID,Original_Minus_Unloaded_Containers,Shipping_State#shipping_state.ship_inventory),
                          % Create the new "shipping_state", using the original "Shipping_State"s
                          % corresponding values for everything but the "New_Port_Inventory" and
                          % the "New_Ship_Inventory".
                          #shipping_state{ships = Shipping_State#shipping_state.ships, containers = Shipping_State#shipping_state.containers, ports = Shipping_State#shipping_state.ports, ship_locations = Shipping_State#shipping_state.ship_locations, ship_inventory = New_Ship_Inventory, port_inventory = New_Port_Inventory}
                      end % End the 6th "if" statement
                  end % End the 5th "if" statement
              end % End the 4th "if" statement
          end % End the 3rd "if" statement
      end % End the 2nd "if" statement
  end. % End the 1st "if" statement and the function itself

% Moves the ship with the id "Ship_ID" to the id and dock "Port_ID"
% and "Dock" respectively, if the "Dock" at the id "Port_ID" is not occupied.
% If the "Dock" at the id "Port_ID" is occupied, return "error".
% Else, return a shipping state with this change.
% Note that a Ship Location is of the form {port, dock, ship}.
set_sail(Shipping_State, Ship_ID, {Port_ID, Dock}) ->
    % Get the ship with the id "Ship_ID" to check if such a ship exists.
    Ship_At_Ship_ID = get_ship(Shipping_State, Ship_ID),
    if
      % If trying to obtain the ship with the id "Ship_ID" produces an error, then the ship does not exist
      Ship_At_Ship_ID == error ->
         % Return an error, since the whole program revolves around moving a ship and the ship doesn't exist.
        error;
      % Else, the ship we want to move exists, so the program can proceed.
      Ship_At_Ship_ID =/= error ->
        % Check if, for the given "Shipping_State", both the port and dock at that port both exist.
        Port_And_Dock_Exist = port_and_dock_existence_checker(Shipping_State,Port_ID,Dock),
        if
          % If the dock "Dock" at the port that has the ID "Port_ID" does not exist
          Port_And_Dock_Exist == false ->
            error; % Return an error, since either the specified "Port_ID" or dock "Dock" at that port doesn't exist.
          % Else, the port with id "Port_ID" and the corresponding dock "Dock" exist, so we can proceed.
          Port_And_Dock_Exist == true ->
            % This variable determines whether the dock at the given port is
            % occupied (i.e. Dock_At_Port_Is_Occupied == true) or if the dock at the
            % given port is vacant (i.e. Dock_At_Port_Is_Occupied == false)
            % by passing the "Port_ID" and "Dock" and the shipping state to check against.
            Dock_At_Port_Is_Occupied = is_dock_at_port_occupied(Shipping_State,Port_ID,Dock),
            if
              % If the "Dock" at the "Port_ID" that we're trying to go to is occupied
              Dock_At_Port_Is_Occupied == true ->
                error;
              % Else, the "Dock" at the "Port_ID" that we're trying to go to is vacant and we can go there.
              Dock_At_Port_Is_Occupied == false ->
                % Create a new list of locations using the current list of locations,
                % the id "Ship_ID" that we want to update the location for,
                % the "Ship_ID"s new "Port_ID" that it is located at,
                % and the "Dock" at the port with "Port_ID" that the ship will dock at.
                New_Ship_Locations = create_updated_locations(Shipping_State#shipping_state.ship_locations, Ship_ID, Port_ID, Dock),
                % Create and return the updated "Shipping_State", where every value in the original
                % "Shipping_State" will be carried over except the original "ship_locations" will now
                % be replaced by "New_Ship_Locations".
                #shipping_state{ships = Shipping_State#shipping_state.ships, containers = Shipping_State#shipping_state.containers, ports = Shipping_State#shipping_state.ports, ship_locations = New_Ship_Locations, ship_inventory = Shipping_State#shipping_state.ship_inventory, port_inventory = Shipping_State#shipping_state.port_inventory}
            end % End the third "if" statement
        end % End the second "if" statement
    end. % End the first "if" statement and the function itself

% Create an updated list of locations and change the location of the ship with id "Ship_ID" to be
% at the port "Port_ID" and the dock "Dock"
% Again note that each Ship Location is of the form {port, dock, ship}.
create_updated_locations(Ship_Locations, Ship_ID, Port_ID, Dock) ->
    case Ship_Locations of % Create a case referencing the Ship_Locations
      % If we have recursively updated the entire list
      [] ->
        []; % Append the empty list to the whole list and stop recursing
      % There is at least one more location to consider
      [Ship_Location | Other_Ship_Locations] ->
        % Get the list form of the Ship_Location tuple so we can search its elements
        Ship_Location_List_Form = tuple_to_list(Ship_Location),
        % Obtain the third value of the "Ship_Location", i.e. the id of the ship that is located
        Ship_Location_Ship_ID = lists:nth(3, Ship_Location_List_Form),
        if
          % If the "Ship_ID" we're looking to replace is the one we're currently examining
          Ship_Location_Ship_ID == Ship_ID ->
            % Then create a new location tuple with the new "Port_ID", "Dock", and "Ship_ID"
            % that the ship is now located at, concatenate that with the updated location list,
            % then recursively call "create_updated_locations" over the rest of the list.
            [{Port_ID, Dock, Ship_ID}] ++ create_updated_locations(Other_Ship_Locations, Ship_ID, Port_ID, Dock);
          % Else, the "Ship_Location" we're examining is NOT the ship we're looking to replace
          Ship_Location_Ship_ID =/= Ship_ID ->
            % So we should concatenate the current location tuple, to the updated location list,
            % since all of the other ships remain in the same place, and call "create_updated_locations"
            % over the rest of the list.
            [Ship_Location] ++ create_updated_locations(Other_Ship_Locations, Ship_ID, Port_ID, Dock)
        end % End the "if" statement
    end. % End the "case" statement and the function itself.

% Checks if both the port and the port's dock exist that have been passed to the program
port_and_dock_existence_checker(Shipping_State, Port_ID, Dock) ->
    % Use the "get_port" function to obtain the data for the port with id "Port_ID", if it exists.
    Port_At_Port_ID = get_port(Shipping_State, Port_ID),
    if
      % If "get_port" returned an error,
      % i.e. a port with the id "Port_ID" does not exist in this "Shipping_State"
      Port_At_Port_ID == error ->
        error; % Return "error".
      % If "get_port" returned a port in the "Shipping_State" with the id "Port_ID"
      Port_At_Port_ID =/= error ->
        % The list of docks at the port is the fourth element of a port in a "Shipping_State".
        % Now that we have determined that the port with id "Port_ID" exists, we retrieve that
        % list of docks for the port after converting the "Port_At_Port_ID" to a searchable list.
        List_Of_Docks_At_Port = lists:nth(4, tuple_to_list(Port_At_Port_ID)),
        % Check if the dock "Dock" is in the list of existing docks for the port with id "Port_ID"
        Dock_Is_In_List_Of_Docks = list_contains_value(Dock,List_Of_Docks_At_Port),
        if
          % If the dock "Dock" is in the list of existing docks
          Dock_Is_In_List_Of_Docks == true ->
            true; % Return true, indicating that the specified "Port_ID" and "Dock" at the port both exist.
          % Else, the port exists, but the dock "Dock" at that port does not exist.
          Dock_Is_In_List_Of_Docks == false ->
            false % Return false, since the dock "Dock" at the specified port does not exist.
        end % End the second "if" statement
    end. % End the first "if" statement and the function itself

% Determines if the "Dock" at the port that has the id "Port_ID" is currently occupied in the "Shipping_State".
% If the "Dock" at the port that has the id "Port_ID" is occupied, the function returns true.
% If the "Dock" at the port that has the id "Port_ID" is vacant, the function returns false.
is_dock_at_port_occupied(Shipping_State, Port_ID, Dock) ->
    % "Occupied_Docks" determines the docks that are occupied for a given "Port_ID".
    Occupied_Docks = get_occupied_docks(Shipping_State,Port_ID),
    % "Dock_In_Occupied_Docks" checks if the "Dock" we're looking to use is currently occupied.
    Dock_In_Occupied_Docks = list_contains_value(Dock,Occupied_Docks),
    if
      % If there is a ship at the port with the ID "Port_ID" that is docked at "Dock" in that port
      Dock_In_Occupied_Docks == true ->
        true; % Return true, indicating that the requested docking location is occupied
      % Else, there is not a ship at the port with the ID "Port_ID" that is docked at "Dock" in that port
      Dock_In_Occupied_Docks == false ->
        false % Return false, indicating that the requested docking location is vacant
    end. % End the "if" statement and the function itself.

%% Determines whether all of the elements of Sub_List are also elements of Target_List
%% @returns true is all elements of Sub_List are members of Target_List; false otherwise
is_sublist(Target_List, Sub_List) ->
    lists:all(fun (Elem) -> lists:member(Elem, Target_List) end, Sub_List).

%% Prints out the current shipping state in a more friendly format
print_state(Shipping_State) ->
    io:format("--Ships--~n"),
    _ = print_ships(Shipping_State#shipping_state.ships, Shipping_State#shipping_state.ship_locations, Shipping_State#shipping_state.ship_inventory, Shipping_State#shipping_state.ports),
    io:format("--Ports--~n"),
    _ = print_ports(Shipping_State#shipping_state.ports, Shipping_State#shipping_state.port_inventory).

%% helper function for print_ships
get_port_helper_for_print_ships([], _Port_ID) -> error;
get_port_helper_for_print_ships([ Port = #port{id = Port_ID} | _ ], Port_ID) -> Port;
get_port_helper_for_print_ships( [_ | Other_Ports ], Port_ID) -> get_port_helper(Other_Ports, Port_ID).

print_ships(Ships, Locations, Inventory, Ports) ->
    case Ships of
        [] ->
            ok;
        [Ship | Other_Ships] ->
            {Port_ID, Dock_ID, _} = lists:keyfind(Ship#ship.id, 3, Locations),

            % Begin me text
            % This line gave errors, since get_port_helper isn't defined (at least at first),
            % but is the original
            Port = get_port_helper_for_print_ships(Ports, Port_ID),

            % NOTE: okay, this is the original, and it might actually be the right thing, since "get_port"
            % takes a "Shipping_State", not a "Ports". And I happened to, purely coincidentally, write
            % a "get_port_helper" statement anyways with the correct parameters.

            {ok, Ship_Inventory} = maps:find(Ship#ship.id, Inventory),
            io:format("Name: ~s(#~w)    Location: Port ~s, Dock ~s    Inventory: ~w~n", [Ship#ship.name, Ship#ship.id, Port#port.name, Dock_ID, Ship_Inventory]),
            print_ships(Other_Ships, Locations, Inventory, Ports)
    end.

print_containers(Containers) ->
    io:format("~w~n", [Containers]).

print_ports(Ports, Inventory) ->
    case Ports of
        [] ->
            ok;
        [Port | Other_Ports] ->
            {ok, Port_Inventory} = maps:find(Port#port.id, Inventory),
            io:format("Name: ~s(#~w)    Docks: ~w    Inventory: ~w~n", [Port#port.name, Port#port.id, Port#port.docks, Port_Inventory]),
            print_ports(Other_Ports, Inventory)
    end.
%% This functions sets up an initial state for this shipping simulation. You can add, remove, or modidfy any of this content. This is provided to you to save some time.
%% @returns {ok, shipping_state} where shipping_state is a shipping_state record with all the initial content.

shipco() ->

    Ships = [#ship{id=1,name="Santa Maria",container_cap=20},
              #ship{id=2,name="Nina",container_cap=20},
              #ship{id=3,name="Pinta",container_cap=20},
              #ship{id=4,name="SS Minnow",container_cap=20},
              #ship{id=5,name="Sir Leaks-A-Lot",container_cap=20}
             ],
    Containers = [
                  #container{id=1,weight=200},
                  #container{id=2,weight=215},
                  #container{id=3,weight=131},
                  #container{id=4,weight=62},
                  #container{id=5,weight=112},
                  #container{id=6,weight=217},
                  #container{id=7,weight=61},
                  #container{id=8,weight=99},
                  #container{id=9,weight=82},
                  #container{id=10,weight=185},
                  #container{id=11,weight=282},
                  #container{id=12,weight=312},
                  #container{id=13,weight=283},
                  #container{id=14,weight=331},
                  #container{id=15,weight=136},
                  #container{id=16,weight=200},
                  #container{id=17,weight=215},
                  #container{id=18,weight=131},
                  #container{id=19,weight=62},
                  #container{id=20,weight=112},
                  #container{id=21,weight=217},
                  #container{id=22,weight=61},
                  #container{id=23,weight=99},
                  #container{id=24,weight=82},
                  #container{id=25,weight=185},
                  #container{id=26,weight=282},
                  #container{id=27,weight=312},
                  #container{id=28,weight=283},
                  #container{id=29,weight=331},
                  #container{id=30,weight=136}
                 ],
    Ports = [
             #port{
                id=1,
                name="New York",
                docks=['A','B','C','D'],
                container_cap=200
               },
             #port{
                id=2,
                name="San Francisco",
                docks=['A','B','C','D'],
                container_cap=200
               },
             #port{
                id=3,
                name="Miami",
                docks=['A','B','C','D'],
                container_cap=200
               }
            ],
    %% {port, dock, ship}
    Locations = [
                 {1,'B',1},
                 {1, 'A', 3},
                 {3, 'C', 2},
                 {2, 'D', 4},
                 {2, 'B', 5}
                ],
    Ship_Inventory = #{
      1=>[14,15,9,2,6],
      2=>[1,3,4,13],
      3=>[],
      4=>[2,8,11,7],
      5=>[5,10,12]},
    Port_Inventory = #{
      1=>[16,17,18,19,20],
      2=>[21,22,23,24,25],
      3=>[26,27,28,29,30]
     },
    #shipping_state{ships = Ships, containers = Containers, ports = Ports, ship_locations = Locations, ship_inventory = Ship_Inventory, port_inventory = Port_Inventory}.
