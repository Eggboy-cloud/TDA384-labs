-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server % atom of the chat server
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_st{
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom
    }.

% handle/2 handles each kind of request from GUI
% Parameters:
%   - the current state of the client (St)
%   - request data from GUI
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to GUI, either the atom `ok` or a tuple {error, Atom, "Error message"}
%   - NewState is the updated state of the client

% Join channel
handle(St, {join, Channel}) ->
    % checks if server is active
    case lists:member(St#client_st.server, registered()) of
        true ->
            try Result = genserver:request(St#client_st.server,{join, St#client_st.nick,Channel,self()}),
                case Result of
                    % pattern matches the result with response
                    ok -> {reply, ok, St};
                    error -> {reply,{error, user_already_joined, "User already in channel"},St}
                    end
            catch
                timeout_error -> {reply,{error, server_not_reached, "Server not responding"}, St}
            end;
        false ->
            {reply,{error, server_not_reached, "No server"}, St}
        end;
    

% Leave channel
handle(St, {leave, Channel}) ->
    Result = genserver:request(list_to_atom(Channel),{leave,self()}),
        case Result of
            % pattern matches the result with response
            ok -> {reply, ok, St};
            error -> {reply,{error, user_not_joined, "User not in channel"},St}
            end;

% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
    % checks if server is active
    case lists:member(list_to_atom(Channel), registered()) of
            true ->
                try Result = genserver:request(list_to_atom(Channel),{message_send, Channel, Msg, St#client_st.nick, self()}),
                    case Result of
                        % pattern matches the result with response
                        ok -> {reply, ok, St};
                        error -> {reply,{error, user_not_joined, "User not in channel"},St}
                        end
                catch
                    timeout_error -> {reply,{error, server_not_reached, "Server not responding"}, St}
                end;
            false ->
                {reply,{error, server_not_reached, "No server"}, St}
            end;


% This case is only relevant for the distinction assignment!
% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
    % checks if server is active
    case lists:member(St#client_st.server, registered()) of
        true ->
            try Result = genserver:request(St#client_st.server,{nick, St#client_st.nick, NewNick}),
                case Result of
                    % pattern matches the result with response
                    ok -> {reply, ok, St#client_st{nick = NewNick}} ;
                    error -> {reply,{error, nick_taken, "Nickname is already taken"},St}
                    end
            catch
                timeout_error -> {reply,{error, server_not_reached, "Server not responding"}, St}
            end;
        false ->
            {reply,{error, server_not_reached, "No server"}, St}
        end;

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St} ;

% Quit client via GUI
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, St} ;

% Catch-all for any unhandled requests
handle(St, _) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .
