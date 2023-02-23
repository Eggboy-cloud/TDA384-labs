-module(server).
-export([start/1,stop/1,handle/2, initial_state/1, findAtom/2]).

-record(server_st, {
    server, % atom of the chat server
    channels, % all current channels
    nicks % all current nicks
}).

%initial state for the server
initial_state(ServerAtom) ->
    #server_st{
        server = ServerAtom,
        channels = [],
        nicks = []
    }.

% - Spawn a new process which waits for a message, handles it, then loops infinitely
% - Register this process to ServerAtom
% - Return the process ID
start(ServerAtom) -> 
    genserver:start(ServerAtom, initial_state(ServerAtom), fun handle/2).

% - Helper function which functions as member from the lists module
% - Return true if Atom exists in list else returns false
findAtom(_, []) ->
    false;
findAtom(Atom, [A | List]) ->
    case Atom == A of
        true ->
            true;
        false ->
            findAtom(Atom, List)
    end.

% handle/2 handles each kind of request from Client
% Parameters:
%   - the current state of the server (St)
%   - request data from Client

% - All the commands to handle/2:
% - Join, joining a channel and registering the nick of the user
% - Leave, leaving a channel
% - Send Message - allows sending messages to all other users in a channel
% - New Nick - changes nick for the user if the new nick is not already used

handle(St,{join,Nick,Channel,From}) ->
    case findAtom(Nick, St#server_st.nicks) of
        false ->
            L = [Nick | St#server_st.nicks];
        true ->
            L = St#server_st.nicks
    end,
    case findAtom(Channel,St#server_st.channels) of
        true ->
            %sends request to Channel thread to add a user to the Channel
            Result = genserver:request(list_to_atom(Channel),{joinClient, From}),
            {reply, Result, St#server_st{nicks = L}}; 
        false ->
            %starts a new thread for the Channel in Genserver
            genserver:start(list_to_atom(Channel), [From], fun handle/2),
            List = [Channel | St#server_st.channels],
            {reply, ok, St#server_st{channels = List, nicks = L}}
        end;

% - Helper function that handle join for clients in Channel threads
handle(List,{joinClient, From}) ->
    case findAtom(From, List) of
        true ->
            % Error - User already joined
            {reply, error, List};
        false ->
            L = [From | List],
            {reply, ok, L}
        end;
        
handle(List,{leave,From}) ->
    case findAtom(From, List) of
        true ->
            % removes user from list
            S = [L|| L <-List, L /= From],
            {reply, ok, S};
        false ->
            % Error - User not in Channel
            {reply, error, List}
        end;

handle(List,{message_send, Channel, Msg, Nick, From}) ->
    case findAtom(From, List) of
        true ->
            % sends request to user for sending message to GUI
            Data = {request, self(), make_ref(),{message_receive, Channel, Nick, Msg}},
            [Client ! Data || Client <- List, Client /=From],
            {reply, ok, List};
        false ->
            % Error - User not in Channel
            {reply, error, List}
        end;

handle(St, {nick, OldNick, NewNick}) ->
    case findAtom(NewNick, St#server_st.nicks) of
        true ->
            % Error - Nick is already used
            {reply, error, St};
        false ->
            %replaces old nick with new nick
            List = [NewNick | lists:delete(OldNick, St#server_st.nicks)],
            {reply, ok, St#server_st{nicks = List}}
        end;

% - Kills all Channels
handle(St,kill) ->
    [genserver:stop(list_to_atom(Channel)) ||Channel <- St#server_st.channels],
    {reply, ok, St#server_st{channels = []}}.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    genserver:request(ServerAtom,kill),
    genserver:stop(ServerAtom).