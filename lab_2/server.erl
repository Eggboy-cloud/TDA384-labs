-module(server).
-import(lists,[append/2]).
-export([start/1,stop/1,handle/2, initial_state/2, findChannel/2, appendClient/3,helper_pid/2,findPid/3,removeClient/3]).

-record(server_st, {
    server, % atom of the chat server
    channels % all current channels
}).

initial_state(ServerAtom, channelList) ->
    #server_st{
        server = ServerAtom,
        channels = channelList
    }.


% - Spawn a new process which waits for a message, handles it, then loops infinitely
% - Register this process to ServerAtom
% - Return the process ID
start(ServerAtom) -> 
    gen_server:start(ServerAtom, initial_state(ServerAtom, [] ), handle).

findChannel(_, []) ->
    false;
findChannel(Channel, [{C,_} | List]) ->
    case Channel = C of
        true ->
            true;
        false ->
            findChannel(Channel, List)
    end.

appendClient(Client, Channel , [{C,X} | List]) ->
    case Channel = C of
        true ->
            X = append(X,Client);
        false ->
            appendClient(Client, Channel , List)
    end,
    [{C,X} | List].


helper_pid(_, []) ->
    false;
helper_pid(Client, [X | List]) ->
    case Client = X of
        true ->
            true;
        false ->
            helper_pid(Client,List)
    end.

findPid(_,_,[]) ->
    false;
findPid(Client,Channel, [{C,X} | List]) ->
    case Channel = C of
        true -> 
            case helper_pid(Client, X) of
                true ->
                    true;
                false ->
                    false
                end;
        false ->
            findPid(Client, Channel, List)
    end.

removeClient(Client, Channel , [{C,X} | List]) ->
    case Channel = C of
        true ->
            X = [L|| L <-X, L /= Client];
        false ->
            removeClient(Client, Channel , List)
    end,
    [{C,X}|List].

handle(St,{join,Channel,From}) ->
    case findChannel(Channel,St#server_st.channels) of
        true ->
            case findPid(From,Channel,St#server_st.channels) of
                true ->
                    {user_already_joined,make_ref(),St};
                false ->
                    list = appendClient(From, Channel, St#server_st.channels),
                    {reply, make_ref(), St#server_st{channels = list}}
                end;
        false ->
            list = append({Channel,[]},St#server_st.channels),
            list = appendClient(From, Channel, list),
            {reply, make_ref(), St#server_st{channels = list}}
        end;
    

handle(St,{leave, Channel,From}) ->
    case findPid(From, Channel ,St#server_st.channels) of
        true ->
            list = removeClient(From, Channel, St#server_st.channels),
            {reply, make_ref(), St#server_st{channels = list}};
        false ->
            {user_not_joined, make_ref(), St}
        end;

handle(St,{message_send, Channel, Msg, Nick, From}) ->
    case findPid(From, Channel ,St#server_st.channels) of
        true ->
            send_message(Channel, Nick, Msg, St#server_st.channels),
            {reply, make_ref(), St};
        false ->
            {user_not_joined, make_ref(), St}
        end.

send_message(Channel, Nick, Msg, [{C,X}|List]) ->
    case Channel = C of
        true ->
            client_send(Channel, Nick, Msg, X);
        false ->
            send_message(Channel, Nick, Msg, List)
        end.

client_send(Channel, Nick, Msg, [From | List]) ->
    From ! {Msg, Channel, Nick, Msg},
    client_send(Channel, Nick, Msg, List).

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    exit(ServerAtom, normal).
