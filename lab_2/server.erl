-module(server).
-export([start/1,stop/1,handle/2, initial_state/2, findChannel/2, appendClient/3,helper_pid/2,findPid/3,removeClient/3,client_send/5]).

-record(server_st, {
    server, % atom of the chat server
    channels % all current channels
}).

initial_state(ServerAtom, ChannelList) ->
    #server_st{
        server = ServerAtom,
        channels = ChannelList
    }.


% - Spawn a new process which waits for a message, handles it, then loops infinitely
% - Register this process to ServerAtom
% - Return the process ID
start(ServerAtom) -> 
    genserver:start(ServerAtom, initial_state(ServerAtom, [] ), fun handle/2).

findChannel(_, []) ->
    false;
findChannel(Channel, [{C,_} | List]) ->
    case Channel == C of
        true ->
            true;
        false ->
            findChannel(Channel, List)
    end.

appendClient(From, Channel, [{C,X} | List]) ->
    case Channel == C of
        true ->
            L = [From | X],
            L2 = List;
        false ->    
            L = X,
            L2 = appendClient(From, Channel , List)
    end,
    io:fwrite("~p~n", [[{C,L} | L2]]),
    [{C,L} | L2].



helper_pid(_, []) ->
    false;
helper_pid(Client, [X | List]) ->
    case Client == X of
        true ->
            true;
        false ->
            helper_pid(Client,List)
    end.

findPid(_,_,[]) ->
    false;
findPid(Client,Channel, [{C,X} | List]) ->
    case Channel == C of
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
    case Channel == C of
        true ->
            S = [L|| L <-X, L /= Client];
        false ->
            S = X,
            List = removeClient(Client, Channel , List)
    end,
    [{C,S}|List].

handle(St,{join,Channel,From}) ->
    io:fwrite("~p~n", [Channel]),
    case findChannel(Channel,St#server_st.channels) of
        true ->
            case findPid(From,Channel,St#server_st.channels) of
                true ->
                    {user_already_joined, ok, St};
                false ->
                    io:fwrite("Old Channel, ~p~n", [From]),
                    List = appendClient(From, Channel, St#server_st.channels),
                    io:fwrite("~p~n", [List]),
                    {reply, ok, St#server_st{channels = List}}
                end;
        false ->
            io:fwrite("New Channel, ~p~n", [From]),
            L1 = [{Channel, []} | St#server_st.channels],
            L2 = appendClient(From, Channel, L1),
            {reply, ok, St#server_st{channels = L2}}
        end;
    

handle(St,{leave, Channel,From}) ->
    case findPid(From, Channel ,St#server_st.channels) of
        true ->
            List = removeClient(From, Channel, St#server_st.channels),
            {reply, ok, St#server_st{channels = List}};
        false ->
            {user_not_joined, ok, St}
        end;

handle(St,{message_send, Channel, Msg, Nick, From}) ->
    case findPid(From, Channel ,St#server_st.channels) of
        true ->
            send_message(From, Channel, Nick, Msg, St#server_st.channels),
            {reply, ok, St};
        false ->
            {user_not_joined, ok, St}
        end.

send_message(From, Channel, Nick, Msg, [{C,X}|List]) ->
    case Channel == C of
        true ->
            client_send(From, Channel, Nick, Msg, X);
        false ->
            send_message(From, Channel, Nick, Msg, List)
        end.
client_send(_,_,_,_,[]) ->
    ok;
client_send(Pid, Channel, Nick, Msg, [From | List]) ->
    io:fwrite("~p~n", [From]),
    case Pid /= From of
        true ->
            genserver:request(From,{message_receive, Channel, Nick, Msg});
        false -> 
            client_send(Pid, Channel, Nick, Msg, List)
        end.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    exit(ServerAtom, normal).
