-module(server).
-export([start/1,stop/1,loop/1]).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) -> 
    spawn(fun () -> genserver::start(ServerAtom, State, do_this) end).

do_this(State, Data) ->
    case catch(Data) of 
        {join , Channel} -> {reply, ok, State},
    end.
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
join(ServerAtom,Channel) ->
    ServerAtom ! {join, Channel, self()},
    % TODO make list for a server that contains all channels and if channels does not exist make new
    % basically like channels[]

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    not_implemented.
