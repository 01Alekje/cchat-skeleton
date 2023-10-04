-module(server).
-export([start/1,handle/2,stop/1]).

-record(server_st, {
    channels
}).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    Pid = genserver:start(
        ServerAtom,
        [],
        fun handle/2
    ),
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % Pid = spawn(fun () -> 
    %     ServerState = server:initial_state(),
    %     genserver:start(ServerAtom, ServerState, handle/2)
    %     end
    % ),
    % - Register this process to ServerAtom
    %register(ServerAtom, Pid), % how to register process (Pid) to ServerAtom???????????????????????? this no work??
    % - Return the process ID
    io:fwrite("~p~n", [registered()]),
    Pid.

% {request, self(), Ref, Data} skickas kanske hit..?
% matcha med skiten i genserver!
% {request, self(), Ref, Data} matcha med detta
handle(St, {join, Channel, ClientPid}) ->
    io:fwrite("~p~n", ["handle join thingy"]),
    NewChannels = [Channel | St#server_st.channels],
    NewState = #server_st{channels = NewChannels},
    ClientPid ! {reply, joined, self()},
    {reply, ok, NewState}.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    ServerAtom ! stop,
    ok.
