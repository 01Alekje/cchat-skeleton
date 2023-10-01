-module(server).
-export([start/1,stop/1]).

% initial state
initial_state(ServerName) ->
    {servername = ServerName}.

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    Pid = spawn(fun () -> loop() end),
    true = register(ServerAtom, Pid),
    Pid.


loop() ->
    receive
        {join, From, Ref} ->
            From ! {ok, Ref},
            loop();
        {stop, _From, _Ref} -> ok
    end.


% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    ServerAtom ! stop,
    ok.
