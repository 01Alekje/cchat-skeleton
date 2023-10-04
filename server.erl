-module(server).
-export([start/1,handle/2,stop/1]).

-record(server_st, {
    name,
    clients,
    channels
}).

initServer(ServerAtom) -> 
    #server_st{
        name = ServerAtom,
        clients = [],
        channels = []
    }.

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    genserver:start(ServerAtom, initServer(ServerAtom), fun handle/2).

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
    genserver:stop(ServerAtom).
