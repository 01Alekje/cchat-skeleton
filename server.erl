-module(server).
-export([start/1,stop/1]).

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
    % finns kanalen
    case lists:member(Channel, St#server_st.channels) of
        true -> % lägg till client i genservern
            genserver:request(list_to_atom(Channel), {join, ClientPid}),
            {reply, joined, St};

        false -> 
            genserver:start(list_to_atom(Channel), [ClientPid], fun channelHandle/2),
            {reply, joined, [Channel | St#server_st.channels]}
    end.

channelHandle(St, {join, Client}) ->
    case lists:member(Client, St#server_st.clients) of
        true ->
            {reply, failed, St};
        false ->
            {reply, joined, [Client | St#server_st.clients]}
    end.



% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    genserver:stop(ServerAtom).
