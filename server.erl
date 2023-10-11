-module(server).
-export([start/1,stop/1]).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    genserver:start(ServerAtom, [], fun handle/2).

handle(St, {join, Channel, ClientPid}) ->
    case lists:member(Channel, St) of
        true -> % Channel exists
            % send join request to channel
            Result = genserver:request(list_to_atom(Channel), {join, ClientPid}),
            
            case Result of
                joined -> {reply, joined, St};
                failed -> {reply, failed, St}
            end;
        false -> % Channel doesn't exist
            % start a new channel (server with channelHandle/2 instead of handle/2)
            genserver:start(list_to_atom(Channel), [ClientPid], fun channelHandle/2),
            % add client to channel
            {reply, joined, [Channel | St]}
    end;

handle(St, {leave, Channel, ClientPid}) ->
    case lists:member(Channel, St) of
        true -> % Channel exists
            % send leave request to channel
            Result = genserver:request(list_to_atom(Channel), {leave, ClientPid}),
            case Result of
                leaved -> {reply, leaved, St};
                failed -> {reply, failed, St}
            end;
        false -> % Channel doesn't exist
            {reply, failed, St}
    end;

% function for stopping all channels
handle(St, stop_channels) -> 
    % for every channel in St (St is just a list of channels)
    lists:foreach(
        fun(Channel) -> 
            % send stop request to channel
            genserver:stop(list_to_atom(Channel))
        end,
    St),
    {reply, ok, []}.

channelHandle(Clients, {message_send, Channel, Nick, Msg, Sender}) -> 
    case lists:member(Sender, Clients) of
        true -> % Sender exists in Clients
            % spawn a new process that handles the message 
            spawn(fun() -> lists:foreach(
                fun(Receiver) ->
                        case Receiver == Sender of
                            true  -> skip; %% don't send message to sender
                            false -> genserver:request(Receiver, {message_receive, Channel, Nick, Msg})
                        end
                end,
                Clients) end),
            {reply, ok, Clients};
        
        false -> {reply, failed, Clients}
    end;

channelHandle(Clients, {leave, Client}) ->
    case lists:member(Client, Clients) of
        % client is part of this channel, so remove the client
        true  -> {reply, leaved, lists:delete(Client, Clients)};
        % client is not part of this channel, so respond with failed and don't remove said client
        false -> {reply, failed, Clients}
    end;

channelHandle(Clients, {join, Client}) ->
    case lists:member(Client, Clients) of
        true -> % client is already in the channel
            {reply, failed, Clients};
        false -> % client is not in channel, respond with joined
            {reply, joined, [Client | Clients]}
    end.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % stop all channels before stopping the 'main' server
    genserver:request(ServerAtom, stop_channels),
    genserver:stop(ServerAtom).