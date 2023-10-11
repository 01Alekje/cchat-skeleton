-module(server).
-export([start/1,stop/1]).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    genserver:start(ServerAtom, [], fun handle/2).



handle(St, {join, Channel, ClientPid}) ->
    % finns kanalen
    case lists:member(Channel, St) of
        true -> % lÃ¤gg till client i genservern
            Result = genserver:request(list_to_atom(Channel), {join, ClientPid}),
            case Result of
                joined -> {reply, joined, St};
                failed -> {reply, failed, St}
            end;
        false -> 
            genserver:start(list_to_atom(Channel), [ClientPid], fun channelHandle/2),
            % [ChannelServer | St#server_st.channels], % add channel process to list of channels
            {reply, joined, [Channel | St]} % add client to channel
    end;

handle(St, {leave, Channel, ClientPid}) ->

    case lists:member(Channel, St) of
        % kanalen finns
        true -> Result = genserver:request(list_to_atom(Channel), {leave, ClientPid}),
            case Result of
                leaved -> {reply, leaved, St};
                failed -> {reply, failed, St}
            end;
        false -> {reply, failed, St}
    end;

handle(St, kill_Channels) -> 
    lists:foreach(
        fun(Channel) -> 
            genserver:stop(list_to_atom(Channel))
        end,
    St),
    {reply, ok, []}.

% handle(St, {message_send, Channel, Nick, Msg, Sender}) ->
    % genserver:request(list_to_atom(Channel), {message_send, Channel, Nick, Msg, Sender}),
    % {reply, ok, St}.


channelHandle(Clients, {message_send, Channel, Nick, Msg, Sender}) -> 
    case lists:member(Sender, Clients) of
        true ->
            lists:foreach(
                fun(Receiver) ->
                    case Receiver == Sender of
                        true  -> skip;
                        false -> genserver:request(Receiver, {message_receive, Channel, Nick, Msg})
                    end
                end,
                Clients),
            {reply, ok, Clients};
        
        false -> {reply, failed, Clients}
    end;

channelHandle(Clients, {leave, Client}) ->
    case lists:member(Client, Clients) of
        true  -> {reply, leaved, lists:delete(Client, Clients)};
        false -> {reply, failed, Clients}
    end;

channelHandle(Clients, {join, Client}) ->
    case lists:member(Client, Clients) of
        true ->
            {reply, failed, Clients};
        false ->
            {reply, joined, [Client | Clients]}
    end.


% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    genserver:request(ServerAtom, kill_Channels),
    genserver:stop(ServerAtom).