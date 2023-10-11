-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server, % atom of the chat server
    channels
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_st{
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom,
        channels = []
    }.

% handle/2 handles each kind of request from GUI
% Parameters:
%   - the current state of the client (St)
%   - request data from GUI
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to GUI, either the atom `ok` or a tuple {error, Atom, "Error message"}
%   - NewState is the updated state of the client

% Join channel
handle(St, {join, Channel}) ->
    
    try
        % Send a request to the client process (St#client_st.server) and wait for a response.
        Response = genserver:request(St#client_st.server, {join, Channel, self()}),

        % Handle the response. 
        case Response of
            joined -> {reply, ok, St#client_st{channels = [Channel | St#client_st.channels]}};
            failed -> {reply, {error, user_already_joined, "failed to join"}, St}
        end
    
    catch
        throw:timeout_error -> {reply, {error, server_not_reached, "server not reached"}, St};
        error:badarg -> {reply, {error, server_not_reached, "server not reached"}, St}
    end;


% Leave channel
handle(St, {leave, Channel}) ->

    try
        Response = genserver:request(St#client_st.server, {leave, Channel, self()}),

        case Response of
            leaved -> {reply, ok, St#client_st{channels = lists:delete(Channel, St#client_st.channels)}};
            failed -> {reply, {error, user_not_joined, "Client not in" ++ Channel}, St}
        end

    catch
        error:badarg -> {reply, ok, St}
    end;

    % {reply, ok, St} ;
    

% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->

    try
        Result = genserver:request(list_to_atom(Channel), {message_send, Channel, St#client_st.nick, Msg, self()}),
        case Result of
            ok -> {reply, ok, St};
            failed -> {reply,  {error, user_not_joined, "user not joined"}, St}
        end
    catch
        error:timeout_error -> {reply, {error, server_not_reached, "server not reached"}, St};
        error:badarg -> {reply, {error, server_not_reached, "server not reached"}, St}
    end;

% This case is only relevant for the distinction assignment!
% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
    {reply, ok, St#client_st{nick = NewNick}} ;

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St} ;

% Quit client via GUI
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, St} ;

% Catch-all for any unhandled requests
handle(St, Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St}.