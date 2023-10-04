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
    io:fwrite("~p~n", [St#client_st.server]),
    
    % Send a request to the client process (St#client_st.server) and wait for a response.
    Response = genserver:request(St#client_st.server, {join, Channel, self()}),

    % Handle the response. 
    case Response of
        {exit, Ref, Reason} ->
            % Transform the exit message into the desired format.
            ErrorAtom = my_error_atom,
            {error, ErrorAtom, "Error message stuff"};
        _ ->
            % Handle other response cases here.
            Response
    end;
    % add this to channels
        %{reply, ok, St#client_st{channels = [Channel | St#client_st.channels]}},

% Leave channel
handle(St, {leave, Channel}) ->
    io:fwrite("~p~n", ["client got leave message"]),
    % TODO: Implement this function
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "leave not implemented"}, St};

% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
    io:fwrite("~p~n", ["client got some message"]),
    % TODO: Implement this function
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "message sending not implemented"}, St} ;

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
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .
