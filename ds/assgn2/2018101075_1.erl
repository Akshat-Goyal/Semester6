-module('2018101075_1').

-export([main/1, token/2]).

main(Args) ->
    [Input_file, Output_file] = Args,
    % reading input from file, {No. of processes, Token}
    {N, Token} = input(Input_file),
    if 
        N > 1 -> 
            % output file descriptor
            {ok, Fd_out} = file:open(Output_file, [write]),
            % creating N - 1 processes
            Nodes = create(N, Fd_out),
            % connects the nodes in ring fashion by sending process i pid of process (i + 1) % N
            connect(Nodes ++ [self()]),

            % current process is process 0
            ID = 0,
            % process 0 sends token to process 1
            send(hd(Nodes), Token, ID),
            % process 0 receives token from process N - 1
            Token = rec(ID, Fd_out),

            % closing output file descriptor
            file:close(Fd_out),
            done;
        true -> done
    end.

% reads input from the input file
input(Input_file) ->
    {ok, Fd} = file:open(Input_file, [read]),
    {ok, [N, Token]} = io:fread(Fd, [], "~d~d"),
    file:close(Fd),
    {N, Token}.

% creates N - 1 processes from 1 to N - 1
create(N, Fd_out) ->
    [spawn(?MODULE, token, [ID, Fd_out]) || ID <- lists:seq(1, N - 1)].

% sends process i pid of process (i + 1) % N for i -> 1 to N - 1
connect([_]) -> ok;
connect([N1 | Nodes]) ->
    N1 ! {connect, hd(Nodes)},
    connect(Nodes).

% process 1 to N - 1 receives the pid of next process and executes token/3
token(ID, Fd_out) ->
    receive
        {connect, NxtNode} ->
            token(ID, NxtNode, Fd_out)
    end.

% receives token from previous process and sends token to next process in ring fashion
token(ID, NxtNode, Fd_out) ->
    Token = rec(ID, Fd_out),
    send(NxtNode, Token, ID).

% process $ID sends Token to next node
send(NxtNode, Token, ID) ->
    NxtNode ! {token, Token, ID}.

% process $ID receives Token from previous node
rec(ID, Fd_out) ->
    receive
        {token, Token, SenderID} ->
            io:format(Fd_out, "Process ~p received token ~p from process ~p.\n", [ID, Token, SenderID]),
            Token
    end.
