-module('2018101075_1').

-export([main/2, token/2]).

main(Input_file, Output_file) ->
    {N, Token} = input(Input_file),
    if 
        N > 1 -> 
            {ok, Fd_out} = file:open(Output_file, [write]),
            Nodes = create(N, Fd_out),
            connect(Nodes ++ [self()]),

            ID = 0,
            send(hd(Nodes), Token, ID),
            Token = rec(ID, Fd_out),

            file:close(Fd_out),
            done;
        true -> 
            done
    end.

input(Input_file) ->
    {ok, Fd} = file:open(Input_file, [read]),
    {ok, Input} = file:read(Fd, 1024),
    file:close(Fd),
    {N, String} = string:to_integer(Input),
    Token = list_to_integer(string:substr(String, 2, string:len(String) - 1)),
    {N, Token}.

create(N, Fd_out) ->
    [spawn(?MODULE, token, [ID, Fd_out]) || ID <- lists:seq(1, N - 1)].

connect([_]) -> ok;
connect([N1 | Nodes]) ->
    N1 ! {connect, hd(Nodes)},
    connect(Nodes).

token(ID, Fd_out) ->
    receive
        {connect, NxtNode} ->
            token(ID, NxtNode, Fd_out)
    end.

token(ID, NxtNode, Fd_out) ->
    Token = rec(ID, Fd_out),
    send(NxtNode, Token, ID).

send(NxtNode, Token, ID) ->
    NxtNode ! {token, Token, ID}.

rec(ID, Fd_out) ->
    receive
        {token, Token, SenderID} ->
            file:write(Fd_out, io_lib:fwrite("Process ~p received token ~p from process ~p.\n", [ID, Token, SenderID])),
            Token
    end.
