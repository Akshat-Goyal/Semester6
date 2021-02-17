-module('2018101075_2').

-export([main/1, parallel_bellman_ford/3]).

% defining INF = 1e10
-define(INF, 10000000000).

main(Args) ->
    [Input_file, Output_file] = Args,
    % reading input from file
    {P, N, M, Src, E} = input(Input_file),
    % creating (P - 1) processes 
    Processes = create(P, E, M div P + M rem P + 1, M div P),

    % subarray of E for process 0
    Edges = lists:sublist(E, M div P + M rem P),
    % Initial Dist array with Dist[Src] = 0 and rest INF
    Dist = [if Node == Src -> 0; true -> ?INF end || Node <- lists:seq(1, N)],
    % Final Dist array using Parallel Bellman Ford Algorithm
    FinalDist = for(Processes, Dist, Edges, P, N),
    % writes the FinalDist to the file
    output(FinalDist, N, Output_file).

% returns list of process pid
create(P, E, Start, Len) ->
    % creates (P - 1) processes and sends each process ID, Pid0, subarray of E
    % E is divided among processes
    [spawn(?MODULE, parallel_bellman_ford, [ID, self(), lists:sublist(E, Start + (ID - 1) * Len, Len)]) || ID <- lists:seq(1, P - 1)].

% recursive version of first for loop from 1 to (N - 1) of Bellman Ford Algo
% returns Dist array which has min path of each node from Src
for(Processes, Dist, _, _, N) when N == 1 ->
    % sends done message to other processes to exit
    lists:foreach(fun(Process) -> Process ! done end, Processes),
    Dist;
for(Processes, Dist, Edges, P, N) ->
    % sends Dist array to other processes
    lists:foreach(fun(Process) -> Process ! {dist, Dist} end, Processes),
    % executes relax_edges on its part of Edges to get new Dist array,
    % receives new Dist array from other processes recursively using receive_updated_dist and updates Dist array using min_dist and executes another for
    for(Processes, receive_updated_dist(relax_edges(Dist, Edges), P), Edges, P, N - 1).

% @params Dist: new dist array from process 0, P: no. of processes
% returns updated Dist array
receive_updated_dist(Dist, P) when P == 1 ->
    Dist;
receive_updated_dist(Dist, P) ->
    receive
        % recursively receives the new Dist array from process 1 to (P - 1) and updates the Dist array using min_dist
        {dist, NewDist} ->
            receive_updated_dist(min_dist(Dist, NewDist), P - 1)
    end.

% returns pairwise min of two Dist array
min_dist(Dist, NewDist) ->
    [lists:min([Val1, Val2]) || {Val1, Val2} <- lists:zip(Dist, NewDist)].

% process 1 to (P - 1) execute this function
parallel_bellman_ford(ID, Pid0, Edges) ->
    receive
        % receives dist array from process 0
        {dist, Dist} ->
            % relaxes each edge in Edges and sends back updated dist array to process 0 
            Pid0 ! {dist, relax_edges(Dist, Edges)},
            parallel_bellman_ford(ID, Pid0, Edges);
        % receives done message from process 0 after being called (N - 1) times and exits
        done -> ok
    end.

% recursively checks each edge and relaxes the edge
% returns the updated Dist array
relax_edges(Dist, []) -> Dist;
relax_edges(Dist, Edges) ->
    {U, V, W} = hd(Edges),
    DistU = lists:nth(U, Dist),
    DistV = lists:nth(V, Dist),
    if 
        DistV > DistU + W ->
            relax_edges(lists:sublist(Dist, V - 1) ++ [DistU + W] ++ lists:nthtail(V, Dist), tl(Edges));
        DistU > DistV + W ->
            relax_edges(lists:sublist(Dist, U - 1) ++ [DistV + W] ++ lists:nthtail(U, Dist), tl(Edges));
        true -> 
            relax_edges(Dist, tl(Edges))
    end.

% reads input from the file and returns input
input(Input_file) ->
    {ok, Fd} = file:open(Input_file, [read]),
    {ok, [P]} = io:fread(Fd, [], "~d"),
    {ok, [N, M]} = io:fread(Fd, [], "~d ~d"),
    E = [{U, V, W} || {ok, [U, V, W]} <- [io:fread(Fd, [], "~d ~d ~d") || _ <- lists:seq(1, M)]],
    {ok, [Src]} = io:fread(Fd, [], "~d"),
    file:close(Fd),
    {P, N, M, Src, E}.

% write list of {Node, min Distance of Node from Src} to the file
output(Dist, N, Output_file) ->
    {ok, Fd_out} = file:open(Output_file, [write]),
    lists:foreach(fun({Node, Val}) -> io:format(Fd_out, "~p ~p\n", [Node, Val]) end, lists:zip(lists:seq(1, N), Dist)),
    file:close(Fd_out).
