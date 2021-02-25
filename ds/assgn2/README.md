# Problem 1

Write a program to pass an integer token value around all processes in a ring-like fashion, and make sure
that it does not have a deadlock.

## Description

1. (N - 1) processes are created where N is the no. of processes.
2. Each process i from 1 to (N - 1) is sent Pid of process (i + 1) % N to connect them in ring fashion.
3. Process 0 sends token to process 1.
4. Each process from 1 to (N - 1) receives token and sends token to next process in ring fashion.
5. Finally process 0 receives token back.

## Analysis

Message Complexity: 2 * N - 1, 
where (N - 1) for connect and N for token.

Time Complexity: O(N) + O(N) * Max message time.


# Problem 2

Given a non-negative weighted, undirected, connected graph and a source vertex in the graph, find the shortest paths from source to all vertices in the graph. You can use any algorithm to solve this problem. Assuming edge weights < 1e10.

## Description

1. (P - 1) processes are created where P is the no. of processes and V is the no. of vertices.
2. Edge list is distributed among processes.
3. Parallel Bellman Ford algorithm is implemented here.
4. Bellman Ford algo: for 1 to (V - 1) -> relax all the edges and update Dist array. After (V - 1) loops, we get final Distance array.
5. Here, for 1 to (V - 1) -> Process 0 shares Dist array with every process. Every process then relaxes their edges and returns new Distance array to process 0. Process 0 gets P Dist array and gets the updated Dist array using pairwise min function.
6. Finally, process 0 sends done message to all process to exit and writes output to the file.

## Analysis

Message Complexity: (P - 1)(2V - 1) or O(PV), 
where (V - 1) times message is sent to P - 1 processes and P - 1 processes send message to process 0. Then finally, process 0 sends done message to (P - 1) processes.

Time Complexity: O(VE/P) + O(PV) * Max message time.
Bellman Ford algo has O(VE) time complexity, but in parallel version E is divides among P processes working in parallel.


# Running the code

```
erlc <file>.erl

erl -noshell -s <file> main <input file> <output file> -s init stop
```