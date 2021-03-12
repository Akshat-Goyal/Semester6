# Problem

Use RMI(Remote Method Invocation) in Java to implement a simple single server architecture with support for multiple clients. The details are as follows:

- The server maintains a list of graphs each associated with a distinct identifier.

- Clients can request to add a new graph, update an existing graph and query for the total weight of
the minimum weight spanning tree of a given graph.

- The server should be able to handle multiple clients simultaneously and should also work with clients
on other machines.

## Architecture

- RMI stands for Remote Method Invocation. The RMI allows an object to invoke methods on an object running in another JVM.
- The client can remotely call functions declared in the interface `MST` which are provided by the server and registered using the call `registry.bind("MST", stub)` in the Server.java code.
- `ImplMST` contains the implementation of the functions declared in `MST`.

## Algorithm

- HashMap is used for mapping graph_identifier to the Graph object.
- Graph class contains edge class for edges, kruskal class for minimum weight of spanning tree using kruskal algorithm, sortedSet for storing edges sorted by weight in ascending order.

## Results and Observations

For a graph with 1e<sup>5</sup> Nodes and around 1e<sup>5</sup> edges, it takes less than 2 seconds to return the weight of the MST.

## Running the code

```
javac *.java
java Server 2000
java Client 127.0.0.1 2000 < <input_file>
```

Client can enter 3 type of commands:

- add_graph <graph_identifier> <n>.
This command will add a new graph on the server with the identifier graph identifier and n number of nodes. The graph identifier is a string with a maximum length of 10 and it won’t already exist. n will be in the range: 1 <= n <= 100,000.

- add_edge <graph_identifier> <u> <v> <w>.
This will add an undirected edge between the nodes u and v with weight w. u and v are the node numbers of the endpoints of the edge such that 1 <= u, v <= n and 0 <= w <= 10,000. n is the number of nodes in the specified graph. A graph with identifier graph identifier will already exist. There can be multiple edges and self-loops added to the graph.

- get_mst <graph_identifier>.
This command returns total weight of minimum spanning tree. The client will print the solution the server returns. In case the graph does not have a spanning tree, -1 should be printed. A graph with identifier graph identifier will already exist.

## Input

```
add_graph graph1 4
get_mst graph1
add_edge graph1 1 2 10
add_edge graph1 2 3 15
add_edge graph1 1 3 5
add_edge graph1 4 2 2
add_edge graph1 4 3 40
get_mst graph1
```

## Output

```
-1
17
```