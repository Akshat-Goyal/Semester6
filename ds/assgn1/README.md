
# Problem 1

Use the numerical identity that the sum of the reciprocals of the squares of integers converges to $\pi$<sup>2</sup> / 6.

## Description

The process 0 divides the range [1, n] equally and sends each process a pair {a, b} to calculate the sum of the reciprocals of the squares of integers from a to b. Each process sends the calculated value back to root process. The root process adds all the values and round it off to 6 decimal places.

## Analysis

For an array of 10000 elements, time taken (in sec) data is :
 
 - 1 Process:         0.000258
 - 5 Processes:     0.000215
 - 11 Processes:   0.000208

# Problem 2
  
Given an array of numbers, your task is to return the array in sorted order by implementing parallel quicksort.

## Description

First the array is equally partitioned by the process 0 among the processes and each process sort the array using randomized quicksort and sent it back to process 0. Then all this np (no. of process) no. of arrays are merged one by one. 

## Analysis

For an array of 100000 elements with values ranging from 0 to 1e9, time taken (in sec) data is :
 
 - 1 Process:         0.041
 - 5 Processes:     0.044
 - 11 Processes:   0.050

For an array of 1000000 elements with values ranging from 0 to 1e9, time taken (in sec) data is :
 
 - 1 Process:         0.55
 - 5 Processes:     0.33
 - 11 Processes:   0.86
  
For an array of 1000000 elements with values ranging from 0 to 1e3, time taken (in sec) data is :
 
 - 1 Process:         5.23
 - 5 Processes:     0.50
 - 11 Processes:   0.61


# Problem 3

Given an undirected graph G, find a proper edge coloring of the graph using 1 + max(Delta(G), Delta(line graph of G)) colors or fewer.
No 2 adjacent edges should have a same color. Delta(G) is the maximum degree of any vertex in G.

## Description

Process 0 broadcasts the edges to all processes and all the processes forms the line graph. Now each edge is a node in our graph. Process 0 gives random color from 1 to Delta to each node and broadcasts the color array.

Process 0 equally partitions the nodes among the processes and all the processes do the following:-

- All the uncolored nodes are selected which have the maximum color value among their uncolored neighbours. These nodes form the independent set.
- All the selected nodes are given the smallest color value not in their neighbour.
- All the process sends the array of selected nodes and their color value to the process 0. Process 0 updates the color array and broadcasts to all the processes.
- Above 2 steps are updated until all the nodes get colored.

## Analysis

For 120 nodes and 1276 edges, time taken (in sec) data is :
 
 - 1 Process:         0.093
 - 5 Processes:     0.052
 - 11 Processes:   0.109

For 128 nodes and 774 edges, time taken (in sec) data is :
 
 - 1 Process:         0.037
 - 5 Processes:     0.022
 - 11 Processes:   0.041

For 36 nodes and 580 edges, time taken (in sec) data is :
 
 - 1 Process:         0.087
 - 5 Processes:     0.042
 - 11 Processes:   0.121

 
# Running the code

```
mpic++ file.cpp

mpirun -np <no of processes> --use-hwthread-cpus --oversubscribe ./a.out <input file> <output file>
```