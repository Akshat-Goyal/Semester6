/* MPI Program Template */

#include <stdio.h>
#include <string.h>
#include "mpi.h"
#include <fstream>
#include <bits/stdc++.h>
using namespace std;
typedef long long int ll;

int main( int argc, char **argv ) {
    int rank, numprocs;

    /* start up MPI */
    MPI_Init( &argc, &argv );

    MPI_Comm_rank( MPI_COMM_WORLD, &rank );
    MPI_Comm_size( MPI_COMM_WORLD, &numprocs );
    
    /*synchronize all processes*/
    MPI_Barrier( MPI_COMM_WORLD );
    double tbeg = MPI_Wtime();

    /* write your code here */

    if(argc != 3){
        if(rank == 0){
            cout << "Error: wrong arguments given!\n";
        }
    }
    else{
        int n, m;
        vector<pair<int, int>> edges;
        if(rank == 0){
            // reading input from the file
            ifstream file;
            string filename = argv[argc - 2];
            file.open(filename.c_str());
            file >> n >> m;
            for(int i = 0, u, v; i < m; i++){
                file >> u >> v;
                edges.push_back({u, v});
            }
            file.close();
        }
        // broadcasting n, m, edges to all processes.
        MPI_Bcast(&n, 1, MPI_INT, 0, MPI_COMM_WORLD);
        MPI_Bcast(&m, 1, MPI_INT, 0, MPI_COMM_WORLD);
        edges.resize(m);
        MPI_Bcast(&(edges[0].first), 2 * m, MPI_INT, 0, MPI_COMM_WORLD);

        // building line graph.
        vector<set<int>> lgraph(m);
        {
            vector<vector<int>> graph(n + 1);
            for(int i = 0; i < m; i++){
                graph[edges[i].first].push_back(i);
                graph[edges[i].second].push_back(i);
            }
            for(int i = 0; i < m; i++){
                for(auto j:graph[edges[i].first]){
                    if(j == i) continue;
                    lgraph[i].insert(j);
                }
                for(auto j:graph[edges[i].second]){
                    if(j == i) continue;
                    lgraph[i].insert(j);
                }
            }
        }
        
        // color[i] is color of node[i] for i 0 to m - 1, color[m] is no. of colored nodes.
        vector<int> color(m + 1, 0);
        if(rank == 0){
            // finding delta = max of degree of nodes.
            int delta = 0;
            for(int i = 0; i < m; i++){
                delta = max(delta, (int)lgraph[i].size());
            }
            delta += 1;
            // assigning random color to nodes from 1 to delta.
            for(int i = 0; i < m; i++){
                color[i] = rand() % delta + 1;
            }
        }
        MPI_Bcast(&(color[0]), m + 1, MPI_INT, 0, MPI_COMM_WORLD);

        if(rank == 0){
            // uncolored nodes.
            set<int> nodes;
            for(int i = 0; i < m; i++) nodes.insert(i);
            vector<bool> iscolored(m, false);

            // while there are uncolored nodes
            while(nodes.size()){
                // nodes whose neighbours have less color value than it, form independent set.
                vector<int>iset;
                for(auto it = nodes.begin(); it != nodes.end();){
                    int mx = 0;
                    for(auto j:lgraph[*it]){
                        if(iscolored[j]) continue;
                        mx = max(mx, color[j]);
                    }
                    if(color[*it] >= mx){
                        iset.push_back(*it);
                        it = nodes.erase(it);
                    }
                    else{
                        it++;
                    }
                }
                // distribute iset equally among processes
                int size = iset.size(), d = size / numprocs, a = d + size % numprocs;
                for(int i = 1; i < numprocs; i++){
                    MPI_Send(&iset[0] + a + d * (i - 1), d , MPI_INT, i, 0, MPI_COMM_WORLD);
                }

                for(int i = 0; i < a; i++){
                    // assign min color to node not in its neighbour.
                    int mn = 1;
                    for(auto j:lgraph[iset[i]]){
                        if(color[j] == mn) mn++;
                        else break;
                    }
                    color[iset[i]] = mn;
                    iscolored[iset[i]] = true;
                    color[m]++;
                }
                for(int i = 1; i < numprocs; i++){
                    // for each process, receives their colored nodes and updates color array.
                    vector<pair<int, int>> colored(d);
                    MPI_Recv(&(colored[0].first), 2 * d, MPI_INT, i, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
                    for(auto j:colored){
                        color[j.first] = j.second;
                        iscolored[j.first] = true;
                        color[m]++;
                    }
                }
                // broadcasts updated color array to processes.
                MPI_Bcast(&(color[0]), m + 1, MPI_INT, 0, MPI_COMM_WORLD);
            }
        }
        else{
            while(color[m] != m){
                MPI_Status status;
                MPI_Probe(0, 0, MPI_COMM_WORLD, &status);
                int size;
                MPI_Get_count(&status, MPI_INT, &size);
                vector<int> iset(size);
                MPI_Recv(&iset[0], size, MPI_INT, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
                // array of {node no., color}.
                vector<pair<int, int>> colored(size);
                for(int i = 0; i < size; i++){
                    // assign min color to node not in its neighbour.
                    int mn = 1;
                    for(auto j:lgraph[iset[i]]){
                        if(color[j] == mn) mn++;
                        else break;
                    }
                    colored[i] = {iset[i], mn};
                }
                // updated nodes sent to root.
                MPI_Send(&(colored[0].first), 2 * size , MPI_INT, 0, 0, MPI_COMM_WORLD);
                // updated color array received from root.
                MPI_Bcast(&(color[0]), m + 1, MPI_INT, 0, MPI_COMM_WORLD);
            }
        }

        if(rank == 0){
            // writing output to the file
            ofstream file;
            string filename = argv[argc - 1];
            file.open(filename.c_str());
            for(int i = 0; i < m; i++){
                file << color[i] << " ";
            }
            cout << "\n";
            file.close();
        }
    }
    
    MPI_Barrier( MPI_COMM_WORLD );
    double elapsedTime = MPI_Wtime() - tbeg;
    double maxTime;
    MPI_Reduce( &elapsedTime, &maxTime, 1, MPI_DOUBLE, MPI_MAX, 0, MPI_COMM_WORLD );
    if ( rank == 0 ) {
        printf( "Total time (s): %f\n", maxTime );
    }

    /* shut down MPI */
    MPI_Finalize();
    return 0;
}