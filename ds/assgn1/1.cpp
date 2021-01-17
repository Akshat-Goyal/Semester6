/* MPI Program Template */

#include <stdio.h>
#include <string.h>
#include "mpi.h"
#include <fstream>
#include <bits/stdc++.h>
using namespace std;
typedef long long int ll;

double sumOfReciprocalOfSquares(int start, int end){
    double ans = 0;
    for(int i = start; i < end; i++){
        ans += (double) 1.0 / (i * i);
    }
    return ans;
}

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
        if(rank == 0){
            int n;
            // reading input from the file
            {
                ifstream file;
                string filename = argv[argc - 2];
                file.open(filename.c_str());
                file >> n;
                file.close();
            }

            int d = n / numprocs, st = 1, en = st + d + n % numprocs;
            for(int i = 1; i < numprocs; i++){
                pair<int, int> p = {st, en};
                MPI_Send(&p, 2 , MPI_INT, i, 0, MPI_COMM_WORLD);
                st = en;
                en += d;
            }
            double ans = sumOfReciprocalOfSquares(st, en);
            for(int i = 1; i < numprocs; i++){
                double ret;
                MPI_Recv(&ret, 1, MPI_DOUBLE, i, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
                ans += ret;
            }

            // writing output to the file
            {
                ofstream file;
                string filename = argv[argc - 1];
                file.open(filename.c_str());
                ans = round(ans * 1000000.0) / 1000000.0;
                file << fixed << setprecision(6) << ans << "\n";
                file.close();
            }
        }
        else{
            pair<int, int> p;
            MPI_Recv(&p, 2, MPI_INT, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
            double ans = sumOfReciprocalOfSquares(p.first, p.second);
            MPI_Send(&ans, 1 , MPI_DOUBLE, 0, 0, MPI_COMM_WORLD);
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