/* MPI Program Template */

#include <stdio.h>
#include <string.h>
#include "mpi.h"
#include <fstream>
#include <bits/stdc++.h>
using namespace std;
typedef long long int ll;

void quicksort(int *arr, int n){
    if(n <= 1) return;
    swap(arr[rand() % n], arr[n - 1]);
    int pivot = arr[n - 1];
    int j = -1;
    for(int i = 0; i < n; i++){
        if(arr[i] <= pivot) swap(arr[++j], arr[i]);
    }
    quicksort(arr, j);
    quicksort(arr + j + 1, n - j - 1);
}

void merge_arr(int *arr, int st, int mid, int en){
    int n1 = mid - st, n2 = en - mid;
    int arr1[n1], arr2[n2];
    for(int i = 0; i < n1; i++) arr1[i] = arr[i + st];
    for(int i = 0; i < n2; i++) arr2[i] = arr[i + mid];
    int i = 0, j = 0, l = st;
    while(i < n1 && j < n2){
        if(arr1[i] <= arr2[j]) arr[l++] = arr1[i++];
        else arr[l++] = arr2[j++];
    }
    while(i < n1) arr[l++] = arr1[i++];
    while(j < n2) arr[l++] = arr2[j++];
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
            int n, *arr;
            // reading input from the file
            {
                ifstream file;
                string filename = argv[argc - 2];
                file.open(filename.c_str());
                file >> n;
                arr = (int*) malloc(n * sizeof(int));
                for(int i = 0, x; i < n; i++){
                    file >> x;
                    arr[i] = x;
                }
                file.close();
            }

            int d = n / numprocs, a = d + n % numprocs;
            for(int i = 1; i < numprocs; i++){
                MPI_Send(arr + a + d * (i - 1), d , MPI_INT, i, 0, MPI_COMM_WORLD);
            }
            quicksort(arr, a);
            for(int i = 1; i < numprocs; i++){
                MPI_Recv(arr + a, d, MPI_INT, i, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
                merge_arr(arr, 0, a, a + d);
                a += d;
            }

            // writing output to the file
            {
                ofstream file;
                string filename = argv[argc - 1];
                file.open(filename.c_str());
                for(int i = 0; i < n; i++){
                    file << arr[i] << " ";
                }
                cout << "\n";
                file.close();
            }
        }
        else{
            MPI_Status status;
            MPI_Probe(0, 0, MPI_COMM_WORLD, &status);
            int n;
            MPI_Get_count(&status, MPI_INT, &n);
            int arr[n];
            MPI_Recv(arr, n, MPI_INT, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
            quicksort(arr, n);
            MPI_Send(arr, n , MPI_INT, 0, 0, MPI_COMM_WORLD);
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