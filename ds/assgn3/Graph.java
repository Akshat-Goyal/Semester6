import java.util.*;

// Graph
public class Graph {  
   
   // Edge
   class Edge implements Comparable<Edge>{  
   
      public int u, v, w;
   
      public Edge(int u, int v, int w){
         this.u = u;
         this.v = v;
         this.w = w;
      }
   
      public int compareTo(Edge edge){
         if(this.w == edge.w){
            if(this.v == edge.v) return this.u - edge.u;
            return this.v - edge.v;
         }
         return this.w - edge.w;
      }
   }

   int n;
   SortedSet<Edge> edges;

   public Graph(int n){
      this.n = n;
      this.edges = new TreeSet<Edge>();
   }

   // adds edge to the set of edges
   public void add_edge(int u, int v, int w){
      edges.add(new Edge(u, v, w));
   }

   // print all edges
   public void print_edges(){
      for(Edge edge: edges){
         System.out.println(edge.u + " " + edge.v + " " + edge.w);
      }
   }

   // Kruskal class to calculate MST
   class Kruskal {  
   
      int parents[];
      int ranks[];
   
      public Kruskal(){
         parents = new int[n + 1];
         ranks = new int[n + 1];
         for(int i = 1; i <= n; i++){
            parents[i] = i;
            ranks[i] = -1;
         }
      }
   
      int find(int u){
         if(parents[u] != u) parents[u] = find(parents[u]);
         return parents[u];
      }
   
      void union(int u, int v){
         if(ranks[u] < ranks[v]){
            parents[u] = v;
         }
         else if(ranks[u] > ranks[v]){
            parents[v] = u;
         }
         else{
            parents[v] = u;
            ranks[u]++;
         }
      }
   
      public int get_mst(){
         int total_weight = 0, count_edges = 0;
         for(Edge edge: edges){
            int pu = find(edge.u);
            int pv = find(edge.v);
            if(pu != pv){
               union(pu, pv);
               total_weight += edge.w;
               count_edges++;
               if(count_edges == n - 1) return total_weight;
            }
         }
         return -1;
      }
   } 

   // returns MST of graph
   public int get_mst(){  
      return new Kruskal().get_mst();
   }  
} 
