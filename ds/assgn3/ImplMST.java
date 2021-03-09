import java.util.*;

// Implementing the remote interface 
public class ImplMST implements MST {  
   
   Map<String, Graph> mapOfGraph;

   public ImplMST(){
      this.mapOfGraph = new HashMap<String, Graph>();
   }

   public void add_graph(String graph_identifier, int n){
      mapOfGraph.put(graph_identifier, new Graph(n));
   }

   public void add_edge(String graph_identifier, int u, int v, int w){
      mapOfGraph.get(graph_identifier).add_edge(u, v, w);
   }

   public int get_mst(String graph_identifier){  
      return mapOfGraph.get(graph_identifier).get_mst();
   }  
} 
