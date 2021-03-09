import java.rmi.registry.LocateRegistry; 
import java.rmi.registry.Registry;  
import java.util.*;

public class Client {  
   private Client() {}  

   public static void main(String[] args) {
      try {  
         // Getting the registry 
         Registry registry = LocateRegistry.getRegistry(args[0], Integer.parseInt(args[1])); 
         
         // Looking up the registry for the remote object 
         MST stub = (MST) registry.lookup("MST"); 
         
         // Reading the input till EOF and Calling the remote method using the obtained object 
         Scanner input = new Scanner(System.in);
         while (input.hasNextLine()) {
            
            String line = input.nextLine();
            String[] params = line.split(" ");
            String graph_identifier = params[1];

            if(params[0].equals("add_graph")){
               int n = Integer.parseInt(params[2]);
               stub.add_graph(graph_identifier, n);
            }
            else if(params[0].equals("add_edge")){
               int u = Integer.parseInt(params[2]);
               int v = Integer.parseInt(params[3]);
               int w = Integer.parseInt(params[4]);
               stub.add_edge(graph_identifier, u, v, w);
            }
            else if(params[0].equals("get_mst")){
               int total_weight = stub.get_mst(graph_identifier);
               System.out.println(total_weight);
            }
            else{
               System.out.println("Error: Wrong command!!");
            }
         }
         input.close();
      }
      catch (Exception e) {
         System.err.println("Client exception: " + e.toString()); 
         e.printStackTrace(); 
      } 
   } 
}
