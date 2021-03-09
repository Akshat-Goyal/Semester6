import java.rmi.Remote; 
import java.rmi.RemoteException;  

// Creating Remote interface for our application 
public interface MST extends Remote {  
   void add_graph(String graph_identifier, int n) throws RemoteException;
   void add_edge(String graph_identifier, int u, int v, int w) throws RemoteException;
   int get_mst(String graph_identifier) throws RemoteException;  
} 
