import java.rmi.registry.Registry; 
import java.rmi.registry.LocateRegistry; 
import java.rmi.RemoteException; 
import java.rmi.server.UnicastRemoteObject; 

public class Server extends ImplMST { 
   public Server() {} 
   public static void main(String args[]) { 
      try { 
         // Instantiating the implementation class 
         ImplMST obj = new ImplMST(); 
    
         // Exporting the object of implementation class  
         // (here we are exporting the remote object to the stub) 
         MST stub = (MST) UnicastRemoteObject.exportObject(obj, 0);  
         
         // creating registry
         LocateRegistry.createRegistry(Integer.parseInt(args[0]));
         
         // host address
         String host = "127.0.0.1";
         // Binding the remote object (stub) in the registry 
         Registry registry = LocateRegistry.getRegistry(host, Integer.parseInt(args[0])); 
         
         registry.bind("MST", stub);  
         System.out.println("Server ready"); 
      }
      catch (Exception e) { 
         System.err.println("Server exception: " + e.toString()); 
         e.printStackTrace(); 
      } 
   } 
} 
