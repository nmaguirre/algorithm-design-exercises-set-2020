package examples.jugs;

import engine.BreadthFirstEngine;
import engine.DepthFirstVisitedEngine;
import java.util.List;

public class JugsSearchApp {
  /**
   * Main for Jugs App.
   * @param args contents Jug A and contents Jug B are expected.
   */
  
  public static void main(String[] args) {
    
    if ((args.length > 2) || (args.length == 0)) {
      System.out.println("*** Usage: java JugsSearchApp <int> <int>");
    } else {
      
      int a = Integer.parseInt(args[0]);
      int b = Integer.parseInt(args[1]);
      
      JugsStateProblem sp1 = new JugsStateProblem(a,b);
      
           
      /*App using Breadth-first visited search */ 
      BreadthFirstEngine<JugsState,JugsStateProblem> engine3 = 
          new BreadthFirstEngine<JugsState,JugsStateProblem>(sp1);
      JugsState successBfS = engine3.performSearch();
      System.out.println();     
      System.out.println("*** Result using Breadth-first search ***");
      System.out.println("Solution found? " + successBfS.toString());
      if (! (successBfS == null)) {
        System.out.print("Path to goal: ");
        List<JugsState> pathBfS = engine3.getPath();
        for (int i = 0; i < pathBfS.size(); i++) {
          JugsState current = (JugsState) pathBfS.get(i);
          System.out.print(current.toString());
        }
        System.out.println();
      }
      engine3.report();
      
      /*App using depth-first visited search */ 
      JugsStateProblem sp2 = new JugsStateProblem(a,b);
      
      DepthFirstVisitedEngine<JugsState,JugsStateProblem> engine2 = 
          new DepthFirstVisitedEngine<JugsState,JugsStateProblem>(sp2);
      JugsState success = engine2.performSearch();
      System.out.println();     
      System.out.println("*** Result using depth-first visited search ***");
      System.out.println("Solution found? " + success.toString());
      if (! (success == null)) {
        System.out.print("Path to goal: ");
        List<JugsState> path = engine2.getPath();
        for (int i = 0; i < path.size(); i++) {
          JugsState current = (JugsState) path.get(i);
          System.out.print(current.toString());
        }
        System.out.println();
      }
      engine2.report();     

    }
    
  } 
    
}
