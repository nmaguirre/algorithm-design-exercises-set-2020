package engine;

import conventionalsearch.Engine;
import conventionalsearch.State;
import conventionalsearch.StateProblem;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.Stack;





/**
 * Title:        DepthFirstVisitedEngine
 * Description:  Class DepthFirstVisitedEngine implements a depth-first visited search strategy
                 which can be used with any instance of StateProblem.
 * @author Nazareno Aguirre 
 */

public class DepthFirstVisitedEngine<S extends State, P extends StateProblem<S>> implements Engine<S,P>  {
   
  
  /**
   * path stores the path to the success.
   */
  private List<S> path; 

  /*Internal representation of the StateProblem*/
  private P sp;
  
  /** 
   * Constructor for class DepthFirstVisitedEngine.  
   * @pre. true.
   * @post. Lists path is initialized as empty.
   */
  public DepthFirstVisitedEngine() {
    path = new LinkedList<S>();
  }
  
  /** 
   * Constructor for class DepthFirstVisitedEngine.
   * @param p is the search sp associated with the engine
     being created.
   * @pre. p!=null.
   * @post. A reference to p is stored in field sp. Lists
     path are initialized as empty.
   */
  public DepthFirstVisitedEngine(P p) {
    this.sp = p;
    path = new LinkedList<S>();
  }


  /** 
   * Starts the search for successful states for sp, following a 
   * depth-first with visited strategy.
   * @return true iff a successful state is found.
   * @pre. sp!=null.
   * @post. the search is performed, true is returned iff a       
     successfull state is found.
   */
  public S performSearch() {

    // we get the initial state
    S initialState = sp.initialState();
    Set<S> visited = new HashSet<S>();
    Stack<S> stack = new Stack<S>();
    stack.push(initialState);
    boolean found = false;
    S goal = null;
    while (!stack.isEmpty() && !found) {
      S curr = stack.pop(); 
      if (!visited.contains(curr)) {
        visited.add(curr);
      }  
      if (curr.isSuccess()) {
        found = true;
        goal = curr;
      } else {
        List<S> succs = sp.getSuccessors(curr);
        for (S s: succs) {
          if (!visited.contains(s)) {
            stack.push(s);
          }

        }
      } 
    }
    if (!(goal == null)) {
      S s = goal;
      while (!(s == null)) {
        path.add(0,s);
        s = (S)s.getParent();
      }
    }
    return goal;
  }
  
   
  
  /** 
   * Returns the path to a previously calculated successful state for problem.
   * @return the list of nodes corresponding to the path from the root to
     the successful node.
   * @pre. performSearch() has been executed and finished successfully.
   * @post. the path to the found success node is returned.  
   */
  public List<S> getPath() {
    return path;
  } // end of getPath()
  

  /** 
   * Reports information regarding a previously executed search.   
   * @pre. performSearch() has been executed and finished.
   * @post. A report regarding the search is printed to standard output.
   */    
  public void report() {
    System.out.println("Length of path to state when search finished: " + path.size());
    
  } // end of report()


}
