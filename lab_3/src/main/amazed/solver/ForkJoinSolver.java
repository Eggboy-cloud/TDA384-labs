package amazed.solver;

import amazed.maze.Maze;

import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.Stack;

/**
 * <code>ForkJoinSolver</code> implements a solver for
 * <code>Maze</code> objects using a fork/join multi-thread
 * depth-first search.
 * <p>
 * Instances of <code>ForkJoinSolver</code> should be run by a
 * <code>ForkJoinPool</code> object.
 */


public class ForkJoinSolver
    extends SequentialSolver
{
    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal.
     *
     * @param maze   the maze to be searched
     */
    public ForkJoinSolver(Maze maze)
    {
        super(maze);
    }

    @Override
    protected void initStructures()
    {
        predecessor = new HashMap<>();
        frontier = new Stack<>();
    }

    /**
     * Set of identifiers of all nodes visited so far during the
     * search.
     */
    protected static ConcurrentSkipListSet<Integer> visited = new ConcurrentSkipListSet<>();

    protected int startPos;
    protected List<ForkJoinSolver> players = new ArrayList<>();
    protected AtomicBoolean found = new AtomicBoolean();
    protected boolean first = true;
    int fstVal = 0;

    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal, forking after a given number of visited
     * nodes.
     *
     * @param maze        the maze to be searched
     * @param forkAfter   the number of steps (visited nodes) after
     *                    which a parallel task is forked; if
     *                    <code>forkAfter &lt;= 0</code> the solver never
     *                    forks new tasks
     */
    public ForkJoinSolver(Maze maze, int forkAfter)
    {
        this(maze);
        this.forkAfter = forkAfter;
        this.startPos = start;
    }

    public ForkJoinSolver(Maze maze, AtomicBoolean found, Map<Integer, Integer> predecessor, 
                          int startPos, int start)
    {
        this(maze);
        this.found = found;
        this.startPos = start;
        this.predecessor = predecessor;
        this.startPos = startPos;
        this.start = start;
    }

    /**
     * Searches for and returns the path, as a list of node
     * identifiers, that goes from the start node to a goal node in
     * the maze. If such a path cannot be found (because there are no
     * goals, or all goals are unreacheable), the method returns
     * <code>null</code>.
     *
     * @return   the list of node identifiers from the start node to a
     *           goal node in the maze; <code>null</code> if such a path cannot
     *           be found.
     */

    
    @Override
    public List<Integer> compute()
    {
        return parallelSearch();
    }

    private List<Integer> parallelSearch()
    {
        int player = maze.newPlayer(startPos);
        // start with start node
        frontier.push(startPos);
        // as long as not all nodes have been processed
        while (!frontier.empty() && !found.get()) {
            // get the new node to process
            int current = frontier.pop();
            // if current node has a goal
            if (maze.hasGoal(current)) {
                found.set(true);
                // move player to goal
                maze.move(player, current);
                // search finished: reconstruct and return path
                return pathFromTo(start, current);
            }
            // if current node has not been visited yet
                // move player to current node
                maze.move(player, current);
                
                int fstVal = 0;
                // for every node nb adjacent to current
                for (int nb: maze.neighbors(current)) {
                    // add nb to the nodes to be processed
                    if(!visited.contains(nb)) {
                        visited.add(nb);
                        predecessor.put(nb, current);
                        if(first) {
                            frontier.push(nb);
                            first = false;
                        }
                        else {                
                            ForkJoinSolver fork = new ForkJoinSolver(maze, found, predecessor, nb, start);
                            players.add(fork);
                            fork.fork();
                        } 
                    }   
                }
                if(fstVal != 0)
                    frontier.push(fstVal);
                first = true;
                
        }
        return joinPlayers();
    }
    private List<Integer> joinPlayers() {
        for (ForkJoinSolver fork : players) {
            List<Integer> result = fork.join();
            if(result != null)
                return result;
        }
        return null;
    }
}
