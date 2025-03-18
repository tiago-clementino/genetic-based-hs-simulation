package heuristics;

import components.Solution;

/**
 * Class that defines the signature for Swap based heuristics
 * @author Felix
 *
 */
public interface SwapHeuristic {
	
	/**
	 * Definition of the signature for the function used to apply the heuristic
	 * @param currentSolution The solution to make the swap to
	 * @param index1 The index in the city tour to swap with the next index (circuar)
	 * @return The fitness of the produced solution
	 */
	public double applyHeuristic(Solution currentSolution, int index1);
	
}
