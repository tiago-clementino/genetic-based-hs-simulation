package heuristics;

import components.Solution;

/**
 * Interface to define the signature for non specific heuristics
 * @author Felix
 *
 */
public interface Heuristic {

	/**
	 * Method to apply the implemented heuristic to a given solution
	 * @param currentSolution The solution to apply the heuristic to
	 * @param dos The depth of search parameter
	 * @param iom The intensity of mutation parameter
	 * @return The fitness of the solution after having the Heuristic applied to it
	 */
	public double applyHeuristic(Solution currentSolution, double dos, double iom);
	
}
