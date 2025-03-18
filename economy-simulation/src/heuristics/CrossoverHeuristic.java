package heuristics;

import components.Solution;

/**
 * Interface defining the signature for the applyHeurstic function for Crossover heuristics
 * @author Felix
 *
 */
public interface CrossoverHeuristic {

	/**
	 * Method to apply the heuristic to the two parent solutions, returning their offspring
	 * @param parent1 The first parent
	 * @param parent2 The second parent
	 * @param dos The Depth of Search parameter
	 * @param iom The Intensity of Mutation parameter
	 * @return The offspring Solution
	 */
	public Solution applyHeuristic(Solution parent1, Solution parent2, double dos, double iom);
	
}
