package heuristics;

import components.Solution;

/**
 * Class to implement the Davis' Hill Climbing algorithm
 * @author Felix
 *
 */
public class DavisHillClimbing implements Heuristic {

	@Override
	/**
	 * applyHeuristic method to perform hill climbing on the currentSolution to search the local solution space for fitter solutions
	 * @param currentSolution The solution to hill climb on
	 * @param dos The Depth of Search parameter
	 * @param iom The Intensity of Mutation parameter
	 * @return The fitness of solution found after hill climbing
	 */
	public double applyHeuristic(Solution currentSolution, double dos, double iom) {
		// TODO Auto-generated method stub
		
		AdjacentSwap mutationHeuristic = new AdjacentSwap();
		int counter = 0;
		Solution bestCandidate = currentSolution;
		Solution candidate;
		double bestFitness, candidateFitness;
		bestFitness = currentSolution.getCost();
		bestCandidate = currentSolution.deepClone();
		while(counter < Math.floor(dos * 10)) {
			
			for(int i = 0; i < currentSolution.getSolutionLength(); i++) {
				candidate = bestCandidate.deepClone();
				candidateFitness = mutationHeuristic.applyHeuristic(candidate, i);
				if(candidateFitness < bestFitness) {
					bestCandidate = candidate.deepClone();
					bestFitness = bestCandidate.getCost();
				}
			}
			
			// Handles swapping the last city with the first city
			candidate = bestCandidate.deepClone();
			candidateFitness = mutationHeuristic.applyHeuristic(candidate, currentSolution.getSolutionLength() - 1);
			if(candidateFitness < bestFitness) {
				bestCandidate = candidate.deepClone();
				bestFitness = bestCandidate.getCost();
			}
			counter++;
		}

		currentSolution = bestCandidate.deepClone();
		return currentSolution.getCost();
	}

}
