package heuristics;

import java.util.ArrayList;
import java.util.Random;

import components.Solution;
import repast.simphony.space.grid.GridPoint;

/**
 * Class to implement the Random Reinsertion heuristic
 * @author Felix
 *
 */
public class RandomReinsertion implements Heuristic {

	@Override
	/**
	 * @param currentSolution The solution to apply the heuristic to
	 * @param dos The Depth of Search parameter
	 * @param The Intensity of Mutation parameter
	 * @return The fitness of the solution as a result of this heuristic
	 */
	public double applyHeuristic(Solution currentSolution, double dos, double iom) {
		for(int counter = 0; counter < iom; counter++) {
		// TODO Auto-generated method stub
		
			ArrayList<GridPoint> solutionRepresentation = currentSolution.getSolutionRepresentation();
			Random r = new Random();
			int cityIndex, reinsertionIndex;
			cityIndex = r.nextInt(solutionRepresentation.size() - 1);
			reinsertionIndex = cityIndex;
			
			while(cityIndex == reinsertionIndex) {
				reinsertionIndex = r.nextInt(solutionRepresentation.size() - 1);
			}
					
			GridPoint tmp = solutionRepresentation.remove(cityIndex);
			solutionRepresentation.add(reinsertionIndex, tmp);
			currentSolution.setSolutionRepresentation(solutionRepresentation);
			currentSolution.calculateCostInclusive();
		}

		return currentSolution.getCost();
	}

}
