package heuristics;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Random;

import components.Solution;
import repast.simphony.space.grid.GridPoint;

/**
 * The AdjacentSwap Heuristic
 * @author Felix
 *
 */
public class AdjacentSwap implements SwapHeuristic {

	@Override
	/**
	 *  @param currentSolution The solution to apply the swap to
	 *  @param The index to perform the city swap to
	 *  @return The fitness of the solution
	 */
	public double applyHeuristic(Solution currentSolution, int index1) {
		// TODO Auto-generated method stub
		
		ArrayList<GridPoint> solutionRepresentation = currentSolution.getSolutionRepresentation();
		int index2;
		if(index1 < 0 || index1 >= solutionRepresentation.size()) {
			Random r = new Random();
			index1 = r.nextInt(solutionRepresentation.size());
			System.out.println("Invalid city locations for AdjacentSwap... Randomly picking city locations.");
		}
		if(index1 >= solutionRepresentation.size() - 1) {
			index2 = 0;
		} 
		else {
			index2 = index1 + 1;
		}
		Collections.swap(solutionRepresentation, index1, index2);
		
		currentSolution.setSolutionRepresentation(solutionRepresentation);
		currentSolution.calculateCostInclusive();
		return currentSolution.getCost();
	}

}
