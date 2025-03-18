package heuristics;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Random;

import org.apache.commons.lang3.ArrayUtils;

import components.Solution;
import repast.simphony.space.grid.GridPoint;
/**
 * Class that implements the OX1 crossover heuristic between two parents to produce offspring
 * @author Felix
 *
 */
public class OrderedCrossover implements CrossoverHeuristic {

	@Override
	/**
	 * @param parent1 The first parent
	 * @param parent2 The second parent
	 * @param dos The Depth of Search parameter
	 * @param iom The Intensity of Mutation parameter
	 * @return The offspring solution generated from this crossover heuristic
	 */
	public Solution applyHeuristic(Solution parent1, Solution parent2, double dos, double iom) {
		// TODO Auto-generated method stub
		Random r = new Random();
		GridPoint[] parent1Tour = parent1.getSolutionRepresentation().toArray(new GridPoint[0]);
		GridPoint[] parent2Tour = parent2.getSolutionRepresentation().toArray(new GridPoint[0]);
		
		int tourLength = parent1Tour.length;
		
		GridPoint[] child1Tour = new GridPoint[tourLength];
		GridPoint[] child2Tour = new GridPoint[tourLength];
		
		Arrays.fill(child1Tour, null);
		Arrays.fill(child2Tour, null);
		 
		int index1 = r.nextInt(tourLength);
		int index2 = index1;
		
		while(index2 == index1) {
			index2 = r.nextInt(tourLength);
		}
		
		// Step 1
		for(int i = smaller(index1, index2); i < larger(index1, index2); i++) {
			child1Tour[i] = parent1Tour[i];
		}
		
		int childIndex = larger(index1, index2);
		int parentIndex = larger(index1, index2);
		while(ArrayUtils.contains(child1Tour, null)) {
			//If the child contains what is in parentIndex in parent2Tour
			if(ArrayUtils.contains(child1Tour, parent2Tour[parentIndex])) {
				//Increment the parentIndex Counter
				parentIndex++;
			}
			//If the element in parentIndex of parent2Tour isnt contained in child1Tour
			else {
				child1Tour[childIndex] = parent2Tour[parentIndex];
				childIndex++;
			}
			
			if(parentIndex == child1Tour.length) {
				parentIndex = 0;
			}
			if(childIndex == child1Tour.length) {
				childIndex = 0;
			}
		}
		
		childIndex = larger(index1, index2);
		parentIndex = larger(index1, index2);
		
		for(int i = smaller(index1, index2); i < larger(index1, index2); i++) {
			child2Tour[i] = parent2Tour[i];
		}
		
		while(ArrayUtils.contains(child2Tour, null)) {
			//If the child contains what is in parentIndex in parent2Tour
			if(ArrayUtils.contains(child2Tour, parent1Tour[parentIndex])) {
				//Increment the parentIndex Counter
				parentIndex++;
			}
			//If the element in parentIndex of parent2Tour isnt contained in child1Tour
			else {
				child2Tour[childIndex] = parent1Tour[parentIndex];
				childIndex++;
			}
			
			if(parentIndex == child2Tour.length) {
				parentIndex = 0;
			}
			if(childIndex == child2Tour.length) {
				childIndex = 0;
			}
		}
		double c = r.nextDouble();
		GridPoint[] childTourArray = (c < 0.5) ? child1Tour : child2Tour;
		ArrayList<GridPoint> finalChildTour = new ArrayList<GridPoint>();
		Collections.addAll(finalChildTour, childTourArray);
		Solution offspring = new Solution(parent1.getHost(), finalChildTour);
		return offspring;
	}
	
	/**
	 * Function to return the larger of two integers (Returns the first if they are equal)
	 * @param a The first integer
	 * @param b The second integer
	 * @return The larger of the two integers
	 */
	public int larger(int a, int b) {
		int ret = (a < b) ? b : a;
		return ret;
	}
	
	/**
	 * Function to return the smaller of two integers (Returns the second if they are equal)
	 * @param a The first integer
	 * @param b The second integer
	 * @return The smaller of the two integers
	 */
	public int smaller(int a, int b) {
		int ret = (a < b) ? a : b;
		return ret;
	}

}
