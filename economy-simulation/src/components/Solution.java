package components;

import java.util.ArrayList;

import agents.Collector;
import repast.simphony.space.grid.GridPoint;

/**
 * Class to encapsulate a single solution to the TSP problem
 * @author Felix
 *
 */
public class Solution {

	private double cost;
	private int solutionLength;
	private ArrayList<GridPoint> solutionRepresentation;
	private Collector host;
	
	/**
	 * Constructor to create a new, blank solution
	 * @param hostAgent Reference to the collector that contains the solution
	 */
	public Solution(Collector hostAgent) {
		this.solutionRepresentation = new ArrayList<GridPoint>();
		this.host = hostAgent;
		this.cost = 0;
	}
	
	/**
	 * Polymorphic constructor to create a new Solution object with a given TSP tour
	 * @param hostAgent Reference to the collector that contains the solution
	 * @param solRep Predefined TSP tour
	 */
	public Solution(Collector hostAgent, ArrayList<GridPoint> solRep) {
		this.solutionRepresentation = solRep;
		this.host = hostAgent;
		finaliseCollection();
	}
	
	/**
	 * Method to return this solutions route
	 * @return The solution representation
	 */
	public ArrayList<GridPoint> getSolutionRepresentation() {
		return this.solutionRepresentation;
	}
	
	/**
	 * Method to set this solutions tour
	 * @param solutionRepresentation ArrayList containing the tour to set give this solution
	 */
	public void setSolutionRepresentation(ArrayList<GridPoint> solutionRepresentation) {
		this.solutionRepresentation = solutionRepresentation;
		this.solutionLength = solutionRepresentation.size();
		this.cost = calculateCostInclusive();
	}
	
	/**
	 * Method to finalise the mapping of the hs in the spatial coordinate system
	 */
	public void finaliseCollection() {
		containsDuplicates("Caller=finaliseCollection() in Solution.java");
		solutionLength = solutionRepresentation.size();
		this.cost = calculateCostInclusive(); 
	}
	
	/**
	 * Setter method to set the fitness of the solution
	 * @param cost The fitness of the solution
	 */
	public void setCost(double cost) {
		this.cost = cost;
	}
	
	/**
	 * Getter method to return the fitness of the solution
	 * @return The fitness of the solution
	 */
	public double getCost() {
		return this.cost;
	}
	
	/**
	 * Method to return the Collector object that owns this solution
	 * @return Reference to the owner of the Solution
	 */
	public Collector getHost() {
		return this.host;
	}
	
	/**
	 * Method to add a new GridPoint to the path
	 * @param newPoint The new GridPoint location to add to the tour
	 */
	public void addPoint(GridPoint newPoint) {
		//System.out.println("Added hs at coordinates X=" + newPoint.getX() + ", Y=" + newPoint.getY());
		solutionRepresentation.add(newPoint);
	}
	
	/**
	 * Getter to return the number of cities in the TSP tour (i.e. Number of hs objects)
	 * @return The length of the solution
	 */
	public int getSolutionLength() {
		return this.solutionLength;
	}
	
	/**
	 * Method to remove a GridPoint from the tour
	 * @param point The point to find and remove
	 * @return A boolean denoting whether or not the point in questin was removed from the tour
	 */
	public boolean removePoint(GridPoint point) {
		if(solutionRepresentation.contains(point)) {
			solutionRepresentation.remove(point);
			
			return true;
		}
		else {
			return false;
		}
	}
	/**
	 * Method to calculate and return the total cost of the solution, including the distance from the Collector to the first hs location
	 * @return The fitness of the solution
	 */
	public double calculateCostInclusive() {
		double accumulatedCost = 0;
		accumulatedCost += calculateEuclideanDistance(host.grid.getLocation(host), solutionRepresentation.get(0));
		accumulatedCost += calculateCostExclusive();
		
		return accumulatedCost;
	}
	
	/**
	 * Method to calculate and return the total cost of the solution, excluding the distance from the Collector to the first hs location
	 * @return The fitness of the solution
	 */
	public double calculateCostExclusive() {
		double accumulatedCost = 0;
		for(int i = 0; i < solutionLength - 1; i++) {
			accumulatedCost += calculateEuclideanDistance(solutionRepresentation.get(i), solutionRepresentation.get(i + 1));
		}
		
		return accumulatedCost;
	}
	
	/**
	 * Method to calculate the Euclidean Distance between two GridPoints
	 * @param point1 The first GridPoint
	 * @param point2 The second GridPoint
	 * @return The euclidean (straight line) distance between them
	 */
	public static double calculateEuclideanDistance(GridPoint point1, GridPoint point2) {
		return Math.sqrt(Math.pow(point1.getX() - point2.getX(), 2) + Math.pow(point1.getY() - point2.getY(), 2));
	}
	
	/**
	 * Method to create a deep clone of this solution, such that all objects are separate in memory
	 * @return The deep clone of this solution
	 */
	public Solution deepClone() {
		ArrayList<GridPoint> cloneRepresentation = new ArrayList<GridPoint>();
		
		for(GridPoint p : solutionRepresentation) {
			int[] coord = new int[2];
			coord[0] = p.getX();
			coord[1] = p.getY();
			cloneRepresentation.add(new GridPoint(coord));
		}
		return new Solution(this.host, cloneRepresentation);
	}
	
	/**
	 * Method to print out the TSP path
	 */
	public void printRoute() {
		String route = new String("[ROUTE INFO] - Cost: ");
		if(cost == 0) {
			route += " Not Yet Calculated, ";
		}
		else {
			route += ("" + this.cost + ", ");
		}
		route += ("Route Length: " + solutionLength + ", Route: ");
		route += "(" + solutionRepresentation.get(0).getX() + "," + solutionRepresentation.get(0).getY() + ")";
		for(int i = 1; i < solutionLength; i++) {
			route+= " -> (" + solutionRepresentation.get(i).getX() + "," + solutionRepresentation.get(i).getY() + ")";
		}
		System.out.println(route);
	}
	
	/**
	 * Debbugging method to print whether or not this tour contains duplicate locations, along with a string to aid debugging
	 * @param debugString the debug string to print at the end of the message
	 */
	public void containsDuplicates(String debugString) {
		for(int i = 0; i < this.solutionLength; i++) {
			if(this.getSolutionRepresentation().lastIndexOf(this.getSolutionRepresentation().get(i)) != i && this.getSolutionRepresentation().lastIndexOf(this.getSolutionRepresentation().get(i)) != -1) {
				System.out.println("[WARNING] - solution contains duplicates. DS: " + debugString);
			}
		}
	}
	
}
