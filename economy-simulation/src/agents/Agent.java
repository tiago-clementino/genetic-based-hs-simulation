package agents;

import java.util.Random;

import repast.simphony.space.SpatialMath;
import repast.simphony.space.continuous.ContinuousSpace;
import repast.simphony.space.continuous.NdPoint;
import repast.simphony.space.grid.Grid;
import repast.simphony.space.grid.GridPoint;

/**
 * This class encapsulates all of the actions in an Agent that moves in the coordinate system, and interacts with other agents there
 * @author Felix
 *
 */
public class Agent {

	/**
	 * Function that handles instantly moving this agent to a coordinate 
	 * @param space The ContinuousSpace projection
	 * @param grid The Grid projection
	 * @param destination A GridPoint object storing the destination coordinates
	 */
	public void moveToLocation(ContinuousSpace<Object> space, Grid<Object> grid, GridPoint destination) {
		if(!destination.equals(grid.getLocation(this))) {
			double[] dst = new double[2];
			
			dst[0] = destination.getX();
			dst[1] = destination.getY();
			
			space.moveTo(this, dst);
			grid.moveTo(this, (int)destination.getX(), (int)destination.getY());
		}
	}
	
	/**
	 * Function to move this agent in a random direction by a certain distance
	 * @param space The ContinuousSpace projection
	 * @param grid The Grid projection
	 * @param r java.util.Random object used to generate the random direction angle
	 * @param distance The distance to move the object by in the random direction
	 */
	public void moveRandomly(ContinuousSpace<Object> space, Grid<Object> grid, Random r, int distance) {
		double angle = r.nextDouble() * 2 * Math.PI;
		
		space.moveByVector(this, distance, angle, 0);
		grid.moveTo(this, (int)space.getLocation(this).getX(), (int)space.getLocation(this).getY());
	}
	
	/**
	 * Function to move this agent in the direction towards a destination, but a certain distance
	 * @param space The ContinuousSpace projection
	 * @param grid The Grid projection
	 * @param destination A GridPoint object storing the destination coordinates
	 * @param distance The distance to move the object by in the random direction
	 */
	public void moveByDistance(ContinuousSpace<Object> space, Grid<Object> grid, GridPoint destination, int distance) {
		if(!destination.equals(grid.getLocation(this))) {
			NdPoint currentPoint = space.getLocation(this);
			NdPoint destinationPoint = new NdPoint(destination.getX(), destination.getY());
			double angle = SpatialMath.calcAngleFor2DMovement(space, currentPoint, destinationPoint);
			
			space.moveByVector(this, 1, angle, 0);
			currentPoint = space.getLocation(this);
			grid.moveTo(this, (int)space.getLocation(this).getX(), (int)space.getLocation(this).getY());
			
		}
	}
	
}
