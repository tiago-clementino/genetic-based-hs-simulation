package agents;

import repast.simphony.space.continuous.ContinuousSpace;
import repast.simphony.space.continuous.NdPoint;
import repast.simphony.space.grid.Grid;
import repast.simphony.util.ContextUtils;

/**
 * Class encapsulating hs to be cleaned by a Collector
 * @author Felix
 *
 */
public class HS {

	private ContinuousSpace<Object> space;
	private Grid<Object> grid;
	
	/**
	 * Constructor for this object
	 * @param space The ContinuousSpace projection
	 * @param grid The Grid projection
	 */
	public HS(ContinuousSpace<Object> space, Grid<Object> grid) {
		this.space = space;
		this.grid = grid;
	}
	
	/**
	 * Method to remove this HS, effectively collecting or cleaning it up
	 */
	public void collect() {
		ContextUtils.getContext(this).remove(this);
	}
	
	/**
	 * Method to return the NdPoint location of this hs object in the ContinuousSpace projection
	 * @return The NdPoint location of this object
	 */
	public NdPoint getLocation() {
		return space.getLocation(this);
	}
	

	@Override 
	/**
	 * Override string to get the location of this rubbiish object in the ContinuousSpace projection
	 * @return The string location of this hs object
	 */
	public String toString() {
		return ("(" + getLocation().getX() + ", " + getLocation().getY() + ")");
	}
}