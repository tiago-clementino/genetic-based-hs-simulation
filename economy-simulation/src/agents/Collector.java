package agents;

import repast.simphony.space.SpatialMath;
import repast.simphony.space.continuous.ContinuousSpace;
import repast.simphony.space.continuous.NdPoint;
import repast.simphony.space.grid.Grid;
import repast.simphony.space.grid.GridPoint;
import repast.simphony.util.ContextUtils;
import repast.simphony.util.SimUtilities;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Random;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import components.AgentState;
import components.MemeticAlgorithmState;
import components.Solution;
import heuristics.DavisHillClimbing;
import heuristics.OrderedCrossover;
import heuristics.RandomReinsertion;
import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.engine.schedule.ScheduledMethod;
import repast.simphony.parameter.Parameters;
import repast.simphony.query.space.grid.GridCell;
import repast.simphony.query.space.grid.GridCellNgh;
import repast.simphony.random.RandomHelper;

/**
 * Collector class extending the Agent object. The collector is the main acting agent in this, it maps hs, optimises its TSP route, then actions the path
 * @author Felix
 *
 */
public class Collector extends Agent {

	public ContinuousSpace<Object> space;
	public Grid<Object> grid;
	private AgentState state;
	private Random r;
	private Solution currentSolution, worstSolution;
	private MemeticAlgorithmState maState;
	private ArrayList<Solution> population;
	private ArrayList<Solution> offspring;
	private GridPoint targetLocation;
	
	private RandomReinsertion mutationHeuristic;
	private OrderedCrossover crossoverHeuristic;
	private DavisHillClimbing hillClimbingHeuristic;
	
	private double generationalGap, bestChange;
	private int speed, viewDistance, populationSize, currentCalculationIteration, maxCalculationIterations, numberOfOffspring, dos, iom, maxMapIterations, currentMapIteration, hsCollected;
	private boolean removed, removeAllHSFlag, goAgain;
	
	
	/**
	 * Constructor to instantiate the variables
	 * @param space The ContinuousSpace projection
	 * @param grid The Grid Projection
	 * @param speed The distance this agent can move by when moving in either the map or action states
	 * @param viewDistance The distance the collector can see for when mapping out hs in the Map phase
	 * @param removeAllHS A boolean flag to denote whether or not the Collector collects all hs it encounters in the Action phase, regardless of its inclusion in its optimised route
	 * @param populationSize The number of individuals in the MA population
	 * @param maxIterations The maximum number of iterations/epochs the GA will run for
	 * @param generationalGap Double variable denoting the percentage of those in the MA population that are replaced by fitter offspring
	 * @param dos The Depth of Search parameter for hill climbing
	 * @param iom The Intensity of Mutation parameter for mutation heuristics
	 * @param maxMapIterations The number of iterations in a MAP_STATE of the collector
	 */
	public Collector(ContinuousSpace<Object> space, 
			Grid<Object> grid, 
			int speed, 
			int viewDistance, 
			boolean removeAllHS, 
			int populationSize, 
			int maxIterations,
			double generationalGap,
			int dos,
			int iom,
			int maxMapIterations
			) {
		
		this.space = space;
		this.grid = grid;
		this.speed = speed;
		this.state = AgentState.MAP_STATE;
		this.viewDistance = viewDistance;
		this.maState = MemeticAlgorithmState.NOT_STARTED;
		this.currentSolution = new Solution(this);
		this.removeAllHSFlag = removeAllHS;
		this.removed = false;
		this.goAgain = false;
		this.currentCalculationIteration = 0;
		this.populationSize = populationSize;
		this.maxCalculationIterations = maxIterations;
		this.generationalGap = generationalGap;
		this.dos = dos;
		this.iom = iom;
		this.maxMapIterations = maxMapIterations;
		this.currentMapIteration = 0;
		this.hsCollected = 0;
		r = new Random();
		this.targetLocation = new GridPoint(r.nextInt(this.grid.getDimensions().getWidth()), r.nextInt(this.grid.getDimensions().getWidth()));
	}
	
	
	@ScheduledMethod(start = 1, interval = 1)
	/**
	 * A scheduled method that determining the agent actions at each tick of the simulation
	 */
	public void step() {
		switch(state) {
			case MAP_STATE:
				GridPoint pt = grid.getLocation(this);
				GridCellNgh<HS> nghCreator = new GridCellNgh<HS>(grid, pt, HS.class, viewDistance, viewDistance);
				List<GridCell<HS>> gridCells = nghCreator.getNeighborhood(true);
				SimUtilities.shuffle(gridCells, RandomHelper.getUniform());

				for(GridCell<HS> cell : gridCells) {
					if(cell.size() > 0) {
						if(!currentSolution.getSolutionRepresentation().contains(cell.getPoint())) {
							//System.out.println("Added hs at coordinates X=" + cell.getPoint().getX() + ", Y=" + cell.getPoint().getY());
							currentSolution.addPoint(cell.getPoint());
							this.goAgain = true;
						}
						else {
							//System.out.println("Not adding X=" + cell.getPoint().getX() + ", Y=" + cell.getPoint().getY());
						}
					}
				}
				
				if( Solution.calculateEuclideanDistance(pt, targetLocation) < viewDistance) {
					this.targetLocation = new GridPoint(r.nextInt(this.grid.getDimensions().getWidth()), r.nextInt(this.grid.getDimensions().getWidth()));
				}
				else {
					moveByDistance(space, grid, targetLocation, speed);
				}
				
				if(currentMapIteration >= maxMapIterations) {
					this.moveToCalculationPhase();
				}
				currentMapIteration++;
				
				break;
			
			case CALCULATION_STATE:
				
				if(maState == MemeticAlgorithmState.NOT_STARTED) {
					
				}
				else if(maState == MemeticAlgorithmState.INITIALISING) {
					startMemeticAlgorithm();
				}
				else if(maState == MemeticAlgorithmState.ONGOING) {
					if(currentCalculationIteration < maxCalculationIterations) {
						nextMemeticAlgorithmIteration();
					}
					else {
						maState = MemeticAlgorithmState.COMPLETE;
					}
					currentCalculationIteration++;
					
				}
				else if(maState == MemeticAlgorithmState.COMPLETE) {
					finishMemeticAlgorithm();
				}
				
				break;
			
			case ACTION_STATE:
				if(!currentSolution.getSolutionRepresentation().isEmpty()) {
					pt = grid.getLocation(this);
					nghCreator = new GridCellNgh<HS>(grid, pt, HS.class, 1, 1);
					gridCells = nghCreator.getNeighborhood(true);
					SimUtilities.shuffle(gridCells, RandomHelper.getUniform());
					for(GridCell<HS> cell : gridCells) {
						removed = currentSolution.removePoint(cell.getPoint());
						if(removeAllHSFlag == true || removed == true) {
							for(HS rub : cell.items()) {
								rub.collect();
								this.hsCollected++;
							}
						}
					}
					if(!removed && !currentSolution.getSolutionRepresentation().isEmpty()) {
						moveByDistance(space, grid, currentSolution.getSolutionRepresentation().get(0), speed);
					}
					removed = false;
				}
				else {
					if(goAgain == true) {
						this.state = AgentState.MAP_STATE;
						this.maState = MemeticAlgorithmState.NOT_STARTED;
						this.currentSolution = new Solution(this);
						this.removed = false;
						this.goAgain = false;
						this.currentCalculationIteration = 0;
						this.currentMapIteration = 0;
						System.out.println("[INFO] - Moving back to MAP STATE");
					}
					else {
						System.out.println("[SUCCESS] - Successfully collected all hs found (" + hsCollected +"). Switching off...");
						this.state = AgentState.DORMANT_STATE;
					}

				}				
				
				break;
			case DORMANT_STATE:
				
				break;
			default:
				break;
				
		}
	}
	
	/**
	 * Transition method to finish the Map phase of the simulation, and begin the Memetic Algorithm
	 */
	public void moveToCalculationPhase() {
		if(this.currentSolution.getSolutionRepresentation().size() < 4) {
			System.out.println("[INFO] - MAP_STATE Yielded tour of size < 4 - Skipping Memetic Algorithm");
			this.maState = MemeticAlgorithmState.COMPLETE;
			moveToActionState();
		}
		else {
				
			this.state = AgentState.CALCULATION_STATE;
			this.maState = MemeticAlgorithmState.INITIALISING;
			this.currentSolution.finaliseCollection();
			System.out.println("[INFO] - MAP_STATE Yielded tour of size >= 4 - Initiating Memetic Algorithm");
		}

	}
	
	/**
	 * Transition function to move the Collector to the action state
	 */
	public void moveToActionState() {
		this.state = AgentState.ACTION_STATE;
	}
	
	/**
	 * Method to initialise the Memetic Algorithm, instantiating a population and creating the relevant heuristics
	 */
	public void startMemeticAlgorithm() {
		System.out.println("[INFO] - Initialising Memetic Algorithm...");
		
		this.mutationHeuristic = new RandomReinsertion();
		this.crossoverHeuristic = new OrderedCrossover();
		this.hillClimbingHeuristic = new DavisHillClimbing();
		currentSolution.printRoute();
		population = new ArrayList<Solution>();
		for(int i = 0; i < populationSize; i++) {
			Solution tmp = currentSolution.deepClone();
			mutationHeuristic.applyHeuristic(tmp, dos, iom);
			population.add(tmp);
		}
		if(this.worstSolution == null || this.worstSolution.getSolutionLength() < currentSolution.getSolutionLength()) {
			this.worstSolution = currentSolution.deepClone();
		}

		this.maState = MemeticAlgorithmState.ONGOING;
		
		numberOfOffspring = (int) Math.floor(population.size() * generationalGap);
	}
	
	/**
	 * Method to process the next Memetic Algorithm iteration, involving parent selection, crossover, mutation, hill climbing and replacement
	 */
	public void nextMemeticAlgorithmIteration() {
		if(this.currentCalculationIteration % 10 == 0) {
			System.out.println("[INFO] - Epoch: " + this.currentCalculationIteration);	
		}
		offspring = new ArrayList<Solution>();
		Solution[] parents;
		for(int i = 0; i < numberOfOffspring; i++) {
			parents = rouletteWheelSelection();
			Solution candidateOffspring = crossoverHeuristic.applyHeuristic(parents[0], parents[1], dos, iom);
	
			mutationHeuristic.applyHeuristic(candidateOffspring, dos, iom);
			hillClimbingHeuristic.applyHeuristic(candidateOffspring, dos, iom);
			offspring.add(candidateOffspring);
		}
		
		for(int i = 0; i < numberOfOffspring; i++) {
			int worstIndex = 0;
			for(int j = 1; j < this.populationSize; j++) {
				if(population.get(j).getCost() > population.get(worstIndex).getCost()) {
					worstIndex = j;
				}
			}
			population.remove(worstIndex);
		}
		population.addAll(offspring);
		
	}
	
	/**
	 * Method to finish the Memetic Algorithm, and transition the Collector to the action phase
	 */
	public void finishMemeticAlgorithm() {
		Solution bestSolution = population.get(0);
		double bestFitness = population.get(0).getCost();
		for(int i = 1; i < population.size(); i++) {
			if(population.get(i).getCost() < bestFitness) {
				bestSolution = population.get(i);
				bestFitness = population.get(i).getCost();
			}
		}
		
		this.currentSolution = bestSolution;
		currentSolution.printRoute();
		System.out.println("[INFO] - Memetic Algorithm Complete!");
		this.state = AgentState.ACTION_STATE;
		
		if(this.worstSolution.getSolutionLength() == this.currentSolution.getSolutionLength()) {
			this.bestChange = worstSolution.getCost() - this.currentSolution.getCost();
		}
	}
	
	/**
	 * Method implementing the Roulette Wheel parent selection algorithm
	 * @return A solution array containing both chosen parents
	 */
	public Solution[] rouletteWheelSelection() {
		Solution[] parents = new Solution[2];
		Random r = new Random();
		double totalFitness = 0;
		double likelihood = 0;
		double spin;
		int parent1, parent2;
		parent1 = 0;
		parent2 = 0;
		for(Solution individual : population) {
			totalFitness += individual.getCost();
		}
		
		while(parent1 == 0) {
			for(int p1counter = 0; p1counter < population.size(); p1counter++) {
				spin = r.nextDouble();
				double candidateCost = population.get(p1counter).getCost();
				likelihood = (candidateCost / totalFitness);
				if(spin < likelihood) {
					parent1 = p1counter;
					parent2 = p1counter;
				}
			}
		}
	
		while(parent1 == parent2) {
			for(int p2counter = 0; p2counter < population.size(); p2counter++) {
				spin = r.nextDouble();
				double candidateCost = population.get(p2counter).getCost();
				likelihood = (candidateCost / totalFitness);
				if(spin < likelihood) {
					parent2 = p2counter;
				}
			}
		}
		parents[0] = population.get(parent1);
		parents[1] = population.get(parent2);
		return parents;
	}

	@ScheduledMethod(start = 1, interval = 1)
	/**
	 * Function to pause the simulation when all hs is collected, and return the change in solution fitness in the longest memetic algorithm
	 */
	public void finalTick() {
		Stream<HS> hsStream = ContextUtils.getContext(this).getObjectsAsStream(HS.class);
		List<HS> hsList = hsStream.collect(Collectors.toList());
		if(hsList.size() == 0) {
			
			System.out.println("[SUCCESS] - Found All HS (" + hsCollected + ") Best Change in solution fitness = " + bestChange);
			RunEnvironment.getInstance().pauseRun();
		}
	}
}
