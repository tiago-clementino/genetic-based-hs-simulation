package controller;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import agents.Collector;
import agents.HS;
import repast.simphony.context.Context;
import repast.simphony.context.space.continuous.ContinuousSpaceFactory;
import repast.simphony.context.space.continuous.ContinuousSpaceFactoryFinder;
import repast.simphony.context.space.grid.GridFactory;
import repast.simphony.context.space.grid.GridFactoryFinder;
import repast.simphony.dataLoader.ContextBuilder;
import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.engine.schedule.ISchedule;
import repast.simphony.engine.schedule.ScheduleParameters;
import repast.simphony.engine.schedule.ScheduledMethod;
import repast.simphony.parameter.Parameters;
import repast.simphony.space.continuous.ContinuousSpace;
import repast.simphony.space.continuous.NdPoint;
import repast.simphony.space.continuous.RandomCartesianAdder;
import repast.simphony.space.grid.Grid;
import repast.simphony.space.grid.GridBuilderParameters;
import repast.simphony.space.grid.SimpleGridAdder;
import repast.simphony.space.grid.WrapAroundBorders;

/**
 * Simulation building class that implements a ContextBuilder
 * @author Felix
 *
 */
public class GeneticHSCollectionBuilder implements ContextBuilder<Object> {

	ContinuousSpace<Object> space;
	Grid<Object> grid;
	Context<Object> context;
	double generationalGap;
	int dimensions, hsCount, collectorCount, collectorSpeed, finishMapTick, viewDistance, maxMemeticAlgorithmIterations, populationSize, dos, iom;
	boolean collectAllHS;
	
	@Override
	/**
	 * Method to build the simulation context
	 */
	public Context build(Context<Object> context) {
		context.setId("genetic-hs-collection");
		
		Parameters parameters = RunEnvironment.getInstance().getParameters();
		this.dimensions = parameters.getInteger("dimensions");
		this.hsCount = parameters.getInteger("hsCount");
		this.collectorCount = parameters.getInteger("collectorCount");
		this.collectorSpeed = parameters.getInteger("collectorSpeed");
		this.finishMapTick = parameters.getInteger("mapPhaseLength");
		this.viewDistance = parameters.getInteger("viewDistance");
		this.populationSize = parameters.getInteger("populationSize");
		this.maxMemeticAlgorithmIterations = parameters.getInteger("geneticAlgorithmCutOffTick");
		this.collectAllHS = parameters.getBoolean("collectAllHS");
		this.generationalGap = parameters.getDouble("generationalGap");
		this.dos = translateGAMultiplierParameter(parameters.getString("depthOfSearch"));
		this.iom = translateGAMultiplierParameter(parameters.getString("intensityOfMutation"));

		
	    //ISchedule schedule = RunEnvironment.getInstance().getCurrentSchedule();
	    //ScheduleParameters scheduleParams = ScheduleParameters.createOneTime(finishMapTick);
	    //schedule.schedule(scheduleParams, this, "triggerMapEnd", context);
		
		ContinuousSpaceFactory spaceFactory = ContinuousSpaceFactoryFinder.createContinuousSpaceFactory(null);
		space = spaceFactory.createContinuousSpace("space", context, new RandomCartesianAdder<Object>(), new repast.simphony.space.continuous.WrapAroundBorders(), dimensions, dimensions);
		
		GridFactory gridFactory = GridFactoryFinder.createGridFactory(null);
		this.grid = gridFactory.createGrid("grid", context, new GridBuilderParameters<Object>(new WrapAroundBorders(), new SimpleGridAdder<Object>(), true, dimensions, dimensions));
		
		
		for(int i = 0; i < hsCount; i++) {
			context.add(new HS(space, grid));
		}
		
		for(int i = 0; i < collectorCount; i++) {
			context.add(new Collector(space, grid, collectorSpeed, viewDistance, collectAllHS, populationSize, maxMemeticAlgorithmIterations, generationalGap, dos, iom, finishMapTick));
		}
		
		
		for(Object obj : context) {
			NdPoint pt = space.getLocation(obj);
			grid.moveTo(obj, (int)pt.getX(), (int)pt.getY());
		}
		this.context = context;
		return context;
	}
	
	/**
	 * @deprecated
	 * Method to trigger the end of the Map phase in all collectors
	 * @param context The simulation context
	 */
	public void triggerMapEnd(Context context) {
		Stream<Collector> collectorStream = context.getObjectsAsStream(Collector.class);
		List<Collector> collectorList = collectorStream.collect(Collectors.toList());
		collectorList.forEach((collector) -> collector.moveToCalculationPhase());
		
	}
	
	/**
	 * 
	 * Method to translate the Multiplier string parameters (DoS and IoM) to usable integers for the heuristics
	 * @param dosString The string parameter
	 * @return The corresponding returned integer
	 */
	public int translateGAMultiplierParameter(String dosString) {
		if(dosString == "2x") {
			return 2;
		}
		else if(dosString == "4x") {
			return 4;
		}
		else if(dosString == "8x") {
			return 8;
		}
		else if(dosString == "10x") {
			return 10;
		}
		return 1;
	}
	
}
