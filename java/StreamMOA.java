import java.util.ArrayList;

import com.yahoo.labs.samoa.instances.Attribute;
import com.yahoo.labs.samoa.instances.DenseInstance;
import com.yahoo.labs.samoa.instances.Instances;

import moa.clusterers.AbstractClusterer;

// for debugging
//import java.io.*;

class StreamMOA {
    public static void update(AbstractClusterer clusterer, double[][] points) {
	DenseInstance p = null;
	// DenStream requires an Instance Header to be set
	// InstancesHeader dataset = new InstancesHeader());
	// DStream even requires the numeric class headers to be set

	//System.err.println("Dim: " + points.length + " x " + points[0].length);

	ArrayList<Attribute> attributes = new ArrayList<Attribute>(points[0].length);
	for (int j = 0; j < points[0].length; j++) {
	    attributes.add(new Attribute("Dim " + j));
	}

	Instances dataset = new Instances(null,	attributes, 0);

	for(int i = 0; i < points.length; i++) {
	    //System.err.println("Updating with point " + i);
	    //System.err.println("length: " + points[i].length);
	    //System.err.println("Point: " + points[i][0] + "," + points[i][1]);
	    p = new DenseInstance(1.0, points[i]);
	    p.setDataset(dataset);
	    //System.err.println("beep");
	    clusterer.trainOnInstanceImpl(p);
	    //System.err.println("beep");
	}
    }
}
