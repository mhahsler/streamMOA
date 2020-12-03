import java.util.ArrayList;

import com.yahoo.labs.samoa.instances.Attribute;
import com.yahoo.labs.samoa.instances.DenseInstance;
import com.yahoo.labs.samoa.instances.Instances;

import moa.clusterers.AbstractClusterer;

class StreamMOA {
	public static void update(AbstractClusterer clusterer, double[][] points) {
		DenseInstance p = null;
		// DenStream requires an Instance Header to be set
		// InstancesHeader dataset = new InstancesHeader());
		// DStream even requires the numeric class headers to be set
		ArrayList<Attribute> attributes = new ArrayList<Attribute>(points[0].length);
		for (int j = 0; j < points[0].length; j++) {
			attributes.add(new Attribute("Dim " + j));
		}
		Instances dataset = new Instances(null,	attributes, 0);

		for(int i=1; i<points.length; i++) {
			p = new DenseInstance(1.0, points[i]);
			p.setDataset(dataset);
			clusterer.trainOnInstanceImpl(p);
		}
	}
}
