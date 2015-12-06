import java.util.ArrayList;

import moa.cluster.Cluster;
import moa.cluster.Clustering;
import moa.clusterers.AbstractClusterer;
import moa.clusterers.macro.dbscan.DBScan;
import moa.core.Measurement;
import moa.options.FloatOption;
import moa.options.IntOption;
import weka.core.DenseInstance;
import weka.core.Instance;

class StreamMOA {
  public static void update(AbstractClusterer clusterer, double[][] points) {
    DenseInstance p = null;

    for(int i=1; i<points.length; i++) {
        p = new DenseInstance(1.0, points[i]);
        clusterer.trainOnInstanceImpl(p);
    }
  }
}