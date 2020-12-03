import java.util.ArrayList;
import java.util.List;
import java.util.LinkedHashMap;

import com.yahoo.labs.samoa.instances.Attribute;
import com.yahoo.labs.samoa.instances.DenseInstance;
import com.yahoo.labs.samoa.instances.Instances;
import com.yahoo.labs.samoa.instances.Instance;

import moa.cluster.Clustering;
import moa.cluster.SphereCluster;
import moa.clusterers.outliers.MCOD.*;
import moa.clusterers.outliers.MCOD.ISBIndex.ISBNode;
import moa.clusterers.outliers.MCOD.ISBIndex.ISBNode.NodeType;
import moa.clusterers.outliers.MCOD.ISBIndex.ISBSearchResult;

/*
 * MCOD is obviously not correctly implemented in the MOA package.
 * For starter, it does not implement retrieving micro-clusters in the interface.
 * In the getInstanceValues method, it is expected that the last dimension is the instance class, which we do not provide,
 * therefore we override the original method here.
 * We provide fixes here... if MCOD gets corrected some day, this can be removed.
 *
 * MCOD must be a single pass clusterer, as it decides for each instance whether it is an inlier or an outlier.
 * Classical apriori update and then assignment does not work here...
 *
 * To everyone writing clustering algorithms in Java: PLEASE don't use default visibility for properties and methods
 */
public class StreamMOA_MCOD extends MCOD {
	private static final long serialVersionUID = 1L;
  private LinkedHashMap<Long,Integer> mapper_id=new LinkedHashMap<Long,Integer>();
  private LinkedHashMap<Integer,MicroCluster> mapper_mc=new LinkedHashMap<Integer,MicroCluster>();
  private int sequence=1;

	@Override
  public void Init() {
    super.Init();
    bShowProgress=false;
  }

	private int totalNodes() {
		int no=this.getOutliersResult().size();
		for(MicroCluster mc:setMC) no+=mc.nodes.size();
		return no;
	}

	@Override
	public boolean implementsMicroClusterer() {
		return true;
	}

	@Override
	public Clustering getMicroClusteringResult() {
		Clustering microClusters=new Clustering();
		int no=totalNodes(),seq=1;
		for(MicroCluster mc:mapper_mc.values()) {
			double[] center = new double[mc.mcc.inst.numValues()];
			for (int i = 0; i < mc.mcc.inst.numValues(); i++) {
				center[i] = mc.mcc.inst.value(i);
			}
			SphereCluster c = new SphereCluster(center, this.m_radius, (double) mc.nodes.size() / (double) no);
			c.setId((double)seq++);
			microClusters.add(c);
		}
		return microClusters;
	}

	public Clustering getOutlierClusteringResult() {
		Clustering outliers=new Clustering();
		int no=totalNodes();
		for(Outlier o:getOutliersResult()) {
			double[] center = new double[o.inst.numValues()];
			for (int i = 0; i < o.inst.numValues(); i++) {
				center[i] = o.inst.value(i);
			}
			SphereCluster c = new SphereCluster(center, this.m_radius, 1.0D / (double) no);
			c.setId((double) o.id);
			outliers.add(c);
		}
		return outliers;
	}

	@Override
	public Clustering getClusteringResult() {
	  return null;
	}

	@Override
  public double[] getInstanceValues(Instance inst) {
      int length = inst.numValues();
      double[] values = new double[length];
      for (int i = 0; i < length; i++) {
          values[i] = inst.value(i);
      }
      return values;
  }

  private Integer remap(MicroCluster mc) {
    if(mapper_id.containsKey(mc.mcc.id)) return mapper_id.get(mc.mcc.id);
    Integer v=Integer.valueOf(sequence++);
    mapper_id.put(mc.mcc.id,v);
    mapper_mc.put(v,mc);
    return v;
  }

	public List<StreamMOA_MCODResult> sm_update(double[][] points) {
		DenseInstance p = null;
		ArrayList<Attribute> attributes = new ArrayList<Attribute>(points[0].length);
		for (int j = 0; j < points[0].length; j++) {
			attributes.add(new Attribute("Dim " + j));
		}
		Instances dataset = new Instances(null,	attributes, 0);
		List<StreamMOA_MCODResult> results = new ArrayList<StreamMOA_MCODResult>();
		for(int i=0; i<points.length; i++) {
			p = new DenseInstance(1.0, points[i]);
			p.setDataset(dataset);
			ProcessNewStreamObj(p);
			ISBNode lastNode=windowNodes.lastElement();
		  StreamMOA_MCODResult r=new StreamMOA_MCODResult();
      if(lastNode.nodeType==NodeType.OUTLIER) { // the main predicted assignment array is reserved for micro-clusters only
        r.setOutlier(true);
        r.setOutlierId(String.valueOf(lastNode.id)); // we mark that this point was an outlier and its clusterer id
      }
      else if(lastNode.nodeType==NodeType.INLIER_MC) {
        Integer mapped_id=remap(lastNode.mc);
        if(mapped_id!=null) r.setId(mapped_id.intValue());
      } else { // we choose the nearest one to be the classified one
        double dist=-1;
        MicroCluster mcres=null;
        for(MicroCluster mc:lastNode.Rmc) {
        	double xd=GetEuclideanDist(lastNode, mc.mcc);
        	if(dist==-1 || xd<dist) mcres=mc;
        }
        if(mcres!=null) {
          Integer mapped_id=remap(mcres);
          if(mapped_id!=null) r.setId(mapped_id.intValue());
        }
      }
      results.add(r);
		}
		return results;
	}

  // we need to repeat since someone used default visibilty in the original method
	private double GetEuclideanDist(ISBNode n1, ISBNode n2)
  {
    double diff;
    double sum = 0;
    int d = n1.obj.dimensions();
    for (int i = 0; i < d; i++) {
      diff = n1.obj.get(i) - n2.obj.get(i);
      sum += Math.pow(diff, 2);
    }
    return Math.sqrt(sum);
  }

  public boolean recheckOutlier(String ocid) {
    Long outlier_id=Long.parseLong(ocid);
    if(outlier_id<GetWindowStart()) return true;
		for(Outlier o:getOutliersResult()) {
		  if(o.id==outlier_id) return true;
		}
		return false;
  }

  private Long GetWindowEnd() {
    return objId - 1;
  }

  private Long GetWindowStart() {
    Long x = GetWindowEnd() - m_WindowSize + 1;
    if (x < FIRST_OBJ_ID)
      x = FIRST_OBJ_ID;
    return x;
  }
}
