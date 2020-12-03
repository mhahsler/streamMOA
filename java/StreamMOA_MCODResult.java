public class StreamMOA_MCODResult {
    private int id=0;
    private boolean outlier=false;
    private String outlier_id=null;

    public void setId(int id) {
      this.id=id;
    }
    public int getId() {
      return this.id;
    }

    public void setOutlier(boolean outlier) {
      this.outlier=outlier;
    }
    public boolean isOutlier() {
      return this.outlier;
    }

    public void setOutlierId(String id) {
      this.outlier_id=id;
    }
    public String getOutlierId() {
      return this.outlier_id;
    }
}
