## Java

.jinit()
jv <- .jcall("java/lang/System", "S", "getProperty", "java.runtime.version")
if(substr(jv, 1L, 2L) == "1.") {
  jvn <- as.numeric(paste0(strsplit(jv, "[.]")[[1L]][1:2], collapse = "."))
  if(jvn < 1.8) stop("Java >= 8 is needed for this package but not available")
}

.onLoad <- function(libname, pkgname) {
    options(java.parameters="-Xrs")  ### so sun java does not kill R on CTRL-C
    .jpackage(pkgname, lib.loc = libname)


    ## Register implementations
    DSC_registry$set_entry(name = "DSC_DenStream",
      DSC_Micro = TRUE, DSC_Macro = FALSE,
      description = "DenStream - Density-Based Clustering over an Evolving Data Stream with Noise. (MOA implementation)")

    DSC_registry$set_entry(name = "DSC_BICO_MOA",
      DSC_Micro = TRUE, DSC_Macro = FALSE,
      description = "BICO - Fast computation of k-means coresets (MOA implementation)")

    DSC_registry$set_entry(name = "DSC_CluStream",
      DSC_Micro = TRUE, DSC_Macro = FALSE,
      description = "CluStream - Micro-cluster-based algorithm with pyramidal Time frame (MOA implementation)")

    DSC_registry$set_entry(name = "DSC_ClusTree",
      DSC_Micro = TRUE, DSC_Macro = FALSE,
      description = "ClusTree - A compact and self-adaptive index structure for maintaining stream summaries. (MOA implementation)")

    DSC_registry$set_entry(name = "DSC_Dstream",
      DSC_Micro = TRUE, DSC_Macro = FALSE,
      description = "Dstream - Density-based clustering for real-time stream data (MOA implementation)")

    DSC_registry$set_entry(name = "DSC_MCOD",
      DSC_Micro = TRUE, DSC_Macro = FALSE,
      description = "MCOD - Micro-cluster Continuous Outlier Detector (MOA implementation)")

    DSC_registry$set_entry(name = "DSC_StreamKM",
      DSC_Micro = TRUE, DSC_Macro = FALSE,
      description = "streamKM++ - a tree-based sampling strategy to obtain coresets (MOA implementation)")

}
