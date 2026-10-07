# Add your package's bundled jar files to the active JVM class path
# Assuming your .jar files reside in `inst/java/` in your source code
jar_path <- system.file("java", package = "streamMOA")

if (jar_path != "") {
  # Dynamically discover all .jar files in the package's java folder
  jars <- list.files(jar_path, pattern = "\\.jar$", full.names = TRUE)

  if (length(jars) > 0) {
    rJava::.jaddClassPath(jars)
  }
}
