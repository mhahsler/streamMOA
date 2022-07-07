## Java

.jinit()
jv <-
  .jcall("java/lang/System",
    "S",
    "getProperty",
    "java.runtime.version")
if (substr(jv, 1L, 2L) == "1.") {
  jvn <-
    as.numeric(paste0(strsplit(jv, "[.]")[[1L]][1:2], collapse = "."))
  if (jvn < 1.8)
    stop("Java >= 8 is needed for this package but not available")
}

.onLoad <- function(libname, pkgname) {
  # use -Xrs so sun java does not kill R on CTRL-C
  .jpackage(
    pkgname,
    lib.loc = libname,
    parameters = "-Xrs",
    own.loader = TRUE
  )
}
