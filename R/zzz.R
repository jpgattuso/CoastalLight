.onAttach <- function(lib, pkg) {
	packageStartupMessage("\n### To have some information about this package, at prompt type :")
	packageStartupMessage("### help(CoastalLight)")
	packageStartupMessage("### help(package = CoastalLight)")
}
.onLoad <- function(lib, pkg) {
	utils::globalVariables(c("envir.geo", "envir.opt00", "envir.opt01", "envir.opt02", "envir.opt03", "envir.opt04", "envir.opt05", "envir.opt06", "envir.opt07", "envir.opt08", "envir.opt09", "envir.opt10", "envir.opt11", "envir.opt12"))
	envir.geo <- NULL
	envir.opt00 <- NULL
	envir.opt01 <- NULL
	envir.opt02 <- NULL
	envir.opt03 <- NULL
	envir.opt04 <- NULL
	envir.opt05 <- NULL
	envir.opt06 <- NULL
	envir.opt07 <- NULL
	envir.opt08 <- NULL
	envir.opt09 <- NULL
	envir.opt10 <- NULL
	envir.opt11 <- NULL
	envir.opt12 <- NULL
}
