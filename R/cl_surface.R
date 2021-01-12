#' @title 
#' Returns surfaces and P-function values
#'
#' @description 
#' This function is the main function of the package. It operates on the three standard regions "NonPolar", "Arctic", and "Antarctic"
#' or on a subregion (see user's guide and function cl_subregion())
#'
#' @param region : [character] choose among : "NonPolar", "Arctic" and "Antarctic"
#'                 or give the name of a subregion of one of these three regions. For example :
#'
#'                 suppose you have calculated the Pfunction of a subregion called "med" from the main region "NonPolar".
#'                 You have downloaded a file called "NonPolar.med.Pfunctions.dat" (see function cl_subregion()).
#'                 In this case pass "NonPolar.med".
#'
#' @param month : [integer] the month (-1, 0, 1-12);
#'                          if -1 gives the values for global and all months;
#'                          if 0 gives the global value;
#'                          if 1 < month < 12 gives the values for the month;
#'
#' @param E : [numeric] \eqn{mol.photons \; m^{-2} \; d^{-1}}
#'
#' @param type : [character] choose among : "Sg" "s" "P" "E" (default "s")
#'
#' @param dir : [character] only used if region is not one of the three standard regions but a subregion;
#'                          in this case, it is the path to the directory where is stored
#'                          the Pfunction file (example : "NonPolar.med.Pfunctions.dat") of this subregion
#'                          ( default = ".", that is the current directory )
#'
#' @return
#' if type == "Sg" [numeric] : an unique value (surface area of the region in \eqn{km^{2}})
#'
#' if type == "s" [numeric vector] : surface areas in \eqn{km^{2}} receiving more than E \eqn{mol.photons \; m^{-2} \; d^{-1}}
#'
#' if type == "P" [numeric array] : values of the P-function in \% for the E values
#'
#' if type == "E" [numeric vector] : discrete values of E used for tabulated P-functions
#'
#' @details
#' This function uses the Pfunction file :
#' \itemize{
#' \item included in the package if region is "NonPolar" "Arctic" or "Antarctic"
#' \item previously calculated and downloaded if region is the name of a subregion.
#' }
#'
#' @examples
#' ## surface of the "NonPolar" region
#' cl_surface("NonPolar", type = "Sg")
#' 
#' ## global P-function
#' cl_surface("NonPolar", month = 0, E = c(0.01,0.02,0.05,0.1,0.2,0.5,1), type = "P")
#' 
#' ## get E values in "Arctic" region then computes and plot global and monthly P-functions
#' region <- "Arctic"
#' E <- cl_surface(region, type = "E")
#' pc <- cl_surface(region, month = -1, E = E, type = "P")
#' matplot(E, pc, type = "l", log = "x", xlim = rev(range(E)),
#'         lty = 1:5, col = 1:6, lwd = 2, ylab = "%", main = region)
#' legend("topleft", legend = colnames(pc),
#'         lty = 1:5, col = 1:6, lwd = 2, bty = "n")
#' 
#' ## example for a subregion
#' region <- "NonPolar.Gabes"
#' ## for this example the directory where to find the P-function file
#' ##   "NonPolar.Gabes.Pfunctions.dat" is in the package itself;
#' ##   you can obtain your own P-function files and save them in
#' ##   the directory of your choice - see function cl_subregion()
#' dir <- system.file("extdata", package = "CoastalLight")
#' E <- cl_surface(region, type = "E", dir = dir)
#' pc <- cl_surface(region, month = -1, E = E, type = "P", dir = dir)
#' matplot(E, pc, type = "l", log = "x", xlim = rev(range(E)),
#'         lty = 1:5, col = 1:6, lwd = 2, ylab = "%", main = region)
#' legend("topleft", legend = colnames(pc),
#'         lty = 1:5, col = 1:6, lwd = 2, bty = "n")
cl_surface <- function(region, month = 0, E = 1.6, type = "s", dir = ".") {
	types <- c("Sg", "s", "P", "E")
	if(! type %in% types) {
		cat("choose type among :", types, "\n")
		return(invisible(NULL))
	}
	if(region == "NonPolar") x <- read.table(file = system.file("extdata", "NonPolar.Pfunctions.dat", package = "CoastalLight"), header = TRUE, as.is = TRUE)
	else if(region == "Arctic") x <- read.table(file = system.file("extdata", "Arctic.Pfunctions.dat", package = "CoastalLight"), header = TRUE, as.is = TRUE)
	else if(region == "Antarctic") x <- read.table(file = system.file("extdata", "Antarctic.Pfunctions.dat", package = "CoastalLight"), header = TRUE, as.is = TRUE)
	else {
		regionfile <- paste(dir, paste(region, "Pfunctions.dat", sep = "."), sep = "/")
		if(file.exists(regionfile)) {
			x <- read.table(file = regionfile, header = TRUE, as.is = TRUE)
		} else {
			cat("choose region among : \"NonPolar\" \"Arctic\" \"Antarctic\" \n")
			cat("  or give the name of a of a subregion (example : \"NonPolar.med\")\n")
			return(invisible(NULL))
		}
	}

	if(type == "E") return(x$E)

	rE <- range(x$E)
	if(!all(E >= rE[1] & E <= rE[2])) {
		cat("E values between", rE[1], "and", rE[2], "\n")
		return(invisible(NULL))
	}
	ns <- names(x)
	nspc <- ns[grep("^pcent", ns)]
	nspcm <- nspc[! nspc %in% "pcent"]
	nspcm <- sub("pcent", "", nspcm)
	if(length(month) > 1) {
		cat("please only  one month : -1, 0 or among", as.integer(nspcm), "\n")
		return(invisible(NULL))
	}
	if(! month %in% c(-1, 0, as.integer(nspcm))) {
		cat("bad month : -1, 0 or among", as.integer(nspcm), "\n")
		return(invisible(NULL))
	}


	if(type == "Sg") {
		Sgeo <- x$Sgeo[1]
		return(Sgeo)
	}


	if(type == "P" | type == "s") {
		if(month == -1) kol <- nspc
		if(month == 0) kol <- "pcent"
		if(month > 0 ) kol <- paste("pcent", formatC(month, format = "d", width = 2, flag = "0"), sep = "")
		f <- function(dum) {approx(x$E, dum, E)$y}
		if(length(E) > 1) pc <- apply(x[kol], 2, f)
		if(length(E) == 1) pc <- array(apply(x[kol], 2, f), dim = c(1, length(kol)))
		rownames(pc) <- E
		colnames(pc) <- kol
		pcmonths <- sub("pcent", "", colnames(pc))
		i.global <- pcmonths == ""
		pcmonths[! i.global] <- month.name[as.numeric(pcmonths[! i.global])]
		pcmonths[i.global] <- "global"
		if(type == "P") {
			colnames(pc) <- pcmonths
			return(pc)
		}
		if(type == "s") {
			Sgeo <- x$Sgeo[1]
			surfs <- pc * Sgeo / 100
			colnames(surfs) <- pcmonths
			return(surfs)
		}
	}
}
