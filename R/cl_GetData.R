#' @title 
#' Get Data
#'
#' @description 
#' This function gets (geographic and optical) data for a geographic zone. Such a zone may be a
#' single geographic point, a longitudinal transect, a latitudinal transect, or an area. 
#' The type of geographic zone depends the arguments \code{lon} and \code{lat}
#' passed to the function (see details)
#'
#' @param lon : [numeric] longitude, vector of length 1 or 2,
#'              (interval [-180; 180], and see details)
#'     ; unit : \eqn{decimal \, degree}
#'
#' @param lat : [numeric] latitude, vector of length 1 or 2,
#'              (interval [-90; 90], and see details)
#'     ; unit : \eqn{decimal \, degree}
#'
#' @param what : [character] a vector of the variables to extract among
#'               "depth", "area", "par", "kdpar" and "parbottom". 
#'               Several variables can be listed. Default is all of them.
#'               Note that longitude and latitude are automatically added to the variables.
#'               Units : 
#'     depth (\eqn{m}),
#'     area (\eqn{km^{2}}),
#'     par, parbottom (\eqn{mol.photons \; m^{-2} \; d^{-1}}),
#'     kdpar (\eqn{m^{-1}}).
#'
#' @param dirdata : [character] The directory where the data files
#'                  (previouly downloaded with the function \code{cl_DownloadData()})
#'                  are stored. (default = "./CoastalLight.d");
#'
#' @param month : [integer] : the decimal value of the month ([0-12] of interest. 0 indicate
#'                the climatology over 21 years (1998-2018), 1 is January, ...
#'
#' @param stat : [character] the statistical value you want for variables "par", "kdpar" and "parbottom";
#'               choose one among "mean" (mean value),
#'               "min" (minimum), "max" (maximum), "sd" (standard deviation)
#'                 (default = "mean")
#' N.B. : "min", "max", "sd", are available only at URL \preformatted{
#'         http://obs-vlfr.fr/Pfunction/
#'                       } 
#'
#' @return : [list] a list with 5 components :
#'           \itemize{
#'              \item \code{type} : the type of geographical zone :
#'                           "Point", "LonTransect", "LatTransect" or "Area"
#'              \item \code{lon} : the lon argument as passed to the function
#'              \item \code{lat} : the lat argument as passed to the function
#'              \item \code{data} : a matrix of data with columns names "longitude", "latitude",
#'                          and variables requested
#'              \item \code{stat} : see argument \code{stat} ("mean", "min", "max", or "sd")
#'           }
#'
#' @details 
#' There are 4 options for parameters \code{lon} and \code{lat}:
#'   \itemize{
#'     \item \code{lon} is of length 1, \code{lat} is of length 1 : the type is "Point"
#'     \item \code{lon} is of length 2, \code{lat} is of length 1 : the type is "LonTransect"
#'     \item \code{lon} is of length 1, \code{lat} is of length 2 : the type is "LatTransect"
#'     \item \code{lon} is of length 2, \code{lat} is of length 2 : the type is "Area"
#'   }
#' To get data along an ordinary transect (i.e. a polygonal line, given by its vertices)
#' you have, in a first time, to get data in an "Area" containing this polygonal line,
#' and, in a second time, to use function \code{cl_Transect()} in order to extract data along the transect
#' (see example of this function).
#'
#' @examples
#'
#' ## All examples assume that data have been downloaded in directory "CoastalLight.d"
#' ## with function cl_DownloadData()
#'
#' ## Area
#' gabes <- cl_GetData(lon = c(10, 14), lat = c(32.5, 36), dir = "./CoastalLight.d")
#' par(mfrow = c(3,2))
#' cl_PlotData(gabes)
#'
#' ## a longitudinal transect in January
#' long.transect <- cl_GetData(lon = c(10, 14), lat = c(34), dir = "./CoastalLight.d", month = 1)
#' par(mfrow = c(1,1))
#' cl_PlotData(long.transect)
#'
#' ## a latitudinal transect in August
#' lat.transect <- cl_GetData(lon = c(12), lat = c(32.5, 36), dir = "./CoastalLight.d", month = 8)
#' par(mfrow = c(1,1))
#' cl_PlotData(lat.transect)
cl_GetData <- function(lon, lat, what = c("depth", "area", "par", "kdpar", "parbottom"), dirdata = "CoastalLight.d", month = 0, stat = "mean") {
	what.all <- c("depth", "area", "par", "kdpar", "parbottom")
	what.geo <- c("longitude", "latitude", "depth", "area")
	whatll <- c("longitude", "latitude", what)
	retNA <- array(NA, dim = c(1, length(whatll)), dimnames = list("0", whatll))
	if(! all(what %in% what.all)) {
		cat("!!! you ask quantities :", what, "\n")
		cat("!!! *** BUT *** allowed quantities are :", what.all, "\n")
		return(list(type = NA, lon = lon, lat = lat, data = retNA))
	}
	reso <- 1. / 240.
	types <- c("CaseArea", "CaseLonTransect", "CaseLatTransect", "CasePoint")
	CaseArea <- TRUE
	CaseLonTransect <- FALSE
	CaseLatTransect <- FALSE
	CasePoint <- FALSE
	if(length(lon) == 2) {
		lonmin <- lon[1]
		lonmax <- lon[2]
		if(is.na(lonmin) | is.na(lonmax)) {
			cat("longitude not available", "\n")
			return(list(type = NA, lon = lon, lat = lat, data = retNA))
		}
		if((lonmax - lonmin) <= reso) {
			cat("!!! lon[2] - lon[1] must be greater than", reso, "\n")
			return(list(type = NA, lon = lon, lat = lat, data = retNA))
		}
	} else if(length(lon) == 1) {
		if(is.na(lon)) {
			cat("longitude not available", "\n")
			return(list(type = NA, lon = lon, lat = lat, data = retNA))
		}
		lonmin <- lon - reso / 2 - 1e-3 * reso
		lonmax <- lon + reso / 2 + 1e-3 * reso
		CaseArea <- FALSE
		CaseLatTransect <- TRUE
	} else {
		cat("!!! length of argument lon must be one or two\n")
		return(list(type = NA, lon = lon, lat = lat, data = retNA))
	}
	if(length(lat) == 2) {
		latmin <- lat[1]
		latmax <- lat[2]
		if(is.na(latmin) | is.na(latmax)) {
			cat("latitude not available", "\n")
			return(list(type = NA, lon = lon, lat = lat, data = retNA))
		}
		if((latmax - latmin) <= reso) {
			cat("!!! lat[2] - lat[1] must be greater than", reso, "\n")
			return(list(type = NA, lon = lon, lat = lat, data = retNA))
		}
	} else if(length(lat) == 1) {
		if(is.na(lat)) {
			cat("latitude not available", "\n")
			return(list(type = NA, lon = lon, lat = lat, data = retNA))
		}
		latmin <- lat - reso / 2 - 1e-3 * reso
		latmax <- lat + reso / 2 + 1e-3 * reso
		CaseArea <- FALSE
		CaseLonTransect <- TRUE
	} else {
		cat("!!! length of argument lat must be one or two\n")
		return(list(type = NA, lon = lon, lat = lat, data = retNA))
	}
	if(CaseLatTransect & CaseLonTransect)  {
		CaseLatTransect <- FALSE
		CaseLonTransect <- FALSE
		CasePoint <- TRUE
	}
	type <- sub("Case", "", types[c(CaseArea, CaseLonTransect, CaseLatTransect, CasePoint)])
	cat("Data type :", type, "\n")
	i <- clu_PixelNumbers(lonmin, lonmax, latmin, latmax, dirdata, type)
	if(is.null(i)) {
#		cat("!!! Your zone : long.", lon, "   /   ", "lat.", lat, "is outside Coastal Zone [0; 200m]\n")
		cat("!!! Your zone : long.", lon, "   /   ", "lat.", lat, "is outside Coastal Zone\n")
		return(list(type = type, lon = lon, lat = lat, data = retNA))
	} else {
		whatll <- c("longitude", "latitude", what)
		ret <- array(NA, dim = c(length(i), length(whatll)), dimnames = list(i, whatll))
		ws <- whatll[whatll %in% what.geo]
		for(w in ws) {
			nc.geo <- get("nc.geo", envir = envir.geo, inherits = FALSE)
			if(! exists(w, envir = envir.geo, inherits = FALSE)) {
				cat("reading", w, "\n")
				dum <- ncvar_get(nc.geo,w)
				assign(w, dum, envir = envir.geo)
			} else {
				dum <- get(w, envir = envir.geo, inherits = FALSE)
			}
			ret[, w] <- dum[i]
		}
		ws <- whatll[! whatll %in% what.geo]
		IsOpenNCfile <- FALSE
		for(w in ws) {
			cmonth <- formatC(month, width = 2, flag = "0")
			envir.month <- paste("envir.opt", cmonth, sep = "")
			if(exists(envir.month)) {
				if(! exists("stat", envir = get(envir.month, envir = .GlobalEnv), inherits = FALSE)) {
					rm(list = ls(get(envir.month, envir = .GlobalEnv)),
						envir = get(envir.month, envir = .GlobalEnv), inherits = FALSE)
				} else {
					stat.actual <- get("stat", envir = get(envir.month, envir = .GlobalEnv))
					if(stat.actual != stat) {
						rm(list = ls(get(envir.month, envir = .GlobalEnv)),
							envir = get(envir.month, envir = .GlobalEnv), inherits = FALSE)
					}
				}
				assign("stat", stat, envir = get(envir.month, envir = .GlobalEnv))
			} else {
				assign(envir.month, new.env(), envir = .GlobalEnv)
				assign("stat", stat, envir = get(envir.month, envir = .GlobalEnv))
			}
			if(! exists(w, envir = get(envir.month, envir = .GlobalEnv), inherits = FALSE)) {
				if(! IsOpenNCfile) {
					if(stat == "mean") {
						f <- paste(dirdata, paste("CoastalLight_", cmonth, ".nc", sep = ""), sep = "/")
					} else {
						f <- paste(dirdata, paste("CoastalLight_", stat, "_", cmonth, ".nc", sep = ""), sep = "/")
					}
					if(! file.exists(f)) {
						cat("!!! no such file :", f, "\n")
						cat("!!! You have to *** download *** it with function cl_DownloadData()\n")
						return(list(type = type, lon = lon, lat = lat, data = retNA))
					}
					cat("opening file", f, "\n")
					nc.opt <- nc_open(f)
					IsOpenNCfile <- TRUE
				}
				cat("reading", w, "\n")
				dum <- ncvar_get(nc.opt,w)
				assign(w, dum, envir = get(envir.month, envir = .GlobalEnv))
			} else {
				dum <- get(w, envir = get(envir.month, envir = .GlobalEnv))
			}
			ret[, w] <- dum[i]
		}
		if(IsOpenNCfile) nc_close(nc.opt)
#		if(DataAsRaster & CaseArea) {
#			r <- raster(xmn = lonmin, xmx = lonmax, ymn = latmin, ymx = latmax, resolution = 1./240.)
#			begin <- TRUE
#			for(w in what) {
#				cat("Rasterize", w, "\n")
#				y <- rasterize(ret[, c("longitude", "latitude")], r, ret[, w], fun = mean, na.rm = TRUE)
#				if(begin) {
#					begin <- FALSE
#					s <- y
#				} else {
#					s <- stack(s, y)
#				}
#			}
#			names(s) <- what
#		} else {
#			s <- NULL
#		}
		return(list(type = type, lon = lon, lat = lat, data = ret, stat = stat))
	}
}
