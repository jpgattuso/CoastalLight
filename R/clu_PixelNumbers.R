#!##' @title 
#!##' Retrieve the numbers of the pixels, given a geographic window
#!##'
#!##' @description 
#!##' This function looks in file "CoastalLight_geo.nc" and retrieve pixel numbers given longitudes and latitudes.
#!##'
#!##' @param lonmin : [numeric] Min. longitude of the window (decimal degree, [-180; 180])
#!##'
#!##' @param lonmax : [numeric] Max. longitude of the window (decimal degree, [-180; 180])
#!##'
#!##' @param latmin : [numeric] Min. latitude of the window (decimal degree, [-90; 90])
#!##'
#!##' @param latmax : [numeric] Max. latitude of the window (decimal degree, [-90; 90])
#!##'
#!##' @param dirdata : [character] The directory where the files are stored
#!##'
#!##' @param type : [character] The type of data extraction : "Area", "LonTransect", "LatTransect", or "Point"
#!##'
#!##' @return : [integer] a vector of numbers
#!##'
#!##' @details 
#!##' Not called by the user.
clu_PixelNumbers <- function(lonmin, lonmax, latmin, latmax, dirdata, type) {
	reso <- 1./240.
	newdatadir <- FALSE
	if(exists("envir.geo")) {
		dirdata.actual <- get("dirdata", envir = get("envir.geo", envir = .GlobalEnv))
		if(! dirdata.actual == dirdata) {
			rm(list = ls(get("envir.geo", envir = .GlobalEnv)),
				envir = get("envir.geo", envir = .GlobalEnv), inherits = FALSE)
			assign("dirdata", dirdata, envir = get("envir.geo", envir = .GlobalEnv))
			newdatadir <- TRUE
			cat("changing data directory from :", dirdata.actual, "to :", dirdata, sep = "\n")
		} else {
			longitude <- get("longitude", envir = envir.geo, inherits = FALSE)
			latitude <- get("latitude", envir = envir.geo, inherits = FALSE)
		}
	}
	if(! exists("envir.geo") || newdatadir) {
		f <- paste(dirdata, "CoastalLight_geo.nc", sep = "/")
		if(! file.exists(f)) {
			cat("!!! no such file :", f, "\n")
			cat("!!! You have to *** download *** it with function cl_DownloadData()\n")
			stop("... try again ...", call. = FALSE)
		}
		cat("opening file", f, "\n")
		nc.geo <- nc_open(f)
		cat("reading longitude\n")
		longitude <- ncvar_get(nc.geo,"longitude")
		cat("reading latitude\n")
		latitude <- ncvar_get(nc.geo,"latitude")
		assign("envir.geo", new.env(), envir = .GlobalEnv)
		assign("nc.geo", nc.geo, envir = envir.geo)
		assign("longitude", longitude, envir = envir.geo)
		assign("latitude", latitude, envir = envir.geo)
		assign("dirdata", dirdata, envir = envir.geo)

		envirs.opt <- ls(envir = .GlobalEnv, pattern = "envir.opt")
		for(e in envirs.opt) {
			rm(list = ls(get(e, envir = .GlobalEnv)), envir = get(e, envir = .GlobalEnv), inherits = FALSE)
		}
	}
	i <- longitude >= lonmin & longitude <= lonmax
	j <- latitude >= latmin & latitude <= latmax
	k <- which(i & j)
	if(type == "LonTransect") {
		lats <- unique(latitude[k])
		lat0 <- lats[1]
		j <- latitude >= lat0 - reso / 4 & latitude <= lat0 + reso / 4
		k <- which(i & j)
	}
	if(type == "LatTransect") {
		lons <- unique(longitude[k])
		lon0 <- lons[1]
		i <- longitude >= lon0 - reso / 4 & longitude <= lon0 + reso / 4
		k <- which(i & j)
	}
	if(type == "Point") {
		k <- k[1]
	}
	if(length(k) > 0) return(k)
	else return(NULL)
}
