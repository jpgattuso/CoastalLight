#' @title 
#' Extracts a transect from data returned by function \code{cl_GetData()}
#'
#' @description 
#' When it is operated with \code{type} "Area"
#' function \code{cl_GetData()} returns data at all available points of a geographic area; 
#' function \code{cl_Transect()} extract a transect inside this area in 2 steps;
#'      \itemize{
#'         \item rasterization of the data
#'         \item extraction of the transect
#'      }
#' output of this function has the same structure as output of function \code{cl_GetData()}
#' and may be plotted by function \code{cl_PlotData()}
#'
#' @param x : [list] a list returned by function \code{cl_GetData()}
#'            whose field \code{type} has value "Area"
#'
#' @param vertices : [list] a polygonal line that represents a transect;
#'        each element of this list is a geographic point,
#'        i.e. a vector of length 2 (longitude, latitude); 
#'
#' @param plt : [logical] make a plot of the transect on the maps of the variables
#'          (default is FALSE)
#'
#' @return : [list] a list with 5 components :
#'           \itemize{
#'              \item the same 4 components as returned by function \code{cl_GetData()} :
#'           \itemize{
#'              \item \code{type} : the type of geographical zone; its value is "Transect"
#'              \item \code{lon} : identical to component \code{lon} of x
#'              \item \code{lat} : identical to component \code{lat} of x
#'              \item \code{data} : a matrix of data along the transect with columns names
#'                                "longitude", "latitude",
#'                          and variables requested. 
#'           }
#'              \item a fifth component \code{distances} which represents the distance
#'                    traveled from the first vertex of the polygonal line
#'           }
#'
#' @examples
#' gabes <- cl_GetData(lon = c(10, 14), lat = c(32.5, 36), dir = "./CoastalLight.d")
#' vertices <- list(c(10.1,33), c(13.5,35), c(13,34))
#' #X11()
#' par(mfrow = c(3, 2))
#' tr <- cl_Transect(gabes, vertices, plt = TRUE)
#' cl_PlotData(tr)
cl_Transect <- function(x, vertices, plt = FALSE) {
	stat <- x$stat
	if(x$type != "Area") {
		cat("the type must be \"Area\"\n")
		return(invisible(NULL))
	}
	lon.in <- TRUE
	lat.in <- TRUE
	for(i in 1:length(vertices)) {
		vertex <- vertices[[i]]
		lon.in <- lon.in & vertex[1] > x$lon[1] & vertex[1] < x$lon[2]
		lat.in <- lat.in & vertex[2] > x$lat[1] & vertex[2] < x$lat[2]
	}
	if(! lon.in) {
		cat("some longitude(s) of the vertices of the polygonal outside the Area\n")
		return(invisible(NULL))
	}
	if(! lat.in) {
		cat("some latitudes(s) of the vertices of the polygonal outside the Area\n")
		return(invisible(NULL))
	}
	if(plt) {
		s <- cl_PlotData(x, vertices = vertices)
	} else {
		s <- clu_rasterize(x)
	}
	lon <- init(s, "x")
	lat <- init(s, "y")
	ns <- names(s)
	s2 <- stack(lon, lat, s)
	ns <- c("longitude", "latitude", ns)
	names(s2) <- ns
	begin <- TRUE
	for(i in 2:length(vertices)) {
		transect <- spLines(rbind(vertices[[i-1]], vertices[[i]]), crs = crs(s2))
		vls <- extract(s2, transect, along = TRUE)[[1]]
		dst <- 0.001 * pointDistance(
			SpatialPoints(vls[, c("longitude", "latitude")], proj4string = crs(s2)),
			SpatialPoints(array(vertices[[i-1]], dim = c(1, 2)), proj4string = crs(s2)))
		if(begin) {
			begin <- FALSE
			vals <- vls
			dists <- dst
		} else {
			vals <- rbind(vals, vls)
			dst <- dst + max(dists)
			dists <- append(dists, dst)
		}
	}
	return(list(type = "Transect", lon = x$lon, lat = x$lat, distances = dists, data = vals, stat = stat))
}
