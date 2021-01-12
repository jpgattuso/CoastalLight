clu_rasterize <- function(x) {
	lon <- x$lon
	lat <- x$lat
	what <- colnames(x$data)
	what <- what[! what %in% c("longitude", "latitude")]
	r <- raster(xmn = min(lon), xmx = max(lon), ymn = min(lat), ymx = max(lat), resolution = 1./240.)
	begin <- TRUE
	for(w in what) {
		cat("Rasterize", w, "\n")
		y <- rasterize(x$data[, c("longitude", "latitude")], r, x$data[, w], fun = mean, na.rm = TRUE)
		if(begin) {
			begin <- FALSE
			s <- y
		} else {
			s <- stack(s, y)
		}
	}
	names(s) <- what
	return(s)
}
