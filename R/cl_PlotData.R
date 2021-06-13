#' @title 
#' Plots the data returned by function \code{cl_GetData()}
#' or function \code{cl_Transect()}
#'
#' @description 
#' Functions \code{cl_GetData()} or \code{cl_Transect()} return a list; 
#' plot differ depending on the field \code{type} of this list :
#'   \describe{  
#'     \item{"Point"}{no plot}
#'     \item{"LonTransect"}{variables are plotted vs longitude}
#'     \item{"LatTransect"}{variables are plotted vs latitude}
#'     \item{"Area"}{geographic maps of the variables are produced}
#'     \item{"Transect"}{variables are plotted vs the distance traveled
#'                      from the first vertex of the polygonal line}
#'   }
#'
#' @param x : [list] a list returned by function \code{cl_GetData()}
#'
#' @param vertices : [list] a polygonal line that represents a transect;
#'        each element of this list is a geographic point,
#'        i.e. a vector of length 2 (longitude, latitude); default is NULL;
#'        this argument is only used if type is "Area" in order to add
#'        a transect to the maps.
#'        N.B. : function \code{cl_Transect()} has an optional argument (\code{plt})
#'               to plot the transect on the maps.
#'
#' @return \itemize{
#' \item a rasterstack with all variables if type is "Area"
#' \item NULL otherwise
#' }
#'
#' @examples
#' ## See function \code{cl_GetData()} examples
cl_PlotData <- function(x, vertices = NULL) {
	type <- x$type
	lon <- x$lon
	lat <- x$lat
	stat <- x$stat
	what <- colnames(x$data)
	what <- what[! what %in% c("longitude", "latitude")]
	what.all <- c("depth", "area", "par", "kdpar", "parbottom")
	if(type == "Point") {
		return(invisible(NULL))
	}
	if(type == "Area") {
		s <- clu_rasterize(x)
		if(!is.null(vertices)) transect <- spLines(t(as.data.frame.list(vertices)), crs = crs(s))
		pdepth <- NA; ddepth <- NA
		if("depth" %in% what) {
			vdepth <- values(s[["depth"]])
			pdepth <- pretty(vdepth)
			ddepth <- diff(pdepth)[1]
		}
		parea <- NA; darea <- NA
		if("area" %in% what) {
			varea <- values(s[["area"]])
			parea <- pretty(varea)
			darea <- diff(parea)[1]
		}
		ppar <- NA; dpar <- NA
		if("par" %in% what) {
			vpar <- values(s[["par"]])
			ppar <- pretty(vpar)
			dpar <- diff(ppar)[1]
		}
		minkdpar <- NA; maxkdpar <- NA
		if("kdpar" %in% what) {
			vkdpar <- values(s[["kdpar"]])
			vkdpar[vkdpar < 1.e-6] <- NA
			minkdpar <- min(vkdpar, na.rm = TRUE)
			maxkdpar <- max(vkdpar, na.rm = TRUE)
		}
		minparbottom <- NA; maxparbottom <- NA
		if("parbottom" %in% what) {
			vparbottom <- values(s[["parbottom"]])
			minparbottom <- 0.001
			maxparbottom <- max(vparbottom, na.rm = TRUE)
		}
		log.all <- c("", "", "", "y", "y"); names(log.all) <- what.all
#		min.all <- c(max(pdepth[1], -200), parea[1],             ppar[1],            minkdpar,  minparbottom); names(min.all) <- what.all
		min.all <- c(pdepth[1], parea[1],             ppar[1],            minkdpar,  minparbottom); names(min.all) <- what.all
#		max.all <- c(0,                    parea[length(parea)], ppar[length(ppar)], maxkdpar,  maxparbottom); names(max.all) <- what.all
		max.all <- c(pdepth[length(pdepth)], parea[length(parea)], ppar[length(ppar)], maxkdpar,  maxparbottom); names(max.all) <- what.all
		by.all <- c(ddepth, darea, dpar, NA, NA); names(by.all) <- what.all
		ticks.all <- list(NA, NA, NA, c(0.02, 0.03, 0.05, 0.07, 0.1, 0.2, 0.3, 0.5, 0.7, 1), c(0.001, 0.01, 0.1, 1, 10)); names(ticks.all) <- what.all
		cs.all <- list(viridis, cividis, plasma, plasma, plasma); names(cs.all) <- what.all
		for(w in names(s)) {
			cat("Plot", w, "\n")
			r <- s[[w]]
			if(log.all[w] == "y") {
				vr <- values(r)
				vr[vr < min.all[w]] <- NA
				values(r) <- log10(vr)
				brk <- seq(log10(min.all[w]), log10(max.all[w]), by = 0.1)
				if(is.na(ticks.all[[w]][1])) {
					at <- seq(ceiling(min(brk)), floor(max(brk)), by = 1)
				} else {
					at <- log10(ticks.all[[w]])
				}
				arg <- list(at = at, labels=10**at)
				col <- cs.all[[w]](length(brk) - 1)
				if(is.null(vertices)) plot(r, main = paste(w, " (", stat, ") ", sep = ""), col = col, breaks=brk, axis.args=arg, legend.shrink = 1, colNA = "grey90")
				if(!is.null(vertices)) plot(r, main = paste(w, " (", stat, ") ", sep = ""), col = col, breaks=brk, axis.args=arg, legend.shrink = 1, colNA = "grey90", addfun = function() lines(transect))
			}
			if(log.all[w] == "") {
				brk <- seq(min.all[w], max.all[w],, 50)
				at <- seq(min(brk), max(brk), by = by.all[w])
				arg <- list(at = at, labels=at)
				col <- cs.all[[w]](length(brk) - 1)
				if(is.null(vertices)) plot(r, main = paste(w, " (", stat, ") ", sep = ""), col = col, breaks=brk, axis.args=arg, legend.shrink = 1, colNA = "grey90")
				if(!is.null(vertices)) plot(r, main = paste(w, " (", stat, ") ", sep = ""), col = col, breaks=brk, axis.args=arg, legend.shrink = 1, colNA = "grey90", addfun = function() lines(transect))
			}
		}
		return(s)
	}
	col.all <- c(1, 1, 2, 3, 2); names(col.all) <- what.all
	lty.all <- c("solid", "72", "solid", "solid", "72"); names(lty.all) <- what.all
	log.all <- c("", "", "y", "y", "y"); names(log.all) <- what.all
	lin.all <- c(0, 3, 6, 0, 3); names(lin.all) <- what.all
	sid.all <- c(2, 2, 2, 4, 4); names(sid.all) <- what.all
#	min.all <- c(-200, 0.02,  0.001, 0.02,  0.001); names(min.all) <- what.all
	pdepth <- NA; ddepth <- NA
	if("depth" %in% what) {
		vdepth <- x$data[, "depth"]
		pdepth <- pretty(vdepth)
		ddepth <- diff(pdepth)[1]
	}
	min.all <- c(pdepth[1],              0.02,  0.001, 0.02,  1.e-4); names(min.all) <- what.all
#	max.all <- c(   0, 0.22,100.000, 1.43,100.000); names(max.all) <- what.all
	max.all <- c(pdepth[length(pdepth)], 0.22,100.000, 1.43,100.000); names(max.all) <- what.all
	insert.na <- function(dum1, i.br) {
		i.deb <- c(0, i.br) + 1
		i.fin <- c(i.br, length(dum1))
		dum2 <- NULL
		for(i in 1:(length(i.br)+1)) {
			i.a <- i.deb[i]
			i.b <- i.fin[i]
			dum3 <- dum1[i.a:i.b]
			if(i != length(i.br) + 1) dum2 <- append(dum2, c(dum3, NA))
			else dum2 <- append(dum2, dum3)
		}
		dum2
	}
	if(type == "LonTransect") {
		u <- x$data[, "longitude"]
		i.br <- which(abs(diff(u)) > 1./240.+0.00001)
		u.na <- insert.na(u, i.br)
		omai <- par("mai"); omgp <- par("mgp")
		par(mai = par("mai") * c(1, 2, 1, 3), mgp = par("mgp") / 2)
		plot(u, rep(0, length(u)), type = "n", axes = FALSE, xlab = "longitude", ylab = "",
			xlim = lon,
			main = paste("latitude", round(x$data[1, "latitude"], digits = 5), "-", stat))
		box()
		axis(1)
		for(w in what) {
			par(new = TRUE)
			dataw.na <- insert.na(x$data[, w], i.br)
			plot(u.na, dataw.na, type = "l", lty = lty.all[w], col = col.all[w], lwd = 2,
				xlim = lon,
				ylim = c(min.all[w], max.all[w]),
				log = log.all[w], axes = FALSE, xlab = "", ylab = "")
			magaxis2(side = sid.all[w], line = lin.all[w], col.ticks = col.all[w], col.axis = "white"   , col = "white"   , lty = 1         , lwd.axis = 2, lwd.ticks = 2, mgp = c(lin.all[w] + 1.25, lin.all[w] + 0.5, lin.all[w]))
			magaxis2(side = sid.all[w], line = lin.all[w], col.ticks = col.all[w], col.axis = col.all[w], col = col.all[w], lty = lty.all[w], lwd.axis = 2, lwd.ticks = 2, mgp = c(lin.all[w] + 1.25, lin.all[w] + 0.5, lin.all[w]))
			mtext(w, side = sid.all[w], line = lin.all[w] + 1.25, col = col.all[w])
		}
		par(mai = omai, mgp = omgp)
		return(invisible(NULL))
	}
	if(type == "LatTransect") {
		u <- x$data[, "latitude"]
		i.br <- which(abs(diff(u)) > 1./240.+0.00001)
		u.na <- insert.na(u, i.br)
		omai <- par("mai"); omgp <- par("mgp")
		par(mai = par("mai") * c(1, 2, 1, 3), mgp = par("mgp") / 2)
		plot(u, rep(0, length(u)), type = "n", axes = FALSE, xlab = "latitude", ylab = "",
			xlim = lat,
			main = paste("longitude", round(x$data[1, "longitude"], digits = 5), "-", stat))
		box()
		axis(1)
		for(w in what) {
			par(new = TRUE)
			dataw.na <- insert.na(x$data[, w], i.br)
			plot(u.na, dataw.na, type = "l", lty = lty.all[w], col = col.all[w], lwd = 2,
				xlim = lat,
				ylim = c(min.all[w], max.all[w]),
				log = log.all[w], axes = FALSE, xlab = "", ylab = "")
			magaxis2(side = sid.all[w], line = lin.all[w], col.ticks = col.all[w], col.axis = "white"   , col = "white"   , lty = 1         , lwd.axis = 2, lwd.ticks = 2, mgp = c(lin.all[w] + 1.25, lin.all[w] + 0.5, lin.all[w]))
			magaxis2(side = sid.all[w], line = lin.all[w], col.ticks = col.all[w], col.axis = col.all[w], col = col.all[w], lty = lty.all[w], lwd.axis = 2, lwd.ticks = 2, mgp = c(lin.all[w] + 1.25, lin.all[w] + 0.5, lin.all[w]))
			mtext(w, side = sid.all[w], line = lin.all[w] + 1.25, col = col.all[w])
		}
		par(mai = omai, mgp = omgp)
		return(invisible(NULL))
	}
	if(type == "Transect") {
		u <- x$distances
		omai <- par("mai"); omgp <- par("mgp")
		par(mai = par("mai") * c(1, 2, 1, 3), mgp = par("mgp") / 2)
		plot(u, rep(0, length(u)), type = "n", axes = FALSE, xlab = "distance [km]", ylab = "",
			xlim = range(u),
			main = stat)
		box()
		axis(1)
		for(w in what) {
			par(new = TRUE)
			dataw <- x$data[, w]
			plot(u, dataw, type = "l", lty = lty.all[w], col = col.all[w], lwd = 2,
				xlim = range(u),
				ylim = c(min.all[w], max.all[w]),
				log = log.all[w], axes = FALSE, xlab = "", ylab = "")
			magaxis2(side = sid.all[w], line = lin.all[w], col.ticks = col.all[w], col.axis = "white"   , col = "white"   , lty = 1         , lwd.axis = 2, lwd.ticks = 2, mgp = c(lin.all[w] + 1.25, lin.all[w] + 0.5, lin.all[w]))
			magaxis2(side = sid.all[w], line = lin.all[w], col.ticks = col.all[w], col.axis = col.all[w], col = col.all[w], lty = lty.all[w], lwd.axis = 2, lwd.ticks = 2, mgp = c(lin.all[w] + 1.25, lin.all[w] + 0.5, lin.all[w]))
			mtext(w, side = sid.all[w], line = lin.all[w] + 1.25, col = col.all[w])
		}
		par(mai = omai, mgp = omgp)
		return(invisible(NULL))
	}
}
