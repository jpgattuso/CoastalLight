#' @title 
#' Download files from pangaea : \preformatted{https://doi.pangaea.de/10.1594/PANGAEA.910898}
#' otherwise from \preformatted{http://obs-vlfr.fr/Pfunction/}
#'
#' @description 
#' Download files used by \code{cl_GetData()} function.
#'
#' @param month : [integer] decimal month (0-12). Default = 0.
#' \itemize{
#'   \item 0 indicates the climatology over 21 years (1998-2018).
#'   \item 1 to 12 indicates the decimal value for a single month (1 = January, ...);
#' }
#'
#' @param DownloadGeo : [logical] download the "geographic" file containing
#'                 longitude, latitude, depth and surface area of the pixels, mandatory for retrieving pixel locations
#'                 (default = TRUE)
#'
#' @param dirdata : [character] The directory where the files are stored (default = "./CoastalLight.d");
#'
#' @param alt : [integer] the way of downloading (0, 1 or 2). Default = 0
#' \itemize{
#'   \item 0 automatically connect to Pangaea database and download
#'   \item 1 launch your default browser and connect for manual download to URL \preformatted{
#'         http://obs-vlfr.fr/Pfunction/
#'                       }  
#'   \item 2 launch your default browser and connect for manual download to URL \preformatted{
#'         https://doi.pangaea.de/10.1594/PANGAEA.910898
#'                       }  
#' }
#' N.B. : if you choose a manual download (alt = 1 or 2), you have to download files in the appropriate directory,
#'        i.e. the argument \code{dirdata} has no effect.
#'
#' @param stat : [character] the statistical value you want for variables "par", "kdpar" and "parbottom";
#'               choose one among "mean" (mean value),
#'               "min" (minimum), "max" (maximum), "sd" (standard deviation)
#'                 (default = "mean")
#' N.B. : "min", "max", "sd", are available only at URL \preformatted{
#'         http://obs-vlfr.fr/Pfunction/
#'                       }  
#' @return : none
#'
#' @examples
#' cl_DownloadData() # annual climatology over 21 years (1998-2018)
#' cl_DownloadData(month = 1) # monthly climatology (January) over 21 years (1998-2018)
#' cl_DownloadData(month = 8) # monthly climatology (August) over 21 years (1998-2018)
#' cl_DownloadData(month = 8, stat = "sd") # idem, 
#' but download standard deviation of optical parameters
cl_DownloadData <- function(month = 0, DownloadGeo = TRUE, dirdata = "CoastalLight.d", alt = 0, stat = "mean") {
	urlobsvlfr <- "http://obs-vlfr.fr/Pfunction/"
	urlpangaea <- "https://doi.pangaea.de/10.1594/PANGAEA.910898"
	if(stat != "mean") {
		cat("data are available only at", urlobsvlfr, "\n")
		alt <- 1
	}
	if(! file.exists(dirdata)) {
		dir.create(dirdata)
		cat("directory", dirdata, "created\n")
	}
	if(alt == 1 || alt == 2) {
		cat("Download these file(s) in directory", dirdata, ":\n")
		if(DownloadGeo) {
			f <- paste(paste("CoastalLight", "geo", sep = "_"), "nc", sep = ".")
			cat(" ", f, "(unless it has already done)\n")
		}
		cmonth <- formatC(month, format = "d", width = 2, flag = "0")
		if(stat != "mean") {
			f <- paste(paste("CoastalLight", stat, cmonth, sep = "_"), "nc", sep = ".")
		} else {
			f <- paste(paste("CoastalLight", cmonth, sep = "_"), "nc", sep = ".")
		}
		cat(" ", f, "\n")
	}
	if(alt == 1) {
		cat("see \"Download optical data\" on the web page\n")
		browseURL(url = urlobsvlfr)
		return(invisible(NULL))
	}
	if(alt == 2) {
		cat("see \"View dataset as HTML\" on the web page\n")
		browseURL(url = urlpangaea)
		return(invisible(NULL))
	}
	if(DownloadGeo) {
		f <- paste(paste("CoastalLight", "geo", sep = "_"), "nc", sep = ".")
		url <- paste("http://hs.pangaea.de/sat/Globcolour/Gattuso-etal_2020", f, sep = "/")
		localf <- paste(dirdata, f, sep = "/")
		if(! file.exists(localf)) {
			cat("---> downloading", f, "\n")
			clu_download(url, localf)
		} else {
			cat(f, "already downloaded in directory", dirdata, "\n")
		}
	}
	cmonth <- formatC(month, format = "d", width = 2, flag = "0")
	f <- paste(paste("CoastalLight", cmonth, sep = "_"), "nc", sep = ".")
	url <- paste("http://hs.pangaea.de/sat/Globcolour/Gattuso-etal_2020", f, sep = "/")
	localf <- paste(dirdata, f, sep = "/")
	if(! file.exists(localf)) {
		cat("---> downloading", f, "\n")
		clu_download(url, localf)
	} else {
		cat(f, "already downloaded in directory", dirdata, "\n")
	}
}
