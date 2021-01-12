clu_download <- function(url, localf) {
	urlobsvlfr <- "http://obs-vlfr.fr/Pfunction/"
	urlpangaea <- "https://doi.pangaea.de/10.1594/PANGAEA.910898"
	cat("###############################################################################\n")
	count <- 0
	maxcount <- 6
	while(TRUE) {
		errcode <- tryCatch(
			download.file(url, localf),
			warning = function(w) {
				cat(
					"*******************************************************************************",
					w$message,
					"if you experience problems, you may try to connect to one of these URLs with your browser:",
					paste(" ", urlobsvlfr, "( cl_DownloadData(alt = 1) )"),
					"or",
					paste(" ", urlpangaea, "( cl_DownloadData(alt = 2) )"),
				 	"*******************************************************************************",
					sep = "\n"
				)
				err <- -1
				i404 <- grep("404", w$message)
				i503 <- grep("503", w$message)
				if(length(i404) == 1) err <- 404
				if(length(i503) == 1) err <- 503
				err
			}
		)
		if(errcode == 0) {
			cat("... DONE\n")
			break
		}
		else if(errcode == 404) {
			cat("ERROR 404 : FILE NOT FOUND\n")
			unlink(localf)
			break
		}
		else if(errcode == 503) {
			count <- count + 1
			cat("ERROR 503 : REQUEST DELAYED ? waiting 10 seconds", "(", count, "/", maxcount, ")\n")
			Sys.sleep(10)
			if(count == maxcount) {
				cat("download aborted\n")
				unlink(localf)
				break
			}
		}
		else {
			cat("download aborted\n")
			unlink(localf)
			break
		}
	}
	cat("###############################################################################\n")
	return(invisible(NULL))
}
