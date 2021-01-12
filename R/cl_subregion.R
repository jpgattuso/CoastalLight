#' @title 
#' Download a P-function for a subregion
#'
#' @description 
#' Connects to a server in order to calculate and download a P-function for a subregion of one of the three standard regions
#'
#' @param browse : [logical] do you want to connect to the server with your default browser ? (default = TRUE).
#'                           If FALSE no connection, but the url is returned
#'                                    (you may connect to this url, independently of the package).
#' 
#' @return : 
#' [character] the url ("http://obs-vlfr.fr/Pfunction/")
#'
#' @examples
#' cl_subregion()
cl_subregion <- function(browse = TRUE) {
	regions <- c("NonPolar", "Arctic", "Antarctic")
	url <- "http://obs-vlfr.fr/Pfunction/"
	cat("this url :", url, "\n")
	cat("  allows to calculate and download a P-function\n")
	cat("  for a subregion of one of the 3 main regions :\n")
	cat("   ", regions, "\n")
	if(browse) browseURL(url = url)
	return(url)
}
