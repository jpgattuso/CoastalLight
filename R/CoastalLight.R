#' @details
#' This package has \strong{two goals} :
#' \enumerate{
#'   \item calculate the surface area of the sea floor which receives more than a given threshold of irradiance.
#'   \item get various geographic and optical data from a database consisting of files to download
#' }
#' \describe{
#'   \item{Goal 1}{: 
#'     \enumerate{
#'       \item function \code{cl_surface()} is the main function;
#'             it returns the surface areas receiving irradiance above given thresholds,
#'             or the P-functions (see Gattuso et al. - reference below) for three standard regions;
#'             \strong{NonPolar}, \strong{Arctic}, and \strong{Antarctic};
#'             It is the only function to use if you are interested in one of these
#'             standard regions;
#'             P-functions for these regions have been tabulated and
#'             the tables included in the package.
#'       \item function \code{cl_subregion()} is used to produce the P-functions
#'             of a \strong{subregion of one of the three standard regions};
#'             it connects to a server that does the calculations and offers to download
#'             a file containing the P-functions for this subregion;
#'             this file is ready to be used by the function \code{cl_surface()}.
#'     }
#'   }
#'   \item{Goal 2}{:
#'     \enumerate{
#'       \item function \code{cl_DownloadData()} downloads the data;
#'       \item function \code{cl_GetData()} gets the data on a geographic zone defined by the user;
#'       \item function \code{cl_PlotData()} plots the data, as returned by function \code{cl_GetData()}.
#'       \item function \code{cl_Transect()} complements function \code{cl_GetData()}
#'             for the extraction of data along a transect.
#'     }
#'   }
#'}
#' \strong{Data and units :}
#'   \itemize{
#'     \item P-functions : \%
#'     \item longitude : \eqn{decimal \, degree \; [-180 ; 180]}
#'     \item latitude : \eqn{decimal \, degree \; [-90 ; 90]}
#'     \item depth : \eqn{m}
#'     \item area : \eqn{km^{2}}
#'     \item par, parbottom : \eqn{mol.photons \; m^{-2} \; d^{-1}}
#'     \item kdpar : \eqn{m^{-1}}
#'   }
#'
#' @references{
#'             Gattuso J.-P., Gentili B., Antoine D. & Doxaran D., 2020.
#'             Global distribution of photosynthetically available radiation on the seafloor.
#'             Earth System Science Data 12:1697-1709.
#'             http://dx.doi.org/10.5194/essd-12-1697-2020
#' }
"_PACKAGE"
#> [1] "_PACKAGE"
