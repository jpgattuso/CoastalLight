% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cl_Transect.R
\name{cl_Transect}
\alias{cl_Transect}
\title{Extracts a transect from data returned by function \code{cl_GetData()}}
\usage{
cl_Transect(x, vertices, plt = FALSE)
}
\arguments{
\item{x}{: [list] a list returned by function \code{cl_GetData()}
whose field \code{type} has value "Area"}

\item{vertices}{: [list] a polygonal line that represents a transect;
each element of this list is a geographic point,
i.e. a vector of length 2 (longitude, latitude);}

\item{plt}{: [logical] make a plot of the transect on the maps of the variables
(default is FALSE)}
}
\value{
: [list] a list with 5 components :
          \itemize{
             \item the same 4 components as returned by function \code{cl_GetData()} :
          \itemize{
             \item \code{type} : the type of geographical zone; its value is "Transect"
             \item \code{lon} : identical to component \code{lon} of x
             \item \code{lat} : identical to component \code{lat} of x
             \item \code{data} : a matrix of data along the transect with columns names
                               "longitude", "latitude",
                         and variables requested. 
          }
             \item a fifth component \code{distances} which represents the distance
                   traveled from the first vertex of the polygonal line
          }
}
\description{
When it is operated with \code{type} "Area"
function \code{cl_GetData()} returns data at all available points of a geographic area; 
function \code{cl_Transect()} extract a transect inside this area in 2 steps;
     \itemize{
        \item rasterization of the data
        \item extraction of the transect
     }
output of this function has the same structure as output of function \code{cl_GetData()}
and may be plotted by function \code{cl_PlotData()}
}
\examples{
gabes <- cl_GetData(lon = c(10, 14), lat = c(32.5, 36), dir = "./CoastalLight.d")
vertices <- list(c(10.1,33), c(13.5,35), c(13,34))
#X11()
par(mfrow = c(3, 2))
tr <- cl_Transect(gabes, vertices, plt = TRUE)
cl_PlotData(tr)
}
