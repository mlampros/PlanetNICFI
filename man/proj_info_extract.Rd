% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nicfi_utils.R
\name{proj_info_extract}
\alias{proj_info_extract}
\title{Extract the Projection from a (virtual) raster file}
\usage{
proj_info_extract(path_to_raster, verbose = FALSE)
}
\arguments{
\item{path_to_raster}{a valid path to a raster file}

\item{verbose}{a boolean. If TRUE then information will be printed out in the console}
}
\value{
a character string with the projection information
}
\description{
Extract the Projection from a (virtual) raster file
}
\examples{

require(PlanetNICFI)

pth_vrt = system.file('data_files/virt_rast.vrt', package = "PlanetNICFI")

proj_info = proj_info_extract(path_to_raster = pth_vrt)

}
