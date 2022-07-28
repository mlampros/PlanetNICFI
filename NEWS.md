
## PlanetNICFI 1.0.4

* I've added the "figures" folder in the "man" directory
* I updated the *README.md* file


## PlanetNICFI 1.0.4

* I've included the *sequential_download_paths()* for a sequential download of the imagery that doesn't require the *aria2c* system requirement (it utilizes internally the base *download.file()* function)
* I removed the *'suppress_warnings'* parameter from the *'proj_info_extract()'* function
* The package *'raster'* was moved to suggests. I can not replace it with the *'terra'* package because the latter is not yet supported from the *'RStoolbox'* package (see vignette)
* I removed the *'gdalUtils'* package due to the fact that it currently gives an error in the *'r-devel-windows-x86_64-new-UL'* test Flavor on CRAN
* I removed the *'return_raster'* parameter from the *'nicfi_crop_images()'* function because I replaced the *'gdalUtils::gdalwarp()'* function with the *'sf::gdal_utils()'* function
* I've updated the README.md file


## PlanetNICFI 1.0.3

* I've added an error case if the user does not have a valid registration for the NICFI Image data (see issue https://github.com/mlampros/PlanetNICFI/issues/1)


## PlanetNICFI 1.0.2

* I updated the *README.md* file


## PlanetNICFI 1.0.1

* I've included the *URL* in the *DESCRIPTION* file
* I've included the *proj_info_extract()* function and modified the examples of the package documentation
* I've modified the *README.md* file by including additional information (especially about instructions on how to use the *docker* image)
* I've included the *virt_rast.vrt* file in the *data_files* directory and I also added one test case


## PlanetNICFI 1.0.0
