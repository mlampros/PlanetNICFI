
[![tic](https://github.com/mlampros/PlanetNICFI/workflows/tic/badge.svg?branch=master)](https://github.com/mlampros/PlanetNICFI/actions)
[![codecov.io](https://codecov.io/github/mlampros/PlanetNICFI/coverage.svg?branch=master)](https://codecov.io/github/mlampros/PlanetNICFI?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/PlanetNICFI)](https://CRAN.R-project.org/package=PlanetNICFI)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/PlanetNICFI?color=blue)](http://www.r-pkg.org/pkg/PlanetNICFI)
<a href="https://www.buymeacoffee.com/VY0x8snyh" target="_blank"><img src="https://www.buymeacoffee.com/assets/img/custom_images/orange_img.png" alt="Buy Me A Coffee" height="21px" ></a>
[![](https://img.shields.io/docker/automated/mlampros/planetnicfi.svg)](https://hub.docker.com/r/mlampros/planetnicfi)
[![Dependencies](https://tinyverse.netlify.com/badge/PlanetNICFI)](https://cran.r-project.org/package=PlanetNICFI)


## PlanetNICFI

<br>

The **PlanetNICFI** R package includes functions to download and process the [NICFI](https://www.nicfi.no/) (**Norway's International Climate and Forest Initiative**) Planet Satellite Imagery utilizing the [Planet Mosaics API](https://developers.planet.com/docs/basemaps/reference/#tag/Basemaps-and-Mosaics). More details on the functionality of PlanetNICFI can be found in the [blog-post](http://mlampros.github.io/2021/06/12/Planet_NICFI_Satellite_Imagery/), Vignette and in the package Documentation ( *scroll down for information on how to use the* **docker image** )

<br>

#### **Parameters**:

<br>

An **important parameter** to keep in mind is the **page_size** of the [nicfi_quads_bbox()](https://mlampros.github.io/PlanetNICFI/reference/nicfi_quads_bbox.html) function. Depending on what the user defines as **bbox_AOI** or **wkt_AOI** the **page_size** parameter needs to be adjusted too. The bigger the Area of Interest is the bigger the **page_size** parameter must be. That means the [nicfi_quads_bbox()](https://mlampros.github.io/PlanetNICFI/reference/nicfi_quads_bbox.html) function will return more Image products for a bigger area and the **page_size** parameter **must** be bigger than the **default** value of **50** so that all available Image products will be returned.

<br>

Another **important information** to keep in mind (if using the *'aria2c'* software to download the data - see the *'sequential_download_paths()'* function for an alternative) is that the user currently **has to download** the NICFI .tif files in a **temporary directory** due to the **aria2c_download_paths()** function. By specifying a different **default_directory** parameter other than a temporary directory in the **aria2c_bulk_donwload()** function the .tif files won't be downloaded in the correct folder.

<br>

#### **System Requirements**:

<br>

##### **GDAL**

The usage of the *PlanetNICFI* package requires a geospatial setup as specified in the [sf](https://github.com/r-spatial/sf#installing) or [terra](https://github.com/rspatial/terra#from-source-code) README.md files.

<br>

##### **aria2c**

Besides the *'sequential_download_paths()'* function the [aria2c](https://aria2.github.io/) software is another option to download the data in parallel. It has to be installed first in the Operating System:

On **Ubuntu** this can be done using:

```R
sudo apt-get install aria2

```

<br>

On **Macintosh** use,

```R
brew install aria2

```

<br>

and on **Windows 10** based on a [web-tutorial](https://www.tutorialexample.com/install-aria2-on-win10-to-download-files-a-beginner-guide/):

* first navigate to the [Github repository of aria2c](https://github.com/aria2/aria2/releases/tag/release-1.35.0)
* then download the **aria2-1.35.0-win-64bit-build1.zip** (where **1.35.0** corresponds to the current version as of **June 2021** - this might change in the future)
* unzip the downloaded file 
* create a folder named as **aria2** in **C:\\**
* copy the **aria2c.exe** file to **C:\\aria2**
* add the **C:\\aria2** to the windows system path by updating the environment variables
* finally open the window command prompt, enter **aria2c** and the output message should show the aria2c options

<br>

To install the package from CRAN use, 

```R
install.packages("PlanetNICFI")

```
<br>

and to download the latest version of the package from Github,

```R
remotes::install_github('mlampros/PlanetNICFI')

```

<br><br>

#### **Tropical Forests Satellite Data Coverage**

<br>

<img src="man/figures/nicfi_tropical_forests.png" ></img>

<br>

#### **Docker Image** (if you want to avoid the installation of the System Requirements)

<br>

**Docker images** of the *PlanetNICFI* package are available to download from my [dockerhub](https://hub.docker.com/r/mlampros/planetnicfi) account. The images come with *Rstudio* and the *R-development* version (latest) installed. The whole process was tested on Ubuntu 18.04. To **pull** & **run** the image do the following,

<br>

```R

docker pull mlampros/planetnicfi:rstudiodev

docker run -d --name rstudio_dev -e USER=rstudio -e PASSWORD=give_here_your_password --rm -p 8787:8787 mlampros/planetnicfi:rstudiodev

```

<br>

The user can also **bind** a home directory / folder to the image to use its files by specifying the **-v** command,

<br>

```R

docker run -d --name rstudio_dev -e USER=rstudio -e PASSWORD=give_here_your_password --rm -p 8787:8787 -v /home/YOUR_DIR:/home/rstudio/YOUR_DIR mlampros/planetnicfi:rstudiodev


```

<br>

In the latter case you might have first give permission privileges for write access to **YOUR_DIR** directory (not necessarily) using,

<br>

```R

chmod -R 777 /home/YOUR_DIR


```

<br>

The **USER** defaults to *rstudio* but you have to give your **PASSWORD** of preference (see [https://rocker-project.org/](https://rocker-project.org/) for more information).

<br>

Open your web-browser and depending where the docker image was *build / run* give, 

<br>

**1st. Option** on your personal computer,

<br>

```R
http://0.0.0.0:8787 

```

<br>

**2nd. Option** on a cloud instance, 

<br>

```R
http://Public DNS:8787

```

<br>

to access the Rstudio console in order to give your username and password.

<br>

### NICFI Satellite Data Program user community code repositories

* https://github.com/NICFI-Satellite-Data-Program/Code-Repository

<br>

### Attribution

Please read the **COPYRIGHTS** file of the **PlanetNICFI** R package especially the section **'OBLIGATIONS AND RESTRICTIONS'**

<br>

### Citation:

If you use the **PlanetNICFI** R package in your paper or research please cite:

<br>

```R
@Manual{,
  title = {{PlanetNICFI}: Processing of the 'Planet NICFI' Satellite Imagery using R},
  author = {Lampros Mouselimis},
  year = {2023},
  note = {R package version 1.0.5 using Imagery 2021 Planet Labs Inc. All use subject to the Participant License Agreement},
  url = {https://CRAN.R-project.org/package=PlanetNICFI},
}
```

<br>
