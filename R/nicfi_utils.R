

#' inner function of 'compute_elapsed_time'
#'
#' @param secs a numeric value specifying the seconds
#' @param estimated a boolean. If TRUE then the output label becomes the 'Estimated time'
#' @return a character string showing the estimated or elapsed time
#'
#' @keywords internal

inner_elapsed_time = function(secs, estimated = FALSE) {
  tmp_hours = as.integer((secs / 60) / 60)
  tmp_hours_minutes = (secs / 60) %% 60
  tmp_seconds = secs %% 60
  est_verb = ifelse(estimated, "Estimated time: ", "Elapsed time: ")
  res_out = paste(c(est_verb, tmp_hours, " hours and ", as.integer(tmp_hours_minutes), " minutes and ", as.integer(tmp_seconds), " seconds."), collapse = "")
  return(res_out)
}


#' elapsed time in hours & minutes & seconds
#'
#' @param time_start a numeric value specifying the start time
#' @return It does not return a value but only prints the time in form of a character string in the R session
#'
#' @keywords internal

compute_elapsed_time = function(time_start) {
  t_end = proc.time()
  time_total = as.numeric((t_end - time_start)['elapsed'])
  time_ = inner_elapsed_time(time_total)
  cat(time_, "\n")
}



#' Extract the Projection from a (virtual) raster file
#'
#' @param path_to_raster a valid path to a raster file
#' @param verbose a boolean. If TRUE then information will be printed out in the console
#' @return a character string with the projection information
#'
#' @importFrom terra rast crs
#'
#' @export
#'
#' @examples
#'
#' require(PlanetNICFI)
#'
#' pth_vrt = system.file('data_files/virt_rast.vrt', package = "PlanetNICFI")
#'
#' proj_info = proj_info_extract(path_to_raster = pth_vrt)
#'

proj_info_extract = function(path_to_raster, verbose = FALSE) {

  if (!file.exists(path_to_raster)) stop(glue::glue("The raster file '{path_to_raster}' does not exist!"), call. = F)

  rst = terra::rast(x = path_to_raster)
  crs_value = terra::crs(x = rst, proj = TRUE)
  proj_dat = trimws(x = crs_value, which = 'both')

  if (is.na(proj_dat)) stop("The projection-info based on the 'terra::crs()' function corresponds to NA! Highly probable 'proj4' is not available in your Operating System!", call. = F)
  return(proj_dat)
}



#' Returns all 'monthly' or 'bi-annually' mosaic files of the NICFI data
#'
#' @param planet_api_key a character string specifying the Planet API key (see the references on how to acquire this key)
#' @param type a character string specifying the type of NICFI data. It can be either 'monthly' or 'bi_annually'
#' @param crs_bbox an integer specifying the Coordinates Reference System for the bounding box computation only.
#' @param URL this character string specifies the default URL which is required to come to the output mosaic metadata information
#' @param verbose a boolean. If TRUE then information will be printed out in the console
#' @return an object of class list
#'
#' @references
#'
#' https://developers.planet.com/quickstart/
#'
#' https://developers.planet.com/quickstart/apis/
#'
#' https://developers.planet.com/docs/basemaps/reference/#tag/Basemaps-and-Mosaics
#'
#' @export
#'
#' @importFrom httr GET authenticate content
#' @importFrom sf st_bbox st_as_sfc st_as_text st_crs
#' @importFrom data.table setDT rbindlist
#' @importFrom glue glue
#'
#' @examples
#'
#' \dontrun{
#'
#' require(PlanetNICFI)
#'
#' api_key = 'use_your_planet_nicfi_API_key'
#'
#' #........
#' # monthly
#' #........
#'
#' mosaic_files = nicfi_mosaics(planet_api_key = api_key,
#'                              type = 'monthly',
#'                              crs_bbox = 4326,
#'                              URL = 'https://api.planet.com/basemaps/v1/mosaics',
#'                              verbose = TRUE)
#' #............
#' # bi-annually
#' #............
#'
#' mosaic_files = nicfi_mosaics(planet_api_key = api_key,
#'                              type = 'bi_annually',
#'                              crs_bbox = 4326,
#'                              URL = 'https://api.planet.com/basemaps/v1/mosaics',
#'                              verbose = TRUE)
#'
#' #........................................
#' # WKT of the area covered from NICFI data
#' #........................................
#'
#' nicfi_aoi = sf::st_as_sfc(mosaic_files$dtbl_mosaic$mosaic_wkt[1], crs = 4326)
#' cat(sf::st_as_text(nicfi_aoi))
#'
#' }


nicfi_mosaics = function(planet_api_key,
                         type = 'monthly',
                         crs_bbox = 4326,
                         URL = 'https://api.planet.com/basemaps/v1/mosaics',
                         verbose = FALSE) {

  if (verbose) t_start = proc.time()
  if (!type %in% c('monthly', 'bi_annually')) stop("The 'type' parameter must be either 'monthly' or 'bi_annually'!", call. = F)

  query_response = httr::GET(url = URL, config = httr::authenticate(user = planet_api_key, password = ""))
  content_response = httr::content(x = query_response, as = "parsed")

  link = content_response$`_links`$`_self`
  mosaics = content_response$mosaics

  valid_nicfi_image_types = unique(unlist(lapply(mosaics, function(x) unlist(x$item_types))))
  user_has_img_types = paste(sapply(valid_nicfi_image_types, function(x) glue::glue("'{x}'")), collapse = ', ')
  if (!all(c("REOrthoTile", "PSScene4Band") %in% valid_nicfi_image_types)) stop(glue::glue("Valid 'NICFI' Image types are 'REOrthoTile' and 'PSScene4Band', whereas you have {user_has_img_types}! Please follow the suggested registration in https://www.planet.com/nicfi/"), call. = F)

  mosaics = lapply(mosaics, function(x) {                                                 # extract the information of all mosaics (including the id's)

    nam_trunc = gsub('planet_medres_normalized_analytic_', '', x$name)
    nam_trunc = gsub('_mosaic', '', nam_trunc)
    nam_trunc = ifelse(nchar(nam_trunc) > 7, 'bi_annually', 'monthly')

    bbx = as.vector(unlist(x$bbox))
    plg = sf::st_bbox(c(xmin = bbx[1],
                        ymin = bbx[2],
                        xmax = bbx[3],
                        ymax = bbx[4]),
                      crs = sf::st_crs(crs_bbox))

    plg = sf::st_as_sfc(plg, crs = crs_bbox)
    plg = sf::st_as_text(plg)

    item = list(id = x$id,
                link_self = x$`_links`$`_self`,
                link_quads = x$`_links`$quads,
                link_tiles = x$`_links`$tiles,
                xmin = bbx[1],
                ymin = bbx[2],
                xmax = bbx[3],
                ymax = bbx[4],
                mosaic_wkt = plg,
                epsg = x$coordinate_system,
                datatype = x$datatype,
                first_acquired = as.Date(x$first_acquired),
                last_acquired = as.Date(x$last_acquired),
                quad_size = x$grid$quad_size,
                resolution = x$grid$resolution,
                item_types = paste(unlist(x$item_types), collapse = '-'),
                level = x$level,
                name = x$name,
                product_type = x$product_type,
                quad_download = x$quad_download,
                type = nam_trunc)

    item[['months']] = length(seq(from = item$first_acquired, to = item$last_acquired, by = '1 months')) - 1
    item$first_acquired = as.character(item$first_acquired)
    item$last_acquired = as.character(item$last_acquired)

    item = data.table::setDT(item)
    item
  })

  mosaics = data.table::rbindlist(mosaics)
  idx = which(mosaics$type == type)
  mosaics = mosaics[idx, ]

  if (verbose) compute_elapsed_time(t_start)

  return(list(dtbl_mosaic = mosaics,
              nicfi_weblink = link))
}



#' Computes the NICFI Quads based on a mosaic-id and a specified Area of Interest (bounding box or Well Known Text)
#'
#' @param planet_api_key a character string specifying the Planet API key (see the references on how to acquire this key)
#' @param mosaic_id a character string specifying the 'Mosaic' id as returned from the 'nicfi_mosaics()' function
#' @param bbox_AOI either NULL or a list of the format "list(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)" that includes the bounding box 'xmin', 'xmax', 'ymin', 'ymax' coordinate values of the Area of Interest (AOI) based on which the intersected NICFI Quads have to be computed
#' @param wkt_AOI either NULL or a character string specifying the Well Known Text (WKT)  of the Area of Interest (AOI) based on which the intersected NICFI Quads have to be computed
#' @param page_size an integer value specifying the number of Quads to return (that intersect with the input bounding box or Well known text)
#' @param crs_bbox an integer specifying the Coordinates Reference System for the bounding box computation only.
#' @param verbose a boolean. If TRUE then information will be printed out in the console
#' @return an object of class list
#'
#' @references
#'
#' https://developers.planet.com/docs/basemaps/reference/#tag/Basemaps-and-Mosaics
#'
#' @export
#'
#' @importFrom httr GET authenticate content
#' @importFrom sf st_as_sfc st_make_valid st_bbox st_as_text st_crs
#' @importFrom data.table setDT rbindlist
#' @importFrom glue glue
#'
#' @examples
#'
#' \dontrun{
#'
#' require(PlanetNICFI)
#'
#' #....................................
#' # first extract the available Mosaics
#' #....................................
#'
#' api_key = 'use_your_planet_nicfi_API_key'
#'
#' mosaic_files = nicfi_mosaics(planet_api_key = api_key,
#'                              type = 'monthly',
#'                              crs_bbox = 4326,
#'                              URL = 'https://api.planet.com/basemaps/v1/mosaics',
#'                              verbose = TRUE)
#'
#' #....................................
#' # keep the mosaic of 'September 2020'
#' #....................................
#'
#' keep_idx = 1
#' mosaic_ID = mosaic_files$dtbl_mosaic$id[keep_idx]
#'
#'
#' #.....................................................
#' # then extract the available Quad files for the Mosaic
#' #.....................................................
#'
#' wkt_file = system.file('data_files/Sugar_Cane_Bolivia.wkt', package = "PlanetNICFI")
#' WKT = readLines(wkt_file, warn = FALSE)
#'
#' quad_files = nicfi_quads_bbox(planet_api_key = api_key,
#'                               mosaic_id = mosaic_ID,
#'                               bbox_AOI = NULL,
#'                               wkt_AOI = WKT,
#'                               page_size = 10,
#'                               crs_bbox = 4326,
#'                               verbose = TRUE)
#' }


nicfi_quads_bbox = function(planet_api_key,
                            mosaic_id,
                            bbox_AOI = NULL,
                            wkt_AOI = NULL,
                            page_size = 50,
                            crs_bbox = 4326,
                            verbose = FALSE) {

  if (verbose) t_start = proc.time()
  if (is.null(bbox_AOI) & is.null(wkt_AOI)) stop("You must specify one of the 'bbox_AOI' or 'wkt_AOI'!", call. = F)
  if (!is.null(bbox_AOI) & !is.null(wkt_AOI)) stop("You must specify ONLY one of the 'bbox_AOI' or 'wkt_AOI' and NOT both!", call. = F)

  if (!is.null(bbox_AOI)) {
    if (!inherits(bbox_AOI, 'list')) stop("The 'bbox_AOI' parameter must be of type list!", call. = F)
    if (!all(names(bbox_AOI) %in% c('xmin', 'xmax', 'ymin', 'ymax'))) stop("The 'bbox_AOI' parameter must be a named list with the following names: 'xmin', 'xmax', 'ymin', 'ymax'!", call. = F)
  }

  if (!is.null(wkt_AOI)) {
    if (!inherits(wkt_AOI, 'character')) stop("The 'wkt_AOI' parameter must be a character string!", call. = F)
    if (length(wkt_AOI) > 1)  stop("The 'wkt_AOI' parameter must be a character string of length 1!", call. = F)

    bbox_AOI = sf::st_as_sfc(wkt_AOI, crs = crs_bbox)
    bbox_AOI = sf::st_make_valid(bbox_AOI)
    bbox_AOI = sf::st_bbox(bbox_AOI)
    bbox_AOI = list(xmin = as.numeric(bbox_AOI['xmin']),
                    xmax = as.numeric(bbox_AOI['xmax']),
                    ymin = as.numeric(bbox_AOI['ymin']),
                    ymax = as.numeric(bbox_AOI['ymax']))
  }

  Quad_Link = as.character(glue::glue('https://api.planet.com/basemaps/v1/mosaics/{mosaic_id}/quads?_page_size={page_size}&bbox={bbox_AOI$xmin},{bbox_AOI$ymin},{bbox_AOI$xmax},{bbox_AOI$ymax}'))

  query_response = httr::GET(url = Quad_Link, config = httr::authenticate(user = planet_api_key, password = ""))
  content_response = httr::content(x = query_response, as = "parsed")

  content_next = content_response$`_links`$`_next`
  links_out = data.table::setDT(list(id_mosaic = mosaic_id,
                                     quad_page_link_next = ifelse(is.null(content_next), NA_character_, content_next),
                                     quad_page_link_self = content_response$`_links`$`_self`))
  quads_out = NULL

  if (length(content_response$items) > 0) {

    iter_quads = lapply(content_response$items, function(x) {

      bbx = as.vector(unlist(x$bbox))
      plg = sf::st_bbox(c(xmin = bbx[1],
                          ymin = bbx[2],
                          xmax = bbx[3],
                          ymax = bbx[4]),
                        crs = sf::st_crs(crs_bbox))
      plg = sf::st_as_sfc(plg, crs = crs_bbox)
      plg = sf::st_as_text(plg)

      iter_lst = list(id_mosaic = mosaic_id,
                      id_quad_page = x$id,
                      quad_link_self = x$`_links`$`_self`,
                      quad_link_download = x$`_links`$download,
                      quad_link_items = x$`_links`$items,
                      quad_link_thumbnail = x$`_links`$thumbnail,
                      quad_xmin = bbx[1],
                      quad_ymin = bbx[2],
                      quad_xmax = bbx[3],
                      quad_ymax = bbx[4],
                      quad_wkt = plg,
                      quad_percent_covered = x$percent_covered)

      iter_lst = data.table::setDT(iter_lst)
      iter_lst
    })

    quads_out = data.table::rbindlist(iter_quads)
  }

  if (verbose) compute_elapsed_time(t_start)

  return(list(links = links_out,
              quads = quads_out,
              Quad_Link = Quad_Link))
}



#' Format Mosaic and Quad weblinks to serve as input to the 'aria2c_bulk_donwload' function
#'
#' @param mosaic_output this parameter must be the output list of the 'nicfi_mosaics()' function
#' @param mosaic_id a character string specifying the mosaic-id as appears in the output column 'id' of the 'nicfi_mosaics()' function
#' @param quads_output this parameter must be the output list of the 'nicfi_quads_bbox()' function
#' @param img_type a character string specifying the image type to download. One of 'tif' or 'thumbnail'. The 'thumbnail' come with a .png image extension
#' @return a character vector
#'
#' @export
#'
#' @details
#'
#' The 'thumbnail' are smaller in size and it might be a good idea to download these images first (just for an overview) before proceeding to the download of the .tif files (which are more than 100 MB each)
#'
#' @importFrom glue glue
#'
#' @examples
#'
#' \dontrun{
#'
#' require(PlanetNICFI)
#'
#' #....................................
#' # first extract the available Mosaics
#' #....................................
#'
#' api_key = 'use_your_planet_nicfi_API_key'
#'
#' mosaic_files = nicfi_mosaics(planet_api_key = api_key,
#'                              type = 'monthly',
#'                              crs_bbox = 4326,
#'                              URL = 'https://api.planet.com/basemaps/v1/mosaics',
#'                              verbose = TRUE)
#'
#' #....................................
#' # keep the mosaic of 'September 2020'
#' #....................................
#'
#' keep_idx = 1
#' mosaic_ID = mosaic_files$dtbl_mosaic$id[keep_idx]
#'
#'
#' #.....................................................
#' # then extract the available Quad files for the Mosaic
#' #.....................................................
#'
#' wkt_file = system.file('data_files/Sugar_Cane_Bolivia.wkt', package = "PlanetNICFI")
#' WKT = readLines(wkt_file, warn = FALSE)
#'
#' quad_files = nicfi_quads_bbox(planet_api_key = api_key,
#'                               mosaic_id = mosaic_ID,
#'                               bbox_AOI = NULL,
#'                               wkt_AOI = WKT,
#'                               page_size = 10,
#'                               crs_bbox = 4326,
#'                               verbose = TRUE)
#'
#' #.............................
#' # download the .png thumbnails  (smaller size for overview)
#' #.............................
#'
#' web_links_aria2c = aria2c_download_paths(mosaic_output = mosaic_files,
#'                                          mosaic_id = mosaic_ID,
#'                                          quads_output = quad_files,
#'                                          img_type = 'thumbnail')
#'
#' #........................
#' # download the .tif files
#' #........................
#'
#' web_links_aria2c = aria2c_download_paths(mosaic_output = mosaic_files,
#'                                          mosaic_id = mosaic_ID,
#'                                          quads_output = quad_files,
#'                                          img_type = 'tif')
#' }


aria2c_download_paths = function(mosaic_output,
                                 mosaic_id,
                                 quads_output,
                                 img_type = 'tif') {

  if (length(mosaic_id) > 1) stop("The 'mosaic_id' parameter must be of length 1", call. = F)
  subs_id = (mosaic_id == mosaic_output$dtbl_mosaic$id)
  if (sum(subs_id) == 0) stop(glue::glue("Make sure that the '{mosaic_id}' appears in the 'id' column of the 'dtbl_mosaic' sublist of the 'mosaic_output' object!"), call. = F)
  idx_id = which(subs_id)

  if (img_type == 'tif') {
    weblinks = quads_output$quads$quad_link_download
  }
  else if (img_type == 'thumbnail') {
    img_type = 'png'
    weblinks = quads_output$quads$quad_link_thumbnail
  }
  else {
    stop("Supported 'img_type' is either 'tif' or 'thumbnail'!", call. = F)
  }

  quad_nams = glue::glue("        out=ID_{quads_output$quads$id_mosaic}_PAGE_{quads_output$quads$id_quad_page}_DATE_{mosaic_output$dtbl_mosaic$first_acquired[idx_id]}.{img_type}")

  path_weblinks = as.vector(unlist(sapply(1:length(weblinks), function(x) {
    c(weblinks[x], quad_nams[x])
  })))

  return(path_weblinks)
}



#' Bulk download of files using 'aria2c'
#'
#' @param vector_or_file_path either a vector of character strings or a valid path to a text file. See the output of the 'aria2c_download_paths()' function for the correct format.
#' @param default_directory a character string specifying a valid path where the files will be saved
#' @param user either NULL or a character string specifying the 'user' (normally this is the 'username' required in specific websites to have access and download files)
#' @param password either NULL or a character string specifying the 'password' (normally this is the 'password' required in specific websites to have access and download files)
#' @param threads an integer value specifying the number of threads to run in parallel
#' @param verbose a boolean. If TRUE then information will be printed out in the console
#' @param secondary_args_aria a character vector specifying the additional parameters that can be passed to the 'aria2c' function. For instance, "--retry-wait": specifies the seconds to wait between retries and "--max-tries=0" means unlimited re-tries. See the References section for more details.
#' @return a character vector based on the verbosity of the function
#'
#' @references
#'
#' https://aria2.github.io/manual/en/html/aria2c.html
#'
#' https://aria2.github.io/manual/en/html/aria2c.html#cmdoption-retry-wait
#'
#' https://aria2.github.io/manual/en/html/aria2c.html#cmdoption-m
#'
#' https://aria2.github.io/manual/en/html/aria2c.html#exit-status
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' require(PlanetNICFI)
#'
#' #....................................
#' # first extract the available Mosaics
#' #....................................
#'
#' api_key = 'use_your_planet_nicfi_API_key'
#'
#' mosaic_files = nicfi_mosaics(planet_api_key = api_key,
#'                              type = 'monthly',
#'                              crs_bbox = 4326,
#'                              URL = 'https://api.planet.com/basemaps/v1/mosaics',
#'                              verbose = TRUE)
#'
#' #....................................
#' # keep the mosaic of 'September 2020'
#' #....................................
#'
#' keep_idx = 1
#' mosaic_ID = mosaic_files$dtbl_mosaic$id[keep_idx]
#'
#'
#' #................................................................
#' # then extract the available Quad files for the Mosaic for an AOI
#' #................................................................
#'
#' wkt_file = system.file('data_files/Sugar_Cane_Bolivia.wkt', package = "PlanetNICFI")
#' WKT = readLines(wkt_file, warn = FALSE)
#'
#' quad_files = nicfi_quads_bbox(planet_api_key = api_key,
#'                               mosaic_id = mosaic_ID,
#'                               bbox_AOI = NULL,
#'                               wkt_AOI = WKT,
#'                               page_size = 10,
#'                               crs_bbox = 4326,
#'                               verbose = TRUE)
#'
#' #..................................
#' # formated aria2c download weblinks
#' #..................................
#'
#' web_links_aria2c = aria2c_download_paths(mosaic_output = mosaic_files,
#'                                          mosaic_id = mosaic_ID,
#'                                          quads_output = quad_files,
#'                                          img_type = 'tif')
#'
#' #.........................................................
#' # download the .tif files that intersect with the bbox AOI
#' #.........................................................
#'
#' temp_dir_out = tempdir()
#'
#' all_threads = parallel::detectCores()
#' set_threads = length(web_links_aria2c) / 2
#' num_threads = ifelse(set_threads < all_threads, set_threads, all_threads)
#' aria_args = '--allow-overwrite --file-allocation=none --retry-wait=5 --max-tries=0'
#'
#' res_downl = aria2c_bulk_donwload(vector_or_file_path = web_links_aria2c,
#'                                  default_directory = temp_dir_out,
#'                                  user = NULL,
#'                                  password = NULL,
#'                                  threads = num_threads,
#'                                  verbose = TRUE,
#'                                  secondary_args_aria = aria_args)
#' }


aria2c_bulk_donwload = function(vector_or_file_path,
                                default_directory,
                                user = NULL,
                                password = NULL,
                                threads = 1,
                                verbose = FALSE,
                                secondary_args_aria = '--allow-overwrite --retry-wait=5 --max-tries=0') {

  if (verbose) t_start = proc.time()

  flag_vec = F
  if (inherits(vector_or_file_path, "character")) {
    if (all(!file.exists(vector_or_file_path))) {                  # input data in this case must be a character vector of strings ( see the examples section for more info ) [ use 'all()' because the character vector will be of length > 1 ]
      if (verbose) cat("The input is not a valid path to a file but rather a vector of character strings!\n")
      flag_vec = T
      tmp_path_save = tempfile(fileext = '.txt')
      fileConn = file(tmp_path_save)
      writeLines(vector_or_file_path, fileConn, sep = "\n")
      close(fileConn)
      vector_or_file_path = tmp_path_save                          # overwrite the initial name of the temporary file
    }
  }
  else {
    stop("The 'vector_or_file_path' parameter must be either a character string (valid path to a file) OR a character vector of strings that will be saved to a text file!", call. = F)
  }

  ARGS = c(paste0('-j', threads),
           secondary_args_aria,
           paste0('-d ', default_directory),
           paste0('--input-file=', vector_or_file_path))

  if (!is.null(user)) {
    ARGS = append(ARGS, paste0('--http-user=', user), after = 1)
  }
  if (!is.null(password)) {
    ARGS = append(ARGS, paste0('--http-passwd=', password), after = 2)
  }

  verbose_dat = system2(command = "aria2c",
                        args = ARGS,
                        stdout = verbose,
                        stderr = verbose)

  if (flag_vec) {
    if (file.exists(vector_or_file_path)) file.remove(vector_or_file_path)           # remove temporary created file
    if (verbose) cat("The temporary created ", vector_or_file_path, " file was removed!\n")
  }

  if (verbose) compute_elapsed_time(t_start)

  return(verbose_dat)
}



#' Download the Planet NICFI images sequentially
#'
#' @param aria2c_file_paths a vector of character strings. See the output of the 'aria2c_download_paths()' function for the correct format.
#' @param default_directory a character string specifying a valid path where the files will be saved
#' @param download_method a character string specifying the download method. Can be for instance "wget", "curl" or any available method of the "download.file()" function
#' @param verbosity an integer specifying the verbosity (between 0 and 2). If 0 then verbosity is disabled, if 1 then only essential verbosity is displayed and if 2 then all available information will be printed out in the console.
#' @return it doesn't return an R object but it saves the files to a directory
#'
#' @importFrom utils flush.console download.file
#'
#' @details
#'
#' This function does not require the 'aria2c' tool (system requirement) to download the imagery. It uses the 'utils::download.file()' function internally
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' require(PlanetNICFI)
#'
#' #....................................
#' # first extract the available Mosaics
#' #....................................
#'
#' api_key = 'use_your_planet_nicfi_API_key'
#'
#' mosaic_files = nicfi_mosaics(planet_api_key = api_key,
#'                              type = 'monthly',
#'                              crs_bbox = 4326,
#'                              URL = 'https://api.planet.com/basemaps/v1/mosaics',
#'                              verbose = TRUE)
#'
#' #....................................
#' # keep the mosaic of 'September 2020'
#' #....................................
#'
#' keep_idx = 1
#' mosaic_ID = mosaic_files$dtbl_mosaic$id[keep_idx]
#'
#'
#' #.....................................................
#' # then extract the available Quad files for the Mosaic
#' #.....................................................
#'
#' wkt_file = system.file('data_files/Sugar_Cane_Bolivia.wkt', package = "PlanetNICFI")
#' WKT = readLines(wkt_file, warn = FALSE)
#'
#' quad_files = nicfi_quads_bbox(planet_api_key = api_key,
#'                               mosaic_id = mosaic_ID,
#'                               bbox_AOI = NULL,
#'                               wkt_AOI = WKT,
#'                               page_size = 10,
#'                               crs_bbox = 4326,
#'                               verbose = TRUE)
#' #........................
#' # download the .tif files
#' #........................
#'
#' web_links_aria2c = aria2c_download_paths(mosaic_output = mosaic_files,
#'                                          mosaic_id = mosaic_ID,
#'                                          quads_output = quad_files,
#'                                          img_type = 'tif')
#'
#' DIR_SAVE = tempdir(check = FALSE)
#' print(DIR_SAVE)
#'
#' res_dat = sequential_download_paths(aria2c_file_paths = web_links_aria2c,
#'                                     default_directory = DIR_SAVE,
#'                                     download_method = 'wget',
#'                                     verbosity = 1)
#' }

sequential_download_paths = function(aria2c_file_paths,
                                     default_directory,
                                     download_method = 'wget',
                                     verbosity = 0) {

  if (verbosity > 0) t_start = proc.time()
  if (!verbosity %in% 0:2) stop("The 'verbosity' parameter must be one of 0, 1 or 2", call. = F)
  SEQ = seq(from = 1, to = length(aria2c_file_paths), by = 2)
  count_seq = 1
  LEN = length(SEQ)

  for (item in SEQ) {

    if (verbosity > 0) {
      message("File: ", count_seq, "/", LEN, " will be downloaded ...\r", appendLF = FALSE)
      utils::flush.console()
    }

    URL_LINK = aria2c_file_paths[item]
    DOWNLOAD_PATH = trimws(x = aria2c_file_paths[item+1], which = 'both')
    DOWNLOAD_PATH = gsub('out=', '', DOWNLOAD_PATH)
    DOWNLOAD_FULL_PATH = file.path(default_directory, DOWNLOAD_PATH)

    utils::download.file(url = URL_LINK,
                         destfile = DOWNLOAD_FULL_PATH,
                         method = download_method,
                         quiet = ifelse(verbosity == 2, FALSE, TRUE))

    count_seq = count_seq + 1
  }

  if (verbosity > 0) compute_elapsed_time(t_start)
}



#' Create a Virtual Raster (VRT) file from the .tif files
#'
#' @param dir_tifs a valid path to a directory where the .tif files are saved
#' @param output_path_VRT a valid path to a file where the Virtual Raster (VRT) will be saved
#' @param file_extension a character string specifying the image file extension from which the .vrt file will be built
#' @param verbose a boolean. If TRUE then information will be printed out in the console
#' @return it doesn't return an object but it saves the output to a file
#'
#' @importFrom glue glue
#' @importFrom sf gdal_utils
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' require(PlanetNICFI)
#'
#' #....................................
#' # first extract the available Mosaics
#' #....................................
#'
#' api_key = 'use_your_planet_nicfi_API_key'
#'
#' mosaic_files = nicfi_mosaics(planet_api_key = api_key,
#'                              type = 'monthly',
#'                              crs_bbox = 4326,
#'                              URL = 'https://api.planet.com/basemaps/v1/mosaics',
#'                              verbose = TRUE)
#'
#' #....................................
#' # keep the mosaic of 'September 2020'
#' #....................................
#'
#' keep_idx = 1
#' mosaic_ID = mosaic_files$dtbl_mosaic$id[keep_idx]
#'
#'
#' #................................................................
#' # then extract the available Quad files for the Mosaic for an AOI
#' #................................................................
#'
#' wkt_file = system.file('data_files/Sugar_Cane_Bolivia.wkt', package = "PlanetNICFI")
#' WKT = readLines(wkt_file, warn = FALSE)
#'
#' quad_files = nicfi_quads_bbox(planet_api_key = api_key,
#'                               mosaic_id = mosaic_ID,
#'                               bbox_AOI = NULL,
#'                               wkt_AOI = WKT,
#'                               page_size = 10,
#'                               crs_bbox = 4326,
#'                               verbose = TRUE)
#'
#' #..................................
#' # formated aria2c download weblinks
#' #..................................
#'
#' web_links_aria2c = aria2c_download_paths(mosaic_output = mosaic_files,
#'                                          mosaic_id = mosaic_ID,
#'                                          quads_output = quad_files,
#'                                          img_type = 'tif')
#'
#' #.........................................................
#' # download the .tif files that intersect with the bbox AOI
#' #.........................................................
#'
#' temp_dir_out = tempdir()
#'
#' all_threads = parallel::detectCores()
#' set_threads = length(web_links_aria2c) / 2
#' num_threads = ifelse(set_threads < all_threads, set_threads, all_threads)
#' aria_args = '--allow-overwrite --file-allocation=none --retry-wait=5 --max-tries=0'
#'
#' res_downl = aria2c_bulk_donwload(vector_or_file_path = web_links_aria2c,
#'                                  default_directory = temp_dir_out,
#'                                  user = NULL,
#'                                  password = NULL,
#'                                  threads = num_threads,
#'                                  verbose = TRUE,
#'                                  secondary_args_aria = aria_args)
#'
#' #........................................
#' # create a Virtual Raster (VRT) file from
#' # the downloaded .tif files
#' #........................................
#'
#' VRT_out = file.path(temp_dir_out, glue::glue("{mosaic_ID}.vrt"))
#'
#' res_vrt = create_VRT_from_dir(dir_tifs = temp_dir_out,
#'                               output_path_VRT = VRT_out,
#'                               file_extension = '.tif',
#'                               verbose = TRUE)
#'
#' #......................................................
#' # load the saved VRT file as raster (which might
#' # consist of multiple files, i.e. a mosaic) and plot it
#' #......................................................
#'
#' rst = terra::rast(VRT_out)
#' sp::plot(rst, axes = F, legend = F)
#'
#' }


create_VRT_from_dir = function(dir_tifs,
                               output_path_VRT,
                               file_extension = '.tif',
                               verbose = FALSE) {

  if (verbose) t_start = proc.time()
  lst_vrt = list.files(dir_tifs, pattern = file_extension, full.names = T)
  if (length(lst_vrt) == 0) stop(glue::glue("The directory '{dir_tifs}' does not include any files of extension '{file_extension}'!"), call. = F)

  if (verbose) cat(glue::glue("The VRT Mosaic will be built from  {length(lst_vrt)}  '{file_extension}' files and will be saved in  '{output_path_VRT}' ..."), '\n')
  vrt_mosaic = sf::gdal_utils(util = 'buildvrt',
                              source = lst_vrt,
                              destination = output_path_VRT,
                              quiet = !verbose)

  if (verbose) compute_elapsed_time(t_start)
}



#' Crop the downloaded NICFI .tif or .vrt file using 'gdalwarp'
#'
#' @param input_pth a valid path to either a .tif or a .vrt (virtual raster) file that should be cropped based on the bounding box using 'gdalwarp'
#' @param output_pth a valid path to the output .tif file. This file path can also point to a .vrt file by setting the 'of' parameter to 'VRT'
#' @param bbox_AOI a list of the format "list(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)" that includes the bounding box 'xmin', 'xmax', 'ymin', 'ymax' coordinate values of the Area of Interest (AOI)
#' @param threads an integer. In case that this parameter is greater than 1 then multiple threads will be utilized in the 'gdalwarp' function
#' @param of a character string specifying the format of the saved file. The default is GeoTIFF (GTiff). For more information see the 'gdal_utils' function of the 'sf' package
#' @param resize_method a character string specifying the resize method. Can be one of 'near', 'bilinear', 'cubic', 'cubicspline', 'lanczos', 'average', 'mode', 'max', 'min', 'med', 'q1', 'q3'. For more information see the 'r' parameter of  the 'gdal_utils' function of the 'sf' package
#' @param verbose a boolean. If TRUE then information will be printed out in the console
#' @return a logical indicating success (i.e., TRUE); in case of failure, an error is raised
#'
#' @export
#'
#' @importFrom sf gdal_utils
#' @importFrom glue glue
#'
#' @examples
#'
#' \dontrun{
#'
#' require(PlanetNICFI)
#'
#' #....................................
#' # first extract the available Mosaics
#' #....................................
#'
#' api_key = 'use_your_planet_nicfi_API_key'
#'
#' mosaic_files = nicfi_mosaics(planet_api_key = api_key,
#'                              type = 'monthly',
#'                              crs_bbox = 4326,
#'                              URL = 'https://api.planet.com/basemaps/v1/mosaics',
#'                              verbose = TRUE)
#'
#' #....................................
#' # keep the mosaic of 'September 2020'
#' #....................................
#'
#' keep_idx = 1
#' mosaic_ID = mosaic_files$dtbl_mosaic$id[keep_idx]
#'
#'
#' #................................................................
#' # then extract the available Quad files for the Mosaic for an AOI
#' #................................................................
#'
#' wkt_file = system.file('data_files/Sugar_Cane_Bolivia.wkt', package = "PlanetNICFI")
#' WKT = readLines(wkt_file, warn = FALSE)
#'
#' quad_files = nicfi_quads_bbox(planet_api_key = api_key,
#'                               mosaic_id = mosaic_ID,
#'                               bbox_AOI = NULL,
#'                               wkt_AOI = WKT,
#'                               page_size = 10,
#'                               crs_bbox = 4326,
#'                               verbose = TRUE)
#'
#' #..................................
#' # formated aria2c download weblinks
#' #..................................
#'
#' web_links_aria2c = aria2c_download_paths(mosaic_output = mosaic_files,
#'                                          mosaic_id = mosaic_ID,
#'                                          quads_output = quad_files,
#'                                          img_type = 'tif')
#'
#' #.........................................................
#' # download the .tif files that intersect with the bbox AOI
#' #.........................................................
#'
#' temp_dir_out = tempdir()
#'
#' all_threads = parallel::detectCores()
#' set_threads = length(web_links_aria2c) / 2
#' num_threads = ifelse(set_threads < all_threads, set_threads, all_threads)
#' aria_args = '--allow-overwrite --file-allocation=none --retry-wait=5 --max-tries=0'
#'
#' res_downl = aria2c_bulk_donwload(vector_or_file_path = web_links_aria2c,
#'                                  default_directory = temp_dir_out,
#'                                  user = NULL,
#'                                  password = NULL,
#'                                  threads = num_threads,
#'                                  verbose = TRUE,
#'                                  secondary_args_aria = aria_args)
#'
#' #........................................
#' # create a Virtual Raster (VRT) file from
#' # the downloaded .tif files
#' #........................................
#'
#' VRT_out = file.path(temp_dir_out, glue::glue("{mosaic_ID}.vrt"))
#'
#' res_vrt = create_VRT_from_dir(dir_tifs = temp_dir_out,
#'                               output_path_VRT = VRT_out,
#'                               file_extension = '.tif',
#'                               verbose = TRUE)
#'
#' #....................................................
#' # Adjust the Coordinate Reference System of the
#' # bounding box from 4326 to the one of the .tif files
#' #....................................................
#'
#' wkt_sf = sf::st_as_sfc(WKT, crs = 4326)
#' proj_info = proj_info_extract(path_to_raster = VRT_out)
#'
#' wkt_transf = sf::st_transform(wkt_sf, crs = proj_info)
#' bbx_transf = sf::st_bbox(wkt_transf)
#'
#'
#' #....................................................
#' # crop the output .vrt file based on the bounding box
#' #....................................................
#'
#' pth_crop_out = file.path(temp_dir_out, glue::glue("{mosaic_ID}_CROPPED.tif"))
#'
#' bbx_crop = list(xmin = as.numeric(bbx_transf['xmin']),
#'                 xmax = as.numeric(bbx_transf['xmax']),
#'                 ymin = as.numeric(bbx_transf['ymin']),
#'                 ymax = as.numeric(bbx_transf['ymax']))
#'
#' warp_obj = nicfi_crop_images(input_pth = VRT_out,
#'                              output_pth = pth_crop_out,
#'                              bbox_AOI = bbx_crop,
#'                              threads = num_threads,
#'                              of = 'GTiff',
#'                              resize_method = 'lanczos',
#'                              verbose = TRUE)
#' }


nicfi_crop_images = function(input_pth,
                             output_pth,
                             bbox_AOI,
                             threads = 1,
                             of = 'GTiff',
                             resize_method = 'lanczos',
                             verbose = FALSE) {

  if (verbose) t_start = proc.time()

  vec_options = c("-te", bbox_AOI$xmin, bbox_AOI$ymin, bbox_AOI$xmax, bbox_AOI$ymax,
                  "-r", resize_method,
                  "-of", of)

  if (threads > 1) {
    vec_options = c(vec_options, "-multi", "-wo", as.character(glue::glue("NUM_THREADS={threads}")))
  }

  res_warp = sf::gdal_utils(util = 'warp',
                            source = input_pth,
                            destination = output_pth,
                            options = vec_options,
                            quiet = !verbose)

  if (verbose) compute_elapsed_time(t_start)
  return(res_warp)
}

