

#---------------------------------------------------------------------------------------  data
path_mosaics = file.path(getwd(), "mosaics.RDS")
path_quads = file.path(getwd(), "quads.RDS")
mosaics = readRDS(path_mosaics)
quads = readRDS(path_quads)
mosaic_id = "755805d4-8eba-4a61-a6ab-7514c1bde810"
pth_vrt = system.file('data_files/virt_rast.vrt', package = "PlanetNICFI")
#---------------------------------------------------------------------------------------


context("tests for the 'aria2c_download_paths' and 'proj_info_extract' functions")


testthat::test_that("the 'aria2c_download_paths()' function returns the correct output", {

  res_tes = aria2c_download_paths(mosaic_output = mosaics,
                                  mosaic_id = mosaic_id,
                                  quads_output = quads,
                                  img_type = 'tif')

  testthat::expect_true( length(res_tes) == 4 & inherits(res_tes, 'character') )
})



testthat::test_that("the 'proj_info_extract()' function is of type character and it starts with the term '+proj'", {

  proj_info = proj_info_extract(path_to_raster = pth_vrt)

  testthat::expect_true( inherits(proj_info, 'character') & gregexpr('^[+proj]', proj_info) != -1 )
})
