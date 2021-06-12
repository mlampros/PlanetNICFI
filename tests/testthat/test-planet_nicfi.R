
#---------------------------------------------------------------------------------------  data
path_mosaics = file.path(getwd(), "mosaics.RDS")
path_quads = file.path(getwd(), "quads.RDS")
mosaics = readRDS(path_mosaics)
quads = readRDS(path_quads)
mosaic_id = "755805d4-8eba-4a61-a6ab-7514c1bde810"
#---------------------------------------------------------------------------------------


context('tests only for the aria2c_download_paths() function')


testthat::test_that("the 'aria2c_download_paths()' function returns the correct output", {

  res_tes = aria2c_download_paths(mosaic_output = mosaics,
                                  mosaic_id = mosaic_id,
                                  quads_output = quads,
                                  img_type = 'tif')

  testthat::expect_true( length(res_tes) == 4 & inherits(res_tes, 'character') )
})


