# Terra/raster compatibility helpers.
#
# AutoSHUD is migrating raster I/O and geometry ops to `terra`, while `rSHUD`
# still expects `raster::Raster*` objects in several APIs. Use these helpers to
# keep the pipeline readable and make conversions explicit at call sites.

read_rast <- function(f, ...) {
  terra::rast(f, ...)
}

as_raster <- function(x, ...) {
  if (inherits(x, "Raster")) {
    return(x)
  }
  if (!inherits(x, "SpatRaster")) {
    stop("as_raster() expects a terra::SpatRaster or raster::Raster* object.")
  }
  if (!requireNamespace("raster", quietly = TRUE)) {
    stop("Package 'raster' is required for rSHUD compatibility.")
  }
  raster::raster(x, ...)
}

rast_crs <- function(x) {
  as.character(terra::crs(x))
}

rast_res <- function(x) {
  terra::res(x)
}

rast_ext <- function(x) {
  terra::ext(x)
}

