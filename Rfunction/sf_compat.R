# Thin sf wrappers to replace sp/rgdal/rgeos runtime usage.
# All wrappers return Spatial* objects for backward compatibility.

.as_sf_safe <- function(x) {
  if (inherits(x, "sf")) {
    return(x)
  }
  if (inherits(x, "sfc")) {
    return(sf::st_sf(geometry = x))
  }
  sf::st_as_sf(x)
}

.as_sp_safe <- function(x) {
  methods::as(x, "Spatial")
}

.crs_to_sf <- function(crs) {
  # Accept sf::crs, EPSG integer, WKT/PROJ string, or sp::CRS.
  out <- tryCatch(sf::st_crs(crs), error = function(e) NULL)
  if (!is.null(out)) {
    return(out)
  }
  sf::st_crs(as.character(crs))
}

read_sf_as_sp <- function(dsn, ...) {
  dots <- list(...)
  if (!"quiet" %in% names(dots)) {
    dots$quiet <- TRUE
  }
  x_sf <- do.call(sf::st_read, c(list(dsn = dsn), dots))
  # Drop empty geometries — sp classes cannot represent them
  empty <- sf::st_is_empty(x_sf)
  if (any(empty)) {
    message("read_sf_as_sp: dropping ", sum(empty), " empty geometries from ", dsn)
    x_sf <- x_sf[!empty, ]
  }
  .as_sp_safe(x_sf)
}

transform_sp <- function(x, crs) {
  x_sf <- .as_sf_safe(x)
  x_tr <- sf::st_transform(x_sf, .crs_to_sf(crs))
  .as_sp_safe(x_tr)
}

buffer_sp <- function(x, dist) {
  x_sf <- .as_sf_safe(x)
  geom <- sf::st_union(sf::st_geometry(x_sf))
  out <- tryCatch(
    sf::st_buffer(geom, dist = dist),
    error = function(e) {
      if (isTRUE(exists("st_make_valid", where = asNamespace("sf"), inherits = FALSE))) {
        geom2 <- sf::st_make_valid(geom)
        return(sf::st_buffer(geom2, dist = dist))
      }
      stop(e)
    }
  )
  .as_sp_safe(out)
}

union_sp <- function(x) {
  x_sf <- .as_sf_safe(x)
  out <- sf::st_union(sf::st_geometry(x_sf))
  .as_sp_safe(out)
}

simplify_sp <- function(x, tol) {
  x_sf <- .as_sf_safe(x)
  out <- sf::st_simplify(x_sf, dTolerance = tol, preserveTopology = TRUE)
  .as_sp_safe(out)
}

area_sp <- function(x, byid = FALSE) {
  x_sf <- .as_sf_safe(x)
  a <- as.numeric(sf::st_area(sf::st_geometry(x_sf)))
  if (byid) a else sum(a)
}

length_sp <- function(x, byid = FALSE) {
  x_sf <- .as_sf_safe(x)
  len <- as.numeric(sf::st_length(sf::st_geometry(x_sf)))
  if (byid) len else sum(len)
}

centroid_sp <- function(x, byid = TRUE) {
  x_sf <- .as_sf_safe(x)
  geom <- sf::st_geometry(x_sf)
  if (!byid) {
    geom <- sf::st_union(geom)
  }
  out <- sf::st_centroid(geom)
  .as_sp_safe(out)
}

intersects_sp <- function(x, y, byid = TRUE) {
  x_sf <- .as_sf_safe(x)
  y_sf <- .as_sf_safe(y)
  if (byid) {
    return(sf::st_intersects(x_sf, y_sf, sparse = TRUE))
  }
  which(sf::st_intersects(x_sf, y_sf, sparse = FALSE), arr.ind = TRUE)
}

