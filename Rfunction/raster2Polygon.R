
raster2Polygon <- function(rx){
  ext= terra::ext(rx)
  res =terra::res(rx)
  xx = seq(ext[1], ext[2], res[1])
  yy = seq(ext[3], ext[4], res[2])
  crs_val = terra::crs(rx)
  if (!nzchar(crs_val)) crs_val = 4326
  spx = rSHUD::fishnet(xx=xx, yy=yy, crs=crs_val)
  return(spx)
}
