
raster2Polygon <- function(rx){
  ext= terra::ext(rx)
  res =terra::res(rx)
  xx = seq(ext[1], ext[2], res[1])
  yy = seq(ext[3], ext[4], res[2])
  spx = rSHUD::fishnet(xx=xx, yy=yy, crs=terra::crs(rx))
  return(spx)
}
