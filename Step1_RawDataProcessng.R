# Task: Load the rawdata and standardize the data.
# 加载原始数据，对原始数据进行初步处理
# 1. 读取建模参数加载必要工具库。
# 2. 处理建模边界，包括：去孔洞、投影、生成缓冲区等。
# 3. 处理DEM数据，包括：剪切(从原始数据中提取研究区范围内数据)，栅格数据重投影为PCS。
# 4. 河网数据处理，包括：数据重投影、河道线段简化、去除重复点、去除重复线段、河流流向一致性检查/修复等。
# 5. 湖泊数据处理，包括：数据重投影、去除孔洞、边界简化等
# 6. 绘图，对以上数据处理过程中的原始空间数据和结果数据进行绘图，方便用户检查和确认数据对象和处理结果。

# Notice:
# 1. The DEM data is merged before this step. The country-wide DEM should be merged.
# 2. Te wbd and stm data must be ready before this step.

rm(list=ls())
source('GetReady.R')
prefix ='S1'
# ================= Boundary =================
wbd0 = read_sf_as_sp(xfg$fsp.wbd)  # Read data
wbd0 = buffer_sp(wbd0, dist = 0) # Remove error from irregular polygon.
# ---- disolve ----
wbd.dis = removeholes(union_sp(wbd0))

# wbd in pcs
wb.p = transform_sp(wbd0, xfg$crs.pcs) 
writeshape(wb.p, pd.pcs$wbd)

# buffer of wbd in pcs
buf.p = buffer_sp(wb.p, dist = xfg$para$DistBuffer) 
writeshape(buf.p, pd.pcs$wbd.buf)

buf.g = transform_sp(buf.p, xfg$crs.gcs)
writeshape(buf.g, pd.gcs$wbd.buf)

wb.g = transform_sp(wb.p, xfg$crs.gcs)
writeshape(wb.g, pd.gcs$wbd)


# ================= DEM =================
if(!file.exists(xfg$fr.dem)){
  source('Rfunction/getDEM.R')
  # debug(getDEM_ASTER)
  fn.dem.tmp = getDEM_ASTER(fn.wbd = pd.gcs$wbd.buf,
                            dir.fig = xfg$dir$fig,
                            dir.out = xfg$dir$out,
                            fn.out = xfg$fr.dem,
                            crop=TRUE)
}

dem0=terra::rast(xfg$fr.dem)
# -------CROP DEM -----------------
# Crop the dem AND conver the dem to PCS.
fun.gdalwarp(f1=xfg$fr.dem, f2=pd.pcs$dem, t_srs = xfg$crs.pcs, s_srs = terra::crs(dem0), 
             opt = paste0('-cutline ', pd.pcs$wbd.buf) )
# Crop the dem, output is in GCS
fun.gdalwarp(f1=xfg$fr.dem, f2=pd.gcs$dem, t_srs = xfg$crs.gcs, s_srs = terra::crs(dem0), 
             opt = paste0('-cutline ', pd.pcs$wbd.buf) )

# =========Stream Network===========================
stm0 = read_sf_as_sp(xfg$fsp.stm)  # data 0: raw data
stm1 = transform_sp(stm0, xfg$crs.pcs)  # data 1: PCS
fun.simplifyRiver <- function(rmDUP=TRUE){
  riv.xy = extractCoords(stm1)
  npoint = nrow(riv.xy)
  mlen = length_sp(stm1) / npoint
  r.dem = terra::rast(pd.pcs$dem)
  dx = mean(terra::res(r.dem))
  if( mlen < dx){
    stm1 = simplify_sp(stm1, tol = dx)
  }
  if(rmDUP){
    res = rmDuplicatedLines(stm1)
  }else{
    res = stm1
  }
  res
}
# debug(sp.RiverDown)
if(xfg$para$flowpath){
  stm1 = fun.simplifyRiver(rmDUP = FALSE)
  stm.p= sp.RiverPath(stm1, tol.simplify = 30)$sp  # clean data with flowpath.
  stm.p = stm1
}else{
  stm.p = stm1
}

writeshape(stm.p, file=pd.pcs$stm)

#' ==========================================
if(LAKEON){
  spl0 = read_sf_as_sp(xfg$fsp.lake)  # data 0: raw data
  spl1 = removeholes(spl0)
  spl.gcs = transform_sp(spl1, xfg$crs.gcs)
  writeshape(spl.gcs, pd.gcs$lake)
  
  spl.pcs = transform_sp(spl.gcs, xfg$crs.pcs)  # data 1: PCS
  writeshape(spl.pcs, pd.pcs$lake)
}

#' ==== PLOT FIGURE ================
dem.p = terra::rast(pd.pcs$dem)
png(filename = file.path(xfg$dir$fig, paste0(prefix, '_Rawdata_Elevation.png')), type='cairo', 
    width = 7, height=7, res=300, unit='in')
terra::plot(dem.p)
plot(wb.p, add=T, border=2)
if(LAKEON){
  plot(spl.pcs, add=TRUE, border='darkblue', lwd=1.5)
}
plot(stm.p, add=T, col=4)
dev.off()
