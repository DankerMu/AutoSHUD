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

# rSHUD compatibility: avoid retired rgeos dependency
# rSHUD::getArea() calls rgeos internally; provide a mesh-based implementation.
getArea <- function(pm = readmesh(), ...) {
  if (inherits(pm, "Spatial")) {
    return(area_sp(pm, byid = TRUE))
  }
  if (!isS4(pm) || !all(c("mesh", "point") %in% methods::slotNames(pm))) {
    stop("getArea() expects an rSHUD mesh (S4 with slots 'mesh' and 'point') or a Spatial* object.")
  }

  tt <- pm@mesh[, 2:4, drop = FALSE]
  pp <- pm@point[, 2:3, drop = FALSE]
  tt <- apply(tt, 2, as.integer)

  x1 <- pp[tt[, 1], 1]
  y1 <- pp[tt[, 1], 2]
  x2 <- pp[tt[, 2], 1]
  y2 <- pp[tt[, 2], 2]
  x3 <- pp[tt[, 3], 1]
  y3 <- pp[tt[, 3], 2]

  abs((x1 * y2 + x2 * y3 + x3 * y1 - x1 * y3 - x2 * y1 - x3 * y2) / 2)
}

# rSHUD compatibility: avoid retired rgeos dependency
# rSHUD::sp.CutSptialLines() calls rgeos::gLength(); provide a `sp`-only implementation.
sp.CutSptialLines <- function(sl, tol) {
  msg = "sp.CutSptialLines::"
  ll = sp::SpatialLinesLengths(sl)
  if (all(ll < tol)) {
    return(sl)
  }

  nsp = length(sl)
  xsl = list()
  ik = 1
  for (i in 1:nsp) {
    sx = sl[i, ]
    pxy = extractCoords(sx, unique = TRUE)
    np = nrow(pxy)
    dseg = sp::LineLength(pxy, sum = FALSE)
    dacc = cumsum(dseg)
    tol = max(c(tol, min(dacc)))
    len = sum(dseg)
    if (len > tol) {
      nsplit = ceiling(len/tol)
    }
    else {
      nsplit = 1
    }
    dd = len/nsplit
    v0 = 1
    message(msg, i, "/", nsp, "\t", nsplit, "\t", round(dd, 2))
    for (k in 1:nsplit) {
      if (v0 >= np) {
        break
      }
      dk = dd * k
      v1 = order(abs(dacc - dk), decreasing = FALSE)[1] + 1
      if (v1 + 1 > np) {
        v1 = np
      }
      message(msg, v0, "\t", v1)
      if (v0 == v1) {
        next
      }
      xsl[[ik]] = sp::Lines(sp::Line(pxy[c(v0:v1), ]), ID = ik)
      ik = ik + 1
      v0 = v1
    }
  }

  tmp = sp::SpatialLines(xsl, proj4string = raster::crs(sl))
  ilen = sp::SpatialLinesLengths(tmp)
  att = data.frame(INDEX = 1:length(tmp), Length = ilen)
  sp::SpatialLinesDataFrame(tmp, data = att)
}

# rSHUD compatibility: ensure legacy Spatial* return types when scripts expect `@data`
ForcingCoverage <- function(...) {
  out <- rSHUD::ForcingCoverage(...)
  if (inherits(out, "sf")) {
    return(methods::as(out, "Spatial"))
  }
  if (inherits(out, "SpatVector")) {
    return(methods::as(sf::st_as_sf(out), "Spatial"))
  }
  out
}

# rSHUD compatibility: avoid retired rgeos dependency
# rSHUD::shud.river() calls rgeos::gLength(); provide a `sp`-only implementation.
shud.river <- function(sl, dem, rivord = NULL, rivdown = NULL, AREA = NULL) {
  msg = "shud.river::"
  sp.slt = sl
  nsp = length(sp.slt)
  xy = data.frame(extractCoords(sp.slt))
  if (is.null(rivord)) {
    message(msg, "\nCalculate river order ...")
    rivord = sp.RiverOrder(sp.slt)
  }
  if (is.null(rivdown)) {
    message(msg, "\nIdentify the downstream ...")
    rivdown = sp.RiverDown(sp.slt)
  }
  message(msg, "\nFrom/To nodes ...")
  ft = rbind(FromToNode(sp.slt, simplify = TRUE)[, 2:3])
  message(msg, "\nSlope and length of river ...")
  zf = raster::extract(dem, xy[ft[, 1], ])
  zt = raster::extract(dem, xy[ft[, 2], ])
  len = sp::SpatialLinesLengths(sp.slt)
  slp = (zf - zt)/len
  row.names(sp.slt) = paste(1:nsp)
  df = data.frame(Index = 1:nsp, Down = rivdown, Type = rivord, Slope = slp,
                  Length = len, BC = len * 0)
  rownames(df) = row.names(sp.slt)
  p.df = data.frame(xy[ft[, 1], ], zf, xy[ft[, 2], ], zt)
  colnames(p.df) = c("From.x", "From.y", "From.z", "To.x", "To.y", "To.z")
  rownames(p.df) = row.names(sp.slt)

  ntype = max(rivord)
  if (is.null(AREA)) {
    rtype = RiverType(ntype)
  }
  else {
    fx = function(a, n = 10) {
      dd = (1/(1:n))^0.8
      rev(8 * log10(a + 1) * a^0.25 * dd)
    }
    wd = round(fx(AREA, ntype), 2)
    rtype = RiverType(ntype, width = wd)
  }
  SHUD.RIVER(river = df, rivertype = data.frame(rtype), point = p.df)
}

# rSHUD compatibility: avoid retired rgeos dependency
# rSHUD::FromToNode(), sp.RiverOrder(), sp.RiverDown() use rgeos::gSimplify().
FromToNode <- function(sp, coord = extractCoords(sp, unique = TRUE), simplify = TRUE) {
  # Compute from/to node IDs from line endpoints (fast), keeping IDs aligned with `coord`.
  spl = methods::as(sp, "SpatialLines")
  nsp = length(spl)
  fr_xy = matrix(NA_real_, nrow = nsp, ncol = 2)
  to_xy = matrix(NA_real_, nrow = nsp, ncol = 2)
  for (i in 1:nsp) {
    parts = spl@lines[[i]]@Lines
    first = parts[[1]]@coords[1, 1:2]
    last_part = parts[[length(parts)]]
    last = last_part@coords[nrow(last_part@coords), 1:2]
    fr_xy[i, ] = first
    to_xy[i, ] = last
  }

  coord_r = round(coord, 8)
  fr_r = round(fr_xy, 8)
  to_r = round(to_xy, 8)
  key = function(xy) paste(xy[, 1], xy[, 2], sep = "_")
  coord_key = key(coord_r)
  fr_id = match(key(fr_r), coord_key)
  to_id = match(key(to_r), coord_key)

  if (anyNA(fr_id)) {
    miss = which(is.na(fr_id))
    for (i in miss) {
      fr_id[i] = which(rowMatch(fr_r[i, ], coord_r))[1]
    }
  }
  if (anyNA(to_id)) {
    miss = which(is.na(to_id))
    for (i in miss) {
      to_id[i] = which(rowMatch(to_r[i, ], coord_r))[1]
    }
  }

  frto = cbind(1:nsp, fr_id, to_id)
  colnames(frto) = c("ID", "FrNode", "ToNode")
  rbind(frto)
}

sp.RiverOrder <- function(sp, coord = extractCoords(sp)) {
  msg = "sp.RiverOrder::"
  get1st <- function(x) {
    fr = x[, 2]
    to = x[, 3]
    tb = table(x[, 2:3])
    pid = as.numeric(names(tb))
    ncount = as.numeric(tb)
    p.out = to[which(!to %in% fr)]
    p.jnt = pid[which(ncount > 2)]
    p.key = c(p.out, p.jnt)
    tid = fr[!fr %in% to]
    nd = length(tid)
    ret = NULL
    for (i in 1:nd) {
      cr = tid[i]
      for (j in 1:1e+05) {
        rid = which(fr %in% cr)
        ret = c(ret, rid)
        sid = to[rid]
        if (any(sid %in% p.key)) {
          break
        }
        else {
          cr = sid
        }
      }
    }
    ret
  }

  ft0 = FromToNode(sp, coord, simplify = FALSE)
  ft = rbind(unique(ft0[, 2:3]))
  if (length(sp) != nrow(ft)) {
    message(msg, "ERROR: duplicated river reches extis.")
    stop("STOP WITH ERROR")
  }
  x = cbind(1:length(sp), ft)
  y = x
  x.ord = x[, 1] * 0
  for (i in 1:10000) {
    ids = y[, 1]
    message(msg, "Order = ", i)
    id = get1st(y)
    x.ord[ids[id]] = i
    y = rbind(y[-1 * id, ])
    ny = length(y)
    if (ny <= 0) {
      break
    }
  }
  x.ord
}

sp.RiverDown <- function(sp, coord = extractCoords(sp)) {
  msg = "sp.RiverDown::"
  ft = rbind(FromToNode(sp, coord = coord, simplify = FALSE)[, 2:3])
  nsp = length(sp)
  idown = rep(-3, nsp)
  for (i in 1:nsp) {
    pto = ft[i, 2]
    id = which(ft[, 1] == pto)
    nid = length(id)
    if (nid == 1) {
      idown[i] = id
    }
    else if (length(id) > 1) {
      message(msg, i, "/", nsp, "\t", nid, " Downstream found")
      print(id)
      idown[i] = id[1]
    }
    else {
    }
  }
  idown
}

# rSHUD compatibility: avoid retired rgeos dependency
# rSHUD::shud.rivseg() calls rgeos::gLength(); provide a `sp`-only implementation.
shud.rivseg <- function(sl) {
  data.frame(Index = 1:length(sl), sl@data, Length = sp::SpatialLinesLengths(sl))
}
