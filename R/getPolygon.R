getPolygon <- function(x, index){
  p1 <- x[index, 1]
  p2 <- gsub(" ", ",", strsplit(strsplit(p1, "POLYGON((", fixed = TRUE)[[1]][2], ")")[[1]][1])
  p3 <- as.numeric(strsplit(p2, ",")[[1]])
  xy <- matrix(p3, ncol=2, byrow=TRUE)
  sp1 <- Polygons(list(Polygon(xy)), index)
  sp2 <- SpatialPolygons(list(sp1), 1L)
  polys.df <- SpatialPolygonsDataFrame(sp2, data.frame(id=index, row.names=index))
  proj4string(polys.df) <- CRS("+proj=longlat +datum=WGS84")
  return(polys.df)
}