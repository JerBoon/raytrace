



  #draft plot - this is just a heatmap function, inverse of tv works best
#' @export
#' @importFrom grid grid.raster
RT.plot <- function(pic, pixel.width, pixel.height) {

  #tv <- 1/tv
  #image(matrix(tv,ncol=pixel.width,nrow=pixel.height,byrow=TRUE),useRaster=TRUE,axes=TRUE)
  
  #tvc <- ifelse(is.na(tv),0,(max(tv,na.rm=TRUE)-tv)/max(tv,na.rm=TRUE))
  #col <-rgb(tvc,tvc,tvc)
  #dim(col) <- c(pixel.height,pixel.width)
  #grid.raster(t(col), interpolate=FALSE)
  
  o.rgb <- apply(pic,1, function(x) rgb(x[1],x[2],x[3]))
  dim(o.rgb) <- c(pixel.height,pixel.width)
  grid.raster(t(o.rgb), interpolate=FALSE)

}
