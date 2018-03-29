
#' @export
RT.PinholeCamera <- function (hole.location=c(0,0,0), film.centre=c(0,0,-0.052), film.up=c(0,1,0), film.right=c(1,0,0), film.size=0.036) {

  r <- list(hole.location=hole.location, film.centre=film.centre, film.up=film.up, film.right=film.right, film.size=film.size)

  class(r) <- append(class(r), "PinholeCamera")

  return(r)
}


#---------------------------------------------------------------------------
# This returns a viewframe of pixel locations in space.
# As a matrix of with width 3 = (x,y,z) cordinates
#    and rows 1 = top left, then across and down
#    for total pixel.height x pixel.width rows
#
# Essentially, this is a matrix of ray origins

#' @export

.RT.GetViewframe <- function (camera,pixel.width,pixel.height) {

  vf <- matrix(NA,nrow=pixel.width*pixel.height,ncol=3)

  pix.size <- max(c(pixel.width,pixel.height))    #for a rectangular viewframe, is the larger of the two dimensions 
  dim.up <- Utils.UnitVector(camera$film.up) * camera$film.size
  dim.right <- Utils.UnitVector(camera$film.right) * camera$film.size

  for (y in 1:pixel.height)
    for (x in 1:pixel.width) {
      #print(x + (y-1)*pixel.width)
      vf[(x + (y-1)*pixel.width),] <- camera$film.centre +
                                      dim.right * (x - 0.5 - pixel.width/2) / pix.size +
                                      dim.up * (y - 0.5 - pixel.height/2) / pix.size
  }

  return(vf)
}


#---------------------------------------------------------------------------
#' @export
RT.trace.PinholeCamera <- function (world,camera,pixel.width,pixel.height) {

  vf <- .RT.GetViewframe(camera, pixel.width, pixel.height)

  background <- c(0,0.3,0.7)

  pix <- matrix(background, ncol=3, nrow=pixel.width*pixel.height, byrow=TRUE)

  for (i in 1:(pixel.width*pixel.height)) {
    rt <- Spc.Intersect(vf[i,], camera$hole.location - vf[i,], world)

    if (!is.na(rt)[1]) {
      pix[i,] <- c(1,1,1)
    }
    if (i %% min(pixel.width,pixel.height) == 0) cat(".")  # A kind of status bar
  }

  print("")  # finish the status bar
  return(pix)
}
