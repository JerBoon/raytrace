


#---------------------------------------------------------------------
#' Constructs a valid pinhole camera object. In particular it defines the notional internal dimensions of said camera, and its location in **vecspace**. 
#'
#' @param hole.location The notional position of the pinhole in x,y,z space. Defaults to c(0,0,0)
#' @param film.centre The notional location of the centre of the film within the camera. The default properties of all of the film.parameters
#'    define a film located back along the negative z axis (i.e "looking" in the (0,0,1) direction) with a size and subsequent view angle
#'    approximating that of a ~50mm SLR lens.
#'  @param film.up The notional orientation of the film. By default is "up" i.e to take a horizontal aligned picture (where the positive y axis defines up)
#'  @param film.right Further property of the notional orientation of the film. By default is orthogonal to the view direction and the up vector,
#'    so as to define a rectangular film orthogonal to the view direction. Interesting photograph techniques could be created by manipulating these
#'    film values to align the film in a different orientation to the view direction.
#'  @param film.size The notional size of the film to be exposed. This defines a square size, which by default approximates a 35mm film.
#'    The aspect ratio of the actual exposure will be defined by image height and width (in pixels) with *film.size* taking the longer of the two
#'    pixel dimensions.
#'
#' @return A validated camera object.
#' 
#' @export
#'
#' @examples
#'   cam <- RT.PinholeCamera()

RT.PinholeCamera <- function (hole.location=c(0,0,0), film.centre=c(0,0,-0.052), film.up=c(0,1,0), film.right=c(1,0,0), film.size=0.036) {

  #centre is just a copy of the pinhole location, so that the Spc.Rotate() method will work. Not the best, but it works..

  r <- list(hole.location=hole.location, film.centre=film.centre, film.up=film.up, film.right=film.right, film.size=film.size, centre=hole.location)

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

    pix[i,] <- .RT.trace(vf[i,],camera$hole.location - vf[i,], world, 1)

    if (i %% min(pixel.width,pixel.height) == 0) cat(".")  # A kind of status bar
  }

  print("")  # finish the status bar
  return(pix)
}


#------------------------------------------------------------------------------
#' @importFrom vecspace Spc.Translate
#' @export
.Spc.Translate.PinholeCamera <- function(cam, vector) {

  cam$hole.location <- cam$hole.location + vector
  cam$film.centre <- cam$film.centre + vector

  return(cam)
}

#------------------------------------------------------------------------------
#' @importFrom vecspace Spc.Rotate
#' @export
.Spc.Rotate.PinholeCamera <- function(cam, pivot.point, pivot.rotMatrix) {

  #r <- list(hole.location=hole.location, film.centre=film.centre, film.up=film.up, film.right=film.right, film.size=film.size)
  #Rotates around the pinhole location. Other than that, most of this code is borrowed
  #from the generic
  cam$hole.location <- (pivot.rotMatrix %*% (cam$hole.location - pivot.point)) + pivot.point
  cam$film.centre <- (pivot.rotMatrix %*% (cam$film.centre - pivot.point)) + pivot.point
  cam$centre <- (pivot.rotMatrix %*% (cam$centre - pivot.point)) + pivot.point
  cam$film.up <- pivot.rotMatrix %*% cam$film.up
  cam$film.right <- pivot.rotMatrix %*% cam$film.right

  return(cam)
}

