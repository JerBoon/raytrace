


#---------------------------------------------------------------------
#' Validate parameters, and return a valid surface object
#'
#'
#' @return Surface object
#' 
#' @export
#'
#' @examples
#'   surf <- RT.asSurface(rgb=c(1,1,0)))

# ultimately I think I need...
#  - colour
#  - matteness - the proportion of matte reflectivity for the surface. If 0, it's a mirror surface, if 1 it's fully matte 
#  - transparency - if 0, then it's not, 1 it's fully transparent
#      - if > 0 also need outside andinside refractive index

RT.Surface <- function(rgb, matteness=0, transparency=0) {

  #----- Some validation checks -----
  if (((typeof(rgb) != "double") ||
       length(rgb) != 3 ||
       min(rgb) < 0 ||
       max(rgb) > 1)
      && typeof(rgb) != "closure") {
    print("RT.Surface: rgb should be a 3 number vector, values between 0 and 1, OR a function which'll return that")
    return(NA)
  }
  if ((typeof(matteness) != "double") ||
      length(matteness) != 1 ||
      matteness < 0 || matteness > 1) {
    print("RT.Surface: matteness should be a scalar value between 0 and 1")
    return(NA)
  }
  if ((typeof(transparency) != "double") ||
      length(transparency) != 1 ||
      transparency < 0 || transparency > 1) {
    print("RT.Surface: transparency should be a scalar value between 0 and 1")
    return(NA)
  }

  r <- list(rgb=rgb,
            matteness=matteness,
            transparency=transparency)

  class(r) = append(class(r),"RTSurface")
  return(r)
}


