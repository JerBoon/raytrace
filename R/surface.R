


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
#  - transparency - if 0, then it's not, plus if > 0
#      - opacity - randomness of refracted light
#      - outside andinside refractive index
#  - reflectiveness (= opposite of transparency, but at some angle refraction won';t occur, so needs to be added here)
#      - specular reflection
#      - matt reflection - is there such a thing as matte-ness ?

RT.Surface <- function(rgb, reflectivity=1, reflectivity.matte=0, transparency=0, transparency.matte=0) {

  #----- Some validation checks -----
  if (((typeof(rgb) != "double") ||
       length(rgb) != 3 ||
       min(rgb) < 0 ||
       max(rgb) > 1)
      && typeof(rgb) != "closure") {
    print("RT.Surface: rgb should be a 3 number vector, values between 0 and 1, OR a function which'll return that")
    return(NA)
  }
  if ((typeof(reflectivity) != "double") ||
      length(reflectivity) != 1 ||
      reflectivity < 0 || reflectivity > 1) {
    print("RT.Surface: reflectivity should be a scalar value between 0 and 1")
    return(NA)
  }
  if ((typeof(transparency) != "double") ||
      length(transparency) != 1 ||
      transparency < 0 || transparency > 1) {
    print("RT.Surface: transparency should be a scalar value between 0 and 1")
    return(NA)
  }
  if (transparency + reflectivity > 1) {
    print("RT.Surface: sum of transparency and reflectivity should be between 0 and 1")
    return(NA)
  }
  if ((typeof(reflectivity.matte) != "double") ||
      length(reflectivity.matte) != 1 ||
      reflectivity.matte < 0 || reflectivity.matte > 1) {
    print("RT.Surface: reflectivity.matte should be a scalar values between 0 and 1")
    return(NA)
  }
  if ((typeof(transparency.matte) != "double") ||
      length(transparency.matte) != 1 ||
      transparency.matte < 0 || transparency.matte > 1) {
    print("RT.Surface: transparency.matte should be a scalar values between 0 and 1")
    return(NA)
  }

  r <- list(rgb=rgb,
            reflectivity=reflectivity,
            reflectivity.matte=reflectivity.matte,
            transparency=transparency,
            transparency.matte=transparency.matte)

  class(r) = append(class(r),"RTSurface")
  return(r)
}


