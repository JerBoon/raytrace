# This is the main recursive ray-tracing bit.
# It traces a single ray, does the recursive bit where necessary, and returns a colour
#
# It expects to intersect, if at all, with a surface defined by the RT.Surface(), so will
# assume the surface properties exist as per that function

.RT.trace <- function (ray.origin, ray.direction, world, proportion=1) {


  if (proportion < 0.01) return (c(0,0,0))


  rt <- Spc.Intersect(ray.origin, ray.direction, world)

  if (is.na(rt)[1]) {
    #print("no intersect")
    return (c(0,0,0))
  }

  # else our ray has intersected with something
  # so do the necessary calculations with the surface etc

  prop <- rt$properties
  normal.unit <- Utils.UnitVector(rt$normal)
  epsilon <- 0.000001
  intersect.point <- ray.origin + ray.direction * rt$distance

  if (class(prop$rgb) == "numeric") 
    surface.colour <- prop$rgb
  else
    surface.colour <- prop$rgb(rt$north,rt$east)

  # [1] matte reflection component
  
    return.matte <- surface.colour * (1-prop$transparency) * prop$matteness *
                    .RT.GetLighting(intersect.point - Utils.UnitVector(ray.direction) * epsilon, normal.unit, world)
  
  # [2] mirror reflection component

    #calculate reflected ray direction
    #From http://cosinekitty.com/raytrace/chapter10_reflection.html

    shinyness <- (1-prop$transparency) * (1-prop$matteness)

    if (shinyness > 0) {
      ray.reflected <- ray.direction - 2 * c(ray.direction %*%  normal.unit) * normal.unit

      return.mirror <- surface.colour * shinyness *
                         .RT.trace(intersect.point + Utils.UnitVector(ray.reflected) * epsilon,
                                   ray.reflected,
                                   world,
                                   proportion * shinyness)
    }
    else
      return.mirror <- c(0,0,0)

  return(return.matte + return.mirror)
}


.RT.GetLighting <- function(point, normal, world) {

  light <- c(-10,40,-100)

  #normal is unit vector already
  incidence <- max(c(normal %*% Utils.UnitVector(light-point)),0)

  return(Spc.NoIntersect(point, light-point, world) * incidence)
}


