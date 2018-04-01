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

  if (class(prop$rgb) == "numeric") 
    surface.colour <- prop$rgb
  else
    surface.colour <- prop$rgb(rt$north,rt$east)

  # [1] matte reflection component
  
    return.matte <- surface.colour * prop$reflectivity * prop$reflectivity.matte
  
  # [2] spectral reflection component

    #calculate reflected ray direction
    #From http://cosinekitty.com/raytrace/chapter10_reflection.html

    if (prop$reflectivity * (1-prop$reflectivity.matte) > 0) {
      ray.reflected <- ray.direction - 2 * Utils.DotProduct(ray.direction, normal.unit) * normal.unit

      return.spectral <- surface.colour *
                         .RT.trace(ray.origin + ray.direction * rt$distance + Utils.UnitVector(ray.reflected) * epsilon,
                                   ray.reflected,
                                   world,
                                   proportion * prop$reflectivity * (1-prop$reflectivity.matte))
    }
    else
      return.spectral <- c(0,0,0)

  return(return.matte + return.spectral)
}
