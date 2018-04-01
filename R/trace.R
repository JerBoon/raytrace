# This is the main recursive ray-tracing bit.
# It traces a single ray, does the recursive bit where necessary, and returns a colour

.RT.trace <- function (ray.origin, ray.direction, world, proportion=1) {

  if (proportion < 0.01) return (c(0,0,0))


  rt <- Spc.Intersect(ray.origin, ray.direction, world)

  if (is.na(rt)) return (c(0,0,0))

    if (!is.na(rt)[1]) {
      if (!is.na(rt$north)) {
        if ((rt$north %/% 10) %% 2 == (rt$east %/% 10) %% 2) 
           pix[i,] <- c(0,0,0)
        else  pix[i,] <- c(1,1,1)
      } else
      pix[i,] <- c(1,1,1)
    }
    if (i %% min(pixel.width,pixel.height) == 0) cat(".")  # A kind of status bar

  print("")  # finish the status bar
  return(pix)
}
