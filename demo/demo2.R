library(vecspace)
library(raytrace)

#-----------------------------------------------
# Demonstrating using a random function to add a
# slight colour texture to the plane surface
#-----------------------------------------------

p.surf <- RT.Surface(rgb = function(x,y) { return (c(0.9,0.4,0.2) + runif(1,-1,1)*0.03) },
                     matteness=1 )
p <- Spc.MakePlane(point=c(0,-100,0),normal=c(0,1,0), properties=p.surf,direction.north=c(0,0,1),direction.east=c(1,0,0))

sp.surf <- RT.Surface(rgb = c(0.6,0.8,0.4),matteness=0.5)
sp <- Spc.MakeSphere(c(-60,60,400),50,properties=sp.surf)

world <- Spc.Combine(list(p,sp), bound=FALSE)


w=500
h=500

cam <- RT.PinholeCamera()
pic <- RT.trace.PinholeCamera(world=world,camera=cam,pixel.width=w,pixel.height=h)
RT.plot(pic,w,h)
