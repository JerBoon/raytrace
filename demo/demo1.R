library(vecspace)
library(raytrace)

p.surf <- RT.Surface(rgb = function(x,y) { if ((x %/% 20) %% 2 == ((y %/% 20) %% 2)) return (c(1,1,1)) else return (c(0,0,0)) },
                     matteness=1 )
p <- Spc.MakePlane(point=c(0,-10,0),normal=c(0,1,0), properties=p.surf,direction.north=c(0,0,1),direction.east=c(1,0,0))
p <- Spc.Rotate(p,pivot.angle=c(0,20,0))

sp.surf <- RT.Surface(rgb = c(0.6,0.8,0.4),matteness=0.5)
sp <- Spc.MakeSphere(c(-2,1,10),1.4,properties=sp.surf)

sp2.surf <- RT.Surface(rgb = c(1,1,1),matteness=0.2)
sp2 <- Spc.MakeSphere(c(1,3,15),1.4,properties=sp2.surf)

cu.surf <- RT.Surface(rgb = c(1,0.3,0.1), matteness=1)
cu <- Spc.Rotate(Spc.MakeCuboid(c(2,1,10),c(1,1,1),properties=cu.surf),pivot.angle=c(20,40,60))

world <- Spc.Combine(list(p,sp,sp2,cu), bound=FALSE)

w=100
h=100

cam <- RT.PinholeCamera()
pic <- RT.trace.PinholeCamera(world=world,camera=cam,pixel.width=w,pixel.height=h)
RT.plot(pic,w,h)
