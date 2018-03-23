
w <- 500
h <- 500

o1 <- Spc.MakeCuboid(c(0,0,10),c(1,1,1),"red")
o2 <-Spc.MakeDodecahedron(c(3,0,10),2,"blue")
#world <-Spc.Combine(list(o1,Spc.Rotate(o2,pivot.angle=c(10,20,30))))
world <-Spc.Combine(list(Spc.Rotate(o1,pivot.angle=c(45,30,45)),Spc.Rotate(o2,pivot.angle=c(10,20,30))))
cam <-RT.PinholeCamera()

vf <- .RT.GetViewframe(cam,w,h)
o.rgb <- character(0)
o.rgb[w*h] <- NA


for (i in 1:(w*h)) {

 r <- Spc.Intersect(vf[i,],cam$hole.location - vf[i,], world)

 if (class(r) != "logical")
    o.rgb[i] <- r$properties
}

print(matrix(o.rgb,ncol=w,byrow=T))
