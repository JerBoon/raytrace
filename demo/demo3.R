

# This file downloads a small-ish bitmap of the surface of Jupiter from https://www.missionjuno.swri.edu
# If interested, a much higher resolution image exists at the same address
# It also utilises the "jpeg" package, which you'll need to install separately

library(vecspace)
library(raytrace)
library(jpeg)

jup.url <- "https://d2xkkdgjnsfvb0.cloudfront.net/Vault/Thumb?VaultID=514&Interlaced=1&Mode=R&ResX=750&OutputFormat=jpg&Quality=90&t=1519669284"
dest.file <- "jupiter_wrap.jpg"

if (!file.exists(dest.file))  {
  print ("Downloading Jupiter bitmap")
  download.file(jup.url,dest.file,mode="wb")
}

# --------- build the world --------

p.surf <- RT.Surface(rgb = function(x,y) { if ((x %/% 5) %% 2 == ((y %/% 5) %% 2)) return (c(0.8,0.8,0.8)) else return (c(0.2,0.2,0.2)) },
                     matteness=1 )
p <- Spc.MakePlane(point=c(0,-4,0),normal=c(0,1,0), properties=p.surf,direction.north=c(0,0,1),direction.east=c(1,0,0))


sp1.prop <- RT.Surface(rgb = c(1,1,1),matteness=0.2)
sp1 <- Spc.MakeSphere(c(0,-1,40), 3, properties=sp1.prop)

sp2.prop <- RT.Surface(rgb = c(0.3,1,0.6), transparency=0.5)
sp2 <- Spc.MakeSphere(c(6,-1,35), 3, properties=sp2.prop)

sp3.rgb <- function(x,y) { if (abs(x) <20) return (c(1,0,0)) else return(c(1,1,1)) }
sp3.prop <- RT.Surface(sp3.rgb, matteness=1)
sp3 <- Spc.MakeSphere(c(9,-1,25), 3, properties=sp3.prop,direction.pole=c(0,1,0),direction.meridian=c(1,0,0))
sp3 <- Spc.Rotate(sp3,pivot.angle=c(40,0,10))

sp4.jupi <- readJPEG(dest.file)
sp4.rgb <- function(x,y) { xp <- (y*750)%/%360+1; yp <- ((90-x)*374)%/%180+1; return(sp4.jupi[yp,xp,])}
sp4.prop <- RT.Surface(sp4.rgb,matteness=1)
sp4 <- Spc.MakeSphere(c(-6,-1,35), 3, properties=sp4.prop, direction.pole=c(0,1,0),direction.meridian=c(1,0,0))
sp4 <- Spc.Rotate(sp4,pivot.angle=c(20,-45,0))


sp5.rgb <- function(x,y) { if (abs(x) <75) return (c(1,0.6,0.2)) else return(c(1,1,1)) }
sp5.prop <- RT.Surface(sp5.rgb, matteness=1)
sp5 <- Spc.MakeSphere(c(-9,-1,25), 3, properties=sp5.prop,direction.pole=c(0,1,0),direction.meridian=c(1,0,0))
sp5 <- Spc.Rotate(sp5,pivot.angle=c(30,0,80))

world <- Spc.Combine(list(p,sp1, sp2, sp3, sp4, sp5),bound=FALSE)
world <- Spc.Rotate(world,pivot.angle=c(-15,0,0),pivot.point=c(0,0,10))

# ---------- ray trace it! --------

w=200
h=200

cam <- RT.PinholeCamera()
pic <- RT.trace.PinholeCamera(world=world,camera=cam,pixel.width=w,pixel.height=h)
RT.plot(pic,w,h)
