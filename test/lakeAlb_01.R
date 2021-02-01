library(mmt)
library(magrittr)
library(stringr)
library(raster)
library(dplyr)
library(lubridate)
library(sf)

#----------- FUNCS
bbox_poly<- function(bbox) { data.frame( x= bbox[c(1,2,2,1,1)], y=bbox[c(4,4,3,3,4)])}

expand_bbox <- function(bbox, expand=.05) bbox+c(-expand,expand,-expand,expand)

prof <- function(x= c(141.2,142.5), y=c(-35.5,-35.4), n=100){
  xdiff= diff(x)/n
  ydiff=diff(y)/n
  data.frame(x=seq(x[1], x[2], xdiff), y=seq(y[1], y[2], ydiff))
}

#-----------

clip.region.s <- c(141.4, 142.4, -36.2, -35) %>% expand_bbox()
clip.region <- c(140.6, 143.5, -36.5, -33.9) %>% expand_bbox()
#clip.region <- c(137.8, 143.5, -29.5, -24.3)

fact=4
mf=1.
dem <-get_srtm3(clip =clip.region, fact=fact)
dem[dem< -10] <- -10

dem.ray <-  (dem*1)   %>%
  as.matrix() %>%
  t() %>%
  rayshader::sphere_shade(texture="desert")


dem.shade <- map_raster(dem.ray,  ras=dem,  plot=F)
dem.shade.max  <- max(maxValue(dem.shade))
eyre.laea=raster::crs(paste0("+proj=laea +lat_0=",
                             raster::extent(dem)[3:4] %>% diff()/2+raster::extent(dem)[3],
                             " +lon_0=",
                             raster::extent(dem)[1:2] %>%
                               diff()/2+raster::extent(dem)[1],
                             " +ellps=WGS84"))
dem.shade.laea <-dem.shade  %>% raster::projectRaster( crs=eyre.laea)
dem.laea <-dem  %>% raster::projectRaster( crs=eyre.laea)
dem.shade.laea.max  <- max(maxValue(dem.shade.laea))


gq.sf <- read_gq()  %>%
  st_as_sf(coords=c("Longitude", "Latitude"), crs=4326) %>%
  st_crop(raster::extent(dem))
gq.laea.sf <-st_transform(gq.sf, crs = eyre.laea@projargs)


s.box  <- bbox_poly(clip.region.s)
s.box.laea <- s.box  %>% convertPts(to=eyre.laea)

### needs to be put into package
source('~/Dropbox/msandifo/documents/programming/r/2019/gab/R/sf.R')
###
read_AHGFNetworkStream()
streams<- AHGFNetworkStream  %>%
  st_crop(raster::extent(dem)) %>%
  subset(UpstrDArea >1e7) %>%
  st_geometry()
streams.laea <- st_transform(streams, crs = eyre.laea@projargs)

read_AHGFWaterbody()
waterbody<- AHGFWaterbody  %>%
  st_crop(raster::extent(dem)) %>%
  subset(Shape_Area >5e-5) %>%
  st_geometry()
waterbody.laea <- st_transform(waterbody, crs = eyre.laea@projargs)



#profile.coords <- prof(x=c(141.2,142.5), y=c(-35.5,-35.4),100 )
profile.coords <- prof(x=c(141.4,143.5),
                       y=c(-35.5,-35.3),500 )
profile.coords.laea <- profile.coords %>%
  convertPts(to=eyre.laea)

fault.locs<- c(-18000,36000, 59000, 127000)
profile.coords.laea$z<- raster::extract(dem.laea,profile.coords.laea)
profile.coords.laea$s <- "l"
profile.coords.laea$s[profile.coords.laea$x < fault.locs[1]] <- "w"
profile.coords.laea$s[profile.coords.laea$x > fault.locs[1] & profile.coords.laea$x<fault.locs[2]] <- "e"
profile.coords.laea$s[profile.coords.laea$x > fault.locs[2] & profile.coords.laea$x < fault.locs[3]] <- "e1"
profile.coords.laea$s[profile.coords.laea$x > fault.locs[3] & profile.coords.laea$x< fault.locs[4]] <- "e2"

fault.inds <-purrr::map(fault.locs , function(x) which.min(abs(profile.coords.laea$x-x )))%>% unlist()



my.col=terrain.colors(255)
n=90
inds <- (1:n/n)^.55*n %>% round(0) *255/n
my.col<- my.col[inds]
#my.col=c(   stretch_brewer(pal ="BrBG",stretch=c( .05,.35, .65,.85, .95 ), n=100)) %>% rev() #terrain.colors(255),

png("~/Desktop/Albacutya_ll.png", width=1500*1.1*mf, height=1500*mf   )
#pdf("~/Desktop/Albacutya.pdf", width=20*1.2*mf, height=20*mf   )

msPlot(dem %>% stretch_ras(c(0.18, .45)), add=F, col=my.col)
plotRGB(dem.shade, scale=dem.shade.max*1.01, alpha=.3 , add=T)
 plot( streams, col="blue3" , add=T,   lwd=1.3)
 plot( waterbody,  fg="blue3",  col="blue3", add=T, lwd=0.01, alpha=1)
 #plot(gq.sf, cex=gq.sf$M/.8*mf, col="black", fg="grey30", add=T)
lines(s.box , lwd=2)
lines(profile.coords, lwd=4)

plot_axes_trad(dem,
               proj=NA,
               cex=2.3*mf,
               ax.int =.5,
               grid=T,
               add=T,
               width=1.5,
               extend=0.06)

raster::plot(dem %>% stretch_ras(c(0.12, .45)), #%>% stretch_ras(lims= stretch.lims )  ,
             horizontal = TRUE,
             smallplot=c(.35, .65, .035, .065), #-.21, -.235),
             col= my.col,            #legend.width=5,
             legend.only = TRUE,
             #alpha=.55,
             #note cant control vertical sep on legend using 'adj' so here I insert \n
             legend.args=list(text= "elevation - m",   cex= 3. , fg="white", bty="o" ),
             axis.args = list(cex.axis =2.3, mgp=c(1,2,0))
)
dev.off()


png("~/Desktop/Albacutya_laea.png", width=1500/1.1*mf, height=1500*mf   )
#pdf("~/Desktop/Albacutya.pdf", width=20*1.2*mf, height=20*mf   )

msPlot(dem.laea %>% stretch_ras(c(0.12, .45)), add=F, col=my.col)
plotRGB(dem.shade.laea, scale=dem.shade.laea.max*1.01, alpha=.3 , add=T)
plot( streams.laea, col="blue3" , add=T,    lwd=1.3)
plot( waterbody.laea,  fg="blue3",  col="blue3", add=T, lwd=0.01, alpha=1)

plot(gq.laea.sf, cex=gq.sf$M/.8*mf, col="black", fg="grey30", add=T)
lines(s.box.laea , lwd=3)
lines(profile.coords.laea , lwd=3)
points(profile.coords.laea[fault.inds,], cex=3, lwd=5)
plot_axes_trad(dem,
               proj=eyre.laea,
               cex=2.3*mf,
               ax.int =.5,
               grid=T,
               add=T,
               width=1.5,
               extend=0.06)

raster::plot(dem %>% stretch_ras(c(0.12, .45)), #%>% stretch_ras(lims= stretch.lims )  ,
             horizontal = TRUE,
             smallplot=c(.35, .65, .035, .065), #-.21, -.235),
             col= my.col,            #legend.width=5,
             legend.only = TRUE,
             #alpha=.55,
             #note cant control vertical sep on legend using 'adj' so here I insert \n
             legend.args=list(text= "elevation - m",   cex= 3. , fg="white", bty="o" ),
             axis.args = list(cex.axis =2.3, mgp=c(1,2,0))
             )
dev.off()

# profile.coords <- prof(x=c(141.4,142.5),
#                        y=c(-35.5,-35.4),500 )%>% convertPts(to=eyre.laea)


 library(ggplot2)
ggplot(profile.coords.laea, aes((x)/1000 -fault.locs[1]/1000, z))+
  geom_vline(xintercept=fault.locs/1000-fault.locs[1]/1000, size=8, alpha=.2)+
  geom_line(size=.24)+
  labs(x="distance - kms", y="m")+
  geom_smooth(data=profile.coords.laea %>% subset(s != "l"), method="lm",
              aes(group=s),
              col="grey20", se=F, show.legend = F, size=1)+
                theme_bw()

ggsave("~/Desktop/profile.pdf", width=10, height=3)
#-----------------------


#clip.region <- c(137.8, 143.5, -29.5, -24.3)

fact=4
mf=1.

dem1 <-get_srtm1(clip =clip.region.s)
#dem1 <-get_srtm1()
dem1[dem1< -10] <- -10

dem1.ray <-  (dem1)   %>%
  as.matrix() %>%
  t() %>%
  rayshader::sphere_shade(texture="bw", zscale = 1, sunangle = 290)


dem1.shade <- map_raster(dem1.ray,  ras=dem1,  plot=F)

# gq.sf <- read_gq()  %>%
#   st_as_sf(coords=c("Longitude", "Latitude"), crs=4326) %>%
#   st_crop(raster::extent(dem))

eyre.laea=raster::crs(paste0("+proj=laea +lat_0=",
                             raster::extent(dem)[3:4] %>% diff()/2+raster::extent(dem)[3],
                             " +lon_0=",
                             raster::extent(dem)[1:2] %>%
                               diff()/2+raster::extent(dem)[1],
                             " +ellps=WGS84"))
dem1.shade.max  <- max(maxValue(dem1.shade))
dem1.shade.laea <-dem1.shade  %>% raster::projectRaster( crs=eyre.laea)
dem1.laea <-dem1  %>% raster::projectRaster( crs=eyre.laea)
dem1.shade.laea.max  <- max(maxValue(dem1.shade.laea))
gq.laea.sf <-st_transform(gq.sf, crs = eyre.laea@projargs)

streams1<- AHGFNetworkStream  %>%
  st_crop(raster::extent(dem1)) %>%
  subset(UpstrDArea >5e8) %>%
  st_geometry()
streams1.laea <- st_transform(streams1, crs = eyre.laea@projargs)


waterbody1<- AHGFWaterbody  %>%
  st_crop(raster::extent(dem1)) %>%
  subset(Shape_Area >5e-5) %>%
  st_geometry()
waterbody1.laea <- st_transform(waterbody1, crs = eyre.laea@projargs)


# my.col=terrain.colors(255)
# my.col=c(   stretch_brewer(pal ="BrBG",stretch=c( .05, .2, .35, .45,.55,.65,.85, .95 ), n=200)) %>% rev() #terrain.colors(255),
 #plotRGB(dem.shade^1 , scale=dem.shade.laea.max*1.01, alpha=1 , add=F)

png("~/Desktop/Albacutya_2.1sec.png", width=1500/1.2*mf, height=1500*mf   )
#pdf("~/Desktop/Albacutya.pdf", width=20*1.2*mf, height=20*mf   )
#my.col=c(   stretch_brewer(pal ="BrBG",stretch=c(  .25, .65,.85 ,.95 ), n=100)) %>% rev() #terrain.colors(255),

msPlot(dem1.laea  %>% stretch_ras(c(0.065, .65)), add=F, col=my.col)

plotRGB(dem1.shade.laea , scale=dem1.shade.laea.max , alpha=.35 , add=T)
plot( streams1.laea, col="blue3" , add=T,    lwd=1.3)
plot( waterbody1.laea,  fg="blue3",  col="blue3", add=T, lwd=0.01, alpha=1)

#plot(gq.sf, cex=gq.sf$M/.8*mf, col="black", fg="grey30", add=T)
plot_axes_trad(dem1,
               proj=eyre.laea,
               cex=2.*mf,
               ax.int =c(.1, .1),
               grid=T,
               add=T,
               width=1.5,double = T,
               extend=0.06, label.int=3)

raster::plot(dem1 %>% stretch_ras(c(0.2, .45)), #%>% stretch_ras(lims= stretch.lims )  ,
             horizontal = TRUE,
             smallplot=c(.35, .65, .035, .065), #-.21, -.235),
             col= my.col,            #legend.width=5,
             legend.only = TRUE,
             #alpha=.55,
             #note cant control vertical sep on legend using 'adj' so here I insert \n
             legend.args=list(text= "elevation - m",   cex= 3. , fg="white", bty="o" ),
             axis.args = list(cex.axis =2.3, mgp=c(1,2,0))
)
dev.off()
 s


