library(magrittr)
library(stringr)
library(raster)
library(dplyr)
library(lubridate)
library(sf)
library(mmt)
expand_bbox <- function(bbox, expand=.05) bbox+c(-expand,expand,-expand,expand)
clip.region <- c(136., 146, -30, -22) %>% expand_bbox()
#clip.region <- c(137.8, 143.5, -29.5, -24.3)

fact=16
mf=1.5
dem <-get_srtm3(clip =clip.region, fact=fact, input.file = "/Volumes/data/data/regional/australia/srtm3/oz_srtm")
dem[dem< -10] <- -10


pro.latlon="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
dem.ray <-  dem  %>%
  as.matrix() %>%
  t() %>%
  rayshader::sphere_shade(texture="desert")


dem.shade <- map_raster(dem.ray,  ras=dem,  plot=F)
crs(dem.shade)=pro.latlon
#dem.shade.i <- dem.shade*255 %>% as.integer()
# rayshader::plot_map(dem.ray)
# msPlot( dem.shade*255)


gq.sf <- read_gq()  %>%
  st_as_sf(coords=c("Longitude", "Latitude"), crs=4326) %>%
  st_crop(raster::extent(dem))


eyre.laea = raster::crs(paste0("+proj=laea +lat_0=",
                      raster::extent(dem)[3:4] %>% diff()/2+raster::extent(dem)[3],
                     " +lon_0=",
                     raster::extent(dem)[1:2] %>%
                      diff()/2+raster::extent(dem)[1],
                     " +ellps=WGS84"))

dem.shade.laea <- dem.shade  %>% raster::projectRaster( crs=eyre.laea)
dem.laea <-dem  %>% raster::projectRaster( crs=eyre.laea)
gq.laea.sf <-st_transform(gq.sf, crs = eyre.laea@projargs)

my.col=terrain.colors(255)

my.col=c(   stretch_brewer(pal ="BrBG",stretch=c( .65, .85, .9, .95), n=100)) %>% rev() #terrain.colors(255),
my.col=c(   stretch_pal(pal ="terrain.colors",stretch=c( .3, .55, .8, .95), n=100)) %>% rev() #terrain.colors(255),
MF=1
png("~/Desktop/Eyre-basin-raster-topo_filt_laeae-ms-small.png", width=1500*1.2*mf, height=1500*mf   )
#pdf("~/Desktop/Eyre-basin-raster-topo_filt_laeae-ms-small.pdf", width=20*1.2*mf, height=20*mf   )

msPlot(dem.laea %>% stretch_ras(c(0.05, .75)), add=F, col=my.col)
plotRGB(dem.shade.laea, scale=1.08, alpha=.6 , add=T)


plot(gq.laea.sf, cex=gq.sf$M/.8*mf, col="black", fg="grey30", add=T)
plot_axes_trad(dem,
               proj=eyre.laea,
               cex=2.3*mf,
               ax.int =1,
               grid=T,
               add=T,
               width=1.5,
               extend=0.06)

raster::plot(dem, #%>% stretch_ras(lims= stretch.lims )  ,
             horizontal = TRUE,
             smallplot=c(.35, .65, .035, .065), #-.21, -.235),
             col= my.col,            #legend.width=5,
             legend.only = TRUE,
             #alpha=.55,
             #note cant control vertical sep on legend using 'adj' so here I insert \n
             legend.args=list(text= "elevation (m)\n",   cex= 3. , fg="white", bty="o" ),
             axis.args = list(cex.axis =2.3, mgp=c(1,2,0))
             )
dev.off()

