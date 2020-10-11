`%/%` <- function(dir, fname,sep="/") {paste(dir, fname, sep=sep)}
`% %` <- function(dir, fname,sep=" ") {paste(dir, fname, sep=sep)}
`%s%` <-  function(dir, fname,sep=" ") {paste(dir, fname, sep=sep)}
`%,%` <-  function(dir, fname,sep=",") {paste(dir, fname, sep=sep)}
`%;%` <-  function(dir, fname,sep=";") {paste(dir, fname, sep=sep)}
`%%` <-  function(dir, fname,sep="") {paste(dir, fname, sep=sep)}

#' Title
#'
#' @param plot.region
#' @param v  certical exageration
#' @param re resize /downsise by factor
#'
#' @return string for idl exec.
#' @export
#'
#' @examples
get_idl_string<- function(plot.region =  c(140.6, 143.9, -36.5, -33.9),
                          v=5,
                          ct = -1,
                          re=2
){


  fir<- system.file("idl",   package="mmt")

  paste0( "source ~/.bashrc; pushd ", fir ,
     ";   /Applications/harris/envi54/idl86/bin/idl -e 'idlshade,  lims=[" ,
    plot.region[3] %,%
    plot.region[1] %,%
    plot.region[4] %,%
    plot.region[2] , "], v=" ,v , ", ct= " , ct, ", re=" , re , ", sea_level=-15 '; popd")

}


#' retiurns shaded image using ms idl shade reoutiens need srtm rotines installed etc,
#'
#' @param plot.region
#' @param v verical exaggeration usied in shading defaulst to 10
#' @param re resize defaults to 1
#'
#' @return
#' @export
#'
#' @examples
#'

make_idl<- function(plot.region =  c(140.6, 143.9, -36.5, -33.9),
                    v=10,
                    re=2,
                    ct = -1
){

idl.string<-get_idl_string(plot.region =plot.region, v=v, ct=ct, re=re)
print(idl.string)
  system2("/usr/local/bin/bash", args = c("-c", shQuote(idl.string)))

  fir<- system.file("idl",   package="mmt") %% "/figures/idl.tiff"
   #raster::stack(fir)
  print(fir)
  im<- raster::brick(fir, crs="+proj=longlat +datum=WGS84")
     raster::extent(im) <-raster::extent(plot.region)
  im
}


#   get_idl_r<- function(file) {
#
#   c(1,1,-1,-1)
#
#  fstring <-  stringr::str_split(file, "/") %>% unlist() %>% tail(1)  %>%
#   stringr::str_remove( ".jpg")
#
#  trans <-  c(1,1,-1,-1)
#  if (stringr::str_detect(fstring, "N"))  trans[c(3,4)]<-c(1,1)
#  if (stringr::str_detect(fstring, "W"))  trans[c(1,2)]<-c(-1,-1)
#
#  bounds<-fstring %>%
#   stringr::str_split("[A-Z_]") %>% unlist() %>% tail(4) %>%
#   as.numeric() %>%
#   `+`(.,c(0,0,.[1],.[2])) %>%
#   `.`[c(2,4,3,1)] %>%
#   `*`(.,trans)
#  im1<-raster::brick(file, crs="+proj=longlat +datum=WGS84")
#  raster::extent(im1) <-raster::extent(bounds)
#  im1
# }
#
#
# dir<-"/Users/msandifo/docs/programming/idl/2018/wimmera/figures/"
# fn<-"S36.3000E141.500_1.30000_1.30000.jpg"
# file <- paste0(dir,fn)
# im1<-get_idl_im(file)
# # im1<-raster::brick(file, crs="+proj=longlat +datum=WGS84")
# # raster::extent(im1) <-raster::extent(get_idl_bounds(file))
# raster::plotRGB(im1 ,
#                 alpha=255)
# im1
