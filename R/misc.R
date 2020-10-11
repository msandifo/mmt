
library(ms)
`%/%` <- function(dir, fname,sep="/") {paste(dir, fname, sep=sep)}
`% %` <- function(dir, fname,sep=" ") {paste(dir, fname, sep=sep)}
`%s%` <-  function(dir, fname,sep=" ") {paste(dir, fname, sep=sep)}
`%,%` <-  function(dir, fname,sep=",") {paste(dir, fname, sep=sep)}
`%;%` <-  function(dir, fname,sep=";") {paste(dir, fname, sep=sep)}
`%%` <-  function(dir, fname,sep="") {paste(dir, fname, sep=sep)}

#' Title
#'
#' @param file
#'
#' @return
#' @export
#'
#' @examples
sys.open <- function (file) {system("open " % % file)}


#mid
#' Rewworking of rayshader's plot map
#'
#' @param hillshade
#' @param rotate
#' @param ras  georeferenced raster that provides extent data and crs.
#' @param plot
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
map_raster <- function (hillshade, rotate = 0, ras=NA, plot=F, ...)
{
  library(rayshader)
  rotatef = function(x) t(apply(x, 2, rev))
  if (!(rotate %in% c(0, 90, 180, 270))) {
    if (length(rotate) == 1) {
      warning(paste0("Rotation value ", rotate, " not in c(0,90,180,270). Ignoring"))
    }
    else {
      warning(paste0("Rotation argument `rotate` not in c(0,90,180,270). Ignoring"))
    }
    number_of_rots = 0
  }
  else {
    number_of_rots = rotate/90
  }
  if (class(hillshade) == "array") {
    if (number_of_rots != 0) {
      newarray = hillshade
      newarrayt = array(0, dim = c(ncol(hillshade), nrow(hillshade),
                                   3))
      for (i in 1:number_of_rots) {
        for (j in 1:3) {
          if (i == 2) {
            newarray[, , j] = rotatef(newarrayt[, , j])
          }
          else {
            newarrayt[, , j] = rotatef(newarray[, , j])
          }
        }
      }
      if (number_of_rots == 2) {
        hillshade = newarray
      }
      else {
        hillshade = newarrayt
      }
    }
    mr <- raster::brick(hillshade,
                        xmn = 0.5, xmx = dim(hillshade)[2] + 0.5, ymn = 0.5,
                        ymx = dim(hillshade)[1] + 0.5 )
    if (class(ras)=="RasterLayer" | class(ras)=="RasterBrick" | class(ras)=="RasterStack" ) {extent(mr) <- extent(ras)
    crs(mr) <- crs(ras)}
    if (plot)    suppressWarnings(raster::plotRGB(mr, scale = 1, maxpixels = nrow(hillshade) *
                                                    ncol(hillshade), ...)) else return(mr) }
  else if (class(hillshade) == "matrix") {
    flipud = function(matrix) {
      matrix[nrow(matrix):1, ]
    }
    fliplr = function(matrix) {
      matrix[, ncol(matrix):1]
    }
    if (number_of_rots != 0) {
      for (j in 1:number_of_rots) {
        hillshade = rotatef(hillshade)
      }
    }
    array_from_mat = array(flipud(t(hillshade)), dim = c(ncol(hillshade),
                                                         nrow(hillshade), 3))
    mr <- raster::brick(array_from_mat,
                        xmn = 0.5, xmx = dim(array_from_mat)[2] + 0.5, ymn = 0.5,
                        ymx = dim(array_from_mat)[1] + 0.5 )
    if (class(ras)=="RasterLayer" | class(ras)=="RasterBrick" | class(ras)=="RasterStack" ) {extent(mr) <- extent(ras)
    crs(mr) <- crs(ras)}
    if (plot)  suppressWarnings(raster::plotRGB(mr, scale = 1,
                                                maxpixels = nrow(hillshade) * ncol(hillshade), ...)) else return(mr)
  }
  else {
    stop("`hillshade` is neither array nor matrix--convert to either to plot.")
  }
}




#/Volumes/edata/aac46307-fce8-449d-e044-00144fdd4fa6/aac46307-fce8-449d-e044-00144fdd4fa6/w001000.tif

get_srtm1<- function(clip=c(141.2, 142.6, -36.2, -35),
                     input.file="/Volumes/edata/aac46307-fce8-449d-e044-00144fdd4fa6/aac46307-fce8-449d-e044-00144fdd4fa6/w001000.tif",
                     output.file= tempfile(), #"/tmp/srtm1_clip.tif",
                     fact=NA  ){

  library(gdalUtils)

  gdalUtils::gdalinfo(input.file)
  # dim.expression<-gdalUtils::gdalinfo(input.file)[11] %>%  str_replace("Pixel Size = ","cellsize=c")
  dim.exp<-gdalUtils::gdalinfo(input.file,verbose=T)[11]
  print(  dim.exp)
  cellsize<-dim.exp %>%  str_remove("Pixel Size = " ) %>% str_remove("[(]") %>% str_remove("[)]") %>% str_split(",")  %>% unlist() %>% as.numeric()

  print("input spacings are:" )

  print(cellsize)
 # cellsize = cellsize*fact
  cellsize =c(0.0002777778*fact,0.0002777778*fact)
  print("output pacings are:")

  print(cellsize)

  if (file.exists(output.file) ) file.remove(output.file)
  #clip <- c(136., 146, -30, -22)


  #-te xmin ymin xmax ymax:
  if (is.na(fact)) {
    gdal.string <-"gdalwarp -te" % %   clip[1] % % clip[3] % % clip[2]  % % clip[4] % %  input.file % % output.file  } else
    {  gdal.string <-"gdalwarp -te" % %   clip[1] % % clip[3] % % clip[2]  % % clip[4] % %  "-tr" % %  cellsize[1] % %  cellsize[2]  % % input.file % % output.file}

  print(gdal.string)
  system(gdal.string)
  raster::raster(output.file)

}


get_srtm3<- function(clip.region=c(136., 146, -30, -22),
                     input.file="/Volumes/data/data/regional/australia/srtm3/oz_srtm",
                     output.file= "/tmp/srtm_clip.tif",
                     fact=NA  ){

  library(gdalUtils)
  `%/%` <- function(dir, fname,sep="/") paste(dir, fname, sep=sep)
  `% %` <- function(dir, fname,sep=" ") paste(dir, fname, sep=sep)

  gdalUtils::gdalinfo(input.file)
  # dim.expression<-gdalUtils::gdalinfo(input.file)[11] %>%  str_replace("Pixel Size = ","cellsize=c")
  dim.exp<-gdalUtils::gdalinfo(input.file,verbose=T)
  dim.exp <-  dim.exp[str_detect( dim.exp,"Pixel Size")]
  print(  dim.exp)
  cellsize<-dim.exp %>%  str_remove("Pixel Size = " ) %>% str_remove("[(]") %>% str_remove("[)]") %>% str_split(",")  %>% unlist() %>% as.numeric()

  print("input spacings are:" )

  print(cellsize)
  cellsize = cellsize*fact

  print("output pacings are:")

  print(cellsize)

  if (file.exists(output.file) ) file.remove(output.file)
  #clip.region <- c(136., 146, -30, -22)


  #-te xmin ymin xmax ymax:
  if (is.na(fact)) {
    gdal.string <-"gdalwarp -te" % %   clip.region[1] % % clip.region[3] % % clip.region[2]  % % clip.region[4] % %  input.file % % output.file  } else
    {  gdal.string <-"gdalwarp -te" % %   clip.region[1] % % clip.region[3] % % clip.region[2]  % % clip.region[4] % %  "-tr" % %  cellsize[1] % %  cellsize[2]  % % input.file % % output.file}

  print(gdal.string)
  system(gdal.string)
  this.ras<- raster::raster(output.file)
  if ( is.na(crs(this.ras))) crs(this.ras)="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

  this.ras
}
# get_srtm3<- function(clip=c(136., 146, -30, -22),
#                      input.file="/Volumes/data/data/regional/australia/srtm3/oz_full_srtm3.tif",
#                      output.file=   tempfile(), #"/tmp/srtm_clip.tif",
#                      fact=NA  ){
#
#   library(gdalUtils)
#
#   gdalUtils::gdalinfo(input.file)
#   # dim.expression<-gdalUtils::gdalinfo(input.file)[11] %>%  str_replace("Pixel Size = ","cellsize=c")
#   dim.exp<-gdalUtils::gdalinfo(input.file,verbose=T)[11]
#   print(  dim.exp)
#   cellsize<-dim.exp %>%  str_remove("Pixel Size = " ) %>% str_remove("[(]") %>% str_remove("[)]") %>% str_split(",")  %>% unlist() %>% as.numeric()
#
#   print("input spacings are:" )
#
#   print(cellsize)
#
#   cellsize = cellsize*fact
#
#   print("output pacings are:")
#
#   print(cellsize)
#
#   if (file.exists(output.file) ) file.remove(output.file)
#   #clip <- c(136., 146, -30, -22)
#
#
#   #-te xmin ymin xmax ymax:
#   if (is.na(fact)) {
#     gdal.string <-"gdalwarp -te" % %   clip[1] % % clip[3] % % clip[2]  % % clip[4] % %  input.file % % output.file  } else
#     {gdal.string <-"gdalwarp -te" % %   clip[1] % % clip[3] % % clip[2]  % % clip[4] % %  "-tr" % %  cellsize[1] % %  cellsize[2]  % % input.file % % output.file}
#
#   print(gdal.string)
#   system(gdal.string)
#   raster::raster(output.file)
#
# }


#' ms idl topo colours Title
#'
#' @param n
#' @param scheme
#'
#' @return
#' @export
#'
#' @examples
idl.colours <-function(n=255, scheme=1){
    r_curr =  approx(c(80, 153, 175, 200, 255), n=n)$y / 255
    g_curr = approx(c(150,135, 150, 170, 240), n=n)$y /255
    b_curr = approx(c(0,   0,  75,  150, 180), n=n)$y /255
    if (scheme==1) return(rgb(r_curr, g_curr,b_curr)) else return(list(r=r_curr, g= g_curr,b=b_curr))
  }
idl.colors <-function(...) idl.colours(...)

#' order colours by hue ..
#'
#' @param s
#' @param f
#' @param n
#'
#' @return
#' @export
#'
#' @examples
get.hues <- function(s=280, f=380,n=11) {
  RGBColors <- col2rgb(colors()[1:length(colors())])
  HSVColors <- rgb2hsv( RGBColors[1,], RGBColors[2,], RGBColors[3,], maxColorValue=255)
  HueOrder  <- order( HSVColors[1,], HSVColors[2,], HSVColors[3,] )

  colors()[HueOrder[seq(s,f,length.out=n)]]
}



#' normalised coordinates  rectangle
#'
#' @param p  rectange as in smallplot definition
#' @param col
#' @param alpha
#' @param lwd
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
nrect<- function(p=c(.35, .65, .035, .065),col="white",alpha=1, lwd=0, from = "ndc", to = "user", ...) {
  col<- mmt::add.alpha(col, alpha)
  x<- grconvertX(p[1:2], from = from, to = to)
  y<- grconvertX(p[3:4], from = from, to = to)
  rect(x[1],y[1],x[2], y[2], col=col, lwd=lwd,...)
}



legend_boxes <- function(rr=c(.8, .85, .5, .75), cols =get.hues(n=10),
                         labels=NA, alpha=.6,cex=2.5 , hjust=-.1 , vjust=.5){

  #   nrect(rr +c(.01, .025,0,-.07), col="white", lwd=1, alpha=alpha)
  y.diff= diff(rr[3:4])/1/length(cols)

  ys <- rev(seq(rr[3]+y.diff/2, rr[4]-y.diff/2, length.out=length(cols)))
  print(ys)
  x <-  mean(rr[1:2])
  x.diff <-   diff(rr[1:2])/2
  x.f<-2.1
  y.f <- 2.1

  # print(x-x.diff/x.f  %>% grconvertX(from = "ndc", to = "user"))
  # print(ys[1]-y.diff/y.f %>% grconvertY(from = "ndc", to = "user"))
  # for (i in 1:length(cols))



  sx <-data.frame(x0 =x-x.diff/x.f +ys *0 ,
                  y0= ys-y.diff/y.f,
                  x1=x+x.diff/x.f +ys *0,
                  y1=ys+y.diff/y.f  )

  sx.con<-data.frame(
    x=sx$x0 %>% grconvertX( from = "ndc", to = "user"),
    y=sx$y0 %>% grconvertY( from = "ndc", to = "user"),
    x1=sx$x1 %>% grconvertX( from = "ndc", to = "user"),
    y1=sx$y1 %>% grconvertY( from = "ndc", to = "user"))

  #  rect( min(sx.con$x)-.01, min(sx.con$y)-.01, max(sx.con$x1)+.03,max(sx.con$y1) +.01, col= add.alpha("white",alpha), lwd=1)

  for (i in 1:length(sx[,1])){ rect( sx.con$x[i], sx.con$y[i],sx.con$x1[i],sx.con$y1[i],
                                     border="black",
                                     col=cols[i])
    if(!is.na(labels[1]) )
      text(sx.con$x1[i] , (sx.con$y[i]+sx.con$y1[i])/2, labels[i], adj=c(hjust,vjust), cex=cex )
  }


  return(sx.con)

  # rect(x-x.diff/x.f  %>% grconvertX(from = "ndc", to = "user"),
  #      ys[i]-y.diff/y.f %>% grconvertY(from = "ndc", to = "user"),
  #      x+x.diff/x.f %>% grconvertX(from = "ndc", to = "user"),
  #      ys[i]+y.diff/y.f%>% grconvertY(from = "ndc", to = "user"),
  #      border="black",
  #      col=cols[i])
}

poly_extent <- function(ex,
                        n=c(100,100),
                        offset=c(0,0) ,
                        proj = NA){

  if (length(n)==1) n= c(n,n)
  if (length(offset)==1) offset=c(offset,offset)
  if (!class(ex)=="Extent") {
    if (length(ex) ==4 & class(ex)=="numeric") ex = raster::extent(ex) else{
      message("no approriate extent value")
      return(NULL)}
  }
  xs = ex[1:2] + c(offset[1], -offset[1])
  ys=ex[3:4] + c(offset[2],-offset[2])

  #anticlockwise form lower left
  x1 <- seq(xs[1], xs[2],diff(xs)/(n[1]-1))
  y1 <- rep(ys[1], n[1])
  x2 <- rep(xs[2], n[2])
  y2 <- seq(ys[1], ys[2],diff(ys)/(n[2]-1))
  x3 <- rev(x1)
  y3 <- rep(ys[2], n[1])
  x4 <- rep(xs[1], n[2])
  y4 <- rev(y2)

  poly.s <- data.frame(x= c(x1,x2,x3,x4), y=c(y1,y2,y3,y4))


  if (is.na(proj)) return(poly.s) else
    return(convertPts(poly.s,to=proj))
}

#poly_extent(clip.region, proj=eyre.laea)
#poly_extent(clip.region, proj=NA)


