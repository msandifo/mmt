library(magrittr)
library(tidyverse)
library(lubridate)

read_gq <-function(file="data/GGcat Australia M2.5 plus-170406.xlsx",
ver=F){

readxl::read_excel(file, sheet=2) %>%
  mutate( cdate=date_decimal(YearF)) %>%
  dplyr::select(cdate,
         ddate= YearF,
         Longitude=LongitudeF,
         Latitude=LatitudeF,
         z=Depth,
         M=Mval,
         Mx=Mx) ->
  this.data

if (ver) glimpse(this.data)

return(this.data)

}


read_gq()->gq

