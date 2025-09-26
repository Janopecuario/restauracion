ruta_ambientes<-"P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/ambientes"
ambientes <- list.files(path=ruta_ambientes,
                        pattern=".shp")
ambientes<-ambientes[3]

library(sf)
library(tidyverse)11
library(terra)

ambientes <- st_read(paste0(path_ambientes,"/",ambientes), quiet = TRUE) 
variables<-colnames(ambientes)[92:105]
ambientes <-st_transform(ambientes,crs=CRS(geoproj))
ext(biovars_regional)
mask<- st_read("~/restauracion/mask.shp") 
mask<- mask %>% rasterize(rast(ext = ext(mask),
                               resolution=1000,
                               crs = crs(ambientes)
                               
), 
field = "OBJECTID", 
# fun = mean, 
# background = NA, 
na.rm = TRUE)
for (v in variables){
  print(v)
  variable_temp<-rasterize(vect(ambientes), 
                           rast(ext = ext(mask),crs=crs(ambientes),resolution=1000), 
                           field = v, 
                           #fun = mean, 
                           #background = NA, 
                           na.rm = TRUE) %>% mask(mask) %>% crop(mask)
  
  writeRaster(variable_temp,filename=paste0(v,".asc"),overwrite=TRUE)
  
}
biovariables_list<-list.files(pattern="\\.asc$")
biovars<-raster::stack()
for (b in biovariables_list){
  print(b)
  temp_raster<-raster(b)
  biovars<-stack(biovars,temp_raster)
}