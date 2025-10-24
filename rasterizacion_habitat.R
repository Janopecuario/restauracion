setwd("P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/THIC")

packages<-c("raster", "terra",
            "rasterVis","sf",
            "tidyverse","readxl")
sapply(packages, require, character.only=T, quietly = FALSE)


extent_regional<-extent(c(-10,4.28,34,44.5)) #coordenadas peninsula + NAfrica
malla <- raster("P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/worldclim/wc2.1_30s_bio_1.tif") %>%
  crop(extent_regional) %>% rast()
malla[!is.na(malla)] <- 0
codigo_habitat<- "5220" 

pastos<-"P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/THIC/CBP_MA_P_20250905.gdb/CBP_MA_P_20250905.gdb"
bosques<- "P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/THIC/CBP_MA_B_20250905.gdb"
matorrales<-"P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/THIC/matorrales.gdb"
layer_habitat<-"CBP_MA_M_PB_20250919b"
query_txt <- sprintf("SELECT * FROM %s WHERE HIC1 = '%s'", 
                     layer_habitat,
                     codigo_habitat)  
habitat<-vect(matorrales,
              layer = layer_habitat, 
              #n_max = 0, 
              query = query_txt
)
habitat_reproject<-terra::project(habitat, terra::crs(malla))

habitat_rast<- terra::rasterize(x=habitat_reproject,y=malla)

raster::writeRaster(habitat_rast,filename=paste0("Habitat_",codigo_habitat,".tif"),overwrite=TRUE)
habitat_table<-as.data.frame(habitat_rast,xy=TRUE)
write.csv(habitat_table,file=paste0("Habitat_",codigo_habitat,".csv"),row.names=FALSE)

