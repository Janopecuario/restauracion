setwd("P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/Litologia")
ruta_litologia<-"P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/Litologia"
packages<-c("raster", "terra",
            "rasterVis","sf",
            "tidyverse","readxl")

sapply(packages, require, character.only=T, quietly = FALSE)

extent_regional<-extent(c(-10,4.28,34,44.5)) #coordenadas peninsula + NAfrica
malla<-raster("P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/worldclim/wc2.1_30s_bio_1.tif") %>% 
  crop(extent_regional)

# Cargar datos
suelos<-list.files(pattern=".tif$")
for (s in suelos){
  cli_alert_success("Reading {s}")
  temp_raster<-raster(s) %>% resample(malla, method="bilinear")
  cat(red("resampling\n"))
  temp_raster[temp_raster==0]<-NA  
  plot(temp_raster,main=s)
  message(red$bold("Writing"))
  writeRaster(temp_raster, filename=paste0("resampled_",s), format="GTiff", overwrite=TRUE)
}
