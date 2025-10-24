packages<-c("raster", "terra","tidyverse", "rasterVis","readxl","crayon")
sapply(packages, require, character.only=T, quietly = FALSE)

setwd("P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/Modelos")

especies_BBEE<-read_excel("P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/THIC/ListadoTipicas.xlsx") %>% 
  mutate(Estructurante = as.factor(Estructurante),
         Caracter = as.factor(Caracter), 
         Abundancia = as.factor(Abundancia))

especies_EU<-read_excel("P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/THIC/typical_species_2013-2018.xlsx") %>% 
  filter(country=="ES")

habitats<-especies_BBEE$Habitat %>% unique()
especies_modelizadas<-list.files(pattern = "\\_ROC.tif$") %>% 
  str_replace_all(pattern="_Covariate_model_bin_ROC.tif",
                  replacement= "") %>%  unique()

# Ensamblaje por habitat----
for (h in habitats){
  message(h)
  #coger todas las especies de ambos listados
  especies_BBEE_habitat<- filter(especies_BBEE, especies_BBEE$Habitat==h)
  indice_estructurante<-which(especies_BBEE_habitat$Estructurante=="Sí")
  if(length(indice_estructurante)>0){
    estructurante<-especies_BBEE_habitat %>% filter(Estructurante=="Sí") %>% 
      select(Especie) %>% unlist() %>% as.vector()
  } else{
    estructurante<-NULL
  }
  
  especies_BBEE_habitat<-especies_BBEE_habitat$Especie %>% unique()
  
  #mi listado EU
  especies_EU_habitat<- filter(especies_EU, habitat==h) 
  especies_EU_habitat<-especies_EU_habitat$species %>% unique()
  
  # unir ambos listados
  especies_ensemble<-c(especies_BBEE_habitat, especies_EU_habitat) %>% unique()
  
  #quitar las que no tengo modelo
  especies_habitat_index<-which(especies_ensemble %in% especies_modelizadas)
  especies_habitat<-especies_ensemble[especies_habitat_index]
  n_especies<-length(especies_habitat)
  
  # Raster base
  raster_ensamblado<-raster("Fagus sylvatica_Covariate_model_bin_ROC.tif")
  raster_ensamblado[] <- ifelse(is.na(raster_ensamblado[]), NA, 0)
  raster_estructura<-raster_ensamblado
  
  for(eh in especies_habitat){
    cat(italic(red(paste0(eh,"\n"))))
    temp_raster<-raster(paste0(eh,"_Covariate_model_bin_ROC.tif"))
    raster_ensamblado<-raster_ensamblado+temp_raster
  }
  # Normalizar para visualizar
  r<- raster_ensamblado
  r_norm <- (r - cellStats(r, "min")) / (cellStats(r, "max") - cellStats(r, "min"))
  plot(r_norm, main=paste0(h,"\n","Sumatorio, N= " ,n_especies))
  # 🔸 Guardar la versión sin estructura (solo sumatorio)
  writeRaster(r_norm,
              paste0("habitat_",h,"_sumatorio.asc"),
              overwrite=TRUE)
  
  # 🔸 Ahora aplicar estructura si existe
  if(!is.null(estructurante)){
    for (re in estructurante){
      cat(italic(bold(paste0(re,"\n"))))
      temp_raster_estructura<-raster(paste0(re,"_Covariate_model_bin_ROC.tif"))
      raster_estructura<-raster_estructura+temp_raster_estructura
    }
    c
    raster_estructura <- calc(raster_estructura, function(x) ifelse(x >= 1, 1, 0))
    raster_ensamblado<-(raster_ensamblado)*raster_estructura
  }
  
  # Normalizar para visualizar
  r_e<- raster_ensamblado
  r_e_norm <- (r_e - cellStats(r_e, "min")) / (cellStats(r_e, "max") - cellStats(r_e, "min"))
  cat(green("Writing raster con estructura\n"))
  writeRaster(r_e_norm,
              paste0("habitat_",h,"_ensamblado.asc"),
              overwrite=TRUE)
  plot(r_e_norm, main=paste0(h,"\n","estructura, N= " ,n_especies))
}
# FIN