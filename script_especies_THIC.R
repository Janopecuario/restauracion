packages<-c("raster", "biomod2", "dismo","mgcv","terra",
            "rasterVis","gstat","shapefiles",
            "sp","ggfortify","reshape","spatialEco",
            "tidyverse","rgbif","sabinaNSDM","stringi",
            "CoordinateCleaner","sf","glmnet","data.table","covsel","stars","readxl")
sapply(packages, require, character.only=T, quietly = FALSE)

lista<-read_excel("P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/THIC/ListadoTipicas.xlsx") %>% 
  mutate(Habitual=as.factor(Habitual),
         Diagnostica=as.factor(Diagnostica), Abundancia=as.factor(Abundancia))

habitats<-lista$Habitat %>% unique()


for (h in habitats){
print(h)  
  ocurrencias<-tibble(
    x = vector("numeric"),
    y = vector("numeric"),
    especie = vector("character"),
    codigoHabitat = vector("character")
  )
  
  sp_hab <- filter(lista,Habitat == h)
  species<-sp_hab$Especie
  for (especie in species) {
    print(paste("Searching",especie))
    raw_occurrences <- occ_search(scientificName=especie,
                                  country = "ES",
                                  hasCoordinate = TRUE,
                                  hasGeospatialIssue= FALSE,
                                  limit = 8000,
                                  fields=c("scientificName","decimalLatitude",
                                           "decimalLongitude","coordinateUncertaintyInMeters"))
    raw_occurrences_data <- raw_occurrences$data
    raw_occurrences_data <- raw_occurrences_data %>%
      dplyr::rename(y=2,x=3,precision=4)
    print(paste(especie, nrow(raw_occurrences_data)))
    filtered_occurrences_data <- raw_occurrences_data %>%  
      filter (precision < 1000) %>%  ##decidir sobre la precisión del filtro
      dplyr::select(x,y) %>% 
      relocate(x, .before=y)
    print(paste(especie, nrow(raw_occurrences_data),nrow(filtered_occurrences_data)))
    
    ocurrencias_temp<-cbind(filtered_occurrences_data,especie,h)
    colnames(ocurrencias_temp) <- colnames(ocurrencias)
    ocurrencias<-rbind(ocurrencias,ocurrencias_temp)
    
    print(paste0("writing",especie))
    
  }
  print(paste("writing ",especie))
  write_csv(ocurrencias,paste0("P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/THIC/EspeciesTHIC_GBIF/", 
  h, ".csv"))
}

filtered_occurrences_data <- raw_occurrences_data %>%  
  #filter (precision < 1000) %>%  ##decidir sobre la precisión del filtro
  dplyr::select(x,y) %>% 
  relocate(x, .before=y)

write_csv(filtered_occurrences_data,paste0("P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/THIC/EspeciesTHIC_GBIF/", 
                 especie, ".csv"))

