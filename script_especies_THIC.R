packages<-c("raster", "biomod2", "dismo","mgcv","terra",
            "rasterVis","gstat","shapefiles",
            "sp","ggfortify","reshape","spatialEco",
            "tidyverse","rgbif","sabinaNSDM","stringi",
            "CoordinateCleaner","sf","glmnet","data.table","covsel","stars","readxl")
sapply(packages, require, character.only=T, quietly = FALSE)

UTMproj<-"+proj=utm +zone=30 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
geoproj<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

lista<-read_excel("P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/THIC/ListadoTipicas.xlsx") %>% 
  mutate(Habitual=as.factor(Habitual),
         Diagnostica=as.factor(Diagnostica), Abundancia=as.factor(Abundancia))

malla<-raster("P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/THIC/ambientes/Tmed.asc") %>% 
projectRaster(crs=crs(geoproj))  

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
    
    
  }
  riqueza = malla %>% raster__aggregate(fact=5)
  values(riqueza) <-0
  species<-unique(ocurrencias$especie)
  estructura<-"Fagus sylvatica"
  species <- setdiff(species, estructura)
  for (s in species){
    ocurrencias_temp<-filter(ocurrencias,especie == s)
    raster_temp<-rasterize(ocurrencias_temp[1:2],y=caracter, field=1, background=0)
    #plot(raster_temp,main=s)
    riqueza=riqueza+raster_temp
    #plot(caracter)
  }
  ocurrencias_estructura<-filter(ocurrencias,especie == estructura)
  raster_estructura<-raster_temp<-rasterize(ocurrencias_estructura[1:2],y=aggregate(malla,fact=5), field=1, background=0)
  riqueza<-raster*estructura
  raster::writeRaster(riqueza,paste0("h,.tif"),overwrite=TRUE)
}








ocurrencias_estructura<-filter(ocurrencias,especie == estructura)
raster_estructura<-raster_temp<-rasterize(ocurrencias_estructura[1:2],y=malla, field=1, background=0)
plot(caracter)
caracter <- caracter*raster_estructura
raster::writeRaster(caracter,"caracter1000.tif")

plot(malla)
ocurrencias %>% filter(especie== "Meconopsis cambrica") %>% select(x,y) %>% points(col="red")
ocurrencias %>% filter(especie== "Ulmus glabra") %>% select(x,y) %>% points(col="black")
writeRaster(caracter,"THIC9130.tif")
