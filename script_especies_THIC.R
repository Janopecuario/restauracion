packages <- c("raster", "biomod2", "dismo","mgcv","terra",
              "rasterVis","gstat","shapefiles",
              "sp","ggfortify","reshape","spatialEco",
              "tidyverse","rgbif","sabinaNSDM","stringi",
              "CoordinateCleaner","sf","glmnet","data.table",
              "covsel","stars","readxl")

sapply(packages, require, character.only = TRUE, quietly = FALSE)

UTMproj <- "+proj=utm +zone=30 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
geoproj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
ruta_descargadas<-"P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/THIC/especies_descargadas.csv"
gbif_descargadas<-read.csv(ruta_descargadas,sep=",")
descargadas<-unique(gbif_descargadas$especie)

lista <- read_excel("P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/THIC/ListadoTipicas.xlsx") %>% 
  mutate(Habitual = as.factor(Habitual),
         Diagnostica = as.factor(Diagnostica), 
         Abundancia = as.factor(Abundancia))

list_EU <- read_excel("P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/THIC/typical_species_2013-2018.xlsx", 
                      sheet = 2) %>% 
  filter(country == "ES") %>% 
  select(3,4) %>% 
  distinct() %>% 
  drop_na() %>% rename(Especie=2, Habitat=1)
  
resolucion <- 10

malla <- raster("P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/ambientes/TMed.asc") %>% 
  projectRaster(crs = crs(geoproj))  %>% aggregate(fact=resolucion)

habitats <- list_EU$Habitat %>% unique()

# log de errores
error_log <- tibble(
  habitat = character(),
  especie = character(),
  error_msg = character()
)

for (h in habitats) {
  print(h)  
  ocurrencias <- tibble(
    x = vector("numeric"),
    y = vector("numeric"),
    especie = vector("character"),
    codigoHabitat = vector("character")
  )
  
  sp_hab <- filter(list_EU, Habitat == h)
  species <- sp_hab$Especie %>% unique()
  
  for (especie in species) {
    gbif_descargadas<-read.csv(ruta_descargadas,sep=",")
    descargadas<-unique(gbif_descargadas$especie)
    if ((especie %in% descargadas)) {
      print(paste(especie,"already downloaded")) 
      
      ocurrencias_temp <- gbif_descargadas %>% filter(especie==especie)
      ocurrencias_temp$codigoHabitat <- h
      
      colnames(ocurrencias_temp) <- colnames(ocurrencias)
      ocurrencias <- rbind(ocurrencias, ocurrencias_temp)
      
    } else {
      print(paste("Searching", especie))
      
      raw_occurrences <- tryCatch({
        occ_search(scientificName = especie,
                   country = "ES",
                   hasCoordinate = TRUE,
                   hasGeospatialIssue = FALSE,
                   limit = 8000,
                   fields=c("scientificName","decimalLatitude","decimalLongitude","coordinateUncertaintyInMeters",
                            "eventDate"))
      }, error = function(e) {
        message(paste("Error en", especie, ":", e$message))
        error_log <<- rbind(error_log, tibble(
          habitat = h,
          especie = especie,
          error_msg = e$message
        ))
        return(NULL)
      })
      
      if (is.null(raw_occurrences) || is.null(raw_occurrences$data)) next
      
      raw_occurrences_data <- raw_occurrences$data 
      
      if (!"eventDate" %in% names(raw_occurrences_data)) {
        raw_occurrences_data$eventDate <- NA
      }
      raw_occurrences_data <- raw_occurrences_data %>%
        dplyr::rename(
          especie = 1, 
          y = 2, 
          x = 3, 
          precision = coordinateUncertaintyInMeters,
          date = eventDate
        )
      
      raw_occurrences_data$especie <- especie
      
      filtered_occurrences_data <- raw_occurrences_data %>%  
        #filter(precision < 10000) %>%  
        #dplyr::select(x, y) %>% 
        relocate(x, .before = y) %>% relocate(especie,.after=y) %>% relocate(precision,.after=date)
      #to_save <- cbind(filtered_occurrences_data,especie)
      if (!file.exists(ruta_descargadas)) {
        write.table(filtered_occurrences_data, ruta_descargadas, sep = ",", row.names = FALSE, col.names = TRUE, append = FALSE)
      } else {
        write.table(filtered_occurrences_data, ruta_descargadas, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      }
      print(paste(especie, nrow(raw_occurrences_data), nrow(filtered_occurrences_data)))
      
      ocurrencias_temp <- filtered_occurrences_data %>% select(x,y,especie)
      ocurrencias_temp$codigoHabitat <- h
      
      colnames(ocurrencias_temp) <- c("x","y","especie","codigoHabitat")
      ocurrencias <- rbind(ocurrencias[1:4], ocurrencias_temp)
      print("sleeping system")
      Sys.sleep(180) # no saturar API
    }
  }
  
  riqueza <- malla
  values(riqueza) <- 0
  species <- unique(ocurrencias$especie)
  
  estructura <- "Fagus sylvatica"
  #species <- setdiff(species, estructura)
  
  for (s in species) {
    ocurrencias_temp <- filter(ocurrencias, especie == s)
    raster_temp <- rasterize(ocurrencias_temp[1:2], y = riqueza, field = 1, background = 0)
    riqueza <- riqueza + raster_temp
  }
  
  ocurrencias_estructura <- filter(ocurrencias, especie == estructura)
  raster_estructura <- rasterize(ocurrencias_estructura[1:2], y = riqueza, field = 1, background = 0)
  riqueza <- riqueza * raster_estructura
  raster::writeRaster(riqueza, paste0("h_", h,"_" ,resolucion,".tif"), overwrite = TRUE)
}



# guardar log de errores
if (nrow(error_log) > 0) {
  write_csv(error_log, "errores_gbif.csv")
  message("Se guardó el log de errores en errores_gbif.csv")
}
