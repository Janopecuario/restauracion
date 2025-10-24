# 0. Constantes globales ----

packages<-c("raster", "biomod2", "dismo","mgcv","terra",
            "rasterVis","gstat","shapefiles",
            "sp","ggfortify","reshape","spatialEco",
            "tidyverse","rgbif","sabinaNSDM","stringi","crayon",
            "CoordinateCleaner","sf","glmnet","data.table","covsel","stars","readxl")
sapply(packages, require, character.only=T, quietly = FALSE)
setwd("P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/Modelos")
#CRSs por si hay que reproyectar
UTMproj<-"+proj=utm +zone=30 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
geoproj<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
extent_regional<-extent(c(-10,4.28,34,44.5)) #coordenadas peninsula + NAfrica
extent_global<-extent(c(-16.47,93.53,18.24,78))
ruta_worldclim<- "P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/worldclim"
background<-read_csv("background.csv")
ruta_descargadas_global <- "P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/THIC/global_especies_descargadas.csv"
ruta_descargadas<-"P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/THIC/especies_descargadas.csv"
ruta_lista_descargadas<-"P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/THIC/lista_especies_descargadas.csv"
ruta_litologia<-"P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/Litologia"
ruta_tpi<-"P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/ambientes/tpi.tif"
## 0.1 Rutas globales----
## 0.1.1 CORINE LAND COVER background----
#codigo para generar el background con CORINE LAND COVER
# ruta_clc<-"~/Corine/clc_UTM.tif"
# m <- matrix(c(
#   1, 1, 1,   # ID=1 → 1
#   2, Inf, 0  # cualquier valor mayor que 1 → 0
# ), ncol = 3, byrow = TRUE)
# clc <- raster(ruta_clc) %>% aggregate(fact=10, fun=modal)
# clc <- projectRaster(clc,crs=crs(geoproj)) 
# background <-as.data.frame(clc,xy=TRUE) %>% 
# filter(clc_UTM == 1) %>% select(-c(clc_UTM))
# write_csv(background,"~/restauracion/background.csv")

## 0.2 Carga de predictores----
### 0.2.1 Worldlim global----

slope_global<-raster("P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/ambientes/slope_global.tif")
tpi<-raster(ruta_tpi)
slope_regional<-raster("P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/ambientes/slope_regional.tif")
worldclim_vars<-list.files(path="P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/worldclim",
                           pattern="^wc.*\\.tif$")

# Limpieza de datos de ocurrencia de especies (GBIF, iNaturalist, etc.)

clean_occ <- function(d){
  
  if (is.null(d) || !nrow(d)) return(NULL)
  
  # Asegurar columnas mínimas; si faltan, créarlas con NA
  
  if (!"decimalLongitude" %in% names(d)) d$decimalLongitude <- NA_real_
  
  if (!"decimalLatitude"  %in% names(d)) d$decimalLatitude  <- NA_real_
  
  if (!"year"             %in% names(d)) d$year             <- NA_integer_
  
  if (!"coordinateUncertaintyInMeters" %in% names(d)) d$coordinateUncertaintyInMeters <- NA_real_
  
  if (!"basisOfRecord"    %in% names(d)) d$basisOfRecord    <- NA_character_
  
  if (!"establishmentMeans"%in% names(d)) d$establishmentMeans <- NA_character_
  
  if (!"species"          %in% names(d)) d$species          <- NA_character_
  
  # data.frame 
  
  d <- data.frame(
    
    x    = as.numeric(d$decimalLongitude),
    
    y    = as.numeric(d$decimalLatitude),
    
    year = as.integer(d$year),
    
    unc  = suppressWarnings(as.numeric(d$coordinateUncertaintyInMeters)),
    
    bor  = as.character(d$basisOfRecord),
    
    est  = as.character(d$establishmentMeans),
    
    species = as.character(d$species),
    
    stringsAsFactors = FALSE
    
  )
  
  # Filtros suaves 
  
  d <- d[is.finite(d$x) & is.finite(d$y), , drop=FALSE]
  
  if (!nrow(d)) return(NULL)
  
  d <- d[
    
    (is.na(d$bor) | !(tolower(d$bor) %in% c("fossil_specimen"))) &
      
      (is.na(d$est) | !(tolower(d$est) %in% c("introduced","invasive","managed"))) &
      
      (is.na(d$unc) | d$unc <= 1000) &                                   # ≤ 1 km
      
      (is.na(d$year) | d$year >= 1990),                                 # año mínimo
    
    , drop=FALSE]
  
  if (!nrow(d)) return(NULL)
  
  # Limpieza espacial con CoordinateCleaner
  
  flags <- CoordinateCleaner::clean_coordinates(
    
    x = d, lon = "x", lat = "y",
    
    tests = c("capitals","centroids","equal","gbif","institutions","zeros","seas"),
    
    value = "flagged"    # <- devuelve data.frame con columnas por test + .summary
    
  )
  
  # Compatibilidad: si alguna versión devolviera vector lógico, convertir a data.frame
  
  if (!is.data.frame(flags)) {
    
    flags <- data.frame(.summary = as.logical(flags))
    
  }
  
  # Asegurar que existen todas las columnas de tests usadas
  
  need <- c("capitals","centroids","equal","gbif","institutions","zeros","seas",".summary")
  
  for (nm in need) if (!nm %in% names(flags)) flags[[nm]] <- FALSE
  
  # Mantener 'buenos' y también los que SOLO fallan por 'seas'
  
  only_seas_bad <- (!flags$.summary) & flags$seas &
    
    !flags$capitals & !flags$centroids & !flags$equal &
    
    !flags$gbif & !flags$institutions & !flags$zeros
  
  keep <- flags$.summary | only_seas_bad
  
  # Guardar la marca 'seas' 
  
  d$flag_seas <- flags$seas
  
  # Aplicar filtro y devolver
  
  d <- d[keep, , drop = FALSE]
  
  if (!nrow(d)) return(NULL)
  
  d
  
}

#correr una vez para toda la sesión----
#lineas para recortar worldclim a escala europea, ya realizado
# for (w in worldclim_vars){
#   print(w)
#   bio_temp<-raster(w) %>% crop(y=extent_global)
#   writeRaster(bio_temp,w,overwrite=TRUE)
# }
#generacion de los objetos a usar como predictores
biovars_global<-stack()
for (w in worldclim_vars){
  cat(green(paste0(w,"\n")))
  bio_temp<-raster(paste0(ruta_worldclim,"/",w))
  biovars_global<-stack(biovars_global,bio_temp)
}
biovars_global<-stack(biovars_global,slope_global,tpi)
### 0.2.2 Worldclim regional y litología ----
biovars_regional<-crop(biovars_global,
                       extent_regional)
litologia_files<-list.files(ruta_litologia,pattern="resampled")
litologia<-stack()
for(lit in litologia_files){
  temp_raster<-raster(paste0(ruta_litologia,"/",lit))
  print(paste0(lit,"\n"))
  litologia<-stack(litologia, temp_raster)
}
biovars_regional<-stack(biovars_regional,litologia)
expl.var.global <- terra::rast(biovars_global)
expl.var.regional <- terra::rast(biovars_regional)
# 0.3 Carga escenarios climáticos----
ruta_escenarios<-"P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/Escenarios/"
# bases <- c("wc2.1_30s_bioc_") #añadir base CHELSA si hace falta
# GCM<- c("ACCESS-CM2")
ssp<- c(126,245,370,585) %>% as.character()
# periods <- c("2041-2060")
# 
# for (g in GCM){
#   for (s in ssp)  {
#     for (p in periods){
#       rutacompleta<-paste0(ruta_escenarios,
#                            bases,g,"_ssp",
#                            s,"_",p,".tif")
#       escenario<-rast(rutacompleta) %>% 
#         crop(y=extent_regional)
#       writeRaster(escenario,rutacompleta,format="GTiff",overwrite=TRUE)
#       
#     }
#   }
# }
#generacion de los escenarios climáticos ACCESS 2041-2060.
env_ssp126 <- rast(paste0(ruta_escenarios,"wc2.1_30s_bioc_ACCESS-CM2_ssp126_2041-2060.tif"))
env_ssp245 <- rast(paste0(ruta_escenarios,"wc2.1_30s_bioc_ACCESS-CM2_ssp245_2041-2060.tif"))
env_ssp370 <- rast(paste0(ruta_escenarios,"wc2.1_30s_bioc_ACCESS-CM2_ssp370_2041-2060.tif"))
env_ssp585 <- rast(paste0(ruta_escenarios,"wc2.1_30s_bioc_ACCESS-CM2_ssp585_2041-2060.tif"))
env_ssp126 <- c(env_ssp126, rast(litologia))
env_ssp245 <- c(env_ssp245, rast(litologia))
env_ssp370 <- c(env_ssp370, rast(litologia))
env_ssp585 <- c(env_ssp585, rast(litologia))
escenarios <- list(env_ssp126, env_ssp245, env_ssp370, env_ssp585)

## 0.4 Carga de los nombres de especies----

lista <- read_excel("P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/THIC/ListadoTipicas.xlsx") %>% 
  mutate(Abundancia = as.factor(Abundancia),
         Caracter = as.factor(Caracter), 
         Estructurante = as.factor(Estructurante))
especies<-lista$Especie %>% unique()

modelizadas<-list.files(pattern = "\\_ROC.tif$") %>% 
  str_replace_all(pattern="_Covariate_model_bin_ROC.tif",
                  replacement= "") %>%  unique()

# 1. Modelización----
gbif_descargadas<-read.csv(ruta_descargadas,sep=",")
descargadas<-gbif_descargadas$especie %>% unique()
descargadas_global<-read.csv(ruta_descargadas_global) #leemos los datos del archivo de ocurrencias globales
especies_descargadas_global<-descargadas_global$especie%>% unique() #especies que tienen los datos gbif descargados
## 1.0 Bucle por especies ----
for(e in especies){
  if (!(e %in% modelizadas)){
    ok <- tryCatch({
      ## --- 1.1 Búsqueda de GBIF global ----
      print(paste0("Modelling ", e))
      
      if (!(e %in% especies_descargadas_global)){
        print("Searching global")
        raw_occurrences_global <- occ_search(
          scientificName = e,
          hasCoordinate = TRUE,
          hasGeospatialIssue = FALSE,
          limit = 8000)
        
        occurrences_data_global <- raw_occurrences_global$data
        occurrences_data_global <- clean_occ(occurrences_data_global)
        occurrences_data_global <- occurrences_data_global %>% 
          rename(date=year,precision=unc,especie=species)%>% 
          
          select(x,y,especie,date,precision)
        occurrences_data_global$especie <- e
        
        if (!file.exists(ruta_descargadas_global)) {
          write.table(occurrences_data_global, ruta_descargadas_global,
                      sep = ",", row.names = FALSE, col.names = TRUE, append = FALSE)
        } else {
          write.table(occurrences_data_global, ruta_descargadas_global,
                      sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
        }
        
        xy.global <- occurrences_data_global %>% dplyr::select(x, y)
      } else {
        xy.global <- read.csv(ruta_descargadas_global) 
        xy.global <- filter(xy.global, especie == e) %>% select(x, y)  
      }
      
      ## --- 1.2 Búsqueda de GBIF regional ----
      #descargar los datos si la especie no se ha descargado previamente con
      #script_species_THIC.R
      if ((e %in% descargadas)) {
        print(paste0(e," already downloaded"))
        xy.regional <- gbif_descargadas %>% 
          filter(especie == e) %>% 
          select(x, y)
      } else {
        print("Searching regional")
        raw_occurrences <- tryCatch({
          occ_search(scientificName = e,
                     country = "ES",
                     hasCoordinate = TRUE,
                     hasGeospatialIssue = FALSE,
                     limit = 8000)
        }, error = function(e) {
          message(paste("Error en", e, ":", e$message))
          error_log <<- rbind(error_log, tibble(
            habitat = h,
            especie = e,
            error_msg = e$message
          ))
          return(NULL)
        })
        
        if (is.null(raw_occurrences) || is.null(raw_occurrences$data)) next
        
        raw_occurrences_data <- raw_occurrences$data
        raw_occurrences_data <- clean_occ(raw_occurrences_data)
        
        filtered_occurrences_data<-raw_occurrences_data %>% 
          rename(date=year,precision=unc,especie=species)%>% 
          
          select(x,y,especie,date,precision)
        
        if (!"eventDate" %in% names(raw_occurrences_data)) {
          raw_occurrences_data$eventDate <- NA
        }
        if (!"coordinateUncertaintyInMeters" %in% names(raw_occurrences_data)) {
          raw_occurrences_data$coordinateUncertaintyInMeters <- NA
        }
        
        
        raw_occurrences_data$especie <- e
        
        if (!file.exists(ruta_lista_descargadas)) {
          write.table(e, ruta_lista_descargadas,
                      sep = ",", row.names = FALSE, col.names = TRUE, append = FALSE)
        } else {
          write.table(e, ruta_lista_descargadas,
                      sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
        }
        
        if (!file.exists(ruta_descargadas)) {
          write_csv(filtered_occurrences_data, ruta_descargadas, append = FALSE)
        } else {
          write_csv(filtered_occurrences_data, ruta_descargadas, append = TRUE)
        }
        
        xy.regional <- filtered_occurrences_data %>% dplyr::select(x, y)
      }  
      
      ## --- 1.3 Modelización ----
      #si el nombre de la especie no es temp, sabina da error.
      print("nsdm input data")
      nsdm_input <- NSDM.InputData(
        SpeciesName = "temp",
        spp.data.global = xy.global, 
        spp.data.regional = xy.regional,
        expl.var.global = expl.var.global,
        expl.var.regional = expl.var.regional,
        #activar si se quieren proyectar los escenarios
        #new.env = escenarios,
        #new.env.names = ssp,
        Background.Global = NULL, 
        Background.Regional = background
      )
      
      print("nsdm finput data")
      nsdm_finput <- NSDM.FormattingData(
        nsdm_input,
        nPoints = nrow(xy.global),
        Min.Dist.Global = "resolution",
        Min.Dist.Regional = "resolution",
        Background.method = "random",
        save.output = TRUE
      )
      
      print("selecting vars")
      nsdm_selvars <- NSDM.SelectCovariates(
        nsdm_finput,
        maxncov.Global = "nocorr",
        maxncov.Regional = "nocorr",
        corcut = 0.7,
        algorithms = c("glm","gam","rf"),
        ClimaticVariablesBands = NULL,
        save.output = TRUE)
      
      print("nsdm global")
      nsdm_global <- NSDM.Global(
        nsdm_selvars,
        algorithms = c("GAM","GBM", "RF","GLM"),
        CV.nb.rep = 4,
        CV.perc = 0.8,
        metric.select.thresh = 0.7,
        CustomModelOptions = NULL,
        save.output = TRUE, 
        rm.biomod.folder = FALSE
      )
      
      print("nsdm regional")
      nsdm_regional <- NSDM.Regional(
        nsdm_selvars,
        algorithms = c("GAM","GBM", "RF","GLM"),
        CV.nb.rep = 2,
        CV.perc = 0.8,
        metric.select.thresh = 0.7,
        CustomModelOptions = NULL, 
        save.output = FALSE,
        rm.biomod.folder = FALSE
      )
      
      print("covariate model")
      nsdm_covariate <- NSDM.Covariate(
        nsdm_global,
        algorithms = c("GAM","GBM", "RF", "GLM"),
        rm.corr = TRUE,
        CV.nb.rep = 4,
        CV.perc = 0.8,
        metric.select.thresh = 0.7,
        CustomModelOptions = NULL,
        save.output = FALSE,
        rm.biomod.folder = FALSE
      )
      
      regional_model <- terra::rast(nsdm_regional$current.projections$Pred)
      plot(regional_model)
      Covariate.model <- terra::rast(nsdm_covariate$current.projections$Pred)
      writeRaster(
        Covariate.model,
        filename = paste0(e,"_Covariate_model.tif"),
        overwrite = TRUE
      )
      
      nsdm_covariate$current.projections$Pred.bin.ROC %>% terra::unwrap() %>% 
        writeRaster(
          filename = paste0(e,"_Covariate_model_bin_ROC.tif"),
          overwrite = TRUE
        )
      
      plot(Covariate.model, main = e)
      points(xy.regional)
      
      TRUE
    }, error = function(err){
      message(paste("⚠️ Error con la especie:", e, "->", err$message))
      FALSE 
    })
    
    if (!ok) next
  }
}
