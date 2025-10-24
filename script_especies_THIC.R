packages <- c("raster", "biomod2", "dismo","mgcv","terra",
              "rasterVis","gstat","shapefiles",
              "sp","ggfortify","reshape","spatialEco",
              "tidyverse","rgbif","sabinaNSDM","stringi",
              "CoordinateCleaner","sf","glmnet","data.table",
              "covsel","stars","readxl")

sapply(packages, require, character.only = TRUE, quietly = FALSE)
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

UTMproj <- "+proj=utm +zone=30 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
geoproj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
ruta_descargadas<-"P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/THIC/especies_descargadas.csv"
ruta_lista_descargadas<-"P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/THIC/lista_especies_descargadas.csv"


gbif_descargadas<-read.csv(ruta_descargadas,sep=",")
descargadas<-read.table(ruta_lista_descargadas,sep=",",header=TRUE)[,1]

lista <- read_excel("P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/THIC/ListadoTipicas.xlsx") %>% 
  mutate(Habitual = as.factor(Habitual),
         Caracter = as.factor(Caracter), 
         Abundancia = as.factor(Abundancia))

list_EU <- read_excel("P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/THIC/typical_species_2013-2018.xlsx") %>% 
  filter(country == "ES") %>% 
  select(3,4) %>% 
  distinct() %>% 
  drop_na() %>% dplyr::rename(Especie=species, Habitat=habitat)

resolucion <- 10
malla <- raster("P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/ambientes/TMed.asc") %>% 
  projectRaster(crs = crs(geoproj))  %>% aggregate(fact=resolucion)
habitats <- list_EU$Habitat %>% unique()
# lista_6310 <- read_excel("P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/THIC/typical_species_2013-2018.xlsx") %>% 
#   filter(country=="ES" & habitat=="6310") %>% select(species) %>% unique()

species <- list_EU #%>% filter(Habitat=="5220")
species <- species$Especie %>% unique() %>% sort()
for (especie in species) {
  descargadas <- read.table(ruta_lista_descargadas, sep = ",", header = TRUE)[,1]
  
  if ((especie %in% descargadas)) {
    print(paste(especie, "already downloaded")) 
    
  } else {
    print(paste("Searching", especie))
    
    raw_occurrences <- tryCatch({
      occ_search(scientificName = especie,
                 country = "ES",
                 hasCoordinate = TRUE,
                 hasGeospatialIssue = FALSE,
                 limit = 8000)
    }, error = function(e) {
      message(paste("Error en", especie, ":", e$message))
      # error_log <<- rbind(error_log, tibble(
      #   habitat = h,
      #   especie = especie,
      #   error_msg = e$message
      #)
    
      return(NULL)
    })
    
    if (is.null(raw_occurrences) || is.null(raw_occurrences$data)) next
    
    raw_occurrences_data <- raw_occurrences$data
    raw_occurrences_data <- clean_occ(raw_occurrences_data)
    
    if (is.null(raw_occurrences_data) || nrow(raw_occurrences_data) == 0) {
      message(paste("⚠️ No hay ocurrencias válidas para", especie, "- se salta."))
      next
    }
    filtered_occurrences_data <- raw_occurrences_data %>% 
      dplyr::rename(date = year, precision = unc, especie = species) %>% 
      select(x, y, especie, date, precision)
    
    raw_occurrences_data$especie <- especie
    
    # 📥 Guardar las ocurrencias descargadas
    if (!file.exists(ruta_descargadas)) {
      write_csv(filtered_occurrences_data, ruta_descargadas, append = FALSE)
    } else {
      write_csv(filtered_occurrences_data, ruta_descargadas, append = TRUE)
    }
    
    # 🆕 👉 Añadir la especie recién descargada a la lista de especies descargadas
    # (si el archivo no existe, lo crea con cabecera)
    if (!file.exists(ruta_lista_descargadas)) {
      write.table(data.frame(Especie = especie),
                  ruta_lista_descargadas,
                  sep = ",",
                  row.names = FALSE,
                  col.names = TRUE,
                  append = FALSE)
    } else {
      write.table(data.frame(Especie = especie),
                  ruta_lista_descargadas,
                  sep = ",",
                  row.names = FALSE,
                  col.names = FALSE,  # importante: no repetir cabecera
                  append = TRUE)
    }
    
    print(paste("✔ Especie añadida a la lista:", especie))
    print("sleeping system")
    Sys.sleep(180) # no saturar API
  }
}

