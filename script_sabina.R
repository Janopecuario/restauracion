# 0. Constantes globales ----
packages<-c("raster", "biomod2", "dismo","mgcv","terra",
            "rasterVis","gstat","shapefiles",
            "sp","ggfortify","reshape","spatialEco",
            "tidyverse","rgbif","sabinaNSDM","stringi",
            "CoordinateCleaner","sf","glmnet","data.table","covsel","stars","readxl")
sapply(packages, require, character.only=T, quietly = FALSE)

#CRSs por si hay que reproyectar
UTMproj<-"+proj=utm +zone=30 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
geoproj<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
extent_regional<-extent(c(-10,4.28,34,44.5)) #coordenadas peninsula + NAfrica
extent_global<-extent(c(-16.47,93.53,18.24,78))

## 0.1 Rutas globales----
# Ir metiendo rutas aquí según empecemos a mover los datos
## 0.1.1 CORINE LAND COVER background----
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
background<-read_csv("~/restauracion/background.csv")
gbif_global <- "P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/THIC/global_especies_descargadas.csv"
ruta_descargadas<-"P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/THIC/especies_descargadas.csv"
## 0.2 Carga de predictores----
setwd("~/restauracion") #moverlo a la red de tragsa

worldclim_vars<-list.files(pattern="\\.tif")

#correr una vez para toda la sesión
#lineas para recortar worldclim a escala europea
# for (w in worldclim_vars){
#   print(w)
#   bio_temp<-raster(w) %>% crop(y=extent_global)
#   writeRaster(bio_temp,w,overwrite=TRUE)
# }
biovars_global<-stack()
for (w in worldclim_vars){
  print(w)
  bio_temp<-raster(w)# %>% crop(y=extent_global) #ya cortadas
  biovars_global<-stack(biovars_global,bio_temp)
}
biovars_regional<-stack()
for (w in worldclim_vars){
  print(w)
  bio_temp<-raster(w) %>% crop(y=extent_regional)
  biovars_regional<-stack(biovars_regional,bio_temp)
}

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

variable_names<-names(expl.var.regional)
env_ssp126 <- rast(paste0(ruta_escenarios,"wc2.1_30s_bioc_ACCESS-CM2_ssp126_2041-2060.tif")) %>% 
  crop(extent_regional)
names(env_ssp126)<-variable_names
terra::writeRaster(env_ssp126,paste0(ruta_escenarios,"wc2.1_30s_bioc_ACCESS-CM2_ssp126_2041-2060.tif"),
                   overwrite=TRUE)

env_ssp245 <- rast(paste0(ruta_escenarios,"wc2.1_30s_bioc_ACCESS-CM2_ssp245_2041-2060.tif"))%>% 
  crop(extent_regional)
names(env_ssp245)<-variable_names
terra::writeRaster(env_ssp245,paste0(ruta_escenarios,"wc2.1_30s_bioc_ACCESS-CM2_ssp245_2041-2060.tif"),
                   overwrite=TRUE)

env_ssp370 <- rast(paste0(ruta_escenarios,"wc2.1_30s_bioc_ACCESS-CM2_ssp370_2041-2060.tif"))%>% 
  crop(extent_regional)
names(env_ssp370)<-variable_names
terra::writeRaster(env_ssp370,paste0(ruta_escenarios,"wc2.1_30s_bioc_ACCESS-CM2_ssp370_2041-2060.tif"),
                   overwrite=TRUE)

env_ssp585 <- rast(paste0(ruta_escenarios,"wc2.1_30s_bioc_ACCESS-CM2_ssp585_2041-2060.tif"))%>% 
  crop(extent_regional)
names(env_ssp585)<-variable_names
terra::writeRaster(env_ssp585,paste0(ruta_escenarios,"wc2.1_30s_bioc_ACCESS-CM2_ssp585_2041-2060.tif"),
                   overwrite=TRUE)

env_ssp126 <- rast(paste0(ruta_escenarios,"wc2.1_30s_bioc_ACCESS-CM2_ssp126_2041-2060.tif"))
env_ssp245 <- rast(paste0(ruta_escenarios,"wc2.1_30s_bioc_ACCESS-CM2_ssp245_2041-2060.tif"))
env_ssp370 <- rast(paste0(ruta_escenarios,"wc2.1_30s_bioc_ACCESS-CM2_ssp370_2041-2060.tif"))
env_ssp585 <- rast(paste0(ruta_escenarios,"wc2.1_30s_bioc_ACCESS-CM2_ssp585_2041-2060.tif"))

escenarios <- list(env_ssp126, env_ssp245, env_ssp370, env_ssp585)

## 0.4 Carga de los nombres de especies----

ruta_descargadas<-"P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/THIC/especies_descargadas.csv"
gbif_descargadas<-read.csv(ruta_descargadas,sep=",")
descargadas<-gbif_descargadas$especie %>% unique()

descargadas_global<-read.csv(gbif_global) #leemos los datos del archivo de ocurrencias globales
descargadas_global<-descargadas_global$especie%>% unique() #especies que tienen los datos gbif descargados

setwd("~/restauracion")
limpiar <- c("archivo.variables", 
             "mapa_ensemble", 
             "test_indvar", 
             "resultado_nbestreplicates", 
             "modelo_replica")

lista <- read_excel("P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/THIC/ListadoTipicas.xlsx") %>% 
  mutate(Habitual = as.factor(Habitual),
         Diagnostica = as.factor(Diagnostica), 
         Abundancia = as.factor(Abundancia))
especies<-lista$Especie %>% unique()

modelizadas<-list.files("~/restauracion/Results/Covariate/Values", ,
                        pattern = "\\.csv$") %>% 
  str_replace_all(pattern="\\.variables.csv|_ensemble.csv|_indvar.csv|_nbestreplicates.csv|_replica.csv",
                  replacement= "") %>%  unique()

# 1. Modelización----

for(e in especies){
if (!(e %in% modelizadas)){
## 1.1 Búsqueda de GBIF ----
# Dataset global

print(e)
if (!(e %in% descargadas_global)){
print("Searching global")
raw_occurrences_global <- occ_search(scientificName=e,
                                     #continent = "europe",
                                     hasCoordinate = TRUE,
                                     hasGeospatialIssue= FALSE,
                                     limit = 8000,
                                     fields=c("scientificName","decimalLatitude",
                                              "decimalLongitude","coordinateUncertaintyInMeters",
                                              "eventDate"))

occurrences_data_global<-raw_occurrences_global$data %>% 
  dplyr::rename(y=decimalLatitude,x=decimalLongitude,
                precision=coordinateUncertaintyInMeters,
                date=eventDate,especie=scientificName) %>%
  mutate(date=as.Date(date)) %>% relocate(x,y,especie,date,precision)

occurrences_data_global$especie <- e

if (!file.exists(gbif_global)) {
  write.table(occurrences_data_global, gbif_global, sep = ",", row.names = FALSE, col.names = TRUE, append = FALSE)
} else {
  write.table(occurrences_data_global, gbif_global, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
}

xy.global<-occurrences_data_global %>% dplyr::select(x,y)
} else {

xy.global<-read.csv(gbif_global) 
xy.global<- filter(xy.global, especie==e) %>% select(x,y)  
  }

## Dataset regional
## FALTA el filtro de AFLIBER!!
print("Searching regional")
# raw_occurrences <- occ_search(scientificName=e,
#                               country = "ES",
#                               hasCoordinate = TRUE,
#                               hasGeospatialIssue= FALSE,
#                               limit = 8000,
#                               fields=c("scientificName","decimalLatitude",
#                                        "decimalLongitude","coordinateUncertaintyInMeters",
#                                        "eventDate"))
# 
# raw_occurrences_data <- raw_occurrences$data
# raw_occurrences_data <- raw_occurrences_data %>%
#   dplyr::rename(y=2,x=3,precision=4,date=5) %>%
#   mutate(date=as.Date(date)) 
# 
# filtered_occurrences_data <- raw_occurrences_data %>%  
#   #filter (precision < 1000) %>%  ##decidir sobre la precisión del filtro
#   dplyr::select(x,y,date) %>% 
#   relocate(x, .before=y) %>% 
# arrange(date)

#xy.regional<-filtered_occurrences_data %>% dplyr::select(x,y)# %>%
  #st_as_sf(coords = c("x", "y"), crs = geoproj)
xy.regional <- gbif_descargadas %>% filter(especie==e) %>% 
  select(x,y)
#desarrollar la limpieza de coordenadas
#decidir sobre la imputación de ocurrencias históricas
## 1.2 Modelización----
print("nsdm input data")
nsdm_input <- NSDM.InputData(SpeciesName = e,
                             spp.data.global = xy.global, 
                             spp.data.regional = xy.regional,
                             expl.var.global = expl.var.global,
                             expl.var.regional = expl.var.regional,
                             new.env = escenarios,
                             new.env.names = ssp,
                             Background.Global = NULL, 

                             Background.Regional = NULL)

print("nsdm finput data")
nsdm_finput <- NSDM.FormattingData(nsdm_input,
#OJO modificar número de puntos de acuerdo al número de ocurrencias                                 
                                   nPoints = 100, # number of background points
                                   Min.Dist.Global = "resolution",
                                   Min.Dist.Regional = "resolution",
                                   Background.method = "random", # method “random" or "stratified” to generate background points 
                                   save.output = FALSE) #save outputs locally
print("selecting vars")
nsdm_selvars <- NSDM.SelectCovariates(nsdm_finput,
                                      maxncov.Global = "nocorr",   # Max number of covariates to be selected at the global scale
                                      maxncov.Regional = "nocorr", # Max number of covariates to be selected at the regional scale
                                      corcut = 0.7, #  correlation threshold
                                      algorithms = c("glm","gam","rf"),
                                      ClimaticVariablesBands = NULL, # covariate bands to be excluded in the covariate selection at the regional scale
                                      save.output = TRUE)
print("nsdm global")
nsdm_global <- NSDM.Global(nsdm_selvars,
                           algorithms = c("GAM","GBM", "RF","GLM"),# Statistical algorithms used for modelling
                           CV.nb.rep = 10, # number of cross-validation repetitions
                           CV.perc = 0.8, # percentage of the data will be used for training in each cross-validation fold
                           metric.select.thresh = 0.7, #  AUC threshold to include replicates in the final ensemble model
                           CustomModelOptions = NULL, # Allows users to apply custom modelling options. 
                           save.output = TRUE, 
                           rm.biomod.folder = TRUE) # Remove the temporary folders created by `biomod2` 
print("nsdm regional")
nsdm_regional <- NSDM.Regional(nsdm_selvars,
                               algorithms = c("GAM","GBM", "RF","GLM"),
                               CV.nb.rep = 10,
                               CV.perc = 0.8,
                               metric.select.thresh = 0.7,
                               CustomModelOptions = NULL, 
                               save.output = TRUE,
                               rm.biomod.folder = TRUE)

print("covariate model")
nsdm_covariate <- NSDM.Covariate(nsdm_global,
                                 algorithms = c("GAM","GBM", "RF", "GLM"),
                                 rm.corr=TRUE,
                                 CV.nb.rep = 10,
                                 CV.perc = 0.8,
                                 metric.select.thresh = 0.7,
                                 CustomModelOptions = NULL,
                                 save.output = TRUE,
                                 rm.biomod.folder = TRUE)
regional_model<-terra::rast(nsdm_regional$current.projections$Pred)
plot(regional_model)
Covariate.model <- terra::rast(nsdm_covariate$current.projections$Pred)
plot(Covariate.model, main=e)
points(xy.regional)
##WRITE RASTER
##WRITE PROJECTION
}
}

