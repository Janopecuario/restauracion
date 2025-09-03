

# 0.VARIABLES GLOBALES ####
rm(list=ls())
setwd("~/restauracion")

packages<-c("raster", "biomod2", "dismo","mgcv","raster",
            "rasterVis","adegenet","gstat","shapefiles",
            "sp","ggfortify","reshape","spatialEco",
            "tidyverse","rgbif","sabinaNSDM",
            "CoordinateCleaner","geodata","sf","tidyverse","rnaturalearth")
sapply(packages, require, character.only=T)

# [NOTA Jess] Paquetes: 'raster' y 'tidyverse' están repetidos
# pero mejor dejar uno si es posible ('raster' o 'terra') para evitar coerciones. 

UTMproj<-"+proj=utm +zone=28 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" # ¿por qué 28?
geoproj<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# [NOTA Jess] Evitar mezclar CRS en lo posible, ej:
# - Prototipo con WorldClim: mantener TODO en WGS84 (EPSG:4326) → puntos y predictores.
# - Cuando llegue la Malla AFR oficial (grid 1 km / 0,5 km): 
#   * Reproyectar los predictores al CRS y resolución de la malla.
#   * Transformar las coordenadas de respuesta (presencias/ausencias) a ese mismo CRS.
#
#   # Ejemplo (raster):
#   mask   <- raster::raster("Malla_AFR.tif")
#   preds  <- raster::projectRaster(preds, mask)  # alinea CRS, resol. y extent
#   # resp_xy como sf:
#   resp_sf <- sf::st_as_sf(resp_xy, coords=c("x","y"), crs=4326)
#   resp_sf <- sf::st_transform(resp_sf, raster::crs(mask))
#
#   # Ejemplo (terra):
#   mask   <- terra::rast("Malla_AFR.tif")
#   preds  <- terra::project(preds, mask)
#   resp_sf <- sf::st_as_sf(resp_xy, coords=c("x","y"), crs=4326)
#   resp_sf <- sf::st_transform(resp_sf, sf::st_crs(mask))

presence.absence.raster <- function (mask.raster,species.data,raster.label="") {
  # set the background cells in the raster to 0
  mask.raster[!is.na(mask.raster)] <- 0
  #set the cells that contain points to 1
  speciesRaster <- rasterize(species.data,mask.raster,field=1)
  speciesRaster <- merge(speciesRaster,mask.raster) 
  #label the raster
  names(speciesRaster) <- raster.label
  return(speciesRaster)
}
# [NOTA Jess] Esta función espera 'species.data' en el MISMO CRS que 'mask.raster';
# Aunque no se usa aun la función, más abajo veo 'sf_obj_UTM' que imagino está en UTM y 
# 'mask' en lat/long -> posible error si se quiere pasar el sf como species.data
# Sugerencia: para la prueba usar aquí species.data en geoproj (WGS84) = CRS(mask)
# en todo caso que CRS(species.data) = CRS(mask.raster)


# 1.PREPARACIÓN PREDICTORES ####
spain<- ne_countries(country="Spain",type="countries")

variables_raw<- worldclim_country(country="ESP", 
                               var="bio", 
                               path=getwd(), 
                               version="2.1")
variables <- variables_raw %>% crop(spain)

# [NOTA Jess] ne_countries() por defecto devuelve sp, si queremos sf hay que especificar returnclass="sf"
# Creo que la escala por defecto que da es pequeña, se le puede decir scale="medium", aunque quizás no es importante
# CRS me parece que era por defecto 4326, recordar para la malla reproyectar

mask <- variables$wc2.1_30s_bio_1 # [NOTA Jess] máscara en WGS84.

# 2.PREPARACION DATOS OCURRENCIA ####
# 2.1 Prueba GBIF #####
  especie <- "Teucrium scorodonia"
  raw_occurrences <- occ_search(scientificName=especie,
                                country = "ES",
                                hasCoordinate = TRUE,
                                hasGeospatialIssue= FALSE,
                                limit = 8000,
                                fields=c("scientificName","decimalLatitude",
                                         "decimalLongitude","coordinateUncertaintyInMeters",
                                         "eventDate"))##quedan sinónimos por filtrar
  
  raw_occurrences_data <- raw_occurrences$data
  raw_occurrences_data <- raw_occurrences_data %>%
    dplyr::rename(y=2,x=3,precision=4,date=5) %>%
    mutate(date=as.Date(date)) 
  dim_ini <- nrow(raw_occurrences_data)
  filtered_occurrences_data <- raw_occurrences_data %>%  
    filter (precision < 1000) %>% 
    dplyr::select(x,y,date) %>% relocate(x, .before=y) %>% 
    arrange(date)
occurrences <- filtered_occurrences_data %>% 
  dplyr::select(x,y)
  dim_end <- nrow(occurrences)

flags <- clean_coordinates(x = raw_occurrences, 
                            lon = "decimalLongitude", 
                            lat = "decimalLatitude",
                            countries ="countryCode",
                            tests = c("capitals", "centroids",
                                      "equal", "zeros", "countries"))
plot(mask)
points(occurrences$x,occurrences$y,col="red",pch=16)

# [NOTA Jess] ok el filtro de precisión (< 1000 m)
# aunque para islas quizás bajar a 500 m? (para no introducir mas borrosidad que la del píxel) o deduplicado
# pendiente de integrar bien con la limpieza CC para quitar capitales/centroides/zeros...
# + deduplicado por celda / thinning espacial para reducir pseudo-replicación
# + filtro temporal si queremos histórico +- reciente (>= 1950, >=2000 ?)

# Paso 1: Convertir a objeto sf
sf_obj <- st_as_sf(occurrences, coords = c("x", "y"), crs = geoproj)

# Paso 2: Reproyectar
sf_obj_UTM <- st_transform(sf_obj, crs = UTMproj) %>% 
st_coordinates() %>% as.data.frame() %>% 
  dplyr::rename(x=X,y=Y)

# [NOTA Jess] Mantendría 'sf_obj' en WGS84.
# Solo proyectaría a metros si es estrictamente necesario (buffers, distancias), y entonces
# usaría EPSG:3035 (LAEA Europe) o la UTM adecuada por región (25829/30/31), NO 28N global.
# Además, evitaría convertir a data.frame con st_coordinates() aquí: perdemos geometría y CRS.
# Si BIOMOD/FormatingData necesita coords, mejor pasar lon/lat (WGS84) coherentes con 'predictores'.
# Por ej:
# Proyección en metros SOLO cuando haga falta (distancias/áreas), sin perder geometría:
# sf_obj_3035 <- st_transform(sf_obj, 3035)  # LAEA Europe
# ... 
# Volver a WGS84 para cruzarlo con rasters climáticos/proyecciones:
# sf_obj_back <- st_transform(sf_obj_3035, 4326)


# 2.2 Limpieza


# 3.MODELIZACIÓN Y VALIDACIÓN ####

presencias<-as.data.frame(presence.absence.raster(mask,sf_obj_UTM,raster.label=especie),xy=TRUE)
presencias.na<-presencias[,3]
presencias.na[presencias.na==0]="NA"
presencias.na<-cbind(presencias[,1:2],presencias.na)
colnames(presencias.na)<-c("x","y",especie)
# [NOTA Jess] Ojo 'sf_obj_UTM', tras 'st_coordinates()' se convierte 
# en data.frame y pierde geometría y CRS. Si luego lo pasamos a 'presence.absence.raster' 
# junto a 'mask' (en WGS84), puede haber desajuste (coords en metros vs raster en grados). 
# Mejor usar directamente 'sf_obj' en WGS84, o reproyectar el raster si de verdad queremos UTM.

myRespName <- especie
# [NOTA Jess] 'myResp' se define dos veces más abajo; dejar una sola definición.
myResp <- as.numeric(presencias.na[,myRespName])
myResp[myResp == 2]  <- "NA"
myResp<-as.numeric(myResp)
myRespXY <- presencias.na[,c("x","y")]


# the presence/absences data for our species
myResp <- as.numeric(presencias.na[,myRespName])
myResp[myResp == 2]  <- "NA"
myResp<-as.numeric(myResp)
# the XY coordinates of species data
myRespXY <- presencias.na[,c("x","y")]
# load the environmental raster layers (could be .img, ArcGIS

# [NOTA Jess] Definir 'predictores' (RasterStack) con BIOs seleccionadas y mismo CRS que coords:
# predictores <- raster::stack(variables[[c("wc2.1_30s_bio_1","wc2.1_30s_bio_5",
#                                           "wc2.1_30s_bio_6","wc2.1_30s_bio_12",
#                                           "wc2.1_30s_bio_15")]])

datos<-BIOMOD_FormatingData(resp.var=myResp,
                            expl.var=predictores,
                            resp.xy = myRespXY,
                            resp.name = myRespName,
                            eval.resp.var = NULL,
                            eval.expl.var = NULL,
                            eval.resp.xy = NULL,
                            PA.nb.rep = 2,
                            PA.nb.absences = nrow(presencias.coords)*6, # [NOTA Jess] Definir presencias.coords
                            # PA.nb.absences = min(10000, length(which(myResp==1)) * 10), # Sugerencia para prueba hasta que tengamos background M
                            PA.strategy = 'random',
                            na.rm = TRUE)
# rasters or any supported format by the raster package)
setwd("C:/Users/agonz/Documents/presencias")
rm(myRespXY,myResp,bio01,bio12,tpi,slope,pet)



#### 5.2 MODELING ####
modelos <- BIOMOD_Modeling(
  datos,
  models = c('GBM','RF',"GLM","GAM"),
  #models.options = myBiomodOption,
  #NbRunEval=3,
  CV.perc=0.8,
  var.import=0,
  prevalence=0.5,
  metric.eval = c("ROC",'TSS'),
  #SaveObj = FALSE,
  #rescal.all.models = TRUE,
  CV.do.full.models = FALSE,
  modeling.id = myRespName)

# [NOTA Jess] 'var.import=0' desactiva la importancia de variables. Si luego queremos consultar
# importancia, conviene 'var.import=1'. Con 'CV.perc=0.8' 
# sin 'NbRunEval', habrá una única partición - imagino que es solo para prueba

proj.modelos<-BIOMOD_Projection(modelos,
              new.env=biovars,
              proj.name=especie,
              xy.new.env = NULL,
              selected.models = 'all',
              binary.meth = "TSS",
              filtered.meth = "TSS","ROC",
              compress = TRUE,
              build.clamping.mask = FALSE)

# [NOTA Jess] 'new.env=biovars' -> no está definido en el script; recordar definir el stack de predictore 
# ('predictores' lo puse más arriba antes de modeling).
# 'filtered.meth = "TSS","ROC"' creo que debe ser vector: filtered.meth = c("TSS","ROC").
# Por claridad, yo nombraría el primer argumento: modeling.output = modelos.

rm(predictores)
rm(tpi,slope,bio01,pet,bio12,bio19)

modelos.ensemble.mean.weight<- BIOMOD_EnsembleModeling( modelos,
                                            chosen.models = 'all',
                                            em.by = 'all',
                                            eval.metric = 'all',
                                            eval.metric.quality.threshold = c(0.8,0.8),
                                            models.eval.meth = c("ROC",'TSS'),
                                            prob.mean = FALSE,
                                            prob.cv = FALSE,
                                            prob.ci = FALSE,
                                            prob.ci.alpha = 0.05,
                                            prob.median = FALSE,
                                            committee.averaging = FALSE,
                                            prob.mean.weight = TRUE,
                                            prob.mean.weight.decay = 'proportional',
                                            VarImport = 1)


 
ensemble.pres.mean.weight<-BIOMOD_EnsembleForecasting( modelos.ensemble.mean.weight,
                                           projection.output = proj.modelos,
                                           selected.models = 'all',
                                           binary.meth = "TSS",
                                           filtered.meth = c("ROC",'TSS'),
                                           total.consensus=TRUE,
                                           compress = TRUE)

setwd("C:/Users/agonz/Documents/presencias")
writeRaster(ensemble.pres.mean.weight@proj@val$presencias_EMwmeanByROC_mergedAlgo_mergedRun_mergedData,"presenteTFM.tiff",overwrite=TRUE)
save.image("C:/Users/agonz/Documents/presencias/presencias.RData")
importance_indiv<-get_variables_importance(modelos, as.data.frame=TRUE)
importance_modelos<-get_variables_importance(modelos.ensemble.mean.weight,as.data.frame=TRUE)


#### 5.3 PROJECTION #### 
models<-c("miroc","csiro")
RCPs<-c(26,60,85)
years<-c(30,50,80)


for (i in models){
model<-i
  for(j in RCPs){
  rcp<-j
   for (h in years){
     year<-h
message(paste(model, rcp, year,sep=" "),appendLF = TRUE)  
setwd(paste("C:/Users/agonz/Documents/presencias/TenerifeCC/Canadas",model,rcp,year,sep="/"))
bio01<-raster("bio01.asc")
bio12<-raster("bio12.asc")
pet<-raster("pet.asc")
tpi<-raster("C:/Users/agonz/Documents/presencias/Tenerife/Canadas/tpi.asc")
slope<-aggregate(raster("C:/Users/agonz/Documents/presencias/Tenerife/Canadas/slope.asc"),fac=4)
bio19<-raster("bio19.asc")

predictores<-stack(pet,
                   #bio19,
                   bio01,bio12,slope,tpi)
message("projecting",appendLF = TRUE)
setwd("C:/Users/agonz/Documents/presencias")
proj.modelos<-BIOMOD_Projection(modelos,
                                new.env=predictores,
                                proj.name="presencias",
                                xy.new.env = NULL,
                                selected.models = 'all',
                                binary.meth = "TSS",
                                filtered.meth = "TSS","ROC",
                                compress = TRUE,
                                build.clamping.mask = FALSE)

rm(predictores)
rm(pet,bio01,bio12,slope,tpi)
message("ensemble modeling",appendLF = TRUE)
modelos.ensemble.mean.weight<- BIOMOD_EnsembleModeling( modelos,
                                                        chosen.models = 'all',
                                                        em.by = 'all',
                                                        eval.metric = 'all',
                                                        eval.metric.quality.threshold = c(0.8,0.8),
                                                        models.eval.meth = c("ROC",'TSS'),
                                                        prob.mean = FALSE,
                                                        prob.cv = FALSE,
                                                        prob.ci = FALSE,
                                                        prob.ci.alpha = 0.05,
                                                        prob.median = FALSE,
                                                        committee.averaging = FALSE,
                                                        prob.mean.weight = TRUE,
                                                        prob.mean.weight.decay = 'proportional',
                                                        VarImport = 0)


message("ensemble forecasting",appendLF = TRUE)
ensemble.pres.mean.weight<-BIOMOD_EnsembleForecasting( modelos.ensemble.mean.weight,
                                                       projection.output = proj.modelos,
                                                       selected.models = 'all',
                                                       binary.meth = "TSS",
                                                       filtered.meth = c("ROC",'TSS'),
                                                       total.consensus=TRUE,
                                                       compress = TRUE)
message("writing raster",appendLF = TRUE)
plot(ensemble.pres.mean.weight@proj@val$presencias_EMwmeanByROC_mergedAlgo_mergedRun_mergedData, main="Ensemble modeling")
writeRaster(ensemble.pres.mean.weight@proj@val$presencias_EMwmeanByROC_mergedAlgo_mergedRun_mergedData,paste(model,rcp,year,"TFM.tiff",sep=""),overwrite=TRUE)
save.image("C:/Users/agonz/Documents/presencias/presencias.RData")

   }
  }
}
evaluations<-get_evaluations(modelos,as.data.frame=TRUE)
evaluations = separate(evaluations, col=Model.name, into=c("model","run","pa"),sep = "_")

ggplot(evaluations,aes(x=model,y=Testing.data))+ stat_boxplot(geom ='errorbar') + geom_boxplot(aes(fill=model))+
  xlab("Algoritmo")+ylab("Puntuación")+facet_grid(.~Eval.metric)+labs(fill="Modelo")+theme_bw()



# [COMENTARIO Jess] Mapas finales / hillshade
presente<-raster("presenteTFM.asc")
setwd("C:/Users/agonz/Documents/presencias")
canadas<-raster("canadas.asc",crs=geoproj)
slope<-terrain(canadas,opt="slope")
aspect<-terrain(canadas,opt="aspect")
hillshade<-hillShade(slope,aspect, 15, 180,filename="hillshade25.270.asc",overwrite=TRUE)
myPal<-colorRampPalette(c("white","white","#BDECB6","#1E5945"))(100)
plot(hillshade,col=grey(0:100/100), main= "Current Niche Suitability",legend=FALSE)
plot(presente,alpha=0.85,col=myPal,add=TRUE,legend=TRUE)


rcl.pres<-c(0, 750, 0,  750, 1000, 1)
rcl.pres<-matrix(rcl.pres, ncol=3, byrow=TRUE)
rcl<-c(0, 850, 0,  850, 1000, 1)
rcl <- matrix(rcl, ncol=3, byrow=TRUE)
presente.rcl<-reclassify(presente,rcl.pres)
plot(presente.rcl)
presente.rcl<-as.data.frame(presente.rcl)
models<-c("csiro","miroc")
RCPs<-c(26,60,85)
years<-c(30,50,80)

areas<-data.frame(matrix(ncol=4))
colnames(areas)<-c("model","RCP","year","surface")
for (i in models){
  model<-i
  for(j in RCPs){
    rcp<-j
    for (h in years){
      year<-h
      
      raster.projected<-raster(paste(i,j,h,".asc",sep=""))
      raster.projected<-reclassify(raster.projected,rcl)
      raster.projected<-as.data.frame(raster.projected)
      raster.projected<-subset(raster.projected,layer==1)
      #presente.projected<-reclassify(presente.rcl)
      presente.projected<-as.data.frame(presente.rcl)
      presente.projected<-subset(presente.projected,layer==1)
      area.presente<-c(i,j,20,nrow(presente.projected))
      area<-c(i,j,h,nrow(raster.projected))
      areas<-rbind(areas,area)
      areas<-rbind(areas,area.presente)
      print(areas)
      
    }
  }
}

# 4.MODELOS ANIDADOS ##### [NOTA Jess] BLOQUE SABINA NSDM
# Species occurrences
data(Fagus.sylvatica.xy.global, package = "sabinaNSDM")
spp.data.global <- Fagus.sylvatica.xy.global

data(Fagus.sylvatica.xy.regional, package = "sabinaNSDM")
spp.data.regional <- Fagus.sylvatica.xy.regional

data(expl.var.regional, package = "sabinaNSDM")

nsdm_input <- NSDM.InputData(SpeciesName = SpeciesName,
                             spp.data.global = NULL, 
                             spp.data.regional = Fagus.sylvatica.xy.regional, 
                             expl.var.global = expl.var.global, 
                             expl.var.regional = expl.var.regional,
                             new.env = new.env,
                             new.env.names = "scenario1",
                             Background.Global = NULL, 
                             Background.Regional = NULL,
                             Absences.Global = NULL,
                             Absences.Regional = NULL)


