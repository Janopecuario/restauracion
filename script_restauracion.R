# 0.VARIABLES GLOBALES ####
rm(list=ls())
setwd("~/restauracion")
# library(remotes)
# remotes::install_github("geoSABINA/sabinaNSDM")
# remotes::install_github("N-SDM/covsel")
packages<-c("raster", "biomod2", "dismo","mgcv",
            "rasterVis","adegenet","gstat","shapefiles",
            "sp","ggfortify","reshape","spatialEco",
            "tidyverse","rgbif","sabinaNSDM",
            "CoordinateCleaner","geodata","sf")
sapply(packages, require, character.only=T)

UTMproj<-"+proj=utm +zone=28 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
geoproj<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

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

# 1.PREPARACIÓN PREDICTORES ####
spain<- st_read("ll_autonomicas_inspire_peninbal_etrs89.shp") %>% st_union()

#correr 1 vez
variables_raw<- worldclim_country(country="ESP", 
                               var="bio", 
                               path=getwd(), 
                               version="2.1")
variables <- variables_raw %>% crop(spain)
mask <- variables$wc2.1_30s_bio_1

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
    rename(y=2,x=3,precision=4,date=5) %>%
    mutate(date=as.Date(date)) 
  dim_ini <- nrow(raw_occurrences_data)
  filtered_occurrences_data <- raw_occurrences_data %>%  
    filter (precision < 1000) %>% 
    select(x,y,date) %>% relocate(x, .before=y) %>% 
    arrange(date)
occurrences <- filtered_occurrences_data %>% 
  select(x,y)
  dim_end <- nrow(occurrences)

flags <- clean_coordinates(x = raw_occurrences, 
                            lon = "decimalLongitude", 
                            lat = "decimalLatitude",
                            countries ="countryCode",
                            tests = c("capitals", "centroids",
                                      "equal", "zeros", "countries"))
plot(mask)
points(occurrences$x,occurrences$y,col="red",pch=16)
 
# 2.2 Limpieza
# 3.MODELIZACIÓN Y VALIDACIÓN ####

presencias<-as.data.frame(presence.absence.raster(mask,occurrences,raster.label=especie),xy=TRUE)
presencias.na<-presencias[,3]
presencias.na[presencias.na==0]="NA"
presencias.na<-cbind(presencias[,1:2],presencias.na)
colnames(viola.na)<-c("x","y","viola")
setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
bio01<-aggregate(raster("bio01.asc"),fac=4)
pet<-aggregate(raster("pet.asc"),fac=4)
bio12<-aggregate(raster("bio12.asc"),fac=4)
tpi<-raster("C:/Users/agonz/Documents/Viola/Tenerife/Canadas/tpi.asc")
bio19<-aggregate(raster("bio19.asc"),fac=4)
predictores<-stack(pet,
                   #bio19,
                   bio01,bio12,slope,tpi)
setwd("C:/Users/agonz/Documents/Viola")
myRespName <- 'viola'
myResp <- as.numeric(viola.na[,myRespName])
myResp[myResp == 2]  <- "NA"
myResp<-as.numeric(myResp)
myRespXY <- viola.na[,c("x","y")]

# the name of studied species
myRespName <- 'viola'
# the presence/absences data for our species
myResp <- as.numeric(viola.na[,myRespName])
myResp[myResp == 2]  <- "NA"
myResp<-as.numeric(myResp)
# the XY coordinates of species data
myRespXY <- viola.na[,c("x","y")]
# load the environmental raster layers (could be .img, ArcGIS

datos<-BIOMOD_FormatingData(resp.var=myResp,
                            expl.var=predictores,
                            resp.xy = myRespXY,
                            resp.name = myRespName,
                            eval.resp.var = NULL,
                            eval.expl.var = NULL,
                            eval.resp.xy = NULL,
                            PA.nb.rep = 2,
                            PA.nb.absences = nrow(viola.coords)*6,
                            PA.strategy = 'random',
                            na.rm = TRUE)
# rasters or any supported format by the raster package)
setwd("C:/Users/agonz/Documents/Viola")
rm(myRespXY,myResp,bio01,bio12,tpi,slope,pet)

#### 5.2 MODELING ####
modelos <- BIOMOD_Modeling(
  datos,
  models = c('GBM','RF',"GLM","GAM"),
  #models.options = myBiomodOption,
  NbRunEval=3,
  DataSplit=80,
  VarImport=1,
  prevalence=0.5,
  models.eval.meth = c("ROC",'TSS'),
  SaveObj = TRUE,
  rescal.all.models = TRUE,
  do.full.models = FALSE,
  modeling.id = paste(myRespName,"FirstModeling",sep=""))

rm(datos)

proj.modelos<-BIOMOD_Projection(modelos,
              new.env=predictores,
              proj.name="viola",
              xy.new.env = NULL,
              selected.models = 'all',
              binary.meth = "TSS",
              filtered.meth = "TSS","ROC",
              compress = TRUE,
              build.clamping.mask = FALSE)

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

setwd("C:/Users/agonz/Documents/Viola")
writeRaster(ensemble.pres.mean.weight@proj@val$viola_EMwmeanByROC_mergedAlgo_mergedRun_mergedData,"presenteTFM.tiff",overwrite=TRUE)
save.image("C:/Users/agonz/Documents/Viola/viola.RData")
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
setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas",model,rcp,year,sep="/"))
bio01<-raster("bio01.asc")
bio12<-raster("bio12.asc")
pet<-raster("pet.asc")
tpi<-raster("C:/Users/agonz/Documents/Viola/Tenerife/Canadas/tpi.asc")
slope<-aggregate(raster("C:/Users/agonz/Documents/Viola/Tenerife/Canadas/slope.asc"),fac=4)
bio19<-raster("bio19.asc")

predictores<-stack(pet,
                   #bio19,
                   bio01,bio12,slope,tpi)
message("projecting",appendLF = TRUE)
setwd("C:/Users/agonz/Documents/Viola")
proj.modelos<-BIOMOD_Projection(modelos,
                                new.env=predictores,
                                proj.name="viola",
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
plot(ensemble.pres.mean.weight@proj@val$viola_EMwmeanByROC_mergedAlgo_mergedRun_mergedData, main="Ensemble modeling")
writeRaster(ensemble.pres.mean.weight@proj@val$viola_EMwmeanByROC_mergedAlgo_mergedRun_mergedData,paste(model,rcp,year,"TFM.tiff",sep=""),overwrite=TRUE)
save.image("C:/Users/agonz/Documents/Viola/viola.RData")

   }
  }
}
evaluations<-get_evaluations(modelos,as.data.frame=TRUE)
evaluations = separate(evaluations, col=Model.name, into=c("model","run","pa"),sep = "_")

ggplot(evaluations,aes(x=model,y=Testing.data))+ stat_boxplot(geom ='errorbar') + geom_boxplot(aes(fill=model))+
  xlab("Algoritmo")+ylab("Puntuación")+facet_grid(.~Eval.metric)+labs(fill="Modelo")+theme_bw()

presente<-raster("presenteTFM.asc")
setwd("C:/Users/agonz/Documents/Viola")
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

# 4.MODELOS ANIDADOS #####
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
