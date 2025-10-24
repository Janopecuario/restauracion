# 0.VARIABLES GLOBALES ####
rm(list=ls())
setwd("P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/Modelos")
packages<-c("raster", "biomod2", "rasterVis","gstat","shapefiles",
            "sp","ggfortify","reshape","spatialEco","FactoMineR","factoextra",
            "tidyverse","crayon","sf","terra","ecospat","hier.part","randomForest")

sapply(packages, require, character.only=T)
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
## 1.1 Rutas globales----
extent_regional<-extent(c(-10,4.28,34,44.5)) #coordenadas peninsula + NAfrica
extent_global<-extent(c(-16.47,93.53,18.24,78))
ruta_worldclim<- "P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/worldclim"
background<-read_csv("background.csv")
ruta_descargadas_global <- "P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/THIC/global_especies_descargadas.csv"
ruta_descargadas<-"P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/THIC/especies_descargadas.csv"
ruta_lista_descargadas<-"P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/THIC/lista_especies_descargadas.csv"
ruta_litologia<-"P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/Litologia"
ruta_tpi<-"P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/ambientes/tpi.tif"

## 1.2 Carga de predictores----
### 1.2.1 Worldlim global----
slope_global<-raster("P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/ambientes/slope_global.tif")
tpi<-raster(ruta_tpi)
worldclim_vars<-list.files(path="P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/worldclim",
                           pattern="^wc.*\\.tif$")
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
  cat(green(paste0(lit,"\n")))
  litologia<-stack(litologia, temp_raster)
}

covariable<-raster("Ziziphus lotus_Covariate_model.tif")

# 2.PREPARACION DATOS OCURRENCIA ####
presencias<-read.csv("P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/THIC/Habitat_5220.csv")

# 2.1 SELECCION DE VARIABLES ####

datos_pca<-raster::extract(x=biovars_regional,presencias[1:2]) %>%
  as.data.frame() %>% na.omit()
correlaciones<-cor(datos_pca,method="pearson")
npred<-ecospat.npred(x=correlaciones,th=0.7)
npred<-npred/2 %>% round()
pca<-PCA(datos_pca,graph=T)

fviz_pca_ind(pca,
             label = "none", # hide individual labels
             #habillage = as.factor(data$species), # color by groups
             #palette = c("#00AFBB", "#E7B800","#33ffa8", "#FF5733"),
             addEllipses = TRUE # Concentration ellipses
)

rand.especies<-as.data.frame(sampleRandom(biovars_regional,nrow(datos_pca), 
                                          ext=biovars_regional,na.rm=TRUE,xy=FALSE))
rand.especies$pres<-rep(0,nrow(rand.especies))
datos_pca$pres<-rep(1,nrow(datos_pca))
hier.data<-rbind(rand.especies,datos_pca) %>% na.omit()
random<-randomForest::randomForest(x=hier.data[1:length(hier.data)-1],y=hier.data$pres)
seleccion<-randomForest::importance(random) %>% data.frame() %>% arrange(desc(IncNodePurity)) %>% 
  data.frame()
seleccion<-rownames(seleccion)
seleccion<-seleccion[1:12]
hier.data<-hier.data %>% dplyr::select(seleccion,pres)
hier<-hier.part(hier.data[,length(hier.data)],hier.data[1:length(hier.data)-1],barplot=TRUE)
hier<-hier$I.perc
hier<-arrange(hier,desc(ind.exp.var))
hier<-rownames(hier)
hier<-hier[1:4]
biovars_regional<-biovars_regional[[hier]]

biovars_regional<-stack(covariable,biovars_regional)
mascara<-biovars_regional[[1]]
expl.var.regional <- terra::rast(biovars_regional)


# 3.MODELIZACIÓN Y VALIDACIÓN ####
presencias<-as.data.frame(presence.absence.raster(mascara,presencias[1:2],raster.label="azufaifares"),xy=TRUE)
presencias.na<-presencias[,3]
presencias.na[presencias.na==0]="NA"
presencias.na<-cbind(presencias[,1:2],presencias.na)
colnames(presencias.na)<-c("x","y","azufaifares")
myRespName <- "azufaifares"
# the presence/absences data for our species
myResp <- as.numeric(presencias.na[,myRespName])
myResp[myResp == 2]  <- "NA"
myResp<-as.numeric(myResp)
# the XY coordinates of species data
myRespXY <- presencias.na[,c("x","y")]

datos<-BIOMOD_FormatingData(resp.var=myResp,
                            expl.var=expl.var.regional,
                            resp.xy = myRespXY,
                            resp.name = myRespName,
                            eval.resp.var = NULL,
                            eval.expl.var = NULL,
                            eval.resp.xy = NULL,
                            PA.nb.rep = 3,
                            #PA.nb.absences = 3000,
                            PA.nb.absences = min(10000, length(which(myResp==1)) * 10), # Sugerencia para prueba hasta que tengamos background M
                            PA.strategy = 'sre',
                            #PA.user.table= background,
                            na.rm = TRUE)


#### 3.2 MODELING ----
modelos <- BIOMOD_Modeling(
  datos,
  models = c('GBM','RF',"GLM","GAM"),
  #models.options = myBiomodOption,
  CV.nb.rep = 3,
  CV.perc=0.8,
  var.import=1,
  prevalence=0.5,
  metric.eval = c("ROC",'TSS'),
  #SaveObj = FALSE,
  #rescal.all.models = TRUE,
  CV.do.full.models = FALSE,
  modeling.id = myRespName,
  do.progress = TRUE)


proj.modelos<-BIOMOD_Projection(modelos,
                                new.env=expl.var.regional,
                                proj.name="azufaifares",
                                xy.new.env = NULL,
                                selected.models = 'all',
                                binary.meth = "TSS",
                                filtered.meth = c("TSS","ROC"),
                                compress = TRUE,
                                build.clamping.mask = FALSE)



modelos.ensemble.mean.weight<- BIOMOD_EnsembleModeling( modelos,
                                                        models.chosen = 'all',
                                                        em.by = 'all',
                                                        metric.select =  'all',
                                                        #metric.select =  0.8,
                                                        metric.eval = c("ROC",'TSS'),
                                                        em.algo=c("EMwmean"),
                                                        var.import=1)



ensemble.pres.mean.weight<-BIOMOD_EnsembleForecasting( modelos.ensemble.mean.weight,
                                                       bm.proj = proj.modelos,
                                                       #models.chosen = 'all',
                                                       #binary.meth = "TSS",
                                                       #filtered.meth = c("ROC",'TSS'),
                                                       #total.consensus=TRUE,
                                                       #compress = TRUE
)

# 4.EXPORTACIÓN RESULTADOS ####

resultados<-ensemble.pres.mean.weight@proj.out@val %>% terra::unwrap()
resultados<-resultados[[1]] %>% terra::mask(rast(mascara))
plot(resultados)
writeRaster(resultados,"azufaifares_covariate.tif",overwrite=TRUE)
