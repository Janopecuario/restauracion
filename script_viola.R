##### 0.PREPARACIÓN
rm(list=ls())

setwd("C:/Users/agonz/Documents/Viola")
packages<-c("raster", "biomod2", "MigClim","dismo","mgcv","rasterVis","polysat","adegenet","rgdal","tidyr","gstat","shapefiles","rgeos","sp","maptools","ggfortify","r2dRue","reshape","spatialEco","adegenet")
UTMproj<-"+proj=utm +zone=28 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
geoproj<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
lapply(packages, require, character.only=T)
viola.coords<-read.table("violacoords.txt",sep=",",header=T)
viola.coords<-viola.coords[,6:7]

geocanadas<-aggregate(raster("mask.asc"),fact=4)

canadas<-crop(raster("mdt.tf.asc",crs=UTMproj),bio01)
writeRaster(bio01,"mask.asc",overwrite=TRUE)
mask[!is.na(mask)] <- 0


##### 1. TOPOGRAFÍA #### hecho, no hay necesidad de tocarlo.

setwd("F:/Canarias05")

mdt1<-raster("MDT05-1088-H28-LIDAR.asc",crs=UTMproj)
mdt2<-raster("MDT05-1089-H28-LIDAR.asc",crs=UTMproj)
mdt3<-raster("MDT05-1091-H28-LIDAR.asc",crs=UTMproj)
mdt4<-raster("MDT05-1092-H28-LIDAR.asc",crs=UTMproj)
mdt5<-raster("MDT05-1096-H28-LIDAR.asc",crs=UTMproj)
mdt6<-raster("MDT05-1097-H28-LIDAR.asc",crs=UTMproj)
mdt7<-raster("MDT05-1102-H28-LIDAR.asc",crs=UTMproj)

mdt.tf<-aggregate(merge(mdt1,mdt2,mdt3,mdt4,mdt5,mdt6,mdt7),fact=4,fun=mean)

setwd("C:/Users/agonz/Documents/Viola")
writeRaster(mdt.tf,"mdt.tf.asc",overwrite=T)
rm(mdt1,mdt2,mdt3,mdt4,mdt5,mdt6,mdt7)
plot(mdt.tf, main="mdt tenerife")
##### 1.1. CORRECCIÓN DE ANOMALÍAS #####

setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
prec01<-resample(raster("prec01.asc"),canadas)


##### 2.ANOMALIES ####


     
anomaly <- function (model,rcp,year) {
  #ir al directorio donde están guardados los archivos
  #DESCOMPRIMIR ANTES 
  message(paste(model,rcp,year,sep="",appendLF = TRUE))
  
  message("### PRECIPITATION ####",appendLF = TRUE)
  message("Unzipping precipitation",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  unzip("prec.zip",overwrite=TRUE)
  message("### PREC01 ####",appendLF = TRUE)
  message("cropping future climate",appendLF=TRUE)
  prec01.cc<-crop(raster("prec_1.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  prec01<-crop(raster("prec_1.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.prec01<-prec01.cc-prec01
  rm(prec01.cc,prec01)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.prec01<-raster::resample(anomaly.prec01,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  prec01<-resample(raster("prec01.asc"),geocanadas)+anomaly.prec01
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(prec01,"prec01.asc",overwrite=TRUE)
  rm(prec01,anomaly.prec01)
  
  message("### PREC02 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  prec02.cc<-crop(raster("prec_2.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  prec02<-crop(raster("prec_2.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.prec02<-prec02.cc-prec02
  rm(prec02.cc,prec02)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.prec02<-raster::resample(anomaly.prec02,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  prec02<-resample(raster("prec02.asc"),geocanadas)+anomaly.prec02
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(prec02,"prec02.asc",overwrite=TRUE)
  rm(prec02,anomaly.prec02)
  
  message("### PREC03 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  prec03.cc<-crop(raster("prec_3.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  prec03<-crop(raster("prec_3.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.prec03<-prec03.cc-prec03
  rm(prec03.cc,prec03)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.prec03<-raster::resample(anomaly.prec03,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  prec03<-resample(raster("prec03.asc"),geocanadas)+anomaly.prec03
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(prec03,"prec03.asc",overwrite=TRUE)
  rm(prec03,anomaly.prec03)
  
  message("### PREC04 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  prec04.cc<-crop(raster("prec_4.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  prec04<-crop(raster("prec_4.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.prec04<-prec04.cc-prec04
  rm(prec04.cc,prec04)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.prec04<-raster::resample(anomaly.prec04,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  prec04<-resample(raster("prec04.asc"),geocanadas)+anomaly.prec04
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(prec04,"prec04.asc",overwrite=TRUE)
  rm(prec04,anomaly.prec04)
  
  message("### PREC05 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  prec05.cc<-crop(raster("prec_5.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  prec05<-crop(raster("prec_5.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.prec05<-prec05.cc-prec05
  rm(prec05.cc,prec05)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.prec05<-raster::resample(anomaly.prec05,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  prec05<-resample(raster("prec05.asc"),geocanadas)+anomaly.prec05
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(prec05,"prec05.asc",overwrite=TRUE)
  rm(prec05,anomaly.prec05)
  
  message("### PREC06 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  prec06.cc<-crop(raster("prec_6.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  prec06<-crop(raster("prec_6.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.prec06<-prec06.cc-prec06
  rm(prec06.cc,prec06)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.prec06<-raster::resample(anomaly.prec06,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  prec06<-resample(raster("prec06.asc"),geocanadas)+anomaly.prec06
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(prec06,"prec06.asc",overwrite=TRUE)
  rm(prec06,anomaly.prec06)
  
  message("### PREC07 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  prec07.cc<-crop(raster("prec_7.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  prec07<-crop(raster("prec_7.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.prec07<-prec07.cc-prec07
  rm(prec07.cc,prec07)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.prec07<-raster::resample(anomaly.prec07,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  prec07<-resample(raster("prec07.asc"),geocanadas)+anomaly.prec07
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(prec07,"prec07.asc",overwrite=TRUE)
  rm(prec07,anomaly.prec07)
  
  message("### PREC08 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  prec08.cc<-crop(raster("prec_8.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  prec08<-crop(raster("prec_8.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.prec08<-prec08.cc-prec08
  rm(prec08.cc,prec08)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.prec08<-raster::resample(anomaly.prec08,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  prec08<-resample(raster("prec08.asc"),geocanadas)+anomaly.prec08
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(prec08,"prec08.asc",overwrite=TRUE)
  rm(prec08,anomaly.prec08)
  
  message("### PREC09 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  prec09.cc<-crop(raster("prec_9.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  prec09<-crop(raster("prec_9.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.prec09<-prec09.cc-prec09
  rm(prec09.cc,prec09)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.prec09<-raster::resample(anomaly.prec09,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  prec09<-resample(raster("prec09.asc"),geocanadas)+anomaly.prec09
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(prec09,"prec09.asc",overwrite=TRUE)
  rm(prec09,anomaly.prec09)
  
  message("### PREC10 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  prec10.cc<-crop(raster("prec_10.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  prec10<-crop(raster("prec_10.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.prec10<-prec10.cc-prec10
  rm(prec10.cc,prec10)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.prec10<-raster::resample(anomaly.prec10,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  prec10<-resample(raster("prec10.asc"),geocanadas)+anomaly.prec10
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(prec10,"prec10.asc",overwrite=TRUE)
  rm(prec10,anomaly.prec10)
  
  message("### PREC11 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  prec11.cc<-crop(raster("prec_11.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  prec11<-crop(raster("prec_11.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.prec11<-prec11.cc-prec11
  rm(prec11.cc,prec11)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.prec11<-raster::resample(anomaly.prec11,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  prec11<-resample(raster("prec11.asc"),geocanadas)+anomaly.prec11
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(prec11,"prec11.asc",overwrite=TRUE)
  rm(prec11,anomaly.prec11)
  
  message("### PREC12 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  prec12.cc<-crop(raster("prec_12.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  prec12<-crop(raster("prec_12.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.prec12<-prec12.cc-prec12
  rm(prec12.cc,prec12)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.prec12<-raster::resample(anomaly.prec12,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  prec12<-resample(raster("prec12.asc"),geocanadas)+anomaly.prec12
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(prec12,"prec12.asc",overwrite=TRUE)
  rm(prec12,anomaly.prec12)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  file.remove("prec_1.asc","prec_2.asc","prec_3.asc","prec_4.asc",
              "prec_5.asc","prec_6.asc","prec_7.asc","prec_8.asc",
              "prec_9.asc","prec_10.asc","prec_11.asc","prec_12.asc")
  message("### MAXIMUM TEMPERATURE ####",appendLF = TRUE)
  message("### unzipping tmax ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  unzip("tmax.zip",overwrite=TRUE)
  message("### tmax01 ####",appendLF = TRUE)
  message("cropping future climate",appendLF=TRUE)
  tmax01.cc<-crop(raster("tmax_1.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmax01<-crop(raster("tmax_1.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmax01<-tmax01.cc-tmax01
  rm(tmax01.cc,tmax01)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmax01<-raster::resample(anomaly.tmax01,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmax01<-resample(raster("tmax01.asc"),geocanadas)+anomaly.tmax01
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmax01,"tmax01.asc",overwrite=TRUE)
  rm(tmax01,anomaly.tmax01)
  
  message("### tmax02 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  tmax02.cc<-crop(raster("tmax_2.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmax02<-crop(raster("tmax_2.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmax02<-tmax02.cc-tmax02
  rm(tmax02.cc,tmax02)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmax02<-raster::resample(anomaly.tmax02,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmax02<-resample(raster("tmax02.asc"),geocanadas)+anomaly.tmax02
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmax02,"tmax02.asc",overwrite=TRUE)
  rm(tmax02,anomaly.tmax02)
  
  message("### tmax03 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  tmax03.cc<-crop(raster("tmax_3.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmax03<-crop(raster("tmax_3.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmax03<-tmax03.cc-tmax03
  rm(tmax03.cc,tmax03)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmax03<-raster::resample(anomaly.tmax03,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmax03<-resample(raster("tmax03.asc"),geocanadas)+anomaly.tmax03
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmax03,"tmax03.asc",overwrite=TRUE)
  rm(tmax03,anomaly.tmax03)
  
  message("### tmax04 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  tmax04.cc<-crop(raster("tmax_4.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmax04<-crop(raster("tmax_4.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmax04<-tmax04.cc-tmax04
  rm(tmax04.cc,tmax04)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmax04<-raster::resample(anomaly.tmax04,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmax04<-resample(raster("tmax04.asc"),geocanadas)+anomaly.tmax04
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmax04,"tmax04.asc",overwrite=TRUE)
  rm(tmax04,anomaly.tmax04)
  
  message("### tmax05 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  tmax05.cc<-crop(raster("tmax_5.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmax05<-crop(raster("tmax_5.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmax05<-tmax05.cc-tmax05
  rm(tmax05.cc,tmax05)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmax05<-raster::resample(anomaly.tmax05,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmax05<-resample(raster("tmax05.asc"),geocanadas)+anomaly.tmax05
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmax05,"tmax05.asc",overwrite=TRUE)
  rm(tmax05,anomaly.tmax05)
  
  message("### tmax06 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  tmax06.cc<-crop(raster("tmax_6.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmax06<-crop(raster("tmax_6.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmax06<-tmax06.cc-tmax06
  rm(tmax06.cc,tmax06)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmax06<-raster::resample(anomaly.tmax06,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmax06<-resample(raster("tmax06.asc"),geocanadas)+anomaly.tmax06
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmax06,"tmax06.asc",overwrite=TRUE)
  rm(tmax06,anomaly.tmax06)
  
  message("### tmax07 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  tmax07.cc<-crop(raster("tmax_7.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmax07<-crop(raster("tmax_7.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmax07<-tmax07.cc-tmax07
  rm(tmax07.cc,tmax07)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmax07<-raster::resample(anomaly.tmax07,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmax07<-resample(raster("tmax07.asc"),geocanadas)+anomaly.tmax07
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmax07,"tmax07.asc",overwrite=TRUE)
  rm(tmax07,anomaly.tmax07)
  
  message("### tmax08 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  tmax08.cc<-crop(raster("tmax_8.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmax08<-crop(raster("tmax_8.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmax08<-tmax08.cc-tmax08
  rm(tmax08.cc,tmax08)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmax08<-raster::resample(anomaly.tmax08,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmax08<-resample(raster("tmax08.asc"),geocanadas)+anomaly.tmax08
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmax08,"tmax08.asc",overwrite=TRUE)
  rm(tmax08,anomaly.tmax08)
  
  message("### tmax09 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  tmax09.cc<-crop(raster("tmax_9.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmax09<-crop(raster("tmax_9.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmax09<-tmax09.cc-tmax09
  rm(tmax09.cc,tmax09)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmax09<-raster::resample(anomaly.tmax09,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmax09<-resample(raster("tmax09.asc"),geocanadas)+anomaly.tmax09
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmax09,"tmax09.asc",overwrite=TRUE)
  rm(tmax09,anomaly.tmax09)
  
  message("### tmax10 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  tmax10.cc<-crop(raster("tmax_10.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmax10<-crop(raster("tmax_10.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmax10<-tmax10.cc-tmax10
  rm(tmax10.cc,tmax10)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmax10<-raster::resample(anomaly.tmax10,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmax10<-resample(raster("tmax10.asc"),geocanadas)+anomaly.tmax10
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmax10,"tmax10.asc",overwrite=TRUE)
  rm(tmax10,anomaly.tmax10)
  
  message("### tmax11 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  tmax11.cc<-crop(raster("tmax_11.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmax11<-crop(raster("tmax_11.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmax11<-tmax11.cc-tmax11
  rm(tmax11.cc,tmax11)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmax11<-raster::resample(anomaly.tmax11,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmax11<-resample(raster("tmax11.asc"),geocanadas)+anomaly.tmax11
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmax11,"tmax11.asc",overwrite=TRUE)
  rm(tmax11,anomaly.tmax11)
  
  message("### tmax12 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  tmax12.cc<-crop(raster("tmax_12.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmax12<-crop(raster("tmax_12.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmax12<-tmax12.cc-tmax12
  rm(tmax12.cc,tmax12)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmax12<-raster::resample(anomaly.tmax12,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmax12<-resample(raster("tmax12.asc"),geocanadas)+anomaly.tmax12
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmax12,"tmax12.asc",overwrite=TRUE)
  rm(tmax12,anomaly.tmax12)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  file.remove("tmax_1.asc","tmax_2.asc","tmax_3.asc","tmax_4.asc",
              "tmax_5.asc","tmax_6.asc","tmax_7.asc","tmax_8.asc",
              "tmax_9.asc","tmax_10.asc","tmax_11.asc","tmax_12.asc")
  
  
  message("### MINIMUM TEMPERATURE ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("unzipping tmin",appendLF = TRUE)
  unzip("tmin.zip",overwrite=TRUE)
  message("### tmin01 ####",appendLF = TRUE)
  message("cropping future climate",appendLF=TRUE)
  tmin01.cc<-crop(raster("tmin_1.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmin01<-crop(raster("tmin_1.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmin01<-tmin01.cc-tmin01
  rm(tmin01.cc,tmin01)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmin01<-raster::resample(anomaly.tmin01,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmin01<-resample(raster("tmin01.asc"),geocanadas)+anomaly.tmin01
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmin01,"tmin01.asc",overwrite=TRUE)
  rm(tmin01,anomaly.tmin01)
  
  message("### tmin02 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  tmin02.cc<-crop(raster("tmin_2.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmin02<-crop(raster("tmin_2.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmin02<-tmin02.cc-tmin02
  rm(tmin02.cc,tmin02)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmin02<-raster::resample(anomaly.tmin02,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmin02<-resample(raster("tmin02.asc"),geocanadas)+anomaly.tmin02
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmin02,"tmin02.asc",overwrite=TRUE)
  rm(tmin02,anomaly.tmin02)
  
  message("### tmin03 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  tmin03.cc<-crop(raster("tmin_3.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmin03<-crop(raster("tmin_3.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmin03<-tmin03.cc-tmin03
  rm(tmin03.cc,tmin03)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmin03<-raster::resample(anomaly.tmin03,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmin03<-resample(raster("tmin03.asc"),geocanadas)+anomaly.tmin03
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmin03,"tmin03.asc",overwrite=TRUE)
  rm(tmin03,anomaly.tmin03)
  
  message("### tmin04 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  tmin04.cc<-crop(raster("tmin_4.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmin04<-crop(raster("tmin_4.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmin04<-tmin04.cc-tmin04
  rm(tmin04.cc,tmin04)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmin04<-raster::resample(anomaly.tmin04,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmin04<-resample(raster("tmin04.asc"),geocanadas)+anomaly.tmin04
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmin04,"tmin04.asc",overwrite=TRUE)
  rm(tmin04,anomaly.tmin04)
  
  message("### tmin05 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  tmin05.cc<-crop(raster("tmin_5.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmin05<-crop(raster("tmin_5.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmin05<-tmin05.cc-tmin05
  rm(tmin05.cc,tmin05)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmin05<-raster::resample(anomaly.tmin05,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmin05<-resample(raster("tmin05.asc"),geocanadas)+anomaly.tmin05
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmin05,"tmin05.asc",overwrite=TRUE)
  rm(tmin05,anomaly.tmin05)
  
  message("### tmin06 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  tmin06.cc<-crop(raster("tmin_6.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmin06<-crop(raster("tmin_6.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmin06<-tmin06.cc-tmin06
  rm(tmin06.cc,tmin06)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmin06<-raster::resample(anomaly.tmin06,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmin06<-resample(raster("tmin06.asc"),geocanadas)+anomaly.tmin06
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmin06,"tmin06.asc",overwrite=TRUE)
  rm(tmin06,anomaly.tmin06)
  
  message("### tmin07 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  tmin07.cc<-crop(raster("tmin_7.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmin07<-crop(raster("tmin_7.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmin07<-tmin07.cc-tmin07
  rm(tmin07.cc,tmin07)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmin07<-raster::resample(anomaly.tmin07,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmin07<-resample(raster("tmin07.asc"),geocanadas)+anomaly.tmin07
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmin07,"tmin07.asc",overwrite=TRUE)
  rm(tmin07,anomaly.tmin07)
  
  message("### tmin08 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  tmin08.cc<-crop(raster("tmin_8.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmin08<-crop(raster("tmin_8.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmin08<-tmin08.cc-tmin08
  rm(tmin08.cc,tmin08)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmin08<-raster::resample(anomaly.tmin08,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmin08<-resample(raster("tmin08.asc"),geocanadas)+anomaly.tmin08
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmin08,"tmin08.asc",overwrite=TRUE)
  rm(tmin08,anomaly.tmin08)
  
  message("### tmin09 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  tmin09.cc<-crop(raster("tmin_9.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmin09<-crop(raster("tmin_9.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmin09<-tmin09.cc-tmin09
  rm(tmin09.cc,tmin09)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmin09<-raster::resample(anomaly.tmin09,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmin09<-resample(raster("tmin09.asc"),geocanadas)+anomaly.tmin09
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmin09,"tmin09.asc",overwrite=TRUE)
  rm(tmin09,anomaly.tmin09)
  
  message("### tmin10 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  tmin10.cc<-crop(raster("tmin_10.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmin10<-crop(raster("tmin_10.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmin10<-tmin10.cc-tmin10
  rm(tmin10.cc,tmin10)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmin10<-raster::resample(anomaly.tmin10,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmin10<-resample(raster("tmin10.asc"),geocanadas)+anomaly.tmin10
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmin10,"tmin10.asc",overwrite=TRUE)
  rm(tmin10,anomaly.tmin10)
  
  message("### tmin11 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  tmin11.cc<-crop(raster("tmin_11.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmin11<-crop(raster("tmin_11.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmin11<-tmin11.cc-tmin11
  rm(tmin11.cc,tmin11)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmin11<-raster::resample(anomaly.tmin11,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmin11<-resample(raster("tmin11.asc"),geocanadas)+anomaly.tmin11
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmin11,"tmin11.asc",overwrite=TRUE)
  rm(tmin11,anomaly.tmin11)
  
  message("### tmin12 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  tmin12.cc<-crop(raster("tmin_12.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmin12<-crop(raster("tmin_12.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmin12<-tmin12.cc-tmin12
  rm(tmin12.cc,tmin12)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmin12<-raster::resample(anomaly.tmin12,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmin12<-resample(raster("tmin12.asc"),geocanadas)+anomaly.tmin12
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmin12,"tmin12.asc",overwrite=TRUE)
  rm(tmin12,anomaly.tmin12)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  file.remove("tmin_1.asc","tmin_2.asc","tmin_3.asc","tmin_4.asc",
              "tmin_5.asc","tmin_6.asc","tmin_7.asc","tmin_8.asc",
              "tmin_9.asc","tmin_10.asc","tmin_11.asc","tmin_12.asc")
}            

models<-c("miroc","csiro")
RCPs<-c(26,60,85)
years<-c(30,50,80)

miroc
  26
    30
    50
    80 ### problema aquí
  60
    30
    50
    80
  85
    30
    50
    80
#csiro
  26
    #30
    #50
    #80
  60
    #30
    #50
    #80
  85
    30
    50
    80
    

#Loop for anomalies every model, every RCP, every year
for (i in models){
model<-i
  for (j in RCPs){
    rcp<-j
      for (h in years){
        year<-h
        anomaly(i,j,h)
      }
  }
}
  
#### 2.1 BIOVARIABLES ####

biov <- function (model, rcp, year) {
  setwd(paste(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas",model,rcp,year,sep="/")))
  prec<-stack(raster("prec01.asc"),raster("prec02.asc"),raster("prec03.asc"),raster("prec04.asc")
              ,raster("prec05.asc"),raster("prec06.asc"),raster("prec07.asc"),raster("prec08.asc")
              ,raster("prec09.asc"),raster("prec10.asc"),raster("prec11.asc"),raster("prec12.asc"))
  tmin<-stack(raster("tmin01.asc"),raster("tmin02.asc"),raster("tmin03.asc"),raster("tmin04.asc")
              ,raster("tmin05.asc"),raster("tmin06.asc"),raster("tmin07.asc"),raster("tmin08.asc")
              ,raster("tmin09.asc"),raster("tmin10.asc"),raster("tmin11.asc"),raster("tmin12.asc"))
  tmax<-stack(raster("tmax01.asc"),raster("tmax02.asc"),raster("tmax03.asc"),raster("tmax04.asc")
              ,raster("tmax05.asc"),raster("tmax06.asc"),raster("tmax07.asc"),raster("tmax08.asc")
              ,raster("tmax09.asc"),raster("tmax10.asc"),raster("tmax11.asc"),raster("tmax12.asc"))
  message(paste("computing biovariables for ",model,rcp,year,sep=", "))
  biovariables<-biovars(prec,tmin,tmax)
  message(paste("writing biovariables for ",model,rcp,year,sep=", "))
  writeRaster(biovariables$bio1,"bio01.asc",overwrite=T)
  writeRaster(biovariables$bio2,"bio02.asc",overwrite=T)
  writeRaster(biovariables$bio3,"bio03.asc",overwrite=T)
  writeRaster(biovariables$bio3,"bio03.asc",overwrite=T)
  writeRaster(biovariables$bio5,"bio05.asc",overwrite=T)
  writeRaster(biovariables$bio6,"bio06.asc",overwrite=T)
  writeRaster(biovariables$bio7,"bio07.asc",overwrite=T)
  writeRaster(biovariables$bio8,"bio08.asc",overwrite=T)
  writeRaster(biovariables$bio9,"bio09.asc",overwrite=T)
  writeRaster(biovariables$bio10,"bio10.asc",overwrite=T)
  writeRaster(biovariables$bio11,"bio11.asc",overwrite=T)
  writeRaster(biovariables$bio12,"bio12.asc",overwrite=T)
  writeRaster(biovariables$bio13,"bio13.asc",overwrite=T)
  writeRaster(biovariables$bio14,"bio14.asc",overwrite=T)
  writeRaster(biovariables$bio15,"bio15.asc",overwrite=T)
  writeRaster(biovariables$bio16,"bio16.asc",overwrite=T)
  writeRaster(biovariables$bio17,"bio17.asc",overwrite=T)
  writeRaster(biovariables$bio18,"bio18.asc",overwrite=T)
  writeRaster(biovariables$bio19,"bio19.asc",overwrite=T)
}

#Loop for biovariables every model, every RCP, every year
for (i in models){
  model<-i
  for (j in RCPs){
    rcp<-j
    for (h in years){
      year<-h
      biov(i,j,h)
    }
  }
}    

##### 2.2 Evapotransp #####

#para el presente
setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")

rad<-c("rad01.txt","rad02.txt","rad03.txt","rad04.txt","rad05.txt","rad06.txt","rad07.txt","rad08.txt"
       ,"rad09.txt","rad10.txt","rad11.txt","rad12.txt")

prec<-c("prec01.asc","prec02.asc","prec03.asc","prec04.asc","prec05.asc","prec06.asc","prec07.asc","prec08.asc"
        ,"prec09.asc","prec10.asc","prec11.asc","prec12.asc")

tmin<-c("tmin01.asc","tmin02.asc","tmin03.asc","tmin04.asc","tmin05.asc","tmin06.asc","tmin07.asc","tmin08.asc"
        ,"tmin09.asc","tmin10.asc","tmin11.asc","tmin12.asc")

tmax<-c("tmax01.asc","tmax02.asc","tmax03.asc","tmax04.asc","tmax05.asc","tmax06.asc","tmax07.asc","tmax08.asc"
        ,"tmax09.asc","tmax10.asc","tmax11.asc","tmax12.asc")

writeRaster(mean(tmin01,tmax01),"tmean01.asc",overwrite=TRUE)
writeRaster(mean(tmin02,tmax02),"tmean02.asc",overwrite=TRUE)
writeRaster(mean(tmin03,tmax03),"tmean03.asc",overwrite=TRUE)
writeRaster(mean(tmin04,tmax04),"tmean04.asc",overwrite=TRUE)
writeRaster(mean(tmin05,tmax05),"tmean05.asc",overwrite=TRUE)
writeRaster(mean(tmin06,tmax06),"tmean06.asc",overwrite=TRUE)
writeRaster(mean(tmin07,tmax07),"tmean07.asc",overwrite=TRUE)
writeRaster(mean(tmin08,tmax08),"tmean08.asc",overwrite=TRUE)
writeRaster(mean(tmin09,tmax09),"tmean09.asc",overwrite=TRUE)
writeRaster(mean(tmin10,tmax10),"tmean10.asc",overwrite=TRUE)
writeRaster(mean(tmin11,tmax11),"tmean11.asc",overwrite=TRUE)
writeRaster(mean(tmin12,tmax12),"tmean12.asc",overwrite=TRUE)

tmean<-c("tmean01.asc","tmean02.asc","tmean03.asc","tmean04.asc","tmean05.asc","tmean06.asc","tmean07.asc","tmean08.asc"
         ,"tmean09.asc","tmean10.asc","tmean11.asc","tmean12.asc")


petfiles<-c("pet01.asc","pet02.asc","pet03.asc","pet04.asc","pet05.asc","pet06.asc","pet07.asc","pet08.asc"
            ,"pet09.asc","pet10.asc","pet11.asc","pet12.asc")
message("calculating PET",appendLF = TRUE)
pet<-batchPetHgsm(petfiles, 01, tmin, tmean, tmax, rad)

#evapotrans is a function to calculate evapotrans
setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
writeRaster(resample(raster("rad01.txt"),geocanadas),"rad01.asc",overwrite=TRUE)
writeRaster(resample(raster("rad02.txt"),geocanadas),"rad02.asc",overwrite=TRUE)
writeRaster(resample(raster("rad03.txt"),geocanadas),"rad03.asc",overwrite=TRUE)
writeRaster(resample(raster("rad04.txt"),geocanadas),"rad04.asc",overwrite=TRUE)
writeRaster(resample(raster("rad05.txt"),geocanadas),"rad05.asc",overwrite=TRUE)
writeRaster(resample(raster("rad06.txt"),geocanadas),"rad06.asc",overwrite=TRUE)
writeRaster(resample(raster("rad07.txt"),geocanadas),"rad07.asc",overwrite=TRUE)
writeRaster(resample(raster("rad08.txt"),geocanadas),"rad08.asc",overwrite=TRUE)
writeRaster(resample(raster("rad09.txt"),geocanadas),"rad09.asc",overwrite=TRUE)
writeRaster(resample(raster("rad10.txt"),geocanadas),"rad10.asc",overwrite=TRUE)
writeRaster(resample(raster("rad11.txt"),geocanadas),"rad11.asc",overwrite=TRUE)
writeRaster(resample(raster("rad12.txt"),geocanadas),"rad12.asc",overwrite=TRUE)


evapotrans<-function(model,rcp,year) {
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas",model,rcp,year,sep="/"))
  message(paste(model,rcp,year,sep=" "),appendLF = TRUE)
  message("setting rasterstack",appendLF = TRUE)
  petfiles<-c("pet01.asc","pet02.asc","pet03.asc","pet04.asc","pet05.asc","pet06.asc","pet07.asc","pet08.asc"
              ,"pet09.asc","pet10.asc","pet11.asc","pet12.asc")
 
  rad<-c("rad01.asc","rad02.asc","rad03.asc","rad04.asc","rad05.asc","rad06.asc","rad07.asc","rad08.asc"
         ,"rad09.asc","rad10.asc","rad11.asc","rad12.asc")
  message("preparing files",appendLF = TRUE)
  
  prec<-c("prec01.asc","prec02.asc","prec03.asc","prec04.asc","prec05.asc","prec06.asc","prec07.asc","prec08.asc"
          ,"prec09.asc","prec10.asc","prec11.asc","prec12.asc")
  
  tmin<-c("tmin01.asc","tmin02.asc","tmin03.asc","tmin04.asc","tmin05.asc","tmin06.asc","tmin07.asc","tmin08.asc"
          ,"tmin09.asc","tmin10.asc","tmin11.asc","tmin12.asc")
  
  tmax<-c("tmax01.asc","tmax02.asc","tmax03.asc","tmax04.asc","tmax05.asc","tmax06.asc","tmax07.asc","tmax08.asc"
          ,"tmax09.asc","tmax10.asc","tmax11.asc","tmax12.asc")
  message("calculating temperature means",appendLF = TRUE)
  message("calculating tmean 01",appendLF = TRUE)
  writeRaster(mean(raster("tmin01.asc"),raster("tmax01.asc")),"tmean01.asc",overwrite=TRUE)
  message("calculating tmean 02",appendLF = TRUE)
  writeRaster(mean(raster("tmin02.asc"),raster("tmax02.asc")),"tmean02.asc",overwrite=TRUE)
  message("calculating tmean 03",appendLF = TRUE)
  writeRaster(mean(raster("tmin03.asc"),raster("tmax03.asc")),"tmean03.asc",overwrite=TRUE)
  message("calculating tmean 04",appendLF = TRUE)
  writeRaster(mean(raster("tmin04.asc"),raster("tmax04.asc")),"tmean04.asc",overwrite=TRUE)
  message("calculating tmean 05",appendLF = TRUE)
  writeRaster(mean(raster("tmin05.asc"),raster("tmax05.asc")),"tmean05.asc",overwrite=TRUE)
  message("calculating tmean 06",appendLF = TRUE)
  writeRaster(mean(raster("tmin06.asc"),raster("tmax06.asc")),"tmean06.asc",overwrite=TRUE)
  message("calculating tmean 07",appendLF = TRUE)
  writeRaster(mean(raster("tmin07.asc"),raster("tmax07.asc")),"tmean07.asc",overwrite=TRUE)
  message("calculating tmean 08",appendLF = TRUE)
  writeRaster(mean(raster("tmin08.asc"),raster("tmax08.asc")),"tmean08.asc",overwrite=TRUE)
  message("calculating tmean 09",appendLF = TRUE)
  writeRaster(mean(raster("tmin09.asc"),raster("tmax09.asc")),"tmean09.asc",overwrite=TRUE)
  message("calculating tmean 10",appendLF = TRUE)
  writeRaster(mean(raster("tmin10.asc"),raster("tmax10.asc")),"tmean10.asc",overwrite=TRUE)
  message("calculating tmean 11",appendLF = TRUE)
  writeRaster(mean(raster("tmin11.asc"),raster("tmax11.asc")),"tmean11.asc",overwrite=TRUE)
  message("calculating tmean 12",appendLF = TRUE)
  writeRaster(mean(raster("tmin12.asc"),raster("tmax12.asc")),"tmean12.asc",overwrite=TRUE)
  message("calculating tmean 12",appendLF = TRUE)
  
  tmean<-c("tmean01.asc","tmean02.asc","tmean03.asc","tmean04.asc","tmean05.asc","tmean06.asc","tmean07.asc","tmean08.asc"
           ,"tmean09.asc","tmean10.asc","tmean11.asc","tmean12.asc")
  message("Processing PET",appendLF = TRUE)
  pet<-batchPetHgsm(petfiles, 01, tmin, tmean, tmax, rad)
  message("calculating PET mean",appendLF = TRUE)
  pet<-mean(stack(raster("pet01.asc"),raster("pet02.asc"),raster("pet03.asc"),raster("pet04.asc"),raster("pet05.asc"),raster("pet06.asc"),
                  raster("pet07.asc"),raster("pet08.asc"),raster("pet09.asc"),raster("pet10.asc"),raster("pet11.asc"),raster("pet12.asc")))
  writeRaster(pet,"pet.asc",overwrite=TRUE)
}

for (i in models){
  model<-i
  for (j in RCPs){
    rcp<-j
    for (h in years){
      year<-h
      evapotrans(i,j,h)
    }
  }
}


#### 2.3 Anomalies abrev ####

anomaly<- function (model,rcp,year) {
  variable.list<-c("prec","tmin","tmax")
  initial.months<-c("1","2","3","4","5","6","7","8","9","10","11","12")
  final.months<-c("01","02","03","04","05","06","07","08","09","10","11","12")
  #ir al directorio donde están guardados los archivos
  #DESCOMPRIMIR ANTES 
  
  for (i in variable.list){
  message(paste("initializing",i,sep=" "))
  target.variable<-i        
  
  message(paste ("Unzipping", i, sep=" ") ,appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  unzip(paste(i,".zip",sep=""),overwrite=TRUE)  
    for (j in initial.months){
  initial.month<-j    
  message(paste("cropping future", i, j, sep=""),appendLF=TRUE)
  variable.cc<-crop(raster(paste(i,"_",j,".asc",sep="")),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  variable<-crop(raster(paste(i,"_",j,".bil",sep="")),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly<-variable.cc-variable
  rm(variable.cc,variable)
  message("resampling anomaly",appendLF = TRUE)
  anomaly<-raster::resample(anomaly,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  variable<-resample(raster(paste(i,final.months[j],".asc",sep="")),geocanadas)+anomaly
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(variable,paste(i,final.months[j],".asc"),overwrite=TRUE)
  rm(variable,anomaly)
    }
  }
}
  
  
  
 
  rm(prec01,anomaly.prec01)
  
  message("### PREC02 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  prec02.cc<-crop(raster("prec_2.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  prec02<-crop(raster("prec_2.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.prec02<-prec02.cc-prec02
  rm(prec02.cc,prec02)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.prec02<-raster::resample(anomaly.prec02,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  prec02<-resample(raster("prec02.asc"),geocanadas)+anomaly.prec02
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(prec02,"prec02.asc",overwrite=TRUE)
  rm(prec02,anomaly.prec02)
  
  message("### PREC03 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  prec03.cc<-crop(raster("prec_3.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  prec03<-crop(raster("prec_3.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.prec03<-prec03.cc-prec03
  rm(prec03.cc,prec03)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.prec03<-raster::resample(anomaly.prec03,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  prec03<-resample(raster("prec03.asc"),geocanadas)+anomaly.prec03
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(prec03,"prec03.asc",overwrite=TRUE)
  rm(prec03,anomaly.prec03)
  
  message("### PREC04 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  prec04.cc<-crop(raster("prec_4.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  prec04<-crop(raster("prec_4.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.prec04<-prec04.cc-prec04
  rm(prec04.cc,prec04)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.prec04<-raster::resample(anomaly.prec04,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  prec04<-resample(raster("prec04.asc"),geocanadas)+anomaly.prec04
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(prec04,"prec04.asc",overwrite=TRUE)
  rm(prec04,anomaly.prec04)
  
  message("### PREC05 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  prec05.cc<-crop(raster("prec_5.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  prec05<-crop(raster("prec_5.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.prec05<-prec05.cc-prec05
  rm(prec05.cc,prec05)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.prec05<-raster::resample(anomaly.prec05,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  prec05<-resample(raster("prec05.asc"),geocanadas)+anomaly.prec05
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(prec05,"prec05.asc",overwrite=TRUE)
  rm(prec05,anomaly.prec05)
  
  message("### PREC06 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  prec06.cc<-crop(raster("prec_6.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  prec06<-crop(raster("prec_6.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.prec06<-prec06.cc-prec06
  rm(prec06.cc,prec06)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.prec06<-raster::resample(anomaly.prec06,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  prec06<-resample(raster("prec06.asc"),geocanadas)+anomaly.prec06
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(prec06,"prec06.asc",overwrite=TRUE)
  rm(prec06,anomaly.prec06)
  
  message("### PREC07 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  prec07.cc<-crop(raster("prec_7.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  prec07<-crop(raster("prec_7.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.prec07<-prec07.cc-prec07
  rm(prec07.cc,prec07)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.prec07<-raster::resample(anomaly.prec07,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  prec07<-resample(raster("prec07.asc"),geocanadas)+anomaly.prec07
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(prec07,"prec07.asc",overwrite=TRUE)
  rm(prec07,anomaly.prec07)
  
  message("### PREC08 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  prec08.cc<-crop(raster("prec_8.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  prec08<-crop(raster("prec_8.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.prec08<-prec08.cc-prec08
  rm(prec08.cc,prec08)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.prec08<-raster::resample(anomaly.prec08,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  prec08<-resample(raster("prec08.asc"),geocanadas)+anomaly.prec08
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(prec08,"prec08.asc",overwrite=TRUE)
  rm(prec08,anomaly.prec08)
  
  message("### PREC09 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  prec09.cc<-crop(raster("prec_9.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  prec09<-crop(raster("prec_9.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.prec09<-prec09.cc-prec09
  rm(prec09.cc,prec09)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.prec09<-raster::resample(anomaly.prec09,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  prec09<-resample(raster("prec09.asc"),geocanadas)+anomaly.prec09
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(prec09,"prec09.asc",overwrite=TRUE)
  rm(prec09,anomaly.prec09)
  
  message("### PREC10 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  prec10.cc<-crop(raster("prec_10.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  prec10<-crop(raster("prec_10.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.prec10<-prec10.cc-prec10
  rm(prec10.cc,prec10)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.prec10<-raster::resample(anomaly.prec10,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  prec10<-resample(raster("prec10.asc"),geocanadas)+anomaly.prec10
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(prec10,"prec10.asc",overwrite=TRUE)
  rm(prec10,anomaly.prec10)
  
  message("### PREC11 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  prec11.cc<-crop(raster("prec_11.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  prec11<-crop(raster("prec_11.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.prec11<-prec11.cc-prec11
  rm(prec11.cc,prec11)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.prec11<-raster::resample(anomaly.prec11,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  prec11<-resample(raster("prec11.asc"),geocanadas)+anomaly.prec11
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(prec11,"prec11.asc",overwrite=TRUE)
  rm(prec11,anomaly.prec11)
  
  message("### PREC12 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  prec12.cc<-crop(raster("prec_12.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  prec12<-crop(raster("prec_12.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.prec12<-prec12.cc-prec12
  rm(prec12.cc,prec12)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.prec12<-raster::resample(anomaly.prec12,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  prec12<-resample(raster("prec12.asc"),geocanadas)+anomaly.prec12
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(prec12,"prec12.asc",overwrite=TRUE)
  rm(prec12,anomaly.prec12)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  file.remove("prec_1.asc","prec_2.asc","prec_3.asc","prec_4.asc",
              "prec_5.asc","prec_6.asc","prec_7.asc","prec_8.asc",
              "prec_9.asc","prec_10.asc","prec_11.asc","prec_12.asc")
  message("### MAXIMUM TEMPERATURE ####",appendLF = TRUE)
  message("### unzipping tmax ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  unzip("tmax.zip",overwrite=TRUE)
  message("### tmax01 ####",appendLF = TRUE)
  message("cropping future climate",appendLF=TRUE)
  tmax01.cc<-crop(raster("tmax_1.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmax01<-crop(raster("tmax_1.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmax01<-tmax01.cc-tmax01
  rm(tmax01.cc,tmax01)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmax01<-raster::resample(anomaly.tmax01,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmax01<-resample(raster("tmax01.asc"),geocanadas)+anomaly.tmax01
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmax01,"tmax01.asc",overwrite=TRUE)
  rm(tmax01,anomaly.tmax01)
  
  message("### tmax02 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  tmax02.cc<-crop(raster("tmax_2.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmax02<-crop(raster("tmax_2.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmax02<-tmax02.cc-tmax02
  rm(tmax02.cc,tmax02)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmax02<-raster::resample(anomaly.tmax02,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmax02<-resample(raster("tmax02.asc"),geocanadas)+anomaly.tmax02
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmax02,"tmax02.asc",overwrite=TRUE)
  rm(tmax02,anomaly.tmax02)
  
  message("### tmax03 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  tmax03.cc<-crop(raster("tmax_3.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmax03<-crop(raster("tmax_3.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmax03<-tmax03.cc-tmax03
  rm(tmax03.cc,tmax03)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmax03<-raster::resample(anomaly.tmax03,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmax03<-resample(raster("tmax03.asc"),geocanadas)+anomaly.tmax03
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmax03,"tmax03.asc",overwrite=TRUE)
  rm(tmax03,anomaly.tmax03)
  
  message("### tmax04 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  tmax04.cc<-crop(raster("tmax_4.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmax04<-crop(raster("tmax_4.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmax04<-tmax04.cc-tmax04
  rm(tmax04.cc,tmax04)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmax04<-raster::resample(anomaly.tmax04,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmax04<-resample(raster("tmax04.asc"),geocanadas)+anomaly.tmax04
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmax04,"tmax04.asc",overwrite=TRUE)
  rm(tmax04,anomaly.tmax04)
  
  message("### tmax05 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  tmax05.cc<-crop(raster("tmax_5.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmax05<-crop(raster("tmax_5.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmax05<-tmax05.cc-tmax05
  rm(tmax05.cc,tmax05)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmax05<-raster::resample(anomaly.tmax05,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmax05<-resample(raster("tmax05.asc"),geocanadas)+anomaly.tmax05
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmax05,"tmax05.asc",overwrite=TRUE)
  rm(tmax05,anomaly.tmax05)
  
  message("### tmax06 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  tmax06.cc<-crop(raster("tmax_6.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmax06<-crop(raster("tmax_6.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmax06<-tmax06.cc-tmax06
  rm(tmax06.cc,tmax06)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmax06<-raster::resample(anomaly.tmax06,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmax06<-resample(raster("tmax06.asc"),geocanadas)+anomaly.tmax06
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmax06,"tmax06.asc",overwrite=TRUE)
  rm(tmax06,anomaly.tmax06)
  
  message("### tmax07 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  tmax07.cc<-crop(raster("tmax_7.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmax07<-crop(raster("tmax_7.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmax07<-tmax07.cc-tmax07
  rm(tmax07.cc,tmax07)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmax07<-raster::resample(anomaly.tmax07,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmax07<-resample(raster("tmax07.asc"),geocanadas)+anomaly.tmax07
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmax07,"tmax07.asc",overwrite=TRUE)
  rm(tmax07,anomaly.tmax07)
  
  message("### tmax08 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  tmax08.cc<-crop(raster("tmax_8.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmax08<-crop(raster("tmax_8.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmax08<-tmax08.cc-tmax08
  rm(tmax08.cc,tmax08)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmax08<-raster::resample(anomaly.tmax08,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmax08<-resample(raster("tmax08.asc"),geocanadas)+anomaly.tmax08
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmax08,"tmax08.asc",overwrite=TRUE)
  rm(tmax08,anomaly.tmax08)
  
  message("### tmax09 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  tmax09.cc<-crop(raster("tmax_9.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmax09<-crop(raster("tmax_9.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmax09<-tmax09.cc-tmax09
  rm(tmax09.cc,tmax09)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmax09<-raster::resample(anomaly.tmax09,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmax09<-resample(raster("tmax09.asc"),geocanadas)+anomaly.tmax09
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmax09,"tmax09.asc",overwrite=TRUE)
  rm(tmax09,anomaly.tmax09)
  
  message("### tmax10 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  tmax10.cc<-crop(raster("tmax_10.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmax10<-crop(raster("tmax_10.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmax10<-tmax10.cc-tmax10
  rm(tmax10.cc,tmax10)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmax10<-raster::resample(anomaly.tmax10,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmax10<-resample(raster("tmax10.asc"),geocanadas)+anomaly.tmax10
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmax10,"tmax10.asc",overwrite=TRUE)
  rm(tmax10,anomaly.tmax10)
  
  message("### tmax11 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  tmax11.cc<-crop(raster("tmax_11.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmax11<-crop(raster("tmax_11.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmax11<-tmax11.cc-tmax11
  rm(tmax11.cc,tmax11)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmax11<-raster::resample(anomaly.tmax11,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmax11<-resample(raster("tmax11.asc"),geocanadas)+anomaly.tmax11
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmax11,"tmax11.asc",overwrite=TRUE)
  rm(tmax11,anomaly.tmax11)
  
  message("### tmax12 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  tmax12.cc<-crop(raster("tmax_12.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmax12<-crop(raster("tmax_12.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmax12<-tmax12.cc-tmax12
  rm(tmax12.cc,tmax12)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmax12<-raster::resample(anomaly.tmax12,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmax12<-resample(raster("tmax12.asc"),geocanadas)+anomaly.tmax12
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmax12,"tmax12.asc",overwrite=TRUE)
  rm(tmax12,anomaly.tmax12)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  file.remove("tmax_1.asc","tmax_2.asc","tmax_3.asc","tmax_4.asc",
              "tmax_5.asc","tmax_6.asc","tmax_7.asc","tmax_8.asc",
              "tmax_9.asc","tmax_10.asc","tmax_11.asc","tmax_12.asc")
  
  
  message("### MINIMUM TEMPERATURE ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("unzipping tmin",appendLF = TRUE)
  unzip("tmin.zip",overwrite=TRUE)
  message("### tmin01 ####",appendLF = TRUE)
  message("cropping future climate",appendLF=TRUE)
  tmin01.cc<-crop(raster("tmin_1.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmin01<-crop(raster("tmin_1.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmin01<-tmin01.cc-tmin01
  rm(tmin01.cc,tmin01)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmin01<-raster::resample(anomaly.tmin01,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmin01<-resample(raster("tmin01.asc"),geocanadas)+anomaly.tmin01
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmin01,"tmin01.asc",overwrite=TRUE)
  rm(tmin01,anomaly.tmin01)
  
  message("### tmin02 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  tmin02.cc<-crop(raster("tmin_2.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmin02<-crop(raster("tmin_2.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmin02<-tmin02.cc-tmin02
  rm(tmin02.cc,tmin02)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmin02<-raster::resample(anomaly.tmin02,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmin02<-resample(raster("tmin02.asc"),geocanadas)+anomaly.tmin02
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmin02,"tmin02.asc",overwrite=TRUE)
  rm(tmin02,anomaly.tmin02)
  
  message("### tmin03 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  tmin03.cc<-crop(raster("tmin_3.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmin03<-crop(raster("tmin_3.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmin03<-tmin03.cc-tmin03
  rm(tmin03.cc,tmin03)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmin03<-raster::resample(anomaly.tmin03,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmin03<-resample(raster("tmin03.asc"),geocanadas)+anomaly.tmin03
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmin03,"tmin03.asc",overwrite=TRUE)
  rm(tmin03,anomaly.tmin03)
  
  message("### tmin04 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  tmin04.cc<-crop(raster("tmin_4.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmin04<-crop(raster("tmin_4.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmin04<-tmin04.cc-tmin04
  rm(tmin04.cc,tmin04)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmin04<-raster::resample(anomaly.tmin04,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmin04<-resample(raster("tmin04.asc"),geocanadas)+anomaly.tmin04
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmin04,"tmin04.asc",overwrite=TRUE)
  rm(tmin04,anomaly.tmin04)
  
  message("### tmin05 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  tmin05.cc<-crop(raster("tmin_5.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmin05<-crop(raster("tmin_5.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmin05<-tmin05.cc-tmin05
  rm(tmin05.cc,tmin05)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmin05<-raster::resample(anomaly.tmin05,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmin05<-resample(raster("tmin05.asc"),geocanadas)+anomaly.tmin05
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmin05,"tmin05.asc",overwrite=TRUE)
  rm(tmin05,anomaly.tmin05)
  
  message("### tmin06 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  tmin06.cc<-crop(raster("tmin_6.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmin06<-crop(raster("tmin_6.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmin06<-tmin06.cc-tmin06
  rm(tmin06.cc,tmin06)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmin06<-raster::resample(anomaly.tmin06,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmin06<-resample(raster("tmin06.asc"),geocanadas)+anomaly.tmin06
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmin06,"tmin06.asc",overwrite=TRUE)
  rm(tmin06,anomaly.tmin06)
  
  message("### tmin07 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  tmin07.cc<-crop(raster("tmin_7.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmin07<-crop(raster("tmin_7.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmin07<-tmin07.cc-tmin07
  rm(tmin07.cc,tmin07)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmin07<-raster::resample(anomaly.tmin07,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmin07<-resample(raster("tmin07.asc"),geocanadas)+anomaly.tmin07
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmin07,"tmin07.asc",overwrite=TRUE)
  rm(tmin07,anomaly.tmin07)
  
  message("### tmin08 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  tmin08.cc<-crop(raster("tmin_8.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmin08<-crop(raster("tmin_8.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmin08<-tmin08.cc-tmin08
  rm(tmin08.cc,tmin08)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmin08<-raster::resample(anomaly.tmin08,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmin08<-resample(raster("tmin08.asc"),geocanadas)+anomaly.tmin08
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmin08,"tmin08.asc",overwrite=TRUE)
  rm(tmin08,anomaly.tmin08)
  
  message("### tmin09 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  tmin09.cc<-crop(raster("tmin_9.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmin09<-crop(raster("tmin_9.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmin09<-tmin09.cc-tmin09
  rm(tmin09.cc,tmin09)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmin09<-raster::resample(anomaly.tmin09,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmin09<-resample(raster("tmin09.asc"),geocanadas)+anomaly.tmin09
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmin09,"tmin09.asc",overwrite=TRUE)
  rm(tmin09,anomaly.tmin09)
  
  message("### tmin10 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  tmin10.cc<-crop(raster("tmin_10.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmin10<-crop(raster("tmin_10.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmin10<-tmin10.cc-tmin10
  rm(tmin10.cc,tmin10)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmin10<-raster::resample(anomaly.tmin10,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmin10<-resample(raster("tmin10.asc"),geocanadas)+anomaly.tmin10
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmin10,"tmin10.asc",overwrite=TRUE)
  rm(tmin10,anomaly.tmin10)
  
  message("### tmin11 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  tmin11.cc<-crop(raster("tmin_11.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmin11<-crop(raster("tmin_11.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmin11<-tmin11.cc-tmin11
  rm(tmin11.cc,tmin11)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmin11<-raster::resample(anomaly.tmin11,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmin11<-resample(raster("tmin11.asc"),geocanadas)+anomaly.tmin11
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmin11,"tmin11.asc",overwrite=TRUE)
  rm(tmin11,anomaly.tmin11)
  
  message("### tmin12 ####",appendLF = TRUE)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  message("cropping future climate",appendLF=TRUE)
  tmin12.cc<-crop(raster("tmin_12.asc"),geocanadas)
  setwd("F:/Worldclim")
  message("cropping worldclim",appendLF=TRUE)
  tmin12<-crop(raster("tmin_12.bil"),geocanadas)
  message("calculating general anomaly",appendLF = TRUE)
  anomaly.tmin12<-tmin12.cc-tmin12
  rm(tmin12.cc,tmin12)
  message("resampling anomaly",appendLF = TRUE)
  anomaly.tmin12<-raster::resample(anomaly.tmin12,geocanadas)
  setwd("C:/Users/agonz/Documents/Viola/Tenerife/Canadas")
  tmin12<-resample(raster("tmin12.asc"),geocanadas)+anomaly.tmin12
  setwd(paste("C:/Users/agonz/Documents/Viola/TenerifeCC/Canadas", model, rcp, year, sep = "/"))
  message("write new raster",appendLF = TRUE)
  writeRaster(tmin12,"tmin12.asc",overwrite=TRUE)
  rm(tmin12,anomaly.tmin12)
  setwd(paste("F:/Climatechange",model,rcp,year,sep="/"))
  file.remove("tmin_1.asc","tmin_2.asc","tmin_3.asc","tmin_4.asc",
              "tmin_5.asc","tmin_6.asc","tmin_7.asc","tmin_8.asc",
              "tmin_9.asc","tmin_10.asc","tmin_11.asc","tmin_12.asc")
}       
#### 3. CLIMATE MODELING ####


#### 3.0 INTERPOLAR A 20 METROS ####
#### 3.1 PRECIPITATION ####

prec<-read.table("clipboard",header=T,sep="\t")
mdt.tf<-raster("mdt.tf.asc")
alt.prec<-extract(mdt.tf,prec[,1:2])
prec<-cbind(prec,alt.prec)
colnames(prec)<-c("x","y","prec01","prec02","prec03","prec04","prec05","prec06","prec07","prec08","prec09","prec10","prec11","prec12","total","z")

x.canadas<-xFromCell(canadas,c(1:2.4e+07))
y.canadas<-yFromCell(canadas,c(1:2.4e+07))
z<-as.data.frame(canadas)
y<-as.data.frame(y.canadas)
x<-as.data.frame(x.canadas)
xyz<-cbind(x,y,z)
colnames(xyz)<-c("x","y","z")
setwd("C:/Users/Alejandro/Documents/Tesis/Trabajo en GIS/Clima/Tenerife/canadas")   
gam.prec01<-gam(prec01~s(x)+s(y)+s(z),data=prec)
gam.prec01<-predict(gam.prec01,xyz)
prec01<-cbind(xyz$x,xyz$y,gam.prec01)
colnames(prec01)<-c("x","y","prec01")
rm(gam.prec01)
prec01<-rasterFromXYZ(prec01,res=c(5,5))
writeRaster(prec01,"prec01.asc",overwrite=T)
rm(prec01)


gam.prec02<-gam(prec02~s(x)+s(y)+s(z),data=prec)
gam.prec02<-predict(gam.prec02,xyz)
prec02<-cbind(xyz$x,xyz$y,gam.prec02)
colnames(prec02)<-c("x","y","prec02")
rm(gam.prec02)
prec02<-rasterFromXYZ(prec02,res=c(5,5))
writeRaster(prec02,"prec02.asc",overwrite=T)
rm(prec02)

gam.prec03<-gam(prec03~s(x)+s(y)+s(z),data=prec)
gam.prec03<-predict(gam.prec03,xyz)
prec03<-cbind(xyz$x,xyz$y,gam.prec03)
colnames(prec03)<-c("x","y","prec03")
rm(gam.prec03)
prec03<-rasterFromXYZ(prec03,res=c(5,5))
writeRaster(prec03,"prec03.asc",overwrite=T)
rm(prec03)
                       
gam.prec04<-gam(prec04~s(x)+s(y)+s(z),data=prec)
gam.prec04<-predict(gam.prec04,xyz)
prec04<-cbind(xyz$x,xyz$y,gam.prec04)
colnames(prec04)<-c("x","y","prec04")
rm(gam.prec04)
prec04<-rasterFromXYZ(prec04,res=c(5,5))
writeRaster(prec04,"prec04.asc",overwrite=T)
rm(prec04)                                              
                       
gam.prec05<-gam(prec05~s(x)+s(y)+s(z),data=prec)
gam.prec05<-predict(gam.prec05,xyz)
prec05<-cbind(xyz$x,xyz$y,gam.prec05)
colnames(prec05)<-c("x","y","prec05")
rm(gam.prec05)
prec05<-rasterFromXYZ(prec05,res=c(5,5))
writeRaster(prec05,"prec05.asc",overwrite=T)
rm(prec05)

gam.prec06<-gam(prec06~s(x)+s(y)+s(z),data=prec)
gam.prec06<-predict(gam.prec06,xyz)
prec06<-cbind(xyz$x,xyz$y,gam.prec06)
colnames(prec06)<-c("x","y","prec06")
rm(gam.prec06)
prec06<-rasterFromXYZ(prec06,res=c(5,5))
writeRaster(prec06,"prec06.asc",overwrite=T)
rm(prec06)

gam.prec07<-gam(prec07~s(x)+s(y)+s(z),data=prec)
gam.prec07<-predict(gam.prec07,xyz)
prec07<-cbind(xyz$x,xyz$y,gam.prec07)
colnames(prec07)<-c("x","y","prec07")
rm(gam.prec07)
prec07<-rasterFromXYZ(prec07,res=c(5,5))
writeRaster(prec07,"prec07.asc",overwrite=T)
rm(prec07)

gam.prec08<-gam(prec08~s(x)+s(y)+s(z),data=prec)
gam.prec08<-predict(gam.prec08,xyz)
prec08<-cbind(xyz$x,xyz$y,gam.prec08)
colnames(prec08)<-c("x","y","prec08")
rm(gam.prec08)
prec08<-rasterFromXYZ(prec08,res=c(5,5))
writeRaster(prec08,"prec08.asc",overwrite=T)
rm(prec08)

gam.prec09<-gam(prec09~s(x)+s(y)+s(z),data=prec)
gam.prec09<-predict(gam.prec09,xyz)
prec09<-cbind(xyz$x,xyz$y,gam.prec09)
colnames(prec09)<-c("x","y","prec09")
rm(gam.prec09)
prec09<-rasterFromXYZ(prec09,res=c(5,5))
writeRaster(prec09,"prec09.asc",overwrite=T)
rm(prec09)

gam.prec10<-gam(prec10~s(x)+s(y)+s(z),data=prec)
gam.prec10<-predict(gam.prec10,xyz)
prec10<-cbind(xyz$x,xyz$y,gam.prec10)
colnames(prec10)<-c("x","y","prec10")
rm(gam.prec10)
prec10<-rasterFromXYZ(prec10,res=c(5,5))
writeRaster(prec10,"prec10.asc",overwrite=T)
rm(prec10)

gam.prec11<-gam(prec11~s(x)+s(y)+s(z),data=prec)
gam.prec11<-predict(gam.prec11,xyz)
prec11<-cbind(xyz$x,xyz$y,gam.prec11)
colnames(prec11)<-c("x","y","prec11")
rm(gam.prec11)
prec11<-rasterFromXYZ(prec11,res=c(5,5))
writeRaster(prec11,"prec11.asc",overwrite=T)
rm(prec11)

gam.prec12<-gam(prec12~s(x)+s(y)+s(z),data=prec)
tM<-reshape(tmax,direction="wide",timevar="MES",idvar=c("INDICATIVO","x","y","z"))
gam.prec12<-predict(gam.prec12,xyz)
prec12<-cbind(xyz$x,xyz$y,gam.prec12)
colnames(prec12)<-c("x","y","prec12")
rm(gam.prec12)
prec12<-rasterFromXYZ(prec12,res=c(5,5))
writeRaster(prec12,"prec12.asc",overwrite=T)
rm(prec12)

#### 3.2 MAX TEMPERATURE #####


xyz<-as.data.frame(geocanadas,xy=TRUE)
colnames(xyz)<-c("x","y","z")

temp<-read.table("clipboard",header=T,sep="\t")
tmax<-aggregate(TM_MAX~MES+INDICATIVO+x+y+z+NOMBRE,temp,FUN="mean")
tmax$x<-tmax$x*-1
tmax$NOMBRE<-NULL
tM<-reshape(tmax,direction="wide",timevar="MES",idvar=c("INDICATIVO","x","y","z"))
colnames(tM)<-c("id","x","y","z","tmax01","tmax02","tmax03","tmax04","tmax05","tmax06",
                "tmax07","tmax08","tmax09","tmax10","tmax11","tmax12")

setwd("C:/Users/Alejandro/Documents/Tesis/Trabajo en GIS/Clima/Tenerife/canadas") 
gam.tmax01<-gam(tmax01~s(x)+s(y)+s(z),data=tM)
gam.tmax01<-predict(gam.tmax01,xyz)
tmax01<-cbind(xyz$x,xyz$y,gam.tmax01)
colnames(tmax01)<-c("x","y","tmax01")
rm(gam.tmax01)
tmax01<-rasterFromXYZ(tmax01,res=res( geocanadas),digits=2)
writeRaster(tmax01,"tmax01.asc",overwrite=T)
rm(tmax01)


gam.tmax02<-gam(tmax02~s(x)+s(y)+s(z),data=tM)
gam.tmax02<-predict(gam.tmax02,xyz)
tmax02<-cbind(xyz$x,xyz$y,gam.tmax02)
colnames(tmax02)<-c("x","y","tmax02")
rm(gam.tmax02)
tmax02<-rasterFromXYZ(tmax02,res=res( geocanadas),digits=2)
writeRaster(tmax02,"tmax02.asc",overwrite=T)
rm(tmax02)

gam.tmax03<-gam(tmax03~s(x)+s(y)+s(z),data=tM)
gam.tmax03<-predict(gam.tmax03,xyz)
tmax03<-cbind(xyz$x,xyz$y,gam.tmax03)
colnames(tmax03)<-c("x","y","tmax03")
rm(gam.tmax03)
tmax03<-rasterFromXYZ(tmax03,,res=res( geocanadas),digits=2)
writeRaster(tmax03,"tmax03.asc",overwrite=T)
rm(tmax03)

gam.tmax04<-gam(tmax04~s(x)+s(y)+s(z),data=tM)
gam.tmax04<-predict(gam.tmax04,xyz)
tmax04<-cbind(xyz$x,xyz$y,gam.tmax04)
colnames(tmax04)<-c("x","y","tmax04")
rm(gam.tmax04)
tmax04<-rasterFromXYZ(tmax04,,res=res( geocanadas),digits=2)
writeRaster(tmax04,"tmax04.asc",overwrite=T)
rm(tmax04)                                              

gam.tmax05<-gam(tmax05~s(x)+s(y)+s(z),data=tM)
gam.tmax05<-predict(gam.tmax05,xyz)
tmax05<-cbind(xyz$x,xyz$y,gam.tmax05)
colnames(tmax05)<-c("x","y","tmax05")
rm(gam.tmax05)
tmax05<-rasterFromXYZ(tmax05,,res=res( geocanadas),digits=2)
writeRaster(tmax05,"tmax05.asc",overwrite=T)
rm(tmax05)

gam.tmax06<-gam(tmax06~s(x)+s(y)+s(z),data=tM)
gam.tmax06<-predict(gam.tmax06,xyz)
tmax06<-cbind(xyz$x,xyz$y,gam.tmax06)
colnames(tmax06)<-c("x","y","tmax06")
rm(gam.tmax06)
tmax06<-rasterFromXYZ(tmax06,,res=res( geocanadas),digits=2)
writeRaster(tmax06,"tmax06.asc",overwrite=T)
rm(tmax06)

gam.tmax07<-gam(tmax07~s(x)+s(y)+s(z),data=tM)
gam.tmax07<-predict(gam.tmax07,xyz)
tmax07<-cbind(xyz$x,xyz$y,gam.tmax07)
colnames(tmax07)<-c("x","y","tmax07")
rm(gam.tmax07)
tmax07<-rasterFromXYZ(tmax07,,res=res( geocanadas),digits=2)
writeRaster(tmax07,"tmax07.asc",overwrite=T)
rm(tmax07)

gam.tmax08<-gam(tmax08~s(x)+s(y)+s(z),data=tM)
gam.tmax08<-predict(gam.tmax08,xyz)
tmax08<-cbind(xyz$x,xyz$y,gam.tmax08)
colnames(tmax08)<-c("x","y","tmax08")
rm(gam.tmax08)
tmax08<-rasterFromXYZ(tmax08,res=res( geocanadas),digits=2)
writeRaster(tmax08,"tmax08.asc",overwrite=T)
rm(tmax08)

gam.tmax09<-gam(tmax09~s(x)+s(y)+s(z),data=tM)
gam.tmax09<-predict(gam.tmax09,xyz)
tmax09<-cbind(xyz$x,xyz$y,gam.tmax09)
colnames(tmax09)<-c("x","y","tmax09")
rm(gam.tmax09)
tmax09<-rasterFromXYZ(tmax09,res=res( geocanadas),digits=2)
writeRaster(tmax09,"tmax09.asc",overwrite=T)
rm(tmax09)

gam.tmax10<-gam(tmax10~s(x)+s(y)+s(z),data=tM)
gam.tmax10<-predict(gam.tmax10,xyz)
tmax10<-cbind(xyz$x,xyz$y,gam.tmax10)
colnames(tmax10)<-c("x","y","tmax10")
rm(gam.tmax10)
tmax10<-rasterFromXYZ(tmax10,res=res( geocanadas),digits=2)
writeRaster(tmax10,"tmax10.asc",overwrite=T)
rm(tmax10)

gam.tmax11<-gam(tmax11~s(x)+s(y)+s(z),data=tM)
gam.tmax11<-predict(gam.tmax11,xyz)
tmax11<-cbind(xyz$x,xyz$y,gam.tmax11)
colnames(tmax11)<-c("x","y","tmax11")
rm(gam.tmax11)
tmax11<-rasterFromXYZ(tmax11,res=res( geocanadas),digits=2)
writeRaster(tmax11,"tmax11.asc",overwrite=T)
rm(tmax11)

gam.tmax12<-gam(tmax12~s(x)+s(y)+s(z),data=tM)
tmax<-reshape(tmax,direction="wide",timevar="MES",idvar=c("INDICATIVO","x","y","z"))
gam.tmax12<-predict(gam.tmax12,xyz)
tmax12<-cbind(xyz$x,xyz$y,gam.tmax12)
colnames(tmax12)<-c("x","y","tmax12")
rm(gam.tmax12)
tmax12<-rasterFromXYZ(tmax12,res=res( geocanadas),digits=2)
writeRaster(tmax12,"tmax12.asc",overwrite=T)
rm(tmax12)


#### 3.2 MIN TEMPERATURE #####


tmin<-aggregate(TM_MIN~MES+INDICATIVO+x+y+z+NOMBRE,temp,FUN="mean")
tmin$x<-tmin$x*-1
tmin$NOMBRE<-NULL
tm<-reshape(tmin,direction="wide",timevar="MES",idvar=c("INDICATIVO","x","y","z"))
colnames(tm)<-c("id","x","y","z","tmin01","tmin02","tmin03","tmin04","tmin05","tmin06",
                "tmin07","tmin08","tmin09","tmin10","tmin11","tmin12")


gam.tmin01<-gam(tmin01~s(x)+s(y)+s(z),data=tm)
gam.tmin01<-predict(gam.tmin01,xyz)
tmin01<-cbind(xyz$x,xyz$y,gam.tmin01)
colnames(tmin01)<-c("x","y","tmin01")
rm(gam.tmin01)
tmin01<-rasterFromXYZ(tmin01,res=res( geocanadas),digits=2)
writeRaster(tmin01,"tmin01.asc",overwrite=T)
rm(tmin01)

gam.tmin02<-gam(tmin02~s(x)+s(y)+s(z),data=tm) 
gam.tmin02<-predict(gam.tmin02,xyz)
tmin02<-cbind(xyz$x,xyz$y,gam.tmin02)
colnames(tmin02)<-c("x","y","tmin02")
rm(gam.tmin02)
tmin02<-rasterFromXYZ(tmin02,res=res( geocanadas),digits=2)
writeRaster(tmin02,"tmin02.asc",overwrite=T)
rm(tmin02)
 

gam.tmin03<-gam(tmin03~s(x)+s(y)+s(z),data=tm) 
gam.tmin03<-predict(gam.tmin03,xyz)
tmin03<-cbind(xyz$x,xyz$y,gam.tmin03)
colnames(tmin03)<-c("x","y","tmin03")
rm(gam.tmin03)
tmin03<-rasterFromXYZ(tmin03,res=res( geocanadas),digits=2)
writeRaster(tmin03,"tmin03.asc",overwrite=T)
rm(tmin03)

gam.tmin04<-gam(tmin04~s(x)+s(y)+s(z),data=tm)
gam.tmin04<-predict(gam.tmin04,xyz)
tmin04<-cbind(xyz$x,xyz$y,gam.tmin04)
colnames(tmin04)<-c("x","y","tmin04")
rm(gam.tmin04)
tmin04<-rasterFromXYZ(tmin04,res=res( geocanadas),digits=2)
writeRaster(tmin04,"tmin04.asc",overwrite=T)
rm(tmin04)

gam.tmin05<-gam(tmin05~s(x)+s(y)+s(z),data=tm)
gam.tmin05<-predict(gam.tmin05,xyz)
tmin05<-cbind(xyz$x,xyz$y,gam.tmin05)
colnames(tmin05)<-c("x","y","tmin05")
rm(gam.tmin05)
tmin05<-rasterFromXYZ(tmin05,res=res( geocanadas),digits=2)
writeRaster(tmin05,"tmin05.asc",overwrite=T)
rm(tmin05)

gam.tmin06<-gam(tmin06~s(x)+s(y)+s(z),data=tm)
gam.tmin06<-predict(gam.tmin06,xyz)
tmin06<-cbind(xyz$x,xyz$y,gam.tmin06)
colnames(tmin06)<-c("x","y","tmin06")
rm(gam.tmin06)
tmin06<-rasterFromXYZ(tmin06,res=res( geocanadas),digits=2)
writeRaster(tmin06,"tmin06.asc",overwrite=T)
rm(tmin06)

gam.tmin07<-gam(tmin07~s(x)+s(y)+s(z),data=tm)
gam.tmin07<-predict(gam.tmin07,xyz)
tmin07<-cbind(xyz$x,xyz$y,gam.tmin07)
colnames(tmin07)<-c("x","y","tmin07")
rm(gam.tmin07)
tmin07<-rasterFromXYZ(tmin07,res=res( geocanadas),digits=2)
writeRaster(tmin07,"tmin07.asc",overwrite=T)
rm(tmin07)

gam.tmin08<-gam(tmin08~s(x)+s(y)+s(z),data=tm)
gam.tmin08<-predict(gam.tmin08,xyz)
tmin08<-cbind(xyz$x,xyz$y,gam.tmin08)
colnames(tmin08)<-c("x","y","tmin08")
rm(gam.tmin08)
tmin08<-rasterFromXYZ(tmin08,res=res( geocanadas),digits=2)
writeRaster(tmin08,"tmin08.asc",overwrite=T)
rm(tmin08)

gam.tmin09<-gam(tmin09~s(x)+s(y)+s(z),data=tm)
gam.tmin09<-predict(gam.tmin09,xyz)
tmin09<-cbind(xyz$x,xyz$y,gam.tmin09)
colnames(tmin09)<-c("x","y","tmin09")
rm(gam.tmin09)
tmin09<-rasterFromXYZ(tmin09,res=res( geocanadas),digits=2)
writeRaster(tmin09,"tmin09.asc",overwrite=T)
rm(tmin09)

gam.tmin10<-gam(tmin10~s(x)+s(y)+s(z),data=tm)
gam.tmin10<-predict(gam.tmin10,xyz)
tmin10<-cbind(xyz$x,xyz$y,gam.tmin10)
colnames(tmin10)<-c("x","y","tmin10")
rm(gam.tmin10)
tmin10<-rasterFromXYZ(tmin10,res=res( geocanadas),digits=2)
writeRaster(tmin10,"tmin10.asc",overwrite=T)
rm(tmin10)

gam.tmin11<-gam(tmin11~s(x)+s(y)+s(z),data=tm)
gam.tmin11<-predict(gam.tmin11,xyz)
tmin11<-cbind(xyz$x,xyz$y,gam.tmin11)
colnames(tmin11)<-c("x","y","tmin11")
rm(gam.tmin11)
tmin11<-rasterFromXYZ(tmin11,res=res( geocanadas),digits=2)
writeRaster(tmin11,"tmin11.asc",overwrite=T)
rm(tmin11)

gam.tmin12<-gam(tmin12~s(x)+s(y)+s(z),data=tm)
tm<-reshape(tm,direction="wide",timevar="MES",idvar=c("INDICATIVO","x","y","z"))
gam.tmin12<-predict(gam.tmin12,xyz)
tmin12<-cbind(xyz$x,xyz$y,gam.tmin12)
colnames(tmin12)<-c("x","y","tmin12")
rm(gam.tmin12)
tmin12<-rasterFromXYZ(tmin12,res=res( geocanadas),digits=2)
writeRaster(tmin12,"tmin12.asc",overwrite=T)
rm(tmin12)

#### 3.3 crop prec ####

writeRaster(crop(raster("prec01.bil"),geocanadas),"prec01.asc",overwrite=T)
writeRaster(crop(raster("prec02.bil"),geocanadas),"prec02.asc",overwrite=T)
writeRaster(crop(raster("prec03.bil"),geocanadas),"prec03.asc",overwrite=T)
writeRaster(crop(raster("prec04.bil"),geocanadas),"prec04.asc",overwrite=T)
writeRaster(crop(raster("prec05.bil"),geocanadas),"prec05.asc",overwrite=T)
writeRaster(crop(raster("prec06.bil"),geocanadas),"prec06.asc",overwrite=T)
writeRaster(crop(raster("prec07.bil"),geocanadas),"prec07.asc",overwrite=T)
writeRaster(crop(raster("prec08.bil"),geocanadas),"prec08.asc",overwrite=T)
writeRaster(crop(raster("prec09.bil"),geocanadas),"prec09.asc",overwrite=T)
writeRaster(crop(raster("prec10.bil"),geocanadas),"prec10.asc",overwrite=T)
writeRaster(crop(raster("prec11.bil"),geocanadas),"prec11.asc",overwrite=T)
writeRaster(crop(raster("prec12.bil"),geocanadas),"prec12.asc",overwrite=T)



#### 5. NICHE MODELING ####

#### 5.1 DATASET ####
setwd("C:/Users/agonz/Documents/Viola")
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
viola<-read.table("C:/Users/agonz/Documents/Viola/violacoords.txt",sep=",",header=T)
viola<-viola[,6:7]
especie<-"violacheirantifolia"
slope<-aggregate(raster("C:/Users/agonz/Documents/Viola/Tenerife/Canadas/slope.asc"),fac=4)
viola<-as.data.frame(presence.absence.raster(slope,viola,raster.label=especie),xy=TRUE)
viola.na<-viola[,3]
viola.na[viola.na==0]="NA"
viola.na<-cbind(viola[,1:2],viola.na)
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
packages<-c("raster", "biomod2", "MigClim","dismo","mgcv","rasterVis","polysat","adegenet","rgdal","tidyr","gstat","shapefiles","rgeos","sp","maptools","ggfortify","r2dRue","reshape","spatialEco","adegenet")
UTMproj<-"+proj=utm +zone=28 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
geoproj<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
lapply(packages, require, character.only=T)
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
  models = c('GBM','RF',"GLM","GAM","MARS","ANN"),
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




##### 10. SIMULACIÓN DE HETEROZIGOSIDAD #####
iter<-500

surface.30<-5000
surface.50<-3000
surface.80<-1000
surface.ini<-8000
prob.ini<-1

heterozig.sim<-function(model="miroc",iter=500,current.area=2000,area30=1000,area50=1000,area80=100){
  
  for (j in 1:iter){
    niter<-j
    breaks<-c(1,2,3)
    year.breaks<-c(2015,2030,2050,2080)
    for(i in breaks){
      initial.horizon<-year.breaks[i]
      final.horizon<-year.breaks[i+1]
      nyears<-initial.horizon-final.horizon
      if (initial.horizon <= 2030){
        initial.area <- current.area
        final.area <- area30
        
      } else if (initial.horizon <= 2050 ) {
        
          initial.area <- area30
        final.area <- area50
        
      } else {
        
        initial.area <-area50
        final.area <- area80
      }
      for (i in 1:nyears){
        current.year<-i
        ratio<-(final.area-initial.area)/nyears
        year.area<-current.area+current.year*ratio
        probability<--1*(year.area-current.area)/current.area
        print(probability)
        
      }
    }
  }
}

#LECTURA STRAND####
#Leyendo formato del STRaND y guardado el objeto en viola_genambig. Esto solo hará falta hacerlo una vez
read.STRand('Strand_266_tet8_corregido_K2.txt', sep = "\t", popInSam = TRUE) -> viola_genambig
viola_genambig <- estimatePloidy(viola_genambig, samples = Samples(viola_genambig), loci = Loci(viola_genambig))
Ploidies(viola_genambig) 
Usatnts(viola_genambig) <- c(2,3,2,4,3,3,2,2,3,3,2,3,4,3)

#Leyendo formato del STRaND para la base de datos con K2 (Teide y GuaPas)
read.STRand('Strand_266_tet8_corregido_K2.txt', sep = "\t", popInSam = TRUE) -> viola_genambig2
Usatnts(viola_genambig2) <- c(2,3,2,4,3,3,2,2,3,3,2,3,4,3)

#Frecuencias alélicas simples. Con la ploidía en Ploidies, al máximo de cada muestra
my_freq <- simpleFreq(viola_genambig)
head(my_freq)
genotypeDiversity(genobject, samples = Samples(genobject),
                  loci = Loci(genobject),
                  d = meandistance.matrix(genobject, samples, loci,
                                          all.distances = TRUE,
                                          distmetric = Lynch.distance),
                  threshold = 0, index = Shannon, ...)

#Frecuencias alélicas para el adegenet. Esta vez pongo la ploidía a 4 ya que adegenet acepta un número fijo de ploidía.
#Cambiar el objeto a viola_genambig2 para la base de datos con K2
Ploidies(viola_genambig) <- 4
my_freq_ploidy4 <- simpleFreq(viola_genambig)

#Exportando a formato adegenet...
gpfreq <- freq.to.genpop(my_freq_ploidy4)
require(adegenet)
viola_mygenpop <- genpop(gpfreq, ploidy=as.integer(4), type="codom")
heterocigosidad <- Hs(viola_mygenpop)



#11 Evaluaciones ####

lm(Testing.data~model,data=evaluations)%>%aov()%>%TukeyHSD()%>%pander()

myPal2<-colorRampPalette(c("white","blue"))(100)
extension=extent(-16.75,-16.55,28.16,28.32)
plot(crop(hillshade,extension),col=grey(0:100/100), legend=FALSE)
plot(raster("C:/Users/agonz/Documents/Viola/Tenerife/Canadas/bio03.asc"),alpha=0.85,add=TRUE,legend=TRUE)
library(spatialEco)
raster.transformation(raster("C:/Users/agonz/Documents/Viola/Tenerife/Canadas/bio03.asc"),trans = "norm")%>%
  crop(extension)%>%
  plot(alpha=0.85,col=myPal2,add=TRUE,legend=TRUE)

#12 areas ####
areas<-na.omit(areas)
areas$year<-as.numeric(areas$year)
areas$surface<-as.numeric(areas$surface)
ggplot(areas,aes(x=year,y=surface))+geom_line(aes(col=model),size=1.15)+theme_bw()+
  xlab("Año")+ylab("Superficie idónea")+facet_grid(RCP~.)+labs(col="Modelo")

myPal3<-colorRampPalette(c("red","white","green"))(100)

plot(hillshade,col=grey(0:100/100),  legend=FALSE)
plot(raster("presenteTFM.asc")-raster("presente.asc"),alpha=0.85,add=TRUE,legend=TRUE)
library(spatialEco)
library(tidyverse)


raster1<-raster(paste0(getwd(),"/Tenerife/Canadas/bio12.asc"))%>%raster.transformation(trans="norm")
raster2<-raster("tpi.asc")%>%resample(raster1)%>%raster.transformation(trans="norm")
raster3<-raster1+raster2%>%
  #raster.transformation(trans="norm")%>%
  +1%>%
  raster.transformation(trans="stretch",smin=0,smax=1)%>%
  raster.transformation(trans="norm")%>%
  crop(extension)
myPal3<-colorRampPalette(c("blue","white"))(100)
plot(crop(hillshade,extension),col=grey(0:100/100), legend=FALSE)
plot(raster3,col=myPal3,alpha=0.90,add=TRUE,legend=F)


lista<-c("GLM","GAM","MARS","ANN","RF","GBM")
for (i in lista){
print(i)
myGLMs<-BIOMOD_LoadModels(modelos, models=i)
myRespPlot2 <- response.plot2(models  = myGLMs,
                               Data = get_formal_data(modelos,'expl.var'), 
                               show.variables= get_formal_data(modelos,'expl.var.names'),
                               do.bivariate = FALSE,
                               fixed.var.metric = 'median',
                               col = c("blue", "red"),
                               legend = TRUE,
                               data_species = get_formal_data(modelos,'resp.var'))
myRespPlot2D
}
