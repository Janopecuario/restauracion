packages<-c("raster", "biomod2", "dismo","mgcv","terra",
            "rasterVis","gstat","shapefiles",
            "sp","ggfortify","reshape","spatialEco",
            "tidyverse","rgbif","sabinaNSDM","stringi",
            "CoordinateCleaner","sf","glmnet","data.table","covsel","stars","readxl")
sapply(packages, require, character.only=T, quietly = FALSE)

setwd("P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/THIC")
ruta_pastos<-"P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/THIC/CBP_MA_P_20250905.gdb/CBP_MA_P_20250905.gdb"
ruta_bosques<- "P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/THIC/CBP_MA_B_20250905.gdb"
h<-9130
habitat<-st_read(ruta_bosques,layer = "CBP_MA_B_PB_20250905", 
                 n_max = 0,
                 #query = "SELECT * FROM CBP_MA_B_PB_20250905 WHERE HIC = '9130'" 
                 )

st_layers(ruta_bosques)
