setwd("P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/THIC")


packages<-c("raster", "terra",
            "rasterVis","sf",
            "tidyverse","readxl")
sapply(packages, require, character.only=T, quietly = FALSE)

UTMproj <- "+proj=utm +zone=30 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
geoproj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

malla <- raster("P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/ambientes/TMed.asc") %>% 
  rast()

malla <- raster::raster("P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/ambientes/Tmed.asc") %>% rast()

ruta_pastos<-"P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/THIC/CBP_MA_P_20250905.gdb/CBP_MA_P_20250905.gdb"
ruta_bosques<- "P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/THIC/CBP_MA_B_20250905.gdb"

lista_especies_habitats <- read_excel("P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/THIC/typical_species_2013-2018.xlsx",
                                      sheet=2,header=TRUE)  %>% select(1,2,3,4)


lista_habitats<- read_excel("P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/THIC/listadohabitats.xlsx",
                            sheet=2,col_names=TRUE, skip=0) %>% select(1) %>% unique()

ocurrencias_habitat<- data.frame(x=numeric(),
                                 y=numeric(),
                                 habitat=character())
habitat_bosques<-vect(ruta_bosques,layer = "CBP_MA_B_PB_20250905", 
              #n_max = 0, 
              #query = query_txt
              )
for(i in (1:nrow(lista_habitats))){ 
query_txt <- sprintf("SELECT * FROM CBP_MA_B_PB_20250905 WHERE HIC1 = '%s'", 
                     #lista_habitats[i,1],
                     "9130"
                     )  
print(paste0("vectorizing", lista_habitats[i,1]))
habitat<-vect(ruta_bosques,layer = "CBP_MA_B_PB_20250905", 
                 #n_max = 0, 
              query = query_txt)
if(nrow(habitat)>0){
#   query_txt <- sprintf("SELECT * FROM CBP_MA_P_20250905 WHERE HIC1 = '%s'", lista_habitats[i,1])  
#   habitat<-vect(ruta_pastos,layer = "CBP_MA_P_20250905", 
#                    #n_max = 0, 
#                 query = query_txt)
# }

  
print(paste0("rasterizing", lista_habitats[i,1]))
  habitat<-vect(ruta_bosques,layer = "CBP_MA_B_PB_20250905", 
                #n_max = 0, 
                query = query_txt) %>% project(crs(malla))
  habitat<-project(habitat,crs(malla))
  habitat_rast<-rasterize(habitat, malla)
habitat_df <-as.data.frame(habitat_rast, xy=TRUE) %>% select(x,y)
ocurrencias_habitat_i <- habitat_df %>% mutate(habitat=lista_habitats[i,1])
ocurrencias_habitat <- rbind(ocurrencias_habitat, ocurrencias_habitat_i)
plot(habitat,main=lista_habitats[i,1])}

}
write.table(habitat_df,"ocurrencias_9130.csv")
writeRaster(habitat_rast,"raster_9139.asc",overwrite=TRUE)





