list.files(pattern=".asc")
library(ggplot2)
library(raster)
library(terra)
library(tidyterra)
library(patchwork)
library(scico)
# Load the raster files

mipaleta <- scico::scico(11, palette = "vik")
bosques<- "C:/Users/Usuario/Documents/BigData/TFM/Backup/THIC/CBP_MA_B_20250919.gdb"
matorrales<-"C:/Users/Usuario/Documents/BigData/TFM/Backup/THIC/matorrales.gdb"
setwd("~/BigData/TFM/Backup/Modelos")

ext_5220 <- ext(-7, 1, 34, 40)
ext_5220_tiny <- ext(-3, -1, 36.5, 37.5)
ext_9130 <- ext(-9, 4, 41, 44)
ext_9130_tiny<-ext(-4.5,-1.5, 42, 44)
ext_6310_tiny<-ext(-10,-5,36,38)

azufaifares<-raster("habitat_5220_ensamblado.asc") %>% rast() %>% crop(ext_5220)
azufaifo<-raster("Ziziphus lotus_Covariate_model.tif") %>% rast() %>% crop(ext_5220)
azufaifares_tiny<-azufaifares %>% crop(ext_5220_tiny)
hayedos<-raster("habitat_9130_ensamblado.asc") %>% rast() %>% crop(ext_9130)
haya<-raster("Fagus sylvatica_Covariate_model.tif") %>% rast() %>% crop(ext_9130)
dehesas<-raster("habitat_6310_ensamblado.asc") %>% rast()
alcornoque<-raster("Quercus suber_Covariate_model.tif") %>% rast()

mdt<-raster("wc2.1_30s_elev.tif") %>% rast() %>% crop(alcornoque)
slope  <- terrain(mdt, "slope", unit = "radians")
aspect <- terrain(mdt, "aspect", unit = "radians")
hill <- shade(slope, aspect, angle = 45, direction = 315)
malla<-mdt
malla[!is.na(malla)] <- 0

#5220----
codigo_habitat<- "5220" 
layer_habitat<-"CBP_MA_M_PB_20250919b"
query_txt <- sprintf("SELECT * FROM %s WHERE HIC1 = '%s'", 
                     layer_habitat,
                     codigo_habitat)  
habitat_5220<-vect(matorrales,
              layer = layer_habitat, 
              #n_max = 0, 
              query = query_txt
)
hill_5220<-terra::crop(hill, ext_5220)
habitat_5220_reproject<-terra::project(habitat_5220, terra::crs(malla)) %>% crop(ext_5220_tiny)
hill_5220_tiny<-terra::crop(hill, ext_5220_tiny)
plot_azufaifares<-ggplot() +
  geom_spatraster(data = hill_5220, aes(fill = after_stat(value))) +
  scale_fill_gradient(low = "grey30", high = "white", guide = "none") +
  
  geom_spatraster(data = azufaifares, alpha = 0.7) +
  
  scale_fill_gradientn(
    colours = mipaleta,
    name = "Idoneidad",
    na.value = "grey85",
    guide = guide_colorbar(
      direction = "horizontal",
      title.position="top",
      barwidth = unit(0.45, "npc"),  # ocupa todo el ancho
      barheight = unit(0.18, "cm"))
   ) + #geom_spatvector(
  #   data = habitat_5220_reproject,
  #   fill = NA,
  #   color = "black",
  #   linewidth = 0.6) +
  # labs(
  #   title = "Hábitat 5220\nMatorrales xerofíticos"#,
  #   #subtitle = "Idoneidad del hábitat para los azuafaifares"
  # 
  # ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "grey97", color = NA),
    plot.background  = element_rect(fill = "grey97", color = NA),
    legend.title = element_text(size = 9, hjust = 0.5,face="bold"),
    legend.text  = element_text(size = 8),
    plot.title = element_text(face = "bold",size= 11),
    legend.position = "bottom"
  )

plot_azufaifo<-ggplot() +
  geom_spatraster(data = hill_5220, aes(fill = after_stat(value))) +
  scale_fill_gradient(low = "grey30", high = "white", guide = "none") +
  
  geom_spatraster(data = azufaifo, alpha = 0.7) +
  
  scale_fill_gradientn(
    colours=mipaleta,
    name = "Idoneidad",
    na.value = "grey85",
    guide = NULL) +
  labs(
    title = "Modelo del Azufaifo"#,#,
    #subtitle = "Idoneidad del hábitat para los azuafaifares"
    
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "grey97", color = NA),
    plot.background  = element_rect(fill = "grey97", color = NA),
    legend.title = element_text(size = 9, hjust = 0.5,face="bold"),
    legend.text  = element_text(size = 8),
    plot.title = element_text(face = "bold",size= 11),
    legend.position = "bottom"
  )
p_azufaifares<-plot_azufaifares + plot_azufaifo + plot_layout(guides = "collect") & theme(legend.position = "bottom")+ theme(axis.text = element_text(size = 8)) 
  ggsave(p_azufaifares,filename="fig_azufaifares.jpeg",dpi=600)
  
p_azufaifares_tiny<-ggplot() +
  geom_spatraster(data = hill_5220_tiny, aes(fill = after_stat(value))) +
  scale_fill_gradient(low = "grey30", high = "white", guide = "none") +
  
  geom_spatraster(data = azufaifares_tiny, alpha = 0.7) +
  
  scale_fill_gradientn(
    colours=mipaleta,
    name = "Idoneidad",
    na.value = "grey85",
    guide = NULL) +
  labs(
    title = "Modelo del Azufaifo\nZona de estudio reducida"#,#,
    #subtitle = "Idoneidad del hábitat para los azuafaifares"
    
  ) +geom_spatvector(
       data = habitat_5220_reproject,
       fill = NA,
       color = "black",
       linewidth = 0.6) +
        labs(
       title = "Hábitat 5220\nMatorrales xerofíticos"#,
       #subtitle = "Idoneidad del hábitat para los azuafaifares"
     
     ) +
  
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "grey97", color = NA),
    plot.background  = element_rect(fill = "grey97", color = NA),
    legend.title = element_text(size = 9, hjust = 0.5,face="bold"),
    legend.text  = element_text(size = 8),
    plot.title = element_text(face = "bold",size= 11),
    legend.position = "bottom",
    axis.text = element_text(size = 8)
  )  
p_azufaifares_tiny
ggsave(p_azufaifares_tiny,filename="fig_azufaifares_tiny.jpeg",dpi=600)
#9130----
codigo_habitat<- "9130" 
layer_habitat<-"CBP_MA_B_PB_20250919"
query_txt <- sprintf("SELECT * FROM %s WHERE HIC1 = '%s'", 
                     layer_habitat,
                     codigo_habitat)  
habitat_9130<-vect(bosques,
              layer = layer_habitat, 
              #n_max = 0, 
              query = query_txt
)
habitat_9130_reproject<-terra::project(habitat_9130, terra::crs(malla)) %>% 
  crop(ext_9130_tiny)
hill_9130<-terra::crop(hill, ext_9130)
hill_9130_tiny<-terra::crop(hill,ext_9130_tiny)
hayedos_tiny<-hayedos %>% crop(ext_9130_tiny)
plot_hayedos<-ggplot() +
  geom_spatraster(data = hill_9130, aes(fill = after_stat(value))) +
  scale_fill_gradient(low = "grey30", high = "white", guide = "none") +
  
  geom_spatraster(data = hayedos, alpha = 0.7) +
  
  scale_fill_gradientn(
    colours = mipaleta,
    name = "Idoneidad",
    na.value = "grey85",
    guide = guide_colorbar(
      direction = "horizontal",
      title.position="top",
      barwidth = unit(0.45, "npc"),  # ocupa todo el ancho
      barheight = unit(0.18, "cm"))
  ) +
  labs(
    title = "Hábitat 9130\nHayedos"#,
    #subtitle = "Idoneidad del hábitat para los azuafaifares"
    
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "grey97", color = NA),
    plot.background  = element_rect(fill = "grey97", color = NA),
    legend.title = element_text(size = 9, hjust = 0.5,face="bold"),
    legend.text  = element_text(size = 8),
    plot.title = element_text(face = "bold",size= 11),
    legend.position = "bottom"
  )

plot_haya<-ggplot() +
  geom_spatraster(data = hill_9130, aes(fill = after_stat(value))) +
  scale_fill_gradient(low = "grey30", high = "white", guide = "none") +
  
  geom_spatraster(data = haya, alpha = 0.7) +
  
  scale_fill_gradientn(
    colours = mipaleta,
    name = "Idoneidad",
    na.value = "grey85",
    guide = NULL) +
  labs(
    title = "Modelo del Haya"#,#,
    #subtitle = "Idoneidad del hábitat para los azufaifares"
    
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "grey97", color = NA),
    plot.background  = element_rect(fill = "grey97", color = NA),
    legend.title = element_text(size = 9, hjust = 0.5),
    legend.text  = element_text(size = 8),
    plot.title = element_text(face = "bold",size= 11),
    legend.position = "bottom"
  )
p_hayedos<-plot_hayedos + plot_haya + plot_layout(guides = "collect") & theme(legend.position = "bottom")+ theme(axis.text = element_text(size = 8))
  ggsave(p_hayedos,filename="fig_hayedos.jpeg",dpi=600)

p_hayedos_tiny<-ggplot() +
    geom_spatraster(data = hill_9130_tiny, aes(fill = after_stat(value))) +
    scale_fill_gradient(low = "grey30", high = "white", guide = "none") +
    
    geom_spatraster(data = hayedos_tiny, alpha = 0.7) +
    
    scale_fill_gradientn(
      colours=mipaleta,
      name = "Idoneidad",
      na.value = "grey85",
      guide = NULL) +
    labs(
      title = "Modelo del hayedo\nZona de estudio reducida"#,#,
      #subtitle = "Idoneidad del hábitat para los azuafaifares"
      
    ) +geom_spatvector(
      data = habitat_9130_reproject,
      fill = NA,
      color = "black",
      linewidth = 0.6) +
    labs(
      title = "Hábitat 9130\nHayedos"#,
      #subtitle = "Idoneidad del hábitat para los azuafaifares"
      
    ) +
    
    theme_minimal(base_size = 14) +
    theme(
      panel.background = element_rect(fill = "grey97", color = NA),
      plot.background  = element_rect(fill = "grey97", color = NA),
      legend.title = element_text(size = 9, hjust = 0.5,face="bold"),
      legend.text  = element_text(size = 8),
      plot.title = element_text(face = "bold",size= 11),
      legend.position = "bottom",
      axis.text = element_text(size = 8)
    )   
ggsave(p_hayedos_tiny,filename="fig_hayedos_tiny.jpeg",dpi=600)  
#6310----
hill_6310_tiny<-hill %>% terra::crop(ext_6310_tiny)
dehesas_tiny<-dehesas %>% terra::crop(ext_6310_tiny)
codigo_habitat<- "6310"
layer_habitat<-"CBP_MA_B_PB_20250919"
query_txt <- sprintf("SELECT * FROM %s WHERE HIC1 = '%s'", 
                     layer_habitat,
                     codigo_habitat)  
habitat_6310<-vect(bosques,
                   layer = layer_habitat, 
                   #n_max = 0, 
                   query = query_txt
)
habitat_6310_reproject<-terra::project(habitat_6310, terra::crs(malla)) %>% 
  crop(ext_6310_tiny)

plot_dehesas<-ggplot() +
  geom_spatraster(data = hill, aes(fill = after_stat(value))) +
  scale_fill_gradient(low = "grey30", high = "white", guide = "none") +
  
  geom_spatraster(data = dehesas, alpha = 0.7) +
  
  scale_fill_gradientn(
    colours = mipaleta,
    name = "Idoneidad",
    na.value = "grey85",
    guide = guide_colorbar(
      direction = "horizontal",
      title.position="top",
      barwidth = unit(0.45, "npc"),  # ocupa todo el ancho
      barheight = unit(0.18, "cm"))
  ) +
  
  labs(
    title = "Hábitat 6310\nDehesas perennifolias"#,
    #subtitle = "Idoneidad del hábitat para los azuafaifares"
    
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "grey97", color = NA),
    plot.background  = element_rect(fill = "grey97", color = NA),
    legend.title = element_text(size = 9, hjust = 0.5),
    legend.text  = element_text(size = 8),
    plot.title = element_text(face = "bold",size= 11),
    legend.position = "bottom"
  )

plot_alcornoque<-ggplot() +
  geom_spatraster(data = hill, aes(fill = after_stat(value))) +
  scale_fill_gradient(low = "grey30", high = "white", guide = "none") +
  
  geom_spatraster(data = alcornoque, alpha = 0.7) +
  
  scale_fill_gradientn(
    colours = mipaleta,
    name = "Idoneidad",
    na.value = "grey85",
    guide = NULL) +
  labs(
    title = "Modelo de alcornoque"#,#,
    #subtitle = "Idoneidad del hábitat para los azufaifares"
    
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "grey97", color = NA),
    plot.background  = element_rect(fill = "grey97", color = NA),
    legend.title = element_text(size = 9, hjust = 0.5),
    legend.text  = element_text(size = 8),
    plot.title = element_text(face = "bold",size= 11),
    legend.position = "bottom"
  )
p_dehesas<-plot_dehesas + plot_alcornoque + plot_layout(guides = "collect") & theme(legend.position = "bottom")
  ggsave(p_dehesas, filename="fig_dehesas.jpeg",dpi=600)
  
p_dehesas_tiny<-ggplot() +
    geom_spatraster(data = hill_6310_tiny, aes(fill = after_stat(value))) +
    scale_fill_gradient(low = "grey30", high = "white", guide = "none") +
    
    geom_spatraster(data = dehesas_tiny, alpha = 0.7) +
    
    scale_fill_gradientn(
      colours=mipaleta,
      name = "Idoneidad",
      na.value = "grey85",
      guide = NULL) +
    labs(
      title = "Modelo de la dehesa\nZona de estudio reducida"#,#,
      #subtitle = "Idoneidad del hábitat para los azuafaifares"
      
    ) +geom_spatvector(
      data = habitat_6310_reproject,
      fill = NA,
      color = "black",
      linewidth = 0.6) +
    labs(
      title = "Hábitat 6310\nDehesas"#,
      #subtitle = "Idoneidad del hábitat para los azuafaifares"
      
    ) +
    
    theme_minimal(base_size = 14) +
    theme(
      panel.background = element_rect(fill = "grey97", color = NA),
      plot.background  = element_rect(fill = "grey97", color = NA),
      legend.title = element_text(size = 9, hjust = 0.5,face="bold"),
      legend.text  = element_text(size = 8),
      plot.title = element_text(face = "bold",size= 11),
      legend.position = "bottom",
      axis.text = element_text(size = 8)
    )  

ggsave(p_dehesas_tiny,filename="fig_dehesas_tiny.jpeg",dpi=600) 
