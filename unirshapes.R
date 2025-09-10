library(sf)
library(tidyverse)
library(raster)
library(terra)
library(foreign)

# Paso 1: Obtener lista de archivos .shp
setwd("~/CartografiaHabitats")
# Definir la carpeta con los archivos .dbf
carpeta <- getwd()  # <- modifica esto

provincias <- c(
  "Álava"="01","Albacete"="02","Alicante"="03","Almería"="04","Ávila"="05","Badajoz"="06",
  "Baleares"="07","Barcelona"="08","Burgos"="09","Cáceres"="10","Cádiz"="11","Castellón"="12",
  "Ciudad Real"="13","Córdoba"="14","La Coruña"="15","Cuenca"="16","Gerona"="17","Granada"="18",
  "Guadalajara"="19","Guipúzcoa"="20","Huelva"="21","Huesca"="22","Jaén"="23","León"="24",
  "Lérida"="25","La Rioja"="26","Lugo"="27","Madrid"="28","Málaga"="29","Murcia"="30",
  "Navarra"="31","Orense"="32","Asturias"="33","Palencia"="34","Las Palmas"="35","Pontevedra"="36",
  "Salamanca"="37","Santa Cruz de Tenerife"="38","Cantabria"="39","Segovia"="40","Sevilla"="41",
  "Soria"="42","Tarragona"="43","Teruel"="44","Toledo"="45","Valencia"="46","Valladolid"="47",
  "Vizcaya"="48","Zamora"="49","Zaragoza"="50","Ceuta"="51","Melilla"="52") %>% as.data.frame()

provincias<-provincias %>% rename(codigo=".") %>% 
  mutate(nombre=rownames(.)) %>% tibble()
rownames(provincias)<-NULL


# Obtener la lista de archivos .csv que comienzan por "Habi"
archivos_csv <- list.files(
   pattern = "^Habi.*\\.csv$",
  full.names = TRUE
)

shapes<-list.files(
  pattern = "^Directiva.*\\.shp$",
  full.names = TRUE
)

lista_sf<-list()
for (i in 1:length(shapes)){
  print(provincias[i,2])
  habitat_temp<-read_csv(archivos_csv[i],,
                         locale = locale(encoding = "ISO-8859-1"),
                         show_col_types = FALSE) %>%
    mutate(across(where(is.character), ~ gsub("\\+", "", .))) %>% 
    dplyr::select(2:8) %>%
    relocate(CODIGO,.before=1) %>% 
    mutate(CODIGO=as.character(CODIGO))
  shape_temp<-st_read(shapes[i]) %>% 
    arrange(CODIGO) %>% 
    group_by(CODIGO) %>%
    summarise(geometry = st_union(geometry), .groups = "drop") %>% 
    left_join(habitat_temp,by="CODIGO",
    relationship="many-to-many") %>% 
    st_transform(crs = 4326)
  #plot(shape_temp)
  lista_sf[[i]]<-shape_temp

}

shape_total <- do.call(rbind, lista_sf)
# Paso 3: Unir todos los shapes
shapes_combined <- bind_rows(shapes)

# Paso 4: Guardar el shapefile combinado
st_write(shapes_combined, "shapefile_combinado.shp", delete_dsn = TRUE)
st_write(shape_total, "shapefile_combinado.shp", delete_dsn = TRUE)

## Prueba 9130 Asperulo fagetum
lista_sf_9130<-list()
for (i in 1:length(shapes)){
  print(provincias[i,2])
  habitat_temp<-read_csv(archivos_csv[i],,
                         locale = locale(encoding = "ISO-8859-1"),
                         show_col_types = FALSE) %>%
    mutate(across(where(is.character), ~ gsub("\\+", "", .))) %>% 
    dplyr::select(2:8) %>%
    relocate(CODIGO,.before=1) %>% 
    mutate(CODIGO=as.character(CODIGO))
  shape_temp<-st_read(shapes[i]) %>% 
    arrange(CODIGO) %>% 
    group_by(CODIGO) %>%
     
    summarise(geometry = st_union(geometry), .groups = "drop") %>% 
    left_join(habitat_temp,by="CODIGO",
              relationship="many-to-many") %>%
    filter(CODIGO_UE == "9130") %>%
    st_transform(crs = 4326)
  #plot(shape_temp)
  lista_sf_9130[[i]]<-shape_temp
  
}

