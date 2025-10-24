# =========================================================
# TPI MULTIESCALA + DoG - DEM Europa + Norte de África (1 km)
# Autor:Alejandro Gonzalez
# Fecha: 2025-10-10
# =========================================================
library(raster)
library(terra)
library(stats)
library(tidyverse)
extent_global<-extent(c(-16.47,93.53,18.24,78))
dem<-rast("P:/Grupos/Ger_Calidad_Eva_Ambiental_Bio/DpMNatural_TEC/3081208_PN_RESTAURACION/CARTOGRAFÍA/ambientes/MDT.tif") %>% 
  crop(extent_global)

tpi<-terrain(dem, v="TPI", neighbors=8)
# ---------------------------------------------------------
# 1. Cargar DEM
# ---------------------------------------------------------
cat("Resolución (m):", res(dem), "\n")

# ---------------------------------------------------------
# 2. Funciones auxiliares
# ---------------------------------------------------------

# Kernel circular (radio en celdas)
circular_w <- function(radius_cells){
  diam <- radius_cells*2 + 1
  m <- matrix(0, nrow=diam, ncol=diam)
  cx <- cy <- radius_cells + 1
  for(i in 1:diam){
    for(j in 1:diam){
      if (sqrt((i-cx)^2 + (j-cy)^2) <= radius_cells) m[i,j] <- 1
    }
  }
  m / sum(m)
}

# TPI = elevación - media vecindad
tpi_by_kernel <- function(dem, kernel, out_name){
  neigh_mean <- focal(dem, w=kernel, fun="mean", na.policy='only',
                      filename=paste0(out_name, "_mean.tif"), overwrite=TRUE)
  tpi <- dem - neigh_mean
  writeRaster(tpi, paste0(out_name, ".tif"), overwrite=TRUE)
  return(tpi)
}

# Z-score global
zscore_raster <- function(r, out_name){
  g <- global(r, c("mean","sd"), na.rm=TRUE)
  r2 <- (r - g[1,1]) / g[1,2]
  writeRaster(r2, paste0(out_name, "_z.tif"), overwrite=TRUE)
  return(r2)
}

# Kernel Gaussiano
gaussian_w <- function(sigma, size = ceiling(sigma*6)){
  if(size %% 2 == 0) size <- size + 1
  cx <- cy <- (size+1)/2
  m <- matrix(0, nrow=size, ncol=size)
  for(i in 1:size){
    for(j in 1:size){
      m[i,j] <- exp(-((i-cx)^2 + (j-cy)^2) / (2 * sigma^2))
    }
  }
  m / sum(m)
}

# ---------------------------------------------------------
# 3. TPI por escalas (5 km, 30 km, 100 km)
# ---------------------------------------------------------
radios_cells <- c(0.0083*5, 0.0083*30, 0.0083*100)
names(radios_cells) <- c("small", "medium", "large")

kernels <- lapply(radios_cells, circular_w)

tpi_layers <- mapply(function(k, rname){
  cat("Calculando TPI para escala:", rname, "\n")
  tpi_by_kernel(dem, k, paste0("TPI_", rname))
}, kernels, names(radios_cells), SIMPLIFY = FALSE)

tpi_z <- mapply(function(r, rname){
  cat("Estandarizando TPI:", rname, "\n")
  zscore_raster(r, paste0("TPI_", rname))
}, tpi_layers, names(radios_cells), SIMPLIFY = FALSE)

tpi_stack <- rast(tpi_z)
names(tpi_stack) <- names(radios_cells)

# ---------------------------------------------------------
# 4. Combinaciones multiescala
# ---------------------------------------------------------

# 4a) Suma ponderada
weights <- c(0.3, 0.5, 0.2)
tpi_weighted <- tpi_stack[[1]]*weights[1] +
  tpi_stack[[2]]*weights[2] +
  tpi_stack[[3]]*weights[3]
writeRaster(tpi_weighted, "TPI_multiescala_weighted.tif", overwrite=TRUE)

# 4b) PCA sobre muestra
sample_size <- 100000
vals <- spatSample(tpi_stack, size=sample_size, method="random", na.rm=TRUE, as.df=TRUE)
pca <- prcomp(vals, center=TRUE, scale.=TRUE)
cat("\n--- PCA ---\n")
print(summary(pca))

pc1_r <- app(tpi_stack, fun=function(...) {
  v <- c(...)
  if(any(is.na(v))) return(NA)
  sc <- (v - pca$center) / pca$scale
  sum(sc * pca$rotation[,1])
})
writeRaster(pc1_r, "TPI_multiescala_PC1.tif", overwrite=TRUE)

# 4c) Escala dominante (por magnitud)
tpi_abs <- abs(tpi_stack)
dominant_scale <- app(tpi_abs, which.max)
writeRaster(dominant_scale, "TPI_dominant_scale.tif", overwrite=TRUE)

# ---------------------------------------------------------
# 5. Difference of Gaussians (DoG)
# ---------------------------------------------------------

sigma1 <- 20  # km
sigma2 <- 100 # km
g1 <- gaussian_w(sigma1)
g2 <- gaussian_w(sigma2)

cat("\nCalculando suavizados Gaussianos (DoG)...\n")

sm1 <- focal(dem, w=g1, fun="mean", na.policy='only',
             filename="DEM_gauss20km.tif", overwrite=TRUE)
sm2 <- focal(dem, w=g2, fun="mean", na.policy='only',
             filename="DEM_gauss100km.tif", overwrite=TRUE)

dog <- sm1 - sm2
writeRaster(dog, "TPI_DoG_20_100km.tif", overwrite=TRUE)

# ---------------------------------------------------------
# 6. Visualización rápida
# ---------------------------------------------------------
par(mfrow=c(2,2))
plot(tpi_weighted, main="TPI multiescala (ponderado)")
plot(pc1_r, main="TPI multiescala (PCA - PC1)")
plot(dominant_scale, main="Escala dominante")
plot(dog, main="DoG (σ=20 - σ=100 km)")

cat("\n✅ Proceso terminado con éxito.\n")
cat("Archivos generados:\n",
    " - TPI_small_z.tif / medium / large\n",
    " - TPI_multiescala_weighted.tif\n",
    " - TPI_multiescala_PC1.tif\n",
    " - TPI_dominant_scale.tif\n",
    " - TPI_DoG_20_100km.tif\n")
