rm(list=ls())
library(tidyverse)
library(tictoc)
library(igraph)
library(raster)
library(landscapemetrics)
library(ggpubr)
library(cowplot)
library(sjPlot)
library(ggcorrplot)
library(ggrepel)
library(ggplot2)
library(ggforce)
library(AMR)

frag_df_2000_glad <- readRDS("/home/ybzou/FRAG/glad_2000/frag_df_2000_glad_extent_5m.rds")
frag_df_2020_glad <- readRDS("/home/ybzou/FRAG/glad_2020/frag_df_2020_glad_extent_5m.rds")


frag_df_full_glad <- full_join(frag_df_2000_glad, frag_df_2020_glad, by=c("longitude","latitude"))


# Function to calculate area of a pixel at a given latitude
pixel_area <- function(lat, res_x, res_y) {
  # Earth radius in meters
  R <- 6378137 
  
  # Convert resolution from degrees to radians
  res_x_rad <- res_x * (pi / 180)
  res_y_rad <- res_y * (pi / 180)
  
  # Calculate the area
  area <- (R^2) * res_x_rad * sin(lat * (pi / 180) + res_y_rad / 2) - (R^2) * res_x_rad * sin(lat * (pi / 180) - res_y_rad / 2)
  return(area)
}


## compute corrected pixel area based on latitude and resolution
pixelArea <- pixel_area(frag_df_full_glad$latitude, 1/24, 1/24)
frag_df_full_glad <- frag_df_full_glad%>%mutate(pixelArea=pixelArea/10^6)


## compute mean patch area for year 2000 and 2020
frag_df_full_glad <- frag_df_full_glad%>%mutate(mpa=cover*pixelArea/np, 
                                                mpa_2020=cover_2020*pixelArea/np_2020)

## extract information of biome and ecoregion for each forest grid cell 
WWF_Biome <- raster("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/biome/WWF_Biome.tif")
Resolve_Ecoregion <- raster("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/biome/Resolve_Ecoregion_Global.tif")

frag_df_full_glad$Ecoregion <- raster::extract(Resolve_Ecoregion,frag_df_full_glad[,c("longitude","latitude")])
frag_df_full_glad$WWF_Biome <- raster::extract(WWF_Biome,frag_df_full_glad[,c("longitude","latitude")])

## merge WWF biome into three mega forest biomes: tropical, temperate and boreal
frag_df_full_glad$Mega_Biome <- NA
frag_df_full_glad$Mega_Biome[frag_df_full_glad$WWF_Biome%in%c(1:3,7)] <- "Tropical"
frag_df_full_glad$Mega_Biome[frag_df_full_glad$WWF_Biome%in%c(4:5,8, 12)] <- "Temperate"
frag_df_full_glad$Mega_Biome[frag_df_full_glad$WWF_Biome%in%c(6)] <- "Boreal"


## function to standardize fragmentation metrics
frag_var_scale <- function(var, direction=1){
  var_2020 <- paste0(var,"_2020")
  
  Q3 <- as.numeric(quantile(frag_df_full_glad[,var_2020], na.rm=T)[4])
  Q1 <- as.numeric(quantile(frag_df_full_glad[,var_2020], na.rm=T)[2])
  IQR <- Q3-Q1
  Q_upper <- Q3 + 1.5*IQR
  Q_lower <- Q1 - 1.5*IQR
  
  # if(Q_lower<0){Q_lower<-0}
  y_2000 <- frag_df_full_glad[,var]
  y_2020 <- frag_df_full_glad[,var_2020]
  
  y_2000_c <- y_2000
  y_2020_c <- y_2020
  y_2000_c[y_2000_c<Q_lower|y_2000_c>Q_upper] <- NA
  y_2020_c[y_2020_c<Q_lower|y_2020_c>Q_upper] <- NA
  
  Y_2000 <- (y_2000-min(y_2000_c,na.rm=T))/(max(y_2000_c,na.rm=T)-min(y_2000_c,na.rm=T))
  Y_2020 <- (y_2020-min(y_2020_c,na.rm=T))/(max(y_2020_c,na.rm=T)-min(y_2020_c,na.rm=T))
  
  Y_2000[Y_2000<0]<-0
  Y_2000[Y_2000>1]<-1
  Y_2020[Y_2020<0]<-0
  Y_2020[Y_2020>1]<-1
  
  # Y_2000[Y_2000<0|Y_2000>1] <- NA
  # Y_2020[Y_2020<0|Y_2020>1] <- NA
  
  if(direction==0){
    Y_2000 <- 1-Y_2000
    Y_2020 <- 1-Y_2020
  }
  
  return(list(Y_2000=Y_2000,Y_2020=Y_2020))
}

selected_var_2000 <- c("tca", "lpi", "ldi", "np", "mpa", "ed", "ai", "pci", "clumpy", "nlsi", "si", "par")
dirction_var <- c(0, 0, 1, 1, 0, 1, 0, 0, 0, 1, 1, 1)


ST_Var <- list()
for(i in 1:length(selected_var_2000)){
  ST_Var[[i]] <- frag_var_scale(selected_var_2000[i], dirction_var[i])
}

frag_df_full_glad <- frag_df_full_glad%>%mutate(TCA_ST=ST_Var[[1]][[1]], TCA_2020_ST=ST_Var[[1]][[2]],
                                                    LPI_ST=ST_Var[[2]][[1]], LPI_2020_ST=ST_Var[[2]][[2]],
                                                    LDI_ST=ST_Var[[3]][[1]], LDI_2020_ST=ST_Var[[3]][[2]],
                                                    NP_ST=ST_Var[[4]][[1]], NP_2020_ST=ST_Var[[4]][[2]],
                                                    MPA_ST=ST_Var[[5]][[1]], MPA_2020_ST=ST_Var[[5]][[2]],
                                                    ED_ST=ST_Var[[6]][[1]], ED_2020_ST=ST_Var[[6]][[2]],
                                                    AI_ST=ST_Var[[7]][[1]], AI_2020_ST=ST_Var[[7]][[2]],
                                                    PCI_ST=ST_Var[[8]][[1]], PCI_2020_ST=ST_Var[[8]][[2]],
                                                    CI_ST=ST_Var[[9]][[1]], CI_2020_ST=ST_Var[[9]][[2]],
                                                    NLSI_ST=ST_Var[[10]][[1]], NLSI_2020_ST=ST_Var[[10]][[2]],
                                                    SI_ST=ST_Var[[11]][[1]], SI_2020_ST=ST_Var[[11]][[2]],
                                                    PAR_ST=ST_Var[[12]][[1]], PAR_2020_ST=ST_Var[[12]][[2]]
)


selected_var_2000_2 <- c("lsi", "enn", "parmn", "pladj")
dirction_var_2 <- c(1, 1, 1, 0)


ST_Var_2 <- list()
for(i in 1:length(selected_var_2000_2)){
  ST_Var_2[[i]] <- frag_var_scale(selected_var_2000_2[i], dirction_var_2[i])
}

frag_df_full_glad <- frag_df_full_glad%>%mutate(LSI_ST=ST_Var_2[[1]][[1]], LSI_2020_ST=ST_Var_2[[1]][[2]],
                                                    ENN_ST=ST_Var_2[[2]][[1]], ENN_2020_ST=ST_Var_2[[2]][[2]],
                                                    PARMN_ST=ST_Var_2[[3]][[1]], PARMN_2020_ST=ST_Var_2[[3]][[2]],
                                                    PLADJ_ST=ST_Var_2[[4]][[1]], PLADJ_2020_ST=ST_Var_2[[4]][[2]]
)
## compute three summarised metrics FFI, AFI and CFI

## FFI is the summarised metric for separation-focused index including NP, MPA and ED
FFI <- (frag_df_full_glad$NP_ST+frag_df_full_glad$ED_ST+frag_df_full_glad$MPA_ST)/3
FFI_2020 <- (frag_df_full_glad$NP_2020_ST+frag_df_full_glad$ED_2020_ST+frag_df_full_glad$MPA_2020_ST)/3

## CFI is the summarised metric for connectivity-focused index including TCA, LPI and LDI
CFI <- (frag_df_full_glad$LPI_ST+frag_df_full_glad$TCA_ST+frag_df_full_glad$LDI_ST)/3
CFI_2020 <- (frag_df_full_glad$LPI_2020_ST+frag_df_full_glad$TCA_2020_ST+frag_df_full_glad$LDI_2020_ST)/3

## AFI is the summarised metric for aggregation-focused index including AI, ENN and PLADJ
AFI <- (frag_df_full_glad$AI_ST+frag_df_full_glad$ENN_ST+frag_df_full_glad$PLADJ_ST)/3
AFI_2020 <- (frag_df_full_glad$AI_2020_ST+frag_df_full_glad$ENN_2020_ST+frag_df_full_glad$PLADJ_2020_ST)/3


frag_df_full_glad$FFI <- FFI
frag_df_full_glad$CFI <- CFI
frag_df_full_glad$AFI <- AFI
frag_df_full_glad$FFI_2020 <- FFI_2020
frag_df_full_glad$CFI_2020 <- CFI_2020
frag_df_full_glad$AFI_2020 <- AFI_2020

saveRDS(frag_df_full_glad, file="/home/ybzou/FRAG/frag_df_Full_glad_extent_5m.rds")



