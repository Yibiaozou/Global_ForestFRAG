rm(list=ls())
# library(tidyverse)
library(tictoc)
library(raster)
library(landscapemetrics)
library(parallel)
library(foreach)
library(doParallel)


# library(RSpectra)
# library(dismo)
# library(readxl)
# library(sf)
# library(stars)
# library(terra)

# library(Reconnect)


raster0 <- raster("/nfs/nas22.ethz.ch/fs2201/usys_ibz_cr_lab/Yibiao/FRAG/Forest_Extent/2000/00N_060W.tif")

extent_Ras <- extent(raster0)

LON <- extent_Ras[1]
LAT <- extent_Ras[3]

Res <- 1/24
N <- 10/Res

vector1 <- rep(1:N, each = N)
vector2 <- rep(1:N, N)

# Combine into a list of pairs
input_list <- mapply(list, vector1, vector2, SIMPLIFY = FALSE)

num_cores <- 120  # Leave eight core free for system processes

# Set up parallel backend to use multicore, if available
cl <- makeCluster(num_cores)
doParallel::registerDoParallel(cl)


setwd("/nfs/nas22.ethz.ch/fs2201/usys_ibz_cr_lab/Yibiao/FRAG/Forest_Extent/2000")

# setwd("/cluster/home/yibzou/FRAG/Forest_Extent/2000/")
fileList <- list.files()

for(s in 1:length(fileList)){
  rs <- fileList[s]
  

  tic()
  
  results <- foreach(pair = iter(input_list),.packages=(.packages()), .combine = 'rbind') %dopar% {
    
    ########################################
    ### Main code for parallel computing----
    i <- pair[[1]]
    j <- pair[[2]]
    
    setwd("/nfs/nas22.ethz.ch/fs2201/usys_ibz_cr_lab/Yibiao/FRAG/Forest_Extent/2000")
    # setwd("/cluster/home/yibzou/FRAG/Forest_Extent/2000_extra/")
    
    raster0 <- raster(rs)
    
    
    extent_Ras <- extent(raster0)
    
    LON <- extent_Ras[1]
    LAT <- extent_Ras[3]
    
    Res <- 1/24
    N <- 10/Res
    
    lon <- LON + (i-1)*Res
    lat <- LAT + (j-1)*Res
    new_extent <- extent(lon, lon+Res, lat, lat+Res)
    
    # MPC_Df_Seg <- data.frame(cbind(longitude=lon, latitude=lat, lsi=0))
    
    MPC_Df_Seg <- data.frame(cbind(longitude=lon, latitude=lat, 
                                   tca=NA, lpi=NA, ldi=NA,
                                   ed=NA, np=NA, mpa=NA,
                                   ai=NA, enn=NA, pladj=NA,
                                   cover=NA, nlsi=NA,
                                   clumpy=NA, parmn=NA,
                                   pci=NA, si=NA))
    
    tryCatch({
    habitat_raster_crop <- crop(raster0, new_extent)
    
    if(sum(as.matrix(habitat_raster_crop))>0){
      
      # Percentage of canopy cover 
      ca <- lsm_c_ca(habitat_raster_crop)
      cover <- ca$value[ca$class==1]/sum(ca$value)
      
      # TCA: Total core area
      tca <- lsm_c_tca(habitat_raster_crop)$value[2]  
      
      # LPI: largest patch index
      lpi <- lsm_c_lpi(habitat_raster_crop)$value[2]   
      
      # Landscape division index (class level)
      ldi <- lsm_c_division(habitat_raster_crop)$value[2]  
      
      # Edge Density
      ed <- lsm_c_ed(habitat_raster_crop)$value[2] 
      
      # Number of patches
      np <- lsm_c_np(habitat_raster_crop)$value[2] 
      
      # Mean patch area
      mpa <- lsm_c_area_mn(habitat_raster_crop)$value[2] 
      
      # Aggregation index
      ai <- lsm_c_ai(habitat_raster_crop)$value[2]   
      
      # Mean of Euclidean nearest-neighbor distance
      enn <- lsm_c_enn_mn(habitat_raster_crop)$value[2] 
      # if(is.na(enn)){enn <- 0}

      # Percentage of Like Adjacencies
      pladj <- lsm_c_pladj(habitat_raster_crop)$value[2] 

      # Normalized landscape shape index
      nlsi <- lsm_c_nlsi(habitat_raster_crop)$value[2]
      
      # Clumpiness index
      clumpy <- lsm_c_clumpy(habitat_raster_crop)$value[2]
      
      # Mean perimeter-area ratio
      parmn <- lsm_c_para_mn(habitat_raster_crop)$value[2]
      
      # Patch cohesion index
      pci <- lsm_c_cohesion(habitat_raster_crop)$value[2] 
      
      # Splitting index
      si <- lsm_c_split(habitat_raster_crop)$value[2]
      
      MPC_Df_Seg <- data.frame(cbind(longitude=lon, latitude=lat, 
                                     tca=tca, lpi=lpi, ldi=ldi,
                                     ed=ed, np=np, mpa=mpa,
                                     ai=ai, enn=enn, pladj=pladj,
                                     cover=cover, nlsi=nlsi,
                                     clumpy=clumpy, parmn=parmn,
                                     pci=pci, si=si))
    }
    
    }, error = function(e){
      MPC_Df_Seg <- data.frame(cbind(longitude=lon, latitude=lat, 
                                     tca=NA, lpi=NA, ldi=NA,
                                     ed=NA, np=NA, mpa=NA,
                                     ai=NA, enn=NA, pladj=NA,
                                     cover=NA, nlsi=NA,
                                     clumpy=NA, parmn=NA,
                                     pci=NA, si=NA))
    })
  }
  toc()
  
  rs_name <- strsplit(rs, split = "\\.")[[1]][1]
  
  # saveRDS(results, file=paste0("C:/Zeus/ETHz/ETHz_S7/Forest_GFC/Outcome/2024_10_29/frag_df_",rs_name,".rds"))
  
  saveRDS(results, file=paste0("/cluster/home/yibzou/FRAG/Outcome/2024_12_2_FRAG_Scale/frag_df_Y2000_",rs_name,"_5km.rds"))
  
}

# Stop the parallel cluster
stopCluster(cl)



