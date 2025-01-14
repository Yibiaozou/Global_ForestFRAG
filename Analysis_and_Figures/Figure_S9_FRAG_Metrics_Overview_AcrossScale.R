rm(list=ls())

library(rvest)
library(tidyverse)
library(tictoc)
library(raster)
library(gridExtra)

####----Preprocess forest extent data by different scales----####

# res <- 20
# year <- 2000

### stack dataset
Df_Stack <- function(res,year){
  setwd(paste0("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Outcome/2024_12_2_FRAG_Scale/",res,"km/",year))
  fileName <- list.files()
  fileList <- lapply(1:length(fileName), function(x) readRDS(fileName[x]))
  
  frag_df <- do.call(rbind, fileList)
  saveRDS(frag_df, file=paste0("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Outcome/2024_12_2_FRAG_Scale/frag_df_",res,"km_",year,".rds"))
}


for(res in c(10, 20, 60)){
  for(year in c(2000, 2020)){
    Df_Stack(res=res, year=year)
  }
}


# res <- 20

### combine data of 2000 and 2020
frag_df_correct <- function(res){
  tic()
  frag_df_2000_glad <- readRDS(paste0("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Outcome/2024_12_2_FRAG_Scale/frag_df_",res,"km_2000.rds"))
  frag_df_2020_glad <- readRDS(paste0("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Outcome/2024_12_2_FRAG_Scale/frag_df_",res,"km_2020.rds"))
  
  AA <- colnames(frag_df_2020_glad)
  BB <- c("longitude", "latitude", sapply(3:11, function(x) paste0(AA[x],"_2020")))
  colnames(frag_df_2020_glad) <- BB
  
  frag_df_full_glad <- full_join(frag_df_2000_glad, frag_df_2020_glad, by=c("longitude","latitude"))
  
  
  frag_df_full_glad <- frag_df_full_glad%>%mutate(tca_diff=tca_2020-tca, lpi_diff=lpi_2020-lpi, ldi_diff=ldi_2020-ldi,
                                                  ed_diff=ed_2020-ed, np_diff=np_2020-np, mpa_diff=mpa_2020-mpa, 
                                                  ai_diff=ai_2020-ai, enn_diff=enn_2020-enn, pladj_diff=pladj_2020-pladj)
  
  saveRDS(frag_df_full_glad, file=paste0("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Outcome/2024_12_2_FRAG_Scale/frag_df_full_glad_",res,"km.rds"))
  toc()
}

for(res in c(10, 20, 60)){
  frag_df_correct(res)
}

### compute ffi and cfi



frag_ffi_cfi <- function(res){
  tic()
  
  frag_var_scale <- function(var){
    var_2020 <- paste0(var,"_2020")
    
    Q3 <- as.numeric(quantile(frag_df_full_glad[,var_2020], na.rm=T)[4])
    Q1 <- as.numeric(quantile(frag_df_full_glad[,var_2020], na.rm=T)[2])
    IQR <- Q3-Q1
    Q_upper <- Q3 + 1.5*IQR
    Q_lower <- Q1 - 1.5*IQR
    
    y_2000 <- frag_df_full_glad[,var]
    y_2020 <- frag_df_full_glad[,var_2020]
    
    # if(Q_lower<0){Q_lower<-0}
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
    
    return(list(Y_2000=Y_2000,Y_2020=Y_2020, Q_max=max(y_2000,na.rm=T), Q_min=min(y_2000,na.rm=T)))
  }
  
  frag_df_full_glad <- readRDS(paste0("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Outcome/2024_12_2_FRAG_Scale/frag_df_full_glad_",res,"km.rds"))
  
  np_scale <- frag_var_scale("np")
  ed_scale <- frag_var_scale("ed")
  mpa_scale <- frag_var_scale("mpa")
  
  FFI_2000 <- (np_scale[[1]]+ed_scale[[1]]+(1-mpa_scale[[1]]))/3
  FFI_2020 <- (np_scale[[2]]+ed_scale[[2]]+(1-mpa_scale[[2]]))/3
  
  tca_scale <- frag_var_scale("tca")
  lpi_scale <- frag_var_scale("lpi")
  ldi_scale <- frag_var_scale("ldi")
  
  CFI_2000 <- (ldi_scale[[1]]+(1-tca_scale[[1]])+(1-lpi_scale[[1]]))/3
  CFI_2020 <- (ldi_scale[[2]]+(1-tca_scale[[2]])+(1-lpi_scale[[2]]))/3
  
  ai_scale <- frag_var_scale("ai")
  enn_scale <- frag_var_scale("enn")
  pladj_scale <- frag_var_scale("pladj")
  
  AFI_2000 <- (enn_scale[[1]]+(1-ai_scale[[1]])+(1-pladj_scale[[1]]))/3
  AFI_2020 <- (enn_scale[[2]]+(1-ai_scale[[2]])+(1-pladj_scale[[2]]))/3
  
  frag_df_full_glad <- frag_df_full_glad%>%mutate(CFI=CFI_2000, CFI_2020=CFI_2020,
                                                  AFI=AFI_2000, AFI_2020=AFI_2020,
                                                  FFI=FFI_2000, FFI_2020=FFI_2020)
  frag_df_full_glad <- frag_df_full_glad%>%mutate(CFI_diff=CFI_2020-CFI, 
                                                  AFI_diff=AFI_2020-AFI,
                                                  FFI_diff=FFI_2020-FFI)
  
  saveRDS(frag_df_full_glad, file=paste0("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Outcome/2024_12_2_FRAG_Scale/frag_df_full_glad_",res,"km.rds"))

  toc()
}

for(res in c(10, 20, 60)){
  frag_ffi_cfi(res)
}

####----Analysis on frag change and cover change----####

res <- 60
frag_df_full_glad <- readRDS(paste0("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Outcome/2024_12_2_FRAG_Scale/frag_df_full_glad_",res,"km.rds"))

# frag_df_full_glad$mpa <- frag_df_full_glad$cover*2500/frag_df_full_glad$np
# frag_df_full_glad$mpa_2020 <- frag_df_full_glad$cover_2020*2500/frag_df_full_glad$np_2020

frag_df_full_glad <- frag_df_full_glad%>%mutate(mpa_diff=mpa_2020-mpa)

WWF_Biome <- raster("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/biome/WWF_Biome.tif")
frag_df_full_glad$WWF_Biome <- raster::extract(WWF_Biome,frag_df_full_glad[,c("longitude","latitude")])
frag_df_full_glad$Mega_Biome <- NA
frag_df_full_glad$Mega_Biome[frag_df_full_glad$WWF_Biome%in%c(1:3,7)] <- "Tropical"
frag_df_full_glad$Mega_Biome[frag_df_full_glad$WWF_Biome%in%c(4:5,8)] <- "Temperate"
frag_df_full_glad$Mega_Biome[frag_df_full_glad$WWF_Biome%in%c(6)] <- "Boreal"
frag_df_full_glad$Mega_Biome[frag_df_full_glad$WWF_Biome%in%c(12)] <- "Mediterranean"

# frag_df_full_glad$abs_cover_diff <- abs(frag_df_full_glad$cover_diff)
frag_df_full_glad$Mega_Biome[frag_df_full_glad$Mega_Biome=="Mediterranean"]<-"Temperate"
# saveRDS(frag_df_full_glad, file=paste0("F:/BackUp_PhD_ETH/ETHz_S5/Forest_FRAG/Outcome/2023.10.30_ScaleAnalysis/frag_df_full_glad_",res,"km.rds"))

frag_df_full_glad$cover <- frag_df_full_glad$tca/max(frag_df_full_glad$tca, na.rm=T)
hist(frag_df_full_glad$cover)

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

AB <- pixel_area(frag_df_full_glad$latitude, res/120, res/120)

frag_df_full_glad <- frag_df_full_glad%>%mutate(pixelArea=AB/10^6)



frag_diff_stat <- function(var, direction=1, biome="All", forestT=0.3, fragT=0, coverDiffT=0){
  
  # var="FFI"
  var_diff <- paste0(var, "_diff")
  
  
  frag_df_full_glad_sub <- frag_df_full_glad[(!is.na(frag_df_full_glad[,var_diff]))&(frag_df_full_glad$cover>=forestT),]
  # frag_df_full_glad_sub <- frag_df_full_glad_sub[!is.na(frag_df_full_glad_sub$abs_cover_diff)&(frag_df_full_glad_sub$abs_cover_diff>=coverDiffT),]
  # 
  if(biome!="All"){
    frag_df_full_glad_sub <- frag_df_full_glad_sub[frag_df_full_glad_sub$Mega_Biome==biome,]
  }
  
  absmax <- max(abs(frag_df_full_glad[,var]), na.rm=T) 
  frag_df_full_glad_sub[,var_diff] <- frag_df_full_glad_sub[,var_diff]/absmax
  
  frag_df_filter_glad_sub <- frag_df_full_glad_sub[!is.na(frag_df_full_glad_sub[,var_diff]),]
  A1 <- frag_df_full_glad_sub[(frag_df_full_glad_sub[,var_diff]>fragT)&!is.na(frag_df_full_glad_sub[,var_diff]),]
  B1 <- frag_df_full_glad_sub[(frag_df_full_glad_sub[,var_diff]< -1*fragT)&!is.na(frag_df_full_glad_sub[,var_diff]),]
  
  pp <- sum(A1$pixelArea)/sum(frag_df_filter_glad_sub$pixelArea)
  # pp <- nrow(A1)/nrow(frag_df_filter_glad_sub)
  pp_2 <- round(pp*100, digits = 1)
  # pp_1 <- nrow(B1)/nrow(frag_df_filter_glad_sub)
  
  pp_1 <- sum(B1$pixelArea)/sum(frag_df_filter_glad_sub$pixelArea)
  pp_1 <- round(pp_1*100, digits = 1)
  # pp_1 <- 100-pp_2
  
  if(direction==1){
    pp <- pp_2
    ForestArea <- sum(A1$cover*A1$pixelArea,na.rm=T)
    RegionArea <- sum(nrow(A1)*A1$pixelArea)
  }else if(direction==0){
    pp <- pp_1
    ForestArea <- sum(B1$cover*B1$pixelArea,na.rm=T)
    RegionArea <- sum(nrow(B1)*B1$pixelArea)
  }
  # corr_change <- cor(frag_df_full_glad_subChange[,var_diff], frag_df_full_glad_subChange$cover_diff, use="complete.obs")
  return(list(pp=pp,
              #corr=corr,corr_change=corr_change,
              ForestArea=ForestArea, RegionArea=RegionArea))
}

frag_vec <- c("CFI", "lpi", "ed","np","ldi", "FFI","mpa", 
              "tca", "AFI", "ai", "enn", "pladj")
direction_vec <- c(1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0)


biome_vec <- c("All", "Tropical", "Temperate", "Boreal")
# biome_vec <- c("All", "Tropical", "Temperate", "Mediterranean", "Boreal")

frag_stat_df <- NULL
for(k in 1:length(biome_vec)){
  for(i in 1:length(frag_vec)){
    tic()
    frag_stat_sub <- frag_diff_stat(var=frag_vec[i], direction=direction_vec[i], biome=biome_vec[k])
    
    frag_stat_df <- rbind(frag_stat_df, data.frame(cbind(pp=frag_stat_sub$pp, 
                                                         corr=frag_stat_sub$corr, 
                                                         corr_change=frag_stat_sub$corr_change,
                                                         ForestArea=frag_stat_sub$ForestArea,
                                                         RegionArea=frag_stat_sub$RegionArea,
                                                         frag=frag_vec[i],
                                                         biome=biome_vec[k])))
    toc()
  }
}

frag_stat_df <- frag_stat_df%>%mutate(pp=as.numeric(pp),
                                      # corr=as.numeric(corr),
                                      # corr_change=as.numeric(corr_change),
                                      ForestArea=as.numeric(ForestArea),
                                      RegionArea=as.numeric(RegionArea))

# frag_stat_df$corr_change_abs <- abs(frag_stat_df$corr_change)
# frag_stat_df$corr_abs <- abs(frag_stat_df$corr)


# frag_stat_df$frag <- frag_vec
# frag_stat_df[frag_stat_df$biome=="All",] <- frag_stat_df[frag_stat_df$biome=="All",][order(frag_stat_df[frag_stat_df$biome=="All",]$corr_change_abs,decreasing = T),]

frag_stat_df$frag <- factor(frag_stat_df$frag, levels=frag_stat_df[frag_stat_df$biome=="All",]$frag, ordered=T)
frag_df_full_glad_sub <- frag_df_full_glad[(!is.na(frag_df_full_glad[,"CFI_diff"]))&frag_df_full_glad$cover>=0.3,]

Global_ForestArea <- sum(frag_df_full_glad_sub$cover*frag_df_full_glad_sub$pixelArea, na.rm=T)
Tropical_ForestArea <- sum(frag_df_full_glad_sub$cover[frag_df_full_glad_sub$Mega_Biome=="Tropical"]*frag_df_full_glad_sub$pixelArea[frag_df_full_glad_sub$Mega_Biome=="Tropical"], na.rm=T)
Temperate_ForestArea <- sum(frag_df_full_glad_sub$cover[frag_df_full_glad_sub$Mega_Biome=="Temperate"]*frag_df_full_glad_sub$pixelArea[frag_df_full_glad_sub$Mega_Biome=="Temperate"], na.rm=T)
Boreal_ForestArea <- sum(frag_df_full_glad_sub$cover[frag_df_full_glad_sub$Mega_Biome=="Boreal"]*frag_df_full_glad_sub$pixelArea[frag_df_full_glad_sub$Mega_Biome=="Boreal"], na.rm=T)

sum(c(Tropical_ForestArea, Temperate_ForestArea, Boreal_ForestArea))

frag_stat_df$ForeatAreaPec <- NA
frag_stat_df$ForeatAreaPec[frag_stat_df$biome=="All"] <- frag_stat_df$ForestArea[frag_stat_df$biome=="All"]/Global_ForestArea
frag_stat_df$ForeatAreaPec[frag_stat_df$biome=="Tropical"] <- frag_stat_df$ForestArea[frag_stat_df$biome=="Tropical"]/Tropical_ForestArea
frag_stat_df$ForeatAreaPec[frag_stat_df$biome=="Temperate"] <- frag_stat_df$ForestArea[frag_stat_df$biome=="Temperate"]/Temperate_ForestArea
frag_stat_df$ForeatAreaPec[frag_stat_df$biome=="Boreal"] <- frag_stat_df$ForestArea[frag_stat_df$biome=="Boreal"]/Boreal_ForestArea
# frag_stat_df <-readRDS("D:/Zeus/ETH_zurich_MSc/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/frag_stat_df_0.3cover_0threshold_0diffT.rds")

# frag_stat_df$frag[frag_stat_df$frag=="ffi"] <- "FFI"
frag_stat_df$class <- NA
frag_stat_df$class[frag_stat_df$frag%in%c("lpi", "ldi","tca", "CFI")] <- "Connectivity"
frag_stat_df$class[frag_stat_df$frag%in%c("ed","np","FFI","mpa")] <- "Separation"
frag_stat_df$class[frag_stat_df$frag%in%c("AFI","enn","ai","pladj")] <- "Aggregation"
saveRDS(frag_stat_df, file=paste0("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Outcome/2024.10.24_ScienceResponse/frag_stat_df_",res,"kmRes_0.3cover_0threshold_0diffT.rds"))



frag_stat_df_select <- frag_stat_df[frag_stat_df$frag%in%c("CFI","FFI"),]
frag_stat_df_select <- frag_stat_df_select%>%mutate(
  biome=fct_relevel(biome,"All","Tropical","Temperate","Boreal")
)

ggplot(frag_stat_df_select, aes(x=biome, y=pp, fill=frag))+
  geom_bar(position="dodge", stat = "identity", alpha=0.5, color="black")+
  # geom_errorbar(aes(ymin = pp_mean-2*pp_se, ymax = pp_mean+2*pp_se), width=0.3,linewidth=1,position=position_dodge(.9)) +
  ylim(c(0,100))+
  # ggtitle("Boreal")+
  scale_fill_manual(values=c( "#ca0020","#0571b0"))+
  xlab("Biome")+
  ylab("Increased fragmentation (%)")+
  # geom_hline(yintercept = baseline, color="red", linetype="dashed",linewidth=1)+
  geom_hline(yintercept = 50, color="grey50", linetype="dashed",linewidth=1)+
  theme_classic()+
  theme(title = element_text(size=15,family = "sans", face="bold"),
        # axis.text.x=element_text(size=12,family = "sans", face="bold", angle=60, vjust = 1, hjust=1),
        axis.text.x=element_text(size=15,family = "sans", face="bold", angle=0),
        axis.text.y=element_text(size=15,family = "sans", face="bold"),
        axis.title.x=element_text(size=20,family = "sans", face="bold"),
        axis.title.y=element_text(size=20,family = "sans", face="bold"),
        # legend.position = "none",
        legend.text = element_text(colour = "black", size = 12,family = "sans", face="bold"),
        legend.title = element_blank())



#### Graphs for all 
frag_stat_df <- frag_stat_df%>%mutate(
  biome=fct_relevel(biome,"All","Tropical","Mediterranean","Temperate","Boreal"),
  class=fct_relevel(class,"Cover-sensitive","Cover-insensitive")
)

se <- function(a){
  a <- a[!is.na(a)]
  return(sd(a)/sqrt(length((a))))
}

frag_stat_df_aggregate <- frag_stat_df%>%group_by(biome, class)%>%
  summarise(pp_mean=mean(pp, na.rm=T), pp_std=sd(pp, na.rm=T), pp_se=se(pp))%>%drop_na()

library(forcats)
frag_stat_df_aggregate <- frag_stat_df_aggregate%>%mutate(
  biome=as.factor(biome),
  class=as.factor(class)
)

frag_stat_df_aggregate <- frag_stat_df_aggregate%>%mutate(
  biome=fct_relevel(biome,"All","Tropical","Mediterranean","Temperate","Boreal"),
  class=fct_relevel(class,"Cover-sensitive","Cover-insensitive")
)

ggplot(frag_stat_df_aggregate, aes(x=biome, y=pp_mean, fill=class))+
  geom_hline(yintercept = 50, color="grey1", linetype="dashed",linewidth=1)+
  geom_bar(position="dodge", stat = "identity", alpha=0.5)+
  geom_errorbar(aes(ymin = pp_mean-2*pp_se, ymax = pp_mean+2*pp_se), width=0.3,linewidth=1,position=position_dodge(.9)) +
  ylim(c(0,100))+
  # ggtitle("Boreal")+
  scale_fill_manual(values=c( "#ca0020","#0571b0"))+
  xlab("Biome")+
  ylab("Increased fragmentation (%)")+
  # geom_hline(yintercept = baseline, color="red", linetype="dashed",linewidth=1)+
  geom_hline(yintercept = 50, color="black", linetype="dashed",linewidth=1)+
  theme_classic()+
  theme(title = element_text(size=15,family = "sans", face="bold"),
        # axis.text.x=element_text(size=12,family = "sans", face="bold", angle=60, vjust = 1, hjust=1),
        axis.text.x=element_text(size=15,family = "sans", face="bold", angle=0),
        axis.text.y=element_text(size=15,family = "sans", face="bold"),
        axis.title.x=element_text(size=20,family = "sans", face="bold"),
        axis.title.y=element_text(size=20,family = "sans", face="bold"),
        legend.position = "none",
        legend.text = element_text(colour = "black", size = 12,family = "sans", face="bold"),
        legend.title = element_blank())