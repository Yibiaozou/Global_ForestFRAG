rm(list=ls())
library(tidyverse)
library(tictoc)
library(raster)
library(landscapemetrics)
library(landscapeR)




#---Generate simulated landscape----
m <- matrix(0, 100, 100)
r <- raster(m, xmn=0, xmx=100, ymn=0, ymx=100)


## Figure 1A,B,D left
centre <- 4850
rr1 <- makeClass(r, 1, size=6000, centre)
plot(rr1)


# rr1 <- raster(rr1)
writeRaster(rr1, file="D:/Zeus/ETH_zurich_MSc/ETHz_S5/Forest_FRAG/Data/Data_Product/example_landscape/fl_1_6000.tif")

## Figure 1B right
rr2 <- makeClass(r, 1, size=2000, centre)
plot(rr2)
# rr2$x <- rep(1:100, 100)
# rr2$y <- rep(1:100, each=100)

writeRaster(rr2, file="D:/Zeus/ETH_zurich_MSc/ETHz_S5/Forest_FRAG/Data/Data_Product/example_landscape/fl_1_2000.tif", overwrite=T)


## Figure 1A right
df <- as.data.frame(as.matrix(rr1), xy=TRUE)

df_cut <- df
df_cut$layer[df_cut$x%in%c(20, 40, 60, 80)]<-0
df_cut$layer[df_cut$y%in%c(20, 40, 60, 80)]<-0
df_cut <- df_cut[,2:4]
coordinates(df_cut) <- ~x + y  # Set the coordinates
gridded(df_cut) <- TRUE  # Convert to a grid format if your data is on a regular grid

rr3 <- raster(df_cut)
plot(rr3)
writeRaster(rr3, filename = "C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/Data_Product/example_landscape/fl_25_6000.tif")


# num <- 600
# size <- 10
# rr3 <- makeClass(r, num, size)
# plot(rr3)
# writeRaster(rr3, file="D:/Zeus/ETH_zurich_MSc/ETHz_S5/Forest_FRAG/Data/Data_Product/example_landscape/fl_600_10.tif")


# rr <- makePatch(r, size=5000,rast=TRUE)
# plot(rr)
#
# rr <- makePatch(r, size=2000, rast=TRUE)
# plot(rr)

## Figure 1C left
num <- 3
size <- 1000
rr4 <- makeClass(r, num, size)
plot(rr4)

writeRaster(rr4, file="D:/Zeus/ETH_zurich_MSc/ETHz_S5/Forest_FRAG/Data/Data_Product/example_landscape/fl_3_1000.tif")

## Figure 1C right
rr5 <- rr4
rr5[,0:40] <- 0
plot(rr5)
writeRaster(rr5, filename="D:/Zeus/ETH_zurich_MSc/ETHz_S5/Forest_FRAG/Data/Data_Product/example_landscape/fl_2_1000_V2.tif", overwrite=TRUE)

# m <- matrix(0, 100, 100)
# r <- raster(m, xmn=0, xmx=100, ymn=0, ymx=100)

# rr <- makePatch(r, size=5000,rast=TRUE)
# plot(rr)
#
# rr <- makePatch(r, size=2000, rast=TRUE)
# plot(rr)
centre <- c(2000, 2200, 3000, 3500, 4000, 4500)
rr6 <- makeClass(r, 6, size=1000, centre)
plot(rr6)

lsm_c_ca(rr6)$value[2]*10000

writeRaster(rr6, file="C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/Data_Product/example_landscape/fl_1_6000_elongatedShape.tif")


## Figure 1E right
centre <- as.matrix(rbind(c(20, 50),
                    c(80,50)))
rr7 <- makeClass(r, 2, size=500, centre)
plot(rr7)



writeRaster(rr7, file="C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/Data_Product/example_landscape/fl_2_500_less_aggregated.tif",overwrite=TRUE)

## Figure 1E left
rr8 <- rr7
rr8[] <- 0

rr8[,11:50] <- rr7[,1:40]
rr8[,46:85] <- rr7[,61:100]

plot(rr8)

writeRaster(rr8, file="C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/Data_Product/example_landscape/fl_2_500_more_aggregated.tif",overwrite=TRUE)

## Figure 1D right
centre <- rbind(c(10, 20), c(30,40), c(50, 70),
                c(70, 40), c(60, 30), c(70, 80))
rr9 <- makeClass(r, 6, size=1000, centre)
plot(rr9)

writeRaster(rr9, file="C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/Data_Product/example_landscape/fl_1_6000_elongatedShape_2.tif")



# plot(rr7[,10:50])
# plot(rr7[,10:50])

#---Visulization and compute frag metrics for simulated landscape----
rr1 <- raster("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/Data_Product/example_landscape/fl_1_6000.tif")
rr2 <- raster("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/Data_Product/example_landscape/fl_1_2000.tif")
rr3 <- raster("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/Data_Product/example_landscape/fl_25_6000.tif")
rr4 <- raster("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/Data_Product/example_landscape/fl_3_1000.tif")
rr5 <- raster("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/Data_Product/example_landscape/fl_2_1000_V2.tif")
rr6 <- raster("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/Data_Product/example_landscape/fl_1_6000_elongatedShape.tif")
rr7 <- raster("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/Data_Product/example_landscape/fl_2_500_less_aggregated.tif")
rr8 <- raster("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/Data_Product/example_landscape/fl_2_500_more_aggregated.tif")
rr9 <- raster("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/Data_Product/example_landscape/fl_1_6000_elongatedShape_2.tif")

# Test_2000 <- raster("E:/ETH_Zurich/ETHz_S5/Forest_FRAG/Forest_Extent/2000/30S_140E.tif")
# Test_2020 <- raster("E:/ETH_Zurich/ETHz_S5/Forest_FRAG/Forest_Extent/2020/30S_140E.tif")
# rr6 <- raster("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/Data_Product/example_landscape/fl_2_1000.tif")


plot(rr1)
plot(rr2)
plot(rr3)
plot(rr4)
plot(rr5)
plot(rr6)
plot(rr7)
plot(rr8)
plot(rr9)

rr9$x <- rep(1:100, 100)
rr9$y <- rep(1:100, each=100)
df_elongated <- as.data.frame(as.matrix(rr9), xy=TRUE)
df_elongated$layer <- df_elongated$fl_1_6000_elongatedShape_2
ggplot(data=df_elongated, aes(x=x, y=y, fill=layer)) +
  geom_tile() +
  scale_fill_gradient(low="grey", high="darkgreen") +
  coord_equal() +
  # theme(
  #   panel.grid.major = element_blank(),
  #   panel.grid.minor = element_blank(),
  #   panel.border = element_rect(fill = NA, colour = "black", linewidth=2),
  #   panel.background = element_blank()
  # )+
  theme(panel.grid=element_blank(),
        panel.background = element_blank())+#去掉灰底和背景网格线+
  theme(legend.position = "none")+
  # theme_bw()+
  theme(axis.text   =element_text(size=18,family="sans"),
        axis.title.x=element_text(size=18,family="sans"),
        axis.title.y=element_text(size=18,family="sans"),
        legend.text =element_text(size=18,family="sans"),
        legend.title=element_text(size=18,family="sans"))


rr7$x <- rep(1:100, 100)
rr7$y <- rep(1:100, each=100)
df_far <- as.data.frame(as.matrix(rr7), xy=TRUE)
df_far$layer <- df_far$fl_2_500_less_aggregated

ggplot(data=df_far, aes(x=x, y=y, fill=layer)) +
  geom_tile() +
  scale_fill_gradient(low="grey", high="darkgreen") +
  coord_equal() +
  # theme(
  #   panel.grid.major = element_blank(),
  #   panel.grid.minor = element_blank(),
  #   panel.border = element_rect(fill = NA, colour = "black", linewidth=2),
  #   panel.background = element_blank()
  # )+
  theme(panel.grid=element_blank(),
        panel.background = element_blank())+#去掉灰底和背景网格线+
  theme(legend.position = "none")+
  # theme_bw()+
  theme(axis.text   =element_text(size=18,family="sans"),
        axis.title.x=element_text(size=18,family="sans"),
        axis.title.y=element_text(size=18,family="sans"),
        legend.text =element_text(size=18,family="sans"),
        legend.title=element_text(size=18,family="sans"))

rr8$x <- rep(1:100, 100)
rr8$y <- rep(1:100, each=100)
df_close <- as.data.frame(as.matrix(rr8), xy=TRUE)
df_close$layer <- df_close$fl_2_500_more_aggregated

ggplot(data=df_close, aes(x=x, y=y, fill=layer)) +
  geom_tile() +
  scale_fill_gradient(low="grey", high="darkgreen") +
  coord_equal() +
  # theme(
  #   panel.grid.major = element_blank(),
  #   panel.grid.minor = element_blank(),
  #   panel.border = element_rect(fill = NA, colour = "black", linewidth=2),
  #   panel.background = element_blank()
  # )+
  theme(panel.grid=element_blank(),
        panel.background = element_blank())+#去掉灰底和背景网格线+
  theme(legend.position = "none")+
  # theme_bw()+
  theme(axis.text   =element_text(size=18,family="sans"),
        axis.title.x=element_text(size=18,family="sans"),
        axis.title.y=element_text(size=18,family="sans"),
        legend.text =element_text(size=18,family="sans"),
        legend.title=element_text(size=18,family="sans"))


rr_Lst <- list(rr1, rr2, rr3, rr4, rr5, rr6, rr7, rr8, rr9)

rr <- rr1

ls_example_frag <- function(rr){

  ed<- lsm_c_ed(rr)$value[2]
  np <- lsm_c_np(rr)$value[2]
  te <- lsm_c_te(rr)$value[2]
  ca <- lsm_c_ca(rr)$value[2]*10000
  mpa <- ca/np
  ldi <- lsm_c_division(rr)$value[2]
  tca <- lsm_c_tca(rr)$value[2]
  lpi <- lsm_c_lpi(rr)$value[2]
  ai <- lsm_c_ai(rr)$value[2]
  par <- te/ca
  pci <- lsm_c_cohesion(rr)$value[2]
  si <- lsm_c_split(rr)$value[2]
  lsi <- lsm_c_lsi(rr)$value[2]
  nlsi <- lsm_c_nlsi(rr)$value[2]
  enn <- lsm_c_enn_mn(rr)$value[2]
  pladj <- lsm_c_pladj(rr)$value[2] 
  parmn <- lsm_c_para_mn(rr)$value[2] 

  # pafrac <- lsm_c_pafrac(rr)$value[2] 
  shapemn <- lsm_c_shape_mn(rr)$value[2] 
  
  df_ls_exa_sub <- as.data.frame(cbind(tca=tca, lpi=lpi, ldi=ldi, ed=ed, np=np, mpa=mpa, parmn=parmn, shapemn=shapemn,
                                       ai=ai, par=par, pci=pci, si=si, lsi=lsi, nlsi=nlsi, enn=enn, pladj=pladj))
  return(df_ls_exa_sub)
}

df_ls_exa <- NULL
for(i in 1:length(rr_Lst)){
  rr <- rr_Lst[[i]]
  df_ls_exa_sub <- ls_example_frag(rr)
  df_ls_exa_sub$ls <- i
  df_ls_exa <- rbind(df_ls_exa, df_ls_exa_sub)
}
df_ls_exa

#---Figure 1F and S2: Compute frag metrics for Actual landscape----
frag_df_full_glad <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/frag_df_Full_glad_extent_5m.rds")

AA <- frag_df_full_glad[(frag_df_full_glad$np_diff< -30)&(frag_df_full_glad$cover_diff<0)&(frag_df_full_glad$mpa_diff>30)&(frag_df_full_glad$ed_diff<0)&(frag_df_full_glad$FFI_diff<0),]

hist(AA$ldi_diff, breaks=100)

BB <- AA[!is.na(AA$longitude)&!is.na(AA$cover_diff)&AA$country=="Brazil",]
# BB <- AA[!is.na(AA$longitude)&!is.na(AA$cover_diff),]


head(BB[,c("longitude","latitude")])

CC <- BB[BB$cover_diff< -0.3,]
# %>%drop_na()
head(CC[,c("longitude","latitude")])
# test <- raster("F:/FRAG/GLAD_2000/forest_frag_raster_glad/FRAG_5degree_2000_GLAD_0_5_.tif")
# plot(test)

DD <- CC %>%
  mutate(
    LON_group = cut(longitude, breaks = seq(-155, 175, by=10),
                    labels = seq(-150, 170, by=10), right = FALSE),
    LAT_group = cut(latitude, breaks = seq(-45, 75, by=10),
                    labels = seq(-40, 70, by=10), right = FALSE)) 

LAT_list <- seq(-40, 70, by=10)
LON_list <- seq(-150, 170, by=10)

DD$LAT_group <- LAT_list[as.numeric(DD$LAT_group)]
DD$LON_group <- LON_list[as.numeric(DD$LON_group)]

class(DD$LAT_group)
class(DD$LON_group)

range(CC$latitude)
range(CC$longitude)


EE <- frag_df_full_glad[(frag_df_full_glad$longitude>=-58.8)&(frag_df_full_glad$longitude<=-58.7)&(frag_df_full_glad$latitude>=-13.05)&(frag_df_full_glad$latitude<=-12.95),]


DD[,c("ai","ai_2020","enn","enn_2020","pladj","pladj_2020","AFI_diff","cover_diff","CFI_diff","longitude","latitude")]

# EE[3,c("country","mpa_diff","np_diff","ed_diff","ai","ai_2020","enn","enn_2020","pladj","pladj_2020","AFI_diff","cover_diff","CFI_diff","longitude","latitude")]

EE <- EE %>%
  mutate(
    LON_group = cut(longitude, breaks = seq(-155, 175, by=10),
                    labels = seq(-150, 170, by=10), right = FALSE),
    LAT_group = cut(latitude, breaks = seq(-45, 75, by=10),
                    labels = seq(-40, 70, by=10), right = FALSE)) 

LAT_list <- seq(-40, 70, by=10)
LON_list <- seq(-150, 170, by=10)

EE$LAT_group <- LAT_list[as.numeric(EE$LAT_group)]
EE$LON_group <- LON_list[as.numeric(EE$LON_group)]

lm1 <- lm(pladj~PLADJ_ST, data=frag_df_full_glad)
summary(lm1)

lm2 <- lm(pladj_2020~PLADJ_2020_ST, data=frag_df_full_glad)
summary(lm2)


EE[5,c("TCA_ST_Diff", "LPI_ST_Diff", "LDI_ST_Diff", "CFI_diff", "AI_ST_Diff", "ENN_ST", "ENN_2020_ST",
      "PLADJ_ST", "PLADJ_2020_ST", "AFI_diff", "NP_ST_Diff", "MPA_ST_Diff", "ED_ST_Diff", "FFI_diff")]
# EE <- DD[,c("longitude","latitude","ffi_diff")]
# 
# FF <- DD[5, ]
# 
# metricSummaryDf <- data.frame(cbind(Variable=c("ai","tca","clumpy",
#                                                "lpi","nlsi","ed","np",
#                                                "ldi", "ffi", "cover", "mpa"),
#                                     Diff=c(FF$ai_diff/FF$ai,FF$tca_diff/FF$tca,
#                                            FF$clumpy_diff/FF$clumpy,FF$lpi_diff/FF$lpi,
#                                            FF$nlsi_diff/FF$nlsi,FF$ed_diff/FF$ed,
#                                            FF$np_diff/FF$np,FF$ldi_diff/FF$ldi,
#                                            FF$ffi_diff/FF$ffi,
#                                            FF$cover_diff/FF$cover,FF$mpa_diff/FF$mpa)))
# 
# metricSummaryDf_Sub <- metricSummaryDf[metricSummaryDf$Variable%in%c("ldi","ffi"),]




i <- 1
LON <- EE$LON_group[i]
LAT <- EE$LAT_group[i]

lon <- EE$longitude[i]
lat <- EE$latitude[i]

# r1_2000 <- raster(paste0("F:/FRAG/GLAD_2000/forest_extent/",abs(LAT),"S_0",abs(LON)+10,"W",".tif"))
# 
# r1_2020 <- raster(paste0("F:/FRAG/GLAD_2020/forest_extent/",abs(LAT),"S_0",abs(LON)+10,"W",".tif"))

r1_2000 <- raster(paste0("D:/FRAG/Forest_Extent/Data_2000/",abs(LAT),"S_0",abs(LON),"W",".tif"))

r1_2020 <- raster(paste0("D:/FRAG/Forest_Extent/Data_2020/",abs(LAT),"S_0",abs(LON),"W",".tif"))


# plot(r1_2000)
# plot(r1_2020)

# tree_2000 <- ((r1_2000 >= 27 & r1_2000 <= 48)|(r1_2000 >= 127 & r1_2000 <= 148))*1
# tree_2020 <- ((r1_2020 >= 27 & r1_2020 <= 48)|(r1_2020 >= 127 & r1_2020 <= 148))*1

Res <- 1/24
new_extent <- extent(lon, lon+Res, lat, lat+Res)

tree_2000_crop <- crop(r1_2000, new_extent)
tree_2020_crop <- crop(r1_2020, new_extent)

tree_2000_df <- as.data.frame(tree_2000_crop, xy=TRUE)
tree_2020_df <- as.data.frame(tree_2020_crop, xy=TRUE)

ggplot(tree_2000_df, aes(x=x, y=y, fill=as.factor(Layer_1)))+
  geom_tile()+
  scale_fill_manual(values=c("grey","darkgreen"))+
  labs(x = "Longitude", y = "Latitude")+
  coord_equal() +
  theme(panel.grid=element_blank(),
        panel.background = element_blank())+#去掉灰底和背景网格线+
  theme(legend.position = "none")+
  # theme_bw()+
  theme(axis.text   =element_text(size=18,family="sans"),
        axis.title.x=element_text(size=18,family="sans"),
        axis.title.y=element_text(size=18,family="sans"),
        legend.text =element_text(size=18,family="sans"),
        legend.title=element_text(size=18,family="sans"))

ggplot(tree_2020_df, aes(x=x, y=y, fill=as.factor(Layer_1)))+
  geom_tile()+
  scale_fill_manual(values=c("grey","darkgreen"))+
  labs(x = "Longitude", y = "Latitude")+
  coord_equal() +
  theme(panel.grid=element_blank(),
        panel.background = element_blank())+#去掉灰底和背景网格线+
  theme(legend.position = "none")+
  # theme_bw()+
  theme(axis.text   =element_text(size=18,family="sans"),
        axis.title.x=element_text(size=18,family="sans"),
        axis.title.y=element_text(size=18,family="sans"),
        legend.text =element_text(size=18,family="sans"),
        legend.title=element_text(size=18,family="sans"))

