rm(list=ls())
library(tidyverse)
library(tictoc)
library(raster)
library(landscapemetrics)
library(sf)
library(rnaturalearth)
library(patchwork)


####----Map frag difference under different cover change scenario: six frag classes----####
frag_df_full_glad <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/frag_df_Full_glad_extent_5m.rds")


frag_df_full_glad <- frag_df_full_glad%>%mutate(ai_diff=ai_2020-ai,
                                                pci_diff=pci_2020-pci,
                                                clumpy_diff=clumpy_2020-clumpy,
                                                si_diff=si_2020-si,
                                                par_diff=par_2020-par)

hist(frag_df_full_glad$ai_diff)
hist(frag_df_full_glad$pci_diff)
hist(frag_df_full_glad$par_diff)

frag_df_full_glad$fragClass6 <- 1
# bath <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/Ocean/ocean_bottom.rds")
# bath <- as.raster(bath)
# plot(bath)

## generate index to represent different fragmentation change scenarios
index_1 <- (!is.na(frag_df_full_glad$longitude))&(!is.na(frag_df_full_glad$cover_diff))&(frag_df_full_glad$cover_diff>=0)
index_2 <- (!is.na(frag_df_full_glad$longitude))&(!is.na(frag_df_full_glad$cover_diff))&(frag_df_full_glad$cover_diff<0)&(frag_df_full_glad$CFI_diff<=0)
index_3 <- (!is.na(frag_df_full_glad$longitude))&(!is.na(frag_df_full_glad$cover_diff))&(frag_df_full_glad$cover_diff<0)&(frag_df_full_glad$CFI_diff>0)
index_4 <- (!is.na(frag_df_full_glad$longitude))&(!is.na(frag_df_full_glad$cover_diff))&(frag_df_full_glad$cover_diff<0)&((frag_df_full_glad$FFI_diff<0)&(frag_df_full_glad$AFI_diff>0))&(frag_df_full_glad$CFI_diff>0)
index_5 <- (!is.na(frag_df_full_glad$longitude))&(!is.na(frag_df_full_glad$cover_diff))&(frag_df_full_glad$cover_diff<0)&((frag_df_full_glad$FFI_diff>0)&(frag_df_full_glad$AFI_diff<0))&(frag_df_full_glad$CFI_diff>0)
index_6 <- (!is.na(frag_df_full_glad$longitude))&(!is.na(frag_df_full_glad$cover_diff))&(frag_df_full_glad$cover_diff<0)&(frag_df_full_glad$FFI_diff<0)&(frag_df_full_glad$AFI_diff<0)&(frag_df_full_glad$CFI_diff>0)


frag_df_full_glad$fragClass6[index_1] <- 1
frag_df_full_glad$fragClass6[index_2] <- 2
frag_df_full_glad$fragClass6[index_3] <- 3
frag_df_full_glad$fragClass6[index_4] <- 4
frag_df_full_glad$fragClass6[index_5] <- 5
frag_df_full_glad$fragClass6[index_6] <- 6

## Function to transform the dataframe of global fragmentation scenarios into a raster
map_frag_diff <- function(var){
  tic()
  
  # FRAG_Df_diff_filter <- frag_df_full_glad
  FRAG_Df_diff_filter <- frag_df_full_glad[frag_df_full_glad$cover>=0.3,]
  
  FRAG_Df_diff_filter$longitude <- FRAG_Df_diff_filter$longitude + 1/48
  FRAG_Df_diff_filter$latitude <- FRAG_Df_diff_filter$latitude + 1/48
  map <- raster(xmn=-180, xmx=180, ymn=-90, ymx=90, resolution=c(1/24,1/24), crs=CRS("+init=epsg:4326"))
  r.map <- rasterize(x=FRAG_Df_diff_filter[, c("longitude", "latitude")], # lon-lat data
                     y=map, # raster object
                     field=FRAG_Df_diff_filter[, var], # vals to fill raster with
                     fun=mean) # aggregate function
  plot(r.map)
  # crs(map_ai) <- CRS("+init=epsg:4326")
  # d.map$Cluster <- raster::extract(fishNet, d.map[,c("longitude", "latitude")])
  
  writeRaster(r.map, filename=paste0("D:/BackUp_PhD_ETH/ETHz_S5/Forest_FRAG/Outcome/2024.4.3/","frag_glad_5km_extent_", var, ".tif"), overwrite=T)
  toc()
}

# map_frag_diff("MPC_ST")
map_frag_diff("fragClass6")

fragClass6 <- raster("D:/BackUp_PhD_ETH/ETHz_S5/Forest_FRAG/Outcome/2024.4.3/frag_glad_5km_extent_fragClass6.tif")

## read the raster file for ocean background of the world map
Ocean_Aggregated <- raster("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/Ocean/Ocean_Aggregated.tif")
# scale_fill_gradient(low = "#21ADE3", high = "#f7fbff", na.value = "transparent", guide = 'none', limits = c(-12000,0)) +

# Project to Equal Earth projection
eqearth_proj <- "+proj=eqearth"


fragclass6_crop <- crop(fragClass6,c(-180, 180, -60, 84))
plot(fragclass6_crop)

bath_crop <- crop(Ocean_Aggregated,c(-180, 180, -60, 84))
plot(bath_crop)
# Project the raster
# projected_fragclass6 <- projectRaster(fragclass6_crop, crs = eqearth_proj)
# 
# # world <- crop(world, c(-180, 180, -60, 84))
# limits <- st_as_sfc(st_bbox(c(xmin = -180, xmax = 180, ymin = -90, ymax = 90), crs = st_crs(world)))
# world_eqearth_clipped <- st_intersection(world, limits)
# 
# 
# 
# world_eqearth <- st_transform(world_clipped, crs = eqearth_proj)
# 
# plot(projected_fragclass6)
# plot(world)

tic()
fragclass6_df <- as.data.frame(fragclass6_crop, xy = TRUE, na.rm = TRUE)

bath_df <- as.data.frame(bath_crop, xy = TRUE, na.rm = TRUE)
toc()

## get the terrestrial country boundaries
world <- ne_countries(scale = "small", returnclass = "sf")


fragclass6_df$frag_glad_5km_extent_fragClass6[fragclass6_df$frag_glad_5km_extent_fragClass6==0] <- 1

table(fragclass6_df$frag)


library(marmap)
# library(ggOceanMapsData)
library(ggOceanMaps)

dt <- expand.grid(lon = c(-180, 180), lat = c(-60, 84))



colnames(fragclass6_df) <- c("longitude","latitude","frag")
# fragclass6_df$fills <- NA
# 
# fragclass6_df$fills[fragclass6_df$frag==1] <- "#1464e1"
# fragclass6_df$fills[fragclass6_df$frag==2] <- "#b68238"
# fragclass6_df$fills[fragclass6_df$frag==3] <- "yellow"
# fragclass6_df$fills[fragclass6_df$frag==4] <- "orange"
# fragclass6_df$fills[fragclass6_df$frag==5] <- "#d70f0a"


# Define the color palette function
blue_palette <- colorRampPalette(c("white", "#2898E0"))

# Generate 10 colors from this palette
colors_ocean <- blue_palette(10)


library(ggnewscale)

Fig3A <- ggplot() + 
  ## ocean background
  geom_raster(data = bath_df, mapping = aes(x = x, y = y, fill = Ocean_Aggregated)) +
  scale_fill_gradient(low = "#21ADE3", high = "#f7fbff", na.value = "transparent", guide = 'none', limits = c(69,157)) +
  
  ## terrestrial background
  geom_sf(data = world, fill = "gray60", color = "gray60") + 
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 84), expand = FALSE) +
  # theme_classic()+
  theme(
    panel.grid.major = element_blank(),  # Removes major grid lines
    panel.grid.minor = element_blank()   # Removes minor grid lines
  )+
  new_scale_fill() +
  
  ## forest fragmentation scenarios
  geom_raster(data = fragclass6_df, aes(x = longitude, y = latitude, fill=as.factor(frag))) + #, fill = fills
  # scale_fill_identity() +
  # scale_fill_viridis_c(name = "FragClass") +
  scale_fill_manual(values=c(
    #colors_ocean,
                             "#1464e1", "purple", "yellow","orange","#b68238", "#d70f0a"))+
  labs(x = "Longitude", y = "Latitude")+
  ylim(c(-60,84))+
  ggtitle("A")+
  # theme(legend.position = "right")+
  theme(
    # axis.title   =element_blank(),
    title=element_text(size=20,family = "sans", face="bold"),
    axis.text   =element_text(size=20,family = "sans", face="bold"),
    axis.title.x=element_text(size=22,family = "sans", face="bold"),
    axis.title.y=element_text(size=22,family = "sans", face="bold"),
    # axis.title.x=element_text(size=16,family = "sans", face="bold"),
    # axis.title.y=element_text(size=16,family = "sans", face="bold"),
    # title=element_blank(),
    legend.position = "none",
    legend.text =element_blank(),
    legend.title=element_blank())


Fig3A

# frag_df_full_glad <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/frag_df_Full_glad_extent_5m.rds")

### latitudinal patterns of changes in three summarised fragmentation index: CFI, AFI and FFI 
se <- function(a){
  a <- a[!is.na(a)]
  return(sd(a)/sqrt(length((a))))
}

FRAG_Df_diff_filter <- frag_df_full_glad[frag_df_full_glad$cover>=0.3,]

data_t <- FRAG_Df_diff_filter %>%
  mutate(group = cut(latitude, breaks = seq(-56, 75, by=1),
                     labels = seq(-55.5, 74.5, by=1), right = FALSE)) %>%
  group_by(group) %>%
  summarize(ldi_mean = mean(ldi_diff, na.rm=T), ldi_sd = sd(ldi_diff, na.rm=T), 
            ldi_se = se(ldi_diff), 
            cfi_mean = mean(CFI_diff, na.rm=T), cfi_sd = sd(CFI_diff, na.rm=T), 
            cfi_se = se(CFI_diff), 
            #ldi_up=CI_up(ldi_diff), ldi_low=CI_low(ldi_diff),
            afi_mean = mean(AFI_diff, na.rm=T), afi_sd = sd(AFI_diff, na.rm=T), 
            afi_se = se(AFI_diff), 
            ffi_mean = mean(FFI_diff, na.rm=T), ffi_sd = sd(FFI_diff, na.rm=T), 
            ffi_se = se(FFI_diff), 
            cover_mean = mean(cover_diff*100, na.rm=T), cover_sd = sd(cover_diff*100, na.rm=T),
            cover_se = se(cover_diff*100)
            # , cover_up=CI_up(cover_diff*100), cover_low=CI_low(cover_diff*100)
  )
# data_t$group <- seq(-50, 80, by=0.5)

class(data_t$group)
data_t$group <- seq(-55.5, 74.5, by=1)[as.numeric(data_t$group)]
#seq(-55.75, 74.75, by=0.5)

data_t_combine <- as.data.frame(cbind(group=rep(data_t$group, 3), mean=c(data_t$cfi_mean, data_t$afi_mean, data_t$ffi_mean),
                                      sd=c(data_t$cfi_sd, data_t$afi_sd, data_t$ffi_sd),
                                      se=c(data_t$cfi_se, data_t$afi_se, data_t$ffi_se),
                                      frag=rep(c("CFI","AFI","FFI"), each=nrow(data_t))))

library(RColorBrewer)
# color1 <- brewer.pal(11,"Spectral")[10]


data_t_combine <- data_t_combine%>%mutate(group=as.numeric(group), mean=as.numeric(mean),
                                          sd=as.numeric(sd), se=as.numeric(se))

data_t_combine$frag <- factor(data_t_combine$frag, levels=c("CFI", "AFI", "FFI"),
                        ordered=T)

Fig3B <- ggplot(data_t_combine, aes(x=group, y=mean, fill=frag)) +
  coord_flip()+
  geom_hline(color="black", yintercept = 0, linewidth=0.5, linetype="dashed")+
  geom_ribbon(aes(ymin = mean-10*se, ymax = mean+10*se), alpha = 0.4) +  # 误差区间
  # geom_ribbon(aes(ymin = cfi_low, ymax = cfi_up), fill = "red", alpha = 0.35) +  # 误差区间
  geom_ribbon(aes(ymin = mean-sd, ymax = mean+sd), alpha = 0.15) +  # 误差区间
  # geom_ribbon(aes(ymin = mean-8*sd, ymax = mean+8*sd), alpha = 0.15) +  # 误差区间
  geom_line(linewidth=0.8,
            # color = "grey20",
            aes(color=frag),
            alpha = 1) +
  # scale_fill_manual(values=c("#ca0020","purple","#0571b0"))+
  # scale_color_manual(values=c("#ca0020","purple","#0571b0"))+
  scale_fill_manual(values=c("#ca0020","orange","#0571b0"))+
  scale_color_manual(values=c("#ca0020","orange","#0571b0"))+
  xlim(c(-60,84))+
  # coord_cartesian(xlim=c(-56,75),ylim = c(-1,1))+
  # scale_x_continuous(breaks = seq(80,150, 20))+
  # scale_y_continuous(breaks = seq(35,65, 10))+
  labs(x = "Latitude", y = "Change in metric")+
  # theme_bw()+
  theme_classic()+
  ggtitle("B")+
  # theme(panel.grid=element_blank(),
  #       panel.background = element_blank())+#去掉灰底和背景网格线+http://127.0.0.1:14741/graphics/plot_zoom_png?width=1552&height=905
  # theme_bw()+
  theme(
    panel.grid.major = element_blank(),  # Removes major grid lines
    panel.grid.minor = element_blank(),  # Removes minor grid lines
    title=element_text(size=20,family = "sans", face="bold"),
    axis.text   =element_text(size=20,family = "sans"),
    axis.title.x=element_text(size=24,family = "sans"),
    axis.title.y=element_text(size=24,family = "sans"),
    legend.text =element_text(size=18,family = "sans", face="bold"),
    legend.position = "right",
    legend.title=element_blank())


library(gridExtra)
MainPlot2 <- grid.arrange(Fig3A, Fig3B, ncol = 2, widths = c(4, 1), heights=c(1,1))
MainPlot2


ggsave("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Results/Week34_2024.11.4_ScienceResponse/Figure3_FRAG_3Metrics_6Class_GlobalMap.pdf", plot=MainPlot2, width=20, height=10)



## Pie chart to reflect the proportion of different scenarios

# frag_df_full_glad$fragClass5[frag_df_full_glad$fragClass5==0] <- 1
frag_df_sub_glad <- frag_df_full_glad[frag_df_full_glad$cover>=0.3,]

# index_0 <- (!is.na(frag_df_sub_glad$longitude))&(!is.na(frag_df_sub_glad$cover_diff))&(frag_df_sub_glad$cover_diff>0)&(!is.na(frag_df_sub_glad$ffi_diff))&(!is.na(frag_df_sub_glad$cfi_diff))
# index_1 <- (!is.na(frag_df_sub_glad$longitude))&(!is.na(frag_df_sub_glad$cover_diff))&(frag_df_sub_glad$cover_diff>=0)
# index_2 <- (!is.na(frag_df_sub_glad$longitude))&(!is.na(frag_df_sub_glad$cover_diff))&(frag_df_sub_glad$cover_diff<0)&(frag_df_sub_glad$CFI_diff<=0)
# index_3 <- (!is.na(frag_df_sub_glad$longitude))&(!is.na(frag_df_sub_glad$cover_diff))&(frag_df_sub_glad$cover_diff<0)&(frag_df_sub_glad$CFI_diff>0)
# index_4 <- (!is.na(frag_df_sub_glad$longitude))&(!is.na(frag_df_sub_glad$cover_diff))&(frag_df_sub_glad$cover_diff<0)&((frag_df_sub_glad$FFI_diff<0)|(frag_df_sub_glad$AFI_diff<0))&(frag_df_sub_glad$CFI_diff>0)
# index_5 <- (!is.na(frag_df_sub_glad$longitude))&(!is.na(frag_df_sub_glad$cover_diff))&(frag_df_sub_glad$cover_diff<0)&(frag_df_sub_glad$FFI_diff<0)&(frag_df_sub_glad$CFI_diff>0)



# A0 <- frag_df_sub_glad[index_0,]
A1 <- frag_df_sub_glad[frag_df_sub_glad$fragClass6==1,]
A2 <- frag_df_sub_glad[frag_df_sub_glad$fragClass6==2,]
A3 <- frag_df_sub_glad[frag_df_sub_glad$fragClass6==3,]
A4 <- frag_df_sub_glad[frag_df_sub_glad$fragClass6==4,]
A5 <- frag_df_sub_glad[frag_df_sub_glad$fragClass6==5,]
A6 <- frag_df_sub_glad[frag_df_sub_glad$fragClass6==6,]

# C0 <- nrow(A0)
# C1 <- nrow(A1)
# C2 <- nrow(A2)-nrow(A3)
# C3 <- nrow(A3)
# C4 <- nrow(A4)

C1 <- sum(A1$pixelArea, na.rm=T)
C2 <- sum(A2$pixelArea, na.rm=T)
C3 <-  sum(A3$pixelArea, na.rm=T)
C4 <- sum(A4$pixelArea, na.rm=T)
C5 <- sum(A5$pixelArea, na.rm=T)
C6 <- sum(A6$pixelArea, na.rm=T)

Cs <- sum(c(C1, C2, C3, C4, C5, C6))

fre_frag_df <- data.frame(cbind(Frequency=c(C1/Cs, C2/Cs, C3/Cs, C4/Cs, C5/Cs, C6/Cs),
                                Category=as.factor(c(1, 2, 3, 4, 5, 6))))
ggplot(fre_frag_df, aes(x = "", y = Frequency, fill = as.factor(Category))) +
  geom_bar(stat = "identity", width = 2, color="white", alpha=0.9) +
  coord_polar(theta = "y") +
  geom_text(color="black", size=15,alpha=1, aes(label = paste0(round(Frequency*100,0), "%")), position = position_stack(vjust=0.5)) + 
  # labs(title = "Pie Chart of Value Counts")+
  #scale_fill_manual(values = c("#1464e1", "#ebc84b", "#d70f0a", "#b68238")) + # "#2bced1", 
  scale_fill_manual(values = c("#1464e1", "purple", "yellow","orange","#b68238", "#d70f0a")) +
  theme_void()+
  theme(legend.position = "none")
