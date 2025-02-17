rm(list=ls())


library(tidyverse)
library(tictoc)
library(raster)
library(patchwork)

packages_list<-list("magrittr", "dplyr",  "MatchIt", "RItools", "this.path", "scales", "ggdendro", "data.table", "openxlsx", 
                    "tibble", "leaps", "pbapply", "RColorBrewer", "ggpubr", "ggdist", "ggh4x") 
lapply(packages_list, library, character.only = TRUE)


# Analysis post-matching ####
data_sample <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/frag_df_SubSet_WDPA_T30perc_buffer40km.rds")
m.nn <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/frag_df_Match_WDPA_T30perc_buffer40km.rds")


# Extracts the matched dataset and calculates the deforestation indicator. 'deforest' is defined as 1 if the forest status changed from present ('Fores_2000' == 1) to absent ('Fores_2021' == 0) over the study period, and 0 otherwise.
y = match.data(m.nn, group="all")


# Map the matched groups
# Retrieve matching pairs from the 'matchit' result to align the treatment (T) and control (C) groups for further analysis.
matches <- data.frame(m.nn$match.matrix)
group1 <- match(row.names(matches), row.names(y))
group2 <- match(matches[, 1], row.names(y))


table(y$WDPA_binary)
hist(y$CFI_diff)

se <- function(a){
  a <- a[!is.na(a)]
  return(sd(a)/sqrt(length((a))))
}

y$WDPA_binary_strict <- NA
y$WDPA_binary_strict[y$WDPA_f%in%c("Ia", "Ib", "II")] <- 2
y$WDPA_binary_strict[y$WDPA_f%in%c("III", "IV", "VI")] <- 1
y$WDPA_binary_strict[y$WDPA_f=="No"] <- 0

## CFI analysis ####
MS_FRAG_Analysis <- function(Biome){
  if(Biome=="Global"){
    y <- y
    data_sample <- data_sample
  }else if(Biome=="Tropical"){
    y <- y[y$Mega_Biome==Biome,]
    data_sample <- data_sample[data_sample$Mega_Biome==Biome,]
  }else if(Biome=="Extratropical"){
    y <- y[y$Mega_Biome%in%c("Mediterranean", "Temperate", "Boreal"),]
    data_sample <- data_sample[data_sample$Mega_Biome%in%c("Mediterranean", "Temperate", "Boreal"),]
  }
  
  y_aggregate_2 <- y[y$WDPA_binary==1&y$WDPA_binary_strict==2,]%>%group_by(WDPA_binary)%>%summarise(CFI_diff_mean=mean(CFI_diff, na.rm=T), 
                                                                                                    CFI_diff_sd=sd(CFI_diff, na.rm=T),
                                                                                                    CFI_diff_se=se(CFI_diff),
                                                                                                    AFI_diff_mean=mean(AFI_diff, na.rm=T), 
                                                                                                    AFI_diff_sd=sd(AFI_diff, na.rm=T),
                                                                                                    AFI_diff_se=se(AFI_diff),
                                                                                                    FFI_diff_mean=mean(FFI_diff, na.rm=T), 
                                                                                                    FFI_diff_sd=sd(FFI_diff, na.rm=T),
                                                                                                    FFI_diff_se=se(FFI_diff),
                                                                                                    Cover_diff_mean=mean(cover_diff, na.rm=T), 
                                                                                                    Cover_diff_sd=sd(cover_diff, na.rm=T),
                                                                                                    Cover_diff_se=se(cover_diff))
  
  y_aggregate_2 <- y_aggregate_2%>%mutate(MS="Strictly Protected")
  
  
  
  y_aggregate_1 <- y[y$WDPA_binary==1&y$WDPA_binary_strict==1,]%>%group_by(WDPA_binary)%>%summarise(CFI_diff_mean=mean(CFI_diff, na.rm=T), 
                                                                            CFI_diff_sd=sd(CFI_diff, na.rm=T),
                                                                            CFI_diff_se=se(CFI_diff),
                                                                            AFI_diff_mean=mean(AFI_diff, na.rm=T), 
                                                                            AFI_diff_sd=sd(AFI_diff, na.rm=T),
                                                                            AFI_diff_se=se(AFI_diff),
                                                                            FFI_diff_mean=mean(FFI_diff, na.rm=T), 
                                                                            FFI_diff_sd=sd(FFI_diff, na.rm=T),
                                                                            FFI_diff_se=se(FFI_diff),
                                                                            Cover_diff_mean=mean(cover_diff, na.rm=T), 
                                                                            Cover_diff_sd=sd(cover_diff, na.rm=T),
                                                                            Cover_diff_se=se(cover_diff))
  
  y_aggregate_1 <- y_aggregate_1%>%mutate(MS="Protected")
  
  y_aggregate_0 <- y[y$WDPA_binary==0,]%>%group_by(WDPA_binary)%>%summarise(CFI_diff_mean=mean(CFI_diff, na.rm=T), 
                                                                            CFI_diff_sd=sd(CFI_diff, na.rm=T),
                                                                            CFI_diff_se=se(CFI_diff),
                                                                            AFI_diff_mean=mean(AFI_diff, na.rm=T), 
                                                                            AFI_diff_sd=sd(AFI_diff, na.rm=T),
                                                                            AFI_diff_se=se(AFI_diff),
                                                                            FFI_diff_mean=mean(FFI_diff, na.rm=T), 
                                                                            FFI_diff_sd=sd(FFI_diff, na.rm=T),
                                                                            FFI_diff_se=se(FFI_diff),
                                                                            Cover_diff_mean=mean(cover_diff, na.rm=T), 
                                                                            Cover_diff_sd=sd(cover_diff, na.rm=T),
                                                                            Cover_diff_se=se(cover_diff))
  y_aggregate_0 <- y_aggregate_0%>%mutate(MS="Matched Outside")
  
  # data_aggregate_1 <- data_sample[data_sample$WDPA_binary==1,]%>%group_by(WDPA_binary)%>%summarise(CFI_diff_mean=mean(CFI_diff, na.rm=T), 
  #                                                                           CFI_diff_sd=sd(CFI_diff, na.rm=T),
  #                                                                           CFI_diff_se=se(CFI_diff),
  #                                                                           Cover_diff_mean=mean(cover_diff, na.rm=T), 
  #                                                                           Cover_diff_sd=sd(cover_diff, na.rm=T),
  #                                                                           Cover_diff_se=se(cover_diff))
  # 
  # data_aggregate_1 <- data_aggregate_1%>%mutate(MS="Unmatched", WDPA="Protected")
  
  data_aggregate_0 <- data_sample[data_sample$WDPA_binary==0,]%>%group_by(WDPA_binary)%>%summarise(CFI_diff_mean=mean(CFI_diff, na.rm=T), 
                                                                                                   CFI_diff_sd=sd(CFI_diff, na.rm=T),
                                                                                                   CFI_diff_se=se(CFI_diff),
                                                                                                   AFI_diff_mean=mean(AFI_diff, na.rm=T), 
                                                                                                   AFI_diff_sd=sd(AFI_diff, na.rm=T),
                                                                                                   AFI_diff_se=se(AFI_diff),
                                                                                                   FFI_diff_mean=mean(FFI_diff, na.rm=T), 
                                                                                                   FFI_diff_sd=sd(FFI_diff, na.rm=T),
                                                                                                   FFI_diff_se=se(FFI_diff),
                                                                                                   Cover_diff_mean=mean(cover_diff, na.rm=T), 
                                                                                                   Cover_diff_sd=sd(cover_diff, na.rm=T),
                                                                                                   Cover_diff_se=se(cover_diff))
  data_aggregate_0 <- data_aggregate_0%>%mutate(MS="All Outside")
  
  frag_df_aggregate <- rbind(y_aggregate_2, y_aggregate_1, y_aggregate_0, data_aggregate_0)
  
  frag_df_aggregate$MS <- factor(frag_df_aggregate$MS, 
                                 levels=c("Strictly Protected", "Protected", "Matched Outside", "All Outside"),
                                 ordered=T)
  frag_df_aggregate$Biome <- Biome
  return(frag_df_aggregate)
}


frag_df_aggregate_Global <- MS_FRAG_Analysis("Global")

Fig1A <- ggplot(frag_df_aggregate_Global, aes(x=MS, y=CFI_diff_mean)) +
  geom_bar(position="stack", stat="identity", alpha=0.4, color="black", width=0.5)+
  geom_errorbar(aes(ymin = CFI_diff_mean  -3*CFI_diff_se , ymax = CFI_diff_mean +3*CFI_diff_se), width=0.1,linewidth=1,) +  # 误差区间
  # ylim(c(0,0.04))+
  # geom_hline(yintercept=0, color="black", linewidth=1, linetype="dashed")+
  labs(x = "WDPA Category", y = "Change in CFI")+
  # coord_cartesian(ylim=c(0.0019,0.04))+
  scale_y_continuous(expand=c(0,0), limits = c(-0.005, 0.03))+
  # scale_color_manual(values=c("#ca0020","#0571b0"))+
  # scale_fill_manual(values=c("#ca0020","#0571b0"))+
  theme_classic()+
  ggtitle("Global")+
  theme(title = element_text(size=18,family = "sans", face="bold"),
        axis.text   =element_text(size=18,family="sans", face="bold"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=18,family="sans", face="bold"),
        legend.text =element_text(size=18,family="sans", face="bold"),
        # legend.position = "none",
        legend.title=element_text(size=18,family="sans", face="bold"))

frag_df_aggregate_Tropical <- MS_FRAG_Analysis("Tropical")
frag_df_aggregate_Tropical <- frag_df_aggregate_Tropical%>%drop_na()

frag_df_aggregate_Tropical_3D <- data.frame(frag_mean=c(frag_df_aggregate_Tropical$CFI_diff_mean,
                                                        frag_df_aggregate_Tropical$AFI_diff_mean,
                                                        frag_df_aggregate_Tropical$FFI_diff_mean),
                                                    frag_se=c(frag_df_aggregate_Tropical$CFI_diff_se,
                                                              frag_df_aggregate_Tropical$AFI_diff_se,
                                                              frag_df_aggregate_Tropical$FFI_diff_se),
                                                    frag_std=c(frag_df_aggregate_Tropical$CFI_diff_sd,
                                                               frag_df_aggregate_Tropical$AFI_diff_sd,
                                                               frag_df_aggregate_Tropical$FFI_diff_sd),
                                                    MS=rep(frag_df_aggregate_Tropical$MS,3),
                                                    frag=rep(c("CFI","AFI","FFI"),each=4))


frag_df_aggregate_Tropical_3D$MS <- factor(frag_df_aggregate_Tropical_3D$MS, 
                                                     levels=c("Strictly Protected", "Protected", 
                                                              "Matched Outside", "All Outside"),
                                                     ordered=T)
frag_df_aggregate_Tropical_3D$frag <- factor(frag_df_aggregate_Tropical_3D$frag, 
                                           levels=c("CFI","AFI","FFI"),
                                           ordered=T)


ggplot(frag_df_aggregate_Tropical_3D, aes(x=MS, y=frag_mean, color=frag, fill=frag)) +
  geom_point(size=3.5, alpha = 0.7) +
  # ylim(c(-0.052,0.031))+
  geom_hline(yintercept=0, color="black", linewidth=1, linetype="dashed")+
  geom_errorbar(aes(ymin = frag_mean-3*frag_se, ymax = frag_mean+3*frag_se), width=0.02) +  # 误差区间
  labs(x = "WDPA Category", y = "Change in Metric")+
  scale_color_manual(values=c("#ca0020","orange","#0571b0"))+
  scale_fill_manual(values=c("#ca0020","orange","#0571b0"))+
  theme_classic()+
  theme(axis.text   =element_text(size=16,family="sans"),
        axis.title.x=element_text(size=20,family="sans"),
        axis.title.y=element_text(size=20,family="sans"),
        legend.text =element_text(size=18,family="sans"),
        # legend.position = "none",
        legend.title=element_blank())


Fig1B <- ggplot(frag_df_aggregate_Tropical, aes(x=MS, y=CFI_diff_mean)) +
  geom_bar(position="stack", stat="identity", alpha=0.4, color="black", width=0.5)+
  geom_errorbar(aes(ymin = CFI_diff_mean  -3*CFI_diff_se , ymax = CFI_diff_mean +3*CFI_diff_se), width=0.1,linewidth=1,) +  # 误差区间
  # ylim(c(0,0.04))+
  # geom_hline(yintercept=0, color="black", linewidth=1, linetype="dashed")+
  labs(x = "WDPA Category", y = "Change in CFI")+
  # coord_cartesian(ylim=c(0.0019,0.04))+
  scale_y_continuous(expand=c(0,0), limits = c(0, 0.03))+
  # scale_color_manual(values=c("#ca0020","#0571b0"))+
  # scale_fill_manual(values=c("#ca0020","#0571b0"))+
  theme_classic()+
  ggtitle("Tropical")+
  theme(title = element_text(size=18,family = "sans", face="bold"),
        axis.text   =element_text(size=18,family="sans", face="bold"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=18,family="sans", face="bold"),
        legend.text =element_text(size=18,family="sans", face="bold"),
        # legend.position = "none",
        legend.title=element_text(size=18,family="sans", face="bold"))

frag_df_aggregate_Temperate <- MS_FRAG_Analysis("Extratropical")

Fig1C <- ggplot(frag_df_aggregate_Temperate, aes(x=MS, y=CFI_diff_mean)) +
  geom_bar(position="stack", stat="identity", alpha=0.4, color="black", width=0.5)+
  geom_errorbar(aes(ymin = CFI_diff_mean  -CFI_diff_se , ymax = CFI_diff_mean +CFI_diff_se), width=0.1,linewidth=1,) +  # 误差区间
  # ylim(c(0,0.04))+
  # geom_hline(yintercept=0, color="black", linewidth=1, linetype="dashed")+
  labs(x = "WDPA Category", y = "Change in CFI")+
  # coord_cartesian(ylim=c(0.0019,0.04))+
  scale_y_continuous(expand=c(0,0), limits = c(0, 0.03))+
  # scale_color_manual(values=c("#ca0020","#0571b0"))+
  # scale_fill_manual(values=c("#ca0020","#0571b0"))+
  theme_classic()+
  ggtitle("Extratropical")+
  theme(title = element_text(size=18,family = "sans", face="bold"),
        axis.text   =element_text(size=18,family="sans", face="bold"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=18,family="sans", face="bold"),
        legend.text =element_text(size=18,family="sans", face="bold"),
        # legend.position = "none",
        legend.title=element_text(size=18,family="sans", face="bold"))


layout <- "
AB
"


## Anova analysis ####
# y_aggregate_2 <- y[y$WDPA_binary==1&y$WDPA_binary_strict==2,]
# y_aggregate_2 <- y_aggregate_2%>%mutate(MS="Strictly Protected")
# y_aggregate_2 <- y_aggregate_2[,c("MS", "CFI_diff", "Mega_Biome")]
# 
# y_aggregate_1 <- y[y$WDPA_binary==1&y$WDPA_binary_strict==1,]
# y_aggregate_1 <- y_aggregate_1%>%mutate(MS="Protected")
# y_aggregate_1 <- y_aggregate_1[,c("MS", "CFI_diff", "Mega_Biome")]
# 
# y_aggregate_0 <- y[y$WDPA_binary==0,]
# y_aggregate_0 <- y_aggregate_0%>%mutate(MS="Matched Outside")
# y_aggregate_0 <- y_aggregate_0[,c("MS", "CFI_diff", "Mega_Biome")]
# 
# data_aggregate_0 <- data_sample[data_sample$WDPA_binary==0,]
# data_aggregate_0 <- data_aggregate_0%>%mutate(MS="All Outside")
# data_aggregate_0 <- data_aggregate_0[,c("MS", "CFI_diff", "Mega_Biome")]
# 
# frag_df_aggregate <- rbind(y_aggregate_2, y_aggregate_1, y_aggregate_0, data_aggregate_0)
# 
# 
# frag_df_tropical <- frag_df_aggregate[frag_df_aggregate$Mega_Biome=="Tropical",]%>%drop_na()
# # Perform ANOVA
# model_T <- aov(CFI_diff ~ MS, data = frag_df_tropical)
# summary(model_T)
# 
# frag_df_extratropical <- frag_df_aggregate[frag_df_aggregate$Mega_Biome!="Tropical",]%>%drop_na()
# # Perform ANOVA
# model_ET <- aov(CFI_diff ~ MS, data = frag_df_extratropical)
# summary(model_ET)
# 
# t.test(frag_df_extratropical[frag_df_extratropical$MS=="Strictly Protected", ]$CFI_diff, frag_df_extratropical[frag_df_extratropical$MS=="Protected", ]$CFI_diff)
# t.test(frag_df_extratropical[frag_df_extratropical$MS=="Strictly Protected", ]$CFI_diff, frag_df_extratropical[frag_df_extratropical$MS=="Matched Outside", ]$CFI_diff)




#Merge plots
# MainPlot = Fig1B + Fig1C + 
#   plot_layout(design = layout) + plot_annotation(tag_levels = 'A')&
#   theme(plot.tag = element_text(face = 'bold'))
# 
# MainPlot

# ggsave("C:/Zeus/ETHz/ETHz_S7/ProtectArea_Evaluation/Results/Week1_20240708/Figure5a_MatchedFRAG_Science_buffer10km.pdf", plot=MainPlot, width=20, height=9)

# Protected Area Driver Analysis 2023 ####
# data_sample <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/frag_df_SubSet_WDPA_T30perc_buffer10km.rds")
# m.nn <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/frag_df_Match_WDPA_T30perc_buffer10km.rds")
# 
# 
# # Extracts the matched dataset and calculates the deforestation indicator. 'deforest' is defined as 1 if the forest status changed from present ('Fores_2000' == 1) to absent ('Fores_2021' == 0) over the study period, and 0 otherwise.
# y = match.data(m.nn, group="all")


# Map the matched groups
# Retrieve matching pairs from the 'matchit' result to align the treatment (T) and control (C) groups for further analysis.
# matches <- data.frame(m.nn$match.matrix)
# group1 <- match(row.names(matches), row.names(y))
# group2 <- match(matches[, 1], row.names(y))


table(y$WDPA_binary)
hist(y$CFI_diff)

se <- function(a){
  a <- a[!is.na(a)]
  return(sd(a)/sqrt(length((a))))
}

y$WDPA_binary_strict <- NA
y$WDPA_binary_strict[y$WDPA_f%in%c("Ia", "Ib", "II")] <- 2
y$WDPA_binary_strict[y$WDPA_f%in%c("III", "IV", "VI")] <- 1
y$WDPA_binary_strict[y$WDPA_f=="No"] <- 0


data_sample$WDPA_binary_strict <- NA
data_sample$WDPA_binary_strict[data_sample$WDPA_f%in%c("Ia", "Ib", "II")] <- 2
data_sample$WDPA_binary_strict[data_sample$WDPA_f%in%c("III", "IV", "VI")] <- 1
data_sample$WDPA_binary_strict[data_sample$WDPA_f=="No"] <- 0

data_sample_select <- data_sample%>%dplyr::select(longitude, latitude, 
                                                  CFI_diff, WDPA_binary, WDPA_binary_strict,
                                                  cover, Driver_2023, Mega_Biome)

y_select <- y%>%dplyr::select(longitude, latitude, 
                              CFI_diff, WDPA_binary, WDPA_binary_strict,
                              cover, Driver_2023, Mega_Biome)

data_sample_select_0 <- data_sample_select[data_sample_select$WDPA_binary==0,]

data_sample_select_0$MS <- "All Outside"

y_select$MS[y_select$WDPA_binary==1&y_select$WDPA_binary_strict==2] <- "Strictly Protected"
y_select$MS[y_select$WDPA_binary==1&y_select$WDPA_binary_strict==1] <- "Protected"  
y_select$MS[y_select$WDPA_binary_strict==0] <- "Matched Outside"


df_MS_Driver <- rbind(y_select, data_sample_select_0)
df_MS_Driver$Driver <- df_MS_Driver$Driver_2023


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



df_MS_Driver$Mega_Biome[df_MS_Driver$Mega_Biome=="Mediterranean"]<-"Temperate"

df_MS_Driver$driverName <- NA
# df_MS_Driver$driverName[df_MS_Driver$Driver==0] <- "Zero or Minor Cover Loss"
df_MS_Driver$driverName[df_MS_Driver$Driver==1] <- "Deforestation"
df_MS_Driver$driverName[df_MS_Driver$Driver==2] <- "Shifting Agriculture"
df_MS_Driver$driverName[df_MS_Driver$Driver==3] <- "Forestry"
df_MS_Driver$driverName[df_MS_Driver$Driver==4] <- "Wildfire"
df_MS_Driver$driverName[df_MS_Driver$Driver==5] <- "Urbanization"

df_MS_Driver$driverName[df_MS_Driver$CFI_diff<=0] <- "No CFI Increase"
df_MS_Driver$driverName[is.na((df_MS_Driver$driverName))] <- "Zero or Minor Cover Loss"

table(df_MS_Driver$driverName)

AB <- pixel_area(df_MS_Driver$latitude, 1/24, 1/24)

df_MS_Driver <- df_MS_Driver%>%ungroup()%>%mutate(pixelArea=AB/10^6)

df_MS_Driver <- df_MS_Driver%>%mutate(RegionArea=pixelArea)

table(df_MS_Driver$Mega_Biome)

# Tropical = 1
MS_Driver_Analysis <- function(Tropical){
  if(Tropical==1){
    df_sub_MS_Driver <- df_MS_Driver[df_MS_Driver$Mega_Biome=="Tropical",]
  }else{
    df_sub_MS_Driver <- df_MS_Driver[df_MS_Driver$Mega_Biome!="Tropical",]
  }
  
  df_sub_MS_Driver_aggregate <- df_sub_MS_Driver%>%
    group_by(driverName, MS)%>%
    summarise(RegionArea=sum(RegionArea, na.rm=T))%>%drop_na()
  
  Area_3 <- sum(df_sub_MS_Driver_aggregate$RegionArea[df_sub_MS_Driver_aggregate$MS=="Strictly Protected"], na.rm=T)
  Area_2 <- sum(df_sub_MS_Driver_aggregate$RegionArea[df_sub_MS_Driver_aggregate$MS=="Protected"], na.rm=T)
  Area_1 <- sum(df_sub_MS_Driver_aggregate$RegionArea[df_sub_MS_Driver_aggregate$MS=="Matched Outside"], na.rm=T)
  Area_0 <- sum(df_sub_MS_Driver_aggregate$RegionArea[df_sub_MS_Driver_aggregate$MS=="All Outside"], na.rm=T)
  
  df_sub_MS_Driver_aggregate$MS <- factor(df_sub_MS_Driver_aggregate$MS, 
                                          levels=c("Strictly Protected","Protected","Matched Outside","All Outside"),
                                          ordered=T)
  
  df_sub_MS_Driver_aggregate$RegionArea_Perc <- NA
  df_sub_MS_Driver_aggregate$RegionArea_Perc[df_sub_MS_Driver_aggregate$MS=="Strictly Protected"] <- df_sub_MS_Driver_aggregate$RegionArea[df_sub_MS_Driver_aggregate$MS=="Strictly Protected"]/Area_3
  df_sub_MS_Driver_aggregate$RegionArea_Perc[df_sub_MS_Driver_aggregate$MS=="Protected"] <- df_sub_MS_Driver_aggregate$RegionArea[df_sub_MS_Driver_aggregate$MS=="Protected"]/Area_2
  df_sub_MS_Driver_aggregate$RegionArea_Perc[df_sub_MS_Driver_aggregate$MS=="Matched Outside"] <- df_sub_MS_Driver_aggregate$RegionArea[df_sub_MS_Driver_aggregate$MS=="Matched Outside"]/Area_1
  df_sub_MS_Driver_aggregate$RegionArea_Perc[df_sub_MS_Driver_aggregate$MS=="All Outside"] <- df_sub_MS_Driver_aggregate$RegionArea[df_sub_MS_Driver_aggregate$MS=="All Outside"]/Area_0
  
  
  df_sub_MS_Driver_aggregate$driverName <- factor(df_sub_MS_Driver_aggregate$driverName, 
                                                  levels=c("No CFI Increase", "Zero or Minor Cover Loss", 
                                                           "Wildfire", "Forestry", "Shifting Agriculture",  
                                                           "Deforestation", "Urbanization"
                                                  ), 
                                                  ordered=T)
  
  return(df_sub_MS_Driver_aggregate)
}

df_MS_Driver_Tropical <- MS_Driver_Analysis(1)
df_MS_Driver_ExtraTropical <- MS_Driver_Analysis(0)


Fig2A <- ggplot(df_MS_Driver_Tropical, aes(fill=driverName, y=RegionArea_Perc, x=MS)) +
  geom_bar(position="stack", stat="identity", alpha=1, color="black")+
  scale_fill_manual(values=c("grey90", "grey75", "#d73027", "#fc8d59","#fee090", "#91bfdb", "#4575b4"))+
  # scale_fill_manual(values=c("grey90", "grey60", "grey30", "#d73027", "#fc8d59","#fee090", "#91bfdb", "#4575b4"))+
  # scale_fill_viridis_d(direction=-1)+
  ggtitle("Tropical")+
  xlab("Protected")+
  ylab("Forests with Increased Fragmentation (%)")+
  scale_y_continuous(expand=c(0,0), limits = c(0,1.001))+
  theme_classic()+
  theme(title = element_text(size=18,family = "sans", face="bold"),
        # axis.text.x=element_text(size=12,family = "sans", face="bold", angle=60, vjust = 1, hjust=1),
        axis.text.x=element_text(size=18,family = "sans", face="bold", angle=0),
        axis.text.y=element_text(size=18,family = "sans", face="bold"),
        axis.title.y=element_text(size=18,family = "sans", face="bold"),
        legend.position = "none",
        axis.title.x=element_blank(),
        # legend.position = "none",
        legend.text = element_text(colour = "black", size = 12,family = "sans", face="bold"),
        legend.title = element_blank())


Fig2B <-  ggplot(df_MS_Driver_ExtraTropical, aes(fill=driverName, y=RegionArea_Perc, x=MS)) +
  geom_bar(position="stack", stat="identity", alpha=1, color="black")+
  scale_fill_manual(values=c("grey90", "grey75", "#d73027", "#fc8d59","#fee090", "#91bfdb", "#4575b4"))+
  # scale_fill_viridis_d(direction=-1)+
  ggtitle("Extratropical")+
  xlab("Protected")+
  ylab("Forests with Increased Fragmentation (%)")+
  scale_y_continuous(expand=c(0,0), limits = c(0,1.001))+
  theme_classic()+
  theme(title = element_text(size=18,family = "sans", face="bold"),
        # axis.text.x=element_text(size=12,family = "sans", face="bold", angle=60, vjust = 1, hjust=1),
        axis.text.x=element_text(size=18,family = "sans", face="bold", angle=0),
        axis.text.y=element_text(size=18,family = "sans", face="bold"),
        axis.title.y=element_text(size=18,family = "sans", face="bold"),
        # legend.position = "none",
        axis.title.x=element_blank(),
        # legend.position = "none",
        legend.text = element_text(colour = "black", size = 12,family = "sans", face="bold"),
        legend.title = element_blank())


layout <- "
AB
CD
"

MainPlot = Fig1B + Fig2A + Fig1C + Fig2B + 
  plot_layout(design = layout) + plot_annotation(tag_levels = 'A')&
  theme(plot.tag = element_text(face = 'bold'))

MainPlot

ggsave("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Results/Week34_2024.11.4_ScienceResponse/Figure5_MatchedFRAG_Science_buffer40km.pdf", plot=MainPlot, width=20, height=18)


# Protected Area Driver Analysis 2015 ####
# data_sample <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/frag_df_SubSet_WDPA_T30perc_buffer10km.rds")
# m.nn <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/frag_df_Match_WDPA_T30perc_buffer10km.rds")
# 
# 
# # Extracts the matched dataset and calculates the deforestation indicator. 'deforest' is defined as 1 if the forest status changed from present ('Fores_2000' == 1) to absent ('Fores_2021' == 0) over the study period, and 0 otherwise.
# y = match.data(m.nn, group="all")


# Map the matched groups
# Retrieve matching pairs from the 'matchit' result to align the treatment (T) and control (C) groups for further analysis.
# matches <- data.frame(m.nn$match.matrix)
# group1 <- match(row.names(matches), row.names(y))
# group2 <- match(matches[, 1], row.names(y))


table(y$WDPA_binary)
hist(y$CFI_diff)

se <- function(a){
  a <- a[!is.na(a)]
  return(sd(a)/sqrt(length((a))))
}

y$WDPA_binary_strict <- NA
y$WDPA_binary_strict[y$WDPA_f%in%c("Ia", "Ib", "II")] <- 2
y$WDPA_binary_strict[y$WDPA_f%in%c("III", "IV", "VI")] <- 1
y$WDPA_binary_strict[y$WDPA_f=="No"] <- 0


data_sample$WDPA_binary_strict <- NA
data_sample$WDPA_binary_strict[data_sample$WDPA_f%in%c("Ia", "Ib", "II")] <- 2
data_sample$WDPA_binary_strict[data_sample$WDPA_f%in%c("III", "IV", "VI")] <- 1
data_sample$WDPA_binary_strict[data_sample$WDPA_f=="No"] <- 0

data_sample_select <- data_sample%>%dplyr::select(longitude, latitude, 
                                                  CFI_diff, WDPA_binary, WDPA_binary_strict,
                                                  cover, Driver, Mega_Biome)

y_select <- y%>%dplyr::select(longitude, latitude, 
                              CFI_diff, WDPA_binary, WDPA_binary_strict,
                              cover, Driver, Mega_Biome)

data_sample_select_0 <- data_sample_select[data_sample_select$WDPA_binary==0,]

data_sample_select_0$MS <- "All Outside"

y_select$MS[y_select$WDPA_binary==1&y_select$WDPA_binary_strict==2] <- "Strictly Protected"
y_select$MS[y_select$WDPA_binary==1&y_select$WDPA_binary_strict==1] <- "Protected"  
y_select$MS[y_select$WDPA_binary_strict==0] <- "Matched Outside"


df_MS_Driver <- rbind(y_select, data_sample_select_0)

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



df_MS_Driver$Mega_Biome[df_MS_Driver$Mega_Biome=="Mediterranean"]<-"Temperate"

df_MS_Driver$driverName <- NA
# df_MS_Driver$driverName[df_MS_Driver$Driver==0] <- "Zero or Minor Cover Loss"
df_MS_Driver$driverName[df_MS_Driver$Driver==1] <- "Deforestation"
df_MS_Driver$driverName[df_MS_Driver$Driver==2] <- "Shifting Agriculture"
df_MS_Driver$driverName[df_MS_Driver$Driver==3] <- "Forestry"
df_MS_Driver$driverName[df_MS_Driver$Driver==4] <- "Wildfire"
df_MS_Driver$driverName[df_MS_Driver$Driver==5] <- "Urbanization"

df_MS_Driver$driverName[df_MS_Driver$CFI_diff<=0] <- "No CFI Increase"
df_MS_Driver$driverName[is.na((df_MS_Driver$driverName))] <- "Zero or Minor Cover Loss"

table(df_MS_Driver$driverName)

AB <- pixel_area(df_MS_Driver$latitude, 1/24, 1/24)

df_MS_Driver <- df_MS_Driver%>%ungroup()%>%mutate(pixelArea=AB/10^6)

df_MS_Driver <- df_MS_Driver%>%mutate(RegionArea=pixelArea)

table(df_MS_Driver$Mega_Biome)

# Tropical = 1
MS_Driver_Analysis <- function(Tropical){
  if(Tropical==1){
    df_sub_MS_Driver <- df_MS_Driver[df_MS_Driver$Mega_Biome=="Tropical",]
  }else{
    df_sub_MS_Driver <- df_MS_Driver[df_MS_Driver$Mega_Biome!="Tropical",]
  }
  
  df_sub_MS_Driver_aggregate <- df_sub_MS_Driver%>%
    group_by(driverName, MS)%>%
    summarise(RegionArea=sum(RegionArea, na.rm=T))%>%drop_na()
  
  Area_3 <- sum(df_sub_MS_Driver_aggregate$RegionArea[df_sub_MS_Driver_aggregate$MS=="Strictly Protected"], na.rm=T)
  Area_2 <- sum(df_sub_MS_Driver_aggregate$RegionArea[df_sub_MS_Driver_aggregate$MS=="Protected"], na.rm=T)
  Area_1 <- sum(df_sub_MS_Driver_aggregate$RegionArea[df_sub_MS_Driver_aggregate$MS=="Matched Outside"], na.rm=T)
  Area_0 <- sum(df_sub_MS_Driver_aggregate$RegionArea[df_sub_MS_Driver_aggregate$MS=="All Outside"], na.rm=T)
  
  df_sub_MS_Driver_aggregate$MS <- factor(df_sub_MS_Driver_aggregate$MS, 
                                          levels=c("Strictly Protected","Protected","Matched Outside","All Outside"),
                                          ordered=T)
  
  df_sub_MS_Driver_aggregate$RegionArea_Perc <- NA
  df_sub_MS_Driver_aggregate$RegionArea_Perc[df_sub_MS_Driver_aggregate$MS=="Strictly Protected"] <- df_sub_MS_Driver_aggregate$RegionArea[df_sub_MS_Driver_aggregate$MS=="Strictly Protected"]/Area_3
  df_sub_MS_Driver_aggregate$RegionArea_Perc[df_sub_MS_Driver_aggregate$MS=="Protected"] <- df_sub_MS_Driver_aggregate$RegionArea[df_sub_MS_Driver_aggregate$MS=="Protected"]/Area_2
  df_sub_MS_Driver_aggregate$RegionArea_Perc[df_sub_MS_Driver_aggregate$MS=="Matched Outside"] <- df_sub_MS_Driver_aggregate$RegionArea[df_sub_MS_Driver_aggregate$MS=="Matched Outside"]/Area_1
  df_sub_MS_Driver_aggregate$RegionArea_Perc[df_sub_MS_Driver_aggregate$MS=="All Outside"] <- df_sub_MS_Driver_aggregate$RegionArea[df_sub_MS_Driver_aggregate$MS=="All Outside"]/Area_0
  
  
  df_sub_MS_Driver_aggregate$driverName <- factor(df_sub_MS_Driver_aggregate$driverName, 
                                                  levels=c("No CFI Increase", "Zero or Minor Cover Loss", 
                                                           "Wildfire", "Forestry", "Shifting Agriculture",  
                                                           "Deforestation", "Urbanization"
                                                  ), 
                                                  ordered=T)
  
  return(df_sub_MS_Driver_aggregate)
}

df_MS_Driver_Tropical <- MS_Driver_Analysis(1)
df_MS_Driver_ExtraTropical <- MS_Driver_Analysis(0)


Fig3A <- ggplot(df_MS_Driver_Tropical, aes(fill=driverName, y=RegionArea_Perc, x=MS)) +
  geom_bar(position="stack", stat="identity", alpha=1, color="black")+
  scale_fill_manual(values=c("grey90", "grey75", "#d73027", "#fc8d59","#fee090", "#91bfdb", "#4575b4"))+
  # scale_fill_manual(values=c("grey90", "grey60", "grey30", "#d73027", "#fc8d59","#fee090", "#91bfdb", "#4575b4"))+
  # scale_fill_viridis_d(direction=-1)+
  ggtitle("Tropical")+
  xlab("Protected")+
  ylab("Forests with Increased Fragmentation (%)")+
  scale_y_continuous(expand=c(0,0), limits = c(0,1.001))+
  theme_classic()+
  theme(title = element_text(size=18,family = "sans", face="bold"),
        # axis.text.x=element_text(size=12,family = "sans", face="bold", angle=60, vjust = 1, hjust=1),
        axis.text.x=element_text(size=18,family = "sans", face="bold", angle=0),
        axis.text.y=element_text(size=18,family = "sans", face="bold"),
        axis.title.y=element_text(size=18,family = "sans", face="bold"),
        legend.position = "none",
        axis.title.x=element_blank(),
        # legend.position = "none",
        legend.text = element_text(colour = "black", size = 12,family = "sans", face="bold"),
        legend.title = element_blank())


Fig3B <-  ggplot(df_MS_Driver_ExtraTropical, aes(fill=driverName, y=RegionArea_Perc, x=MS)) +
  geom_bar(position="stack", stat="identity", alpha=1, color="black")+
  scale_fill_manual(values=c("grey90", "grey75", "#d73027", "#fc8d59","#fee090", "#91bfdb", "#4575b4"))+
  # scale_fill_viridis_d(direction=-1)+
  ggtitle("Extratropical")+
  xlab("Protected")+
  ylab("Forests with Increased Fragmentation (%)")+
  scale_y_continuous(expand=c(0,0), limits = c(0,1.001))+
  theme_classic()+
  theme(title = element_text(size=18,family = "sans", face="bold"),
        # axis.text.x=element_text(size=12,family = "sans", face="bold", angle=60, vjust = 1, hjust=1),
        axis.text.x=element_text(size=18,family = "sans", face="bold", angle=0),
        axis.text.y=element_text(size=18,family = "sans", face="bold"),
        axis.title.y=element_text(size=18,family = "sans", face="bold"),
        # legend.position = "none",
        axis.title.x=element_blank(),
        # legend.position = "none",
        legend.text = element_text(colour = "black", size = 12,family = "sans", face="bold"),
        legend.title = element_blank())

layout <- "
AB
"

MainPlot = Fig3A + Fig3B +
  plot_layout(design = layout) + plot_annotation(tag_levels = 'A')&
  theme(plot.tag = element_text(face = 'bold'))

MainPlot

ggsave("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Results/Week34_2024.11.4_ScienceResponse/FigureSX_MatchedFRAG_Driver2015_Science_buffer40km.pdf", plot=MainPlot, width=20, height=9)



