rm(list=ls())

library(rvest)
library(tidyverse)
library(tictoc)
library(raster)
library(gridExtra)



### Figure 4A ####
frag_df_full_glad <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/frag_df_Full_glad_extent_5m.rds")
frag_df_full_glad$Mega_Biome[frag_df_full_glad$Mega_Biome=="Mediterranean"]<-"Temperate"
frag_df_full_glad$count <- 1

frag_df_full_glad_Area <- frag_df_full_glad[frag_df_full_glad$cover>=0.3,]%>%
  dplyr::select(cover, Mega_Biome, count, pixelArea)%>%drop_na()%>%group_by(Mega_Biome)%>%
  summarise(ForestArea=sum(cover*pixelArea),
            RegionArea=sum(pixelArea))


frag_df_full_glad$driverName <- NA
frag_df_full_glad$driverName[frag_df_full_glad$Driver==0] <- "Zero or Minor Cover Loss"
frag_df_full_glad$driverName[frag_df_full_glad$Driver==1] <- "Deforestation"
frag_df_full_glad$driverName[frag_df_full_glad$Driver==2] <- "Shifting Agriculture"
frag_df_full_glad$driverName[frag_df_full_glad$Driver==3] <- "Forestry"
frag_df_full_glad$driverName[frag_df_full_glad$Driver==4] <- "Wildfire"
frag_df_full_glad$driverName[frag_df_full_glad$Driver==5] <- "Urbanization"

frag_df_full_glad_filtered <- frag_df_full_glad[frag_df_full_glad$cover>=0.3,]
frag_df_full_glad_select <- frag_df_full_glad_filtered%>%dplyr::select(CFI_diff, FFI_diff, pixelArea,
                                                                       Driver, driverName, Mega_Biome, cover, count)

### global
frag_df_full_glad_CFI <- frag_df_full_glad_select

frag_df_full_glad_CFI$driverName[frag_df_full_glad_CFI$CFI_diff<=0] <- "No CFI Increase"
frag_df_full_glad_CFI$driverName[is.na((frag_df_full_glad_CFI$driverName))] <- "Uncertain"

frag_df_full_glad_CFI <- frag_df_full_glad_CFI%>%drop_na()

table(frag_df_full_glad_CFI$driverName)

frag_df_full_glad_CFI <- frag_df_full_glad_CFI%>%mutate(area=cover*pixelArea, RegionArea=count*pixelArea)

frag_df_full_glad_CFI_aggregate_global <- frag_df_full_glad_CFI%>%
  group_by(driverName)%>%
  summarise(ForestArea=sum(area),
            RegionArea=sum(RegionArea))
frag_df_full_glad_CFI_aggregate_global$Mega_Biome <-"Global"

frag_df_full_glad_CFI_aggregate_global <- frag_df_full_glad_CFI_aggregate_global[,c(1,4,2,3)]

frag_df_full_glad_CFI_aggregate_biomes <- frag_df_full_glad_CFI%>%
  group_by(driverName, Mega_Biome)%>%
  summarise(ForestArea=sum(area),
            RegionArea=sum(RegionArea))


frag_df_full_glad_aggregate <- rbind(frag_df_full_glad_CFI_aggregate_global, frag_df_full_glad_CFI_aggregate_biomes)

# frag_df_full_glad_aggregate%>%group_by(FRAG)%>%summarise(areaSum=sum(ForestArea), areaSum_Region=sum(RegionArea))


# Area <- sum(frag_df_full_glad_Area$ForestArea)

frag_df_full_glad_aggregate$driverName <- factor(frag_df_full_glad_aggregate$driverName, 
                                                 levels=c("No CFI Increase", "Zero or Minor Cover Loss", 
                                                          "Wildfire", "Forestry", "Shifting Agriculture",  
                                                          "Deforestation", "Urbanization"), 
                                                 ordered=T)

frag_df_full_glad_aggregate$Mega_Biome <- factor(frag_df_full_glad_aggregate$Mega_Biome, 
                                                 levels=c("Global", "Tropical", "Temperate", 
                                                          "Boreal"), 
                                                 ordered=T)

# library("scales")
# library("ggsci")
# show_col(pal_npg("nrc")(6))

# colors <- c("grey", rev(pal_npg("nrc")(6)))

# colors <- c("grey", "cyan4","mediumorchid3","goldenrod1","skyblue1","darkorange", "red")

Fig4A <- ggplot(frag_df_full_glad_aggregate, aes(fill=driverName, y=RegionArea, x=Mega_Biome)) +
  geom_bar(position="stack", stat="identity", alpha=1, color="black")+
  # scale_fill_manual(values=c("grey90", "#4575b4", "#91bfdb", "#e0f3f8", "#fee090", "#fc8d59", "#d73027"))+
  scale_fill_manual(values=c("grey90", "grey75", "#d73027", "#fc8d59","#fee090", "#91bfdb", "#4575b4"))+
  # scale_fill_viridis_d(direction=-1)+
  # ggtitle("(A)")+
  xlab("Fragmentation metrics")+
  ylab("Forests with Increased Fragmentation (km2)")+
  theme_classic()+
  scale_y_continuous(expand=c(0,0))+
  theme(title = element_text(size=18,family = "sans", face="bold"),
        # axis.text.x=element_text(size=12,family = "sans", face="bold", angle=60, vjust = 1, hjust=1),
        axis.text.x=element_text(size=18,family = "sans", face="bold", angle=0),
        axis.text.y=element_text(size=14,family = "sans", face="bold"),
        axis.title.x=element_blank(),
        # element_text(size=16,family = "sans", face="bold"),
        axis.title.y=element_text(size=18,family = "sans", face="bold"),
        legend.position = c(0.8, 0.85),
        legend.text = element_text(colour = "black", size = 16,family = "sans", face="bold"),
        legend.title = element_blank())


ggsave("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Results/Week32_2024.6.3/Figure4.pdf", plot=Fig4A, width=9, height=6)


### Figure 4B and 4C Tropics ####

# frag_df_full_glad <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/frag_df_Full_glad_extent_5m.rds")

table(frag_df_full_glad$WDPA_f)
table(frag_df_full_glad$WDPA_binary)

table(frag_df_full_glad$Driver)

frag_df_full_glad$Mega_Biome[frag_df_full_glad$Mega_Biome=="Mediterranean"]<-"Temperate"
frag_df_full_glad$count <- 1

frag_df_full_glad$driverName <- NA
# frag_df_full_glad$driverName[frag_df_full_glad$Driver==0] <- "Zero or minor loss"
frag_df_full_glad$driverName[frag_df_full_glad$Driver==1] <- "Deforestation"
frag_df_full_glad$driverName[frag_df_full_glad$Driver==2] <- "Agriculture"
frag_df_full_glad$driverName[frag_df_full_glad$Driver==3] <- "Forestry"
frag_df_full_glad$driverName[frag_df_full_glad$Driver==4] <- "Wildfire"
frag_df_full_glad$driverName[frag_df_full_glad$Driver==5] <- "Urbanization"

frag_df_full_glad$WDPA_binary_strict <- NA
frag_df_full_glad$WDPA_binary_strict[frag_df_full_glad$WDPA_f%in%c("Ia", "Ib", "II")] <- 2
frag_df_full_glad$WDPA_binary_strict[frag_df_full_glad$WDPA_f%in%c("III", "IV", "VI")] <- 1
frag_df_full_glad$WDPA_binary_strict[frag_df_full_glad$WDPA_f=="No"] <- 0

table(frag_df_full_glad$WDPA_binary_strict)

frag_df_full_glad_filtered <- frag_df_full_glad[frag_df_full_glad$cover>=0.3,]
frag_df_full_glad_select <- frag_df_full_glad_filtered%>%dplyr::select(CFI_diff, FFI_diff, cover_diff, WDPA_binary_strict,WDPA_f,
                                                                       Driver, driverName, Mega_Biome, cover, count)

frag_df_full_glad_select$driverName[frag_df_full_glad_select$CFI_diff<=0] <- "No increase"
frag_df_full_glad_select$driverName[is.na((frag_df_full_glad_select$driverName))] <- "Uncertain"

se <- function(a){
  a <- a[!is.na(a)]
  return(sd(a)/sqrt(length((a))))
}

### differences in frag in a certain biome

biome="Tropical"

if(biome=="All"){frag_df_sub_glad <- frag_df_full_glad_select
}else{frag_df_sub_glad <- frag_df_full_glad_select[frag_df_full_glad_select$Mega_Biome==biome,]}

table(frag_df_sub_glad$WDPA_binary_strict)


frag_df_sub_glad <- frag_df_sub_glad[,c("WDPA_f" ,"WDPA_binary_strict","CFI_diff","cover_diff","FFI_diff", "Mega_Biome")]%>%drop_na()
frag_df_sub_glad$cover_diff <- frag_df_sub_glad$cover_diff*100



### mean with std/se for all WDPA category


t.test(frag_df_sub_glad$CFI_diff[frag_df_sub_glad$WDPA_binary_strict==1],
       frag_df_sub_glad$CFI_diff[frag_df_sub_glad$WDPA_binary_strict==0],
       alternative="two.sided")

t.test(frag_df_sub_glad$cover_diff[frag_df_sub_glad$WDPA_binary_strict==1],
       frag_df_sub_glad$cover_diff[frag_df_sub_glad$WDPA_binary_strict==0],
       alternative="two.sided")

### mean with std/se for binary WDPA category
frag_df_aggregate_glad_binary <- frag_df_sub_glad%>%
  group_by(WDPA_binary_strict, Mega_Biome)%>%
  summarise(cover_mean=mean(cover_diff, na.rm=T),
            cover_std=sd(cover_diff, na.rm=T),
            cover_se=se(cover_diff),
            CFI_mean=mean(CFI_diff, na.rm=T),
            CFI_std=sd(CFI_diff, na.rm=T),
            CFI_se=se(CFI_diff),
            FFI_mean=mean(FFI_diff, na.rm=T),
            FFI_std=sd(FFI_diff, na.rm=T),
            FFI_se=se(FFI_diff))


frag_df_aggregate_glad_binary <- frag_df_aggregate_glad_binary%>%drop_na()
frag_df_aggregate_glad_binary$WDPA <- c("Non-protected", "Protected", "Strictly-protected")
frag_df_aggregate_glad_binary$WDPA <- factor(frag_df_aggregate_glad_binary$WDPA, 
                                             levels=c("Strictly-protected", "Protected", "Non-protected"),
                                             ordered=T)

frag_df_aggregate_glad_binary_CFI_FFI <- data.frame(frag_mean=c(frag_df_aggregate_glad_binary$CFI_mean,
                                                                frag_df_aggregate_glad_binary$FFI_mean),
                                                    frag_se=c(frag_df_aggregate_glad_binary$CFI_se,
                                                              frag_df_aggregate_glad_binary$FFI_se),
                                                    frag_std=c(frag_df_aggregate_glad_binary$CFI_std,
                                                               frag_df_aggregate_glad_binary$FFI_std),
                                                    WDPA=rep(frag_df_aggregate_glad_binary$WDPA,2),
                                                    frag=rep(c("CFI","FFI"),each=3))
frag_df_aggregate_glad_binary_CFI_FFI$WDPA <- factor(frag_df_aggregate_glad_binary_CFI_FFI$WDPA, 
                                                     levels=c("Strictly-protected", "Protected", "Non-protected"),
                                                     ordered=T)

Fig4B <- ggplot(frag_df_aggregate_glad_binary, aes(x=WDPA, y=cover_mean))+
  # coord_flip()+
  # geom_ribbon(aes(ymin = ldi_mean-10*ldi_se, ymax = ldi_mean+10*ldi_se), fill = "red", alpha = 0.4) +  # 误差区间
  # geom_ribbon(aes(ymin = ldi_low, ymax = ldi_up), fill = "red", alpha = 0.35) +  # 误差区间
  # geom_hline(color="black", yintercept = frag_df_aggregate_glad_binary$cover_mean[2], linewidth=0.8, linetype="dashed")+
  geom_hline(color="black", yintercept = 0, linewidth=0.8, linetype="dashed")+
  geom_bar(position="stack", stat="identity", alpha=0.4, color="black", width=0.5)+
  # ylim(c(-3.6,0.152))+
  # geom_point(size=3, alpha = 0.7) +
  # geom_hline(yintercept=0, color="black", linewidth=1, linetype="dashed")+
  geom_errorbar(aes(ymin = cover_mean-8*cover_se, ymax = cover_mean+8*cover_se), width=0.1,linewidth=1,) +  # 误差区间
  labs(x = "WDPA Category", y = "Change in forest cover (%)")+
  theme_classic()+
  # ggtitle("(B)")+
  theme(title = element_text(size=18,family = "sans", face="bold"),
        axis.text   =element_text(size=18,family="sans", face="bold"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=18,family="sans", face="bold"),
        legend.text =element_text(size=18,family="sans", face="bold"),
        # legend.position = "none",
        legend.title=element_text(size=18,family="sans", face="bold"))


frag_df_aggregate_glad_binary_FFI <- frag_df_aggregate_glad_binary_CFI_FFI[frag_df_aggregate_glad_binary_CFI_FFI$frag=="CFI",]

Fig4C <- ggplot(frag_df_aggregate_glad_binary_FFI, aes(x=WDPA, y=frag_mean)) +
  # coord_flip()+
  # geom_ribbon(aes(ymin = ldi_mean-10*ldi_se, ymax = ldi_mean+10*ldi_se), fill = "red", alpha = 0.4) +  # 误差区间
  # geom_ribbon(aes(ymin = ldi_low, ymax = ldi_up), fill = "red", alpha = 0.35) +  # 误差区间
  # geom_hline(color="#ca0020", yintercept = frag_df_aggregate_glad_binary$CFI_mean[2], linewidth=0.8, linetype="dashed")+
  # geom_hline(color="#0571b0", yintercept = frag_df_aggregate_glad_binary$FFI_mean[2], linewidth=0.8, linetype="dashed")+
  geom_hline(color="black", yintercept = 0, linewidth=0.8, linetype="dashed")+
  # geom_point(size=3, alpha = 0.7) +
  geom_bar(position="stack", stat="identity", alpha=0.4, color="black", width=0.5)+
  geom_errorbar(aes(ymin = frag_mean-8*frag_se, ymax = frag_mean+8*frag_se), width=0.1,linewidth=1,) +  # 误差区间
  # ylim(c(0,0.04))+
  # geom_hline(yintercept=0, color="black", linewidth=1, linetype="dashed")+
  labs(x = "WDPA Category", y = "Change in CFI")+
  # scale_color_manual(values=c("#ca0020","#0571b0"))+
  # scale_fill_manual(values=c("#ca0020","#0571b0"))+
  theme_classic()+
  # ggtitle("(C)")+
  theme(title = element_text(size=18,family = "sans", face="bold"),
        axis.text   =element_text(size=18,family="sans", face="bold"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=18,family="sans", face="bold"),
        legend.text =element_text(size=18,family="sans", face="bold"),
        # legend.position = "none",
        legend.title=element_text(size=18,family="sans", face="bold"))


## Figure 4E and 4F Temperate and boreal####

frag_df_full_glad$Mega_Biome[frag_df_full_glad$Mega_Biome=="Mediterranean"]<-"Temperate"
frag_df_full_glad$count <- 1

frag_df_full_glad$driverName <- NA
# frag_df_full_glad$driverName[frag_df_full_glad$Driver==0] <- "Zero or minor loss"
frag_df_full_glad$driverName[frag_df_full_glad$Driver==1] <- "Deforestation"
frag_df_full_glad$driverName[frag_df_full_glad$Driver==2] <- "Agriculture"
frag_df_full_glad$driverName[frag_df_full_glad$Driver==3] <- "Forestry"
frag_df_full_glad$driverName[frag_df_full_glad$Driver==4] <- "Wildfire"
frag_df_full_glad$driverName[frag_df_full_glad$Driver==5] <- "Urbanization"

frag_df_full_glad$WDPA_binary_strict <- NA
frag_df_full_glad$WDPA_binary_strict[frag_df_full_glad$WDPA_f%in%c("Ia", "Ib", "II")] <- 2
frag_df_full_glad$WDPA_binary_strict[frag_df_full_glad$WDPA_f%in%c("III", "IV", "VI")] <- 1
frag_df_full_glad$WDPA_binary_strict[frag_df_full_glad$WDPA_f=="No"] <- 0

table(frag_df_full_glad$WDPA_binary_strict)

frag_df_full_glad_filtered <- frag_df_full_glad[frag_df_full_glad$cover>=0.3,]
frag_df_full_glad_select <- frag_df_full_glad_filtered%>%dplyr::select(CFI_diff, FFI_diff, cover_diff, WDPA_binary_strict,WDPA_f,
                                                                       Driver, driverName, Mega_Biome, cover, count)

frag_df_full_glad_select$driverName[frag_df_full_glad_select$CFI_diff<=0] <- "No increase"
frag_df_full_glad_select$driverName[is.na((frag_df_full_glad_select$driverName))] <- "Uncertain"

se <- function(a){
  a <- a[!is.na(a)]
  return(sd(a)/sqrt(length((a))))
}

### differences in frag in a certain biome

biome=c("Temperate", "Boreal")

frag_df_sub_glad <- frag_df_full_glad_select[frag_df_full_glad_select$Mega_Biome%in%biome,]

table(frag_df_sub_glad$WDPA_binary_strict)





frag_df_sub_glad <- frag_df_sub_glad[,c("WDPA_f" ,"WDPA_binary_strict","CFI_diff","cover_diff","FFI_diff", "Mega_Biome")]%>%drop_na()
frag_df_sub_glad$cover_diff <- frag_df_sub_glad$cover_diff*100


### mean with std/se for all WDPA category
t.test(frag_df_sub_glad$CFI_diff[frag_df_sub_glad$WDPA_binary_strict==2],
       frag_df_sub_glad$CFI_diff[frag_df_sub_glad$WDPA_binary_strict==0],
       alternative="two.sided")

t.test(frag_df_sub_glad$cover_diff[frag_df_sub_glad$WDPA_binary_strict==2],
       frag_df_sub_glad$cover_diff[frag_df_sub_glad$WDPA_binary_strict==0],
       alternative="two.sided")

### mean with std/se for binary WDPA category
frag_df_aggregate_glad_binary <- frag_df_sub_glad%>%
  group_by(WDPA_binary_strict)%>%
  summarise(cover_mean=mean(cover_diff, na.rm=T),
            cover_std=sd(cover_diff, na.rm=T),
            cover_se=se(cover_diff),
            CFI_mean=mean(CFI_diff, na.rm=T),
            CFI_std=sd(CFI_diff, na.rm=T),
            CFI_se=se(CFI_diff),
            FFI_mean=mean(FFI_diff, na.rm=T),
            FFI_std=sd(FFI_diff, na.rm=T),
            FFI_se=se(FFI_diff))


frag_df_aggregate_glad_binary <- frag_df_aggregate_glad_binary%>%drop_na()
frag_df_aggregate_glad_binary$WDPA <- c("Non-protected", "Protected", "Strictly-protected")
frag_df_aggregate_glad_binary$WDPA <- factor(frag_df_aggregate_glad_binary$WDPA, 
                                             levels=c("Strictly-protected", "Protected", "Non-protected"),
                                             ordered=T)

frag_df_aggregate_glad_binary_CFI_FFI <- data.frame(frag_mean=c(frag_df_aggregate_glad_binary$CFI_mean,
                                                                frag_df_aggregate_glad_binary$FFI_mean),
                                                    frag_se=c(frag_df_aggregate_glad_binary$CFI_se,
                                                              frag_df_aggregate_glad_binary$FFI_se),
                                                    frag_std=c(frag_df_aggregate_glad_binary$CFI_std,
                                                               frag_df_aggregate_glad_binary$FFI_std),
                                                    WDPA=rep(frag_df_aggregate_glad_binary$WDPA,2),
                                                    frag=rep(c("CFI","FFI"),each=3))
frag_df_aggregate_glad_binary_CFI_FFI$WDPA <- factor(frag_df_aggregate_glad_binary_CFI_FFI$WDPA, 
                                                     levels=c("Strictly-protected", "Protected", "Non-protected"),
                                                     ordered=T)

Fig4E <- ggplot(frag_df_aggregate_glad_binary, aes(x=WDPA, y=cover_mean))+
  # coord_flip()+
  # geom_ribbon(aes(ymin = ldi_mean-10*ldi_se, ymax = ldi_mean+10*ldi_se), fill = "red", alpha = 0.4) +  # 误差区间
  # geom_ribbon(aes(ymin = ldi_low, ymax = ldi_up), fill = "red", alpha = 0.35) +  # 误差区间
  # geom_hline(color="black", yintercept = frag_df_aggregate_glad_binary$cover_mean[2], linewidth=0.8, linetype="dashed")+
  geom_hline(color="black", yintercept = 0, linewidth=0.8, linetype="dashed")+
  geom_bar(position="stack", stat="identity", alpha=0.4, color="black", width=0.5)+
  ggtitle("Extratropical")+
  # ylim(c(-3.6,0.152))+
  # geom_point(size=3, alpha = 0.7) +
  # geom_hline(yintercept=0, color="black", linewidth=1, linetype="dashed")+
  geom_errorbar(aes(ymin = cover_mean-8*cover_se, ymax = cover_mean+8*cover_se), width=0.1,linewidth=1,) +  # 误差区间
  labs(x = "WDPA Category", y = "Change in forest cover (%)")+
  theme_classic()+
  theme(title = element_text(size=18,family = "sans", face="bold"),
        axis.text   =element_text(size=18,family="sans", face="bold"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=18,family="sans", face="bold"),
        legend.text =element_text(size=18,family="sans", face="bold"),
        # legend.position = "none",
        legend.title=element_text(size=18,family="sans", face="bold"))


frag_df_aggregate_glad_binary_FFI <- frag_df_aggregate_glad_binary_CFI_FFI[frag_df_aggregate_glad_binary_CFI_FFI$frag=="CFI",]

Fig4F <- ggplot(frag_df_aggregate_glad_binary_FFI, aes(x=WDPA, y=frag_mean)) +
  # coord_flip()+
  # geom_ribbon(aes(ymin = ldi_mean-10*ldi_se, ymax = ldi_mean+10*ldi_se), fill = "red", alpha = 0.4) +  # 误差区间
  # geom_ribbon(aes(ymin = ldi_low, ymax = ldi_up), fill = "red", alpha = 0.35) +  # 误差区间
  # geom_hline(color="#ca0020", yintercept = frag_df_aggregate_glad_binary$CFI_mean[2], linewidth=0.8, linetype="dashed")+
  # geom_hline(color="#0571b0", yintercept = frag_df_aggregate_glad_binary$FFI_mean[2], linewidth=0.8, linetype="dashed")+
  geom_hline(color="black", yintercept = 0, linewidth=0.8, linetype="dashed")+
  # geom_point(size=3, alpha = 0.7) +
  geom_bar(position="stack", stat="identity", alpha=0.4, color="black", width=0.5)+
  ggtitle("Extratropical")+
  geom_errorbar(aes(ymin = frag_mean-8*frag_se, ymax = frag_mean+8*frag_se), width=0.1,linewidth=1,) +  # 误差区间
  # ylim(c(0,0.04))+
  # geom_hline(yintercept=0, color="black", linewidth=1, linetype="dashed")+
  labs(x = "WDPA Category", y = "Change in CFI")+
  # scale_color_manual(values=c("#ca0020","#0571b0"))+
  # scale_fill_manual(values=c("#ca0020","#0571b0"))+
  theme_classic()+
  theme(title = element_text(size=18,family = "sans", face="bold"),
        axis.text   =element_text(size=18,family="sans", face="bold"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=18,family="sans", face="bold"),
        legend.text =element_text(size=18,family="sans", face="bold"),
        legend.position = "none",
        legend.title=element_text(size=18,family="sans", face="bold"))



### Integrate all panels into one figure ####

library(ggplot2)
library(patchwork)

# Example ggplot2 graphs
p1 <- ggplot(mtcars, aes(mpg, wt)) + geom_point() + ggtitle("Plot 1")
p2 <- ggplot(mtcars, aes(mpg, hp)) + geom_point() + ggtitle("Plot 2")
p3 <- ggplot(mtcars, aes(mpg, qsec)) + geom_point() + ggtitle("Plot 3")

# Combine plots using patchwork
# Layout: p1 in the first column, p2 and p3 in the second column
combined_plot <- (p1 | (p2 / p3)) + plot_layout(widths = c(1, 1))

# Display the combined plot
print(combined_plot)



Figure4 <- (Fig4A | (Fig4B / Fig4C)) + plot_layout(widths = c(1, 1))

ggsave("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Results/Week31_2024.5.21/Figure4.pdf", plot=Figure4, width=14, height=10)

layout3 <- "
AABB
AABB
AACC
AACC
"

#Merge plots
MainPlot = Fig4A + Fig4B + Fig4C +
  plot_layout(design = layout3) + plot_annotation(tag_levels = 'A')&
  theme(plot.tag = element_text(face = 'bold'))

MainPlot

ggsave("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Results/Week31_2024.5.21/Figure4.pdf", plot=MainPlot, width=14, height=10)



### Figure SX3 ####
FigSX <- ggplot(frag_df_aggregate_glad_binary_CFI_FFI, aes(x=WDPA, y=frag_mean, color=frag, fill=frag)) +
  # coord_flip()+
  # geom_ribbon(aes(ymin = ldi_mean-10*ldi_se, ymax = ldi_mean+10*ldi_se), fill = "red", alpha = 0.4) +  # 误差区间
  # geom_ribbon(aes(ymin = ldi_low, ymax = ldi_up), fill = "red", alpha = 0.35) +  # 误差区间
  # geom_hline(color="#ca0020", yintercept = frag_df_aggregate_glad_binary$CFI_mean[2], linewidth=0.8, linetype="dashed")+
  # geom_hline(color="#0571b0", yintercept = frag_df_aggregate_glad_binary$FFI_mean[2], linewidth=0.8, linetype="dashed")+
  geom_hline(color="grey70", yintercept = 0, linewidth=0.8)+
  geom_point(size=3, alpha = 0.7) +
  # ylim(c(-0.052,0.031))+
  # geom_hline(yintercept=0, color="black", linewidth=1, linetype="dashed")+
  geom_errorbar(aes(ymin = frag_mean-8*frag_se, ymax = frag_mean+8*frag_se), width=0.02) +  # 误差区间
  labs(x = "WDPA Category", y = "Change in Metric")+
  scale_color_manual(values=c("#ca0020","#0571b0"))+
  scale_fill_manual(values=c("#ca0020","#0571b0"))+
  theme_classic()+
  theme(axis.text   =element_text(size=18,family="sans"),
        axis.title.x=element_text(size=18,family="sans"),
        axis.title.y=element_text(size=18,family="sans"),
        legend.text =element_text(size=18,family="sans"),
        # legend.position = "none",
        legend.title=element_text(size=18,family="sans"))


ggsave("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Results/Week31_2024.5.21/FigureSX_WDPA_FFI_CFI.pdf", plot=FigSX, width=8, height=6)
