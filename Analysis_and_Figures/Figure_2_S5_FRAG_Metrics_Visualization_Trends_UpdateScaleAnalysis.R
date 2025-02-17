rm(list=ls())

library(rvest)
library(tidyverse)
library(tictoc)
library(raster)
library(gridExtra)
library(patchwork)
library(landscapemetrics)
library(landscapeR)

####----Figure 2A: Check PCA distribution of the selected fragmentation metrics----####
# frag_df_full_glad_pca <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/frag_df_Full_glad_extent_5m.rds")
# colnames(frag_df_full_glad_pca)


selected_var_2000 <- c("TCA_ST", "LPI_ST", "LDI_ST", "CFI", "NP_ST", 
                       "MPA_ST", "ED_ST", "FFI", "AI_ST", "PCI_ST", 
                       "NLSI_ST", "CI_ST", "SI_ST", "PAR_ST",
                       "cover", "MPC_ST", "AFI", "ENN_ST", "LSI_ST", "PARMN_ST",
                       "PLADJ_ST"
)


replace_var <- c("TCA", "LPI", "LDI", "CFI", "NP", "MPA", 
                 "ED", "FFI", "AI", "PCI", "NLSI", "CI", "SI", "PAR",
                 "Cover", "MPC", "AFI", "ENN", "LSI", "PARMN", "PLADJ")

# set.seed(1245)
# frag_df_sub_glad_pca <- frag_df_full_glad_pca%>%group_by(Mega_Biome)%>%slice_sample(prop=0.01)
# frag_df_sub_glad <- frag_df_sub_glad_pca[,selected_var_2000]
# colnames(frag_df_sub_glad) <- replace_var

frag_df_sub_glad_pca <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Outcome/2024.10.24_ScienceResponse/frag_df_sub_glad_pca_1per.rds")
frag_df_sub_glad <- frag_df_sub_glad_pca[,selected_var_2000]
colnames(frag_df_sub_glad) <- replace_var

data <- frag_df_sub_glad %>% dplyr::select(TCA, LPI, LDI, CFI, NP, MPA, ED, FFI,
                                           AI,
                                           # PCI, 
                                           ENN,
                                           MPC,
                                           AFI,
                                           # Cover,
                                           #NLSI, CI, SI, 
                                           PLADJ)%>%drop_na()
pca <- prcomp(data, scale. = TRUE, center = TRUE)

pca_summary <- summary(pca)
variance_explained <- pca_summary$sdev^2 / sum(pca_summary$sdev^2)
print(variance_explained)

# Extract variance explained by PC1 and PC2
variance_pc1 <- variance_explained[1]
variance_pc2 <- variance_explained[2]

scores <- as.data.frame(pca$x)  # PCA scores
loadings <- pca$rotation  # PCA loadings

loadings_df <- as.data.frame(loadings[, 1:2])  # Only consider first two PCs
loadings_df$variable <- rownames(loadings)

loadings_df$group <- c("C", "C", "C", "C", "S", "S", "S", "S", "A", "A", "C", "A", "A")

manova_fit <- manova(cbind(PC1, PC2) ~ group, data = loadings_df)
summary(manova_fit)

# loadings_df$variable <- factor(loadings_df$variable, 
#                                                 levels=c("MPC", "CFI", "TCA", "LPI", "LDI",  
#                                                          "AFI", "AI", "PAR", "PCI", "FFI",
#                                                          "MPA", "NP", "ED"
#                                                 ), 
#                                                 ordered=T)
loadings_df$alphas <- c(0.7, 0.7, 0.7, 0.9, 0.7, 0.7, 0.7, 0.9, 0.7, 0.7, 0.9, 0.9, 0.7)

# loadings_df$sizes <- c(4, 4, 4, 6, 4, 4, 4, 6, 4, 4, 6, 6, 4)

# loadings_df$alphas <- c(0.7, 0.7, 0.7, 0.9, 0.7, 0.7, 0.7, 0.9, 0.7, 0.7, 0.9, 0.9, 0.7)

loadings_df$colors <- c("#fe8181","#fe8181","#fe8181","#ca0020","#74add1","#74add1","#74add1","#2166ac","orange","orange","black","darkorange3","orange" )
# Define maximum length for arrows for visibility
# max_arrow_length <- max(abs(loadings_df[, 1]), abs(loadings_df[, 2]))
max_arrow_length <- 0.1

loadings_df_1 <- loadings_df[loadings_df$variable%in%c("MPC", "CFI","AFI","FFI"),]
loadings_df_2 <- loadings_df[!loadings_df$variable%in%c("MPC", "CFI","AFI","FFI"),]

# loadings_df_2$alphas <- 0.9
# colors <- c("#f4a582", "#ca0020","#ca0020","#ca0020","#ca0020","#7b3294","#7b3294","#7b3294","#7b3294",
#             "#0571b0","#0571b0","#0571b0","#0571b0")

Fig2A <- ggplot(data = scores, aes(x = PC1, y = PC2)) +
  geom_point(size = 3, color="gray20", alpha=0.01) +  # Color by a factor, e.g., 'cyl' from mtcars
  geom_segment(data = loadings_df, aes(x = 0, y = 0, xend = PC1 / max_arrow_length, yend = PC2 / max_arrow_length, color = colors, alpha=0.9, size=2),
               arrow = arrow(type = "closed", length = unit(0.2, "inches"))) +
  # geom_segment(data = loadings_df_2, aes(x = 0, y = 0, xend = PC1 / max_arrow_length, yend = PC2 / max_arrow_length, color = colors, alpha=0.4),
  #              arrow = arrow(type = "closed", length = unit(0.2, "inches"))) +
  geom_text(data = loadings_df, aes(x = PC1 / max_arrow_length * 1.1, y = PC2 / max_arrow_length * 1.1, label = variable, size=6),
            hjust = 0.5, vjust = 0.5, color = "black", family = "sans") +
  # geom_text_repel(data = loadings_df, aes(x = PC1 / max_arrow_length * 1.1, y = PC2 / max_arrow_length * 1.1, label = variable),
  #                 point.padding = unit(0.1, "inches"), color = "black", size = 6, family = "sans") +
  scale_color_identity() +
  # scale_color_manual(values = colors) +
  labs(title = "PCA Biplot", x = paste0("PC1 (", round(variance_pc1*100,1), "%)"), y = paste0("PC2 (", round(variance_pc2*100,1), "%)")) +
  theme_classic()+
  theme(axis.text   =element_text(size=16,family = "sans", face="bold"),
        axis.title.x=element_text(size=20,family = "sans", face="bold"),
        axis.title.y=element_text(size=20,family = "sans", face="bold"),
        title = element_blank(),
        legend.position = "none",
        legend.text =element_blank(),
        legend.title=element_blank())

####----Figure 2B: frag ~ cover curve----####
####----Actual landscape Y2000: Check relationship between cover and CFI/FFI
fragDf_Simulated_Landscape <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/frag_df_Full_glad_extent_5m.rds")
colnames(fragDf_Simulated_Landscape)

# fragDf_Simulated_Landscape_visu <- data.frame(cbind(cover=rep(fragDf_Simulated_Landscape$cover,2),frag=c(fragDf_Simulated_Landscape$cfi,fragDf_Simulated_Landscape$ffi),
#                                                     group=rep(c("CFI", "FFI"), each=nrow(fragDf_Simulated_Landscape))))
# 
# fragDf_Simulated_Landscape_visu$cover <- as.numeric(fragDf_Simulated_Landscape_visu$cover)
# fragDf_Simulated_Landscape_visu$frag <- as.numeric(fragDf_Simulated_Landscape_visu$frag)
# 
# class(fragDf_Simulated_Landscape_visu$frag)

se <- function(a){
  a <- a[!is.na(a)]
  return(sd(a)/sqrt(length((a))))
}

upper <- function(a){
  qa <- as.numeric(quantile(a, probs=0.975, na.rm=T))
  return(qa)
}

lower <- function(a){
  qa <- as.numeric(quantile(a, probs=0.025, na.rm=T))
  return(qa)
}


fragDf_Simulated_Landscape <- fragDf_Simulated_Landscape%>%dplyr::select(cover,CFI,FFI,AFI)%>%drop_na()
# AA <- as.numeric(upper(fragDf_Simulated_Landscape$cover))
colnames(fragDf_Simulated_Landscape) <- c("cover", "cfi", "ffi", "afi")

bins <- seq(0, 1, by=0.01)
labels <- seq(0.005, 0.995, by=0.01)
data_t <- fragDf_Simulated_Landscape %>%
  mutate(group = cut(cover, breaks = bins,
                     labels = labels, right = FALSE)) %>%
  group_by(group) %>%
  summarize(cfi_mean = mean(cfi, na.rm=T), cfi_sd = sd(cfi, na.rm=T), 
            cfi_se = se(cfi), cfi_median = median(cfi, na.rm=T),
            cfi_upper = upper(cfi), cfi_lower=lower(cfi),
            ffi_mean = mean(ffi, na.rm=T), ffi_sd = sd(ffi, na.rm=T), 
            ffi_se = se(ffi), ffi_median = median(ffi, na.rm=T),
            ffi_upper = upper(ffi), ffi_lower=lower(ffi),
            afi_mean = mean(afi, na.rm=T), afi_sd = sd(afi, na.rm=T), 
            afi_se = se(afi), afi_median = median(afi, na.rm=T),
            afi_upper = upper(afi), afi_lower=lower(afi)
            # , cover_up=CI_up(cover_diff*100), cover_low=CI_low(cover_diff*100)
  )%>%drop_na()

data_t$group <- as.numeric(labels)

data_v <- data.frame(cbind(cover=rep(data_t$group,2),
                           frag_mean=c(data_t$cfi_mean,data_t$ffi_mean,data_t$afi_mean),
                           frag_median=c(data_t$cfi_median,data_t$ffi_median,data_t$afi_median),
                           frag_sd=c(data_t$cfi_sd, data_t$ffi_sd, data_t$afi_sd),
                           frag_se=c(data_t$cfi_se, data_t$ffi_se, data_t$afi_se),
                           frag_upper=c(data_t$cfi_upper, data_t$ffi_upper, data_t$afi_upper),
                           frag_lower=c(data_t$cfi_lower, data_t$ffi_lower, data_t$afi_lower),
                           Metric=rep(c("CFI", "FFI", "AFI"), each=nrow(data_t))
))

data_v <- data_v%>%mutate(cover=as.numeric(cover),frag_mean=as.numeric(frag_mean),
                          frag_sd=as.numeric(frag_sd), frag_se=as.numeric(frag_se),
                          frag_median=as.numeric(frag_median),
                          frag_upper=as.numeric(frag_upper), frag_lower=as.numeric(frag_lower))

# data_0 <- as.data.frame(cbind(cover=c(0,0,0), frag_mean=c(1, 1/3, 1), 
#                               frag_median=c(1, 1/3, 1),frag_sd=c(0,0,0),
#                               frag_se=c(0,0,0), frag_upper=c(1, 1/3, 1),
#                               frag_lower=c(1, 1/3, 1),Metric=c("CFI", "FFI", "AFI")))
# data_0 <- data_0%>%mutate(cover=as.numeric(cover),frag_mean=as.numeric(frag_mean),
#                           frag_sd=as.numeric(frag_sd), frag_se=as.numeric(frag_se),
#                           frag_median=as.numeric(frag_median),
#                           frag_upper=as.numeric(frag_upper), frag_lower=as.numeric(frag_lower))
# 
# data_1 <- as.data.frame(cbind(cover=c(1,1,1), frag_mean=c(0, 0, 0), 
#                               frag_median=c(0, 0, 0),frag_sd=c(0,0,0),
#                               frag_se=c(0,0,0), frag_upper=c(0, 0, 0),
#                               frag_lower=c(0, 0, 0),Metric=c("CFI", "FFI", "AFI")))
# data_1 <- data_1%>%mutate(cover=as.numeric(cover),frag_mean=as.numeric(frag_mean),
#                           frag_sd=as.numeric(frag_sd), frag_se=as.numeric(frag_se),
#                           frag_median=as.numeric(frag_median),
#                           frag_upper=as.numeric(frag_upper), frag_lower=as.numeric(frag_lower))
# 
# 
# 
# data_v <- rbind(data_v,data_0)
# data_v <- rbind(data_v,data_1)



data_v$Metric <- factor(data_v$Metric, levels=c("CFI", "AFI", "FFI"),
                        ordered=T)

Fig2B <- ggplot(data_v, aes(x=cover*100, y=frag_median, color=Metric, fill=Metric)) +
  # coord_flip()+
  # geom_ribbon(aes(ymin = ldi_mean-10*ldi_se, ymax = ldi_mean+10*ldi_se), fill = "red", alpha = 0.4) +  # 误差区间
  geom_ribbon(aes(ymin = frag_lower, ymax = frag_upper), alpha = 0.3) +  # 误差区间
  # geom_hline(color="grey", yintercept = 0, linewidth=0.8, linetype="dashed")+
  geom_line(linewidth=2, alpha = 0.7) +
  scale_fill_manual(values=c("#ca0020", "orange", "#0571b0"))+
  scale_color_manual(values=c("#ca0020", "orange", "#0571b0"))+
  labs(x = "Canopy Cover (%)", y = "Degree of fragmentation")+
  theme_classic()+
  theme(axis.text   =element_text(size=15,family="sans", face="bold"),
        axis.title.x=element_text(size=20,family="sans", face="bold"),
        axis.title.y=element_text(size=20,family="sans", face="bold"),
        # title = element_text(size=20,family = "sans", face="bold"),
        legend.text =element_text(size=15,family="sans", face="bold"),
        legend.position = c(0.75, 0.95),
        legend.title=element_blank())

####----Figure 2C: proportion of fragmented forest in percentage based on different grid cell size----####
frag_stat_df_acrossScale <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Outcome/2024.10.24_ScienceResponse/frag_stat_df_AcrossScale_0.3cover_0threshold_0diffT.rds")
frag_stat_df_acrossScale_globe_3D <- frag_stat_df_acrossScale[frag_stat_df_acrossScale$frag%in%c("CFI","AFI","SFI")&frag_stat_df_acrossScale$biome=="All",]
frag_stat_df_acrossScale_globe_3D <- frag_stat_df_acrossScale_globe_3D%>%mutate(
  frag=fct_relevel(frag,"CFI","AFI","SFI")
)


Fig2C <- ggplot(frag_stat_df_acrossScale_globe_3D, aes(x=as.factor(scale), y=pp, fill=frag))+
  # geom_point(size=4)+
  geom_hline(yintercept = 50, color="grey50", linetype="dashed",linewidth=0.8)+
  geom_bar(position="dodge", stat = "identity", alpha=0.6, color="black")+
  # geom_errorbar(aes(ymin = pp_mean-2*pp_se, ymax = pp_mean+2*pp_se), width=0.3,linewidth=1,position=position_dodge(.9)) +
  scale_y_continuous(expand=c(0,0), limits = c(0,100))+
  # ggtitle("Boreal")+
  # scale_x_log10()+
  scale_fill_manual(values=c( "#ca0020","orange","#0571b0"))+
  xlab("Grid cell length (km)")+
  ylab("Increased fragmentation (%)")+
  # geom_hline(yintercept = baseline, color="red", linetype="dashed",linewidth=1)+
  # geom_hline(yintercept = 50, color="black", linetype="dashed",linewidth=1)+
  theme_classic()+
  theme(title = element_text(size=15,family = "sans", face="bold"),
        # axis.text.x=element_text(size=12,family = "sans", face="bold", angle=60, vjust = 1, hjust=1),
        axis.text.x=element_text(size=15,family = "sans", face="bold", angle=0),
        axis.text.y=element_text(size=15,family = "sans", face="bold"),
        axis.title.x=element_text(size=20,family = "sans", face="bold"),
        axis.title.y=element_text(size=20,family = "sans", face="bold"),
        legend.position = c(0.75, 0.95),
        legend.text = element_text(colour = "black", size = 15,family = "sans", face="bold"),
        legend.title = element_blank())


####----Figure 2D: show proportion of fragmented forest in percentage based on different metrics across biomes----####
frag_stat_df_acrossScale_3D <- frag_stat_df_acrossScale[frag_stat_df_acrossScale$frag%in%c("CFI","AFI","SFI"),]

# frag_stat_df_acrossScale_3D$frag <- as.character(frag_stat_df_acrossScale_3D$frag)
frag_stat_df_aggregate <- frag_stat_df_acrossScale_3D%>%group_by(frag, biome)%>%summarise(frag_mean=mean(pp),frag_sd=sd(pp))
# frag_stat_df_aggregate$frag <- as.character(frag_stat_df_aggregate$frag)
# frag_stat_df_aggregate$biome <- as.character(frag_stat_df_aggregate$biome)

frag_stat_df_aggregate<-frag_stat_df_aggregate%>%ungroup()

frag_stat_df_aggregate <- frag_stat_df_aggregate%>%mutate(
  biome=fct_relevel(biome,"All","Tropical","Temperate","Boreal")
)

frag_stat_df_aggregate <- frag_stat_df_aggregate%>%mutate(
  frag=fct_relevel(frag,"CFI","AFI","SFI")
)


Fig2D <- ggplot(frag_stat_df_aggregate, aes(x=biome, y=frag_mean, fill=frag))+
  geom_hline(yintercept = 50, color="grey50", linetype="dashed",linewidth=0.8)+
  geom_bar(position="dodge", stat = "identity", alpha=0.6, color="black")+
  # geom_errorbar(aes(ymin = R2_mean-R2_std, ymax = R2_mean+R2_std), width=0.3,linewidth=1,position=position_dodge(.9)) +
  # ylim(c(0,1))+
  geom_errorbar(aes(ymin = frag_mean-frag_sd, ymax = frag_mean+frag_sd), width=0.3,linewidth=0.75,position=position_dodge(.9)) +
  xlab("Fragmentation metrics")+
  ylab("Increased fragmentation (%)")+
  scale_fill_manual(values=c("#ca0020","orange","#0571b0"))+
  # geom_col(aes())+
  # scale_color_manual(values=c("black", "white"))+
  theme_classic()+
  scale_y_continuous(expand=c(0,0), limits = c(0,100))+
  # geom_hline(yintercept = 0.7, color="black", linetype="dashed",linewidth=1)+
  theme(
    # title = element_text(size=20,family = "sans", face="bold"),
    # axis.text.x=element_text(size=12,family = "sans", face="bold", angle=60, vjust = 1, hjust=1),
    axis.text.x=element_text(size=15,family = "sans", face="bold", angle=0),
    axis.text.y=element_text(size=15,family = "sans", face="bold"),
    axis.title.x=element_text(size=20,family = "sans", face="bold"),
    axis.title.y=element_text(size=20,family = "sans", face="bold"),
    # legend.position = "none",
    legend.position = c(0.75, 0.95),
    legend.text = element_text(colour = "black", size = 15,family = "sans", face="bold"),
    legend.title = element_blank())+
  guides(colour = guide_legend(override.aes = list(alpha = 0.5)))





## do the same for year 2020
fragDf_Simulated_Landscape <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/frag_df_Full_glad_extent_5m.rds")
colnames(fragDf_Simulated_Landscape)

# fragDf_Simulated_Landscape_visu <- data.frame(cbind(cover=rep(fragDf_Simulated_Landscape$cover,2),frag=c(fragDf_Simulated_Landscape$cfi,fragDf_Simulated_Landscape$ffi),
#                                                     group=rep(c("CFI", "FFI"), each=nrow(fragDf_Simulated_Landscape))))
# 
# fragDf_Simulated_Landscape_visu$cover <- as.numeric(fragDf_Simulated_Landscape_visu$cover)
# fragDf_Simulated_Landscape_visu$frag <- as.numeric(fragDf_Simulated_Landscape_visu$frag)
# 
# class(fragDf_Simulated_Landscape_visu$frag)

se <- function(a){
  a <- a[!is.na(a)]
  return(sd(a)/sqrt(length((a))))
}

upper <- function(a){
  qa <- as.numeric(quantile(a, probs=0.975, na.rm=T))
  return(qa)
}

lower <- function(a){
  qa <- as.numeric(quantile(a, probs=0.025, na.rm=T))
  return(qa)
}

fragDf_Simulated_Landscape <- fragDf_Simulated_Landscape%>%dplyr::select(cover_2020,CFI_2020,FFI_2020,AFI_2020)%>%drop_na()
# AA <- as.numeric(upper(fragDf_Simulated_Landscape$cover))
colnames(fragDf_Simulated_Landscape) <- c("cover", "cfi", "ffi", "afi")

bins <- seq(0, 1, by=0.01)
labels <- seq(0.005, 0.995, by=0.01)
data_t <- fragDf_Simulated_Landscape %>%
  mutate(group = cut(cover, breaks = bins,
                     labels = labels, right = FALSE)) %>%
  group_by(group) %>%
  summarize(cfi_mean = mean(cfi, na.rm=T), cfi_sd = sd(cfi, na.rm=T), 
            cfi_se = se(cfi), cfi_median = median(cfi, na.rm=T),
            cfi_upper = upper(cfi), cfi_lower=lower(cfi),
            ffi_mean = mean(ffi, na.rm=T), ffi_sd = sd(ffi, na.rm=T), 
            ffi_se = se(ffi), ffi_median = median(ffi, na.rm=T),
            ffi_upper = upper(ffi), ffi_lower=lower(ffi),
            afi_mean = mean(afi, na.rm=T), afi_sd = sd(afi, na.rm=T), 
            afi_se = se(afi), afi_median = median(afi, na.rm=T),
            afi_upper = upper(afi), afi_lower=lower(afi)
            # , cover_up=CI_up(cover_diff*100), cover_low=CI_low(cover_diff*100)
  )%>%drop_na()

data_t$group <- as.numeric(labels)

data_v <- data.frame(cbind(cover=rep(data_t$group,2),
                           frag_mean=c(data_t$cfi_mean,data_t$ffi_mean,data_t$afi_mean),
                           frag_median=c(data_t$cfi_median,data_t$ffi_median,data_t$afi_median),
                           frag_sd=c(data_t$cfi_sd, data_t$ffi_sd, data_t$afi_sd),
                           frag_se=c(data_t$cfi_se, data_t$ffi_se, data_t$afi_se),
                           frag_upper=c(data_t$cfi_upper, data_t$ffi_upper, data_t$afi_upper),
                           frag_lower=c(data_t$cfi_lower, data_t$ffi_lower, data_t$afi_lower),
                           Metric=rep(c("CFI", "FFI", "AFI"), each=nrow(data_t))
))

data_v <- data_v%>%mutate(cover=as.numeric(cover),frag_mean=as.numeric(frag_mean),
                          frag_sd=as.numeric(frag_sd), frag_se=as.numeric(frag_se),
                          frag_median=as.numeric(frag_median),
                          frag_upper=as.numeric(frag_upper), frag_lower=as.numeric(frag_lower))

# data_0 <- as.data.frame(cbind(cover=c(0,0,0), frag_mean=c(1, 1/3, 1), 
#                               frag_median=c(1, 1/3, 1),frag_sd=c(0,0,0),
#                               frag_se=c(0,0,0), frag_upper=c(1, 1/3, 1),
#                               frag_lower=c(1, 1/3, 1),Metric=c("CFI", "FFI", "AFI")))
# data_0 <- data_0%>%mutate(cover=as.numeric(cover),frag_mean=as.numeric(frag_mean),
#                           frag_sd=as.numeric(frag_sd), frag_se=as.numeric(frag_se),
#                           frag_median=as.numeric(frag_median),
#                           frag_upper=as.numeric(frag_upper), frag_lower=as.numeric(frag_lower))
# 
# data_1 <- as.data.frame(cbind(cover=c(1,1,1), frag_mean=c(0, 0, 0), 
#                               frag_median=c(0, 0, 0),frag_sd=c(0,0,0),
#                               frag_se=c(0,0,0), frag_upper=c(0, 0, 0),
#                               frag_lower=c(0, 0, 0),Metric=c("CFI", "FFI", "AFI")))
# data_1 <- data_1%>%mutate(cover=as.numeric(cover),frag_mean=as.numeric(frag_mean),
#                           frag_sd=as.numeric(frag_sd), frag_se=as.numeric(frag_se),
#                           frag_median=as.numeric(frag_median),
#                           frag_upper=as.numeric(frag_upper), frag_lower=as.numeric(frag_lower))
# 
# 
# 
# data_v <- rbind(data_v,data_0)
# data_v <- rbind(data_v,data_1)



data_v$Metric <- factor(data_v$Metric, levels=c("CFI", "AFI", "FFI"),
                        ordered=T)

FigSX <- ggplot(data_v, aes(x=cover*100, y=frag_median, color=Metric, fill=Metric)) +
  # coord_flip()+
  # geom_ribbon(aes(ymin = ldi_mean-10*ldi_se, ymax = ldi_mean+10*ldi_se), fill = "red", alpha = 0.4) +  # 误差区间
  geom_ribbon(aes(ymin = frag_lower, ymax = frag_upper), alpha = 0.3) +  # 误差区间
  # geom_hline(color="grey", yintercept = 0, linewidth=0.8, linetype="dashed")+
  geom_line(linewidth=2, alpha = 0.7) +
  scale_fill_manual(values=c("#ca0020", "orange", "#0571b0"))+
  scale_color_manual(values=c("#ca0020", "orange", "#0571b0"))+
  labs(x = "Canopy Cover (%)", y = "Degree of fragmentation")+
  theme_classic()+
  theme(axis.text   =element_text(size=15,family="sans", face="bold"),
        axis.title.x=element_text(size=20,family="sans", face="bold"),
        axis.title.y=element_text(size=20,family="sans", face="bold"),
        # title = element_text(size=20,family = "sans", face="bold"),
        legend.text =element_text(size=15,family="sans", face="bold"),
        legend.position = c(0.75, 0.95),
        legend.title=element_blank())

####----Figure 2: summary of all----####
layout <- "
AB
CD
"

MainPlot = Fig2A + Fig2B + Fig2C + Fig2D +  
  plot_layout(design = layout) + plot_annotation(tag_levels = 'A')&
  theme(plot.tag = element_text(size=24,family="sans", face="bold"))

MainPlot


ggsave("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Results/Week34_2024.11.4_ScienceResponse/Figure2_Metrics_Overview_UpdateScaleAnalysis.pdf", plot=MainPlot, width=18, height=16)


