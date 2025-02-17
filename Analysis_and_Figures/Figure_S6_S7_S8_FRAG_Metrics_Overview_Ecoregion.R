rm(list=ls())

library(rvest)
library(tidyverse)
library(tictoc)
library(raster)
library(gridExtra)
library(patchwork)


frag_df_full_glad <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/frag_df_Full_glad_extent_5m.rds")

# frag_df_full_glad <- frag_df_full_glad%>%mutate(mpa=cover*pixelArea/np, 
#                                                 mpa_2020=cover_2020*pixelArea/np_2020,
#                                                 par=ed/(cover*pixelArea),
#                                                 par_2020=ed_2020/(cover_2020*pixelArea))

c("ai", "pci", "clumpy", "nlsi", "si", "par", "AFI")

frag_df_full_glad <- frag_df_full_glad%>%mutate(ai_diff=ai_2020-ai,
                                                pci_diff=pci_2020-pci,
                                                clumpy_diff=clumpy_2020-clumpy,
                                                si_diff=si_2020-si,
                                                par_diff=par_2020-par,
                                                enn_diff=enn_2020-enn,
                                                # parmn_diff=parmn_2020-parmn,
                                                pladj_diff=pladj_2020-pladj
                                                )


frag_df_ecoregion_biome <- unique(frag_df_full_glad[,c("Ecoregion","Mega_Biome")])
frag_df_ecoregion_biome <- frag_df_ecoregion_biome%>%drop_na()

TT <- table(frag_df_ecoregion_biome$Ecoregion)
ecoregion_filtered <- as.numeric(names(TT)[TT==1])
frag_df_ecoregion_biome <- frag_df_ecoregion_biome[frag_df_ecoregion_biome$Ecoregion%in%ecoregion_filtered,]


# frag_df_sub_glad <- frag_df_full_glad[frag_df_full_glad$cover>=0.3,]
# 
# lm1 <- lm(cover~mpa, data = frag_df_sub_glad)
# summary(lm1)
# 
# lm2 <- lm(cover~ed, data = frag_df_sub_glad)
# summary(lm2)
# 
# lm3 <- lm(cover~np, data = frag_df_sub_glad)
# summary(lm3)


frag_diff_corr_ecoregion <- function(var, ecoregion=0, forestT=0.3, fragT=0, coverDiffT=0, fpT=30){ # fpT: Threshold for number of forest pixels in a ecoregion
  
  # ecoregion=0
  # forestT=0.3
  # fragT=0
  # coverDiffT=0
  
  
  var_diff <- paste0(var, "_diff")
  
  frag_df_full_glad_sub <- frag_df_full_glad[(!is.na(frag_df_full_glad[,var_diff]))&!frag_df_full_glad[,var_diff]==0&frag_df_full_glad$cover>=forestT,]
  
  frag_df_full_glad_sub <- frag_df_full_glad_sub[frag_df_full_glad_sub$Ecoregion==ecoregion,]
  
  
  corr <- NA
  corr_change <- NA
  R2 <- NA
  R2_change <- NA
  abs_corr_change <- NA
  
  
  if(nrow(frag_df_full_glad_sub)>fpT){
    corr <- cor(frag_df_full_glad_sub[,var], frag_df_full_glad_sub$cover, use="complete.obs")
    corr_change <- cor(frag_df_full_glad_sub[,var_diff], frag_df_full_glad_sub$cover_diff, use="complete.obs")
    
    lm1 <- lm(frag_df_full_glad_sub[,var]~frag_df_full_glad_sub$cover)
    R2 <- summary(lm1)$adj.r.squared
    
    lm1_change <- lm(frag_df_full_glad_sub[,var_diff]~frag_df_full_glad_sub$cover_diff)
    R2_change <- summary(lm1_change)$adj.r.squared
    
    abs_corr_change <- abs(corr_change)
  }
  
  return(list(corr=corr,corr_change=corr_change, R2=R2, R2_change=R2_change, abs_corr_change=abs_corr_change))
  
}

# frag_vec <- c("cfi", "clumpy", "lpi", "nlsi","ed","np","ldi","ffi","mpa", 
#               "tca")

# frag_vec <- c("FFI", "CFI", "lpi", "ldi", "tca", "mpa", "np", "ed")
# frag_vec <- c("ai", "pci", "clumpy", "nlsi", "si", "par")

frag_vec <- c("AFI", "enn", "pladj")

frag_df_full_glad <- frag_df_full_glad[frag_df_full_glad$cover>=0.3,]
Ecoregion <- unique(frag_df_full_glad$Ecoregion)

BB <- table(frag_df_full_glad$Ecoregion)
CC <- BB[BB >= 50]
DD <- as.numeric(names(CC))

# frag_df_full_glad$FFI <- frag_df_full_glad$FFI_2000

frag_corr_df <- NULL
for(k in 1:length(DD)){
  for(i in 1:length(frag_vec)){
    tic()
    frag_stat_sub <- frag_diff_corr_ecoregion(var=frag_vec[i],  ecoregion=DD[k])
    
    frag_corr_df <- rbind(frag_corr_df, data.frame(cbind(abs_corr_change=frag_stat_sub$abs_corr_change,
                                                         corr=frag_stat_sub$corr, 
                                                         corr_change=frag_stat_sub$corr_change,
                                                         R2=frag_stat_sub$R2,
                                                         R2_change=frag_stat_sub$R2_change,
                                                         frag=frag_vec[i],
                                                         ecoregion=DD[k])))
    toc()
  }
}


frag_corr_df <- frag_corr_df%>%mutate(corr=as.numeric(corr),
                                      corr_change=as.numeric(corr_change),
                                      R2=as.numeric(R2),
                                      R2_change=as.numeric(R2_change),
                                      abs_corr_change=as.numeric(abs_corr_change))



saveRDS(frag_corr_df, "C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Outcome/2024.10.24_ScienceResponse/AFI_updated_corr_R2_df_0.3cover_0threshold_0diffT_50forestPixels.rds")

# frag_corr_df <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Outcome/2024.10.24_ScienceResponse/AFI_corr_R2_df_0.3cover_0threshold_0diffT_50forestPixels.rds")

frag_corr_df_0 <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Outcome/2024.10.24_ScienceResponse/AFI_updated_corr_R2_df_0.3cover_0threshold_0diffT_50forestPixels.rds")
# # frag_corr_df_1 <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Outcome/2024.10.24_ScienceResponse/frag_corr_R2_df_0.3cover_0threshold_0diffT_50forestPixels.rds")
# # frag_corr_df_2 <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Outcome/2024.10.24_ScienceResponse/fragExtra_corr_R2_df_0.3cover_0threshold_0diffT_50forestPixels.rds")

# frag_corr_df <- rbind(frag_corr_df_0, frag_corr_df_1, frag_corr_df_2)
# saveRDS(frag_corr_df, "C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Outcome/2024.10.24_ScienceResponse/fragAll_corr_R2_df_0.3cover_0threshold_0diffT_50forestPixels.rds")


frag_corr_df <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Outcome/2024.10.24_ScienceResponse/fragAll_corr_R2_df_0.3cover_0threshold_0diffT_50forestPixels.rds")

# frag_corr_df <- frag_corr_df[frag_corr_df$frag!="AFI",]
frag_corr_df <- rbind(frag_corr_df, frag_corr_df_0)

saveRDS(frag_corr_df, "C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Outcome/2024.10.24_ScienceResponse/fragAll_corr_R2_df_0.3cover_0threshold_0diffT_50forestPixels.rds")


## compute the range of R2 and pearson correlation for different fragmentation metrics
frag_corr_df_corr_R2_summary <- frag_corr_df%>%group_by(frag)%>%summarise(corr_max=max(corr, na.rm = T),
                                                                          corr_min=min(corr, na.rm = T),
                                                                          corr_change_max=max(corr_change, na.rm = T),
                                                                          corr_change_min=min(corr_change, na.rm = T),
                                                                          R2_max=max(R2, na.rm = T),
                                                                          R2_min=min(R2, na.rm = T),
                                                                          R2_change_max=max(R2_change, na.rm = T),
                                                                          R2_change_min=min(R2_change, na.rm = T))



frag_corr_df_CFI <- frag_corr_df[frag_corr_df$frag=="CFI",]
hist(frag_corr_df_CFI$R2_change, breaks=100)

frag_corr_df_LDI <- frag_corr_df[frag_corr_df$frag=="ldi",]
hist(frag_corr_df_LDI$R2_change, breaks=100)


quantile(frag_corr_df_CFI$R2_change)

hist(frag_corr_df_CFI$abs_corr_change, breaks=100)

frag_corr_df_FFI <- frag_corr_df[frag_corr_df$frag=="FFI",]
hist(frag_corr_df_FFI$R2_change, breaks=100)
hist(frag_corr_df_FFI$corr_change, breaks=100)



## AFI histograms----
Fig0A <- ggplot(frag_corr_df[frag_corr_df$frag=="enn",], aes(x=R2_change)) +
  geom_histogram(binwidth = 0.05, fill = "orange", color = "black", alpha=0.5) +
  labs(x = "R2", y = "Frequency")+
  scale_y_continuous(expand=c(0,0), limits = c(0, 300))+
  scale_x_continuous(limits = c(0, 1))+
  theme_classic()+
  ggtitle(expression(Delta * "ENN" ~ "~" * Delta * "Cover")) +
  theme(title = element_text(size=18,family = "sans", face="bold"),
        axis.text   =element_text(size=18,family="sans", face="bold"),
        # axis.title.x=element_blank(),
        axis.title.y=element_text(size=18,family="sans", face="bold"),
        legend.text =element_text(size=18,family="sans", face="bold"),
        # legend.position = "none",
        legend.title=element_text(size=18,family="sans", face="bold"))

Fig0B <- ggplot(frag_corr_df[frag_corr_df$frag=="ai",], aes(x=R2_change)) +
  geom_histogram(binwidth = 0.05, fill = "orange", color = "black", alpha=0.5) +
  labs(x = "R2", y = "Frequency")+
  scale_y_continuous(expand=c(0,0), limits = c(0, 300))+
  scale_x_continuous(limits = c(0, 1))+
  theme_classic()+
  ggtitle(expression(Delta * "AI" ~ "~" * Delta * "Cover")) +
  theme(title = element_text(size=18,family = "sans", face="bold"),
        axis.text   =element_text(size=18,family="sans", face="bold"),
        # axis.title.x=element_blank(),
        axis.title.y=element_text(size=18,family="sans", face="bold"),
        legend.text =element_text(size=18,family="sans", face="bold"),
        # legend.position = "none",
        legend.title=element_text(size=18,family="sans", face="bold"))



Fig0C <- ggplot(frag_corr_df[frag_corr_df$frag=="pladj",], aes(x=R2_change)) +
  geom_histogram(binwidth = 0.05, fill = "orange", color = "black", alpha=0.5) +
  labs(x = "R2", y = "Frequency")+
  scale_y_continuous(expand=c(0,0), limits = c(0, 300))+
  scale_x_continuous(limits = c(0, 1))+
  theme_classic()+
  ggtitle(expression(Delta * "PLADJ" ~ "~" * Delta * "Cover")) +
  theme(title = element_text(size=18,family = "sans", face="bold"),
        axis.text   =element_text(size=18,family="sans", face="bold"),
        # axis.title.x=element_blank(),
        axis.title.y=element_text(size=18,family="sans", face="bold"),
        legend.text =element_text(size=18,family="sans", face="bold"),
        # legend.position = "none",
        legend.title=element_text(size=18,family="sans", face="bold"))


## AFI frag histograms
Fig0D <- ggplot(frag_corr_df[frag_corr_df$frag=="AFI",], aes(x=R2_change)) +
  geom_histogram(binwidth = 0.05, fill = "orange", color = "black", alpha=0.8) +
  labs(x = "R2", y = "Frequency")+
  scale_y_continuous(expand=c(0,0), limits = c(0, 300))+
  scale_x_continuous(limits = c(0, 1))+
  theme_classic()+
  ggtitle(expression(Delta * "AFI" ~ "~" * Delta * "Cover")) +
  theme(title = element_text(size=18,family = "sans", face="bold"),
        axis.text   =element_text(size=18,family="sans", face="bold"),
        # axis.title.x=element_blank(),
        axis.title.y=element_text(size=18,family="sans", face="bold"),
        legend.text =element_text(size=18,family="sans", face="bold"),
        # legend.position = "none",
        legend.title=element_text(size=18,family="sans", face="bold"))
layout <- "
AB
CD
"

#Merge plots
MainPlot0 = Fig0D + Fig0A + Fig0B + Fig0C + 
  plot_layout(design = layout) + plot_annotation(tag_levels = 'A')&
  theme(plot.tag = element_text(face = 'bold'))

MainPlot0

ggsave("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Results/Week34_2024.11.4_ScienceResponse/FigureSX_AFI_R2_Histogram.pdf", plot=MainPlot0, width=10, height=8)



## CFI histograms----

Fig1A <- ggplot(frag_corr_df[frag_corr_df$frag=="CFI",], aes(x=R2_change)) +
  geom_histogram(binwidth = 0.05, fill = "#ca0020", color = "black", alpha=0.8) +
  labs(x = "R2", y = "Frequency")+
  scale_y_continuous(expand=c(0,0), limits = c(0, 300))+
  scale_x_continuous(limits = c(0, 1))+
  theme_classic()+
  ggtitle(expression(Delta * "CFI" ~ "~" * Delta * "Cover")) +
  theme(title = element_text(size=18,family = "sans", face="bold"),
        axis.text   =element_text(size=18,family="sans", face="bold"),
        # axis.title.x=element_blank(),
        axis.title.y=element_text(size=18,family="sans", face="bold"),
        legend.text =element_text(size=18,family="sans", face="bold"),
        # legend.position = "none",
        legend.title=element_text(size=18,family="sans", face="bold"))

Fig1B <- ggplot(frag_corr_df[frag_corr_df$frag=="tca",], aes(x=R2_change)) +
  geom_histogram(binwidth = 0.05, fill = "#ca0020", color = "black", alpha=0.5) +
  labs(x = "R2", y = "Frequency")+
  scale_y_continuous(expand=c(0,0), limits = c(0, 300))+
  scale_x_continuous(limits = c(0, 1))+
  theme_classic()+
  ggtitle(expression(Delta * "TCA" ~ "~" * Delta * "Cover")) +
  theme(title = element_text(size=18,family = "sans", face="bold"),
        axis.text   =element_text(size=18,family="sans", face="bold"),
        # axis.title.x=element_blank(),
        axis.title.y=element_text(size=18,family="sans", face="bold"),
        legend.text =element_text(size=18,family="sans", face="bold"),
        # legend.position = "none",
        legend.title=element_text(size=18,family="sans", face="bold"))

Fig1C <- ggplot(frag_corr_df[frag_corr_df$frag=="ldi",], aes(x=R2_change)) +
  geom_histogram(binwidth = 0.05, fill = "#ca0020", color = "black", alpha=0.5) +
  labs(x = "R2", y = "Frequency")+
  scale_y_continuous(expand=c(0,0), limits = c(0, 300))+
  scale_x_continuous(limits = c(0, 1))+
  theme_classic()+
  ggtitle(expression(Delta * "LDI" ~ "~" * Delta * "Cover")) +
  theme(title = element_text(size=18,family = "sans", face="bold"),
        axis.text   =element_text(size=18,family="sans", face="bold"),
        # axis.title.x=element_blank(),
        axis.title.y=element_text(size=18,family="sans", face="bold"),
        legend.text =element_text(size=18,family="sans", face="bold"),
        # legend.position = "none",
        legend.title=element_text(size=18,family="sans", face="bold"))

Fig1D <- ggplot(frag_corr_df[frag_corr_df$frag=="lpi",], aes(x=R2_change)) +
  geom_histogram(binwidth = 0.05, fill = "#ca0020", color = "black", alpha=0.5) +
  labs(x = "R2", y = "Frequency")+
  scale_y_continuous(expand=c(0,0), limits = c(0, 300))+
  scale_x_continuous(limits = c(0, 1))+
  theme_classic()+
  ggtitle(expression(Delta * "LPI" ~ "~" * Delta * "Cover")) +
  theme(title = element_text(size=18,family = "sans", face="bold"),
        axis.text   =element_text(size=18,family="sans", face="bold"),
        # axis.title.x=element_blank(),
        axis.title.y=element_text(size=18,family="sans", face="bold"),
        legend.text =element_text(size=18,family="sans", face="bold"),
        # legend.position = "none",
        legend.title=element_text(size=18,family="sans", face="bold"))


layout <- "
AB
CD
"

#Merge plots
MainPlot1 = Fig1A + Fig1B + Fig1C + Fig1D + 
  plot_layout(design = layout) + plot_annotation(tag_levels = 'A')&
  theme(plot.tag = element_text(face = 'bold'))

MainPlot1

ggsave("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Results/Week34_2024.11.4_ScienceResponse/FigureSX_CFI_R2_Histogram.pdf", plot=MainPlot1, width=10, height=8)


## FFI histograms----
Fig2A <- ggplot(frag_corr_df[frag_corr_df$frag=="FFI",], aes(x=R2_change)) +
  geom_histogram(binwidth = 0.05, fill = "#0571b0", color = "black", alpha=0.8) +
  labs(x = "R2", y = "Frequency")+
  scale_y_continuous(expand=c(0,0), limits = c(0, 300))+
  scale_x_continuous(limits = c(0, 1))+
  theme_classic()+
  ggtitle(expression(Delta * "FFI" ~ "~" * Delta * "Cover")) +
  theme(title = element_text(size=18,family = "sans", face="bold"),
        axis.text   =element_text(size=18,family="sans", face="bold"),
        # axis.title.x=element_blank(),
        axis.title.y=element_text(size=18,family="sans", face="bold"),
        legend.text =element_text(size=18,family="sans", face="bold"),
        # legend.position = "none",
        legend.title=element_text(size=18,family="sans", face="bold"))

Fig2B <- ggplot(frag_corr_df[frag_corr_df$frag=="np",], aes(x=R2_change)) +
  geom_histogram(binwidth = 0.05, fill = "#0571b0", color = "black", alpha=0.5) +
  labs(x = "R2", y = "Frequency")+
  scale_y_continuous(expand=c(0,0), limits = c(0, 300))+
  scale_x_continuous(limits = c(0, 1))+
  theme_classic()+
  ggtitle(expression(Delta * "NP" ~ "~" * Delta * "Cover")) +
  theme(title = element_text(size=18,family = "sans", face="bold"),
        axis.text   =element_text(size=18,family="sans", face="bold"),
        # axis.title.x=element_blank(),
        axis.title.y=element_text(size=18,family="sans", face="bold"),
        legend.text =element_text(size=18,family="sans", face="bold"),
        # legend.position = "none",
        legend.title=element_text(size=18,family="sans", face="bold"))

Fig2C <- ggplot(frag_corr_df[frag_corr_df$frag=="ed",], aes(x=R2_change)) +
  geom_histogram(binwidth = 0.05, fill = "#0571b0", color = "black", alpha=0.5) +
  labs(x = "R2", y = "Frequency")+
  scale_y_continuous(expand=c(0,0), limits = c(0, 300))+
  scale_x_continuous(limits = c(0, 1))+
  theme_classic()+
  ggtitle(expression(Delta * "ED" ~ "~" * Delta * "Cover")) +
  theme(title = element_text(size=18,family = "sans", face="bold"),
        axis.text   =element_text(size=18,family="sans", face="bold"),
        # axis.title.x=element_blank(),
        axis.title.y=element_text(size=18,family="sans", face="bold"),
        legend.text =element_text(size=18,family="sans", face="bold"),
        # legend.position = "none",
        legend.title=element_text(size=18,family="sans", face="bold"))

Fig2D <- ggplot(frag_corr_df[frag_corr_df$frag=="mpa",], aes(x=R2_change)) +
  geom_histogram(binwidth = 0.05, fill = "#0571b0", color = "black", alpha=0.5) +
  labs(x = "R2", y = "Frequency")+
  scale_y_continuous(expand=c(0,0), limits = c(0, 300))+
  scale_x_continuous(limits = c(0, 1))+
  theme_classic()+
  ggtitle(expression(Delta * "MPA" ~ "~" * Delta * "Cover")) +
  theme(title = element_text(size=18,family = "sans", face="bold"),
        axis.text   =element_text(size=18,family="sans", face="bold"),
        # axis.title.x=element_blank(),
        axis.title.y=element_text(size=18,family="sans", face="bold"),
        legend.text =element_text(size=18,family="sans", face="bold"),
        # legend.position = "none",
        legend.title=element_text(size=18,family="sans", face="bold"))


layout <- "
AB
CD
"

#Merge plots
MainPlot2 = Fig2A + Fig2B + Fig2C + Fig2D + 
  plot_layout(design = layout) + plot_annotation(tag_levels = 'A')&
  theme(plot.tag = element_text(face = 'bold'))

MainPlot2

ggsave("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Results/Week34_2024.11.4_ScienceResponse/FigureSX_FFI_R2_Histogram.pdf", plot=MainPlot2, width=10, height=8)
