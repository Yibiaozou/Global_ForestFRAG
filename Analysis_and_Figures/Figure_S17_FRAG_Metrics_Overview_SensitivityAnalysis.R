rm(list=ls())

library(rvest)
library(tidyverse)
library(tictoc)
library(raster)
library(gridExtra)
library(patchwork)
library(landscapemetrics)
library(landscapeR)


####----Figure SX1: show proportion of fragmented forest in percentage and area based on different metrics across different cover threshold----####
### 10% threshold
frag_stat_df <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Outcome/2024.10.24_ScienceResponse/fragThreeDim_stat_df_0.1cover_0threshold_0diffT_NStandard_Metrics_AreaCorrection.rds")
frag_stat_df$frag <- as.character(frag_stat_df$frag)


frag_stat_df$frag[frag_stat_df$frag=="lpi"] <- "LPI"
frag_stat_df$frag[frag_stat_df$frag=="ldi"] <- "LDI"
frag_stat_df$frag[frag_stat_df$frag=="tca"] <- "TCA"
frag_stat_df$frag[frag_stat_df$frag=="ed"] <- "ED"
frag_stat_df$frag[frag_stat_df$frag=="np"] <- "NP"
frag_stat_df$frag[frag_stat_df$frag=="mpa"] <- "MPA"
frag_stat_df$frag[frag_stat_df$frag=="ai"] <- "AI"
frag_stat_df$frag[frag_stat_df$frag=="enn"] <- "ENN"
frag_stat_df$frag[frag_stat_df$frag=="pladj"] <- "PLADJ"


frag_stat_df_select <- frag_stat_df[frag_stat_df$frag%in%c("CFI","AFI","FFI"),]
frag_stat_df_select <- frag_stat_df_select%>%mutate(
  biome=fct_relevel(biome,"All","Tropical","Temperate","Boreal"),
  frag=fct_relevel(frag,"CFI","AFI","FFI")
)


Fig1A <- ggplot(frag_stat_df_select, aes(x=biome, y=pp, fill=frag))+
  geom_bar(position="dodge", stat = "identity", alpha=0.5, color="black")+
  # geom_errorbar(aes(ymin = pp_mean-2*pp_se, ymax = pp_mean+2*pp_se), width=0.3,linewidth=1,position=position_dodge(.9)) +
  # ylim(c(0,60))+
  # ggtitle("Boreal")+
  scale_fill_manual(values=c( "#ca0020","orange","#0571b0"))+
  ggtitle("10%")+
  xlab("Biome")+
  ylab("Increased fragmentation (%)")+
  # geom_hline(yintercept = baseline, color="red", linetype="dashed",linewidth=1)+
  geom_hline(yintercept = 50, color="grey50", linetype="dashed",linewidth=1)+
  scale_y_continuous(expand=c(0,0), limits = c(0,65))+
  theme_classic()+
  theme(
    # title = element_text(size=20,family = "sans", face="bold"),
    # axis.text.x=element_text(size=12,family = "sans", face="bold", angle=60, vjust = 1, hjust=1),
    axis.text.x=element_text(size=15,family = "sans", face="bold", angle=0),
    axis.text.y=element_text(size=15,family = "sans", face="bold"),
    axis.title.x=element_text(size=20,family = "sans", face="bold"),
    axis.title.y=element_text(size=20,family = "sans", face="bold"),
    # legend.position = "none",
    legend.position = c(0.8, 0.95),
    legend.text = element_text(colour = "black", size = 15,family = "sans", face="bold"),
    legend.title = element_blank())

### 20% threshold
frag_stat_df <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Outcome/2024.10.24_ScienceResponse/fragThreeDim_stat_df_0.2cover_0threshold_0diffT_NStandard_Metrics_AreaCorrection.rds")
frag_stat_df$frag <- as.character(frag_stat_df$frag)


frag_stat_df$frag[frag_stat_df$frag=="lpi"] <- "LPI"
frag_stat_df$frag[frag_stat_df$frag=="ldi"] <- "LDI"
frag_stat_df$frag[frag_stat_df$frag=="tca"] <- "TCA"
frag_stat_df$frag[frag_stat_df$frag=="ed"] <- "ED"
frag_stat_df$frag[frag_stat_df$frag=="np"] <- "NP"
frag_stat_df$frag[frag_stat_df$frag=="mpa"] <- "MPA"
frag_stat_df$frag[frag_stat_df$frag=="ai"] <- "AI"
frag_stat_df$frag[frag_stat_df$frag=="enn"] <- "ENN"
frag_stat_df$frag[frag_stat_df$frag=="pladj"] <- "PLADJ"


frag_stat_df_select <- frag_stat_df[frag_stat_df$frag%in%c("CFI","AFI","FFI"),]
frag_stat_df_select <- frag_stat_df_select%>%mutate(
  biome=fct_relevel(biome,"All","Tropical","Temperate","Boreal"),
  frag=fct_relevel(frag,"CFI","AFI","FFI")
)


Fig1B <- ggplot(frag_stat_df_select, aes(x=biome, y=pp, fill=frag))+
  geom_bar(position="dodge", stat = "identity", alpha=0.5, color="black")+
  # geom_errorbar(aes(ymin = pp_mean-2*pp_se, ymax = pp_mean+2*pp_se), width=0.3,linewidth=1,position=position_dodge(.9)) +
  # ylim(c(0,60))+
  # ggtitle("Boreal")+
  scale_fill_manual(values=c( "#ca0020","orange","#0571b0"))+
  ggtitle("20%")+
  xlab("Biome")+
  ylab("Increased fragmentation (%)")+
  # geom_hline(yintercept = baseline, color="red", linetype="dashed",linewidth=1)+
  geom_hline(yintercept = 50, color="grey50", linetype="dashed",linewidth=1)+
  scale_y_continuous(expand=c(0,0), limits = c(0,65))+
  theme_classic()+
  theme(
    # title = element_text(size=20,family = "sans", face="bold"),
    # axis.text.x=element_text(size=12,family = "sans", face="bold", angle=60, vjust = 1, hjust=1),
    axis.text.x=element_text(size=15,family = "sans", face="bold", angle=0),
    axis.text.y=element_text(size=15,family = "sans", face="bold"),
    axis.title.x=element_text(size=20,family = "sans", face="bold"),
    axis.title.y=element_text(size=20,family = "sans", face="bold"),
    # legend.position = "none",
    legend.position = c(0.8, 0.95),
    legend.text = element_text(colour = "black", size = 15,family = "sans", face="bold"),
    legend.title = element_blank())


### 40% threshold
frag_stat_df <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Outcome/2024.10.24_ScienceResponse/fragThreeDim_stat_df_0.4cover_0threshold_0diffT_NStandard_Metrics_AreaCorrection.rds")
frag_stat_df$frag <- as.character(frag_stat_df$frag)


frag_stat_df$frag[frag_stat_df$frag=="lpi"] <- "LPI"
frag_stat_df$frag[frag_stat_df$frag=="ldi"] <- "LDI"
frag_stat_df$frag[frag_stat_df$frag=="tca"] <- "TCA"
frag_stat_df$frag[frag_stat_df$frag=="ed"] <- "ED"
frag_stat_df$frag[frag_stat_df$frag=="np"] <- "NP"
frag_stat_df$frag[frag_stat_df$frag=="mpa"] <- "MPA"
frag_stat_df$frag[frag_stat_df$frag=="ai"] <- "AI"
frag_stat_df$frag[frag_stat_df$frag=="enn"] <- "ENN"
frag_stat_df$frag[frag_stat_df$frag=="pladj"] <- "PLADJ"


frag_stat_df_select <- frag_stat_df[frag_stat_df$frag%in%c("CFI","AFI","FFI"),]
frag_stat_df_select <- frag_stat_df_select%>%mutate(
  biome=fct_relevel(biome,"All","Tropical","Temperate","Boreal"),
  frag=fct_relevel(frag,"CFI","AFI","FFI")
)


Fig1C <- ggplot(frag_stat_df_select, aes(x=biome, y=pp, fill=frag))+
  geom_bar(position="dodge", stat = "identity", alpha=0.5, color="black")+
  # geom_errorbar(aes(ymin = pp_mean-2*pp_se, ymax = pp_mean+2*pp_se), width=0.3,linewidth=1,position=position_dodge(.9)) +
  # ylim(c(0,60))+
  # ggtitle("Boreal")+
  scale_fill_manual(values=c( "#ca0020","orange","#0571b0"))+
  ggtitle("40%")+
  xlab("Biome")+
  ylab("Increased fragmentation (%)")+
  # geom_hline(yintercept = baseline, color="red", linetype="dashed",linewidth=1)+
  geom_hline(yintercept = 50, color="grey50", linetype="dashed",linewidth=1)+
  scale_y_continuous(expand=c(0,0), limits = c(0,65))+
  theme_classic()+
  theme(
    # title = element_text(size=20,family = "sans", face="bold"),
    # axis.text.x=element_text(size=12,family = "sans", face="bold", angle=60, vjust = 1, hjust=1),
    axis.text.x=element_text(size=15,family = "sans", face="bold", angle=0),
    axis.text.y=element_text(size=15,family = "sans", face="bold"),
    axis.title.x=element_text(size=20,family = "sans", face="bold"),
    axis.title.y=element_text(size=20,family = "sans", face="bold"),
    # legend.position = "none",
    legend.position = c(0.8, 0.95),
    legend.text = element_text(colour = "black", size = 15,family = "sans", face="bold"),
    legend.title = element_blank())

layout <- "
ABC
"

MainPlot = Fig1A + Fig1B + Fig1C + 
  plot_layout(design = layout) + plot_annotation(tag_levels = 'A')&
  theme(plot.tag = element_text(size=20, family = "sans", face = 'bold'))

MainPlot

ggsave("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Results/Week34_2024.11.4_ScienceResponse/FigureSX_MetricsOverview_AcrossCoverThreshold.pdf", plot=MainPlot, width=20, height=6)


####----Figure SX2: show proportion of fragmented forest in percentage and area based on different metrics across different spatial scale----####

### 10km
frag_stat_df <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Outcome/2024.10.24_ScienceResponse/frag_stat_df_10kmRes_0.3cover_0threshold_0diffT.rds")
frag_stat_df$frag <- as.character(frag_stat_df$frag)


frag_stat_df$frag[frag_stat_df$frag=="lpi"] <- "LPI"
frag_stat_df$frag[frag_stat_df$frag=="ldi"] <- "LDI"
frag_stat_df$frag[frag_stat_df$frag=="tca"] <- "TCA"
frag_stat_df$frag[frag_stat_df$frag=="ed"] <- "ED"
frag_stat_df$frag[frag_stat_df$frag=="np"] <- "NP"
frag_stat_df$frag[frag_stat_df$frag=="mpa"] <- "MPA"
frag_stat_df$frag[frag_stat_df$frag=="ai"] <- "AI"
frag_stat_df$frag[frag_stat_df$frag=="enn"] <- "ENN"
frag_stat_df$frag[frag_stat_df$frag=="pladj"] <- "PLADJ"


frag_stat_df_select <- frag_stat_df[frag_stat_df$frag%in%c("CFI","AFI","FFI"),]
frag_stat_df_select <- frag_stat_df_select%>%mutate(
  biome=fct_relevel(biome,"All","Tropical","Temperate","Boreal"),
  frag=fct_relevel(frag,"CFI","AFI","FFI")
)


Fig2A <- ggplot(frag_stat_df_select, aes(x=biome, y=pp, fill=frag))+
  geom_bar(position="dodge", stat = "identity", alpha=0.5, color="black")+
  # geom_errorbar(aes(ymin = pp_mean-2*pp_se, ymax = pp_mean+2*pp_se), width=0.3,linewidth=1,position=position_dodge(.9)) +
  # ylim(c(0,60))+
  # ggtitle("Boreal")+
  scale_fill_manual(values=c( "#ca0020","orange","#0571b0"))+
  ggtitle("10km")+
  xlab("Biome")+
  ylab("Increased fragmentation (%)")+
  # geom_hline(yintercept = baseline, color="red", linetype="dashed",linewidth=1)+
  geom_hline(yintercept = 50, color="grey50", linetype="dashed",linewidth=1)+
  scale_y_continuous(expand=c(0,0), limits = c(0,100))+
  theme_classic()+
  theme(
    # title = element_text(size=20,family = "sans", face="bold"),
    # axis.text.x=element_text(size=12,family = "sans", face="bold", angle=60, vjust = 1, hjust=1),
    axis.text.x=element_text(size=15,family = "sans", face="bold", angle=0),
    axis.text.y=element_text(size=15,family = "sans", face="bold"),
    axis.title.x=element_text(size=20,family = "sans", face="bold"),
    axis.title.y=element_text(size=20,family = "sans", face="bold"),
    # legend.position = "none",
    legend.position = c(0.8, 0.95),
    legend.text = element_text(colour = "black", size = 15,family = "sans", face="bold"),
    legend.title = element_blank())

### 20km
frag_stat_df <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Outcome/2024.10.24_ScienceResponse/frag_stat_df_20kmRes_0.3cover_0threshold_0diffT.rds")
frag_stat_df$frag <- as.character(frag_stat_df$frag)


frag_stat_df$frag[frag_stat_df$frag=="lpi"] <- "LPI"
frag_stat_df$frag[frag_stat_df$frag=="ldi"] <- "LDI"
frag_stat_df$frag[frag_stat_df$frag=="tca"] <- "TCA"
frag_stat_df$frag[frag_stat_df$frag=="ed"] <- "ED"
frag_stat_df$frag[frag_stat_df$frag=="np"] <- "NP"
frag_stat_df$frag[frag_stat_df$frag=="mpa"] <- "MPA"
frag_stat_df$frag[frag_stat_df$frag=="ai"] <- "AI"
frag_stat_df$frag[frag_stat_df$frag=="enn"] <- "ENN"
frag_stat_df$frag[frag_stat_df$frag=="pladj"] <- "PLADJ"


frag_stat_df_select <- frag_stat_df[frag_stat_df$frag%in%c("CFI","AFI","FFI"),]
frag_stat_df_select <- frag_stat_df_select%>%mutate(
  biome=fct_relevel(biome,"All","Tropical","Temperate","Boreal"),
  frag=fct_relevel(frag,"CFI","AFI","FFI")
)


Fig2B <- ggplot(frag_stat_df_select, aes(x=biome, y=pp, fill=frag))+
  geom_bar(position="dodge", stat = "identity", alpha=0.5, color="black")+
  # geom_errorbar(aes(ymin = pp_mean-2*pp_se, ymax = pp_mean+2*pp_se), width=0.3,linewidth=1,position=position_dodge(.9)) +
  # ylim(c(0,60))+
  # ggtitle("Boreal")+
  scale_fill_manual(values=c( "#ca0020","orange","#0571b0"))+
  ggtitle("20km")+
  xlab("Biome")+
  ylab("Increased fragmentation (%)")+
  # geom_hline(yintercept = baseline, color="red", linetype="dashed",linewidth=1)+
  geom_hline(yintercept = 50, color="grey50", linetype="dashed",linewidth=1)+
  scale_y_continuous(expand=c(0,0), limits = c(0,100))+
  theme_classic()+
  theme(
    # title = element_text(size=20,family = "sans", face="bold"),
    # axis.text.x=element_text(size=12,family = "sans", face="bold", angle=60, vjust = 1, hjust=1),
    axis.text.x=element_text(size=15,family = "sans", face="bold", angle=0),
    axis.text.y=element_text(size=15,family = "sans", face="bold"),
    axis.title.x=element_text(size=20,family = "sans", face="bold"),
    axis.title.y=element_text(size=20,family = "sans", face="bold"),
    # legend.position = "none",
    legend.position = c(0.8, 0.95),
    legend.text = element_text(colour = "black", size = 15,family = "sans", face="bold"),
    legend.title = element_blank())

### 60km
frag_stat_df <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Outcome/2024.10.24_ScienceResponse/frag_stat_df_60kmRes_0.3cover_0threshold_0diffT.rds")
frag_stat_df$frag <- as.character(frag_stat_df$frag)


frag_stat_df$frag[frag_stat_df$frag=="lpi"] <- "LPI"
frag_stat_df$frag[frag_stat_df$frag=="ldi"] <- "LDI"
frag_stat_df$frag[frag_stat_df$frag=="tca"] <- "TCA"
frag_stat_df$frag[frag_stat_df$frag=="ed"] <- "ED"
frag_stat_df$frag[frag_stat_df$frag=="np"] <- "NP"
frag_stat_df$frag[frag_stat_df$frag=="mpa"] <- "MPA"
frag_stat_df$frag[frag_stat_df$frag=="ai"] <- "AI"
frag_stat_df$frag[frag_stat_df$frag=="enn"] <- "ENN"
frag_stat_df$frag[frag_stat_df$frag=="pladj"] <- "PLADJ"


frag_stat_df_select <- frag_stat_df[frag_stat_df$frag%in%c("CFI","AFI","FFI"),]
frag_stat_df_select <- frag_stat_df_select%>%mutate(
  biome=fct_relevel(biome,"All","Tropical","Temperate","Boreal"),
  frag=fct_relevel(frag,"CFI","AFI","FFI")
)


Fig2C <- ggplot(frag_stat_df_select, aes(x=biome, y=pp, fill=frag))+
  geom_bar(position="dodge", stat = "identity", alpha=0.5, color="black")+
  # geom_errorbar(aes(ymin = pp_mean-2*pp_se, ymax = pp_mean+2*pp_se), width=0.3,linewidth=1,position=position_dodge(.9)) +
  # ylim(c(0,60))+
  # ggtitle("Boreal")+
  scale_fill_manual(values=c( "#ca0020","orange","#0571b0"))+
  ggtitle("60km")+
  xlab("Biome")+
  ylab("Increased fragmentation (%)")+
  # geom_hline(yintercept = baseline, color="red", linetype="dashed",linewidth=1)+
  geom_hline(yintercept = 50, color="grey50", linetype="dashed",linewidth=1)+
  scale_y_continuous(expand=c(0,0), limits = c(0,100))+
  theme_classic()+
  theme(
    # title = element_text(size=20,family = "sans", face="bold"),
    # axis.text.x=element_text(size=12,family = "sans", face="bold", angle=60, vjust = 1, hjust=1),
    axis.text.x=element_text(size=15,family = "sans", face="bold", angle=0),
    axis.text.y=element_text(size=15,family = "sans", face="bold"),
    axis.title.x=element_text(size=20,family = "sans", face="bold"),
    axis.title.y=element_text(size=20,family = "sans", face="bold"),
    # legend.position = "none",
    legend.position = c(0.8, 0.95),
    legend.text = element_text(colour = "black", size = 15,family = "sans", face="bold"),
    legend.title = element_blank())


layout <- "
ABC
"

MainPlot2 = Fig2A + Fig2B + Fig2C + 
  plot_layout(design = layout) + plot_annotation(tag_levels = 'A')&
  theme(plot.tag = element_text(size=20, family = "sans", face = 'bold'))

MainPlot2

ggsave("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Results/Week34_2024.11.4_ScienceResponse/FigureSX_MetricsOverview_AcrossScale.pdf", plot=MainPlot2, width=20, height=6)

