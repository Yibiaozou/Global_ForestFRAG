rm(list=ls())
library(tidyverse)
library(tictoc)
library(raster)
library(ggpubr)
library(patchwork)

frag_df_full_glad <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/frag_df_Full_glad_extent_5m.rds")

NPP_GPP <- brick("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/NPP/MODIS_NPP_5km.tif")

plot(NPP_GPP$NPP_2001)

frag_df_full_glad$NPP_2001 <- raster::extract(NPP_GPP$NPP_2001, frag_df_full_glad[,c("longitude","latitude")])
frag_df_full_glad$NPP_2020 <- raster::extract(NPP_GPP$NPP_2020, frag_df_full_glad[,c("longitude","latitude")])
frag_df_full_glad$GPP_2001 <- raster::extract(NPP_GPP$GPP_2001, frag_df_full_glad[,c("longitude","latitude")])
frag_df_full_glad$GPP_2020 <- raster::extract(NPP_GPP$GPP_2020, frag_df_full_glad[,c("longitude","latitude")])

frag_df_full_glad$NPP_2001 <- frag_df_full_glad$NPP_2001/10000
frag_df_full_glad$NPP_2020 <- frag_df_full_glad$NPP_2020/10000
frag_df_full_glad$GPP_2001 <- frag_df_full_glad$GPP_2001/10000
frag_df_full_glad$GPP_2020 <- frag_df_full_glad$GPP_2020/10000


hist(frag_df_full_glad$NPP_2001, breaks=100)

set.seed(1)

frag_df_full_glad$Mega_Biome[frag_df_full_glad$Mega_Biome=="Mediterranean"] <- "Temperate"

frag_df_sub_glad <- frag_df_full_glad%>%group_by(Mega_Biome)%>%slice_sample(prop=0.001)%>%ungroup()%>%
  dplyr::select(CFI, CFI_2020, AFI, AFI_2020, FFI, FFI_2020, mpcdens, cover,
                NPP_2001, NPP_2020, GPP_2001, GPP_2020, MPC_ST,
         Mega_Biome)%>%drop_na()

frag_df_sub_glad <- frag_df_sub_glad%>%mutate(
  biome=fct_relevel(Mega_Biome,"Tropical","Temperate","Boreal")
)


A1 <- lm(mpcdens~mpc, data=frag_df_full_glad)
summary(A1)

B0 <- lm(MPC_ST~cover, data=frag_df_full_glad)
summary(B0)

B1 <- lm(MPC_ST~cover, data=frag_df_full_glad[frag_df_full_glad$Mega_Biome=="Tropical",])
sB1 <- summary(B1)

FigS0X <- ggplot(frag_df_sub_glad, aes(x=cover*100, y=1-MPC_ST))+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.x.npc = 0.6, label.y.npc = "top", method = "pearson",label.sep = "/n",size = 5)+
  # # stat_regline_equation(
  # #   aes(label =  paste(..adj.rr.label..,  sep = "~`,`~")), label.x.npc = 0.3, label.y.npc = 1, label.sep = "/n", size = 5
  # # ) +
  geom_smooth(method="lm", se=F, linewidth=2, alpha=1, color="red")+
  geom_point(size=4, alpha=0.05)+
  ggtitle("Y2000")+
  xlab("Canopy Cover (%)")+
  ylab("Metapopulation Capacity")+
  # scale_color_manual(values=c("#d7191c", "darkcyan", "#2c7bb6"))+
  # scale_fill_manual(values=c("#d7191c", "darkcyan", "#2c7bb6"))+
  theme_classic()+
  # geom_hline(yintercept = 0.6, color="black", linetype="dashed",linewidth=1)+
  theme(title = element_text(size=15,family = "sans"),
        # axis.text.x=element_text(size=12,family = "sans", face="bold", angle=60, vjust = 1, hjust=1),
        axis.text.x=element_text(size=15,family = "sans", angle=0),
        axis.text.y=element_text(size=15,family = "sans"),
        axis.title.x=element_text(size=20,family = "sans"),
        axis.title.y=element_text(size=20,family = "sans"),
        legend.position = "none",
        legend.text = element_text(colour = "black", size = 12,family = "sans"),
        legend.title = element_blank())


FigS1X <- ggplot(frag_df_sub_glad, aes(x=cover*100, y=MPC_ST))+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.x.npc = 0.6, label.y.npc = "top", method = "pearson",label.sep = "/n",size = 5)+
  # # stat_regline_equation(
  # #   aes(label =  paste(..adj.rr.label..,  sep = "~`,`~")), label.x.npc = 0.3, label.y.npc = 1, label.sep = "/n", size = 5
  # # ) +
  geom_smooth(method="lm", se=F, linewidth=2, alpha=1, color="red")+
  geom_point(size=4, alpha=0.05)+
  ggtitle("Y2000")+
  xlab("Canopy Cover (%)")+
  ylab("Metapopulation Capacity")+
  # scale_color_manual(values=c("#d7191c", "darkcyan", "#2c7bb6"))+
  # scale_fill_manual(values=c("#d7191c", "darkcyan", "#2c7bb6"))+
  theme_classic()+
  # geom_hline(yintercept = 0.6, color="black", linetype="dashed",linewidth=1)+
  theme(title = element_text(size=15,family = "sans"),
        # axis.text.x=element_text(size=12,family = "sans", face="bold", angle=60, vjust = 1, hjust=1),
        axis.text.x=element_text(size=15,family = "sans", angle=0),
        axis.text.y=element_text(size=15,family = "sans"),
        axis.title.x=element_text(size=20,family = "sans"),
        axis.title.y=element_text(size=20,family = "sans"),
        legend.position = "none",
        legend.text = element_text(colour = "black", size = 12,family = "sans"),
        legend.title = element_blank())


FigS1A <- ggplot(frag_df_sub_glad, aes(x=CFI, y=NPP_2001, color=biome, fill=biome))+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.x.npc = 0.3, label.y.npc = "top", method = "pearson",label.sep = "/n", color=c("#d7191c", "darkcyan", "#2c7bb6"),size = 5)+
  geom_smooth(method="lm", se=F, linewidth=2, alpha=1)+
  geom_point(size=4, alpha=0.05)+
  ggtitle("Y2000")+
  xlab("CFI")+
  ylab("NPP (kgC/m2)")+
  scale_color_manual(values=c("#d7191c", "darkcyan", "#2c7bb6"))+
  scale_fill_manual(values=c("#d7191c", "darkcyan", "#2c7bb6"))+
  theme_classic()+
  # geom_hline(yintercept = 0.6, color="black", linetype="dashed",linewidth=1)+
  theme(title = element_text(size=15,family = "sans"),
        # axis.text.x=element_text(size=12,family = "sans", face="bold", angle=60, vjust = 1, hjust=1),
        axis.text.x=element_text(size=15,family = "sans", angle=0),
        axis.text.y=element_text(size=15,family = "sans"),
        axis.title.x=element_text(size=20,family = "sans"),
        axis.title.y=element_text(size=20,family = "sans"),
        legend.position = "none",
        legend.text = element_text(colour = "black", size = 12,family = "sans"),
        legend.title = element_blank())


FigS1B <- ggplot(frag_df_sub_glad, aes(x=CFI_2020, y=NPP_2020, color=biome, fill=biome))+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.x.npc = 0.3, label.y.npc = "top", method = "pearson",label.sep = "/n", color=c("#d7191c", "darkcyan", "#2c7bb6"),size = 5)+
  geom_smooth(method="lm", se=F, linewidth=2, alpha=1)+
  geom_point(size=4, alpha=0.05)+
  ggtitle("Y2020")+
  xlab("CFI")+
  ylab("NPP (kgC/m2)")+
  scale_color_manual(values=c("#d7191c", "darkcyan", "#2c7bb6"))+
  scale_fill_manual(values=c("#d7191c", "darkcyan", "#2c7bb6"))+
  theme_classic()+
  # geom_hline(yintercept = 0.6, color="black", linetype="dashed",linewidth=1)+
  theme(title = element_text(size=15,family = "sans"),
        # axis.text.x=element_text(size=12,family = "sans", face="bold", angle=60, vjust = 1, hjust=1),
        axis.text.x=element_text(size=15,family = "sans", angle=0),
        axis.text.y=element_text(size=15,family = "sans"),
        axis.title.x=element_text(size=20,family = "sans"),
        axis.title.y=element_text(size=20,family = "sans"),
        legend.position = "none",
        legend.text = element_text(colour = "black", size = 12,family = "sans"),
        legend.title = element_blank())

FigS1C <- ggplot(frag_df_sub_glad, aes(x=CFI, y=(1-MPC_ST), color=biome, fill=biome))+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.x.npc = 0.3, label.y.npc = "top", method = "pearson",label.sep = "/n", color=c("#d7191c", "darkcyan", "#2c7bb6"),size = 5)+
  geom_smooth(method="lm", se=F, linewidth=2, alpha=1)+
  geom_point(size=4, alpha=0.05)+
  ggtitle("Y2000")+
  xlab("CFI")+
  ylab("Metapopulation Capacity")+
  scale_color_manual(values=c("#d7191c", "darkcyan", "#2c7bb6"))+
  scale_fill_manual(values=c("#d7191c", "darkcyan", "#2c7bb6"))+
  theme_classic()+
  # geom_hline(yintercept = 0.6, color="black", linetype="dashed",linewidth=1)+
  theme(title = element_text(size=15,family = "sans"),
        # axis.text.x=element_text(size=12,family = "sans", face="bold", angle=60, vjust = 1, hjust=1),
        axis.text.x=element_text(size=15,family = "sans", angle=0),
        axis.text.y=element_text(size=15,family = "sans"),
        axis.title.x=element_text(size=20,family = "sans"),
        axis.title.y=element_text(size=20,family = "sans"),
        legend.position = "none",
        legend.text = element_text(colour = "black", size = 12,family = "sans"),
        legend.title = element_blank())

FigS1D <- ggplot(frag_df_sub_glad, aes(x=AFI, y=NPP_2001, color=biome, fill=biome))+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.x.npc = 0.3, label.y.npc = "top", method = "pearson",label.sep = "/n", color=c("#d7191c", "darkcyan", "#2c7bb6"),size = 5)+
  geom_smooth(method="lm", se=F, linewidth=2, alpha=1)+
  geom_point(size=4, alpha=0.05)+
  ggtitle("Y2000")+
  xlab("AFI")+
  ylab("NPP (kgC/m2)")+
  scale_color_manual(values=c("#d7191c", "darkcyan", "#2c7bb6"))+
  scale_fill_manual(values=c("#d7191c", "darkcyan", "#2c7bb6"))+
  theme_classic()+
  # geom_hline(yintercept = 0.6, color="black", linetype="dashed",linewidth=1)+
  theme(title = element_text(size=15,family = "sans"),
        # axis.text.x=element_text(size=12,family = "sans", face="bold", angle=60, vjust = 1, hjust=1),
        axis.text.x=element_text(size=15,family = "sans", angle=0),
        axis.text.y=element_text(size=15,family = "sans"),
        axis.title.x=element_text(size=20,family = "sans"),
        axis.title.y=element_text(size=20,family = "sans"),
        legend.position = "none",
        legend.text = element_text(colour = "black", size = 12,family = "sans"),
        legend.title = element_blank())


FigS1E <- ggplot(frag_df_sub_glad, aes(x=AFI_2020, y=NPP_2020, color=biome, fill=biome))+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.x.npc = 0.3, label.y.npc = "top", method = "pearson",label.sep = "/n", color=c("#d7191c", "darkcyan", "#2c7bb6"),size = 5)+
  geom_smooth(method="lm", se=F, linewidth=2, alpha=1)+
  geom_point(size=4, alpha=0.05)+
  ggtitle("Y2020")+
  xlab("AFI")+
  ylab("NPP (kgC/m2)")+
  scale_color_manual(values=c("#d7191c", "darkcyan", "#2c7bb6"))+
  scale_fill_manual(values=c("#d7191c", "darkcyan", "#2c7bb6"))+
  theme_classic()+
  # geom_hline(yintercept = 0.6, color="black", linetype="dashed",linewidth=1)+
  theme(title = element_text(size=15,family = "sans"),
        # axis.text.x=element_text(size=12,family = "sans", face="bold", angle=60, vjust = 1, hjust=1),
        axis.text.x=element_text(size=15,family = "sans", angle=0),
        axis.text.y=element_text(size=15,family = "sans"),
        axis.title.x=element_text(size=20,family = "sans"),
        axis.title.y=element_text(size=20,family = "sans"),
        legend.position = "none",
        legend.text = element_text(colour = "black", size = 12,family = "sans"),
        legend.title = element_blank())

FigS1F <- ggplot(frag_df_sub_glad, aes(x=AFI, y=(1-MPC_ST), color=biome, fill=biome))+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.x.npc = 0.3, label.y.npc = "top", method = "pearson",label.sep = "/n", color=c("#d7191c", "darkcyan", "#2c7bb6"),size = 5)+
  geom_smooth(method="lm", se=F, linewidth=2, alpha=1)+
  geom_point(size=4, alpha=0.05)+
  ggtitle("Y2000")+
  xlab("AFI")+
  ylab("Metapopulation Capacity")+
  ylim(c(0,1))+
  scale_color_manual(values=c("#d7191c", "darkcyan", "#2c7bb6"))+
  scale_fill_manual(values=c("#d7191c", "darkcyan", "#2c7bb6"))+
  theme_classic()+
  # geom_hline(yintercept = 0.6, color="black", linetype="dashed",linewidth=1)+
  theme(title = element_text(size=15,family = "sans"),
        # axis.text.x=element_text(size=12,family = "sans", face="bold", angle=60, vjust = 1, hjust=1),
        axis.text.x=element_text(size=15,family = "sans", angle=0),
        axis.text.y=element_text(size=15,family = "sans"),
        axis.title.x=element_text(size=20,family = "sans"),
        axis.title.y=element_text(size=20,family = "sans"),
        # legend.position = "none",
        legend.text = element_text(colour = "black", size = 12,family = "sans"),
        legend.title = element_blank())

FigS1G <- ggplot(frag_df_sub_glad, aes(x=FFI, y=NPP_2001, color=biome, fill=biome))+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.x.npc = 0.3, label.y.npc = "top", method = "pearson",label.sep = "/n", color=c("#d7191c", "darkcyan", "#2c7bb6"),size = 5)+
  geom_smooth(method="lm", se=F, linewidth=2, alpha=1)+
  geom_point(size=4, alpha=0.05)+
  ggtitle("Y2000")+
  xlab("FFI")+
  ylab("NPP (kgC/m2)")+
  scale_color_manual(values=c("#d7191c", "darkcyan", "#2c7bb6"))+
  scale_fill_manual(values=c("#d7191c", "darkcyan", "#2c7bb6"))+
  theme_classic()+
  # geom_hline(yintercept = 0.6, color="black", linetype="dashed",linewidth=1)+
  theme(title = element_text(size=15,family = "sans"),
        # axis.text.x=element_text(size=12,family = "sans", face="bold", angle=60, vjust = 1, hjust=1),
        axis.text.x=element_text(size=15,family = "sans", angle=0),
        axis.text.y=element_text(size=15,family = "sans"),
        axis.title.x=element_text(size=20,family = "sans"),
        axis.title.y=element_text(size=20,family = "sans"),
        legend.position = "none",
        legend.text = element_text(colour = "black", size = 12,family = "sans"),
        legend.title = element_blank())


FigS1H <- ggplot(frag_df_sub_glad, aes(x=FFI_2020, y=NPP_2020, color=biome, fill=biome))+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.x.npc = 0.3, label.y.npc = "top", method = "pearson",label.sep = "/n", color=c("#d7191c", "darkcyan", "#2c7bb6"),size = 5)+
  geom_smooth(method="lm", se=F, linewidth=2, alpha=1)+
  geom_point(size=4, alpha=0.05)+
  ggtitle("Y2020")+
  xlab("FFI")+
  ylab("NPP (kgC/m2)")+
  scale_color_manual(values=c("#d7191c", "darkcyan", "#2c7bb6"))+
  scale_fill_manual(values=c("#d7191c", "darkcyan", "#2c7bb6"))+
  theme_classic()+
  # geom_hline(yintercept = 0.6, color="black", linetype="dashed",linewidth=1)+
  theme(title = element_text(size=15,family = "sans"),
        # axis.text.x=element_text(size=12,family = "sans", face="bold", angle=60, vjust = 1, hjust=1),
        axis.text.x=element_text(size=15,family = "sans", angle=0),
        axis.text.y=element_text(size=15,family = "sans"),
        axis.title.x=element_text(size=20,family = "sans"),
        axis.title.y=element_text(size=20,family = "sans"),
        legend.position = "none",
        legend.text = element_text(colour = "black", size = 12,family = "sans"),
        legend.title = element_blank())

FigS1I <- ggplot(frag_df_sub_glad, aes(x=FFI, y=(1-MPC_ST), color=biome, fill=biome))+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.x.npc = 0.3, label.y.npc = "top", method = "pearson",label.sep = "/n", color=c("#d7191c", "darkcyan", "#2c7bb6"),size = 5)+
  geom_smooth(method="lm", se=F, linewidth=2, alpha=1)+
  geom_point(size=4, alpha=0.05)+
  ggtitle("Y2000")+
  xlab("FFI")+
  ylab("Metapopulation Capacity")+
  ylim(c(0,1))+
  scale_color_manual(values=c("#d7191c", "darkcyan", "#2c7bb6"))+
  scale_fill_manual(values=c("#d7191c", "darkcyan", "#2c7bb6"))+
  theme_classic()+
  # geom_hline(yintercept = 0.6, color="black", linetype="dashed",linewidth=1)+
  theme(title = element_text(size=15,family = "sans"),
        # axis.text.x=element_text(size=12,family = "sans", face="bold", angle=60, vjust = 1, hjust=1),
        axis.text.x=element_text(size=15,family = "sans", angle=0),
        axis.text.y=element_text(size=15,family = "sans"),
        axis.title.x=element_text(size=20,family = "sans"),
        axis.title.y=element_text(size=20,family = "sans"),
        legend.position = "none",
        legend.text = element_text(colour = "black", size = 12,family = "sans"),
        legend.title = element_blank())


layout <- "
ABC
DEF
GHI
"

MainPlot = FigS1A + FigS1B + FigS1C + FigS1D + FigS1E + FigS1F + FigS1G + FigS1H + FigS1I +
  plot_layout(design = layout) + plot_annotation(tag_levels = 'A')&
  theme(plot.tag = element_text(size=20,family="sans", face="bold"))

MainPlot


ggsave("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Results/Week34_2024.11.4_ScienceResponse/FigureSX_3D_FRAG_vs_NPP_MPC.pdf", plot=MainPlot, width=18, height=15)

