rm(list=ls())
library(tidyverse)
library(tictoc)
library(raster)
library(landscapemetrics)
library(landscapeR)

####----Figure S1: scatter plot between frag and cover----####

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


fragDf_Simulated_Landscape <- fragDf_Simulated_Landscape%>%dplyr::select(cover, TCA_ST, LPI_ST, LDI_ST, MPA_ST, ED_ST, NP_ST, ENN_ST, AI_ST, PLADJ_ST, MPC_ST)%>%drop_na()
# AA <- as.numeric(upper(fragDf_Simulated_Landscape$cover))
colnames(fragDf_Simulated_Landscape) <- c("cover","tca", "lpi", "ldi",
                                          "mpa", "ed", "np", "enn", "ai",
                                          "pladj", "mpc")

# hist(fragDf_Simulated_Landscape$mpa)
# hist(fragDf_Simulated_Landscape$ed)
# hist(fragDf_Simulated_Landscape$np)

bins <- seq(0, 1, by=0.01)
labels <- seq(0.005, 0.995, by=0.01)
data_t <- fragDf_Simulated_Landscape %>%
  mutate(group = cut(cover, breaks = bins,
                     labels = labels, right = FALSE)) %>%
  group_by(group) %>%
  summarize(tca_median = median(tca, na.rm=T),
            tca_upper = upper(tca), tca_lower=lower(tca),
            lpi_median = median(lpi, na.rm=T),
            lpi_upper = upper(lpi), lpi_lower=lower(lpi),
            ldi_median = median(ldi, na.rm=T),
            ldi_upper = upper(ldi), ldi_lower=lower(ldi),
            mpa_median = median(mpa, na.rm=T),
            mpa_upper = upper(mpa), mpa_lower=lower(mpa),
            ed_median = median(ed, na.rm=T),
            ed_upper = upper(ed), ed_lower=lower(ed),
            np_median = median(np, na.rm=T),
            np_upper = upper(np), np_lower=lower(np),
            enn_median = median(enn, na.rm=T),
            enn_upper = upper(enn), enn_lower=lower(enn),
            ai_median = median(ai, na.rm=T),
            ai_upper = upper(ai), ai_lower=lower(ai),
            pladj_median = median(pladj, na.rm=T),
            pladj_upper = upper(pladj), pladj_lower=lower(pladj),
            mpc_median = median(mpc, na.rm=T),
            mpc_upper = upper(mpc), mpc_lower=lower(mpc)
            # , cover_up=CI_up(cover_diff*100), cover_low=CI_low(cover_diff*100)
  )%>%drop_na()

data_t$group <- as.numeric(labels)

data_v <- data.frame(cbind(cover=rep(data_t$group,6),
                           frag_median=c(data_t$tca_median,data_t$lpi_median,
                                         data_t$ldi_median,data_t$mpa_median,
                                         data_t$ed_median,data_t$np_median,
                                         data_t$enn_median,data_t$ai_median,
                                         data_t$pladj_median,data_t$mpc_median),
                           frag_upper=c(data_t$tca_upper,data_t$lpi_upper,
                                        data_t$ldi_upper,data_t$mpa_upper,
                                        data_t$ed_upper,data_t$np_upper,
                                        data_t$enn_upper,data_t$ai_upper,
                                        data_t$pladj_upper,data_t$mpc_upper
                                        ),
                           frag_lower=c(data_t$tca_lower,data_t$lpi_lower,
                                        data_t$ldi_lower,data_t$mpa_lower,
                                        data_t$ed_lower,data_t$np_lower,
                                        data_t$enn_lower,data_t$ai_lower,
                                        data_t$pladj_lower,data_t$mpc_lower),
                           Metric=rep(c("tca", "lpi", "ldi",
                                        "mpa", "ed", "np",
                                        "enn", "ai", "pladj", "mpc"), each=nrow(data_t))
))

data_v <- data_v%>%mutate(cover=as.numeric(cover),
                          frag_median=as.numeric(frag_median),
                          frag_upper=as.numeric(frag_upper), frag_lower=as.numeric(frag_lower))

# data_0 <- as.data.frame(cbind(cover=c(0, 0, 0, 0, 0, 0),
#                               frag_median=c(0, 0, 1, 0, 0, 0), frag_upper=c(0, 0, 1, 0, 0, 0),
#                               frag_lower=c(0, 0, 1, 0, 0, 0),Metric=c("tca", "lpi", "ldi",
#                                                                       "mpa", "ed", "np")))
# data_0 <- data_0%>%mutate(cover=as.numeric(cover),
#                           frag_median=as.numeric(frag_median),
#                           frag_upper=as.numeric(frag_upper), frag_lower=as.numeric(frag_lower))
# 
# data_1 <- as.data.frame(cbind(cover=c(1, 1, 1, 1, 1, 1), 
#                               frag_median=c(1, 1, 0, 1, 0, 0),frag_upper=c(1, 1, 0, 1, 0, 0),
#                               frag_lower=c(1, 1, 0, 1, 0, 0),Metric=c("tca", "lpi", "ldi",
#                                                                       "mpa", "ed", "np")))
# data_1 <- data_1%>%mutate(cover=as.numeric(cover),
#                           frag_median=as.numeric(frag_median),
#                           frag_upper=as.numeric(frag_upper), frag_lower=as.numeric(frag_lower))
# 
# 
# 
# data_v <- rbind(data_v,data_0)
# data_v <- rbind(data_v,data_1)
# ggplot(data_v, aes(x=cover, y=frag_mean, color=Metric, fill=Metric)) +
#   # coord_flip()+
#   # geom_ribbon(aes(ymin = ldi_mean-10*ldi_se, ymax = ldi_mean+10*ldi_se), fill = "red", alpha = 0.4) +  # 误差区间
#   # geom_ribbon(aes(ymin = ldi_low, ymax = ldi_up), fill = "red", alpha = 0.35) +  # 误差区间
#   # geom_hline(color="grey", yintercept = 0, linewidth=0.8, linetype="dashed")+
#   geom_point(size=3, alpha = 0.7) +
#   scale_fill_manual(values=c("#ca0020", "#0571b0"))+
#   scale_color_manual(values=c("#ca0020", "#0571b0"))+
#   geom_errorbar(aes(ymin = frag_mean-frag_sd, ymax = frag_mean+frag_sd), width=0.02) +  # 误差区间
#   labs(x = "Cover", y = "Degree of fragmentation")+
#   theme_classic()+
#   theme(axis.text   =element_text(size=18,family="sans"),
#         axis.title.x=element_text(size=18,family="sans"),
#         axis.title.y=element_text(size=18,family="sans"),
#         legend.text =element_text(size=18,family="sans"),
#         legend.title=element_text(size=18,family="sans"))

A1 <- lm(mpc~cover, data=fragDf_Simulated_Landscape)
summary(A1)

FigS1X <- ggplot(data_v[data_v$Metric=="mpc",], aes(x=cover*100, y=frag_median)) +
  # coord_flip()+
  # geom_ribbon(aes(ymin = ldi_mean-10*ldi_se, ymax = ldi_mean+10*ldi_se), fill = "red", alpha = 0.4) +  # 误差区间
  geom_ribbon(aes(ymin = frag_lower, ymax = frag_upper), alpha = 0.3) +  # 误差区间
  # geom_hline(color="grey", yintercept = 0, linewidth=0.8, linetype="dashed")+
  geom_line(linewidth=2, alpha = 0.7) +
  # scale_fill_manual(values=c("#ca0020"))+
  # scale_color_manual(values=c("#ca0020"))+
  labs(x = "Canopy Cover (%)", y = "Metapopulation Capacity (MPC)")+
  theme_classic()+
  theme(axis.text   =element_text(size=15,family="sans", face="bold"),
        axis.title.x=element_text(size=20,family="sans", face="bold"),
        axis.title.y=element_text(size=20,family="sans", face="bold"),
        legend.text =element_text(size=18,family="sans", face="bold"),
        legend.position = "none",
        legend.title=element_text(size=18,family="sans", face="bold"))


FigS1A <- ggplot(data_v[data_v$Metric=="tca",], aes(x=cover*100, y=frag_median)) +
  # coord_flip()+
  # geom_ribbon(aes(ymin = ldi_mean-10*ldi_se, ymax = ldi_mean+10*ldi_se), fill = "red", alpha = 0.4) +  # 误差区间
  geom_ribbon(aes(ymin = frag_lower, ymax = frag_upper), alpha = 0.3) +  # 误差区间
  # geom_hline(color="grey", yintercept = 0, linewidth=0.8, linetype="dashed")+
  geom_line(linewidth=2, alpha = 0.7) +
  # scale_fill_manual(values=c("#ca0020"))+
  # scale_color_manual(values=c("#ca0020"))+
  labs(x = "Canopy Cover (%)", y = "Total core area (TCA)")+
  theme_classic()+
  theme(axis.text   =element_text(size=15,family="sans", face="bold"),
        axis.title.x=element_text(size=20,family="sans", face="bold"),
        axis.title.y=element_text(size=20,family="sans", face="bold"),
        legend.text =element_text(size=18,family="sans", face="bold"),
        legend.position = "none",
        legend.title=element_text(size=18,family="sans", face="bold"))

FigS1B <- ggplot(data_v[data_v$Metric=="lpi",], aes(x=cover*100, y=frag_median)) +
  # coord_flip()+
  # geom_ribbon(aes(ymin = ldi_mean-10*ldi_se, ymax = ldi_mean+10*ldi_se), fill = "red", alpha = 0.4) +  # 误差区间
  geom_ribbon(aes(ymin = frag_lower, ymax = frag_upper), alpha = 0.3) +  # 误差区间
  # geom_hline(color="grey", yintercept = 0, linewidth=0.8, linetype="dashed")+
  geom_line(linewidth=2, alpha = 0.7) +
  # scale_fill_manual(values=c("#ca0020"))+
  # scale_color_manual(values=c("#ca0020"))+
  labs(x = "Canopy Cover (%)", y = "Largest patch index (LPI)")+
  theme_classic()+
  theme(axis.text   =element_text(size=15,family="sans", face="bold"),
        axis.title.x=element_text(size=20,family="sans", face="bold"),
        axis.title.y=element_text(size=20,family="sans", face="bold"),
        legend.text =element_text(size=18,family="sans", face="bold"),
        legend.position = "none",
        legend.title=element_text(size=18,family="sans", face="bold"))


FigS1C <- ggplot(data_v[data_v$Metric=="ldi",], aes(x=cover*100, y=frag_median)) +
  # coord_flip()+
  # geom_ribbon(aes(ymin = ldi_mean-10*ldi_se, ymax = ldi_mean+10*ldi_se), fill = "red", alpha = 0.4) +  # 误差区间
  geom_ribbon(aes(ymin = frag_lower, ymax = frag_upper), alpha = 0.3) +  # 误差区间
  # geom_hline(color="grey", yintercept = 0, linewidth=0.8, linetype="dashed")+
  geom_line(linewidth=2, alpha = 0.7) +
  # scale_fill_manual(values=c("#ca0020"))+
  # scale_color_manual(values=c("#ca0020"))+
  labs(x = "Canopy Cover (%)", y = "Landscape division index (LDI)")+
  theme_classic()+
  theme(axis.text   =element_text(size=15,family="sans", face="bold"),
        axis.title.x=element_text(size=20,family="sans", face="bold"),
        axis.title.y=element_text(size=20,family="sans", face="bold"),
        legend.text =element_text(size=18,family="sans", face="bold"),
        legend.position = "none",
        legend.title=element_text(size=18,family="sans", face="bold"))


FigS1D <- ggplot(data_v[data_v$Metric=="ai",], aes(x=cover*100, y=frag_median)) +
  # coord_flip()+
  # geom_ribbon(aes(ymin = ldi_mean-10*ldi_se, ymax = ldi_mean+10*ldi_se), fill = "red", alpha = 0.4) +  # 误差区间
  geom_ribbon(aes(ymin = frag_lower, ymax = frag_upper), alpha = 0.3) +  # 误差区间
  # geom_hline(color="grey", yintercept = 0, linewidth=0.8, linetype="dashed")+
  geom_line(linewidth=2, alpha = 0.7) +
  # scale_fill_manual(values=c("#ca0020"))+
  # scale_color_manual(values=c("#ca0020"))+
  labs(x = "Canopy Cover (%)", y = "Aggregation index (AI)")+
  theme_classic()+
  theme(axis.text   =element_text(size=15,family="sans", face="bold"),
        axis.title.x=element_text(size=20,family="sans", face="bold"),
        axis.title.y=element_text(size=20,family="sans", face="bold"),
        legend.text =element_text(size=18,family="sans", face="bold"),
        legend.position = "none",
        legend.title=element_text(size=18,family="sans", face="bold"))


FigS1E <- ggplot(data_v[data_v$Metric=="pladj",], aes(x=cover*100, y=frag_median)) +
  # coord_flip()+
  # geom_ribbon(aes(ymin = ldi_mean-10*ldi_se, ymax = ldi_mean+10*ldi_se), fill = "red", alpha = 0.4) +  # 误差区间
  geom_ribbon(aes(ymin = frag_lower, ymax = frag_upper), alpha = 0.3) +  # 误差区间
  # geom_hline(color="grey", yintercept = 0, linewidth=0.8, linetype="dashed")+
  geom_line(linewidth=2, alpha = 0.7) +
  # scale_fill_manual(values=c("#ca0020"))+
  # scale_color_manual(values=c("#ca0020"))+
  labs(x = "Canopy Cover (%)", y = "Percentage of like adjacencies (PLADJ)")+
  theme_classic()+
  theme(axis.text   =element_text(size=15,family="sans", face="bold"),
        axis.title.x=element_text(size=20,family="sans", face="bold"),
        axis.title.y=element_text(size=20,family="sans", face="bold"),
        legend.text =element_text(size=18,family="sans", face="bold"),
        legend.position = "none",
        legend.title=element_text(size=18,family="sans", face="bold"))


FigS1F <- ggplot(data_v[data_v$Metric=="enn",], aes(x=cover*100, y=frag_median)) +
  # coord_flip()+
  # geom_ribbon(aes(ymin = ldi_mean-10*ldi_se, ymax = ldi_mean+10*ldi_se), fill = "red", alpha = 0.4) +  # 误差区间
  geom_ribbon(aes(ymin = frag_lower, ymax = frag_upper), alpha = 0.3) +  # 误差区间
  # geom_hline(color="grey", yintercept = 0, linewidth=0.8, linetype="dashed")+
  geom_line(linewidth=2, alpha = 0.7) +
  # scale_fill_manual(values=c("#ca0020"))+
  # scale_color_manual(values=c("#ca0020"))+
  labs(x = "Canopy Cover (%)", y = "Mean distance between patches (ENN)")+
  theme_classic()+
  theme(axis.text   =element_text(size=15,family="sans", face="bold"),
        axis.title.x=element_text(size=20,family="sans", face="bold"),
        axis.title.y=element_text(size=20,family="sans", face="bold"),
        legend.text =element_text(size=18,family="sans", face="bold"),
        legend.position = "none",
        legend.title=element_text(size=18,family="sans", face="bold"))


FigS1G <- ggplot(data_v[data_v$Metric=="np",], aes(x=cover*100, y=frag_median)) +
  # coord_flip()+
  # geom_ribbon(aes(ymin = ldi_mean-10*ldi_se, ymax = ldi_mean+10*ldi_se), fill = "red", alpha = 0.4) +  # 误差区间
  geom_ribbon(aes(ymin = frag_lower, ymax = frag_upper), alpha = 0.3) +  # 误差区间
  # geom_hline(color="grey", yintercept = 0, linewidth=0.8, linetype="dashed")+
  geom_line(linewidth=2, alpha = 0.7) +
  # scale_fill_manual(values=c("#ca0020"))+
  # scale_color_manual(values=c("#ca0020"))+
  labs(x = "Canopy Cover (%)", y = "Number of patches (NP)")+
  theme_classic()+
  theme(axis.text   =element_text(size=15,family="sans", face="bold"),
        axis.title.x=element_text(size=20,family="sans", face="bold"),
        axis.title.y=element_text(size=20,family="sans", face="bold"),
        legend.text =element_text(size=18,family="sans", face="bold"),
        legend.position = "none",
        legend.title=element_text(size=18,family="sans", face="bold"))

FigS1H <- ggplot(data_v[data_v$Metric=="ed",], aes(x=cover*100, y=frag_median)) +
  # coord_flip()+
  # geom_ribbon(aes(ymin = ldi_mean-10*ldi_se, ymax = ldi_mean+10*ldi_se), fill = "red", alpha = 0.4) +  # 误差区间
  geom_ribbon(aes(ymin = frag_lower, ymax = frag_upper), alpha = 0.3) +  # 误差区间
  # geom_hline(color="grey", yintercept = 0, linewidth=0.8, linetype="dashed")+
  geom_line(linewidth=2, alpha = 0.7) +
  # scale_fill_manual(values=c("#ca0020"))+
  # scale_color_manual(values=c("#ca0020"))+
  labs(x = "Canopy Cover (%)", y = "Edge density (ED)")+
  theme_classic()+
  theme(axis.text   =element_text(size=15,family="sans", face="bold"),
        axis.title.x=element_text(size=20,family="sans", face="bold"),
        axis.title.y=element_text(size=20,family="sans", face="bold"),
        legend.text =element_text(size=18,family="sans", face="bold"),
        legend.position = "none",
        legend.title=element_text(size=18,family="sans", face="bold"))


FigS1I <- ggplot(data_v[data_v$Metric=="mpa",], aes(x=cover*100, y=frag_median)) +
  # coord_flip()+
  # geom_ribbon(aes(ymin = ldi_mean-10*ldi_se, ymax = ldi_mean+10*ldi_se), fill = "red", alpha = 0.4) +  # 误差区间
  geom_ribbon(aes(ymin = frag_lower, ymax = frag_upper), alpha = 0.3) +  # 误差区间
  # geom_hline(color="grey", yintercept = 0, linewidth=0.8, linetype="dashed")+
  geom_line(linewidth=2, alpha = 0.7) +
  # scale_fill_manual(values=c("#ca0020"))+
  # scale_color_manual(values=c("#ca0020"))+
  labs(x = "Canopy Cover (%)", y = "Mean patch area (MPA)")+
  theme_classic()+
  theme(axis.text   =element_text(size=15,family="sans", face="bold"),
        axis.title.x=element_text(size=20,family="sans", face="bold"),
        axis.title.y=element_text(size=20,family="sans", face="bold"),
        legend.text =element_text(size=18,family="sans", face="bold"),
        legend.position = "none",
        legend.title=element_text(size=18,family="sans", face="bold"))



layout <- "
ABC
DEF
GHI
"

MainPlot = FigS1A + FigS1B + FigS1C + FigS1D + FigS1E + FigS1F + FigS1G + FigS1H + FigS1I + FigS1X+
  plot_layout(design = layout) + plot_annotation(tag_levels = 'A')&
  theme(plot.tag = element_text(size=20,family="sans", face="bold"))

MainPlot


ggsave("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Results/Week34_2024.11.4_ScienceResponse/FigureSX_All_FRAG_vs_Cover.pdf", plot=MainPlot, width=25, height=18)



