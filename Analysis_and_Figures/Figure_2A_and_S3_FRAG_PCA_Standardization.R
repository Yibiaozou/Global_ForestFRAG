rm(list=ls())
library(tidyverse)
library(tictoc)
library(raster)
library(AMR)

####----Exploration Check of PCA distribution of the selected fragmentation metrics----####
frag_df_full_glad_pca <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/frag_df_Full_glad_extent_5m.rds")
colnames(frag_df_full_glad_pca)
# 

# selected_var_2000 <- c("TCA_ST", "LPI_ST", "LDI_ST", "CFI", "NP_ST", "MPA_ST", "ED_ST", "FFI", "NLSI_ST", "CI_ST")
selected_var_2020 <- c("TCA_2020_ST", "LPI_2020_ST", "LDI_2020_ST", "CFI_2020", "NP_2020_ST", "MPA_2020_ST", 
                       "ED_2020_ST", "FFI_2020", "AI_2020_ST", "PCI_2020_ST", "NLSI_2020_ST", "CI_2020_ST","SI_2020_ST", "PAR_2020_ST",
                       "cover_2020", "MPC_ST", "AFI_2020", "ENN_2020_ST", "LSI_2020_ST", "PARMN_2020_ST",
                       "PLADJ_2020_ST")


selected_var_2000 <- c("TCA_ST", "LPI_ST", "LDI_ST", "CFI", "NP_ST", 
                       "MPA_ST", "ED_ST", "FFI", "AI_ST", "PCI_ST", 
                       "NLSI_ST", "CI_ST", "SI_ST", "PAR_ST",
                       "cover", "MPC_ST", "AFI", "ENN_ST", "LSI_ST", "PARMN_ST",
                       "PLADJ_ST"
                       )

# selected_var_diff <- c("tca_diff", "lpi_diff", "ldi_diff", "cfi_diff", "np_diff", 
#                        "mpa_diff", "ed_diff", "ffi_diff", "nlsi_diff", "clumpy_diff", "cover_diff")
selected_var_diff <- c("TCA_ST_Diff", "LPI_ST_Diff", "LDI_ST_Diff", "CFI_ST_Diff", "NP_ST_Diff", 
                       "MPA_ST_Diff", "ED_ST_Diff", "FFI_ST_Diff", "AI_ST_Diff", "PCI_ST_Diff", 
                       "NLSI_ST_Diff", "CI_ST_Diff", "SI_ST_Diff", "PAR_ST_Diff")

replace_var <- c("TCA", "LPI", "LDI", "CFI", "NP", "MPA", 
                 "ED", "FFI", "AI", "PCI", "NLSI", "CI", "SI", "PAR",
                 "Cover", "MPC", "AFI", "ENN", "LSI", "PARMN", "PLADJ")
replace_var_diff <- c("ΔTCA", "ΔLPI", "ΔLDI", "ΔCFI", "ΔNP", "ΔMPA", 
                      "ΔED", "ΔFFI", "ΔAI", "ΔPCI", "ΔNLSI", "ΔCI", "ΔSI", "ΔPAR")

### delta pca: eight 
set.seed(1234)
frag_df_sub_glad_pca <- frag_df_full_glad_pca%>%group_by(Mega_Biome)%>%slice_sample(prop=0.01)
frag_df_sub_glad <- frag_df_sub_glad_pca[,selected_var_diff]
colnames(frag_df_sub_glad) <- replace_var_diff

# pca_result <- frag_df_sub_glad %>% pca(TCA, LPI, LDI, CFI, NP, MPA, ED, FFI)
# pca_result <- frag_df_sub_glad %>% pca(ΔTCA, ΔLPI, ΔLDI, ΔCFI, ΔNP, ΔMPA, ΔED, ΔFFI)

pca_result <- frag_df_sub_glad %>% pca(ΔTCA, ΔLPI, ΔLDI, ΔCFI, ΔNP, ΔMPA, ΔED, ΔFFI
                                       #ΔAI, ΔPCI, ΔNLSI, ΔCI, ΔSI, 
                                       # ΔPAR
                                       )

ggplot_pca(pca_result, points_alpha = 0.01, arrows_size = 1, arrows_textsize = 5,
           arrows_textangled = T, arrows_alpha = 0.6, arrows_colour = "darkred",) +
  # labs(title = "PCA Biplot for Fragmentation Metrics")+
  # scale_colour_viridis_d()+
  ylim(c(-2.5,2.5))+
  xlim(c(-2.5,2.5))+
  theme(axis.text   =element_text(size=16,family = "sans", face="bold"),
        axis.title.x=element_text(size=16,family = "sans", face="bold"),
        axis.title.y=element_text(size=16,family = "sans", face="bold"),
        legend.text =element_text(size=8,family = "sans", face="bold"),
        legend.title=element_text(size=8,family = "sans", face="bold"))


### delta pca: eight + par
set.seed(1234)
frag_df_sub_glad_pca <- frag_df_full_glad_pca%>%group_by(Mega_Biome)%>%slice_sample(prop=0.01)
frag_df_sub_glad <- frag_df_sub_glad_pca[,selected_var_diff]
colnames(frag_df_sub_glad) <- replace_var_diff

# pca_result <- frag_df_sub_glad %>% pca(TCA, LPI, LDI, CFI, NP, MPA, ED, FFI)
# pca_result <- frag_df_sub_glad %>% pca(ΔTCA, ΔLPI, ΔLDI, ΔCFI, ΔNP, ΔMPA, ΔED, ΔFFI)

pca_result <- frag_df_sub_glad %>% pca(ΔTCA, ΔLPI, ΔLDI, ΔCFI, ΔNP, ΔMPA, ΔED, ΔFFI,
                                       #ΔAI, ΔPCI, ΔNLSI, ΔCI, ΔSI, 
                                       ΔPAR)

ggplot_pca(pca_result, points_alpha = 0.01, arrows_size = 1, arrows_textsize = 5,
           arrows_textangled = T, arrows_alpha = 0.6, arrows_colour = "darkred",) +
  # labs(title = "PCA Biplot for Fragmentation Metrics")+
  # scale_colour_viridis_d()+
  ylim(c(-2.5,2.5))+
  xlim(c(-2.5,2.5))+
  theme(axis.text   =element_text(size=16,family = "sans", face="bold"),
        axis.title.x=element_text(size=16,family = "sans", face="bold"),
        axis.title.y=element_text(size=16,family = "sans", face="bold"),
        legend.text =element_text(size=8,family = "sans", face="bold"),
        legend.title=element_text(size=8,family = "sans", face="bold"))

### delta pca: all
set.seed(1234)
frag_df_sub_glad_pca <- frag_df_full_glad_pca%>%group_by(Mega_Biome)%>%slice_sample(prop=0.01)
frag_df_sub_glad <- frag_df_sub_glad_pca[,selected_var_diff]
colnames(frag_df_sub_glad) <- replace_var_diff

# pca_result <- frag_df_sub_glad %>% pca(TCA, LPI, LDI, CFI, NP, MPA, ED, FFI)
# pca_result <- frag_df_sub_glad %>% pca(ΔTCA, ΔLPI, ΔLDI, ΔCFI, ΔNP, ΔMPA, ΔED, ΔFFI)

pca_result <- frag_df_sub_glad %>% pca(ΔTCA, ΔLPI, ΔLDI, ΔCFI, ΔNP, ΔMPA, ΔED, ΔFFI,
                                       ΔAI,
                                       ΔPCI,
                                       # ΔNLSI, ΔCI,
                                       # ΔSI,
                                       ΔPAR)

ggplot_pca(pca_result, points_alpha = 0.01, arrows_size = 1, arrows_textsize = 5,
           arrows_textangled = T, arrows_alpha = 0.6, arrows_colour = "darkred",) +
  # labs(title = "PCA Biplot for Fragmentation Metrics")+
  # scale_colour_viridis_d()+
  ylim(c(-2.5,2.5))+
  xlim(c(-2.5,2.5))+
  theme(axis.text   =element_text(size=16,family = "sans", face="bold"),
        axis.title.x=element_text(size=16,family = "sans", face="bold"),
        axis.title.y=element_text(size=16,family = "sans", face="bold"),
        legend.text =element_text(size=8,family = "sans", face="bold"),
        legend.title=element_text(size=8,family = "sans", face="bold"))



### 2000 pca: eight 
frag_df_sub_glad_pca <- frag_df_full_glad_pca%>%group_by(Mega_Biome)%>%slice_sample(prop=0.01)
frag_df_sub_glad <- frag_df_sub_glad_pca[,selected_var_2000]
colnames(frag_df_sub_glad) <- replace_var

# pca_result <- frag_df_sub_glad %>% pca(TCA, LPI, LDI, CFI, NP, MPA, ED, FFI)
# pca_result <- frag_df_sub_glad %>% pca(TCA, LPI, LDI, CFI, NP, MPA, ED, FFI)

pca_result <- frag_df_sub_glad %>% pca(TCA, LPI, LDI, CFI, NP, MPA, ED, FFI,
                                       MPC
                                       #AI, PCI, NLSI, CI, SI, 
                                       #PAR
                                       )

ggplot_pca(pca_result, points_alpha = 0.01, arrows_size = 1, arrows_textsize = 5,
           arrows_textangled = T, arrows_alpha = 0.6, arrows_colour = "darkred",) +
  # labs(title = "PCA Biplot for Fragmentation Metrics")+
  # scale_colour_viridis_d()+
  ylim(c(-2.5,2.5))+
  xlim(c(-2.5,2.5))+
  theme(axis.text   =element_text(size=16,family = "sans", face="bold"),
        axis.title.x=element_text(size=16,family = "sans", face="bold"),
        axis.title.y=element_text(size=16,family = "sans", face="bold"),
        legend.text =element_text(size=8,family = "sans", face="bold"),
        legend.title=element_text(size=8,family = "sans", face="bold"))


### 2000 pca: eight + par
frag_df_sub_glad_pca <- frag_df_full_glad_pca%>%group_by(Mega_Biome)%>%slice_sample(prop=0.01)
frag_df_sub_glad <- frag_df_sub_glad_pca[,selected_var_2000]
colnames(frag_df_sub_glad) <- replace_var

# pca_result <- frag_df_sub_glad %>% pca(TCA, LPI, LDI, CFI, NP, MPA, ED, FFI)
# pca_result <- frag_df_sub_glad %>% pca(TCA, LPI, LDI, CFI, NP, MPA, ED, FFI)

pca_result <- frag_df_sub_glad %>% pca(TCA, LPI, LDI, CFI, NP, MPA, ED, FFI,
                                       #AI, PCI, NLSI, CI, SI, 
                                       PAR)

ggplot_pca(pca_result, points_alpha = 0.01, arrows_size = 1, arrows_textsize = 5,
           arrows_textangled = T, arrows_alpha = 0.6, arrows_colour = "darkred",) +
  # labs(title = "PCA Biplot for Fragmentation Metrics")+
  # scale_colour_viridis_d()+
  ylim(c(-2.5,2.5))+
  xlim(c(-2.5,2.5))+
  theme(axis.text   =element_text(size=16,family = "sans", face="bold"),
        axis.title.x=element_text(size=16,family = "sans", face="bold"),
        axis.title.y=element_text(size=16,family = "sans", face="bold"),
        legend.text =element_text(size=8,family = "sans", face="bold"),
        legend.title=element_text(size=8,family = "sans", face="bold"))


### 2000 pca: eight + par + ai
frag_df_sub_glad_pca <- frag_df_full_glad_pca%>%group_by(Mega_Biome)%>%slice_sample(prop=0.01)
frag_df_sub_glad <- frag_df_sub_glad_pca[,selected_var_2000]
colnames(frag_df_sub_glad) <- replace_var

# pca_result <- frag_df_sub_glad %>% pca(TCA, LPI, LDI, CFI, NP, MPA, ED, FFI)
# pca_result <- frag_df_sub_glad %>% pca(TCA, LPI, LDI, CFI, NP, MPA, ED, FFI)

pca_result <- frag_df_sub_glad %>% pca(TCA, LPI, LDI, CFI, NP, MPA, ED, FFI,
                                       AI, 
                                       # PCI, NLSI, CI, SI, 
                                       PAR)

ggplot_pca(pca_result, points_alpha = 0.01, arrows_size = 1, arrows_textsize = 5,
           arrows_textangled = T, arrows_alpha = 0.6, arrows_colour = "darkred",) +
  # labs(title = "PCA Biplot for Fragmentation Metrics")+
  # scale_colour_viridis_d()+
  ylim(c(-2.5,2.5))+
  xlim(c(-2.5,2.5))+
  theme(axis.text   =element_text(size=16,family = "sans", face="bold"),
        axis.title.x=element_text(size=16,family = "sans", face="bold"),
        axis.title.y=element_text(size=16,family = "sans", face="bold"),
        legend.text =element_text(size=8,family = "sans", face="bold"),
        legend.title=element_text(size=8,family = "sans", face="bold"))


### 2000 pca: eight + par + ai + pci
set.seed(1245)

frag_df_sub_glad_pca <- frag_df_full_glad_pca%>%group_by(Mega_Biome)%>%slice_sample(prop=0.01)
frag_df_sub_glad <- frag_df_sub_glad_pca[,selected_var_2020]
colnames(frag_df_sub_glad) <- replace_var


# frag_df_sub_glad$Cover <-  1-frag_df_sub_glad$Cover
# pca_result <- frag_df_sub_glad %>% pca(TCA, LPI, LDI, CFI, NP, MPA, ED, FFI)
# pca_result <- frag_df_sub_glad %>% pca(TCA, LPI, LDI, CFI, NP, MPA, ED, FFI)

pca_result <- frag_df_sub_glad %>% pca(TCA, LPI, LDI, CFI, NP, MPA, ED, FFI,
                                       AI,
                                       PCI,
                                       # SI,
                                       # MPC,
                                       # Cover,
                                       #NLSI, CI, SI, 
                                       PAR)

ggplot_pca(pca_result, points_alpha = 0.01, arrows_size = 1, arrows_textsize = 5,
           arrows_textangled = T, arrows_alpha = 0.6, arrows_colour = "darkred",) +
  # labs(title = "PCA Biplot for Fragmentation Metrics")+
  # scale_colour_viridis_d()+
  ylim(c(-2.5,2.5))+
  xlim(c(-2.5,2.5))+
  theme(axis.text   =element_text(size=16,family = "sans", face="bold"),
        axis.title.x=element_text(size=16,family = "sans", face="bold"),
        axis.title.y=element_text(size=16,family = "sans", face="bold"),
        legend.text =element_text(size=8,family = "sans", face="bold"),
        legend.title=element_text(size=8,family = "sans", face="bold"))

### 2000 pca: eight + par + ai + pci + mpc
set.seed(1245)
frag_df_sub_glad_pca <- frag_df_full_glad_pca%>%group_by(Mega_Biome)%>%slice_sample(prop=0.01)
frag_df_sub_glad <- frag_df_sub_glad_pca[,selected_var_2020]
colnames(frag_df_sub_glad) <- replace_var


# frag_df_sub_glad$Cover <-  1-frag_df_sub_glad$Cover
# pca_result <- frag_df_sub_glad %>% pca(TCA, LPI, LDI, CFI, NP, MPA, ED, FFI)
# pca_result <- frag_df_sub_glad %>% pca(TCA, LPI, LDI, CFI, NP, MPA, ED, FFI)

pca_result <- frag_df_sub_glad %>% pca(TCA, LPI, LDI, CFI, NP, MPA, ED, FFI,
                                       AI,
                                       PCI,
                                       MPC,
                                       AFI,
                                       ENN,
                                       PARMN,
                                       # Cover,
                                       NLSI,
                                       CI,
                                       SI,
                                       # PAR,
                                       # LSI,
                                       PLADJ
                                       )

ggplot_pca(pca_result, points_alpha = 0.01, arrows_size = 1, arrows_textsize = 5,
           arrows_textangled = T, arrows_alpha = 0.6, arrows_colour = "darkred",) +
  # labs(title = "PCA Biplot for Fragmentation Metrics")+
  # scale_colour_viridis_d()+
  ylim(c(-2.5,2.5))+
  xlim(c(-2.5,2.5))+
  theme(axis.text   =element_text(size=16,family = "sans", face="bold"),
        axis.title.x=element_text(size=16,family = "sans", face="bold"),
        axis.title.y=element_text(size=16,family = "sans", face="bold"),
        legend.text =element_text(size=8,family = "sans", face="bold"),
        legend.title=element_text(size=8,family = "sans", face="bold"))


AA <- lm(cover_diff~AFI_diff, data=frag_df_full_glad_pca)
BB <- lm(cover~ENN_ST, data=frag_df_full_glad_pca)
CC <- lm(cover_diff~nlsi_diff, data=frag_df_full_glad_pca)
DD <- lm(cover~PLADJ_ST, data=frag_df_full_glad_pca)

summary(AA)
summary(BB)
summary(CC)
summary(DD)


####----Figure 2A: Check PCA distribution of the 13 final selected metrics----####
# Assuming 'data' is your dataframe
# library(ggrepel)
set.seed(1245)
frag_df_sub_glad_pca <- frag_df_full_glad_pca%>%group_by(Mega_Biome)%>%slice_sample(prop=0.1)
frag_df_sub_glad <- frag_df_sub_glad_pca[,selected_var_2000]
colnames(frag_df_sub_glad) <- replace_var

saveRDS(frag_df_sub_glad_pca, file="C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Outcome/2024.10.24_ScienceResponse/frag_df_sub_glad_pca_1per.rds")

# frag_df_sub_glad$Cover <-  1-frag_df_sub_glad$Cover

data <- frag_df_sub_glad %>% dplyr::select(TCA, LPI, LDI, CFI, NP, MPA, ED, FFI,
                                 AI,
                                 ENN,
                                 MPC,
                                 AFI,
                                 PLADJ)%>%drop_na()
pca <- prcomp(data, scale. = TRUE, center = TRUE)

scores <- as.data.frame(pca$x)  # PCA scores
loadings <- pca$rotation  # PCA loadings

loadings_df <- as.data.frame(loadings[, 1:2])  # Only consider first two PCs
loadings_df$variable <- rownames(loadings)

# loadings_df$variable <- factor(loadings_df$variable, 
#                                                 levels=c("MPC", "CFI", "TCA", "LPI", "LDI",  
#                                                          "AFI", "AI", "PAR", "PCI", "FFI",
#                                                          "MPA", "NP", "ED"
#                                                 ), 
#                                                 ordered=T)
loadings_df$alphas <- c(0.7, 0.7, 0.7, 0.9, 0.7, 0.7, 0.7, 0.9, 0.7, 0.7, 0.9, 0.9, 0.7)

# loadings_df$sizes <- c(4, 4, 4, 6, 4, 4, 4, 6, 4, 4, 6, 6, 4)

# loadings_df$alphas <- c(0.7, 0.7, 0.7, 0.9, 0.7, 0.7, 0.7, 0.9, 0.7, 0.7, 0.9, 0.9, 0.7)

## Metrics focusing on connectivity are in red, 
# those focusing on aggregation are in orange, 
# and those on separation are in blue. 
# The integrated indices—Connectivity-Focused Index (CFI), Aggregation-Focused Index (AFI), and Separation-Focused Index (FFI)—are emphasized in darker shades. 
# The Metapopulation Capacity (MPC), a critical metric for functional connectivity, is shown in black, illustrating its proximity to the connectivity-focused metrics. 

loadings_df$colors <- c("#fe8181","#fe8181","#fe8181","#ca0020","#74add1","#74add1","#74add1","#2166ac","orange","orange","black","darkorange3","orange" )
# Define maximum length for arrows for visibility
# max_arrow_length <- max(abs(loadings_df[, 1]), abs(loadings_df[, 2]))
max_arrow_length <- 0.1

loadings_df_1 <- loadings_df[loadings_df$variable%in%c("MPC", "CFI","AFI","FFI"),]
loadings_df_2 <- loadings_df[!loadings_df$variable%in%c("MPC", "CFI","AFI","FFI"),]

# loadings_df_2$alphas <- 0.9
# colors <- c("#f4a582", "#ca0020","#ca0020","#ca0020","#ca0020","#7b3294","#7b3294","#7b3294","#7b3294",
#             "#0571b0","#0571b0","#0571b0","#0571b0")

ggplot(data = scores, aes(x = PC1, y = PC2)) +
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
  labs(title = "PCA Biplot", x = "PC1 (63.8%)", y = "PC2 (22.6%)") +
  theme_classic()+
  theme(axis.text   =element_text(size=16,family = "sans", face="bold"),
        axis.title.x=element_text(size=16,family = "sans", face="bold"),
        axis.title.y=element_text(size=16,family = "sans", face="bold"),
        title=element_blank(),
        legend.position = "none",
        legend.text =element_blank(),
        legend.title=element_blank())
  # theme(legend.title = element_blank())  # Remove legend title


data <- frag_df_sub_glad %>% dplyr::select(CFI, FFI,
                                           AFI, MPC
                                           )%>%drop_na()
pca <- prcomp(data, scale. = TRUE, center = TRUE)

scores <- as.data.frame(pca$x)  # PCA scores
loadings <- pca$rotation  # PCA loadings

loadings_df <- as.data.frame(loadings[, 1:2])  # Only consider first two PCs
loadings_df$variable <- rownames(loadings)

# loadings_df$variable <- factor(loadings_df$variable, 
#                                                 levels=c("MPC", "CFI", "TCA", "LPI", "LDI",  
#                                                          "AFI", "AI", "PAR", "PCI", "FFI",
#                                                          "MPA", "NP", "ED"
#                                                 ), 
#                                                 ordered=T)
# loadings_df$alphas <- c(0.7, 0.7, 0.7, 0.9, 0.7, 0.7, 0.7, 0.9, 0.7, 0.7, 0.9, 0.9, 0.7)

# loadings_df$sizes <- c(4, 4, 4, 6, 4, 4, 4, 6, 4, 4, 6, 6, 4)

# loadings_df$alphas <- c(0.7, 0.7, 0.7, 0.9, 0.7, 0.7, 0.7, 0.9, 0.7, 0.7, 0.9, 0.9, 0.7)

loadings_df$colors <- c("#ca0020","#2166ac","purple","yellow")
# Define maximum length for arrows for visibility
# max_arrow_length <- max(abs(loadings_df[, 1]), abs(loadings_df[, 2]))
max_arrow_length <- 0.3

# loadings_df_1 <- loadings_df[loadings_df$variable%in%c("MPC", "CFI","AFI","FFI"),]
# loadings_df_2 <- loadings_df[!loadings_df$variable%in%c("MPC", "CFI","AFI","FFI"),]

# loadings_df_2$alphas <- 0.9
# colors <- c("#f4a582", "#ca0020","#ca0020","#ca0020","#ca0020","#7b3294","#7b3294","#7b3294","#7b3294",
#             "#0571b0","#0571b0","#0571b0","#0571b0")

ggplot(data = scores, aes(x = PC1, y = PC2)) +
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
  labs(title = "PCA Biplot", x = "PC1 (72.1%)", y = "PC2 (19.5%)") +
  theme_classic()+
  theme(axis.text   =element_text(size=16,family = "sans", face="bold"),
        axis.title.x=element_text(size=16,family = "sans", face="bold"),
        axis.title.y=element_text(size=16,family = "sans", face="bold"),
        title=element_blank(),
        legend.position = "none",
        legend.text =element_blank(),
        legend.title=element_blank())


####----Figure 2XA and 2XB: Check PCA distribution of the 14 fragmentation metrics----####

frag_df_full_glad_pca <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/frag_df_Full_glad_extent_5m.rds")


selected_var_2020 <- c("TCA_2020_ST", "LPI_2020_ST", "LDI_2020_ST", "CFI_2020", "NP_2020_ST", "MPA_2020_ST", 
                       "ED_2020_ST", "FFI_2020", "AI_2020_ST", "PCI_2020_ST", "NLSI_2020_ST", "CI_2020_ST","SI_2020_ST", "PAR_2020_ST",
                       "cover_2020", "MPC_ST", "AFI_2020", "ENN_2020_ST", "LSI_2020_ST", "PARMN_2020_ST",
                       "PLADJ_2020_ST")


selected_var_2000 <- c("TCA_ST", "LPI_ST", "LDI_ST", "CFI", "NP_ST", 
                       "MPA_ST", "ED_ST", "FFI", "AI_ST", "PCI_ST", 
                       "NLSI_ST", "CI_ST", "SI_ST", "PAR_ST",
                       "cover", "MPC_ST", "AFI", "ENN_ST", "LSI_ST", "PARMN_ST",
                       "PLADJ_ST"
)


replace_var <- c("TCA", "LPI", "LDI", "CFI", "NP", "MPA", 
                 "ED", "FFI", "AI", "PCI", "NLSI", "CI", "SI", "PAR",
                 "Cover", "MPC", "AFI", "ENN", "LSI", "PARMN", "PLADJ")



set.seed(1245)
frag_df_sub_glad_pca <- frag_df_full_glad_pca%>%group_by(Mega_Biome)%>%slice_sample(prop=1)
frag_df_sub_glad <- frag_df_sub_glad_pca[,selected_var_2000]
colnames(frag_df_sub_glad) <- replace_var


# frag_df_sub_glad$Cover <-  1-frag_df_sub_glad$Cover
# pca_result <- frag_df_sub_glad %>% pca(TCA, LPI, LDI, CFI, NP, MPA, ED, FFI)
# pca_result <- frag_df_sub_glad %>% pca(TCA, LPI, LDI, CFI, NP, MPA, ED, FFI)

data <- frag_df_sub_glad %>% dplyr::select(TCA, LPI, LDI, CFI, NP, MPA, ED, FFI,
                                           AI,
                                           PCI,
                                           # MPC,
                                           # AFI,
                                           ENN,
                                           PARMN,
                                           # Cover,
                                           NLSI,
                                           CI,
                                           SI,
                                           # PAR,
                                           # LSI,
                                           PLADJ)%>%drop_na()
pca <- prcomp(data, scale. = TRUE, center = TRUE)


pca_summary <- summary(pca)

# Print the summary to see the variance explained by each component
print(pca_summary)

# Specifically extract the proportion of variance explained
variance_explained <- pca_summary$sdev^2 / sum(pca_summary$sdev^2)
print(variance_explained)

# Extract variance explained by PC1 and PC2
variance_pc1 <- variance_explained[1]
variance_pc2 <- variance_explained[2]


scores <- as.data.frame(pca$x)  # PCA scores
loadings <- pca$rotation  # PCA loadings

loadings_df <- as.data.frame(loadings[, 1:2])  # Only consider first two PCs
loadings_df$variable <- rownames(loadings)

# loadings_df$variable <- factor(loadings_df$variable, 
#                                                 levels=c("MPC", "CFI", "TCA", "LPI", "LDI",  
#                                                          "AFI", "AI", "PAR", "PCI", "FFI",
#                                                          "MPA", "NP", "ED"
#                                                 ), 
#                                                 ordered=T)
# loadings_df$alphas <- c(0.7, 0.7, 0.7, 0.9, 0.7, 0.7, 0.7, 0.9, 0.7, 0.7, 0.9, 0.9, 0.7)

# loadings_df$sizes <- c(4, 4, 4, 6, 4, 4, 4, 6, 4, 4, 6, 6, 4)

# loadings_df$alphas <- c(0.7, 0.7, 0.7, 0.9, 0.7, 0.7, 0.7, 0.9, 0.7, 0.7, 0.9, 0.9, 0.7)

loadings_df$colors <- c("#fe8181","#fe8181","#fe8181","#ca0020","#74add1","#74add1","#74add1","#2166ac",rep("purple",8) )
# Define maximum length for arrows for visibility
# max_arrow_length <- max(abs(loadings_df[, 1]), abs(loadings_df[, 2]))
max_arrow_length <- 0.1

# loadings_df_1 <- loadings_df[loadings_df$variable%in%c("MPC", "CFI","AFI","FFI"),]
# loadings_df_2 <- loadings_df[!loadings_df$variable%in%c("MPC", "CFI","AFI","FFI"),]

# loadings_df_2$alphas <- 0.9
# colors <- c("#f4a582", "#ca0020","#ca0020","#ca0020","#ca0020","#7b3294","#7b3294","#7b3294","#7b3294",
#             "#0571b0","#0571b0","#0571b0","#0571b0")

## For year 2000
Fig2XA <-ggplot(data = scores, aes(x = PC1, y = PC2)) +
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
        title=element_blank(),
        legend.position = "none",
        legend.text =element_blank(),
        legend.title=element_blank())


# frag_df_full_glad_pca <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/frag_df_Full_glad_extent_5m.rds")

set.seed(1245)
frag_df_sub_glad_pca <- frag_df_full_glad_pca%>%group_by(Mega_Biome)%>%slice_sample(prop=1)
frag_df_sub_glad <- frag_df_sub_glad_pca[,selected_var_2020]
colnames(frag_df_sub_glad) <- replace_var


# frag_df_sub_glad$Cover <-  1-frag_df_sub_glad$Cover
# pca_result <- frag_df_sub_glad %>% pca(TCA, LPI, LDI, CFI, NP, MPA, ED, FFI)
# pca_result <- frag_df_sub_glad %>% pca(TCA, LPI, LDI, CFI, NP, MPA, ED, FFI)

data <- frag_df_sub_glad %>% dplyr::select(TCA, LPI, LDI, CFI, NP, MPA, ED, FFI,
                                           AI,
                                           PCI,
                                           # MPC,
                                           # AFI,
                                           ENN,
                                           PARMN,
                                           # Cover,
                                           NLSI,
                                           CI,
                                           SI,
                                           # PAR,
                                           # LSI,
                                           PLADJ)%>%drop_na()
pca <- prcomp(data, scale. = TRUE, center = TRUE)
pca_summary <- summary(pca)

# Print the summary to see the variance explained by each component
print(pca_summary)

# Specifically extract the proportion of variance explained
variance_explained <- pca_summary$sdev^2 / sum(pca_summary$sdev^2)
print(variance_explained)

# Extract variance explained by PC1 and PC2
variance_pc1 <- variance_explained[1]
variance_pc2 <- variance_explained[2]

scores <- as.data.frame(pca$x)  # PCA scores
loadings <- pca$rotation  # PCA loadings

loadings_df <- as.data.frame(loadings[, 1:2])  # Only consider first two PCs
loadings_df$variable <- rownames(loadings)

# loadings_df$variable <- factor(loadings_df$variable, 
#                                                 levels=c("MPC", "CFI", "TCA", "LPI", "LDI",  
#                                                          "AFI", "AI", "PAR", "PCI", "FFI",
#                                                          "MPA", "NP", "ED"
#                                                 ), 
#                                                 ordered=T)
# loadings_df$alphas <- c(0.7, 0.7, 0.7, 0.9, 0.7, 0.7, 0.7, 0.9, 0.7, 0.7, 0.9, 0.9, 0.7)

# loadings_df$sizes <- c(4, 4, 4, 6, 4, 4, 4, 6, 4, 4, 6, 6, 4)

# loadings_df$alphas <- c(0.7, 0.7, 0.7, 0.9, 0.7, 0.7, 0.7, 0.9, 0.7, 0.7, 0.9, 0.9, 0.7)

loadings_df$colors <- c("#fe8181","#fe8181","#fe8181","#ca0020","#74add1","#74add1","#74add1","#2166ac",rep("purple",8) )
# Define maximum length for arrows for visibility
# max_arrow_length <- max(abs(loadings_df[, 1]), abs(loadings_df[, 2]))
max_arrow_length <- 0.1

# loadings_df_1 <- loadings_df[loadings_df$variable%in%c("MPC", "CFI","AFI","FFI"),]
# loadings_df_2 <- loadings_df[!loadings_df$variable%in%c("MPC", "CFI","AFI","FFI"),]

# loadings_df_2$alphas <- 0.9
# colors <- c("#f4a582", "#ca0020","#ca0020","#ca0020","#ca0020","#7b3294","#7b3294","#7b3294","#7b3294",
#             "#0571b0","#0571b0","#0571b0","#0571b0")

## For year 2020
Fig2XB <-ggplot(data = scores, aes(x = PC1, y = PC2)) +
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
        title=element_blank(),
        legend.position = "none",
        legend.text =element_blank(),
        legend.title=element_blank())


####----Figure 2XC: Check PCA distribution of the selected fragmentation metrics in year 2000----####
# frag_df_full_glad_pca <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/frag_df_Full_glad_extent_5m.rds")
# colnames(frag_df_full_glad_pca)


# set.seed(1245)
# frag_df_sub_glad_pca <- frag_df_full_glad_pca%>%group_by(Mega_Biome)%>%slice_sample(prop=0.01)
# frag_df_sub_glad <- frag_df_sub_glad_pca[,selected_var_2000]
# colnames(frag_df_sub_glad) <- replace_var

# frag_df_sub_glad_pca <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Outcome/2024.10.24_ScienceResponse/frag_df_sub_glad_pca_1per.rds")
frag_df_sub_glad <- frag_df_sub_glad_pca[,selected_var_2000]
colnames(frag_df_sub_glad) <- replace_var

data <- frag_df_sub_glad %>% dplyr::select(TCA, LPI, LDI, CFI, NP, MPA, ED, FFI,
                                           AI,
                                           # PCI, 
                                           ENN,
                                           # MPC,
                                           AFI,
                                           # Cover,
                                           #NLSI, CI, SI, 
                                           PLADJ)%>%drop_na()
pca <- prcomp(data, scale. = TRUE, center = TRUE)
pca_summary <- summary(pca)

# Print the summary to see the variance explained by each component
print(pca_summary)

# Specifically extract the proportion of variance explained
variance_explained <- pca_summary$sdev^2 / sum(pca_summary$sdev^2)
print(variance_explained)

# Extract variance explained by PC1 and PC2
variance_pc1 <- variance_explained[1]
variance_pc2 <- variance_explained[2]

scores <- as.data.frame(pca$x)  # PCA scores
loadings <- pca$rotation  # PCA loadings

loadings_df <- as.data.frame(loadings[, 1:2])  # Only consider first two PCs
loadings_df$variable <- rownames(loadings)

# loadings_df$variable <- factor(loadings_df$variable, 
#                                                 levels=c("MPC", "CFI", "TCA", "LPI", "LDI",  
#                                                          "AFI", "AI", "PAR", "PCI", "FFI",
#                                                          "MPA", "NP", "ED"
#                                                 ), 
#                                                 ordered=T)
loadings_df$alphas <- c(0.7, 0.7, 0.7, 0.9, 0.7, 0.7, 0.7, 0.9, 0.7, 0.7, 0.9,  0.7)

# loadings_df$sizes <- c(4, 4, 4, 6, 4, 4, 4, 6, 4, 4, 6, 6, 4)

# loadings_df$alphas <- c(0.7, 0.7, 0.7, 0.9, 0.7, 0.7, 0.7, 0.9, 0.7, 0.7, 0.9, 0.9, 0.7)

loadings_df$colors <- c("#fe8181","#fe8181","#fe8181","#ca0020","#74add1","#74add1","#74add1","#2166ac","orange","orange","darkorange3","orange" )
# Define maximum length for arrows for visibility
# max_arrow_length <- max(abs(loadings_df[, 1]), abs(loadings_df[, 2]))
max_arrow_length <- 0.1

# loadings_df_1 <- loadings_df[loadings_df$variable%in%c("MPC", "CFI","AFI","FFI"),]
# loadings_df_2 <- loadings_df[!loadings_df$variable%in%c("MPC", "CFI","AFI","FFI"),]

# loadings_df_2$alphas <- 0.9
# colors <- c("#f4a582", "#ca0020","#ca0020","#ca0020","#ca0020","#7b3294","#7b3294","#7b3294","#7b3294",
#             "#0571b0","#0571b0","#0571b0","#0571b0")

Fig2XC <- ggplot(data = scores, aes(x = PC1, y = PC2)) +
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

####----Figure 2XD: Check PCA distribution of the selected fragmentation metrics in year 2020----####
# frag_df_full_glad_pca <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/frag_df_Full_glad_extent_5m.rds")
# colnames(frag_df_full_glad_pca)


# set.seed(1245)
# frag_df_sub_glad_pca <- frag_df_full_glad_pca%>%group_by(Mega_Biome)%>%slice_sample(prop=0.01)
# frag_df_sub_glad <- frag_df_sub_glad_pca[,selected_var_2000]
# colnames(frag_df_sub_glad) <- replace_var

# frag_df_sub_glad_pca <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Outcome/2024.10.24_ScienceResponse/frag_df_sub_glad_pca_1per.rds")
frag_df_sub_glad <- frag_df_sub_glad_pca[,selected_var_2020]
colnames(frag_df_sub_glad) <- replace_var

data <- frag_df_sub_glad %>% dplyr::select(TCA, LPI, LDI, CFI, NP, MPA, ED, FFI,
                                           AI,
                                           # PCI, 
                                           ENN,
                                           # MPC,
                                           AFI,
                                           # Cover,
                                           #NLSI, CI, SI, 
                                           PLADJ)%>%drop_na()
pca <- prcomp(data, scale. = TRUE, center = TRUE)
pca_summary <- summary(pca)

# Print the summary to see the variance explained by each component
print(pca_summary)

# Specifically extract the proportion of variance explained
variance_explained <- pca_summary$sdev^2 / sum(pca_summary$sdev^2)
print(variance_explained)

# Extract variance explained by PC1 and PC2
variance_pc1 <- variance_explained[1]
variance_pc2 <- variance_explained[2]

scores <- as.data.frame(pca$x)  # PCA scores
loadings <- pca$rotation  # PCA loadings

loadings_df <- as.data.frame(loadings[, 1:2])  # Only consider first two PCs
loadings_df$variable <- rownames(loadings)

# loadings_df$variable <- factor(loadings_df$variable, 
#                                                 levels=c("MPC", "CFI", "TCA", "LPI", "LDI",  
#                                                          "AFI", "AI", "PAR", "PCI", "FFI",
#                                                          "MPA", "NP", "ED"
#                                                 ), 
#                                                 ordered=T)
loadings_df$alphas <- c(0.7, 0.7, 0.7, 0.9, 0.7, 0.7, 0.7, 0.9, 0.7, 0.7, 0.9,  0.7)

# loadings_df$sizes <- c(4, 4, 4, 6, 4, 4, 4, 6, 4, 4, 6, 6, 4)

# loadings_df$alphas <- c(0.7, 0.7, 0.7, 0.9, 0.7, 0.7, 0.7, 0.9, 0.7, 0.7, 0.9, 0.9, 0.7)

loadings_df$colors <- c("#fe8181","#fe8181","#fe8181","#ca0020","#74add1","#74add1","#74add1","#2166ac","orange","orange","darkorange3","orange" )
# Define maximum length for arrows for visibility
# max_arrow_length <- max(abs(loadings_df[, 1]), abs(loadings_df[, 2]))
max_arrow_length <- 0.1

# loadings_df_1 <- loadings_df[loadings_df$variable%in%c("MPC", "CFI","AFI","FFI"),]
# loadings_df_2 <- loadings_df[!loadings_df$variable%in%c("MPC", "CFI","AFI","FFI"),]

# loadings_df_2$alphas <- 0.9
# colors <- c("#f4a582", "#ca0020","#ca0020","#ca0020","#ca0020","#7b3294","#7b3294","#7b3294","#7b3294",
#             "#0571b0","#0571b0","#0571b0","#0571b0")

Fig2XD <- ggplot(data = scores, aes(x = PC1, y = PC2)) +
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