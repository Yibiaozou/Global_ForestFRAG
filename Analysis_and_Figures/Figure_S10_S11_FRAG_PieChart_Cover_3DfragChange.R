rm(list=ls())
library(tidyverse)
library(tictoc)
library(ggrepel)


####----Fragmentation scenarios: Make pie chart----####
frag_df_full_glad <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/frag_df_Full_glad_extent_5m.rds")
frag_df_sub_glad <- frag_df_full_glad[frag_df_full_glad$cover>=0.3,c("cover_diff", "CFI_diff", "AFI_diff", "FFI_diff","pixelArea")]
# %>%drop_na()

colnames(frag_df_sub_glad) <- c("cover_diff", "cfi_diff", "afi_diff", "ffi_diff", "pixelArea")
## overall
A1 <- frag_df_sub_glad[(frag_df_sub_glad$cover_diff<0),]
A2 <- frag_df_sub_glad[(frag_df_sub_glad$cover_diff==0),]
A3 <- frag_df_sub_glad[(frag_df_sub_glad$cover_diff>0),]

# C1 <- nrow(A1)
# C2 <- nrow(A2)
# C3 <- nrow(A3)

C1 <- sum(A1$pixelArea, na.rm=T)
C2 <- sum(A2$pixelArea, na.rm=T)
C3 <- sum(A3$pixelArea, na.rm=T)

Cs <- sum(c(C1, C2, C3))

fre_frag_df <- data.frame(cbind(Frequency=c(C1/Cs, C2/Cs, C3/Cs),
                                Category=as.factor(c(1, 2, 3)),
                                Scenario=c("Cover down", "Cover unchange", "Cover up")
))

# colorBasic <- rainbow(9)
# colors <- c(colorBasic[1],colorBasic[9],colorBasic[2:8])

fre_frag_df$Frequency <- as.numeric(fre_frag_df$Frequency)
fre_frag_df <- fre_frag_df%>%mutate(Label=paste(Scenario,paste0(round(Frequency*100,1), "%"), sep="/n")) 

ggplot(fre_frag_df, aes(x = "", y = Frequency, fill = Scenario), alpha=0.9) +
  geom_bar(stat = "identity", width = 2, color="white") +
  coord_polar(theta = "y", start=0) +
  geom_text(color="black", size=8,alpha=1, aes(label = paste0(round(Frequency*100,1), "%")), position = position_stack(vjust=0.5)) +
  # scale_fill_viridis_d(direction=-1)+
  scale_fill_manual(values=c("#e5f5f9","#99d8c9","#2ca25f"))+
  theme_void()+
  theme(legend.text =element_text(size=18,family="serif"),
        legend.position = "none",
        legend.title=element_text(size=18,family="serif"))


## for regions with decreased forest cover
A1.1 <- A1[(A1$cfi_diff>0)&(A1$afi_diff>0)&(A1$ffi_diff>0),]%>%drop_na()
A1.2 <- A1[(A1$cfi_diff>0)&(A1$afi_diff>0)&(A1$ffi_diff<=0),]%>%drop_na()
A1.3 <- A1[(A1$cfi_diff>0)&(A1$afi_diff<=0)&(A1$ffi_diff>0),]%>%drop_na()
A1.4 <- A1[(A1$cfi_diff>0)&(A1$afi_diff<=0)&(A1$ffi_diff<=0),]%>%drop_na()
A1.5 <- A1[(A1$cfi_diff<=0)&(A1$afi_diff>0)&(A1$ffi_diff>0),]%>%drop_na()
A1.6 <- A1[(A1$cfi_diff<=0)&(A1$afi_diff>0)&(A1$ffi_diff<=0),]%>%drop_na()
A1.7 <- A1[(A1$cfi_diff<=0)&(A1$afi_diff<=0)&(A1$ffi_diff>0),]%>%drop_na()
A1.8 <- A1[(A1$cfi_diff<=0)&(A1$afi_diff<=0)&(A1$ffi_diff<=0),]%>%drop_na()

C1.1 <- sum(A1.1$pixelArea, na.rm=T)
C1.2 <- sum(A1.2$pixelArea, na.rm=T)
C1.3 <- sum(A1.3$pixelArea, na.rm=T)
C1.4 <- sum(A1.4$pixelArea, na.rm=T)
C1.5 <- sum(A1.5$pixelArea, na.rm=T)
C1.6 <- sum(A1.6$pixelArea, na.rm=T)
C1.7 <- sum(A1.7$pixelArea, na.rm=T)
C1.8 <- sum(A1.8$pixelArea, na.rm=T)



Cs_1 <- sum(c(C1.1, C1.2, C1.3, C1.4, C1.5, C1.6, C1.7, C1.8))

fre_frag_df_1 <- data.frame(cbind(Frequency=c(C1.1/Cs_1, 
                                              C1.2/Cs_1, 
                                              C1.3/Cs_1,
                                              C1.4/Cs_1,
                                              C1.5/Cs_1, 
                                              C1.6/Cs_1,
                                              C1.7/Cs_1, 
                                              C1.8/Cs_1 
),
Category=as.factor(c(1, 2, 3, 4, 5, 6, 7, 8)),
Scenario=c("ΔCFI > 0, ΔAFI > 0, ΔFFI > 0",
           "ΔCFI > 0, ΔAFI > 0, ΔFFI ≤ 0",
           "ΔCFI > 0, ΔAFI ≤ 0, ΔFFI > 0",
           "ΔCFI > 0, ΔAFI ≤ 0, ΔFFI ≤ 0",
           "ΔCFI ≤ 0, ΔAFI > 0, ΔFFI > 0",
           "ΔCFI ≤ 0, ΔAFI > 0, ΔFFI ≤ 0",
           "ΔCFI ≤ 0, ΔAFI ≤ 0, ΔFFI > 0",
           "ΔCFI ≤ 0, ΔAFI ≤ 0, ΔFFI ≤ 0"

)))

fre_frag_df_1 <- fre_frag_df_1%>%mutate(Scenario=fct_relevel(Scenario, 
                                                             "ΔCFI > 0, ΔAFI > 0, ΔFFI > 0",
                                                             "ΔCFI > 0, ΔAFI > 0, ΔFFI ≤ 0",
                                                             "ΔCFI > 0, ΔAFI ≤ 0, ΔFFI > 0",
                                                             "ΔCFI > 0, ΔAFI ≤ 0, ΔFFI ≤ 0",
                                                             "ΔCFI ≤ 0, ΔAFI > 0, ΔFFI > 0",
                                                             "ΔCFI ≤ 0, ΔAFI > 0, ΔFFI ≤ 0",
                                                             "ΔCFI ≤ 0, ΔAFI ≤ 0, ΔFFI > 0",
                                                             "ΔCFI ≤ 0, ΔAFI ≤ 0, ΔFFI ≤ 0"))

# colorBasic <- rainbow(9)
# colors <- c(colorBasic[1],colorBasic[9],colorBasic[2:8])

fre_frag_df_1$Frequency <- as.numeric(fre_frag_df_1$Frequency)
ggplot(fre_frag_df_1, aes(x = "", y = Frequency, fill = Scenario), alpha=0.9) +
  geom_bar(stat = "identity", width = 2, color="white") +
  coord_polar(theta = "y") +
  # geom_text(color="white", size=8,alpha=1, aes(label = paste0(round(Frequency*100,0), "%")), position = position_stack(vjust=0.5)) + 
  geom_text_repel(color="black", size=8,alpha=1, aes(label = paste0(round(Frequency*100,1), "%")), position = position_stack(vjust=0.5)) + 
  # labs(title = "Pie Chart of Value Counts")+
  # scale_fill_manual(values = c("#ca0020", "#f4a582", "#92c5de", "#0571b0")) + # "#2bced1",
  scale_fill_manual(values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", 
                               "#ff7f00", "#ffff33", "#a65628", "#f781bf")) + # "#2bced1",
  # scale_fill_manual(values=colors)+
  # scale_fill_viridis_d(direction=-1)+
  theme_void()+
  theme(legend.text =element_text(size=18,family="serif"),
        legend.title=element_text(size=18,family="serif"),
        legend.position = "none")

# ggplot(fre_frag_df_1, aes(x = "", y = Frequency, fill = Scenario))+
#   geom_bar(width = 1, stat = 'identity')+
#   coord_polar('y', start = 0)+
#   scale_fill_brewer(palette = 'YlGn', direction = -1)+
#   theme_minimal()+
#   theme(axis.title = element_blank(), panel.border = element_blank(), panel.grid = element_blank(), axis.ticks = element_blank(), plot.title = element_text(size = 14, face = 'bold'))+
#   theme(axis.text.x = element_blank())+
#   geom_text(aes(y = Frequency/3 + c(0, cumsum(Frequency)[-length(Frequency)]), label = paste0(round(Frequency*100,1), "%")), size = 5)


## for regions with unchanged forest cover
A2.1 <- A2[(A2$cfi_diff>0)&(A2$afi_diff>0)&(A2$ffi_diff>0),]%>%drop_na()
A2.2 <- A2[(A2$cfi_diff>0)&(A2$afi_diff>0)&(A2$ffi_diff<=0),]%>%drop_na()
A2.3 <- A2[(A2$cfi_diff>0)&(A2$afi_diff<=0)&(A2$ffi_diff>0),]%>%drop_na()
A2.4 <- A2[(A2$cfi_diff>0)&(A2$afi_diff<=0)&(A2$ffi_diff<=0),]%>%drop_na()
A2.5 <- A2[(A2$cfi_diff<=0)&(A2$afi_diff>0)&(A2$ffi_diff>0),]%>%drop_na()
A2.6 <- A2[(A2$cfi_diff<=0)&(A2$afi_diff>0)&(A2$ffi_diff<=0),]%>%drop_na()
A2.7 <- A2[(A2$cfi_diff<=0)&(A2$afi_diff<=0)&(A2$ffi_diff>0),]%>%drop_na()
A2.8 <- A2[(A2$cfi_diff<=0)&(A2$afi_diff<=0)&(A2$ffi_diff<=0),]%>%drop_na()

C2.1 <- sum(A2.1$pixelArea, na.rm=T)
C2.2 <- sum(A2.2$pixelArea, na.rm=T)
C2.3 <- sum(A2.3$pixelArea, na.rm=T)
C2.4 <- sum(A2.4$pixelArea, na.rm=T)
C2.5 <- sum(A2.5$pixelArea, na.rm=T)
C2.6 <- sum(A2.6$pixelArea, na.rm=T)
C2.7 <- sum(A2.7$pixelArea, na.rm=T)
C2.8 <- sum(A2.8$pixelArea, na.rm=T)



Cs_2 <- sum(c(C2.1, C2.2, C2.3, C2.4, C2.5, C2.6, C2.7, C2.8))

fre_frag_df_2 <- data.frame(cbind(Frequency=c(C2.1/Cs_2, 
                                              C2.2/Cs_2, 
                                              C2.3/Cs_2,
                                              C2.4/Cs_2,
                                              C2.5/Cs_2, 
                                              C2.6/Cs_2,
                                              C2.7/Cs_2, 
                                              C2.8/Cs_2 
),
Category=as.factor(c(1, 2, 3, 4, 5, 6, 7, 8)),
Scenario=c("ΔCFI > 0, ΔAFI > 0, ΔFFI > 0",
           "ΔCFI > 0, ΔAFI > 0, ΔFFI ≤ 0",
           "ΔCFI > 0, ΔAFI ≤ 0, ΔFFI > 0",
           "ΔCFI > 0, ΔAFI ≤ 0, ΔFFI ≤ 0",
           "ΔCFI ≤ 0, ΔAFI > 0, ΔFFI > 0",
           "ΔCFI ≤ 0, ΔAFI > 0, ΔFFI ≤ 0",
           "ΔCFI ≤ 0, ΔAFI ≤ 0, ΔFFI > 0",
           "ΔCFI ≤ 0, ΔAFI ≤ 0, ΔFFI ≤ 0"
           
)))

fre_frag_df_2 <- fre_frag_df_2%>%mutate(Scenario=fct_relevel(Scenario, 
                                                             "ΔCFI > 0, ΔAFI > 0, ΔFFI > 0",
                                                             "ΔCFI > 0, ΔAFI > 0, ΔFFI ≤ 0",
                                                             "ΔCFI > 0, ΔAFI ≤ 0, ΔFFI > 0",
                                                             "ΔCFI > 0, ΔAFI ≤ 0, ΔFFI ≤ 0",
                                                             "ΔCFI ≤ 0, ΔAFI > 0, ΔFFI > 0",
                                                             "ΔCFI ≤ 0, ΔAFI > 0, ΔFFI ≤ 0",
                                                             "ΔCFI ≤ 0, ΔAFI ≤ 0, ΔFFI > 0",
                                                             "ΔCFI ≤ 0, ΔAFI ≤ 0, ΔFFI ≤ 0"))

# colorBasic <- rainbow(9)
# colors <- c(colorBasic[1],colorBasic[9],colorBasic[2:8])

fre_frag_df_2$Frequency <- as.numeric(fre_frag_df_2$Frequency)
ggplot(fre_frag_df_2, aes(x = "", y = Frequency, fill = Scenario), alpha=0.9) +
  geom_bar(stat = "identity", width = 2, color="white") +
  coord_polar(theta = "y") +
  geom_text_repel(color="black", size=8,alpha=1, aes(label = paste0(round(Frequency*100,1), "%")), position = position_stack(vjust=0.5)) + 
  # labs(title = "Pie Chart of Value Counts")+
  # scale_fill_manual(values = c("#ca0020", "#f4a582", "#92c5de", "#0571b0")) + # "#2bced1",
  scale_fill_manual(values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", 
                               "#ff7f00", "#ffff33", "#a65628", "#f781bf")) + # "#2bced1",
  # scale_fill_manual(values=colors)+
  # scale_fill_viridis_d(direction=-1)+
  theme_void()+
  theme(legend.text =element_text(size=18,family="serif"),
        legend.title=element_text(size=18,family="serif"),
        legend.position = "none")

## for regions with increased forest cover
A3.1 <- A3[(A3$cfi_diff>0)&(A3$afi_diff>0)&(A3$ffi_diff>0),]%>%drop_na()
A3.2 <- A3[(A3$cfi_diff>0)&(A3$afi_diff>0)&(A3$ffi_diff<=0),]%>%drop_na()
A3.3 <- A3[(A3$cfi_diff>0)&(A3$afi_diff<=0)&(A3$ffi_diff>0),]%>%drop_na()
A3.4 <- A3[(A3$cfi_diff>0)&(A3$afi_diff<=0)&(A3$ffi_diff<=0),]%>%drop_na()
A3.5 <- A3[(A3$cfi_diff<=0)&(A3$afi_diff>0)&(A3$ffi_diff>0),]%>%drop_na()
A3.6 <- A3[(A3$cfi_diff<=0)&(A3$afi_diff>0)&(A3$ffi_diff<=0),]%>%drop_na()
A3.7 <- A3[(A3$cfi_diff<=0)&(A3$afi_diff<=0)&(A3$ffi_diff>0),]%>%drop_na()
A3.8 <- A3[(A3$cfi_diff<=0)&(A3$afi_diff<=0)&(A3$ffi_diff<=0),]%>%drop_na()

C3.1 <- sum(A3.1$pixelArea, na.rm=T)
C3.2 <- sum(A3.2$pixelArea, na.rm=T)
C3.3 <- sum(A3.3$pixelArea, na.rm=T)
C3.4 <- sum(A3.4$pixelArea, na.rm=T)
C3.5 <- sum(A3.5$pixelArea, na.rm=T)
C3.6 <- sum(A3.6$pixelArea, na.rm=T)
C3.7 <- sum(A3.7$pixelArea, na.rm=T)
C3.8 <- sum(A3.8$pixelArea, na.rm=T)



Cs_3 <- sum(c(C3.1, C3.2, C3.3, C3.4, C3.5, C3.6, C3.7, C3.8))

fre_frag_df_3 <- data.frame(cbind(Frequency=c(C3.1/Cs_3, 
                                              C3.2/Cs_3, 
                                              C3.3/Cs_3,
                                              C3.4/Cs_3,
                                              C3.5/Cs_3, 
                                              C3.6/Cs_3,
                                              C3.7/Cs_3, 
                                              C3.8/Cs_3 
),
Category=as.factor(c(1, 2, 3, 4, 5, 6, 7, 8)),
Scenario=c("ΔCFI > 0, ΔAFI > 0, ΔFFI > 0",
           "ΔCFI > 0, ΔAFI > 0, ΔFFI ≤ 0",
           "ΔCFI > 0, ΔAFI ≤ 0, ΔFFI > 0",
           "ΔCFI > 0, ΔAFI ≤ 0, ΔFFI ≤ 0",
           "ΔCFI ≤ 0, ΔAFI > 0, ΔFFI > 0",
           "ΔCFI ≤ 0, ΔAFI > 0, ΔFFI ≤ 0",
           "ΔCFI ≤ 0, ΔAFI ≤ 0, ΔFFI > 0",
           "ΔCFI ≤ 0, ΔAFI ≤ 0, ΔFFI ≤ 0"
           
)))

fre_frag_df_3 <- fre_frag_df_3%>%mutate(Scenario=fct_relevel(Scenario, 
                                                             "ΔCFI > 0, ΔAFI > 0, ΔFFI > 0",
                                                             "ΔCFI > 0, ΔAFI > 0, ΔFFI ≤ 0",
                                                             "ΔCFI > 0, ΔAFI ≤ 0, ΔFFI > 0",
                                                             "ΔCFI > 0, ΔAFI ≤ 0, ΔFFI ≤ 0",
                                                             "ΔCFI ≤ 0, ΔAFI > 0, ΔFFI > 0",
                                                             "ΔCFI ≤ 0, ΔAFI > 0, ΔFFI ≤ 0",
                                                             "ΔCFI ≤ 0, ΔAFI ≤ 0, ΔFFI > 0",
                                                             "ΔCFI ≤ 0, ΔAFI ≤ 0, ΔFFI ≤ 0"))

# colorBasic <- rainbow(9)
# colors <- c(colorBasic[1],colorBasic[9],colorBasic[2:8])

fre_frag_df_3$Frequency <- as.numeric(fre_frag_df_3$Frequency)
ggplot(fre_frag_df_3, aes(x = "", y = Frequency, fill = Scenario), alpha=0.9) +
  geom_bar(stat = "identity", width = 2, color="white") +
  coord_polar(theta = "y") +
  geom_text_repel(color="black", size=8,alpha=1, aes(label = paste0(round(Frequency*100,1), "%")), position = position_stack(vjust=0.5)) + 
  # labs(title = "Pie Chart of Value Counts")+
  # scale_fill_manual(values = c("#ca0020", "#f4a582", "#92c5de", "#0571b0")) + # "#2bced1",
  scale_fill_manual(values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", 
                               "#ff7f00", "#ffff33", "#a65628", "#f781bf")) + # "#2bced1",
  # scale_fill_manual(values=colors)+
  # scale_fill_viridis_d(direction=-1)+
  theme_void()+
  theme(legend.text =element_text(size=18,family="serif"),
        legend.title=element_text(size=18,family="serif"),
        legend.position = "none")