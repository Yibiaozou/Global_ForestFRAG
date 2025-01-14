# Load libraries
# List necessary packages
rm(list=ls())

library(patchwork)
library(tidyverse)
library(raster)

library(tictoc)
library(plyr)
library(Hmisc)

# packages_list<-list("magrittr", "dplyr",  "MatchIt", "RItools", "this.path", "scales", "ggdendro", "data.table", "openxlsx", "Hmisc", "plyr",
#                     "tibble", "leaps", "pbapply", "RColorBrewer", "ggpubr", "ggdist", "ggh4x") # "Hmisc", "plyr"
# 
# # Install necessary packages not installed
# packagesPrev<- .packages(all.available = TRUE)
# lapply(packages_list, function(x) {   if ( ! x %in% packagesPrev ) { install.packages(x, force=T)}    })
# 
# # Load libraries

packages_list<-list("magrittr", "dplyr",  "MatchIt", "RItools", "this.path", "scales", "ggdendro", "data.table", "openxlsx", 
                    "tibble", "leaps", "pbapply", "RColorBrewer", "ggpubr", "ggdist", "ggh4x") 
lapply(packages_list, library, character.only = TRUE)


data <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/frag_df_SubSet_WDPA_T30perc_buffer40km.rds")
# data <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/frag_df_Full_glad_extent_5m.rds")


# accessibity_1 <- raster("C:/Zeus/ETHz/ETHz_S7/Forest_GFC/Data/Accessibility/global_tree_richness_accessibility0000000000-0000000000.tif")
# accessibity_2 <- raster("C:/Zeus/ETHz/ETHz_S7/Forest_GFC/Data/Accessibility/global_tree_richness_accessibility0000000000-0000032768.tif")
# 
# accessibity <- merge(accessibity_1, accessibity_2)
# plot(accessibity)
# writeRaster(accessibity, filename = "C:/Zeus/ETHz/ETHz_S7/Forest_GFC/Data/Accessibility/Accessibity.tif")
# 
# accessibity <- raster("C:/Zeus/ETHz/ETHz_S7/Forest_GFC/Data/Accessibility/Accessibity.tif")
# 
# 
# 
# 
# accessibity_frag <- raster::extract(accessibity, data[,c("longitude", "latitude")])
# data <- data %>% mutate(accessibity=accessibity_frag)
# # hist(data$accessibity)
# saveRDS(data, file="C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/frag_df_Full_glad_extent_5m.rds")

colnames(data)
data <- data[data$cover>=0.3,]

# frag_df_full_glad$hfp_mean

sum(!is.na(data$WDPA_binary))

# Specify the column name in data that defines spatial units in relation to governance type. 
# This column indicates which spatial units are associated with a type of governance (1) and which are not (0).
type_gov<- "WDPA_binary"
table(data[,type_gov])
data<- data[!is.na(data[,type_gov]),]
data<- data[!is.na(data[,"PPP_Mean"]),]
data<- data[!is.na(data[,"accessibity"]),]
data<- data[!is.na(data[,"MAT_mean"]),]
data<- data[!is.na(data[,"TAP_mean"]),]
data<- data[!is.na(data[,"fireFreq_mean"]),]
data<- data[!is.na(data[,"Humidity_mean"]),]

# test multicolinearity
# List preliminary covariates-columns in "data" table to estimate similarity for matching.
# These are considered preliminary as they will undergo multicollinearity tests and significance checks in relation to governance type.
covars<-c("MAT_mean", "TAP_mean",  "fireFreq_mean", "Humidity_mean", "PPP_Mean", "accessibity")


# Evaluate multicollinearity
formula_test_multicor<- as.formula( paste0(type_gov, "~", paste0(covars, collapse = "+")) )
test_multicor<- glm(formula_test_multicor, data = data, family = binomial()) # sort by variance inflation

print(formula_test_multicor)
test_multicor

cordataR<- summary(test_multicor, correlation=T)[["correlation"]] %>% as.data.frame.matrix()
cordataR[,"(Intercept)"]<- NULL; cordataR<- cordataR[2:nrow(cordataR), ]# ELIMINATE INTERCEPT CORRELATION MATRIX

NACol<- names(which(rowSums(is.na(cordataR)) > (ncol(cordataR)/2) ))
cordata<- cordataR %>% {.[!names(.) %in% NACol,]} %>% {.[,!colnames(.) %in% NACol]}; cordata[is.na(cordata)]<-0

str(cordata)
head(cordata)

# From the correlation matrix, we must decide which variables to remove to reduce multicollinearity. To achieve this, we generate groups of correlated variables using a correlation threshold.
cor_threshold<- 0.7 # define correlation threshold

# Covariate clustering
corhclust <- hclust(as.dist(1-abs(cordata))) 
cordend<-as.dendrogram(corhclust)
cordend_data <- dendro_data(cordend)

# Plot dendrogram of correlated variables. The dendrogram visualizes the correlation among variables, highlighting groups of correlated covariables based on the defined correlation threshold (cor_threshold) marked by a red line.
var_table <- with(cordend_data$labels, data.frame(y_center = x, y_min= x-0.5, y_max=x+0.5, Variable = as.character(label), height = 1))
col1<- "#EBEBEB"; col2<- "white"; is.even<- function(x){ x %% 2 == 0 }
var_table$col<- rep_len(c(col1, col2), length.out=length(var_table$Variable)) %>% {if(is.even(length(.))) {rev(.)} else {.}}
segment_data <- with(segment(cordend_data), data.frame(x = y, y = x, xend = yend, yend = xend, cor= 1-yend))

# ggdendroPlot <-   ggplot()+
#   annotate("rect", xmin = -0.05, xmax = 1.04, fill = var_table$col,ymin = var_table$y_min , ymax = var_table$y_max, alpha = 0.75 )+
#   geom_segment(data= segment_data, aes(x = 1-x, y = y, xend = 1-xend, yend = yend, label= cor), size= 0.3)+
#   scale_y_continuous(breaks = cordend_data$labels$x,  labels = cordend_data$labels$label )+
#   coord_cartesian(expand = F)+
#   labs(x= "Correlation", y= "Variables") +
#   geom_vline(xintercept = cor_threshold, linetype = "dashed", col= "red") +
#   theme(legend.position =  "bottom", legend.key.width = unit(50, 'pt'),
#         plot.margin = margin(t = 0, r = 0,  b = 0,l = 0),
#         panel.grid.major = element_line(color = "gray"),
#         axis.ticks.length   = unit(0.3, "mm"),
#         text = element_text(size = 10))
# 
# print(ggdendroPlot)

# Remove high-correlated variables. Following exploration, we select one variable per group to reduce multicollinearity, choosing the variable with the lowest VIF in each group. 
vif_data<- car::vif(test_multicor) %>% as.data.frame() %>% {data.frame(Var= rownames(.), VIF= .[,1])} %>% arrange(VIF)
vif_values <-  vif_data %>% 
  dplyr::mutate(Variable1= Var, VIF_var1= VIF, Variable2= Var, VIF_var2= VIF) %>%  dplyr::arrange("VIF")
rank_covars<- cutree(corhclust, h = 1-cor_threshold) %>% as.data.frame %>% rownames_to_column("Var") %>% setnames(".", "group") %>%
  dplyr::filter(!Var %in% "(Intercept)") %>% list(vif_data) %>% join_all() %>% arrange(group, VIF)

covars_no_multicol<- dplyr::filter(rank_covars, !duplicated(group))$Var

print(rank_covars)
print(covars_no_multicol)



# Optimization and adjustment for selecting the best model ####
pre_formula_glm<- as.formula( paste0(type_gov, "~", paste0(covars_no_multicol, collapse = "+")) ) # new formula with variables that do not exhibit multicollinearity

print(pre_formula_glm)

# Estimate forward and backward models. adds and removes variables through multiple iterations to find the optimal model that balances complexity with predictive power. We use AIC to select the best model.
model  <- regsubsets(pre_formula_glm, data = data, nvmax = length(covars), method = "seqrep")
summ_model<-summary(model)[c("rsq", "rss", "adjr2", "cp", "bic" )] %>% as.data.frame() %>% dplyr::mutate(model= seq(nrow(.)))
list_models<- seq(nrow(summ_model))
AIC_models<- pblapply(list_models, function(x){
  coefs<- coef(model, id = x) # get coefficients
  vars<- names(coefs)[-1] # get vars
  form_test<- as.formula( paste0(type_gov, "~", paste0(vars, collapse = "+")) ) # organize form
  glm_test<- glm(form_test, data = data, family = binomial()) # run glm
  data_AIC<- data.frame(model= x, AIC= extractAIC(glm_test)[2]) # get AIC
  data_vars <- data.frame(model= x, vars= vars) # get data vars
  data_form<- data.frame(model= x, formula = paste0(type_gov, "~", paste0(vars, collapse = "+")) ) # get data forms
  list(data_AIC=data_AIC, data_vars=data_vars, data_form= data_form )   })

# Ranking models
# Compiles the formulas of models evaluated by AIC into a single dataframe for easy comparison.
forms_models<- rbind.fill(purrr::map(AIC_models, "data_form"))

print(forms_models)

# Aggregates AIC information, merges it with summary model data, and ranks models based on BIC and AIC values to identify the best performers.
better_models<- rbind.fill(purrr::map(AIC_models, "data_AIC")) %>% list(summ_model) %>% join_all() %>% 
  dplyr::arrange( bic) %>% dplyr::mutate(rank_BIC= seq(nrow(.))) %>% 
  dplyr::arrange(AIC) %>% dplyr::mutate(rank_AIC= seq(nrow(.)))

print(better_models)

# Selection of the best model
# Allows the researcher to review the ranked models based on selected criteria (default: AIC)
critera<- "AIC" # DEFAULT

# Collates variables from the best AIC models, ranks them by frequency of appearance, and prepares them for visualization analysis.
data_vars<-  rbind.fill(purrr::map(AIC_models, "data_vars")) %>% list(better_models) %>% 
  join_all() %>%  dplyr::group_by(vars) %>% dplyr::mutate(freq_var= n()) %>% 
  dplyr::arrange(freq_var) %>% dplyr::mutate(vars= factor(vars, levels = unique(.$vars)) ) 

# Organizes and ranks models based on the selected criterion preparing for the selection of the best model.
vars_models<- data_vars %>% dplyr::arrange(eval(sym(critera))) %>% 
  dplyr::mutate(model= factor(model, levels = unique(.$model)) ) %>% as.data.frame()

# Identifies the best model based on the ranking, setting it aside for in-depth analysis and use in matching.
better_model<- unique(vars_models[1,1])

print(better_model)

# Selected variables for the best model
selected_variables<- data_vars %>% dplyr::arrange(eval(sym(critera))) %>% 
  dplyr::filter(model %in% better_model) %>% {as.character(.$vars)}

print(selected_variables)

# Plot best models' AIC by variable. The heatmap displays the top-performing models horizontally and the most impactful variables vertically. Warmer hues indicate superior AIC values, illustrating the effectiveness of each variable within the highest-ranked models as per the chosen metric (AIC). 
plot_better_model<-  ggplot()+
  geom_tile(data= vars_models, aes(x= model , y= vars, fill = eval(sym(critera)) ), color="black", alpha= 0.5, size=0.2)+
  scale_fill_gradientn(critera, colors = brewer.pal(11, "Spectral"))  

print(plot_better_model)

# Pre-matching exploratory analysis ####
# This phase involves analyzing distributions and assessing the balance of variables across the type of governance groups. It provides an initial diagnostic of the data, setting the stage for understanding the impact of the matching process.

# Propensity scores calculation.
# Identifies the 'treated' group based on type of governance, then calculates the standardized differences for selected variables before matching.
treated <-(data[,type_gov] ==1) 
cov <-data[,selected_variables]
std.diff <-apply(cov,2,function(x) 100*(mean(x[treated])- mean(x[!treated]))/(sqrt(0.5*(var(x[treated]) + var(x[!treated]))))) %>% abs()

# Generate a propensity score model
formula_glm<- as.formula( paste0(type_gov, "~", paste0(selected_variables, collapse = "+")) )

print(formula_glm)

ps <- glm(formula_glm, data = data, family = binomial())


# Estimate propensity scores predicted by the logistic model.
data$psvalue <- predict(ps, type = "response")

# Organizes standardized differences for easy analysis and visualization, highlighting variables with the most imbalance.
PreMatchingIndexData<- data.frame(abs(std.diff)) %>% set_names("imbalance") %>% tibble::rownames_to_column(var="Variable") %>%  arrange(imbalance)

# A good balance is considered to be less than 25%. For visualization purposes, imbalances greater than 100% are capped at 100%. # An imbalance over 25% indicates significant differences in group characteristics.
PreMatchingIndexDataV2<- PreMatchingIndexData  %>%  arrange(desc(imbalance)) %>%
  mutate(imbalance= ifelse(imbalance>=100,100,imbalance)) %>% 
  mutate(Variable= factor(Variable, levels = unique(.$Variable)), label= paste0(paste(paste(rep("   ",3), collapse = ""), collapse = ""), Variable, sep=""))

print(PreMatchingIndexDataV2)

# Execute matching analysis ####
# Performs matching based on the specified criteria
# The 'formula_glm' defines the treatment indicator and covariates for matching, 'method = "nearest"'.

set.seed(20240516)
# data_sample <- data%>%group_by(WDPA_binary, Mega_Biome)%>%slice_sample(prop=0.2)

saveRDS(data, file="C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/frag_df_SubSet_WDPA_T30perc_buffer40km.rds")


# data_sample <- data
data_sample <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/frag_df_SubSet_WDPA_T30perc_buffer40km.rds")
tic()
m.nn <- matchit(formula_glm, data =data_sample, method= "nearest", ratio = 1)
toc()

saveRDS(m.nn, file="C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/frag_df_Match_WDPA_T30perc_buffer40km.rds")

# Extracts the matched dataset and calculates the deforestation indicator. 'deforest' is defined as 1 if the forest status changed from present ('Fores_2000' == 1) to absent ('Fores_2021' == 0) over the study period, and 0 otherwise.
# y=match.data(m.nn, group="all")

data_sample <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/frag_df_SubSet_WDPA_T30perc_buffer40km.rds")
m.nn <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/frag_df_Match_WDPA_T30perc_buffer40km.rds")
y=match.data(m.nn, group="all")


# Propensity scores calculation. 
# type_gov<- "WDPA_binary"

treated1 <-(y[, type_gov]==1)


# selected_variables <- c("MAT_mean", "TAP_mean",  "fireFreq_mean", "Humidity_mean", "PPP_Mean", "accessibity")
cov1 <-y[, selected_variables]
std.diff1 <-apply(cov1,2,function(x) 100*(mean(x[treated1])- mean(x[!treated1]))/(sqrt(0.5*(var(x[treated1]) + var(x[!treated1]))))) 

# Organizes standardized differences for easy analysis and visualization
posmatchingIndexData<- data.frame(abs(std.diff1)) %>% set_names("imbalance") %>% tibble::rownames_to_column(var="Variable") %>%  arrange(imbalance)
posmatchingIndexDataV2<- posmatchingIndexData  %>%  arrange(desc(imbalance)) %>%
  mutate(imbalance= ifelse(imbalance>=100,100,imbalance)) %>% 
  mutate(Variable= factor(Variable, levels = unique(.$Variable)), label= paste0(paste(paste(rep("   ",3), collapse = ""), collapse = ""), Variable, sep=""))

# Estimate percent balance improvement. Organize the standardized differences before and after matching data.
summ_Imbalancedata<- plyr::rbind.fill(list( dplyr::mutate(PreMatchingIndexDataV2, Match= "Unmatched"),  dplyr::mutate(posmatchingIndexDataV2, Match= "Matched")))


# Plotting imbalance data. The plot demonstrates the improvements in variable imbalance before and after matching.
y_axis_title_unbalanced_vars_posmatch<- "Index of covariate imbalance"
x_axis_title_unbalanced_vars_posmatch<- "Variables"
legend_title_unbalanced_vars_posmatch<- "Pixeles de la ventana"
plot_title_unbalanced_vars_posmatch<- "Pos Matching"
color_vline_unbalanced_vars_posmatch<- "red"
pos_vline_unbalanced_vars_posmatch<- 25

gg_summ_Imbalancedata<- ggplot(data= summ_Imbalancedata)+  
  geom_point(aes(x=imbalance, y= Variable, color= Match), size= 4) +
  labs(x= y_axis_title_unbalanced_vars_posmatch, y= x_axis_title_unbalanced_vars_posmatch)+
  geom_vline(aes(xintercept= pos_vline_unbalanced_vars_posmatch),  size= 0.5, linetype="dashed", color = color_vline_unbalanced_vars_posmatch)+
  theme(
    plot.margin = margin(t = 0, r = 0,  b = 0,l = 0),
    axis.ticks.length   = unit(0.3, "mm"),
    text = element_text(size = 10),
    panel.background = element_rect(fill = NA), panel.grid.major = element_line(color = "gray"),
    axis.line = element_line(size = 0.5, colour = "black") )+
  scale_x_continuous(expand = c(0,0), limits = c(0,110))+
  # ggtitle(type_gov)+
  theme(axis.text   =element_text(size=16,family = "sans", face="bold"),
        axis.title.x=element_text(size=20,family = "sans", face="bold"),
        axis.title.y=element_text(size=20,family = "sans", face="bold"),
        title=element_blank(),
        # # legend.position = "none",
        legend.text =element_text(size=16,family = "sans", face="bold")
        # legend.title=element_blank()
        )

print(gg_summ_Imbalancedata)


# Plotting Imbalance Figure. ####
# This plot visualizes the distribution of propensity scores before and after matching through histograms.

# Splitting the pre-matching data based on the 'type_gov' variable to compare groups inside and outside the treatment condition.
# saveRDS(data_sample, file="C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/frag_df_SubSet_WDPA.rds")
data_sample <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/frag_df_SubSet_WDPA_T30perc_buffer40km.rds")
m.nn <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/frag_df_Match_WDPA_T30perc_buffer40km.rds")
y=match.data(m.nn, group="all")


includedListPreMatching <- split(data_sample, data_sample[, type_gov])

# Set graphic parameters for pre-matching histograms.
in_area_prematch <- list(title= paste0("In ", type_gov), color= "goldenrod")
out_area_prematch <- list(title= paste0("Out ", type_gov), color= "olivedrab")
y_axis_title_prematch <- "Number of Units"
x_axis_title_prematch <- "Propensity Score"
legend_title_prematch <- "Window Pixels"
plot_title_prematch <- "Pre Matching"

# Create pre-matching plot.
PreMatchversusplot <- ggplot(data = includedListPreMatching[["0"]]) +
  geom_histogram(data = includedListPreMatching[["1"]], aes(x = psvalue, y = ..count.., 
                                                            fill = in_area_prematch$title, color = in_area_prematch$title)) +
  geom_histogram(aes(x = psvalue, y = -..count.., fill = out_area_prematch$title, color = out_area_prematch$title)) +
  scale_y_continuous(labels = abs, limits = c(-10000, 10000)) +
  scale_fill_manual(values = alpha(c(out_area_prematch$color, in_area_prematch$color), 0.5), name = legend_title_prematch) +
  scale_color_manual(values = alpha(c(out_area_prematch$color, in_area_prematch$color), 1), name = legend_title_prematch) +
  labs(x = x_axis_title_prematch, y = y_axis_title_prematch, title = plot_title_prematch) +
  guides(size = "none", fill = guide_legend(title.position="top", title.hjust = 0.5)) +
  theme_minimal() + theme(legend.position = "bottom", text = element_text(size = 10))+
  theme(axis.text   =element_text(size=16,family = "sans", face="bold"),
        axis.title.x=element_text(size=20,family = "sans", face="bold"),
        axis.title.y=element_text(size=20,family = "sans", face="bold")
        # title=element_blank(),
        # # legend.position = "none",
        # legend.text =element_text(size=16,family = "sans", face="bold"),
        # legend.title=element_blank()
  )

# Splitting the post-matching data.
includedListposmatching <- split(y, y[, type_gov])

# Set graphic parameters for post-matching histograms.
in_area_posmatch <- list(title= paste0("In ", type_gov), color= "goldenrod")
out_area_posmatch <- list(title= paste0("Out ", type_gov), color= "olivedrab")
y_axis_title_posmatch <- "Number of Units"
x_axis_title_posmatch <- "Propensity Score"
legend_title_posmatch <- "Window Pixels"
plot_title_posmatch <- "Post Matching"

# Create post-matching plot.
posmatchversusplot <- ggplot(data = includedListposmatching[["0"]]) +
  geom_histogram(data = includedListposmatching[["1"]], aes(x = psvalue, y = ..count.., 
                                                            fill = in_area_posmatch$title, color = in_area_posmatch$title)) +
  geom_histogram(aes(x = psvalue, y = -..count.., fill = out_area_posmatch$title, color = out_area_posmatch$title)) +
  scale_y_continuous(labels = abs, limits = c(-10000, 10000)) +
  scale_fill_manual(values = alpha(c(out_area_posmatch$color, in_area_posmatch$color), 0.5), name = legend_title_posmatch) +
  scale_color_manual(values = alpha(c(out_area_posmatch$color, in_area_posmatch$color), 1), name = legend_title_posmatch) +
  labs(x = x_axis_title_posmatch, y = y_axis_title_posmatch, title = plot_title_posmatch) +
  scale_x_continuous(limits = c(0, 1)) +
  guides(size = "none", fill = guide_legend(title.position="top", title.hjust = 0.5)) +
  theme_minimal() + theme(legend.position = "bottom", text = element_text(size = 10))+
  theme(axis.text   =element_text(size=16,family = "sans", face="bold"),
        axis.title.x=element_text(size=20,family = "sans", face="bold"),
        axis.title.y=element_text(size=20,family = "sans", face="bold")
        # title=element_blank(),
        # # legend.position = "none",
        # legend.text =element_text(size=16,family = "sans", face="bold"),
        # legend.title=element_blank()
  )

# Arrange pre and post matching plots together for comparison.
summ_matching_propension_plot <- ggpubr::ggarrange(plotlist = list(PreMatchversusplot, posmatchversusplot), common.legend = T, legend = "bottom")

print(summ_matching_propension_plot)



# Analysis post-matching ####

library(tidyverse)
data_sample <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/frag_df_SubSet_WDPA_T30perc_buffer40km.rds")
m.nn <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/frag_df_Match_WDPA_T30perc_buffer40km.rds")

# Extracts the matched dataset and calculates the deforestation indicator. 'deforest' is defined as 1 if the forest status changed from present ('Fores_2000' == 1) to absent ('Fores_2021' == 0) over the study period, and 0 otherwise.
y = match.data(m.nn, group="all")


# Map the matched groups
# Retrieve matching pairs from the 'matchit' result to align the treatment (T) and control (C) groups for further analysis.
matches <- data.frame(m.nn$match.matrix)
group1 <- match(row.names(matches), row.names(y))
group2 <- match(matches[, 1], row.names(y))


# y$group <- NA
# y$group[group1] <- "Protected"
# y$group[group2] <- "Non-Protected"
# 
# table(y$group)
# 
# in_area_posmatch <- list(title= paste0("In ", type_gov), color= "goldenrod")
# out_area_posmatch <- list(title= paste0("Out ", type_gov), color= "olivedrab")

table(y$WDPA_binary)
hist(y$CFI_diff)

se <- function(a){
  a <- a[!is.na(a)]
  return(sd(a)/sqrt(length((a))))
}

 
## CFI analysis ####
MS_FRAG_Analysis <- function(Biome){
  if(Biome=="Global"){
    y <- y
    data_sample <- data_sample
  }else{
    y <- y[y$Mega_Biome==Biome,]
    data_sample <- data_sample[data_sample$Mega_Biome==Biome,]
  }
  y_aggregate_1 <- y[y$WDPA_binary==1,]%>%group_by(WDPA_binary)%>%summarise(CFI_diff_mean=mean(CFI_diff, na.rm=T), 
                                                                            CFI_diff_sd=sd(CFI_diff, na.rm=T),
                                                                            CFI_diff_se=se(CFI_diff),
                                                                            Cover_diff_mean=mean(cover_diff, na.rm=T), 
                                                                            Cover_diff_sd=sd(cover_diff, na.rm=T),
                                                                            Cover_diff_se=se(cover_diff))
  
  y_aggregate_1 <- y_aggregate_1%>%mutate(MS="Protected")
  
  y_aggregate_0 <- y[y$WDPA_binary==0,]%>%group_by(WDPA_binary)%>%summarise(CFI_diff_mean=mean(CFI_diff, na.rm=T), 
                                                                            CFI_diff_sd=sd(CFI_diff, na.rm=T),
                                                                            CFI_diff_se=se(CFI_diff),
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
                                                                                                   Cover_diff_mean=mean(cover_diff, na.rm=T), 
                                                                                                   Cover_diff_sd=sd(cover_diff, na.rm=T),
                                                                                                   Cover_diff_se=se(cover_diff))
  data_aggregate_0 <- data_aggregate_0%>%mutate(MS="All Outside")
  
  frag_df_aggregate <- rbind(y_aggregate_1, y_aggregate_0, data_aggregate_0)
  
  frag_df_aggregate$MS <- factor(frag_df_aggregate$MS, 
                                 levels=c("Protected", "Matched Outside", "All Outside"),
                                 ordered=T)
  frag_df_aggregate$Biome <- Biome
  return(frag_df_aggregate)
}


# y$WDPA_binary <- as.numeric(y$WDPA_binary)
# ggplot(y, aes(x=WDPA_binary, y=CFI_diff, color=WDPA_binary, fill=WDPA_binary))+
#   geom_boxplot(alpha=0.2, width=0.6, outlier.alpha = 0, notch=T)+
#   # geom_jitter(width = 0.2)+
#   # geom_violin(alpha=0.2)+
#   ylim(c(-0.1,0.1))+
#   theme_classic()+
#   # geom_hline(yintercept=0, color="black", linewidth=1, linetype="dashed")+
#   # labs(
#   #   y = var)+
#   xlab("WDPA Category")+
#   ylab("Difference in CFI")+
#   # scale_x_log10()+
#   theme(title = element_text(size=12,face="bold"),
#         axis.text.x=element_blank(),
#         axis.text.y=element_text(size=12, face="bold"),
#         axis.title.x=element_blank(),
#         axis.title.y=element_text(size=14,face="bold"),
#         legend.text = element_text(colour = "black", size = 10, face = "bold"),
#         legend.title = element_blank())

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

Fig1B <- ggplot(frag_df_aggregate_Tropical, aes(x=MS, y=CFI_diff_mean)) +
  geom_bar(position="stack", stat="identity", alpha=0.4, color="black", width=0.5)+
  geom_errorbar(aes(ymin = CFI_diff_mean  -3*CFI_diff_se , ymax = CFI_diff_mean +3*CFI_diff_se), width=0.1,linewidth=1,) +  # 误差区间
  # ylim(c(0,0.04))+
  # geom_hline(yintercept=0, color="black", linewidth=1, linetype="dashed")+
  labs(x = "WDPA Category", y = "Change in CFI")+
  # coord_cartesian(ylim=c(0.0019,0.04))+
  # scale_y_continuous(expand=c(0,0), limits = c(-0.005, 0.03))+
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

frag_df_aggregate_Temperate <- MS_FRAG_Analysis("Temperate")

Fig1C <- ggplot(frag_df_aggregate_Temperate, aes(x=MS, y=CFI_diff_mean)) +
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
  ggtitle("Temperate")+
  theme(title = element_text(size=18,family = "sans", face="bold"),
        axis.text   =element_text(size=18,family="sans", face="bold"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=18,family="sans", face="bold"),
        legend.text =element_text(size=18,family="sans", face="bold"),
        # legend.position = "none",
        legend.title=element_text(size=18,family="sans", face="bold"))

frag_df_aggregate_Boreal <- MS_FRAG_Analysis("Boreal")

Fig1D <- ggplot(frag_df_aggregate_Boreal, aes(x=MS, y=CFI_diff_mean)) +
  geom_bar(position="stack", stat="identity", alpha=0.4, color="black", width=0.5)+
  geom_errorbar(aes(ymin = CFI_diff_mean  -3*CFI_diff_se , ymax = CFI_diff_mean +3*CFI_diff_se), width=0.1,linewidth=1,) +  # 误差区间
  # ylim(c(0,0.04))+
  # geom_hline(yintercept=0, color="black", linewidth=1, linetype="dashed")+
  labs(x = "WDPA Category", y = "Change in CFI")+
  # coord_cartesian(ylim=c(0.0019,0.04))+
  # scale_y_continuous(expand=c(0,0), limits = c(-0.005, 0.03))+
  # scale_color_manual(values=c("#ca0020","#0571b0"))+
  # scale_fill_manual(values=c("#ca0020","#0571b0"))+
  theme_classic()+
  ggtitle("Boreal")+
  theme(title = element_text(size=18,family = "sans", face="bold"),
        axis.text   =element_text(size=18,family="sans", face="bold"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=18,family="sans", face="bold"),
        legend.text =element_text(size=18,family="sans", face="bold"),
        # legend.position = "none",
        legend.title=element_text(size=18,family="sans", face="bold"))

layout <- "
AB
CD
"

#Merge plots
MainPlot = Fig1A + Fig1B + Fig1C + Fig1D +
  plot_layout(design = layout) + plot_annotation(tag_levels = 'A')&
  theme(plot.tag = element_text(face = 'bold'))

MainPlot

ggsave("C:/Zeus/ETHz/ETHz_S7/ProtectArea_Evaluation/Results/Week1_20240708/Figure1_MatchedFRAG.pdf", plot=MainPlot, width=14, height=9)


## Driver analysis ####
data_sample <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/frag_df_SubSet_WDPA_T30perc.rds")
m.nn <- readRDS("C:/Zeus/ETHz/ETHz_S5/Forest_FRAG/Data/FRAG_Raster/frag_df_Match_WDPA_T30perc.rds")

# Extracts the matched dataset and calculates the deforestation indicator. 'deforest' is defined as 1 if the forest status changed from present ('Fores_2000' == 1) to absent ('Fores_2021' == 0) over the study period, and 0 otherwise.
y = match.data(m.nn, group="all")

data_sample_select <- data_sample%>%dplyr::select(longitude, latitude, 
                                                  CFI_diff, WDPA_binary,
                                                  cover, Driver,Mega_Biome)

y_select <- y%>%dplyr::select(longitude, latitude, 
                                                  CFI_diff, WDPA_binary,
                                                  cover, Driver,Mega_Biome)

data_sample_select_0 <- data_sample_select[data_sample_select$WDPA_binary==0,]

data_sample_select_0$MS <- "All Outside"

y_select$MS <- ifelse(y_select$WDPA_binary==1, "Protected", "Matched Outside")

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
  
  Area_2 <- sum(df_sub_MS_Driver_aggregate$RegionArea[df_sub_MS_Driver_aggregate$MS=="Protected"], na.rm=T)
  Area_1 <- sum(df_sub_MS_Driver_aggregate$RegionArea[df_sub_MS_Driver_aggregate$MS=="Matched Outside"], na.rm=T)
  Area_0 <- sum(df_sub_MS_Driver_aggregate$RegionArea[df_sub_MS_Driver_aggregate$MS=="All Outside"], na.rm=T)
  
  df_sub_MS_Driver_aggregate$MS <- factor(df_sub_MS_Driver_aggregate$MS, 
                                                 levels=c("Protected","Matched Outside","All Outside"),
                                                 ordered=T)
  
  df_sub_MS_Driver_aggregate$RegionArea_Perc <- NA
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
  scale_y_continuous(expand=c(0,0), limits = c(0,1))+
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
  scale_y_continuous(expand=c(0,0), limits = c(0,1))+
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

#Merge plots
MainPlot2 = Fig2A + Fig2B  +
  plot_layout(design = layout) + plot_annotation(tag_levels = 'A')&
  theme(plot.tag = element_text(face = 'bold'))

MainPlot2

ggsave("C:/Zeus/ETHz/ETHz_S7/ProtectArea_Evaluation/Results/Week1_20240708/Figure2_MS_Driver.pdf", plot=MainPlot2, width=17, height=7.5)


