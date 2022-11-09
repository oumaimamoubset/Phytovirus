---
title: " ANR Phytovirus (#1) "
author: "Virginie Ravigné, OUmaima Moubset, Frédéric Mahé"
date: '09 novembre 2022'

output:
  rmarkdown::html_document:
    theme: lumen
    toc: yes
    toc_float: TRUE
    keep_md: yes
    # code_folding: hide
---



***

#### Load packages


```r
library(readxl)
library(tidyverse)
library(janitor)
library(purrr)
```
#### Generate the plant data set from field reports in /data

```plant
source("scripts/Distrib_cover_fred.R")
plant_data %>% summary()
```
  
# # Simplified version of the data set to focus on features of quadrats (not plants within quadrats)
# plant_data %>%
#   select(
#     Year,
#     Locality,
#     Grid,
#     Quadrat,
#     Vegetation,
#     Total_cover,
#     Total_cover_max,
#     Plant_richness,
#     Plant_diversity
#   ) %>%
#   distinct() -> quadrat_data
# 
# quadrat_data %>% view()
# 
# # Graphs----
# ## Distribution of total cover----
# ggplot(quadrat_data, aes(fill = Locality, y = Total_cover, x = Grid)) +
#   geom_boxplot(position = "dodge") +
#   theme(axis.text.x = element_text(angle = 90)) +
#   ggtitle("Distribution of total cover over all grids of ANG, BDX and CAM")
# ggsave(
#   "Distrib_total_cover_by_grid.pdf",
#   width = 20,
#   height = 15,
#   units = "cm"
# )
# 
# ggplot(quadrat_data, aes(x = Total_cover, fill = Locality)) +
#   geom_histogram(position = "dodge") +
#   ggtitle("Distribution of total cover over all grids of ANG, BDX and CAM")
# ggsave(
#   "Distrib_total_cover_by_loc.pdf",
#   width = 20,
#   height = 15,
#   units = "cm"
# )
# 
# ## Correspondence between Vegetation and Total_cover----
# quadrat_data %>%
#   filter(Locality == "CAM") %>%
#   ggplot(., aes(x = Vegetation, y = Total_cover)) +
#   geom_point(position = position_jitter(0.1)) +
#   geom_boxplot() +
#   theme(axis.text.x = element_text(angle = 90)) +
#   ggtitle("Vegetation vs total cover")
# ggsave(
#   "Vegetation_vs_total_cover.pdf",
#   width = 20,
#   height = 15,
#   units = "cm"
# )
# 
# ## Relationship between diversity and richness----
# ggplot(quadrat_data, aes(x = Plant_richness, y = Plant_diversity)) +
#   geom_point() +
#   geom_smooth(method = "lm", se = T) +
#   theme(axis.text.x = element_text(angle = 90)) +
#   ggtitle("Richness-diversity")
# ggsave(
#   "Richness_diversity.pdf",
#   width = 20,
#   height = 15,
#   units = "cm"
# )
# 
# ggplot(quadrat_data, aes(x = Plant_richness, y = Plant_diversity)) +
#   geom_point() +
#   geom_smooth(method = "lm", se = T) +
#   theme(axis.text.x = element_text(angle = 90)) +
#   facet_grid( ~ Locality) +
#   ggtitle("Richness-diversity")
# ggsave(
#   "Richness_diversity_by_loc.pdf",
#   width = 20,
#   height = 15,
#   units = "cm"
# )
# 
# ## Distributions of abundances----
# ggplot(plant_data, aes(x = Cover_classes)) +
#   geom_histogram() +
#   ggtitle("Distribution of cover classes over all grids of ANG, BDX and CAM")
# ggsave(
#   "Distrib_cover_classes.pdf",
#   width = 20,
#   height = 15,
#   units = "cm"
# )
# 
# ggplot(plant_data, aes(x = Cover_classes, fill = Locality)) +
#   geom_histogram(position = "dodge") +
#   ggtitle("Distribution of cover classes over all grids of ANG, BDX and CAM")
# ggsave(
#   "Distrib_cover_by_loc.pdf",
#   width = 20,
#   height = 15,
#   units = "cm"
# )
# 
# ggplot(plant_data, aes(x = Relative_abundance)) +
#   geom_histogram() +
#   ggtitle("Distribution of relative abundances over all grids of ANG, BDX and CAM")
# ggsave(
#   "Distrib_rel_ab.pdf",
#   width = 20,
#   height = 15,
#   units = "cm"
# )
# 
# ggplot(plant_data, aes(x = log10(Relative_abundance))) +
#   geom_histogram() +
#   ggtitle("Distribution of log(relative abundances) over all grids of ANG, BDX and CAM")
# ggsave(
#   "Distrib_rel_abLOG.pdf",
#   width = 20,
#   height = 15,
#   units = "cm"
# )
# 
# ggplot(plant_data, aes(x = Relative_abundance, fill = Locality)) +
#   geom_histogram() +
#   ggtitle("Distribution of relative abundances over all grids of ANG, BDX and CAM")
# ggsave(
#   "Distrib_rel_ab_by_loc.pdf",
#   width = 20,
#   height = 15,
#   units = "cm"
# )
# 
# ## Abundance profiles----
# ggplot(plant_data, aes(x = Rank, y = Relative_abundance)) +
#   geom_point() +
#   stat_summary(
#     fun.y = mean,
#     geom = "point",
#     shape = 20,
#     size = 3,
#     color = "orange",
#     fill = "orange"
#   ) +
#   theme(axis.text.x = element_text(angle = 90)) +
#   facet_grid( ~ Locality) +
#   ggtitle("Abundance profile")
# 
# ggplot(plant_data, aes(x = Rank, y = Relative_abundance)) +
#   geom_point() +
#   stat_summary(
#     fun = median,
#     geom = "line",
#     size = 1,
#     color = "orange"
#   ) +
#   theme(axis.text.x = element_text(angle = 90)) +
#   ggtitle("Abundance profiles")
# ggsave(
#   "Abundance_profile.pdf",
#   width = 20,
#   height = 15,
#   units = "cm"
# )
# 
# ggplot(plant_data,
#        aes(x = Rank, y = Relative_abundance, color = Plant_richness)) +
#   geom_point() +
#   theme(axis.text.x = element_text(angle = 90)) +
#   facet_wrap( ~ Plant_richness) +
#   ggtitle("Abundance profiles in each richness class")
# ggsave(
#   "Abundance_profile_by_richness.pdf",
#   width = 20,
#   height = 15,
#   units = "cm"
# )
# 
# ggplot(plant_data, aes(x = Rank, y = Relative_abundance, col = Locality)) +
#   geom_point() +
#   stat_summary(
#     fun = median,
#     geom = "line",
#     size = 1,
#     color = "black"
#   ) +
#   theme(axis.text.x = element_text(angle = 90)) +
#   facet_grid( ~ Locality) +
#   ggtitle("Abundance profiles")
# ggsave(
#   "Abundance_profile_by_loc.pdf",
#   width = 20,
#   height = 15,
#   units = "cm"
# )
# 
# ggplot(plant_data, aes(x = Rank, y = Cover_classes)) +
#   geom_point(position = position_jitter(width = 0.25)) +
#   theme(axis.text.x = element_text(angle = 90)) +
#   ggtitle("Abundance profile")
# 
# plant_data %>%
#   filter(Locality == "CAM") %>%
#   mutate(Rank_as_factor = as.factor(Rank)) %>%
#   ggplot(., aes(x = Rank_as_factor, y = Relative_abundance)) +
#   geom_boxplot() +
#   geom_point(position = position_jitter(width = 0.1)) +
#   stat_summary(
#     fun = median,
#     geom = "line",
#     size = 1,
#     color = "black"
#   ) +
#   theme(axis.text.x = element_text(angle = 90)) +
#   ggtitle("Abundance profiles")
# ggsave(
#   "Abundance_profile_CAM.pdf",
#   width = 20,
#   height = 15,
#   units = "cm"
# )
# 
# plant_data %>%
#   filter(Locality == "CAM") %>%
#   mutate(Rank_as_factor = as.factor(Rank)) %>% summary()
# 
# plant_data %>%
#   filter(Locality == "CAM") %>%
#   mutate(Rank_as_factor = as.factor(Rank)) %>%
#   ggplot(., aes(x = Rank_as_factor, y = log(
#     Relative_abundance / (1 - Relative_abundance)
#   ))) +
#   geom_boxplot() +
#   geom_point(position = position_jitter(width = 0.1)) +
#   stat_summary(
#     fun = median,
#     geom = "line",
#     size = 1,
#     color = "black"
#   ) +
#   theme(axis.text.x = element_text(angle = 90)) +
#   ggtitle("Abundance profiles")
# 
# plant_data %>%
#   filter(Locality == "CAM") %>%
#   mutate(Rank_as_factor = as.factor(Rank)) %>%
#   ggplot(., aes(y = Relative_abundance)) +
#   geom_histogram() +
#   theme(axis.text.x = element_text(angle = 90)) +
#   facet_wrap(~ Rank_as_factor)
# ggtitle("Abundance profiles")

# SSU rRNA metabarcoding (16S V3-V4)

## decontamination and subsampling

### variables and functions


```r
smallest_sample_size <- 56865 
```


As expected, there are now
 56,865
 reads in all samples.


***


```r
sessionInfo()
```

```
## R version 4.2.1 (2022-06-23 ucrt)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 10 x64 (build 19044)
## 
## Matrix products: default
## 
## locale:
## [1] LC_COLLATE=French_France.utf8  LC_CTYPE=French_France.utf8   
## [3] LC_MONETARY=French_France.utf8 LC_NUMERIC=C                  
## [5] LC_TIME=French_France.utf8    
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] janitor_2.1.0   forcats_0.5.1   stringr_1.4.0   dplyr_1.0.9    
##  [5] purrr_0.3.4     readr_2.1.2     tidyr_1.2.0     tibble_3.1.7   
##  [9] ggplot2_3.3.6   tidyverse_1.3.1 readxl_1.4.0   
## 
## loaded via a namespace (and not attached):
##  [1] tidyselect_1.1.2 xfun_0.30        bslib_0.3.1      haven_2.5.0     
##  [5] snakecase_0.11.0 colorspace_2.0-3 vctrs_0.4.1      generics_0.1.2  
##  [9] htmltools_0.5.2  yaml_2.3.5       utf8_1.2.2       rlang_1.0.2     
## [13] jquerylib_0.1.4  pillar_1.7.0     withr_2.5.0      glue_1.6.2      
## [17] DBI_1.1.2        dbplyr_2.1.1     modelr_0.1.8     lifecycle_1.0.1 
## [21] munsell_0.5.0    gtable_0.3.0     cellranger_1.1.0 rvest_1.0.2     
## [25] evaluate_0.15    knitr_1.39       tzdb_0.3.0       fastmap_1.1.0   
## [29] fansi_1.0.3      broom_0.8.0      backports_1.4.1  scales_1.2.0    
## [33] jsonlite_1.8.0   fs_1.5.2         hms_1.1.1        digest_0.6.29   
## [37] stringi_1.7.6    grid_4.2.1       cli_3.3.0        tools_4.2.1     
## [41] magrittr_2.0.3   sass_0.4.1       crayon_1.5.1     pkgconfig_2.0.3 
## [45] ellipsis_0.3.2   xml2_1.3.3       reprex_2.0.1     lubridate_1.8.0 
## [49] assertthat_0.2.1 rmarkdown_2.14   httr_1.4.3       rstudioapi_0.13 
## [53] R6_2.5.1         compiler_4.2.1
```

```r
rm(list = ls())
```
