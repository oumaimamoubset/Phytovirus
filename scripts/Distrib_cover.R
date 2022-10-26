# EXPLORING THE PLANT DATASET----

# install.packages("janitor")

# Load packages----
library(readxl)
library(tidyverse)
library(janitor)
library(here)

# Testing bits of the script----
fichier_test <- "Data/Echantillonnage EDGG 21_CAM.xlsx"
feuille_test <- "EDGG1"


fichier_test %>%
  str_remove(., ".* ") %>%
  str_remove(., ".xlsx") -> grid_name_base

feuille_test %>%
  str_remove(., "EDGG") %>%
  str_pad(.,
          width = 2,
          side = c("left"),
          pad = "0") -> grid_number

readxl::read_excel(fichier_test,
                   sheet = feuille_test,
                   col_names = TRUE,
                   skip = 8) %>%
  rename(Quadrat = 1) %>%
  filter(Quadrat == "Végétation") %>%
  pivot_longer(
    cols = everything(),
    names_to = "Quadrat",
    values_to = "Vegetation",
    values_drop_na = TRUE
  )

grid_name = paste(grid_name_base, grid_number, sep = "_")

readxl::excel_sheets(fichier_test) %>%
  subset(., str_starts(string = ., pattern = "EDGG")) -> sheets

# THE function to read a grid (sheet) in a place and year (file)----
read_a_given_sheet <- function(filename, sheet_name) {
  # Code à 6 chiffres année_localité
  filename %>%
    str_remove(., ".* ") %>%
    str_remove(., ".xlsx") -> grid_name_base
  
  # Numéro de la grille
  sheet_name %>%
    str_remove(., "EDGG") %>%
    str_pad(.,
            width = 2,
            side = c("left"),
            pad = "0") -> grid_number
  
  # Indice de végétation pour chaque quadrat
  readxl::read_excel(filename,
                     sheet = sheet_name,
                     col_names = TRUE,
                     skip = 8) %>%
    rename(Quadrat = 1) %>%
    filter(Quadrat == "Végétation") %>%
    pivot_longer(
      cols = everything(),
      names_to = "Quadrat",
      values_to = "Vegetation",
      values_drop_na = TRUE
    ) -> Vegetation_by_quadrat
  
  # Indice de végétation pour chaque quadrat
  readxl::read_excel(filename,
                     sheet = sheet_name,
                     col_names = TRUE,
                     skip = 8) %>%
    rename(Plant = 1) %>%
    pivot_longer(
      cols = -Plant,
      names_to = "Quadrat",
      values_to = "Cover",
      values_drop_na = TRUE
    ) %>%
    filter(!Plant %in% c("Végétation", "Sol nu", "Litière")) %>%
    mutate(
      Year = str_sub(grid_name_base, 0, 2),
      Locality = str_sub(grid_name_base, 4, 6),
      Grid = paste(grid_name_base, grid_number, sep = "_"),
      Cover = str_remove_all(Cover, "%"),
      Cover_classes = case_when(
        Cover == "1-5"   ~ 3,
        Cover == "<1"    ~ 0.5,
        Cover == "5-15"  ~ 10,
        Cover == "15-25" ~ 20,
        Cover == "25-50" ~ 37.5,
        Cover == "50-75" ~ 62.5,
        Cover == ">75"   ~ 87.5,
        TRUE             ~ NaN
      ),
      Cover_max = case_when(
        Cover == "1-5"   ~ 5,
        Cover == "<1"    ~ 1,
        Cover == "5-15"  ~ 15,
        Cover == "15-25" ~ 25,
        Cover == "25-50" ~ 50,
        Cover == "50-75" ~ 75,
        Cover == ">75"   ~ 100,
        TRUE             ~ NaN
      )
    ) %>%
    left_join(., Vegetation_by_quadrat, by = "Quadrat") %>%
    select(-Cover) %>%
    group_by(Quadrat) %>%
    mutate(
      Total_cover = sum(Cover_classes),
      Relative_abundance = Cover_classes / sum(Cover_classes),
      Total_cover_max = sum(Cover_max),
      Plant_richness = length(Plant),
      Plant_diversity = exp(-sum(
        Relative_abundance * log(Relative_abundance)
      )),
      Rank = seq(1, length(Plant))
    ) %>%
    ungroup() %>%
    group_by(Plant) %>%
    mutate(Plant_genus = str_split(Plant, " ")[[1]][1]) %>%
    ungroup()
}

# Read one grid----
read_a_given_sheet("Data/Echantillonnage EDGG 21_CAM.xlsx", "EDGG3")  -> df_rel_ab

read_a_given_sheet("Data/Echantillonnage EDGG 20_BDX.xlsx", "EDGG3")  -> df_rel_ab

# Read all grids of a given place and year----
# List grids
readxl::excel_sheets("Data/Echantillonnage EDGG 20_BDX.xlsx") %>%
  subset(., str_starts(string = ., pattern = "EDGG")) -> sheets

# Apply the function to each grid in the list
lapply(sheets, function(sn)
  read_a_given_sheet("Data/Echantillonnage EDGG 20_BDX.xlsx", sn)) %>%
  do.call(rbind, .) -> df_rel_ab

# Read all grids of several places and years----
# Establish the list of files to be read
list.files("./", full.names = T, recursive = T) %>%
  subset(., str_detect(., "xls")) %>%
  subset(., str_detect(., "Echant")) %>%
  subset(., str_detect(., "REU", negate = TRUE)) -> liste_fichiers

# Apply the function to each file in the list and each grid in each file
lapply(liste_fichiers, function(nom_fichier) {
  # List grids in the considered file
  readxl::excel_sheets(nom_fichier) %>%
    subset(., str_starts(string = ., pattern = "EDGG")) -> sheets
  # Apply the function to all grides in the file
  lapply(sheets, function(sn)
    read_a_given_sheet(nom_fichier, sn)) %>%
    do.call(rbind, .)
}) %>%
  do.call(rbind, .) -> plant_data

plant_data %>% view()

# Simplified version of the data set to focus on features of quadrats (not plants within quadrats)
plant_data %>%
  select(
    Year,
    Locality,
    Grid,
    Quadrat,
    Vegetation,
    Total_cover,
    Total_cover_max,
    Plant_richness,
    Plant_diversity
  ) %>%
  distinct() -> quadrat_data

quadrat_data %>% view()

# Graphs----
## Distribution of total cover----
ggplot(quadrat_data, aes(fill = Locality, y = Total_cover, x = Grid)) +
  geom_boxplot(position = "dodge") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Distribution of total cover over all grids of ANG, BDX and CAM")
ggsave(
  "Distrib_total_cover_by_grid.pdf",
  width = 20,
  height = 15,
  units = "cm"
)

ggplot(quadrat_data, aes(x = Total_cover, fill = Locality)) +
  geom_histogram(position = "dodge") +
  ggtitle("Distribution of total cover over all grids of ANG, BDX and CAM")
ggsave(
  "Distrib_total_cover_by_loc.pdf",
  width = 20,
  height = 15,
  units = "cm"
)

## Correspondence between Vegetation and Total_cover----
quadrat_data %>%
  filter(Locality == "CAM") %>%
  ggplot(., aes(x = Vegetation, y = Total_cover)) +
  geom_point(position = position_jitter(0.1)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Vegetation vs total cover")
ggsave(
  "Vegetation_vs_total_cover.pdf",
  width = 20,
  height = 15,
  units = "cm"
)

## Relationship between diversity and richness----
ggplot(quadrat_data, aes(x = Plant_richness, y = Plant_diversity)) +
  geom_point() +
  geom_smooth(method = "lm", se = T) +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Richness-diversity")
ggsave(
  "Richness_diversity.pdf",
  width = 20,
  height = 15,
  units = "cm"
)

ggplot(quadrat_data, aes(x = Plant_richness, y = Plant_diversity)) +
  geom_point() +
  geom_smooth(method = "lm", se = T) +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_grid( ~ Locality) +
  ggtitle("Richness-diversity")
ggsave(
  "Richness_diversity_by_loc.pdf",
  width = 20,
  height = 15,
  units = "cm"
)

## Distributions of abundances----
ggplot(plant_data, aes(x = Cover_classes)) +
  geom_histogram() +
  ggtitle("Distribution of cover classes over all grids of ANG, BDX and CAM")
ggsave(
  "Distrib_cover_classes.pdf",
  width = 20,
  height = 15,
  units = "cm"
)

ggplot(plant_data, aes(x = Cover_classes, fill = Locality)) +
  geom_histogram(position = "dodge") +
  ggtitle("Distribution of cover classes over all grids of ANG, BDX and CAM")
ggsave(
  "Distrib_cover_by_loc.pdf",
  width = 20,
  height = 15,
  units = "cm"
)

ggplot(plant_data, aes(x = Relative_abundance)) +
  geom_histogram() +
  ggtitle("Distribution of relative abundances over all grids of ANG, BDX and CAM")
ggsave(
  "Distrib_rel_ab.pdf",
  width = 20,
  height = 15,
  units = "cm"
)

ggplot(plant_data, aes(x = log10(Relative_abundance))) +
  geom_histogram() +
  ggtitle("Distribution of log(relative abundances) over all grids of ANG, BDX and CAM")
ggsave(
  "Distrib_rel_abLOG.pdf",
  width = 20,
  height = 15,
  units = "cm"
)

ggplot(plant_data, aes(x = Relative_abundance, fill = Locality)) +
  geom_histogram() +
  ggtitle("Distribution of relative abundances over all grids of ANG, BDX and CAM")
ggsave(
  "Distrib_rel_ab_by_loc.pdf",
  width = 20,
  height = 15,
  units = "cm"
)

## Abundance profiles----
ggplot(plant_data, aes(x = Rank, y = Relative_abundance)) +
  geom_point() +
  stat_summary(
    fun.y = mean,
    geom = "point",
    shape = 20,
    size = 3,
    color = "orange",
    fill = "orange"
  ) +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_grid( ~ Locality) +
  ggtitle("Abundance profile")

ggplot(plant_data, aes(x = Rank, y = Relative_abundance)) +
  geom_point() +
  stat_summary(
    fun = median,
    geom = "line",
    size = 1,
    color = "orange"
  ) +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Abundance profiles")
ggsave(
  "Abundance_profile.pdf",
  width = 20,
  height = 15,
  units = "cm"
)

ggplot(plant_data,
       aes(x = Rank, y = Relative_abundance, color = Plant_richness)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap( ~ Plant_richness) +
  ggtitle("Abundance profiles in each richness class")
ggsave(
  "Abundance_profile_by_richness.pdf",
  width = 20,
  height = 15,
  units = "cm"
)

ggplot(plant_data, aes(x = Rank, y = Relative_abundance, col = Locality)) +
  geom_point() +
  stat_summary(
    fun = median,
    geom = "line",
    size = 1,
    color = "black"
  ) +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_grid( ~ Locality) +
  ggtitle("Abundance profiles")
ggsave(
  "Abundance_profile_by_loc.pdf",
  width = 20,
  height = 15,
  units = "cm"
)

ggplot(plant_data, aes(x = Rank, y = Cover_classes)) +
  geom_point(position = position_jitter(width = 0.25)) +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Abundance profile")

plant_data %>%
  filter(Locality == "CAM") %>%
  mutate(Rank_as_factor = as.factor(Rank)) %>%
  ggplot(., aes(x = Rank_as_factor, y = Relative_abundance)) +
  geom_boxplot() +
  geom_point(position = position_jitter(width = 0.1)) +
  stat_summary(
    fun = median,
    geom = "line",
    size = 1,
    color = "black"
  ) +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Abundance profiles")
ggsave(
  "Abundance_profile_CAM.pdf",
  width = 20,
  height = 15,
  units = "cm"
)

plant_data %>%
  filter(Locality == "CAM") %>%
  mutate(Rank_as_factor = as.factor(Rank)) %>% summary()

plant_data %>%
  filter(Locality == "CAM") %>%
  mutate(Rank_as_factor = as.factor(Rank)) %>%
  ggplot(., aes(x = Rank_as_factor, y = log(
    Relative_abundance / (1 - Relative_abundance)
  ))) +
  geom_boxplot() +
  geom_point(position = position_jitter(width = 0.1)) +
  stat_summary(
    fun = median,
    geom = "line",
    size = 1,
    color = "black"
  ) +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Abundance profiles")

plant_data %>%
  filter(Locality == "CAM") %>%
  mutate(Rank_as_factor = as.factor(Rank)) %>%
  ggplot(., aes(y = Relative_abundance)) +
  geom_histogram() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~ Rank_as_factor)
ggtitle("Abundance profiles")