

#install.packages("janitor")

#Load packages----

library(readxl)
library(tidyverse)
library(janitor)

#Assemble "Sample code" and "Sequencing plate" from sheet "Plan de Plaques" in file "Récap projet EDGG_A.xlsx"----

readxl::read_excel("Data/Récap projet EDGG_A.xlsx",
                   sheet = "PLAN DE PLAQUES",
                   col_names = FALSE) %>%
  select(1:10) %>%
  fill(1, .direction = c("down")) %>%
  row_to_names(row_number = 1) %>%
  rename(plate_row = 2, plate = 1) %>%
  pivot_longer(
    cols = -c(plate, plate_row),
    names_to = "plate_column",
    values_to = "Sample_code"
  ) %>%
  filter(plate_row != 0) %>%
  mutate(
    plate = str_remove(plate, "PLAQUE "),
    plate = str_replace(plate, "_", "-"),
    plate_row = as.character(plate_row),
    plate_row = str_pad(
      plate_row,
      width = 2,
      side = c("left"),
      pad = "0"
    )
  ) %>%
  unite(plate, plate, plate_column, sep = "_") %>%
  unite(plate, plate, plate_row, sep = "") %>%
  rename(`Sequencing plate` = plate) -> metadata_for_Denis

metadata_for_Denis %>% head()

#Add "Host community" from sheet "EDGG1" in file "Echantillonnage EDGG 21_CAM.xls"----

readxl::read_excel(
  "Data/Echantillonnage EDGG 21_CAM.xlsx",
  sheet = "EDGG1",
  col_names = FALSE,
  n_max = 1
) %>%
  pull() %>%
  str_remove(".* ") -> sample_code_prefix

readxl::read_excel(
  "Data/Echantillonnage EDGG 21_CAM.xlsx",
  sheet = "EDGG1",
  col_names = TRUE,
  skip = 8
) %>%
  rename(plants = 1) %>%
  pivot_longer(
    cols = -plants,
    names_to = "Sample_code",
    values_to = "cover",
    values_drop_na = TRUE
  ) %>%
  filter(!plants %in% c("Végétation", "Sol nu", "Litière")) %>%
  mutate(
    cover = str_remove(cover, "%"),
    cover_classes = case_when(
      cover == "1-5"   ~ "3",
      cover == "<1"    ~ "0,5",
      cover == "5-15"  ~ "10",
      cover == "15-25" ~ "20",
      cover == "25-50" ~ "37,5",
      cover == "50-75" ~ "62,5",
      cover == ">75"   ~ "87,5",
      TRUE             ~ "oups"
    )
  ) %>%
  select(-cover) %>%
  unite(host_community, plants, cover_classes, sep = "_") %>%
  group_by(Sample_code) %>%
  summarise(host_community = paste0(host_community, collapse = ";")) %>%
  ungroup() %>%
  mutate(
    Sample_code = str_remove(Sample_code, ".* "),
    Sample_code = str_pad(
      Sample_code,
      width = 2,
      side = c("left"),
      pad = "0"
    ),
    Sample_code = paste0(sample_code_prefix, Sample_code),
    Sample_code = paste0(Sample_code, "00")
  ) -> other_metadata

# charger Grid_table
readxl::read_xlsx("Data/Grid_table.xlsx")
  
  
  
  
# #Add date----
# readxl::read_xlsx(
#   "Data/Echantillonnage EDGG 21_CAM.xlsx",
#   sheet = "EDGG1",
#   col_names = FALSE,
#   skip = 1,
#   n_max = 1
# ) %>%
#   pull(`...1`) %>%
#   str_remove(".*: ") %>%
#   as.Date(format = "%d/%m/%Y") -> date
# 
# other_metadata %>%
#   mutate(`Collection date` = date) -> other_metadata
# 
# #Add GPS----
# readxl::read_xlsx(
#   "Data/Echantillonnage EDGG 21_CAM.xlsx",
#   sheet = "EDGG1",
#   col_names = FALSE,
#   skip = 3,
#   n_max = 1
# ) %>%
#   pull(`...1`) %>%
#   str_remove(".*sur ") -> which

#Assemble everything----

left_join(x = metadata_for_Denis,
          y = other_metadata,
          by = "Sample_code") %>%  
  mutate(Grid_name = str_trunc(Sample_code,width = 9,ellipsis = "")) %>% 
  left_join(y = readxl::read_xlsx("Data/Grid_table.xlsx"),
            by = "Grid_name") %>% 
  unite("GPS localisation", GPS_lat, GPS_long, sep = "/") %>% 
  add_column(Ecosystem = "") %>% 
  select(Sample_code, host_community, "GPS localisation", Locality, Country,Ecosystem, Date, "Sequencing plate") %>% 
  rename("Sample code" = Sample_code, 
         "Host community" = host_community,
         "Collection date" = Date) %>% 
  view()
  
  #Distribution of relative abundances----

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
grid_name = paste(grid_name_base, grid_number, sep = "_")

readxl::excel_sheets(fichier_test) %>%
  subset(., str_starts(string = ., pattern = "EDGG")) -> sheets

read_a_given_sheet <- function(filename, sheet_name) {
  filename %>%
    str_remove(., ".* ") %>%
    str_remove(., ".xlsx") -> grid_name_base
  
  sheet_name %>%
    str_remove(., "EDGG") %>%
    str_pad(.,
            width = 2,
            side = c("left"),
            pad = "0") -> grid_number
  
  readxl::read_excel(filename,
                     sheet = sheet_name,
                     col_names = TRUE,
                     skip = 8) %>%
    rename(Plant = 1) %>%
    pivot_longer(
      cols = -Plant,
      names_to = "Sample code",
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
        Cover == "1-5"   ~ "3",
        Cover == "<1"    ~ "0.5",
        Cover == "5-15"  ~ "10",
        Cover == "15-25" ~ "20",
        Cover == "25-50" ~ "37.5",
        Cover == "50-75" ~ "62.5",
        Cover == ">75"   ~ "87.5",
        TRUE             ~ "oups"
      ),
      Cover_classes = as.numeric(Cover_classes)
    ) %>%
    select(-Cover) %>%
    group_by(`Sample code`) %>%
    mutate(Total_cover = sum(Cover_classes)) %>%
    ungroup() %>%
    mutate(Relative_abundance = Cover_classes / Total_cover)
}

#Tester sur une feuille d'un fichier----
read_a_given_sheet("Data/Echantillonnage EDGG 21_CAM.xlsx", "EDGG3")  -> df_rel_ab

read_a_given_sheet("Data/Echantillonnage EDGG 20_BDX.xlsx", "EDGG3")  -> df_rel_ab

#Tester sur toutes les feuilles d'un fichier----
readxl::excel_sheets("Data/Echantillonnage EDGG 20_BDX.xlsx") %>%
  subset(., str_starts(string = ., pattern = "EDGG")) -> sheets

lapply(sheets, function(sn)
  read_a_given_sheet("Data/Echantillonnage EDGG 20_BDX.xlsx", sn)) %>%
  do.call(rbind, .) %>% View()

#Tester sur une liste de fichiers----
list.files("./", full.names = T, recursive = T) %>%
  subset(., str_detect(., "xls")) %>%
  subset(., str_detect(., "Echant")) %>%
  subset(., str_detect(., "REU", negate = TRUE)) -> liste_fichiers

lapply(liste_fichiers, function(nom_fichier) {
  #quelles feuilles dans ce fichier nom_fichier
  readxl::excel_sheets(nom_fichier) %>%
    subset(., str_starts(string = ., pattern = "EDGG")) -> sheets
  
  lapply(sheets, function(sn)
    read_a_given_sheet(nom_fichier, sn)) %>%
    do.call(rbind, .)
}) %>%
  do.call(rbind, .) -> ouf

ouf %>%
  summary()

ggplot(ouf, aes(x = Relative_abundance)) +
  geom_histogram()

ggplot(ouf, aes(x = Relative_abundance, fill = Locality)) +
  geom_histogram(position = "dodge")
