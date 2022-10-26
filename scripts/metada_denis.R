

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
         "Collection date" = Date)