## Exploring the Plant Dataset

## ------------------------------------------------------------------- preamble

### ------------------------------------------------ clean-up and load packages

rm(list = ls())
library(readxl)
library(tidyverse)


### -------------------------------------------- global variables and functions

list_file_names_and_sheet_names <- function() {
    list.files(path = 'data',
               recursive = TRUE,
               full.names = TRUE,
               pattern = '^Echant.*(ANG|BDX|CAM)\\.xlsx$') %>%
        purrr::set_names() %>%
        purrr::map(., readxl::excel_sheets) %>%
        enframe(name = "files", value = "sheets") %>%
        unnest_longer(sheets) %>%
        filter(str_starts(sheets, "EDGG")) %>%
        purrr::map(., as.list)
}


read_and_filter_plant_coverage_values <- function(filename, sheet_name) {
    soil_aspect <- c("Végétation", "Sol nu", "Litière")

    filename %>%
        readxl::read_excel(sheet = sheet_name,
                           col_names = TRUE,
                           skip = 8) %>%
        rename(Plant = 1) %>%
        pivot_longer(
            cols = -Plant,
            names_to = "Quadrat",
            values_to = "Cover",
            values_drop_na = TRUE) %>%
        filter(! Plant %in% soil_aspect)
}


get_grid_name <- function(filename) {
    ## year (last 2 digits), dash or underscore, locality (3 capital letters)
    filename %>%
        str_remove(., ".* ") %>%
        str_remove(., ".xlsx$")
}


get_grid_number <- function(sheet_name) {
    sheet_name %>%
        str_remove(., "EDGG") %>%
        str_pad(.,
                width = 2,
                side = c("left"),
                pad = "0")
}


get_vegetation_index_per_quadrat <- function(filename, sheet_name) {
    filename %>%
        readxl::read_excel(sheet = sheet_name,
                           col_names = TRUE,
                           skip = 8) %>%
        rename(Coverage = 1) %>%
        filter(Coverage == "Végétation") %>%
        select(-Coverage) %>%
        pivot_longer(
            cols = everything(),
            names_to = "Quadrat",
            values_to = "Vegetation",
            values_drop_na = TRUE)
}

mutate_and_recode_coverage <- function(input_data, filename, sheet_name) {
    get_grid_name(filename) -> grid_name_base
    get_grid_number(sheet_name) -> grid_number
    get_vegetation_index_per_quadrat(filename, sheet_name) -> vegetation_by_quadrat
    
    input_data %>%
        mutate(
            Year = str_sub(grid_name_base, 0, 2),
            Locality = str_sub(grid_name_base, 4, 6),
            Grid = paste(grid_name_base, grid_number, sep = "_"),
            Cover = str_remove_all(Cover, "%"),
            Cover_classes = case_when(
                Cover == "<1"    ~ 0.5,
                Cover == "1-5"   ~ 3,
                Cover == "5-15"  ~ 10,
                Cover == "15-25" ~ 20,
                Cover == "25-50" ~ 37.5,
                Cover == "50-75" ~ 62.5,
                Cover == ">75"   ~ 87.5,
                TRUE             ~ NaN),
            Cover_max = case_when(
                Cover == "<1"    ~ 1,
                Cover == "1-5"   ~ 5,
                Cover == "5-15"  ~ 15,
                Cover == "15-25" ~ 25,
                Cover == "25-50" ~ 50,
                Cover == "50-75" ~ 75,
                Cover == ">75"   ~ 100,
                TRUE             ~ NaN)) %>%
        left_join(., vegetation_by_quadrat, by = "Quadrat") %>%
        select(-Cover)
}


. %>%
    mutate(
        Vegetation = str_remove_all(Vegetation, "%"),
        Vegetation = str_trim(Vegetation, side = "both"),
        Vegetation_classes = case_when(
            Vegetation == "<1"    ~ "0.5",
            Vegetation == "1-5"   ~ "3",
            Vegetation == "5-15"  ~ "10",
            Vegetation == "15-25" ~ "20",
            Vegetation == "25-50" ~ "37.5",
            Vegetation == "50-75" ~ "62.5",
            Vegetation == ">75"   ~ "87.5",
            TRUE                  ~ Vegetation)) %>%
    mutate(Vegetation = as.double(Vegetation_classes)) %>%
    select(-Vegetation_classes) -> recode_vegetation


. %>%
    group_by(Quadrat) %>%
    mutate(
        Total_cover = sum(Cover_classes),
        Relative_abundance = Cover_classes / sum(Cover_classes),
        Total_cover_max = sum(Cover_max),
        Plant_richness = length(Plant),
        Plant_diversity = exp(-sum(Relative_abundance * log(Relative_abundance))),
        Rank = seq(1, length(Plant))) %>%
    ungroup() %>%
    group_by(Plant) %>%
    mutate(Plant_genus = str_remove(Plant, " .*")) %>%
    ungroup() -> compute_basic_diversity_stats


read_a_given_sheet <- function(filename, sheet_name) {
    read_and_filter_plant_coverage_values(filename, sheet_name) %>%
        mutate_and_recode_coverage(., filename, sheet_name) %>%
        recode_vegetation %>%
        compute_basic_diversity_stats
}


## --------------------------------------------- read per-plant coverage values

list_file_names_and_sheet_names() %>%
    purrr::pmap_dfr(., ~ read_a_given_sheet(.x, .y)) -> plant_data
#write.csv2(plant_data, "./interm/plant_data.csv")


## Note: 'compute_basic_diversity_stats' should be use *outside* of
## the main reading function


## -------------------------------------------------- focus on quadrat features

## Note: plots should all be saved to a results folder, not the root of
## the project

## create the results folder and sub-folder on the fly via R code.


#sessionInfo()
#rm(list = ls())
#quit(save = "no")
