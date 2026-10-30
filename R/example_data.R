# create_sample_sheets.R
# BUILD NINE STUDY SHEETS, THREE PER SPECIES, EACH WITH ITS OWN ENTERER AND SITES

# requirements
# install.packages(c('dplyr', 'here'))
library(dplyr)

species_list <- c("setosa", "versicolor", "virginica")
meas <- c("Sepal.L", "Sepal.W", "Petal.L", "Petal.W")

# BUILD NINE STUDY SHEETS, THREE PER SPECIES, EACH WITH ITS OWN ENTERER AND SITES
synthetic_datasheets <- function() {
  sites <- tibble(
    site = c("Riviere-Madeleine", "Mont-Albert", "Cap-Chat", "Murdochville", "Perce", "Matane"),
    lat  = c(49.05, 48.95, 49.10, 48.70, 48.60, 48.85),
    lon  = c(-65.90, -65.20, -66.40, -65.60, -64.80, -64.60)
  )
  
  set.seed(1936)
  sheets <- iris %>%
    mutate(species = as.character(Species)) %>%
    group_by(species) %>%
    mutate(part = c("A", "B", "C")[ceiling(row_number() / 17)]) %>%
    ungroup() %>%
    mutate(sheet = paste0(species, "_", part),
           whos  = recode(sheet, setosa_A = "EA", setosa_B = "MB", setosa_C = "JC",
                          versicolor_A = "RT", versicolor_B = "SK", versicolor_C = "NP",
                          virginica_A = "DL", virginica_B = "HW", virginica_C = "GF")) %>%
    group_by(sheet) %>%
    mutate(row = row_number(),
           site = sample(sites$site, n(), replace = TRUE)) %>%
    ungroup() %>%
    left_join(sites, by = "site") %>%
    transmute(sheet, row, whos, species, site, lat, lon,
              Sepal.L = Sepal.Length, Sepal.W = Sepal.Width,
              Petal.L = Petal.Length, Petal.W = Petal.Width)
  
  # SEED ERRORS
  seed <- function(d, s, r, col, value) {
    d[[col]][d$sheet == s & d$row %in% r] <- value
    d
  }
  sheets <- sheets %>%
    seed("setosa_A", 14, "Petal.L", 14.0) %>%          # DECIMAL SLIP (1.4 TYPED AS 14)
    seed("versicolor_B", 7, "Sepal.W", -2.9) %>%       # NEGATIVE VALUE
    seed("setosa_B", 10:14, "Petal.W", NA) %>%         # BLANK CELLS IN ONE COLUMN
    seed("versicolor_A", 9, "species", "Versicolor ") # SPECIES NOT ON THE LIST
  
  vb <- sheets$sheet == "virginica_C"                 # WHOLE SHEET ENTERED IN MM
  sheets[vb, c("Sepal.L", "Sepal.W", "Petal.L", "Petal.W")] <-
    sheets[vb, c("Sepal.L", "Sepal.W", "Petal.L", "Petal.W")] * 10
  
  sw <- sheets$sheet == "versicolor_A" & sheets$row %in% 3:5   # PETAL LENGTH AND WIDTH SWAPPED
  sheets[sw, c("Petal.L", "Petal.W")] <- sheets[sw, c("Petal.W", "Petal.L")]
  
  sheets <- sheets %>%
    seed("virginica_A", 10, "species", "setosa") %>%   # WRONG SPECIES ON THE LIST
    seed("versicolor_B", 12, "lon", 64.60) %>%         # DROPPED MINUS ON LONGITUDE
    seed("virginica_A", 15, "lat", 48.70) %>%          # POINT IN THE GULF OF ST LAWRENCE
    seed("virginica_A", 15, "lon", -63.60)
  sheets[sheets$sheet == "setosa_A" & sheets$row == 16, c("lat", "lon")] <- list(-64.60, 48.85)  # LAT AND LON SWAPPED
  
  dups <- sheets %>% filter(sheet == "setosa_A", row %in% 1:5) %>%   # ROWS PASTED INTO A SECOND SHEET
    mutate(sheet = "setosa_C", row = 17:21, whos = "JC")
  sheets <- bind_rows(sheets, dups) %>% arrange(sheet, row)
  
  sheets <- sheets %>% mutate(id = paste0(sheet, " r", row, " (", whos, ")"))
  return(sheets)
}

#' standardize where example data goes
example_data_folder<- function(){
  return(here::here(file.path("data", "example_data")))
}

#' save a CSV for each unique name in the 'sheet' column
save_sheet_csvs<- function(sheets){ 
  sapply(
    unique(sheets$sheet),
    function(sheet_name){ 
      csv_file <- file.path(example_data_folder(), paste0(sheet_name, ".csv"))
      dplyr::filter(sheets, sheet == sheet_name) %>% 
      write.csv(csv_file,row.names = FALSE)
    }
  )
}

#' read csvs in sample data and combine for , 
#' generic function not specific to this project
read_example_csvs <-function(path= example_data_folder()){
  csvs = list.files(path=path, pattern="\\.csv$", full.names = TRUE)
  sheets = lapply(csvs, read.csv) %>% dplyr::bind_rows()
  return(sheets)
}


#' create data sheets in google drive from our synthetic data
save_example_google_sheets <- function(sheets, shared_drive_name ="IBEEM_Data", folder_name="collaborator_pkg" ){
  library(googlesheets4)
  library(googledrive)
  my_shared_drive <- shared_drive_get(shared_drive_name)
  target_folder <- drive_find(
    pattern = folder_name, type = "folder", shared_drive = my_shared_drive
  )
  sapply(
    unique(sheets$sheet),
    function(sheet_name){ 
      gsheet <- gs4_create(sheet_name)
      drive_mv(gsheet, target_folder)
    }
  )
}
