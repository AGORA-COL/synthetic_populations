setwd('/mnt/disco_aux/trace/apps/synthetic_populations/scripts')
##===============================================================#
## Generate synthetic workplaces
## Author: Diego Veloza
## Date: 2023/09/12
##===============================================================#
library(dplyr)
library(tidyverse)
library(stringr)

##===============================================================#
## Read Input-------------
##===============================================================#
workplace_file  = "../data/raw_data_/workplacedata/CENSUS_WORKPLACESv2.csv"
df <- read_csv(workplace_file)

##===============================================================#
## Function to clean col names
##===============================================================#
clean_column_names <- function(df) {
  cleaned_names <- gsub(" ", "", names(df))
  cleaned_names <- gsub("[^a-zA-Z0-9_-]", "", cleaned_names)
  cleaned_names <- toupper(cleaned_names)
  names(df) <- cleaned_names
  return(df)
}

##===============================================================#
## Function to distribute workers randomly but within specified bounds
##===============================================================#
distribute_workers <- function(units, workers, min_size, max_size) {
  ##browser()
  if (is.na(units) | is.na(workers) | is.na(min_size) | is.na(max_size) | units == 0) {
    return(NA)
  }
  #print(paste0(units, " ", workers, " ", min_size, " ", max_size))

  # Initialize with minimum workers
  distribution <- rep(min_size, units)
  remaining_workers <- workers - (min_size * units)

  for (i in 1:remaining_workers) {
    valid_indices <- which(distribution < max_size)
    random_index <- sample(valid_indices, 1)
    distribution[random_index] <- distribution[random_index] + 1
  }

  return(distribution)
}

generate_ids <- function(codigo, units) {
  if (is.na(units)){return(NA)}
  ids <- seq_len(units)
  return(sprintf("%s%06d", codigo, ids))
}

##===============================================================#
## Processing function to apply the above operations for any given column pair
##===============================================================#
process_data <- function(df, unit_col, worker_col, min=NULL, estim_min_max=FALSE) {
  print(paste0("Processing : ", unit_col))
  ##browser()
  # Extract min and max from column name
  min_max <- str_extract(unit_col, "\\d+-\\d+") %>% 
              str_split("-") %>% 
              unlist() %>% 
              as.integer()

  if (estim_min_max){
    max = df[[worker_col]] - min*(df[[unit_col]] - 1)
  } else {
    min =  min_max[1]
    max =  min_max[2]
  }

  # Distribute workers
  distributions <- mapply(distribute_workers, df[[unit_col]], df[[worker_col]], min, max, SIMPLIFY = FALSE)

  # Generate IDs
  ids <- mapply(generate_ids, df$CODIGO, df[[unit_col]], SIMPLIFY = FALSE)

  # Convert lists to data.frames and return
  combined <- data.frame(
    ID = unlist(ids),
    CODIGO = rep(df$CODIGO, times = sapply(ids, length)),
    NUM_WORKERS = unlist(distributions)
  )
  return(combined)
}

##===============================================================#
## Applying the processing function
##===============================================================#
df <- clean_column_names(df) %>% rename("UNIDADESECONOMICAS0-1"         = "UNIDADESECONOMICAS1PER", 
                                        "PERSONASQUETRABAJARONPROM0-1"  = "PERSONASQUETRABAJARONPROM1PER",
                                        "UNIDADESECONOMICAS9-10"        = "UNIDADESECONOMICAS10",
                                        "PERSONASQUETRABAJARONPROM9-10" = "PERSONASQUETRABAJARONPROM10")

cols <- names(df)[10:25]#[10:25]
unit_cols   <- cols[grepl("UNIDADESECONOMICAS", cols)]
worker_cols <- cols[grepl("PERSONASQUETRABAJARONPROM", cols)]

workplaces <- lapply(seq_along(unit_cols), function(i) {
  process_data(df, unit_cols[i], worker_cols[i])
})

## Get workplaces bigger that 500
workplaces_up_500 <- process_data(df, "UNIDADESECONOMICAS500", "PERSONASQUETRABAJARONPROM500", min=501, estim_min_max=TRUE)

##===============================================================#
## Binding together and save
##===============================================================#
final_df <- na.omit(bind_rows(workplaces, workplaces_up_500)) %>% rename("workplace_id" = "ID",
                                                                         "CODE" = "CODIGO",
                                                                         "TotalWorkers" = "NUM_WORKERS")

write_csv(final_df, '../data/processed_data/workplacedata/workplace_colombia_data.csv')




# write_csv(mov_df, '../data/processed_data/workplacedata/mobility_matrix_bogota_data.csv')