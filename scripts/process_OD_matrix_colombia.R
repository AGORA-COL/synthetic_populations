setwd('/mnt/disco_aux/trace/apps/synthetic_populations/scripts')
Sys.setenv(PROJ_LIB = "/mnt/disco_aux/trace/apps/mambaforge/share/proj")
##===============================================================#
## Generate synthetic workplaces mobility
## Author: Diego Veloza
## Date: 2023/09/21
##===============================================================#
library(tidyverse)
library(dplyr)
library(stringr)
library(tidyr)
library(stringi)
library(sf)

##===============================================================#
## Read Input-------------
##===============================================================#
workplace_file  = "../data/raw_data_/workplacedata/CENSUS_WORKPLACESv2.csv"
ipm_file        = "../data/raw_data/schooldata/IPM_MZA_CNPV2018_NAL.csv"
travel_file     = "../data/raw_data/geodata/travel_times_colombia_municipios.csv"

census_workplace    <- read_csv(workplace_file)
ipm_data            <- read_csv(ipm_file)
travel_distance_time <- read_csv(travel_file)

# Calculate the mean IPM for each MPIO
ipm_municpio <- ipm_data %>%
  group_by(MPIO) %>%
  summarise(mean_IPM = mean(IPM))


partial_time_worker_data = floor(census_workplace %>% rename( "COD_MUNCIP" = "codigo",
                                                        "NUM_WORKERS_INSIDE"  = "Personas en el mismo municipio",
                                                        "NUM_WORKERS_OUTSIDE" = "Personas en otro municipio",             
                                                        "0_10_MIN"  = "Menos de 10 minutos",                    
                                                        "10_15_MIN" = "De 10 a 15 minutos",                      
                                                        "15_30_MIN" = "De 16 a 30 minutos",                      
                                                        "30_45_MIN" = "De 31 a 45 minutos",                      
                                                        "46_60_MIN" = "De 46 a 60 minutos",                      
                                                        "60_9999_MIN" = "Mas de 1 hora" ) %>%
                                    dplyr::select("COD_MUNCIP", "NUM_WORKERS_INSIDE","NUM_WORKERS_OUTSIDE", 
                                                "0_10_MIN", "10_15_MIN", "15_30_MIN",
                                                "30_45_MIN", "46_60_MIN", "60_9999_MIN"))


# Calculate proportions for each time frame
worker_data <- partial_time_worker_data  # Replace with your actual data
time_cols <- c("0_10_MIN","10_15_MIN","15_30_MIN","30_45_MIN","46_60_MIN","60_9999_MIN")
worker_data$Total_Workers <- rowSums(worker_data[, time_cols])
worker_data$Prop_Inside <- worker_data$NUM_WORKERS_INSIDE / worker_data$Total_Workers
worker_data$Prop_Outside <- worker_data$NUM_WORKERS_OUTSIDE / worker_data$Total_Workers

# Distribute workers
for(time_col in time_cols){
  worker_data[paste("Inside_", time_col, sep = "")] <- worker_data$Prop_Inside * worker_data[, time_col]
  worker_data[paste("Outside_", time_col, sep = "")] <- worker_data$Prop_Outside * worker_data[, time_col]
}

# Read time distance data
merged_data <- merge(travel_distance_time, worker_data, by.x = "Code_Origin", by.y = "COD_MUNCIP")

merged_data_with_ipm <- merge(merged_data, ipm_municpio, by.x = "Code_Origin", by.y = "MPIO")
merged_data_with_ipm$Total_Workers <- merged_data_with_ipm$NUM_WORKERS_INSIDE + merged_data_with_ipm$NUM_WORKERS_OUTSIDE


threshold = 30
merged_data_with_ipm$mobility_rate <- with(merged_data_with_ipm, 
                                        Total_Workers / ifelse(Time_minutes_driving == 0 | Time_minutes_walking == 0, 
                                                                    5, # Replace zero with 5 min to avoid division by zero
                                                                    ifelse(mean_IPM > threshold, 
                                                                            Time_minutes_driving, 
                                                                            Time_minutes_walking)))
                                                                       

# Aggregate the data
final_data <- merged_data_with_ipm %>%
  group_by(Code_Origin, Code_Destination) %>%
  summarise(Mobility_Rate = sum(mobility_rate))

# To keep it as a data frame
final_data <- as.data.frame(final_data)
final_data$Mobility_Rate[is.na(final_data$Mobility_Rate) | is.infinite(final_data$Mobility_Rate)] <- 0

workers_mobility_file = '../data/processed_data/workplacedata/mobility_matrix_colombia_data.csv'
write_csv(final_data, workers_mobility_file)


enrolled_students_file  = '../data/raw_data/schooldata/Alumnos matriculados en educación tradicional y CLEI según rangos de edad por jornada.csv'
loc_schools_file        = '../data/raw_data/schooldata/Localizacion_Colegios.csv'
prop_students_per_municpio_file = '../data/raw_data/schooldata/ESTADISTICAS_EN_EDUCACION_BASICA_POR_MUNICIPIO.csv'

enrolled_students <- read.csv(enrolled_students_file, sep=';')
loc_schools <- read_csv(loc_schools_file)
prop_students_per_municpio <- read_csv(prop_students_per_municpio_file)

enrolled_students$NUM_STUDENTS <- enrolled_students$JORNTRA_CANTIDAD_HOMBRE + enrolled_students$JORNTRA_CANTIDAD_MUJER

loc_enrolled_students <- merge(loc_schools, enrolled_students, 
                                by.x = 'cod_sede_principal', by.y ='SEDE_CODIGO') %>% 
                                dplyr::select("municipio", "NUM_STUDENTS") %>%
                                group_by(municipio) %>%
                                summarise(CAPACITY = sum(NUM_STUDENTS))

# Function to remove tildes and convert to uppercase
remove_tilde_uppercase <- function(x) {
  x <- stri_trans_general(x, "Latin-ASCII")
  x <- toupper(x)
  return(x)
}

# Apply function to all columns
prop_students_per_municpio <- prop_students_per_municpio %>% 
                                mutate(MUNICIPIO = remove_tilde_uppercase(MUNICIPIO)) %>%
                                rename('YEAR' = 'AÑO',
                                       'COD_MUN' = 'CÓDIGO_MUNICIPIO',
                                       'POP_5_16' = 'POBLACIÓN_5_16',
                                       'PROP' = 'TASA_MATRICULACIÓN_5_16') %>% 
                                dplyr::filter(YEAR == 2018) %>%
                                dplyr::select("COD_MUN", "MUNICIPIO", "POP_5_16", "PROP")


school_enrollment_data <- merge(prop_students_per_municpio, loc_enrolled_students, 
                                by.x = 'MUNICIPIO', by.y = 'municipio')



municipios_shp_file <- '../data/raw_data/geodata/Colombia_shp/Municipios.shp'
colombia_municipios <- st_read(municipios_shp_file)

# Assuming school_data is your school enrollment dataframe and closest_n is the dataframe with closest municipios

# Step 1: Calculate enrolled students
school_data <- school_enrollment_data %>% 
  mutate(enrolled_students = POP_5_16 * PROP / 100,
         available_capacity = CAPACITY - enrolled_students)

# Initialize an empty results dataframe
results <- data.frame(origin_municipio = numeric(),
                      destination_municipio = numeric(),
                      student_count = numeric())

N = 20 # Number of closest available municipios to consider

# Loop over each municipio
for(municipio_id in school_data$COD_MUN) {
#for(municipio_id in c(63001)) {
    print(paste("Processing municipio", municipio_id, "..."))
    municipio_shp <- colombia_municipios %>% filter(ID_ESPACIA == municipio_id)
    distances <- st_distance(municipio_shp, colombia_municipios)

    # Get data for the current origin municipio
    origin_data <- filter(school_data, COD_MUN == municipio_id)

    # If capacity is lower than the enrolled students, find closest municipios
    if (origin_data$CAPACITY < origin_data$enrolled_students) {
        print(paste("Distributing municipio", municipio_id, "..."))

        # Find closest municipios exhaustively until all excess students are assigned
        remaining_excess <- origin_data$enrolled_students - origin_data$CAPACITY
        while (round(remaining_excess) > 0) {
            # Get the N closest available municipios
            closest_ns <- order(distances)[1:N]
            closest_municipio_ids <- colombia_municipios$ID_ESPACIA[closest_ns]

            # Filter them based on available capacity
            closest_available <- filter(school_data, COD_MUN %in% closest_municipio_ids & (CAPACITY - enrolled_students) > 0)

            if (nrow(closest_available) == 0) {
                break
            }

            # Choose one randomly
            chosen_row <- sample_n(closest_available, 1)
            chosen_municipio_id <- chosen_row$COD_MUN

            # Calculate available capacity in the chosen municipio
            available_capacity <- chosen_row$CAPACITY - chosen_row$enrolled_students

            # Allocate a random number of students to the chosen municipio, without exceeding available capacity or remaining excess
            # rounded to the nearest integer
            allocated_students <- sample(min(round(available_capacity), round(remaining_excess)), 1)

            # Update data frames
            school_data <- mutate(school_data, enrolled_students = ifelse(COD_MUN == chosen_municipio_id,
                                        enrolled_students + allocated_students, enrolled_students))

            # Append to results
            temp_result <- data.frame(origin_municipio = municipio_id,
                                    destination_municipio = chosen_municipio_id,
                                    student_count = allocated_students)

            results <- rbind(results, temp_result)

            # Update remaining excess students
            remaining_excess <- remaining_excess - allocated_students
        }
    }

    # Account for students staying in the same municipio
    students_staying <- min(origin_data$CAPACITY, origin_data$enrolled_students)
    results <- rbind(results, data.frame(origin_municipio = municipio_id,
                                        destination_municipio = municipio_id,
                                        student_count = students_staying))
}

# Group results by origin and destination to get final counts
final_results <- results %>%
  group_by(origin_municipio, destination_municipio) %>%
  summarise(student_count = sum(student_count)) %>%
  ungroup()


workers_mobility_file = '../data/processed_data/schooldata/movility_matrix_students_estimation.csv'
write_csv(final_results, workers_mobility_file)



### GEOINFO
esc_shp = rgdal::readOGR('../data/raw_data/geodata/Colombia_shp/Municipios.shp')

geodata_info <- esc_shp@data %>%
  dplyr::select("ID_ESPACIA", "NOM_MUNICI", "COD_DEPTO", "NOM_DEPART") %>%
  rename("COD_MUN" = "ID_ESPACIA") %>%
  distinct()

# Define the mapping of department names to abbreviations
dept_to_abbrev <- c(
  "AMAZONAS" = "AMZ",
  "GUAVIARE" = "GVR",
  "VAUPES" = "VPS",
  "GUAINIA" = "GNA",
  "VICHADA" = "VCD",
  "META" = "MTA",
  "CASANARE" = "CSR",
  "BOYACÁ" = "BOY",
  "ARAUCA" = "ARA",
  "NORTE DE SANTANDER" = "NSD",
  "CAUCA" = "CAU",
  "NN" = "NN",  # Stands for 'Not Known', you may want to replace this
  "NARIÑO" = "NAR",
  "PUTUMAYO" = "PUT",
  "CAQUETÁ" = "CAQ",
  "HUILA" = "HUI",
  "SANTANDER" = "SAN",
  "CUNDINAMARCA" = "CUN",
  "BOLÍVAR" = "BOL",
  "CESAR" = "CES",
  "SATA FE DE BOGOTÁ D.C." = "BOG",  # Bogotá, D.C.
  "ANTIOQUIA" = "ANT",
  "CÓRDOBA" = "COR",
  "SUCRE" = "SUC",
  "TOLIMA" = "TOL",
  "CALDAS" = "CLD",
  "VALLE DEL CAUCA" = "VDC",
  "QUINDIO" = "QND",
  "RISARALDA" = "RIS",
  "CHOCÓ" = "CHO",
  "ATLANTICO" = "ATL",
  "LA GUAJIRA" = "GJR",
  "MAGDALENA" = "MAG",
  "ARCHIPIÉLAGO DE SAN ANDRÉS, PROVIDENCIA Y SANTA CATALINA" = "SAP"  # San Andrés and Providencia
)

# Add a new STABBR column
geodata_info <- geodata_info %>%
  mutate(STABBR = dept_to_abbrev[NOM_DEPART])


geoinfo_file <- '../data/processed_data/geodata/geoinfo_municipios_colombia.csv'
write_csv(geodata_info, geoinfo_file)