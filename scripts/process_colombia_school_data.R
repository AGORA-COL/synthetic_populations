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
library(stringi)
library(tidyr)


ies_data_file <- '../data/raw_data/schooldata/IES_Estudiantes_matriculados_2019.csv'
ies_data <- read_csv(ies_data_file) %>% rename("School_id" = "CÓDIGO DE LA INSTITUCIÓN" ,
                                                "Name" = "INSTITUCIÓN DE EDUCACIÓN SUPERIOR (IES)",
                                                "Mun_code" = "CÓDIGO DEL MUNICIPIO (PROGRAMA)" ,
                                                "Year" = "AÑO",
                                                "Semester" = "SEMESTRE" ,
                                                "Ungraded" = "MATRICULADOS") %>%
                                        filter(Semester == 1, Year == 2019) %>%
                                        dplyr::select(School_id, Name, Mun_code, Ungraded) %>%
                                        group_by(School_id, Name, Mun_code) %>%
                                        summarise(Ungraded = sum(Ungraded)) %>%
                                        mutate(Prek = 0, Kinder = 0, Gr01_gr12 = 0, Income = 0)

synth_colleges = '../data/processed_data/schooldata/IES_colombia_ESC.csv'
write_csv(ies_data, synth_colleges)



enrolled_students_file  = '../data/raw_data/schooldata/Alumnos matriculados en educación tradicional y CLEI según rangos de edad por jornada.csv'
loc_schools_file        = '../data/raw_data/schooldata/Localizacion_Colegios.csv'
prop_students_per_municpio_file = '../data/raw_data/schooldata/ESTADISTICAS_EN_EDUCACION_BASICA_POR_MUNICIPIO.csv'
prop_students_per_municpio <- read_csv(prop_students_per_municpio_file)

# Function to remove tildes and convert to uppercase
remove_tilde_uppercase <- function(x) {
  x <- stri_trans_general(x, "Latin-ASCII")
  x <- toupper(x)
  return(x)
}

# Apply function to all columns
code_municpios <- prop_students_per_municpio %>% 
                                mutate(MUNICIPIO = remove_tilde_uppercase(MUNICIPIO)) %>%
                                rename('COD_MUN' = 'CÓDIGO_MUNICIPIO') %>%
                                dplyr::select('COD_MUN', 'MUNICIPIO') %>%
                                distinct()


enrolled_students <- read.csv(enrolled_students_file, sep=';')
loc_schools <- read_csv(loc_schools_file)

enrolled_students$NUM_STUDENTS <- enrolled_students$JORNTRA_CANTIDAD_HOMBRE + enrolled_students$JORNTRA_CANTIDAD_MUJER
enrolled_students <- enrolled_students %>%
  # Split 'Age_range' into 'Min_age' and 'Max_age'
  mutate(Min_age = str_extract(RANGOEDAD_NOMBRE, "\\d+(?=-)"),
         Max_age = str_extract(RANGOEDAD_NOMBRE, "(?<=-)\\d+")) %>%
  # Handle 'above' or 'y más'
  mutate(Max_age = if_else(str_detect(RANGOEDAD_NOMBRE, "y más"),
                           "99",
                           Max_age),
         Min_age = if_else(is.na(Min_age),
                           str_extract(RANGOEDAD_NOMBRE, "\\d+"),
                           Min_age)) %>%
  # Convert to numeric
  mutate(across(c(Min_age, Max_age), as.numeric))

loc_enrolled_students <- merge(loc_schools, enrolled_students, 
                                by.x = 'cod_sede_principal', by.y ='SEDE_CODIGO') %>% 
                                mutate(CAPACITY = NUM_STUDENTS) %>%
                                rename( "School_Code" = "id_sede", 
                                        "School_Name" = "nombre_institucion",
                                        "Type" = "sector", 
                                        "Grade" = "GRADO_CODIGO", 
                                        "Zone" = "municipio",
                                        "Latitude" = "latitud", 
                                        "Longitude" = "longitud") %>%
                                dplyr::select("School_Code", "School_Name", "Type", "Grade", "Min_age", "Max_age", "Zone", "Latitude", "Longitude", "CAPACITY") %>%
                                group_by(School_Code, School_Name, Type, Grade, Min_age, Max_age, Zone, Latitude, Longitude) %>%
                                summarise(Students = sum(CAPACITY))

school_enrollment_data <- merge(loc_enrolled_students, code_municpios, 
                                by.x = 'Zone', by.y = 'MUNICIPIO')


student_capacity_file = '../data/processed_data/schooldata/Schools_processed_capacity_colombia.csv'
write_csv(school_enrollment_data, student_capacity_file)







mobility_data_file <- "../data/processed_data/schooldata/movility_matrix_students_estimation.csv"
mobility_data <- read_csv(mobility_data_file)

# # Assuming you've loaded mobility_data and school_enrollment_data
# mobility_data <- data.frame(
#   origin_municipio = c(5001, 5002, 5002, 5004, 5021),
#   destination_municipio = c(5001, 5002, 5282, 5004, 5021),
#   student_count = c(373888, 2679, 12, 251, 591)
# )

# school_enrollment_data <- data.frame(
#   Zone = c("ABEJORRAL", "ABEJORRAL"),
#   School_Code = c(11851, 27968),
#   School_Name = c("CENT EDUC RUR VILLA INES", "I.E PEDRO PABLO RAMIREZ"),
#   Type = c("OFICIAL", "OFICIAL"),
#   Grade = c("TR", "08"),
#   Min_age = c(3, 16),
#   Max_age = c(5, 19),
#   Students = c(1, 6),
#   COD_MUN = c(5002, 5002)
# )

# First let's work with the school_enrollment_data to get counts by municipality and grade
grade_counts <- school_enrollment_data %>% 
  group_by(COD_MUN, Grade, Min_age, Max_age) %>% 
  summarise(students_per_grade = sum(Students))

# Merge grade_counts with the mobility data
merged_data <- grade_counts %>% 
  full_join(mobility_data, by = c("COD_MUN" = "origin_municipio")) %>% 
  rename(destination_municipio = destination_municipio, origin_municipio = COD_MUN) 

# Calculate TotalStudents and ResidentStudents
merged_data <- merged_data %>% 
  group_by(destination_municipio, origin_municipio, Grade, Min_age, Max_age) %>% 
  summarise(TotalStudents = sum(student_count, na.rm = TRUE), 
            ResidentStudents = sum(ifelse(destination_municipio == origin_municipio, student_count, 0), na.rm = TRUE)) %>% 
  mutate(PropResidence = ResidentStudents / TotalStudents)

# Final result, arranged for clarity
final_data <- merged_data %>% 
  arrange(destination_municipio, origin_municipio, Grade) %>% 
  select(destination_municipio, PropResidence, origin_municipio, Grade, Min_age, Max_age, TotalStudents, ResidentStudents)

# Show the final result
student_mobility_file = '../data/processed_data/schooldata/Students_by_municipio.csv'
write_csv(final_data, student_mobility_file)
