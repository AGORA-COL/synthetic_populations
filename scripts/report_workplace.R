library(tidyverse)
work_df = readxl::read_xlsx('../data/raw_data/workplacedata/08092020 Unidad Economica ID.xlsx')

work_df = work_df %>% rename(Total = "Número total de empleados")

brks_work = c(0,5,10,50,100,500,1000,5000,2000000)
hist(work_df$Total, breaks = brks_work,)
