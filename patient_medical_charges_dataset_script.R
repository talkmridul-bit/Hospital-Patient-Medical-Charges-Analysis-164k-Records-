library(tidyverse)
library(readxl)
data_raw<- read_excel(file.choose())
data_raw
glimpse(data_raw)
clean_hospital_patient_data <- data_raw %>% distinct() %>%
  mutate(
    `Total Discharges` = as.integer(`Total Discharges`),
    `Average Covered Charges` = as.numeric(`Average Covered Charges`),
    `Average Total Payments` = as.numeric(`Average Total Payments`),
    `Average Medicare Payments` = as.numeric(`Average Medicare Payments`)
  ) %>%
  filter(`Total Discharges` > 0 & `Average Total Payments` > 0 &
           `Average Covered Charges` > 0 & `Average Medicare Payments` > 0)%>%
  group_by(`Provider Name`,`Provider City`,`Hospital Referral Region Description`)%>%
   
  summarise(
    Avg_Discharges = mean(`Total Discharges`),
    Total_Avg_Cov_Charges = sum(`Average Covered Charges`),
    Total_Avg_Payments = sum(`Average Total Payments`),
    Total_Avge_Medicare_Payments = sum(`Average Medicare Payments`),
  .groups = 'drop'  
  ) %>% arrange(desc(Total_Avg_Payments))
clean_hospital_patient_data    
write.csv(clean_hospital_patient_data,'patient_medical_charges_dataset_data.csv',
          row.names = FALSE) 
getwd()    
          
           
                   
  
  