#loading libraries
library(dplyr)
library(readr)
library(stringr)
library(tidyr)

#getting file
setwd("C:/Users/thoma/Documents/AA_Molecular_Medicine/Research_Project/PCAWG/Metadata")
data<- read_tsv(file= 'community_metadata.tsv')

#metadata columns to ignore
metadata_columns <- c("report_id", "donor_sex", "Study_Name", "project", "icgc_donor_id", "project_code", "country", "donor_sex", "donor_vital_status",
                      "donor_diagnosis_icd10", "first_therapy_type", "first_therapy_response", "donor_age_at_diagnosis", "donor_survival_time", "donor_interval_of_last_followup",
                      "tobacco_smoking_history_indicator", "tobacco_smoking_intensity", "alcohol_history", "dcc_specimen_type")

#filter for cancer type
brain_cancer_data <- data %>%
  filter(Study_Name == "Stomach") %>%
  select(-one_of(metadata_columns))

#total reads for all genera within cancer type
genus_sums <- brain_cancer_data %>%
  summarise(across(everything(), sum, na.rm = TRUE))

#filtering for top 10
top_10_genera <- genus_sums %>%
  gather(key = "genus", value = "reads") %>%
  arrange(desc(reads)) %>%
  slice(1:10)

#print and save
print(top_10_genera)

write.table(top_10_genera, file = "C:/Users/thoma/Documents/AA_Molecular_Medicine/Research_Project/PCAWG/genera10/stomach_top_10.tsv", col.names = TRUE, row.names = FALSE, sep = "\t", 
            quote = FALSE)

