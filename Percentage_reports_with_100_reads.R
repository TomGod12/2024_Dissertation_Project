#loading libraries
library(dplyr)

setwd('C:/Users/thoma/Documents/AA_Molecular_Medicine/Research_Project/PCAWG/Metadata')

#grabbing file
data <- read_tsv(file='community_metadata.tsv')

#filter for bodily location, change for different cancer type
body_data <- data %>% 
  filter(Study_Name == "Stomach")

#number of reports with 100 or more reads of genus, change for different genera
body_high_reads <- body_data %>% 
  filter(Cytomegalovirus >= 100) %>% 
  nrow()

#total number of bodily site reports
total_body_reports <- nrow(body_data)

#finding percentage
percentage <- (body_high_reads / total_body_reports) * 100

print(percentage)