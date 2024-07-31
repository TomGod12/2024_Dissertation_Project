#loading library
library(dplyr)

#getting file
setwd('C:/Users/thoma/Documents/AA_Molecular_Medicine/Research_Project/PCAWG/Metadata')
data<- read_tsv(file= 'community_metadata.tsv')

#filtering for cancer type
cancer_data<- data %>%
  filter(Study_Name == "Stomach") %>% #change for cancer type
  select(Study_Name, Cytomegalovirus) #change for genus

#getting genus reads within cancer type
genus_reads<- sum(cancer_data$Cytomegalovirus, na.rm = TRUE)

total_reads<- sum(data$Cytomegalovirus, na.rm = TRUE)

#calculate percentage
perc<- (genus_reads / total_reads) * 100

print(perc)