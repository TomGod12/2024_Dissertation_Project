#loading libraries
library(Rtsne)
library(ggplot2)
library(readr)
library(dplyr)
library(ClassDiscovery)
library(tidyverse)

setwd('C:/Users/thoma/Documents/AA_Molecular_Medicine/Research_Project/PCAWG/Metadata')

#reading files
md <- read_tsv(file='WGS.metadata.tsv')
comm <- read_tsv(file='PCAWG_clean_data.tsv')
clin <- read_tsv(file='pcawg_donor_clinical_August2016_v9.txt')
abbrev <- read_tsv(file='tcga_study_abbreviations.txt')

#remove ".bam"
md$report_id <- gsub('.bam', '', md$report_id)

#set rownames for community matrix
community_matrix <- comm %>% column_to_rownames('report_id')

#filter non-zero rows and columns
comm_filt <- community_matrix[rowSums(community_matrix) != 0, ]
comm_filt <- comm_filt[, colSums(comm_filt) != 0]

#calculate distance matrix
ppcg_pearson <- distanceMatrix(as.data.frame(t(comm_filt)), metric = 'pearson')

#adjust perplexity to be less than number of samples divided by 3
num_samples <- nrow(comm_filt)
perplexity_value <- min(30, floor(num_samples / 3))

#t-SNE
ppcg_tsne <- Rtsne(ppcg_pearson, dims = 2, perplexity = perplexity_value, pca = FALSE, max_iter = 5000, is_distance = TRUE, verbose = TRUE)

#creating data frame with t-SNE results
ppcg_dat <- as.data.frame(ppcg_tsne$Y)
ppcg_dat$sample <- rownames(comm_filt)

#merge t-SNE results with metadata to get cancer type
md_comm <- merge(md, comm, by='report_id', all=FALSE)

#remove duplicates
md_comm <- md_comm %>% select(c(icgc_donor_id, dcc_specimen_type, colnames(comm)))
md_comm <- md_comm %>% distinct()

#merge clinical data with md_comm
md_clin_comm <- merge(clin, md_comm, by='icgc_donor_id', all=FALSE)

#select relevant columns
md_clin_comm <- md_clin_comm %>% select(c(icgc_donor_id, project_code, donor_sex, donor_vital_status,
                                          donor_diagnosis_icd10, first_therapy_type, first_therapy_response,
                                          donor_age_at_diagnosis, donor_survival_time, donor_interval_of_last_followup,
                                          tobacco_smoking_history_indicator, tobacco_smoking_intensity, alcohol_history,
                                          dcc_specimen_type, colnames(comm)))

#split project_code and merge with abbreviations
md_clin_comm <- md_clin_comm %>% separate(project_code, into=c('project', 'country'), remove=FALSE)
md_clin_comm <- merge(md_clin_comm, abbrev, by.x='project', by.y='Study_Abbreviation', all.x=TRUE, all.y=FALSE)

#merge t-SNE results with study name
ppcg_dat <- merge(ppcg_dat, md_clin_comm %>% select(report_id, Study_Name), by.x='sample', by.y='report_id', all.x=TRUE)

#plotting data with colors based on cancer type
ggplot(ppcg_dat, aes(x=V1, y=V2, color=Study_Name)) +
  geom_point() +
  theme_minimal() +
  labs(x='t-SNE 1', y='t-SNE 2', 
       title='Kraken report t-SNE',  
       subtitle=paste0('Pearson distance - Samples: ', nrow(ppcg_dat))) +
  theme(plot.title=element_text(hjust=0.5), 
        plot.subtitle=element_text(hjust=0.5),
        legend.position = "top",
        legend.title = element_blank()
        )

ggsave(filename = "t-SNE.pdf", width = 8.7, height = 8.7)