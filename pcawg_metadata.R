library(tidyverse)

setwd('C:/Users/thoma/Documents/AA_Molecular_Medicine/Research_Project/PCAWG/Metadata')

#downloaded metadata from https://github.com/ICGC-TCGA-PanCancer/pcawg-metadata/tree/main - 24 June 2024
md <- read_tsv(file='WGS.metadata.tsv')
comm <- read_tsv(file='PCAWG_clean_data.tsv')
clin <- read_tsv(file='pcawg_donor_clinical_August2016_v9.txt')

md$report_id <- gsub('.bam', '', md$report_id)

md_comm <- merge(md, comm, by='report_id', all=FALSE)


# There are lots of duplicates in the metadata, perhaps due to different aliquots. Let's filter down to remove duplicates
md_comm <- md_comm %>% select(c(icgc_donor_id, dcc_specimen_type, colnames(comm) ))

length(which(duplicated(md_comm)))

md_comm <- md_comm %>% distinct()

#double check there are no more duplicates
length(which(duplicated(md_comm$report_id)))


#2,471 obs reduced to 2, after merging

md_clin_comm <- merge(clin, md_comm, by='icgc_donor_id', all=FALSE)


md_clin_comm <- md_clin_comm %>% select(c(icgc_donor_id, project_code, donor_sex, donor_vital_status,
                                          donor_diagnosis_icd10, first_therapy_type, first_therapy_response,
                                          donor_age_at_diagnosis, donor_survival_time, donor_interval_of_last_followup,
                                          tobacco_smoking_history_indicator, tobacco_smoking_intensity, alcohol_history,
                                          dcc_specimen_type,
                                          colnames(comm)))




#load in tumour type abbreviation - https://gdc.cancer.gov/resources-tcga-users/tcga-code-tables/tcga-study-abbreviations
abbrev <- read_tsv(file='tcga_study_abbreviations.txt')


md_clin_comm <- md_clin_comm %>% separate(project_code, into=c('project', 'country'), remove=FALSE)


md_clin_comm <- merge(md_clin_comm, abbrev, by.x='project', by.y='Study_Abbreviation', all.x=TRUE, all.y=FALSE)


write.table(md_clin_comm, file='community_metadata.tsv', row.names = FALSE, col.names = TRUE, sep='\t', quote=FALSE)





