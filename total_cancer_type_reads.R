#loading library
.libPaths(c(.libPaths(), '~/scratch/Rpackages/4.3.1/'))
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(ggplot2)
library(scales)

#getting and reading file
comm_file<- "/gpfs/home/mgz20ysu/scratch/PCAWG/community_metadata.tsv"
comm_data<- read_tsv(comm_file)

#excluding non reads columns from data
exclude_columns<- c("project", "icgc_donor_id", "project_code", "country", "donor_sex", "donor_vital_status", "donor_diagnosis_icd10", 
	"first_therapy_type", "first_therapy_response", "donor_age_at_diagnosis", "donor_survival_time", "donor_interval_of_last_followup", 
	"tobacco_smoking_history_indicator", "tobacco_smoking_intensity", "alcohol_history", "dcc_specimen_type")

#finding reads by cancer type
cancer_type_reads<- comm_data %>%
	select(-all_of(exclude_columns)) %>%
       group_by(Study_Name) %>%
       summarise(total_genus_reads = sum(across(where(is.numeric), ~sum(.x, na.rm = TRUE)))) %>%
	arrange(desc(total_genus_reads))

print(cancer_type_reads)

#plotting the graph
p<- ggplot(cancer_type_reads, aes(y = reorder(Study_Name, total_genus_reads), x = log10(total_genus_reads))) +
	geom_bar(stat = "identity", fill = "steelblue") +
	geom_text(aes(label = scales::comma(total_genus_reads)), hjust = 1.1, size = 5, colour = "white") +
	labs(title = "Total Genera Reads by Cancer Type",
		x = "Total reads (Log scale)",
		y = "Cancer Type") +
	scale_x_log10(labels = scales::comma) +
	theme_minimal() +
	theme(
		axis.text.y = element_text(size = 14, angle = 0, hjust = 1),
		axis.text.x = element_text(size = 18),
		plot.title = element_text(size = 20, hjust = 0.5),
		axis.title.y = element_text(size = 16, hjust = 0.2),
		axis.title.x = element_text(size = 16),
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank()
	) +
	coord_cartesian(clip = "off")

# Save the plot as a PDF
pdf("total_genus_reads_by_cancer_type.pdf", width = 12, height = 8)
print(p)
dev.off()

#finding top 10 genera by cancer type
#pivot longer to make easier to analyse
#cm_data_long<- cm_data %>%
#       pivot_longer(cols = -c(report_id, cancer_type), names_to = "genus", values_to = "reads")

#number of reads per genus for cancer type
#genus_count_per_ct<- cm_data_long %>%
#       pivot_longer(cols = -c(report_id, cancer_type), names_to = "genus", values_to = "reads")

#number of reads per genus for cancer type
#genus_count_per_ct<- cm_data_long %>%
#       group_by(cancer_type, genus) %>%
#       summarise(total_reads = sum(reads, na.rm = TRUE, .groups = 'drop')

#finding top 10 per cancer type
#top_10_genera_per_ct<- genus_count_per_ct %>%
#       group_by(cancer_type) %>%
#       arrange(desc(total_reads)) %>%
#       slice_head(n=10) %>%
#       ungroup()

#print(top_10_genera_per_ct)

#write_tsv(top_10_genera_per_ct, "/gfps/home/mgz20ysu/PCAWG/top_10_genera_per_ct.tsv")

