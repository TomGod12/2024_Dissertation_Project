#load libaries
.libPaths(c(.libPaths(), '~/scratch/Rpackages/4.3.1/'))
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(ggplot2)
library(scales)

#grabbing file
CT_file<- "/gpfs/home/mgz20ysu/scratch/PCAWG/community_metadata.tsv"
data<- read_tsv(CT_file)

#specify the columns to exclude
exclude_columns <- c("report_id", "donor_sex", "Study_Name", "project", "icgc_donor_id", "project_code", "country", "donor_sex", "donor_vital_status", 
"donor_diagnosis_icd10", "first_therapy_type", "first_therapy_response", "donor_age_at_diagnosis", "donor_survival_time", "donor_interval_of_last_followup",
"tobacco_smoking_history_indicator", "tobacco_smoking_intensity", "alcohol_history", "dcc_specimen_type")

#identify the genera columns by excluding the specified columns
genera_columns <- setdiff(names(data), exclude_columns)

#calculate the sum of all genera reads by report_id
data_summarized <- data %>%
  rowwise() %>%
  mutate(total_reads = sum(c_across(all_of(genera_columns)), na.rm = TRUE)) %>%
  ungroup()

#calculate the median number of reads by report_id within each cancer type
median_reads_by_cancer <- data_summarized %>%
  group_by(Study_Name) %>%
  summarize(median_reads = median(total_reads, na.rm = TRUE)) %>%
	arrange(desc(median_reads))

#print the result
print(median_reads_by_cancer)

#plotting the graph
p<- ggplot(median_reads_by_cancer, aes(y = reorder(Study_Name, median_reads), x = log10(median_reads))) +
        geom_bar(stat = "identity", fill = "steelblue") +
        geom_text(aes(label = scales::comma(median_reads)), hjust = 1.1, size = 5, colour = "white") +
        labs(title = "Median Genera Reads by Cancer Type",
                x = "Median reads (Log scale)",
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
pdf("median_genus_reads_by_cancer_type.pdf", width = 12, height = 8)
print(p)
dev.off()

