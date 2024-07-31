#loading library
.libPaths(c(.libPaths(), '~/scratch/Rpackages/4.3.1/'))
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(ggplot2)
library(scales)

#getting file
clean_file<- "/gpfs/home/mgz20ysu/scratch/PCAWG/PCAWG_clean_data.tsv"
meta_file<- "/gfps/home/mgz20ysu/scratch/PCAWG/metadata_PCAWG.tsv"
clean_data<- read_tsv(clean_file)

clean_data[,-1]<- lapply(clean_data[,-1], as.numeric)

#summing reads and creating data frame
genus_sum<- colSums(clean_data[,-1])
genus_df<- data.frame(genus = names(genus_sum), total_reads = genus_sum)

#finding top 10
top_10_genera<- genus_df %>%
	arrange(desc(total_reads)) %>%
	slice(1:10)

print(top_10_genera)

# Create a bar chart using ggplot2
p <- ggplot(top_10_genera, aes(y = reorder(genus, total_reads), x = log10(total_reads))) +
    geom_bar(stat = "identity", fill = "steelblue") +
	geom_text(aes(label = scales::comma(total_reads)), hjust = 1.1, size = 7, colour = "white") +
    labs(title = "Top 10 Most Prevalent Genera by Number of Reads",
         x = "Total reads (Log scale)",
         y = "Genus") +
	scale_x_log10(labels = scales::comma) +
    theme_minimal() +
    theme(
	axis.text.y = element_text(size = 18, angle = 0, hjust = 1, face = "italic"),
	axis.text.x = element_text(size = 18),
	plot.title = element_text(size = 20, hjust = 0.5),
	axis.title.y = element_text(size = 16),
	axis.title.x = element_text(size = 16),
	panel.grid.major = element_blank(),
	panel.grid.minor = element_blank()
	) +
	coord_cartesian(clip = "off")

# Save the plot as a PDF
pdf("top_10_genera.pdf", width = 12, height = 8)
print(p)
dev.off()





#finding number of reads by cancer type
#merging results with metadata
#meta_data<- read_tsv(meta_file)

#cm_data<- clean_data %>%
#	left_join(meta_data, by = "report_id")

#finding top 10
#cancer_type_reads<- cm_data %>%
#	group_by(cancer_type) %>%
#	summarise(across(-report_id, sum, na.rm = TRUE))
#
#print(cancer_type_reads)

#write_tsv(cancer_type_reads, "/gfps/home/mgz20ysu/scratch/PCAWG/Cancer_reads.tsv")

#finding top 10 genera by cancer type
#pivot longer to make easier to analyse
#cm_data_long<- cm_data %>%
#	pivot_longer(cols = -c(report_id, cancer_type), names_to = "genus", values_to = "reads")

#number of reads per genus for cancer type
#genus_count_per_ct<- cm_data_long %>%
#	group_by(cancer_type, genus) %>%
#	summarise(total_reads = sum(reads, na.rm = TRUE, .groups = 'drop')

#finding top 10 per cancer type
#top_10_genera_per_ct<- genus_count_per_ct %>%
#	group_by(cancer_type) %>%
#	arrange(desc(total_reads)) %>%
#	slice_head(n=10) %>%
#	ungroup()
#
#print(top_10_genera_per_ct)
#
#write_tsv(top_10_genera_per_ct, "/gfps/home/mgz20ysu/PCAWG/top_10_genera_per_ct.tsv")

