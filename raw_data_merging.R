#create a community matrix for kraken reports

#load libraries
.libPaths(c(.libPaths(), '~/scratch/Rpackages/4.3.1/'))
library(dplyr)
library(readr)
library(stringr)
library(tidyr)

#define work directory
report_dir<- "/gpfs/home/mgz20ysu/scratch/PCAWG/kraken_reports/"
kraken_files<- list.files(report_dir, pattern="*.kraken_report", full.names=TRUE)

#empty data frame to store results
all_results<- data.frame()

#loop through each file
for (file in kraken_files) {
    data<- read_tsv(file, col_names=TRUE, skip=2, show_col_types = FALSE)

    filtered_data<- data%>%
        select(reads, rank, taxName)

    report_id<- str_remove(basename(file), ".kraken_report")
    filtered_data<- filtered_data %>% 
        mutate(report_id = report_id)
    
    all_results<- bind_rows(all_results, filtered_data)

}

#filtering ranks for each clade
ranks_of_interest<- c("phylum", "class", "order", "family", "genus", "species")

filtered_reports<- all_results %>%
	filter(rank %in% ranks_of_interest)	

#making the report data frame
kraken_reports<- filtered_reports %>%
    group_by(report_id, rank, taxName) %>%
    summarise(reads_by_clade = sum(reads), .groups = "drop")

print(kraken_reports)

#creating desired community matrix

genus_table<- kraken_reports %>%
	filter(rank == "genus", taxName != "no rank") %>%
	select(report_id, taxName, reads_by_clade) %>%
	pivot_wider(names_from = taxName, values_from = reads_by_clade, values_fill = list(reads_by_clade = 0))

print(genus_table) 

#saving as a tsv

write.table(genus_table, file = "PCAWG_raw_data.tsv", col.names = TRUE, row.names = FALSE, sep = "\t", quote = FALSE)


