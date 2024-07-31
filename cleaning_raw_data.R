#cleaning the data

#loading libraries
.libPaths(c(.libPaths(), '~/scratch/Rpackages/4.3.1/'))
library(dplyr)
library(readr)
library(stringr)
library(tidyr)

#finding file for input 
input_file<- "/gpfs/home/mgz20ysu/scratch/PCAWG/PCAWG_raw_data.tsv"

#reading raw data
raw_data<- read_tsv(input_file)

#removing contaminants
contaminants<- c("Afipia", "Aquabacterium", "Asticcacaulis", "Aurantimonas", "Beijerinckia", "Bosea", "Bradyrhizobium", "Brevundim",
"Caulobacter", "Craurococcus", "Devosia", "Hoeflea", "Mesorhizobium", "Methylobacterium", "Novosphingobium", "Ochrobactrum", "Paracoccus", "Pedomicrobium",
"Phyllobacterium", "Rhizobium", "Roseomonas", "Sphingobium", "Sphingomonas", "Sphingopyxis", "Acidovorax", "Azoarcus", "Azospira", "Burkholderia",
"Comamonas", "Cupriavidus", "Curvibacter", "Delftia", "Duganella", "Herbaspirillum", "Janthinobacterium", "Kingella", "Leptothrix", "Limnobacter",
"Massilia", "Methylophilus", "Methyloversatilis", "Oxalobacter", "Pelomonas", "Polaromonas", "Ralstonia", "Schlegelella", "Sulfuritalea", "Undibacterium",
"Variovorax", "Acinetobacter", "Enhydrobacter", "Enterobacter", "Escherichia", "Nevskia", "Pseudomonas", "Pseudoxanthomonas", "Psychrobacter",
"Stenotrophomonas", "Xanthomonas", "Aeromicrobium", "Arthrobacter", "Beutenbergia", "Brevibacterium", "Corynebacterium", "Curtobacterium", "Dietzia",
"Geodermatophilus", "Janibacter", "Kocuria", "Microbacterium", "Micrococcus", "Microlunatus", "Patulibacter", "Propionibacterium", "Rhodococcus",
"Tsukamurella", "Abiotrophia", "Bacillus", "Brevibacillus", "Brochothrix", "Facklamia", "Paenibacillus", "Streptococcus", "Chryseobacterium",
"Dyadobacter", "Flavobacterium", "Hydrotalea", "Niastella", "Olivibacter", "Pedobacter", "Wautersiella", "Deinococcus", "Homo")

contam_present<- contaminants[contaminants %in% colnames(raw_data)]

contam_data<- raw_data %>%
	select(-all_of(contam_present))

#removing false positives

#converting to numeric
contam_data[,-1] <- lapply(contam_data[,-1], as.numeric)

#setting a read threshold
contam_data[, -1][contam_data[, -1] < 10]<- 0

#removing columns where sum is 0
cols_kept<- sapply(contam_data[,-1], function(col) sum(col) >0)
clean_data<- contam_data[, c(TRUE, cols_kept)]

#converting report id back to chr because it keeps breaking the code
clean_data$report_id <- as.character(clean_data$report_id)

#removing reports with 0 reads
clean_data <- clean_data[rowSums(clean_data[, -1]) > 0, ]

print(clean_data)

write.table(clean_data, file = "/gpfs/home/mgz20ysu/scratch/PCAWG/PCAWG_clean_data.tsv", col.names = TRUE, row.names = FALSE, sep = "\t", quote = FALSE)


