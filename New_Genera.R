#loading libraries
library(tidyverse)
library(stringr)

setwd('C:/Users/thoma/Documents/AA_Molecular_Medicine/Research_Project/PCAWG/Metadata')

#grabbing file
data <- read_tsv(file='community_metadata.tsv')

#filtering for specific studies and selecting necessary columns
cancer_data <- data %>%
  filter(Study_Name == 'Stomach') %>% #cancer type goes here
  select(Study_Name, Bacteroides, Tannerella, Cytomegalovirus) #3 investigated genera go here

#reshaping data for plotting
cancer_long <- cancer_data %>%
  pivot_longer(cols =	c(Bacteroides, Tannerella, Cytomegalovirus), names_to = "Genus", values_to = "Abundance")

#replacing 0 reads with 1
cancer_long <- cancer_long %>%
  mutate(Abundance = ifelse(Abundance < 1, 1, Abundance))

#creating title
plot_title <- "Distribution of reads of three \nprevalent genera in Stomach cancer" 

#plotting
ggplot(cancer_long, aes(x = Genus, y = Abundance, fill = Genus)) +
  geom_boxplot(fill = "cyan") +
  coord_flip() +
  scale_y_log10() +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, margin = margin(t = 10, b = 10)),
    plot.title.position = "plot",
    legend.position = "none",
    axis.text.y = element_text(size = 11),
    plot.margin = margin(t = 10, r = 20, b = 10, l = 20)
  ) +
  labs(title = plot_title,
       x = "Bodily Site",
       y = "Number of reads (log scale)")