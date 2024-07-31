#loading libraries
library(tidyverse)
library(stringr)

setwd('C:/Users/thoma/Documents/AA_Molecular_Medicine/Research_Project/PCAWG/Metadata')

#grabbing file
data <- read_tsv(file='community_metadata.tsv')

#filtering for specific studies and selecting necessary columns
stomach_data <- data %>%
  filter(Study_Name %in% c("Stomach", "Oral", "Cervical", "Liver", "Brain", "Colorectal", 
                           "Acute Myeloid Leukemia")) %>%
  select(Study_Name, Staphylococcus) #change for investigated genus

#reshaping data for plotting
stomach_long <- stomach_data %>%
  pivot_longer(cols =	Staphylococcus, names_to = "Genus", values_to = "Abundance")

#replacing 0 reads with 1
stomach_long <- stomach_long %>%
  mutate(Abundance = ifelse(Abundance < 1, 1, Abundance))

#wrapping the specific study name
stomach_long <- stomach_long %>%
  mutate(Study_Name = ifelse(Study_Name == "Acute Myeloid Leukemia", 
                             str_wrap(Study_Name, width = 15), 
                             Study_Name))

#manually wrapping the title with newline character
plot_title <- "Number of Reads of Staphylococcus in Cancer \nFrom Seven Different Bodily Locations"

# plotting
ggplot(stomach_long, aes(x = Study_Name, y = Abundance, fill = Genus)) +
  geom_boxplot(fill = "cyan") +
  coord_flip() +
  scale_y_log10() +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, margin = margin(t = 10, b = 10)),
    plot.title.position = "plot",
    legend.position = "none",
    axis.text.y = element_text(size = 11)
  ) +
  labs(title = plot_title,
       x = "Bodily Site",
       y = "Number of reads (log scale)")
