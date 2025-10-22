---
  title: "Assignment 5: Data Visualization"
author: "Student Name"
date: "Fall 2025"
output: pdf_document
geometry: margin=2.54cm
fig_height: 3
fig_width: 4
editor_options: 
  chunk_output_type: console
---
  
  ## OVERVIEW
  
  This exercise accompanies the lessons in Environmental Data Analytics on Data Visualization 

## Directions
1. Rename this file `<FirstLast>_A05_DataVisualization.Rmd` (replacing `<FirstLast>` with your first and last name).
2. Change "Student Name" on line 3 (above) with your name.
3. Work through the steps, **creating code and output** that fulfill each instruction.
4. Be sure your code is tidy; use line breaks to ensure your code fits in the knitted output.
5. Be sure to **answer the questions** in this assignment document.
6. When you have completed the assignment, **Knit** the text and code into a single PDF file.

---
  
  ## Set up your session 
  
  1.Set up your session. Load the tidyverse, here & cowplot packages, and verify your home directory. Read in the NTL-LTER processed data files for nutrients and chemistry/physics for Peter and Paul Lakes (use the tidy `NTL-LTER_Lake_Chemistry_Nutrients_PeterPaul_Processed.csv` version in the Processed_KEY folder) and the processed data file for the Niwot Ridge litter dataset (use the `NEON_NIWO_Litter_mass_trap_Processed.csv` version, again from the Processed_KEY folder). 

2. Make sure R is reading dates as date format; if not change the format to date.

```{r initialize project}
#1 
# Load required packages
library(tidyverse) 
library(here)
library(cowplot)

getwd()

NTL_LTER_PeterPaul <- read.csv(
  file = here("Data/Processed_KEY/NTL-LTER_Lake_Chemistry_Nutrients_PeterPaul_Processed.csv"),
  stringsAsFactors = TRUE)

Niwot_Ridge <- read.csv(
  file = here("Data/Processed_KEY/NEON_NIWO_Litter_mass_trap_Processed.csv"),
  stringsAsFactors = TRUE)

# 2. Convert dates to proper format using lubridate (as shown in your lab)
NTL_LTER_PeterPaul$sampledate <- lubridate::ymd(NTL_LTER_PeterPaul$sampledate)
Niwot_Ridge$collectDate <- lubridate::ymd(Niwot_Ridge$collectDate)

#3
my_theme <- theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(size = 11, face = "italic"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

theme_set(my_theme)

#4 
ggplot(NTL_LTER_PeterPaul, aes(x = po4, y = tp_ug, color = lakename)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +  
  scale_color_manual(values = c("red", "orange")) +  
  xlim(0, 50) +  
  ylim(0, 100) +
  labs(
    title = "Total Phosphorus vs Phosphate by Lake",
    x = "Phosphate (PO4)",
    y = "Total Phosphorus (μg/L)",
    color = "Lake Name"
  )

#5
# Create month variable using factor with month abbreviations (as shown in your lab)
NTL_LTER_PeterPaul$month <- factor(
  format(NTL_LTER_PeterPaul$sampledate, "%b"),
  levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
             "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
)

# a) Temperature
temp_plot <- NTL_LTER_PeterPaul %>%
  filter(!is.na(temperature_C)) %>%
  ggplot(aes(x = month, y = temperature_C, fill = lakename)) +
  geom_boxplot() +
  labs(x = "Month", y = "Temperature (°C)", fill = "Lake") +
  theme(legend.position = "none")

# b) Total Phosphorus
tp_plot <- NTL_LTER_PeterPaul %>%
  filter(!is.na(tp_ug)) %>%
  ggplot(aes(x = month, y = tp_ug, fill = lakename)) +
  geom_boxplot() +
  labs(x = "Month", y = "Total Phosphorus (μg/L)", fill = "Lake") +
  theme(legend.position = "none",
        axis.title.y = element_blank())

# c) Total Nitrogen  
tn_plot <- NTL_LTER_PeterPaul %>%
  filter(!is.na(tn_ug)) %>%
  ggplot(aes(x = month, y = tn_ug, fill = lakename)) +
  geom_boxplot() +
  labs(x = "Month", y = "Total Nitrogen (μg/L)", fill = "Lake") +
  theme(legend.position = "none",
        axis.title.y = element_blank())

# Extract legend from one plot (using cowplot as shown in your lab)
legend_plot <- NTL_LTER_PeterPaul %>%
  filter(!is.na(temperature_C)) %>%
  ggplot(aes(x = month, y = temperature_C, fill = lakename)) +
  geom_boxplot() +
  theme(legend.position = "bottom")
legend <- cowplot::get_legend(legend_plot)

# Combine plots using cowplot (as shown in your lab)
combined_plot <- cowplot::plot_grid(
  temp_plot, tp_plot, tn_plot,
  nrow = 1,
  align = "h",
  labels = c("A", "B", "C")
)

# Add the legend at the bottom
final_plot <- cowplot::plot_grid(combined_plot, legend, ncol = 1, rel_heights = c(1, 0.1))
final_plot

#6
needle_data <- Niwot_Ridge %>%
  filter(functionalGroup == "Needles")

# 6 - Color aesthetic version
plot_color <- ggplot(needle_data, 
                     aes(x = collectDate, y = dryMass, color = nlcdClass)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    title = "Needle Litter Mass Over Time (Color Aesthetic)",
    x = "Collection Date",
    y = "Dry Mass (g)",
    color = "Land Cover"
  )

print(plot_color)

# 7 - Faceted version (using facet_wrap as shown in your lab)
plot_facet <- ggplot(needle_data, 
                     aes(x = collectDate, y = dryMass)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~nlcdClass, ncol = 1) +
  labs(
    title = "Needle Litter Mass Over Time (Faceted)",
    x = "Collection Date",
    y = "Dry Mass (g)"
  )

print(plot_facet)


