#------------------------------------------------------------------------------#
# Script Analysis -------------------------------------------------------------
# One Hundred Years of Ceriantharia (Cnidaria): A Mixed-Methods Review
# of Research Trends, Biases, and Knowledge Gaps in Tube-dwelling Anemones
# Authors: Marcela Aparecida de Barros
# E-mail: marcelapdebarros@gmail.com
# Date: 02-02-2026
#
# Description ---
# This script supports a mixed-methods review synthesizing more
# than 100 years (1900–2024) of scientific research on
# Ceriantharia (tube-dwelling anemones). It evaluates temporal
# publication trends, thematic and methodological biases, and
# major knowledge gaps in the literature.
#
# Reproducibility:
# All data processing, analyses, and figure generation were
# performed in R. Scripts are publicly available on GitHub.
#------------------------------------------------------------------------------#

#--------------------------------------------------------------
# Load the packages
#--------------------------------------------------------------
library(revtools)
library(tidyverse)
library(dplyr)
library(tidyr)
library(extrafont)
library(patchwork)
library(countrycode)
library(maps)

# STEP 1. SCREENING THE RAW DATA AND COMPILING THE FINAL DATASET (89).
#
# ================================================================= #
# 01. Dataset operations (screening)
# ================================================================= #
# 01 Load dataset
# ----------------------------------------------------------
raw_data <- read_bibliography("01_data/full_dataset_raw.csv")

# 02 Find duplicates
# ----------------------------------------------------------
duplicates <- find_duplicates(
  raw_data,
  match_variable = "title",
  group_variables = NULL,
  match_function = "exact"
)

# 03 Screen potential duplicates manually
# ----------------------------------------------------------
manual_duplicates <- screen_duplicates(raw_data)

# 04 Remove columns with errors
# ----------------------------------------------------------
raw_data <- raw_data %>%
  select(title, abstract, year, doi_or_id)

# 05 Extract unique data
# ----------------------------------------------------------
unique_refs <- extract_unique_references(raw_data, matches = duplicates)

# 05.1 Export dataset
# ----------------------------------------------------------
write.csv(unique_refs, "01_data/unique_dataset.csv", row.names = FALSE)

# 06 Screen abstracts 
# ----------------------------------------------------------
abstracts_eval <- screen_abstracts(unique_refs)

# ================================================================= #
# 02. Analysis w/ final dataset
# ================================================================= #
# Load the definitive dataset
# ----------------------------------------------------------
dataset <- read.csv("definitive_dataset.csv")

# Load the palette to all analysis
# ----------------------------------------------------------
okabe_ito_palette <- c(
  "#E69F00", 
  "#D6EAF8", 
  "#00BFC4", 
  "#F0E442", 
  "#0072B2", 
  "#B2182B", 
  "#CC79A7", 
  "#542788", 
  "#1B7837", 
  "#999999", 
  "#7FC97F", 
  "#E31A1C", 
  "#FF7F00", 
  "#F4A6C1", 
  "#B8860B", 
  "#D43F8D", 
  "#F4A582",
  "#B15928"
)

# STEP 2. ANALYSES OF THE FINAL DATASET, AFTER COLUNM DEFINITION AND ARTICLES REVIEW.
#
# ==============================================================================
# Figure 2. Publications per year
# ==============================================================================

# 2.1 Arrange the data
# ----------------------------------------------------------
pub_per_year <- dataset %>%
  group_by(year) %>%
  summarise(n = n()) %>%
  arrange(year) %>%
  mutate(cumulative = cumsum(n))  

dataset$year <- as.numeric(dataset$year)

# 2.2 Regression model
# ----------------------------------------------------------
regression_model <- lm(n ~ year, data = pub_per_year)
summary(regression_model)

summary_model <- summary(regression_model)
r2 <- summary_model$r.squared
p_val <- summary_model$coefficients[2,4]

max_n <- max(pub_per_year$n)
max_cum <- max(pub_per_year$cumulative)
scale_factor <- max_n / max_cum

# 2.3 Graph
# ----------------------------------------------------------
temporal_trend <- ggplot(pub_per_year, aes(x = year)) +
  geom_point(aes(y = n), color = "#999999", size = 2) +
  geom_line(aes(y = n), color = "#000000", linewidth = 0.8) +
  geom_smooth(
    aes(y = n),
    method = "lm",
    se = FALSE,
    color = "#D55E00",
    linetype = "dashed",
    linewidth = 1
  ) +
  scale_x_continuous(
    limits = c(1925, 2024),
    breaks = c(seq(1925, 2015, by = 10), 2024),
    expand = c(0, 1)
  ) +
  labs(
    title = NULL,
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_family = "Times New Roman", base_size = 12) +
  annotate(
    "text",
    x = 1926,
    y = max(pub_per_year$n) * 0.95,
    label = paste0(
      "R² = ", round(r2, 2), "\n",
      "p = ", signif(p_val, 3)
    ),
    hjust = 0,
    size = 4,
    family = "Times New Roman"
  ) +
  theme(
    axis.line.x = element_line(color = "black", linewidth = 0.8),
    axis.line.y = element_line(color = "black", linewidth = 0.8),
    panel.grid = element_blank()
  )

# Graph plot
plot(temporal_trend)

# ==============================================================================
# Figure 3. Presence of clear objectives and related areas
# ==============================================================================

# 3.1 Arrange the data
# ----------------------------------------------------------
dataset <- definitive_dataset_new
pub_year_objective <- dataset %>%
  filter(!is.na(objective_code)) %>%
  group_by(year, objective_code) %>%
  summarise(n = n(), .groups = "drop")

# 3.1.1 Levels so as to the plot
objective_levels <- tibble(
  objective_code = 0:8
)

# 3.2 Classification and description of objective codes and year
# ----------------------------------------------------------
objective_freq <- dataset %>%
  filter(!is.na(objective_code)) %>%
  group_by(objective_code) %>%
  summarise(n = n(), .groups = "drop") %>%
  right_join(objective_levels, by = "objective_code") %>%
  mutate(n = ifelse(is.na(n), 0, n)) %>%
  arrange(objective_code)

pub_year_objective <- dataset %>%
  filter(!is.na(year), !is.na(objective_code)) %>%
  mutate(
    year = as.integer(year),
    objective_code = as.integer(objective_code)
  ) %>%
  group_by(year, objective_code) %>%
  summarise(n = n(), .groups = "drop") %>%
  complete(
    year = seq(min(year), max(year), by = 1),
    objective_code = 0:8,
    fill = list(n = 0)
  )

objective_order <- objective_freq %>%
  arrange(desc(n)) %>%
  pull(objective_code)

objective_freq <- objective_freq %>%
  mutate(percent = n / 89 * 100)

# 3.3 Plot results
# ----------------------------------------------------------
obj_code <- ggplot(
  pub_year_objective,
  aes(
    x = year,
    y = n,
    fill = factor(objective_code)
  )
) +
  geom_col(
    width = 0.85,
    color = "black",
    linewidth = 0.1
  ) +
  scale_fill_manual(
    name = "Study objectives",
    values = okabe_ito_palette,
    labels = c(
      "0" = "0. Unknown",
      "1" = "1. Anatomy AND/OR Physiology",
      "2" = "2. Ecology AND/OR Behavior",
      "3" = "3. Methodology AND/OR Protocol",
      "4" = "4. Report AND/OR Species checklist",
      "5" = "5. Morphology AND/OR Histology",
      "6" = "6. Taxonomy AND/OR Phylogeny",
      "7" = "7. Molecular Biology AND/OR\nBioinformatics AND/OR\nBiochemistry",
      "8" = "8. Biogeography"
    )
  ) +  
  scale_x_continuous(
    limits = c(1925, 2024),
    breaks = c(seq(1925, 2015, by = 10), 2024),
    expand = c(0, 1)
  ) +
  labs(
    x = "Year",
    y = "Number of publications"
  ) +
  theme_minimal(base_family = "Times New Roman", base_size = 12) +
  theme(
    axis.line.x = element_line(color = "black", linewidth = 0.8),
    axis.line.y = element_line(color = "black", linewidth = 0.8),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 8),
    legend.text  = element_text(size = 8)
  ) +
  guides(
    fill = guide_legend(
      ncol = 3,
      byrow = TRUE
    )
  )

plot(obj_code)

# ==============================================================================
# Figure 4. Research and focus
# ==============================================================================
# 4.1. Arrange the data
# ----------------------------------------------------------
df_long <- dataset %>%
  separate_rows(research_area, sep = ";") %>%
  separate_rows(research_focus, sep = ";") %>%
  mutate(
    research_area  = str_squish(research_area),
    research_focus = str_squish(research_focus)
  )

total_articles <- n_distinct(dataset$id)

# 4.2 Group the articles by area and focus
# ----------------------------------------------------------
df_plot <- df_long %>%
  filter(
    !is.na(research_area),
    !is.na(research_focus),
    !is.na(research_focus),
    !is.na(research_focus),
    research_area  != "",
    research_focus != ""
  ) %>%
  group_by(research_area, research_focus) %>%
  summarise(
    n = n_distinct(id),   
    .groups = "drop"
  ) %>%
  mutate(
    perc  = n / total_articles,
    label = paste0(round(perc * 100, 1), "%")
  )

df_plot <- df_plot %>%
  mutate(
    research_area = factor(
      research_area,
      levels = sort(unique(as.character(research_area)))
    ),
    research_focus = factor(
      research_focus,
      levels = sort(unique(as.character(research_focus)))
    )
  )

df_plot$research_area <- factor(
  df_plot$research_area,
  levels = rev(levels(df_plot$research_area))
)

# 4.3 Plot the results
# ----------------------------------------------------------
area_focus <- ggplot(
  df_plot,
  aes(x = research_area, y = perc, fill = research_focus)
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.4,
    color = "black",
    linewidth = 0.1
  ) +
  geom_text(
    aes(label = label),
    position = position_dodge(width = 0.8),
    hjust = -0.2,
    size = 2.6,
    family = "Times New Roman",
    fontface = "bold"
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.19))
  ) +
  labs(
    x = NULL,
    y = NULL,
    fill = "Research focus"
  ) +
  scale_fill_manual(
    values = okabe_ito_palette,
    guide = guide_legend(ncol = 5, byrow = TRUE)
  ) +
  theme_minimal(base_family = "Times New Roman", base_size = 8) +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.2),
    panel.grid = element_blank(),
    legend.position = "bottom"
  )

plot(area_focus)

# ==============================================================================
# Analysis 5. Geographical Distribution of Ceriantharia Research
# ==============================================================================
# 5.1 Load the base map
# ----------------------------------------------------------
world <- map_data("world")

# 5.2 Geographical distribution based on affiliations
# ----------------------------------------------------------

# 5.2.1 Expand affiliations by country
# ----------------------------------------------------------
affiliations_df <- dataset %>%
  filter(!is.na(affiliations)) %>%
  separate_rows(affiliations, sep = ";\\s*") %>%
  mutate(affiliations = str_trim(affiliations)) %>%
  distinct(id, affiliations)

total_articles <- n_distinct(dataset$id)

# 5.2.2 Arrange the data
affiliations_freq <- affiliations_df %>%
  group_by(affiliations) %>%
  summarise(n = n_distinct(id), .groups = "drop") %>%
  mutate(percent = n / total_articles * 100) %>%
  arrange(desc(n))


affiliations_freq$affiliations <- countrycode(
  affiliations_freq$affiliations,
  origin = "country.name",
  destination = "country.name",
  warn = TRUE
)

affiliations_freq <- affiliations_freq %>%
  mutate(
    category = case_when(
      n <= 2   ~ "1-2",
      n <= 5   ~ "3-5",
      n <= 10  ~ "6-10",
      n <= 20  ~ "10-20",
      n > 20   ~ "20+"
    )
  )

# 5.2.3 Join with world map
world_affiliations <- left_join(
  world,
  affiliations_freq,
  by = c("region" = "affiliations")
)

world_affiliations$category <- factor(
  world_affiliations$category,
  levels = c("1-2","3-5","6-10","10-20","20+")
)

# 5.2.4. Plot the results
affiliations <- ggplot(world_affiliations, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = category), color = "white") +
  scale_fill_manual(
    values = c(
      "1-2"   = "#D6EAF8",
      "3-5"   = "#00BFC4",
      "6-10"  = "#0072B2",
      "10-20" = "#B2182B",
      "20+"   = "#4B000F"
    ),
    na.value = "grey90"
  ) +
  labs(fill = "Studies", x = NULL, y = NULL) +
  theme_minimal(base_family = "Times New Roman", base_size = 12) +
  theme(legend.position = "bottom")

plot(affiliations)

# 5.3 Based on sampling effort
# ----------------------------------------------------------

# 5.3.1 Arrange the data
locality_freq <- dataset %>%
  filter(!is.na(locality_country)) %>%
  separate_rows(locality_country, sep = ";\\s*") %>%
  mutate(locality_country = str_trim(locality_country)) %>%
  group_by(country = locality_country) %>%
  summarise(n_localities = n(), .groups = "drop")

locality_freq$country <- countrycode(
  locality_freq$country,
  origin = "country.name",
  destination = "country.name",
  warn = TRUE
)

locality_freq <- locality_freq %>%
  filter(!is.na(country)) %>%
  group_by(country) %>%
  summarise(n_localities = sum(n_localities), .groups = "drop") %>%
  mutate(
    category = case_when(
      n_localities <= 2   ~ "1-2",
      n_localities <= 5   ~ "3-5",
      n_localities <= 10  ~ "6-10",
      n_localities <= 20  ~ "10-20",
      n_localities > 20   ~ "20+"
    )
  )


# 5.3.2 Join with world map
world_locality <- left_join(
  world,
  locality_freq,
  by = c("region" = "country")
)

world_locality$category <- factor(
  world_locality$category,
  levels = c("1-2","3-5","6-10","10-20","20+")
)

effort <- ggplot(world_locality, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = category), color = "white") +
  scale_fill_manual(
    values = c(
      "1-2"   = "#D6EAF8",
      "3-5"   = "#00BFC4",
      "6-10"  = "#0072B2",
      "10-20" = "#B2182B",
      "20+"   = "#4B000F"
    ),
    na.value = "grey90"
  ) +
  labs(fill = "Studies (Locality)", x = NULL, y = NULL) +
  theme_minimal(base_family = "Times New Roman", base_size = 12) +
  theme(legend.position = "bottom")

plot(effort)

# 5.4 Combine the maps
combined_geo <- (affiliations + effort) +
  plot_layout(guides = "collect") &   
  theme(legend.position = "bottom")

plot(combined_geo)

# ==============================================================================
# Analysis 6. Environmental and methodological data
# ==============================================================================

# 6.1 Arrange the data
# ----------------------------------------------------------
dataset <- dataset %>%
  mutate(
    id             = as.character(id),
    sampling_method  = str_squish(as.character(sampling_method)),
    study_design = str_squish(as.character(study_design)),
    habitat_code = str_squish(as.character(habitat_code)),
    region_code = str_squish(as.character(region_code))
  )

df_long <- dataset %>%
  separate_rows(region_code, sep = ";") %>%
  separate_rows(habitat_code, sep = ";") %>%
  separate_rows(study_design, sep = ";") %>%
  separate_rows(sampling_method, sep = ";")

total_articles <- n_distinct(dataset$id)

df_plot <- df_long %>%
  filter(
    !is.na(region_code),
    !is.na(habitat_code),
    !is.na(study_design),
    !is.na(sampling_method),
    region_code  != "",
    habitat_code != "",
    study_design != "",
    sampling_method != ""
  ) %>%
  group_by(region_code, habitat_code, study_design, sampling_method) %>%
  summarise(
    n = n_distinct(id),   
    .groups = "drop"
  ) %>%
  mutate(
    perc  = n / total_articles,
    label = paste0(round(perc * 100, 1), "%")
  )

# 6.1.1 Define the levels and the pallete's categories
cat_levels <- c("1-2","3-5","6-10","10-20","20+")

cat_colors <- c(
  "1-2"   = "#D6EAF8",
  "3-5"   = "#00BFC4",
  "6-10"  = "#0072B2",
  "10-20" = "#B2182B",
  "20+"   = "#4B000F"
)


# 6.1.2 Reference table
sampling_ref <- tibble(
  code = 0:7,
  label = c(
    "Unknown","Human diving","Purchased","Sampling gear",
    "Photo AND/OR video records","Dredged",
    "Museum specimens","Online databases"
  )
)

study_design_ref <- tibble(
  code = 0:3,
  label = c(
    "Unknown","Observational and/or descriptive",
    "Empirical and/or experimental","Review"
  )
)

region_ref <- tibble(
  code = 0:4,
  label = c(
    "Unknown","Local","Regional",
    "Global","Geographic coordinates"
  )
)

habitat_ref <- tibble(
  code = 0:7,
  label = c(
    "Unknown","Depth","Substrate","Latitude",
    "Temperature","Microhabitat and/or biotic structure",
    "Hydrodynamics","Salinity"
  )
)

# 6.2 Joint the categories
# ----------------------------------------------------------
prep_freq <- function(data, col, ref_table) {
  data %>%
    mutate({{ col }} := as.numeric({{ col }})) %>%
    count({{ col }}) %>%
    rename(code = {{ col }}) %>%
    left_join(ref_table, by = "code") %>%
    mutate(
      category = case_when(
        n <= 2  ~ "1-2",
        n <= 5  ~ "3-5",
        n <= 10 ~ "6-10",
        n <= 20 ~ "10-20",
        n > 20  ~ "20+"
      ),
      category = factor(category, levels = cat_levels),
      label = factor(label, levels = label[order(n)])
    )
}

sampling_freq <- prep_freq(dataset, sampling_method, sampling_ref)
study_freq    <- prep_freq(dataset, study_design, study_design_ref)
region_freq   <- prep_freq(dataset, region_code, region_ref)
habitat_freq  <- prep_freq(dataset, habitat_code, habitat_ref)

force_colors <- function(df) {
  df %>%
    bind_rows(
      tibble(
        label = levels(df$label)[1],
        n = 0.0001,
        category = factor(cat_levels, levels = cat_levels)
      )
    )
}

sampling_freq <- force_colors(sampling_freq)
study_freq    <- force_colors(study_freq)
region_freq   <- force_colors(region_freq)
habitat_freq  <- force_colors(habitat_freq)

# 6.3
# ----------------------------------------------------------
base_theme <- theme_minimal(
  base_family = "Times New Roman",
  base_size = 10
) +
  theme(
    plot.title = element_text(
      hjust = 0,
      margin = margin(b = 6, l = -10)  # 
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x  = element_text(),
    axis.text.y  = element_text(size = 10),
    axis.ticks   = element_blank()
  )


# 6.4 Each graph
# ----------------------------------------------------------
pA <- ggplot(sampling_freq, aes(x = n, y = label, fill = category)) +
  geom_col() +
  labs(title = "A) Sampling methods") +
  base_theme +
  guides(fill = "none")

pB <- ggplot(study_freq, aes(x = n, y = label, fill = category)) +
  geom_col() +
  labs(title = "B) Study design") +
  base_theme +
  guides(fill = "none")

pC <- ggplot(region_freq, aes(x = n, y = label, fill = category)) +
  geom_col() +
  labs(title = "C) Spatial scale and georeferencing") +
  base_theme +
  guides(fill = "none")

pD <- ggplot(habitat_freq, aes(x = n, y = label, fill = category)) +
  geom_col() +
  labs(title = "D) Environmental variables addressed") +
  base_theme

# 6.5 Plot the results 
# ----------------------------------------------------------
final_panel <- (pA | pB) / (pC | pD) +
  plot_layout(guides = "collect") &
  scale_fill_manual(
    values = cat_colors,
    limits = cat_levels,
    drop = FALSE,
    name = "Studies"
  ) &
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.direction = "horizontal",
    plot.margin = margin(
      t = 5,
      r = 50,   
      b = 5,
      l = 1   
    )
  )

plot(final_panel)

# ==============================================================================
# For save each analysis
# ==============================================================================
ggsave(
  filename = "path/image.png",
  plot = final_panel,
  width = 18,        #cm
  height = 14,       #cm
  units = "cm",
  dpi = 600,         
  bg = "white"
)
