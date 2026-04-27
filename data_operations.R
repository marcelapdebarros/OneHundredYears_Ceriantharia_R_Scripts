#------------------------------------------------------------------------------#
# Analysis workflow ------------------------------------------------------------
# One Hundred Years of Ceriantharia (Cnidaria): A Mixed-Methods Review
# of Research Trends, Biases, and Knowledge Gaps in Tube-dwelling Anemones
# Authors: Marcela Aparecida de Barros, Gustavo R. Brito & Sergio N. Stampar
# E-mail: marcela.barros@unesp.br or marcelapdebarros@gmail.com
# Date: 27-04-2026
#
# Description ---
# This script supports the workflow of a mixed-methods review synthesizing over
# a century (1900–2024) of scientific research on Ceriantharia
# (tube-dwelling anemones), including data processing, analysis, and generation
# of figures presented in the manuscript.
#
#------------------------------------------------------------------------------#

# ------------------------------------------------------------------------------
# Load required packages and ensure reproducibility
# ------------------------------------------------------------------------------
library(revtools)
library(tidyverse)
library(dplyr)
library(tidyr)
library(extrafont)
library(patchwork)
library(countrycode)
library(maps)
library(stringr)
library(maps)
library(ggplot2)
library(forcats)
library(grid)

set.seed(5315)

# ============================================================================ #
# STEP 1. SCREENING THE RAW DATA AND COMPILING THE FINAL DATASET.
# ============================================================================ #

# 01 Load dataset
# ------------------------------------------------------------------------------
raw_data <- read_bibliography("01_data/full_dataset_raw.csv")

# 02 Find duplicates
# ------------------------------------------------------------------------------
duplicates <- find_duplicates(
  raw_data,
  match_variable = "title",
  group_variables = NULL,
  match_function = "exact"
)

# 03 Screen potential duplicates manually
# ------------------------------------------------------------------------------
manual_duplicates <- screen_duplicates(raw_data)

# 04 Remove columns with errors
# ------------------------------------------------------------------------------
raw_data <- raw_data %>%
  select(title, abstract, year, doi_or_id)

# 05 Extract unique data
# ------------------------------------------------------------------------------
unique_refs <- extract_unique_references(raw_data, matches = duplicates)

# 05.1 Export dataset
# ------------------------------------------------------------------------------
write.csv(unique_refs, "01_data/unique_dataset.csv", row.names = FALSE)

# 06 Screen abstracts 
# ------------------------------------------------------------------------------
abstracts_eval <- screen_abstracts(unique_refs)

# ============================================================================ #
# STEP 2. ANALYSIS (FIGURES) OF THE ARTICLE
# ============================================================================ #

# Load the definitive dataset
# ----------------------------------------------------------
dataset <- read.csv("definitive_dataset.csv")

# Arrange data
# ------------------------------------------------------------------------------
box <- function(xmin, xmax, ymin, ymax) {
  annotate(
    "rect",
    xmin = xmin, xmax = xmax,
    ymin = ymin, ymax = ymax,
    fill = "white",
    color = "black",
    linewidth = 0.4
  )
}

label <- function(x, y, text, size = 3.2, face = "plain") {
  annotate(
    "text",
    x = x,
    y = y,
    label = text,
    family = "Arial",
    size = size,
    fontface = face,
    lineheight = 0.95
  )
}

arrow_down <- function(x, y1, y2) {
  annotate(
    "segment",
    x = x,
    xend = x,
    y = y1,
    yend = y2,
    linewidth = 0.4,
    arrow = arrow(length = unit(0.15, "cm"), type = "closed")
  )
}

arrow_right <- function(x1, x2, y) {
  annotate(
    "segment",
    x = x1,
    xend = x2,
    y = y,
    yend = y,
    linewidth = 0.4,
    arrow = arrow(length = unit(0.15, "cm"), type = "closed")
  )
}

bracket <- function(x, y1, y2) {
  list(
    annotate("segment", x = x, xend = x, y = y1, yend = y2, linewidth = 0.4),
    annotate("segment", x = x, xend = x + 0.15, y = y2, yend = y2, linewidth = 0.4),
    annotate("segment", x = x, xend = x + 0.15, y = y1, yend = y1, linewidth = 0.4)
  )
}

# Plot
# ------------------------------------------------------------------------------
fig1 <- ggplot() +
  coord_cartesian(xlim = c(-0.4, 10), ylim = c(0, 10), clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  
  # Titles
  label(3.5, 9.5, "Identification of studies via databases", 3.6, "bold") +
  label(8.0, 9.5, "Records removed", 3.6, "bold") +
  
  # Left labels
  # Left labels
  label(0.25, 8.1, "Identification", 2.8, "bold") +
  label(0.25, 5.2, "Screening", 2.8, "bold") +
  label(0.25, 1.9, "Inclusion", 2.8, "bold") +
  
  # Main boxes
  box(1.3, 5.7, 7.6, 8.6) +
  label(3.5, 8.1, "Web of Science (n = 78)\nScopus (n = 542)") +
  
  box(1.3, 5.7, 6.1, 7.1) +
  label(3.5, 6.6, "Records screened\n(n = 620)") +
  
  box(1.3, 5.7, 4.6, 5.6) +
  label(3.5, 5.1, "Studies assessed for\neligibility* (n = 538)") +
  
  box(1.3, 5.7, 3.1, 4.1) +
  label(3.5, 3.6, "Studies assessed for\neligibility** (n = 96)") +
  
  box(1.3, 5.7, 1.4, 2.4) +
  label(3.5, 1.9, "Final dataset\n(n = 89)") +
  
  # Right boxes
  box(6.3, 9.5, 7.8, 8.6) +
  label(7.9, 8.2, "(n = 38)") +
  
  box(6.3, 9.5, 6.2, 7.1) +
  label(7.9, 6.6, "(n = 82)") +
  
  box(6.3, 9.5, 4.7, 5.6) +
  label(7.9, 5.1, "(n = 44)") +
  
  box(6.3, 9.5, 3.2, 4.1) +
  label(7.9, 3.6, "(n = 7)") +
  
  # Continuous flow arrows between main boxes
  arrow_down(3.5, 7.6, 7.1) +
  arrow_down(3.5, 6.1, 5.6) +
  arrow_down(3.5, 4.6, 4.1) +
  arrow_down(3.5, 3.1, 2.4) +
  
  # Arrows to excluded records
  arrow_right(5.7, 6.3, 8.2) +
  arrow_right(5.7, 6.3, 6.6) +
  arrow_right(5.7, 6.3, 5.1) +
  arrow_right(5.7, 6.3, 3.6) +
  
  # Brackets
  bracket(1.05, 7.6, 8.6) +
  bracket(1.05, 3.1, 7.1) +
  bracket(1.05, 1.4, 2.4)

plot(fig1)


# Save 
# ------------------------------------------------------------------------------
ggsave(
  filename = "new/figures/Figure_1.tiff",
  plot = fig1,
  device = ragg::agg_tiff,
  width = 18,
  height = 12,
  units = "cm",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

# ==============================================================================
# FIGURE 2. PUBLICATIONS PER YEAR
# ==============================================================================

# Arrange data
# ----------------------------------------------------------
dataset$year <- as.numeric(dataset$year)

pub_per_year <- dataset %>%
  group_by(year) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(year) %>%
  mutate(cumulative = cumsum(n))

# Regression model
# ----------------------------------------------------------
regression_model <- lm(n ~ year, data = pub_per_year)
summary(regression_model)

summary_model <- summary(regression_model)
r2 <- summary_model$r.squared
p_val <- summary_model$coefficients[2, 4]

# Graph
# ----------------------------------------------------------
temporal_trend <- ggplot(pub_per_year, aes(x = year)) +
  
  # Annual publication counts as discrete points
  geom_point(
    aes(y = n),
    color = "#000000",
    size = 1.5
  ) +
  
  # Linear regression trend
  geom_smooth(
    aes(y = n),
    method = "lm",
    se = FALSE,
    color = "#E69F00",
    linetype = "dashed",
    linewidth = 0.8
  ) +
  
  scale_x_continuous(
    limits = c(1925, 2024),
    breaks = c(seq(1925, 2015, by = 10), 2024),
    expand = c(0, 1)
  ) +
  
  labs(
    title = NULL,
    x = "Year",
    y = "Number of publications"
  ) +
  
  theme_minimal(
    base_family = "Arial",
    base_size = 11
  ) +
  
  annotate(
    "text",
    x = 1926,
    y = max(pub_per_year$n) * 0.95,
    label = paste0(
      "R² = ", round(r2, 2), "\n",
      "p = ", signif(p_val, 3)
    ),
    hjust = 0,
    size = 3,
    family = "Arial"
  ) +
  
  theme(
    axis.line.x = element_line(color = "black", linewidth = 0.8),
    axis.line.y = element_line(color = "black", linewidth = 0.8),
    panel.grid = element_blank()
  )

# Graph plot
plot(temporal_trend)

# Save
ggsave(
  filename = "new/figures/Figure_2.tiff",
  plot = temporal_trend,
  width = 15,
  height = 10,
  units = "cm",
  dpi = 600,
  compression = "lzw"
)

# ==============================================================================
# FIGURE 3. PRESENCE OF CLEAR OBJECTIVES AND RELATED AREAS
# ==============================================================================

# Arrange data
# ------------------------------------------------------------------------------
dataset$year <- as.numeric(dataset$year)
dataset$objective_code <- as.integer(dataset$objective_code)

objective_levels <- tibble(
  objective_code = 0:8
)

# Frequency of objective codes
# ------------------------------------------------------------------------------
objective_freq <- dataset %>%
  filter(!is.na(objective_code)) %>%
  group_by(objective_code) %>%
  summarise(n = n(), .groups = "drop") %>%
  right_join(objective_levels, by = "objective_code") %>%
  mutate(n = ifelse(is.na(n), 0, n)) %>%
  arrange(objective_code) %>%
  mutate(percent = n / 89 * 100)

# Keep only categories with data
objective_codes_present <- objective_freq %>%
  filter(n > 0) %>%
  pull(objective_code)

# Publications per year and objective code
# ------------------------------------------------------------------------------
pub_year_objective <- dataset %>%
  filter(
    !is.na(year),
    year >= 1925,
    year <= 2024,
    !is.na(objective_code),
    objective_code %in% objective_codes_present
  ) %>%
  mutate(
    year = as.integer(year),
    objective_code = as.integer(objective_code)
  ) %>%
  group_by(year, objective_code) %>%
  summarise(n = n(), .groups = "drop") %>%
  complete(
    year = seq(min(year), max(year), by = 1),
    objective_code = objective_codes_present,
    fill = list(n = 0)
  )

# Labels
# ------------------------------------------------------------------------------
objective_labels <- c(
  "1" = "Anatomy AND/OR Physiology",
  "2" = "Ecology AND/OR Behavior",
  "4" = "Report AND/OR Species checklist",
  "5" = "Morphology AND/OR Histology",
  "6" = "Taxonomy AND/OR Phylogeny",
  "7" = "Molecular Biology AND/OR\nBioinformatics AND/OR\nBiochemistry",
  "8" = "Biogeography"
)

objective_labels_present <- objective_labels[
  as.character(objective_codes_present)
]

# Okabe-Ito palette
# ------------------------------------------------------------------------------
okabe_ito_palette_named <- c(
  "1" = "#E69F00",  
  "2" = "#000080",
  "3" = "#17BECF",
  "4" = "#F0E442",
  "5" = "#0072B2",
  "6" = "#350a06",
  "7" = "#CC79A7",
  "8" = "#542788"
)

objective_colors_present <- okabe_ito_palette_named[
  as.character(objective_codes_present)
]

# Plot
# ------------------------------------------------------------------------------
obj_code <- ggplot(
  pub_year_objective,
  aes(
    x = year,
    y = n,
    fill = as.character(objective_code)
  )
) +
  geom_col(
    width = 0.85,
    color = "black",
    linewidth = 0.1
  ) +
  scale_fill_manual(
    name = "Study objectives",
    values = objective_colors_present,
    breaks = as.character(objective_codes_present),
    labels = objective_labels_present,
    drop = TRUE
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
  theme_minimal(
    base_family = "Arial",
    base_size = 11
  ) +
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

# Plot
# ------------------------------------------------------------------------------
plot(obj_code)

# Save
# ------------------------------------------------------------------------------
ggsave(
  filename = "new/figures/Figure_3.tiff",
  plot = obj_code,
  device = ragg::agg_tiff,
  width = 18,
  height = 12,
  units = "cm",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

# ==============================================================================
# FIGURE 4. RESEARCH AREA AND FOCUS
# ==============================================================================

# Arrange the data
# ------------------------------------------------------------------------------
df_long <- dataset %>%
  separate_rows(research_area, sep = ";") %>%
  separate_rows(research_focus, sep = ";") %>%
  mutate(
    research_area  = str_squish(research_area),
    research_focus = str_squish(research_focus)
  )

total_articles <- n_distinct(dataset$id)

# Group the articles by area and focus
# ------------------------------------------------------------------------------
df_plot <- df_long %>%
  filter(
    !is.na(research_area),
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

# Order factors
# ------------------------------------------------------------------------------
df_plot <- df_plot %>%
  mutate(
    research_area = factor(
      research_area,
      levels = rev(sort(unique(as.character(research_area))))
    ),
    research_focus = factor(
      research_focus,
      levels = sort(unique(as.character(research_focus)))
    )
  )

# Color palette for research focus
# ------------------------------------------------------------------------------
focus_palette <- c(
  "Abiotic Factors" = "#E69F00",
  "Anatomy" = "#000080",
  "Behavior" = "#17BECF",
  "Biodiversity" = "#F0E442",
  "Biogeography" = "#0072B2",
  "Biological Inventory" = "#350a06",
  "Cnida" = "#CC79A7",
  "Conservation" = "#542788",
  "Ecological Interactions" = "#238443",
  "Evolution" = "#999999",
  "Life Cycle" = "#7FC97F",
  "Morphology" = "#E41A1C",
  "Overview" = "#FF7F00",
  "Phylogenetic" = "#F4A6C1",
  "Phylogenomic" = "#B8860B",
  "Phylogeography" = "#CC3D96",
  "Physiology" = "#F4A582",
  "Protein characterization" = "#B15928"
)

# Keep only colors present in the data
# ------------------------------------------------------------------------------
focus_palette_present <- focus_palette[
  names(focus_palette) %in% levels(df_plot$research_focus)
]

# Plot the results
# ------------------------------------------------------------------------------
area_focus <- ggplot(
  df_plot,
  aes(x = research_area, y = perc, fill = research_focus)
) +
  geom_col(
    position = position_dodge(width = 0.9),
    width = 0.85,
    color = NA
  ) +
  geom_text(
    aes(label = ifelse(perc >= 0.02, label, "")),
    position = position_dodge(width = 0.9),
    hjust = -0.08,
    size = 2.4,
    family = "Arial",
    fontface = "bold"
  ) +
  coord_flip(clip = "off") +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 0.20, by = 0.05),
    limits = c(0, 0.21),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    x = NULL,
    y = "Percentage of studies",
    fill = "Research focus"
  ) +
  scale_fill_manual(
    values = focus_palette_present,
    drop = TRUE,
    guide = guide_legend(ncol = 5, byrow = TRUE)
  ) +
  theme_minimal(base_family = "Arial", base_size = 8) +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.2),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title.x = element_text(color = "black", size = 9),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    plot.margin = margin(5, 25, 5, 5)
  )

plot(area_focus)

# Save
# ------------------------------------------------------------------------------
ggsave(
  filename = "new/figures/Figure_4.tiff",
  plot = area_focus,
  device = ragg::agg_tiff,
  width = 18,
  height = 12,
  units = "cm",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

# ==============================================================================
# FIGURE 5. GEOGRAPHICAL DISTRIBUTION OF CERIANTHARIA RESEARCH
# ==============================================================================

# Base map
# ------------------------------------------------------------------------------
world <- map_data("world")

total_articles <- n_distinct(dataset$id)

# Palette and levels
# ------------------------------------------------------------------------------
map_palette <- c(
  "1-2"   = "#D6EAF8",
  "3-5"   = "#56B4E9",
  "6-10"  = "#0072B2",
  "10-20" = "#003F5C",
  "20+"   = "#4B000F"
)

no_data_fill <- "#EAF4E2"
sea_fill     <- "#FFFFFF"

category_levels <- c("1-2","3-5","6-10","10-20","20+","No data")

# AFFILIATIONS
# ==============================================================================
affiliations_df <- dataset %>%
  filter(!is.na(affiliations)) %>%
  separate_rows(affiliations, sep = ";\\s*") %>%
  mutate(affiliations = str_trim(affiliations)) %>%
  distinct(id, affiliations)

affiliations_freq <- affiliations_df %>%
  group_by(affiliations) %>%
  summarise(n = n_distinct(id), .groups = "drop")

affiliations_freq$affiliations <- countrycode(
  affiliations_freq$affiliations,
  origin = "country.name",
  destination = "country.name"
)

affiliations_freq <- affiliations_freq %>%
  filter(!is.na(affiliations)) %>%
  group_by(affiliations) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  mutate(
    category = case_when(
      n <= 2  ~ "1-2",
      n <= 5  ~ "3-5",
      n <= 10 ~ "6-10",
      n <= 20 ~ "10-20",
      n > 20  ~ "20+"
    )
  )

world_affiliations <- left_join(
  world,
  affiliations_freq,
  by = c("region" = "affiliations")
)

world_affiliations$category[is.na(world_affiliations$category)] <- "No data"
world_affiliations$category <- factor(world_affiliations$category, levels = category_levels)

# LOCALITY (SAMPLING EFFORT)
# ==============================================================================
locality_freq <- dataset %>%
  filter(!is.na(locality_country)) %>%
  separate_rows(locality_country, sep = ";\\s*") %>%
  mutate(locality_country = str_trim(locality_country)) %>%
  group_by(country = locality_country) %>%
  summarise(n = n(), .groups = "drop")

locality_freq$country <- countrycode(
  locality_freq$country,
  origin = "country.name",
  destination = "country.name"
)

locality_freq <- locality_freq %>%
  filter(!is.na(country)) %>%
  group_by(country) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  mutate(
    category = case_when(
      n <= 2  ~ "1-2",
      n <= 5  ~ "3-5",
      n <= 10 ~ "6-10",
      n <= 20 ~ "10-20",
      n > 20  ~ "20+"
    )
  )

world_locality <- left_join(
  world,
  locality_freq,
  by = c("region" = "country")
)

world_locality$category[is.na(world_locality$category)] <- "No data"
world_locality$category <- factor(world_locality$category, levels = category_levels)

# Theme map
# ------------------------------------------------------------------------------
map_theme <- theme_minimal(base_family = "Arial", base_size = 11) +
  theme(
    plot.background = element_rect(fill = sea_fill, color = NA),
    panel.background = element_rect(fill = sea_fill, color = NA),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 8),
    plot.title = element_text(face = "bold", size = 10, hjust = 0.5)
  )

map_palette_full <- c(map_palette, "No data" = no_data_fill)

# Plot
# ------------------------------------------------------------------------------
affiliations_map <- ggplot(world_affiliations, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = category), color = "black", linewidth = 0.15) +
  scale_fill_manual(
    values = map_palette_full,
    breaks = category_levels,
    drop = FALSE
  ) +
  coord_quickmap() +
  labs(title = "A. Author affiliations") +
  map_theme +
  theme(legend.position = "none")

effort_map <- ggplot(world_locality, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = category), color = "black", linewidth = 0.15) +
  scale_fill_manual(
    values = map_palette_full,
    breaks = category_levels,
    drop = FALSE
  ) +
  coord_quickmap() +
  labs(title = "B. Sampling localities") +
  map_theme +
  theme(legend.position = "none")

# Manual legend
# ------------------------------------------------------------------------------
legend_df <- data.frame(
  category = factor(category_levels, levels = category_levels),
  x = seq_along(category_levels),
  y = 1
)

manual_legend <- ggplot(legend_df, aes(x = x, y = y)) +
  geom_tile(
    aes(fill = category),
    width = 0.35,
    height = 0.35,
    color = "black",
    linewidth = 0.2
  ) +
  geom_text(
    aes(label = category),
    x = legend_df$x + 0.35,
    y = 1,
    hjust = 0,
    family = "Arial",
    size = 3
  ) +
  scale_fill_manual(values = map_palette_full, drop = FALSE) +
  annotate(
    "text",
    x = 0.2,
    y = 1,
    label = "Number of studies / records",
    hjust = 1,
    family = "Arial",
    fontface = "bold",
    size = 3
  ) +
  coord_cartesian(
    xlim = c(-1.4, length(category_levels) + 1),
    ylim = c(0.7, 1.3),
    clip = "off"
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = margin(0, 0, 0, 0)
  )

# Remove legends
# ------------------------------------------------------------------------------
affiliations_map <- affiliations_map +
  theme(legend.position = "none")

effort_map <- effort_map +
  theme(legend.position = "none")


# Manual legend
# ------------------------------------------------------------------------------
legend_df <- data.frame(
  category = factor(
    c("1-2", "3-5", "6-10", "10-20", "20+", "NA"),
    levels = c("1-2", "3-5", "6-10", "10-20", "20+", "NA")
  ),
  x = 1:6,
  y = 1
)

manual_legend <- ggplot(legend_df, aes(x = x, y = y)) +
  geom_tile(
    aes(fill = category),
    width = 0.25,
    height = 0.25,
    color = "black",
    linewidth = 0.2
  ) +
  geom_text(
    aes(label = category),
    x = legend_df$x + 0.3,
    y = 1,
    hjust = 0,
    size = 3,
    family = "Arial"
  ) +
  scale_fill_manual(
    values = c(
      "1-2"     = "#D6EAF8",
      "3-5"     = "#56B4E9",
      "6-10"    = "#0072B2",
      "10-20"   = "#003F5C",
      "20+"     = "#4B000F",
      "NA" = "#EAF4E2"
    ),
    drop = FALSE
  ) +
  annotate(
    "text",
    x = 0.3,
    y = 1,
    label = "Number of studies / records",
    hjust = 1,
    size = 3,
    family = "Arial",
    fontface = "bold"
  ) +
  coord_cartesian(
    xlim = c(-1.2, 7),
    ylim = c(0.7, 1.3),
    clip = "off"
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = margin(-5, 0, 0, 0)   # 👈 aproxima a legenda
  )


# Combine maps + legend (AJUSTADO)
# ------------------------------------------------------------------------------
combined_geo <- (affiliations_map + effort_map) /
  manual_legend +
  plot_layout(
    heights = c(5, 0.5)   # 👈 diminui espaço entre mapa e legenda
  )

plot(combined_geo)

# Save
# ------------------------------------------------------------------------------
ggsave(
  filename = "new/figures/Figure_5.tiff",
  plot = combined_geo,
  width = 18,
  height = 10,
  units = "cm",
  dpi = 600,
  compression = "lzw",
  bg = sea_fill
)

# ==============================================================================
# FIGURE 6. ENVIRONMENTAL AND METHODOLOGICAL DATA
# ==============================================================================

# Arrange dataset
# ------------------------------------------------------------------------------
dataset_env <- dataset %>%
  mutate(
    id = as.character(id),
    sampling_method = str_squish(as.character(sampling_method)),
    study_design    = str_squish(as.character(study_design)),
    habitat_code    = str_squish(as.character(habitat_code)),
    region_code     = str_squish(as.character(region_code))
  )

total_articles <- n_distinct(dataset_env$id)

# Category levels and colors
# ------------------------------------------------------------------------------
cat_levels <- c("1-2", "3-5", "6-10", "10-20", "20+")

cat_colors <- c(
  "1-2"   = "#E69F00",
  "3-5"   = "#F0E442",
  "6-10"  = "#17BECF",
  "10-20" = "#000080",
  "20+"   = "#350a06"
)

# Reference tables
# ------------------------------------------------------------------------------
sampling_ref <- tibble(
  code = 0:7,
  label = c(
    "Unknown",
    "Human diving",
    "Purchased",
    "Sampling gear",
    "Photo and/or video records",
    "Dredged",
    "Museum specimens",
    "Online databases"
  )
)

study_design_ref <- tibble(
  code = 0:3,
  label = c(
    "Unknown",
    "Observational and/or descriptive",
    "Empirical and/or experimental",
    "Review"
  )
)

region_ref <- tibble(
  code = 0:4,
  label = c(
    "Unknown",
    "Local",
    "Regional",
    "Global",
    "Geographic coordinates"
  )
)

habitat_ref <- tibble(
  code = 0:7,
  label = c(
    "Unknown",
    "Depth",
    "Substrate",
    "Latitude",
    "Temperature",
    "Microhabitat and/or biotic structure",
    "Hydrodynamics",
    "Salinity"
  )
)

# Function to prepare frequencies
# ------------------------------------------------------------------------------
prep_freq <- function(data, column_name, ref_table) {
  
  data %>%
    select(id, all_of(column_name)) %>%
    filter(
      !is.na(.data[[column_name]]),
      .data[[column_name]] != ""
    ) %>%
    separate_rows(all_of(column_name), sep = ";\\s*") %>%
    mutate(
      code = as.numeric(str_squish(.data[[column_name]]))
    ) %>%
    filter(!is.na(code)) %>%
    distinct(id, code) %>%
    count(code, name = "n") %>%
    right_join(ref_table, by = "code") %>%
    mutate(
      n = ifelse(is.na(n), 0, n),
      perc = n / total_articles,
      category = case_when(
        n == 0  ~ NA_character_,
        n <= 2  ~ "1-2",
        n <= 5  ~ "3-5",
        n <= 10 ~ "6-10",
        n <= 20 ~ "10-20",
        n > 20  ~ "20+"
      ),
      category = factor(category, levels = cat_levels),
      label = fct_reorder(label, n)
    )
}

sampling_freq <- prep_freq(dataset_env, "sampling_method", sampling_ref)
study_freq <- study_freq %>%
  filter(code != 0) %>%
  mutate(label = fct_reorder(label, n))
region_freq   <- prep_freq(dataset_env, "region_code", region_ref)
habitat_freq  <- prep_freq(dataset_env, "habitat_code", habitat_ref)

# Theme
# ------------------------------------------------------------------------------
base_theme <- theme_minimal(
  base_family = "Arial",
  base_size = 10
) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 10,
      hjust = 0,
      margin = margin(b = 5)
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey85", linewidth = 0.25),
    axis.title.x = element_text(size = 9, color = "black", margin = margin(t = 5)),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 8, color = "black"),
    axis.text.y = element_text(size = 9, color = "black"),
    axis.line.x = element_line(color = "black", linewidth = 0.3),
    axis.ticks.x = element_line(color = "black", linewidth = 0.25),
    legend.position = "none"
  )

# Function to plot each panel
# ------------------------------------------------------------------------------
make_panel <- function(df, title_text) {
  
  ggplot(df, aes(x = n, y = label, fill = category)) +
    geom_col(
      width = 0.65,
      color = NA,
      na.rm = TRUE
    ) +
    geom_text(
      aes(label = ifelse(n > 0, n, "")),
      hjust = -0.25,
      size = 2.8,
      family = "Arial",
      fontface = "bold",
      color = "black"
    ) +
    scale_fill_manual(
      values = cat_colors,
      limits = cat_levels,
      drop = FALSE,
      na.value = "transparent"
    ) +
    scale_x_continuous(
      expand = expansion(mult = c(0, 0.20))
    ) +
    labs(
      title = title_text,
      x = "Number of studies",
      y = NULL
    ) +
    base_theme
}

# Build panels
# ------------------------------------------------------------------------------
pA <- make_panel(sampling_freq, "A. Sampling methods")
pB <- make_panel(study_freq, "B. Study design")
pC <- make_panel(region_freq, "C. Spatial scale and georeferencing")
pD <- make_panel(habitat_freq, "D. Environmental variables addressed")

main_panel <- (pA + pB) / (pC + pD)

# Manual legend
# ------------------------------------------------------------------------------
manual_legend_df <- data.frame(
  category = factor(cat_levels, levels = cat_levels),
  x = seq_along(cat_levels),
  y = 1
)

manual_legend <- ggplot(manual_legend_df, aes(x = x, y = y)) +
  geom_tile(
    aes(fill = category),
    width = 0.28,
    height = 0.28,
    color = "black",
    linewidth = 0.15
  ) +
  geom_text(
    aes(label = category),
    x = manual_legend_df$x + 0.28,
    y = 1,
    hjust = 0,
    size = 3,
    family = "Arial",
    color = "black"
  ) +
  scale_fill_manual(
    values = cat_colors,
    limits = cat_levels,
    drop = FALSE
  ) +
  annotate(
    "text",
    x = 0.35,
    y = 1,
    label = "Number of studies",
    hjust = 1,
    size = 3,
    family = "Arial",
    fontface = "bold",
    color = "black"
  ) +
  coord_cartesian(
    xlim = c(-1.1, length(cat_levels) + 1),
    ylim = c(0.75, 1.25),
    clip = "off"
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = margin(-8, 0, 0, 0)
  )

# Combine correctly
# ------------------------------------------------------------------------------
final_panel <- wrap_plots(
  main_panel,
  manual_legend,
  ncol = 1,
  heights = c(10, 0.6)
)

plot(final_panel)

# Save
# ------------------------------------------------------------------------------
ggsave(
  filename = "new/figures/Figure_6.tiff",
  plot = final_panel,
  width = 18,
  height = 14,
  units = "cm",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)