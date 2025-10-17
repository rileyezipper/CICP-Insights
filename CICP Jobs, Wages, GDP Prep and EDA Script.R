# ============================================================================
# CICP Advanced Industries Dashboard - Exploratory Data Analysis
# ============================================================================
# Purpose: Automated EDA and insights generation for CICP data refreshes
# Author: [Your Name]
# Last Updated: 2025-10-07
# ============================================================================

# SETUP -----------------------------------------------------------------------

# Load required packages
library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)

# Set data directory (MODIFY THIS FOR EACH DATA RUN)
data_dir <- "CICP_20250709"
output_dir <- paste0("outputs_", gsub("CICP_", "", data_dir))

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Set professional plot theme
theme_set(theme_minimal(base_size = 13, base_family = "sans") +
            theme(
              # Text elements
              plot.title = element_text(face = "bold", size = 16, 
                                       margin = margin(b = 8)),
              plot.subtitle = element_text(size = 12, color = "gray30", 
                                          margin = margin(b = 12)),
              plot.caption = element_text(size = 9, color = "gray50", 
                                         hjust = 0, margin = margin(t = 12)),
              
              # Axis elements
              axis.title = element_text(size = 11, face = "bold"),
              axis.title.x = element_text(margin = margin(t = 10)),
              axis.title.y = element_text(margin = margin(r = 10)),
              axis.text = element_text(size = 10, color = "gray20"),
              
              # Grid and panel
              panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "white", color = NA),
              plot.background = element_rect(fill = "white", color = NA),
              
              # Legend
              legend.position = "bottom",
              legend.title = element_text(size = 10, face = "bold"),
              legend.text = element_text(size = 9),
              legend.key.size = unit(0.8, "lines"),
              
              # Margins
              plot.margin = margin(15, 15, 15, 15)
            ))

# Define color palette for initiatives
initiative_colors <- c(
  "Advanced & Traded Industries" = "#1565C0",
  "AgriNovus" = "#2E7D32",
  "BioCrossroads" = "#6A1B9A",
  "Conexus" = "#D84315",
  "TechPoint" = "#F57C00",
  "Total Employment" = "#424242"
)

# HELPER FUNCTIONS ------------------------------------------------------------

# Function to read and standardize data files
read_cicp_data <- function(file_path, data_type) {
  df <- read_csv(file_path, show_col_types = FALSE)
  
  # Standardize column names
  df <- df %>%
    rename_with(tolower) %>%
    mutate(data_type = data_type,
           file_name = basename(file_path))
  
  # Handle NAICS column name variations
  if ("naics" %in% names(df)) {
    df <- df %>% rename(naics_code = naics)
  }
  
  return(df)
}

# Function to calculate growth rates
calculate_growth <- function(df, value_col, group_vars) {
  df %>%
    arrange(!!!syms(group_vars), year) %>%
    group_by(!!!syms(group_vars)) %>%
    mutate(
      yoy_growth = ({{value_col}} / lag({{value_col}}) - 1) * 100,
      yoy_change = {{value_col}} - lag({{value_col}}),
      cagr_3yr = if_else(
        n() >= 3 & !is.na({{value_col}}) & !is.na(lag({{value_col}}, 2)),
        ({{value_col}} / lag({{value_col}}, 2))^(1/2) - 1,
        NA_real_
      ) * 100,
      cagr_5yr = if_else(
        n() >= 5 & !is.na({{value_col}}) & !is.na(lag({{value_col}}, 4)),
        ({{value_col}} / lag({{value_col}}, 4))^(1/4) - 1,
        NA_real_
      ) * 100
    ) %>%
    ungroup()
}

# Function to identify outliers using IQR method
identify_outliers <- function(df, metric_col, group_var = NULL) {
  if (!is.null(group_var)) {
    df %>%
      group_by({{group_var}}) %>%
      mutate(
        q1 = quantile({{metric_col}}, 0.25, na.rm = TRUE),
        q3 = quantile({{metric_col}}, 0.75, na.rm = TRUE),
        iqr = q3 - q1,
        is_outlier = {{metric_col}} < (q1 - 1.5 * iqr) | 
                     {{metric_col}} > (q3 + 1.5 * iqr)
      ) %>%
      ungroup()
  } else {
    df %>%
      mutate(
        q1 = quantile({{metric_col}}, 0.25, na.rm = TRUE),
        q3 = quantile({{metric_col}}, 0.75, na.rm = TRUE),
        iqr = q3 - q1,
        is_outlier = {{metric_col}} < (q1 - 1.5 * iqr) | 
                     {{metric_col}} > (q3 + 1.5 * iqr)
      )
  }
}

# DATA LOADING ----------------------------------------------------------------

cat("Loading data files...\n")

# Get all relevant file paths
jobs_files <- list.files(data_dir, pattern = "industry_jobs.*\\.csv$", 
                         full.names = TRUE)
wage_files <- list.files(data_dir, pattern = "industry_wage.*\\.csv$", 
                         full.names = TRUE)
gdp_files <- list.files(data_dir, pattern = "gdp_LC.*\\.csv$", 
                        full.names = TRUE)

# Read and combine data
jobs_data <- map_dfr(jobs_files, ~read_cicp_data(.x, "jobs"))
wage_data <- map_dfr(wage_files, ~read_cicp_data(.x, "wages"))
gdp_data <- map_dfr(gdp_files, ~read_cicp_data(.x, "gdp"))

# Extract initiative names from file names
extract_initiative <- function(filename) {
  str_extract(filename, "(?<=_)[A-Za-z]+(?=\\.csv)") %>%
    str_to_title() %>%
    str_replace("Advind", "Advanced & Traded Industries") %>%
    str_replace("Agrinovus", "AgriNovus") %>%
    str_replace("Biox", "BioCrossroads") %>%
    str_replace("Conexus", "Conexus") %>%
    str_replace("Techpoint", "TechPoint") %>%
    str_replace("Totals", "Total Employment")
}

jobs_data <- jobs_data %>% 
  mutate(initiative = extract_initiative(file_name))
wage_data <- wage_data %>% 
  mutate(initiative = extract_initiative(file_name))
gdp_data <- gdp_data %>% 
  mutate(initiative = extract_initiative(file_name))

cat(sprintf("Loaded %d jobs records, %d wage records, %d GDP records\n", 
            nrow(jobs_data), nrow(wage_data), nrow(gdp_data)))

# DATA PREPARATION ------------------------------------------------------------

cat("Preparing data for analysis...\n")

# Calculate growth rates for jobs
jobs_growth <- jobs_data %>%
  filter(!is.na(jobs)) %>%
  calculate_growth(jobs, c("initiative", "geo_area", "naics_title", "display_level"))

# Calculate growth rates for wages
wage_growth <- wage_data %>%
  filter(!is.na(wages)) %>%
  calculate_growth(wages, c("initiative", "geo_area", "naics_title", "display_level"))

# Calculate growth rates for GDP
gdp_growth <- gdp_data %>%
  filter(!is.na(grp)) %>%
  calculate_growth(grp, c("initiative", "geo_area", "naics_title", "display_level")) %>%
  calculate_growth(jobs, c("initiative", "geo_area", "naics_title", "display_level"))

# Identify geography types
jobs_data <- jobs_data %>%
  mutate(geo_type = case_when(
    geo_area == "United States" ~ "US",
    geo_area == "Indiana" ~ "State",
    TRUE ~ "Metro"
  ))

wage_data <- wage_data %>%
  mutate(geo_type = case_when(
    geo_area == "United States" ~ "US",
    geo_area == "Indiana" ~ "State",
    TRUE ~ "Metro"
  ))

gdp_data <- gdp_data %>%
  mutate(geo_type = case_when(
    geo_area == "United States" ~ "US",
    geo_area == "Indiana" ~ "State",
    TRUE ~ "Metro"
  ))

jobs_growth <- jobs_growth %>%
  mutate(geo_type = case_when(
    geo_area == "United States" ~ "US",
    geo_area == "Indiana" ~ "State",
    TRUE ~ "Metro"
  ))

wage_growth <- wage_growth %>%
  mutate(geo_type = case_when(
    geo_area == "United States" ~ "US",
    geo_area == "Indiana" ~ "State",
    TRUE ~ "Metro"
  ))

gdp_growth <- gdp_growth %>%
  mutate(geo_type = case_when(
    geo_area == "United States" ~ "US",
    geo_area == "Indiana" ~ "State",
    TRUE ~ "Metro"
  ))

# ANALYSIS 1: INITIATIVE-LEVEL OVERVIEW --------------------------------------

cat("\n=== ANALYSIS 1: Initiative-Level Overview ===\n")

# ANALYSIS SETUP - Define most recent years for each dataset
# Use 2023 as the most recent year with complete data across all metrics
recent_year <- 2023

cat(sprintf("Using %d as most recent year (last year with complete data)\n", recent_year))

# Indiana-specific summary by initiative (excluding Total Employment)
indiana_initiative_summary <- jobs_data %>%
  filter(display_level == 0, 
         year == recent_year, 
         geo_area == "Indiana",
         initiative != "Total Employment") %>%
  left_join(
    wage_data %>% 
      filter(display_level == 0, year == recent_year, geo_area == "Indiana") %>%
      select(initiative, wages),
    by = "initiative"
  ) %>%
  left_join(
    gdp_data %>% 
      filter(display_level == 0, year == recent_year, geo_area == "Indiana") %>%
      select(initiative, grp),
    by = "initiative"
  ) %>%
  select(initiative, jobs, wages, grp) %>%
  arrange(desc(jobs))

print(indiana_initiative_summary)

# Metro-level summary (top metros by total advanced industry employment)
metro_summary <- jobs_data %>%
  filter(display_level == 0, 
         year == recent_year, 
         geo_type == "Metro",
         initiative != "Total Employment") %>%
  group_by(geo_area) %>%
  summarise(
    total_jobs = sum(jobs, na.rm = TRUE),
    n_initiatives = n_distinct(initiative[jobs > 0]),
    .groups = "drop"
  ) %>%
  arrange(desc(total_jobs)) %>%
  head(10)

print(metro_summary)

# ANALYSIS 2: GROWTH RATE ANALYSIS -------------------------------------------

cat("\n=== ANALYSIS 2: Growth Rate Analysis ===\n")

# Jobs growth by initiative in Indiana
indiana_jobs_growth <- jobs_growth %>%
  filter(display_level == 0, 
         year == recent_year,
         geo_area == "Indiana",
         initiative != "Total Employment",
         !is.na(cagr_3yr)) %>%
  select(initiative, cagr_3yr, yoy_growth, jobs) %>%
  arrange(desc(cagr_3yr))

print(indiana_jobs_growth)

# Sector-level growth contributors by geography
sector_growth_contrib <- jobs_growth %>%
  filter(display_level == 1, 
         year == recent_year,
         geo_area == "Indiana",
         !is.na(yoy_change)) %>%
  group_by(initiative, sector_name) %>%
  summarise(
    job_change = sum(yoy_change, na.rm = TRUE),
    growth_rate = weighted.mean(yoy_growth, lag(jobs), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(initiative, desc(abs(job_change))) %>%
  group_by(initiative) %>%
  slice_max(abs(job_change), n = 3) %>%
  ungroup()

print(sector_growth_contrib)

# ANALYSIS 3: GEOGRAPHIC ANALYSIS --------------------------------------------

cat("\n=== ANALYSIS 3: Geographic Analysis ===\n")

# Growth by metro (for metros only)
metro_growth <- jobs_growth %>%
  filter(display_level == 0, 
         year == recent_year,
         geo_type == "Metro",
         initiative != "Total Employment",
         !is.na(yoy_growth)) %>%
  group_by(geo_area) %>%
  summarise(
    total_jobs = sum(jobs, na.rm = TRUE),
    avg_growth = weighted.mean(yoy_growth, lag(jobs), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_jobs))

print(head(metro_growth, 10))

# Geographic outliers in growth (metros only)
geo_outliers <- jobs_growth %>%
  filter(display_level == 0,
         year == recent_year,
         geo_type == "Metro",
         initiative != "Total Employment",
         !is.na(yoy_growth)) %>%
  identify_outliers(yoy_growth) %>%
  filter(is_outlier) %>%
  select(initiative, geo_area, jobs, yoy_growth, yoy_change) %>%
  arrange(desc(abs(yoy_growth)))

if (nrow(geo_outliers) > 0) {
  cat("\nGeographic Outliers (High Growth/Decline):\n")
  print(geo_outliers)
}

# ANALYSIS 4: INDUSTRY DEEP DIVE ---------------------------------------------

cat("\n=== ANALYSIS 4: Industry Analysis ===\n")

# Top growing industries in Indiana
top_growing_industries_in <- jobs_growth %>%
  filter(display_level >= 2,
         year == recent_year,
         geo_area == "Indiana",
         !is.na(yoy_change)) %>%
  group_by(initiative, naics_title) %>%
  summarise(
    job_change = sum(yoy_change, na.rm = TRUE),
    growth_rate = weighted.mean(yoy_growth, lag(jobs), na.rm = TRUE),
    current_jobs = sum(jobs, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(job_change)) %>%
  head(20)

print(top_growing_industries_in)

# Fastest growing industries (by percentage) in Indiana
fastest_growing_industries_in <- jobs_growth %>%
  filter(display_level >= 2,
         year == recent_year,
         geo_area == "Indiana",
         lag(jobs) >= 50,  # Minimum base for meaningful percentages
         !is.na(yoy_growth)) %>%
  group_by(initiative, naics_title) %>%
  summarise(
    growth_rate = weighted.mean(yoy_growth, lag(jobs), na.rm = TRUE),
    base_jobs = sum(lag(jobs), na.rm = TRUE),
    current_jobs = sum(jobs, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(growth_rate)) %>%
  head(20)

print(fastest_growing_industries_in)

# ANALYSIS 5: WAGE ANALYSIS --------------------------------------------------

cat("\n=== ANALYSIS 5: Wage Analysis ===\n")

# Wage comparison by initiative in Indiana
indiana_wage_comparison <- wage_data %>%
  filter(display_level == 0, 
         year == recent_year, 
         geo_area == "Indiana",
         initiative != "Total Employment",
         !is.na(wages)) %>%
  select(initiative, wages) %>%
  arrange(desc(wages))

print(indiana_wage_comparison)

# High-wage industries in Indiana
high_wage_industries_in <- wage_data %>%
  filter(display_level >= 2,
         year == recent_year,
         geo_area == "Indiana",
         !is.na(wages)) %>%
  left_join(
    jobs_data %>%
      filter(display_level >= 2, year == recent_year, geo_area == "Indiana") %>%
      select(initiative, naics_title, jobs),
    by = c("initiative", "naics_title")
  ) %>%
  filter(jobs >= 50) %>%  # Minimum threshold
  select(initiative, naics_title, wages, jobs) %>%
  arrange(desc(wages)) %>%
  head(20)

print(high_wage_industries_in)

# ANALYSIS 6: ECONOMIC IMPACT METRICS ----------------------------------------

cat("\n=== ANALYSIS 6: Economic Impact Analysis ===\n")

# GDP per job (productivity metric) in Indiana
indiana_gdp_per_job <- gdp_data %>%
  filter(display_level == 0, 
         year == recent_year, 
         geo_area == "Indiana",
         initiative != "Total Employment") %>%
  mutate(gdp_per_job = grp / jobs) %>%
  select(initiative, gdp_per_job, grp, jobs) %>%
  arrange(desc(gdp_per_job))

print(indiana_gdp_per_job)

# GDP growth vs Jobs growth comparison in Indiana
indiana_gdp_jobs_comparison <- gdp_growth %>%
  filter(display_level == 0, 
         year == recent_year, 
         geo_area == "Indiana",
         initiative != "Total Employment") %>%
  mutate(
    gdp_growth_rate = yoy_growth,
    jobs_growth_rate = (jobs / lag(jobs) - 1) * 100,
    productivity_change = gdp_growth_rate - jobs_growth_rate
  ) %>%
  select(initiative, gdp_growth_rate, jobs_growth_rate, productivity_change) %>%
  arrange(desc(productivity_change))

print(indiana_gdp_jobs_comparison)

# EXPORT KEY TABLES ----------------------------------------------------------

cat("\n=== Exporting Results ===\n")

# Export summary tables
write_csv(indiana_initiative_summary, 
          file.path(output_dir, "indiana_initiative_summary.csv"))
write_csv(indiana_jobs_growth, 
          file.path(output_dir, "indiana_jobs_growth.csv"))
write_csv(metro_summary, 
          file.path(output_dir, "top_metros.csv"))
write_csv(metro_growth, 
          file.path(output_dir, "metro_growth_rates.csv"))
write_csv(sector_growth_contrib, 
          file.path(output_dir, "sector_growth_contributors.csv"))
write_csv(top_growing_industries_in, 
          file.path(output_dir, "top_growing_industries_indiana.csv"))
write_csv(high_wage_industries_in, 
          file.path(output_dir, "high_wage_industries_indiana.csv"))
write_csv(indiana_gdp_jobs_comparison, 
          file.path(output_dir, "indiana_gdp_jobs_comparison.csv"))

# SAVE PROCESSED DATA FOR VISUALIZATION SCRIPT -------------------------------

cat("\n=== Saving processed data for visualization script ===\n")

save(jobs_data, wage_data, gdp_data, 
     jobs_growth, wage_growth, gdp_growth,
     recent_year, initiative_colors,
     file = file.path(output_dir, "processed_data_jobs_gdp.RData"))

cat(sprintf("Processed data saved to '%s/processed_data_jobs_gdp.RData'\n", output_dir))

# SUMMARY INSIGHTS REPORT ----------------------------------------------------

cat("\n" , rep("=", 70), "\n", sep = "")
cat("EXECUTIVE SUMMARY - KEY INSIGHTS (INDIANA)\n")
cat(rep("=", 70), "\n\n", sep = "")

cat("1. INITIATIVE PERFORMANCE IN INDIANA\n")
cat(sprintf("   - Total advanced industry jobs: %s\n", 
            comma(sum(indiana_initiative_summary$jobs, na.rm = TRUE))))
cat(sprintf("   - Highest employment: %s (%s jobs)\n",
            indiana_initiative_summary$initiative[1],
            comma(indiana_initiative_summary$jobs[1])))
cat(sprintf("   - Highest average wage: %s ($%s)\n",
            indiana_wage_comparison$initiative[1],
            comma(round(indiana_wage_comparison$wages[1]))))

cat("\n2. GROWTH TRENDS IN INDIANA\n")
if (nrow(indiana_jobs_growth) > 0) {
  cat(sprintf("   - Fastest growing initiative: %s (%.1f%% CAGR)\n",
              indiana_jobs_growth$initiative[1],
              indiana_jobs_growth$cagr_3yr[1]))
  if (any(indiana_jobs_growth$cagr_3yr < 0)) {
    declining <- indiana_jobs_growth %>% filter(cagr_3yr < 0)
    cat(sprintf("   - Declining initiatives: %d\n", nrow(declining)))
  }
}

cat("\n3. GEOGRAPHIC CONCENTRATION\n")
cat(sprintf("   - Top metro: %s (%s jobs)\n",
            metro_summary$geo_area[1],
            comma(metro_summary$total_jobs[1])))
cat(sprintf("   - Number of active metros: %d\n", nrow(metro_summary)))
if (nrow(geo_outliers) > 0) {
  cat(sprintf("   - Geographic outliers identified: %d metros\n", 
              nrow(geo_outliers)))
}

cat("\n4. INDUSTRY DYNAMICS IN INDIANA\n")
if (nrow(top_growing_industries_in) > 0) {
  cat(sprintf("   - Top growing industry: %s (+%s jobs)\n",
              top_growing_industries_in$naics_title[1],
              comma(round(top_growing_industries_in$job_change[1]))))
}
if (nrow(high_wage_industries_in) > 0) {
  cat(sprintf("   - Highest wage industry: %s ($%s)\n",
              high_wage_industries_in$naics_title[1],
              comma(round(high_wage_industries_in$wages[1]))))
}

cat("\n", rep("=", 70), "\n", sep = "")

# ============================================================================
# INITIATIVE DEEP DIVE - INDUSTRY-LEVEL ANALYSIS
# ============================================================================
# Add this section after your existing analyses

cat("\n" , rep("=", 70), "\n", sep = "")
cat("INITIATIVE DEEP DIVE - INDUSTRY-LEVEL ANALYSIS\n")
cat(rep("=", 70), "\n\n", sep = "")

# ANALYSIS 7: TOP INDUSTRIES WITHIN EACH INITIATIVE --------------------------

cat("\n=== ANALYSIS 7: Top Industries Within Each Initiative ===\n")

# Top industries by employment within each initiative (all geographies)
top_industries_by_initiative <- jobs_data %>%
  filter(display_level >= 2,  # Industry detail level
         year == recent_year,
         initiative != "Total Employment",
         naics_code != "000000") %>%
  group_by(initiative, statefips, countyfips, metrofips, geo_area, year) %>%
  arrange(initiative, statefips, countyfips, geo_area, year, desc(jobs)) %>%
  slice_head(n = 10) %>%
  ungroup() %>%
  select(statefips, countyfips, metrofips, display_level, geo_area, year, initiative, 
         naics_title, naics_code, jobs) %>%
  arrange(statefips, countyfips, metrofips, geo_area, year, initiative, desc(jobs))

print(top_industries_by_initiative)

# Calculate concentration metrics by initiative and geography
initiative_concentration <- jobs_data %>%
  filter(display_level >= 2,
         year == recent_year,
         initiative != "Total Employment",
         naics_code != "000000") %>%
  group_by(initiative, statefips, countyfips, metrofips, geo_area, year) %>%
  mutate(
    total_init_jobs = sum(jobs, na.rm = TRUE),
    industry_share = jobs / total_init_jobs * 100
  ) %>%
  arrange(desc(jobs)) %>%
  mutate(
    cumulative_share = cumsum(industry_share),
    industry_rank = row_number()
  ) %>%
  summarise(
    total_industries = n(),
    top5_concentration = sum(industry_share[industry_rank <= 5], na.rm = TRUE),
    top10_concentration = sum(industry_share[industry_rank <= 10], na.rm = TRUE),
    herfindahl_index = sum(industry_share^2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  select(statefips, countyfips, metrofips, geo_area, year, initiative,
         total_industries, top5_concentration, top10_concentration, 
         herfindahl_index) %>%
  arrange(statefips, countyfips, metrofips, geo_area, year, initiative)

cat("\nInitiative Concentration Metrics:\n")
print(initiative_concentration)

# ANALYSIS 8: INDUSTRY GROWTH CONTRIBUTORS ------------------------------------

cat("\n=== ANALYSIS 8: Industry Growth Contributors by Initiative ===\n")

# Industries contributing most to growth/decline within each initiative
industry_growth_contrib <- jobs_growth %>%
  filter(display_level >= 2,
         year == recent_year,
         initiative != "Total Employment",
         naics_code != "000000",
         !is.na(yoy_change)) %>%
  group_by(initiative, statefips, countyfips, metrofips, geo_area, year) %>%
  mutate(
    total_init_change = sum(yoy_change, na.rm = TRUE),
    contribution_pct = (yoy_change / abs(total_init_change)) * 100
  ) %>%
  arrange(desc(abs(yoy_change))) %>%
  slice_head(n = 15) %>%
  ungroup() %>%
  select(statefips, countyfips, metrofips, geo_area, year, initiative,
         naics_title, naics_code, jobs, yoy_change, yoy_growth, 
         contribution_pct) %>%
  arrange(statefips, countyfips, metrofips, geo_area, year, initiative, 
          desc(yoy_change))

print(industry_growth_contrib)

# ANALYSIS 9: INDUSTRY WAGE DISTRIBUTION -------------------------------------

cat("\n=== ANALYSIS 9: Industry Wage Distribution by Initiative ===\n")

# Wage distribution within initiatives by geography
industry_wage_dist <- wage_data %>%
  filter(display_level >= 2,
         year == recent_year,
         initiative != "Total Employment",
         naics_code != "000000",
         !is.na(wages)) %>%
  # Remove any duplicate rows before joining
  distinct(statefips, countyfips, metrofips, geo_area, initiative, 
           naics_code, naics_title, year, .keep_all = TRUE) %>%
  left_join(
    jobs_data %>%
      filter(display_level >= 2, year == recent_year, 
             naics_code != "000000") %>%
      # Remove any duplicate rows before joining
      distinct(statefips, countyfips, metrofips, geo_area, initiative, 
               naics_code, naics_title, .keep_all = TRUE) %>%
      select(statefips, countyfips, metrofips, geo_area, initiative, 
             naics_code, naics_title, jobs),
    by = c("statefips", "countyfips", "metrofips", "geo_area", 
           "initiative", "naics_code", "naics_title")
  ) %>%
  group_by(statefips, countyfips, metrofips, geo_area, year, initiative) %>%
  summarise(
    n_industries = n(),
    min_wage = min(wages, na.rm = TRUE),
    q25_wage = quantile(wages, 0.25, na.rm = TRUE),
    median_wage = median(wages, na.rm = TRUE),
    q75_wage = quantile(wages, 0.75, na.rm = TRUE),
    max_wage = max(wages, na.rm = TRUE),
    weighted_avg_wage = weighted.mean(wages, jobs, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  select(statefips, countyfips, metrofips, geo_area, year, initiative,
         n_industries, min_wage, q25_wage, median_wage, q75_wage, 
         max_wage, weighted_avg_wage) %>%
  arrange(statefips, countyfips, metrofips, geo_area, year, initiative)

cat("\nWage Distribution by Initiative and Geography:\n")
print(industry_wage_dist)

# High-wage vs low-wage industries within initiatives
wage_extremes_by_init <- wage_data %>%
  filter(display_level >= 2,
         year == recent_year,
         initiative != "Total Employment",
         naics_code != "000000",
         !is.na(wages)) %>%
  # Remove any duplicate rows before joining
  distinct(statefips, countyfips, metrofips, geo_area, initiative, 
           naics_code, naics_title, year, .keep_all = TRUE) %>%
  left_join(
    jobs_data %>%
      filter(display_level >= 2, year == recent_year,
             naics_code != "000000") %>%
      # Remove any duplicate rows before joining
      distinct(statefips, countyfips, metrofips, geo_area, initiative, 
               naics_code, naics_title, .keep_all = TRUE) %>%
      select(statefips, countyfips, metrofips, geo_area, initiative, 
             naics_code, naics_title, jobs),
    by = c("statefips", "countyfips", "metrofips", "geo_area", 
           "initiative", "naics_code", "naics_title")
  ) %>%
  filter(jobs >= 50) %>%  # Minimum threshold
  group_by(initiative, statefips, countyfips, metrofips, geo_area, year) %>%
  filter(n() >= 6) %>%  # Only include groups with at least 6 industries
  arrange(desc(wages)) %>%
  slice(c(1:3, (n()-2):n())) %>%
  mutate(category = ifelse(row_number() <= 3, "Highest Wage", "Lowest Wage")) %>%
  ungroup() %>%
  select(statefips, countyfips, metrofips, geo_area, year, initiative, 
         category, naics_title, naics_code, wages, jobs) %>%
  arrange(statefips, countyfips, metrofips, geo_area, year, initiative, 
          desc(wages))

print(wage_extremes_by_init)

# ANALYSIS 10: INDUSTRY PRODUCTIVITY METRICS ----------------------------------

cat("\n=== ANALYSIS 10: Industry Productivity by Initiative ===\n")

# GDP per job by industry within initiatives
industry_productivity <- gdp_data %>%
  filter(display_level >= 2,
         year == recent_year,
         initiative != "Total Employment",
         naics_code != "000000",
         !is.na(grp), !is.na(jobs)) %>%
  mutate(gdp_per_job = grp / jobs) %>%
  group_by(initiative, statefips, countyfips, metrofips, geo_area, year) %>%
  arrange(desc(gdp_per_job)) %>%
  slice_head(n = 10) %>%
  ungroup() %>%
  select(statefips, countyfips, metrofips, geo_area, year, initiative,
         naics_title, naics_code, jobs, grp, gdp_per_job) %>%
  arrange(statefips, countyfips, metrofips, geo_area, year, initiative, 
          desc(gdp_per_job))

cat("\nTop Productivity Industries by Initiative and Geography:\n")
print(industry_productivity)

# ANALYSIS 11: CROSS-METRO INDUSTRY COMPARISON --------------------------------

cat("\n=== ANALYSIS 11: Cross-Geography Industry Comparison ===\n")

# For each initiative, compare key industries across geographies
industry_geo_comparison <- jobs_data %>%
  filter(display_level >= 2,
         year == recent_year,
         initiative != "Total Employment",
         naics_code != "000000") %>%
  group_by(initiative, naics_title, naics_code, year) %>%
  summarise(
    n_geographies = n_distinct(geo_area),
    total_jobs = sum(jobs, na.rm = TRUE),
    avg_jobs_per_geo = mean(jobs, na.rm = TRUE),
    max_geo_jobs = max(jobs, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(initiative, year) %>%
  arrange(desc(total_jobs)) %>%
  slice_head(n = 10) %>%
  ungroup() %>%
  select(year, initiative, naics_title, naics_code, n_geographies, 
         total_jobs, avg_jobs_per_geo, max_geo_jobs) %>%
  arrange(year, initiative, desc(total_jobs))

print(industry_geo_comparison)

# Industries unique to or concentrated in specific geographies
industry_specialization <- jobs_data %>%
  filter(display_level >= 2,
         year == recent_year,
         initiative != "Total Employment",
         naics_code != "000000") %>%
  # Remove duplicates before joining
  distinct(statefips, countyfips, metrofips, geo_area, initiative, 
           naics_code, naics_title, year, .keep_all = TRUE) %>%
  # Get US industry jobs by initiative
  left_join(
    jobs_data %>%
      filter(display_level >= 2, year == recent_year,
             geo_area == "United States", initiative != "Total Employment",
             naics_code != "000000") %>%
      distinct(initiative, naics_code, naics_title, .keep_all = TRUE) %>%
      select(initiative, naics_code, naics_title, us_industry_jobs = jobs),
    by = c("initiative", "naics_code", "naics_title")
  ) %>%
  # Get local initiative total employment
  left_join(
    jobs_data %>%
      filter(display_level == 0, year == recent_year,
             initiative != "Total Employment") %>%
      distinct(statefips, countyfips, metrofips, geo_area, initiative, .keep_all = TRUE) %>%
      select(statefips, countyfips, metrofips, geo_area, initiative, 
             local_init_total = jobs),
    by = c("statefips", "countyfips", "metrofips", "geo_area", "initiative")
  ) %>%
  # Get US initiative total employment
  left_join(
    jobs_data %>%
      filter(display_level == 0, year == recent_year,
             geo_area == "United States", initiative != "Total Employment") %>%
      distinct(initiative, .keep_all = TRUE) %>%
      select(initiative, us_init_total = jobs),
    by = "initiative"
  ) %>%
  mutate(
    local_industry_share = jobs / local_init_total * 100,
    us_industry_share = us_industry_jobs / us_init_total * 100,
    init_specific_lq = local_industry_share / us_industry_share
  ) %>%
  filter(init_specific_lq >= 2,  # Strong specialization
         jobs >= 100,  # Minimum size
         geo_area != "United States") %>%  # Exclude US from specialization
  select(statefips, countyfips, metrofips, geo_area, year, initiative,
         naics_title, naics_code, jobs, init_specific_lq) %>%
  arrange(desc(init_specific_lq)) %>%
  head(30)

cat("\nHighly Specialized Industries (LQ >= 2):\n")
print(industry_specialization)

# ANALYSIS 12: INDUSTRY VOLATILITY AND STABILITY -----------------------------

cat("\n=== ANALYSIS 12: Industry Volatility Analysis ===\n")

# Calculate coefficient of variation in growth rates
industry_volatility <- jobs_growth %>%
  filter(display_level >= 2,
         initiative != "Total Employment",
         naics_code != "000000",
         !is.na(yoy_growth)) %>%
  group_by(initiative, statefips, countyfips, metrofips, geo_area, 
           naics_title, naics_code) %>%
  summarise(
    n_years = n(),
    avg_growth = mean(yoy_growth, na.rm = TRUE),
    sd_growth = sd(yoy_growth, na.rm = TRUE),
    cv_growth = sd_growth / abs(avg_growth),
    current_jobs = last(jobs),
    .groups = "drop"
  ) %>%
  filter(n_years >= 3, current_jobs >= 50) %>%
  group_by(initiative, statefips, countyfips, metrofips, geo_area) %>%
  arrange(desc(cv_growth)) %>%
  slice_head(n = 10) %>%
  ungroup() %>%
  select(statefips, countyfips, metrofips, geo_area, initiative,
         naics_title, naics_code, n_years, avg_growth, sd_growth, 
         cv_growth, current_jobs) %>%
  arrange(statefips, countyfips, metrofips, geo_area, initiative, 
          desc(cv_growth))

cat("\nMost Volatile Industries by Initiative and Geography:\n")
print(industry_volatility)

# Most stable industries (lowest volatility, positive growth)
industry_stability <- jobs_growth %>%
  filter(display_level >= 2,
         initiative != "Total Employment",
         naics_code != "000000",
         !is.na(yoy_growth)) %>%
  group_by(initiative, statefips, countyfips, metrofips, geo_area,
           naics_title, naics_code) %>%
  summarise(
    n_years = n(),
    avg_growth = mean(yoy_growth, na.rm = TRUE),
    sd_growth = sd(yoy_growth, na.rm = TRUE),
    cv_growth = sd_growth / abs(avg_growth),
    current_jobs = last(jobs),
    .groups = "drop"
  ) %>%
  filter(n_years >= 3, current_jobs >= 100, avg_growth > 0) %>%
  group_by(initiative, statefips, countyfips, metrofips, geo_area) %>%
  arrange(cv_growth) %>%
  slice_head(n = 10) %>%
  ungroup() %>%
  select(statefips, countyfips, metrofips, geo_area, initiative,
         naics_title, naics_code, n_years, avg_growth, sd_growth,
         cv_growth, current_jobs) %>%
  arrange(statefips, countyfips, metrofips, geo_area, initiative, cv_growth)

cat("\nMost Stable High-Growth Industries by Initiative and Geography:\n")
print(industry_stability)

# EXPORT INITIATIVE DEEP DIVE TABLES ------------------------------------------

cat("\n=== Exporting Initiative Deep Dive Results ===\n")

write_csv(top_industries_by_initiative,
          file.path(output_dir, "top_industries_by_initiative.csv"))
write_csv(initiative_concentration,
          file.path(output_dir, "initiative_concentration_metrics.csv"))
write_csv(industry_growth_contrib,
          file.path(output_dir, "industry_growth_contributors.csv"))
write_csv(industry_wage_dist,
          file.path(output_dir, "industry_wage_distribution.csv"))
write_csv(wage_extremes_by_init,
          file.path(output_dir, "wage_extremes_by_initiative.csv"))
write_csv(industry_productivity,
          file.path(output_dir, "industry_productivity.csv"))
write_csv(industry_geo_comparison,
          file.path(output_dir, "industry_geo_comparison.csv"))
write_csv(industry_specialization,
          file.path(output_dir, "industry_specialization.csv"))
write_csv(industry_volatility,
          file.path(output_dir, "industry_volatility.csv"))
write_csv(industry_stability,
          file.path(output_dir, "industry_stability.csv"))

# SAVE ADDITIONAL DATA FOR VISUALIZATIONS ------------------------------------

save(top_industries_by_initiative, initiative_concentration,
     industry_growth_contrib, industry_wage_dist, wage_extremes_by_init,
     industry_productivity, industry_geo_comparison, industry_specialization,
     industry_volatility, industry_stability,
     file = file.path(output_dir, "processed_data_initiative_deepdive.RData"))

cat(sprintf("Initiative deep dive data saved to '%s/processed_data_initiative_deepdive.RData'\n", 
            output_dir))

# DEEP DIVE SUMMARY -----------------------------------------------------------

cat("\n" , rep("=", 70), "\n", sep = "")
cat("INITIATIVE DEEP DIVE SUMMARY\n")
cat(rep("=", 70), "\n\n", sep = "")

# Summary by geography type
for(geo_type in c("State", "Metro")) {
  cat(sprintf("\n%s LEVEL SUMMARY\n", toupper(geo_type)))
  cat(rep("=", 70), "\n", sep = "")
  
  # Get relevant geographies
  if(geo_type == "State") {
    geo_list <- c("Indiana")
  } else {
    geo_list <- top_industries_by_initiative %>%
      filter(statefips == 18, !is.na(metrofips)) %>%
      distinct(geo_area) %>%
      arrange(geo_area) %>%
      pull(geo_area) %>%
      head(5)  # Top 5 metros
  }
  
  for(geo in geo_list) {
    cat(sprintf("\n%s\n", geo))
    cat(rep("-", nchar(geo)), "\n", sep = "")
    
    for(init in unique(top_industries_by_initiative$initiative)) {
      # Top 3 industries
      top_3 <- top_industries_by_initiative %>%
        filter(geo_area == geo, initiative == init) %>%
        head(3)
      
      if(nrow(top_3) > 0) {
        cat(sprintf("\n  %s:\n", init))
        cat(sprintf("    Top 3 Industries by Employment:\n"))
        for(i in 1:min(3, nrow(top_3))) {
          cat(sprintf("      %d. %s (%s jobs)\n", i, top_3$naics_title[i], 
                      comma(top_3$jobs[i])))
        }
        
        # Concentration
        conc <- initiative_concentration %>% 
          filter(geo_area == geo, initiative == init)
        if(nrow(conc) > 0) {
          cat(sprintf("    Concentration: Top 5 industries = %.1f%% of jobs\n", 
                      conc$top5_concentration))
        }
        
        # Growth contributors
        growth_top <- industry_growth_contrib %>%
          filter(geo_area == geo, initiative == init) %>%
          head(2)
        
        if(nrow(growth_top) > 0) {
          cat(sprintf("    Top Growth Contributor: %s (%+s jobs)\n",
                      growth_top$naics_title[1],
                      comma(round(growth_top$yoy_change[1]))))
        }
        
        # Wages
        wages <- industry_wage_dist %>% 
          filter(geo_area == geo, initiative == init)
        if(nrow(wages) > 0) {
          cat(sprintf("    Wage Range: $%s - $%s (median: $%s)\n",
                      comma(round(wages$min_wage)),
                      comma(round(wages$max_wage)),
                      comma(round(wages$median_wage))))
        }
      }
    }
  }
}

cat("\n", rep("=", 70), "\n", sep = "")