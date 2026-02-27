# ============================================================================
# CICP Talent Demand - Occupation Data - Exploratory Data Analysis
# ============================================================================
# Purpose: Automated EDA and insights generation for CICP occupation data
# Author: Riley Hudelson-Zipper
# Last Updated: 2025-10-27
# ============================================================================

# SETUP -----------------------------------------------------------------------

# Load required packages
library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)

# Set data directory (MODIFY THIS FOR EACH DATA RUN)
data_dir <- "CICP_20251104"  # Update with your data folder name
output_dir <- paste0("outputs_occupation_", gsub("CICP_", "", data_dir))

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

# Define color palette for initiatives (matches main CICP pipeline)
initiative_colors <- c(
  "Advanced & Traded Industries" = "#1565C0",
  "AgriNovus" = "#2E7D32",
  "BioCrossroads" = "#6A1B9A",
  "Conexus - Manufacturing" = "#D84315",
  "Conexus - Logistics" = "#BF360C",
  "TechPoint" = "#F57C00",
  "Finance & Insurance" = "#19909a",
  "Healthcare" = "#c92f6c",
  "Total Employment" = "#424242"
)

# HELPER FUNCTIONS ------------------------------------------------------------

# Function to calculate growth rates
calculate_growth <- function(df, value_col, group_vars) {
  df %>%
    arrange(!!!syms(group_vars), year) %>%
    group_by(!!!syms(group_vars)) %>%
    mutate(
      yoy_growth = ({{value_col}} / lag({{value_col}}) - 1) * 100,
      yoy_change = {{value_col}} - lag({{value_col}}),
      cagr_2yr = if_else(
        n() >= 3 & !is.na({{value_col}}) & !is.na(lag({{value_col}}, 2)),
        ({{value_col}} / lag({{value_col}}, 2))^(1/2) - 1,
        NA_real_
      ) * 100,
      cagr_4yr = if_else(
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

cat("Loading occupation data files...\n")

# Read employment by occupation data
emp_file <- file.path(data_dir, "talentDemand_empByOcc.csv")
if (file.exists(emp_file)) {
  emp_data <- read_csv(emp_file, show_col_types = FALSE) %>%
    rename_with(tolower)
  cat(sprintf("Loaded %d employment records\n", nrow(emp_data)))
} else {
  stop(sprintf("Employment file not found: %s", emp_file))
}

# Read wage by occupation data
wage_file <- file.path(data_dir, "talentDemand_MedianWageByOcc.csv")
if (file.exists(wage_file)) {
  wage_data <- read_csv(wage_file, show_col_types = FALSE) %>%
    rename_with(tolower)
  cat(sprintf("Loaded %d wage records\n", nrow(wage_data)))
} else {
  stop(sprintf("Wage file not found: %s", wage_file))
}

# DATA PREPARATION ------------------------------------------------------------

cat("Preparing data for analysis...\n")

# Standardize column names between datasets
emp_data <- emp_data %>%
  filter(year >= 2019 & year <= 2024) %>%
  rename(
    # Ensure consistent naming
    occ_code = occ,
    occupation = occ_title
  )

wage_data <- wage_data %>%
  filter(year >= 2019 & year <= 2024) %>%
  rename(
    year = `year`,  # Handle potential capital Y
    occ_code = occ,
    occupation = description
  )

# Split "Conexus" into Manufacturing and Logistics by SOC major group
# SOC 53-xxxx = Transportation & Material Moving → Logistics; all others → Manufacturing
emp_data <- emp_data %>%
  mutate(
    cicp_initiative = case_when(
      cicp_initiative == "Conexus" & str_starts(occ_code, "53-") ~ "Conexus - Logistics",
      cicp_initiative == "Conexus" ~ "Conexus - Manufacturing",
      TRUE ~ cicp_initiative
    )
  )

wage_data <- wage_data %>%
  mutate(
    cicp_initiative = case_when(
      cicp_initiative == "Conexus" & str_starts(occ_code, "53-") ~ "Conexus - Logistics",
      cicp_initiative == "Conexus" ~ "Conexus - Manufacturing",
      TRUE ~ cicp_initiative
    )
  )

# Calculate growth rates for employment
emp_growth <- emp_data %>%
  filter(!is.na(jobs)) %>%
  calculate_growth(jobs, c("cicp_initiative", "geo_area", "occ_code", "occupation"))

# Calculate growth rates for wages
wage_growth <- wage_data %>%
  filter(!is.na(median_annual_earnings)) %>%
  calculate_growth(median_annual_earnings, 
                   c("cicp_initiative", "geo_area", "occ_code", "occupation"))

# Identify geography types
emp_data <- emp_data %>%
  mutate(geo_type = case_when(
    statefips == "0" & metrofips == "00000" ~ "U.S.",
    statefips == "18" ~ "State",
    metrofips != "00000" ~ "Metro",
    TRUE ~ "Unknown"
  ))

wage_data <- wage_data %>%
  mutate(geo_type = case_when(
    statefips == "0" & metrofips == "00000" ~ "U.S.",
    statefips == "18" ~ "State",
    metrofips != "00000" ~ "Metro",
    TRUE ~ "Unknown"
  ))

emp_growth <- emp_growth %>%
  mutate(geo_type = case_when(
    statefips == "0" & metrofips == "00000" ~ "U.S.",
    statefips == "18" ~ "State",
    metrofips != "00000" ~ "Metro",
    TRUE ~ "Unknown"
  ))

wage_growth <- wage_growth %>%
  mutate(geo_type = case_when(
    statefips == "0" & metrofips == "00000" ~ "U.S.",
    statefips == "18" ~ "State",
    metrofips != "00000" ~ "Metro",
    TRUE ~ "Unknown"
  ))

# ANALYSIS SETUP --------------------------------------------------------------

# Define most recent year
recent_year <- 2024
cat(sprintf("Using %d as most recent year\n", recent_year))

# ANALYSIS 1: INITIATIVE-LEVEL OCCUPATION OVERVIEW ---------------------------

cat("\n=== ANALYSIS 1: Initiative-Level Occupation Overview ===\n")

# Top occupations by employment in Indiana
indiana_top_occupations <- emp_data %>%
  filter(year == recent_year, 
         geo_area == "Indiana",
         cicp_initiative != "Total Employment") %>%
  group_by(cicp_initiative, occ_code, occupation) %>%
  summarise(
    total_jobs = sum(jobs, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(cicp_initiative) %>%
  arrange(desc(total_jobs)) %>%
  slice_head(n = 10) %>%
  ungroup() %>%
  arrange(cicp_initiative, desc(total_jobs))

cat("\nTop 10 Occupations by Jobs in Each Initiative (Indiana):\n")
print(indiana_top_occupations)

# Occupation summary by initiative in Indiana
indiana_occ_summary <- emp_data %>%
  filter(year == recent_year, 
         geo_area == "Indiana",
         cicp_initiative != "Total Employment") %>%
  left_join(
    wage_data %>%
      filter(year == recent_year, geo_area == "Indiana") %>%
      select(cicp_initiative, occ_code, median_annual_earnings),
    by = c("cicp_initiative", "occ_code")
  ) %>%
  group_by(cicp_initiative) %>%
  summarise(
    n_occupations = n_distinct(occ_code[jobs > 0]),
    total_jobs = sum(jobs, na.rm = TRUE),
    avg_median_wage = mean(median_annual_earnings, na.rm = TRUE),
    median_wage = median(median_annual_earnings, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_jobs))

cat("\nOccupation Summary by Initiative (Indiana):\n")
print(indiana_occ_summary)

# ANALYSIS 2: OCCUPATION GROWTH ANALYSIS --------------------------------------

cat("\n=== ANALYSIS 2: Occupation Growth Analysis ===\n")

# Fastest growing occupations in Indiana by initiative
indiana_occ_growth <- emp_growth %>%
  filter(year == recent_year,
         geo_area == "Indiana",
         cicp_initiative != "Total Employment",
         !is.na(cagr_2yr)) %>%
  group_by(cicp_initiative, occ_code, occupation, jobs) %>%
  summarise(
    cagr_2yr = mean(cagr_2yr, na.rm = TRUE),
    yoy_growth = mean(yoy_growth, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(jobs >= 100) %>%  # Minimum employment threshold
  group_by(cicp_initiative) %>%
  arrange(desc(cagr_2yr)) %>%
  slice_head(n = 10) %>%
  ungroup() %>%
  arrange(cicp_initiative, desc(cagr_2yr))

cat("\nFastest Growing Occupations by Initiative (Indiana, min 100 jobs):\n")
print(indiana_occ_growth)

# Occupations with declining employment
indiana_declining_occs <- emp_growth %>%
  filter(year == recent_year,
         geo_area == "Indiana",
         cicp_initiative != "Total Employment",
         !is.na(cagr_2yr),
         cagr_2yr < -2,  # At least 2% annual decline
         jobs >= 50) %>%
  group_by(cicp_initiative, occ_code, occupation, jobs) %>%
  summarise(
    cagr_2yr = mean(cagr_2yr, na.rm = TRUE),
    yoy_growth = mean(yoy_growth, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(cagr_2yr)

cat("\nDeclining Occupations (Indiana, CAGR < -2%):\n")
if (nrow(indiana_declining_occs) > 0) {
  print(indiana_declining_occs)
} else {
  cat("No occupations with significant decline identified.\n")
}

# ANALYSIS 3: WAGE ANALYSIS ---------------------------------------------------

cat("\n=== ANALYSIS 3: Wage Analysis ===\n")

# Highest paid occupations by initiative in Indiana
indiana_high_wage_occs <- wage_data %>%
  filter(year == recent_year,
         geo_area == "Indiana",
         cicp_initiative != "Total Employment") %>%
  left_join(
    emp_data %>%
      filter(year == recent_year, geo_area == "Indiana") %>%
      select(cicp_initiative, occ_code, jobs),
    by = c("cicp_initiative", "occ_code")
  ) %>%
  filter(jobs >= 100) %>%
  group_by(cicp_initiative) %>%
  arrange(desc(median_annual_earnings)) %>%
  slice_head(n = 10) %>%
  ungroup() %>%
  select(cicp_initiative, occ_code, occupation, jobs, 
         median_hourly_earnings, median_annual_earnings) %>%
  arrange(cicp_initiative, desc(median_annual_earnings))

cat("\nHighest Paid Occupations by Initiative (Indiana, min 100 jobs):\n")
print(indiana_high_wage_occs)

# Wage growth by occupation
indiana_wage_growth <- wage_growth %>%
  filter(year == recent_year,
         geo_area == "Indiana",
         cicp_initiative != "Total Employment",
         !is.na(cagr_2yr)) %>%
  left_join(
    emp_data %>%
      filter(year == recent_year, geo_area == "Indiana") %>%
      select(cicp_initiative, occ_code, jobs),
    by = c("cicp_initiative", "occ_code")
  ) %>%
  filter(jobs >= 100) %>%
  group_by(cicp_initiative) %>%
  arrange(desc(cagr_2yr)) %>%
  slice_head(n = 10) %>%
  ungroup() %>%
  select(cicp_initiative, occ_code, occupation, jobs, 
         median_annual_earnings, cagr_2yr, yoy_growth) %>%
  arrange(cicp_initiative, desc(cagr_2yr))

cat("\nFastest Wage Growth Occupations by Initiative (Indiana, min 100 jobs):\n")
print(indiana_wage_growth)

# ANALYSIS 4: GEOGRAPHIC COMPARISON -------------------------------------------

cat("\n=== ANALYSIS 4: Geographic Comparison ===\n")

# Top metros by occupation employment
metro_occ_summary <- emp_data %>%
  filter(year == recent_year,
         geo_type == "Metro",
         cicp_initiative != "Total Employment") %>%
  group_by(geo_area) %>%
  summarise(
    total_jobs = sum(jobs, na.rm = TRUE),
    n_occupations = n_distinct(occ_code[jobs > 0]),
    n_initiatives = n_distinct(cicp_initiative),
    .groups = "drop"
  ) %>%
  arrange(desc(total_jobs)) %>%
  head(10)

cat("\nTop Metros by Total Occupation Employment:\n")
print(metro_occ_summary)

# ANALYSIS 5: OCCUPATION CONCENTRATION ----------------------------------------

cat("\n=== ANALYSIS 5: Occupation Concentration Analysis ===\n")

# Calculate occupation concentration within each initiative (Indiana)
indiana_occ_concentration <- emp_data %>%
  filter(year == recent_year,
         geo_area == "Indiana",
         cicp_initiative != "Total Employment") %>%
  group_by(cicp_initiative) %>%
  mutate(
    total_initiative_jobs = sum(jobs, na.rm = TRUE),
    occ_share = jobs / total_initiative_jobs * 100
  ) %>%
  ungroup() %>%
  arrange(cicp_initiative, desc(occ_share)) %>%
  group_by(cicp_initiative) %>%
  mutate(
    cumulative_share = cumsum(occ_share),
    top_10_indicator = row_number() <= 10
  ) %>%
  ungroup()

# Top 10 occupation concentration by initiative
indiana_top10_concentration <- indiana_occ_concentration %>%
  filter(top_10_indicator) %>%
  group_by(cicp_initiative) %>%
  summarise(
    n_top_occupations = n(),
    top10_jobs = sum(jobs),
    top10_share = sum(occ_share),
    .groups = "drop"
  ) %>%
  arrange(desc(top10_share))

cat("\nTop 10 Occupation Concentration by Initiative (Indiana):\n")
print(indiana_top10_concentration)

# Most concentrated occupations (high share in single initiative)
occupation_specialization <- emp_data %>%
  filter(year == recent_year,
         geo_area == "Indiana",
         cicp_initiative != "Total Employment") %>%
  group_by(occ_code, occupation) %>%
  mutate(
    total_occ_jobs = sum(jobs, na.rm = TRUE),
    init_share = jobs / total_occ_jobs * 100
  ) %>%
  ungroup() %>%
  filter(init_share >= 60, total_occ_jobs >= 100) %>%
  arrange(desc(init_share)) %>%
  select(occ_code, occupation, cicp_initiative, jobs, 
         total_occ_jobs, init_share)

cat("\nHighly Specialized Occupations (>60% in single initiative, min 100 jobs):\n")
if (nrow(occupation_specialization) > 0) {
  print(occupation_specialization)
} else {
  cat("No highly specialized occupations identified.\n")
}

# ANALYSIS 6: VOLATILITY AND STABILITY ----------------------------------------

cat("\n=== ANALYSIS 6: Occupation Volatility and Stability ===\n")

# Most volatile occupations (high coefficient of variation in growth)
occupation_volatility <- emp_growth %>%
  filter(cicp_initiative != "Total Employment",
         geo_area == "Indiana",
         !is.na(yoy_growth),
         year <= recent_year) %>%
  group_by(cicp_initiative, occ_code, occupation) %>%
  summarise(
    n_years = n(),
    avg_growth = mean(yoy_growth, na.rm = TRUE),
    sd_growth = sd(yoy_growth, na.rm = TRUE),
    cv_growth = sd_growth / abs(avg_growth),
    current_jobs = last(jobs),
    .groups = "drop"
  ) %>%
  filter(n_years >= 3, current_jobs >= 50) %>%
  arrange(desc(cv_growth)) %>%
  head(20)

cat("\nMost Volatile Occupations (Indiana, min 50 jobs):\n")
print(occupation_volatility)

# Most stable high-growth occupations
occupation_stability <- emp_growth %>%
  filter(cicp_initiative != "Total Employment",
         geo_area == "Indiana",
         !is.na(yoy_growth),
         year <= recent_year) %>%
  group_by(cicp_initiative, occ_code, occupation) %>%
  summarise(
    n_years = n(),
    avg_growth = mean(yoy_growth, na.rm = TRUE),
    sd_growth = sd(yoy_growth, na.rm = TRUE),
    cv_growth = sd_growth / abs(avg_growth),
    current_jobs = last(jobs),
    .groups = "drop"
  ) %>%
  filter(n_years >= 3, current_jobs >= 100, avg_growth > 0) %>%
  arrange(cv_growth) %>%
  head(20)

cat("\nMost Stable High-Growth Occupations (Indiana, min 100 jobs):\n")
print(occupation_stability)

# EXPORT KEY TABLES -----------------------------------------------------------

# cat("\n=== Exporting Results ===\n")

# write_csv(indiana_top_occupations, 
#           file.path(output_dir, "indiana_top_occupations.csv"))
# write_csv(indiana_occ_summary, 
#           file.path(output_dir, "indiana_occupation_summary.csv"))
# write_csv(indiana_occ_growth, 
#           file.path(output_dir, "indiana_occupation_growth.csv"))
# write_csv(indiana_declining_occs, 
#           file.path(output_dir, "indiana_declining_occupations.csv"))
# write_csv(indiana_high_wage_occs, 
#           file.path(output_dir, "indiana_high_wage_occupations.csv"))
# write_csv(indiana_wage_growth, 
#           file.path(output_dir, "indiana_wage_growth_occupations.csv"))
# write_csv(metro_occ_summary, 
#           file.path(output_dir, "metro_occupation_summary.csv"))
# write_csv(state_comparison, 
#           file.path(output_dir, "state_occupation_comparison.csv"))
# write_csv(indiana_top10_concentration, 
#           file.path(output_dir, "indiana_occupation_concentration.csv"))
# write_csv(occupation_specialization, 
#           file.path(output_dir, "occupation_specialization.csv"))
# write_csv(occupation_volatility, 
#           file.path(output_dir, "occupation_volatility.csv"))
# write_csv(occupation_stability, 
#           file.path(output_dir, "occupation_stability.csv"))

# SAVE PROCESSED DATA FOR VISUALIZATION SCRIPT --------------------------------

cat("\n=== Saving processed data for visualization script ===\n")

save(emp_data, wage_data,
     emp_growth, wage_growth,
     recent_year, initiative_colors, data_dir,
     file = file.path(output_dir, "processed_data_occupations.RData"))

cat(sprintf("Processed data saved to '%s/processed_data_occupations.RData'\n", 
            output_dir))

# SUMMARY INSIGHTS REPORT -----------------------------------------------------

cat("\n" , rep("=", 70), "\n", sep = "")
cat("EXECUTIVE SUMMARY - OCCUPATION INSIGHTS (INDIANA)\n")
cat(rep("=", 70), "\n\n", sep = "")

cat("1. OCCUPATION OVERVIEW\n")
cat(sprintf("   - Total occupations tracked: %s\n", 
            comma(n_distinct(emp_data$occ_code))))
cat(sprintf("   - Total jobs across initiatives: %s\n", 
            comma(sum(indiana_occ_summary$total_jobs, na.rm = TRUE))))
cat(sprintf("   - Initiative with most occupations: %s (%s occupations)\n",
            indiana_occ_summary$cicp_initiative[which.max(indiana_occ_summary$n_occupations)],
            comma(max(indiana_occ_summary$n_occupations, na.rm = TRUE))))

cat("\n2. TOP OCCUPATIONS\n")
top_occ_overall <- emp_data %>%
  filter(year == recent_year, geo_area == "Indiana", 
         cicp_initiative != "Total Employment") %>%
  group_by(occ_code, occupation) %>%
  summarise(total_jobs = sum(jobs, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_jobs)) %>%
  head(1)
cat(sprintf("   - Highest employment occupation: %s (%s jobs)\n",
            top_occ_overall$occupation[1],
            comma(top_occ_overall$total_jobs[1])))

if (nrow(indiana_high_wage_occs) > 0) {
  top_wage_occ <- indiana_high_wage_occs %>%
    arrange(desc(median_annual_earnings)) %>%
    head(1)
  cat(sprintf("   - Highest paid occupation: %s ($%s/year)\n",
              top_wage_occ$occupation[1],
              comma(round(top_wage_occ$median_annual_earnings[1]))))
}

cat("\n3. GROWTH TRENDS\n")
if (nrow(indiana_occ_growth) > 0) {
  fastest_growing <- indiana_occ_growth %>%
    arrange(desc(cagr_2yr)) %>%
    head(1)
  cat(sprintf("   - Fastest growing occupation: %s (%.1f%% CAGR)\n",
              fastest_growing$occupation[1],
              fastest_growing$cagr_2yr[1]))
  cat(sprintf("   - Total occupations with positive growth: %d\n",
              sum(indiana_occ_growth$cagr_2yr > 0, na.rm = TRUE)))
}

if (nrow(indiana_declining_occs) > 0) {
  cat(sprintf("   - Occupations in decline: %d\n", nrow(indiana_declining_occs)))
}

cat("\n4. CONCENTRATION\n")
if (nrow(indiana_top10_concentration) > 0) {
  most_concentrated <- indiana_top10_concentration %>%
    arrange(desc(top10_share)) %>%
    head(1)
  cat(sprintf("   - Most concentrated initiative: %s (%.1f%% in top 10 occupations)\n",
              most_concentrated$cicp_initiative[1],
              most_concentrated$top10_share[1]))
}

cat("\n5. WAGE ANALYSIS\n")
wage_summary <- wage_data %>%
  filter(year == recent_year, geo_area == "Indiana",
         cicp_initiative != "Total Employment") %>%
  summarise(
    avg_median_wage = mean(median_annual_earnings, na.rm = TRUE),
    median_wage = median(median_annual_earnings, na.rm = TRUE)
  )
cat(sprintf("   - Average median annual wage: $%s\n",
            comma(round(wage_summary$avg_median_wage[1]))))
cat(sprintf("   - Median annual wage: $%s\n",
            comma(round(wage_summary$median_wage[1]))))

cat("\n" , rep("=", 70), "\n", sep = "")
cat("ANALYSIS COMPLETE\n")
cat(sprintf("Results exported to: %s/\n", output_dir))
cat(rep("=", 70), "\n\n", sep = "")