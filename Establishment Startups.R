################################################################################
# CICP Innovation Report: Establishment Growth Analysis
# Data Source: IBRC Longitudinal Database (LDB) - New Establishments
# 
# Purpose: Analyze establishment growth (new business starts) by initiative
# Author: Analysis for CICP Innovation Report
# Date: October 30, 2025
################################################################################

# Load required libraries
library(tidyverse)
library(readxl)
library(janitor)
library(scales)
library(lubridate)

# Set working directory and options
options(scipen = 999)  # Disable scientific notation
theme_set(theme_minimal())

################################################################################
# 0. CONFIGURATION
################################################################################

# Set the most recent year of establishment data
estab_recent_year <- 2022

################################################################################
# 1. DATA LOADING AND INITIAL STRUCTURE
################################################################################

# Note: Update this path to where your file is located
# The file is NOT in the main data folder due to size
estab_file_path <- "../Estab Growth by Initiative.xlsx"

# Load the data
cat("Loading establishment growth data...\n")
estab_growth <- read_excel(estab_file_path)

# Clean column names
estab_growth <- estab_growth %>%
  clean_names()

# Recode initiative names for consistency with other CICP datasets
estab_growth <- estab_growth %>%
  mutate(
    cicp_initiative = case_when(
      cicp_initiative == "Advanced Industries" ~ "Advanced & Traded Industries",
      cicp_initiative == "All Indiana" ~ "Total",
      TRUE ~ cicp_initiative
    ),
    # Calculate establishment age (years since founding)
    establishment_age = reporting_yr - cohort_year,
    # Flag true startups (1-20 employees) vs larger qualified establishments
    is_true_startup = (new_est_type == 1)
  )

cat("✓ Recoded 'Advanced Industries' to 'Advanced & Traded Industries'\n")
cat("✓ Recoded 'All Indiana' to 'Total'\n")
cat("✓ Calculated establishment_age (reporting_yr - cohort_year)\n")
cat("✓ Added is_true_startup flag (TRUE for new_est_type == 1)\n\n")

# Filter out "Not Qualified" establishments (new_est_type == 5)
estab_growth <- estab_growth %>%
  filter(new_est_type != 5)

cat("✓ Excluded new_est_type == 5 (Not Qualified establishments)\n\n")

# Display structure and dimensions
cat("Data Structure:\n")
glimpse(estab_growth)

cat("\nData Dimensions:\n")
cat("Rows:", nrow(estab_growth), "\n")
cat("Columns:", ncol(estab_growth), "\n")

################################################################################
# 2. DATA PREPARATION - CALCULATE ANNUAL METRICS
################################################################################

cat("\n" %+% paste(rep("=", 80), collapse = "") %+% "\n")
cat("CALCULATING ANNUAL METRICS FROM MONTHLY/QUARTERLY DATA\n")
cat(paste(rep("=", 80), collapse = "") %+% "\n\n")

# Calculate annual employment (average of monthly employment) - USING ADJUSTED VALUES
employment_cols_adjusted <- grep("^x\\d{2}_month_.*_employment_adjusted$", names(estab_growth), value = TRUE)

estab_growth <- estab_growth %>%
  mutate(
    annual_employment = rowMeans(select(., all_of(employment_cols_adjusted)), na.rm = TRUE),
    annual_wages = q1_total_wages_adjusted + q2_total_wages_adjusted + 
                   q3_total_wages_adjusted + q4_total_wages_adjusted,
    avg_annual_wage = if_else(annual_employment > 0, 
                              annual_wages / annual_employment, 
                              NA_real_)
  )

cat("✓ Calculated annual_employment (average of 12 monthly ADJUSTED values)\n")
cat("✓ Calculated annual_wages (sum of 4 quarterly ADJUSTED values)\n")
cat("✓ Calculated avg_annual_wage (annual_wages / annual_employment)\n\n")

################################################################################
# 3. DATA QUALITY AND SUMMARY STATISTICS
################################################################################

cat(paste(rep("=", 80), collapse = "") %+% "\n")
cat("DATA QUALITY CHECKS\n")
cat(paste(rep("=", 80), collapse = "") %+% "\n\n")

# Check for missing values in key columns
key_cols <- c("cohort_year", "reporting_yr", "county_desc", "cicp_initiative", 
              "est_count_adjusted", "annual_employment", "annual_wages", "sector_name")

missing_summary <- estab_growth %>%
  select(all_of(key_cols)) %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Missing_Count") %>%
  mutate(Missing_Pct = round(Missing_Count / nrow(estab_growth) * 100, 2)) %>%
  arrange(desc(Missing_Count))

print(missing_summary)

# Year ranges
cat("\nYear Ranges:\n")
cat("Cohort Years:", min(estab_growth$cohort_year, na.rm = TRUE), 
    "to", max(estab_growth$cohort_year, na.rm = TRUE), "\n")
cat("Reporting Years:", min(estab_growth$reporting_yr, na.rm = TRUE), 
    "to", max(estab_growth$reporting_yr, na.rm = TRUE), "\n")

# Initiative breakdown (aggregate all sectors)
cat("\nInitiative Distribution:\n")
initiative_summary <- estab_growth %>%
  group_by(cicp_initiative) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(n))
print(initiative_summary)

# Economic Growth Region breakdown (aggregate all sectors)
cat("\nEconomic Growth Region (EGR) Distribution:\n")
egr_summary <- estab_growth %>%
  group_by(egr) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(egr)
print(egr_summary)

# MSA breakdown (aggregate all sectors)
cat("\nMSA Distribution:\n")
msa_summary <- estab_growth %>%
  filter(!is.na(msa), msa != "NULL") %>%
  group_by(msa) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(n))
print(head(msa_summary, 10))

# County distribution
cat("\nNumber of unique counties:", n_distinct(estab_growth$county_desc), "\n")

# Sector distribution
cat("\nSector Distribution:\n")
sector_summary <- estab_growth %>%
  filter(!is.na(sector_name), sector_name != "All") %>%
  count(sector_name) %>%
  arrange(desc(n))
print(sector_summary)

# New Establishment Type distribution
cat("\nNew Establishment Type Distribution:\n")
est_type_summary <- estab_growth %>%
  count(new_est_type, new_est_type_desc) %>%
  arrange(new_est_type)
print(est_type_summary)

################################################################################
# 4. KEY METRICS BY INITIATIVE (STATEWIDE)
################################################################################

cat("\n" %+% paste(rep("=", 80), collapse = "") %+% "\n")
cat("ESTABLISHMENT GROWTH METRICS BY INITIATIVE (STATEWIDE)\n")
cat(paste(rep("=", 80), collapse = "") %+% "\n\n")

# Most recent reporting year analysis
most_recent_year <- max(estab_growth$reporting_yr, na.rm = TRUE)

# STATEWIDE: Aggregate all sectors by initiative
recent_initiative_summary <- estab_growth %>%
  filter(reporting_yr == most_recent_year) %>%
  group_by(cicp_initiative) %>%
  summarise(
    Total_Establishments = sum(est_count_adjusted, na.rm = TRUE),
    Total_Employment = sum(annual_employment, na.rm = TRUE),
    Total_Wages = sum(annual_wages, na.rm = TRUE),
    Avg_Wage = weighted.mean(avg_annual_wage, annual_employment, na.rm = TRUE),
    Counties = n_distinct(county_desc),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Establishments)) %>%
  mutate(
    Pct_of_Total_Estabs = Total_Establishments / sum(Total_Establishments) * 100,
    Pct_of_Total_Employment = Total_Employment / sum(Total_Employment) * 100
  )

cat("Summary for Most Recent Year (", most_recent_year, "):\n\n", sep = "")
print(recent_initiative_summary)

################################################################################
# 4B. KEY METRICS BY GEOGRAPHY, INITIATIVE, AND SECTOR
################################################################################

cat("\n" %+% paste(rep("=", 80), collapse = "") %+% "\n")
cat("ESTABLISHMENT METRICS BY GEOGRAPHY, INITIATIVE, INIT_SECT, AND SECTOR\n")
cat(paste(rep("=", 80), collapse = "") %+% "\n\n")

# STATEWIDE by initiative, cicp_init_sect, and sector_name
statewide_init_sect_summary <- estab_growth %>%
  filter(reporting_yr == most_recent_year) %>%
  group_by(cicp_initiative, cicp_init_sect, sector_name) %>%
  summarise(
    Total_Establishments = sum(est_count_adjusted, na.rm = TRUE),
    Total_Employment = sum(annual_employment, na.rm = TRUE),
    Total_Wages = sum(annual_wages, na.rm = TRUE),
    Avg_Wage = weighted.mean(avg_annual_wage, annual_employment, na.rm = TRUE),
    Counties = n_distinct(county_desc),
    .groups = "drop"
  ) %>%
  arrange(cicp_initiative, cicp_init_sect, desc(Total_Establishments))

cat("Statewide by Initiative, Init_Sect, and Sector (", most_recent_year, "):\n\n", sep = "")
print(head(statewide_init_sect_summary, 20))

# EGR by initiative, cicp_init_sect, and sector_name
egr_init_sect_summary <- estab_growth %>%
  filter(reporting_yr == most_recent_year) %>%
  group_by(egr, cicp_initiative, cicp_init_sect, sector_name) %>%
  summarise(
    Total_Establishments = sum(est_count_adjusted, na.rm = TRUE),
    Total_Employment = sum(annual_employment, na.rm = TRUE),
    Total_Wages = sum(annual_wages, na.rm = TRUE),
    Avg_Wage = weighted.mean(avg_annual_wage, annual_employment, na.rm = TRUE),
    Counties = n_distinct(county_desc),
    .groups = "drop"
  ) %>%
  arrange(egr, cicp_initiative, cicp_init_sect, desc(Total_Establishments))

cat("\n\nEGR by Initiative, Init_Sect, and Sector (", most_recent_year, "):\n\n", sep = "")
print(head(egr_init_sect_summary, 20))

# MSA by initiative, cicp_init_sect, and sector_name
msa_init_sect_summary <- estab_growth %>%
  filter(reporting_yr == most_recent_year, !is.na(msa), msa != "NULL") %>%
  group_by(msa, cicp_initiative, cicp_init_sect, sector_name) %>%
  summarise(
    Total_Establishments = sum(est_count_adjusted, na.rm = TRUE),
    Total_Employment = sum(annual_employment, na.rm = TRUE),
    Total_Wages = sum(annual_wages, na.rm = TRUE),
    Avg_Wage = weighted.mean(avg_annual_wage, annual_employment, na.rm = TRUE),
    Counties = n_distinct(county_desc),
    .groups = "drop"
  ) %>%
  arrange(msa, cicp_initiative, cicp_init_sect, desc(Total_Establishments))

cat("\n\nMSA by Initiative, Init_Sect, and Sector (", most_recent_year, "):\n\n", sep = "")
print(head(msa_init_sect_summary, 20))

################################################################################
# 5. COHORT ANALYSIS - NEW ESTABLISHMENT TRENDS (STATEWIDE)
################################################################################

cat("\n" %+% paste(rep("=", 80), collapse = "") %+% "\n")
cat("NEW ESTABLISHMENT COHORT ANALYSIS (STATEWIDE)\n")
cat(paste(rep("=", 80), collapse = "") %+% "\n\n")

# Annual new establishments by initiative (aggregate all sectors)
new_estabs_by_year <- estab_growth %>%
  filter(cohort_year == reporting_yr) %>%
  group_by(cohort_year, cicp_initiative) %>%
  summarise(
    New_Establishments = sum(est_count_adjusted, na.rm = TRUE),
    Initial_Employment = sum(annual_employment, na.rm = TRUE),
    Initial_Wages = sum(annual_wages, na.rm = TRUE),
    .groups = "drop"
  )

cat("New Establishments by Cohort Year and Initiative:\n")
print(new_estabs_by_year %>% arrange(desc(cohort_year), cicp_initiative))

# Total new establishments by year (all initiatives)
new_estabs_total <- new_estabs_by_year %>%
  filter(cicp_initiative != "Total") %>%  # Exclude Total to avoid double counting
  group_by(cohort_year) %>%
  summarise(
    Total_New_Establishments = sum(New_Establishments),
    Total_Initial_Employment = sum(Initial_Employment),
    .groups = "drop"
  )

cat("\nTotal New Establishments by Year (All Initiatives):\n")
print(new_estabs_total)

################################################################################
# 6. SURVIVAL ANALYSIS - ESTABLISHMENT PERSISTENCE
################################################################################

cat("\n" %+% paste(rep("=", 80), collapse = "") %+% "\n")
cat("ESTABLISHMENT SURVIVAL ANALYSIS\n")
cat(paste(rep("=", 80), collapse = "") %+% "\n\n")

# Calculate establishment survival rates by tracking cohorts over time
# Aggregate all sectors by initiative
survival_data <- estab_growth %>%
  mutate(Years_Since_Start = reporting_yr - cohort_year) %>%
  filter(Years_Since_Start >= 0) %>%
  group_by(cicp_initiative, cohort_year, Years_Since_Start) %>%
  summarise(
    Establishments = sum(est_count_adjusted, na.rm = TRUE),
    Employment = sum(annual_employment, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate survival rate (establishments remaining vs initial cohort)
survival_rates <- survival_data %>%
  group_by(cicp_initiative, cohort_year) %>%
  arrange(Years_Since_Start) %>%
  mutate(
    Initial_Establishments = first(Establishments),
    Survival_Rate = Establishments / Initial_Establishments * 100
  ) %>%
  ungroup()

# Average survival rates by initiative at key milestones
survival_milestones <- survival_rates %>%
  filter(Years_Since_Start %in% c(1, 3, 5, 10), cicp_initiative != "Total") %>%
  group_by(cicp_initiative, Years_Since_Start) %>%
  summarise(
    Avg_Survival_Rate = mean(Survival_Rate, na.rm = TRUE),
    Cohorts_Analyzed = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Years_Since_Start,
    values_from = Avg_Survival_Rate,
    names_prefix = "Year_"
  )

cat("Average Establishment Survival Rates by Initiative (%):\n")
cat("(Percentage of establishments still operating after X years)\n\n")
print(survival_milestones)

################################################################################
# 6B. SURVIVAL ANALYSIS - TRUE STARTUPS VS ALL QUALIFIED ESTABLISHMENTS
################################################################################

cat("\n" %+% paste(rep("=", 80), collapse = "") %+% "\n")
cat("SURVIVAL ANALYSIS: TRUE STARTUPS VS ALL QUALIFIED ESTABLISHMENTS\n")
cat(paste(rep("=", 80), collapse = "") %+% "\n\n")

cat("NOTE: 'True Startups' = Type 1 establishments (1-20 employees at start)\n")
cat("      'All Qualified' = Types 1-4 (all qualified new establishments)\n\n")

# TRUE STARTUPS ONLY (Type 1: 1-20 employees)
cat("Analyzing TRUE STARTUPS (Type 1 only)...\n")

true_startup_survival_data <- estab_growth %>%
  filter(new_est_type == 1) %>%  # Only true startups
  mutate(Years_Since_Start = reporting_yr - cohort_year) %>%
  filter(Years_Since_Start >= 0) %>%
  group_by(cicp_initiative, cohort_year, Years_Since_Start) %>%
  summarise(
    Establishments = sum(est_count_adjusted, na.rm = TRUE),
    Employment = sum(annual_employment, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate survival rate for true startups
true_startup_survival_rates <- true_startup_survival_data %>%
  group_by(cicp_initiative, cohort_year) %>%
  arrange(Years_Since_Start) %>%
  mutate(
    Initial_Establishments = first(Establishments),
    Survival_Rate = Establishments / Initial_Establishments * 100
  ) %>%
  ungroup()

# Average survival rates for TRUE STARTUPS at key milestones
true_startup_milestones <- true_startup_survival_rates %>%
  filter(Years_Since_Start %in% c(1, 3, 5, 10), cicp_initiative != "Total") %>%
  group_by(cicp_initiative, Years_Since_Start) %>%
  summarise(
    Avg_Survival_Rate = mean(Survival_Rate, na.rm = TRUE),
    Cohorts_Analyzed = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Years_Since_Start,
    values_from = Avg_Survival_Rate,
    names_prefix = "Year_"
  )

cat("\nTRUE STARTUP Survival Rates by Initiative (Type 1 only - 1-20 employees):\n")
cat("(Percentage of true startups still operating after X years)\n\n")
print(true_startup_milestones)

# ALL QUALIFIED ESTABLISHMENTS (Types 1-4)
cat("\n\nAnalyzing ALL QUALIFIED ESTABLISHMENTS (Types 1-4)...\n")

all_qualified_survival_data <- estab_growth %>%
  filter(new_est_type %in% c(1, 2, 3, 4)) %>%  # All qualified types
  mutate(Years_Since_Start = reporting_yr - cohort_year) %>%
  filter(Years_Since_Start >= 0) %>%
  group_by(cicp_initiative, cohort_year, Years_Since_Start) %>%
  summarise(
    Establishments = sum(est_count_adjusted, na.rm = TRUE),
    Employment = sum(annual_employment, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate survival rate for all qualified
all_qualified_survival_rates <- all_qualified_survival_data %>%
  group_by(cicp_initiative, cohort_year) %>%
  arrange(Years_Since_Start) %>%
  mutate(
    Initial_Establishments = first(Establishments),
    Survival_Rate = Establishments / Initial_Establishments * 100
  ) %>%
  ungroup()

# Average survival rates for ALL QUALIFIED at key milestones
all_qualified_milestones <- all_qualified_survival_rates %>%
  filter(Years_Since_Start %in% c(1, 3, 5, 10), cicp_initiative != "Total") %>%
  group_by(cicp_initiative, Years_Since_Start) %>%
  summarise(
    Avg_Survival_Rate = mean(Survival_Rate, na.rm = TRUE),
    Cohorts_Analyzed = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Years_Since_Start,
    values_from = Avg_Survival_Rate,
    names_prefix = "Year_"
  )

cat("\nALL QUALIFIED Survival Rates by Initiative (Types 1-4 combined):\n")
cat("(Percentage of all qualified establishments still operating after X years)\n\n")
print(all_qualified_milestones)

# COMPARISON: TRUE STARTUPS VS ALL QUALIFIED
cat("\n\n" %+% paste(rep("-", 80), collapse = "") %+% "\n")
cat("COMPARISON: TRUE STARTUPS vs ALL QUALIFIED\n")
cat(paste(rep("-", 80), collapse = "") %+% "\n\n")

comparison_survival <- bind_rows(
  true_startup_milestones %>% 
    mutate(Category = "True Startups (Type 1: 1-20 employees)"),
  all_qualified_milestones %>% 
    mutate(Category = "All Qualified (Types 1-4)")
) %>%
  select(Category, cicp_initiative, everything(), -Cohorts_Analyzed)

# Calculate the difference in survival rates
# First check which year columns exist
year_cols <- names(true_startup_milestones)[grepl("^Year_", names(true_startup_milestones))]

survival_difference <- true_startup_milestones %>%
  select(cicp_initiative, starts_with("Year_")) %>%
  rename_with(~paste0("TrueStartup_", .), starts_with("Year_")) %>%
  left_join(
    all_qualified_milestones %>%
      select(cicp_initiative, starts_with("Year_")) %>%
      rename_with(~paste0("AllQualified_", .), starts_with("Year_")),
    by = "cicp_initiative"
  )

# Calculate differences for available years
if ("TrueStartup_Year_1" %in% names(survival_difference) && "AllQualified_Year_1" %in% names(survival_difference)) {
  survival_difference <- survival_difference %>%
    mutate(Diff_Year_1 = TrueStartup_Year_1 - AllQualified_Year_1)
}

if ("TrueStartup_Year_3" %in% names(survival_difference) && "AllQualified_Year_3" %in% names(survival_difference)) {
  survival_difference <- survival_difference %>%
    mutate(Diff_Year_3 = TrueStartup_Year_3 - AllQualified_Year_3)
}

if ("TrueStartup_Year_5" %in% names(survival_difference) && "AllQualified_Year_5" %in% names(survival_difference)) {
  survival_difference <- survival_difference %>%
    mutate(Diff_Year_5 = TrueStartup_Year_5 - AllQualified_Year_5)
}

if ("TrueStartup_Year_10" %in% names(survival_difference) && "AllQualified_Year_10" %in% names(survival_difference)) {
  survival_difference <- survival_difference %>%
    mutate(Diff_Year_10 = TrueStartup_Year_10 - AllQualified_Year_10)
}

survival_difference <- survival_difference %>%
  select(cicp_initiative, starts_with("Diff_"))

cat("Survival Rate Comparison by Initiative:\n\n")
print(comparison_survival)

cat("\n\nSurvival Rate DIFFERENCE (True Startups - All Qualified):\n")
cat("Positive values = True startups survive better\n")
cat("Negative values = All qualified establishments survive better\n\n")
print(survival_difference)

# Summary statistics
cat("\n\n" %+% paste(rep("-", 80), collapse = "") %+% "\n")
cat("KEY FINDINGS:\n")
cat(paste(rep("-", 80), collapse = "") %+% "\n\n")

# Calculate average 5-year survival across all initiatives (if Year_5 exists)
if ("Year_5" %in% names(true_startup_milestones) && "Year_5" %in% names(all_qualified_milestones)) {
  avg_true_startup_5yr <- mean(true_startup_milestones$Year_5, na.rm = TRUE)
  avg_all_qualified_5yr <- mean(all_qualified_milestones$Year_5, na.rm = TRUE)
  difference_5yr <- avg_true_startup_5yr - avg_all_qualified_5yr
  
  cat("AVERAGE 5-YEAR SURVIVAL RATES (across all initiatives):\n")
  cat(sprintf("  True Startups (1-20 employees):     %.1f%%\n", avg_true_startup_5yr))
  cat(sprintf("  All Qualified (1-100+ employees):   %.1f%%\n", avg_all_qualified_5yr))
  cat(sprintf("  Difference:                         %.1f percentage points\n\n", difference_5yr))
  
  if (difference_5yr > 0) {
    cat("INTERPRETATION: True startups (1-20 employees) have HIGHER survival rates\n")
    cat("than the broader category of all qualified establishments.\n")
  } else if (difference_5yr < 0) {
    cat("INTERPRETATION: Larger qualified establishments (21+ employees) have HIGHER\n")
    cat("survival rates than true startups (1-20 employees).\n")
  } else {
    cat("INTERPRETATION: Survival rates are similar between true startups and\n")
    cat("larger qualified establishments.\n")
  }
} else {
  # Check what years we do have
  available_years <- year_cols
  cat("NOTE: 5-year survival data not available with current cohort range.\n")
  cat("Available survival milestones: ", paste(gsub("Year_", "", available_years), "years"), "\n\n")
  
  # Use the longest available period
  if ("Year_3" %in% names(true_startup_milestones) && "Year_3" %in% names(all_qualified_milestones)) {
    avg_true_startup_3yr <- mean(true_startup_milestones$Year_3, na.rm = TRUE)
    avg_all_qualified_3yr <- mean(all_qualified_milestones$Year_3, na.rm = TRUE)
    difference_3yr <- avg_true_startup_3yr - avg_all_qualified_3yr
    
    cat("AVERAGE 3-YEAR SURVIVAL RATES (across all initiatives):\n")
    cat(sprintf("  True Startups (1-20 employees):     %.1f%%\n", avg_true_startup_3yr))
    cat(sprintf("  All Qualified (1-100+ employees):   %.1f%%\n", avg_all_qualified_3yr))
    cat(sprintf("  Difference:                         %.1f percentage points\n\n", difference_3yr))
    
    if (difference_3yr > 0) {
      cat("INTERPRETATION: True startups (1-20 employees) have HIGHER 3-year survival rates\n")
      cat("than the broader category of all qualified establishments.\n")
    } else if (difference_3yr < 0) {
      cat("INTERPRETATION: Larger qualified establishments (21+ employees) have HIGHER\n")
      cat("3-year survival rates than true startups (1-20 employees).\n")
    } else {
      cat("INTERPRETATION: 3-year survival rates are similar between true startups and\n")
      cat("larger qualified establishments.\n")
    }
  }
}

cat("\nPOLICY IMPLICATIONS:\n")
cat("- Different support strategies may be needed for micro-startups vs larger\n")
cat("  new establishments\n")
cat("- Survival patterns can inform resource allocation for entrepreneurship programs\n")
cat("- Consider whether initiatives should focus on quantity (more startups) or\n")
cat("  quality (higher survival rates)\n")

################################################################################
# 7. GEOGRAPHIC PATTERNS BY ECONOMIC GROWTH REGION
################################################################################

cat("\n" %+% paste(rep("=", 80), collapse = "") %+% "\n")
cat("GEOGRAPHIC PATTERNS: ESTABLISHMENT GROWTH BY ECONOMIC GROWTH REGION\n")
cat(paste(rep("=", 80), collapse = "") %+% "\n\n")

# Recent year analysis by economic growth region (aggregate all sectors)
egr_initiative_summary <- estab_growth %>%
  filter(reporting_yr == most_recent_year) %>%
  group_by(egr, cicp_initiative) %>%
  summarise(
    Establishments = sum(est_count_adjusted, na.rm = TRUE),
    Employment = sum(annual_employment, na.rm = TRUE),
    Avg_Wage = weighted.mean(avg_annual_wage, annual_employment, na.rm = TRUE),
    Counties = n_distinct(county_desc),
    .groups = "drop"
  ) %>%
  arrange(egr, desc(Establishments))

cat("Establishment Distribution by EGR and Initiative (", 
    most_recent_year, "):\n\n", sep = "")
print(egr_initiative_summary %>% filter(cicp_initiative != "Total"))

# EGR totals
egr_totals <- estab_growth %>%
  filter(reporting_yr == most_recent_year, cicp_initiative == "Total") %>%
  group_by(egr) %>%
  summarise(
    Total_Establishments = sum(est_count_adjusted, na.rm = TRUE),
    Total_Employment = sum(annual_employment, na.rm = TRUE),
    Avg_Wage = weighted.mean(avg_annual_wage, annual_employment, na.rm = TRUE),
    Counties = n_distinct(county_desc),
    .groups = "drop"
  ) %>%
  arrange(egr)

cat("\n\nEconomic Growth Region Totals:\n")
print(egr_totals)

################################################################################
# 7B. GEOGRAPHIC PATTERNS BY MSA (METRO AREAS)
################################################################################

cat("\n" %+% paste(rep("=", 80), collapse = "") %+% "\n")
cat("GEOGRAPHIC PATTERNS: ESTABLISHMENT GROWTH BY MSA (METRO AREAS)\n")
cat(paste(rep("=", 80), collapse = "") %+% "\n\n")

# Recent year analysis by MSA (aggregate all sectors)
msa_initiative_summary <- estab_growth %>%
  filter(reporting_yr == most_recent_year, 
         !is.na(msa), msa != "NULL") %>%
  group_by(msa, cicp_initiative) %>%
  summarise(
    Establishments = sum(est_count_adjusted, na.rm = TRUE),
    Employment = sum(annual_employment, na.rm = TRUE),
    Avg_Wage = weighted.mean(avg_annual_wage, annual_employment, na.rm = TRUE),
    Counties = n_distinct(county_desc),
    .groups = "drop"
  ) %>%
  arrange(msa, desc(Establishments))

cat("Establishment Distribution by MSA and Initiative (", 
    most_recent_year, "):\n\n", sep = "")
print(msa_initiative_summary %>% filter(cicp_initiative != "Total"))

# MSA totals
msa_totals <- estab_growth %>%
  filter(reporting_yr == most_recent_year, 
         cicp_initiative == "Total", !is.na(msa), msa != "NULL") %>%
  group_by(msa) %>%
  summarise(
    Total_Establishments = sum(est_count_adjusted, na.rm = TRUE),
    Total_Employment = sum(annual_employment, na.rm = TRUE),
    Avg_Wage = weighted.mean(avg_annual_wage, annual_employment, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Establishments))

cat("\n\nMSA Totals:\n")
print(msa_totals)

################################################################################
# 8. GROWTH RATE CALCULATIONS
################################################################################

cat("\n" %+% paste(rep("=", 80), collapse = "") %+% "\n")
cat("ESTABLISHMENT GROWTH RATES\n")
cat(paste(rep("=", 80), collapse = "") %+% "\n\n")

# Calculate compound annual growth rate (CAGR) for new establishments
min_year <- min(estab_growth$cohort_year, na.rm = TRUE)
max_year <- max(estab_growth$cohort_year, na.rm = TRUE)

# Annual new establishments by initiative
growth_trend <- new_estabs_by_year %>%
  filter(cicp_initiative != "Total") %>%
  group_by(cicp_initiative) %>%
  arrange(cohort_year) %>%
  summarise(
    First_Year = first(cohort_year),
    Last_Year = last(cohort_year),
    Initial_New_Estabs = first(New_Establishments),
    Recent_New_Estabs = last(New_Establishments),
    Years = Last_Year - First_Year,
    CAGR = ((Recent_New_Estabs / Initial_New_Estabs)^(1/Years) - 1) * 100,
    Total_New_Over_Period = sum(New_Establishments),
    .groups = "drop"
  ) %>%
  arrange(desc(CAGR))

cat("Compound Annual Growth Rate (CAGR) for New Establishments:\n")
cat("Period:", min_year, "to", max_year, "\n\n")
print(growth_trend)

################################################################################
# 8B. YEAR-OVER-YEAR ESTABLISHMENT GROWTH ANALYSIS
################################################################################

cat("\n" %+% paste(rep("=", 80), collapse = "") %+% "\n")
cat("YEAR-OVER-YEAR ESTABLISHMENT GROWTH ANALYSIS\n")
cat(paste(rep("=", 80), collapse = "") %+% "\n\n")

# Calculate total establishments by year and initiative (aggregate all sectors)
yoy_establishment_growth <- estab_growth %>%
  group_by(reporting_yr, cicp_initiative) %>%
  summarise(
    Total_Establishments = sum(est_count_adjusted, na.rm = TRUE),
    Total_Employment = sum(annual_employment, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(cicp_initiative) %>%
  arrange(reporting_yr) %>%
  mutate(
    Previous_Year_Estabs = lag(Total_Establishments),
    YoY_Estab_Change = Total_Establishments - Previous_Year_Estabs,
    YoY_Estab_Pct_Change = (Total_Establishments / Previous_Year_Estabs - 1) * 100,
    Previous_Year_Employment = lag(Total_Employment),
    YoY_Employment_Change = Total_Employment - Previous_Year_Employment,
    YoY_Employment_Pct_Change = (Total_Employment / Previous_Year_Employment - 1) * 100
  ) %>%
  ungroup()

# View recent years
cat("Year-over-Year Establishment Growth (Recent Years):\n\n")
print(yoy_establishment_growth %>% 
        filter(reporting_yr >= 2015, cicp_initiative != "Total") %>%
        select(reporting_yr, cicp_initiative, Total_Establishments, 
               YoY_Estab_Change, YoY_Estab_Pct_Change))

# Summary by initiative - average annual growth
yoy_summary_by_initiative <- yoy_establishment_growth %>%
  filter(!is.na(YoY_Estab_Pct_Change), cicp_initiative != "Total") %>%
  group_by(cicp_initiative) %>%
  summarise(
    Years_Analyzed = n(),
    Avg_Annual_Growth_Rate = mean(YoY_Estab_Pct_Change, na.rm = TRUE),
    Median_Annual_Growth = median(YoY_Estab_Pct_Change, na.rm = TRUE),
    Total_Growth_Period = (last(Total_Establishments) / first(Total_Establishments) - 1) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(Avg_Annual_Growth_Rate))

cat("\n\nAverage Annual Growth Rates by Initiative:\n")
print(yoy_summary_by_initiative)

# Overall totals (all initiatives combined, using Total row)
yoy_total <- estab_growth %>%
  filter(cicp_initiative == "Total") %>%
  group_by(reporting_yr) %>%
  summarise(
    Total_Establishments = sum(est_count_adjusted, na.rm = TRUE),
    Total_Employment = sum(annual_employment, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(reporting_yr) %>%
  mutate(
    Previous_Year_Estabs = lag(Total_Establishments),
    YoY_Estab_Change = Total_Establishments - Previous_Year_Estabs,
    YoY_Estab_Pct_Change = (Total_Establishments / Previous_Year_Estabs - 1) * 100
  )

cat("\n\nOverall Year-over-Year Growth (All Initiatives Combined):\n")
print(yoy_total)

################################################################################
# 9. EMPLOYMENT CHARACTERISTICS OF NEW ESTABLISHMENTS
################################################################################

cat("\n" %+% paste(rep("=", 80), collapse = "") %+% "\n")
cat("EMPLOYMENT CHARACTERISTICS OF NEW ESTABLISHMENTS\n")
cat(paste(rep("=", 80), collapse = "") %+% "\n\n")

# Analyze initial size and wages of new establishments (aggregate all sectors)
new_estab_characteristics <- estab_growth %>%
  filter(cohort_year == reporting_yr) %>%
  group_by(cicp_initiative, cohort_year) %>%
  summarise(
    Total_New_Estabs = sum(est_count_adjusted, na.rm = TRUE),
    Total_New_Employment = sum(annual_employment, na.rm = TRUE),
    Avg_Initial_Employment = Total_New_Employment / Total_New_Estabs,
    Median_Initial_Wage = median(avg_annual_wage, na.rm = TRUE),
    .groups = "drop"
  )

# Average across all cohorts by initiative
avg_characteristics <- new_estab_characteristics %>%
  filter(cicp_initiative != "Total") %>%
  group_by(cicp_initiative) %>%
  summarise(
    Avg_Employees_Per_New_Estab = mean(Avg_Initial_Employment, na.rm = TRUE),
    Avg_Starting_Wage = mean(Median_Initial_Wage, na.rm = TRUE),
    Total_New_Jobs_Created = sum(Total_New_Employment, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Avg_Employees_Per_New_Estab))

cat("Average Characteristics of New Establishments by Initiative:\n\n")
print(avg_characteristics)

################################################################################
# 10. TOP COUNTIES FOR NEW ESTABLISHMENT ACTIVITY
################################################################################

cat("\n" %+% paste(rep("=", 80), collapse = "") %+% "\n")
cat("TOP COUNTIES FOR NEW ESTABLISHMENT ACTIVITY\n")
cat(paste(rep("=", 80), collapse = "") %+% "\n\n")

# Counties with most new establishments (last 5 years)
recent_years <- (most_recent_year - 4):most_recent_year

top_counties_new <- estab_growth %>%
  filter(
    cohort_year %in% recent_years,
    cohort_year == reporting_yr,
    cicp_initiative == "Total"
  ) %>%
  group_by(county_desc, egr) %>%
  summarise(
    New_Establishments = sum(est_count_adjusted, na.rm = TRUE),
    New_Jobs = sum(annual_employment, na.rm = TRUE),
    Avg_Wage = weighted.mean(avg_annual_wage, annual_employment, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(New_Establishments)) %>%
  head(20)

cat("Top 20 Counties by New Establishments (Last 5 Years):\n")
cat("Period:", paste(recent_years, collapse = ", "), "\n\n")
print(top_counties_new)

################################################################################
# 11. YEAR-OVER-YEAR CHANGES IN NEW ESTABLISHMENTS
################################################################################

cat("\n" %+% paste(rep("=", 80), collapse = "") %+% "\n")
cat("YEAR-OVER-YEAR CHANGES IN NEW ESTABLISHMENTS\n")
cat(paste(rep("=", 80), collapse = "") %+% "\n\n")

# Calculate YoY changes in new establishment formation
yoy_changes <- new_estabs_by_year %>%
  filter(cicp_initiative != "Total") %>%
  group_by(cicp_initiative) %>%
  arrange(cohort_year) %>%
  mutate(
    Previous_Year_New = lag(New_Establishments),
    YoY_Change = New_Establishments - Previous_Year_New,
    YoY_Pct_Change = (New_Establishments / Previous_Year_New - 1) * 100
  ) %>%
  ungroup()

# Recent YoY changes
recent_yoy <- yoy_changes %>%
  filter(cohort_year >= most_recent_year - 3) %>%
  select(cohort_year, cicp_initiative, New_Establishments, YoY_Change, YoY_Pct_Change) %>%
  arrange(desc(cohort_year), cicp_initiative)

cat("Recent Year-over-Year Changes in New Establishments:\n\n")
print(recent_yoy)

################################################################################
# 12. SUMMARY STATISTICS FOR REPORT
################################################################################

cat("\n" %+% paste(rep("=", 80), collapse = "") %+% "\n")
cat("KEY SUMMARY STATISTICS FOR CICP INNOVATION REPORT\n")
cat(paste(rep("=", 80), collapse = "") %+% "\n\n")

# Overall summary metrics
summary_stats <- list(
  "Total Establishments (Most Recent Year)" = sum(estab_growth$est_count_adjusted[
    estab_growth$reporting_yr == most_recent_year & 
    estab_growth$cicp_initiative == "Total"], na.rm = TRUE),
  "Total Employment (Most Recent Year)" = sum(estab_growth$annual_employment[
    estab_growth$reporting_yr == most_recent_year & 
    estab_growth$cicp_initiative == "Total"], na.rm = TRUE),
  "Counties Analyzed" = n_distinct(estab_growth$county_desc),
  "Initiatives Tracked" = n_distinct(estab_growth$cicp_initiative[estab_growth$cicp_initiative != "Total"]),
  "Cohort Years Covered" = paste(min(estab_growth$cohort_year, na.rm = TRUE), 
                                  "to", 
                                  max(estab_growth$cohort_year, na.rm = TRUE)),
  "Reporting Years Covered" = paste(min(estab_growth$reporting_yr, na.rm = TRUE), 
                                    "to", 
                                    max(estab_growth$reporting_yr, na.rm = TRUE))
)

cat("Overall Summary:\n")
for (name in names(summary_stats)) {
  cat(sprintf("%-45s: %s\n", name, format(summary_stats[[name]], big.mark = ",")))
}

################################################################################
# 13. DATA EXPORT FOR FURTHER ANALYSIS (Main Aggregations)
################################################################################

cat("\n\nExporting key datasets for visualization and reporting...\n")

# Create output directory with year suffix if it doesn't exist
output_dir <- paste0("estab_output_", estab_recent_year)
if (!dir.exists(output_dir)) dir.create(output_dir)

# Export key summary tables - Main aggregations
write_csv(recent_initiative_summary, file.path(output_dir, "estab_initiative_summary.csv"))
write_csv(statewide_init_sect_summary, file.path(output_dir, "estab_statewide_init_sect_sector.csv"))
write_csv(egr_init_sect_summary, file.path(output_dir, "estab_egr_init_sect_sector.csv"))
write_csv(msa_init_sect_summary, file.path(output_dir, "estab_msa_init_sect_sector.csv"))
write_csv(new_estabs_by_year, file.path(output_dir, "estab_new_by_year_initiative.csv"))
write_csv(survival_milestones, file.path(output_dir, "estab_survival_rates.csv"))
write_csv(egr_initiative_summary, file.path(output_dir, "estab_egr_initiative_summary.csv"))
write_csv(msa_initiative_summary, file.path(output_dir, "estab_msa_initiative_summary.csv"))
write_csv(growth_trend, file.path(output_dir, "estab_growth_rates.csv"))
write_csv(yoy_establishment_growth, file.path(output_dir, "estab_yoy_growth.csv"))

# Export survival comparison data
write_csv(true_startup_survival_rates, 
          file.path(output_dir, "survival_true_startups_detailed.csv"))
write_csv(all_qualified_survival_rates, 
          file.path(output_dir, "survival_all_qualified_detailed.csv"))
write_csv(true_startup_milestones, 
          file.path(output_dir, "survival_true_startups_milestones.csv"))
write_csv(all_qualified_milestones, 
          file.path(output_dir, "survival_all_qualified_milestones.csv"))
write_csv(comparison_survival, 
          file.path(output_dir, "survival_comparison.csv"))
write_csv(survival_difference, 
          file.path(output_dir, "survival_difference.csv"))

################################################################################
# 14. NEW ESTABLISHMENT TYPE AGGREGATIONS
################################################################################

cat("\n" %+% paste(rep("=", 80), collapse = "") %+% "\n")
cat("NEW ESTABLISHMENT TYPE ANALYSIS\n")
cat(paste(rep("=", 80), collapse = "") %+% "\n\n")

# STATEWIDE by initiative and new_est_type (aggregate all sectors)
statewide_by_esttype <- estab_growth %>%
  filter(reporting_yr == estab_recent_year) %>%
  group_by(cicp_initiative, new_est_type, new_est_type_desc) %>%
  summarise(
    Total_Establishments = sum(est_count_adjusted, na.rm = TRUE),
    Total_Employment = sum(annual_employment, na.rm = TRUE),
    Total_Wages = sum(annual_wages, na.rm = TRUE),
    Avg_Wage = weighted.mean(avg_annual_wage, annual_employment, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(cicp_initiative, new_est_type)

cat("Statewide by Initiative and New Establishment Type (", estab_recent_year, "):\n\n", sep = "")
print(statewide_by_esttype)

# EGR by initiative and new_est_type (aggregate all sectors)
egr_by_esttype <- estab_growth %>%
  filter(reporting_yr == estab_recent_year) %>%
  group_by(egr, cicp_initiative, new_est_type, new_est_type_desc) %>%
  summarise(
    Total_Establishments = sum(est_count_adjusted, na.rm = TRUE),
    Total_Employment = sum(annual_employment, na.rm = TRUE),
    Total_Wages = sum(annual_wages, na.rm = TRUE),
    Avg_Wage = weighted.mean(avg_annual_wage, annual_employment, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(egr, cicp_initiative, new_est_type)

cat("\n\nEGR by Initiative and New Establishment Type (", estab_recent_year, "):\n\n", sep = "")
print(head(egr_by_esttype, 20))

# MSA by initiative and new_est_type (aggregate all sectors)
msa_by_esttype <- estab_growth %>%
  filter(reporting_yr == estab_recent_year,
         !is.na(msa), msa != "NULL") %>%
  group_by(msa, cicp_initiative, new_est_type, new_est_type_desc) %>%
  summarise(
    Total_Establishments = sum(est_count_adjusted, na.rm = TRUE),
    Total_Employment = sum(annual_employment, na.rm = TRUE),
    Total_Wages = sum(annual_wages, na.rm = TRUE),
    Avg_Wage = weighted.mean(avg_annual_wage, annual_employment, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(msa, cicp_initiative, new_est_type)

cat("\n\nMSA by Initiative and New Establishment Type (", estab_recent_year, "):\n\n", sep = "")
print(head(msa_by_esttype, 20))

# COHORT ANALYSIS by new_est_type (aggregate all sectors)
new_estabs_by_year_type <- estab_growth %>%
  filter(cohort_year == reporting_yr) %>%
  group_by(cohort_year, cicp_initiative, new_est_type, new_est_type_desc) %>%
  summarise(
    New_Establishments = sum(est_count_adjusted, na.rm = TRUE),
    Initial_Employment = sum(annual_employment, na.rm = TRUE),
    Initial_Wages = sum(annual_wages, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(cohort_year), cicp_initiative, new_est_type)

cat("\n\nNew Establishments by Cohort Year, Initiative, and Type:\n\n")
print(head(new_estabs_by_year_type, 30))

# SURVIVAL ANALYSIS by new_est_type (aggregate all sectors)
survival_by_esttype <- estab_growth %>%
  mutate(Years_Since_Start = reporting_yr - cohort_year) %>%
  filter(Years_Since_Start >= 0, Years_Since_Start <= 10) %>%
  group_by(cicp_initiative, new_est_type, new_est_type_desc, cohort_year, Years_Since_Start) %>%
  summarise(
    Establishments = sum(est_count_adjusted, na.rm = TRUE),
    Employment = sum(annual_employment, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(cicp_initiative, new_est_type, new_est_type_desc, cohort_year) %>%
  arrange(Years_Since_Start) %>%
  mutate(
    Initial_Establishments = first(Establishments),
    Survival_Rate = Establishments / Initial_Establishments * 100
  ) %>%
  ungroup()

# Average survival rates by initiative and est type at key milestones
survival_milestones_by_type <- survival_by_esttype %>%
  filter(Years_Since_Start %in% c(1, 3, 5, 10), cicp_initiative != "Total") %>%
  group_by(cicp_initiative, new_est_type, new_est_type_desc, Years_Since_Start) %>%
  summarise(
    Avg_Survival_Rate = mean(Survival_Rate, na.rm = TRUE),
    Cohorts_Analyzed = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Years_Since_Start,
    values_from = Avg_Survival_Rate,
    names_prefix = "Year_"
  )

cat("\n\nSurvival Rates by Initiative and New Establishment Type:\n\n")
print(survival_milestones_by_type)

# Export new establishment type aggregations
write_csv(statewide_by_esttype, file.path(output_dir, "estab_statewide_by_esttype.csv"))
write_csv(egr_by_esttype, file.path(output_dir, "estab_egr_by_esttype.csv"))
write_csv(msa_by_esttype, file.path(output_dir, "estab_msa_by_esttype.csv"))
write_csv(new_estabs_by_year_type, file.path(output_dir, "estab_new_by_year_esttype.csv"))
write_csv(survival_milestones_by_type, file.path(output_dir, "estab_survival_by_esttype.csv"))

cat("\n\nAll exports complete. Files saved to '", output_dir, "/' directory.\n", sep = "")
cat("\nExported files:\n")
cat("  - Main aggregations (initiative-level): 10 files\n")
cat("  - Sector-level detail: 3 files (statewide, EGR, MSA)\n")
cat("  - Survival comparison: 6 files\n")
cat("  - New establishment type aggregations: 5 files\n")
cat("  Total: 24 CSV files\n")

cat("\n" %+% paste(rep("=", 80), collapse = "") %+% "\n")
cat("ANALYSIS COMPLETE\n")
cat(paste(rep("=", 80), collapse = "") %+% "\n")
cat("\nNext Steps:\n")
cat("1. Review summary statistics and identify key insights\n")
cat("2. Create visualizations using the exported datasets\n")
cat("3. Compare establishment growth across initiatives\n")
cat("4. Analyze survival rates and their policy implications\n")
cat("5. Examine geographic patterns by EGR and MSA\n")
cat("6. Analyze new establishment types and size categories\n")
cat("7. Compare true startups vs all qualified establishments\n")
cat("8. Integrate findings into CICP Innovation Report\n")