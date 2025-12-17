################################################################################
# CICP Establishment Growth Visualizations
# Selected visualizations based on establishment startup data
################################################################################

library(tidyverse)
library(plotly)
library(scales)
library(viridis)
library(patchwork)

# Set theme
theme_set(theme_minimal())

# Create output directory for plots
plots_dir <- paste0("estab_output_", estab_recent_year, "/plots")
if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)

# CORRECT Color palette for CICP initiatives (updated)
initiative_colors <- c(
  "Advanced & Traded Industries" = "#1f77b4",  # Blue
  "BioX" = "#ff7f0e",                          # Orange
  "TechPoint" = "#2ca02c",                     # Green
  "AgriNovus" = "#d62728",                     # Red
  "Conexus" = "#9467bd",                       # Purple
  "Total" = "#7f7f7f"                          # Gray
)

################################################################################
# VIZ A: NEW ESTABLISHMENT FORMATION OVER TIME (2018-2022)
################################################################################

cat("\nCreating Visualization A: New Establishment Formation Over Time...\n")

# Prepare data: filter to 2018+ and exclude Total and Other
viz_a_data <- new_estabs_by_year %>%
  filter(cohort_year >= 2018, 
         cicp_initiative != "Total",
         cicp_initiative != "Other")

# Static version
p_a_static <- viz_a_data %>%
  ggplot(aes(x = cohort_year, y = New_Establishments, color = cicp_initiative, group = cicp_initiative)) +
  geom_line(linewidth = 1.2, alpha = 0.9) +
  geom_point(size = 3, alpha = 0.9) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) +
  scale_x_continuous(breaks = 2018:2022) +
  scale_color_manual(values = initiative_colors) +
  labs(
    title = "New Establishment Formation by Initiative (2018-2022)",
    subtitle = "Annual count of newly formed establishments tracked in their birth year",
    x = "Cohort Year",
    y = "Number of New Establishments",
    color = "Initiative",
    caption = "Source: IBRC Longitudinal Database, via BLS | Excludes establishments not qualified for tracking"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

ggsave(file.path(plots_dir, "A_new_establishments_timeline_static.png"), 
       p_a_static, width = 12, height = 7, dpi = 300, bg = "white")

# Interactive version with Plotly
p_a_interactive <- plot_ly(viz_a_data, 
                           x = ~cohort_year, 
                           y = ~New_Establishments,
                           color = ~cicp_initiative,
                           colors = initiative_colors,
                           type = 'scatter',
                           mode = 'lines+markers',
                           line = list(width = 3),
                           marker = list(size = 8),
                           hovertemplate = paste(
                             "<b>%{fullData.name}</b><br>",
                             "Year: %{x}<br>",
                             "New Establishments: %{y:,}<br>",
                             "<extra></extra>"
                           )) %>%
  layout(
    title = list(
      text = "New Establishment Formation by Initiative (2018-2022)<br><sub>Click legend to toggle initiatives on/off</sub>",
      font = list(size = 16)
    ),
    xaxis = list(title = "Cohort Year", dtick = 1),
    yaxis = list(title = "Number of New Establishments", separatethousands = TRUE),
    hovermode = "x unified",
    legend = list(title = list(text = "Initiative"))
  )

htmlwidgets::saveWidget(p_a_interactive, 
                       file.path(plots_dir, "A_new_establishments_timeline_interactive.html"),
                       selfcontained = TRUE)

cat("✓ Visualization A complete\n")

################################################################################
# VIZ B: YEAR-OVER-YEAR GROWTH RATE HEATMAP (2018-2022)
################################################################################

cat("\nCreating Visualization B: YoY Growth Rate Heatmap (2018-2022)...\n")

# Prepare data: YoY changes for 2018-2022, exclude Total and Other
viz_b_data <- yoy_establishment_growth %>%
  filter(reporting_yr %in% c(2018, 2019, 2020, 2021, 2022), 
         cicp_initiative != "Total",
         cicp_initiative != "Other") %>%
  select(reporting_yr, cicp_initiative, YoY_Estab_Pct_Change, Total_Establishments) %>%
  mutate(
    YoY_Label = sprintf("%.1f%%", YoY_Estab_Pct_Change),
    YoY_Category = case_when(
      YoY_Estab_Pct_Change >= 10 ~ "Strong Growth (≥10%)",
      YoY_Estab_Pct_Change >= 0 ~ "Modest Growth (0-10%)",
      YoY_Estab_Pct_Change >= -5 ~ "Slight Decline (0 to -5%)",
      TRUE ~ "Significant Decline (<-5%)"
    )
  )

# Static heatmap
p_b_static <- viz_b_data %>%
  ggplot(aes(x = factor(reporting_yr), y = cicp_initiative, fill = YoY_Estab_Pct_Change)) +
  geom_tile(color = "white", linewidth = 2) +
  geom_text(aes(label = YoY_Label), color = "white", fontface = "bold", size = 5) +
  scale_fill_gradient2(
    low = "#d73027",
    mid = "#ffffbf", 
    high = "#1a9850",
    midpoint = 0,
    limits = c(-10, 10),
    oob = scales::squish,
    labels = function(x) paste0(x, "%"),
    name = "YoY Growth Rate"
  ) +
  labs(
    title = "Year-over-Year Establishment Growth Rates (2018-2022)",
    subtitle = "Green = growth, Red = decline | Percentage change in total establishments",
    x = "Year",
    y = NULL,
    caption = "Source: IBRC Longitudinal Database, via BLS"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 11),
    panel.grid = element_blank(),
    legend.position = "right"
  )

ggsave(file.path(plots_dir, "B_yoy_growth_heatmap_static.png"), 
       p_b_static, width = 10, height = 6, dpi = 300, bg = "white")

# Interactive heatmap with white grid lines
p_b_interactive <- plot_ly(
  data = viz_b_data,
  x = ~factor(reporting_yr),
  y = ~cicp_initiative,
  z = ~YoY_Estab_Pct_Change,
  type = "heatmap",
  colorscale = list(
    c(0, "#d73027"),      # Red for negative
    c(0.5, "#ffffbf"),    # Yellow for zero
    c(1, "#1a9850")       # Green for positive
  ),
  zmid = 0,
  zmin = -10,
  zmax = 10,
  colorbar = list(
    title = "YoY Growth<br>Rate (%)",
    ticksuffix = "%"
  ),
  text = ~paste0(
    "<b>", cicp_initiative, "</b><br>",
    "Year: ", reporting_yr, "<br>",
    "YoY Change: ", YoY_Label, "<br>",
    "Total Establishments: ", scales::comma(Total_Establishments), "<br>",
    "Category: ", YoY_Category
  ),
  hovertemplate = "%{text}<extra></extra>",
  xgap = 3,  # Add white space between columns
  ygap = 3   # Add white space between rows
) %>%
  layout(
    title = list(
      text = "Year-over-Year Establishment Growth Rates (2018-2022)<br><sub>Green = growth, Red = decline | Hover for details</sub>",
      font = list(size = 16)
    ),
    xaxis = list(
      title = "Year",
      tickfont = list(size = 12)
    ),
    yaxis = list(
      title = "",
      tickfont = list(size = 12)
    ),
    margin = list(l = 200),
    plot_bgcolor = 'white',  # White background to show gaps
    paper_bgcolor = 'white'
  )

htmlwidgets::saveWidget(p_b_interactive, 
                       file.path(plots_dir, "B_yoy_growth_heatmap_interactive.html"),
                       selfcontained = TRUE)

cat("✓ Visualization B complete (static + interactive)\n")

################################################################################
# VIZ H: SURVIVAL BY ESTABLISHMENT SIZE (2015+) - FIXED
################################################################################

cat("\nCreating Visualization H: Survival by Establishment Size...\n")

# Prepare survival data by establishment type, filtered to 2015+, exclude Other
viz_h_data <- survival_by_esttype %>%
  filter(
    cohort_year >= 2015,
    Years_Since_Start <= 10,
    cicp_initiative != "Total",
    cicp_initiative != "Other"
  ) %>%
  group_by(new_est_type_desc, Years_Since_Start, cicp_initiative) %>%
  summarise(
    Mean_Survival_Rate = mean(Survival_Rate, na.rm = TRUE),
    SE = sd(Survival_Rate, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  # Cap values at 100% to prevent display issues
  mutate(
    Mean_Survival_Rate = pmin(Mean_Survival_Rate, 100),
    SE = replace_na(SE, 0)  # Replace NA standard errors with 0
  )

# Static version - multi-panel survival curves
p_h_static <- viz_h_data %>%
  ggplot(aes(x = Years_Since_Start, y = Mean_Survival_Rate, 
             color = new_est_type_desc, fill = new_est_type_desc)) +
  geom_line(linewidth = 1.2, alpha = 0.9) +
  geom_point(size = 2.5) +
  geom_ribbon(aes(ymin = pmax(Mean_Survival_Rate - SE, 0), 
                  ymax = pmin(Mean_Survival_Rate + SE, 100)),
              alpha = 0.2, color = NA) +
  facet_wrap(vars(cicp_initiative)) + 
  scale_x_continuous(breaks = 0:10) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), 
                    limits = c(0, 100),
                    breaks = seq(0, 100, 20)) +
  scale_color_viridis_d(option = "plasma", end = 0.9) +
  scale_fill_viridis_d(option = "plasma", end = 0.9) +
  labs(
    title = "Establishment Survival Rates by Initial Size (2015-2022 Cohorts)",
    subtitle = "Percentage of establishments still operating after X years | Shaded area = standard error",
    x = "Years Since Establishment",
    y = "Survival Rate (%)",
    color = "Initial Size Category",
    fill = "Initial Size Category",
    caption = "Source: IBRC Longitudinal Database, via BLS | All initiatives combined"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray30"),
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_legend(nrow = 2), fill = guide_legend(nrow = 2))

ggsave(file.path(plots_dir, "H_survival_by_size_static.png"), 
       p_h_static, width = 12, height = 8, dpi = 300, bg = "white")

# Interactive version with faceting by initiative
p_h_interactive <- plot_ly()

# Get unique initiatives and size categories
initiatives <- unique(viz_h_data$cicp_initiative)
all_size_categories <- unique(viz_h_data$new_est_type_desc)
n_initiatives <- length(initiatives)

# Calculate subplot layout (2 columns)
n_cols <- 2
n_rows <- ceiling(n_initiatives / n_cols)

# Calculate max years available in the data
max_years <- max(viz_h_data$Years_Since_Start, na.rm = TRUE)

# Create consistent color mapping for all size categories
color_palette <- viridis::viridis(length(all_size_categories), option = "plasma", end = 0.9)
color_map <- setNames(color_palette, all_size_categories)

# Prepare data with establishment counts for hover text
viz_h_data_with_counts <- survival_by_esttype %>%
  filter(
    cohort_year >= 2015,
    Years_Since_Start <= max_years,
    cicp_initiative != "Total",
    cicp_initiative != "Other"
  ) %>%
  group_by(new_est_type_desc, Years_Since_Start, cicp_initiative) %>%
  summarise(
    Mean_Survival_Rate = mean(Survival_Rate, na.rm = TRUE),
    SE = sd(Survival_Rate, na.rm = TRUE) / sqrt(n()),
    Count = sum(Establishments, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Mean_Survival_Rate = pmin(Mean_Survival_Rate, 100),
    SE = replace_na(SE, 0),
    Count_Display = ifelse(Count > 5, as.character(round(Count)), "<5")
  )

# Create a trace for each size category × initiative combination
for(init in initiatives) {
  for(size_cat in all_size_categories) {  # Loop through ALL categories to maintain order
    data_subset <- viz_h_data_with_counts %>% 
      filter(cicp_initiative == init, new_est_type_desc == size_cat)
    
    if(nrow(data_subset) > 0) {
      # Determine which subplot this goes in
      init_index <- which(initiatives == init)
      row_num <- ceiling(init_index / n_cols)
      col_num <- ((init_index - 1) %% n_cols) + 1
      
      p_h_interactive <- p_h_interactive %>%
        add_trace(
          data = data_subset,
          x = ~Years_Since_Start,
          y = ~Mean_Survival_Rate,
          name = size_cat,
          type = 'scatter',
          mode = 'lines+markers',
          line = list(width = 3, color = color_map[size_cat]),  # Explicit color assignment
          marker = list(size = 8, color = color_map[size_cat]),  # Explicit color assignment
          legendgroup = size_cat,
          showlegend = (init_index == 1),
          xaxis = paste0('x', ifelse(init_index == 1, '', init_index)),
          yaxis = paste0('y', ifelse(init_index == 1, '', init_index)),
          hovertemplate = paste(
            "<b>", size_cat, "</b><br>",
            "Years: %{x}<br>",
            "Survival Rate: %{y:.1f}%<br>",
            "Establishments: %{text}<br>",
            "<extra></extra>"
          ),
          text = ~Count_Display
        )
    }
  }
}

# Create subplot annotations for initiative names
annotations <- lapply(seq_along(initiatives), function(i) {
  row_num <- ceiling(i / n_cols)
  col_num <- ((i - 1) %% n_cols) + 1
  
  list(
    x = (col_num - 0.5) / n_cols,
    y = 1 - (row_num - 1) / n_rows - 0.02,
    text = paste0("<b>", initiatives[i], "</b>"),
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE,
    font = list(size = 12)
  )
})

# Build subplot layout
subplot_layout <- list()
for(i in seq_along(initiatives)) {
  row_num <- ceiling(i / n_cols)
  col_num <- ((i - 1) %% n_cols) + 1
  
  # X-axis (limited to actual data range)
  x_domain <- c((col_num - 1) / n_cols + 0.02, col_num / n_cols - 0.02)
  subplot_layout[[paste0('xaxis', ifelse(i == 1, '', i))]] <- list(
    domain = x_domain,
    title = if(row_num == n_rows) "Years Since Establishment" else "",
    range = c(-0.3, max_years + 0.3),
    dtick = 1
  )
  
  # Y-axis (extend slightly above 100% to prevent cutoff)
  y_domain <- c(1 - row_num / n_rows + 0.08, 1 - (row_num - 1) / n_rows - 0.05)
  subplot_layout[[paste0('yaxis', ifelse(i == 1, '', i))]] <- list(
    domain = y_domain,
    title = if(col_num == 1) "Survival Rate (%)" else "",
    range = c(0, 103)
  )
}

p_h_interactive <- p_h_interactive %>%
  layout(
    title = list(
      text = "Establishment Survival Rates by Initial Size and Initiative (2015-2022 Cohorts)<br><sub>Click legend to toggle size categories</sub>",
      font = list(size = 16)
    ),
    annotations = annotations,
    hovermode = "closest",
    legend = list(
      orientation = "h",
      yanchor = "bottom",
      y = -0.15,
      xanchor = "center",
      x = 0.5
    )
  )

# Apply subplot layout
p_h_interactive <- do.call(layout, c(list(p_h_interactive), subplot_layout))

htmlwidgets::saveWidget(p_h_interactive, 
                       file.path(plots_dir, "H_survival_by_size_interactive.html"),
                       selfcontained = TRUE)

cat("✓ Visualization H complete\n")

################################################################################
# VIZ J: BUBBLE CHART - GROWTH VS SURVIVAL VS SCALE (WITH SIZE DROPDOWN)
################################################################################

cat("\nCreating Visualization J: Growth vs Survival vs Scale Bubble Chart...\n")

# Define size categories based on annual employment
create_size_category <- function(employment) {
  case_when(
    employment <= 20 ~ "Small (1-20 employees)",
    employment <= 50 ~ "Medium (21-50 employees)",
    TRUE ~ "Large (51+ employees)"
  )
}

# Prepare data for "All Establishments" view (current viz)
viz_j_growth_all <- growth_trend %>%
  filter(cicp_initiative != "Total", cicp_initiative != "Other") %>%
  select(cicp_initiative, CAGR)

viz_j_survival_all <- survival_milestones %>%
  filter(cicp_initiative != "Total", cicp_initiative != "Other") %>%
  select(cicp_initiative, Year_5)

viz_j_scale_all <- recent_initiative_summary %>%
  filter(cicp_initiative != "Total", cicp_initiative != "Other") %>%
  select(cicp_initiative, Total_Establishments, Avg_Wage)

viz_j_data_all <- viz_j_growth_all %>%
  left_join(viz_j_survival_all, by = "cicp_initiative") %>%
  left_join(viz_j_scale_all, by = "cicp_initiative") %>%
  mutate(size_category = "All Establishments")

# Prepare data by size category
# Using annual_employment to determine size at cohort inception

# First, get the size category for each establishment at founding (cohort_year)
estab_with_size <- estab_growth %>%
  filter(
    cicp_initiative != "Total",
    cicp_initiative != "Other",
    cohort_year == reporting_yr  # Size at founding
  ) %>%
  mutate(
    size_category = create_size_category(annual_employment)
  ) %>%
  select(cohort_year, cnty_fips, naics, cicp_initiative, size_category)

# Join size category back to full estab_growth data
estab_growth_with_size <- estab_growth %>%
  left_join(
    estab_with_size,
    by = c("cohort_year", "cnty_fips", "naics", "cicp_initiative")
  )

# Calculate CAGR by size category
# Group establishments by their founding size and track growth over time
viz_j_growth_by_size <- estab_growth_with_size %>%
  filter(
    cicp_initiative != "Total",
    cicp_initiative != "Other",
    !is.na(size_category)
  ) %>%
  group_by(cicp_initiative, size_category, reporting_yr) %>%
  summarise(
    establishments = sum(est_count_adjusted, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Calculate CAGR from first to last year for each size category
  group_by(cicp_initiative, size_category) %>%
  arrange(reporting_yr) %>%
  summarise(
    first_year = first(reporting_yr),
    last_year = last(reporting_yr),
    first_count = first(establishments),
    last_count = last(establishments),
    years_diff = last_year - first_year,
    .groups = "drop"
  ) %>%
  filter(years_diff > 0, first_count > 0, last_count > 0) %>%
  mutate(
    CAGR = ((last_count / first_count)^(1 / years_diff) - 1) * 100
  ) %>%
  select(cicp_initiative, size_category, CAGR)

# Calculate survival rates by size category
viz_j_survival_by_size <- estab_growth_with_size %>%
  filter(
    cicp_initiative != "Total",
    cicp_initiative != "Other",
    !is.na(size_category)
  ) %>%
  group_by(cicp_initiative, size_category, cohort_year) %>%
  summarise(
    cohort_count = sum(est_count_adjusted[reporting_yr == cohort_year], na.rm = TRUE),
    year5_count = sum(est_count_adjusted[reporting_yr == cohort_year + 5], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(cohort_count > 0) %>%
  group_by(cicp_initiative, size_category) %>%
  summarise(
    total_cohort = sum(cohort_count, na.rm = TRUE),
    total_year5 = sum(year5_count, na.rm = TRUE),
    Year_5 = (total_year5 / total_cohort) * 100,
    .groups = "drop"
  ) %>%
  select(cicp_initiative, size_category, Year_5)

# Calculate establishment counts by size category (for 2022)
viz_j_scale_by_size <- estab_growth_with_size %>%
  filter(
    cicp_initiative != "Total",
    cicp_initiative != "Other",
    reporting_yr == 2022,
    !is.na(size_category)
  ) %>%
  group_by(cicp_initiative, size_category) %>%
  summarise(
    Total_Establishments = sum(est_count_adjusted, na.rm = TRUE),
    Avg_Wage = weighted.mean(avg_annual_wage, est_count_adjusted, na.rm = TRUE),
    .groups = "drop"
  )

# Combine all size-specific data
viz_j_data_by_size <- viz_j_growth_by_size %>%
  left_join(viz_j_survival_by_size, by = c("cicp_initiative", "size_category")) %>%
  left_join(viz_j_scale_by_size, by = c("cicp_initiative", "size_category")) %>%
  filter(!is.na(CAGR), !is.na(Year_5), !is.na(Total_Establishments))

# Combine "All" and size-specific data
viz_j_data_combined <- bind_rows(
  viz_j_data_all,
  viz_j_data_by_size
)

# Calculate means for reference lines by size category
reference_lines <- viz_j_data_combined %>%
  group_by(size_category) %>%
  summarise(
    mean_cagr = mean(CAGR, na.rm = TRUE),
    mean_survival = mean(Year_5, na.rm = TRUE),
    min_cagr = min(CAGR, na.rm = TRUE),
    max_cagr = max(CAGR, na.rm = TRUE),
    min_survival = min(Year_5, na.rm = TRUE),
    max_survival = max(Year_5, na.rm = TRUE),
    .groups = "drop"
  )

# Static version (keep as-is, using "All Establishments")
viz_j_static_data <- viz_j_data_combined %>%
  filter(size_category == "All Establishments")

mean_cagr <- mean(viz_j_static_data$CAGR, na.rm = TRUE)
mean_survival <- mean(viz_j_static_data$Year_5, na.rm = TRUE)

p_j_static <- viz_j_static_data %>%
  ggplot(aes(x = CAGR, y = Year_5, size = Total_Establishments, 
             color = cicp_initiative, label = cicp_initiative)) +
  geom_point(alpha = 0.7) +
  geom_text(size = 3, fontface = "bold", 
            color = "black", vjust = -1.5, show.legend = FALSE) +
  geom_vline(xintercept = mean_cagr, linetype = "dashed", color = "gray50", alpha = 0.7) +
  geom_hline(yintercept = mean_survival, 
             linetype = "dashed", color = "gray50", alpha = 0.7) +
  scale_size_continuous(range = c(5, 20), labels = comma, name = "Total\nEstablishments") +
  scale_color_manual(values = initiative_colors, guide = "none") +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Initiative Performance: Growth Rate vs. Survival Rate (2022)",
    subtitle = "Bubble size = total establishments | Dashed lines show averages",
    x = "Compound Annual Growth Rate (CAGR) →\nHigher = More Growth",
    y = "5-Year Survival Rate (%) →\nHigher = More Durable",
    caption = "Source: IBRC Longitudinal Database, via BLS | Growth rate calculated from first to last available year"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 10, face = "bold"),
    axis.title.y = element_text(size = 10, face = "bold")
  )

ggsave(file.path(plots_dir, "J_growth_survival_scale_bubble_static.png"), 
       p_j_static, width = 10, height = 8, dpi = 300, bg = "white")

# ============================================================================
# INTERACTIVE VERSION WITH SIZE CATEGORY DROPDOWN
# ============================================================================

p_j_interactive <- plot_ly()

# Get all unique size categories
size_categories <- c("All Establishments", "Small (1-20 employees)", 
                     "Medium (21-50 employees)", "Large (51+ employees)")

# Get ALL initiatives that should appear (from the "All Establishments" data)
all_initiatives <- viz_j_data_combined %>%
  filter(size_category == "All Establishments") %>%
  pull(cicp_initiative) %>%
  unique()

# Track traces for visibility control
trace_counter <- 0
trace_map <- list()

# Add traces for each size category and initiative
for(size_cat in size_categories) {
  
  cat_data <- viz_j_data_combined %>%
    filter(size_category == size_cat)
  
  if(nrow(cat_data) > 0) {
    # Get reference lines for this category
    ref_lines <- reference_lines %>% filter(size_category == size_cat)
    
    # Add bubble traces for EACH initiative (even if missing in this size category)
    for(init in all_initiatives) {  # CHANGED: Use all_initiatives, not unique(cat_data$cicp_initiative)
      init_data <- cat_data %>% filter(cicp_initiative == init)
      
      # CHANGED: Add trace even if no data (will be invisible but preserves legend)
      if(nrow(init_data) > 0) {
        trace_counter <- trace_counter + 1
        
        trace_map[[trace_counter]] <- list(
          size_category = size_cat,
          initiative = init,
          type = "bubble",
          has_data = TRUE
        )
        
        p_j_interactive <- p_j_interactive %>%
          add_trace(
            data = init_data,
            x = ~CAGR,
            y = ~Year_5,
            size = ~Total_Establishments,
            sizes = c(100, 800),
            color = I(initiative_colors[init]),
            name = init,
            type = 'scatter',
            mode = 'markers',
            marker = list(
              opacity = 0.7,
              line = list(width = 2, color = 'white')
            ),
            text = ~paste0(
              "<b>", cicp_initiative, "</b><br>",
              "Size: ", size_category, "<br>",
              "CAGR: ", round(CAGR, 2), "%<br>",
              "5-Year Survival: ", round(Year_5, 1), "%<br>",
              "Total Establishments: ", format(Total_Establishments, big.mark = ",", scientific = FALSE)
            ),
            hoverinfo = 'text',
            visible = (size_cat == "All Establishments"),
            legendgroup = init,
            showlegend = TRUE  # CHANGED: Always show in legend
          )
      } else {
        # ADDED: Add empty trace to preserve legend entry
        trace_counter <- trace_counter + 1
        
        trace_map[[trace_counter]] <- list(
          size_category = size_cat,
          initiative = init,
          type = "bubble",
          has_data = FALSE
        )
        
        p_j_interactive <- p_j_interactive %>%
          add_trace(
            x = NULL,
            y = NULL,
            color = I(initiative_colors[init]),
            name = init,
            type = 'scatter',
            mode = 'markers',
            marker = list(
              opacity = 0.7,
              line = list(width = 2, color = 'white')
            ),
            visible = (size_cat == "All Establishments"),
            legendgroup = init,
            showlegend = TRUE,  # CHANGED: Always show in legend
            hoverinfo = 'skip'
          )
      }
    }
    
    # Add vertical reference line (mean CAGR)
    if(nrow(ref_lines) > 0) {
      trace_counter <- trace_counter + 1
      trace_map[[trace_counter]] <- list(
        size_category = size_cat,
        type = "vline"
      )
      
      p_j_interactive <- p_j_interactive %>%
        add_segments(
          x = ref_lines$mean_cagr, 
          xend = ref_lines$mean_cagr,
          y = ref_lines$min_survival - 2,
          yend = ref_lines$max_survival + 2,
          line = list(color = 'gray', width = 2, dash = 'dash'),
          showlegend = FALSE,
          hoverinfo = 'text',
          text = paste0("Mean CAGR: ", round(ref_lines$mean_cagr, 2), "%"),
          visible = (size_cat == "All Establishments"),
          inherit = FALSE
        )
      
      # Add horizontal reference line (mean survival)
      trace_counter <- trace_counter + 1
      trace_map[[trace_counter]] <- list(
        size_category = size_cat,
        type = "hline"
      )
      
      p_j_interactive <- p_j_interactive %>%
        add_segments(
          x = ref_lines$min_cagr - 1,
          xend = ref_lines$max_cagr + 1,
          y = ref_lines$mean_survival, 
          yend = ref_lines$mean_survival,
          line = list(color = 'gray', width = 2, dash = 'dash'),
          showlegend = FALSE,
          hoverinfo = 'text',
          text = paste0("Mean 5-Year Survival: ", round(ref_lines$mean_survival, 1), "%"),
          visible = (size_cat == "All Establishments"),
          inherit = FALSE
        )
    }
  }
}

# DIAGNOSTIC: Check which initiatives have data in each size category
cat("\n=== Initiatives by Size Category ===\n")
initiative_coverage <- viz_j_data_combined %>%
  group_by(size_category) %>%
  summarise(
    initiatives = paste(sort(unique(cicp_initiative)), collapse = ", "),
    n_initiatives = n_distinct(cicp_initiative),
    .groups = "drop"
  )
print(initiative_coverage)

# Create dropdown buttons
dropdown_buttons <- lapply(seq_along(size_categories), function(s) {
  size_cat <- size_categories[s]
  
  # Create visibility vector - now includes empty traces
  visible_vec <- rep(FALSE, trace_counter)
  
  # Set visibility for this size category's traces
  for(t in seq_along(trace_map)) {
    if(trace_map[[t]]$size_category == size_cat) {
      visible_vec[t] <- TRUE
    }
  }
  
  # Subtitle based on size category
  subtitle <- if(size_cat == "All Establishments") {
    "All establishment sizes | Bubble size = total establishments | Dashed lines show averages"
  } else {
    paste0(size_cat, " only | Bubble size = total establishments | Dashed lines show averages")
  }
  
  list(
    method = "update",
    args = list(
      list(visible = visible_vec),
      list(
        title = list(
          text = paste0(
            "<b>Initiative Performance: Growth Rate vs. Survival Rate (2022)</b><br>",
            "<sup>", subtitle, " | Hover for details</sup>"
          )
        )
      )
    ),
    label = size_cat
  )
})

p_j_interactive <- p_j_interactive %>%
  layout(
    title = list(
      text = "<b>Initiative Performance: Growth Rate vs. Survival Rate (2022)</b><br><sup>All establishment sizes | Bubble size = total establishments | Dashed lines show averages | Hover for details</sup>",
      font = list(size = 16)
    ),
    xaxis = list(
      title = "Compound Annual Growth Rate (CAGR) %",
      ticksuffix = "%",
      zeroline = FALSE
    ),
    yaxis = list(
      title = "5-Year Survival Rate (%)",
      ticksuffix = "%"
    ),
    showlegend = TRUE,
    legend = list(title = list(text = "Initiative")),
    hovermode = "closest",
    updatemenus = list(
      list(
        active = 0,
        type = "dropdown",
        x = 0.15,
        y = 1.15,
        xanchor = "left",
        yanchor = "top",
        buttons = dropdown_buttons
      )
    )
  )

htmlwidgets::saveWidget(p_j_interactive, 
                       file.path(plots_dir, "J_growth_survival_scale_bubble_interactive.html"),
                       selfcontained = TRUE)

cat("✓ Visualization J complete\n")

################################################################################
# VIZ N: 2022 SNAPSHOT INTERACTIVE DASHBOARD
################################################################################

cat("\nCreating Visualization N: 2022 Snapshot Dashboard...\n")

# Prepare data for each panel
# Panel 1: Establishments by Initiative (Bar)
panel1_data <- recent_initiative_summary %>%
  filter(cicp_initiative != "Total", cicp_initiative != "Other") %>%
  arrange(desc(Total_Establishments))

# Panel 2: Employment by Initiative (Bar)
panel2_data <- panel1_data

# Panel 3: Avg Wage by Initiative (Bar)
panel3_data <- panel1_data

# Panel 4: Initiative Share (Pie)
panel4_data <- panel1_data

# Create dashboard with subplots
p_n_interactive <- plot_ly()

# Panel 1: Establishments (top left)
p_n_interactive <- p_n_interactive %>%
  add_trace(
    data = panel1_data,
    x = ~Total_Establishments,
    y = ~reorder(cicp_initiative, Total_Establishments),
    type = 'bar',
    orientation = 'h',
    marker = list(color = initiative_colors[panel1_data$cicp_initiative]),
    name = "Establishments",
    showlegend = FALSE,
    xaxis = 'x1',
    yaxis = 'y1',
    hovertemplate = paste(
      "<b>%{y}</b><br>",
      "Establishments: %{x:,}<br>",
      "<extra></extra>"
    )
  )

# Panel 2: Employment (top right)
p_n_interactive <- p_n_interactive %>%
  add_trace(
    data = panel2_data,
    x = ~Total_Employment,
    y = ~reorder(cicp_initiative, Total_Employment),
    type = 'bar',
    orientation = 'h',
    marker = list(color = initiative_colors[panel2_data$cicp_initiative]),
    name = "Employment",
    showlegend = FALSE,
    xaxis = 'x2',
    yaxis = 'y2',
    hovertemplate = paste(
      "<b>%{y}</b><br>",
      "Employment: %{x:,}<br>",
      "<extra></extra>"
    )
  )

# Panel 3: Avg Wage (bottom left)
p_n_interactive <- p_n_interactive %>%
  add_trace(
    data = panel3_data,
    x = ~Avg_Wage,
    y = ~reorder(cicp_initiative, Avg_Wage),
    type = 'bar',
    orientation = 'h',
    marker = list(color = initiative_colors[panel3_data$cicp_initiative]),
    name = "Avg Wage",
    showlegend = FALSE,
    xaxis = 'x3',
    yaxis = 'y3',
    hovertemplate = paste(
      "<b>%{y}</b><br>",
      "Avg Wage: $%{x:,.0f}<br>",
      "<extra></extra>"
    )
  )

# Panel 4: Initiative Share Pie (bottom right)
p_n_interactive <- p_n_interactive %>%
  add_trace(
    data = panel4_data,
    labels = ~cicp_initiative,
    values = ~Total_Establishments,
    type = 'pie',
    marker = list(colors = initiative_colors[panel4_data$cicp_initiative]),
    textposition = 'inside',
    textinfo = 'label+percent',
    name = "Share",
    domain = list(x = c(0.55, 0.95), y = c(0, 0.4)),
    hovertemplate = paste(
      "<b>%{label}</b><br>",
      "Establishments: %{value:,}<br>",
      "Share: %{percent}<br>",
      "<extra></extra>"
    )
  )

# Layout with proper subplot positioning
p_n_interactive <- p_n_interactive %>%
  layout(
    title = list(
      text = "2022 Establishment Snapshot Dashboard<br><sub>Key metrics by initiative</sub>",
      font = list(size = 16)
    ),
    # Panel 1: Top left (Establishments)
    xaxis = list(
      domain = c(0, 0.45),
      title = "Total Establishments",
      anchor = 'y1'
    ),
    yaxis = list(
      domain = c(0.55, 1),
      title = "",
      anchor = 'x1'
    ),
    # Panel 2: Top right (Employment)
    xaxis2 = list(
      domain = c(0.55, 1),
      title = "Total Employment",
      anchor = 'y2'
    ),
    yaxis2 = list(
      domain = c(0.55, 1),
      title = "",
      anchor = 'x2'
    ),
    # Panel 3: Bottom left (Avg Wage)
    xaxis3 = list(
      domain = c(0, 0.45),
      title = "Average Wage ($)",
      anchor = 'y3'
    ),
    yaxis3 = list(
      domain = c(0, 0.4),
      title = "",
      anchor = 'x3'
    ),
    # Note: Panel 4 (pie) uses domain parameter in add_trace, not here
    showlegend = FALSE,
    margin = list(t = 100, b = 50, l = 150, r = 50)
  )

htmlwidgets::saveWidget(p_n_interactive, 
                       file.path(plots_dir, "N_snapshot_dashboard_interactive.html"),
                       selfcontained = TRUE)

cat("✓ Visualization N complete\n")

################################################################################
# SUMMARY
################################################################################

cat("\n" %+% paste(rep("=", 80), collapse = "") %+% "\n")
cat("VISUALIZATION GENERATION COMPLETE\n")
cat(paste(rep("=", 80), collapse = "") %+% "\n\n")

cat("Created visualizations:\n")
cat("  A. New Establishment Formation Over Time (2018-2022)\n")
cat("     - Static: A_new_establishments_timeline_static.png\n")
cat("     - Interactive: A_new_establishments_timeline_interactive.html\n\n")
cat("  B. Year-over-Year Growth Rate Heatmap (2021-2022)\n")
cat("     - Static: B_yoy_growth_heatmap_static.png\n")
cat("     - Interactive: B_yoy_growth_heatmap_interactive.html\n\n")
cat("  H. Survival by Establishment Size (2018+)\n")
cat("     - Static: H_survival_by_size_static.png\n")
cat("     - Interactive: H_survival_by_size_interactive.html\n\n")
cat("  J. Growth vs Survival vs Scale Bubble Chart (2022)\n")
cat("     - Static: J_growth_survival_scale_bubble_static.png\n")
cat("     - Interactive: J_growth_survival_scale_bubble_interactive.html\n\n")
cat("  N. 2022 Snapshot Dashboard\n")
cat("     - Interactive: N_2022_snapshot_dashboard.html\n\n")

cat("All files saved to: ", plots_dir, "\n", sep = "")
cat("\nTotal: 5 static PNG files + 5 interactive HTML files\n")

################################################################################
# ADDITIONAL ENTREPRENEURSHIP VISUALIZATIONS
################################################################################

cat("\n" %+% paste(rep("=", 80), collapse = "") %+% "\n")
cat("CREATING ADDITIONAL ENTREPRENEURSHIP VISUALIZATIONS\n")
cat(paste(rep("=", 80), collapse = "") %+% "\n\n")

################################################################################
# VIZ O: NEW STARTUPS OVER TIME BY TYPE
################################################################################

cat("\nCreating Visualization O: New Startups Over Time by Type...\n")

# Prepare data: separate true startups from larger establishments
viz_o_data <- estab_growth %>%
  filter(
    cohort_year == reporting_yr,
    cohort_year >= 2015,
    cicp_initiative != "Total",
    cicp_initiative != "Other"
  ) %>%
  mutate(
    startup_category = case_when(
      new_est_type == 1 ~ "True Startups (1-20 employees)",
      new_est_type %in% c(2, 3, 4) ~ "Larger New Establishments (21+ employees)",
      TRUE ~ "Other"
    )
  )

# Create OVERVIEW data (initiative totals)
viz_o_overview <- viz_o_data %>%
  group_by(cohort_year, cicp_initiative, startup_category) %>%
  summarise(
    New_Establishments = sum(est_count_adjusted, na.rm = TRUE),
    .groups = "drop"
  )

# Static version (KEEP AS-IS - just overview)
p_o_static <- viz_o_overview %>%
  ggplot(aes(x = cohort_year, y = New_Establishments, 
             color = startup_category, linetype = startup_category)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  facet_wrap(~cicp_initiative, scales = "free_y") +
  scale_color_manual(
    values = c("True Startups (1-20 employees)" = "#440154", 
               "Larger New Establishments (21+ employees)" = "#FDE725"),
    name = "Establishment Type"
  ) +
  scale_linetype_manual(
    values = c("True Startups (1-20 employees)" = "solid",
               "Larger New Establishments (21+ employees)" = "dashed"),
    name = "Establishment Type"
  ) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "New Business Formation Over Time by Establishment Size",
    subtitle = "True Startups = 1-20 employees at founding | Larger = 21+ employees at founding",
    x = "Cohort Year",
    y = "Number of New Establishments",
    caption = "Source: IBRC Longitudinal Database, via BLS"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray30"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(file.path(plots_dir, "O_new_startups_by_type_static.png"), 
       p_o_static, width = 12, height = 8, dpi = 300, bg = "white")

# ============================================================================
# INTERACTIVE VERSION 1: INITIATIVE OVERVIEW
# ============================================================================

p_o_interactive_overview <- plot_ly()

initiatives <- unique(viz_o_overview$cicp_initiative)
n_initiatives <- length(initiatives)
n_cols <- 2
n_rows <- ceiling(n_initiatives / n_cols)

startup_types <- c("True Startups (1-20 employees)", 
                   "Larger New Establishments (21+ employees)")
type_colors <- c("#440154", "#FDE725")
type_dash <- c("solid", "dash")

# Format data with establishment counts for hover
viz_o_overview_formatted <- viz_o_overview %>%
  mutate(
    Estab_Display = ifelse(New_Establishments > 5, 
                           as.character(round(New_Establishments)), 
                           "<5")
  )

for(init in initiatives) {
  for(i in seq_along(startup_types)) {
    data_subset <- viz_o_overview_formatted %>%
      filter(cicp_initiative == init, startup_category == startup_types[i])
    
    if(nrow(data_subset) > 0) {
      init_index <- which(initiatives == init)
      
      p_o_interactive_overview <- p_o_interactive_overview %>%
        add_trace(
          data = data_subset,
          x = ~cohort_year,
          y = ~New_Establishments,
          name = startup_types[i],
          type = 'scatter',
          mode = 'lines+markers',
          line = list(width = 3, color = type_colors[i], dash = type_dash[i]),
          marker = list(size = 8, color = type_colors[i]),
          legendgroup = startup_types[i],
          showlegend = (init_index == 1),
          xaxis = paste0('x', ifelse(init_index == 1, '', init_index)),
          yaxis = paste0('y', ifelse(init_index == 1, '', init_index)),
          text = ~Estab_Display,
          hovertemplate = paste(
            "<b>", startup_types[i], "</b><br>",
            "Year: %{x}<br>",
            "New Establishments: %{text}<br>",
            "<extra></extra>"
          )
        )
    }
  }
}

# Subplot annotations
annotations_overview <- lapply(seq_along(initiatives), function(i) {
  row_num <- ceiling(i / n_cols)
  col_num <- ((i - 1) %% n_cols) + 1
  
  list(
    x = (col_num - 0.5) / n_cols,
    y = 1 - (row_num - 1) / n_rows - 0.02,
    text = paste0("<b>", initiatives[i], "</b>"),
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE,
    font = list(size = 12)
  )
})

# Build subplot layout
subplot_layout_overview <- list(
  title = list(
    text = "<b>New Business Formation Over Time by Establishment Size</b><br><sup>Initiative Totals | True Startups = 1-20 employees at founding | Click legend to toggle types</sup>",
    font = list(size = 16)
  ),
  annotations = annotations_overview,
  hovermode = "closest",
  legend = list(
    orientation = "h",
    yanchor = "bottom",
    y = -0.15,
    xanchor = "center",
    x = 0.5
  )
)

for(i in seq_along(initiatives)) {
  row_num <- ceiling(i / n_cols)
  col_num <- ((i - 1) %% n_cols) + 1
  
  x_domain <- c((col_num - 1) / n_cols + 0.02, col_num / n_cols - 0.02)
  subplot_layout_overview[[paste0('xaxis', ifelse(i == 1, '', i))]] <- list(
    domain = x_domain,
    title = if(row_num == n_rows) "Cohort Year" else "",
    anchor = paste0('y', ifelse(i == 1, '', i))
  )
  
  y_domain <- c(1 - row_num / n_rows + 0.08, 1 - (row_num - 1) / n_rows - 0.05)
  subplot_layout_overview[[paste0('yaxis', ifelse(i == 1, '', i))]] <- list(
    domain = y_domain,
    title = if(col_num == 1) "New Establishments" else "",
    side = "left",
    anchor = paste0('x', ifelse(i == 1, '', i))
  )
}

p_o_interactive_overview <- do.call(layout, c(list(p_o_interactive_overview), subplot_layout_overview))

htmlwidgets::saveWidget(p_o_interactive_overview, 
                       file.path(plots_dir, "O_new_startups_by_type_overview.html"),
                       selfcontained = TRUE)

cat("✓ Overview interactive chart saved\n")

# ============================================================================
# INTERACTIVE VERSION 2: SUBCLUSTER BREAKDOWN WITH DROPDOWN
# ============================================================================

# Create subcluster data with shortened initiative names
viz_o_subclusters <- viz_o_data %>%
  mutate(
    init_short = case_when(
      cicp_initiative == "Advanced & Traded Industries" ~ "A&TI",
      cicp_initiative == "AgriNovus" ~ "Ag",
      cicp_initiative == "TechPoint" ~ "Tech",
      cicp_initiative == "Conexus" ~ "CX",
      cicp_initiative == "BioCrossroads" ~ "BioX",
      TRUE ~ cicp_initiative
    ),
    subcluster_label = paste0(init_short, " - ", sector_name)
  ) %>%
  group_by(cohort_year, cicp_initiative, init_short, sector_name, 
           subcluster_label, startup_category) %>%
  summarise(
    New_Establishments = sum(est_count_adjusted, na.rm = TRUE),
    .groups = "drop"
  )

# Get all unique subclusters, sorted
all_subclusters <- viz_o_subclusters %>%
  distinct(subcluster_label, init_short) %>%
  arrange(init_short, subcluster_label) %>%
  pull(subcluster_label)

# Format data with establishment counts for hover
viz_o_subclusters_formatted <- viz_o_subclusters %>%
  mutate(
    Estab_Display = ifelse(New_Establishments > 5, 
                           as.character(round(New_Establishments)), 
                           "<5")
  )

p_o_interactive_subclusters <- plot_ly()

# Track traces for visibility control
trace_counter <- 0
trace_map <- list()  # Will store: list(subcluster, startup_type, trace_index)

# Add traces for each subcluster
for(subcluster in all_subclusters) {
  for(i in seq_along(startup_types)) {
    
    data_subset <- viz_o_subclusters_formatted %>%
      filter(subcluster_label == subcluster, 
             startup_category == startup_types[i])
    
    if(nrow(data_subset) > 0) {
      trace_counter <- trace_counter + 1
      
      # Store trace mapping
      trace_map[[trace_counter]] <- list(
        subcluster = subcluster,
        startup_type = startup_types[i],
        trace_index = trace_counter
      )
      
      p_o_interactive_subclusters <- p_o_interactive_subclusters %>%
        add_trace(
          data = data_subset,
          x = ~cohort_year,
          y = ~New_Establishments,
          name = startup_types[i],
          type = 'scatter',
          mode = 'lines+markers',
          line = list(width = 3, color = type_colors[i], dash = type_dash[i]),
          marker = list(size = 8, color = type_colors[i]),
          legendgroup = startup_types[i],
          showlegend = (subcluster == all_subclusters[1] && i == 1),  # Show legend once
          visible = (subcluster == all_subclusters[1]),  # Only first visible by default
          text = ~Estab_Display,
          hovertemplate = paste(
            "<b>", startup_types[i], "</b><br>",
            "Year: %{x}<br>",
            "New Establishments: %{text}<br>",
            "<extra></extra>"
          )
        )
    }
  }
}

# Create dropdown buttons
dropdown_buttons <- lapply(seq_along(all_subclusters), function(s) {
  subcluster <- all_subclusters[s]
  
  # Create visibility vector
  visible_vec <- rep(FALSE, trace_counter)
  
  for(t in seq_along(trace_map)) {
    if(trace_map[[t]]$subcluster == subcluster) {
      visible_vec[t] <- TRUE
    }
  }
  
  list(
    method = "update",
    args = list(
      list(visible = visible_vec),
      list(
        title = list(
          text = paste0(
            "<b>New Business Formation: ", subcluster, "</b><br>",
            "<sup>True Startups = 1-20 employees at founding | Click legend to toggle types</sup>"
          )
        )
      )
    ),
    label = subcluster
  )
})

# Layout for subcluster version
p_o_interactive_subclusters <- p_o_interactive_subclusters %>%
  layout(
    title = list(
      text = paste0(
        "<b>New Business Formation: ", all_subclusters[1], "</b><br>",
        "<sup>True Startups = 1-20 employees at founding | Click legend to toggle types</sup>"
      ),
      font = list(size = 16)
    ),
    xaxis = list(
      title = "Cohort Year",
      tickmode = "linear",      # ADD THIS
      tick0 = min(viz_o_subclusters_formatted$cohort_year),  # ADD THIS
      dtick = 1                 # ADD THIS - forces 1-year intervals
    ),
    yaxis = list(title = "Number of New Establishments"),
    hovermode = "closest",
    legend = list(
      orientation = "h",
      yanchor = "bottom",
      y = -0.15,
      xanchor = "center",
      x = 0.5
    ),
    updatemenus = list(
      list(
        active = 0,
        type = "dropdown",
        x = 0.15,
        y = 1.15,
        xanchor = "left",
        yanchor = "top",
        buttons = dropdown_buttons
      )
    )
  )

htmlwidgets::saveWidget(p_o_interactive_subclusters, 
                       file.path(plots_dir, "O_new_startups_by_subcluster.html"),
                       selfcontained = TRUE)

cat("✓ Subcluster interactive chart saved\n")
cat("✓ Visualization O complete - 2 interactive versions created\n")

################################################################################
# VIZ Q: COHORT PERFORMANCE TABLE
################################################################################

# Create output directory for tables
tables_dir <- paste0("estab_output_", estab_recent_year, "/tables")
if (!dir.exists(tables_dir)) dir.create(tables_dir, recursive = TRUE)

cat("\nCreating Visualization Q: Cohort Performance Table...\n")

# Prepare cohort performance data
viz_q_data <- estab_growth %>%
  filter(
    cohort_year >= 2015,
    cicp_initiative != "Total",
    cicp_initiative != "Other"
  ) %>%
  group_by(cohort_year, cicp_initiative) %>%
  summarise(
    # New establishments in birth year
    New_Establishments = sum(est_count_adjusted[reporting_yr == cohort_year], na.rm = TRUE),
    # Initial employment
    Initial_Employment = sum(annual_employment[reporting_yr == cohort_year], na.rm = TRUE),
    # 1-year survival (if available)
    Estabs_Year_1 = sum(est_count_adjusted[reporting_yr == cohort_year + 1], na.rm = TRUE),
    # Current establishments (most recent year available for this cohort)
    Current_Establishments = sum(est_count_adjusted[reporting_yr == max(reporting_yr)], na.rm = TRUE),
    Max_Year_Observed = max(reporting_yr),
    .groups = "drop"
  ) %>%
  mutate(
    Survival_Rate_Year_1 = ifelse(New_Establishments > 0, 
                                   (Estabs_Year_1 / New_Establishments) * 100, 
                                   NA),
    Years_Tracked = Max_Year_Observed - cohort_year,
    Current_Survival_Rate = ifelse(New_Establishments > 0,
                                    (Current_Establishments / New_Establishments) * 100,
                                    NA)
  ) %>%
  select(cohort_year, cicp_initiative, New_Establishments, Initial_Employment,
         Survival_Rate_Year_1, Years_Tracked, Current_Survival_Rate)

# Create formatted table for each initiative
for(init in unique(viz_q_data$cicp_initiative)) {
  init_data <- viz_q_data %>%
    filter(cicp_initiative == init) %>%
    select(-cicp_initiative) %>%
    arrange(desc(cohort_year))
  
  # Format for display
  init_data_formatted <- init_data %>%
    mutate(
      New_Establishments = format(New_Establishments, big.mark = ","),
      Initial_Employment = format(Initial_Employment, big.mark = ","),
      Survival_Rate_Year_1 = ifelse(is.na(Survival_Rate_Year_1), "—", 
                                     paste0(round(Survival_Rate_Year_1, 1), "%")),
      Current_Survival_Rate = paste0(round(Current_Survival_Rate, 1), "%")
    )
  
  # Export to CSV
  write_csv(init_data, 
            file.path(tables_dir, paste0("Q_cohort_performance_", 
                                         gsub(" |&", "_", init), ".csv")))
}

# Create combined table
viz_q_combined <- viz_q_data %>%
  arrange(cicp_initiative, desc(cohort_year))

write_csv(viz_q_combined, 
          file.path(tables_dir, "Q_cohort_performance_all_initiatives.csv"))

cat("✓ Visualization Q complete - Tables exported to", tables_dir, "\n")

################################################################################
# VIZ R: SECTOR ENTREPRENEURSHIP WITHIN INITIATIVES (TABLE)
################################################################################

cat("\nCreating Visualization R: Sector Entrepreneurship Table...\n")

# Prepare sector-level data
viz_r_data <- estab_growth %>%
  filter(
    cohort_year == reporting_yr,
    cohort_year >= 2015,
    reporting_yr == estab_recent_year,  # Most recent year only
    cicp_initiative != "Total",
    cicp_initiative != "Other",
    !is.na(sector_name),
    sector_name != "All"
  ) %>%
  group_by(cicp_initiative, sector_name) %>%
  summarise(
    New_Establishments_2022 = sum(est_count_adjusted, na.rm = TRUE),
    New_Employment_2022 = sum(annual_employment, na.rm = TRUE),
    Avg_Initial_Wage = weighted.mean(avg_annual_wage, annual_employment, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(cicp_initiative) %>%
  mutate(
    Pct_of_Initiative = (New_Establishments_2022 / sum(New_Establishments_2022)) * 100
  ) %>%
  ungroup() %>%
  arrange(cicp_initiative, desc(New_Establishments_2022))

# Create table for each initiative
for(init in unique(viz_r_data$cicp_initiative)) {
  init_data <- viz_r_data %>%
    filter(cicp_initiative == init) %>%
    select(-cicp_initiative)
  
  write_csv(init_data, 
            file.path(tables_dir, paste0("R_sector_entrepreneurship_", 
                                         gsub(" |&", "_", init), ".csv")))
}

# Create combined table
write_csv(viz_r_data, 
          file.path(tables_dir, "R_sector_entrepreneurship_all_initiatives.csv"))

cat("✓ Visualization R complete - Tables exported to", tables_dir, "\n")

cat("\n" %+% paste(rep("=", 80), collapse = "") %+% "\n")
cat("ALL ENTREPRENEURSHIP VISUALIZATIONS COMPLETE\n")
cat(paste(rep("=", 80), collapse = "") %+% "\n\n")