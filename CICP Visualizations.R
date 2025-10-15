# ============================================================================
# CICP Advanced Industries Dashboard - Interactive Visualizations
# ============================================================================
# Purpose: Create professional, interactive charts for reporting
# Note: Run main analysis script first to generate processed_data.RData
# ============================================================================

library(tidyverse)
library(scales)
library(plotly)
library(patchwork)

# Load processed data from main analysis
output_dir <- "outputs_20250709"  # Modify to match your output directory
load(file.path(output_dir, "processed_data_jobs_gdp.RData"))

# Create visualizations directory
viz_dir <- file.path(output_dir, "visualizations")
if (!dir.exists(viz_dir)) {
  dir.create(viz_dir, recursive = TRUE)
}

# VISUALIZATION 1: Indiana Jobs by Initiative ---------------------------------

p1_data <- jobs_data %>%
  filter(display_level == 0, 
         year == recent_year, 
         geo_area == "Indiana",
         initiative != "Total Employment") %>%
  arrange(desc(jobs))

p1 <- p1_data %>%
  ggplot(aes(x = reorder(initiative, jobs), y = jobs, fill = initiative)) +
  geom_col(show.legend = FALSE, alpha = 0.9) +
  geom_text(aes(label = comma(jobs)), 
            hjust = -0.1, 
            size = 4,
            fontface = "bold") +
  coord_flip() +
  scale_y_continuous(labels = comma, 
                    expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = initiative_colors) +
  labs(
    title = "Total Employment by Initiative",
    subtitle = paste("Indiana |", recent_year),
    x = NULL, 
    y = "Total Jobs",
    caption = "Source: CICP Advanced Industries Dashboard"
  ) +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 11))

# Save static version
ggsave(file.path(viz_dir, "01_indiana_jobs_by_initiative.png"), 
       p1, width = 12, height = 7, dpi = 300, bg = "white")

# Create interactive version
p1_interactive <- ggplotly(p1, tooltip = c("x", "y")) %>%
  layout(
    title = list(
      text = paste0("<b>Total Employment by Initiative in Indiana</b><br><sup>Year: ", recent_year, "</sup>"),
      font = list(size = 20)
    ),
    hoverlabel = list(bgcolor = "white", font = list(size = 12)),
    margin = list(l = 200)
  )

htmlwidgets::saveWidget(
  p1_interactive,
  file.path(viz_dir, "01_indiana_jobs_by_initiative.html"),
  selfcontained = TRUE
)

# VISUALIZATION 2: Indiana Growth Rates by Initiative -------------------------

p2_data <- jobs_growth %>%
  filter(display_level == 0, 
         year == recent_year, 
         geo_area == "Indiana",
         initiative != "Total Employment",
         !is.na(cagr_3yr)) %>%
  mutate(growth_type = ifelse(cagr_3yr >= 0, "Growth", "Decline"))

p2 <- p2_data %>%
  ggplot(aes(x = reorder(initiative, cagr_3yr), y = cagr_3yr, fill = growth_type)) +
  geom_col(show.legend = FALSE, alpha = 0.9) +
  geom_hline(yintercept = 0, linetype = "solid", color = "gray30", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.1f%%", cagr_3yr)), 
            hjust = ifelse(p2_data$cagr_3yr >= 0, -0.15, 1.15), 
            size = 4,
            fontface = "bold") +
  coord_flip() +
  scale_y_continuous(labels = label_percent(scale = 1),
                    expand = expansion(mult = c(0.15, 0.15))) +
  scale_fill_manual(values = c("Growth" = "#2E7D32", "Decline" = "#C62828")) +
  labs(
    title = "Job Growth Rate by Initiative",
    subtitle = paste("3-Year CAGR | Indiana | Through", recent_year),
    x = NULL, 
    y = "3-Year Compound Annual Growth Rate",
    caption = "Source: CICP Advanced Industries Dashboard"
  ) +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 11))

ggsave(file.path(viz_dir, "02_indiana_growth_by_initiative.png"), 
       p2, width = 12, height = 7, dpi = 300, bg = "white")

p2_interactive <- ggplotly(p2, tooltip = c("x", "y")) %>%
  layout(
    hoverlabel = list(bgcolor = "white", font = list(size = 12)),
    margin = list(l = 200)
  )

htmlwidgets::saveWidget(
  p2_interactive,
  file.path(viz_dir, "02_indiana_growth_by_initiative.html"),
  selfcontained = TRUE
)

# VISUALIZATION 3: Top 10 Metro Areas by Total Jobs ---------------------------

p3_data <- jobs_data %>%
  filter(display_level == 0, 
         year == recent_year, 
         geo_type == "Metro",
         initiative != "Total Employment") %>%
  group_by(geo_area) %>%
  summarise(
    total_jobs = sum(jobs, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_jobs)) %>%
  head(10)

p3 <- p3_data %>%
  ggplot(aes(x = reorder(geo_area, total_jobs), y = total_jobs)) +
  geom_col(fill = "#1565C0", alpha = 0.9) +
  geom_text(aes(label = comma(total_jobs)), 
            hjust = -0.1, 
            size = 4,
            fontface = "bold") +
  coord_flip() +
  scale_y_continuous(labels = comma, 
                    expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Top 10 Metro Areas by Total Advanced Industry Jobs",
    subtitle = paste("All Initiatives Combined |", recent_year),
    x = NULL, 
    y = "Total Jobs",
    caption = "Source: CICP Advanced Industries Dashboard"
  ) +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 10))

ggsave(file.path(viz_dir, "03_top_metros_total_jobs.png"), 
       p3, width = 12, height = 7, dpi = 300, bg = "white")

p3_interactive <- ggplotly(p3, tooltip = c("x", "y")) %>%
  layout(
    hoverlabel = list(bgcolor = "white", font = list(size = 12)),
    margin = list(l = 200)
  )

htmlwidgets::saveWidget(
  p3_interactive,
  file.path(viz_dir, "03_top_metros_total_jobs.html"),
  selfcontained = TRUE
)

# VISUALIZATION 4: Average Wages by Initiative in Indiana --------------------

p4_data <- wage_data %>%
  filter(display_level == 0, 
         year == recent_year, 
         geo_area == "Indiana",
         initiative != "Total Employment",
         !is.na(wages)) %>%
  arrange(desc(wages))

p4 <- p4_data %>%
  ggplot(aes(x = reorder(initiative, wages), y = wages, fill = initiative)) +
  geom_col(show.legend = FALSE, alpha = 0.9) +
  geom_text(aes(label = dollar(wages, accuracy = 1)), 
            hjust = -0.1, 
            size = 4,
            fontface = "bold") +
  coord_flip() +
  scale_y_continuous(labels = dollar, 
                    expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = initiative_colors) +
  labs(
    title = "Average Annual Wage by Initiative",
    subtitle = paste("Indiana |", recent_year),
    x = NULL, 
    y = "Average Annual Wage",
    caption = "Source: CICP Advanced Industries Dashboard"
  ) +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 11))

ggsave(file.path(viz_dir, "04_indiana_wages_by_initiative.png"), 
       p4, width = 12, height = 7, dpi = 300, bg = "white")

p4_interactive <- ggplotly(p4, tooltip = c("x", "y")) %>%
  layout(
    hoverlabel = list(bgcolor = "white", font = list(size = 12)),
    margin = list(l = 200)
  )

htmlwidgets::saveWidget(
  p4_interactive,
  file.path(viz_dir, "04_indiana_wages_by_initiative.html"),
  selfcontained = TRUE
)

# VISUALIZATION 5: Metro Growth Rates (All metros) -----------------------

# First, get the starting jobs (5 years ago) before summarizing
p5_data <- jobs_data %>%
  filter(display_level == 0, 
         geo_type == "Metro") %>%
  arrange(initiative, geo_area, year) %>%
  group_by(initiative, geo_area) %>%
  mutate(
    starting_jobs = if_else(year == recent_year, 
                           lag(jobs, 4),  # Jobs from 5 years ago
                           NA_real_)
  ) %>%
  ungroup() %>%
  filter(year == recent_year) %>%
  left_join(
    jobs_growth %>%
      filter(display_level == 0, 
             year == recent_year,
             geo_type == "Metro",
             !is.na(cagr_5yr)) %>%
      select(initiative, geo_area, cagr_5yr),
    by = c("initiative", "geo_area")
  ) %>%
  filter(!is.na(cagr_5yr)) %>%
  select(initiative, geo_area, current_jobs = jobs, starting_jobs, cagr_5yr) %>%
  mutate(growth_type = ifelse(cagr_5yr >= 0, "Growth", "Decline"),
         color_var = ifelse(cagr_5yr >= 0, "#2E7D32", "#C62828"))

# Get all metros
all_metros_list <- p5_data %>%
  group_by(geo_area) %>%
  summarise(total_jobs = sum(current_jobs, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_jobs)) %>%
  pull(geo_area)

# Filter to all metros
p5_data <- p5_data %>%
  filter(geo_area %in% all_metros_list)

# Create interactive plot with dropdown
p5_interactive <- plot_ly()

# Add a trace for each initiative
initiative_list <- unique(p5_data$initiative)

for(init in initiative_list) {
  trace_data <- p5_data %>% 
    filter(initiative == init) %>%
    arrange(cagr_5yr)
  
  p5_interactive <- p5_interactive %>%
    add_trace(
      x = trace_data$cagr_5yr,
      y = trace_data$geo_area,
      type = "bar",
      orientation = "h",
      name = init,
      visible = if(init == initiative_list[1]) TRUE else FALSE,
      marker = list(color = trace_data$color_var),
      text = sprintf("%.1f%%", trace_data$cagr_5yr),
      textposition = "outside",
      textfont = list(size = 12, color = "black"),
      hovertemplate = paste0(
        "<b>", trace_data$geo_area, "</b><br>",
        "5-Year CAGR: ", sprintf("%.1f%%", trace_data$cagr_5yr), "<br>",
        "Starting Jobs (", recent_year - 4, "): ", format(trace_data$starting_jobs, big.mark = ","), "<br>",
        "Current Jobs (", recent_year, "): ", format(trace_data$current_jobs, big.mark = ","), "<br>",
        "<extra></extra>"
      )
    )
}

# Create dropdown menu
updatemenus <- list(
  list(
    active = 0,
    type = "dropdown",
    x = 0.15,
    y = 1.15,
    xanchor = "left",
    yanchor = "top",
    buttons = lapply(seq_along(initiative_list), function(i) {
      list(
        method = "update",
        args = list(
          list(visible = sapply(seq_along(initiative_list), function(j) j == i)),
          list(title = list(
            text = paste0("<b>Job Growth Rate Across All Metro Areas</b><br><sup>5-Year CAGR | ", 
                         initiative_list[i], " | ", recent_year, "</sup>")
          ))
        ),
        label = initiative_list[i]
      )
    })
  )
)

p5_interactive <- p5_interactive %>%
  layout(
    title = list(
      text = paste0("<b>Job Growth Rate Across All Metro Areas</b><br><sup>5-Year CAGR | ", 
                   initiative_list[1], " | ", recent_year, "</sup>"),
      font = list(size = 18)
    ),
    xaxis = list(
      title = "5-Year Compound Annual Growth Rate (%)",
      showgrid = TRUE,
      gridcolor = "lightgray",
      zeroline = TRUE,
      zerolinecolor = "gray",
      zerolinewidth = 2
    ),
    yaxis = list(
      title = "",
      showgrid = FALSE
    ),
    updatemenus = updatemenus,
    showlegend = FALSE,
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    margin = list(l = 200, r = 100, t = 100, b = 60)
  )

htmlwidgets::saveWidget(
  p5_interactive,
  file.path(viz_dir, "05_all_metros_growth_rates.html"),
  selfcontained = TRUE
)

# Also create a static version for the first initiative (top 10 for readability)
p5_static_data <- p5_data %>% 
  filter(initiative == initiative_list[1]) %>%
  arrange(desc(current_jobs)) %>%
  head(10)

p5 <- p5_static_data %>%
  ggplot(aes(x = reorder(geo_area, cagr_5yr), y = cagr_5yr, fill = growth_type)) +
  geom_col(show.legend = FALSE, alpha = 0.9) +
  geom_hline(yintercept = 0, linetype = "solid", color = "gray30", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.1f%%", cagr_5yr)), 
            hjust = ifelse(p5_static_data$cagr_5yr >= 0, -0.15, 1.15), 
            size = 4,
            fontface = "bold") +
  coord_flip() +
  scale_y_continuous(labels = label_percent(scale = 1),
                    expand = expansion(mult = c(0.15, 0.15))) +
  scale_fill_manual(values = c("Growth" = "#2E7D32", "Decline" = "#C62828")) +
  labs(
    title = "Job Growth Rate - Top 10 Metro Areas",
    subtitle = paste("5-Year CAGR |", initiative_list[1], "|", recent_year),
    x = NULL, 
    y = "5-Year Compound Annual Growth Rate",
    caption = "Source: CICP Advanced Industries Dashboard"
  ) +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 10))

ggsave(file.path(viz_dir, "05_top_metros_growth_rates.png"), 
       p5, width = 12, height = 7, dpi = 300, bg = "white")

# VISUALIZATION 6: Initiative Share by All Metros (GROUPED BARS) ---------------

# Get total employment for each metro
metro_totals <- jobs_data %>%
  filter(display_level == 0,
         year == recent_year,
         geo_type == "Metro",
         initiative == "Total Employment") %>%
  select(geo_area, total_employment = jobs)

# Get initiative employment as % of total for each metro
p6_data <- jobs_data %>%
  filter(display_level == 0, 
         year == recent_year, 
         geo_type == "Metro",
         initiative != "Total Employment") %>%
  left_join(metro_totals, by = "geo_area") %>%
  mutate(share = (jobs / total_employment) * 100) %>%
  select(geo_area, initiative, jobs, total_employment, share)

# Sort metros by total employment
metro_order <- p6_data %>%
  group_by(geo_area) %>%
  summarise(total_employment = first(total_employment), .groups = "drop") %>%
  arrange(desc(total_employment)) %>%
  head(10) %>%
  pull(geo_area)

p6_data <- p6_data %>%
  filter(geo_area %in% metro_order) %>%  # ADD THIS LINE
  mutate(
    geo_area = factor(geo_area, levels = metro_order),
    initiative = factor(initiative, 
                       levels = c("Advanced & Traded Industries", 
                                 "AgriNovus", 
                                 "BioCrossroads", 
                                 "Conexus", 
                                 "TechPoint"))
  )

# Create grouped bar chart (not stacked)
p6 <- p6_data %>%
  ggplot(aes(x = geo_area, y = share, fill = initiative)) +
  geom_col(position = position_dodge(width = 0.9), alpha = 0.9) +
  coord_flip() +
  scale_y_continuous(labels = label_percent(scale = 1), 
                    expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = initiative_colors) +
  labs(
    title = "Share of Total Employment by Initiative Across All Metro Areas",
    subtitle = paste("Each Initiative as % of Total Metro Employment |", recent_year),
    x = NULL, 
    y = "Share of Total Employment (%)",
    fill = "Initiative",
    caption = "Source: CICP Advanced Industries Dashboard\nNote: Initiatives may overlap; some jobs may be counted in multiple initiatives"
  ) +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 9),
        legend.position = "right")

ggsave(file.path(viz_dir, "06_metro_initiative_share.png"), 
       p6, width = 14, height = 12, dpi = 300, bg = "white")

# Create interactive version
p6_interactive <- plot_ly(
  p6_data,
  x = ~share,
  y = ~geo_area,
  color = ~initiative,
  colors = initiative_colors,
  type = "bar",
  orientation = "h",
  hovertemplate = paste0(
    "<b>%{fullData.name}</b><br>",
    "%{y}<br>",
    "Share: %{x:.1f}%<br>",
    "Jobs: %{customdata:,}<br>",
    "<extra></extra>"
  ),
  customdata = ~jobs
) %>%
  layout(
    title = list(
      text = paste0("<b>Share of Total Employment by Initiative Across Top 10 Metro Areas</b><br><sup>Each Initiative as % of Total Metro Employment | ", 
                   recent_year, "</sup>"),
      font = list(size = 18)
    ),
    xaxis = list(
      title = "Share of Total Employment (%)",
      showgrid = TRUE,
      gridcolor = "lightgray"
    ),
    yaxis = list(
      title = "",
      showgrid = FALSE
    ),
    barmode = "group",
    legend = list(
      title = list(text = "<b>Initiative</b>"),
      orientation = "v",
      x = 1.02,
      y = 0.5,
      font = list(size = 11)
    ),
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    margin = list(l = 200, r = 200, t = 100, b = 80),
    annotations = list(
      list(
        text = "Note: Initiatives may overlap; some jobs may be counted in multiple initiatives",
        showarrow = FALSE,
        xref = "paper",
        yref = "paper",
        x = 0,
        y = -0.06,
        xanchor = "left",
        yanchor = "top",
        font = list(size = 10, color = "gray50")
      )
    )
  )

htmlwidgets::saveWidget(
  p6_interactive,
  file.path(viz_dir, "06_metro_initiative_share.html"),
  selfcontained = TRUE
)

# VISUALIZATION 7: Time Series - Jobs by Initiative with Geography Dropdown ----

# VISUALIZATION 7: Time Series - Jobs by Initiative with Geography Dropdown ----

# Prepare data for all geographies
p7_data <- jobs_data %>%
  filter(display_level == 0, 
         initiative != "Total Employment") %>%
  mutate(is_projected = year > 2024)

# Get list of all geographies
geography_list <- p7_data %>%
  distinct(geo_area) %>%
  arrange(geo_area) %>%
  pull(geo_area)

# Create static version for Indiana
p7_static_data <- p7_data %>%
  filter(geo_area == "Indiana")

# Split data into actual and projected
p7_actual <- p7_static_data %>% filter(year <= 2024)
p7_projected <- p7_static_data %>% filter(year >= 2024)  # Include 2024 to connect

p7 <- ggplot(p7_static_data, aes(x = year, y = jobs, color = initiative, group = initiative)) +
  # Actual data (solid lines)
  geom_line(data = p7_actual, linewidth = 1.2, alpha = 0.9) +
  geom_point(data = p7_actual, size = 2.5, alpha = 0.9) +
  # Projected data (dashed lines)
  geom_line(data = p7_projected, linewidth = 1.2, alpha = 0.9, linetype = "dashed") +
  geom_point(data = p7_projected %>% filter(year > 2024), 
             size = 2.5, alpha = 0.9, shape = 1) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = unique(p7_static_data$year)) +
  scale_color_manual(values = initiative_colors) +
  labs(
    title = "Employment Trends by Initiative in Indiana",
    subtitle = "Total Jobs Over Time | Dashed lines indicate projected employment",
    x = "Year", 
    y = "Total Jobs",
    color = "Initiative",
    caption = "Source: CICP Advanced Industries Dashboard"
  ) +
  theme(legend.position = "right",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(viz_dir, "07_indiana_jobs_time_series.png"), 
       p7, width = 14, height = 8, dpi = 300, bg = "white")

# Create interactive version with geography dropdown
p7_interactive <- plot_ly()

# Add traces for each geography and initiative combination
for(geo in geography_list) {
  for(init in unique(p7_data$initiative)) {
    # Actual data (solid line)
    trace_actual <- p7_data %>% 
      filter(geo_area == geo, initiative == init, year <= 2024) %>%
      arrange(year)
    
    if(nrow(trace_actual) > 0) {
      p7_interactive <- p7_interactive %>%
        add_trace(
          data = trace_actual,
          x = ~year,
          y = ~jobs,
          type = "scatter",
          mode = "lines+markers",
          name = init,
          line = list(
            width = 3,
            color = initiative_colors[init],
            dash = "solid"
          ),
          marker = list(
            size = 8,
            color = initiative_colors[init]
          ),
          visible = if(geo == "Indiana") TRUE else FALSE,
          legendgroup = init,
          showlegend = if(geo == geography_list[1]) TRUE else FALSE,
          hovertemplate = paste0(
            "<b>", init, "</b><br>",
            "Year: %{x}<br>",
            "Jobs: %{y:,}<br>",
            "<extra></extra>"
          )
        )
    }
    
    # Projected data (dashed line)
    trace_projected <- p7_data %>% 
      filter(geo_area == geo, initiative == init, year >= 2024) %>%
      arrange(year)
    
    if(nrow(trace_projected) > 0) {
      p7_interactive <- p7_interactive %>%
        add_trace(
          data = trace_projected,
          x = ~year,
          y = ~jobs,
          type = "scatter",
          mode = "lines+markers",
          name = init,
          line = list(
            width = 3,
            color = initiative_colors[init],
            dash = "dash"
          ),
          marker = list(
            size = 8,
            color = initiative_colors[init],
            symbol = "circle-open",
            line = list(width = 2)
          ),
          visible = if(geo == "Indiana") TRUE else FALSE,
          legendgroup = init,
          showlegend = FALSE,
          hovertemplate = paste0(
            "<b>", init, " (Projected)</b><br>",
            "Year: %{x}<br>",
            "Jobs: %{y:,}<br>",
            "<extra></extra>"
          )
        )
    }
  }
}

# Create dropdown menu for geographies
n_initiatives <- length(unique(p7_data$initiative))
n_traces_per_geo <- n_initiatives * 2  # 2 traces per initiative (actual + projected)

updatemenus <- list(
  list(
    active = which(geography_list == "Indiana") - 1,
    type = "dropdown",
    x = 0.15,
    y = 1.15,
    xanchor = "left",
    yanchor = "top",
    buttons = lapply(seq_along(geography_list), function(i) {
      geo <- geography_list[i]
      visible_vector <- rep(FALSE, length(geography_list) * n_traces_per_geo)
      start_idx <- (i - 1) * n_traces_per_geo + 1
      end_idx <- i * n_traces_per_geo
      visible_vector[start_idx:end_idx] <- TRUE
      
      list(
        method = "update",
        args = list(
          list(visible = visible_vector),
          list(title = list(
            text = paste0("<b>Employment Trends by Initiative - ", geo, 
                         "</b><br><sup>Total Jobs Over Time | Dashed lines indicate projected employment</sup>")
          ))
        ),
        label = geo
      )
    })
  )
)

p7_interactive <- p7_interactive %>%
  layout(
    title = list(
      text = "<b>Employment Trends by Initiative - Indiana</b><br><sup>Total Jobs Over Time | Dashed lines indicate projected employment</sup>",
      font = list(size = 18)
    ),
    xaxis = list(
      title = "Year",
      showgrid = TRUE,
      gridcolor = "lightgray"
    ),
    yaxis = list(
      title = "Total Jobs",
      showgrid = TRUE,
      gridcolor = "lightgray"
    ),
    legend = list(
      orientation = "v",
      x = 1.02,
      y = 0.5,
      font = list(size = 12),
      title = list(text = "<b>Initiative</b>")
    ),
    updatemenus = updatemenus,
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    margin = list(l = 80, r = 150, t = 100, b = 80),
    annotations = list(
      list(
        text = "Dashed lines and hollow points indicate projected employment",
        showarrow = FALSE,
        xref = "paper",
        yref = "paper",
        x = 0.5,
        y = -0.12,
        xanchor = "center",
        yanchor = "top",
        font = list(size = 11, color = "gray50")
      )
    )
  )

htmlwidgets::saveWidget(
  p7_interactive,
  file.path(viz_dir, "07_jobs_time_series_by_geography.html"),
  selfcontained = TRUE
)

# VISUALIZATION 8: GDP vs Jobs Growth Scatter ---------------------------------

# Calculate growth rates using Total Employment only (no double-counting)
p8_data <- gdp_growth %>%
  filter(display_level == 0, 
         geo_type %in% c("Metro", "State", "US"),
         initiative == "Total Employment") %>%  # CHANGED: only Total Employment
  # Create explicit start/end columns before filtering to recent year
  mutate(
    jobs_start = lag(jobs),
    jobs_end = jobs,
    grp_start = lag(grp),
    grp_end = grp,
    jobs_yoy_growth = yoy_growth,
    gdp_yoy_growth = (grp / lag(grp) - 1) * 100
  ) %>%
  # Filter to recent year
  filter(year == recent_year,
         !is.na(yoy_growth)) %>%
  # No grouping needed - one row per geography already
  select(
    geo_area,
    geo_type,
    jobs_growth = jobs_yoy_growth,
    gdp_growth = gdp_yoy_growth,
    total_jobs = jobs_end,
    total_jobs_start = jobs_start,
    total_jobs_end = jobs_end,
    total_grp = grp_end,
    total_grp_start = grp_start,
    total_grp_end = grp_end
  ) %>%
  filter(!is.na(gdp_growth), !is.na(jobs_growth), 
         !is.infinite(gdp_growth), !is.infinite(jobs_growth)) %>%
  mutate(
    gdp_per_job = total_grp / total_jobs
  )

# Check if we have data
if(nrow(p8_data) > 0) {
  
  p8 <- p8_data %>%
    mutate(geo_type_label = case_when(
      geo_type == "US" ~ "National",
      geo_type == "State" ~ "State",
      TRUE ~ "Metro"
    )) %>%
    ggplot(aes(x = jobs_growth, y = gdp_growth, size = gdp_per_job, color = geo_type_label)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(alpha = 0.6) +
    geom_text(aes(label = geo_area), 
              size = 3, 
              vjust = -0.8, 
              check_overlap = TRUE,
              show.legend = FALSE) +
    scale_size_continuous(labels = dollar, 
                         range = c(3, 15),
                         name = "GDP per Job") +
    scale_color_manual(values = c("Metro" = "#1565C0", "State" = "#2E7D32", "National" = "#D84315"),
                      name = "Geography Type") +
    scale_x_continuous(labels = label_percent(scale = 1)) +
    scale_y_continuous(labels = label_percent(scale = 1)) +
    labs(
      title = "GDP Growth vs. Jobs Growth by Geography",
      subtitle = paste("Year-over-Year Growth |", recent_year),
      x = "Jobs Growth (%)", 
      y = "GDP Growth (%)",
      caption = "Source: CICP Advanced Industries Dashboard\nBubble size represents GDP per job (productivity)"
    ) +
    theme(legend.position = "right")
  
  ggsave(file.path(viz_dir, "08_gdp_vs_jobs_growth_scatter.png"), 
         p8, width = 14, height = 9, dpi = 300, bg = "white")
  
  # Interactive version
  p8_interactive <- plot_ly(
    p8_data,
    x = ~jobs_growth,
    y = ~gdp_growth,
    text = ~geo_area,
    type = "scatter",
    mode = "markers",
    marker = list(
      opacity = 0.6,
      size = ~gdp_per_job,
      sizemode = "diameter",
      sizeref = max(p8_data$gdp_per_job) / 50,
      line = list(color = "white", width = 1),
      color = ~case_when(
        geo_type == "US" ~ "#D84315",
        geo_type == "State" ~ "#2E7D32",
        TRUE ~ "#1565C0"
      )
    ),
    hovertemplate = paste0(
      "<b>%{text}</b><br>",
      "Jobs Growth: %{x:.1f}%<br>",
      "GDP Growth: %{y:.1f}%<br>",
      "GDP per Job: $", format(p8_data$gdp_per_job, big.mark = ",", digits = 2, nsmall = 0), "<br>",
      "Jobs Start: ", format(p8_data$total_jobs_start, big.mark = ",", nsmall = 0), "<br>",
      "Jobs End: ", format(p8_data$total_jobs_end, big.mark = ",", nsmall = 0), "<br>",
      "GRP Start: $", format(p8_data$total_grp_start, big.mark = ",", nsmall = 0), "<br>",
      "GRP End: $", format(p8_data$total_grp_end, big.mark = ",", nsmall = 0), "<br>",
      "<extra></extra>"
    )
  ) %>%
    add_annotations(
      x = p8_data$jobs_growth,
      y = p8_data$gdp_growth,
      text = p8_data$geo_area,
      showarrow = FALSE,
      yshift = 10,
      font = list(size = 9, color = "gray30")
    ) %>%
    layout(
      title = list(
        text = paste0("<b>GDP Growth vs. Jobs Growth by Geography</b><br><sup>Year-over-Year Growth | ", 
                     recent_year, " | Bubble size = GDP per Job (Productivity)</sup>"),
        font = list(size = 18)
      ),
      xaxis = list(
        title = "Jobs Growth (%)",
        showgrid = TRUE,
        gridcolor = "lightgray",
        zeroline = TRUE,
        zerolinecolor = "gray",
        zerolinewidth = 2
      ),
      yaxis = list(
        title = "GDP Growth (%)",
        showgrid = TRUE,
        gridcolor = "lightgray",
        zeroline = TRUE,
        zerolinecolor = "gray",
        zerolinewidth = 2
      ),
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      margin = list(l = 80, r = 80, t = 100, b = 80),
      showlegend = FALSE
    )
  
  htmlwidgets::saveWidget(
    p8_interactive,
    file.path(viz_dir, "08_gdp_vs_jobs_growth_scatter.html"),
    selfcontained = TRUE
  )
  
  # Export data with all columns for verification
  write_csv(p8_data, 
            file.path(output_dir, "gdp_vs_jobs_scatter_data.csv"))
  
  cat("Visualization 8 created successfully\n")
  cat("Data exported to gdp_vs_jobs_scatter_data.csv for verification\n")
  
} else {
  cat("Warning: No data available for GDP vs Jobs Growth scatter plot\n")
}

# ============================================================================
# CICP Advanced Industries - Additional Comparative Visualizations (9-14)
# ============================================================================
# Add these to your existing visualization script after Visualization 8

# VISUALIZATION 9: Location Quotient Heatmap ---------------------------------

cat("\n=== Creating Visualization 9: Location Quotient Heatmap ===\n")

# Calculate location quotients for top metros
lq_data <- jobs_data %>%
  filter(display_level == 0,
         year == recent_year,
         initiative != "Total Employment") %>%
  # Get US percentages as baseline
  cross_join(
    jobs_data %>%
      filter(display_level == 0, year == recent_year, 
             geo_area == "United States", initiative == "Total Employment") %>%
      select(us_total = jobs)
  ) %>%
  left_join(
    jobs_data %>%
      filter(display_level == 0, year == recent_year, 
             geo_area == "United States", initiative != "Total Employment") %>%
      select(initiative, us_initiative_jobs = jobs),
    by = "initiative"
  ) %>%
  # Get metro totals
  left_join(
    jobs_data %>%
      filter(display_level == 0, year == recent_year,
             initiative == "Total Employment") %>%
      select(geo_area, metro_total = jobs),
    by = "geo_area"
  ) %>%
  mutate(
    metro_pct = (jobs / metro_total) * 100,
    us_pct = (us_initiative_jobs / us_total) * 100,
    location_quotient = metro_pct / us_pct
  ) %>%
  filter(geo_type == "Metro") %>%
  group_by(geo_area) %>%
  mutate(total_metro_jobs = first(metro_total)) %>%
  ungroup() %>%
  arrange(desc(total_metro_jobs)) %>%
  filter(geo_area %in% unique(geo_area)[1:15]) %>%  # Top 15 metros
  select(geo_area, initiative, location_quotient)

# Create heatmap
p9 <- lq_data %>%
  ggplot(aes(x = initiative, y = reorder(geo_area, location_quotient), fill = location_quotient)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", location_quotient)), size = 3) +
  scale_fill_gradient2(
    low = "#2166AC", 
    mid = "white", 
    high = "#B2182B",
    midpoint = 1,
    name = "Location\nQuotient",
    limits = c(0, max(lq_data$location_quotient, na.rm = TRUE))
  ) +
  labs(
    title = "Industry Specialization by Metro Area",
    subtitle = paste("Location Quotient vs. US Average |", recent_year, 
                    "| LQ > 1 indicates specialization"),
    x = NULL,
    y = NULL,
    caption = "Source: CICP Advanced Industries Dashboard\nLocation Quotient = (Metro % in Initiative) / (US % in Initiative)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    panel.grid = element_blank(),
    legend.position = "right"
  )

ggsave(file.path(viz_dir, "09_location_quotient_heatmap.png"), 
       p9, width = 14, height = 10, dpi = 300, bg = "white")

# Interactive version
p9_interactive <- plot_ly(
  lq_data,
  x = ~initiative,
  y = ~reorder(geo_area, location_quotient),
  z = ~location_quotient,
  type = "heatmap",
  colors = colorRamp(c("#2166AC", "white", "#B2182B")),
  zmid = 1,
  text = ~sprintf("%.2f", location_quotient),
  texttemplate = "%{text}",
  textfont = list(size = 10),
  hovertemplate = paste0(
    "<b>%{y}</b><br>",
    "%{x}<br>",
    "Location Quotient: %{z:.2f}<br>",
    "<extra></extra>"
  ),
  colorbar = list(title = "Location\nQuotient")
) %>%
  layout(
    title = list(
      text = paste0("<b>Industry Specialization by Metro Area</b><br><sup>Location Quotient vs. US Average | ", 
                   recent_year, " | LQ > 1 indicates specialization</sup>"),
      font = list(size = 18)
    ),
    xaxis = list(title = "", tickangle = 45),
    yaxis = list(title = ""),
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    margin = list(l = 200, r = 100, t = 100, b = 150)
  )

htmlwidgets::saveWidget(
  p9_interactive,
  file.path(viz_dir, "09_location_quotient_heatmap.html"),
  selfcontained = TRUE
)

# VISUALIZATION 10: Initiative Mix Faceted Comparison -------------------------

cat("\n=== Creating Visualization 10: Initiative Mix Faceted Comparison ===\n")

# Prepare data
initiative_mix_data <- jobs_data %>%
  filter(display_level == 0,
         year == recent_year,
         initiative != "Total Employment") %>%
  left_join(
    jobs_data %>%
      filter(display_level == 0, year == recent_year,
             initiative == "Total Employment") %>%
      select(geo_area, total_employment = jobs),
    by = "geo_area"
  ) %>%
  mutate(share = (jobs / total_employment) * 100) %>%
  filter(geo_type %in% c("Metro", "State", "US")) %>%
  mutate(
    geo_category = case_when(
      geo_area == "United States" ~ "National",
      geo_area == "Indiana" ~ "State",
      TRUE ~ "Metro"
    ),
    # Create custom ordering: US first, then IN, then metros alphabetically
    geo_order = case_when(
      geo_area == "United States" ~ "AAA_US",
      geo_area == "Indiana" ~ "AAB_Indiana",
      TRUE ~ paste0("B_", geo_area)
    )
  ) %>%
  arrange(geo_order)

# Create factor with proper order
geo_levels <- initiative_mix_data %>%
  distinct(geo_area, geo_order) %>%
  arrange(geo_order) %>%
  pull(geo_area)

initiative_mix_data <- initiative_mix_data %>%
  mutate(geo_area = factor(geo_area, levels = geo_levels))

p10 <- initiative_mix_data %>%
  ggplot(aes(x = geo_area, y = share, fill = geo_category)) +
  geom_col(alpha = 0.8) +
  facet_wrap(~initiative, nrow = 5, ncol = 1, scales = "free_y") +
  scale_fill_manual(
    values = c("Metro" = "#1565C0", "State" = "#2E7D32", "National" = "#D84315"),
    name = "Geography Type"
  ) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title = "Initiative Share of Total Employment by Geography",
    subtitle = paste("All Geographies |", recent_year),
    x = NULL,
    y = "Share of Total Employment (%)",
    caption = "Source: CICP Advanced Industries Dashboard"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7),
    strip.text = element_text(size = 11, face = "bold"),
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.spacing.y = unit(1, "lines")
  )

ggsave(file.path(viz_dir, "10_initiative_mix_faceted.png"), 
       p10, width = 16, height = 14, dpi = 300, bg = "white")

# Interactive version using subplot
initiative_list <- unique(initiative_mix_data$initiative)
color_map <- c("Metro" = "#1565C0", "State" = "#2E7D32", "National" = "#D84315")

plots_list <- list()

for(i in seq_along(initiative_list)) {
  init <- initiative_list[i]
  data_subset <- initiative_mix_data %>% 
    filter(initiative == init) %>%
    arrange(geo_area)  # Ensure consistent ordering
  
  p <- plot_ly(
    data_subset,
    x = ~geo_area,
    y = ~share,
    type = "bar",
    color = ~geo_category,
    colors = color_map,
    hovertemplate = paste0(
      "<b>%{x}</b><br>",
      init, "<br>",
      "Share: %{y:.1f}%<br>",
      "Total Jobs: %{customdata:,}<br>",
      "<extra></extra>"
    ),
    customdata = ~round(jobs, 0),
    showlegend = if(i == 1) TRUE else FALSE,
    legendgroup = ~geo_category,
    name = ~geo_category
  ) %>%
    layout(
      xaxis = list(title = ""),
      yaxis = list(title = "Share (%)"),
      annotations = list(
        list(
          x = 0.02,
          y = 0.98,
          text = paste0("<b>", init, "</b>"),
          xref = "paper",
          yref = "paper",
          xanchor = "left",
          yanchor = "top",
          showarrow = FALSE,
          font = list(size = 11, color = "black")
        )
      )
    )
  
  plots_list[[i]] <- p
}

p10_interactive <- subplot(
  plots_list,
  nrows = 5,
  shareX = TRUE,
  titleY = TRUE,
  titleX = FALSE,
  margin = 0.03
) %>%
  layout(
    title = list(
      text = paste0("<br><b>Initiative Share of Total Employment by Geography</b><br><sup>All Geographies | ", 
                   recent_year, "</sup>"),
      font = list(size = 16),
      y = 0.99
    ),
    xaxis5 = list(tickangle = 90, tickfont = list(size = 7)),
    height = 925,  # Reduced from 1000
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    margin = list(l = 60, r = 60, t = 100, b = 120),  # Increased top margin
    legend = list(
      orientation = "h",
      x = 0.5,
      y = 1.05,  # Moved up
      xanchor = "center",
      yanchor = "bottom"
    )
  )

htmlwidgets::saveWidget(
  p10_interactive,
  file.path(viz_dir, "10_initiative_mix_faceted.html"),
  selfcontained = TRUE
)

cat("Visualization 10 created (interactive + static)\n")

# VISUALIZATION 11: Wage Premium Analysis -------------------------------------

cat("\n=== Creating Visualization 11: Wage Premium Analysis ===\n")

# Calculate wage premiums
wage_premium_data <- wage_data %>%
  filter(display_level == 0,
         year == recent_year,
         initiative != "Total Employment",
         !is.na(wages)) %>%
  left_join(
    wage_data %>%
      filter(display_level == 0, year == recent_year,
             initiative == "Total Employment") %>%
      select(geo_area, total_wage = wages),
    by = "geo_area"
  ) %>%
  # Join jobs data for ranking
  left_join(
    jobs_data %>%
      filter(display_level == 0, year == recent_year,
             initiative != "Total Employment") %>%
      select(geo_area, initiative, jobs),
    by = c("geo_area", "initiative")
  ) %>%
  mutate(
    wage_premium = ((wages - total_wage) / total_wage) * 100,
    wage_ratio = wages / total_wage
  ) %>%
  filter(geo_type %in% c("Metro", "State", "US")) %>%
  mutate(
    geo_category = case_when(
      geo_area == "United States" ~ "National",
      geo_area == "Indiana" ~ "State",
      TRUE ~ "Metro"
    ),
    # Create custom ordering: US first, then IN, then metros alphabetically
    geo_order = case_when(
      geo_area == "United States" ~ "AAA_US",
      geo_area == "Indiana" ~ "AAB_Indiana",
      TRUE ~ paste0("B_", geo_area)
    )
  ) %>%
  arrange(geo_order)

# Create factor with proper order for y-axis (reverse for bottom-to-top display)
geo_levels <- wage_premium_data %>%
  distinct(geo_area, geo_order) %>%
  arrange(desc(geo_order)) %>%  # Reverse so US is at top
  pull(geo_area)

wage_premium_data <- wage_premium_data %>%
  mutate(
    geo_area_ordered = factor(geo_area, levels = geo_levels)
  )

p11 <- wage_premium_data %>%
  ggplot(aes(x = initiative, y = geo_area_ordered, fill = wage_premium)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.0f%%", wage_premium)), size = 3) +
  scale_fill_gradient2(
    low = "#B2182B",
    mid = "white",
    high = "#2166AC",
    midpoint = 0,
    name = "Wage\nPremium (%)",
    labels = label_percent(scale = 1)
  ) +
  labs(
    title = "Wage Premium by Initiative and Geography",
    subtitle = paste("% Difference from Total Employment Wage |", recent_year),
    x = NULL,
    y = NULL,
    caption = "Source: CICP Advanced Industries Dashboard\nPositive values indicate initiative wages exceed regional average"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    panel.grid = element_blank(),
    legend.position = "right"
  )

ggsave(file.path(viz_dir, "11_wage_premium_heatmap.png"), 
       p11, width = 12, height = 10, dpi = 300, bg = "white")

# Interactive version - reverse the order
geo_levels_plotly <- wage_premium_data %>%
  distinct(geo_area, geo_order) %>%
  arrange(desc(geo_order)) %>%
  pull(geo_area)

wage_premium_data_plotly <- wage_premium_data %>%
  mutate(
    geo_area_ordered = factor(geo_area, levels = geo_levels_plotly),
    hover_text = paste0(
      "<b>", geo_area, "</b><br>",
      initiative, "<br>",
      "Wage Premium: ", sprintf("%.1f%%", wage_premium), "<br>",
      "Initiative Wage: $", format(round(wages, 0), big.mark = ","), "<br>",
      "Regional Avg Wage: $", format(round(total_wage, 0), big.mark = ",")
    )
  )

p11_interactive <- plot_ly(
  wage_premium_data_plotly,
  x = ~initiative,
  y = ~geo_area_ordered,
  z = ~wage_premium,
  type = "heatmap",
  colors = colorRamp(c("#B2182B", "white", "#2166AC")),
  zmid = 0,
  text = ~sprintf("%.0f%%", wage_premium),
  texttemplate = "%{text}",
  textfont = list(size = 10),
  hovertext = ~hover_text,
  hoverinfo = "text",
  colorbar = list(title = "Wage<br>Premium (%)")
) %>%
  layout(
    title = list(
      text = paste0("<b>Wage Premium by Initiative and Geography</b><br><sup>% Difference from Total Employment Wage | ", 
                   recent_year, "<br>Positive values indicate initiative wages exceed regional average</sup>"),
      font = list(size = 16)
    ),
    xaxis = list(title = "", tickangle = 45),
    yaxis = list(title = ""),
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    margin = list(l = 200, r = 100, t = 120, b = 150)
  )

htmlwidgets::saveWidget(
  p11_interactive,
  file.path(viz_dir, "11_wage_premium_heatmap.html"),
  selfcontained = TRUE
)

# VISUALIZATION 12: Growth Rate Comparison by Initiative (Faceted Time Series) -------------

cat("\n=== Creating Visualization 12: Growth Rate Comparison Faceted ===\n")

# Prepare data - Indiana metros plus IN and US
growth_comparison_data <- jobs_data %>%
  filter(display_level == 0,
         initiative != "Total Employment",
         geo_area %in% c("Indiana", "United States") | geo_type == "Metro") %>%
  arrange(initiative, geo_area, year) %>%
  group_by(initiative, geo_area) %>%
  mutate(
    indexed_jobs = (jobs / jobs[year == min(year)]) * 100
  ) %>%
  ungroup() %>%
  mutate(
    geo_highlight = case_when(
      geo_area == "United States" ~ "US",
      geo_area == "Indiana" ~ "Indiana",
      TRUE ~ "Metro"
    ),
    line_size = if_else(geo_area %in% c("Indiana", "United States"), 1.5, 0.5),
    line_alpha = if_else(geo_area %in% c("Indiana", "United States"), 1, 0.3),
    is_projected = year > 2024
  )

# Split data into actual and projected
growth_actual <- growth_comparison_data %>% filter(year <= 2024)
growth_projected <- growth_comparison_data %>% filter(year >= 2024)

p12 <- ggplot(growth_comparison_data, aes(x = year, y = indexed_jobs, group = geo_area, 
             color = geo_highlight, size = line_size, alpha = line_alpha)) +
  # Actual lines (solid)
  geom_line(data = growth_actual) +
  # Projected lines (dashed)
  geom_line(data = growth_projected, linetype = "dashed") +
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray50", linewidth = 0.3) +
  facet_wrap(~initiative, scales = "free_y", ncol = 2) +
  scale_color_manual(
    values = c("Metro" = "gray60", "Indiana" = "#2E7D32", "US" = "#D84315"),
    name = NULL
  ) +
  scale_size_identity() +
  scale_alpha_identity() +
  scale_x_continuous(breaks = seq(min(growth_comparison_data$year), 
                                  max(growth_comparison_data$year), by = 2)) +
  labs(
    title = "Employment Growth Trends by Initiative",
    subtitle = paste("Indexed to", min(growth_comparison_data$year), "= 100 | Metros (gray) vs. Indiana (green) and US (red) | Dashed = projected"),
    x = "Year",
    y = "Indexed Employment (Base Year = 100)",
    caption = "Source: CICP Advanced Industries Dashboard"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(viz_dir, "12_growth_comparison_faceted.png"), 
       p12, width = 16, height = 12, dpi = 300, bg = "white")

# Interactive version with dropdown
initiative_list <- unique(growth_comparison_data$initiative)
base_year <- min(growth_comparison_data$year)

p12_interactive <- plot_ly()

# Add traces for each initiative
for(init in initiative_list) {
  # Metro traces (gray, thin, transparent)
  metro_list <- growth_comparison_data %>%
    filter(initiative == init, geo_highlight == "Metro") %>%
    pull(geo_area) %>%
    unique()
  
  for(metro in metro_list) {
    # Actual data (up to and including 2024)
    metro_actual <- growth_comparison_data %>% 
      filter(initiative == init, geo_area == metro, year <= 2024)
    
    if(nrow(metro_actual) > 0) {
      p12_interactive <- p12_interactive %>%
        add_trace(
          data = metro_actual,
          x = ~year,
          y = ~indexed_jobs,
          type = "scatter",
          mode = "lines",
          name = metro,
          line = list(color = "rgba(150, 150, 150, 0.3)", width = 1),
          visible = if(init == initiative_list[1]) TRUE else FALSE,
          legendgroup = "Metro",
          showlegend = FALSE,
          hoverinfo = "text",
          text = paste0("<b>", metro, "</b><br>",
                       "Year: ", metro_actual$year, "<br>",
                       "Indexed Jobs: ", sprintf("%.1f", metro_actual$indexed_jobs))
        )
    }
    
    # Projected data (from 2024 onward to connect)
    metro_projected <- growth_comparison_data %>% 
      filter(initiative == init, geo_area == metro, year >= 2024)
    
    if(nrow(metro_projected) > 0) {
      p12_interactive <- p12_interactive %>%
        add_trace(
          data = metro_projected,
          x = ~year,
          y = ~indexed_jobs,
          type = "scatter",
          mode = "lines",
          name = metro,
          line = list(color = "rgba(150, 150, 150, 0.3)", width = 1, dash = "dash"),
          visible = if(init == initiative_list[1]) TRUE else FALSE,
          legendgroup = "Metro",
          showlegend = FALSE,
          hoverinfo = "text",
          text = paste0("<b>", metro, ifelse(metro_projected$year > 2024, " (Projected)", ""), "</b><br>",
                       "Year: ", metro_projected$year, "<br>",
                       "Indexed Jobs: ", sprintf("%.1f", metro_projected$indexed_jobs))
        )
    }
  }
  
  # Indiana traces
  indiana_actual <- growth_comparison_data %>%
    filter(initiative == init, geo_area == "Indiana", year <= 2024)
  
  if(nrow(indiana_actual) > 0) {
    p12_interactive <- p12_interactive %>%
      add_trace(
        data = indiana_actual,
        x = ~year,
        y = ~indexed_jobs,
        type = "scatter",
        mode = "lines+markers",
        name = "Indiana",
        line = list(color = "#2E7D32", width = 3),
        marker = list(size = 6, color = "#2E7D32"),
        visible = if(init == initiative_list[1]) TRUE else FALSE,
        legendgroup = "Indiana",
        showlegend = if(init == initiative_list[1]) TRUE else FALSE,
        hoverinfo = "text",
        text = paste0("<b>Indiana</b><br>",
                     "Year: ", indiana_actual$year, "<br>",
                     "Indexed Jobs: ", sprintf("%.1f", indiana_actual$indexed_jobs))
      )
  }
  
  indiana_projected <- growth_comparison_data %>%
    filter(initiative == init, geo_area == "Indiana", year >= 2024)
  
  if(nrow(indiana_projected) > 0) {
    p12_interactive <- p12_interactive %>%
      add_trace(
        data = indiana_projected,
        x = ~year,
        y = ~indexed_jobs,
        type = "scatter",
        mode = "lines+markers",
        name = "Indiana",
        line = list(color = "#2E7D32", width = 3, dash = "dash"),
        marker = list(size = 6, color = "#2E7D32", 
                     symbol = ifelse(indiana_projected$year > 2024, "circle-open", "circle"), 
                     line = list(width = 2)),
        visible = if(init == initiative_list[1]) TRUE else FALSE,
        legendgroup = "Indiana",
        showlegend = FALSE,
        hoverinfo = "text",
        text = paste0("<b>Indiana", ifelse(indiana_projected$year > 2024, " (Projected)", ""), "</b><br>",
                     "Year: ", indiana_projected$year, "<br>",
                     "Indexed Jobs: ", sprintf("%.1f", indiana_projected$indexed_jobs))
      )
  }
  
  # US traces
  us_actual <- growth_comparison_data %>%
    filter(initiative == init, geo_area == "United States", year <= 2024)
  
  if(nrow(us_actual) > 0) {
    p12_interactive <- p12_interactive %>%
      add_trace(
        data = us_actual,
        x = ~year,
        y = ~indexed_jobs,
        type = "scatter",
        mode = "lines+markers",
        name = "United States",
        line = list(color = "#D84315", width = 3),
        marker = list(size = 6, color = "#D84315"),
        visible = if(init == initiative_list[1]) TRUE else FALSE,
        legendgroup = "US",
        showlegend = if(init == initiative_list[1]) TRUE else FALSE,
        hoverinfo = "text",
        text = paste0("<b>United States</b><br>",
                     "Year: ", us_actual$year, "<br>",
                     "Indexed Jobs: ", sprintf("%.1f", us_actual$indexed_jobs))
      )
  }
  
  us_projected <- growth_comparison_data %>%
    filter(initiative == init, geo_area == "United States", year >= 2024)
  
  if(nrow(us_projected) > 0) {
    p12_interactive <- p12_interactive %>%
      add_trace(
        data = us_projected,
        x = ~year,
        y = ~indexed_jobs,
        type = "scatter",
        mode = "lines+markers",
        name = "United States",
        line = list(color = "#D84315", width = 3, dash = "dash"),
        marker = list(size = 6, color = "#D84315", 
                     symbol = ifelse(us_projected$year > 2024, "circle-open", "circle"), 
                     line = list(width = 2)),
        visible = if(init == initiative_list[1]) TRUE else FALSE,
        legendgroup = "US",
        showlegend = FALSE,
        hoverinfo = "text",
        text = paste0("<b>United States", ifelse(us_projected$year > 2024, " (Projected)", ""), "</b><br>",
                     "Year: ", us_projected$year, "<br>",
                     "Indexed Jobs: ", sprintf("%.1f", us_projected$indexed_jobs))
      )
  }
}

# Count traces per initiative (each geo has 2 traces: actual + projected)
n_metros <- growth_comparison_data %>%
  filter(geo_highlight == "Metro") %>%
  distinct(geo_area) %>%
  nrow()
traces_per_initiative <- (n_metros * 2) + 4  # (metros * 2) + (IN * 2) + (US * 2)

# Create dropdown menu
updatemenus <- list(
  list(
    active = 0,
    type = "dropdown",
    x = 0.15,
    y = 1.15,
    xanchor = "left",
    yanchor = "top",
    buttons = lapply(seq_along(initiative_list), function(i) {
      visible_vector <- rep(FALSE, length(initiative_list) * traces_per_initiative)
      start_idx <- (i - 1) * traces_per_initiative + 1
      end_idx <- i * traces_per_initiative
      visible_vector[start_idx:end_idx] <- TRUE
      
      list(
        method = "update",
        args = list(
          list(visible = visible_vector),
          list(title = list(
            text = paste0("<b>Employment Growth Trends - ", initiative_list[i], 
                         "</b><br><sup>Indexed to ", base_year, " = 100 | Metros (gray) vs. Indiana (green) and US (red) | Dashed = projected</sup>")
          ))
        ),
        label = initiative_list[i]
      )
    })
  )
)

p12_interactive <- p12_interactive %>%
  layout(
    title = list(
      text = paste0("<b>Employment Growth Trends - ", initiative_list[1], 
                   "</b><br><sup>Indexed to ", base_year, " = 100 | Metros (gray) vs. Indiana (green) and US (red) | Dashed = projected</sup>"),
      font = list(size = 16)
    ),
    xaxis = list(
      title = "Year",
      showgrid = TRUE,
      gridcolor = "lightgray"
    ),
    yaxis = list(
      title = "Indexed Employment (Base Year = 100)",
      showgrid = TRUE,
      gridcolor = "lightgray"
    ),
    updatemenus = updatemenus,
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    margin = list(l = 80, r = 80, t = 100, b = 80),
    legend = list(
      orientation = "h",
      x = 0.5,
      y = -0.15,
      xanchor = "center",
      yanchor = "top"
    ),
    shapes = list(
      list(
        type = "line",
        x0 = min(growth_comparison_data$year),
        x1 = max(growth_comparison_data$year),
        y0 = 100,
        y1 = 100,
        line = list(color = "gray", width = 1, dash = "dash")
      )
    )
  )

htmlwidgets::saveWidget(
  p12_interactive,
  file.path(viz_dir, "12_growth_comparison_interactive.html"),
  selfcontained = TRUE
)

cat("Visualization 12 created (interactive + static)\n")

# VISUALIZATION 13: Growth vs. Level Quadrant Chart ---------------------------

cat("\n=== Creating Visualization 13: Growth vs. Level Quadrant Chart ===\n")

# Calculate for metros only
quadrant_data <- jobs_growth %>%
  filter(display_level == 0,
         year == recent_year,
         geo_type == "Metro",
         initiative != "Total Employment",
         !is.na(cagr_5yr)) %>%
  left_join(
    jobs_data %>%
      filter(display_level == 0, year == recent_year,
             initiative == "Total Employment") %>%
      select(geo_area, total_employment = jobs),
    by = "geo_area"
  ) %>%
  mutate(
    employment_share = (jobs / total_employment) * 100
  ) %>%
  group_by(initiative) %>%
  mutate(
    median_growth = median(cagr_5yr, na.rm = TRUE),
    median_share = median(employment_share, na.rm = TRUE),
    quadrant = case_when(
      cagr_5yr >= median_growth & employment_share >= median_share ~ "Stars",
      cagr_5yr >= median_growth & employment_share < median_share ~ "Emerging",
      cagr_5yr < median_growth & employment_share >= median_share ~ "Mature",
      TRUE ~ "Declining"
    )
  ) %>%
  ungroup()

# Get IN and US benchmarks
benchmark_data <- jobs_growth %>%
  filter(display_level == 0,
         year == recent_year,
         geo_area %in% c("Indiana", "United States"),
         initiative != "Total Employment",
         !is.na(cagr_5yr)) %>%
  left_join(
    jobs_data %>%
      filter(display_level == 0, year == recent_year,
             initiative == "Total Employment",
             geo_area %in% c("Indiana", "United States")) %>%
      select(geo_area, total_employment = jobs),
    by = "geo_area"
  ) %>%
  mutate(
    employment_share = (jobs / total_employment) * 100
  )

p13 <- quadrant_data %>%
  ggplot(aes(x = employment_share, y = cagr_5yr)) +
  geom_hline(aes(yintercept = median_growth), linetype = "dashed", color = "gray50") +
  geom_vline(aes(xintercept = median_share), linetype = "dashed", color = "gray50") +
  geom_point(aes(color = quadrant, size = jobs), alpha = 0.6) +
  geom_point(data = benchmark_data, aes(shape = geo_area), 
             size = 4, color = "black", stroke = 2) +
  facet_wrap(~initiative, scales = "free", ncol = 2) +
  scale_color_manual(
    values = c("Stars" = "#2E7D32", "Emerging" = "#1565C0", 
               "Mature" = "#F57C00", "Declining" = "#C62828"),
    name = "Quadrant"
  ) +
  scale_shape_manual(
    values = c("Indiana" = 17, "United States" = 15),
    name = "Benchmark"
  ) +
  scale_size_continuous(range = c(2, 10), name = "Total Jobs", labels = comma) +
  scale_x_continuous(labels = label_percent(scale = 1)) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title = "Strategic Positioning: Growth vs. Employment Share",
    subtitle = paste("5-Year CAGR vs. Share of Total Employment |", recent_year, 
                    "| Quadrants based on median values"),
    x = "Share of Total Employment (%)",
    y = "5-Year CAGR (%)",
    caption = "Source: CICP Advanced Industries Dashboard\nDashed lines represent median values for each initiative"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

ggsave(file.path(viz_dir, "13_growth_vs_level_quadrant.png"), 
       p13, width = 16, height = 12, dpi = 300, bg = "white")


# Interactive version with dropdown
initiative_list <- unique(quadrant_data$initiative)
quadrant_colors <- c("Stars" = "#2E7D32", "Emerging" = "#1565C0", 
                    "Mature" = "#F57C00", "Declining" = "#C62828")

p13_interactive <- plot_ly()

# Add traces for each initiative
for(init in initiative_list) {
  init_data <- quadrant_data %>% filter(initiative == init)
  init_benchmarks <- benchmark_data %>% filter(initiative == init)
  
  median_g <- first(init_data$median_growth)
  median_s <- first(init_data$median_share)
  
  # Calculate log scale for bubble sizes
  min_jobs <- min(init_data$jobs, na.rm = TRUE)
  max_jobs <- max(init_data$jobs, na.rm = TRUE)
  
  # Add metro points by quadrant with log scale sizing
  for(quad in c("Stars", "Emerging", "Mature", "Declining")) {
    quad_data <- init_data %>% 
      filter(quadrant == quad) %>%
      mutate(
        # Use log scale with a minimum size
        bubble_size = 5 + 20 * (log10(jobs + 1) - log10(min_jobs + 1)) / (log10(max_jobs + 1) - log10(min_jobs + 1))
      )
    
    if(nrow(quad_data) > 0) {
      p13_interactive <- p13_interactive %>%
        add_trace(
          data = quad_data,
          x = ~employment_share,
          y = ~cagr_5yr,
          type = "scatter",
          mode = "markers",
          name = quad,
          marker = list(
            color = quadrant_colors[quad],
            size = ~bubble_size,
            opacity = 0.6,
            line = list(color = "white", width = 1)
          ),
          visible = if(init == initiative_list[1]) TRUE else FALSE,
          legendgroup = quad,
          showlegend = if(init == initiative_list[1]) TRUE else FALSE,
          hoverinfo = "text",
          text = paste0("<b>", quad_data$geo_area, "</b><br>",
                       "Quadrant: ", quad, "<br>",
                       "Share: ", sprintf("%.1f%%", quad_data$employment_share), "<br>",
                       "5-Year CAGR: ", sprintf("%.1f%%", quad_data$cagr_5yr), "<br>",
                       "Total Jobs: ", format(round(quad_data$jobs, 0), big.mark = ","))
        )
    }
  }
  
  # Add Indiana benchmark
  in_data <- init_benchmarks %>% filter(geo_area == "Indiana")
  if(nrow(in_data) > 0) {
    p13_interactive <- p13_interactive %>%
      add_trace(
        data = in_data,
        x = ~employment_share,
        y = ~cagr_5yr,
        type = "scatter",
        mode = "markers",
        name = "Indiana",
        marker = list(
          symbol = "diamond",
          size = 15,
          color = "black",
          line = list(color = "black", width = 2)
        ),
        visible = if(init == initiative_list[1]) TRUE else FALSE,
        legendgroup = "Indiana",
        showlegend = if(init == initiative_list[1]) TRUE else FALSE,
        hoverinfo = "text",
        text = paste0("<b>Indiana</b><br>",
                     "Share: ", sprintf("%.1f%%", in_data$employment_share), "<br>",
                     "5-Year CAGR: ", sprintf("%.1f%%", in_data$cagr_5yr), "<br>",
                     "Total Jobs: ", format(round(in_data$jobs, 0), big.mark = ","))
      )
  }
  
  # Add US benchmark
  us_data <- init_benchmarks %>% filter(geo_area == "United States")
  if(nrow(us_data) > 0) {
    p13_interactive <- p13_interactive %>%
      add_trace(
        data = us_data,
        x = ~employment_share,
        y = ~cagr_5yr,
        type = "scatter",
        mode = "markers",
        name = "United States",
        marker = list(
          symbol = "square",
          size = 15,
          color = "black",
          line = list(color = "black", width = 2)
        ),
        visible = if(init == initiative_list[1]) TRUE else FALSE,
        legendgroup = "US",
        showlegend = if(init == initiative_list[1]) TRUE else FALSE,
        hoverinfo = "text",
        text = paste0("<b>United States</b><br>",
                     "Share: ", sprintf("%.1f%%", us_data$employment_share), "<br>",
                     "5-Year CAGR: ", sprintf("%.1f%%", us_data$cagr_5yr), "<br>",
                     "Total Jobs: ", format(round(us_data$jobs, 0), big.mark = ","))
      )
  }
}

# Count traces per initiative (4 quadrants + 2 benchmarks)
traces_per_initiative <- 6

# Create dropdown menu
updatemenus <- list(
  list(
    active = 0,
    type = "dropdown",
    x = 0.15,
    y = 1.15,
    xanchor = "left",
    yanchor = "top",
    buttons = lapply(seq_along(initiative_list), function(i) {
      init <- initiative_list[i]
      init_data <- quadrant_data %>% filter(initiative == init)
      median_g <- first(init_data$median_growth)
      median_s <- first(init_data$median_share)
      
      visible_vector <- rep(FALSE, length(initiative_list) * traces_per_initiative)
      start_idx <- (i - 1) * traces_per_initiative + 1
      end_idx <- i * traces_per_initiative
      visible_vector[start_idx:end_idx] <- TRUE
      
      list(
        method = "update",
        args = list(
          list(visible = visible_vector),
          list(
            title = list(
              text = paste0("<b>Strategic Positioning: Growth vs. Employment Share - ", 
                           init, "</b><br><sup>5-Year CAGR vs. Share of Total Employment | ", 
                           recent_year, " | Quadrants based on median values</sup>")
            ),
            shapes = list(
              # Vertical line at median share
              list(
                type = "line",
                x0 = median_s,
                x1 = median_s,
                y0 = 0,
                y1 = 1,
                yref = "paper",
                line = list(color = "gray", width = 1, dash = "dash")
              ),
              # Horizontal line at median growth
              list(
                type = "line",
                x0 = 0,
                x1 = 1,
                xref = "paper",
                y0 = median_g,
                y1 = median_g,
                line = list(color = "gray", width = 1, dash = "dash")
              )
            )
          )
        ),
        label = init
      )
    })
  )
)

# Get initial median lines
init_data <- quadrant_data %>% filter(initiative == initiative_list[1])
median_g_init <- first(init_data$median_growth)
median_s_init <- first(init_data$median_share)

p13_interactive <- p13_interactive %>%
  layout(
    title = list(
      text = paste0("<b>Strategic Positioning: Growth vs. Employment Share - ", 
                   initiative_list[1], "</b><br><sup>5-Year CAGR vs. Share of Total Employment | ", 
                   recent_year, " | Quadrants based on median values</sup>"),
      font = list(size = 16)
    ),
    xaxis = list(
      title = "Share of Total Employment (%)",
      showgrid = TRUE,
      gridcolor = "lightgray"
    ),
    yaxis = list(
      title = "5-Year CAGR (%)",
      showgrid = TRUE,
      gridcolor = "lightgray"
    ),
    updatemenus = updatemenus,
    shapes = list(
      # Vertical line at median share
      list(
        type = "line",
        x0 = median_s_init,
        x1 = median_s_init,
        y0 = 0,
        y1 = 1,
        yref = "paper",
        line = list(color = "gray", width = 1, dash = "dash")
      ),
      # Horizontal line at median growth
      list(
        type = "line",
        x0 = 0,
        x1 = 1,
        xref = "paper",
        y0 = median_g_init,
        y1 = median_g_init,
        line = list(color = "gray", width = 1, dash = "dash")
      )
    ),
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    margin = list(l = 80, r = 150, t = 100, b = 80),
    legend = list(
      orientation = "v",
      x = 1.02,
      y = 0.5,
      font = list(size = 11)
    )
  )

htmlwidgets::saveWidget(
  p13_interactive,
  file.path(viz_dir, "13_growth_vs_level_quadrant_interactive.html"),
  selfcontained = TRUE
)

cat("Visualization 13 created (interactive + static)\n")

# VISUALIZATION 14: Acceleration/Deceleration Analysis ------------------------

cat("\n=== Creating Visualization 14: Acceleration/Deceleration Analysis ===\n")

# Set max year to exclude projections
max_est_year <- 2024

# Calculate early vs. late period CAGRs
accel_data <- jobs_data %>%
  filter(display_level == 0,
         initiative != "Total Employment",
         geo_type %in% c("Metro", "State", "US")) %>%
  arrange(initiative, geo_area, year) %>%
  group_by(initiative, geo_area, geo_type) %>%
  summarise(
    # Early period: first 3 years with data
    years_available = n(),
    min_year = min(year),
    max_year = max_est_year,
    mid_year = min_year + ceiling((max_year - min_year) / 2),
    # Calculate CAGRs for two periods
    early_cagr = if_else(
      years_available >= 6,
      ((jobs[year == mid_year] / jobs[year == min_year])^(1/(mid_year - min_year)) - 1) * 100,
      NA_real_
    ),
    late_cagr = if_else(
      years_available >= 6,
      ((jobs[year == max_year] / jobs[year == mid_year])^(1/(max_year - mid_year)) - 1) * 100,
      NA_real_
    ),
    total_jobs = jobs[year == max_year],
    .groups = "drop"
  ) %>%
  filter(!is.na(early_cagr), !is.na(late_cagr),
         !is.infinite(early_cagr), !is.infinite(late_cagr)) %>%
  mutate(
    acceleration = late_cagr - early_cagr,
    trend = case_when(
      late_cagr > early_cagr ~ "Accelerating",
      TRUE ~ "Decelerating"
    ),
    geo_category = case_when(
      geo_area == "United States" ~ "National",
      geo_area == "Indiana" ~ "State",
      TRUE ~ "Metro"
    )
  )

p14 <- accel_data %>%
  ggplot(aes(x = early_cagr, y = late_cagr, color = geo_category)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
  geom_point(alpha = 0.6, size = 3) +
  geom_text(data = filter(accel_data, geo_area %in% c("Indiana", "United States")),
            aes(label = geo_area), vjust = -1, size = 3, show.legend = FALSE) +
  facet_wrap(~initiative, scales = "free", ncol = 2) +
  scale_color_manual(
    values = c("Metro" = "#1565C0", "State" = "#2E7D32", "National" = "#D84315"),
    name = "Geography Type"
  ) +
  scale_x_continuous(labels = label_percent(scale = 1)) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title = "Employment Growth Acceleration/Deceleration by Initiative",
    subtitle = paste("Early Period CAGR vs. Late Period CAGR |", accel_data$min_year, "through", max_est_year, "| Points above line = accelerating growth"),
    x = paste("Early Period CAGR (%)"),
    y = paste("Late Period CAGR (%)"),
    caption = paste0("Source: CICP Advanced Industries Dashboard\nPeriods split at midpoint (", accel_data$mid_year, "); projections excluded") 
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

ggsave(file.path(viz_dir, "14_acceleration_deceleration.png"), 
       p14, width = 16, height = 12, dpi = 300, bg = "white")

# Interactive version with dropdown
initiative_list <- unique(accel_data$initiative)
geo_colors <- c("Metro" = "#1565C0", "State" = "#2E7D32", "National" = "#D84315")

p14_interactive <- plot_ly()

# Add traces for each initiative
for(init in initiative_list) {
  init_data <- accel_data %>% filter(initiative == init)
  
  # Add metro points
  metro_data <- init_data %>% filter(geo_category == "Metro")
  
  if(nrow(metro_data) > 0) {
    p14_interactive <- p14_interactive %>%
      add_trace(
        data = metro_data,
        x = ~early_cagr,
        y = ~late_cagr,
        type = "scatter",
        mode = "markers",
        name = "Metro",
        marker = list(
          color = geo_colors["Metro"],
          size = 8,
          opacity = 0.6,
          line = list(color = "white", width = 1)
        ),
        visible = if(init == initiative_list[1]) TRUE else FALSE,
        legendgroup = "Metro",
        showlegend = if(init == initiative_list[1]) TRUE else FALSE,
        hoverinfo = "text",
        text = paste0("<b>", metro_data$geo_area, "</b><br>",
                     "Early CAGR: ", sprintf("%.1f%%", metro_data$early_cagr), "<br>",
                     "Late CAGR: ", sprintf("%.1f%%", metro_data$late_cagr), "<br>",
                     "Trend: ", metro_data$trend)
      )
  }
  
  # Add Indiana point
  in_data <- init_data %>% filter(geo_area == "Indiana")
  
  if(nrow(in_data) > 0) {
    p14_interactive <- p14_interactive %>%
      add_trace(
        data = in_data,
        x = ~early_cagr,
        y = ~late_cagr,
        type = "scatter",
        mode = "markers+text",
        name = "Indiana",
        marker = list(
          color = geo_colors["State"],
          size = 12,
          opacity = 0.8,
          line = list(color = "white", width = 2)
        ),
        text = "Indiana",
        textposition = "top",
        textfont = list(size = 10, color = "black"),
        visible = if(init == initiative_list[1]) TRUE else FALSE,
        legendgroup = "State",
        showlegend = if(init == initiative_list[1]) TRUE else FALSE,
        hoverinfo = "text",
        hovertext = paste0("<b>Indiana</b><br>",
                          "Early CAGR: ", sprintf("%.1f%%", in_data$early_cagr), "<br>",
                          "Late CAGR: ", sprintf("%.1f%%", in_data$late_cagr), "<br>",
                          "Trend: ", in_data$trend)
      )
  }
  
  # Add US point
  us_data <- init_data %>% filter(geo_area == "United States")
  
  if(nrow(us_data) > 0) {
    p14_interactive <- p14_interactive %>%
      add_trace(
        data = us_data,
        x = ~early_cagr,
        y = ~late_cagr,
        type = "scatter",
        mode = "markers+text",
        name = "United States",
        marker = list(
          color = geo_colors["National"],
          size = 12,
          opacity = 0.8,
          line = list(color = "white", width = 2)
        ),
        text = "United States",
        textposition = "top",
        textfont = list(size = 10, color = "black"),
        visible = if(init == initiative_list[1]) TRUE else FALSE,
        legendgroup = "National",
        showlegend = if(init == initiative_list[1]) TRUE else FALSE,
        hoverinfo = "text",
        hovertext = paste0("<b>United States</b><br>",
                          "Early CAGR: ", sprintf("%.1f%%", us_data$early_cagr), "<br>",
                          "Late CAGR: ", sprintf("%.1f%%", us_data$late_cagr), "<br>",
                          "Trend: ", us_data$trend)
      )
  }
}

# Count traces per initiative (metro + state + national)
traces_per_initiative <- 3

# Create dropdown menu
updatemenus <- list(
  list(
    active = 0,
    type = "dropdown",
    x = 0.15,
    y = 1.15,
    xanchor = "left",
    yanchor = "top",
    buttons = lapply(seq_along(initiative_list), function(i) {
      visible_vector <- rep(FALSE, length(initiative_list) * traces_per_initiative)
      start_idx <- (i - 1) * traces_per_initiative + 1
      end_idx <- i * traces_per_initiative
      visible_vector[start_idx:end_idx] <- TRUE
      
      list(
        method = "update",
        args = list(
          list(visible = visible_vector),
          list(title = list(
            text = paste0("<b>Employment Growth Acceleration/Deceleration - ", initiative_list[i], 
                         "</b><br><sup>Early Period CAGR vs. Late Period CAGR | ", accel_data$min_year, " through ", max_est_year, " | Points above line = accelerating growth</sup>")
          ))
        ),
        label = initiative_list[i]
      )
    })
  )
)

p14_interactive <- p14_interactive %>%
  layout(
    title = list(
      text = paste0("<b>Employment Growth Acceleration/Deceleration - ", initiative_list[1], 
                   "</b><br><sup>Early Period CAGR vs. Late Period CAGR | ", accel_data$min_year, " through ", max_est_year, " | Points above line = accelerating growth</sup>"),
      font = list(size = 16)
    ),
    xaxis = list(
      title = "Early Period CAGR (%)",
      showgrid = TRUE,
      gridcolor = "lightgray"
    ),
    yaxis = list(
      title = "Late Period CAGR (%)",
      showgrid = TRUE,
      gridcolor = "lightgray"
    ),
    updatemenus = updatemenus,
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    margin = list(l = 80, r = 150, t = 100, b = 80),
    legend = list(
      orientation = "v",
      x = 1.02,
      y = 0.5,
      font = list(size = 11)
    ),
    shapes = list(
      list(
        type = "line",
        x0 = 0,
        x1 = 1,
        xref = "paper",
        y0 = 0,
        y1 = 1,
        yref = "paper",
        line = list(color = "gray", width = 1, dash = "dash")
      )
    )
  )

htmlwidgets::saveWidget(
  p14_interactive,
  file.path(viz_dir, "14_acceleration_deceleration_interactive.html"),
  selfcontained = TRUE
)

cat("Visualization 14 created (interactive + static)\n")

# SUMMARY ---------------------------------------------------------------------

cat("\n", rep("=", 70), "\n", sep = "")
cat("COMPARATIVE VISUALIZATIONS CREATED (9-14)\n")
cat(rep("=", 70), "\n\n", sep = "")
cat("Visualizations saved to:", viz_dir, "\n\n")
cat("  9. Location Quotient Heatmap (interactive + static)\n")
cat(" 10. Initiative Mix Faceted Comparison (static)\n")
cat(" 11. Wage Premium Analysis (interactive + static)\n")
cat(" 12. Growth Rate Comparison Faceted Time Series (static)\n")
cat(" 13. Growth vs. Level Quadrant Chart (static)\n")
cat(" 14. Acceleration/Deceleration Analysis (static)\n")
cat("\n", rep("=", 70), "\n", sep = "")

# SUMMARY ----------------------------------------------------------------------

cat("\n", rep("=", 70), "\n", sep = "")
cat("VISUALIZATIONS CREATED\n")
cat(rep("=", 70), "\n\n", sep = "")
cat("Static PNG files and interactive HTML files saved to:\n")
cat(sprintf("  %s\n\n", viz_dir))
cat("Charts created:\n")
cat("  1. Indiana Jobs by Initiative\n")
cat("  2. Indiana Growth Rates by Initiative\n")
cat("  3. Top 10 Metro Areas by Total Jobs\n")
cat("  4. Indiana Average Wages by Initiative\n")
cat("  5. Top 10 Metro Areas Growth Rates\n")
cat("  6. Initiative Mix in Top 10 Metro Areas\n")
cat("  7. Indiana Jobs Time Series\n")
cat("  8. GDP vs Jobs Growth Scatter Plot\n")
cat("\n", rep("=", 70), "\n", sep = "")_data <- wage_data %>%
  filter(display_level == 0, 
         year == recent_year, 
         geo_area == "Indiana",
         initiative != "Total Employment",
         !is.na(wages)) %>%
  arrange(desc(wages))

