# ============================================================================
# CICP Initiative Deep Dive - Industry-Level Visualizations
# ============================================================================
# Purpose: Create detailed visualizations for industries within initiatives
# Note: Run main analysis and deep dive prep scripts first
# ============================================================================

library(tidyverse)
library(scales)
library(plotly)
library(patchwork)

# Load processed data
output_dir <- "outputs_20250709"
load(file.path(output_dir, "processed_data_jobs_gdp.RData"))
load(file.path(output_dir, "processed_data_initiative_deepdive.RData"))

# Create visualizations directory
viz_dir <- file.path(output_dir, "visualizations")
if (!dir.exists(viz_dir)) {
  dir.create(viz_dir, recursive = TRUE)
}

# VISUALIZATION 15: Top Industries Within Each Initiative (Faceted Bar) ------

cat("\n=== Creating Visualization 15: Top Industries by Initiative ===\n")

p15_data <- top_industries_by_initiative %>%
  filter(geo_area == "Indiana") %>%  # Focus on Indiana for static visualization
  group_by(initiative) %>%
  mutate(
    rank = row_number(),
    naics_short = str_trunc(naics_title, 40, "right")
  ) %>%
  ungroup()

p15 <- p15_data %>%
  ggplot(aes(x = reorder(naics_short, jobs), 
             y = jobs, fill = initiative)) +
  geom_col(show.legend = FALSE, alpha = 0.9) +
  geom_text(aes(label = comma(jobs)), hjust = -0.1, size = 2.5) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = initiative_colors) +
  coord_flip() +
  facet_wrap(~initiative, scales = "free_y", ncol = 2) +
  labs(
    title = "Top 10 Industries by Employment Within Each Initiative",
    subtitle = paste("Indiana |", recent_year),
    x = NULL,
    y = "Total Jobs",
    caption = "Source: CICP Advanced Industries Dashboard"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 7)
  )

ggsave(file.path(viz_dir, "15_top_industries_by_initiative.png"),
       p15, width = 16, height = 14, dpi = 300, bg = "white")

# Interactive version with dropdown - RECREATION APPROACH
initiative_list <- unique(p15_data$initiative)

# Create the initial plot for the first initiative
init_data_1 <- p15_data %>%
  filter(initiative == initiative_list[1]) %>%
  arrange(jobs)

p15_interactive <- plot_ly(
  data = init_data_1,
  x = ~jobs,
  y = ~reorder(naics_short, jobs),
  type = "bar",
  orientation = "h",
  marker = list(color = initiative_colors[initiative_list[1]]),
  text = ~comma(jobs),
  textposition = "outside",
  textfont = list(size = 10),
  hoverinfo = "skip",  # CHANGED: removed hovertemplate
  showlegend = FALSE
)

# Create update buttons that will restyle the plot with new data
updatemenus <- list(
  list(
    active = 0,
    type = "dropdown",
    x = 0.15,
    y = 1.15,
    buttons = lapply(seq_along(initiative_list), function(i) {
      init <- initiative_list[i]
      init_data <- p15_data %>%
        filter(initiative == init) %>%
        arrange(jobs)
      
      list(
        method = "restyle",
        args = list(
          list(
            x = list(init_data$jobs),
            y = list(init_data$naics_short),
            text = list(comma(init_data$jobs)),
            marker = list(color = initiative_colors[init]),
            hoverinfo = "skip"  # CHANGED: removed hovertemplate
          )
        ),
        label = init
      )
    })
  )
)

# Add a second button list for title updates
title_updatemenus <- list(
  list(
    active = 0,
    type = "dropdown",
    x = 0.15,
    y = 1.15,
    buttons = lapply(seq_along(initiative_list), function(i) {
      list(
        method = "relayout",
        args = list(
          list(
            title = list(
              text = paste0("<b>Top 10 Industries by Employment - ", 
                           initiative_list[i], "</b><br><sup>Indiana | ", 
                           recent_year, "</sup>")
            )
          )
        ),
        label = initiative_list[i]
      )
    })
  )
)

p15_interactive <- p15_interactive %>%
  layout(
    title = list(
      text = paste0("<b>Top 10 Industries by Employment - ", 
                   initiative_list[1], "</b><br><sup>Indiana | ", 
                   recent_year, "</sup>"),
      font = list(size = 16)
    ),
    xaxis = list(
      title = "Total Jobs", 
      showgrid = TRUE, 
      gridcolor = "lightgray"
    ),
    yaxis = list(
      title = "",
      showgrid = FALSE
    ),
    updatemenus = updatemenus,
    margin = list(l = 300, r = 100, t = 100, b = 80),
    plot_bgcolor = "white",
    paper_bgcolor = "white"
  )

htmlwidgets::saveWidget(
  p15_interactive,
  file.path(viz_dir, "15_top_industries_by_initiative.html"),
  selfcontained = TRUE
)

# VISUALIZATION 16: Industry Concentration (Lorenz Curve) --------------------

cat("\n=== Creating Visualization 16: Industry Concentration ===\n")

# Calculate Lorenz curve data for each initiative
lorenz_data <- jobs_data %>%
  filter(display_level >= 2,
         year == recent_year,
         geo_area == "Indiana",
         initiative != "Total Employment") %>%
  group_by(initiative) %>%
  arrange(jobs) %>%
  mutate(
    cumulative_industries = row_number() / n() * 100,
    cumulative_jobs = cumsum(jobs) / sum(jobs) * 100
  ) %>%
  ungroup()

p16 <- lorenz_data %>%
  ggplot(aes(x = cumulative_industries, y = cumulative_jobs, 
             color = initiative)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", 
              color = "gray50", linewidth = 0.8) +
  geom_line(linewidth = 1.2, alpha = 0.8) +
  scale_color_manual(values = initiative_colors) +
  scale_x_continuous(labels = label_percent(scale = 1)) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title = "Industry Concentration Within Initiatives (Lorenz Curve)",
    subtitle = paste("Indiana |", recent_year, 
                    "| Distance from diagonal = concentration level"),
    x = "Cumulative % of Industries",
    y = "Cumulative % of Jobs",
    color = "Initiative",
    caption = "Source: CICP Advanced Industries Dashboard\nDashed line represents perfect equality"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

ggsave(file.path(viz_dir, "16_industry_concentration_lorenz.png"),
       p16, width = 12, height = 8, dpi = 300, bg = "white")

# Interactive version
p16_interactive <- plot_ly()

for(init in initiative_list) {
  init_data <- lorenz_data %>% filter(initiative == init)
  
  p16_interactive <- p16_interactive %>%
    add_trace(
      data = init_data,
      x = ~cumulative_industries,
      y = ~cumulative_jobs,
      type = "scatter",
      mode = "lines",
      name = init,
      line = list(width = 3, color = initiative_colors[init]),
      hovertemplate = paste0(
        "<b>", init, "</b><br>",
        "Industries: %{x:.1f}%<br>",
        "Jobs: %{y:.1f}%<br>",
        "<extra></extra>"
      )
    )
}

p16_interactive <- p16_interactive %>%
  add_trace(
    x = c(0, 100),
    y = c(0, 100),
    type = "scatter",
    mode = "lines",
    name = "Perfect Equality",
    line = list(dash = "dash", color = "gray", width = 2),
    hoverinfo = "skip",
    showlegend = TRUE
  ) %>%
  layout(
    title = list(
      text = paste0("<b>Industry Concentration Within Initiatives (Lorenz Curve)</b><br><sup>Indiana | ", 
                   recent_year, " | Distance from diagonal = concentration level</sup>"),
      font = list(size = 16)
    ),
    xaxis = list(title = "Cumulative % of Industries"),
    yaxis = list(title = "Cumulative % of Jobs"),
    legend = list(x = 0.02, y = 0.98),
    margin = list(l = 80, r = 80, t = 100, b = 80)
  )

htmlwidgets::saveWidget(
  p16_interactive,
  file.path(viz_dir, "16_industry_concentration_lorenz.html"),
  selfcontained = TRUE
)

# VISUALIZATION 17: Growth Contributors Waterfall Chart ----------------------

cat("\n=== Creating Visualization 17: Growth Contributors Waterfall ===\n")

# Prepare waterfall data for each initiative and geography
# First, add the prior year to the waterfall_data
waterfall_data <- industry_growth_contrib %>%
  group_by(initiative, geo_area, statefips, countyfips, metrofips) %>%
  arrange(desc(yoy_change)) %>%
  slice_head(n = 10) %>%
  mutate(
    position = row_number(),
    color_cat = ifelse(yoy_change >= 0, "Positive", "Negative"),
    prior_year = year - 1  # Add prior year
  ) %>%
  arrange(position) %>%
  mutate(
    end = cumsum(yoy_change),
    start = lag(end, default = 0),
    naics_short = str_trunc(naics_title, 30, "right")
  ) %>%
  ungroup()

prior_year <- recent_year - 1

# Create static version for one initiative AND one geography (Indiana)
init_example <- initiative_list[1]
geo_example <- "Indiana"

p17_data <- waterfall_data %>% 
  filter(initiative == init_example, geo_area == geo_example)

p17 <- p17_data %>%
  ggplot() +
  geom_rect(aes(xmin = position - 0.4, xmax = position + 0.4,
                ymin = start, ymax = end, fill = color_cat),
            alpha = 0.8) +
  geom_text(aes(x = position, y = end, 
                label = comma(round(yoy_change))),
            vjust = ifelse(p17_data$yoy_change >= 0, -0.5, 1.5),
            size = 3) +
  scale_x_continuous(breaks = 1:10, 
                    labels = p17_data$naics_short) +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("Positive" = "#2E7D32", "Negative" = "#C62828"),
                   name = "Change Type") +
  coord_flip() +
  labs(
    title = paste("Top Growth Contributors -", init_example),
    subtitle = paste("Year-over-Year Job Change |", geo_example, "|", recent_year - 1, "to", recent_year),
    x = NULL,
    y = "Cumulative Job Change",
    caption = "Source: CICP Advanced Industries Dashboard"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "bottom"
  )

ggsave(file.path(viz_dir, "17_growth_contributors_waterfall.png"),
       p17, width = 12, height = 8, dpi = 300, bg = "white")

# Interactive version with initiative AND geography dropdowns
initiative_list_viz17 <- unique(waterfall_data$initiative)
geography_list_viz17 <- unique(waterfall_data$geo_area)

# Then in the interactive version, add hover and fix subtitle:
p17_interactive <- plot_ly()

# Track combinations and their trace counts
combo_info <- list()
trace_counter <- 0
annotation_list <- list()

for(init in initiative_list_viz17) {
  for(geo in geography_list_viz17) {
    
    combo_data <- waterfall_data %>% 
      filter(initiative == init, geo_area == geo)
    
    if(nrow(combo_data) > 0) {
      # Store combo info
      combo_info[[length(combo_info) + 1]] <- list(
        initiative = init,
        geography = geo,
        start_trace = trace_counter + 1,
        n_traces = nrow(combo_data),
        data = combo_data
      )
      
      # Add the bars
      for(i in 1:nrow(combo_data)) {
        trace_counter <- trace_counter + 1
        row <- combo_data[i,]
        
        p17_interactive <- p17_interactive %>%
          add_trace(
            x = c(row$start, row$end, row$end, row$start),
            y = c(row$position - 0.4, row$position - 0.4, 
                  row$position + 0.4, row$position + 0.4),
            type = "scatter",
            mode = "none",
            fill = "toself",
            fillcolor = ifelse(row$color_cat == "Positive", 
                              "rgba(46, 125, 50, 0.8)", 
                              "rgba(198, 40, 40, 0.8)"),
            visible = if(init == initiative_list_viz17[1] && 
                        geo == geography_list_viz17[1]) TRUE else FALSE,
            showlegend = FALSE,
            hoverinfo = "skip"
          )
        
        # Store annotation
        annotation_list[[trace_counter]] <- list(
          x = row$end,
          y = row$position,
          text = paste0(ifelse(row$yoy_change >= 0, "+", ""), 
                       comma(round(row$yoy_change))),
          xanchor = "left",
          xshift = 5,
          showarrow = FALSE,
          font = list(size = 10),
          visible = if(init == initiative_list_viz17[1] && 
                      geo == geography_list_viz17[1]) TRUE else FALSE
        )
      }
    }
  }
}

# Prepare shorter labels for y-axis (60 characters)
waterfall_data <- waterfall_data %>%
  mutate(naics_shorter = str_trunc(naics_title, 60, "right"))

updatemenus <- list(
  # Initiative dropdown
  list(
    active = 0,
    type = "dropdown",
    x = 0.15,
    y = 1.15,
    xanchor = "left",
    yanchor = "top",
    buttons = lapply(seq_along(initiative_list_viz17), function(i) {
      init <- initiative_list_viz17[i]
      geo <- geography_list_viz17[1]  # First geography
      
      # Find the combo
      combo_idx <- which(sapply(combo_info, function(c) 
        c$initiative == init && c$geography == geo))
      
      if(length(combo_idx) > 0) {
        combo <- combo_info[[combo_idx[1]]]
        
        # Create visibility vector
        visible_vec <- rep(FALSE, trace_counter)
        visible_vec[combo$start_trace:(combo$start_trace + combo$n_traces - 1)] <- TRUE
        
        # Update annotations
        visible_annotations <- lapply(1:trace_counter, function(idx) {
          ann <- annotation_list[[idx]]
          ann$visible <- visible_vec[idx]
          return(ann)
        })
        
        list(
          method = "update",
          args = list(
            list(visible = visible_vec),
            list(
              title = list(
                text = paste0("<b>Top Growth Contributors - ", 
                             init, " - ", geo, "</b><br><sup>", 
                             prior_year, " to ", recent_year, " Job Change</sup>")
              ),
              annotations = visible_annotations,
              yaxis = list(
                title = "",
                tickmode = "array",
                tickvals = 1:nrow(combo$data),
                ticktext = combo$data$naics_shorter,
                tickfont = list(size = 9)
              )
            )
          ),
          label = init
        )
      }
    })
  ),
  # Geography dropdown
  list(
    active = 0,
    type = "dropdown",
    x = 0.4,
    y = 1.15,
    xanchor = "left",
    yanchor = "top",
    buttons = lapply(seq_along(geography_list_viz17), function(j) {
      init <- initiative_list_viz17[1]  # First initiative
      geo <- geography_list_viz17[j]
      
      # Find the combo
      combo_idx <- which(sapply(combo_info, function(c) 
        c$initiative == init && c$geography == geo))
      
      if(length(combo_idx) > 0) {
        combo <- combo_info[[combo_idx[1]]]
        
        # Create visibility vector
        visible_vec <- rep(FALSE, trace_counter)
        visible_vec[combo$start_trace:(combo$start_trace + combo$n_traces - 1)] <- TRUE
        
        # Update annotations
        visible_annotations <- lapply(1:trace_counter, function(idx) {
          ann <- annotation_list[[idx]]
          ann$visible <- visible_vec[idx]
          return(ann)
        })
        
        list(
          method = "update",
          args = list(
            list(visible = visible_vec),
            list(
              title = list(
                text = paste0("<b>Top Growth Contributors - ", 
                             init, " - ", geo, "</b><br><sup>", 
                             prior_year, " to ", recent_year, " Job Change</sup>")
              ),
              annotations = visible_annotations,
              yaxis = list(
                title = "",
                tickmode = "array",
                tickvals = 1:nrow(combo$data),
                ticktext = combo$data$naics_shorter,
                tickfont = list(size = 9)
              )
            )
          ),
          label = geo
        )
      }
    })
  )
)

initial_combo <- combo_info[[1]]
initial_annotations <- lapply(1:trace_counter, function(idx) {
  ann <- annotation_list[[idx]]
  ann$visible <- (idx >= initial_combo$start_trace && 
                 idx < initial_combo$start_trace + initial_combo$n_traces)
  return(ann)
})

p17_interactive <- p17_interactive %>%
  layout(
    title = list(
      text = paste0("<b>Top Growth Contributors - ", initial_combo$initiative,
                   " - ", initial_combo$geography, 
                   "</b><br><sup>", prior_year, " to ", recent_year, 
                   " Job Change</sup>"),
      font = list(size = 16)
    ),
    xaxis = list(title = "Cumulative Job Change"),
    yaxis = list(
      title = "",
      tickmode = "array",
      tickvals = 1:nrow(initial_combo$data),
      ticktext = initial_combo$data$naics_shorter,
      tickfont = list(size = 9)
    ),
    annotations = initial_annotations,
    updatemenus = updatemenus,
    margin = list(l = 350, r = 80, t = 100, b = 80)
  )

htmlwidgets::saveWidget(
  p17_interactive,
  file.path(viz_dir, "17_growth_contributors_waterfall.html"),
  selfcontained = TRUE
)

cat("Visualization 17 created (static + interactive with initiative and geography dropdowns)\n")


# VISUALIZATION 18: Wage Distribution Box Plots ------------------------------

cat("\n=== Creating Visualization 18: Wage Distribution by Initiative ===\n")

p18_data <- wage_data %>%
  filter(display_level >= 2,
         year == recent_year,
         geo_area == "Indiana",
         initiative != "Total Employment",
         naics_code != "000000",
         !is.na(wages)) %>%
  distinct(statefips, countyfips, metrofips, geo_area, initiative, 
           naics_code, naics_title, year, .keep_all = TRUE) %>%
  left_join(
    jobs_data %>%
      filter(display_level >= 2, year == recent_year, geo_area == "Indiana",
             naics_code != "000000") %>%
      distinct(statefips, countyfips, metrofips, geo_area, initiative, 
               naics_code, naics_title, .keep_all = TRUE) %>%
      select(initiative, naics_title, naics_code, jobs),
    by = c("initiative", "naics_title", "naics_code")
  ) %>%
  filter(jobs >= 50)  # Minimum threshold for meaningful wages

p18 <- p18_data %>%
  ggplot(aes(x = initiative, y = wages, fill = initiative)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.5, show.legend = FALSE) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, 
               fill = "white", color = "black") +
  scale_y_continuous(labels = dollar, expand = expansion(mult = c(0.05, 0.1))) +
  scale_fill_manual(values = initiative_colors) +
  labs(
    title = "Wage Distribution by Initiative",
    subtitle = paste("Industries with 50+ jobs | Indiana |", recent_year, 
                    "| Diamond = mean, line = median"),
    x = NULL,
    y = "Average Annual Wage",
    caption = "Source: CICP Advanced Industries Dashboard"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    panel.grid.major.x = element_blank()
  )

ggsave(file.path(viz_dir, "18_wage_distribution_boxplots.png"),
       p18, width = 12, height = 8, dpi = 300, bg = "white")

# Interactive version with geography dropdown
geography_list_viz18 <- c("Indiana", unique(wage_data$geo_area[wage_data$geo_type == "Metro"]))

# Get ALL possible initiatives across all geographies
all_initiatives <- unique(p18_data$initiative)

p18_interactive <- plot_ly()

# Track which traces belong to which geography
trace_info <- list()
trace_counter <- 0

# Add traces for each geography
for(geo in geography_list_viz18) {
  geo_data <- wage_data %>%
    filter(display_level >= 2,
           year == recent_year,
           geo_area == geo,
           initiative != "Total Employment",
           naics_code != "000000",
           !is.na(wages)) %>%
    distinct(statefips, countyfips, metrofips, geo_area, initiative, 
             naics_code, naics_title, year, .keep_all = TRUE) %>%
    left_join(
      jobs_data %>%
        filter(display_level >= 2, year == recent_year, geo_area == geo,
               naics_code != "000000") %>%
        distinct(statefips, countyfips, metrofips, geo_area, initiative, 
                 naics_code, naics_title, .keep_all = TRUE) %>%
        select(initiative, naics_title, naics_code, jobs),
      by = c("initiative", "naics_title", "naics_code")
    ) %>%
    filter(jobs >= 50) %>%
    mutate(wages_rounded = round(wages))
  
  if(nrow(geo_data) > 0) {
    # Loop through ALL initiatives (not just those present in this geo)
    for(init in all_initiatives) {
      init_data <- geo_data %>% filter(initiative == init)
      
      # Only add trace if this initiative exists in this geography
      if(nrow(init_data) > 0) {
        trace_counter <- trace_counter + 1
        
        # Store trace info
        trace_info[[trace_counter]] <- list(
          geography = geo,
          initiative = init,
          trace_index = trace_counter
        )
        
        p18_interactive <- p18_interactive %>%
          add_trace(
            data = init_data,
            x = ~initiative,
            y = ~wages_rounded,
            type = "box",
            name = init,
            marker = list(color = "#1565C0", opacity = 0.5),  # CHANGED: added opacity
            fillcolor = "rgba(21, 101, 192, 0.5)",  # CHANGED: rgba with 0.5 alpha
            line = list(color = "#1565C0"),  # ADDED: set line color too
            visible = if(geo == geography_list_viz18[1]) TRUE else FALSE,
            legendgroup = init,
            showlegend = FALSE,
            boxmean = "sd",
            hovertemplate = paste0(
              "%{y:$,d}",
              "<extra></extra>"
            )
          )
      }
    }
  }
}

# Create geography dropdown with proper trace visibility
updatemenus <- list(
  list(
    active = 0,
    type = "dropdown",
    x = 0.15,
    y = 1.15,
    xanchor = "left",
    yanchor = "top",
    buttons = lapply(seq_along(geography_list_viz18), function(i) {
      geo <- geography_list_viz18[i]
      
      # Create visibility vector based on trace_info
      visible_vec <- sapply(1:trace_counter, function(idx) {
        trace_info[[idx]]$geography == geo
      })
      
      list(
        method = "update",
        args = list(
          list(visible = visible_vec),
          list(title = list(
            text = paste0("<b>Wage Distribution by Initiative - ", 
                         geo, 
                         "</b><br><sup>Industries with 50+ jobs | ", 
                         recent_year, "</sup>")
          ))
        ),
        label = geo
      )
    })
  )
)

p18_interactive <- p18_interactive %>%
  layout(
    title = list(
      text = paste0("<b>Wage Distribution by Initiative - ", 
                   geography_list_viz18[1], 
                   "</b><br><sup>Industries with 50+ jobs | ", 
                   recent_year, "</sup>"),
      font = list(size = 16)
    ),
    xaxis = list(title = ""),
    yaxis = list(title = "Average Annual Wage", tickformat = "$,"),
    showlegend = FALSE,
    updatemenus = updatemenus,
    margin = list(l = 80, r = 80, t = 100, b = 120)
  )

htmlwidgets::saveWidget(
  p18_interactive,
  file.path(viz_dir, "18_wage_distribution_boxplots.html"),
  selfcontained = TRUE
)

cat("Visualization 18 created (static + interactive with geography dropdown)\n")

# VISUALIZATION 19: Industry Size vs Growth Scatter --------------------------

cat("\n=== Creating Visualization 19: Industry Size vs Growth ===\n")

p19_data <- jobs_growth %>%
  filter(display_level >= 2,
         year == recent_year,
         geo_area == "Indiana",
         initiative != "Total Employment",
         naics_code != "000000",
         !is.na(cagr_2yr),
         jobs >= 50) %>%
  distinct(statefips, countyfips, metrofips, geo_area, initiative, 
           naics_code, naics_title, year, .keep_all = TRUE)

# Count total industries before filtering
total_industries <- nrow(p19_data)

# Filter to reasonable growth range (e.g., between -50% and +50%)
growth_limit <- 50
p19_data_filtered <- p19_data %>%
  filter(cagr_2yr >= -growth_limit, cagr_2yr <= growth_limit)

# Count excluded
n_excluded <- total_industries - nrow(p19_data_filtered)

p19 <- p19_data_filtered %>%
  ggplot(aes(x = jobs, y = cagr_2yr, color = initiative, size = jobs)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(alpha = 0.6) +
  scale_x_log10(labels = comma) +
  scale_y_continuous(labels = label_percent(scale = 1), 
                    limits = c(-growth_limit, growth_limit)) +
  scale_color_manual(values = initiative_colors) +
  scale_size_continuous(range = c(2, 10), guide = "none") +
  labs(
    title = "Industry Size vs. Growth Rate",
    subtitle = paste("Indiana | 2-Year CAGR (", recent_year - 2, "-", recent_year, ") | Industries with 50+ jobs"),
    x = "Total Jobs (log scale)",
    y = "2-Year CAGR (%)",
    color = "Initiative",
    caption = paste0("Source: CICP Advanced Industries Dashboard\n",
                    "Note: ", n_excluded, " industries with growth rates outside ±", 
                    growth_limit, "% excluded for chart clarity")
  ) +
  theme_minimal() +
  theme(legend.position = "right")

ggsave(file.path(viz_dir, "19_size_vs_growth_scatter.png"),
       p19, width = 12, height = 8, dpi = 300, bg = "white")

# Interactive version with geography dropdown
geography_list_viz19 <- c("Indiana", unique(jobs_growth$geo_area[jobs_growth$geo_type == "Metro"]))

p19_interactive <- plot_ly()

# Track which traces belong to which geography
trace_info <- list()
trace_counter <- 0

# Add traces for each geography
for(geo in geography_list_viz19) {
  geo_data <- jobs_growth %>%
    filter(display_level >= 2,
           year == recent_year,
           geo_area == geo,
           initiative != "Total Employment",
           naics_code != "000000",
           !is.na(cagr_2yr),
           jobs >= 50) %>%
    distinct(statefips, countyfips, metrofips, geo_area, initiative, 
             naics_code, naics_title, year, .keep_all = TRUE)
  
  # Count total industries before filtering
  total_industries <- nrow(geo_data)
  
  # Filter to reasonable growth range
  growth_limit <- 50
  geo_data_filtered <- geo_data %>%
    filter(cagr_2yr >= -growth_limit, cagr_2yr <= growth_limit)
  
  # Count excluded
  n_excluded <- total_industries - nrow(geo_data_filtered)
  
  if(nrow(geo_data_filtered) > 0) {
    # Add traces for each initiative within this geography
    for(init in unique(geo_data_filtered$initiative)) {
      trace_counter <- trace_counter + 1
      
      init_data <- geo_data_filtered %>% filter(initiative == init)
      
      # Store trace info
      trace_info[[trace_counter]] <- list(
        geography = geo,
        initiative = init,
        n_excluded = n_excluded,
        trace_index = trace_counter
      )
      
      p19_interactive <- p19_interactive %>%
        add_trace(
          data = init_data,
          x = ~jobs,
          y = ~cagr_2yr,
          type = "scatter",
          mode = "markers",
          name = init,
          marker = list(
            color = initiative_colors[init],
            size = 8,
            opacity = 0.6
          ),
          text = ~naics_title,
          visible = if(geo == geography_list_viz19[1]) TRUE else FALSE,
          legendgroup = init,
          showlegend = if(geo == geography_list_viz19[1]) TRUE else FALSE,
          hovertemplate = paste0(
            "<b>%{text}</b><br>",
            "Initiative: ", init, "<br>",
            "Jobs: %{x:,d}<br>",
            "2-Year CAGR: %{y:.1f}%<br>",
            "<extra></extra>"
          )
        )
    }
  }
}

# Create geography dropdown
updatemenus <- list(
  list(
    active = 0,
    type = "dropdown",
    direction = "down",
    x = 0.01,
    y = 0.99,
    xanchor = "left",
    yanchor = "top",
    buttons = lapply(seq_along(geography_list_viz19), function(i) {
      geo <- geography_list_viz19[i]
      
      # Create visibility vector based on trace_info
      visible_vec <- sapply(1:trace_counter, function(idx) {
        trace_info[[idx]]$geography == geo
      })
      
      # Get n_excluded for this geography
      geo_n_excluded <- if(any(visible_vec)) {
        trace_info[[which(visible_vec)[1]]]$n_excluded
      } else {
        0
      }
      
      list(
        method = "update",
        args = list(
          list(visible = visible_vec),
          list(
            title = list(
              text = paste0("<b>Industry Size vs. Growth Rate - ", geo, 
                           "</b><br><sup>2-Year CAGR (", 
                           recent_year - 2, "-", recent_year, 
                           ") | Industries with 50+ jobs<br>",
                           geo_n_excluded, " industries with growth rates outside ±50% excluded for chart clarity</sup>")
            )
          )
        ),
        label = geo
      )
    })
  )
)

p19_interactive <- p19_interactive %>%
  layout(
    title = list(
      text = paste0("<b>Industry Size vs. Growth Rate - ", geography_list_viz19[1], 
                   "</b><br><sup>2-Year CAGR (", 
                   recent_year - 2, "-", recent_year, 
                   ") | Industries with 50+ jobs<br>",
                   trace_info[[1]]$n_excluded, " industries with growth rates outside ±50% excluded for chart clarity</sup>"),
      font = list(size = 16)
    ),
    xaxis = list(
      title = "Total Jobs (log scale)",
      type = "log",
      tickmode = "array",
      tickvals = c(10, 100, 1000, 10000, 100000),
      ticktext = c("10", "100", "1,000", "10,000", "100,000")
    ),
    yaxis = list(
      title = "2-Year CAGR (%)", 
      zeroline = TRUE, 
      range = c(-50, 50)
    ),
    hovermode = "closest",
    updatemenus = updatemenus,
    margin = list(l = 80, r = 150, t = 150, b = 80),
    shapes = list(
      list(
        type = "line",
        x0 = 0,
        x1 = 1,
        xref = "paper",
        y0 = 0,
        y1 = 0,
        line = list(color = "gray", width = 1, dash = "dash")
      )
    )
  )

htmlwidgets::saveWidget(
  p19_interactive,
  file.path(viz_dir, "19_size_vs_growth_scatter.html"),
  selfcontained = TRUE
)

cat("Visualization 19 created (interactive with geography dropdown)\n")

# VISUALIZATION 20: Top Specialized Industries by Initiative ------------------

cat("\n=== Creating Visualization 20: Top Specialized Industries by Initiative ===\n")

# Calculate traditional location quotients for all geographies
p20_data_all <- jobs_data %>%
  filter(display_level >= 2,
         year == recent_year,
         initiative != "Total Employment",
         naics_code != "000000",
         !is.na(jobs)) %>%
  distinct(statefips, countyfips, metrofips, geo_area, initiative, 
           naics_code, naics_title, year, .keep_all = TRUE) %>%
  # Get total employment for each geography
  left_join(
    jobs_data %>%
      filter(display_level == 0, year == recent_year,
             initiative == "Total Employment") %>%
      select(statefips, countyfips, metrofips, geo_area, total_employment = jobs),
    by = c("statefips", "countyfips", "metrofips", "geo_area")
  ) %>%
  # Get US industry employment
  left_join(
    jobs_data %>%
      filter(display_level >= 2, year == recent_year,
             geo_area == "United States",
             naics_code != "000000") %>%
      distinct(initiative, naics_code, naics_title, .keep_all = TRUE) %>%
      select(initiative, naics_code, naics_title, us_industry_jobs = jobs),
    by = c("initiative", "naics_code", "naics_title")
  ) %>%
  # Get US total employment
  left_join(
    jobs_data %>%
      filter(display_level == 0, year == recent_year,
             geo_area == "United States", initiative == "Total Employment") %>%
      select(us_total = jobs),
    by = character()
  ) %>%
  mutate(
    local_share = (jobs / total_employment) * 100,
    us_share = (us_industry_jobs / us_total) * 100,
    location_quotient = local_share / us_share,
    short_init = case_when(
      initiative == "Advanced & Traded Industries" ~ "A&TI",
      initiative == "AgriNovus" ~ "Ag",
      initiative == "BioCrossroads" ~ "BioX",
      initiative == "Conexus" ~ "CX",
      initiative == "TechPoint" ~ "Tech"
    )
  ) %>%
  filter(!is.na(location_quotient), 
         !is.infinite(location_quotient),
         jobs >= 100,  # Minimum size
         geo_area != "United States") %>%
  select(statefips, countyfips, metrofips, geo_area, year, initiative, short_init,
         naics_title, naics_code, jobs, location_quotient)

# Get top 3 most specialized industries per initiative for each geography
p20_data_top <- p20_data_all %>%
  group_by(geo_area, initiative, short_init) %>%
  arrange(desc(location_quotient)) %>%
  slice_head(n = 3) %>%
  ungroup() %>%
  mutate(
    naics_short = str_trunc(naics_title, 45, "right")
  )

# Create static version for Indiana
p20_data_indiana <- p20_data_top %>%
  filter(geo_area == "Indiana") %>%
  mutate(
    init_industry = paste0(short_init, " - ", naics_short)
  )

p20 <- p20_data_indiana %>%
  ggplot(aes(x = initiative, y = reorder(init_industry, location_quotient), 
             fill = location_quotient)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.1f", location_quotient)), size = 3) +
  scale_fill_gradient(low = "white", high = "#1565C0", 
                     name = "Location\nQuotient") +
  labs(
    title = "Most Specialized Industries by Initiative",
    subtitle = paste("Top 3 per initiative | Indiana |", recent_year, 
                    "| LQ measures concentration vs. US average"),
    x = NULL,
    y = NULL,
    caption = "Source: CICP Advanced Industries Dashboard\nLocation Quotient = (Local % in Industry) / (US % in Industry)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )

ggsave(file.path(viz_dir, "20_specialized_industries_heatmap.png"),
       p20, width = 12, height = 10, dpi = 300, bg = "white")

# Interactive version with geography dropdown
geography_list_viz20 <- c("Indiana", unique(p20_data_top$geo_area[p20_data_top$geo_area != "Indiana"]))

# Get all possible initiatives
all_initiatives_viz20 <- unique(p20_data_top$initiative)

# Create the heatmap data structure for plotly
p20_interactive <- plot_ly()

# Store y-axis info for each geography
yaxis_info <- list()

for(geo in geography_list_viz20) {
  geo_data <- p20_data_top %>%
    filter(geo_area == geo) %>%
    arrange(short_init, desc(location_quotient))
  
  if(nrow(geo_data) > 0) {
    # Create y-axis labels (initiative + industry name)
    geo_data <- geo_data %>%
      mutate(
        y_label = paste0(short_init, ": ", naics_short),
        y_order = row_number()
      )
    
    # Store y-axis info
    yaxis_info[[geo]] <- list(
      tickvals = geo_data$y_order,
      ticktext = geo_data$y_label
    )
    
    # Pre-build hover text in R
    geo_data <- geo_data %>%
      mutate(
        hover_text = paste0(
          "<b>", naics_title, "</b><br>",
          initiative, "<br>",
          "Jobs: ", format(jobs, big.mark = ",", digits = 1), "<br>",
          "Location Quotient: ", sprintf("%.2f", location_quotient)
        )
      )
    
    p20_interactive <- p20_interactive %>%
      add_trace(
        data = geo_data,
        x = ~initiative,  # CHANGED BACK: full initiative names
        y = ~y_order,
        z = ~location_quotient,
        type = "heatmap",
        colors = colorRamp(c("white", "#1565C0")),
        text = ~sprintf("%.1f", location_quotient),
        texttemplate = "%{text}",
        textfont = list(size = 10),
        visible = if(geo == geography_list_viz20[1]) TRUE else FALSE,
        showscale = if(geo == geography_list_viz20[1]) TRUE else FALSE,
        hovertext = ~hover_text,  # CHANGED: use pre-built hover text
        hoverinfo = "text",  # CHANGED: simple text mode
        colorbar = list(title = "Location<br>Quotient")
      )
  }
}

# Create geography dropdown
updatemenus_viz20 <- list(
  list(
    active = 0,
    type = "dropdown",
    x = 0.15,
    y = 1.25,  # CHANGED: moved down to avoid title overlap
    xanchor = "left",
    yanchor = "top",
    buttons = lapply(seq_along(geography_list_viz20), function(i) {
      geo <- geography_list_viz20[i]
      
      # Create visibility vector
      visible_vec <- rep(FALSE, length(geography_list_viz20))
      visible_vec[i] <- TRUE
      
      list(
        method = "update",
        args = list(
          list(visible = visible_vec),
          list(
            title = list(
              text = paste0("<b>Most Specialized Industries by Initiative - ", geo,
                           "</b><br><sup>Top 3 per initiative | ", recent_year, 
                           " | LQ measures concentration vs. US average</sup>")
            ),
            yaxis = list(
              tickmode = "array",
              tickvals = yaxis_info[[geo]]$tickvals,
              ticktext = yaxis_info[[geo]]$ticktext,
              tickfont = list(size = 9)
            )
          )
        ),
        label = geo
      )
    })
  )
)

# Get initial geography y-axis info
initial_geo <- geography_list_viz20[1]

p20_interactive <- p20_interactive %>%
  layout(
    title = list(
      text = paste0("<b>Most Specialized Industries by Initiative - ", 
                   initial_geo, 
                   "</b><br><sup>Top 3 per initiative | ", recent_year, 
                   " | LQ measures concentration vs. US average</sup>"),
      font = list(size = 16)
    ),
    xaxis = list(title = "", tickangle = 45),
    yaxis = list(
      title = "",
      tickmode = "array",
      tickvals = yaxis_info[[initial_geo]]$tickvals,
      ticktext = yaxis_info[[initial_geo]]$ticktext,
      tickfont = list(size = 9)
    ),
    margin = list(l = 350, r = 100, t = 150, b = 100),  # CHANGED: increased top margin
    updatemenus = updatemenus_viz20
  )

htmlwidgets::saveWidget(
  p20_interactive,
  file.path(viz_dir, "20_specialized_industries_heatmap.html"),
  selfcontained = TRUE
)

cat("Visualization 20 created (static + interactive with geography dropdown)\n")

cat("Visualization 20 created (static + interactive with geography dropdown)\n")

# SUMMARY ---------------------------------------------------------------------

cat("\n", rep("=", 70), "\n", sep = "")
cat("INDUSTRY DEEP DIVE VISUALIZATIONS CREATED\n")
cat(rep("=", 70), "\n\n", sep = "")
cat("Visualizations saved to:", viz_dir, "\n\n")
cat(" 15. Top Industries by Initiative (static + interactive with dropdown)\n")
cat(" 16. Industry Concentration Lorenz Curves (static + interactive)\n")
cat(" 17. Growth Contributors Waterfall (static + interactive with 2 dropdowns)\n")
cat(" 18. Wage Distribution Box Plots (static + interactive)\n")
cat(" 19. Industry Size vs Growth Scatter (static + interactive)\n")
cat(" 20. Top Specialized Industries Heatmap (static + interactive)\n")
cat("\n", rep("=", 70), "\n", sep = "")