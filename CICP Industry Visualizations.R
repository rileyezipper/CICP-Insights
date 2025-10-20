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
         !is.na(wages)) %>%
  left_join(
    jobs_data %>%
      filter(display_level >= 2, year == recent_year, geo_area == "Indiana") %>%
      select(initiative, naics_title, jobs),
    by = c("initiative", "naics_title")
  )

p18 <- p18_data %>%
  ggplot(aes(x = initiative, y = wages, fill = initiative)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.5, show.legend = FALSE) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, 
               fill = "white", color = "black") +
  scale_y_continuous(labels = dollar) +