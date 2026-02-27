# ============================================================================
# CICP Talent Demand - Occupation Visualizations
# ============================================================================
# Purpose: Create detailed visualizations for occupations within initiatives
# Note: Run CICP_Occupation_Prep_and_EDA_Script.R first
# ============================================================================

library(tidyverse)
library(scales)
library(plotly)
library(patchwork)
library(htmlwidgets)
library(janitor)

# Load processed data
output_dir <- "outputs_occupation_20251104"  # Update with your output folder name
load(file.path(output_dir, "processed_data_occupations.RData"))

# Create visualizations directory
viz_dir <- file.path(output_dir, "visualizations")
if (!dir.exists(viz_dir)) {
  dir.create(viz_dir, recursive = TRUE)
}

cat("\n=== Starting Occupation Visualizations ===\n")

# VISUALIZATION 1: Top Occupations Within Each Initiative (Faceted Bar) ------

cat("\n=== Creating Visualization 1: Top Occupations by Initiative ===\n")

p1_data <- emp_data %>%
  filter(year == recent_year, 
         geo_area == "Indiana",
         cicp_initiative != "Total Employment") %>%
  group_by(cicp_initiative) %>%
  arrange(desc(jobs)) %>%
  slice_head(n = 10) %>%
  mutate(
    rank = row_number(),
    occ_short = str_trunc(occupation, 45, "right")
  ) %>%
  ungroup()

p1 <- p1_data %>%
  ggplot(aes(x = reorder(occ_short, jobs), 
             y = jobs, fill = cicp_initiative)) +
  geom_col(show.legend = FALSE, alpha = 0.9) +
  geom_text(aes(label = comma(jobs)), hjust = -0.1, size = 2.5) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = initiative_colors) +
  coord_flip() +
  facet_wrap(~cicp_initiative, scales = "free_y", ncol = 2) +
  labs(
    title = "Top 10 Occupations by Employment Within Each Initiative",
    subtitle = paste("Indiana |", recent_year),
    x = NULL,
    y = "Total Jobs",
    caption = "Source: CICP Talent Demand Data"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 7)
  )

ggsave(file.path(viz_dir, "01_top_occupations_by_initiative.png"),
       p1, width = 16, height = 14, dpi = 300, bg = "white")

# Interactive version with dropdown
initiative_list <- unique(p1_data$cicp_initiative)

init_data_1 <- p1_data %>%
  filter(cicp_initiative == initiative_list[1]) %>%
  arrange(jobs)

p1_interactive <- plot_ly(
  data = init_data_1,
  x = ~jobs,
  y = ~reorder(occ_short, jobs),
  type = "bar",
  orientation = "h",
  marker = list(color = initiative_colors[initiative_list[1]]),
  text = ~comma(jobs),
  textposition = "outside",
  textfont = list(size = 10),
  hoverinfo = "skip",
  showlegend = FALSE
)

updatemenus <- list(
  list(
    active = 0,
    type = "dropdown",
    x = 0.15,
    y = 1.15,
    buttons = lapply(seq_along(initiative_list), function(i) {
      init <- initiative_list[i]
      init_data <- p1_data %>%
        filter(cicp_initiative == init) %>%
        arrange(jobs)
      
      list(
        method = "update",
        args = list(
          list(
            x = list(init_data$jobs),
            y = list(init_data$occ_short),
            text = list(comma(init_data$jobs)),
            marker = list(color = initiative_colors[init]),
            hoverinfo = "skip"
          ),
          list(
            title = list(
              text = paste0("<b>Top 10 Occupations - ", init, 
                           "</b><br><sup>Indiana | ", recent_year, "</sup>")
            )
          )
        ),
        label = init
      )
    })
  )
)

p1_interactive <- p1_interactive %>%
  layout(
    title = list(
      text = paste0("<b>Top 10 Occupations - ", initiative_list[1], 
                   "</b><br><sup>Indiana | ", recent_year, "</sup>"),
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
    margin = list(l = 350, r = 100, t = 100, b = 80),
    plot_bgcolor = "white",
    paper_bgcolor = "white"
  )

saveWidget(
  p1_interactive,
  file.path(viz_dir, "01_top_occupations_by_initiative.html"),
  selfcontained = TRUE
)

cat("Visualization 1 created (static + interactive)\n")

# VISUALIZATION 2: Occupation Concentration (Lorenz Curve) -------------------

cat("\n=== Creating Visualization 2: Occupation Concentration ===\n")

lorenz_data <- emp_data %>%
  filter(year == recent_year,
         geo_area == "Indiana",
         cicp_initiative != "Total Employment") %>%
  group_by(cicp_initiative) %>%
  arrange(jobs) %>%
  mutate(
    cumulative_occupations = row_number() / n() * 100,
    cumulative_jobs = cumsum(jobs) / sum(jobs) * 100
  ) %>%
  ungroup()

p2 <- lorenz_data %>%
  ggplot(aes(x = cumulative_occupations, y = cumulative_jobs, 
             color = cicp_initiative)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", 
              color = "gray50", linewidth = 0.8) +
  geom_line(linewidth = 1.2, alpha = 0.8) +
  scale_color_manual(values = initiative_colors) +
  scale_x_continuous(labels = label_percent(scale = 1)) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title = "Occupation Concentration Within Initiatives (Lorenz Curve)",
    subtitle = paste("Indiana |", recent_year, 
                    "| Distance from diagonal = concentration level"),
    x = "Cumulative % of Occupations",
    y = "Cumulative % of Jobs",
    color = "Initiative",
    caption = "Source: CICP Talent Demand Data\nDashed line represents perfect equality"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

ggsave(file.path(viz_dir, "02_occupation_concentration_lorenz.png"),
       p2, width = 12, height = 8, dpi = 300, bg = "white")

# Interactive version
p2_interactive <- plot_ly()

for(init in initiative_list) {
  init_data <- lorenz_data %>% filter(cicp_initiative == init)
  
  p2_interactive <- p2_interactive %>%
    add_trace(
      data = init_data,
      x = ~cumulative_occupations,
      y = ~cumulative_jobs,
      type = "scatter",
      mode = "lines",
      name = init,
      line = list(width = 3, color = initiative_colors[init]),
      hovertemplate = paste0(
        "<b>", init, "</b><br>",
        "Occupations: %{x:.1f}%<br>",
        "Jobs: %{y:.1f}%<br>",
        "<extra></extra>"
      )
    )
}

p2_interactive <- p2_interactive %>%
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
      text = paste0("<b>Occupation Concentration Within Initiatives</b><br><sup>Indiana | ", 
                   recent_year, "</sup>"),
      font = list(size = 16)
    ),
    xaxis = list(title = "Cumulative % of Occupations"),
    yaxis = list(title = "Cumulative % of Jobs"),
    legend = list(x = 0.02, y = 0.98),
    margin = list(l = 80, r = 80, t = 100, b = 80)
  )

saveWidget(
  p2_interactive,
  file.path(viz_dir, "02_occupation_concentration_lorenz.html"),
  selfcontained = TRUE
)

cat("Visualization 2 created (static + interactive)\n")

# VISUALIZATION 3: Occupation Growth Rates (Lollipop Chart) ------------------

cat("\n=== Creating Visualization 3: Occupation Growth Rates ===\n")

p3_data <- emp_growth %>%
  filter(year == recent_year,
         geo_area == "Indiana",
         cicp_initiative != "Total Employment",
         !is.na(cagr_2yr),
         jobs >= 100) %>%
  group_by(cicp_initiative) %>%
  arrange(desc(cagr_2yr)) %>%
  slice_head(n = 15) %>%
  ungroup() %>%
  mutate(
    occ_short = str_trunc(occupation, 40, "right"),
    growth_type = ifelse(cagr_2yr >= 0, "Positive", "Negative")
  )

p3 <- p3_data %>%
  ggplot(aes(x = reorder(occ_short, cagr_2yr), y = cagr_2yr, 
             color = growth_type)) +
  geom_segment(aes(xend = occ_short, y = 0, yend = cagr_2yr), 
               linewidth = 1, alpha = 0.8) +
  geom_point(size = 3, alpha = 0.9) +
  scale_color_manual(values = c("Positive" = "#2E7D32", "Negative" = "#D84315")) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  coord_flip() +
  facet_wrap(~cicp_initiative, scales = "free_y", ncol = 2) +
  labs(
    title = "Fastest Growing/Declining Occupations by Initiative",
    subtitle = paste("2-Year CAGR | Min 100 jobs | Indiana |", recent_year),
    x = NULL,
    y = "2-Year CAGR (%)",
    color = NULL,
    caption = "Source: CICP Talent Demand Data"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 7)
  )

ggsave(file.path(viz_dir, "03_occupation_growth_rates.png"),
       p3, width = 16, height = 14, dpi = 300, bg = "white")

# Interactive version - single trace approach so y-axis categories only
# reflect the currently selected initiative (not all initiatives at once)

# Helper: build lollipop segment shapes for one initiative's data
make_lollipop_shapes <- function(init_data) {
  lapply(seq_len(nrow(init_data)), function(j) {
    list(
      type = "line",
      x0 = 0, x1 = init_data$cagr_2yr[j],
      y0 = init_data$occ_short[j], y1 = init_data$occ_short[j],
      xref = "x", yref = "y",
      line = list(
        color = ifelse(init_data$cagr_2yr[j] >= 0, "#2E7D32", "#D84315"),
        width = 2
      )
    )
  })
}

first_init_p3 <- initiative_list[1]
init_data_first <- p3_data %>%
  filter(cicp_initiative == first_init_p3) %>%
  arrange(cagr_2yr)

p3_interactive <- plot_ly(
  data = init_data_first,
  x = ~cagr_2yr,
  y = ~occ_short,
  type = "scatter",
  mode = "markers",
  marker = list(
    size = 10,
    color = ifelse(init_data_first$cagr_2yr >= 0, "#2E7D32", "#D84315")
  ),
  hovertemplate = paste0(
    "<b>%{y}</b><br>",
    "2-Year CAGR: %{x:.1f}%<br>",
    "<extra></extra>"
  ),
  showlegend = FALSE
)

updatemenus <- list(
  list(
    active = 0,
    type = "dropdown",
    x = 0.15,
    y = 1.15,
    buttons = lapply(seq_along(initiative_list), function(i) {
      init <- initiative_list[i]
      init_data <- p3_data %>%
        filter(cicp_initiative == init) %>%
        arrange(cagr_2yr)

      list(
        method = "update",
        args = list(
          # Trace update: replace x/y/color — only one trace, no bleed
          list(
            x = list(init_data$cagr_2yr),
            y = list(init_data$occ_short),
            marker = list(
              size = 10,
              color = list(ifelse(init_data$cagr_2yr >= 0, "#2E7D32", "#D84315"))
            )
          ),
          # Layout update: title + segments + y-axis
          list(
            title = list(
              text = paste0("<b>Occupation Growth Rates - ", init,
                           "</b><br><sup>2-Year CAGR | Min 100 jobs | Indiana | ",
                           recent_year, "</sup>")
            ),
            shapes = make_lollipop_shapes(init_data),
            yaxis = list(
              categoryarray = init_data$occ_short,
              categoryorder = "array",
              title = "",
              showgrid = FALSE
            )
          )
        ),
        label = init
      )
    })
  )
)

p3_interactive <- p3_interactive %>%
  layout(
    title = list(
      text = paste0("<b>Occupation Growth Rates - ", initiative_list[1],
                   "</b><br><sup>2-Year CAGR | Min 100 jobs | Indiana | ",
                   recent_year, "</sup>"),
      font = list(size = 16)
    ),
    xaxis = list(
      title = "2-Year CAGR (%)",
      showgrid = TRUE,
      gridcolor = "lightgray"
    ),
    yaxis = list(
      title = "",
      showgrid = FALSE,
      categoryarray = init_data_first$occ_short,
      categoryorder = "array"
    ),
    shapes = make_lollipop_shapes(init_data_first),
    updatemenus = updatemenus,
    margin = list(l = 350, r = 100, t = 100, b = 80),
    plot_bgcolor = "white",
    paper_bgcolor = "white"
  )

saveWidget(
  p3_interactive,
  file.path(viz_dir, "03_occupation_growth_rates.html"),
  selfcontained = TRUE
)

cat("Visualization 3 created (static + interactive)\n")

# VISUALIZATION 4: Wage vs Employment Scatter ---------------------------------

cat("\n=== Creating Visualization 4: Wage vs Employment by Occupation ===\n")

p4_data <- emp_data %>%
  filter(year == recent_year,
         geo_area == "Indiana",
         cicp_initiative != "Total Employment") %>%
  left_join(
    wage_data %>%
      filter(year == recent_year, geo_area == "Indiana") %>%
      select(cicp_initiative, occ_code, median_annual_earnings),
    by = c("cicp_initiative", "occ_code")
  ) %>%
  filter(!is.na(median_annual_earnings), jobs >= 50) %>%
  mutate(occ_short = str_trunc(occupation, 30, "right"))

p4 <- p4_data %>%
  ggplot(aes(x = jobs, y = median_annual_earnings, 
             color = cicp_initiative, size = jobs)) +
  geom_point(alpha = 0.6) +
  scale_x_log10(labels = comma) +
  scale_y_continuous(labels = dollar) +
  scale_color_manual(values = initiative_colors) +
  scale_size_continuous(range = c(2, 10), guide = "none") +
  labs(
    title = "Wage vs Employment by Occupation",
    subtitle = paste("Bubble size = employment level | Min 50 jobs | Indiana |", 
                    recent_year),
    x = "Total Jobs (log scale)",
    y = "Median Annual Earnings",
    color = "Initiative",
    caption = "Source: CICP Talent Demand Data"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(file.path(viz_dir, "04_wage_vs_employment_scatter.png"),
       p4, width = 12, height = 10, dpi = 300, bg = "white")

# Interactive version
p4_interactive <- plot_ly()

for(init in initiative_list) {
  init_data <- p4_data %>% filter(cicp_initiative == init)
  
  if(nrow(init_data) > 0) {
    p4_interactive <- p4_interactive %>%
      add_trace(
        data = init_data,
        x = ~jobs,
        y = ~median_annual_earnings,
        type = "scatter",
        mode = "markers",
        name = init,
        marker = list(
          size = ~sqrt(jobs) / 3,
          color = initiative_colors[init],
          opacity = 0.6,
          line = list(color = "white", width = 0.5)
        ),
        text = ~occ_short,
        hovertemplate = paste0(
          "<b>%{text}</b><br>",
          "Jobs: %{x:,}<br>",
          "Median Wage: %{y:$,}<br>",
          "<extra>", init, "</extra>"
        )
      )
  }
}

p4_interactive <- p4_interactive %>%
  layout(
    title = list(
      text = paste0("<b>Wage vs Employment by Occupation</b><br><sup>",
                   "Bubble size = employment level | Min 50 jobs | Indiana | ",
                   recent_year, "</sup>"),
      font = list(size = 16)
    ),
    xaxis = list(
      title = "Total Jobs",
      type = "log",
      showgrid = TRUE,
      gridcolor = "lightgray"
    ),
    yaxis = list(
      title = "Median Annual Earnings",
      showgrid = TRUE,
      gridcolor = "lightgray"
    ),
    legend = list(x = 0.02, y = 0.98),
    margin = list(l = 80, r = 80, t = 100, b = 80)
  )

saveWidget(
  p4_interactive,
  file.path(viz_dir, "04_wage_vs_employment_scatter.html"),
  selfcontained = TRUE
)

cat("Visualization 4 created (static + interactive)\n")

# VISUALIZATION 5: Wage Distribution Box Plots --------------------------------

cat("\n=== Creating Visualization 5: Wage Distribution by Initiative ===\n")

p5_data <- wage_data %>%
  filter(year == recent_year,
         geo_area == "Indiana",
         cicp_initiative != "Total Employment",
         !is.na(median_annual_earnings)) %>%
  left_join(
    emp_data %>%
      filter(year == recent_year, geo_area == "Indiana") %>%
      select(cicp_initiative, occ_code, jobs),
    by = c("cicp_initiative", "occ_code")
  ) %>%
  filter(jobs >= 50)

p5 <- p5_data %>%
  ggplot(aes(x = cicp_initiative, y = median_annual_earnings, 
             fill = cicp_initiative)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.5, show.legend = FALSE) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, 
               fill = "white", color = "black") +
  scale_y_continuous(labels = dollar, expand = expansion(mult = c(0.05, 0.1))) +
  scale_fill_manual(values = initiative_colors) +
  labs(
    title = "Wage Distribution by Initiative",
    subtitle = paste("Occupations with 50+ jobs | Indiana |", recent_year, 
                    "| Diamond = mean, line = median"),
    x = NULL,
    y = "Median Annual Earnings",
    caption = "Source: CICP Talent Demand Data"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    panel.grid.major.x = element_blank()
  )

ggsave(file.path(viz_dir, "05_wage_distribution_boxplots.png"),
       p5, width = 12, height = 8, dpi = 300, bg = "white")

# Interactive version with geography dropdown
geography_list <- c("Indiana", 
                   unique(wage_data$geo_area[wage_data$geo_type == "Metro"]))

all_initiatives <- unique(p5_data$cicp_initiative)

p5_interactive <- plot_ly()

trace_info <- list()
trace_counter <- 0

for(geo in geography_list) {
  geo_data <- wage_data %>%
    filter(year == recent_year,
           geo_area == geo,
           cicp_initiative != "Total Employment",
           !is.na(median_annual_earnings)) %>%
    left_join(
      emp_data %>%
        filter(year == recent_year, geo_area == geo) %>%
        select(cicp_initiative, occ_code, jobs),
      by = c("cicp_initiative", "occ_code")
    ) %>%
    filter(jobs >= 50) %>%
    mutate(wage_rounded = round(median_annual_earnings))
  
  if(nrow(geo_data) > 0) {
    for(init in all_initiatives) {
      init_data <- geo_data %>% filter(cicp_initiative == init)
      
      if(nrow(init_data) > 0) {
        trace_counter <- trace_counter + 1
        
        trace_info[[trace_counter]] <- list(
          geography = geo,
          initiative = init,
          trace_index = trace_counter
        )
        
        p5_interactive <- p5_interactive %>%
          add_trace(
            data = init_data,
            x = ~cicp_initiative,
            y = ~wage_rounded,
            type = "box",
            name = init,
            marker = list(color = initiative_colors[init], opacity = 0.5),
            fillcolor = paste0("rgba(", 
                              paste(col2rgb(initiative_colors[init]), 
                                    collapse = ","), ", 0.5)"),
            line = list(color = initiative_colors[init]),
            visible = if(geo == geography_list[1]) TRUE else FALSE,
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

updatemenus <- list(
  list(
    active = 0,
    type = "dropdown",
    x = 0.15,
    y = 1.15,
    xanchor = "left",
    yanchor = "top",
    buttons = lapply(seq_along(geography_list), function(i) {
      geo <- geography_list[i]
      
      visible_vec <- sapply(1:trace_counter, function(idx) {
        trace_info[[idx]]$geography == geo
      })
      
      list(
        method = "update",
        args = list(
          list(visible = visible_vec),
          list(title = list(
            text = paste0("<b>Wage Distribution by Initiative - ", geo, 
                         "</b><br><sup>Occupations with 50+ jobs | ", 
                         recent_year, "</sup>")
          ))
        ),
        label = geo
      )
    })
  )
)

p5_interactive <- p5_interactive %>%
  layout(
    title = list(
      text = paste0("<b>Wage Distribution by Initiative - ", geography_list[1],
                   "</b><br><sup>Occupations with 50+ jobs | ", recent_year, 
                   "</sup>"),
      font = list(size = 16)
    ),
    xaxis = list(
      title = "",
      tickangle = -45
    ),
    yaxis = list(
      title = "Median Annual Earnings",
      showgrid = TRUE,
      gridcolor = "lightgray"
    ),
    updatemenus = updatemenus,
    margin = list(l = 80, r = 80, t = 120, b = 120),
    showlegend = FALSE
  )

saveWidget(
  p5_interactive,
  file.path(viz_dir, "05_wage_distribution_boxplots.html"),
  selfcontained = TRUE
)

cat("Visualization 5 created (static + interactive)\n")

# VISUALIZATION 6: Time Series - Employment Trends ----------------------------

cat("\n=== Creating Visualization 6: Employment Trends Over Time ===\n")

# Get top 5 occupations per initiative
top_occs_per_init <- emp_data %>%
  filter(year == recent_year,
         geo_area == "Indiana",
         cicp_initiative != "Total Employment") %>%
  group_by(cicp_initiative) %>%
  arrange(desc(jobs)) %>%
  slice_head(n = 5) %>%
  ungroup() %>%
  select(cicp_initiative, occ_code, occupation)

p6_data <- emp_data %>%
  filter(geo_area == "Indiana",
         cicp_initiative != "Total Employment") %>%
  inner_join(top_occs_per_init, by = c("cicp_initiative", "occ_code")) %>%
  mutate(occ_short = str_trunc(occupation.x, 35, "right"))

p6 <- p6_data %>%
  ggplot(aes(x = year, y = jobs, color = occ_short, group = occ_code)) +
  geom_line(linewidth = 1, alpha = 0.8) +
  geom_point(size = 2, alpha = 0.6) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = seq(min(p6_data$year), 
                                  max(p6_data$year), by = 2)) +
  facet_wrap(~cicp_initiative, scales = "free_y", ncol = 2) +
  labs(
    title = "Employment Trends for Top 5 Occupations by Initiative",
    subtitle = paste("Indiana | Years:", min(p6_data$year), "-", 
                    max(p6_data$year)),
    x = "Year",
    y = "Total Jobs",
    color = "Occupation",
    caption = "Source: CICP Talent Demand Data"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "bottom",
    legend.text = element_text(size = 7),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
  ) +
  guides(color = guide_legend(ncol = 2))

ggsave(file.path(viz_dir, "06_employment_trends_over_time.png"),
       p6, width = 16, height = 14, dpi = 300, bg = "white")

# Interactive version
p6_interactive <- plot_ly()

for(init in initiative_list) {
  init_data <- p6_data %>% filter(cicp_initiative == init)
  
  if(nrow(init_data) > 0) {
    for(occ in unique(init_data$occ_code)) {
      occ_data <- init_data %>% filter(occ_code == occ)
      
      p6_interactive <- p6_interactive %>%
        add_trace(
          data = occ_data,
          x = ~year,
          y = ~jobs,
          type = "scatter",
          mode = "lines+markers",
          name = unique(occ_data$occ_short),
          visible = if(init == initiative_list[1]) TRUE else FALSE,
          legendgroup = init,
          hovertemplate = paste0(
            "<b>%{fullData.name}</b><br>",
            "Year: %{x}<br>",
            "Jobs: %{y:,}<br>",
            "<extra></extra>"
          )
        )
    }
  }
}

# Create dropdown for initiatives
n_traces_per_init <- sapply(initiative_list, function(init) {
  length(unique(p6_data$occ_code[p6_data$cicp_initiative == init]))
})

updatemenus <- list(
  list(
    active = 0,
    type = "dropdown",
    x = 0.15,
    y = 1.15,
    buttons = lapply(seq_along(initiative_list), function(i) {
      init <- initiative_list[i]
      
      # Calculate trace indices for this initiative
      if(i == 1) {
        start_idx <- 1
      } else {
        start_idx <- sum(n_traces_per_init[1:(i-1)]) + 1
      }
      end_idx <- start_idx + n_traces_per_init[i] - 1
      
      visible_vec <- rep(FALSE, sum(n_traces_per_init))
      visible_vec[start_idx:end_idx] <- TRUE
      
      list(
        method = "update",
        args = list(
          list(visible = visible_vec),
          list(
            title = list(
              text = paste0("<b>Employment Trends - ", init,
                           "</b><br><sup>Top 5 Occupations | Indiana | ",
                           min(p6_data$year), "-", max(p6_data$year), "</sup>")
            )
          )
        ),
        label = init
      )
    })
  )
)

p6_interactive <- p6_interactive %>%
  layout(
    title = list(
      text = paste0("<b>Employment Trends - ", initiative_list[1],
                   "</b><br><sup>Top 5 Occupations | Indiana | ",
                   min(p6_data$year), "-", max(p6_data$year), "</sup>"),
      font = list(size = 16)
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
    updatemenus = updatemenus,
    legend = list(x = 1.02, y = 1),
    margin = list(l = 80, r = 200, t = 100, b = 80)
  )

saveWidget(
  p6_interactive,
  file.path(viz_dir, "06_employment_trends_over_time.html"),
  selfcontained = TRUE
)

cat("Visualization 6 created (static + interactive)\n")

# VISUALIZATION 7: Metro Comparison - Top Occupations ------------------------

cat("\n=== Creating Visualization 7: Metro Comparison ===\n")

# Get top metros
top_metros <- emp_data %>%
  filter(year == recent_year,
         geo_type == "Metro",
         cicp_initiative != "Total Employment") %>%
  group_by(geo_area) %>%
  summarise(total_jobs = sum(jobs, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_jobs)) %>%
  head(5) %>%
  pull(geo_area)

# Get top occupations across these metros
p7_data <- emp_data %>%
  filter(year == recent_year,
         geo_area %in% c("Indiana", top_metros),
         cicp_initiative != "Total Employment") %>%
  group_by(geo_area, occ_code, occupation) %>%
  summarise(total_jobs = sum(jobs, na.rm = TRUE), .groups = "drop") %>%
  group_by(geo_area) %>%
  arrange(desc(total_jobs)) %>%
  slice_head(n = 10) %>%
  ungroup() %>%
  mutate(occ_short = str_trunc(occupation, 40, "right"))

p7 <- p7_data %>%
  ggplot(aes(x = reorder(occ_short, total_jobs), y = total_jobs)) +
  geom_col(fill = "#1565C0", alpha = 0.8) +
  geom_text(aes(label = comma(total_jobs)), hjust = -0.1, size = 2.5) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  coord_flip() +
  facet_wrap(~geo_area, scales = "free", ncol = 2) +
  labs(
    title = "Top 10 Occupations by Employment",
    subtitle = paste("Indiana & Top Metros |", recent_year),
    x = NULL,
    y = "Total Jobs (across all initiatives)",
    caption = "Source: CICP Talent Demand Data"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 7)
  )

ggsave(file.path(viz_dir, "07_metro_comparison_top_occupations.png"),
       p7, width = 16, height = 14, dpi = 300, bg = "white")

# Interactive version - single trace so y-axis only shows the selected geography
geography_list_p7 <- c("Indiana", top_metros)

first_geo_p7 <- geography_list_p7[1]
geo_data_first_p7 <- p7_data %>% filter(geo_area == first_geo_p7) %>% arrange(total_jobs)

p7_interactive <- plot_ly(
  data = geo_data_first_p7,
  x = ~total_jobs,
  y = ~occ_short,
  type = "bar",
  orientation = "h",
  marker = list(color = "#1565C0"),
  text = ~comma(total_jobs),
  textposition = "outside",
  textfont = list(size = 10),
  showlegend = FALSE,
  hovertemplate = paste0(
    "<b>%{y}</b><br>",
    "Total Jobs: %{x:,}<br>",
    "<extra></extra>"
  )
)

updatemenus <- list(
  list(
    active = 0,
    type = "dropdown",
    x = 0.15,
    y = 1.15,
    buttons = lapply(seq_along(geography_list_p7), function(i) {
      geo <- geography_list_p7[i]
      geo_data <- p7_data %>% filter(geo_area == geo) %>% arrange(total_jobs)

      list(
        method = "update",
        args = list(
          list(
            x = list(geo_data$total_jobs),
            y = list(geo_data$occ_short),
            text = list(comma(geo_data$total_jobs))
          ),
          list(
            title = list(
              text = paste0("<b>Top 10 Occupations - ", geo,
                           "</b><br><sup>Total across all initiatives | ",
                           recent_year, "</sup>")
            ),
            yaxis = list(
              categoryarray = geo_data$occ_short,
              categoryorder = "array",
              title = "",
              showgrid = FALSE
            )
          )
        ),
        label = geo
      )
    })
  )
)

p7_interactive <- p7_interactive %>%
  layout(
    title = list(
      text = paste0("<b>Top 10 Occupations - ", geography_list_p7[1],
                   "</b><br><sup>Total across all initiatives | ",
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
      showgrid = FALSE,
      categoryarray = geo_data_first_p7$occ_short,
      categoryorder = "array"
    ),
    updatemenus = updatemenus,
    margin = list(l = 350, r = 100, t = 100, b = 80),
    plot_bgcolor = "white",
    paper_bgcolor = "white"
  )

saveWidget(
  p7_interactive,
  file.path(viz_dir, "07_metro_comparison_top_occupations.html"),
  selfcontained = TRUE
)

cat("Visualization 7 created (static + interactive)\n")

# VISUALIZATION 8: Wage Growth Heatmap ----------------------------------------

cat("\n=== Creating Visualization 8: Wage Growth Heatmap ===\n")

p8_data <- wage_growth %>%
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
  slice_head(n = 15) %>%
  ungroup() %>%
  mutate(occ_short = str_trunc(occupation, 35, "right"))

p8 <- p8_data %>%
  ggplot(aes(x = cicp_initiative, y = reorder(occ_short, cagr_2yr), 
             fill = cagr_2yr)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.1f%%", cagr_2yr)), size = 2.5) +
  scale_fill_gradient2(
    low = "#D84315", 
    mid = "white", 
    high = "#2E7D32",
    midpoint = 0,
    labels = label_percent(scale = 1)
  ) +
  labs(
    title = "Wage Growth Rate by Occupation and Initiative",
    subtitle = paste("2-Year CAGR | Min 100 jobs | Indiana |", recent_year),
    x = NULL,
    y = NULL,
    fill = "2-Year\nCAGR",
    caption = "Source: CICP Talent Demand Data"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 7),
    panel.grid = element_blank(),
    legend.position = "right"
  )

ggsave(file.path(viz_dir, "08_wage_growth_heatmap.png"),
       p8, width = 14, height = 12, dpi = 300, bg = "white")

# Interactive version
p8_interactive <- plot_ly(
  data = p8_data,
  x = ~cicp_initiative,
  y = ~reorder(occ_short, cagr_2yr),
  z = ~cagr_2yr,
  type = "heatmap",
  colorscale = list(
    c(0, "#D84315"),
    c(0.5, "white"),
    c(1, "#2E7D32")
  ),
  zmid = 0,
  text = ~sprintf("%.1f%%", cagr_2yr),
  texttemplate = "%{text}",
  textfont = list(size = 10),
  hovertemplate = paste0(
    "<b>%{y}</b><br>",
    "%{x}<br>",
    "2-Year CAGR: %{z:.1f}%<br>",
    "<extra></extra>"
  ),
  colorbar = list(title = "2-Year\nCAGR (%)")
) %>%
  layout(
    title = list(
      text = paste0("<b>Wage Growth Rate by Occupation and Initiative</b><br><sup>",
                   "2-Year CAGR | Min 100 jobs | Indiana | ", recent_year, 
                   "</sup>"),
      font = list(size = 16)
    ),
    xaxis = list(
      title = "",
      tickangle = -45
    ),
    yaxis = list(
      title = "",
      tickfont = list(size = 9)
    ),
    margin = list(l = 280, r = 100, t = 100, b = 120)
  )

saveWidget(
  p8_interactive,
  file.path(viz_dir, "08_wage_growth_heatmap.html"),
  selfcontained = TRUE
)

cat("Visualization 8 created (static + interactive)\n")

# ============================================================================
# CICP Talent Demand - STEM and TECH Classification Visualizations
# ============================================================================
# Purpose: Analyze STEM vs non-STEM and TECH vs non-TECH employment patterns
# ============================================================================

cat("\n=== Loading STEM and TECH Classifications ===\n")

library(readxl)

# Load STEM classifications
stem_classifications <- read_excel(file.path(data_dir, "CICP_STEM.xlsx")) %>%
  clean_names() %>%
  mutate(
    soccode = str_trim(soccode),  # Clean any whitespace
    is_stem = TRUE
  ) %>%
  select(soccode, is_stem) %>%
  distinct()

# Load TECH classifications
tech_classifications <- read_excel(file.path(data_dir, "CICP_TECH.xlsx")) %>%
  clean_names() %>%
  mutate(
    soccode = str_trim(soccode),
    is_tech = TRUE
  ) %>%
  select(soccode, is_tech) %>%
  distinct()

cat(sprintf("Loaded %d STEM occupations and %d TECH occupations\n", 
            nrow(stem_classifications), nrow(tech_classifications)))

# Join STEM and TECH flags to employment data
emp_data_classified <- emp_data %>%
  left_join(stem_classifications, by = c("occ_code" = "soccode")) %>%
  left_join(tech_classifications, by = c("occ_code" = "soccode")) %>%
  mutate(
    is_stem = replace_na(is_stem, FALSE),
    is_tech = replace_na(is_tech, FALSE),
    stem_category = if_else(is_stem, "STEM", "Non-STEM"),
    tech_category = if_else(is_tech, "TECH", "Non-TECH")
  )

# Join to wage data as well
wage_data_classified <- wage_data %>%
  left_join(stem_classifications, by = c("occ_code" = "soccode")) %>%
  left_join(tech_classifications, by = c("occ_code" = "soccode")) %>%
  mutate(
    is_stem = replace_na(is_stem, FALSE),
    is_tech = replace_na(is_tech, FALSE),
    stem_category = if_else(is_stem, "STEM", "Non-STEM"),
    tech_category = if_else(is_tech, "TECH", "Non-TECH")
  )

# Diagnostic: Check classification coverage
cat("\n=== Classification Coverage ===\n")
coverage <- emp_data_classified %>%
  filter(year == recent_year, geo_area == "Indiana") %>%
  summarise(
    total_jobs = sum(jobs, na.rm = TRUE),
    stem_jobs = sum(jobs[is_stem], na.rm = TRUE),
    tech_jobs = sum(jobs[is_tech], na.rm = TRUE),
    stem_pct = stem_jobs / total_jobs * 100,
    tech_pct = tech_jobs / total_jobs * 100
  )
print(coverage)

# VISUALIZATION 9: STEM vs Non-STEM Employment -------------------------------

cat("\n=== Creating Visualization 9: STEM vs Non-STEM Employment ===\n")

# Prepare data by initiative only (Indiana)
stem_by_initiative <- emp_data_classified %>%
  filter(year == recent_year,
         geo_area == "Indiana",
         cicp_initiative != "Total Employment") %>%
  group_by(cicp_initiative, stem_category) %>%
  summarise(
    jobs = sum(jobs, na.rm = TRUE),
    .groups = "drop"
  )

# Static: Stacked bar chart
p9_static <- stem_by_initiative %>%
  ggplot(aes(x = cicp_initiative, y = jobs, fill = stem_category)) +
  geom_col(position = "stack", alpha = 0.8) +
  geom_text(
    data = stem_by_initiative %>%
      group_by(cicp_initiative) %>%
      summarise(total = sum(jobs), .groups = "drop"),
    aes(x = cicp_initiative, y = total, label = comma(total)),
    inherit.aes = FALSE,
    vjust = -0.5,
    size = 3,
    fontface = "bold"
  ) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(
    values = c("STEM" = "#1565C0", "Non-STEM" = "#BDBDBD"),
    name = NULL
  ) +
  labs(
    title = "STEM vs Non-STEM Employment by Initiative",
    subtitle = paste("Indiana |", recent_year),
    x = NULL,
    y = "Total Jobs",
    caption = "Source: CICP Talent Demand Data"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = "top",
    panel.grid.major.x = element_blank()
  )

ggsave(file.path(viz_dir, "09_stem_employment_by_initiative.png"),
       p9_static, width = 12, height = 8, dpi = 300, bg = "white")

# Interactive: Dropdown for geography breakdown
stem_by_geo <- emp_data_classified %>%
  filter(year == recent_year,
         cicp_initiative != "Total Employment") %>%
  group_by(cicp_initiative, geo_area, stem_category) %>%
  summarise(
    jobs = sum(jobs, na.rm = TRUE),
    .groups = "drop"
  )

# Get unique initiative-geography combinations
init_geo_combos <- stem_by_geo %>%
  distinct(cicp_initiative, geo_area) %>%
  arrange(cicp_initiative, geo_area)

p9_interactive <- plot_ly()

trace_counter <- 0
trace_map <- list()

# Add traces for each combo
for(i in 1:nrow(init_geo_combos)) {
  combo <- init_geo_combos[i, ]
  
  combo_data <- stem_by_geo %>%
    filter(cicp_initiative == combo$cicp_initiative,
           geo_area == combo$geo_area)
  
  if(nrow(combo_data) > 0) {
    # Add STEM bar
    stem_data <- combo_data %>% filter(stem_category == "STEM")
    if(nrow(stem_data) > 0) {
      trace_counter <- trace_counter + 1
      trace_map[[trace_counter]] <- list(
        initiative = combo$cicp_initiative,
        geo = combo$geo_area
      )
      
      p9_interactive <- p9_interactive %>%
        add_trace(
          data = stem_data,
          x = ~stem_category,
          y = ~jobs,
          type = "bar",
          name = "STEM",
          marker = list(color = "#1565C0"),
          visible = (combo$cicp_initiative == init_geo_combos$cicp_initiative[1] &&
                    combo$geo_area == init_geo_combos$geo_area[1]),
          legendgroup = "STEM",
          showlegend = (i == 1),
          hovertemplate = paste0(
            "<b>STEM</b><br>",
            "Jobs: %{y:,}<br>",
            "<extra></extra>"
          )
        )
    }
    
    # Add Non-STEM bar
    nonstem_data <- combo_data %>% filter(stem_category == "Non-STEM")
    if(nrow(nonstem_data) > 0) {
      trace_counter <- trace_counter + 1
      trace_map[[trace_counter]] <- list(
        initiative = combo$cicp_initiative,
        geo = combo$geo_area
      )
      
      p9_interactive <- p9_interactive %>%
        add_trace(
          data = nonstem_data,
          x = ~stem_category,
          y = ~jobs,
          type = "bar",
          name = "Non-STEM",
          marker = list(color = "#BDBDBD"),
          visible = (combo$cicp_initiative == init_geo_combos$cicp_initiative[1] &&
                    combo$geo_area == init_geo_combos$geo_area[1]),
          legendgroup = "Non-STEM",
          showlegend = (i == 1),
          hovertemplate = paste0(
            "<b>Non-STEM</b><br>",
            "Jobs: %{y:,}<br>",
            "<extra></extra>"
          )
        )
    }
  }
}

# Create two-level dropdown: Initiative first, then Geography
init_list <- unique(init_geo_combos$cicp_initiative)

dropdown_buttons_init <- lapply(seq_along(init_list), function(i) {
  init <- init_list[i]
  
  # Get first geography for this initiative
  first_geo <- init_geo_combos %>%
    filter(cicp_initiative == init) %>%
    slice(1) %>%
    pull(geo_area)
  
  visible_vec <- sapply(trace_map, function(tm) {
    tm$initiative == init && tm$geo == first_geo
  })
  
  list(
    method = "update",
    args = list(
      list(visible = visible_vec),
      list(
        title = list(
          text = paste0("<b>STEM vs Non-STEM Employment</b><br><sup>",
                       init, " | ", first_geo, " | ", recent_year, "</sup>")
        )
      )
    ),
    label = init
  )
})

dropdown_buttons_geo <- lapply(1:nrow(init_geo_combos), function(i) {
  combo <- init_geo_combos[i, ]
  
  visible_vec <- sapply(trace_map, function(tm) {
    tm$initiative == combo$cicp_initiative && tm$geo == combo$geo_area
  })
  
  list(
    method = "update",
    args = list(
      list(visible = visible_vec),
      list(
        title = list(
          text = paste0("<b>STEM vs Non-STEM Employment</b><br><sup>",
                       combo$cicp_initiative, " | ", combo$geo_area, 
                       " | ", recent_year, "</sup>")
        )
      )
    ),
    label = combo$geo_area
  )
})

p9_interactive <- p9_interactive %>%
  layout(
    title = list(
      text = paste0("<b>STEM vs Non-STEM Employment</b><br><sup>",
                   init_geo_combos$cicp_initiative[1], " | ", 
                   init_geo_combos$geo_area[1], " | ", recent_year, "</sup>"),
      font = list(size = 16)
    ),
    xaxis = list(title = ""),
    yaxis = list(title = "Total Jobs", tickformat = ","),
    barmode = "stack",
    updatemenus = list(
      # Initiative dropdown
      list(
        active = 0,
        type = "dropdown",
        x = 0.15,
        y = 1.15,
        xanchor = "left",
        yanchor = "top",
        buttons = dropdown_buttons_init
      ),
      # Geography dropdown
      list(
        active = 0,
        type = "dropdown",
        x = 0.4,
        y = 1.15,
        xanchor = "left",
        yanchor = "top",
        buttons = dropdown_buttons_geo
      )
    ),
    legend = list(orientation = "h", x = 0.5, y = -0.15, xanchor = "center"),
    margin = list(l = 80, r = 80, t = 120, b = 80)
  )

saveWidget(
  p9_interactive,
  file.path(viz_dir, "09_stem_employment_interactive.html"),
  selfcontained = TRUE
)

cat("Visualization 9 created (static + interactive)\n")

# VISUALIZATION 10: TECH vs Non-TECH Employment ------------------------------

cat("\n=== Creating Visualization 10: TECH vs Non-TECH Employment ===\n")

# Prepare data by initiative only (Indiana)
tech_by_initiative <- emp_data_classified %>%
  filter(year == recent_year,
         geo_area == "Indiana",
         cicp_initiative != "Total Employment") %>%
  group_by(cicp_initiative, tech_category) %>%
  summarise(
    jobs = sum(jobs, na.rm = TRUE),
    .groups = "drop"
  )

# Static: Stacked bar chart
p10_static <- tech_by_initiative %>%
  ggplot(aes(x = cicp_initiative, y = jobs, fill = tech_category)) +
  geom_col(position = "stack", alpha = 0.8) +
  geom_text(
    data = tech_by_initiative %>%
      group_by(cicp_initiative) %>%
      summarise(total = sum(jobs), .groups = "drop"),
    aes(x = cicp_initiative, y = total, label = comma(total)),
    inherit.aes = FALSE,
    vjust = -0.5,
    size = 3,
    fontface = "bold"
  ) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(
    values = c("TECH" = "#2E7D32", "Non-TECH" = "#BDBDBD"),
    name = NULL
  ) +
  labs(
    title = "TECH vs Non-TECH Employment by Initiative",
    subtitle = paste("Indiana |", recent_year),
    x = NULL,
    y = "Total Jobs",
    caption = "Source: CICP Talent Demand Data"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = "top",
    panel.grid.major.x = element_blank()
  )

ggsave(file.path(viz_dir, "10_tech_employment_by_initiative.png"),
       p10_static, width = 12, height = 8, dpi = 300, bg = "white")

# Interactive: Dropdown for geography breakdown
tech_by_geo <- emp_data_classified %>%
  filter(year == recent_year,
         cicp_initiative != "Total Employment") %>%
  group_by(cicp_initiative, geo_area, tech_category) %>%
  summarise(
    jobs = sum(jobs, na.rm = TRUE),
    .groups = "drop"
  )

p10_interactive <- plot_ly()

trace_counter <- 0
trace_map <- list()

# Add traces for each combo (reuse init_geo_combos from above)
for(i in 1:nrow(init_geo_combos)) {
  combo <- init_geo_combos[i, ]
  
  combo_data <- tech_by_geo %>%
    filter(cicp_initiative == combo$cicp_initiative,
           geo_area == combo$geo_area)
  
  if(nrow(combo_data) > 0) {
    # Add TECH bar
    tech_data <- combo_data %>% filter(tech_category == "TECH")
    if(nrow(tech_data) > 0) {
      trace_counter <- trace_counter + 1
      trace_map[[trace_counter]] <- list(
        initiative = combo$cicp_initiative,
        geo = combo$geo_area
      )
      
      p10_interactive <- p10_interactive %>%
        add_trace(
          data = tech_data,
          x = ~tech_category,
          y = ~jobs,
          type = "bar",
          name = "TECH",
          marker = list(color = "#2E7D32"),
          visible = (combo$cicp_initiative == init_geo_combos$cicp_initiative[1] &&
                    combo$geo_area == init_geo_combos$geo_area[1]),
          legendgroup = "TECH",
          showlegend = (i == 1),
          hovertemplate = paste0(
            "<b>TECH</b><br>",
            "Jobs: %{y:,}<br>",
            "<extra></extra>"
          )
        )
    }
    
    # Add Non-TECH bar
    nontech_data <- combo_data %>% filter(tech_category == "Non-TECH")
    if(nrow(nontech_data) > 0) {
      trace_counter <- trace_counter + 1
      trace_map[[trace_counter]] <- list(
        initiative = combo$cicp_initiative,
        geo = combo$geo_area
      )
      
      p10_interactive <- p10_interactive %>%
        add_trace(
          data = nontech_data,
          x = ~tech_category,
          y = ~jobs,
          type = "bar",
          name = "Non-TECH",
          marker = list(color = "#BDBDBD"),
          visible = (combo$cicp_initiative == init_geo_combos$cicp_initiative[1] &&
                    combo$geo_area == init_geo_combos$geo_area[1]),
          legendgroup = "Non-TECH",
          showlegend = (i == 1),
          hovertemplate = paste0(
            "<b>Non-TECH</b><br>",
            "Jobs: %{y:,}<br>",
            "<extra></extra>"
          )
        )
    }
  }
}

# Create two-level dropdown
dropdown_buttons_init_tech <- lapply(seq_along(init_list), function(i) {
  init <- init_list[i]
  
  first_geo <- init_geo_combos %>%
    filter(cicp_initiative == init) %>%
    slice(1) %>%
    pull(geo_area)
  
  visible_vec <- sapply(trace_map, function(tm) {
    tm$initiative == init && tm$geo == first_geo
  })
  
  list(
    method = "update",
    args = list(
      list(visible = visible_vec),
      list(
        title = list(
          text = paste0("<b>TECH vs Non-TECH Employment</b><br><sup>",
                       init, " | ", first_geo, " | ", recent_year, "</sup>")
        )
      )
    ),
    label = init
  )
})

dropdown_buttons_geo_tech <- lapply(1:nrow(init_geo_combos), function(i) {
  combo <- init_geo_combos[i, ]
  
  visible_vec <- sapply(trace_map, function(tm) {
    tm$initiative == combo$cicp_initiative && tm$geo == combo$geo_area
  })
  
  list(
    method = "update",
    args = list(
      list(visible = visible_vec),
      list(
        title = list(
          text = paste0("<b>TECH vs Non-TECH Employment</b><br><sup>",
                       combo$cicp_initiative, " | ", combo$geo_area, 
                       " | ", recent_year, "</sup>")
        )
      )
    ),
    label = combo$geo_area
  )
})

p10_interactive <- p10_interactive %>%
  layout(
    title = list(
      text = paste0("<b>TECH vs Non-TECH Employment</b><br><sup>",
                   init_geo_combos$cicp_initiative[1], " | ", 
                   init_geo_combos$geo_area[1], " | ", recent_year, "</sup>"),
      font = list(size = 16)
    ),
    xaxis = list(title = ""),
    yaxis = list(title = "Total Jobs", tickformat = ","),
    barmode = "stack",
    updatemenus = list(
      list(
        active = 0,
        type = "dropdown",
        x = 0.15,
        y = 1.15,
        xanchor = "left",
        yanchor = "top",
        buttons = dropdown_buttons_init_tech
      ),
      list(
        active = 0,
        type = "dropdown",
        x = 0.4,
        y = 1.15,
        xanchor = "left",
        yanchor = "top",
        buttons = dropdown_buttons_geo_tech
      )
    ),
    legend = list(orientation = "h", x = 0.5, y = -0.15, xanchor = "center"),
    margin = list(l = 80, r = 80, t = 120, b = 80)
  )

saveWidget(
  p10_interactive,
  file.path(viz_dir, "10_tech_employment_interactive.html"),
  selfcontained = TRUE
)

cat("Visualization 10 created (static + interactive)\n")

# VISUALIZATION 11: STEM/TECH Summary Tables ----------------------------------

cat("\n=== Creating Visualization 11: STEM/TECH Summary Tables ===\n")

# Table 1: STEM by Initiative and Geography
stem_summary_table <- emp_data_classified %>%
  filter(year == recent_year,
         cicp_initiative != "Total Employment") %>%
  group_by(cicp_initiative, geo_area) %>%
  summarise(
    total_jobs = sum(jobs, na.rm = TRUE),
    stem_jobs = sum(jobs[is_stem], na.rm = TRUE),
    nonstem_jobs = sum(jobs[!is_stem], na.rm = TRUE),
    stem_pct = (stem_jobs / total_jobs) * 100,
    .groups = "drop"
  ) %>%
  arrange(cicp_initiative, geo_area)

write_csv(stem_summary_table,
          file.path(output_dir, "stem_employment_summary.csv"))

# Table 2: TECH by Initiative and Geography
tech_summary_table <- emp_data_classified %>%
  filter(year == recent_year,
         cicp_initiative != "Total Employment") %>%
  group_by(cicp_initiative, geo_area) %>%
  summarise(
    total_jobs = sum(jobs, na.rm = TRUE),
    tech_jobs = sum(jobs[is_tech], na.rm = TRUE),
    nontech_jobs = sum(jobs[!is_tech], na.rm = TRUE),
    tech_pct = (tech_jobs / total_jobs) * 100,
    .groups = "drop"
  ) %>%
  arrange(cicp_initiative, geo_area)

write_csv(tech_summary_table,
          file.path(output_dir, "tech_employment_summary.csv"))

cat("Summary tables saved to output directory\n")

# VISUALIZATION 12: STEM/TECH Trends Over Time --------------------------------

cat("\n=== Creating Visualization 12: STEM/TECH Trends Over Time ===\n")

# Prepare trend data - ONLY REAL DATA (2019-2024)
stem_trends <- emp_data_classified %>%
  filter(geo_area == "Indiana",
         cicp_initiative != "Total Employment",
         year >= 2019,
         year <= 2024) %>%
  group_by(cicp_initiative, year, stem_category) %>%
  summarise(
    jobs = sum(jobs, na.rm = TRUE),
    .groups = "drop"
  )

tech_trends <- emp_data_classified %>%
  filter(geo_area == "Indiana",
         cicp_initiative != "Total Employment",
         year >= 2019,
         year <= 2024) %>%
  group_by(cicp_initiative, year, tech_category) %>%
  summarise(
    jobs = sum(jobs, na.rm = TRUE),
    .groups = "drop"
  )

# Static: STEM trends
p12a_static <- stem_trends %>%
  ggplot(aes(x = year, y = jobs, color = stem_category, linetype = stem_category)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = 2019:2024) +
  scale_color_manual(values = c("STEM" = "#1565C0", "Non-STEM" = "#757575")) +
  scale_linetype_manual(values = c("STEM" = "solid", "Non-STEM" = "dashed")) +
  facet_wrap(~cicp_initiative, scales = "free_y", ncol = 2) +
  labs(
    title = "STEM vs Non-STEM Employment Trends",
    subtitle = "Indiana | 2019-2024 (Actual Data Only)",
    x = "Year",
    y = "Total Jobs",
    color = NULL,
    linetype = NULL,
    caption = "Source: CICP Talent Demand Data"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(file.path(viz_dir, "12a_stem_trends_over_time.png"),
       p12a_static, width = 14, height = 10, dpi = 300, bg = "white")

# Static: TECH trends
p12b_static <- tech_trends %>%
  ggplot(aes(x = year, y = jobs, color = tech_category, linetype = tech_category)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = 2019:2024) +
  scale_color_manual(values = c("TECH" = "#2E7D32", "Non-TECH" = "#757575")) +
  scale_linetype_manual(values = c("TECH" = "solid", "Non-TECH" = "dashed")) +
  facet_wrap(~cicp_initiative, scales = "free_y", ncol = 2) +
  labs(
    title = "TECH vs Non-TECH Employment Trends",
    subtitle = "Indiana | 2019-2024 (Actual Data Only)",
    x = "Year",
    y = "Total Jobs",
    color = NULL,
    linetype = NULL,
    caption = "Source: CICP Talent Demand Data"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(file.path(viz_dir, "12b_tech_trends_over_time.png"),
       p12b_static, width = 14, height = 10, dpi = 300, bg = "white")

# Interactive: STEM trends with initiative dropdown
p12a_interactive <- plot_ly()

for(init in unique(stem_trends$cicp_initiative)) {
  for(category in c("STEM", "Non-STEM")) {
    init_cat_data <- stem_trends %>%
      filter(cicp_initiative == init, stem_category == category)
    
    if(nrow(init_cat_data) > 0) {
      p12a_interactive <- p12a_interactive %>%
        add_trace(
          data = init_cat_data,
          x = ~year,
          y = ~jobs,
          type = "scatter",
          mode = "lines+markers",
          name = category,
          line = list(
            width = 3,
            color = if(category == "STEM") "#1565C0" else "#757575",
            dash = if(category == "STEM") "solid" else "dash"
          ),
          marker = list(
            size = 8,
            color = if(category == "STEM") "#1565C0" else "#757575"
          ),
          visible = (init == unique(stem_trends$cicp_initiative)[1]),
          legendgroup = category,
          showlegend = (init == unique(stem_trends$cicp_initiative)[1]),
          hovertemplate = paste0(
            "<b>", category, "</b><br>",
            "Year: %{x}<br>",
            "Jobs: %{y:,}<br>",
            "<extra></extra>"
          )
        )
    }
  }
}

# Create dropdown for STEM trends
initiative_list_stem <- unique(stem_trends$cicp_initiative)
updatemenus_stem <- list(
  list(
    active = 0,
    type = "dropdown",
    x = 0.15,
    y = 1.15,
    buttons = lapply(seq_along(initiative_list_stem), function(i) {
      init <- initiative_list_stem[i]
      
      visible_vec <- rep(FALSE, length(initiative_list_stem) * 2)  # 2 categories
      start_idx <- (i - 1) * 2 + 1
      visible_vec[start_idx:(start_idx + 1)] <- TRUE
      
      list(
        method = "update",
        args = list(
          list(visible = visible_vec),
          list(
            title = list(
              text = paste0("<b>STEM vs Non-STEM Trends - ", init,
                           "</b><br><sup>Indiana | 2019-2024 (Actual Data Only)</sup>")
            )
          )
        ),
        label = init
      )
    })
  )
)

p12a_interactive <- p12a_interactive %>%
  layout(
    title = list(
      text = paste0("<b>STEM vs Non-STEM Trends - ", initiative_list_stem[1],
                   "</b><br><sup>Indiana | 2019-2024 (Actual Data Only)</sup>"),
      font = list(size = 16)
    ),
    xaxis = list(
      title = "Year",
      tickmode = "linear",
      tick0 = 2019,
      dtick = 1
    ),
    yaxis = list(title = "Total Jobs", tickformat = ","),
    updatemenus = updatemenus_stem,
    legend = list(orientation = "h", x = 0.5, y = -0.15, xanchor = "center"),
    margin = list(l = 80, r = 80, t = 120, b = 80)
  )

saveWidget(
  p12a_interactive,
  file.path(viz_dir, "12a_stem_trends_interactive.html"),
  selfcontained = TRUE
)

# Interactive: TECH trends with initiative dropdown
p12b_interactive <- plot_ly()

for(init in unique(tech_trends$cicp_initiative)) {
  for(category in c("TECH", "Non-TECH")) {
    init_cat_data <- tech_trends %>%
      filter(cicp_initiative == init, tech_category == category)
    
    if(nrow(init_cat_data) > 0) {
      p12b_interactive <- p12b_interactive %>%
        add_trace(
          data = init_cat_data,
          x = ~year,
          y = ~jobs,
          type = "scatter",
          mode = "lines+markers",
          name = category,
          line = list(
            width = 3,
            color = if(category == "TECH") "#2E7D32" else "#757575",
            dash = if(category == "TECH") "solid" else "dash"
          ),
          marker = list(
            size = 8,
            color = if(category == "TECH") "#2E7D32" else "#757575"
          ),
          visible = (init == unique(tech_trends$cicp_initiative)[1]),
          legendgroup = category,
          showlegend = (init == unique(tech_trends$cicp_initiative)[1]),
          hovertemplate = paste0(
            "<b>", category, "</b><br>",
            "Year: %{x}<br>",
            "Jobs: %{y:,}<br>",
            "<extra></extra>"
          )
        )
    }
  }
}

# Create dropdown for TECH trends
initiative_list_tech <- unique(tech_trends$cicp_initiative)
updatemenus_tech <- list(
  list(
    active = 0,
    type = "dropdown",
    x = 0.15,
    y = 1.15,
    buttons = lapply(seq_along(initiative_list_tech), function(i) {
      init <- initiative_list_tech[i]
      
      visible_vec <- rep(FALSE, length(initiative_list_tech) * 2)  # 2 categories
      start_idx <- (i - 1) * 2 + 1
      visible_vec[start_idx:(start_idx + 1)] <- TRUE
      
      list(
        method = "update",
        args = list(
          list(visible = visible_vec),
          list(
            title = list(
              text = paste0("<b>TECH vs Non-TECH Trends - ", init,
                           "</b><br><sup>Indiana | 2019-2024 (Actual Data Only)</sup>")
            )
          )
        ),
        label = init
      )
    })
  )
)

p12b_interactive <- p12b_interactive %>%
  layout(
    title = list(
      text = paste0("<b>TECH vs Non-TECH Trends - ", initiative_list_tech[1],
                   "</b><br><sup>Indiana | 2019-2024 (Actual Data Only)</sup>"),
      font = list(size = 16)
    ),
    xaxis = list(
      title = "Year",
      tickmode = "linear",
      tick0 = 2019,
      dtick = 1
    ),
    yaxis = list(title = "Total Jobs", tickformat = ","),
    updatemenus = updatemenus_tech,
    legend = list(orientation = "h", x = 0.5, y = -0.15, xanchor = "center"),
    margin = list(l = 80, r = 80, t = 120, b = 80)
  )

saveWidget(
  p12b_interactive,
  file.path(viz_dir, "12b_tech_trends_interactive.html"),
  selfcontained = TRUE
)

cat("Visualization 12 created (static + interactive for both STEM and TECH)\n")

# SUMMARY ---------------------------------------------------------------------

cat("\n" , rep("=", 70), "\n", sep = "")
cat("STEM/TECH VISUALIZATION SUMMARY\n")
cat(rep("=", 70), "\n\n", sep = "")

cat("Created 4 new visualization sets:\n\n")

cat("  9. STEM vs Non-STEM Employment\n")
cat("     - Static: Stacked bar by initiative (Indiana only)\n")
cat("     - Interactive: Two dropdowns (Initiative + Geography)\n\n")

cat(" 10. TECH vs Non-TECH Employment\n")
cat("     - Static: Stacked bar by initiative (Indiana only)\n")
cat("     - Interactive: Two dropdowns (Initiative + Geography)\n\n")

cat(" 11. Summary Tables (CSV exports)\n")
cat("     - STEM employment by initiative/geography\n")
cat("     - TECH employment by initiative/geography\n\n")

cat(" 12. Trends Over Time\n")
cat("     - Static: Line charts for STEM and TECH (faceted by initiative)\n")
cat("     - Interactive: Dropdown for initiative selection\n\n")

cat(rep("=", 70), "\n", sep = "")

# SUMMARY ---------------------------------------------------------------------

cat("\n" , rep("=", 70), "\n", sep = "")
cat("VISUALIZATION SUMMARY\n")
cat(rep("=", 70), "\n\n", sep = "")

cat(sprintf("Created 8 visualization sets in: %s/\n\n", viz_dir))

cat("Static Visualizations (PNG):\n")
cat("  1. Top occupations by initiative (faceted bar charts)\n")
cat("  2. Occupation concentration (Lorenz curves)\n")
cat("  3. Occupation growth rates (lollipop charts)\n")
cat("  4. Wage vs employment scatter plot\n")
cat("  5. Wage distribution box plots\n")
cat("  6. Employment trends over time (line charts)\n")
cat("  7. Metro comparison (faceted bar charts)\n")
cat("  8. Wage growth heatmap\n\n")

cat("Interactive Visualizations (HTML):\n")
cat("  - All 8 visualizations have interactive versions\n")
cat("  - Features: dropdowns, hover tooltips, zooming\n")
cat("  - Can be opened directly in web browser\n\n")

cat(rep("=", 70), "\n", sep = "")
cat("VISUALIZATION CREATION COMPLETE\n")
cat(rep("=", 70), "\n\n", sep = "")