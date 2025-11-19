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

# Load processed data
output_dir <- "outputs_occupation_20250709"  # Update with your output folder name
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

# Interactive version with initiative dropdown
p3_interactive <- plot_ly()

for(init in initiative_list) {
  init_data <- p3_data %>%
    filter(cicp_initiative == init) %>%
    arrange(cagr_2yr)
  
  if(nrow(init_data) > 0) {
    p3_interactive <- p3_interactive %>%
      add_trace(
        data = init_data,
        x = ~cagr_2yr,
        y = ~reorder(occ_short, cagr_2yr),
        type = "scatter",
        mode = "markers",
        marker = list(
          size = 10,
          color = ~ifelse(cagr_2yr >= 0, "#2E7D32", "#D84315")
        ),
        name = init,
        visible = if(init == initiative_list[1]) TRUE else FALSE,
        hovertemplate = paste0(
          "<b>%{y}</b><br>",
          "2-Year CAGR: %{x:.1f}%<br>",
          "Current Jobs: ", comma(init_data$jobs), "<br>",
          "<extra></extra>"
        )
      )
  }
}

# Add segments for each initiative
for(init in initiative_list) {
  init_data <- p3_data %>%
    filter(cicp_initiative == init) %>%
    arrange(cagr_2yr)
  
  if(nrow(init_data) > 0) {
    for(i in 1:nrow(init_data)) {
      p3_interactive <- p3_interactive %>%
        add_trace(
          x = c(0, init_data$cagr_2yr[i]),
          y = c(init_data$occ_short[i], init_data$occ_short[i]),
          type = "scatter",
          mode = "lines",
          line = list(
            color = ifelse(init_data$cagr_2yr[i] >= 0, "#2E7D32", "#D84315"),
            width = 2
          ),
          showlegend = FALSE,
          visible = if(init == initiative_list[1]) TRUE else FALSE,
          hoverinfo = "skip"
        )
    }
  }
}

# Create dropdown
updatemenus <- list(
  list(
    active = 0,
    type = "dropdown",
    x = 0.15,
    y = 1.15,
    buttons = lapply(seq_along(initiative_list), function(i) {
      init <- initiative_list[i]
      
      # Calculate number of traces per initiative (1 scatter + n segments)
      n_occs <- nrow(p3_data %>% filter(cicp_initiative == init))
      n_traces_per_init <- 1 + n_occs  # 1 scatter + segments
      
      # Create visibility vector
      visible_vec <- rep(FALSE, length(initiative_list) * n_traces_per_init)
      start_idx <- (i - 1) * n_traces_per_init + 1
      end_idx <- start_idx + n_traces_per_init - 1
      visible_vec[start_idx:end_idx] <- TRUE
      
      list(
        method = "update",
        args = list(
          list(visible = visible_vec),
          list(
            title = list(
              text = paste0("<b>Occupation Growth Rates - ", init,
                           "</b><br><sup>2-Year CAGR | Min 100 jobs | Indiana | ",
                           recent_year, "</sup>")
            )
          )
        ),
        label = init
      )
    })
  )
)

init_data_first <- p3_data %>% filter(cicp_initiative == initiative_list[1])

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
      showgrid = FALSE
    ),
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
  mutate(occ_short = str_trunc(occupation, 35, "right"))

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

# Interactive version
geography_list_p7 <- c("Indiana", top_metros)

p7_interactive <- plot_ly()

for(geo in geography_list_p7) {
  geo_data <- p7_data %>% 
    filter(geo_area == geo) %>%
    arrange(total_jobs)
  
  p7_interactive <- p7_interactive %>%
    add_trace(
      data = geo_data,
      x = ~total_jobs,
      y = ~reorder(occ_short, total_jobs),
      type = "bar",
      orientation = "h",
      marker = list(color = "#1565C0"),
      text = ~comma(total_jobs),
      textposition = "outside",
      textfont = list(size = 10),
      visible = if(geo == geography_list_p7[1]) TRUE else FALSE,
      name = geo,
      showlegend = FALSE,
      hovertemplate = paste0(
        "<b>%{y}</b><br>",
        "Total Jobs: %{x:,}<br>",
        "<extra></extra>"
      )
    )
}

updatemenus <- list(
  list(
    active = 0,
    type = "dropdown",
    x = 0.15,
    y = 1.15,
    buttons = lapply(seq_along(geography_list_p7), function(i) {
      geo <- geography_list_p7[i]
      
      visible_vec <- rep(FALSE, length(geography_list_p7))
      visible_vec[i] <- TRUE
      
      list(
        method = "update",
        args = list(
          list(visible = visible_vec),
          list(
            title = list(
              text = paste0("<b>Top 10 Occupations - ", geo,
                           "</b><br><sup>Total across all initiatives | ",
                           recent_year, "</sup>")
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
      showgrid = FALSE
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