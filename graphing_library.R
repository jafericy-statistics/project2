data_path <- "C:\\Users\\jacob\\git\\NCSU\\project2\\data\\combined.csv" 

#additional data packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)   
  library(skimr)     
  library(scales)    
})

theme_set(theme_minimal(base_size = 13))

#import in data and clean names
df_raw <- readr::read_csv(data_path, show_col_types = FALSE)  # use readRDS() if needed
df <- df_raw |> clean_names()

#pre-select some options here
num_cols <- names(select(df, where(is.numeric)))
cat_cols <- names(select(df, where(is_categorical)))

#this cleans data into cat_meta and then selects relative to levels (combination types of vars)
# with the 6 below, we revise towards average combinations.
cat_meta <- tibble(
  var = cat_cols,
  nlev = map_int(cat_cols, ~ n_distinct(df[[.x]], na.rm = TRUE))
) |>
  arrange(abs(nlev - 6))

#define cat vars
cat1 <- if (nrow(cat_meta) >= 1) cat_meta$var[1] else NA_character_
cat2 <- if (nrow(cat_meta) >= 2) cat_meta$var[2] else NA_character_

#define num vars
num1 <- if (length(num_cols) >= 1) num_cols[1] else NA_character_
num2 <- if (length(num_cols) >= 2) num_cols[2] else NA_character_

#one-way cont tables ----------
cat("\n=== ONE-WAY CONTINGENCY TABLES")
if (length(cat_cols) > 0) {
  for (v in head(cat_cols, 3)) {
    #cat("\n--", v, "--\n")
    tab <- df |>
      count(.data[[v]], name = "n") |>
      mutate(pct = n / sum(n)) |>
      arrange(desc(n))
    print(tab)
  }
} else cat("No cat vars")

#two-way cont table
cat("\n=== TWO-WAY CONTINGENCY TABLE")
if (!is.na(cat1) && !is.na(cat2) && cat1 != cat2) {
  tw_tab <- df |>
    tabyl(.data[[cat1]], .data[[cat2]]) |>
    adorn_totals(where = c("row", "col")) |>
    adorn_percentages("row") |>
    adorn_pct_formatting(digits = 1) |>
    adorn_ns()
  print(tw_tab)
} else cat("Not enough distinct cat vars for a 2-way table.")

#num summaries by levels of cat1
cat("\n=== NUMERICAL SUMMARIES BY GROUP")
if (!is.na(cat1)) {
  num_summary <- df |>
    group_by(.data[[cat1]]) |>
    summarise(
      across(
        where(is.numeric),
        list(
          n      = ~sum(!is.na(.x)),
          mean   = ~mean(.x, na.rm = TRUE),
          sd     = ~sd(.x, na.rm = TRUE),
          median = ~median(.x, na.rm = TRUE),
          p25    = ~quantile(.x, 0.25, na.rm = TRUE),
          p75    = ~quantile(.x, 0.75, na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    )
  print(num_summary)
} else cat("No cat vars")

#plots in fig folder for now
dir.create("figs", showWarnings = FALSE)

#plot 1 hist by group
if (!is.na(num1) && !is.na(cat1)) {
  p1 <- ggplot(df, aes(x = .data[[num1]], fill = .data[[cat1]])) +
    geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
    labs(title = paste("Distribution of", num1, "by", cat1), x = num1, y = "Count", fill = cat1)
  ggsave("figs/plot1_hist_by_group.png", p1, width = 8, height = 5, dpi = 150)
}

#plot 2 — boxplot with faceting
if (!is.na(num1) && !is.na(cat1)) {
  p2 <- ggplot(df, aes(x = .data[[cat1]], y = .data[[num1]], fill = .data[[cat1]])) +
    geom_boxplot(outlier.alpha = 0.5) +
    labs(title = paste("Boxplot of", num1, "across", cat1), x = cat1, y = num1) +
    theme(legend.position = "none")
  if (!is.na(cat2) && cat2 != cat1) {
    p2 <- p2 + facet_wrap(as.formula(paste("~", cat2))) +
      labs(subtitle = paste("Faceted by", cat2))
  }
  ggsave("figs/plot2_box_facet.png", p2, width = 9, height = 5, dpi = 150)
}

#plot 3 scatter with smooth
if (!is.na(num1) && !is.na(num2)) {
  p3 <- ggplot(df, aes(x = .data[[num1]], y = .data[[num2]])) +
    geom_point(aes(color = if (!is.na(cat1)) .data[[cat1]] else NULL), alpha = 0.7, na.rm = TRUE) +
    geom_smooth(method = "loess", se = TRUE, na.rm = TRUE) +
    labs(title = paste("Scatter of", num2, "vs", num1), x = num1, y = num2,
         color = if (!is.na(cat1)) cat1 else NULL)
  if (!is.na(cat2) && cat2 != cat1) {
    p3 <- p3 + facet_wrap(as.formula(paste("~", cat2))) +
      labs(subtitle = paste("Faceted by", cat2))
  }
  ggsave("figs/plot3_scatter_smooth.png", p3, width = 9, height = 5, dpi = 150)
}

#plot 4 prop stacked bars
if (!is.na(cat1) && !is.na(cat2) && cat1 != cat2) {
  p4 <- ggplot(df, aes(x = .data[[cat1]], fill = .data[[cat2]])) +
    geom_bar(position = "fill") +
    scale_y_continuous(labels = percent_format()) +
    labs(title = paste("Composition of", cat1, "by", cat2), x = cat1, y = "Percent", fill = cat2)
  ggsave("figs/plot4_prop_bar.png", p4, width = 8, height = 5, dpi = 150)
}

#plot 5 corr heatmap
if (length(num_cols) >= 2) {
  cor_mat <- cor(select(df, all_of(num_cols)), use = "pairwise.complete.obs")
  cor_long <- as_tibble(cor_mat, rownames = "var1") |>
    pivot_longer(-var1, names_to = "var2", values_to = "cor")
  p5 <- ggplot(cor_long, aes(x = var1, y = var2, fill = cor)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", mid = "orange", high = "red", midpoint = 0) +
    labs(title = "Correlation Heatmap of Numeric Variables", x = NULL, y = NULL, fill = "r") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave("figs/plot5_correlation_heatmap.png", p5, width = 8, height = 7, dpi = 150)
}

#plot 6 density with facets
if (!is.na(num2)) {
  p6 <- ggplot(df, aes(x = .data[[num2]])) +
    geom_density(aes(fill = if (!is.na(cat1)) .data[[cat1]] else NULL), alpha = 0.5, na.rm = TRUE) +
    labs(title = paste0("Density of ", num2, if (!is.na(cat1)) paste0(" by ", cat1) else ""),
         x = num2, y = "Density", fill = if (!is.na(cat1)) cat1 else NULL)
  if (!is.na(cat2) && cat2 != cat1) {
    p6 <- p6 + facet_wrap(as.formula(paste("~", cat2))) +
      labs(subtitle = paste("Faceted by", cat2))
  }
  ggsave("figs/plot6_density_facet.png", p6, width = 9, height = 5, dpi = 150)
}

# ---------- 6) Missingness & simple outlier check ----------
#cat("\n=== MISSINGNESS BY COLUMN ===\n")
#miss_by_col <- df |>
#  summarise(across(everything(), ~sum(is.na(.)))) |>
#  pivot_longer(everything(), names_to = "variable", values_to = "n_miss") |>
#  arrange(desc(n_miss))
#print(miss_by_col)

#cat("\n=== SIMPLE OUTLIER CHECK (IQR) FOR num1 ===\n")
#if (!is.na(num1)) {
#  q <- quantile(df[[num1]], probs = c(0.25, 0.75), na.rm = TRUE)
#  iqr <- q[2] - q[1]
#  lower <- q[1] - 1.5 * iqr
#  upper <- q[2] + 1.5 * iqr
#  n_out <- sum(df[[num1]] < lower | df[[num1]] > upper, na.rm = TRUE)
#  sample_out <- df |>
#    filter(.data[[num1]] < lower | .data[[num1]] > upper) |>
#    select(any_of(c(num1, cat1, cat2))) |>
#    head(10)
#  print(list(cuts = c(lower = lower, upper = upper),
#             n_outliers = n_out, sample = sample_out))
#} else cat("num1 not available for outlier check.\n")
#
#cat("\nDone. Plots saved in ./figs/\n")