## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, warning = FALSE, message = FALSE----------------------------------
library(dabestr)

## -----------------------------------------------------------------------------
set.seed(12345) # Fix the seed so the results are reproducible.
N <- 20 # The number of samples taken from each population

# Create samples
c1 <- rnorm(N, mean = 3, sd = 0.4)
c2 <- rnorm(N, mean = 3.5, sd = 0.75)
c3 <- rnorm(N, mean = 3.25, sd = 0.4)

t1 <- rnorm(N, mean = 3.5, sd = 0.5)
t2 <- rnorm(N, mean = 2.5, sd = 0.6)
t3 <- rnorm(N, mean = 3, sd = 0.75)
t4 <- rnorm(N, mean = 3.5, sd = 0.75)
t5 <- rnorm(N, mean = 3.25, sd = 0.4)
t6 <- rnorm(N, mean = 3.25, sd = 0.4)

# Add a `gender` column for coloring the data.
gender <- c(rep("Male", N / 2), rep("Female", N / 2))

# Add an `id` column for paired data plotting.
id <- 1:N

# Combine samples and gender into a DataFrame.
df <- tibble::tibble(
  `Control 1` = c1, `Control 2` = c2, `Control 3` = c3,
  `Test 1` = t1, `Test 2` = t2, `Test 3` = t3, `Test 4` = t4, `Test 5` = t5, `Test 6` = t6,
  Gender = gender, ID = id
)

df <- df %>%
  tidyr::gather(key = Group, value = Measurement, -ID, -Gender)

## ---- warning = FALSE---------------------------------------------------------
two_groups_paired_sequential <- load(df,
  x = Group, y = Measurement,
  idx = c("Control 1", "Test 1"),
  paired = "sequential", id_col = ID
)

print(two_groups_paired_sequential)

## ---- warning = FALSE---------------------------------------------------------
two_groups_paired_baseline <- load(df,
  x = Group, y = Measurement,
  idx = c("Control 1", "Test 1"),
  paired = "baseline", id_col = ID
)

print(two_groups_paired_baseline)

## -----------------------------------------------------------------------------
two_groups_paired_sequential.mean_diff <- mean_diff(two_groups_paired_sequential)
two_groups_paired_baseline.mean_diff <- mean_diff(two_groups_paired_baseline)

## -----------------------------------------------------------------------------
print(two_groups_paired_sequential.mean_diff)

## -----------------------------------------------------------------------------
print(two_groups_paired_baseline.mean_diff)

## -----------------------------------------------------------------------------
dabest_plot(two_groups_paired_sequential.mean_diff,
  raw_marker_size = 0.5, raw_marker_alpha = 0.3
)

## ---- warning = FALSE, eval = FALSE-------------------------------------------
#  dabest_plot(two_groups_paired_sequential.mean_diff,
#    float_contrast = FALSE,
#    raw_marker_size = 0.5, raw_marker_alpha = 0.3,
#    contrast_ylim = c(-0.3, 1.3)
#  )

## ---- warning = FALSE, echo = FALSE-------------------------------------------
pp_plot <- dabest_plot(two_groups_paired_sequential.mean_diff,
  float_contrast = FALSE,
  raw_marker_size = 0.5, raw_marker_alpha = 0.3,
  contrast_ylim = c(-0.3, 1.3)
)

cowplot::plot_grid(
  plotlist = list(NULL, pp_plot, NULL),
  nrow = 1,
  ncol = 3,
  rel_widths = c(2.5, 5, 2.5)
)

## ---- warning = FALSE---------------------------------------------------------
dabest_plot(two_groups_paired_baseline.mean_diff,
  raw_marker_size = 0.5, raw_marker_alpha = 0.3
)

## ---- warning = FALSE, eval = FALSE-------------------------------------------
#  dabest_plot(two_groups_paired_baseline.mean_diff,
#    float_contrast = FALSE,
#    raw_marker_size = 0.5, raw_marker_alpha = 0.3,
#    contrast_ylim = c(-0.3, 1.3)
#  )

## ---- warning = FALSE, echo = FALSE-------------------------------------------
pp_plot <- dabest_plot(two_groups_paired_baseline.mean_diff,
  float_contrast = FALSE,
  raw_marker_size = 0.5, raw_marker_alpha = 0.3,
  contrast_ylim = c(-0.3, 1.3)
)

cowplot::plot_grid(
  plotlist = list(NULL, pp_plot, NULL),
  nrow = 1,
  ncol = 3,
  rel_widths = c(2.5, 5, 2.5)
)

## ---- warning = FALSE---------------------------------------------------------
sequential_repeated_measures.mean_diff <- load(df,
  x = Group, y = Measurement,
  idx = c(
    "Control 1", "Test 1",
    "Test 2", "Test 3"
  ),
  paired = "sequential", id_col = ID
) %>%
  mean_diff()

print(sequential_repeated_measures.mean_diff)

## ---- warning = FALSE---------------------------------------------------------
dabest_plot(sequential_repeated_measures.mean_diff,
  raw_marker_size = 0.5, raw_marker_alpha = 0.3
)

## ---- warning = FALSE---------------------------------------------------------
baseline_repeated_measures.mean_diff <- load(df,
  x = Group, y = Measurement,
  idx = c(
    "Control 1", "Test 1",
    "Test 2", "Test 3"
  ),
  paired = "baseline", id_col = ID
) %>%
  mean_diff()

print(baseline_repeated_measures.mean_diff)

## ---- warning = FALSE---------------------------------------------------------
dabest_plot(baseline_repeated_measures.mean_diff,
  raw_marker_size = 0.5, raw_marker_alpha = 0.3
)

## ---- warning = FALSE---------------------------------------------------------
multi_baseline_repeated_measures.mean_diff <- load(df,
  x = Group, y = Measurement,
  idx = list(
    c(
      "Control 1", "Test 1",
      "Test 2", "Test 3"
    ),
    c(
      "Control 2", "Test 4",
      "Test 5", "Test 6"
    )
  ),
  paired = "baseline", id_col = ID
) %>%
  mean_diff()

dabest_plot(multi_baseline_repeated_measures.mean_diff,
  raw_marker_size = 0.5, raw_marker_alpha = 0.3
)

