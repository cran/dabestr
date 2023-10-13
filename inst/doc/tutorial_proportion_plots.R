## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, warning = FALSE, message = FALSE----------------------------------
library(dabestr)

## -----------------------------------------------------------------------------
set.seed(12345) # Fix the seed so the results are replicable.
N <- 40 # The number of samples taken from each population

# Create samples
size <- 1
c1 <- rbinom(N, size, prob = 0.2)
c2 <- rbinom(N, size, prob = 0.2)
c3 <- rbinom(N, size, prob = 0.8)

t1 <- rbinom(N, size, prob = 0.35)
t2 <- rbinom(N, size, prob = 0.2)
t3 <- rbinom(N, size, prob = 0.3)
t4 <- rbinom(N, size, prob = 0.4)
t5 <- rbinom(N, size, prob = 0.5)
t6 <- rbinom(N, size, prob = 0.6)
t7 <- c(rep(1, N))

# Add a `gender` column for coloring the data.
gender <- c(rep("Male", N / 2), rep("Female", N / 2))

# Add an `id` column for paired data plotting.
id <- 1:N

# Combine samples and gender into a DataFrame.
df <- tibble::tibble(
  `Control 1` = c1, `Control 2` = c2, `Control 3` = c3,
  `Test 1` = t1, `Test 2` = t2, `Test 3` = t3, `Test 4` = t4, `Test 5` = t5,
  `Test 6` = t6, `Test 7` = t7,
  Gender = gender, ID = id
)

df <- df %>%
  tidyr::gather(key = Group, value = Success, -ID, -Gender)

## -----------------------------------------------------------------------------
knitr::kable(head(df))

## -----------------------------------------------------------------------------
two_groups_unpaired <- load(df,
  x = Group, y = Success,
  idx = c("Control 1", "Test 1"),
  proportional = TRUE
)

print(two_groups_unpaired)

## -----------------------------------------------------------------------------
two_groups_unpaired.mean_diff <- mean_diff(two_groups_unpaired)

print(two_groups_unpaired.mean_diff)

## -----------------------------------------------------------------------------
two_groups_unpaired.cohens_h <- cohens_h(two_groups_unpaired)

print(two_groups_unpaired.cohens_h)

## -----------------------------------------------------------------------------
dabest_plot(two_groups_unpaired.mean_diff)
dabest_plot(two_groups_unpaired.cohens_h)

## ---- eval = FALSE------------------------------------------------------------
#  dabest_plot(two_groups_unpaired.mean_diff, float_contrast = FALSE)

## ---- echo = FALSE------------------------------------------------------------
pp_plot <- dabest_plot(two_groups_unpaired.mean_diff,
  float_contrast = FALSE,
  swarm_y_text = 11, contrast_y_text = 11
)

cowplot::plot_grid(
  plotlist = list(NULL, pp_plot, NULL),
  nrow = 1,
  ncol = 3,
  rel_widths = c(2.5, 5, 2.5)
)

## -----------------------------------------------------------------------------
dabest_plot(two_groups_unpaired.mean_diff, raw_bar_width = 0.15)

## -----------------------------------------------------------------------------
dabest_plot(two_groups_unpaired.mean_diff,
  swarm_label = "success", contrast_label = "difference"
)

## -----------------------------------------------------------------------------
two_groups_baseline.mean_diff <- load(df,
  x = Group, y = Success,
  idx = c("Control 1", "Test 1"),
  proportional = TRUE,
  paired = "baseline", id_col = ID
) %>%
  mean_diff()

dabest_plot(two_groups_baseline.mean_diff)

## ---- eval = FALSE------------------------------------------------------------
#  dabest_plot(two_groups_baseline.mean_diff, float_contrast = FALSE)

## ---- echo = FALSE------------------------------------------------------------
pp_plot <- dabest_plot(two_groups_baseline.mean_diff,
  float_contrast = FALSE,
  swarm_y_text = 11, contrast_y_text = 11,
  raw_bar_width = 0.2
)

cowplot::plot_grid(
  plotlist = list(NULL, pp_plot, NULL),
  nrow = 1,
  ncol = 3,
  rel_widths = c(2.5, 5, 2.5)
)

## -----------------------------------------------------------------------------
multi_group_baseline.mean_diff <- load(df,
  x = Group, y = Success,
  idx = list(
    c(
      "Control 1", "Test 1",
      "Test 2", "Test 3"
    ),
    c(
      "Test 4", "Test 5",
      "Test 6"
    )
  ),
  proportional = TRUE,
  paired = "baseline", id_col = ID
) %>%
  mean_diff()

dabest_plot(multi_group_baseline.mean_diff,
  swarm_y_text = 11, contrast_y_text = 11
)

## -----------------------------------------------------------------------------
multi_group_sequential.mean_diff <- load(df,
  x = Group, y = Success,
  idx = list(
    c(
      "Control 1", "Test 1",
      "Test 2", "Test 3"
    ),
    c(
      "Test 4", "Test 5",
      "Test 6"
    )
  ),
  proportional = TRUE,
  paired = "sequential", id_col = ID
) %>%
  mean_diff()

dabest_plot(multi_group_sequential.mean_diff,
  swarm_y_text = 11, contrast_y_text = 11
)

## -----------------------------------------------------------------------------
multi_group_baseline_specify.mean_diff <- load(df,
  x = Group, y = Success,
  idx = c(
    "Control 1", "Test 1",
    "Test 2", "Test 3",
    "Test 4", "Test 5",
    "Test 6"
  ),
  proportional = TRUE,
  paired = "baseline", id_col = ID
) %>%
  mean_diff()

dabest_plot(multi_group_baseline_specify.mean_diff,
  swarm_y_text = 11, contrast_y_text = 11
)

## -----------------------------------------------------------------------------
separate_control.mean_diff <- load(df,
  x = Group, y = Success,
  idx = list(
    c("Control 1", "Test 1"),
    c("Test 2", "Test 3"),
    c("Test 4", "Test 5", "Test 6")
  ),
  proportional = TRUE,
  paired = "sequential", id_col = ID
) %>%
  mean_diff()

dabest_plot(separate_control.mean_diff, swarm_y_text = 11, contrast_y_text = 11)
dabest_plot(separate_control.mean_diff,
  swarm_y_text = 11, contrast_y_text = 11,
  sankey = FALSE
)
dabest_plot(separate_control.mean_diff,
  swarm_y_text = 11, contrast_y_text = 11,
  flow = FALSE
)

