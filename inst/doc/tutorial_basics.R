## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, warning = FALSE, message = FALSE----------------------------------
library(dabestr)

## -----------------------------------------------------------------------------
set.seed(12345) # Fix the seed so the results are replicable.
# pop_size = 10000 # Size of each population.
N <- 20

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

## -----------------------------------------------------------------------------
knitr::kable(head(df))

## -----------------------------------------------------------------------------
two_groups_unpaired <- load(df,
  x = Group, y = Measurement,
  idx = c("Control 1", "Test 1")
)

## -----------------------------------------------------------------------------
print(two_groups_unpaired)

## -----------------------------------------------------------------------------
two_groups_unpaired_ci90 <- load(df,
  x = Group, y = Measurement,
  idx = c("Control 1", "Test 1"), ci = 90
)

## -----------------------------------------------------------------------------
print(two_groups_unpaired_ci90)

## -----------------------------------------------------------------------------
two_groups_unpaired.mean_diff <- mean_diff(two_groups_unpaired)

print(two_groups_unpaired.mean_diff)

## -----------------------------------------------------------------------------
dabest_plot(two_groups_unpaired.mean_diff)

## ----eval = FALSE-------------------------------------------------------------
# dabest_plot(two_groups_unpaired.mean_diff,
#   float_contrast = FALSE,
#   contrast_ylim = c(-0.3, 1.3)
# )

## ----echo = FALSE-------------------------------------------------------------
pp_plot <- dabest_plot(two_groups_unpaired.mean_diff,
  float_contrast = FALSE,
  contrast_ylim = c(-0.3, 1.3)
)

cowplot::plot_grid(
  plotlist = list(NULL, pp_plot, NULL),
  nrow = 1,
  ncol = 3,
  rel_widths = c(2.5, 5, 2.5)
)

## ----warning = FALSE----------------------------------------------------------
multi_2group <- load(df,
  x = Group, y = Measurement,
  idx = list(
    c("Control 1", "Test 1"),
    c("Control 2", "Test 2")
  )
)
multi_2group %>%
  mean_diff() %>%
  dabest_plot()

## ----warnings = FALSE---------------------------------------------------------
shared_control <- load(df,
  x = Group, y = Measurement,
  idx = c(
    "Control 1", "Test 1", "Test 2", "Test 3",
    "Test 4", "Test 5", "Test 6"
  )
)

print(shared_control)

## ----warnings = FALSE---------------------------------------------------------
shared_control.mean_diff <- mean_diff(shared_control)

print(shared_control.mean_diff)

## -----------------------------------------------------------------------------
dabest_plot(shared_control.mean_diff)

## ----warnings = FALSE---------------------------------------------------------
multi_groups <- load(df,
  x = Group, y = Measurement,
  idx = list(
    c("Control 1", "Test 1"),
    c("Control 2", "Test 2", "Test 3"),
    c("Control 3", "Test 4", "Test 5", "Test 6")
  )
)

print(multi_groups)

## ----warnings = FALSE---------------------------------------------------------
multi_groups.mean_diff <- mean_diff(multi_groups)

print(multi_groups.mean_diff)

## ----warnings = FALSE---------------------------------------------------------
dabest_plot(multi_groups.mean_diff)

