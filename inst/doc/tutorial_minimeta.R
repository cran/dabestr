## ---- warning = FALSE, include = FALSE----------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, warning = FALSE, message = FALSE----------------------------------
library(dabestr)

## ---- warning = FALSE---------------------------------------------------------
set.seed(12345) # Fix the seed so the results are reproducible.
# pop_size = 10000 # Size of each population.
N <- 20 # The number of samples taken from each population

# Create samples
c1 <- rnorm(N, mean = 3, sd = 0.4)
c2 <- rnorm(N, mean = 3.5, sd = 0.75)
c3 <- rnorm(N, mean = 3.25, sd = 0.4)

t1 <- rnorm(N, mean = 3.5, sd = 0.5)
t2 <- rnorm(N, mean = 2.5, sd = 0.6)
t3 <- rnorm(N, mean = 3, sd = 0.75)

# Add a `gender` column for coloring the data.
gender <- c(rep("Male", N / 2), rep("Female", N / 2))

# Add an `id` column for paired data plotting.
id <- 1:N

# Combine samples and gender into a DataFrame.
df <- tibble::tibble(
  `Control 1` = c1, `Control 2` = c2, `Control 3` = c3,
  `Test 1` = t1, `Test 2` = t2, `Test 3` = t3,
  Gender = gender, ID = id
)

df <- df %>%
  tidyr::gather(key = Group, value = Measurement, -ID, -Gender)

## -----------------------------------------------------------------------------
knitr::kable(head(df))

## ---- warning = FALSE---------------------------------------------------------
unpaired <- load(df,
  x = Group, y = Measurement,
  idx = list(
    c("Control 1", "Test 1"),
    c("Control 2", "Test 2"),
    c("Control 3", "Test 3")
  ),
  minimeta = TRUE
)

## ---- warning = FALSE---------------------------------------------------------
print(unpaired)

## ---- warning = FALSE---------------------------------------------------------
unpaired.mean_diff <- mean_diff(unpaired)

print(unpaired.mean_diff)

## ---- warning = FALSE---------------------------------------------------------
unpaired.mean_diff$boot_result

## ---- warning = FALSE---------------------------------------------------------
dabest_plot(unpaired.mean_diff)

## ---- warning = FALSE---------------------------------------------------------
dabest_plot(unpaired.mean_diff, show_mini_meta = FALSE)

## ---- warning = FALSE---------------------------------------------------------
paired.mean_diff <- load(df,
  x = Group, y = Measurement,
  idx = list(
    c("Control 1", "Test 1"),
    c("Control 2", "Test 2"),
    c("Control 3", "Test 3")
  ),
  paired = "baseline", id_col = ID,
  minimeta = TRUE
) %>%
  mean_diff()

dabest_plot(paired.mean_diff, raw_marker_size = 0.5, raw_marker_alpha = 0.3)

