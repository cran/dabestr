## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- include = FALSE, warning = FALSE, message = FALSE-----------------------
library(dabestr)

## ---- echo = FALSE, warning = FALSE, message = FALSE--------------------------
df <- data.frame(
  `s` = c("Drug", "Placebo"),
  `Wild type` = c("$X_D, W$", "$X_P, W$"),
  `Mutant` = c("$X_D, M$", "$X_P, M$")
)
colnames(df) <- c(" ", "Wild type", "Mutant")
knitr::kable(df, escape = FALSE) %>%
  kableExtra::column_spec(1, bold = TRUE) %>%
  kableExtra::column_spec(1:2, border_right = TRUE)

## ----setup, eval = FALSE------------------------------------------------------
#  library(dabestr)

## -----------------------------------------------------------------------------
set.seed(12345) # Fix the seed so the results are replicable.
# pop_size = 10000 # Size of each population.
N <- 20 # The number of samples taken from each population

# Create samples
placebo <- rnorm(N / 2, mean = 4, sd = 0.4)
placebo <- c(placebo, rnorm(N / 2, mean = 2.8, sd = 0.4))
drug <- rnorm(N / 2, mean = 3, sd = 0.4)
drug <- c(drug, rnorm(N / 2, mean = 2.5, sd = 0.4))

# Add a `Genotype` column as the second variable
genotype <- c(rep("M", N / 2), rep("W", N / 2))

# Add an `id` column for paired data plotting.
id <- 1:N

# Add a `Rep` column as the first variable for the 2 replicates of experiments done
Rep <- rep(c("Rep1", "Rep2"), N / 2)

# Combine all columns into a DataFrame.
df <- tibble::tibble(
  Placebo = placebo,
  Drug = drug,
  Genotype = genotype,
  ID = id,
  Rep = Rep
)

df <- df %>%
  tidyr::gather(key = Treatment, value = Measurement, -ID, -Genotype, -Rep)

## -----------------------------------------------------------------------------
knitr::kable(head(df))

## ---- eval = FALSE------------------------------------------------------------
#  unpaired_delta2 <- load(df,
#    x = Genotype, y = Measurement,
#    experiment = Treatment, colour = Genotype,
#    delta2 = TRUE
#  )

## ---- echo = FALSE------------------------------------------------------------
unpaired_delta2 <- load(df,
  x = Genotype, y = Measurement,
  experiment = Treatment, colour = Genotype,
  delta2 = TRUE,
  experiment_label = c("Placebo", "Drug"),
  x1_level = c("W", "M")
)

## -----------------------------------------------------------------------------
print(unpaired_delta2)

## -----------------------------------------------------------------------------
unpaired_delta2.mean_diff <- mean_diff(unpaired_delta2)

print(unpaired_delta2.mean_diff)

## -----------------------------------------------------------------------------
dabest_plot(unpaired_delta2.mean_diff)

## -----------------------------------------------------------------------------
unpaired_delta2_specified.mean_diff <- load(df,
  x = Genotype, y = Measurement,
  experiment = Treatment, colour = Genotype,
  delta2 = TRUE,
  experiment_label = c("Drug", "Placebo"),
  x1_level = c("M", "W")
) %>%
  mean_diff()

dabest_plot(unpaired_delta2_specified.mean_diff)

## -----------------------------------------------------------------------------
paired_delta2.mean_diff <- load(df,
  x = Treatment, y = Measurement,
  experiment = Genotype, colour = Rep,
  delta2 = TRUE,
  idx = list(
    c("Placebo W", "Drug W"),
    c("Placebo M", "Drug M")
  ),
  paired = "baseline", id_col = ID
) %>%
  mean_diff()

dabest_plot(paired_delta2.mean_diff,
  raw_marker_size = 0.5, raw_marker_alpha = 0.3
)

## -----------------------------------------------------------------------------
dabest_plot(unpaired_delta2.mean_diff, show_delta2 = FALSE)

## -----------------------------------------------------------------------------
# cohens_d(unpaired_delta2)

## -----------------------------------------------------------------------------
print(unpaired_delta2.mean_diff$boot_result)

## -----------------------------------------------------------------------------
# print(unpaired_delta2.mean_diff$permtest_pvals$permutation_test_results)
print(unpaired_delta2.mean_diff$permtest_pvals$pval_permtest)

## -----------------------------------------------------------------------------
print(unpaired_delta2.mean_diff$permtest_pvals$pval_for_tests)

## -----------------------------------------------------------------------------
print(unpaired_delta2.mean_diff$permtest_pvals$pvalues)

