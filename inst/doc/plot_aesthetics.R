## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval = FALSE------------------------------------------------------------
#  dabest_plot(
#    dabest_effectsize_obj,
#    float_contrast = TRUE,
#    plot_component = "adjustment_value"
#  )

## ---- include = FALSE---------------------------------------------------------
library(dabestr)

data(non_proportional_data)
data(proportional_data)
data(deltadelta_data)
dabest_twogroup_obj.mean_diff <- load(non_proportional_data, x = Group, y = Measurement, idx = c("Control 1", "Test 1")) %>%
  mean_diff()

dabest_multigroup_obj.mean_diff <- load(non_proportional_data,
  x = Group, y = Measurement,
  idx = list(c("Control 1", "Test 1", "Test 2"), c("Control 2", "Test 3"))
) %>%
  mean_diff()

dabest_unpaired_props.mean_diff <- load(proportional_data,
  x = Group, y = Success,
  idx = list(c("Control 1", "Test 1")),
  proportional = TRUE
) %>%
  mean_diff()

dabest_paired_props.mean_diff <- load(proportional_data,
  x = Group, y = Success,
  idx = list(c("Control 1", "Test 1", "Test 2", "Test 3"), c("Control 2", "Test 4")),
  proportional = TRUE, paired = "sequential",
  id_col = ID
) %>%
  mean_diff()

## -----------------------------------------------------------------------------
dabest_plot(
  dabest_twogroup_obj.mean_diff,
  float_contrast = TRUE,
  swarm_x_text = 30,
  swarm_y_text = 1,
  contrast_x_text = 30,
  contrast_y_text = 5
)

## -----------------------------------------------------------------------------
dabest_plot(
  dabest_twogroup_obj.mean_diff,
  float_contrast = TRUE,
  swarm_label = "I love estimation statistics.",
  contrast_label = "I love it more than you do!"
)

## -----------------------------------------------------------------------------
A <- dabest_plot(dabest_twogroup_obj.mean_diff,
  float_contrast = TRUE,
  swarm_label = "", contrast_label = "",
  raw_marker_size = 1, raw_marker_alpha = 1
)
B <- dabest_plot(dabest_twogroup_obj.mean_diff,
  float_contrast = TRUE,
  swarm_label = "", contrast_label = "",
  raw_marker_size = 2, raw_marker_alpha = 0.5
)

cowplot::plot_grid(
  plotlist = list(A, B),
  nrow = 1,
  ncol = 2,
  labels = "AUTO"
)

## -----------------------------------------------------------------------------
dabest_plot(dabest_multigroup_obj.mean_diff,
  float_contrast = FALSE,
  contrast_label = "More negative is better!",
  swarm_ylim = c(1, 5), contrast_ylim = c(0.7, -1.2)
)

## -----------------------------------------------------------------------------
npg <- dabest_plot(dabest_unpaired_props.mean_diff,
  swarm_label = "", contrast_label = "",
  custom_palette = "npg"
)
nejm <- dabest_plot(dabest_unpaired_props.mean_diff,
  swarm_label = "", contrast_label = "",
  custom_palette = "nejm"
)
jama <- dabest_plot(dabest_unpaired_props.mean_diff,
  swarm_label = "", contrast_label = "",
  custom_palette = "jama"
)
locuszoom <- dabest_plot(dabest_unpaired_props.mean_diff,
  swarm_label = "", contrast_label = "",
  custom_palette = "locuszoom"
)

cowplot::plot_grid(
  plotlist = list(npg, nejm, jama, locuszoom),
  nrow = 2,
  ncol = 2
)

## -----------------------------------------------------------------------------
dabest_plot(dabest_paired_props.mean_diff, sankey = FALSE, raw_bar_width = 0.15)

## -----------------------------------------------------------------------------
dabest_plot(dabest_paired_props.mean_diff, flow = FALSE, raw_bar_width = 0.15)

## -----------------------------------------------------------------------------
right <- dabest_plot(dabest_twogroup_obj.mean_diff,
  float_contrast = FALSE,
  swarm_label = "", contrast_label = "",
  asymmetric_side = "right"
)
left <- dabest_plot(dabest_twogroup_obj.mean_diff,
  float_contrast = FALSE,
  swarm_label = "", contrast_label = "",
  asymmetric_side = "left"
)

cowplot::plot_grid(
  plotlist = list(right, left),
  nrow = 1,
  ncol = 2
)

## -----------------------------------------------------------------------------
dabest_plot(dabest_multigroup_obj.mean_diff,
  float_contrast = FALSE,
  show_baseline_ec = TRUE
)

