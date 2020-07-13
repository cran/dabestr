## ----create.data, message=FALSE-----------------------------------------------
library(dplyr)

set.seed(54321)

N = 40
c1 <- rnorm(N, mean = 100, sd = 25)
c2 <- rnorm(N, mean = 100, sd = 50)
g1 <- rnorm(N, mean = 120, sd = 25)
g2 <- rnorm(N, mean = 80, sd = 50)
g3 <- rnorm(N, mean = 100, sd = 12)
g4 <- rnorm(N, mean = 100, sd = 50)
gender <- c(rep('Male', N/2), rep('Female', N/2))
dummy <- rep("Dummy", N)
id <- 1: N


wide.data <- 
  tibble::tibble(
    Control1 = c1, Control2 = c2,
    Group1 = g1, Group2 = g2, Group3 = g3, Group4 = g4,
    Dummy = dummy,
    Gender = gender, ID = id)


my.data   <- 
  wide.data %>%
  tidyr::gather(key = Group, value = Measurement, -ID, -Gender, -Dummy)

head(my.data)

## ----two.group.unpaired-------------------------------------------------------

library(dabestr)

two.group.unpaired <- 
  my.data %>%
  dabest(Group, Measurement, 
         # The idx below passes "Control" as the control group, 
         # and "Group1" as the test group. The mean difference
         # will be computed as mean(Group1) - mean(Control1).
         idx = c("Control1", "Group1"), 
         paired = FALSE)

# Calling the object automatically prints out a summary.
two.group.unpaired 

## ----compute.mean.diff--------------------------------------------------------
two.group.unpaired.meandiff <- mean_diff(two.group.unpaired)

# Calling the above object produces a textual summary of the computed effect size.
two.group.unpaired.meandiff

## ----create.gardner.altman.plot1, fig.width = 7, fig.height = 4---------------
plot(two.group.unpaired.meandiff, color.column = Gender)

## ----create.gardner.altman.plot2, fig.width = 7, fig.height = 4---------------
two.group.unpaired %>% hedges_g() %>% plot(color.column = Gender)

## ----two.group.paired, fig.width = 7, fig.height = 4--------------------------
two.group.paired <- 
  my.data %>%
  dabest(Group, Measurement, 
         idx = c("Control1", "Group1"), 
         paired = TRUE, id.col = ID)


# The summary indicates this is a paired comparison. 
two.group.paired


# Create a paired plot.
two.group.paired %>% 
  mean_diff() %>% 
  plot(color.column = Gender)

## ----multi.two.group.unpaired, fig.width = 7, fig.height = 4------------------

multi.two.group.unpaired <- 
  my.data %>%
  dabest(Group, Measurement, 
         idx = list(c("Control1", "Group1"), 
                    c("Control2", "Group2")),
         paired = FALSE)


# Compute the mean difference.
multi.two.group.unpaired.meandiff <- mean_diff(multi.two.group.unpaired)


# Create a multi-two group plot.
multi.two.group.unpaired.meandiff %>% 
  plot(color.column = Gender)

## ----multi.two.group.unpaired.median.summaries, fig.width = 7, fig.height = 4----
plot(multi.two.group.unpaired.meandiff, 
     color.column = Gender, 
     group.summaries = "median_quartiles")

## ----multi.two.group.paired, fig.width = 7, fig.height = 4--------------------

multi.two.group.paired <- 
  my.data %>%
  dabest(Group, Measurement, 
         idx = list(c("Control1", "Group1"), 
                    c("Control2", "Group2")),
         paired = TRUE, id.col = ID
         )


multi.two.group.paired.mean_diff <- mean_diff(multi.two.group.paired)


plot(multi.two.group.paired.mean_diff, 
     color.column = Gender, 
     slopegraph = TRUE)

## ----shared.control, fig.width = 7, fig.height = 4----------------------------

shared.control <- 
  my.data %>%
  dabest(Group, Measurement, 
         idx = c("Control2", "Group2", "Group4"),
         paired = FALSE
         )

shared.control.mean_diff <- shared.control %>% mean_diff()

plot(shared.control.mean_diff, 
     color.column = Gender,
     rawplot.type = "swarmplot")


## ----multi.group, fig.width = 7, fig.height = 4-------------------------------

multi.group <- 
  my.data %>%
  dabest(Group, Measurement, 
         idx = list(c("Control1", "Group1", "Group3"), 
                    c("Control2", "Group2", "Group4")),
         paired = FALSE
        )

multi.group.mean_diff <- multi.group %>% mean_diff() 

plot(multi.group.mean_diff, color.column = Gender)

## ----ylim.demo, fig.width = 7, fig.height = 4---------------------------------

plot(multi.group.mean_diff, 
     color.column = Gender,
     rawplot.ylim = c(-100, 200),
     effsize.ylim = c(-60, 60)
    )


## ----markersize.groupwidth.demo, fig.width = 7, fig.height = 4----------------

plot(multi.group.mean_diff, 
     color.column = Gender,
     rawplot.markersize = 1,
     rawplot.groupwidth = 0.4
    )

## ----ylabel.demo, fig.width = 7, fig.height = 4-------------------------------

plot(multi.group.mean_diff, 
     color.column = Gender,
     rawplot.ylabel = "Rawplot Title?",
     effsize.ylabel = "My delta plot!"
    )

## ----ylabel.fontsize, fig.width = 7, fig.height = 4---------------------------

plot(multi.group.mean_diff, 
     color.column = Gender,
     axes.title.fontsize = 10 # default is 14.
    )

## ----palette.demo1, fig.width = 7, fig.height = 4-----------------------------
plot(multi.group.mean_diff, 
     color.column = Gender,
     palette = "Dark2" # The default is "Set1".
     )

## ----palette.demo2, fig.width = 7, fig.height = 4-----------------------------
plot(multi.group.mean_diff, 
     color.column = Gender,
     # A custom palette consisting of a vector of colors,
     # specified as RGB hexcode, or as a R named color.
     # See all 657 named R colors with `colors()`.
     palette = c("#FFA500", "sienna4")
     )

## ----different.theme, fig.width = 7, fig.height = 4---------------------------
plot(multi.group.mean_diff, 
     color.column = Gender,
     theme = ggplot2::theme_gray()
     )

