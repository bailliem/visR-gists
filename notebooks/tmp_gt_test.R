library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(gt)

iris %>%
  group_by(Species) %>%
  nest() %>%
  # calculate a column of global min and max - needed for setting plot limits
  mutate(
    Sepal.Length.Min = min(unlist(map(data, ~.$Sepal.Length))),
    Sepal.Length.Max = max(unlist(map(data, ~.$Sepal.Length))),
    Sepal.Length.Mean = mean(unlist(map(data, ~.$Sepal.Length)))
  ) %>%
  # build row plots
  mutate(`min - mean - max` = pmap(
    list(data, Sepal.Length.Min, Sepal.Length.Max, Sepal.Length.Mean), 
    ~ggplot(..1) + 
      geom_vline(
        xintercept = ..4, 
        size = 10,
        color = "blue") + 
      geom_errorbarh(
        mapping = aes(
          xmin = min(..1$Sepal.Length), 
          xmax = max(..1$Sepal.Length),
          y = 0),
        size = 10) + 
      geom_point(
        mapping = aes(
          x = mean(..1$Sepal.Length),
          y = 0),
        size = 50) + 
      scale_x_continuous(limits = c(..2, ..3)) + 
      theme_void() + 
      theme(
        plot.background = element_blank(),
        panel.background = element_blank())
  )) %>%
  select(Species, `min - mean - max`) %>%
  gt() %>%
  # make plots elongated - getting errors when trying really high aspect ratios
  # Error: Dimensions exceed 50 inches (height and width are specified in
  #    'in' not pixels). If you're sure you want a plot that big, use
  #    `limitsize = FALSE`.
  fmt_ggplot(
    columns = vars(`min - mean - max`),
    aspect_ratio = 5,
    height = 20) %>%
  tab_spanner(
    label = "Sepal.Length",
    columns = vars("min - mean - max")) %>%
  # make all rows have a white background so plots with white background don't
  # stand out
  tab_style(
    style = cells_styles(bkgd_color = rgb(1, 1, 1)),
    locations = cells_data(rows = TRUE))





##########
gtcars_plot_column <-
  gtcars %>%
  dplyr::group_by(mfr) %>%
  tidyr::nest(.key = plot) %>%
  dplyr::mutate(
    plot = map(plot, ~ggplot(., aes(hp, trq)) + geom_point())) %>%
  head(3)

tab_1 <- 
  gtcars_plot_column %>%
  gt()

tab_2 <- 
  gtcars_plot_column %>%
  gt() %>%
  fmt_ggplot(
    columns = vars(plot),
    height = 200,
    aspect_ratio = 2.5
  )