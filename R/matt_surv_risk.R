

#----------------------------------------------------------
# Create the counts table below
#----------------------------------------------------------
#' Table of no at risk to insert below KM plot
#'
#' @param fit survit output
#' @param times numeric vector of times to show at risk counts for
#' @param colours options vector of colours
#' @param range_x range of x axis to align with KM plot
#'
#' @return plot object
#' @export
#'
#' @examples km_times (fit, times = c(0,3,6,9,12,15))
#'
km_plot_times <- function (fit, times = c(0,3,6,9,12,15),
                           colours = scales::hue_pal()(2),
                           range_x,
                           trt_order,
                           size = 's') {
  
  if (size == 's') base_size = 6.6
  if (size == 'm') base_size = 8.6
  if (size == 'l') base_size = 12.6
  text_size <-  (5/14) * base_size
  
  #browser()
  sumfit <- summary(fit, times=times)
  n_risk <- data_frame('Arm' = sumfit$strata,
                       'Time' =  sumfit$time,
                       'Nrisk' =   sumfit$n.risk)
  
  labels <- data_frame ('Arm' = "Time",
                        'Time' =  sumfit$time,
                        'Nrisk' =   sumfit$time)
  
  spacer <-  data_frame ('Arm' = " ",
                         'Time' =  NA,
                         'Nrisk' =   NA)
  
  n_risk <- bind_rows (n_risk, labels, spacer) %>%
    mutate (Arm = gsub("TRT01P=","", Arm)) %>%
    mutate (Arm = factor (Arm, levels = c(" ", "Time", trt_order)))
  
  tbl <-
    ggplot(n_risk, aes(x = Time, y = Arm, colour = Arm, label=Nrisk)) +
    geom_text(size=4.5) +
    theme_bw(base_size=12.6) +
    scale_y_discrete(name="", limits = rev(levels(n_risk$Arm))) +
    scale_colour_manual (values = c('black', 'black', colours[[1]], colours[[2]])) +
    theme(
      legend.position = "none",
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks=element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_blank()
    )
  
  label_df <- data_frame (x = 2, y = 4, label = "No. at risk")
  
  # at at risk line
  tbl2 <-
    tbl +
    #scale_y_discrete(breaks=c("Group.B","Group.A"), labels=c("Group B", "Group A")) +
    geom_hline (aes (yintercept = 3.6))  +
    #geom_text (data=NULL, aes(x =2, y=4.0, label  = "No. at risk"), size=4.5, hjust=1, family = 'sans', fontface = 'plain') +
    geom_text (data=label_df, aes(x =x, y=y, label  = label, colour=NULL), size=base_size, hjust=1, family = 'sans', fontface = 'plain') +
    theme(aspect.ratio=1/9) +
    scale_x_continuous (limits = range_x) +
    theme(axis.text=element_text(size= base_size),axis.title=element_text(size= base_size))
  
  gt2 <- ggplotGrob(tbl2)
  gt2$layout$clip[gt2$layout$name == "panel"] <- "off"
  
  # grid.newpage()
  # grid.draw (gt2)
  
  return (tbl2)
}


fit <- survfit(Surv(time, status) ~ sex, data = lung)

km_plot_times (fit, times = c(0,3,6,9,12,15),
                           colours = scales::hue_pal()(2),
                           range_x,
                           trt_order,
                           size = 's')
