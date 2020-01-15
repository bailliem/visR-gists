library(data.table)
library(dplyr)

#Download the Austin indicator data set
#Original data set from: https://data.austintexas.gov/City-Government/Imagine-Austin-Indicators/apwj-7zty/data
austinData= data.table::fread('https://raw.githubusercontent.com/lgellis/MiscTutorial/master/Austin/Imagine_Austin_Indicators.csv', data.table=FALSE, header = TRUE, stringsAsFactors = FALSE)
i1 <- austinData %>%
  filter(`Indicator Name` %in% 
           c('Prevalence of Obesity', 'Prevalence of Tobacco Use', 
             'Prevalence of Cardiovascular Disease', 'Prevalence of Diabetes')) %>%
  select(c(`Indicator Name`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`)) %>%
  mutate (Average = round(rowMeans(
    cbind(`2011`, `2012`, `2013`, `2014`, `2015`, `2016`), na.rm=T),2), 
    `Improvement` = round((`2011`-`2016`)/`2011`*100,2))
prevalence = i1

library(formattable)
library(sparkline)
prevalence$`&nbsp` = c(4.1, -.3, .5, 1.4)
prevalence$`2012` = apply(prevalence[, 2:7], 1, FUN = function(x) as.character(htmltools::as.tags(sparkline(as.numeric(x), type = "line"))))
names(prevalence)[3] = "&nbsp&nbsp"
new.prevalance = prevalence[, c(1, 2, 3, 7, 10)]                          
out = as.htmlwidget(formattable(new.prevalance,
                                align = c("l",rep("r", NCOL(prevalence) - 1)), 
                                list(`Indicator Name` = formatter("span", style = ~ style(color = "grey", .weight = "bold")),
                                     "&nbsp" = formatter("span", 
                                                         style = ~ style(color = ifelse(`2016` > `2011`, "green", "red")),                                    
                                                         ~ icontext(sapply(`&nbsp`, function(x) if (x < -1.96) "arrow-down" else if (x> 1.96) "arrow-up" else ""))))))                          
out$dependencies = c(out$dependencies, htmlwidgets:::widget_dependencies("sparkline", "sparkline"))
out