library(installr)
library(tidyverse)
library(lubridate)

#### Used as.Date for R to recognize as dates in normal R format
dates <-
  as.Date(
    c(
      "3/22/2020",
      "3/25/2020",
      "3/25/2020",
      "3/18/2020",
      "3/22/2020",
      "3/29/2020",
      "3/26/2020",
      "3/27/2020",
      "3/26/2020",
      "4/6/2020",
      "4/6/2020",
      "4/4/2020",
      "4/9/2020",
      "4/6/2020",
      "4/6/2020",
      "4/2/2020",
      "4/6/2020",
      "4/9/2020",
      "4/17/2020",
      "4/12/2020",
      "4/3/2020",
      "4/15/2020",
      "4/20/2020",
      "4/28/2020",
      "4/5/2020"
    ),
    "%m/%d/%Y"
  )

##### Prepared dates as data.frame to tidy data
dates_df <- data.frame(dates)

#### Arranged dates in chronoglogical order and counted distinct dates
dates_df_ordered <- dates_df %>%
  arrange(dates) %>%
  count(dates)


plot <-
  ggplot(dates_df_ordered,
         
         #### PLotted dates as factor to show ALL dates to avoid missing dates
         #### y-axis is count of each reported case
         aes(x = factor(dates), y = n)) + geom_col(fill = "navy blue") + theme_bw() +
  
  #### Labeling for x and y axis, caption, main title and subtitle
  labs(
    x = "Date Employee Entered Quarantine",
    y = "Count per Day",
    caption = "TLC Operations",
    title = "TLC Employee Confirmed Cases",
    subtitle = "Total Cases: 25"
  )

#### Added theme to angle x-axis labels diagonally
#### use 'hjust = 1' to move x-axis labels below grid line (better visually)
plot_and_theme <- plot + theme(axis.text.x = element_text(angle = 40, hjust = 1))

#### Add labels to over each column
plot_column_labels <- plot_and_theme + geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25)