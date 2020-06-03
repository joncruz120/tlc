library(tidyverse)
library(ggrepel)
library(magrittr)
library(scales)
library(labeling)
library(readxl)
library(shiny)
library(devtools)
library(gganimate)
library(moderndive)
library(na.tools)
library(ggimage)
library(janitor)
library(forcats)
library(skimr)
library(ggimage)
library(ggthemes)
library(tidytext)

# read in chart
weekly_chart <- read_csv("weekly_inventory.csv", na = c("0", "N/A"))

# change column types
weekly_chart$week_of <- as.Date(weekly_chart$week_of, format = "%m/%d/%Y")
weekly_chart$site <- as.factor(weekly_chart$site) 
weekly_chart$supply_category <- as.factor(weekly_chart$supply_category) 

# week 1
april_4 <- weekly_chart %>%
  filter(week_of == "2020-04-04" & !is.na(utilization_rate)) %>%
  ggplot(aes(reorder(item, -utilization_rate),
             utilization_rate,
             fill = item)) +geom_text(aes(label = comma(distributed), vjust= -0.25)) + geom_bar(stat = "identity") + scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.ticks = element_blank(), legend.position = "none", plot.title = element_text(hjust = 0.5)) +labs(
          x = "Items",
          y = "Utilization",
          fill = "Key",
          title = "Week ending: April 4",
          caption = "TLC Operations",
          subtitle = "Note: Labels represent quantity distrubted that week. For gloves, however, it is cases distributed." 
        )
    
show(april_4)

# week 2
april_11 <- weekly_chart %>%
  filter(week_of == "2020-04-11" & !is.na(utilization_rate)) %>%
  ggplot(aes(reorder(item, -utilization_rate),
             utilization_rate,
             fill = item)) +geom_text(aes(label = comma(distributed), vjust= -0.25)) + geom_bar(stat = "identity") + scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.ticks = element_blank(), legend.position = "none", plot.title = element_text(hjust = 0.5)) +labs(
    x = "Items",
    y = "Utilization",
    fill = "Key",
    title = "Week ending: April 11",
    caption = "TLC Operations",
    subtitle = "Note: Labels represent quantity distrubted that week. For gloves, however, it is cases distributed." 
  )

show(april_11)

# week 3
april_18 <- weekly_chart %>%
  filter(week_of == "2020-04-18" & !is.na(utilization_rate)) %>%
  ggplot(aes(reorder(item, -utilization_rate),
             utilization_rate,
             fill = item)) +geom_text(aes(label = comma(distributed, accuracy =1), vjust= -0.25)) + geom_bar(stat = "identity") + scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.ticks = element_blank(), legend.position = "none", plot.title = element_text(hjust = 0.5)) +labs(
    x = "Items",
    y = "Utilization",
    fill = "Key",
    title = "Week ending: April 18",
    caption = "TLC Operations",
    subtitle = "Note: Labels represent quantity distrubted that week. For gloves, however, it is cases distributed." 
  )

show(april_18)

# week 4
april_25 <- weekly_chart %>%
  filter(week_of == "2020-04-25" & !is.na(utilization_rate)) %>%
  ggplot(aes(reorder(item, -utilization_rate),
             utilization_rate,
             fill = item)) +geom_text(aes(label = comma(distributed), vjust= -0.25)) + geom_bar(stat = "identity") + scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.ticks = element_blank(), legend.position = "none", plot.title = element_text(hjust = 0.5)) +labs(
    x = "Items",
    y = "Utilization",
    fill = "Key",
    title = "Week ending: April 25",
    caption = "TLC Operations",
    subtitle = "Note: Labels represent quantity distrubted that week. For gloves, however, it is cases distributed." 
  )

show(april_25)

# week 5

may_2 <- weekly_chart %>%
  filter(week_of == "2020-05-02" & !is.na(utilization_rate)) %>%
  ggplot(aes(reorder(item, -utilization_rate),
             utilization_rate,
             fill = item)) +geom_text(aes(label = comma(distributed), vjust= -0.25)) + geom_bar(stat = "identity") + scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.ticks = element_blank(), legend.position = "none", plot.title = element_text(hjust = 0.5)) +labs(
    x = "Items",
    y = "Utilization",
    fill = "Key",
    title = "Week ending: May 2",
    caption = "TLC Operations",
    subtitle = "Note: Labels represent quantity distrubted that week. For gloves, however, it is cases distributed." 
  )

show(may_2)

# week 6

may_9 <- weekly_chart %>%
  filter(week_of == "2020-05-09" & !is.na(utilization_rate)) %>%
  ggplot(aes(reorder(item, -utilization_rate),
             utilization_rate,
             fill = item)) +geom_text(aes(label = comma(distributed, accuracy = 1), vjust= -0.25)) + geom_bar(stat = "identity") + scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.ticks = element_blank(), legend.position = "none", plot.title = element_text(hjust = 0.5)) +labs(
    x = "Items",
    y = "Utilization",
    fill = "Key",
    title = "Week ending: May 9",
    caption = "TLC Operations",
    subtitle = "Note: Labels represent quantity distrubted that week. For gloves, however, it is cases distributed." 
  )

show(may_9)

# week 7

may_16 <- weekly_chart %>%
  filter(week_of == "2020-05-16" & !is.na(utilization_rate)) %>%
  ggplot(aes(reorder(item, -utilization_rate),
             utilization_rate,
             fill = item)) +geom_text(aes(label = comma(distributed), vjust= -0.25)) + geom_bar(stat = "identity") + scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.ticks = element_blank(), legend.position = "none", plot.title = element_text(hjust = 0.5)) +labs(
    x = "Items",
    y = "Utilization",
    fill = "Key",
    title = "Week ending: May 16",
    caption = "TLC Operations",
    subtitle = "Note:  TLC received 50,000 cloth masks on May 7."
  )

show(may_16)

# week 8

may_23 <- weekly_chart %>%
  filter(week_of == "2020-05-23" & !is.na(utilization_rate)) %>%
  ggplot(aes(reorder(item, -utilization_rate),
             utilization_rate,
             fill = item)) +geom_text(aes(label = comma(distributed, accuracy = 1), vjust= -0.25)) + geom_bar(stat = "identity") + scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.ticks = element_blank(), legend.position = "none", plot.title = element_text(hjust = 0.5)) +labs(
    x = "Items",
    y = "Utilization",
    fill = "Key",
    title = "Week ending: May 23",
    caption = "TLC Operations",
    subtitle = "Note: Labels represent quantity distrubted that week. For gloves, however, it is cases distributed." 
  )

show(may_23)

# week 9

may_30 <- weekly_chart %>%
  filter(week_of == "2020-05-30" & !is.na(utilization_rate)) %>%
  ggplot(aes(reorder(item, -utilization_rate),
             utilization_rate,
             fill = item)) +geom_text(aes(label = comma(distributed), vjust= -0.25)) + geom_bar(stat = "identity") + scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.ticks = element_blank(), legend.position = "none", plot.title = element_text(hjust = 0.5)) +labs(
    x = "Items",
    y = "Utilization",
    fill = "Key",
    title = "Week ending: May 30",
    caption = "TLC Operations",
    subtitle = "Note: Labels represent quantity distrubted that week. For gloves, however, it is cases distributed." 
  )

show(may_30)

# attach months to dates in new column and then organize columns
monthly_chart <- weekly_chart %>%
  mutate(month = lubridate::month(week_of, label = TRUE, abbr = FALSE)) %>%
  select(week_of, month, everything())
  
april_facet_titles <- c(
    "2020-04-04" = "Week ending: April 4",
    "2020-04-11" = "Week ending: April 11",
    "2020-04-18" = "Week ending: April 18",
    "2020-04-25" = "Week ending: April 25"
  )

april <- monthly_chart %>%
  filter(month == "April" & !is.na(utilization_rate)) %>%
  ggplot(aes(
    reorder_within(item,-utilization_rate, week_of),
    utilization_rate,
    fill = item
  )) +  scale_x_reordered() +
  geom_bar(stat = "identity") +
  facet_wrap(~ week_of,
             scales = "free",
             labeller = as_labeller(april_facet_titles)) + scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1)) + labs(x = "Items",
                                                                  y = "Utilization",
                                                                  fill = "Key",
                                                                  title = "April 2020",
                                                                  caption = "TLC Operations") +
  geom_text(aes(
    label = item,
    vjust = -0.5
  ), size = 2.5, vjust="inward", angle = 45) +  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.background = element_rect(colour = "black", fill = "white"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    legend.position = "none", plot.title = element_text(hjust = 0.5))


show(april)
may_facet_titles <- c(
  "2020-05-02" = "Week ending: May 2",
  "2020-05-09" = "Week ending: May 9",
  "2020-05-16" = "Week ending: May 16",
  "2020-05-23" = "Week ending: May 23",
  "2020-05-30" = "Week ending: May 30"
)

may <- monthly_chart %>%
  filter(month == "May" & !is.na(utilization_rate)) %>%
  ggplot(aes(
    reorder_within(item,-utilization_rate, week_of),
    utilization_rate,
    fill = item
  )) +  scale_x_reordered() +
  geom_bar(stat = "identity") +
  facet_wrap(~ week_of,
             scales = "free",
             labeller = as_labeller(may_facet_titles)) + scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1)) + labs(x = "Items",
                                                                  y = "Utilization",
                                                                  fill = "Key",
                                                                  title = "May 2020",
                                                                  caption = "TLC Operations") +
  geom_text(aes(
    label = item,
    vjust = -0.5
  ), size = 2.5, vjust="inward", angle = 45) +  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.background = element_rect(colour = "black", fill = "white"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    legend.position = "none", plot.title = element_text(hjust = 0.5))

show(may)
