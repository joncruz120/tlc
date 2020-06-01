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
    vjust = 1.5
  ), size = 2.5, vjust="inward", angle = 45) +  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.background = element_rect(colour = "black", fill = "white"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    legend.position = "none", plot.title = element_text(hjust = 0.5)
    
  )

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
