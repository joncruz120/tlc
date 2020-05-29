# Read in data
weekly_chart <- read_csv("weekly_inventory.csv", na = c("N/A", "0"))

# Format column types
weekly_chart$week_of <- as.Date(weekly_chart$week_of, format = "%m/%d/%Y")

weekly_chart$supply_category <- as.factor(weekly_chart$supply_category)

weekly_chart$utilization_rate <- as.numeric(weekly_chart$utilization_rate)

weekly_chart$current_count <- as.integer(weekly_chart$current_count)

weekly_chart$delivered <- as.integer(weekly_chart$delivered)

weekly_chart$distributed <- as.integer(weekly_chart$distributed)

glimpse(weekly_chart)

weekly_chart <- weekly_chart

# Last week in March chart
april_4 <- weekly_chart %>% filter(week_of == "2020-04-04" &
                                     !is.na(utilization_rate)) %>%
  ggplot(aes(
    x = reorder(item, -utilization_rate),
    y = utilization_rate,
    fill = item
  )) + geom_bar(stat = "identity", position = "dodge") + scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1)) + labs(x = "Items", y = "Weekly Utilization Rate", fill = "Key", title = "For the week ending: April 4") +
  geom_text(aes(label = scales::comma(current_count), vjust = -0.5))

show(april_4)

# First week in April

april_11 <- weekly_chart %>% filter(week_of == "2020-04-11" &
                                      !is.na(utilization_rate)) %>%
  ggplot(aes(
    x = reorder(item, -utilization_rate),
    y = utilization_rate,
    fill = item
  )) + geom_bar(stat = "identity", position = "dodge") + scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1)) + labs(x = "Items", y = "Weekly Utilization Rate", fill = "Key",  title = "For the week ending: April 11") +
  geom_text(aes(label = scales::comma(current_count), vjust = -0.5))

show(april_11)

# Second week in April

april_18 <- weekly_chart %>% filter(week_of == "2020-04-18" &
                          !is.na(utilization_rate)) %>%
  ggplot(aes(
    x = reorder(item, -utilization_rate),
    y = utilization_rate,
    fill = item
  )) + geom_bar(stat = "identity", position = "dodge") + scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1)) + labs(x = "Items", y = "Weekly Utilization Rate", fill = "Key", title = "For the week ending: April 18") +
  geom_text(aes(label = scales::comma(current_count, accuracy = 1), vjust = -0.5))

show(april_18)

# Third week in April
april_25 <- weekly_chart %>% filter(week_of == "2020-04-25" &
                                      !is.na(utilization_rate)) %>%
  ggplot(aes(
    x = reorder(item, -utilization_rate),
    y = utilization_rate,
    fill = item
  )) + geom_bar(stat = "identity", position = "dodge") + scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1)) + labs(x = "Items", y = "Weekly Utilization Rate", fill = "Key", title = "For the week ending: April 25") +
  geom_text(aes(label = scales::comma(current_count, accuracy = 1), vjust = -0.5))

show(april_25)

# Last week in April
may_2 <- weekly_chart %>% filter(week_of == "2020-05-02" &
                                      !is.na(utilization_rate)) %>%
  ggplot(aes(
    x = reorder(item, -utilization_rate),
    y = utilization_rate,
    fill = item
  )) + geom_bar(stat = "identity", position = "dodge") + scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1)) + labs(x = "Items", y = "Weekly Utilization Rate", fill = "Key", title = "For the week ending: May 2") +
  geom_text(aes(label = scales::comma(current_count, accuracy = 1), vjust = -0.5))

show(may_2)


# First week in May

may_9 <- weekly_chart %>% filter(week_of == "2020-05-09" &
                                   !is.na(utilization_rate)) %>%
  ggplot(aes(
    x = reorder(item, -utilization_rate),
    y = utilization_rate,
    fill = item
  )) + geom_bar(stat = "identity", position = "dodge") + scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1)) + labs(x = "Items", y = "Weekly Utilization Rate", fill = "Key", title = "For the week ending: May 9") +
  geom_text(aes(label = scales::comma(current_count, accuracy = 1), vjust = -0.5))

show(may_9)

# Second week in May

may_16 <- weekly_chart %>% filter(week_of == "2020-05-16" &
                                   !is.na(utilization_rate)) %>%
  ggplot(aes(
    x = reorder(item, -utilization_rate),
    y = utilization_rate,
    fill = item
  )) + geom_bar(stat = "identity", position = "dodge") + scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1)) + labs(x = "Items", y = "Weekly Utilization Rate", fill = "Key", title = "For the week ending: May 16") +
  geom_text(aes(label = scales::comma(current_count, accuracy = 1), vjust = -0.5))

show(may_16)

# Third week in May 

may_23 <- weekly_chart %>% filter(week_of == "2020-05-23" &
                                    !is.na(utilization_rate)) %>%
  ggplot(aes(
    x = reorder(item, -utilization_rate),
    y = utilization_rate,
    fill = item
  )) + geom_bar(stat = "identity", position = "dodge") + scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1)) + labs(x = "Items", y = "Weekly Utilization Rate", fill = "Key", title = "For the week ending: May 23") +
  geom_text(aes(label = scales::comma(current_count, accuracy = 1), vjust = -0.5))

show(may_23)

# Fourth week in May

# attach months to dates in new column and then organize columns
weekly_chart <- weekly_chart %>%
  mutate(month = lubridate::month(week_of, label = TRUE, abbr= FALSE)) %>%
  select(week_of, month, everything()) %>%
  filter(!is.na(utilization_rate))

base_plot <- weekly_chart %>%
  ggplot(aes(
    x = reorder(item, -utilization_rate),
    y = utilization_rate,
    fill = item
  )) + geom_bar(stat = "identity", position = "dodge") + facet_wrap( ~ week_of, scales = "free_x") + scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) 

show(base_plot)

