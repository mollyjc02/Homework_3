if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)



# 1. Present a bar graph showing the proportion of states with a change in their cigarette tax in each year from 1970 to 1985. 
### compare by year to identify tax changes for each state 
final.data.q1 <- final.data %>%
  filter(Year <= 1985) %>%
  arrange(state, Year) %>%
  group_by(state) %>%
  mutate(tax_change = if_else(is.na(lag(tax_dollar)) | tax_dollar != lag(tax_dollar), 1, 0)) %>%
  ungroup()

### calculate proportion of states with tax change 
tax_change_proportion <- final.data.q1 %>%
  group_by(Year) %>%
  summarize(proportion_change = mean(tax_change, na.rm = TRUE))

### plot the bar graph
tax_change_plot <- ggplot(tax_change_proportion, aes(x = Year, y = proportion_change)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(
    title = "Proportion of States with a Change in Cigarette Tax (1970-1985)",
    x = "Year",
    y = "Proportion of States with Tax Change"
  ) +
  theme_minimal()

print(tax_change_plot)



# 2. Plot on a single graph the average tax (in 2012 dollars) on cigarettes and the average price of a pack of cigarettes from 1970 to 2018.
### adjust taxes to 2012 values 
final.data <- final.data %>%
  mutate(tax_cpi = tax_dollar * (218 / index))

### reshape the data to long format for easy plotting
final.data.q2 <- final.data %>%
  select(Year, tax_cpi, price_cpi) %>%
  pivot_longer(cols = c(tax_cpi, price_cpi), names_to = "measure", values_to = "value")

# plot the data
tax_price_plot <- ggplot(final.data.q2 %>% filter(Year >= 1970 & Year <= 2018), aes(x = Year, y = value, color = measure)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("tax_cpi" = "red", "price_cpi" = "blue")) +
  labs(
    title = "Average Cigarette Tax and Price (1970-2018) Adjusted to 2012 Dollars",
    x = "Year",
    y = "Value (2012 Dollars)",
    color = "Measure"
  ) +
  theme_minimal() +
  theme(legend.position = "top")
print(tax_price_plot)
