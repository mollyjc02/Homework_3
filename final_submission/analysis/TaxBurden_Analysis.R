if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)



# 1. Present a bar graph showing the proportion of states with a change in their cigarette tax in each year from 1970 to 1985. 
### compare by year to identify tax changes for each state 
final.data.q1 <- final.data %>%
  filter(Year >= 1970, Year <= 1985) %>%
  arrange(state, Year) %>%
  group_by(state) %>%
  mutate(tax_change = tax_state != lag(tax_state, default = first(tax_state))) %>%
  ungroup()

### calculate proportion of states with tax change 
tax.change.proportion <- final.data.q1 %>%
  group_by(Year) %>%
  summarize(proportion_change = mean(tax_change, na.rm = TRUE))

### plot the bar graph
tax.change.plot <- ggplot(tax.change.proportion, aes(x = Year, y = proportion_change)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Year",
       y = "Proportion of States") +
  theme_minimal()
print(tax.change.plot)


# 2. Plot on a single graph the average tax (in 2012 dollars) on cigarettes and the average price of a pack of cigarettes from 1970 to 2018.
### adjust taxes to 2012 values 
final.data <- final.data %>%
  mutate(price_real = cost_per_pack * (230/index),
         tax_real = tax_dollar * (230/index))

### plot the data
tax.plot <- final.data %>%
  filter(Year >= 1970 & Year <= 2018) %>%
  group_by(Year) %>%
  summarize(avg_tax = mean(tax_real, na.rm = TRUE),
            avg_price = mean(price_real, na.rm = TRUE))

tax.price.plot <- ggplot(tax.plot, aes(x = Year)) +
  geom_line(aes(y = avg_tax, color = "Average Tax"), size = 1.2) +
  geom_line(aes(y = avg_price, color = "Average Price"), size = 1.2) +
  geom_text(data = tax.plot %>% filter(Year == 2018), 
            aes(x = Year - 10, y = avg_tax + 0.2, label = "Average Tax"), 
            hjust = 0, color = "black") +
  geom_text(data = tax.plot %>% filter(Year == 2018), 
            aes(x = Year - 10, y = avg_price + 0.2, label = "Average Price"), 
            hjust = 0, color = "black") +
  labs(x = "Year", y = "Price") +
  theme_minimal() +
  theme(legend.position = "none")

print(tax.price.plot)



#3. Identify the 5 states with the highest increases in cigarette prices (in dollars) over the time period. Plot the average number of packs sold per capita for those states from 1970 to 2018. 
final.data <- final.data %>%
  mutate(Year = as.integer(Year))

### calculate price difference 
final.data.q3 <- final.data %>% 
  filter(Year == 1970) %>% select(state, price_1970=price_real) %>% #### price_real vs price_cpi!!!
  left_join(final.data %>%  filter(Year == 2018) %>% select(state, price_2018=price_real), by=c("state")) %>% 
  mutate(price_change=price_2018-price_1970)

high.change <- final.data.q3 %>% slice_max(price_change, n=5) %>% mutate (change_group = "high")
low.change <- final.data.q3 %>% slice_min(price_change, n=5) %>% mutate (change_group = "low")
change.group <- rbind(high.change, low.change)

top.bottom.price <- final.data %>% ungroup() %>% 
inner_join(change.group %>% select(state, change_group),
            by=c("state"))


## Plot the sales per capita for these states 
top.5.plot <- top.bottom.price %>% filter(change_group=="high") %>% 
  ggplot(aes(x = Year, y = sales_per_capita, color = state)) +
  stat_summary(fun="mean", geom="line") +
  labs(x = "Year",
       y = "Packs Sold Per Capita",
       color = "State") +
  theme_minimal()

print(top.5.plot)


# 4. Identify the 5 states with the lowest increases in cigarette prices over the time period. Plot the average number of packs sold per capita for those states from 1970 to 2018.
bot.5.plot <- top.bottom.price %>% filter(change_group=="low") %>% 
  ggplot(aes(x = Year, y = sales_per_capita, color = state)) +
  stat_summary(fun="mean", geom="line") +
  labs(x = "Year",
       y = "Packs Sold Per Capita",
       color = "State") +
  theme_minimal()

print(bot.5.plot)



# 5. Compare the trends in sales from the 5 states with the highest price increases to those with the lowest price increases.
comparison.plot <- top.bottom.price %>% 
  ggplot(aes(x = Year, y=sales_per_capita, color=change_group)) + 
  stat_summary(fun="mean", geom="line") + 
  labs(x = "Year",
       y = "Average Packs Sold Per Capita",
       color = "Level of Price Increase"
  ) + 
  geom_text(data=top.bottom.price %>% group_by(Year, change_group) %>% 
            summarise(mean_sales=mean(sales_per_capita, na.rm=TRUE)) %>% 
            filter(Year==2015), 
            aes(label=c("High Increases", "Low Increases"), 
            x=Year-4, 
            y=mean_sales-5), 
            color = "black") +
  theme_minimal() +
  theme(legend.position = "none") 
print(comparison.plot)



#### 1970-1990 

# 6a. Regress log sales on log prices to estimate the price elasticity of demand over that period.
### create log transformed variables
final.data <- final.data %>%
  mutate(log_sales = log(sales_per_capita),
         log_price = log(price_real),
         log_total_tax = log(tax_real))

### run the regression 
library(fixest)
ols.a <- feols(log_sales ~ log_price, data = final.data %>% filter(Year >= 1970 & Year <= 1990))


# 7a. Again limiting to 1970 to 1990, regress log sales on log prices using the total (federal and state) cigarette tax (in dollars) as an instrument for log prices. 
ivs.a <- feols(log_sales ~ 1 | log_price ~ log_total_tax, data = final.data %>% filter(Year >= 1970 & Year <= 1990))


# 8a. Show the first stage and reduced-form results from the instrument.
### first stage 
first.stage.a <- feols(log_price ~ log_total_tax, data = final.data %>% filter(Year >= 1970 & Year <= 1990))


### reduced form 
reduced.form.a <- feols(log_sales ~ log_total_tax, data = final.data %>% filter(Year >= 1970 & Year <= 1990))



#### 1991-2015 
final.data.91.15 <- final.data %>%
  filter(Year >= 1991 & Year <= 2015)


# 6b. Regress log sales on log prices to estimate the price elasticity of demand over that period.
ols.b <- feols(log_sales ~ log_price, data = final.data %>% filter(Year >= 1991 & Year <= 2015))


# 7b. Again limiting to 1970 to 1990, regress log sales on log prices using the total (federal and state) cigarette tax (in dollars) as an instrument for log prices. 
ivs.b <- feols(log_sales ~ 1 | log_price ~ log_total_tax, data = final.data %>% filter(Year >= 1991 & Year <= 2015))


# 8b. Show the first stage and reduced-form results from the instrument.
### first stage 
first.stage.b <- feols(log_price ~ log_total_tax, data = final.data %>% filter(Year >= 1991 & Year <= 2015))


### reduced form 
reduced.form.b <- feols(log_sales ~ log_total_tax, data = final.data %>% filter(Year >= 1991 & Year <= 2015))



rm(list = setdiff(ls(), c("tax.change.plot", "tax.price.plot", "top.5.plot", "bot.5.plot", "comparison.plot", "ols.a", "ivs.a", "first.stage.a", "reduced.form.a", "ols.b", "ivs.b", "first.stage.b", "reduced.form.b")))
save.image("final_submission/results/hwk3_workspace.RData")