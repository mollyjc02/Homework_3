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
tax.change.proportion <- final.data.q1 %>%
  group_by(Year) %>%
  summarize(proportion_change = mean(tax_change, na.rm = TRUE))

### plot the bar graph
tax.change.plot <- ggplot(tax.change.proportion, aes(x = Year, y = proportion_change)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(
    title = "Proportion of States with a Change in Cigarette Tax (1970-1985)",
    x = "Year",
    y = "Proportion of States with Tax Change"
  ) +
  theme_minimal()
print(tax.change.plot)



# 2. Plot on a single graph the average tax (in 2012 dollars) on cigarettes and the average price of a pack of cigarettes from 1970 to 2018.
### adjust taxes to 2012 values 
final.data <- final.data %>%
  mutate(tax_cpi = tax_dollar * (218 / index))

### reshape the data to long format for easy plotting
final.data.q2 <- final.data %>%
  select(Year, tax_cpi, price_cpi) %>%
  pivot_longer(cols = c(tax_cpi, price_cpi), names_to = "measure", values_to = "value")

### plot the data
tax.price.plot <- ggplot(final.data.q2 %>% filter(Year >= 1970 & Year <= 2018), aes(x = Year, y = value, color = measure)) +
  geom_line(linewidth = 1) +
  geom_point(size = 0.8, shape = 16, alpha = 0.5) +
  scale_color_manual(values = c("tax_cpi" = "red", "price_cpi" = "blue")) +
  labs(
    title = "Average Cigarette Tax and Price Adjusted to 2012 Dollars",
    x = "Year",
    y = "Value (2012 Dollars)",
    color = "Measure"
  ) +
  theme_minimal() +
  theme(legend.position = "top")
print(tax.price.plot)



#3. Identify the 5 states with the highest increases in cigarette prices (in dollars) over the time period. Plot the average number of packs sold per capita for those states from 1970 to 2018. 
final.data <- final.data %>%
  mutate(Year = as.integer(Year))

### calculate price difference 
final.data.q3 <- final.data %>% 
  filter(Year == 1970 | Year == 2017) %>%
  group_by(state) %>%
  summarize(price_1970 = first(price_cpi), price_2018 = last(price_cpi)) %>%
  mutate(price_increase = price_2018 - price_1970) %>%
  arrange(desc(price_increase))
print(final.data.q3)

### isolate top 5 
top.5.states <- final.data.q3 %>%
  arrange(desc(price_increase)) %>%
  slice_head(n = 5) %>%
  pull(state) 
print(top.5.states)

### filter data for top 5 and calculate average packs sold 
top.5.data <- final.data %>%
  filter(state %in% top.5.states) %>%
  group_by(Year, state) %>%
  summarize(avg_packs_per_capita = mean(sales_per_capita, na.rm = TRUE)) %>%
  ungroup()

### plot 
top.5.plot <- ggplot(top.5.data, aes(x = Year, y = avg_packs_per_capita, color = state)) +
  geom_line(linewidth = 1) +
  geom_point(size = 0.8, shape = 16, alpha = 0.5) +
  labs(
    title = "Average Packs Sold Per Capita (Top 5 States with Highest Price Increase)",
    x = "Year",
    y = "Average Packs Sold Per Capita",
    color = "State"
  ) +
  theme_minimal() +
  theme(legend.position = "top")
print(top.5.plot)



# 4. Identify the 5 states with the lowest increases in cigarette prices over the time period. Plot the average number of packs sold per capita for those states from 1970 to 2018.
### isolate bottom 5 
bot.5.states <- final.data.q3 %>%
  arrange(price_increase) %>%
  slice_head(n = 5) %>%
  pull(state) 
print(bot.5.states)

### filter data for bottom 5 and calculate average packs sold 
bot.5.data <- final.data %>%
  filter(state %in% bot.5.states) %>%
  group_by(Year, state) %>%
  summarize(avg_packs_per_capita = mean(sales_per_capita, na.rm = TRUE)) %>%
  ungroup()

### plot 
bot.5.plot <- ggplot(bot.5.data, aes(x = Year, y = avg_packs_per_capita, color = state)) +
  geom_line(linewidth = 1) +
  geom_point(size = 0.8, shape = 16, alpha = 0.5) +
  labs(
    title = "Average Packs Sold Per Capita (Top 5 States with Lowest Price Increase)",
    x = "Year",
    y = "Average Packs Sold Per Capita",
    color = "State"
  ) +
  theme_minimal() +
  theme(legend.position = "top")
print(bot.5.plot)



# 5. Compare the trends in sales from the 5 states with the highest price increases to those with the lowest price increases.
### compute average packs sold per capita for the top and bottom 5 states
top.5.avg <- final.data %>%
  inner_join(top.5.states %>% tibble(state = .), by = "state") %>%  # Efficient filtering
  group_by(Year) %>%
  summarize(avg_packs_top5 = mean(sales_per_capita, na.rm = TRUE)) %>%
  ungroup()

bot.5.avg <- final.data %>%
  inner_join(bot.5.states %>% tibble(state = .), by = "state") %>%  
  group_by(Year) %>%
  summarize(avg_packs_bot5 = mean(sales_per_capita, na.rm = TRUE)) %>%
  ungroup()

### merge the two datasets
final.data.q4 <- left_join(top.5.avg, bot.5.avg, by = "Year")

### plot the comparison
comparison.plot <- ggplot(final.data.q4, aes(x = Year)) + 
  geom_line(aes(y = avg_packs_top5, color = "Top 5 Price Increase"), linewidth = 1.2) +
  geom_line(aes(y = avg_packs_bot5, color = "Bottom 5 Price Increase"), linewidth = 1.2) +
  labs(
    title = "Comparison of Cigarette Sales in States with High vs. Low Price Increases",
    x = "Year",
    y = "Average Packs Sold Per Capita",
    color = "Group"
  ) +
  theme_minimal() +
  theme(legend.position = "top")
print(comparison.plot)



#### 1970-1990 
final.data.70.90 <- final.data %>%
  filter(Year >= 1970 & Year <= 1990)


# 6a. Regress log sales on log prices to estimate the price elasticity of demand over that period.
### create log transformed variables
final.data.70.90 <- final.data.70.90 %>%
  mutate(log_sales = log(sales_per_capita), 
         log_price = log(price_cpi)) 

### run the regression 
model.a <- lm(log_sales ~ log_price, data = final.data.70.90)
summary(model.a)



# 7a. Again limiting to 1970 to 1990, regress log sales on log prices using the total (federal and state) cigarette tax (in dollars) as an instrument for log prices. 
### create log transformations 
final.data.70.90 <- final.data.70.90 %>%
  mutate(log_sales = log(sales_per_capita),
         log_price = log(price_cpi),
         log_total_tax = log(tax_dollar))

### run regression using feols 
library(fixest)
ivs.a <- feols(log_sales ~ 1 | log_price ~ log_total_tax, data = final.data.70.90)
summary(ivs.a)



# 8a. Show the first stage and reduced-form results from the instrument.
### first stage 
first.stage.a <- lm(log_price ~ log_total_tax, data = final.data.70.90)
summary(first.stage.a) 

### reduced form 
reduced.form.a <- lm(log_sales ~ log_total_tax, data = final.data.70.90)
summary(reduced.form.a)




#### 1991-2015 
final.data.91.15 <- final.data %>%
  filter(Year >= 1991 & Year <= 2015)


# 6b. Regress log sales on log prices to estimate the price elasticity of demand over that period.
### create log transformed variables
final.data.91.15 <- final.data.91.15 %>%
  mutate(log_sales = log(sales_per_capita), 
         log_price = log(price_cpi)) 

### run the regression 
model.b <- lm(log_sales ~ log_price, data = final.data.91.15)
summary(model.b)



# 7b. Again limiting to 1970 to 1990, regress log sales on log prices using the total (federal and state) cigarette tax (in dollars) as an instrument for log prices. 
### create log transformations 
final.data.91.15 <- final.data.91.15 %>%
  mutate(log_sales = log(sales_per_capita),
         log_price = log(price_cpi),
         log_total_tax = log(tax_dollar))

### run regression using feols 
library(fixest)
ivs.b <- feols(log_sales ~ 1 | log_price ~ log_total_tax, data = final.data.91.15)
summary(ivs.b)



# 8b. Show the first stage and reduced-form results from the instrument.
### first stage 
first.stage.b <- lm(log_price ~ log_total_tax, data = final.data.91.15)
summary(first.stage.b) 

### reduced form 
reduced.form.b <- lm(log_sales ~ log_total_tax, data = final.data.91.15)
summary(reduced.form.b) 



# 10. Table comparing estimates 
coef.a <- coef(ivs.a)
coef.b <- coef(ivs.b)  

comparison.table <- data.frame(
  "1970-1990" = coef.a["fit_log_price"],
  "1991-2015" = coef.b["fit_log_price"])

### create a nice table 
library(knitr)
rownames(comparison.table) <- "Slope Estimate"
kable(comparison.table, col.names = c("1970-1990", "1991-2015"), 
      caption = "Comparison of Slope Estimates for 1970-1990 and 1991-2015", 
      format = "markdown", align = "c")



###rm(list = setdiff(ls(), c("tax.change.plot", "tax.price.plot", "top.5.plot", "bot.5.plot", "comparison.plot", "model.a", "ivs.a", "first.stage.a", "reduced.form.a", "model.b", "ivs.b", "first.stage.b", "reduced.form.b", "comparison.table")))
###save.image("submission_1/hwk3_workspace.RData")
