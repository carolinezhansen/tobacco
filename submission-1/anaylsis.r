# submission 1 analysis

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, dplyr,tidyr, AER, fixest)

source("data/output/TaxBurden_Data.rds")

final.data$Year
# problem 1
# Present a bar graph showing the proportion of states with a change in their cigarette tax in each year from 1970 to 1985.

length(filtered.data1$tax_change)
length(Year)

filtered.data1 <- final.data[final.data$Year >= 1970 & final.data$Year <= 1985,]# nolint1
filtered.data1
calculate_tax_change <- function(filtered.data1) {
  tax_change <- c(NA, diff(filtered.data1$tax_state)) # nolint
  return(filtered.data1)
}
filtered.data1 <- calculate_tax_change(filtered.data1)
# Aggregate tax changes by year
tax_changes_by_year <- aggregate(tax_change ~ Year, data = filtered.data1, FUN = mean, na.rm = TRUE)
length(tax_change)
length(filtered.data1$Year)
# Create a bar plot of tax changes over years
graph1 <- barplot(tax_changes_by_year$tax_change, 
        names.arg = tax_changes_by_year$Year, 
        col = "skyblue", 
        main = "Average Tax Change by Year",
        xlab = "Year",
        ylab = "Average Tax Change")


# problem 2

# Plot on a single graph the average tax (in 2012 dollars) on cigarettes and the average price of a pack of cigarettes from 1970 to 2018.
total_tax_cpi_2012 <- final.data$tax_dollar*(cpi.2012/index)

price_cpi_2012 <- final.data$cost_per_pack*(cpi.2012/final.data$index)
average_tax1 <- final.data %>%
  group_by(Year) %>%
  summarize(avg_tax = mean(total_tax_cpi_2012, na.rm = TRUE))
average_tax1
average_price1 <- final.data %>%
  group_by(Year) %>%
  summarize(avg_price = mean(price_cpi_2012, na.rm = TRUE))

average_price

# Merge the two datasets based on the "Year" column
merged_data <- merge(average_tax1, average_price1, by = "Year", all = TRUE)

graph2 <- ggplot(merged_data, aes(x = Year)) +
  geom_line(aes(y = avg_tax, color = "Average Tax"), size = 1.5) +
  geom_line(aes(y = avg_price, color = "Average Price"), linetype = "dashed", size = 1.5) +
  labs(title = "Average Tax and Price of Cigarettes (1970-2018)",
       x = "Year",
       y = "Average Amount (in 2012 dollars)",
       color = "Legend") +
  scale_color_manual(values = c("Average Tax" = "blue", "Average Price" = "red")) +
  theme_minimal()
graph2
# problem 3
# Identify the 5 states with the highest increases in cigarette prices (in dollars) over the time period. Plot the average number of packs sold per capita for those states from 1970 to 2018.
# Step 1: Calculate the increase in cigarette prices for each state over the time period
price_change <- c(NA, diff(final.data$cost_per_pack)) # nolint
price_increase <- aggregate(price_change ~ state, final.data, FUN = function(x) tail(x, 1) - x[1])

top_5_states <- head(price_increase[order(-price_increase$price), ], 5)$state


filtered_data <- final.data[final.data$state %in% top_5_states, ]

graph3 <- ggplot(filtered_data, aes(x = Year, y = sales_per_capita, group = state, color = state)) +
  geom_line() +
  labs(x = "Year", y = "Average Packs Sold per Capita", 
       title = "Average Packs Sold per Capita for Top 5 States with Highest Price Increases",
       color = "State") +
  theme_minimal()

# problem 4
# Identify the 5 states with the lowest increases in cigarette prices over the time period. Plot the average number of packs sold per capita for those states from 1970 to 2018.

bottom_5_states <- head(price_increase[order(price_increase$price), ], 5)$state

filtered_data2 <- final.data[final.data$state %in% bottom_5_states, ]

graph4 <- ggplot(filtered_data2, aes(x = Year, y = sales_per_capita, group = state, color = state)) +
  geom_line() +
  labs(x = "Year", y = "Average Packs Sold per Capita", 
       title = "Average Packs Sold per Capita for Top 5 States with lowest Price Increases",
       color = "State") +
  theme_minimal()

# Problem 5
# Compare the trends in sales from the 5 states with the highest price increases to those with the lowest price increases.

compareincreases <- ggplot() +
  geom_line(data = filtered_data, aes(x = Year, y = sales_per_capita, color = state), linetype = "solid") +
  geom_line(data = filtered_data2, aes(x = Year, y = sales_per_capita, color = state), linetype = "dashed") +
  labs(x = "Year", y = "Average Packs Sold per Capita", 
       title = "Comparison of Sales Trends between High and Low Price Increase States",
       color = "State") +
  scale_color_viridis_d() + 
  theme_minimal()
print(compareincreases)
# problem 6
# Focusing only on the time period from 1970 to 1990, regress log sales on log prices to estimate the price elasticity of demand over that period. Interpret your results.

filtered_data3 <- final.data[final.data$Year >= 1970 & final.data$Year <= 1990, ]
final.data <- final.data %>% mutate(ln_sales=log(sales_per_capita),
                                ln_price_cpi=log(price_cpi),
                                ln_price=log(cost_per_pack),
                                tax_cpi=tax_state*(218/final.data$index),
                                total_tax_cpi=tax_dollar*(218/inal.data$index),
                                ln_total_tax=log(total_tax_cpi),                             
                                ln_state_tax=log(tax_cpi))



ols1 <- feols(ln_sales~ln_price2021, data=final.data %>% filter(Year<1991))







# the p-value is less than zero which means that there is a statistically significant relationship between sales and cost per pack. The coeficcient for cost per pack is -0.171 which means that there in an inelastic demand. So when 1% increase in cigarette price leds to a less than 1% decrease in sales per capita. 

# problem 7
# Again limiting to 1970 to 1990, regress log sales on log prices using the total (federal and state) cigarette tax (in dollars) as an instrument for log prices. Interpret your results and compare your estimates to those without an instrument. Are they different? If so, why?

iv1 = feols(ln_sales ~ 2 | ln_price_2012 ~ ln_tax_2012, data=cig.data %>% filter(Year<1991))

# problem 8
# Show the first stage and reduced-form results from the instrument.

firststage1 <- feols(ln_price2012~lin_tax_2012, data=final.data, %>% filter(Year<1991))

Reducedform1 <- feols(ln_price2012~lin_tax_2012, data=final.data, %>% filter(Year<1991))
# problem 9 
# Repeat questions 1-3 focusing on the period from 1991 to 2015.

Ols2 <- feols(ln_sales~ln_price2021, data=final.data %>% filter(Year>=1991 & Year<2015))

firststage2 <- feols(ln_price2012~lin_tax_2012, data=final.data, %>% filter(Year>=1991 & Year<2015))

# problem 10
# Compare your elasticity estimates from 1970-1990 versus those from 1991-2015. Are they different? If so, why?

# The elasticity estimate from 1970-1990 versus 1991-2015 are different. 

save.image("submission-1/Hwk3_workspace.Rdata")