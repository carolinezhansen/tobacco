anaylsis

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, dplyr,tidyr)

source("data/output/TaxBurden_Data.rds")

final.data
# hw 1
# Present a bar graph showing the proportion of states with a change in their cigarette tax in each year from 1970 to 1985.
calculate_tax_change <- function(final.data) {
  final.data$tax_change <- c(NA, diff(final.data$tax_state))
  return(final.data)
}

# View the updated dataset
print(calculate_tax_change)

barplot(calculate_tax_change, 
        names.arg = years, 
        col = "skyblue", 
        main = "Proportion of States with Change in Cigarette Tax (1970-1985)",
        xlab = "Year",
        ylab = "Proportion of States",
        ylim = c(0, 1),  # Adjust y-axis limits as needed
        las = 2,  # Rotate x-axis labels
        cex.names = 0.8,  # Adjust label size
        border = NA)  # Remove bar borders
