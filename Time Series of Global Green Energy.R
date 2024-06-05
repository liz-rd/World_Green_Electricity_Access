
### need to join the data sets 

library(tidyverse)
library(janitor)
#this package was to easily change the header names to be accurate
library(patchwork)
library(readr)

filter_electricity <-  access_to_electricity_by_percent %>%
janitor::row_to_names(row_number = 3, remove_rows_above = TRUE) 

filter_green <-  green_energy_ %>%
janitor::row_to_names(row_number = 3, remove_rows_above = TRUE)

## filter out unnecessary data in both sets, leaving just regional data
## and join the data sets
join_temp <- filter_electricity |>
  filter(`Country Code` == "EAS"   |
           `Country Code` == "ECS" | 
           `Country Code` == "LCN" |
           `Country Code`== "MEA"  |
           `Country Code` == "NAC" |
           `Country Code` == "SAS" | 
           `Country Code` == "SSF") |>
  left_join(filter_green, by = 'Country Code',
            suffix = c("_e", "_g"))

join_temp

join_temp <- join_temp[-c(4:44)]
join_temp <- join_temp[-c(20:27)]
join_temp <- join_temp[-c(21:51)]
join_temp <- join_temp[-c(21:30, 47:53)]


## Then create the new variable(s) accounting for total access and green energy
#pull out the region data instead of individual countries (or income data)

test_loop <- join_temp
years <- 2000:2015

# Initialize an empty data frame to store the results
result <- data.frame(matrix(NA, nrow = nrow(test_loop), ncol = length(years)))

# Loop through each year
for (year in years) {
  new_column <- paste0(year, "_e")
  growth_column <- paste0(year, "_g")
  #formula for determining the percentage of the pop. with access to renewnable energy 
  result[[paste0(year)]] <- as.numeric(test_loop[[new_column]]) * (as.numeric(test_loop[[growth_column]]) / 100)
}

result <- result[-c(1:16)]
result <- result |>
  mutate(result, join_temp$`Country Name_e`,.before = "2000")

col_index <- which(colnames(result) == "join_temp$`Country Name_e`")

# Change the column name at that index
colnames(result)[col_index] <- "Region Name"
green_total <- result
#saving the df externally for later use
write_csv(green_total, "/Users/liz/Desktop/Class/2023 class/Program in R/green_total.csv")

# looking at a time series of total green energy access by year to predict growth 
# changing data from wide to long

x_green <- c(2:17)

ts_green_total <- green_total %>%
  pivot_longer(cols = all_of(x_green), names_to = 'Year', values_to = 'Percent of Population')

#creating plot for each region to look at the change over 15 years
  #sorted by region
c_region <- c('#cc241d','#0054FF','#BCBD22','#458588','violet','#00BC00','#d65d0e')

#mapping of all 7
ts_green_total |>
  group_by(`Country Name_e`) |>
ggplot(aes(x=Year, y=`Percent of Population`, group = `Country Name_e`, color= `Country Name_e`)) +
  geom_line(stat = "identity") +
  geom_point()+
  scale_colour_manual('Region',values = c_region) +
  labs(x = "Year",
       y = "Percent of Population With Access to Green Energy",
       title = 'Population Access to Green Energy By Year and Geographic Region',
       subtitle = 'figure 1') +
  theme_linedraw() + 
  theme(legend.position="bottom",
        text=element_text(size=12),
        legend.title=element_text(size=14))

#east asia mapping 
EAP_total <- ts_green_total |>
  filter(`Country Name_e` == 'East Asia & Pacific') |>
  ggplot(aes(x=Year, y=`Percent of Population`, group = 7, color= '#cc241d')) +
  geom_line(stat = "identity", color= '#cc241d') +
  geom_point(color= '#cc241d') +
  labs(x = "Year",
       y = "Percent of Population With Access",
       title = 'Population Access in East Asia and the Pacific',
       subtitle = 'figure 2') +
  theme_linedraw() + 
  theme(legend.position="bottom",
        text=element_text(size=12),
        legend.title=element_text(size=14)) + 
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))
EAP_total

#Europe and Central Asia
ECA_total <- ts_green_total |>
  filter(`Country Name_e` == 'Europe & Central Asia') |>
  ggplot(aes(x=Year, y=`Percent of Population`,group = 1, color= '#0054FF')) +
  geom_line(stat = "identity", color= '#0054FF') +
  geom_point(color= '#0054FF') + 
  labs(x = "Year",
       y = "Percent of Population With Access",
       title = 'Population Access in Europe & Central Asia',
       subtitle = 'figure 3') +
  theme_linedraw() + 
  theme(legend.position="bottom",
        text=element_text(size=12),
        legend.title=element_text(size=14)) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))
ECA_total                 

#Latin America + Caribean 
LAC_total <- ts_green_total |>
  filter(`Country Name_e` == 'Latin America & Caribbean') |>
  ggplot(aes(x=Year, y=`Percent of Population`, group = 7, color= '#BCBD22')) +
  geom_line(stat = "identity", color= '#BCBD22') +
  geom_point(color= '#BCBD22') +
  labs(x = "Year",
       y = "Percent of Population",
       title = 'Population Access in Latin America & Caribbean',
       subtitle = 'figure 4') +
  theme_linedraw() + 
  theme(legend.position="bottom",
        text=element_text(size=12),
        legend.title=element_text(size=14)) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))

#Middle east and North Africa

MEA_total <- ts_green_total |>
  filter(`Country Name_e` == 'Middle East & North Africa') |>
  ggplot(aes(x=Year, y=`Percent of Population`, group = 7, color= '#458588')) +
  geom_line(stat = "identity", color= '#458588') +
  geom_point(color= '#458588') +
  labs(x = "Year",
       y = "Percent of Population",
       title = 'Population Access in the Middle East and N. Africa',
       subtitle = 'figure 5') +
  theme_linedraw() + 
  theme(legend.position="bottom",
        text=element_text(size=12),
        legend.title=element_text(size=14))+
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))

#North America

NAC_total <- ts_green_total |>
  filter(`Country Name_e` == 'North America') |>
  ggplot(aes(x=Year, y=`Percent of Population`, group = 7, color= 'violet')) +
  geom_line(stat = "identity", color= 'violet') +
  geom_point(color= 'violet') +
  labs(x = "Year",
       y = "Percent of Population",
       title = 'Population Access in North America',
       subtitle = 'figure 6') +
  theme_linedraw() + 
  theme(legend.position="bottom",
        text=element_text(size=12),
        legend.title=element_text(size=14))+
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))

#South Asia 

SAS_total <- ts_green_total |>
  filter(`Country Name_e` == 'South Asia') |>
  ggplot(aes(x=Year, y=`Percent of Population`, group = 7, color= '#00BC00')) +
  geom_line(stat = "identity", color= '#00BC00') +
  geom_point(color= '#00BC00') +
  labs(x = "Year",
       y = "Percent of Population",
       title = 'Population Access in South Asia',
       subtitle = 'figure 7') +
  theme_linedraw() + 
  theme(legend.position="bottom",
        text=element_text(size=12),
        legend.title=element_text(size=14)) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))
SAS_total
#Sub-Saharan Africa

SSA_total <- ts_green_total |>
  filter(`Country Name_e` == 'Sub-Saharan Africa') |>
  ggplot(aes(x=Year, y=`Percent of Population`, group = 7, color= '#d65d0e')) +
  geom_line(stat = "identity",color= '#d65d0e') +
  geom_point(color= '#d65d0e') + 
  labs(x = "Year",
       y = "Percent of Population",
       title = 'Population Access in Sub-Saharan Africa',
       subtitle = 'figure 8') +
  theme_linedraw() + 
  theme(legend.position="bottom",
        text=element_text(size=12),
        legend.title=element_text(size=14)) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))
SSA_total
#patchworking them together to save sanity 

grand_total1 <- (EAP_total + ECA_total)/(LAC_total + MEA_total)
grand_total2 <- (NAC_total + SAS_total)/SSA_total
grand_total1
grand_total2
