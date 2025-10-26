# Loading required libraries
library(tidyverse)
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)

# Loading and cleaning the data
library(readxl)
HIV_data_2000_2023 <- read_excel("C:/Users/ADMIN/Downloads/HIV data 2000-2023.xlsx")
View(HIV_data_2000_2023)

# Extracting numeric estimates from the Value column
df <- HIV_data_2000_2023 %>%
  mutate(
    Estimate = str_extract(Value, "^\\d+[\\s\\d]*") %>% 
      str_replace_all(" ", "") %>%
      as.numeric()
  ) %>%
  filter(!is.na(Estimate)) %>%
  rename(Country = Location, Region = ParentLocationCode, Year = Period)
View(df)

# Calculating cumulative burden per country (2000–2023)
cumulative_burden <- df %>%
  group_by(Country) %>%
  summarise(TotalEstimate = sum(Estimate, na.rm = TRUE)) %>%
  arrange(desc(TotalEstimate)) %>%
  mutate(CumSum = cumsum(TotalEstimate),
         GlobalTotal = sum(TotalEstimate),
         CumPercentage = CumSum / GlobalTotal) %>%
  filter(CumPercentage <= 0.75)

# Extracting the countries
top_countries_cumulative <- cumulative_burden$Country

# Creating a bar plot for cumulative burden per country
ggplot(cumulative_burden, aes(x = reorder(Country, -CumSum), y = CumSum)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Cumulative Burden per Country (2000–2023)",
       x = "Country",
       y = "Cumulative Burden") +
  theme_minimal() +
  coord_flip()  # Flipping coordinates for better readability

# Cumulative estimates per country per region
cumulative_by_region <- df %>%
  group_by(Region, Country) %>%
  summarise(TotalEstimate = sum(Estimate, na.rm = TRUE), .groups = "drop") %>%
  arrange(Region, desc(TotalEstimate)) %>%
  group_by(Region) %>%
  mutate(
    RegionTotal = sum(TotalEstimate),  
    CumSum = cumsum(TotalEstimate),    
    CumPerc = CumSum / RegionTotal
  ) %>%
  filter(CumPerc <= 0.75)  

top_countries_per_region <- cumulative_by_region %>%
  pull(Country) %>%  # Country column
  unique() 
print(top_countries_per_region)

# Filtering original dataset for those countries
regional_trend_data <- df %>%
  filter(Country %in% top_countries_per_region) 

# Trends per region
ggplot(regional_trend_data, aes(x = Year, y = Estimate, color = Country)) +
  geom_line(size = 1) + 
  facet_wrap(~ Region, scales = "free_y") + 
  labs(
    title = "HIV Trend in Countries Contributing to 75% of Regional Burden (2000–2023)",
    y = "Estimated People Living with HIV",
    x = "Year"
  ) +
  theme_minimal() +  
  theme(legend.position = "bottom")

# Loading the multi-dimensional poverty data
library(readxl)
multidimensional_poverty <- read_excel("C:/Users/ADMIN/Downloads/multidimensional_poverty.xlsx")
View(multidimensional_poverty)

# Cleaning the data
MDP <- multidimensional_poverty %>%
  rename(
    Country = `Economy`,
    Year = `Reporting year`,
    MPI = `Multidimensional poverty headcount ratio (%)`,
    Edu_Attain = `Educational attainment (%)`,
    Edu_Enroll = `Educational enrollment (%)`,
    Electricity = `Electricity (%)`,
    Sanitation = `Sanitation (%)`,
    Water = `Drinking water (%)`
  ) %>%
  select(Country, Year, MPI, Edu_Attain, Edu_Enroll, Electricity, Sanitation, Water)
View (MDP)

# Loading required library
#install.packages("lme4")
library(lme4)

# Checking unique values in the problematic columns
unique_values_Edu_Enroll <- unique(merged_df$Edu_Enroll)
unique_values_Edu_Attain <- unique(merged_df$Edu_Attain)
unique_values_Electricity <- unique(merged_df$Electricity)
unique_values_Sanitation <- unique(merged_df$Sanitation)
unique_values_Water <- unique(merged_df$Water)

# Printing unique values to identify non-numeric entries
print(unique_values_Edu_Enroll)
print(unique_values_Edu_Attain)
print(unique_values_Electricity)
print(unique_values_Sanitation)
print(unique_values_Water)

# Cleaning the data: Replacing non-numeric values with NA or appropriate values
merged_df <- merged_df %>%
  mutate(
    Edu_Enroll = as.numeric(gsub("N/A", NA, Edu_Enroll)),
    Edu_Attain = as.numeric(gsub("N/A", NA, Edu_Attain)),
    Electricity = as.numeric(gsub("N/A", NA, Electricity)),
    Sanitation = as.numeric(gsub("N/A", NA, Sanitation)),
    Water = as.numeric(gsub("N/A", NA, Water))
  )
# Removing rows with NA values in specific columns
merged_df <- merged_df %>%
  filter(!is.na(Edu_Enroll) & !is.na(Edu_Attain) & !is.na(Electricity) & 
           !is.na(Sanitation) & !is.na(Water))

View(merged_df)

summary(merged_df)

library(car)

# Fitting the model
model <- lm(Estimate ~ MPI + Edu_Attain + Edu_Enroll + Electricity + Sanitation + Water, data = merged_df_scaled)

# Calculating VIF for each variable
vif(model)

# Scaling numeric predictors for better model convergence
merged_df_scaled <- merged_df %>%
  mutate(across(c(Edu_Attain, Edu_Enroll, Sanitation, Water), scale))

# Random intercept by year only
model <- lmer(
  Estimate ~Edu_Attain + Edu_Enroll + Sanitation + Water +
    (1 |Year),
  data = merged_df_scaled
)

# Viewing summary of the model
summary(model)



