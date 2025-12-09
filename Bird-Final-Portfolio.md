Final-RMD-Project\[Bird\]
================
Roman Matthew Bird
2025-12-09

- [ABSTRACT](#abstract)
- [INTRODUCTION](#introduction)
- [METHODS](#methods)
- [AIR QUALITY INDEX](#air-quality-index)
  - [PEARSON TEST & GLM](#pearson-test--glm)
- [DISCUSSION](#discussion)
- [CONCLUSION](#conclusion)
- [REFERENCES](#references)

# ABSTRACT

Chronic Obstructive Pulmonary Disease (COPD) and air quality have been
extensively studied to explore their interconnection. Our objective is
to investigate this relationship by addressing the question: Does
decreased air quality contribute to an increase in COPD cases across the
United States? To achieve this, we analyzed air quality data from the
World Population Review alongside COPD rates provided by the American
Lung Association. Utilizing statistical methods, including the Pearson
correlation test and generalized linear models, we sought to identify
any associations between the two datasets. Our findings indicated a
modest yet significant relationship, with 11% of COPD cases linked to
prolonged exposure to poor air quality. Therefore, it is evident that
exposure to low air quality can lead to COPD and other pulmonary
diseases, underscoring the need for measures to improve air quality in
order to enhance life expectancy in the United States.

# INTRODUCTION

The Air Quality Index (AQI) is a standardized system designed to assess
the quality of the air we breathe. It is determined by measuring four
major air pollutants outlined by the Clean Air Act: ground-level ozone,
particle pollution, carbon monoxide, and sulfur dioxide. The AQI is
presented on a scale from 0 to 500, where 0 indicates the cleanest air
and 500 signifies the most hazardous conditions. This scale effectively
categorizes air quality, with lower values representing higher quality
and higher values indicating poorer quality. The AQI has become a
valuable tool that is widely integrated into weather applications,
making it easily accessible to the general public.

Chronic obstructive pulmonary disease (COPD) is one of the leading
causes of death in urban areas, and its development can be attributed to
various factors such as tobacco use, occupational hazards, infections,
and air pollution. This disease progresses gradually and tends to worsen
with ongoing exposure to harmful substances. Unfortunately, the damage
caused by COPD is typically irreversible and can ultimately lead to
pulmonary failure. Several factors influence the prevalence of COPD,
including geographical region, age, and gender.

STUDY QUESTION & HYPOTHESIS:

With this information in mind, we have formulated the following
question: “Do regions with poor air quality experience higher rates of
Chronic Obstructive Pulmonary Disease (COPD)?” While numerous factors
contribute to the development of COPD, a negative correlation between
air quality and COPD rates could highlight several preventative measures
that can be implemented to enhance lung health and mitigate the risk of
the disease. These insights could influence health practices and empower
individuals at risk for COPD to steer clear of environmental hazards,
such as low air quality. Although various elements can affect COPD
prevalence, we hypothesize that such a negative correlation does exist,
and that areas characterized by lower air quality will indeed have
higher rates of Chronic Obstructive Pulmonary Disease.

# METHODS

We collected data from two databases, World Population Review air
quality database, and The National Lung Association COPD rates. All data
was formatted, normalized, and tested in R. We ran the data through two
statistical tests, a Pearson correlation test, and a generalized linear
model.

This formats our data so R can effectively read and format the states
data First we define define our pattern and format the COPD data Second
we create our data frame

``` r
lines <- str_split(data_text, "\n")[[1]]
lines <- trimws(lines)
lines <- lines[lines != ""]
lines <- lines[-1]

# 4. Define the pattern
pattern <- "^([A-Za-z .()0-9]+?)\\s+(\\d+\\.\\d)\\s+([\\d,]+)\\s+(\\d+\\.\\d)\\s+([\\d,]+)\\s+(\\d+\\.\\d)\\s+([\\d,]+)$"
rows <- str_match(lines, pattern)
rows <- rows[!is.na(rows[,1]), ]

# 7. Create the data frame
df <- as.data.frame(rows[,2:8])
colnames(df) <- c("State","Percent_Male","Count_Male","Percent_Female","Count_Female","Percent_Total","Count_Total")
df <- df %>%
  # Filter out rows i don't want to plot
  filter(State != "United States") %>%
  filter(State != "District of Columbia") %>% # <-- NEW FIX: Remove D.C.
  # Convert columns to the right type
  mutate(across(starts_with("Percent"), as.numeric),
         across(starts_with("Count"), ~ as.numeric(str_replace_all(.x, ",", ""))),
         
         State = str_replace(State, "\\s*\\(.*\\)", ""), # Removes (2020)
         State = str_trim(State))                       # Removes whitespace
```

Creation of the COPD gender maps and colors We first join the map data
and create the color scale We then map the data

``` r
# 1. map data
states_map <- map_data("state")

# 2. heat map data to join
heatmap_data <- df %>%
  select(State, Percent_Female, Percent_Male) %>%
  pivot_longer(cols = starts_with("Percent_"),
               names_to = "Group",
               values_to = "Percent") %>%
  mutate(Group = str_replace(Group, "Percent_", ""),
         region = tolower(State)) 
map_plot_data <- left_join(states_map, heatmap_data, by = "region", relationship = "many-to-many")

map_plot_data %>%
  filter(!is.na(Group)) %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = Percent)) +
  
  geom_polygon(color = "white", linewidth = 0.1) +
  scale_fill_viridis_c(name = "Percent") +
  facet_wrap(~ Group) + 
  labs(title = "COPD Prevalence by U.S. State and Gender") +
  theme_void() + 
  coord_map() + 
  theme(
    legend.position = "right",
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 14, face = "bold")
  
  )
```

![](Bird-Final-Portfolio_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

Figure 1. COPD prevalence is compared between males and females in the
continental United States. Females show a slightly higher average of
COPD.

This code creates the difference between sexes map. We first
differentiate male and female. Second we plot the difference. Lastly the
color scale is created and the map is printed.

``` r
states_map <- map_data("state") 
difference_data <- df %>%
  mutate(
    Percent_Difference = Percent_Female - Percent_Male,
    region = tolower(State) # <-- This creates the join key
  ) %>%
  select(region, Percent_Difference)
map_diff_data <- left_join(states_map, difference_data, by = "region")
ggplot(map_diff_data, aes(x = long, y = lat, group = group, fill = Percent_Difference)) +
  geom_polygon(color = "white", linewidth = 0.1) + # Draw the states
  # 5. Use the DIVERGING color scale
  scale_fill_gradient2(
    name = "Percent Difference\n(Female - Male)",
    low = "blue",      # States where males are higher (negative)
    mid = "white",     # States where rates are equal (zero)
    high = "red",      # States where females are higher (positive)
    midpoint = 0       # We center the scale at zero
  ) +
  
  labs(title = "Difference in COPD Prevalence (Female vs. Male)") +
  theme_void() + # Use a clean theme
  coord_map() +  # Use correct map projection
  theme(
    legend.position = "right",
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )
```

![](Bird-Final-Portfolio_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

Figure 2. COPD percent difference for females compared to males in
shown, with most states having a higher percentage of COPD prevalence in
females.

# AIR QUALITY INDEX

This code chunk creates the overall AQI map. We begin by reformatting
the raw data. Second we join the reformatted data to the mapping
package. Lastly we print the map

``` r
states_map <- map_data("state")

aq_data_to_plot <- aq_df %>%
  # Use the correct column names: `state` and `AirQuality_AirQualityIndexViaUSA_num_YearFree`
  # We also rename the long AQI column to `Overall_AQI` to make it easier to use
  select(State = state, Overall_AQI = `AirQuality_AirQualityIndexViaUSA_num_YearFree`) %>% 
  mutate(region = tolower(State)) # <-- This creates the join key

map_plot_data <- left_join(states_map, aq_data_to_plot, by = "region")


map_plot_data <- map_plot_data %>%
  filter(!is.na(Overall_AQI))

ggplot(map_plot_data, aes(x = long, y = lat, group = group, fill = Overall_AQI)) +
  geom_polygon(color = "white", linewidth = 0.1) + # Draw the states
  
  scale_fill_viridis_c(name = "Overall AQI") +
  
  labs(title = "Overall Air Quality Index (AQI) by U.S. State (2025)") +
  theme_void() + # Use a clean theme
  coord_map() +  # Use correct map projection
  theme(
    legend.position = "right",
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )
```

![](Bird-Final-Portfolio_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

Figure 3. Overall AQI is shown for each of the continental United
States, averaged over the year 2025. All states land generally within an
acceptable range for daily intake, with some on the higher side
indicating a lower AQI.

This code normalizes and reformats our data. We begin by loading needed
libraries. Then we load in our data and format to recombine them. Lastly
we normalize our data using the SQRT method.

``` r
# 1. Make sure you have the 'readr' library
library(readr)
library(dplyr)
library(tidyr) # For pivoting later
library(ggplot2) # For the scatter plot

# 2. Reload two data frames 
aq_df <- read_csv("air-quality-by-state-2025.csv")
```

    ## Rows: 51 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): state
    ## dbl (4): AirQuality_AirQualityIndexViaUSA_num_YearFree, AirQualityRankViaUSN...
    ## lgl (1): stateFlagCode
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# 3. Join the two data frames by state
# We need to make sure the state names match.
# 'df' has "State" (e.g., "Alabama")
# 'aq_df' has "state" (e.g., "Alabama")

combined_df <- inner_join(df, aq_df, by = c("State" = "state"))


aqi_column_name <- "AirQuality_AirQualityIndexViaUSA_num_YearFree"

combined_df <- combined_df %>%
  mutate(
    # A. Normalize COPD rates (Square Root)
    Percent_Total_Norm = sqrt(Percent_Total),
    Percent_Male_Norm = sqrt(Percent_Male),
    Percent_Female_Norm = sqrt(Percent_Female),
    
    # B. Normalize AQI (Reflect + Square Root)
    k = max(!!sym(aqi_column_name)) + 1,
    AQI_Normalized = sqrt(k - !!sym(aqi_column_name))
  )
```

This code plots our SQRT and SQRT reflected data. We first create the
histogram and colors. Second we create the axes

``` r
ggplot(df, aes(x = Percent_Total)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(
    title = "Distribution of Overall COPD Prevalence by State",
    x = "Overall COPD Rate (%)",
    y = "Number of States"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )
```

![](Bird-Final-Portfolio_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

Figure 4. Overall COPD rate percent is shown to be around 6, with some
variation among other states.

``` r
ggplot(aq_df, aes(x = `AirQuality_AirQualityIndexViaUSA_num_YearFree`)) +
  # AQI values are clustered, so a smaller binwidth is better
  geom_histogram(binwidth = 2, fill = "lightgreen", color = "black") +
  labs(
    title = "Distribution of Overall AQI by State (2025)",
    # We can set a cleaner label for the x-axis
    x = "Overall Air Quality Index (AQI)",
    y = "Number of States"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )
```

![](Bird-Final-Portfolio_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

Figure 5. Overall AQI is shown to be around 44, with a wide variation
among other states.

## PEARSON TEST & GLM

This code runs our combined data through a pearson correlation test, and
a GLM model validate our data significance

``` r
# Run a Pearson's Correlation Test
total_corr_test <- cor.test(combined_df$AQI_Normalized, combined_df$Percent_Total_Norm)

print(total_corr_test)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  combined_df$AQI_Normalized and combined_df$Percent_Total_Norm
    ## t = -2.4511, df = 48, p-value = 0.01793
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.55989949 -0.06083131
    ## sample estimates:
    ##        cor 
    ## -0.3335316

``` r
# Run the Simple Linear Regression (lm)
total_lm_model <- lm(Percent_Total_Norm ~ AQI_Normalized, data = combined_df)

print(summary(total_lm_model))
```

    ## 
    ## Call:
    ## lm(formula = Percent_Total_Norm ~ AQI_Normalized, data = combined_df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.80346 -0.19194 -0.06826  0.19992  0.90824 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     3.02207    0.18689  16.170   <2e-16 ***
    ## AQI_Normalized -0.14496    0.05914  -2.451   0.0179 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3281 on 48 degrees of freedom
    ## Multiple R-squared:  0.1112, Adjusted R-squared:  0.09273 
    ## F-statistic: 6.008 on 1 and 48 DF,  p-value: 0.01793

This code runs the same code as above between sexes

``` r
# Correlation for MALES
male_corr_test <- cor.test(combined_df$AQI_Normalized, combined_df$Percent_Male_Norm)
print(male_corr_test)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  combined_df$AQI_Normalized and combined_df$Percent_Male_Norm
    ## t = -2.0346, df = 48, p-value = 0.04743
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.519386051 -0.003718359
    ## sample estimates:
    ##        cor 
    ## -0.2817744

``` r
# Correlation for FEMALES
female_corr_test <- cor.test(combined_df$AQI_Normalized, combined_df$Percent_Female_Norm)
print(female_corr_test)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  combined_df$AQI_Normalized and combined_df$Percent_Female_Norm
    ## t = -2.6847, df = 48, p-value = 0.009937
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.58121237 -0.09224833
    ## sample estimates:
    ##        cor 
    ## -0.3613184

This code runs a GLM model against each sex to determine if there is
statistical significance between sexes

``` r
long_df <- combined_df %>%
  select(State, AQI_Normalized, Percent_Male_Norm, Percent_Female_Norm) %>%
  pivot_longer(
    cols = c("Percent_Male_Norm", "Percent_Female_Norm"),
    names_to = "Gender",
    values_to = "COPD_Norm"
  ) %>%
  mutate(Gender = str_replace(Gender, "Percent_", "")) %>%
  mutate(Gender = str_replace(Gender, "_Norm", ""))
gender_interaction_model <- lm(COPD_Norm ~ AQI_Normalized * Gender, data = long_df)

summary(gender_interaction_model)
```

    ## 
    ## Call:
    ## lm(formula = COPD_Norm ~ AQI_Normalized * Gender, data = long_df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.89027 -0.21358 -0.05057  0.17768  0.93263 
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                3.22954    0.19065  16.940  < 2e-16 ***
    ## AQI_Normalized            -0.17132    0.06033  -2.840  0.00551 ** 
    ## GenderMale                -0.43652    0.26961  -1.619  0.10872    
    ## AQI_Normalized:GenderMale  0.05610    0.08532   0.657  0.51245    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3347 on 96 degrees of freedom
    ## Multiple R-squared:  0.2218, Adjusted R-squared:  0.1975 
    ## F-statistic: 9.121 on 3 and 96 DF,  p-value: 2.273e-05

This code generates the final trendline plot and plots the data from the
GLM.

``` r
#This code chunk was troubleshooted and enhanced with AI
ggplot(combined_df, aes(x = AQI_Normalized, y = Percent_Total_Norm)) +
  geom_point(color = "black", alpha = 0.6, size = 2.5) +
  
  # Add the regression line
  geom_smooth(method = "lm", color = "darkblue", fill = "lightblue") +
  
  labs(
    title = "Overall Relationship: Air Quality vs. COPD Rates",
    x = "Normalized AQI Score \n(← Worse Air Quality | Better Air Quality →)",
    y = "Normalized Total COPD Rate"
  ) +
  
  theme_classic(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text = element_text(color = "black")
  )
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Bird-Final-Portfolio_files/figure-gfm/final%20plot-1.png)<!-- -->

Fig. 5 The graph visualizes the results from the Pearson correlation and
the GLM. In this plot it is evident that there is a correlation between
AQI and COPD rates

# DISCUSSION

Our primary analysis revealed a strong statistical correlation between
the Air Quality Index (AQI) and rates of Chronic Obstructive Pulmonary
Disease (COPD) (p=0.0179). This leads us to reject the null hypothesis
and support our initial hypothesis, confirming that states with poorer
average air quality have higher rates of COPD. Although the relationship
is significant, the effect size is moderate. Our model produced an R²
value of 0.1112, indicating that AQI accounts for approximately 11.1% of
the variation in COPD rates. This implies that other unaccounted
confounding factors, such as smoking, occupation, and genetics, likely
play a more substantial role.

We also examined the impact of AQI on biological sexes. The Pearson
correlation test indicated a slightly stronger relationship for females
(p = 0.0099) compared to males (p = 0.0474); however, our generalized
linear model revealed that this difference was not statistically
significant. The interaction term between gender and air quality yielded
a p-value of 0.512, suggesting that the regression slopes for males and
females are statistically indistinguishable. This indicates that poor
air quality is an equal-opportunity risk factor; as air quality
declines, COPD rates increase at similar rates for both sexes.

Limitations:

Firstly, relying on aggregate-level state data may not accurately
represent the average air quality experienced by individuals. For
instance, in Utah, much of the poor air quality is concentrated in the
two major valley centers, while residents outside these valleys often
enjoy significantly better air quality. Secondly, migration likely has a
substantial impact on the data. An individual might develop COPD in a
state with poor air quality and then relocate to a state with better
conditions, which could distort the overall findings.

# CONCLUSION

Our data indicates a correlation between areas with higher Air Quality
Index (AQI) numbers and increased rates of Chronic Obstructive Pulmonary
Disease (COPD). With this understanding, we can better inform residents
in regions with elevated AQI levels to take necessary precautions to
mitigate the risk of developing COPD. Recommended practices include
minimizing outdoor activities during times of poor air quality,
utilizing effective air filters in their homes, and curtailing the use
of machinery that contributes to elevated AQI levels. Leveraging this
knowledge could prove invaluable in reducing COPD rates across the
United States and helping individuals regain the quality of life they
deserve.

# REFERENCES

American Lung Association. (2023). COPD prevalence rates and counts by
state and gender. Retrieved November 18, 2025, from
<https://www.lung.org/research/trends-in-lung-disease/copd-trends-brief/data-tables/copd-prevalence-rates-by-state-gender>

Duan, R.R., Hao, K., & Yang, T. (2020). Air pollution and chronic
obstructive pulmonary disease. Chronic Diseases and Translational
Medicine, 6(4), 260–269. <https://doi.org/10.1016/j.cdtm.2020.05.004>

Google. (2025). Gemini (Version Jan 2025) \[Large language model\].
Accessed 2025-12-09.

OpenAI. (2025). ChatGPT (Jan 2025 version) \[Large language model\].
Accessed 2025-12-09.

World Population Review. (2025). Air Quality by State 2025.
<https://worldpopulationreview.com/state-rankings/air-quality-by-state>
