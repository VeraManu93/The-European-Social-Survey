# Step 1: Load libraries
library(ggplot2)
library(dplyr)
library(readr)
library(tidyverse)

ESS1 <- read.csv("ESS1.csv")
View(ESS1)


# Step 2a: Removing unwanted columns (Data Cleaning)
ESS1 <- select(ESS1,-"dweight",-"pspwght",-"pweight",-"anweight",-"prob",-"stratum",-"psu")
View(ESS1)


# Step 2b: Remove records with special values (e.g., 999 for missing age)
ESS1 <- ESS1 |>
  filter(
    !hhmmb %in% c(77,88,99),
    !agea %in% c(999),
    !eduyrs %in% c(77,88,99),
    !imbgeco %in% c(77,88,99),
    !ppltrst %in% c(77,88,99)
  )
View(ESS1)


# Step 3:  Perform linear regressions of each dependent variable using only the essround variable as the independent variable. Based on this, determine whether there would be a tendency for each selected dependent variable to increase or decrease in the future (i.e. for the next ESS round, number 12).
lm_dep1 <- lm(imbgeco ~ essround, data = ESS1)
lm_dep2 <- lm(ppltrst ~ essround, data = ESS1)
summary(lm_dep1)
summary(lm_dep2)



# Step 4: Repeat the analysis performed in item 3, considering exclusively the data from each of the two previously chosen countries. Is there any difference in this regard? In other words, what differences are there between the trends in these countries?
cntry1_data <- ESS1 %>% filter(cntry == "FR")
cntry2_data <- ESS1 %>% filter(cntry == "FI")

# Step 4a: Regression for FR
lm_cntry1A <- lm(imbgeco ~ essround, data = cntry1_data)
lm_cntry1B <- lm(ppltrst ~ essround, data = cntry1_data)
summary(lm_cntry1A)
summary(lm_cntry1B)

# Step 4b: Regression for FI
lm_cntry2A <- lm(imbgeco ~ essround, data = cntry2_data)
lm_cntry2B <- lm(ppltrst ~ essround, data = cntry2_data)
summary(lm_cntry2A)
summary(lm_cntry2B)


# Step 5:  Repeat the analysis performed in item 3, considering exclusively the data from the 2 most recent ESS rounds. What differences are there between the trends in this case, compared to those obtained from the data from all rounds?
ESS2_rounds <- ESS1 |> filter(essround %in% c(10,11))
View(ESS2_rounds)

lm_recentA <- lm(ESS2_rounds$imbgeco ~ essround, data = ESS2_rounds)

lm_recentB <- lm(ESS2_rounds$ppltrst ~ essround, data = ESS2_rounds)
summary(lm_recentA)
summary(lm_recentB)



# Step 6: Graphically represent the linear regressions performed in items 3, 4 and 5, in a scatter plot essround vs “dependent variable”, also showing the line relative to the linear regression, as exemplified in the course material.
# Graph for model lm_dep1
plot1 <- ggplot(ESS1, aes(x = essround, y = imbgeco)) +
  geom_point() + geom_smooth(method = "lm", col = "blue") +
  ggtitle("Trend of Imbgeco Over ESS Rounds")
print(plot1)

# Graph for model lm_dep2
plot2 <- ggplot(ESS1, aes(x = essround, y = ppltrst)) +
  geom_point() + geom_smooth(method = "lm", col = "red") +
  ggtitle("Trend of ppltrst Over ESS Rounds")
print(plot2)

# Graphy for model lm_cntry1A
plot3 <- ggplot(cntry1_data, aes(x = essround, y = imbgeco)) +
  geom_point() + geom_smooth(method = "lm", col = "orange") +
  ggtitle("Trend of imbgeco Over ESS Rounds")
print(plot3)

# Graphy for model lm_cntry1B
plot4 <- ggplot(cntry1_data, aes(x = essround, y = ppltrst)) +
  geom_point() + geom_smooth(method = "lm", col = "blue") +
  ggtitle("Trend of ppltrst Over ESS Rounds")
print(plot4)

# Graph for model lm_cntry2A
plot5 <- ggplot(cntry2_data, aes(x = essround, y = imbgeco)) +
  geom_point() + geom_smooth(method = "lm", col = "black") +
  ggtitle("Trend of imbgeco Over ESS Rounds")
print(plot5)

# Graph for model lm_cntry2B
plot6 <- ggplot(cntry2_data, aes(x = essround, y = ppltrst)) +
  geom_point() + geom_smooth(method = "lm", col = "green") +
  ggtitle("Trend of ppltrst Over ESS Rounds")
print(plot6)

# Grapy for the model lm_recentA
plot7 <- ggplot(ESS2_rounds, aes(x = essround, y = imbgeco)) +
  geom_point() + geom_smooth(method = "lm", col = "blue") +
  ggtitle("Trend of imbgeco Over ESS Rounds")
print(plot7)

# Graph for model lm_recentB
plot8 <- ggplot(ESS2_rounds, aes(x = essround, y = ppltrst)) +
  geom_point() + geom_smooth(method = "lm", col = "purple") +
  ggtitle("Trend of ppltrst Over ESS Rounds")
print(plot8)


# Step 7:  Perform linear regressions of each dependent variable with hhmmb, agea, eduyrs and essround as independent variables. 
lm_multiple1 <- lm(imbgeco ~ hhmmb + agea + eduyrs + essround, data = ESS1)
summary(lm_multiple1)

lm_multiple2 <- lm(ppltrst ~ hhmmb + agea + eduyrs + essround, data = ESS1)
summary(lm_multiple2)

# Step 7a:Try to improve the models obtained by removing some variables as well as by adding new variables constructed from the original ones (e.g., calculating an individual's body mass index from their weight and height), as exemplified in the course material. Explain each step of this process.
#Removal of variable hmmmb and essround
lm_multiple3 <- lm(imbgeco ~  agea + eduyrs, data = ESS1)
summary(lm_multiple3)

# Removal of hmmmb, essround and eduyrs
lm_multiple4 <- lm(ppltrst ~  agea, data = ESS1)
summary(lm_multiple4)

# Step 7b: adding new variables constructed from the original ones (e.g., calculating an individual's body mass index from their weight and height), as exemplified in the course material.
# Create a new variable: age_trust_diff
ESS1 <- ESS1 %>%
mutate(age_trust_diff = agea - ppltrst)
View(ESS1)

# Step 7c: Try to improve the model
mode1 <- lm(ppltrst ~ age_trust_diff + hhmmb + agea + eduyrs + essround, data = ESS1)
summary(mode1)

# Create a new variable: age_imbgeco_diff
ESS1 <- ESS1 %>%
mutate(age_imbgeco_diff = agea - imbgeco)
View(ESS1)

mode2 <- lm(imbgeco ~ age_imbgeco_diff + hhmmb + agea + eduyrs + essround, data = ESS1)
summary(mode2)

# Step 8: Model Comparison
summary(lm_multiple1)$adj.r.squared
summary(lm_multiple2)$adj.r.squared
summary(lm_multiple3)$adj.r.squared
summary(lm_multiple4)$adj.r.squared
summary(mode1)$adj.r.squared
summary(mode2)$adj.r.squared

# Step 8a: Store Adjusted R-squared values
adj_r_squared_values <- c(
  mode1 = summary(mode1)$adj.r.squared,
  mode2 = summary(mode2)$adj.r.squared,
  lm_multiple1 = summary(lm_multiple1)$adj.r.squared,
  lm_multiple2 = summary(lm_multiple2)$adj.r.squared,
  lm_multiple3 = summary(lm_multiple3)$adj.r.squared,
  lm_multiple4 = summary(lm_multiple4)$adj.r.squared
)

# Step 8b: Rank models based on Adjusted R-squared in descending order
ranked_models <- sort(adj_r_squared_values, decreasing = TRUE)
print(ranked_models)


# Step 9: For each model obtained in item 7, produce a scatter plot of “dependent variable” vs. residuals, in which each of the elements of the model's training sample is represented. Can you observe any pattern or trend?

# Graph for model lm_multiple1 vs residuals
plot_residuals1 <- ggplot(ESS1, aes(x = imbgeco, y = residuals(lm_multiple1 ))) +
  geom_point() + geom_hline(yintercept = 0, col = "red") +
  ggtitle("Residual Plot for Multiple Regression Model")
print(plot_residuals1)

# Graph for model lm_multiple2 vs residuals
plot_residuals2 <- ggplot(ESS1, aes(x = ppltrst, y = residuals( lm_multiple2 ))) +
  geom_point() + geom_hline(yintercept = 0, col = "blue") +
  ggtitle("Residual Plot for Multiple Regression Model")
print(plot_residuals2)

# Graph for model lm_multiple3 vs residuals
plot_residuals3 <- ggplot(ESS1, aes(x = imbgeco, y = residuals( lm_multiple3 ))) +
  geom_point() + geom_hline(yintercept = 0, col = "purple") +
  ggtitle("Residual Plot for Multiple Regression Model")
print(plot_residuals3)

# Graph for model lm_multiple4 vs residuals
plot_residuals4 <- ggplot(ESS1, aes(x = ppltrst, y = residuals( lm_multiple4 ))) +
  geom_point() + geom_hline(yintercept = 0, col = "red") +
  ggtitle("Residual Plot for Multiple Regression Model")
print(plot_residuals4)

#Graph for mode1 vs residuals
plot_residuals5 <- ggplot(ESS1, aes(x = ppltrst, y = residuals( mode1 ))) +
  geom_point() + geom_hline(yintercept = 0, col = "blue") +
  ggtitle("Residual Plot for Multiple Regression Model")
print(plot_residuals5)

# Graph for mode2 vs residuals
plot_residuals6 <- ggplot(ESS1, aes(x = imbgeco, y = residuals( mode2 ))) +
  geom_point() + geom_hline(yintercept = 0, col = "blue") +
  ggtitle("Residual Plot for Multiple Regression Model")
print(plot_residuals6)

