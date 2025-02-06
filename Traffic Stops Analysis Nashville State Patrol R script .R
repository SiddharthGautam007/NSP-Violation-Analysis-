# clears global environment
rm(list = ls())  
# clears packages
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) 
# clears plots
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) 
# disables scientific notion for entire R session
options(scipen = 100) 
# clears console
cat("\014")  

install.packages('pacman')
library(pacman)
#need tidyverse to run "summarise". If doesn't work try p_load(tidyverse)
install.packages('tidyverse')
library(tidyverse)
#p_load(tidyverse)


data <- readRDS("C:\\cps\\yg821jf8611_tn_nashville_2020_04_01.rds")
View(data)
#Reduce my table to feature only relevant columns
data <- data[, c("raw_row_number", "date", "time", "location", "lat", 
                 "lng", "precinct", 
                 "zone", "subject_age","type","violation","outcome","subject_sex","frisk_performed","arrest_made","subject_race")]

#change column as factor:
data$violation <- factor(data$violation)

###Examples of other ways to change column types. these already are these types but it's good to know the functions###
data$date <- as.Date(data$date)
data$subject_sex <- factor(data$subject_sex)
data$type <- factor(data$type)
data$outcome <- factor(data$outcome)
###################################################################

# Add new column with day of week
data <- data |> mutate(dayofweek = wday(date, label = TRUE))

# Frequency table of stops by age
table(data$subject_age)

age <- data |> group_by(subject_age) |> summarize(counts = n())

# Cross-tabulation of age and day of week
table(data$subject_age, data$subject_sex)

#remove Other, Unknown and NA from table
data <- data |> filter(subject_sex != 'unknown', 
                       subject_sex != 'other', subject_sex != 'NA')

# More advanced cross-tabulation
install.packages('gmodels')
library(gmodels)
library(ggplot2) #library for visualizations


# Bar Chart of stops by day of week
ggplot(data, aes(x = dayofweek)) +
  geom_bar(fill = "yellow", color = "black") +
  labs(title = "Distribution of Traffic Stops", 
       x = "Day of Week", y = "Frequency")


# Bar plot of stops by age
ggplot(data, aes(x = subject_age, fill = subject_age)) +
  geom_bar() +
  labs(title = "Traffic Stops by age", x = "Age", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Faceted histogram using ggplot2 to compare across multiple classes
ggplot(data, aes(x = dayofweek)) +
  geom_bar(fill = "lightblue", color = "black") +
  facet_wrap(~ subject_sex) +
  labs(title = "Distribution of Stop Times by sex",
       x = "Time of Day",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Calculate the proportion of searches for each sex
search_proportions <- data |>
  group_by(subject_sex) |>
  summarise(search_rate = n(), na.rm = TRUE)


#install package formattable to use percent function below
install.packages('formattable')
library("formattable") 


####More interactive style plots#####
install.packages('plotly')
library(plotly)
p <- ggplot(data, aes(x = subject_sex, fill = violation)) +
  geom_bar(position = "dodge") +
  labs(title = "violation based on sex", x = "sex", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


library(ggplot2)
ggplotly(p)

library(psych)
library(dplyr)
library(tidyverse)
library(janitor)
age <- data |> group_by(subject_age) |> summarize(counts = n())

data <- data |> filter(subject_age != 'NA') #remove NAs

data <- data |> filter(subject_age > 16) #remove age less than legal driving age (16 in MA)


overall_stats <- t(psych::describe(data$subject_age))
median(data$subject_age)


## By group ('subject_sex')
group_stats <- data %>%
  group_by(subject_sex) %>%
  summarise(
    mean_age = mean(subject_age, na.rm = TRUE),
    sd_age = sd(subject_age, na.rm = TRUE),
    min_age = min(subject_age, na.rm = TRUE),
    max_age = max(subject_age, na.rm = TRUE),
    N = n()
  )


# Print statistics in a three-line table format
 -print(knitr::kable(group_stats, format = "pipe"))




######################Visualizations####################################


outcome <- data |> group_by(outcome)|> summarize(counts = n())
data_outcome <- data[, c("outcome", "subject_age")] |> 
  filter(outcome != 'NA')

## Scatter plot of age vs. outcome (using jiter to handle categorical outcome)
png("scatter_plot.png", width = 800, height = 600)
ggplot(data_outcome, aes(x = subject_age, y = outcome)) +
  geom_jitter(width = 0.3, alpha = 0.5) +
  labs(title = "Age vs. Outcome",
       x = "Age", y = "Outcome")
dev.off()



#Reduce table to new outcome table with only the two columns needed for the chart
data_outcome <- data[, c("outcome", "subject_age")] |> 
  filter(outcome != 'NA')


# Box plot of age by violation 
png("boxplot.png", width = 800, height = 600)
boxplot(subject_age ~ subject_sex , data = data,
        main = "Age Distribution by sex",
        xlab = "sex", ylab = "age")
dev.off()


# Jitter plot for search violation by age
png("jitter_plot.png", width = 800, height = 600)
ggplot(data, aes(x = subject_age, y = violation)) +
  geom_jitter(width = 0.3, alpha = 0.5) +
  labs(title = "Violation by Different Age Group",
       x = "AGE", y = "VIOLAION")
dev.off()



library(dplyr)

group_stats_mean <- data %>%
  summarise(
    mean_age = mean(subject_age, na.rm = TRUE),
    sd_age = sd(subject_age, na.rm = TRUE),
    min_age = min(subject_age, na.rm = TRUE),
    max_age = max(subject_age, na.rm = TRUE),
    N = n())
##########t-test##########################

t_test_age <- t.test(data$subject_age, mu = 37, alternative = "two.sided")
print(t_test_age)

if(t_test_age$p.value < 0.05) {
  cat("Since the p-value is less than 0.05, we reject the null hypothesis. 
      There is evidence to suggest that the mean age of individuals stopped is significantly different from 37 years.\n")
} else {
  cat("Since the p-value is greater than or equal to 0.05, we fail to reject the null hypothesis. 
      There is no significant evidence to suggest that the mean age differs from 37 years.\n")
}

t_test_age_2 <- t.test(data$subject_age, mu =  37.11991 , alternative = "two.sided")


if(t_test_age_2$p.value < 0.05) {
  cat("Since the p-value is less than 0.05, we reject the null hypothesis. 
      There is evidence to suggest that the mean age of individuals stopped 
      is significantly different from 37.11991 years.\n")
} else {
  cat("Since the p-value is greater than or equal to 0.05, we fail to reject the null hypothesis. 
      There is no significant evidence to suggest that the mean age differs from 37.11991 years.\n")
}

race <- data |> group_by(subject_race) |> summarize(counts = n())
 
########## hypo 1 #####################


table(data$violation,data$subject_sex)

observed_1<- matrix(c(904205,630750,4743,2986),nrow = 2,byrow = TRUE,
                    dimnames = list(violation=c("moving traffic violation","parking violation"),
                                    gender=c("male","female")))
print(observed_1)

chi_test_1<-chisq.test(observed_1)
print(chi_test_1)

if(chi_test_1$p.value<0.05){
  cat("Since the p-value is less than 0.05, we reject the null hypothesis.
      there is significane evidence to suggest that moving traffic violation are more
      than parking violation by both male and female.\n")
} else{
  cat("Since the p-value is more than 0.05, we fail the null hypothesis.
      there is no evidence to suggest that moving traffic violation are more
      than parking violation by both male and female.\n")
}


########## hypo 2#####################

table(data$subject_race, data$subject_sex)

observed_2 <- matrix(c(642128,516630 ,1000892 ,658170 ), nrow = 2, byrow = TRUE,
                   dimnames = list(Race = c("Black", "White"),
                                   Gender = c("male", "female")))

print(observed_2)

chi_test_2<-chisq.test(observed_2)

print(chi_test_2)


if(chi_test_2$p.value < 0.05) {
  cat("Since the p-value is less than 0.05, we reject the null hypothesis.
      There is evidence to suggest that their are more Black male drivers than White drivers.\n")
} else {
  cat("Since the p-value is greater than or equal to 0.05, we fail to reject the null hypothesis.
      There is no significant evidence to suggest that their are more Black male drivers than White drivers.\n")}


######## hypo 3#####################
table(data$subject_race,data$outcome)

observed_3<- matrix(c(284,4725),nrow = 1,byrow = TRUE,
                    dimnames = list(outcome=c("arrest"),
                                    race=c("asian/pacific islander","hispanic")))
print(observed_3)


chi_test_3<-chisq.test(observed_3)
print(chi_test_3)


if(chi_test_3$p.value<0.05){
  cat("Since the p-value is less than 0.05, we reject the null hypothesis
      so their is significance evidence that hispanic people got arrested more as an outcome than asian people")
}else{cat("Since the p-value is more than 0.05, we fail the null hypothesis.
so their is no  significance evidence that hispanic people got arrested more as an outcome than asian people"
)}



################################# Correlation ##################################

# clears global environment
rm(list = ls())  
# clears packages
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) 
# clears plots
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) 
# disables scientific notion for entire R session
options(scipen = 100) 
# clears console
cat("\014")  

library(dplyr)
library(corrplot)
library(tidyverse)
library(caret)
library(scales)
library(pROC)
data1 <- readRDS("C:\\cps\\yg821jf8611_tn_nashville_2020_04_01.rds")



#correlation----
sd_data_typed <- data1 %>%
  mutate(
    subject_age = as.numeric(subject_age),
    contraband_found = as.numeric(contraband_found),
    search_conducted = as.numeric(search_conducted),
    citation_issued = as.numeric(citation_issued),
    warning_issued = as.numeric(warning_issued),
    arrest_made = as.numeric(arrest_made)
  )



sd_numeric <- sd_data_typed %>%
  select_if(is.numeric) %>%
  na.omit()


# Remove zero-variance columns and recalculate correlation
sd_numeric_clean <- sd_numeric %>%
  select(subject_age, search_conducted, citation_issued, 
         warning_issued, arrest_made, contraband_found) %>%
  filter(!is.na(search_conducted))



# Create correlation matrix
cor_matrix <- cor(sd_numeric_clean, use = "complete.obs")
print(round(cor_matrix, 2))


# Visualize correlation matrix

corrplot(cor_matrix, 
         method = "color",
         type = "upper",
         addCoef.col = "red",
         number.cex = 0.7,
         tl.col = "black",
         tl.srt = 45,
         title = "Nashville Police Stops Correlation Matrix",
         mar = c(0, 0, 2, 0),  
         cl.lim = c(-1, 1), 
         cl.length = 20,    
         cl.cex = 0.7     
)


# Create multiple scatter plots for key relationships


# 1. Citation vs Warning 
ggplot(sd_numeric_clean, aes(x = citation_issued, y = warning_issued)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "cyan") +
  labs(title = "Citation vs Warning",
       x = "Citation Issued",
       y = "Warning Issued") +
  theme_minimal()

# 2. Arrest vs Contraband 
ggplot(sd_numeric_clean, aes(x = contraband_found, y = arrest_made)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "green") +
  labs(title = "Contraband vs Arrest",
       x = "Contraband Found",
       y = "Arrest Made") +labs(title = "Contraband vs Arrest",
                                x = "Contraband Found",
                                y = "Arrest Made") +
  theme_minimal()



# Model 1: Predicting arrests

arrest_model <- glm(arrest_made ~ contraband_found + citation_issued + warning_issued,
                    family = "binomial",
                    data = sd_numeric_clean)

summary(arrest_model)


# Calculate model performance metrics
# For arrest model
arrest_pred <- predict(arrest_model, type = "response")
arrest_pred_class <- ifelse(arrest_pred > 0.5, 1, 0)
cm <- confusionMatrix(factor(arrest_pred_class), factor(sd_numeric_clean$arrest_made))
cm_df <- as.data.frame(cm$table)

# Plot the confusion matrix
ggplot(cm_df, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = 1) +
  scale_fill_gradient(low = "white", high = "pink") +
  labs(title = "Confusion Matrix")


roc_curve <- roc(sd_numeric_clean$arrest_made, arrest_pred)
plot(roc_curve, main = "ROC Curve for Arrest Prediction Model")
auc(roc_curve)
theme_minimal()
  





