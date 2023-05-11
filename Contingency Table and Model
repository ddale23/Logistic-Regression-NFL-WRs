#libraries
library(ggplot2)
library(dplyr)
library(sjPlot)
library(MASS)
library(vcdExtra)
library(caret)


## bring in base data sets
draft = read.csv("https://raw.githubusercontent.com/ddale23/Logistic-Regression-NFL-WRs/main/completed_draft.csv")
stats = read.csv("https://raw.githubusercontent.com/ddale23/Logistic-Regression-NFL-WRs/main/completed_stats.csv")

draft %>%
  group_by(same_team) %>%
  summarize(n())
  

## build contingency table
d_counts = draft %>% group_by(round_draft, same_team) %>% 
  count()

round_table = as.table(matrix(c(d_counts$n), nrow = 2, ncol = 7, byrow = FALSE, 
                              dimnames = list(Same_Team = c("No","Yes"), Draft_Round = c(1,2,3,4,5,6,7))))

round_table = addmargins(round_table)


## plots

# WRs Drafted and 5-Year Retention by Draft Age
draft %>%
  ggplot() + geom_bar(aes(x = as.factor(age_draft), fill = as.factor(same_team), ), position = 'dodge') + 
  labs(title = "WRs Drafted and 5-Year Retention By Draft Age", x = "Draft Age", fill = "Same Team") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


#WR 5 Year Retention by Team 
draft %>%
  ggplot() + geom_bar(aes(y = clean_team_draft, fill = as.factor(same_team))) + 
  labs(title = "WR 5-Year Retention By Team", y = "Team", fill = "Same Team") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


#WRs drafted and 5 year retention by Draft Round
draft %>%
  ggplot() + geom_bar(aes(x = as.factor(round_draft), fill = as.factor(same_team), ), position = 'dodge') + 
  labs(title = "WRs Drafted and 5-Year Retention By Draft Round", x = "Draft Round", fill = "Same Team") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


#Box and Whisker Distribution of WR Draft Picks based on 5 Year Retention
draft %>%
  ggplot() + geom_boxplot(aes(x = as.factor(same_team), y = pick_draft, fill = as.factor(same_team))) + 
  labs(title = "Distribution of WR Draft Picks Based on 5-Year Retention", x = "Same Team After 5 Years", 
       y = "Draft Pick Number", fill = "Same Team After 5 Years") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


#WR Draft Pick by Year and Round
draft %>%
  ggplot() + geom_point(aes(x = pick_draft, y = season_draft, col = as.factor(same_team))) + 
  facet_wrap(~as.factor(round_draft), ncol = 1, strip.position = "right") + 
  labs(title = "WR Draft Pick By Year and Round", subtitle = "Colored by 5-Year Retention", 
       x = "Draft Pick", y = "Draft Year", col = "Same Team After 5 Years") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "bold"))


## Test for Independence
sjPlot::tab_xtab(var.row = draft$same_team, var.col = draft$round_draft, 
                 title = "Crosstabs for Draft Round and 5-Year Retention", show.row.prc = TRUE)

round_table.loglm <- loglm(~Same_Team + Draft_Round, data = round_table)
round_table.loglm




## Logistic Model

#replace null values with 0 
stats = replace(stats, is.na(stats),0)

#set seed
set.seed(1234)

#build test and training models
intrain = createDataPartition(y = stats$X, p = .75, list = FALSE)
training = stats[intrain,]
testing = stats[-intrain,]


str(training)
str(testing)


## Data Viz
#Density plot of total Receiving Yards in first four seasons among WRs on same team versus not
stats %>%
  ggplot(aes( x = total_rec_yards, fill = factor(same_team))) + 
  geom_density(alpha = 0.5) +
  theme_bw() + 
  labs(title = "Density Plot: Total Receiving Yards in First 4 Seasons",
       subtitle = "Among WRs who were on the same team that drafted them heading into year 5 versus not",
       x = "Total Receiving Yards",
       y = "Density") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "bold"))

#Density plot of total receptions in first four seasons among WRs on same team versus not
stats %>%
  ggplot(aes( x = total_receptions, fill = factor(same_team))) + 
  geom_density(alpha = 0.5) +
  theme_bw() + 
  labs(title = "Density Plot: Total Receptions in First 4 Seasons",
       subtitle = "Among WRs who were on the same team that drafted them heading into year 5 versus not",
       x = "Total Receptions",
       y = "Density") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "bold"))


## Bar Plot of WRs drafted by team and number retained
ggplot(stats, aes(y = clean_team_draft, fill = factor(same_team))) +
  geom_bar()+
  labs(y = "Team", title = "WRs Drafted and 5 Year Retention by Team",  fill  = "Same Team?")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) 




# stepwise model
statstrainmodel = glm(as.factor(same_team) ~ total_receptions + as.factor(round_draft) + total_targets + total_rec_yards + 
                        total_receiving_tds + total_yac + avg_tgt_share + avg_ay_share + age_draft, 
                      data = training, family = binomial)

step.model = statstrainmodel %>%
  stepAIC(trace= FALSE)

summary(step.model)



### Why is the model only using one predictor?

# first we make a model using both total receiving yards and total receptions
model_two_params = glm(same_team ~ total_rec_yards + total_receptions, data = training, family = binomial)
summary(model_two_params)

# Not a significant variable when using both total_rec_yards & total_receptions

# next, we compare a glm using only total receptions to the stepwise model that only uses total_rec_yards
model_just_receptions = glm(same_team ~ total_receptions , data = training, family = binomial)
summary(model_just_receptions)

# A worse (aka Higher) AIC when just using total_receptions




# Evaluation of model
pred_same_team_train = predict(step.model, newdata = training, type = "response")
pred_class_same_team_train = ifelse(pred_same_team_train > .5, 1, 0)

train_accuracy = mean(pred_class_same_team_train == training$same_team)

pred_same_team_test = predict(step.model, newdata = testing, type = "response")
pred_class_same_team_test = ifelse(pred_same_team_test > .5, 1, 0)

test_accuracy = mean(pred_class_same_team_test == testing$same_team)
cat("Training accuracy:", round(train_accuracy, 4)*100, '%.', "Testing accuracy:", round(test_accuracy, 4)*100, '%.')

actual_team = testing$same_team

#Contingency Table
cm = table(pred_class_same_team_test, actual_team)
cm

#Sensitivity and Precision
sensitivity(cm)
precision(cm)






