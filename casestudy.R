library(tidyverse)
library(caret)
library(randomForest)
library(mice)
library(ggplot2)
library(VIM)
library(naniar)
library(e1071)
require(pROC)
library(ROCR)

cs.train <- read.csv("Data/cs-training.csv")

cs.train <- cs.train %>%
  select(-1) %>%
  mutate(age = ifelse(cs.train$age == 0, 52.3, cs.train$age))


#Explore Data
str(cs.train)
summary(cs.train)
cs.train[cs.train$MonthlyIncome != "NA",]
cs.train[cs.train$RevolvingUtilizationOfUnsecuredLines > 20000,]
cs.train[cs.train$DebtRatio > 100000,]
cs.train[cs.train$NumberOfTime30.59DaysPastDueNotWorse > 80,]


#Plot Delinquency Ratio
temp1 <- as.data.frame(prop.table(table(cs.train$SeriousDlqin2yrs))) %>%
  rename(pay = Var1, total = Freq)
temp1 <- temp1 %>%
  mutate(pay = ifelse(temp1$pay == 0, "No", "Yes")) %>%
  mutate_if(is.double, round, 2)

ggplot(temp1, aes(x = pay, y = total)) +
  geom_bar(stat="identity", fill = "#772210", width=.5) +
  labs(title = "How many users are not able to pay back?", x = "User unable to pay", y = "Percentage") +
  geom_text(label = temp1$total, vjust = -0.4) +
  theme(axis.line = element_line(colour = "#00485d", size = 2, linetype = "solid"),
        panel.background = element_rect(fill = "#d4e2e9"), 
        plot.background = element_rect(fill = "#bfd3dc", color = "#bfd3dc"))


#Plot Age Histogram
ggplot(cs.train, aes(cs.train$age)) +
  geom_histogram(aes(y = ..density..),
                 col="black", 
                 fill="#772210") +
  geom_density(col= "#00485d", fill = "white", alpha = 0.1,
               size = 1) + 
  labs(title="Age Histogram") +
  labs(x="Age", y="Count") +
  scale_x_continuous(breaks = seq(min(cs.train$age), max(cs.train$age), by = 5)) +
  theme(axis.line = element_line(colour = "#00485d", size = 2, linetype = "solid"),
        panel.background = element_rect(fill = "#d4e2e9"), 
        plot.background = element_rect(fill = "#bfd3dc", color = "#bfd3dc"))


#Plot Mising Values
gg_miss_upset(cs.train)
gg_miss_var(cs.train)


#Plot Debt Ratio
nasub.not <- as.data.frame(subset(cs.train, !is.na(MonthlyIncome)))
nasub <- as.data.frame(subset(cs.train, is.na(MonthlyIncome)))

temp3 <- data.frame("var" = c("All", "Salary disclosed", "Salary not disclosed"),
                    "mean" = c(mean(cs.train$DebtRatio),
                               mean(nasub.not$DebtRatio),
                               mean(nasub$DebtRatio)))
temp3 <- temp3 %>%
  mutate_if(is.double, round, 2)

ggplot(temp3, aes(x = var, y = mean)) +
  geom_bar(stat="identity", fill = "#772210", width=.5) +
  labs(title = "Debt Ratio Differences", x = "Monthly Income", y = "Debt Ratio (%)") +
  geom_text(label = temp3$mean, vjust = -0.4) +
  theme(axis.line = element_line(colour = "#00485d", size = 2, linetype = "solid"),
        panel.background = element_rect(fill = "#d4e2e9"), 
        plot.background = element_rect(fill = "#bfd3dc", color = "#bfd3dc"))


#Prediction
##Naive Bayes

intrain.na <- sample(nrow(cs.train), nrow(cs.train)*0.8)
train.na <- cs.train[intrain.na, ]
test.na <- cs.train[-intrain.na, ]

str(train.na)

nb.model2 <- naiveBayes(factor(SeriousDlqin2yrs) ~ ., train.na, laplace = 1, na.action = na.pass)
nb.model2

prediction.nb2 <- predict(nb.model2, test.na)

nbtb2 <- table("prediction" = prediction.nb2, "actual" = test.na$SeriousDlqin2yrs)
nbtb2

acc.nb2 <- round((nbtb2[1] + nbtb2[4]) / nrow(test.na), 2)
rec.nb2 <- round(nbtb2[4] / (nbtb2[3] + nbtb2[4]), 2)
paste("Accuracy:", acc.nb2)
paste("Specificity:", rec.nb2)


##Random Forest
###Delete N/A rows
set.seed(123)
cs.train.del <- cs.train %>%
  filter(!is.na(MonthlyIncome)) %>%
  filter(!is.na(NumberOfDependents))

intrain.del <- sample(nrow(cs.train.del), nrow(cs.train.del)*0.8)
rf10k.train.del <- cs.train.del[intrain.del, ]
rf10k.test.del <- cs.train.del[-intrain.del, ]

rf_model.del <- randomForest(factor(SeriousDlqin2yrs) ~ ., data = rf10k.train.del)
prediction.del <- predict(rf_model.del, rf10k.test.del)
table.del <- table(prediction = prediction.del, actual= rf10k.test.del$SeriousDlqin2yrs)

acc.del <- round((table.del[1] + table.del[4]) / nrow(rf10k.test.del), 2)
rec.del <- round(table.del[4] / (table.del[3] + table.del[4]), 2)
paste("Accuracy:", acc.del)
paste("Specificity:", rec.del)

rf.roc.del <- roc(rf10k.train.del$SeriousDlqin2yrs,rf_model.del$votes[,2])
plot(rf.roc.del, print.auc = TRUE)


###Replace missing values using median
cs.train.median <- cs.train %>%
  mutate(MonthlyIncome = ifelse(is.na(cs.train$MonthlyIncome), 5400, cs.train$MonthlyIncome)) %>%
  mutate(NumberOfDependents = ifelse(is.na(cs.train$NumberOfDependents), 0, cs.train$NumberOfDependents))

intrain.median <- sample(nrow(cs.train.median), nrow(cs.train.median)*0.8)
rf10k.train.median <- cs.train.median[intrain.median, ]
rf10k.test.median <- cs.train.median[-intrain.median, ]

rf_model.median <- randomForest(factor(SeriousDlqin2yrs) ~ ., data = rf10k.train.median)
prediction.median <- predict(rf_model.median, rf10k.test.median)
table.median <- table(prediction = prediction.median, actual= rf10k.test.median$SeriousDlqin2yrs)

acc.median <- round((table.median[1] + table.median[4]) / nrow(rf10k.test.median), 2)
rec.median <- round(table.median[4] / (table.median[3] + table.median[4]), 2)
paste("Accuracy:", acc.median)
paste("Specificity:", rec.median)

rf.roc.median <- roc(rf10k.train.median$SeriousDlqin2yrs,rf_model.median$votes[,2])
plot(rf.roc.median, print.auc = TRUE)


###Replace missing values with -1
cs.train.min <- cs.train %>%
  mutate(MonthlyIncome = ifelse(is.na(cs.train$MonthlyIncome), -1, cs.train$MonthlyIncome)) %>%
  mutate(NumberOfDependents = ifelse(is.na(cs.train$NumberOfDependents), -1, cs.train$NumberOfDependents))

intrain.min <- sample(nrow(cs.train.min), nrow(cs.train.min)*0.8)
rf10k.train.min <- cs.train.min[intrain.min, ]
rf10k.test.min <- cs.train.min[-intrain.min, ]

rf_model.min <- randomForest(factor(SeriousDlqin2yrs) ~ ., data = rf10k.train.min)
prediction.min <- predict(rf_model.min, rf10k.test.min)
str(prediction.min)
table.min <- table(prediction = prediction.min, actual= rf10k.test.min$SeriousDlqin2yrs)

acc.min <- round((table.min[1] + table.min[4]) / nrow(rf10k.test.min), 2)
rec.min <- round(table.min[4] / (table.min[3] + table.min[4]), 2)
paste("Accuracy:", acc.min)
paste("Specificity:", rec.min)

rf.roc.min <- roc(rf10k.train.min$SeriousDlqin2yrs,rf_model.min$votes[,2])
plot(rf.roc.min, print.auc = TRUE)


###Create Balanced Data and predict
train30k.no <- cs.train[(is.na(cs.train$MonthlyIncome)), ]
train30k.yes <- cs.train[sample(which(!is.na(cs.train$MonthlyIncome)),30000),]
train.merge <- rbind(train30k.no, train30k.yes)

cs.train.cat <- train.merge %>%
  mutate(MonthlyIncome = ifelse(is.na(train.merge$MonthlyIncome), 5400, train.merge$MonthlyIncome)) %>%
  mutate(NumberOfDependents = ifelse(is.na(train.merge$NumberOfDependents), 0, train.merge$NumberOfDependents))

intrain.cat <- sample(nrow(cs.train.cat), nrow(cs.train.cat)*0.8)
train.cat <- cs.train.cat[intrain.cat, ]
test.cat <- cs.train.cat[-intrain.cat, ]

rf_model.cat <- randomForest(factor(SeriousDlqin2yrs) ~ ., data = train.cat, ntree = 800)
prediction.cat <- predict(rf_model.cat, test.cat)
table.cat <- table(prediction = prediction.cat, actual= test.cat$SeriousDlqin2yrs)

acc.cat <- round((table.cat[1] + table.cat[4]) / nrow(test.cat), 2)
rec.cat <- round(table.cat[4] / (table.cat[3] + table.cat[4]), 2)
paste("Accuracy:", acc.cat)
paste("Specificity:", rec.cat)

rf.roc.cat<- roc(train.cat$SeriousDlqin2yrs,rf_model.cat$votes[,2])
plot(rf.roc.cat, print.auc = TRUE)


#Plot Variable Importance
importance <- importance(rf_model.median)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = "darkred") +
  labs(x = 'Variables') +
  coord_flip()
