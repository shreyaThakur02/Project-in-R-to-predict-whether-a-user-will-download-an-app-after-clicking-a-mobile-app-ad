library(dplyr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(mltools)
library(data.table)
library(caret)
library(ROCR) 
library(knitr)
library(rmarkdown)
library("caTools")
train_data <- read.csv(file = 'C:/Users/HP/Dropbox/My PC (LAPTOP-OPQMBV8V)/Downloads/t.csv', header = T)
#test_set <- fread(file = 'C:/Users/HP/Dropbox/My PC (LAPTOP-OPQMBV8V)/Downloads/train_sampl.csv', header = T, nrows = 1e7)
set.seed(123)
sample=sample.split(train_data,SplitRatio = 0.8)
train_set=subset(train_data,sample=TRUE)
test_set=subset(train_data,sample=FALSE)


any(is.na(train_set))
any(is.na(test_set))
dim(train_set)
head(train_set)
str(train_set)
dim(test_set)
head(test_set)
str(test_set)
train_set$is_attributed <- as.factor(train_set$is_attributed)
test_set$is_attributed <- as.factor(test_set$is_attributed)
table(train_set$is_attributed)

summary(train_set)
summary(test_set)

length(unique(train_set$ip))
length(unique(test_set$ip)) 
head(rev(sort(table(train_set$ip))))
head(rev(sort(table(test_set$ip))))

dupl_ips_train <- train_set[duplicated(train_set$ip), 1]
length(dupl_ips_train)

length(unique(dupl_ips_train))

round(prop.table(table(train_set$is_attributed[train_set$ip %in% 
                                                 unique(dupl_ips_train)])) * 100, 2)
dupl_ips_test <- train_set[duplicated(test_set$ip), 1]
length(dupl_ips_test)

length(unique(dupl_ips_test))

round(prop.table(table(train_set$is_attributed[train_set$ip %in% 
                                                 unique(dupl_ips_test)])) * 100, 2)

n_dupl_ips_train <- train_set %>%
  count(ip, wt = n() ) %>%
  arrange(desc(n))
head(n_dupl_ips_train)
sum(n_dupl_ips_train$n)
sum(n_dupl_ips_test$n)
train_set <- left_join(train_set, n_dupl_ips_train, by = 'ip')
head(train_set)
test_set <- left_join(test_set, n_dupl_ips_test, by = 'ip')
head(test_set)
names(train_set)[9] <- 'repetitions'
labels(train_set)[[2]]
names(test_set)[9] <- 'repetitions'
labels(test_set)[[2]]

c = 1
values <- unique(n_dupl_ips_train$n)
df <- data.frame(repetitions = rep(NA, length(values)))

for (i in values) {
  df$repetitions[c] <- i
  tab <- table(train_set$is_attributed[train_set$repetitions == i])
  df$no[c] <- tab[1]
  df$no_prop[c] <- round(tab[1] * 100 / sum(tab), 2)
  df$yes[c] <- tab[2]
  df$yes_prop[c] <- round(tab[2] * 100 / sum(tab), 2)
  c = c + 1
}
sum(df$no, df$yes)
df_prop <- df %>% 
  filter(yes_prop > 0) %>%
  arrange(desc(yes_prop))

df_prop

brks <- cut(df_prop$repetitions, breaks = c(0, 5, 10, 100, 700))
ggplot(data = df_prop) +
  geom_point(aes(no/1000, yes, color = brks,
                 size = yes_prop), alpha = 0.8) +
  xlab('no (x10³)') +
  scale_color_manual(values = c(1,3,2,4)) +
  scale_size(breaks = c(0.5, 1, 2)) +
  coord_trans(x = 'log', y = 'log') +
  ggtitle('The app was downloaded') +
  theme_linedraw()

ggplot(data = df_prop) +
  geom_point(aes(repetitions, yes_prop, size = yes_prop), alpha = 0.8) +
  scale_color_manual(values = c(1,3,2,4)) +
  scale_size(breaks = c(0.5, 1, 2)) +
  coord_trans(x = 'log') +
  ggtitle('The app was downloaded') +
  theme_linedraw()
gt <- df_prop$repetitions[df_prop$yes_prop > 0.4]

train_set <- train_set %>% 
  mutate(yes_prop = ifelse(repetitions %in% gt, 1, 0))

train_set$click_time <- as.Date(train_set$click_time, format = '%Y-%m-%d')
unique(months(train_set$click_time))  
unique(year(train_set$click_time)) 
unique(day(train_set$click_time))
unique(weekdays(train_set$click_time))
test_set$click_time <- as.Date(test_set$click_time, format = '%Y-%m-%d')
unique(months(test_set$click_time))
unique(year(test_set$click_time))
unique(day(test_set$click_time))
unique(weekdays(test_set$click_time))
train_set$click_day <- day(train_set$click_time)
test_set$click_day <- day(test_set$click_time)
ggplot(train_set, aes(click_day)) +
  geom_histogram(binwidth = 1, fill = 'green', col = 'black', alpha = 0.6) +
  theme_bw()
train_set %>%
  count(click_day)
train_set$attributed_time <- ymd_hms(train_set$attributed_time)
train_set$attributed_day <- day(train_set$attributed_time)
train_set$attributed_hour <- hour(train_set$attributed_time) + 
  ifelse(minute(train_set$attributed_time) >= 30, 1, 0)
ggplot(train_set, aes(attributed_day, attributed_hour, 
                      fill = cut(attributed_hour, breaks = c(0,12,18,24)))) +
  geom_bar(stat = "identity", alpha = 0.6)
train_set %>%
  count(attributed_day)
hist(train_set$attributed_hour[train_set$attributed_day == 7], 
     col = rgb(1,0,0,0.5), breaks = 24,
     main = 'Histogram of the app downloaded per hour', xlab = 'Hour')
hist(train_set$attributed_hour[train_set$attributed_day == 8], 
     col = rgb(0,0,1,0.5), breaks = 24, add = T)
legend(x = "topright", legend = c('Day 7','Day 8'), 
       col = c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)), pch = 15)
hist(train_set$attributed_hour, 
     breaks = 24, main = 'Histogram of the app downloaded per hour in the two days', 
     xlab = 'Hour')
div_app<- bin_data(c(train_set$app, test_set$app), bins = 4, binType = "quantile")
levels(div_app)
## [1] "[0, 3)"    "[3, 12)"   "[12, 15)"  "[15, 675]"

train_set$app_fac <- cut(train_set$app, breaks = c(0, 3, 12, 18, nrow(train_set)), 
                         right = F, labels = c(1, 2, 3, 4))

test_set$app_fac <- cut(test_set$app, breaks = c(0, 3, 12, 18, nrow(test_set)), 
                        right = F, labels = c(1, 2, 3, 4))
plot(train_set$app_fac, xlab = 'App id class (train data set)', ylab = 'Frequency')
plot(test_set$app_fac, xlab = 'App id class (test data set)', ylab = 'Frequency')
sort(unique(train_set$device))
sort(unique(test_set$device))
summary(train_set$device)
summary(test_set$device)
hist(train_set$device, freq = F, breaks = 40, col = rgb(1,0,0,0.5),
     main = 'Device histograms', xlab = 'Devices')
hist(test_set$device, freq = F, breaks = 40, col = rgb(0,0,1,0.5), add = T)
legend(x = "topright", legend = c('Train data set','Test data set'), 
       col = c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)), pch = 15)
a <- train_set %>%
  count(device, sort = T)
head(a)
b <- test_set %>%
  count(device, sort = T)
head(b)
( a[1,2]/sum(a) )
( b[1,2]/sum(b) )
class_device <- function(x) {ifelse(x == 1, 1, 2)} 
train_set$device_fac <- as.factor(class_device(train_set$device))
levels(train_set$device_fac)
test_set$device_fac <- as.factor(class_device(test_set$device))
levels(test_set$device_fac)
sort(unique(train_set$os))
sort(unique(test_set$os))
summary(train_set$os)
summary(test_set$os)
hist(train_set$os, freq = F, xlim = c(0,800), ylim = c(0, 0.07), breaks = 100, 
     col = rgb(1,0,0,0.5), main = 'OS histograms', xlab = 'OS id')
hist(test_set$os, freq = F, xlim = c(0,800), breaks = 50,
     col = rgb(0,0,1,0.5), add = T)
legend(x = "topright", legend = c('Train data set','Test data set'), 
       col = c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)), pch = 15)
hist(train_set$os, freq = F, xlim = c(0, 100), ylim = c(0, 0.07), breaks = 100, 
     col = rgb(1,0,0,0.5), main = 'OS histograms', xlab = 'OS id')
hist(test_set$os, freq = F, xlim = c(0,100), breaks = 50,
     col = rgb(0,0,1,0.5), add = T)
legend(x = "topright", legend = c('Train data set','Test data set'), 
       col = c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)), pch = 15)
a <- train_set %>%
  count(os, sort = T)
head(a)
b <- test_set %>%
  count(os, sort = T)
head(b)
( (a[1,2] + a[2,2]) / sum(a) )
( (b[1,2] + b[2,2]) / sum(b) )
class_os <- function(x) {
  if (x == 13) {2}
  else if (x == 19) {3}
  else if (x > 19) {4}
  else {1}
}
train_set$os_fac <- as.factor(sapply(train_set$os, class_os))
plot(train_set$os_fac, xlab = 'OS classes (train data set)', ylab = 'Frequency')
test_set$os_fac <- as.factor(sapply(test_set$os, class_os))
plot(test_set$os_fac, xlab = 'OS classes (test data set)', ylab = 'Frequency')
sort(unique(train_set$channel))
sort(unique(test_set$channel))
summary(train_set$channel)
summary(test_set$channel)
hist(train_set$channel, freq = F, ylim = c(0, 0.01), breaks = 20, 
     col = rgb(1,0,0,0.5), main = 'Channel histograms', xlab = 'Channel id')
hist(test_set$channel, freq = F, breaks = 20,
     col = rgb(0,0,1,0.5), add = T)
legend(x = "topright", legend = c('Train data set','Test data set'), 
       col = c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)), pch = 15)
a <- train_set %>%
  count(channel, sort = T)
head(a)
b <- test_set %>%
  count(channel, sort = T)
head(b)
div_channel <- bin_data(c(train_set$channel, test_set$channel), 
                        bins = 4, binType = "quantile")
levels(div_channel)
train_set$channel_fac <- cut(train_set$channel, 
                             breaks = c(0, 135, 236, 401, nrow(train_set)),
                             right = F, labels = c(1, 2, 3, 4))

test_set$channel_fac <- cut(test_set$channel, 
                            breaks = c(0, 135, 236, 401, nrow(test_set)),
                            right = F, labels = c(1, 2, 3, 4))
plot(train_set$channel_fac, xlab = 'Channel id class (train data set)', 
     ylab = 'Frequency')
plot(test_set$channel_fac, xlab = 'Channel id class (test data set)', 
     ylab = 'Frequency')
dim(train_set)
any(is.na(train_set[,1:6]))
any(is.na(train_set[,8:11]))
any(is.na(train_set[,14:17]))
any(is.na(train_set[,7]))
labels(train_set)[[2]][7]
head(unique(train_set$attributed_time))
any(is.na(train_set[,12]))
labels(train_set)[[2]][12]
unique(train_set$attributed_day)
any(is.na(train_set[,13]))
labels(train_set)[[2]][13]
unique(train_set$attributed_hour)

n <- nrow(train_set[train_set$is_attributed == 1, ])
n
train_no <- train_set %>%
  filter(is_attributed == 0) %>%
  slice_sample(n = n, replace = F)
nrow(train_no)
train_yes <- train_set %>%
  filter(is_attributed == 1)
nrow(train_yes)
train_set1 <- rbind(train_no, train_yes)
train_set1 <- train_set1 %>% 
  slice_sample(n = nrow(train_set1), replace = F)
nrow(train_set1)/2
rm(list = setdiff(ls(), c('train_set', 'train_set1', 'test_set')))
ls()
gc()

# Predictions


plot.roc.curve <- function(predictions, title.text){
  perf <- performance(predictions, "tpr", "fpr")
  plot(perf, col = "black", lty = 1, lwd = 2, 
       main = title.text, cex.main = 0.6, 
       cex.lab = 0.8, xaxs="i", yaxs="i")
  abline(0,1, col = "red")
  auc <- performance(predictions, "auc")
  auc <- unlist(slot(auc, "y.values"))
  auc <- round(auc,2)
  legend(0.4,0.4, legend = c(paste0("AUC: ",auc)), cex = 0.6, bty = "n", box.col = "white")
  
}

plot.pr.curve <- function(predictions, title.text){
  perf <- performance(predictions, "prec", "rec")
  plot(perf, col = "black", lty = 1, lwd = 2,
       main = title.text, cex.main = 0.6, cex.lab = 0.8, xaxs = "i", yaxs = "i")
}

#Logistic regression model with the most significant variables
model2 <- glm(is_attributed ~ repetitions + device_fac + os_fac, 
              data = train_set, 
              family = "binomial")

# Summary of the model
summary(model2)
# Predictions

predictions2 <- predict(model2, train_set1, type="response")
predictions2 <- round(predictions2)

# Evaluation
confusionMatrix(as.factor(predictions2), 
                reference = train_set1$is_attributed, positive = '1')

# Criando curvas ROC
predictions2_roc <- prediction(predictions2, train_set1$is_attributed)


par(mfrow = c(1,2))
plot.roc.curve(predictions2_roc, title.text = "Curva ROC")
plot.pr.curve(predictions2_roc, title.text = "Curva Precision/Recall")
par(mfrow = c(1,1))

#Regression Tree model with the most significant variables
# Evaluation of the most important features for the model
model12 <- train(is_attributed ~ repetitions + app_fac + 
                   device_fac + os_fac + channel_fac, 
                 data = train_set1,
                 method = 'rpart')
varImp(model12)
library("rpart")
# Regression Trees model with the most significant variables
model12 <- rpart(is_attributed ~ repetitions + app_fac + 
                   device_fac + channel_fac, 
                 data = train_set)

# Predictions
predictions12 <- predict(model12, train_set1, type="class")

# Evaluation
confusionMatrix(predictions12, 
                reference = train_set1$is_attributed, positive = '1')


# Random forest model with the balanced target variable by SMOTE

library(DMwR)

table(train_set$is_attributed) 

test_set1<-train_set1
library(randomForest)
# Random forest model
model15 <- randomForest(is_attributed ~ repetitions * app + 
                          channel * app_fac, 
                        data = train_set, 
                        ntree = 30,
                        nodesize = 1)

#View(test_set)
# Predictions
predictions15 <- predict(model15, test_set1, type="class")

# Evaluation
confusionMatrix(predictions15, 
                reference = test_set1$is_attributed, positive = '1')








