# Q1
library(MASS)
library(dplyr)
library(ggplot2)
itemdata <- read.csv("C:/Users/75581/Desktop/1/MALD1_SelectedItemData.csv", sep = "\t")
str(itemdata)
head(itemdata)
responsedata <- read.csv("C:/Users/75581/Desktop/1/MALD1_SelectedResponseData.csv", sep = "\t")
str(responsedata)
head(responsedata)
g1 <- ggplot(responsedata, aes(RT))
g1 <- g1 + geom_bar()
g1

combine <- merge(responsedata,itemdata,all = TRUE)
summary_0 <- summarise(combine, 
                       max_RT = max(RT), min_RT = min(RT), avg_RT = mean(RT), sd_RT = sd(RT),
                       max_fre = max(FreqCOCAspok), min_fre = min(FreqCOCAspok), avg_fre = mean(FreqCOCAspok), sd_fre = sd(FreqCOCAspok))
# For RT and frequency
Item_list_1 <- group_by(combine, Item)
summary_1 <- summarise(Item_list_1, count = n(), 
                   max_RT = max(RT), min_RT = min(RT), avg_RT = mean(RT), sd_RT = sd(RT),
                   max_fre = max(FreqCOCAspok), min_fre = min(FreqCOCAspok), avg_fre = mean(FreqCOCAspok), sd_fre = sd(FreqCOCAspok))
# For words and pseudowords among the dataset
Item_list_2 <- group_by(combine, IsWord)                   
summary_2 <- summarise(Item_list_2, count = n(), 
                           max_RT = max(RT), min_RT = min(RT), avg_RT = mean(RT), sd_RT = sd(RT))
# For words and pseudowords in subject 15351
combine_15351 <- combine[which(combine$Subject == 15351),]
Item_list_3 <- group_by(combine_15351, IsWord)
summary_3 <- summarise(Item_list_3, count = n(), 
                       max_RT = max(RT), min_RT = min(RT), avg_RT = mean(RT), sd_RT = sd(RT))
# For words and pseudowords in subject 16854
combine_16854 <- combine[which(combine$Subject == 16854),]
Item_list_4 <- group_by(combine_16854, IsWord)
summary_4 <- summarise(Item_list_4, count = n(), 
                       max_RT = max(RT), min_RT = min(RT), avg_RT = mean(RT), sd_RT = sd(RT))
# For words and pseudowords in subject 170373
combine_170373 <- combine[which(combine$Subject == 170373),]
Item_list_5 <- group_by(combine_170373, IsWord)
summary_5 <- summarise(Item_list_5, count = n(), 
                       max_RT = max(RT), min_RT = min(RT), avg_RT = mean(RT), sd_RT = sd(RT))

# Q2

cohend <- function(x1, x2) {
  if(length(x1)==length(x2)){
    s = sqrt((var(x1)+var(x2))/2)
  }
  else{
    s = sqrt(((length(x1)-1)*var(x1)+(length(x2)-1)*var(x2))/(length(x1)+length(x2)-2))
  }
  d = (mean(x1)-mean(x2))/s
  return(d)
}
#RTs for words and pseudowords for Subject numbered 15351.
RT_words_15351 <- combine_15351[which(combine_15351$IsWord == TRUE),]$RT
RT_pseudowords_15351  <- combine_15351[which(combine_15351$IsWord == FALSE),]$RT
cohend_15351 <- cohend(RT_words_15351, RT_pseudowords_15351)
#RTs for words and pseudowords for Subject numbered 16854.
RT_words_16854 <- combine_16854[which(combine_16854$IsWord == TRUE),]$RT
RT_pseudowords_16854  <- combine_16854[which(combine_16854$IsWord == FALSE),]$RT
cohend_16854 <- cohend(RT_words_16854, RT_pseudowords_16854)
#RTs for words and pseudowords for Subject numbered 170373.
RT_words_170373 <- combine_170373[which(combine_170373$IsWord == TRUE),]$RT
RT_pseudowords_170373  <- combine_170373[which(combine_170373$IsWord == FALSE),]$RT
cohend_170373 <- cohend(RT_words_170373, RT_pseudowords_170373)
#RTs for all words and pseudowords.
RT_words <- combine[which(combine$IsWord == TRUE),]$RT
RT_pseudowords  <- combine[which(combine$IsWord == FALSE),]$RT
cohend_0 <- cohend(RT_words, RT_pseudowords)
#RTs for the two vectors provided below as word 15292 and pseudoword 15292
word_15292 <- c(2206, 1583, 1154, 1010, 865, 931, 1129, 683, 820, 1132, 1049,
                1211, 1261, 957, 1058, 790, 851, 1908, 1504, 1400, 924)
pseudoword_15292 <- c(677, 949, 889, 881, 917, 769, 772, 922, 1944, 881, 976,
                      1087, 1252, 914, 1277, 825, 1295, 1336, 788, 885, 932)
cohend_15292 <- cohend(word_15292, pseudoword_15292)

# Q3

tcalculation <- function(x1, x2) {
  if(length(x1)==length(x2)){
    e = sqrt((var(x1)+var(x2))/length(x1))
  }
  else{
    e = sqrt(var(x1)/length(x1)+var(x2)/length(x2))
  }
  t = (mean(x1)-mean(x2))/e
  return(t)
}

#RTs for words and pseudowords for Subject numbered 15351.
tcalculation_15351 <- tcalculation(RT_words_15351, RT_pseudowords_15351)
#RTs for all words and pseudowords.
tcalculation_0 <- tcalculation(RT_words, RT_pseudowords)
#RTs for the two vectors provided below as word 15292 and pseudoword 15292 
tcalculation_15292 <- tcalculation(word_15292, pseudoword_15292)


#generate.t <- function() {
#  mysample <- rnorm(20, mean = 0, sd = 10)
#  tvalue <- mean(mysample)/sqrt((var(mysample)/20))
#  tvalue
#}

#simulated_data <- rep(NA, 2e+05)
#for (i in 1:length(simulated_data)) {
#  simulated_data[i] <- generate.t()
#}
# Q-Q plot qqplot(qt(ppoints(200000), df=19), simulated_data)
# qqline(simulated_data, distribution= function(p) qt(p, df=19)) Not used -
# see the png plot in this assignment
# Histograms comparing t-values from simulated data and predicted based on
# the t-probability distribution
#par(mfrow = c(2, 1))
#hist(simulated_data)
#hist(qt(ppoints(2e+05), df = 19))


# Q4
# For NHST right-tail test
p_value_1 <- pt(tcalculation_15292, df=40, lower.tail = FALSE)
# For Two-tailed test
p_value_2 <- 2*pt(tcalculation_15292, df=40, lower.tail = FALSE)
# This is near p-value for t-test function which is 0.1417

#squaring, cubing, taking an inverse,taking square root, or log-transforming data.

#result = boxcox(RT_words~1, lambda = seq(-0.5,0,5))
#mylambda = result$x[which.max(result$y)]
#RT_words_change = (RT_words^mylambda-1)/mylambda
hist(word_15292, breaks = 10)
word_15292_change_1 <- 1/word_15292
pseudoword_15292_change_1 <- 1/pseudoword_15292
hist(word_15292_change_1, breaks = 20)
hist(pseudoword_15292_change_1, breaks =20)
ggplot(word_15292_change_1)+geom_density()


word_15292_change_2 <- sqrt(word_15292)
pseudoword_15292_change_2 <- sqrt(pseudoword_15292)
#word_15292_change_2 <- word_15292 ^ 2
#pseudoword_15292_change_2 <- pseudoword_15292 ^ 2
hist(word_15292_change_2, breaks = 20)
hist(pseudoword_15292_change_2, breaks = 20)


t.test(word_15292_change_2, pseudoword_15292_change_2)



RT_words_change_1 <- log10(RT_words)
RT_pseudowords_change_1 <- log10(RT_pseudowords)
hist(RT_words_change_1)
hist(RT_pseudowords_change_1)
RT_words_change_2 <- sqrt(RT_words)
RT_pseudowords_change_2 <- sqrt(RT_pseudowords)
df_1=data.frame(RT_words_change_2, RT_pseudowords_change_2)
ggplot(df_1, aes(x=RT_words_change_2))+geom_density()
ggplot(df_1, aes(x=RT_pseudowords_change_2))+geom_density()
df_2=data.frame(RT_words, RT_pseudowords)
ggplot(df_2, aes(x=RT_words))+geom_density()
ggplot(df_2, aes(x=RT_pseudowords))+geom_density()
hist(RT_words_change_2)
hist(RT_pseudowords_change_2)
#two-tailed
t.test(RT_words_change_2, RT_pseudowords_change_2)

# Q5
combine_words <- combine[which(combine$IsWord == TRUE),]
combine_pseudowords  <- combine[which(combine$IsWord == FALSE),]
combine_words_1 <- group_by(combine_words, Subject)
combine_pseudowords_1  <- group_by(combine_pseudowords, Subject)
summary_combine_words_1 <- summarise(combine_words_1, avg_RT = mean(RT))
summary_combine_pseudowords_1 <- summarise(combine_pseudowords_1, avg_RT = mean(RT))
t.test(summary_combine_words_1$avg_RT, summary_combine_pseudowords_1$avg_RT, paired = TRUE)


# Q8
m1 <- lm(combine$RT ~ combine$IsWord)
print(summary(m1))
plot (combine$RT, combine$IsWord)
abline(m1)
plot(density(m1$resid))
#par(mfrow = c(2, 2))
#plot(m1, ask=FALSE)
qqnorm(m1$resid)
qqline(m1$resid)

m2 <- lm(combine$RT ~ combine$ACC)
print(summary(m2))
plot (combine$RT, combine$ACC)
abline(m2)
plot(density(m2$resid))
#par(mfrow = c(2, 2))
#plot(m2, ask=FALSE)
qqnorm(m2$resid)
qqline(m2$resid)

m3 <- lm(combine$RT ~ combine$IsWord * combine$ACC)
print(summary(m3))
plot(density(m3$resid))
#par(mfrow = c(2, 2))
#plot(m3, ask=FALSE)
qqnorm(m3$resid)
qqline(m3$resid)
# Q9
summary_combine_words_1 <- summarise(group_by(combine_words, Subject), avg_RT = mean(RT))
RT_0 <- log10(summary_combine_words_1$avg_RT)
summary_combine_words_1 <- summarise(group_by(combine_words, Subject), avg_freq = mean(FreqCOCA))
freq_0 <- summary_combine_words_1$avg_freq
summary_combine_words_1 <- summarise(group_by(combine_words, Subject), avg_freq = mean(FreqGoogle))
freq_1 <- summary_combine_words_1$avg_freq
summary_combine_words_1 <- summarise(group_by(combine_words, Subject), avg_freq = mean(FreqSUBTLEX))
freq_2 <- summary_combine_words_1$avg_freq
summary_combine_words_1 <- summarise(group_by(combine_words, Subject), avg_freq = mean(FreqCOCAspok))
freq_3 <- summary_combine_words_1$avg_freq

model_0 <- lm(RT_0 ~ freq_0)
print(summary(model_0))
model_1 <- lm(RT_0 ~ freq_1)
print(summary(model_1))
model_2 <- lm(RT_0 ~ freq_2)
print(summary(model_2))
model_3 <- lm(RT_0 ~ freq_3)
print(summary(model_3))

RT_0_0 <- combine$RT
my_data <- data.frame(RT_0_0, freq_0, freq_1, freq_2, freq_3)
ggplot(my_data, aes( x = freq_0, y = RT_0_0, )) + geom_point(size=1) + geom_smooth(method = 'lm', formula = y ~ x, se = T)
my_data$freq_0 <- log10(freq_0)
ggplot(my_data, aes( x = freq_0, y = RT_0_0, )) + geom_point(size=1) + geom_smooth(method = 'lm', formula = y ~ x, se = T)
ggplot(my_data, aes( x = freq_1, y = RT_0_0, )) + geom_point(siz.e=1) + geom_smooth(method = 'lm', formula = y ~ x, se = T)
my_data$freq_1 <- log10(freq_1)
ggplot(my_data, aes( x = freq_1, y = RT_0_0, )) + geom_point(size=1) + geom_smooth(method = 'lm', formula = y ~ x, se = T)
ggplot(my_data, aes( x = freq_2, y = RT_0_0, )) + geom_point(size=1) + geom_smooth(method = 'lm', formula = y ~ x, se = T)
my_data$freq_2 <- log10(freq_2)
ggplot(my_data, aes( x = freq_2, y = RT_0_0, )) + geom_point(size=1) + geom_smooth(method = 'lm', formula = y ~ x, se = T)
ggplot(my_data, aes( x = freq_3, y = RT_0_0, )) + geom_point(size=1) + geom_smooth(method = 'lm', formula = y ~ x, se = T)
my_data$freq_3 <- log10(freq_3)
ggplot(my_data, aes( x = freq_3, y = RT_0_0, )) + geom_point(size=1) + geom_smooth(method = 'lm', formula = y ~ x, se = T)
