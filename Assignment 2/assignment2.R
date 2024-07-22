## ----include=FALSE------------------------------------------------------------
opts_chunk$set(fig.path='figures/figure')


## ----include=FALSE------------------------------------------------------------
opts_chunk$set(tidy=TRUE)

## ----echo=TRUE----------------------------------------------------------------

library(dplyr)
library(ggplot2)

exp1 <- read.table("C:/Users/75581/Desktop/2/cunnings_sturt_exp1.csv",
                   col.names = c("subject",
                                 "item",
                                 "region",
                                 "measure",
                                 "condition",
                                 "rt"), sep=" ") %>%
  mutate(region = ifelse(region == 2, "verb", "spillover"),
         condition = case_when(
           condition == 1 ~ "a",
           condition == 2 ~ "b",
           condition == 3 ~ "c",
           condition == 4 ~ "d")) %>%
  mutate(plausibility= ifelse(condition %in% c("a", "b"), "plausible", "implausible"),
         interference = ifelse(condition %in% c("a", "c"), "interf", "nointerf"))

glimpse(exp1)

# Q2
exp1_fp <- exp1[which(exp1$measure == "fp"),]
exp1_fp_verb <- exp1_fp[which(exp1_fp$region == "verb"),]
exp1_fp_spillover <- exp1_fp[which(exp1_fp$region == "spillover"),]
exp1_fp_verb_plausible <- exp1_fp_verb[which(exp1_fp_verb$plausibility == "plausible"),]
exp1_fp_verb_implausible <- exp1_fp_verb[which(exp1_fp_verb$plausibility == "implausible"),]
exp1_fp_spillover_plausible <- exp1_fp_spillover[which(exp1_fp_spillover$plausibility == "plausible"),]
exp1_fp_spillover_implausible <- exp1_fp_spillover[which(exp1_fp_spillover$plausibility == "implausible"),]
# t-test
exp1_fp_verb_plausible_1  <- group_by(exp1_fp_verb_plausible, subject)
exp1_fp_verb_plausible_1_mean <- summarise(exp1_fp_verb_plausible_1, avg_rt = mean(rt))
exp1_fp_verb_implausible_1  <- group_by(exp1_fp_verb_implausible, subject)
exp1_fp_verb_implausible_1_mean <- summarise(exp1_fp_verb_implausible_1, avg_rt = mean(rt))
exp1_fp_spillover_plausible_1  <- group_by(exp1_fp_spillover_plausible, subject)
exp1_fp_spillover_plausible_1_mean <- summarise(exp1_fp_spillover_plausible_1, avg_rt = mean(rt))
exp1_fp_spillover_implausible_1  <- group_by(exp1_fp_spillover_implausible, subject)
exp1_fp_spillover_implausible_1_mean <- summarise(exp1_fp_spillover_implausible_1, avg_rt = mean(rt))
plausible <- rep('plausible', 48)
implausible <- rep('implausible', 48)
exp1_fp_verb_plausible_1_mean_1 <- data.frame(exp1_fp_verb_plausible_1_mean, plausibility = plausible)
exp1_fp_verb_implausible_1_mean_1 <- data.frame(exp1_fp_verb_implausible_1_mean, plausibility = implausible)
exp1_fp_verb_mean <- rbind(exp1_fp_verb_plausible_1_mean_1, exp1_fp_verb_implausible_1_mean_1)
exp1_fp_spillover_plausible_1_mean_1 <- data.frame(exp1_fp_spillover_plausible_1_mean, plausibility = plausible)
exp1_fp_spillover_implausible_1_mean_1 <- data.frame(exp1_fp_spillover_implausible_1_mean, plausibility = implausible)
exp1_fp_spillover_mean <- rbind(exp1_fp_spillover_plausible_1_mean_1, exp1_fp_spillover_implausible_1_mean_1)
t.test(exp1_fp_verb_plausible_1_mean$avg_rt, exp1_fp_verb_implausible_1_mean$avg_rt,paired = TRUE)
t.test(exp1_fp_spillover_plausible_1_mean$avg_rt, exp1_fp_spillover_implausible_1_mean$avg_rt,paired = TRUE)
# linear model version 1
m1 <- lm(exp1_fp_verb$rt ~ exp1_fp_verb$plausibility)
m2 <- lm(exp1_fp_spillover$rt ~ exp1_fp_spillover$plausibility)
print(summary(m1))
print(summary(m2))
# linear model version 2
m3 <- lm(exp1_fp_verb_mean$avg_rt ~ exp1_fp_verb_mean$plausibility)
m4 <- lm(exp1_fp_spillover_mean$avg_rt ~ exp1_fp_spillover_mean$plausibility)
print(summary(m3))
print(summary(m4))


# Q3
# includes the random effect structures for subjects and items 
exp1_fp_verb_new <- exp1_fp_verb
exp1_fp_verb_new$rt[which(exp1_fp_verb_new$rt == 0)]<-1
exp1_fp_verb_new$rt <- log10(exp1_fp_verb_new$rt)
exp1_fp_spillover_new <- exp1_fp_spillover
exp1_fp_spillover_new$rt[which(exp1_fp_spillover_new$rt == 0)]<-1
exp1_fp_spillover_new$rt <- log10(exp1_fp_spillover_new$rt)

m5 <- lmer(exp1_fp_verb_new$rt ~ 1 + exp1_fp_verb_new$plausibility * exp1_fp_verb_new$interference + (1 + exp1_fp_verb_new$plausibility * exp1_fp_verb_new$interference|exp1_fp_verb_new$subject) + (1 + exp1_fp_verb_new$plausibility * exp1_fp_verb_new$interference|exp1_fp_verb_new$item))
print(summary(m5))
m6 <- lmer(exp1_fp_spillover_new$rt ~ 1 + exp1_fp_spillover_new$plausibility * exp1_fp_spillover_new$interference + (1 + exp1_fp_spillover_new$plausibility * exp1_fp_spillover_new$interference|exp1_fp_spillover_new$subject) + (1 + exp1_fp_spillover_new$plausibility * exp1_fp_spillover_new$interference|exp1_fp_spillover_new$item))
print(summary(m6))

exp1_tt <- exp1[which(exp1$measure == "tt"),]
exp1_tt_verb <- exp1_fp[which(exp1_fp$region == "verb"),]
exp1_tt_spillover <- exp1_fp[which(exp1_fp$region == "spillover"),]

exp1_tt_verb_new <- exp1_tt_verb
exp1_tt_verb_new$rt[which(exp1_tt_verb_new$rt == 0)]<-1
exp1_tt_verb_new$rt <- log10(exp1_tt_verb_new$rt)
exp1_tt_spillover_new <- exp1_tt_spillover
exp1_tt_spillover_new$rt[which(exp1_tt_spillover_new$rt == 0)]<-1
exp1_tt_spillover_new$rt <- log10(exp1_tt_spillover_new$rt)

m7 <- lmer(exp1_tt_verb_new$rt ~ 1 + exp1_tt_verb_new$plausibility * exp1_tt_verb_new$interference + (1 + exp1_tt_verb_new$plausibility * exp1_tt_verb_new$interference|exp1_tt_verb_new$subject) + (1 + exp1_tt_verb_new$plausibility * exp1_tt_verb_new$interference|exp1_tt_verb_new$item))
print(summary(m7))
m8 <- lmer(exp1_tt_spillover_new$rt ~  1 + exp1_tt_spillover_new$plausibility * exp1_tt_spillover_new$interference + (1 + exp1_tt_spillover_new$plausibility * exp1_tt_spillover_new$interference|exp1_tt_spillover_new$subject) + (1 + exp1_tt_spillover_new$plausibility * exp1_tt_spillover_new$interference|exp1_spillover_verb_new$item))
print(summary(m8))

# Q4
exp1_tt_verb_new <- exp1_tt_verb
exp1_tt_verb_new$rt[which(exp1_tt_verb_new$rt == 0)]<- NA
exp1_tt_verb_new$rt <- log10(exp1_tt_verb_new$rt)
exp1_tt_spillover_new <- exp1_tt_spillover
exp1_tt_spillover_new$rt[which(exp1_tt_spillover_new$rt == 0)]<- NA
exp1_tt_spillover_new$rt <- log10(exp1_tt_spillover_new$rt)

m9 <- lmer(exp1_tt_verb_new$rt ~ 1 + exp1_tt_verb_new$plausibility * exp1_tt_verb_new$interference + (1 + exp1_tt_verb_new$plausibility * exp1_tt_verb_new$interference|exp1_tt_verb_new$subject) + (1 + exp1_tt_verb_new$plausibility * exp1_tt_verb_new$interference|exp1_tt_verb_new$item))
print(summary(m9))
m10 <- lmer(exp1_tt_spillover_new$rt ~  1 + exp1_tt_spillover_new$plausibility * exp1_tt_spillover_new$interference + (1 + exp1_tt_spillover_new$plausibility * exp1_tt_spillover_new$interference|exp1_tt_spillover_new$subject) + (1 + exp1_tt_spillover_new$plausibility * exp1_tt_spillover_new$interference|exp1_spillover_verb_new$item))
print(summary(m10))
m11 <- lm(exp1_spillover_verb_new$rt ~ exp1_spillover_verb_new$plausibility)
print(summary(m11))
m12 <- lm(exp1_tt_verb_new$rt ~ exp1_tt_verb_new$plausibility)
print(summary(m12))
# second part

## ----echo=TRUE----------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(lme4)
selfies <- read.csv("C:/Users/75581/Desktop/2/selfies.csv")
str(selfies)

selfies %>%
  group_by(ResponseId) %>%
  summarise(n = n(), m = mean(Boring))

glimpse(selfies)

head(selfies)

library(extraDistr)

# Q5
data_q5 <- data.frame(selfies$ResponseId, selfies$Gender, selfies$Boring)
data_q5_male <- data_q5[which(data_q5$selfies.Gender == "Male"),]
data_q5_female <- data_q5[which(data_q5$selfies.Gender == "Female"),]
t.test(data_q5_male$selfies.Boring, data_q5_female$selfies.Boring, paired = TRUE)
data_q5_male <- group_by(data_q5_male, selfies.ResponseId)
data_q5_male_mean <- summarise(data_q5_male, Boring = mean(selfies.Boring))
data_q5_female <- group_by(data_q5_female, selfies.ResponseId)
data_q5_female_mean <- summarise(data_q5_female, Boring = mean(selfies.Boring))
t.test(data_q5_male_mean$Boring, data_q5_female_mean$Boring)

BoringYesNo <- c(1:2154)
selfies_1 <- data.frame(selfies, BoringYesNo)
selfies_1$BoringYesNo[which(selfies_1$Boring == 4)] <- 1
selfies_1$BoringYesNo[which(selfies_1$Boring == 5)] <- 1
selfies_1$BoringYesNo[which(selfies_1$Boring == 2)] <- 0
selfies_1$BoringYesNo[which(selfies_1$Boring == 1)] <- 0
selfies_1$BoringYesNo[which(selfies_1$Boring == 3)] <- NA
fm_1<-glmer(formula = as.factor(BoringYesNo) ~ 1 + Gender + (1 + Gender | ResponseId), family = binomial(link = "logit"), data = selfies_1)
print(summary(fm_1), corr=FALSE)
library(sjPlot)
pp1 <- plot_model(fm_1,type="pred")
print(pp1)
fm_2<-glmer(formula = as.factor(BoringYesNo) ~ 1 + StimGender + (1 + StimGender | ResponseId), family = binomial(link = "logit"), data = selfies_1)
print(summary(fm_2), corr=FALSE)


## ----echo=TRUE----------------------------------------------------------------
fm_3<-glmer(formula = as.factor(BoringYesNo) ~ 1 + StimGender*Gender + (1 + StimGender * Gender | ResponseId), family = binomial(link = "logit"), data = selfies_1)
print(summary(fm_3), corr=FALSE)
fm_4<-glmer(formula = as.factor(BoringYesNo) ~ 1 + StimGender*Gender + (1 | ResponseId), family = binomial(link = "logit"), data = selfies_1)
print(summary(fm_4), corr=FALSE)
## ----echo=TRUE----------------------------------------------------------------

novel_selfies <- read.csv("C:/Users/75581/Desktop/2/novel_data.csv")
mypredictions <- predict(fm_4,novel_selfies)


## ----echo=TRUE----------------------------------------------------------------

drawprobabilities <- function(probs) {

    if (length(probs) != 400) {
        print("Wrong length of the vector of calculated probabilities. Should be 400 data points.")
    }
    else {

    matrixprobs <- matrix(ifelse(probs>0.5, "X", ""), nrow=20)

    x <- rep(NA, 400)
    y <- rep(NA, 400)

    k <- 1

    for (i in 1:20) {
        for (j in 1:20) {
            if (matrixprobs[i, j] == "X") {
                y[k] <- i
                x[k] <- j
                k <- k+1
            }
        }
    }

    plot(x, y, xlim=c(0, 40), ylim=c(0, 40), pch=15)

    }
}
drawprobabilities(mypredictions)

## ----echo=TRUE----------------------------------------------------------------
library(ordinal)

# model investigation here


## ----echo=TRUE----------------------------------------------------------------

#Boring ~ StimGender + (1|ResponseId)
fmm1 <- clmm(as.factor(Boring) ~ StimGender + (1|ResponseId),data = selfies)
summary(fmm1)

