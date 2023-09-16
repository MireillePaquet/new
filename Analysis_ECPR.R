# Analysis of Experiment 1 - ECPR paper

# Packages
library(tidyr)
library(tidyverse)
library(haven)
library(car)
library(psych)
library(texreg)
library(modelsummary)
library(ggeffects)
library(ggpubr)
library(wec) # weighted effect coding
library(haven)

# Data
df <- read_sav("Data/CONC04_Base_Prelim_SPSS.sav")
df <- zap_labels(df) # Remove labels



# Drop those that fail attention check
#- Place cursor between 60 and 70 (drop those that are under 55 or over 75)
df$Q6_1[df$Q6_1 >= 100] <- NA # Remove NA (don't know they weren't paying attention)
df$fail_check1 <- ifelse(df$Q6_1 < 60 | df$Q6_1 > 70, 1, 0)

table(df$Q6_1, df$fail_check1)
prop.table(table(df$fail_check1))

# Select blue
table(df$Q9_1) # response 3
df$Q9_1[df$Q9_1 == 9] <- NA
df$fail_check2 <- ifelse(df$Q9_1 == 3, 0, 1)
table(df$Q9_1, df$fail_check2)
prop.table(table(df$fail_check2))

# Select always
table(df$Q13_1)
df$Q13_1[df$Q13_1 == 9] <- NA
df$fail_check3 <- ifelse(df$Q13_1 == 4, 0, 1)
table(df$Q13_1, df$fail_check3)
prop.table(table(df$fail_check3))

# Dummy variable if failed all three attention checks
df$inattentive_score <- (df$fail_check1 + df$fail_check2 + df$fail_check3)
table(df$inattentive_score)

df <- df %>% filter(inattentive_score != 3)

#---- Recoding variables ----
# Pre-treatment covariates (higher scores = support admitting less migrants)
df$ptcov1 <- car::recode(df$Q16_1, "9 = NA; 5 = 1; 4 = 2; 3 = 3; 2 = 4; 1 = 5")
df$ptcov2 <- car::recode(df$Q16_2, "9 = NA; 5 = 1; 4 = 2; 3 = 3; 2 = 4; 1 = 5")
df$ptcov3 <- car::recode(df$Q16_3, "9 = NA; 5 = 1; 4 = 2; 3 = 3; 2 = 4; 1 = 5")
df$ptcov4 <- car::recode(df$Q16_4, "9 = NA; 5 = 1; 4 = 2; 3 = 3; 2 = 4; 1 = 5")
df$ptcov5 <- car::recode(df$Q16_5, "9 = NA; 5 = 1; 4 = 2; 3 = 3; 2 = 4; 1 = 5")

ptcov.items <- c("ptcov1", "ptcov2", "ptcov3", "ptcov4", "ptcov5")
ptcov.items <- df[ptcov.items]
psych::alpha(ptcov.items) # alpha = 0.84

df$ptcov_scale <- (df$ptcov1 + df$ptcov2 +df$ptcov3 + df$ptcov4 + df$ptcov5) / 5
describe(df$ptcov_scale)

# Experimental condition
table(df$LIBQ4)
df$exp1_cond <- car::recode(df$LIBQ4, "1 = 'A'; 2 = 'B'; 3 = 'C'; 4 = 'D'; 5 = 'E'; 6 = 'F'; 7 = 'G'; 8 = 'H'; 9 = 'I'; 10 = 'J'")
table(df$LIBQ4, df$exp1_cond)

# Support for less immigration
table(df$Q4_1) # Recode so "less immigrants" = 1, "more" or "about the same" = 0
df$dv1 <- car::recode(df$Q4_1, "1:2 = 0; 3 = 1; 9 = NA")
table(df$Q4_1, df$dv1)

# Immigrants' impact on Canadian society (recode so higher scores = negative impact) 
table(df$Q4_2)
df$dv2 <- car::recode(df$Q4_2, "99 = NA; 1 = 10; 2 = 9; 3 = 8; 4 = 7; 5 = 6; 6 = 5; 7 = 4; 8 = 3; 9 = 2; 10 = 1")
table(df$Q4_2, df$dv2)

# Immigrants' impact on Canadian economy (recode so higher scores = negative impact) 
table(df$Q4_3)
df$dv3 <- car::recode(df$Q4_3, "99 = NA; 1 = 10; 2 = 9; 3 = 8; 4 = 7; 5 = 6; 6 = 5; 7 = 4; 8 = 3; 9 = 2; 10 = 1")
table(df$Q4_3, df$dv3)

# Immigrants harm Canadian culture (coded so 1 = bad, 0 = good or no effect)
table(df$Q8_4)
df$dv4 <- car::recode(df$Q8_4, "1:2 = 0; 3 = 1; 9 = NA")

# Immigrants harm Canadian housing market (coded so 1 = bad, 0 = good or no effect)
table(df$Q8_4B)
df$dv5 <- car::recode(df$Q8_4B, "1:2 = 0; 3 = 1; 9 = NA")

#---- Retain completed cases ----
# Listwise deletion so all models have the same respondents 
# This drops ~ 1,000 respondents. Maybe not what we want (commented out for now - need to discuss)
# vars <- c("dv1", "dv2", "dv3", "dv4", "dv5", "ptcov_scale", "exp1_cond")
# df.complete <- df[vars] 
# df.complete <- df.complete[complete.cases(df.complete),] 
# describe(df.complete)

#---- Overview of all question-wording effects ----
# Regression models predicting the treatment effects, controlling for baseline policy preferences
reg1 <- lm(dv1 ~ exp1_cond + ptcov_scale, data = df)
reg2 <- lm(dv2 ~ exp1_cond + ptcov_scale, data = df)
reg3 <- lm(dv3 ~ exp1_cond + ptcov_scale, data = df)
reg4 <- lm(dv4 ~ exp1_cond + ptcov_scale, data = df)
reg5 <- lm(dv5 ~ exp1_cond + ptcov_scale, data = df)

screenreg(list(reg1, reg2, reg3, reg4, reg5)) # Compares each experimental condition to the true control (A)

exp.plot1 <- ggeffect(reg1, terms = "exp1_cond") %>% plot() +
  ggtitle("Question wording effect on support for less immigration (ref. = 'more' or 'about the same')") +
  ylab("Predicted probability of supporting lower immigration levels") +
  xlab("Experimental condition") +
  labs(caption = "Control condition = A, no information.")

exp.plot2 <- ggeffect(reg2, terms = "exp1_cond") %>% plot() +
  ggtitle("Question wording effect on perceived impact of immigration on Canadian society") +
  ylab("Predicted value") +
  xlab("Experimental condition") +
  labs(caption = "Control condition = A, no information.")

exp.plot3 <- ggeffect(reg3, terms = "exp1_cond") %>% plot() +
  ggtitle("Question wording effect on perceived impact of immigration on Canadian economy") +
  ylab("Predicted value") +
  xlab("Experimental condition") +
  labs(caption = "Control condition = A, no information.")

ggarrange(exp.plot2, exp.plot3,
          nrow = 2)

# ---- Planned comparisons (effect coding) ----
#--- Define contrasts
# H1 - 500,000 vs no mention (ref)
AvB <- c(-1, 1, 0, 0, 0, 0, 0, 0, 0, 0) # H1: A v B Control vs 500,000 (reference category is -1)

# H2 - economic immigrant vs refugee (ref)
CvD <- c(0, 0, 1, -1, 0, 0, 0, 0, 0, 0) # H2 @ national: C v F (without 500,000 mentioned)
EvF <- c(0, 0, 0, 0, 1, -1, 0, 0, 0, 0) # H2 @ national: E v F (with 500,000 mentioned)

GvH <- c(0, 0, 0, 0, 0, 0, 1, -1, 0, 0) # H2 @ provincial: GvH (without 500,000 mentioned)
IvJ <- c(0, 0, 0, 0, 0, 0, 0, 0, 1, -1) # H2 @ provincial: I v J (with 500,000 mentioned)

# H3 - provincial vs national (ref)
CvG <- c(0, 0, -1, 0, 0, 0, 1, 0, 0, 0) # H3-provincial economic vs national economic (without 500,000 mentioned)
DvH <- c(0, 0, 0, -1, 0, 0, 0, 1, 0, 0) # H3-provincial refugee vs national refugee (without 500,000 mentioned)

EvI <- c(0, 0, 0, 0, -1, 0, 0, 0, 1, 0) # H3-provincial economic vs national economic (with 500,000 mentioned) 
FvJ <- c(0, 0, 0, 0, 0, -1, 0, 0, 0, 1) # H3-provincial refugee vs national refugee (with 500,000 mentioned)

# Add contrasts against "true control" and "levels control" in an appendix... but not for ECPR

#--- Create weighting to adjust for unbalanced experimental design (weighting sample sizes relative to condition A)
df$exp1_cond.wec <- as.factor(df$exp1_cond)
contrasts(df$exp1_cond.wec) <- contr.wec(df$exp1_cond.wec, "A")
contrasts(df$exp1_cond.wec)

#--- Preliminary analyses ---
describeBy(df$dv1, df$exp1_cond) # Reduce immigration
describeBy(df$dv2, df$exp1_cond) # Impact on Canadian society
describeBy(df$dv3, df$exp1_cond) # Impact on Canadian economy
describeBy(df$dv4, df$exp1_cond) # Impact on Canadian culture
describeBy(df$dv5, df$exp1_cond) # Impact on Canadian housing market

#---- Hypothesis 1 ----
# Calculate descriptive statistics by experimental condition for each experimental condition
matrix.h1 <- cbind(AvB)
matrix.h1

# Coefplot of the contrast coefficient
models.h1 <- list(
  
  "Negative impact on Canadian housing market" = reg.h1.dv5 <- lm(dv5 ~ exp1_cond.wec + ptcov_scale, 
                                                                  data = df,
                                                                  contrasts=list(exp1_cond.wec = matrix.h1)),
  
  "Negative impact on Canadian culture" = reg.h1.dv4 <- lm(dv4 ~ exp1_cond.wec + ptcov_scale, 
                                                           data = df,
                                                           contrasts=list(exp1_cond.wec = matrix.h1)),
  
  "Negative impact on Canadian economy" = reg.h1.dv3 <- lm(dv3 ~ exp1_cond.wec + ptcov_scale, 
                                                           data = df,
                                                           contrasts=list(exp1_cond.wec = matrix.h1)),
  
  "Negative impact on Canadian society" = reg.h1.dv2 <- lm(dv2 ~ exp1_cond.wec + ptcov_scale, 
                                                           data = df,
                                                           contrasts=list(exp1_cond.wec = matrix.h1)),
  
  "Reduce immigration" = reg.h1.dv1 <- lm(dv1 ~ exp1_cond.wec + ptcov_scale, 
                                          data = df,
                                          contrasts=list(exp1_cond.wec = matrix.h1))
)

coef_to_plot.h1 <- c("exp1_cond.wecAvB" = "2025 levels vs Control (ref.)")

modelsummary(models.h1, stars = T,
             output = "tableh1.docx")

####### SLIDE PLOT1
SLIDEPLOT1 <-modelplot(models.h1, 
                       facet = T,
                       coef_map = coef_to_plot.h1) + 
  ggtitle("500,000 target treatment condition vs control condition (reference category)") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(strip.text = element_blank(),
        text = element_text(size = 20)) +
  xlab("Adjusted difference-in-means (95%CI)")

### to do: ) add text at the bottom about contrasts ("Adjusted difference-in-means by message, coeficients estimates and 95% confidence intervals"), instead of at the top
### 
text = element_text(size = 20)

#---- Hypothesis 2 ----
matrix.h2a <- cbind(CvD) # Contrasts the two w/o 500,000 conditions @ national level.
matrix.h2b <- cbind(EvF) # Contrasts the two w/ 500,000 conditions @ national level. 
matrix.h2c <- cbind(GvH) # Contrast the two w/o 500,000 conditions @ provincial level.
matrix.h2d <- cbind(IvJ) # Contrast the two w/ 500,000 conditions @ provincial level.

matrix.h2a
matrix.h2b
matrix.h2c
matrix.h2d

# Coefplot of the contrast coefficient
#- Table for hypothesis 2a (national level contrasts)
models.h2a <- list(
  
  "Negative impact housing housing market" = reg.h2a.dv5 <- lm(dv5 ~ exp1_cond.wec + ptcov_scale, 
                                                               data = df,
                                                               contrasts=list(exp1_cond.wec = matrix.h2a)),
  
  "Negative impact on Canadian culture" = reg.h2a.dv4 <- lm(dv4 ~ exp1_cond.wec + ptcov_scale, 
                                                            data = df,
                                                            contrasts=list(exp1_cond.wec = matrix.h2a)),
  
  "Negative impact on Canadian economy" = reg.h2a.dv3 <- lm(dv3 ~ exp1_cond.wec + ptcov_scale, 
                                                            data = df,
                                                            contrasts=list(exp1_cond.wec = matrix.h2a)),
  
  "Negative impact on Canadian society" = reg.h2a.dv2 <- lm(dv2 ~ exp1_cond.wec + ptcov_scale, 
                                                            data = df,
                                                            contrasts=list(exp1_cond.wec = matrix.h2a)),
  
  "Reduce immigration" = reg.h2a.dv1 <- lm(dv1 ~ exp1_cond.wec + ptcov_scale, 
                                           data = df,
                                           contrasts=list(exp1_cond.wec = matrix.h2a))
)

models.h2b <- list(
  
  "Negative impact on Canadian housing market" = reg.h2b.dv5 <- lm(dv5 ~ exp1_cond.wec + ptcov_scale, 
                                                                   data = df,
                                                                   contrasts=list(exp1_cond.wec = matrix.h2b)),
  
  "Negative impact on Canadian culture" = reg.h2b.dv4 <- lm(dv4 ~ exp1_cond.wec + ptcov_scale, 
                                                            data = df,
                                                            contrasts=list(exp1_cond.wec = matrix.h2b)),
  
  "Negative impact on Canadian economy" = reg.h2b.dv3 <- lm(dv3 ~ exp1_cond.wec + ptcov_scale, 
                                                            data = df,
                                                            contrasts=list(exp1_cond.wec = matrix.h2b)),
  
  "Negative impact on Canadian society" = reg.h2b.dv2 <- lm(dv2 ~ exp1_cond.wec + ptcov_scale, 
                                                            data = df,
                                                            contrasts=list(exp1_cond.wec = matrix.h2b)),
  
  "Reduce immigration" = reg.h2b.dv1 <- lm(dv1 ~ exp1_cond.wec + ptcov_scale, 
                                           data = df,
                                           contrasts=list(exp1_cond.wec = matrix.h2b))
)

# Table for Hypothesis 2b (Provincial level contrasts)
models.h2c <- list(
  
  "Negative impact on Canadian housing market" = reg.h2c.dv5 <- lm(dv5 ~ exp1_cond.wec + ptcov_scale, 
                                                                   data = df,
                                                                   contrasts=list(exp1_cond.wec = matrix.h2c)),
  
  "Negative impact on Canadian culture" = reg.h2c.dv4 <- lm(dv4 ~ exp1_cond.wec + ptcov_scale, 
                                                            data = df,
                                                            contrasts=list(exp1_cond.wec = matrix.h2c)),
  
  "Negative impact on Canadian economy" = reg.h2c.dv3 <- lm(dv3 ~ exp1_cond.wec + ptcov_scale, 
                                                            data = df,
                                                            contrasts=list(exp1_cond.wec = matrix.h2c)),
  
  "Negative impact on Canadian society" = reg.h2c.dv2 <- lm(dv2 ~ exp1_cond.wec + ptcov_scale, 
                                                            data = df,
                                                            contrasts=list(exp1_cond.wec = matrix.h2c)),
  
  "Reduce immigration" = reg.h2c.dv1 <- lm(dv1 ~ exp1_cond.wec + ptcov_scale, 
                                           data = df,
                                           contrasts=list(exp1_cond.wec = matrix.h2c))
)

models.h2d <- list(
  
  "Negative impact on Canadian housing market" = reg.h2d.dv5 <- lm(dv5 ~ exp1_cond.wec + ptcov_scale, 
                                                                   data = df,
                                                                   contrasts=list(exp1_cond.wec = matrix.h2d)),
  
  "Negative impact on Canadian culture" = reg.h2d.dv4 <- lm(dv4 ~ exp1_cond.wec + ptcov_scale, 
                                                            data = df,
                                                            contrasts=list(exp1_cond.wec = matrix.h2d)),
  
  "Negative impact on Canadian economy" = reg.h2d.dv3 <- lm(dv3 ~ exp1_cond.wec + ptcov_scale, 
                                                            data = df,
                                                            contrasts=list(exp1_cond.wec = matrix.h2d)),
  
  "Negative impact on Canadian society" = reg.h2d.dv2 <- lm(dv2 ~ exp1_cond.wec + ptcov_scale, 
                                                            data = df,
                                                            contrasts=list(exp1_cond.wec = matrix.h2d)),
  
  "Reduce immigration" = reg.h2d.dv1 <- lm(dv1 ~ exp1_cond.wec + ptcov_scale, 
                                           data = df,
                                           contrasts=list(exp1_cond.wec = matrix.h2d))
)

coef_to_plot.h2a <- c("exp1_cond.wecCvD" = "National economic levels vs national refugee levels (ref.) (w/o 2025 levels target)",
                      "exp1_cond.wecEvF" = "National economic immigrants vs national refugees (ref.) (w/ 2025 levels target)")

coef_to_plot.h2b <- c("exp1_cond.wecGvH" = "Provincial economic levels vs provincial refugee levels (ref.) (w/o 2025 levels target)",
                      "exp1_cond.wecIvJ" = "Provincial economic levels vs provincial refugee levels (ref.) (w/ 2025 levels target)")

modelsummary(models.h2a, stars = T, output = "tableh2a1.docx")
modelsummary(models.h2b, stars = T, output = "tableh2a2.docx")
modelsummary(models.h2c, stars = T, output = "tableh2b1.docx")
modelsummary(models.h2d, stars = T, output = "tableh2b2.docx")

plot.h2a1 <- modelplot(models.h2a, 
                       facet = F,
                       coef_map = coef_to_plot.h2a) + 
  ggtitle("National economic levels vs national refugee levels (ref.)") +
  labs(subtitle = "Without 2025 levels target") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(axis.text.y = element_blank(),
        text = element_text(size = 18)) +
  xlab("Adjusted difference-in-means (95%CI)") +
  scale_color_discrete(breaks = c("Reduce immigration",
                                  "Negative impact on Canadian society",
                                  "Negative impact on Canadian economy",
                                  "Negative impact on Canadian culture",
                                  "Negative impact on Canadian housing market"))

plot.h2a2 <- modelplot(models.h2b, 
                       facet = F,
                       coef_map = coef_to_plot.h2a) + 
  ggtitle("National economic levels vs national refugee levels (ref.)") +
  labs(subtitle = "With 2025 levels target") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(axis.text.y = element_blank(),
        text = element_text(size = 18)) +
  xlab("Adjusted difference-in-means (95%CI)") +
  scale_color_discrete(breaks = c("Reduce immigration",
                                  "Negative impact on Canadian society",
                                  "Negative impact on Canadian economy",
                                  "Negative impact on Canadian culture",
                                  "Negative impact on Canadian housing market"))

plot.h2b1 <- modelplot(models.h2c, 
                       facet = F,
                       coef_map = coef_to_plot.h2b) + 
  ggtitle("Provincial economic levels vs Provincial refugee levels (ref.)") +
  labs(subtitle = "Without 2025 levels target") + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(axis.text.y = element_blank(),
        text = element_text(size = 18)) +
  xlab("Adjusted difference-in-means (95%CI)") +
  scale_color_discrete(breaks = c("Reduce immigration",
                                  "Negative impact on Canadian society",
                                  "Negative impact on Canadian economy",
                                  "Negative impact on Canadian culture",
                                  "Negative impact on Canadian housing market"))

plot.h2b2 <- modelplot(models.h2d, 
                       facet = F,
                       coef_map = coef_to_plot.h2b) + 
  ggtitle("Provincial economic levels vs Provincial refugee levels (ref.)") +
  labs(subtitle = "With 2025 levels target") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(axis.text.y = element_blank(),
        text = element_text(size = 18)) +
  xlab("Adjusted difference-in-means (95%CI)") +
  scale_color_discrete(breaks = c("Reduce immigration",
                                  "Negative impact on Canadian society",
                                  "Negative impact on Canadian economy",
                                  "Negative impact on Canadian culture",
                                  "Negative impact on Canadian housing market"))
# Fig 2a
ggarrange(plot.h2a1, plot.h2a2, 
          common.legend = T, 
          legend = "bottom")

# Fig 2b
ggarrange(plot.h2b1, plot.h2b2, 
          common.legend = T, 
          legend = "bottom")

#---- Hypothesis 3 ----
matrix.h3a <- cbind(CvG) # H3-provincial economic vs national economic (without 500,000 mentioned)
matrix.h3b <- cbind(EvI) # H3-provincial economic vs national economic (with 500,000 mentioned) 
matrix.h3c <- cbind(DvH) # H3-provincial refugee vs national refugee (without 500,000 mentioned)
matrix.h3d <- cbind(FvJ) # H3-provincial refugee vs national refugee (with 500,000 mentioned)

matrix.h3a
matrix.h3b
matrix.h3c
matrix.h3d

# Coefplot of the contrast coefficient
models.h3a <- list(
  
  "Negative impact on Canadian housing market" = reg.h3a.dv5 <- lm(dv5 ~ exp1_cond.wec + ptcov_scale, 
                                                                   data = df,
                                                                   contrasts=list(exp1_cond.wec = matrix.h3a)),
  
  "Negative impact on Canadian culture" = reg.h3a.dv4 <- lm(dv4 ~ exp1_cond.wec + ptcov_scale, 
                                                            data = df,
                                                            contrasts=list(exp1_cond.wec = matrix.h3a)),
  
  "Negative impact on Canadian economy" = reg.h3a.dv3 <- lm(dv3 ~ exp1_cond.wec + ptcov_scale, 
                                                            data = df,
                                                            contrasts=list(exp1_cond.wec = matrix.h3a)),
  
  "Negative impact on Canadian society" = reg.h3a.dv2 <- lm(dv2 ~ exp1_cond.wec + ptcov_scale, 
                                                            data = df,
                                                            contrasts=list(exp1_cond.wec = matrix.h3a)),
  
  "Reduce immigration" = reg.h3a.dv1 <- lm(dv1 ~ exp1_cond.wec + ptcov_scale, 
                                           data = df,
                                           contrasts=list(exp1_cond.wec = matrix.h3a))
)

models.h3b <- list(
  
  "Negative impact on Canadian housing market" = reg.h3b.dv5 <- lm(dv5 ~ exp1_cond.wec + ptcov_scale, 
                                                                   data = df,
                                                                   contrasts=list(exp1_cond.wec = matrix.h3b)),
  
  "Negative impact on Canadian culture" = reg.h3b.dv4 <- lm(dv4 ~ exp1_cond.wec + ptcov_scale, 
                                                            data = df,
                                                            contrasts=list(exp1_cond.wec = matrix.h3b)),
  
  "Negative impact on Canadian economy" = reg.h3b.dv3 <- lm(dv3 ~ exp1_cond.wec + ptcov_scale, 
                                                            data = df,
                                                            contrasts=list(exp1_cond.wec = matrix.h3b)),
  
  "Negative impact on Canadian society" = reg.h3b.dv2 <- lm(dv2 ~ exp1_cond.wec + ptcov_scale, 
                                                            data = df,
                                                            contrasts=list(exp1_cond.wec = matrix.h3b)),
  
  "Reduce immigration" = reg.h3b.dv1 <- lm(dv1 ~ exp1_cond.wec + ptcov_scale, 
                                           data = df,
                                           contrasts=list(exp1_cond.wec = matrix.h3b))
)

models.h3c <- list(
  
  "Negative impact on Canadian housing market" = reg.h3c.dv5 <- lm(dv5 ~ exp1_cond.wec + ptcov_scale, 
                                                                   data = df,
                                                                   contrasts=list(exp1_cond.wec = matrix.h3c)),
  
  "Negative impact on Canadian culture" = reg.h3c.dv4 <- lm(dv4 ~ exp1_cond.wec + ptcov_scale, 
                                                            data = df,
                                                            contrasts=list(exp1_cond.wec = matrix.h3c)),
  
  "Negative impact on Canadian economy" = reg.h3c.dv3 <- lm(dv3 ~ exp1_cond.wec + ptcov_scale, 
                                                            data = df,
                                                            contrasts=list(exp1_cond.wec = matrix.h3c)),
  
  "Negative impact on Canadian society" = reg.h3c.dv2 <- lm(dv2 ~ exp1_cond.wec + ptcov_scale, 
                                                            data = df,
                                                            contrasts=list(exp1_cond.wec = matrix.h3c)),
  
  "Reduce immigration" = reg.h3c.dv1 <- lm(dv1 ~ exp1_cond.wec + ptcov_scale, 
                                           data = df,
                                           contrasts=list(exp1_cond.wec = matrix.h3c))
)

models.h3d <- list(
  
  "Negative impact on Canadian housing market" = reg.h3d.dv5 <- lm(dv5 ~ exp1_cond.wec + ptcov_scale, 
                                                                   data = df,
                                                                   contrasts=list(exp1_cond.wec = matrix.h3d)),
  
  "Negative impact on Canadian culture" = reg.h3d.dv4 <- lm(dv4 ~ exp1_cond.wec + ptcov_scale, 
                                                            data = df,
                                                            contrasts=list(exp1_cond.wec = matrix.h3d)),
  
  "Negative impact on Canadian economy" = reg.h3d.dv3 <- lm(dv3 ~ exp1_cond.wec + ptcov_scale, 
                                                            data = df,
                                                            contrasts=list(exp1_cond.wec = matrix.h3d)),
  
  "Negative impact on Canadian society" = reg.h3d.dv2 <- lm(dv2 ~ exp1_cond.wec + ptcov_scale, 
                                                            data = df,
                                                            contrasts=list(exp1_cond.wec = matrix.h3d)),
  
  "Reduce immigration" = reg.h3d.dv1 <- lm(dv1 ~ exp1_cond.wec + ptcov_scale, 
                                           data = df,
                                           contrasts=list(exp1_cond.wec = matrix.h3d))
)

coef_to_plot.h3a <- c("exp1_cond.wecCvG" = "Provincial economic immigrants vs national economic immigrants (ref.) (without 500,000 mentioned)",
                      "exp1_cond.wecEvI" = "Provincial economic immigrants vs national economic immigrants (ref.) (with 500,000 mentioned)")

coef_to_plot.h3b <- c("exp1_cond.wecDvH" = "Provincial refugees vs national refugees (ref.) (without 500,000 mentioned)",
                      "exp1_cond.wecFvJ" = "Provincial refugees vs national refugees (ref.) (with 500,000 mentioned)")

modelsummary(models.h3a, stars = T, output = "tableh3a.docx")
modelsummary(models.h3b, stars = T, output = "tableh3b.docx")
modelsummary(models.h3c, stars = T, output = "tableh3c.docx")
modelsummary(models.h3d, stars = T, output = "tableh3d.docx")

plot.h3a1 <- modelplot(models.h3a, 
                       facet = F,
                       coef_map = coef_to_plot.h3a) + 
  ggtitle("Provincial economic levels vs national economic levels (ref.)") +
  labs(subtitle = "Without 2025 levels target") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(axis.text.y = element_blank(),
        text = element_text(size = 18)) +
  xlab("Adjusted difference-in-means (95%CI)") +
  scale_color_discrete(breaks = c("Reduce immigration",
                                  "Negative impact on Canadian society",
                                  "Negative impact on Canadian economy",
                                  "Negative impact on Canadian culture",
                                  "Negative impact on Canadian housing market"))

plot.h3a2 <- modelplot(models.h3b, 
                       facet = F,
                       coef_map = coef_to_plot.h3a) + 
  ggtitle("Provincial economic levels vs national economic levels (ref.)") +
  labs(subtitle = "With 2025 levels target") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(axis.text.y = element_blank(),
        text = element_text(size = 18)) +
  xlab("Adjusted difference-in-means (95%CI)") +
  scale_color_discrete(breaks = c("Reduce immigration",
                                  "Negative impact on Canadian society",
                                  "Negative impact on Canadian economy",
                                  "Negative impact on Canadian culture",
                                  "Negative impact on Canadian housing market"))

plot.h3b1 <- modelplot(models.h3c, 
                       facet = F,
                       coef_map = coef_to_plot.h3b) + 
  ggtitle("Provincial refugee levels vs national refugee levels (ref.)") +
  labs(subtitle = "Without 2025 levels target") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(axis.text.y = element_blank(),
        text = element_text(size = 18)) +
  xlab("Adjusted difference-in-means (95%CI)") +
  scale_color_discrete(breaks = c("Reduce immigration",
                                  "Negative impact on Canadian society",
                                  "Negative impact on Canadian economy",
                                  "Negative impact on Canadian culture",
                                  "Negative impact on Canadian housing market"))

plot.h3b2 <- modelplot(models.h3d, 
                       facet = F,
                       coef_map = coef_to_plot.h3b) + 
  ggtitle("Provincial refugee levels vs National refugee levels (ref.)") +
  labs(subtitle = "With 2025 levels target") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(axis.text.y = element_blank(),
        text = element_text(size = 18)) +
  xlab("Adjusted difference-in-means (95%CI)") +
  scale_color_discrete(breaks = c("Reduce immigration",
                                  "Negative impact on Canadian society",
                                  "Negative impact on Canadian economy",
                                  "Negative impact on Canadian culture",
                                  "Negative impact on Canadian housing market"))

# Plot 3a
ggarrange(plot.h3a1, plot.h3a2,
          common.legend = T, 
          legend = "bottom")

# Plot 3b
ggarrange(plot.h3b1, plot.h3b2, 
          common.legend = T, 
          legend = "bottom")

