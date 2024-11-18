# This script is for the metacognition project.
# multiple rows indent: ctrl + [
# add comments: ctrl + shift + c
# install.packages("bruceR", dep=TRUE)
# install.packages("fitdistrplus")

library(bruceR)
# library(fitdistrplus)

set.wd() # locate the current directory

# clear the workspace
# rm(list = ls())

# ---【1. for behavioral performance】---
# 【subject-level】
# load data
dataRaw_subjLvl <- import("data_preprocessed_old.xlsx", sheet = 1)
dataSelect_subjLvl <- dataRaw_subjLvl
# Change column names for performing ANOVA
names(dataSelect_subjLvl) <- c("subjNo", "group", "sex", "age", "education","ACC", "RT", "ratings", "rating1", "rating2", "rating3", "rating4")
colnames(dataSelect_subjLvl)[c(9:12)] <- c("rating1", "rating2", "rating3", "rating4")

# ---【examine the distribution of variables】---
# Younger Adults
Describe(
  dataSelect_subjLvl[dataSelect_subjLvl$group==1,c(4:ncol(dataSelect_subjLvl))],
  all.as.numeric = TRUE,
  digits = 2,
  file = NULL,
  plot = TRUE,
  upper.triangle = FALSE,
  upper.smooth = "none",
  plot.file = NULL,
  plot.width = 8,
  plot.height = 6,
  plot.dpi = 500
)
# Older Adults
Describe(
  dataSelect_subjLvl[dataSelect_subjLvl$group==2,c(4:ncol(dataSelect_subjLvl))],
  all.as.numeric = TRUE,
  digits = 2,
  file = NULL,
  plot = TRUE,
  upper.triangle = FALSE,
  upper.smooth = "none",
  plot.file = NULL,
  plot.width = 8,
  plot.height = 6,
  plot.dpi = 500
)

# 【normality testing】
# for the whole
ncol_stats <- length(c(4:ncol(dataSelect_subjLvl)))
normality <- matrix(NA, nrow = 2, ncol = ncol_stats)
normality <- as.data.frame(normality)
for (vari in 4:ncol(dataSelect_subjLvl)) {
  normality[1, vari-3] <- shapiro.test(dataSelect_subjLvl[, vari])$statistic # w
  normality[2, vari-3] <- shapiro.test(dataSelect_subjLvl[, vari])$p.value
}
names(normality) <- c("age", "education","ACC", "RT", "ratings", "rating1", "rating2", "rating3", "rating4")

# for each group
normality_group <- array(NA, dim=c(2,ncol_stats,2))
for (groupi in 1:2){
  for (vari in 4:ncol(dataSelect_subjLvl)) {
    normality_group[1, vari-3, groupi] <- shapiro.test(dataSelect_subjLvl[dataSelect_subjLvl$group==groupi, vari])$statistic # w
    normality_group[2, vari-3, groupi] <- shapiro.test(dataSelect_subjLvl[dataSelect_subjLvl$group==groupi, vari])$p.value
  }
}
print(normality_group)
normality_YA <- as.data.frame(normality_group[, , 1])
normality_OA <- as.data.frame(normality_group[, , 2])
names(normality_YA) <- c("age", "education","ACC", "RT", "ratings", "rating1", "rating2", "rating3", "rating4")
names(normality_OA) <- c("age", "education","ACC", "RT", "ratings", "rating1", "rating2", "rating3", "rating4")

# 【group difference in demographics】
# age and education
compare_edu_t <- TTEST(dataSelect_subjLvl, "education", x="group", var.equal=FALSE)
print(sprintf("education - t: %.2f, p: %.4f", compare_edu_t$t, compare_edu_t$pval))
compare_edu_nps <- wilcox.test(dataSelect_subjLvl$group, dataSelect_subjLvl$education, paired=FALSE)
print(sprintf("education - V: %.2f, p: %.4f", compare_edu_nps$statistic, compare_edu_nps$p.value))

# sex
library(BayesFactor)
M_YA <- nrow(dataSelect_subjLvl[dataSelect_subjLvl$group==1 & dataSelect_subjLvl$sex==1,])
F_YA <- nrow(dataSelect_subjLvl[dataSelect_subjLvl$group==1 & dataSelect_subjLvl$sex==2,])
M_OA <- nrow(dataSelect_subjLvl[dataSelect_subjLvl$group==2 & dataSelect_subjLvl$sex==1,])
F_OA <- nrow(dataSelect_subjLvl[dataSelect_subjLvl$group==2 & dataSelect_subjLvl$sex==2,])
###!!! Perform chi-square test !!!###
data_sex <- matrix(c(M_YA, F_YA, M_OA, F_OA), , nrow = 2, byrow = TRUE)
rownames(data_sex) <- c("YA", "OA")
colnames(data_sex) <- c("Male", "Female")
# print(data_sex)
chi_square_result <- chisq.test(data_sex)
# effect size
eff_size <- phi(data_sex, adjust = TRUE, alternative = "two.sided")
print(eff_size)
# Display the results
print(chi_square_result)
# Calculate Bayes Factor using contingencyTableBF()
bf <- contingencyTableBF(data_sex, sampleType = "jointMulti")
print(bf)

# 【examine fixed effects using manova at the group level】
# 1. behavioral performance
# for accuracy
MANOVA(dataSelect_subjLvl, dv="ACC",
       between=c("group","sex"), 
       cov=c("education"),
       sph.correction="GG") %>%
  EMMEANS("group") %>%
  EMMEANS("sex")

compare_accuracy_t <- TTEST(dataSelect_subjLvl, "ACC", x="group", var.equal=FALSE)
print(sprintf("accuracy - t: %.2f, p: %.4f", compare_accuracy_t$t, compare_accuracy_t$pval))
compare_accuracy_nps <- wilcox.test(dataSelect_subjLvl$group, dataSelect_subjLvl$accuracy, paired=FALSE)
print(sprintf("accuracy - V: %.2f, p: %.4f", compare_accuracy_nps$statistic, compare_accuracy_nps$p.value))

# for RT
MANOVA(dataSelect_subjLvl, dv="RT",
       between=c("group","sex"), 
       cov=c("education"),
       sph.correction="GG") %>%
  EMMEANS("group") %>%
  EMMEANS("sex")

compare_rt_t <- TTEST(dataSelect_subjLvl, "RT", x="group", var.equal=FALSE)
print(sprintf("RT - t: %.2f, p: %.4f", compare_rt_t$t, compare_rt_t$pval))
compare_rt_nps <- wilcox.test(dataSelect_subjLvl$group, dataSelect_subjLvl$RT, paired=FALSE)
print(sprintf("RT - V: %.2f, p: %.4f", compare_rt_nps$statistic, compare_rt_nps$p.value))

# for ratings
MANOVA(dataSelect_subjLvl, dvs="rating1:rating4", dvs.pattern="rating(.)",
       between=c("group","sex"), 
       within="type",
       cov=c("education"),
       sph.correction="GG") %>%
  EMMEANS("type", by="group") %>%
  EMMEANS("group", by="type")

# examine fixed and random effects using lmer, noting that only for long format data
# model <- lmer(cbind(RT1, RT2) ~ difficulty * group + (1 | subject), data = dataSelect)

# 【for the trial-level data】
# load data
dataRaw_trlLvl <- import("data_preprocessed_old.xlsx", sheet = 2)
dataSelect_trlLvl <- dataRaw_trlLvl[, c(1:5, 9:11)]
names(dataSelect_trlLvl) <- c("subjNo", "group", "sex", "age", "education", "RT", "ACC", "rating")

# examine fixed and random effects using lmer, noting that only for long format data
# model <- lmer(RT ~ age + education + group + condition + (1 | subjNo), data = dataSelect)
model_rt <- lmer(RT ~ education + group + (1 | subjNo), data = dataSelect_trlLvl)
model_acc <- lmer(ACC ~ education + group + (1 | subjNo), data = dataSelect_trlLvl)
model_rating <- lmer(rating ~ education + group + (1 | subjNo), data = dataSelect_trlLvl)
# HLM_summary(model_acc)
model_summary(model_rt)
model_summary(model_acc)
model_summary(model_rating)

# ---【2. for the BIM results】---
# 【subject-level】
# load data
BIM_Raw_subjLvl <- import("BIM.xlsx", sheet = 1)
BIM_Select_subjLvl <- BIM_Raw_subjLvl

# Change column names for performing ANOVA
colnames(BIM_Select_subjLvl)[c(7:10)] <- c("rating1", "rating2", "rating3", "rating4")

# examine fixed effects using manova
# for Pexp
MANOVA(BIM_Select_subjLvl, dv="Pexp",
       between=c("group","sex"), 
       cov=c("education"),
       sph.correction="GG") %>%
  EMMEANS("group") %>%
  EMMEANS("sex")

# for rho
MANOVA(BIM_Select_subjLvl, dv="rho",
       between=c("group","sex"), 
       cov=c("education"),
       sph.correction="GG") %>%
  EMMEANS("group") %>%
  EMMEANS("sex")

# for d_bim
MANOVA(BIM_Select_subjLvl, dv="d_bim",
       between=c("group","sex"), 
       cov=c("education"),
       sph.correction="GG") %>%
  EMMEANS("group") %>%
  EMMEANS("sex")

# for C_bim
MANOVA(BIM_Select_subjLvl, dv="C_bim",
       between=c("group","sex"), 
       cov=c("education"),
       sph.correction="GG") %>%
  EMMEANS("group") %>%
  EMMEANS("sex")

# for ranting
MANOVA(BIM_Select_subjLvl, dvs="rating1:rating4", dvs.pattern="rating(.)",
       between=c("group","sex"), 
       within="type",
       cov=c("education"),
       sph.correction="GG") %>%
  EMMEANS("type", by="group") %>%
  EMMEANS("group", by="type")


# ---【3. for the meta_d results】---
# load data
meta_d_Raw_subjLvl <- import("meta_d_fit.xlsx", sheet = 1)
meta_d_subjLvl <- meta_d_Raw_subjLvl
# for M_ratio
MANOVA(meta_d_subjLvl, dv="M_ratio",
       between=c("group","sex"), 
       cov=c("education"),
       sph.correction="GG") %>%
  EMMEANS("group") %>%
  EMMEANS("sex")

# for meta_d
MANOVA(meta_d_subjLvl, dv="meta_d",
       between=c("group","sex"), 
       cov=c("education"),
       sph.correction="GG") %>%
  EMMEANS("group") %>%
  EMMEANS("sex")

