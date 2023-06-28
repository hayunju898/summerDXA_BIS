data = read.csv("/Users/yunjuha/Desktop/SROP/DXA_BIS_Project/2023-06-23_DXA_BIS_data.csv")

#creating dummy variables
age_categories <- cut(data$RA1PRAGE, breaks = c(-Inf, 40, 50, 61, Inf), labels = c("Outside", "40-50", "51-61", "61+"))

# Create dummy variables using model.matrix() with defaulting others to 0
dummy_variables <- model.matrix(~ ifelse(age_categories == "40-50", 1, 0) +
                                  ifelse(age_categories == "51-61", 1, 0) +
                                  ifelse(age_categories == "61+", 1, 0))
data_with_dummies <- cbind(data, dummy_variables)

data = subset(data_with_dummies, select = -`(Intercept)`)

#renaming the dummy variable columns
names(data)[names(data) == "ifelse(age_categories == \"40-50\", 1, 0)"] <- "Age_40_50"
names(data)[names(data) == "ifelse(age_categories == \"51-61\", 1, 0)"] <- "Age_51_61"
names(data)[names(data) == "ifelse(age_categories == \"61+\", 1, 0)"] <- "Age_61plus"

################################################################################
#covariates only
covariate = lm(tRA4IMaxGrip ~ Age_40_50 + Age_51_61 + Age_61plus + RA1PRSEX + RA1PF7A + tRA4PBMI, data = data)
summary(covariate)

#alm
alm = lm(tRA4IMaxGrip ~ Age_40_50 + Age_51_61 + Age_61plus + RA1PRSEX + RA1PF7A + tRA4PBMI + tRA4IALM, data = data)
summary(alm)

#alm/ht^2
alm_ht2 = lm(tRA4IMaxGrip ~ Age_40_50 + Age_51_61 + Age_61plus + RA1PRSEX + RA1PF7A + tRA4PBMI + tRA4IALMbyHt2, data = data)
summary(alm_ht2)

#alm/bmi
alm_bmi = lm(tRA4IMaxGrip ~ Age_40_50 + Age_51_61 + Age_61plus + RA1PRSEX + RA1PF7A + tRA4PBMI + tRA4IALMbyBMI, data = data)
summary(alm_bmi)

#alm/(E/I)w
alm_ei = lm(tRA4IMaxGrip ~ Age_40_50 + Age_51_61 + Age_61plus + RA1PRSEX + RA1PF7A + tRA4PBMI + tRA4IALMbyE2Icor, data = data)
summary(alm_ei)

#llm
llm = lm(tRA4IMaxGrip ~ Age_40_50 + Age_51_61 + Age_61plus + RA1PRSEX + RA1PF7A + tRA4PBMI + tRA4ILLM, data = data)
summary(llm)

#llm/(E/I)l
llm_ei = lm(tRA4IMaxGrip ~ Age_40_50 + Age_51_61 + Age_61plus + RA1PRSEX + RA1PF7A + tRA4PBMI + tRA4ILLMbyLE2Icor, data = data)
summary(llm_ei)


