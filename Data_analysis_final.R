#--------------------DATA ANALYSIS--------------

##Load packages####
library(readxl) #For importing excel file
library(tidyverse) #Data cleaning tools
library(stats) #Statistics functions
library(DescTools) #For producing easily readable descriptive statistics

###PRELIMINARY ANALYSIS####
#Summary statistics for Inside 50 chains
I50_summary <- summarise(group_by(Inside50Chains, EndState), n = n(), '%' = n()/nrow(Inside50Chains)*100)

#Summary statistics for Inside 50 Turnover outcomes
I50TO_summary <- summarise(group_by(I50TO, factor(Result_code, labels = c("Negative", "Neutral", "Positive", "OppoRushed", "Unresolved"))), n = n(), '%' = n()/nrow(I50TO)*100)


####MAIN ANALYSIS####

###Descriptives#####

Descriptives_positive <- DescTools::Desc(filter(Analysis_variables_cat, Result_code == "Positive"))
Descriptives_negative <- DescTools::Desc(filter(Analysis_variables_cat, Result_code == "Negative"))


###Normality check####
Positive_AV1 <- Analysis_variables1 %>%
  filter(Result_code == "Positive")

Negative_AV1 <- Analysis_variables1 %>%
  filter(Result_code == "Negative")

Numerical_AV1_Positive <- Analysis_variables1 %>%
  filter(Result_code == "Positive") %>%
  dplyr::select(Width, Length, n_players, Mean_dist_from_cent, Mean_dist_from_TO, XcentroidNorm, YcentroidNorm, TOxNorm, TOyNorm,
                Xcent_dist_from_TO, Ycent_dist_from_TO, Rcent_dist_from_TO, Hull_area, m2_per_player)

Numerical_AV1_Negative <- Analysis_variables1 %>%
  filter(Result_code == "Negative") %>%
  dplyr::select(Width, Length, n_players, Mean_dist_from_cent, Mean_dist_from_TO, XcentroidNorm, YcentroidNorm, TOxNorm, TOyNorm,
                Xcent_dist_from_TO, Ycent_dist_from_TO, Rcent_dist_from_TO, Hull_area, m2_per_player)
normality_check_positive <- lapply(Numerical_AV1_Positive, shapiro.test)
attributes(normality_check_positive)

Positive_normality1 <- as.data.frame(rbind(normality_check_positive$Width, normality_check_positive$Length,
                                           normality_check_positive$XcentroidNorm,normality_check_positive$YcentroidNorm,
                                           normality_check_positive$n_players,normality_check_positive$Mean_dist_from_cent,
                                           normality_check_positive$Mean_dist_from_TO,normality_check_positive$Xcent_dist_from_TO,
                                           normality_check_positive$Ycent_dist_from_TO,normality_check_positive$Rcent_dist_from_TO,
                                           normality_check_positive$Hull_area,normality_check_positive$m2_per_player,
                                           normality_check_positive$TOxNorm,normality_check_positive$TOyNorm)) %>%
  dplyr::select(-method, -data.name) %>%
  mutate(Variable = c("Width", "Length", "XcentroidNorm", "YcentroidNorm", "n_players", "Mean_dist_from_cent",
                      "Mean_dist_from_TO", "Xcent_dist_from_TO", "Ycent_dist_from_TO", "Rcent_dist_from_TO", "Hull_area",
                      "m2_per_player", "TOxNorm", "TOyNorm"),
         statistic = digits(as.double(statistic), digits = 6),
         p.value = digits(as.double(p.value), digits = 6),
         outcome = "Positive")
##Only Hull area and mean dist from cent are normally distributed


normality_check_Negative <- lapply(Numerical_AV1_Negative, shapiro.test)
attributes(normality_check_Negative)

Negative_normality1 <- as.data.frame(rbind(normality_check_Negative$Width, normality_check_Negative$Length,
                                           normality_check_Negative$XcentroidNorm,normality_check_Negative$YcentroidNorm,
                                           normality_check_Negative$n_players,normality_check_Negative$Mean_dist_from_cent,
                                           normality_check_Negative$Mean_dist_from_TO,normality_check_Negative$Xcent_dist_from_TO,
                                           normality_check_Negative$Ycent_dist_from_TO,normality_check_Negative$Rcent_dist_from_TO,
                                           normality_check_Negative$Hull_area,normality_check_Negative$m2_per_player,
                                           normality_check_Negative$TOxNorm,normality_check_Negative$TOyNorm)) %>%
  dplyr::select(-method, -data.name) %>%
  mutate(Variable = c("Width", "Length", "XcentroidNorm", "YcentroidNorm", "n_players", "Mean_dist_from_cent",
                      "Mean_dist_from_TO", "Xcent_dist_from_TO", "Ycent_dist_from_TO", "Rcent_dist_from_TO", "Hull_area",
                      "m2_per_player", "TOxNorm", "TOyNorm"),
         statistic = digits(as.double(statistic), digits = 6),
         p.value = digits(as.double(p.value), digits = 6),
         outcome = "Negative")
###Mean dist from cent, TOxNorm, TOyNorm, Length, Hull_area, XcentroidNorm are normally distributed
all_normality1 <-  rbind(Positive_normality1, Negative_normality1)
write.csv(all_normality1, "Data/two_outcome_normality.csv")


######Mann-Whitney U Tests####

help(wilcox.test)

wilcox_Width <- wilcox.test(Positive_AV1$Width, Negative_AV1$Width, conf.int = TRUE, conf.level = 0.95, paired = FALSE)
wilcox_Length <- wilcox.test(Positive_AV1$Length, Negative_AV1$Length, conf.int = TRUE, conf.level = 0.95, paired = FALSE)
wilcox_n_players <-wilcox.test(Positive_AV1$n_players, Negative_AV1$n_players, conf.int = TRUE, conf.level = 0.95, paired = FALSE)
wilcox_Xcentroid_Norm <- wilcox.test(Positive_AV1$XcentroidNorm, Negative_AV1$XcentroidNorm, conf.int = TRUE, conf.level = 0.95, paired = FALSE)
wilcox_Ycentroid_Norm <- wilcox.test(Positive_AV1$YcentroidNorm, Negative_AV1$YcentroidNorm, conf.int = TRUE, conf.level = 0.95, paired = FALSE)
wilcox_TOxNorm <- wilcox.test(Positive_AV1$TOxNorm, Negative_AV1$TOxNorm, conf.int = TRUE, conf.level = 0.95, paired = FALSE)
wilcox_TOyNorm <- wilcox.test(Positive_AV1$TOyNorm, Negative_AV1$TOyNorm, conf.int = TRUE, conf.level = 0.95, paired = FALSE)
wilcox_Mean_dist_from_cent <- wilcox.test(Positive_AV1$Mean_dist_from_cent, Negative_AV1$Mean_dist_from_cent, conf.int = TRUE, conf.level = 0.95, paired = FALSE)
wilcox_Mean_dist_from_TO <- wilcox.test(Positive_AV1$Mean_dist_from_TO, Negative_AV1$Mean_dist_from_TO, conf.int = TRUE, conf.level = 0.95, paired = FALSE)
wilcox_Xcent_dist_from_TO <- wilcox.test(Positive_AV1$Xcent_dist_from_TO, Negative_AV1$Xcent_dist_from_TO, conf.int = TRUE, conf.level = 0.95, paired = FALSE)
wilcox_Ycent_dist_from_TO <- wilcox.test(Positive_AV1$Ycent_dist_from_TO, Negative_AV1$Ycent_dist_from_TO, conf.int = TRUE, conf.level = 0.95, paired = FALSE)
wilcox_Rcent_dist_from_TO <- wilcox.test(Positive_AV1$Rcent_dist_from_TO, Negative_AV1$Rcent_dist_from_TO, conf.int = TRUE, conf.level = 0.95, paired = FALSE)
wilcox_Hull_area <- wilcox.test(Positive_AV1$Hull_area, Negative_AV1$Hull_area, conf.int = TRUE, conf.level = 0.95, paired = FALSE)
wilcox_m2_per_player <- wilcox.test(Positive_AV1$m2_per_player, Negative_AV1$m2_per_player, conf.int = TRUE, conf.level = 0.95, paired = FALSE)
wilcox_SD_dist_from_cent <- wilcox.test(Positive_AV1$SD_dist_from_cent, Negative_AV1$SD_dist_from_cent, conf.int = TRUE, conf.level = 0.95, paired = FALSE)
#None are statistically significant

####Chi-square tests to determine univariate reltionships between discrete variables and outcome####

##CHI squared test for entry_zone2
CHIentry<- chisq.test(Analysis_variables_cat$Result_code, Analysis_variables_cat$Entry_zone2)
CHIentry$expected
CHIentry$observed
CHIentry

##CHI squared test for entry_zone2
CHIentry<- chisq.test(Analysis_variables_cat$Result_code, Analysis_variables_cat$Entry_zone2)
CHIentry$expected
CHIentry$observed
CHIentry

##CHI squared test for same path
CHIpath<- chisq.test(Analysis_variables_cat$Result_code, Analysis_variables_cat$Same_path)
CHIpath$expected
CHIpath$observed
CHIpath


##CHI squared test for wide or long
CHIwol<- chisq.test(Analysis_variables_cat$Result_code, Analysis_variables_cat$Wide_or_Long)
CHIwol$expected
CHIwol$observed
CHIwol




####Check correlations for collinearity using pearson r2#### Cuttoff point at 0.95

Numerical_AV <- Analysis_variables %>%
  select(Width, Length, n_players, Mean_dist_from_cent, Mean_dist_from_TO, XcentroidNorm, YcentroidNorm, TOxNorm, TOyNorm,
         Xcent_dist_from_TO, Ycent_dist_from_TO, Rcent_dist_from_TO, Hull_area, m2_per_player)

correlations <- cor(Numerical_AV)


write.csv(correlations, "Data/Pearson_correlations.csv")


####Logistic regression####
###Analysis_varirables_cat was moved to Orange at this point and removal of collinear variables, as well as mixed sampling were undertaken. 
#Data is then returned to R for Logistic regression
logistic_sample <- read_csv("Data/Analysis_variables_cat_mixedsample.csv")
logistic_variables <- logistic_sample %>%
  select(-TOid, -Round)
###Model including all variables
logistic_model <- glm(data = logistic_variables, as.factor(Result_code) ~ ., family = binomial)
logistic_summary <- summary(logistic_model)
Odds_Ratio <- exp(coef(logistic_model)) %>%
  na.omit()
Confidence_int <- exp(confint(logistic_model)) %>%
  as.data.frame.matrix() %>%
  rownames_to_column() 

OR_Conf_int <- data.frame(Confidence_int$rowname, digits(Confidence_int$`2.5 %`, 2), digits(Confidence_int$`97.5 %`, 2)) %>%
  rename(Variable = "Confidence_int.rowname",
         Lower = "digits.Confidence_int..2.5.....2.",
         Upper = "digits.Confidence_int..97.5.....2.") %>%
  na.omit()



predictions <- predict(logistic_model, logistic_variables, type="response")
predictions.rd <- ifelse(predictions > 0.5, 1, 0)
accuracy <- table(predictions.rd, logistic_variables$Result_code)

Classification_accuracy <- sum(diag(accuracy))/sum(accuracy)

Nagelkerke_pseudor2 <- PseudoR2(logistic_model, which = "Nagelkerke") ##0.0927

Logistic_coefficients <- arrange_at(rownames_to_column(as.data.frame(logistic_summary$coefficients)),.vars = "Estimate", .funs = desc)

coefficients_OR <- cbind(rownames_to_column(as.data.frame(logistic_summary$coefficients)), Odds_Ratio, OR_Conf_int)
COR_ordered <- arrange_at(coefficients_OR, .vars = "Odds_Ratio", .funs = desc)



write.csv(coefficients_OR, "Data/Logistic_coefficients_and_odds.csv")





