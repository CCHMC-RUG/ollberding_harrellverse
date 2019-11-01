


#########################################################################


# Script with basic R code to follow along with CCHMC RUG presentation:
# "An Introduction to the Harrell“verse”: Predictive Modeling using the Hmisc and rms Packages"


#notes: - this script provides the bare R code to follow along with 
#         the analyses  presented in the talk


#Created by: Nicholas Ollberding

#On: 11-01-2019


#########################################################################




# Installing required packages
.cran_packages <- c("tidyverse", "cowplot", "rms", "devtools")

.inst <- .cran_packages %in% installed.packages()
if(any(!.inst)) {
  install.packages(.cran_packages[!.inst])
}

devtools::install_github("coatless/ucidata")         #Install ucidata package from github (requires devtools)




#Loading packages and exmaple dataset
library("tidyverse"); packageVersion("tidyverse")
library("rms"); packageVersion("rms")
library("cowplot"); packageVersion("cowplot")
library("ucidata"); packageVersion("ucidata")



#Loading example dataset and recoding outcome
data(wine)

mydata <- wine %>%
  dplyr::mutate(red_wine = ifelse(color == "Red", 1, 0)) %>%
  dplyr::select(-color)  





# Tip 1. Examining your data with Hmisc
(d <- Hmisc::describe(mydata))
p <- plot(d)
p$Continuous

(s <- Hmisc::summaryM(fixed_acidity + volatile_acidity + citric_acid + residual_sugar + chlorides + free_sulfur_dioxide + 
              total_sulfur_dioxide+ density + pH + sulphates + alcohol + quality  ~ red_wine, data = mydata,
              overall = TRUE, test = TRUE, continuous = 5))
 

dd <- datadist(mydata)
options(datadist = "dd")
a <- ggplot(mydata, aes(x = alcohol, y = red_wine)) +
  Hmisc::histSpikeg(red_wine ~ alcohol, lowess = TRUE, data = mydata) +
  labs(x = "\nAlcohol Content", y = "Probability(Red Wine)\n")
b <- ggplot(mydata, aes(x = citric_acid, y = red_wine)) +
    Hmisc::histSpikeg(red_wine ~ citric_acid, lowess = TRUE, data = mydata) +
    labs(x = "\nCitric Acid", y = "Probability(Red Wine)\n")
c <- ggplot(mydata, aes(x = sulphates, y = red_wine)) +
    Hmisc::histSpikeg(red_wine ~ sulphates, lowess = TRUE, data = mydata) +
    labs(x = "\nSulphates", y = "Probability(Red Wine)\n")
cowplot::plot_grid(a, b, c,  nrow = 1, ncol = 3, scale = .9, labels = "AUTO")


missing_df <- mydata %>%
  dplyr::mutate(fixed_acidity = ifelse(row_number() %in% c(1:100), NA, fixed_acidity),
         volatile_acidity = ifelse(row_number() %in% c(1:200), NA, volatile_acidity),
         citric_acid = ifelse(row_number() %in% c(50:400), NA, citric_acid),
         residual_sugar = ifelse(row_number() %in% c(1000:1050), NA, residual_sugar),
         chlorides = ifelse(row_number() %in% c(1000:1100), NA, chlorides))
par(mfrow = c(1,2))
na_patterns <- Hmisc::naclus(missing_df)
Hmisc::naplot(na_patterns, 'na per var')
plot(na_patterns)
par(mfrow = c(1,1))



# Tip 2. Regression modeling allowing for complexity with rms 
m0 <- lrm(red_wine ~ rcs(fixed_acidity, 4) + rcs(volatile_acidity, 4) + rcs(citric_acid, 4) + rcs(residual_sugar, 4) + 
          rcs(chlorides, 4) + rcs(free_sulfur_dioxide, 4) + rcs(total_sulfur_dioxide, 4) + rcs(density, 4) + rcs(pH, 4) + 
          rcs(sulphates, 4) + rcs(alcohol, 4) + rcs(quality, 3),
          data = mydata, x = TRUE, y = TRUE)
print(m0, coef = FALSE)
plot(anova(m0))


m2 <- lrm(red_wine ~ rcs(pH, 3) + rcs(sulphates, 3), data = mydata, x = TRUE, y = TRUE)
print(m2)
anova(m2)
p1 <- ggplot(Predict(m2, pH))
p2 <- ggplot(Predict(m2, sulphates))
p3 <- ggplot(Predict(m2, pH, fun = plogis))
p4 <- ggplot(Predict(m2, sulphates, fun = plogis))
cowplot::plot_grid(p1, p2, p3, p4, nrow = 2, ncol = 2, scale = .9)


m3 <- lrm(red_wine ~ (rcs(pH, 3) + rcs(sulphates, 3))^2, data = mydata, x = TRUE, y = TRUE)
print(m3, coef = FALSE)
anova(m3)
pred_intx <- Predict(m3, 'pH','sulphates', fun = plogis, np = 75)
bplot(pred_intx, yhat ~ pH + sulphates, lfun = wireframe,
      ylab = "Sulphates", zlab = "Pr(Red Wine)\n")
ggplot(Predict(m2, sulphates, pH = c(3.0, 3.2, 3.4), fun = plogis))


m4 <- lrm(red_wine ~ rcs(pH, 3) + rcs(sulphates, 3) + pH %ia% sulphates, data = mydata, x = TRUE, y = TRUE)  
print(m4, coef = FALSE)
anova(m4)
pred_intx_r <- Predict(m4, 'pH','sulphates', fun = plogis, np = 75)
bplot(pred_intx_r, yhat ~ pH + sulphates, lfun = wireframe,
      ylab = "Sulphates", zlab = "Pr(Red Wine)\n")


summary(m4)
summary(m4, pH = c(2.97, 3.50))       #contrast of 5th verus 95th %tile

r <- mydata
r$fitted <- predict(m4, type = "fitted")
head(r$fitted)



# Tip 3. Validating fitted models with rms::validate() and rms:calibrate()
(val <- validate(m4, B = 200))
(c_opt_corr <- 0.5 * (val[1, 5] + 1))

cal <- calibrate(m4, B = 200)
plot(cal)




# Tip 4. Penalized regression with rms::pentrace()
pentrace(m4, seq(.01, .1, by = .01))
m5 <- update(m4, penalty = .01)
m5

pentrace(m4, list(simple = 0.01, nonlinear = c(0, 0.01, 0.02, 0.03), interaction = c(0, 0.01, 0.02, 0.03)))
m6 <- update(m4, penalty = list(simple = 0.01, nonlinear = 0.02, interaction = 0.02))                          #update this webpage!
m6
effective.df(m6)



# Tip 5. Models other than OLS for continuous or semi-continuous Y
lm1 <- ols(residual_sugar ~ (rcs(pH, 3) + rcs(sulphates, 3))^2, data = mydata, x = TRUE, y = TRUE) 
print(lm1, coefs = FALSE)
r <- mydata
r$resid <- resid(lm1)
r$fitted <- fitted(lm1)
r1 <- ggplot(data = r, aes(x = fitted, y = resid)) + geom_point() + geom_smooth()
r2 <- ggplot(data = r, aes(x = pH, y = resid)) + geom_point() + geom_smooth()
r3 <- ggplot(data = r, aes(x = sulphates, y = resid)) + geom_point() + geom_smooth()
r4 <- ggplot(data = r, aes(sample = resid)) + stat_qq() + geom_abline(intercept = mean(r$resid), slope = sd(r$resid))
cowplot::plot_grid(r1, r2, r3, r4, nrow = 2, ncol = 2, scale = .9)
ggplot(Predict(lm1, sulphates, pH = c(3, 3.2, 3.4)))


orm1 <- orm(residual_sugar ~ (rcs(pH, 3) + rcs(sulphates, 3))^2, data = mydata, x = TRUE, y = TRUE) 
print(orm1, coefs = FALSE)
M <- Mean(orm1)
qu <- Quantile(orm1)
med <- function(x) qu(0.5, x)
p1 <- ggplot(Predict(orm1, sulphates, pH = c(3, 3.2, 3.4), fun = M)) + coord_cartesian(ylim = c(1, 8))
p2 <- ggplot(Predict(orm1, sulphates, pH = c(3, 3.2, 3.4), fun = med)) + coord_cartesian(ylim = c(1, 8))
plot_grid(p1, p2, nrow = 1, ncol = 2, scale = 0.9, labels = c("ORM: Mean", "ORM: Median"))


rq1 <- Rq(residual_sugar ~ (rcs(pH, 3) + rcs(sulphates, 3))^2, data = mydata, x = TRUE, y = TRUE, tau = 0.5) 
print(rq1, coefs = FALSE)
ggplot(Predict(rq1, sulphates, pH = c(3, 3.2, 3.4)))



#Bonus: taking predictions outside of R
(pred_logit <- Function(m4))


#Session Info
sessionInfo()
