library(beezdemand)
library(broom)
library(dplyr)
library(ggplot2)
library(ggsci)
library(kableExtra)
library(knitr)
library(nlme)
library(scales)
library(tidyr)

tn <- trans_new("inhs",
                function(x) log((x * 0.5) + ((0.5^2) * (x^2) + 1)^0.5)/log(10),
                function(y) (1/10^(1*y))*((10^(2*y)) - 1),
                domain = c(0, Inf),
                breaks = c(0, 0.1, 1, 10, 100))

mSurveyRes <- readxl::read_xlsx("RawResults.xlsx",
                                na = "NA")

skipThese <- c("R_10vBc6q1iYD6juF", "R_ZJKpsAPP9SYGJpL",
               "R_x9LkdDQPqQE5t8B", "R_bBf7jB2Sawm4T3b",
               "R_Di2tmQrWWLHg2iJ", "R_1oF4LBnCdBU70XC",
               "R_u8tvJegfSTRC14B", "R_2anmKehkAtc6Pf2",
               "R_REM9nx7gSRyNfrz")

mSurveyRes <- mSurveyRes %>%
  filter(!is.na(`Pure Demand Task_1(2)`)) %>%
  filter(Finished == 1)

crossPriceABA <- mSurveyRes %>%
  select(c(ResponseID,
           `Substitution Task_1(2)`,
           `Substitution Task_1(3)`,
           `Substitution Task_1(4)`,
           `Substitution Task_1(5)`,
           `Substitution Task_1(7)`,
           `Substitution Task_1(8)`,
           `Substitution Task_1(9)`,
           `Substitution Task_1(10)`,
           `Substitution Task_1(11)`,
           `Substitution Task_1(12)`,
           `Substitution Task_1(13)`,
           `Substitution Task_1(14)`,
           `Substitution Task_1(15)`,
           numChildren,
           bxConcern,
           Education,
           Sex,
           Income)
  ) %>%
  rename(`50`   = `Substitution Task_1(2)`,
         `100`  = `Substitution Task_1(3)`,
         `150`  = `Substitution Task_1(4)`,
         `200`  = `Substitution Task_1(5)`,
         `250`  = `Substitution Task_1(7)`,
         `300`  = `Substitution Task_1(8)`,
         `400`  = `Substitution Task_1(9)`,
         `500`  = `Substitution Task_1(10)`,
         `750`  = `Substitution Task_1(11)`,
         `1000` = `Substitution Task_1(12)`,
         `2000` = `Substitution Task_1(13)`,
         `3000` = `Substitution Task_1(14)`,
         `5000` = `Substitution Task_1(15)`)

crossPriceABA.long.all <- crossPriceABA %>%
  gather(Price, Consumption, colnames(crossPriceABA)[2:14], factor_key = TRUE) %>%
  mutate(Price = as.numeric(as.character(Price))) %>%
  mutate(Consumption = as.numeric(Consumption)) %>%
  as.data.frame()

crossPriceABA.long.clean = crossPriceABA.long.all %>%
  filter(!(ResponseID %in% skipThese ))

crossPriceDemandGroupFrame <- crossPriceABA.long.clean %>%
  group_by(Price) %>%
  summarise(
    ConsumptionAve = mean(Consumption),
    ConsumptionMdn = median(Consumption),
  )

zbeFit.full.closed <- nlmrt::wrapnls(
  formula = log((Consumption * 0.5) + ((0.5^2) * (Consumption^2) + 1)^0.5)/log(10) ~
    log((q0 * 0.5) + ((0.5^2) * (q0^2) + 1)^0.5)/log(10) +
    k *
    (exp(-alpha * q0 * Price) - 1),
  start   = list(q0    = max(crossPriceDemandGroupFrame$ConsumptionAve),
                 k     = 1,
                 alpha = 0.01),
  data    = crossPriceABA.long.clean)

zbeFit.abbrev.closed <- nls(formula = log((Consumption * 0.5) + ((0.5^2) * (Consumption^2) + 1)^0.5)/log(10) ~
                              (log((q0 * 0.5) + ((0.5^2) * (q0^2) + 1)^0.5)/log(10)) *
                              (exp(((-alpha)/(log((q0 * 0.5) + ((0.5^2) * (q0^2) + 1)^0.5)/log(10))) * q0 * Price)),
                            data = crossPriceABA.long.clean,
                            start   = list(q0    = max(crossPriceDemandGroupFrame$ConsumptionAve),
                                           alpha = 0.001))

zbeFit.null.closed <- nls(
  formula = log((Consumption * 0.5) + ((0.5^2) * (Consumption^2) + 1)^0.5)/log(10) ~
    log((q0 * 0.5) + ((0.5^2) * (q0^2) + 1)^0.5)/log(10),
  start   = list(q0    = max(crossPriceDemandGroupFrame$ConsumptionAve)),
  data    = crossPriceABA.long.clean)

ss1 <- sum(resid(zbeFit.abbrev.closed)^2)
ss2 <- sum(resid(zbeFit.full.closed)^2)
df1 <- df.residual(zbeFit.abbrev.closed)
df2 <- df.residual(zbeFit.full.closed)

Fval <- ((ss1 - ss2)/ss2)/((df1 - df2)/df2)
pval <- 1 - pf(Fval, (df1 - df2), df2)
critF <- qf(c(0.025, 0.975), (df1 - df2), df2)

print("Null hypothesis: No Specific Span Parameter Necessary (Q0 == K)")
print("Alternative hypothesis: K is DIFFERENT from overall span")
print(paste0("Conclusion: ", if (pval < .05) "reject" else "fail to reject", " the null hypothesis"))
print(paste0("F(", (df1 - df2), ",", df2, ") = ", round(Fval, 4), ", p = ", round(pval, 4)))

ss1 <- sum(resid(zbeFit.null.closed)^2)
ss2 <- sum(resid(zbeFit.full.closed)^2)
df1 <- df.residual(zbeFit.null.closed)
df2 <- df.residual(zbeFit.full.closed)

Fval <- ((ss1 - ss2)/ss2)/((df1 - df2)/df2)
pval <- 1 - pf(Fval, (df1 - df2), df2)
critF <- qf(c(0.025, 0.975), (df1 - df2), df2)

print("Null hypothesis: No Span Necessary")
print("Alternative hypothesis: Need a rate parameter and span")
print(paste0("Conclusion: ", if (pval < .05) "reject" else "fail to reject", " the null hypothesis"))
print(paste0("F(", (df1 - df2), ",", df2, ") = ", round(Fval, 4), ", p = ", round(pval, 4)))

#keep full!

# pull in whole set
crossPriceABA.long <- crossPriceABA.long.all %>%
  mutate(Education = recode(Education,
                            `Master's degree`                       = ">= 4 Yr College",
                            `Bachelor's degree in college (4-year)` = ">= 4 Yr College",
                            `Associate degree in college (2-year)`  = "<= 2 Yr College",
                            `Some college but no degree`            = "<= 2 Yr College",
                            `High school graduate (high school diploma or equivalent including GED)` = "No College")) %>%
  mutate(Education = factor(Education,
                     levels = c("No College", "<= 2 Yr College", ">= 4 Yr College" ))) %>%
  mutate(FamilySize = ifelse(numChildren > 1, "Multiple", "Single"))

crossPriceABA.long <- crossPriceABA.long %>%
  mutate(ResponseID = factor(ResponseID),
         bxConcern = factor(bxConcern),
         Sex = factor(Sex),
         FamilySize = factor(FamilySize)) %>%
  arrange(ResponseID, Price)

crossPriceABA.long.clean = crossPriceABA.long %>%
  filter(!(ResponseID %in% skipThese ))

noRE <- gnls(log((Consumption * 0.5) + ((0.5^2) * (Consumption^2) + 1)^0.5)/log(10) ~
              log((q0 * 0.5) + ((0.5^2) * (q0^2) + 1)^0.5)/log(10) +
              k *
              (exp(-alpha * q0 * Price) - 1),
            data = crossPriceABA.long.clean,
            na.action = na.omit,
            params = list(q0 + alpha ~ Education + Sex + FamilySize, k ~ 1),
            start = list( fixed = c(q0 = c(12, 1, 1, 1, 1),
                                    alpha = rep(0.001, 5),
                                    k = 1)),
            weights = varPower(),
            control = gnlsControl(msMaxIter = 1000,
                                  maxIter = 1000,
                                  tolerance = 1,
                                  nlsTol = 1,
                                  apVar = F,
                                  returnObject = TRUE))

withRE <- nlme(log((Consumption * 0.5) + ((0.5^2) * (Consumption^2) + 1)^0.5)/log(10) ~
                 log((q0 * 0.5) + ((0.5^2) * (q0^2) + 1)^0.5)/log(10) +
                 k *(exp(-alpha * q0 * Price) - 1),
               data = crossPriceABA.long.clean,
               na.action = na.omit,
               fixed = list(q0 + alpha ~ Education + Sex + FamilySize,
                            k ~ 1),
               random = list(pdDiag(q0 + alpha ~ 1)),
               start = coef(noRE),
               groups = ~ ResponseID,
               control = nlmeControl(msMaxIter = 1000,
                                     maxIter = 1000,
                                     tolerance = 1,
                                     nlsTol = 1,
                                     apVar = F,
                                     returnObject = TRUE))

# RE's worth the df
anova(noRE, withRE)

withRE.full <- nlme(log((Consumption * 0.5) + ((0.5^2) * (Consumption^2) + 1)^0.5)/log(10) ~
                      log((q0 * 0.5) + ((0.5^2) * (q0^2) + 1)^0.5)/log(10) +
                      k *
                      (exp(-alpha * q0 * Price) - 1),
                    data = crossPriceABA.long,
                    na.action = na.omit,
                    fixed = list(q0 + alpha ~ Education + Sex + FamilySize,
                                 k ~ 1),
                    random = list(pdDiag(q0 + alpha ~ 1)),
                    start = fixed.effects(withRE),
                    groups = ~ ResponseID,
                    weights = varPower(),
                    control = nlmeControl(msMaxIter = 1000,
                                          maxIter = 1000,
                                          tolerance = 1,
                                          nlsTol = 1,
                                          apVar = F,
                                          returnObject = TRUE))

# very little overall difference in interpretation, go full
summary(withRE, correlations = FALSE)
summary(withRE.full, correlations = FALSE)

pmaxFrame = crossPriceABA.long %>%
  select(Price, Consumption)

zbeFit.full.closed.Full <- nlmrt::wrapnls(
  formula = log((Consumption * 0.5) + ((0.5^2) * (Consumption^2) + 1)^0.5)/log(10) ~
    log((q0 * 0.5) + ((0.5^2) * (q0^2) + 1)^0.5)/log(10) +
    k *
    (exp(-alpha * q0 * Price) - 1),
  start   = list(q0    = 15,
                 k     = 1,
                 alpha = 0.01),
  data    = pmaxFrame)

ps <- seq(0.1, 5000, length.out = 10000)

q0     <- as.numeric(coef(zbeFit.full.closed.Full)[1])
alpha  <- as.numeric(coef(zbeFit.full.closed.Full)[3])
k      <- as.numeric(coef(zbeFit.full.closed.Full)[2])

dataProj <- data.frame(
  Prices      = ps,
  Projections = NA,
  Work        = NA
)

dataProj$Projections <- log((q0 * 0.5) + ((0.5^2) * (q0^2) + 1)^0.5)/log(10) +
  k *
  (exp(-alpha * q0 * dataProj$Prices) - 1)

# calculate lower limit
llower = tn$inverse(tn$transform(q0) - k)

# clip at lower limit, prevent tail resulting from llower
dataProj$Projections = tn$inverse(dataProj$Projections) - llower

dataProj$Work <- dataProj$Prices * dataProj$Projections

dataProj[which.max(dataProj$Work),]

#Prices    Projections  Work
#590.6472  2.523025     1490.218

plot(dataProj$Prices, dataProj$Work,
     log = 'x',
     main = 'projected expenditure')
