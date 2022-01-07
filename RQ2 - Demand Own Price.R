# Note this is the final analysis for RQ2

library(beezdemand)
library(broom)
library(dplyr)
library(ggplot2)
library(ggsci)
library(kableExtra)
library(knitr)
library(nlme)
library(readtext)
library(scales)
library(tidyr)

tn <- trans_new("inhs",
                function(x) log((x * 0.5) + ((0.5^2) * (x^2) + 1)^0.5)/log(10),
                function(y) (1/10^(1*y))*((10^(2*y)) - 1),
                domain = c(0, Inf),
                breaks = c(0, 0.1, 1, 10, 100))

mSurveyRes <- readxl::read_xlsx("RawResults.xlsx",
                                na = "NA")

# skipThese <- c("R_10vBc6q1iYD6juF", "R_ZJKpsAPP9SYGJpL",
#                "R_x9LkdDQPqQE5t8B", "R_bBf7jB2Sawm4T3b",
#                "R_Di2tmQrWWLHg2iJ", "R_1oF4LBnCdBU70XC",
#                "R_u8tvJegfSTRC14B", "R_2anmKehkAtc6Pf2",
#                "R_REM9nx7gSRyNfrz")

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

crossPriceABA.long <- crossPriceABA %>%
  gather(Price, Consumption, colnames(crossPriceABA)[2:14], factor_key = TRUE) %>%
  mutate(Price = as.numeric(as.character(Price))) %>%
  mutate(Consumption = as.numeric(Consumption)) %>%
  as.data.frame()

crossPriceABA.Full <- mSurveyRes %>%
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

crossPriceABA.long.Full <- crossPriceABA.Full %>%
  gather(Price, Consumption, colnames(crossPriceABA)[2:14], factor_key = TRUE) %>%
  mutate(Price = as.numeric(as.character(Price))) %>%
  mutate(Consumption = as.numeric(Consumption)) %>%
  as.data.frame()

crossPriceDemandGroupFrame <- crossPriceABA.long %>%
  group_by(Price) %>%
  summarise(
    ConsumptionAve = mean(Consumption),
    ConsumptionMdn = median(Consumption),
  )

crossPriceABA.long <- crossPriceABA.long %>%
  mutate(Education = recode(Education,
                            `Master's degree`                       = ">= 4 Yr College",
                            `Bachelor's degree in college (4-year)` = ">= 4 Yr College",
                            `Associate degree in college (2-year)`  = "<= 2 Yr College",
                            `Some college but no degree`            = "<= 2 Yr College",
                            `High school graduate (high school diploma or equivalent including GED)` = "No College")) %>%
  mutate(Education = factor(Education,
                     levels = c("No College", "<= 2 Yr College", ">= 4 Yr College" ))) %>%
  mutate(FamilySize = ifelse(numChildren > 1, "Multiple", "Single"))

out <- gnls(log((Consumption * 0.5) + ((0.5^2) * (Consumption^2) + 1)^0.5)/log(10) ~
              log((q0 * 0.5) + ((0.5^2) * (q0^2) + 1)^0.5)/log(10) +
              k *
              (exp(-alpha * q0 * Price) - 1),
            data = crossPriceABA.long,
            na.action = na.omit,
            params = list(q0 + alpha ~ Education + Sex + FamilySize, k ~ 1),
            start = list( fixed = c(q0 = rep(12, 5),
                                    alpha = rep(0.001, 5),
                                    k = 1)),
            weights = varPower(),
            control = gnlsControl(msMaxIter = 200,
                                  maxIter = 300,
                                  tolerance = 1,
                                  nlsTol = 1,
                                  apVar = F,
                                  returnObject = TRUE,
                                  opt = "optim"))

crossPriceABA.long$FamilySize = as.factor(crossPriceABA.long$FamilySize)
crossPriceABA.long$Sex = as.factor(crossPriceABA.long$Sex)

out2 <- nlme(log((Consumption * 0.5) + ((0.5^2) * (Consumption^2) + 1)^0.5)/log(10) ~
               log((q0 * 0.5) + ((0.5^2) * (q0^2) + 1)^0.5)/log(10) +
               k *
               (exp(-alpha * q0 * Price) - 1),
             data = crossPriceABA.long,
             na.action = na.omit,
             fixed = list(q0 + alpha ~ Education + Sex + FamilySize,
                          k ~ 1),
             random = list(pdDiag(q0 + alpha ~ 1)),
             start = list( fixed = c(q0 = rep(12, 5),
                                     alpha = rep(0.001, 5),
                                     k = 1)),
             groups = ~ ResponseID,
             weights = varPower(),
             control = nlmeControl(msMaxIter = 1000,
                                   maxIter = 1000,
                                   tolerance = 1,
                                   nlsTol = 1,
                                   apVar = F,
                                   #minScale = .0001,
                                   returnObject = TRUE))

anova(out, out2)

crossPriceABA.long$pred  <- tn$inverse(predict(out2, level = 0))
crossPriceABA.long$predi <- tn$inverse(predict(out2, level = 1))

crossPriceABA.long = crossPriceABA.long %>%
  rename(`Family Size` = FamilySize) %>%
  rename(`Gender` = Sex)

plot <- ggplot(crossPriceABA.long, aes(Price, pred,
                                       lty = `Family Size`)) +
  geom_line(size = 1) +
  geom_line(mapping = aes(Price, predi,
                          lty = `Family Size`,
                          group = ResponseID),
            size  = 0.8,
            alpha = 0.25) +
  scale_x_continuous(breaks = c(50, 100, 500, 1000, 5000),
                     labels = c(50, 100, 500, 1000, 5000),
                     limits = c(50, 5000),
                     trans = tn) +
  scale_y_continuous(breaks = c(0, 1, 5, 10, 20),
                     labels = c(0, 1, 5, 10, 20),
                     limits = c(-1, 25),
                     trans = tn) +
  theme_bw() +
  scale_color_grey() +
  xlab("Price/Hour of Treatment") +
  ylab("Treatment Consumption (Hours)") +
  theme(legend.position  = "bottom",
        strip.background = element_blank(),
        strip.text       = element_text(colour = 'black',
                                        face   = 'bold'),
        text             = element_text(family = "Times New Roman",
                                        size   = 10),
        panel.spacing    = unit(1.25, "lines"),
        axis.title.y     = element_text(margin = margin(t = 0,
                                                        r = 2,
                                                        b = 0,
                                                        l = 0)),
        axis.title.x     = element_text(margin = margin(t = 10,
                                                        r = 0,
                                                        b = 0,
                                                        l = 0))) +
  facet_wrap(Gender ~ Education,
             ncol = 3,
             scales = "free") +
  beezdemand::theme_apa()

ggsave("Figure 2 - Own Price.png",
       plot = plot,
       units = "in",
       height = 5,
       width = 10,
       device = "png",
       dpi = 300)

