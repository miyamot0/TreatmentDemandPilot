# Note this is the final analysis for RQ1

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

pureDemandFrame <- mSurveyRes %>%
  select(c(ResponseID,
           `Pure Demand Task_1(2)`,
           `Pure Demand Task_1(3)`,
           `Pure Demand Task_1(4)`,
           `Pure Demand Task_1(5)`,
           `Pure Demand Task_1(8)`,
           `Pure Demand Task_1(9)`,
           `Pure Demand Task_1(10)`,
           `Pure Demand Task_1(11)`,
           `Pure Demand Task_1(12)`,
           `Pure Demand Task_1(13)`,
           `Pure Demand Task_1(14)`,
           `Pure Demand Task_1(15)`,
           `Pure Demand Task_1(16)`,
           numChildren,
           bxConcern,
           Education,
           Sex,
           Income)) %>%
  rename(`50`   = `Pure Demand Task_1(2)`,
         `100`  = `Pure Demand Task_1(3)`,
         `150`  = `Pure Demand Task_1(4)`,
         `200`  = `Pure Demand Task_1(5)`,
         `250`  = `Pure Demand Task_1(8)`,
         `300`  = `Pure Demand Task_1(9)`,
         `400`  = `Pure Demand Task_1(10)`,
         `500`  = `Pure Demand Task_1(11)`,
         `750`  = `Pure Demand Task_1(12)`,
         `1000` = `Pure Demand Task_1(13)`,
         `2000` = `Pure Demand Task_1(14)`,
         `3000` = `Pure Demand Task_1(15)`,
         `5000` = `Pure Demand Task_1(16)`)

pureDemandFrame.long <- pureDemandFrame %>%
  gather(Price, Consumption, colnames(pureDemandFrame)[2:14], factor_key = TRUE) %>%
  mutate(Price = as.numeric(as.character(Price))) %>%
  mutate(Consumption = as.numeric(Consumption)) %>%
  as.data.frame()

pureDemandFrame.long <- pureDemandFrame.long %>%
  mutate(Education = recode(Education,
                            `Master's degree`                       = ">= 4 Yr College",
                            `Bachelor's degree in college (4-year)` = ">= 4 Yr College",
                            `Associate degree in college (2-year)`  = "<= 2 Yr College",
                            `Some college but no degree`            = "<= 2 Yr College",
                            `High school graduate (high school diploma or equivalent including GED)` = "No College")) %>%
  mutate(Education = factor(Education,
                     levels = c("No College", "<= 2 Yr College", ">= 4 Yr College" ))) %>%
  mutate(FamilySize = ifelse(numChildren > 1, "Multiple", "Single"))

pureDemandFrame.long <- pureDemandFrame.long %>%
  mutate(ResponseID = factor(ResponseID),
         bxConcern = factor(bxConcern),
         Sex = factor(Sex),
         FamilySize = factor(FamilySize)) %>%
  arrange(ResponseID, Price)

out2 <- nlme(log((Consumption * 0.5) + ((0.5^2) * (Consumption^2) + 1)^0.5)/log(10) ~
             log((q0 * 0.5) + ((0.5^2) * (q0^2) + 1)^0.5)/log(10) +
             k *
             (exp(-alpha * q0 * Price) - 1),
           data = pureDemandFrame.long,
           na.action = na.omit,
           fixed = list(q0 + alpha ~ Education + Sex + FamilySize,
                        k ~ 1),
           random = list(pdDiag(q0 + alpha ~ 1)),
           start = list( fixed = c(q0 = rep(12, 5),
                                   alpha = rep(0.00001, 5),
                                   k = 1)),
           groups = ~ ResponseID,
           weights = varPower(),
           control = nlmeControl(msMaxIter = 1000,
                                 maxIter = 1000))

summary(out2)

pureDemandFrame.long$pred  <- tn$inverse(predict(out2, level = 0))
pureDemandFrame.long$predi <- tn$inverse(predict(out2, level = 1))

library(extrafont)

loadfonts(device = 'win')

pureDemandFrame.long = pureDemandFrame.long %>%
  rename(`Family Size` = FamilySize) %>%
  rename(`Gender` = Sex)

plot <- ggplot(pureDemandFrame.long,
               aes(Price, pred,
                   lty = Gender)) +
  geom_line(size = 1) +
  geom_line(mapping = aes(Price, predi,
                          lty = Gender,
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
                                                        r = 0,
                                                        b = 0,
                                                        l = 0)),
        axis.title.x     = element_text(margin = margin(t = 10,
                                                        r = 0,
                                                        b = 0,
                                                        l = 0))) +
  facet_wrap(`Family Size` ~ Education,
             ncol = 3,
             scales = "free") +
  beezdemand::theme_apa()

plot

ggsave("Figure 1 - Alone Price.png",
       plot = plot,
       units = "in",
       height = 5,
       width = 10,
       device = "png",
       dpi = 300)
