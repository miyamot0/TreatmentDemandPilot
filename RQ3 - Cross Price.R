# Note this is the final analysis for RQ2

library(beezdemand)
library(broom)
library(dplyr)
library(emmeans)
library(ggplot2)
library(kableExtra)
library(knitr)
library(MuMIn)
library(nlme)
library(readtext)
library(scales)
library(tibble)
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
  filter(!(ResponseID %in% skipThese )) %>%
  filter(Finished == 1)

crossPricePAT <- mSurveyRes %>%
  select(c(ResponseID,
           `Substitution Task_2(2)`,
           `Substitution Task_2(3)`,
           `Substitution Task_2(4)`,
           `Substitution Task_2(5)`,
           `Substitution Task_2(7)`,
           `Substitution Task_2(8)`,
           `Substitution Task_2(9)`,
           `Substitution Task_2(10)`,
           `Substitution Task_2(11)`,
           `Substitution Task_2(12)`,
           `Substitution Task_2(13)`,
           `Substitution Task_2(14)`,
           `Substitution Task_2(15)`,
           numChildren,
           bxConcern,
           Education,
           Sex,
           Income)
  ) %>%
  rename(`50`   = `Substitution Task_2(2)`,
         `100`  = `Substitution Task_2(3)`,
         `150`  = `Substitution Task_2(4)`,
         `200`  = `Substitution Task_2(5)`,
         `250`  = `Substitution Task_2(7)`,
         `300`  = `Substitution Task_2(8)`,
         `400`  = `Substitution Task_2(9)`,
         `500`  = `Substitution Task_2(10)`,
         `750`  = `Substitution Task_2(11)`,
         `1000` = `Substitution Task_2(12)`,
         `2000` = `Substitution Task_2(13)`,
         `3000` = `Substitution Task_2(14)`,
         `5000` = `Substitution Task_2(15)`)

crossPricePAT.long <- crossPricePAT %>%
  gather(Price, Consumption, colnames(crossPricePAT)[2:14], factor_key = TRUE) %>%
  mutate(Price = as.numeric(as.character(Price))) %>%
  mutate(Consumption = as.numeric(Consumption)) %>%
  as.data.frame()

crossPriceDemandGroupFrame <- crossPricePAT.long %>%
  group_by(Price) %>%
  summarise(
    ConsumptionAve = mean(Consumption),
    ConsumptionMdn = median(Consumption)
  )

crossPricePAT.long <- crossPricePAT.long %>%
  mutate(Education = recode(Education,
                            `Master's degree`                       = ">= 4 Yr College",
                            `Bachelor's degree in college (4-year)` = ">= 4 Yr College",
                            `Associate degree in college (2-year)`  = "<= 2 Yr College",
                            `Some college but no degree`            = "<= 2 Yr College",
                            `High school graduate (high school diploma or equivalent including GED)` = "No College")) %>%
  mutate(Education = factor(Education,
                            levels = c("No College", "<= 2 Yr College", ">= 4 Yr College" ))) %>%
  mutate(FamilySize = ifelse(numChildren > 1, "Multiple", "Single"))

geeFittingFrame <- as_tibble(crossPricePAT.long) %>%
  mutate(NewID = as.numeric(factor(ResponseID, levels = unique(ResponseID)))) %>%
  select(NewID, Price, Consumption, Sex, Education, FamilySize) %>%
  arrange(NewID, Price, Consumption, Sex, Education, FamilySize)

library(geepack)

fit.full <- geeglm(Consumption ~ Price * Sex * FamilySize * Education,
               id = NewID,
               data = geeFittingFrame,
               family = gaussian,
               corstr = "exchangeable")

fit.1 <- geeglm(Consumption ~ Price * Sex * FamilySize + Education,
                   id = NewID,
                   data = geeFittingFrame,
                   family = gaussian,
                   corstr = "exchangeable")

fit.2 <- geeglm(Consumption ~ Price * Sex * FamilySize,
                id = NewID,
                data = geeFittingFrame,
                family = gaussian,
                corstr = "exchangeable")

fit.3 <- geeglm(Consumption ~ Price * Sex + FamilySize,
                id = NewID,
                data = geeFittingFrame,
                family = gaussian,
                corstr = "exchangeable")

fit.4 <- geeglm(Consumption ~ Price + Sex + FamilySize,
                id = NewID,
                data = geeFittingFrame,
                family = gaussian,
                corstr = "exchangeable")

fit.5 <- geeglm(Consumption ~ Price + Sex,
                id = NewID,
                data = geeFittingFrame,
                family = gaussian,
                corstr = "exchangeable")

fit.6 <- geeglm(Consumption ~ Price,
                id = NewID,
                data = geeFittingFrame,
                family = gaussian,
                corstr = "exchangeable")

model.sel(fit.full,
          fit.1,
          fit.2,
          fit.3,
          fit.4,
          fit.5,
          fit.6,
          rank = "QIC")

emmeaner <- emmeans(fit.6, specs = pairwise ~ Price, type = "response",
                    at = list(Price = c(50,
                                        100,
                                        150,
                                        200,
                                        250,
                                        300,
                                        400,
                                        500,
                                        750,
                                        1000,
                                        2000,
                                        3000,
                                        5000)))

plotFrame <- emmip(fit.6, ~ Price,
      CIs = TRUE,
      type = "response",
      at = list(Price = c(50,
                          100,
                          150,
                          200,
                          250,
                          300,
                          400,
                          500,
                          750,
                          1000,
                          2000,
                          3000,
                          5000)),
      plotit = FALSE)

geeFittingFrame$Price = factor(geeFittingFrame$Price,
                               levels = c(50,
                                          100,
                                          150,
                                          200,
                                          250,
                                          300,
                                          400,
                                          500,
                                          750,
                                          1000,
                                          2000,
                                          3000,
                                          5000),
                               labels = c(50,
                                          100,
                                          150,
                                          200,
                                          250,
                                          300,
                                          400,
                                          500,
                                          750,
                                          1000,
                                          2000,
                                          3000,
                                          5000))

pos = position_jitter(0.125, 0.025)

geeFittingFrame$Price_J = jitter(as.numeric(as.character(geeFittingFrame$Price)),
                                 factor = 1)

geeFittingFrame$Consumption_J = jitter(as.numeric(geeFittingFrame$Consumption),
                                       factor = 1)

plot = ggplot(geeFittingFrame, aes(Price_J, Consumption_J, group = NewID)) +
  geom_line(alpha = 0.125) +
  geom_point(alpha = 0.125) +
  scale_x_continuous(breaks = c(50, 100, 500, 1000, 5000),
                     labels = c(50, 100, 500, 1000, 5000),
                     limits = c(40, 6000),
                     trans = tn) +
  scale_y_continuous(breaks = c(0, 1, 5, 10, 20),
                     labels = c(0, 1, 5, 10, 20),
                     limits = c(-0.25, 22),
                     trans = tn) +
  geom_line(data     = plotFrame,
            mapping  = aes(x = xvar,
                           y = yvar,
                           group = 1),
            size     = 1.25,
            inherit.aes = FALSE) +
  geom_errorbar(data     = plotFrame,
                mapping  = aes(ymin = LCL,
                               ymax = UCL,
                               x    = xvar,
                               y    = yvar),
                width    = 0.05,
                inherit.aes = FALSE) +
  theme_bw() +
  xlab("Price/Hour of Evidence based Treatment") +
  ylab("'Alternative' Treatment Consumption (Hours)") +
  theme(legend.position  = "bottom",
        strip.background = element_blank(),
        strip.text       = element_text(colour = 'black',
                                        face   = 'bold'),
        text             = element_text(family = "Times New Roman",
                                        size   = 12),
        panel.spacing    = unit(1.25, "lines"),
        axis.title.y     = element_text(margin = margin(t = 0,
                                                        r = 3,
                                                        b = 0,
                                                        l = 1)),
        axis.title.x     = element_text(margin = margin(t = 10,
                                                        r = 0,
                                                        b = 0,
                                                        l = 0))) +
  beezdemand::theme_apa()

plot

library(Cairo)

ggsave("Figure 3 - Cross Price.png",
       plot = plot,
       units = "in",
       height = 5,
       width = 9,
       dpi = 300,
       device = "png")
