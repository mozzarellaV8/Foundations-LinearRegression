# Foundations of Data Science
# Unit 7.1 - Linear Regression Exercise

## Exercise: least squares regression
## ──────────────────────────────────

##   Use the /states.rds/ data set. 
##   Fit a model predicting energy consumed per capita (energy) 
##   from the percentage of residents living in metropolitan areas (metro). 

##   1. Examine/plot the data before fitting the model
##   2. Print and interpret the model `summary'
##   3. `plot' the model to look for deviations from modeling assumptions

##   Select one or more additional predictors to add to your model and
##   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?

# load data -------------------------------------------------------------------

states.data <- readRDS("data/states.rds")

# derive labels from attributes
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])
tail(states.info, 8)

# preliminary analysis --------------------------------------------------------
# for an energy ~ metro model

metro.energy <- subset(states.data, select = c("metro", "energy"))
summary(metro.energy)
is.na(metro.energy)
# Looks like there is only 1 NA row - the District of Columbia. 
# Because it's a Federal District and relatively small population, 
# I feel OK omitting it - 
# if only just to get an _approximate_ sense of correlation 
# between Metropolitan Population % and Energy Consumption.

cor(na.omit(metro.energy))
#             metro     energy
# metro   1.0000000 -0.3397445
# energy -0.3397445  1.0000000

# Seeing a weak downhill linear relationship - cor value at -0.339.

par(mfrow = c(1, 1), mar = c(8, 8, 8, 8), family = "Times")
plot(metro.energy, xlab = "Metropolitan area population, %",
     ylab = "Per capita energy consumed, Btu",
     main = "Energy Consumption ~ Metropolitan Population %, US")

library(ggplot2)

metro.energyP1 <- ggplot(metro.energy, aes(metro, energy)) + 
  theme_minimal() +
  geom_point(aes(color = metro), size = 4.75, shape = 17) +
  ggtitle("Energy Consumption ~ Metropolitan Population % (US)") +
  theme(plot.title = element_text(family = "Times", face = "bold", size = 18)) +
  labs(x = "Metropolitan area population, %", y = "Per capita energy consumed, BTU") +
  theme(axis.title.x = element_text(family = "Times", face = "italic", size = 14)) +
  theme(axis.title.y = element_text(family = "Times", face = "italic", size = 14)) +
  theme(axis.text.x = element_text(family = "Times", face = "plain", size = 11)) +
  theme(axis.text.y = element_text(family = "Times", face = "plain", size = 11)) +
  theme(plot.margin = unit(c(3, 3, 3, 3), "cm"))

# model 01 - Energy ~ Metropolitan --------------------------------------------

energy.metro.mod <- lm(energy ~ metro, data = states.data)
summary(energy.metro.mod)

# My intuition coming into this was that the more people living in 
# metropolitan areas, the more energy would be consumed per capita. 
# But judging from the Coefficients, % living in metropolitan areas 
# doesn't seem to be the strongest predictor of energy consumption:

# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
#  (Intercept) 501.0292    61.8136   8.105 1.53e-10 ***
#  metro        -2.2871     0.9139  -2.503   0.0158 *  

# The R-Squared values also appear to suggest a weak relationship: 
# Multiple R-squared:  0.1154,	Adjusted R-squared:  0.097 

par(mfrow = c(2, 2), mar = c(7, 7, 7, 7), family = "Times")
plot(energy.metro.mod, 
     main = "Energy Consumption ~ % of Population Living in Metropolitan Areas, U.S.")

class(energy.metro.mod)
names(energy.metro.mod)
methods(class = class(energy.metro.mod))[1:9]

# Let's plot again with outliers highlighted and lm line fit. 
metro.energyP1 <- metro.energyP1 + 
  stat_smooth(method = lm, level = 0.95, se = FALSE, colour = "#CD2626")

metro.energyP2 <- metro.energyP1 + 
  annotate("text", x = 35, y = 765, label = "Wyoming; 29.6%, 786 btu",
           family = "Times", size = 4) +
  annotate("text", x = 46, y = 972, label = "Alaska; 41.6%, 991 btu",
           family = "Times", size = 4) +
  annotate("text", x = 74.45, y = 764, label = "Louisiana; 69.5%, 783 btu",
           family = "Times", size = 4) +
  annotate("text", x = 86.25, y = 550.25, label = "Texas; 64.86%, 569 btu",
           family = "Times", size = 4) +
  theme(legend.title = element_text(family = "Times")) +
  theme(legend.text = element_text(family = "Times"))

metro.energyP2

metro.energyP3 <- metro.energyP2 +
  annotate("text", x = 106, y = 316, label = "New Jersey; 100%, 296 btu",
           family = "Times", size = 4) +
  annotate("text", x = 29, y = 212, label = "Vermont; 23.4%, 232 btu",
           family = "Times", size = 4) +
  labs(x = "% of population living in metropolitan areas",
       y = "per capita energy consumed, BTU")

metro.energyP3

# Outlier commentary --------------------------------------

# The outliers in this case appear to be states 2, 19, and 51 - 
# Alaska, Louisiana, and Wyoming. 

# When ranked by total population,
# Wyoming and Alaska are the top 2 least populated states, respectively.
# Louisiana falls toward the middle of this list. 

# The same goes for population density.

# What might be interesting is that Wyoming tops the list of greenhouse 
# gases released, Louisiana tops the list of toxics released. 
# These factors may be a result rather than cause of energy consumption, though.

# Alaska has the most % of adults with a HS diploma, most area + least density.

# All three of these outlier states rank in the top 8 least % of 
# House and Senate voting on environmental law. Could it be that
# legislation is a better predictor of energy consumption than 
# metropolitan population?

# Perhaps house and senate %, or density of population would 
# make a better predictor of energy consumption.

# Another explanatory variable might be `area` - the land area of 
# state in square miles. 

# Correlation Plot ------------------------------------------------------------

# Before fitting a model with variaables of my own choosing, I'd like to do 
# a correlation plot to see what relationships exist between the 
# independent variables.

states.q <- states.data
states.index <- data.frame(state.num = 1:51, state = states.data$state)

# remove categorical data for cor()
states.q$region <- NULL
states.q$state <- NULL

states.cor <- cor(states.q, use = "complete.obs")
states.cor <- round(states.cor, digits = 2)

# let's keep these tables handy
write.table(states.cor, file = "state_cor.csv", sep = ",", row.names = T)

library(corrplot)
library(extrafontdb)
library(extrafont)
fonttable()

par(mfrow = c(1, 1), mar = c(8, 8, 6, 12), family = "Arial Rounded MT Bold")
corrplot(states.cor)

# shade
par(mfrow = c(1, 1), mar = c(12, 6, 8, 20), family = "Arial Rounded MT Bold")
corrplot(states.cor, method = "shade", shade.col = NA, tl.col = "firebrick3", 
         tl.srt = 45, tl.cex = 1.0)

# method h-clust
par(mfrow = c(1, 1), mar = c(12, 6, 8, 20), family = "Arial Rounded MT Bold")
corrplot(states.cor, method = "ellipse", order = "hclust", hclust.method = "ward.D",
         tl.col = "firebrick3", tl.srt = 45, tl.cex = 1.0)

# ellipse
corrplot(states.cor, method = "ellipse", is.corr = FALSE, tl.col = "Firebrick3",
         tl.srt = 45, tl.cex = 1.0, order = "FPC")

# OK - so the main problem with this correlation plot is that I removed the categorical variable
# region - which might actually be the strongest predictor of energy consumption in this dataset. 

# Revisiting my first intuitions - particularly on why Alaska
# might be a leader in energy consumption - got me to think about
# the uniquely cold climate of Alaska and their role in energy production,
# namely crude oil, mining for natural resources and such. 

# Further investigation lead me to believe the reason certain states
# have higher energy consumption rates is because of the energy 
# production industry. 

# So! Let's take a look at energy as a function of waste, toxics, 
# greenhouse gas emission variables. 

# I am suspicious about this model but would like to see what it returns nonetheless.

# Model 02 - Energy ~ Toxics + Green ------------------------------------------

summary(lm(energy ~ waste + toxic + green, data = states.data))
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
#  (Intercept) 157.4614    45.6658   3.448  0.00125 ** 
#  waste        19.9039    37.9809   0.524  0.60287    
#  toxic         2.6940     0.4805   5.607 1.27e-06 ***
#  green         4.7469     0.5565   8.529 7.10e-11 ***

# I'm going to eliminate the waste variable.

toxic.green.model <- lm(energy ~ toxic + green, data = states.data)
summary(toxic.green.model)
#   Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
#  (Intercept) 179.8260    16.1194  11.156 1.51e-14 ***
#  toxic         2.6455     0.4676   5.657 1.01e-06 ***
#  green         4.6722     0.5336   8.756 2.81e-11 ***

#   Multiple R-squared:  0.7627,	Adjusted R-squared:  0.7521

# This model has the highest R-squared and coefficient significance yet. 
# Could it be that toxic and green are too highly correlated, or
# exhibiting multicolllinearity? Also, could energy consumption and 
# greenhouse gas emissions be too correlated? 

cor(states.data$toxic, states.data$green, use = "pairwise")
# 0.2622973
cor(states.data$toxic, states.data$energy, use = "pairwise")
# 0.5624524
cor(states.data$green, states.data$energy, use = "pairwise")
# 0.7706181

# The most highly correlated are `Energy` and `Green`.

par(mfrow = c(1, 1), mar = c(6, 6, 6, 6))
plot(states.data$energy, states.data$toxic)
plot(states.data$energy, states.data$green)

energy.toxic.plot <- ggplot(states.data, aes(toxic, energy)) +
  theme_minimal() +
  geom_point(aes(color = toxic), size = 4.75, shape = 17) +
  stat_smooth(method = lm, se = FALSE, colour = "#CD2626") +
  ggtitle("Energy Consumption ~ Per capita toxics released (US)") +
  theme(plot.title = element_text(family = "Times", face = "bold", size = 18)) +
  labs(x = "Per capita toxics released, lbs.", y = "Per capita energy consumed, BTU") +
  theme(axis.title.x = element_text(family = "Times", face = "italic", size = 14)) +
  theme(axis.title.y = element_text(family = "Times", face = "italic", size = 14)) +
  theme(axis.text.x = element_text(family = "Times", face = "plain", size = 11)) +
  theme(axis.text.y = element_text(family = "Times", face = "plain", size = 11)) +
  theme(plot.margin = unit(c(3, 3, 3, 3), "cm"))

energy.toxic.plot

energy.green.plot <- ggplot(states.data, aes(green, energy)) +
  theme_minimal() +
  geom_point(aes(color = green), size = 4.75, shape = 17) +
  stat_smooth(method = lm, se = FALSE, colour = "#CD2626") +
  ggtitle("Energy Consumption ~ Per capita greenhouse gas (US)") +
  theme(plot.title = element_text(family = "Times", face = "bold", size = 18)) +
  labs(x = "Per capita greenhouse gas, tons", y = "Per capita energy consumed, BTU") +
  theme(axis.title.x = element_text(family = "Times", face = "italic", size = 14)) +
  theme(axis.title.y = element_text(family = "Times", face = "italic", size = 14)) +
  theme(axis.text.x = element_text(family = "Times", face = "plain", size = 11)) +
  theme(axis.text.y = element_text(family = "Times", face = "plain", size = 11)) +
  theme(plot.margin = unit(c(3, 3, 3, 3), "cm"))

energy.green.plot

# So this model appears to be stronger than the original model using Metropolitan
# area percentage. But my intuition again says that cause and effect, while they 
# cannot be definitely inferred, are still too confused using these variables.
# I.E., greenhouse gas emissions could be a result of energy consumption,
# rather than an explanation for it.

confint(toxic.green.model)
#                  2.5 %     97.5 %
# (Intercept) 147.359753 212.292223
# toxic         1.703605   3.587324
# green         3.597436   5.747016

anova(energy.metro.mod, toxic.green.model)

# Using AIC to choose variables -----------------------------------------------

library(MASS)

states.factors <- states.data
states.factors$state <- as.factor(states.factors$state)
states.factors$state <- NULL

model <- lm(energy ~ ., data = states.factors)
summary(model)


# Coefficients: (1 not defined because of singularities)
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   -2.363e+02  5.794e+02  -0.408  0.68667    
# regionN. East  4.008e-01  6.942e+01   0.006  0.99544    
# regionSouth    7.089e+01  4.717e+01   1.503  0.14445    
# regionMidwest  4.577e+01  4.300e+01   1.064  0.29658    
# pop           -2.480e-07  3.866e-06  -0.064  0.94931    
# area           8.315e-04  3.766e-04   2.208  0.03594 *  
# density        3.904e-02  9.142e-02   0.427  0.67270    
# metro          2.549e-01  9.508e-01   0.268  0.79063    
# waste         -1.449e+01  6.244e+01  -0.232  0.81824    
# miles          4.013e+00  1.558e+01   0.258  0.79868    
# toxic          2.510e+00  6.768e-01   3.709  0.00095 ***
# green          4.527e+00  9.412e-01   4.809 5.08e-05 ***
# house          1.624e-01  1.010e+00   0.161  0.87340    
# senate        -8.870e-02  6.475e-01  -0.137  0.89206    
# csat          -1.719e+00  2.826e+00  -0.608  0.54808    
# vsat           3.761e+00  5.926e+00   0.635  0.53095    
# msat                  NA         NA      NA       NA    
# percent        8.267e-01  1.695e+00   0.488  0.62962    
# expense        1.455e-02  1.627e-02   0.894  0.37915    
# income         2.941e-01  4.679e+00   0.063  0.95034    
# high           3.301e+00  4.806e+00   0.687  0.49802    
# college       -6.965e+00  6.358e+00  -1.096  0.28296    

# Multiple R-squared:  0.8379,	Adjusted R-squared:  0.7178 
# F-statistic: 6.978 on 20 and 27 DF,  p-value: 3.173e-06

# Toxic and green again show up as the strongest independent variables. 

aic <- step(model)
summary(aic)

# This doesn't really look great. What about getting rid of half the variables
# related to college testing, that don't seem to be relevant?

states.factors$college <- NULL
states.factors$msat <- NULL
states.factors$vsat <- NULL
states.factors$csat <- NULL
states.factors$expense <- NULL
states.factors$percent <- NULL
states.factors$high <- NULL

model02 <- lm(energy ~ ., data = states.factors)
summary(model02)

# Significant coefficients again are toxic and green. Of less significance are
# area and just barely - regionSouth.

aic02 <- step(model02)
# Step:  AIC=391.48
# energy ~ region + area + toxic + green

#          Df Sum of Sq    RSS    AIC
# - region  3     16412 141324 391.41
# <none>                124912 391.48
# - area    1     17972 142884 395.93
# - toxic   1     77706 202618 412.70
# - green   1    238288 363200 440.71

# Step:  AIC=391.41
# energy ~ area + toxic + green

#          Df Sum of Sq    RSS    AIC
#  <none>               141324 391.41
#  - area   1     11258 152582 393.08
#  - toxic  1     99897 241221 415.07
#  - green  1    237987 379311 436.80

# With college test and high school expense variables removed, we have 
# the lowest AIC values again in the toxic, green, and area variables. 

aicToxicGreen <- step(toxic.green.model)
# Start:  AIC=393.08
# energy ~ toxic + green

#           Df Sum of Sq    RSS    AIC
#   <none>               152582 393.08
#   - toxic  1    108514 261096 416.87
#   - green  1    259929 412511 438.82

step(energy.metro.mod)
# Start:  AIC=496.25
# energy ~ metro

#         Df Sum of Sq     RSS    AIC
# <none>                943103 496.25
# - metro  1    123064 1066166 500.38


# Exercise Part 2 -------------------------------------------------------------
## Exercise: interactions and factors

##   Use the states data set.

##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.

##   2. Try adding region to the model. Are there significant differences
##      across the four regions?

toxic.green.interaction <- lm(energy ~ (toxic + green)*area, data = states.data)
summary(toxic.green.interaction)

toxic.green.region <- lm(energy ~ toxic+green+region, data = states.data)
summary(toxic.green.region)

levels(states.data$region)
#  "West"    "N. East" "South"   "Midwest"

energy.region <- lm(energy ~ region, data = states.data)
summary(energy.region)
# Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
#       (Intercept)     405.62      39.23  10.339  1.4e-13 ***
#       regionN. East  -156.50      61.34  -2.552   0.0141 *  
#       regionSouth     -25.49      52.82  -0.483   0.6317    
#       regionMidwest   -61.62      56.63  -1.088   0.2822  

# The only region not included in the coefficients is the West.
# This wikipedia article seems to also cite that region. 

anova(energy.region)
#           Df Sum Sq Mean Sq F value  Pr(>F)  
# region     3 145757   48586  2.4282 0.07737 .
# Residuals 46 920410   20009   
