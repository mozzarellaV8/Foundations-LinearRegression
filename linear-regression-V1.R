# Foundations of Data Science
# Unit 7.1 - Linear Regression Exercise

## Exercise: least squares regression
## ────────────────────────────────────────

##   Use the /states.rds/ data set. Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro). Be sure to
##   1. Examine/plot the data before fitting the model
##   2. Print and interpret the model `summary'
##   3. `plot' the model to look for deviations from modeling assumptions

##   Select one or more additional predictors to add to your model and
##   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?

# load data -------------------------------------------------------------------

states.data <- readRDS("data/states.rds")

# get labels
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])

#look at last few labels
tail(states.info, 8)
head(states.info, 10)
summary(states.data$metro)
summary(states.data$energy)

# preliminary analysis --------------------------------------------------------
# for an energy ~ metro model

metro.energy <- subset(states.data, select = c("metro", "energy"))
summary(metro.energy)

cor(metro.energy) 
# It looks like the NA values throw off the correlation test.
# Even though I'd be throwing out data, I'd like to see some value.
# How many NA's are there? Significant amount?

is.na(metro.energy)
# Looks like there is only 1 NA row - the District of Columbia. 
# Because it's a special district and not a state, I feel OK omitting it - 
# if just to get an approximate sense of correlation 
# between Metropolitan Population and Energy Consumption.

cor(na.omit(metro.energy))
#             metro     energy
# metro   1.0000000 -0.3397445
# energy -0.3397445  1.0000000
# seeing a weak downhill linear relationship - cor value at -0.339.

par(mfrow = c(1, 1), mar = c(5, 5, 5, 5))
plot(metro.energy, xlab = "Metropolitan area population, %",
     ylab = "Per capita energy consumed, Btu",
     main = "Energy Consumption ~ Metropolitan Population %, US")

library(ggplot2)

metro.energyP1 <- ggplot(metro.energy, aes(metro, energy)) + theme_minimal() +
  geom_point(aes(color = energy), size = 5, shape = 19) +
  ggtitle("Energy Consumption ~ Metropolitan Population %, US") +
  theme(plot.title = element_text(family = "Times", face = "bold")) +
  labs(x = "Metropolitan area population, %", y = "Per capita energy consumed, Btu") +
  theme(axis.title.x = element_text(family = "Times", face = "italic")) +
  theme(axis.title.y = element_text(family = "Times", face = "italic")) +
  theme(axis.text.x = element_text(family = "Times", face = "plain")) +
  theme(axis.text.y = element_text(family = "Times", face = "plain")) +
  theme(plot.margin = unit(c(3, 3, 3, 3), "cm"))

metro.energyP1

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

par(mfrow = c(2, 2), mar = c(7, 7, 7, 7))
plot(energy.metro.mod)


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

# model 02 - House Voting and State Area --------------------------------------

# house pre-plot --------------------------------------
par(mfrow = c(1, 1), mar = c(5, 5, 5, 5))
plot(states.data$house, states.data$energy,
     xlab = "House '91 environ. voting, %",
     ylab = "Per capita energy consumed, BTU",
     main = "Energy Consumption ~ House Voting")

# state pre-plot -----------------------------------------
par(mfrow = c(1, 1), mar = c(5, 5, 5, 5))
plot(states.data$house, states.data$area,
     xlab = "Land area, square miles",
     ylab = "Per capita energy consumed, BTU",
     main = "Energy Consumption ~ Land Area")

# correlation tests for House and Area --------------------

house.energy <- subset(states.data, select = c("house", "energy"))
cor(na.omit(house.energy))
#             house     energy
# house   1.0000000 -0.6346872
# energy -0.6346872  1.0000000
# A stronger downhill relationship, above a -0.50 threshold, is observed.

area.energy <- subset(states.data, select = c("area", "energy"))
cor(na.omit(area.energy))
#             area    energy
# area   1.0000000 0.6626519
# energy 0.6626519 1.0000000
# A stronger uphill relationship, above a 0.50 threshold, is observed.

summary(lm(energy ~ house + area, data = states.data))
#  Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)  4.257e+02  4.748e+01   8.966 9.59e-12 ***
#  house       -2.825e+00  8.135e-01  -3.473 0.001116 ** 
#  area         7.829e-04  1.965e-04   3.985 0.000233 ***

# Multiple R-squared:  0.5537,	Adjusted R-squared:  0.5347 

en.house.area.model <- lm(energy ~ house + area, data = states.data)

# plot the model 
par(mfrow = c(2, 2), mar = c(5, 5, 5, 5))
plot(en.house.area.model)

# Revisiting my first intuitions - particularly on why Alaska
# might be a leader in energy consumption - got me to think about
# the uniquely cold climate of Alaska and their role in energy production,
# namely crude oil, mining for natural resources and such. 

# Further investigation lead me to believe the reason certain states
# have higher energy consumption rates is because of the energy 
# production industry. 

# So! Let's take a look at energy as a function of waste, toxics, 
# greenhouse gas emission variables. 

# model 03 - Energy ~ Toxics + Green ------------------------------------------

summary(lm(energy ~ waste + toxic + green + metro, data = states.data))
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
#  (Intercept) 150.3571    49.6130   3.031  0.00412 ** 
#  waste        13.3368    41.9194   0.318  0.75191    
#  toxic         2.6957     0.4852   5.556 1.61e-06 ***
#  green         4.8176     0.5908   8.154 2.87e-10 ***
#  metro         0.1831     0.4718   0.388  0.69984   

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

cor(states.data$toxic, states.data$green)
toxic.greenhouse <- subset(states.data, select = c("toxic", "green"))
cor(na.omit(toxic.greenhouse))
#           toxic     green
# toxic 1.0000000 0.2622973
# green 0.2622973 1.0000000

energy.green <- subset(states.data, select = c("energy", "green"))
cor(na.omit(energy.green))
#           energy     green
# energy 1.0000000 0.7706181
# green  0.7706181 1.0000000

energy.toxic <- subset(states.data, select = c("energy", "toxic"))
cor(na.omit(energy.toxic))
#           energy     toxic
# energy 1.0000000 0.5624524
# toxic  0.5624524 1.0000000

par(mfrow = c(1, 1), mar = c(6, 6, 6, 6))
plot(toxic.greenhouse)
plot(states.data$energy, states.data$toxic)
plot(states.data$energy, states.data$green)

# So this model appears to be stronger than the original model using Metropolitan
# area percentage. But my intuition again says that cause and effect, while they 
# cannot be definitely inferred, are still too confused using these variables.
# I.E., greenhouse gas emissions could be a result of energy consumption.





