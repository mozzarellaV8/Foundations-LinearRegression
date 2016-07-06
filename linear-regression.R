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

# Exercise Part 2 -------------------------------------------------------------
## Exercise: interactions and factors
## ────────────────────────────────────────

##   Use the states data set.

##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.

##   2. Try adding region to the model. Are there significant differences
##      across the four regions?





