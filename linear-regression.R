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