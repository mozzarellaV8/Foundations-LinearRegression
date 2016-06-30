# Linear Regression - Exercise

_Student work in R_

## Instructions

## Exercise: least squares regression

Use the /states.rds/ data set. Fit a model predicting energy consumed per capita (energy) from the percentage of residents living in metropolitan areas (metro). Be sure to

1. Examine/plot the data before fitting the model
2. Print and interpret the model `summary'
3. `plot' the model to look for deviations from modeling assumptions

Select one or more additional predictors to add to your model and repeat steps 1-3. Is this model significantly better than the model with /metro/ as the only predictor?

Here is a glimpse of the data:

![sampledata01](plots/sampledata01.png)

And information about the variables:

![attributes](plots/sampledata02.png)

## Process and Analysis

#### Loading State Data and Info

``` r
states.data <- readRDS("data/states.rds")
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])
```

`states.data` contains the qualitative information on each US State across 21 variables. 

There is one categorical variable - `region` - which groups each state into one of four regions across the country (e.g. 'North East, Midwest'). 

`states.info` contains attribute details for each of the 21 variables in `states.data` - an easy-to-read dataframe explaining the variables. 

#### Preliminary: Correlation Test and Plotting

We'll be taking a look at the relationship between Energy Consumption by State, and the percentage of the population living in Metropolitan Areas. A naive intuition might suggest there's a correlation between the two - the more people there are living in cities, the more power will be used or consumed. 

Before diving directly into a linear model, I thought it'd be a good idea to look at some correlation tests and general plots of the variables of interest. As a student I still find the `lm()` function quite powerful and wherever possible would like to get a sense of the data before running code. 


``` r
metro.energy <- subset(states.data, select = c("metro", "energy"))
summary(metro.energy)
```

``` r
cor(metro.energy)
```

	##        metro energy
	## metro      1     NA
	## energy    NA      1

It looks like the NA values throw off the correlation test. Even though I'd be throwing out data, I'd like to see some value just to get a sense of the data. How many NA's are there? Significant amount?

``` r
is.na(metro.energy)
```

Looks like there is only row of NAs - the District of Columbia. Given the relatively small population of D.C. and it's status as a Federal District rather than state - I feel OK to throw out this data if just to get an approximate sense of correlation between Metropolitan Population and Energy Consumption.

``` r
cor(na.omit(metro.energy))
```

	#             metro     energy
	# metro   1.0000000 -0.3397445
	# energy -0.3397445  1.0000000

Seeing a weak downhill linear relationship - cor value at -0.339. Now for a quick plot to see where the data points lie before fitting a model. 

``` r
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
```

![metro, energy plot from states.data](plots/02-energy.model-EDA.jpg)





