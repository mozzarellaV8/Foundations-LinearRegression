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

#### load the data

				states.data <- readRDS("data/states.rds")
				
				states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])

`states.data` contains the qualitative information on each US State across 21 variables. There is one categorical variable - `region` - which groups each state into one of four regions across the country (e.g. 'North East, Midwest'). 

`states.info` is the other dataset provided - this contains attribute details for each of the 21 variables in `states.data`. 

#### preliminary correlation test and plotting

We'll be taking a look at the relationship between Energy Consumption by State, and the percentage of the population living in Metropolitan Areas. A naive intuition might suggest there's a correlation between the two - the more people there are living in cities, the more power will be used or consumed. 

Before diving directly into a linear model, I thought it'd be a good idea to look at some correlation tests and general plots of the variables of interest. As a student I still find the `lm()` function quite powerful and wherever possible would like to get a sense of the data before running code. 

`metro.energy <- subset(states.data, select = c("metro", "energy"))`
`summary(metro.energy)`




