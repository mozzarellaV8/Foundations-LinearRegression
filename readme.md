Linear Regression - an exercise
===============================

_Student work in R_

This is an exercise in fitting a linear regression model to data, as part of Springboard's [Foundations of Data Science](https://www.springboard.com/workshops/data-science/learn#1090-data-analysis-in-depth) class. The exercise is adapted from Harvard University's [statistical software workshop](http://tutorials.iq.harvard.edu/R/Rstatistics/Rstatistics.html).

- [Process and Analysis](process-analysis.md)
- [presentation code](linear-regression-V1.R) - edited from working code
- [working code](linear-regression.R) - includes all trials and tests
- [Data](data)

_Least Squares? Linear Regression?_

Linear Regression is a statistical method for fitting a line to points of data. This line can be thought of as a predictor for future data points - given significant data that's been properly formatted, thoughtfully analyzed, and taken into consideration with domain knowledge or real-world situations. 

Many lines can be approximately fit to multiple data points; the goal for a more 'accurate' predictor is to find the _line of best fit_. 'Best fit' in this sense refers to reduction of error - distance between the line fit and the data points. A common metric for accuracy is to take the square of this distance. 

## Instructions: least squares regression

Using data gathered on US States:

Fit a model predicting energy consumed per capita (`energy`) from the percentage of residents living in metropolitan areas (`metro`).

1. Examine/plot the data before fitting the model
2. Print and interpret the model `summary'
3. `plot' the model to look for deviations from modeling assumptions

After fitting this model, choose one more additional predictors from the data to add to your model and repeat the above steps. 

Is the second model significantly better than the model with `metro` as the only predictor?


## the Data

Here is a glimpse of the data:

![sampledata01](plots/sampledata01.png)

And information about the variables:

![attributes](plots/sampledata02.png)

Per capita energy consumption and % of population living in metropolitan areas are two of 21 total variables that can be used to fit a linear regression model. 

## Process and Analysis

- [Process and Analysis](process-analysis.md) is a more detailed look at my thought process and analysis for this exercise. After some preliminary work on a model predicting energy consumption from metropolitan population percentages - I'm working with the theory that the states that consume the most energy also produce the most energy. Generally this energy production is in oil, coal, and industry. From here, I'm taking a look into what becomes of this energy - is it used domestically or internationally? If domestically, can users be traced to those living in metropolitan areas? 

## Historical Tangent: Adriene-Marie Legendre

![Adriene-Marie Legendre](plots/legendre.jpg)

Adriene-Marie Legendre was a French mathematician credited as the inventor of the Least Squares method. In the only surviving picture of him, he appears to be less than content but with an active head of hair. Not pictured in the rest of this caricature drawing is a portrait of a contented, seemingly well-fed Joseph Fourier. The unfinished caricature suggests they were in the same room. Surrounding each portrait are pencilled-in mathematical equations, which appear to grow in complexity with their proximity to Fourier. 

![Legendre and Fourier](plots/legendre_fourier.jpg)





