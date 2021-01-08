# Analysis of Compass Airlines Flight Patterns

Nick Milikich /
December 2020

This project is an analysis of flight patterns for the fictional Compass Airlines. The airline is having trouble with delayed flights, particularly from Highland airport (HIX), and is looking to understand more about these delays and what is causing them, and to generate a model that will allow them to predict which flights will be delayed to take remediary measures for customer satisfaction ahead of time. Available is a summary of all flights departing from Highland airport in the month of December 2019.

The project is contained in CompassAirlinesAnalysis.R, and can be run by simply executing this one file. Also included in this repository is CompassAirlinesComments.pdf, which has a summary of the motivation, general insights, methods, and recommendations of this analysis.

Running this code requires that the R packages GGally, rpart, randomForest, and MASS be installed.

To run the source code, the following should be included in the same directory (included in this repository):
- `Flight_on_time_HIX.csv`, a summary of all flights leaving Highland airport in December 2019.
- `weather.csv`, a summary of the weather conditions at several airports by hour.
(Also included in this repository are data dictionaries for each of these raw data files.)

This project was run using R version 4.0.3.
