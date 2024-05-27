# Prediction of Staffing and Efficiency of Municipalities in Kanton Zürich

We worked for a company (let's call it XYZ) that consults Swiss municipalities and offers consulting on staffing (among others). Municipalities frequently want to assess whether their staffing is sufficient to meet service requirements of the various municipal departments. XYZ covers this by comparing the staffing of the municipality per department with other municipalities. Other municipalities are selected such that they are similar in population size and organizational structure. XYZ then distributes surveys to collect staffing data from all involved municipalities. This generates a substantial amount of work each of these, but they have an incentive to contribute as they are offered the results of the comparison in the end. 

However, this willingness to collaborate in staffing surveys has decreased over the past few years and XYZ is looking for alternative sources of information on municipal staffing. There are many publicly available governmental data in Switzerland and in particular, the canton of Zürich publishes a comprehensive catalogue of characteristics and finances about each of its municipalities. 

## Research questions

The research question was thus whether these publicly available data could be leveraged to estimate municipal staffing on department level. If this was possible with sufficient precision, it would allow replacing the manual surveys with a statistical approach and save XYZ and municipalities time and money.

Furthermore, this publicly available data could be leveraged to assess efficiency of municipalities. Given similar characteristics, municipalities should have similar expenses on staffing. This constitutes our second research question and could help XYZ identify inefficient municipalities as new customers.

## Models on municipality level
At first, we built models on to predict staff costs and FTEs on municipality level. Find the corresponding data exploration, preparation, modeling and evaluation R-files in `/model_muni`. 

## Models on municipality and department level
In a second modeling attempt, we predict on municipality and department level. Find the corresponding data  preparation and modeling R-files in `/model_muni_dep`. 

## Visualization
Find visualizations like code for efficiency heatmaps and the R-Shiny dashboard in `visualization`. 
