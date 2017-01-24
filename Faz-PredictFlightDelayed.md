---
title: "Predicting Flight Delays in United States"
author: "Bruce Zhang, Jaime Andaluz, Frazil Mustapa, Isha Singh  "
output:
  html_document:
    css: ../../INSEADAnalytics/AnalyticsStyles/default.css
    theme: paper
    toc: yes
    toc_float:
      collapsed: no
      smooth_scroll: yes
  pdf_document:
    includes:
      in_header: ../../INSEADAnalytics/AnalyticsStyles/default.sty
always_allow_html: yes
---




```r
# Please ENTER the name of the file with the data used. The file should be a
# .csv with one row per observation (e.g. person) and one column per
# attribute. Do not add .csv at the end, make sure the data are numeric.
datafile_name = "Jan_AA.csv"

# Please enter the minimum number below which you would like not to print -
# this makes the readability of the tables easier. Default values are either
# 10e6 (to print everything) or 0.5. Try both to see the difference.
MIN_VALUE = 0.5

# Please enter the maximum number of observations to show in the report and
# slides.  DEFAULT is 10. If the number is large the report may be slow.
max_data_report = 10
```


```r
ProjectData <- read.csv(datafile_name)
ProjectData <- data.matrix(ProjectData) 
ProjectData_INITIAL <- ProjectData

factor_attributes_used= c(4:23)

factor_attributes_used = unique(sapply(factor_attributes_used,function(i) min(ncol(ProjectData), max(i,1))))
ProjectDataFactor=ProjectData[,factor_attributes_used]
ProjectDataFactor <- ProjectData <- data.matrix(ProjectDataFactor)
```


## Introduction

This project is done by a group of INSEAD MBA(17J) students who are enrolled in the Big Data and Analytics for Business elective conducted by Prof Theos Evgeniou in 2017 Jan. We use several statistical analysis models learned in the class to analyze a new dataset about US deomestic flight delay in 2008, with the aim to find out the main causes for flight delay in general and provide a statistical guideline to minimize flight delay in US. The work is written in R programming language. 


## Project Description

Delays are not an unusual occurrence in the airline industry, and generates additional cost to the airline companies and also increases unsatisfaction to the passengers. However, we recognize that flight delay is not caused by a single factor but multiple ones that spans from previous flight delay, to ground operation, and passenger security check. Based on the data, we would like to analyse what are the major cause for flight delays and assign a probability on whether a particular flight will be delayed. 

## Analytic Process 

###1. Understand the business problem
asdf

###2. Data Consolidation 

Original Data


```r
rownames(ProjectDataFactor) <- paste0("observation ", sprintf("%02i", 1:nrow(ProjectDataFactor)))
knitr::kable(round(ProjectDataFactor, 2)[1:min(max_data_report,nrow(ProjectDataFactor)), ])
```



|               | DepTime| CRSDepTime| ArrTime| CRSArrTime| ActualElapsedTime| CRSElapsedTime| AirTime| ArrDelay| DepDelay| Origin| Dest| Distance| TaxiIn| TaxiOut| Diverted| CarrierDelay| WeatherDelay| NASDelay| SecurityDelay| LateAircraftDelay|
|:--------------|-------:|----------:|-------:|----------:|-----------------:|--------------:|-------:|--------:|--------:|------:|----:|--------:|------:|-------:|--------:|------------:|------------:|--------:|-------------:|-----------------:|
|observation 01 |    1909|       1900|    2256|       2230|               407|            390|     333|       26|        9|     33|   36|     2475|     33|      41|        0|            9|            0|       17|             0|                 0|
|observation 02 |     847|        810|    1252|       1155|               365|            345|     339|       57|       37|     36|   25|     2556|      7|      19|        0|           37|            0|       20|             0|                 0|
|observation 03 |    1741|       1705|    2000|       1935|               139|            150|     106|       25|       36|     52|   16|      802|      6|      27|        0|            0|           25|        0|             0|                 0|
|observation 04 |     942|        905|    1504|       1425|               562|            560|     538|       39|       37|     52|   25|     4243|      4|      20|        0|            0|           37|        2|             0|                 0|
|observation 05 |    1358|       1300|    1613|       1520|               135|            140|     111|       53|       58|     16|   52|      802|      8|      16|        0|           53|            0|        0|             0|                 0|
|observation 06 |    1231|        920|    2100|       1740|               329|            320|     289|      200|      191|     36|   20|     2454|      7|      33|        0|          191|            0|        9|             0|                 0|
|observation 07 |    2217|       1845|     106|       2200|               349|            375|     323|      186|      212|     20|   36|     2454|      6|      20|        0|           15|            0|        0|             0|               171|
|observation 08 |    1757|       1630|    2109|       1955|               372|            385|     349|       74|       87|      7|   36|     2611|      6|      17|        0|            2|            0|        0|             0|                72|
|observation 09 |    2331|       2325|     812|        750|               341|            325|     319|       22|        6|     67|    7|     2704|      6|      16|        0|            5|            0|       17|             0|                 0|
|observation 10 |    1758|       1715|    2119|       2030|               141|            135|     116|       49|       43|     52|    7|      867|      4|      21|        0|            0|           25|        6|             0|                18|

Data Scaling


```r
ProjectDatafactor_scaled = apply(ProjectDataFactor, 2, function(r) {
    if (sd(r) != 0) 
        res = (r - mean(r))/sd(r) else res = 0 * r
    res
})
```


```r
knitr::kable(round(my_summary(ProjectDatafactor_scaled), 2))
```



|                  |   min| 25 percent| median| mean| 75 percent|   max| std|
|:-----------------|-----:|----------:|------:|----:|----------:|-----:|---:|
|DepTime           | -3.44|      -0.70|   0.12|    0|       0.79|  1.85|   1|
|CRSDepTime        | -3.54|      -0.70|   0.09|    0|       0.80|  2.08|   1|
|ArrTime           | -2.58|      -0.44|   0.29|    0|       0.77|  1.24|   1|
|CRSArrTime        | -3.53|      -0.57|   0.13|    0|       0.74|  1.41|   1|
|ActualElapsedTime | -1.95|      -0.67|  -0.11|    0|       0.51|  5.54|   1|
|CRSElapsedTime    | -1.88|      -0.60|  -0.10|    0|       0.47|  5.52|   1|
|AirTime           | -1.88|      -0.66|  -0.12|    0|       0.50|  5.68|   1|
|ArrDelay          | -0.81|      -0.62|  -0.30|    0|       0.27| 23.30|   1|
|DepDelay          | -0.91|      -0.60|  -0.29|    0|       0.28| 21.16|   1|
|Origin            | -1.79|      -1.04|   0.01|    0|       0.76|  2.10|   1|
|Dest              | -1.81|      -1.09|   0.02|    0|       0.65|  1.94|   1|
|Distance          | -1.77|      -0.63|  -0.10|    0|       0.52|  5.56|   1|
|TaxiIn            | -1.00|      -0.45|  -0.32|    0|       0.23| 24.60|   1|
|TaxiOut           | -1.34|      -0.57|  -0.29|    0|       0.12| 14.19|   1|
|Diverted          |  0.00|       0.00|   0.00|    0|       0.00|  0.00|   0|
|CarrierDelay      | -0.42|      -0.42|  -0.37|    0|       0.06| 26.07|   1|
|WeatherDelay      | -0.21|      -0.21|  -0.21|    0|      -0.21| 52.89|   1|
|NASDelay          | -0.48|      -0.48|  -0.35|    0|       0.01| 34.56|   1|
|SecurityDelay     | -0.05|      -0.05|  -0.05|    0|      -0.05| 40.72|   1|
|LateAircraftDelay | -0.66|      -0.66|  -0.38|    0|       0.22| 10.68|   1|

###3. Factor Analysis 
asdf

###4. Intepretation
asdf
###5. Conclusion 
asdf


```r
summary(cars)
```

```
##      speed           dist       
##  Min.   : 4.0   Min.   :  2.00  
##  1st Qu.:12.0   1st Qu.: 26.00  
##  Median :15.0   Median : 36.00  
##  Mean   :15.4   Mean   : 42.98  
##  3rd Qu.:19.0   3rd Qu.: 56.00  
##  Max.   :25.0   Max.   :120.00
```

## Including Plots

You can also embed plots, for example:



Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
