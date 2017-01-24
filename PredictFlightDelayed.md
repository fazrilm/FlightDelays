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

factor_attributes_used= c(1:7)

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
The data file is ...


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

```
## Error in inherits(x, "list"): could not find function "my_summary"
```

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
