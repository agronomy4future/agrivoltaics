<!-- README.md is generated from README.Rmd. Please edit that file -->
# agrivoltaics
<!-- badges: start -->
<!-- badges: end -->

The goal of the agrivoltaics package is to streamline mixed-effects analysis for agrivoltaics experiments.

□ Code explained: https://agronomy4future.com/archives/24556

## Installation

You can install agrivoltaics() like so:

Before installing, please download Rtools (https://cran.r-project.org/bin/windows/Rtools)

``` r
if(!require(remotes)) install.packages("remotes")
if (!requireNamespace("agrivoltaics", quietly = TRUE)) {
  remotes::install_github("agronomy4future/agrivoltaics", force= TRUE)
}
library(remotes)
library(agrivoltaics)
```

## Example

This is a basic code for agrivoltaics()

``` r
model= agrivoltaics(
  output= independent variable (default),
  treatment= treatment between AV and control (default),
  genotype= genotype (optional),
  plot= experimental plot (optional),
  block= experimental block or replicate (default),
  row= rows within experimental plot (optional),
  season= different seasons (optional),
  location= different location (optional),
  data= dataset
)
```

## Let’s practice with actual dataset

``` r
# data upload 
if(!require(readr)) install.packages("readr")
library(readr)
github="https://raw.githubusercontent.com/agronomy4future/raw_data_practice/refs/heads/main/agrivoltaics.csv"
df= data.frame(read_csv(url(github), show_col_types=FALSE))
df$Plot= as.factor(df$Plot)
df$Yield= as.numeric (df$Yield)
set.seed(100)
print(df[sample(nrow(df),5),])
         Season Location AV_Site Genotype Plot Block  Row  Yield
202 2015 season     East      AV      cv1  101     I East 195.38
112 2016 season  MidWest Control      cv1  115    IV East 625.07
206 2015 season     East      AV      cv2  102    II East 135.86
4   2015 season  MidWest Control      cv1  109     I West 384.36
311 2016 season     East      AV      cv1  107   III East 125.11
.
.
.

model= agrivoltaics(
  output= Yield,
  treatment= AV_Site,
  genotype= NULL,
  plot= NULL,
  block= Block,
  row= NULL,
  season= NULL,
  location= NULL,
  data= df
)

 MODEL FORMULA USED:
Yield ~ AV_Site + (1 | AV_Site:Block) 

 VARIANCE COMPONENTS:
 Groups        Name        Variance
 AV_Site:Block (Intercept)  113.57 
 Residual                  8086.79 

 VARIANCE COMPONENT BREAKDOWN (%):
Random [AV_Site:Block]: 1.38%
Residual: 98.62%

 TYPE III ANOVA:
Type III Analysis of Variance Table with Satterthwaite's method
         Sum Sq Mean Sq NumDF DenDF F value    Pr(>F)    
AV_Site 5311986 5311986     1 5.972  656.87 2.455e-07 ***

###
post_hoc= cld (emmeans(model, ~ AV_Site), adjust= "sidak", Letters=letters, reverse= TRUE)

print(post_hoc)
 AV_Site emmean   SE   df lower.CL upper.CL .group
 Control    475 8.90 6.02      448      501  a    
 AV         152 8.88 5.98      126      179   b   
