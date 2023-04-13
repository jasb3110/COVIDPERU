# GAM

## Contents
-   [Introduction](#introduction)
-   [Principal outcomes](#principal-outcomes)
-   [Discussion](##discussion)
-   [Conclusions](#conclusions)
-   [References](#references)
-   [R code](#r-code)

## Introduction

In the previous stages, COVID-19 data provided by the Peruvian government were analyzed to fit statistical and/or mathematical models in order to answer questions about the spread of COVID-19 in Peru. For example, the effectiveness of using vaccination to control COVID-19 and the relationships between COVID-19 deaths, positivity, free ICU beds, and vaccinated people were investigated.

To demonstrate or refute the previously proposed questions, we use Generalized Additive Models (GAMs). GAMs are models similar to Generalized Linear Models that consider the response variables adjusted to the family of exponential functions, but in the case of GAMs, they are associated with a smoothed link function of the predictor variable. This generates great flexibility for model selection but creates the problem of selecting the most optimal model (Wood & Augustin, 2002).

When working with multiple input variables, it is necessary to assign a form of reparameterization of the smoothed variables (Wood, 2017). This can be done in two ways: the first is the reparameterization necessary to absorb the identifiability constraints in the matrix of smoothed input variables, and the second is a reparameterization that helps understand the effective degrees of freedom of each smoothed variable (Wood, 2017).

One advantage of GAMs is that they can handle non-linear relationships between the response and predictor variables, as well as interactions between predictors, in a flexible and computationally efficient way. GAMs are particularly useful in fields like ecology, epidemiology, and finance, where complex non-linear relationships are often encountered. GAMs have been implemented in a variety of software packages, including R (with the mgcv package) and Python (with the pygam package).

##  Principal outcomes


### GAM on MINSA´s deaths


|[![Figure 1.](plotting/MINSA.4var.covid.gam.all.png)](https://github.com/jasb3110/COVIDPERU/blob/5377a555cd47975a35802003c1453d5f49c2335c/plotting/MINSA.4var.covid.gam.all.png?raw=true)|
|:------------------------------------------------------:|
|*Figure 1. MINSA's deaths  inferred for GAM: time, Molecular positivity, free ICU´s bed, and Vaccinated people*| 


|[![Figure 2.](?raw=true)](https://github.com/jasb3110/COVIDPERU/blob/5377a555cd47975a35802003c1453d5f49c2335c/plotting/MINSA.gam.model.compare.all.png?raw=true)|
|:------------------------------------------------------:|
|*Figure 2. MINSA's deaths  real vs MINSA's deaths  virtual inferred for GAM*| 



## Discussion

## Conclusions

## R code


## References

- Wood, S. N., & Augustin, N. H. (2002). GAMs with integrated model selection using penalized regression splines and applications to environmental modelling. Ecological Modelling, 157(2–3), 157–177. https://doi.org/10.1016/S0304-3800(02)00193-X
- Wood S (2017). Generalized Additive Models: An Introduction with R, 2 edition. Chapman and Hall/CRC.

