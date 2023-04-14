# GAM

## Contents
-   [Introduction](#introduction)
-   [Principal outcomes](#principal-outcomes)
-   [Conclusions](#conclusions)
-   [References](#references)
-   [R code](#r-code)

## Introduction

In the previous stages, COVID-19 data provided by the Peruvian government were analyzed to fit statistical and/or mathematical models in order to answer questions about the spread of COVID-19 in Peru. For example, the effectiveness of using vaccination to control COVID-19 and the relationships between COVID-19 deaths, positivity, free ICU beds, and vaccinated people were investigated.

To demonstrate or refute the previously proposed questions, we use Generalized Additive Models (GAMs). GAMs are models similar to Generalized Linear Models that consider the response variables adjusted to the family of exponential functions, but in the case of GAMs, they are associated with a smoothed link function of the predictor variable. This generates great flexibility for model selection but creates the problem of selecting the most optimal model (Wood & Augustin, 2002).

When working with multiple input variables, it is necessary to assign a form of reparameterization of the smoothed variables (Wood, 2017). This can be done in two ways: the first is the reparameterization necessary to absorb the identifiability constraints in the matrix of smoothed input variables, and the second is a reparameterization that helps understand the effective degrees of freedom of each smoothed variable (Wood, 2017).

One advantage of GAMs is that they can handle non-linear relationships between the response and predictor variables, as well as interactions between predictors, in a flexible and computationally efficient way. GAMs are particularly useful in fields like ecology, epidemiology, and finance, where complex non-linear relationships are often encountered. GAMs have been implemented in a variety of software packages, including R (with the mgcv package) and Python (with the pygam package).

##  Principal outcomes

It is possible to use SINADEF's deaths data; however, there is a gap in the data on October 12th, 2022. Therefore, I only used MINSA's deaths data as the COVID-19 mortality index.

### GAM on MINSA´s deaths
|[![Figure 1.](plotting/MINSA.4var.covid.gam.all.png)](https://github.com/jasb3110/COVIDPERU/blob/5377a555cd47975a35802003c1453d5f49c2335c/plotting/MINSA.4var.covid.gam.all.png?raw=true)|
|:------------------------------------------------------:|
|*Figure 1. MINSA's deaths  inferred for GAM: time, Molecular positivity, free ICU´s bed, and Vaccinated people*| 

In Fig.1, an autoregressive GAM model is presented, which demonstrates the relationship between SINADEF's time series data, including Molecular positivity, free ICU's bed, and Vaccinated people as input data, to infer MINSA's deaths. The model suggests that as time and COVID-19 positivity increase, MINSA's deaths also increase. However, the negative relationship between free ICU beds and vaccinated people with MINSA's deaths suggests that these factors may play a role in mitigating COVID-19 mortality. 

|[![Figure 2.](plotting/MINSA.gam.model.compare.all.png)](https://github.com/jasb3110/COVIDPERU/blob/5377a555cd47975a35802003c1453d5f49c2335c/plotting/MINSA.gam.model.compare.all.png?raw=true)|
|:------------------------------------------------------:|
|*Figure 2. MINSA's deaths  real versus MINSA's deaths  virtual inferred for GAM*| 

In Fig. 2, I attempted to reconstruct the COVID-19 mortality rate up to the present using an autoregressive GAM. This graph allows us to assess the extent to which the GAM model fits with MINSA's reported deaths.

## Conclusions

It has been possible to successfully fit MINSA's deaths in an autoregressive GAM model. It suggests that the increase in the number of free ICU beds and the number of vaccinated people has a positive impact on the health of Peruvians and can help reduce COVID-19 mortality rates. This information could be valuable for the government in promoting vaccination campaigns and improving the healthcare infrastructure.

## References

- Wood, S. N., & Augustin, N. H. (2002). GAMs with integrated model selection using penalized regression splines and applications to environmental modelling. Ecological Modelling, 157(2–3), 157–177. https://doi.org/10.1016/S0304-3800(02)00193-X
- Wood S (2017). Generalized Additive Models: An Introduction with R, 2 edition. Chapman and Hall/CRC.
