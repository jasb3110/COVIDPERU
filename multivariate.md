# Multivariate analyze

## Contents
-   [Introduction](#introduction)
-   [Principal outcomes](#principal-outcomes)
-   [Discussion](##discussion)
-   [Conclusions](#conclusions)
-   [R code](#r-code)

## Introduction
After cleaning and sorting the COVID-19 data, it is possible to merge and analyze it to reveal patterns and trends in the spread of COVID-19. the next step is to perform a multivariate analysis, which will help to generate hypotheses about the spread of COVID-19 in the future.

## Principal outcomes
I merged 20 variables, including date, molecular positivity, number of samples for molecular and antigen tests, serological positivity, number of samples for serological tests, excess of SINADEF's deaths, MINSA's deaths, DIRESA+DIRIS's deaths, DIRESA+DIRIS's COVID-19 patients, non-free beds in ICUs, and vaccinated people. These variables were then clustered into four groups based on their affinity: infected people, deaths, available ICU beds (%), and vaccinated people.
                 
### Factorial Principal components (FPCs)
|[![Figure .1](plotting/AMV.biplot.png)](https://github.com/jasb3110/COVIDPERU/blob/1ee55684f2eadadde76548a45d09be429252cf5f/plotting/AMV.biplot.png?raw=true)|
|:----------------------------------------------------------------------------:|
|*Figure 1.Analysis factorial of COVID-19:infected people, deaths, free bed(%) and vaccinated people *|

DIM1 (∼64%) is interpreted as signal of COVID-19 infected & mortality effect on people in ICU. DIM1 has the highest loading for free ICU beds %, COVID-19 deaths, and COVID-19 infected people. DIM2 (∼22%) is interpreted as a signal of COVID-19 mortality effect decreasing for vaccination. DIM2 has the highest loading for free vaccinated people, and COVID-19. deaths.

