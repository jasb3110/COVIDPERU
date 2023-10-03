---
output:
  pdf_document: default
  html_document: default
---
# COVIDPERU
This project is trying of improving quality and order of COVID-19 of Peru. First, I managed to order and clean conflict and bug due to fail type and others issues.

## To clean and sort
This part, I show that how to do in R script. my assumption is beginning date is 4th March, 2020 and last update is 3rd April, 2023, [click here](https://github.com/jasb3110/COVIDPERU/blob/8bcb0eecec38dd7fd2cddc7d678ce8c93c8a7718/to%20clean%20and%20sort.md).

## Multivariate analyze
Here, I merged four groups of variables, namely COVID-19 infected people, COVID-19 deaths, available beds of intensive care unit (ICU), and COVID-19 vaccinated people. This data was used as input for Factorial multivariate analysis, which is a special kind of multivariate analysis, [click here](https://github.com/jasb3110/COVIDPERU/blob/d0ee33689df6331148b31f079913527f4aa75f55/multivariate.md).

## GAM model
Generalized additive model(GAM) is one best way to discover non-linear relationship between many variables. So that, I relate Covid mortality normalized how dependence variable and Time(days),COVID-19  positivy(%), available beds of ICU(%), COVID-19 vaccinated people(%) how independence variables, [click here](https://github.com/jasb3110/COVIDPERU/blob/68db1536a241104782918a7946672550af5740b1/GAM.md).

## Source
-   [OPENCOVID-PERÚ](https://www.tagacat.com/covid/links)
-   [Covid vaccination](https://www.datosabiertos.gob.pe/dataset/vacunaci%C3%B3n-contra-covid-19-ministerio-de-salud-minsa)
-   [Covid positivy](https://www.datosabiertos.gob.pe/dataset/casos-positivos-por-covid-19-ministerio-de-salud-minsa)
-   [Covid mortality](https://www.datosabiertos.gob.pe/dataset/fallecidos-por-covid-19-ministerio-de-salud-minsa)
-   [Covid Molecular test](https://www.datosabiertos.gob.pe/dataset/dataset-de-pruebas-moleculares-del-instituto-nacional-de-salud-para-covid-19-ins)
-   [Covid intensive care unit](https://www.datosabiertos.gob.pe/dataset/data-hist%C3%B3rica-del-registro-de-camas-diarias-disponibles-y-ocupadas-del-formato-f5002-v2)
-   [Total mortality](https://www.datosabiertos.gob.pe/dataset/informaci%C3%B3n-de-fallecidos-del-sistema-inform%C3%A1tico-nacional-de-defunciones-sinadef-ministerio)
-   [Available bed in ICU](https://www.dge.gob.pe/portalnuevo/informacion-publica/disponibilidad-de-camas-covid-19)
