# COVIDPERU

This project is trying of improving quality and order of COVID-19 of Peru. First, I managed to order and clean conflict and bug due to fail type and others issues.

## To clean and sort

This part, I show that how to do in R script. my assumption is beginning date is 4th March, 2020 and last update is 3rd April, 2023 [click here](https://github.com/jasb3110/COVIDPERU/blob/7d45ce76cc625757856c1cd3a60a7890105a2756/to%20clean%20and%20sort.md).

## Multivariate analyze

Here, I merged four groups of variables: Covid infected people, Covid death, available beds oF intensive care unit (ICU) and Covid vaccination, [click here](https://github.com/jasb3110/COVIDPERU/blob/98773553ffb736d9fecc15573338e64dd07b5629/multivariate.md).

## GAM model

Generalized additive model(GAM) is one best way to discover non-linear relationship between many variables. So that, I relate Covid mortality normalized how dependence variable and Time(days),Covid positivy(%), available beds of ICU(%), Covid vaccination how independence variables, [click here](https://github.com/jasb3110/COVIDPERU/blob/9ef00bc56553571b0910769353e16eba32d375ef/GAM.md).

## Source
-   [Covid vaccination](https://www.datosabiertos.gob.pe/dataset/vacunaci%C3%B3n-contra-covid-19-ministerio-de-salud-minsa)
-   [Covid positivy](https://www.datosabiertos.gob.pe/dataset/casos-positivos-por-covid-19-ministerio-de-salud-minsa)
-   [Covid mortality](https://www.datosabiertos.gob.pe/dataset/fallecidos-por-covid-19-ministerio-de-salud-minsa)
-   [Covid Molecular test](https://www.datosabiertos.gob.pe/dataset/dataset-de-pruebas-moleculares-del-instituto-nacional-de-salud-para-covid-19-ins)
-   [Covid intensive care unit](https://www.datosabiertos.gob.pe/dataset/data-hist%C3%B3rica-del-registro-de-camas-diarias-disponibles-y-ocupadas-del-formato-f5002-v2)
-   [Total mortality](https://www.datosabiertos.gob.pe/dataset/informaci%C3%B3n-de-fallecidos-del-sistema-inform%C3%A1tico-nacional-de-defunciones-sinadef-ministerio)
-   [Available bed in ICU](https://www.dge.gob.pe/portalnuevo/informacion-publica/disponibilidad-de-camas-covid-19)
