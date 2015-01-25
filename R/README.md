# svmfortopic

svmfortopic contains all code written by Andreas Keller Leth Laursen for the 10
ECTS points topic: "Comparing Support Vector Machines to Classical Time Series
Models: An Empirical Application on the Power Market." The code covers:

* Data scraping.
* Data treatment.
* Model estimation.
* Out of sample forecasting.
* Plots.
* Tex table outputs.

Install the package by running:
```
devtools::install_github("AKLLaursen/svmfortopic")
```

Note that currently a non-CRAN version of gridExtra is required to plot the ACF
and PACF.