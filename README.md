
gClinBiomarker
==============

[![Build Status](https://img.shields.io/badge/build-passing-brightgreen.svg?style=flat-square)]() [![Code Coverage](https://img.shields.io/badge/coverage-0%25-red.svg?style=flat-square)]()

Overview
--------

gClinBiomarker is an R package that contains functions to perform baseline and longitutinal biomarker analyses

Source code: <https://github.roche.com/Rpackages/gClinBiomarker>

Project page: <https://pages.github.roche.com/Rpackages/gClinBiomarker/>

Documentations could be found under the "article" tab of the project page. Two documents are provided: - A user vignette demonstrates how gClinBiomarker package may help in your biomarker analysis - An exapmle use case document which contains more detailed use cases

pdf version of these two documents can be found [here](https://github.roche.com/Rpackages/gClinBiomarker/tree/master/inst/doc).

![untitled drawing 2](https://media.github.roche.com/user/48/files/92962930-4e04-11e8-9064-94754ebf0ab5)


gClinBiomarker also provide a set of Rmarkdown templates that allow you to generate biomarker analysis report by "one click". The R markdown templates can be found at [here](https://github.roche.com/lengn/gClinbiomarker_documents/tree/master/Markdown_templates), along with some slide decks.

Getting Started
---------------

### Installation

To install this package from R, use `install_github()` function from the **devtools** package

In R, type:

``` r
## install.packages("devtools")
library(devtools)
install_github("RPackages/gClinBiomarker", host="https://github.roche.com/api/v3")
```

Note that on bce (r.bas.roche.com), the default R is from an older version. To install, type

``` r
install_github("RPackages/gClinBiomarker", host="github.roche.com/api/v3")
```

### Getting the Documentation

Use the command `vignette(package = 'gClinBiomarker')` to view a list of avaialble vignettes for the `gClinBiomarker` package. Then use the command `vignette(<vignett_name>)` to view the relevant documentation.

gClinBiomarker Analysis and Workflows
-------------------------------------

### Supported endpoint and biomarker types

#### Endpoints

-   Time-to-event
-   Binary
-   Continuous
-   Longitudinal

#### Biomarkers

-   Continuous
-   Ordinal
-   Categorical
-   Longitudinal

#### Analysis

-   2-arm study: predictive and prognostic biomarker analysis
-   1-arm study: prognostic biomarker analysis

### Workflow

#### Step 1:

Is your biomarker population your full patient population? Take a look at these functions:

`SummaryVars()`, `CompareKM()`, `PlotRspBar()`

#### Step 2:

Is your biomarker response a dynamic range? Does it have a skewed distribution? Is it correlated with clinical variables?

`PlotProperty()`, `PlotTabForestMulti()`

#### Step 3:

Is your biomarker response associated with a clinical outcome? Is it prognostic or predictive? Is there an optimal biomarker cutoff with a consistent trend?

`PlotTabForestBiomarker()`, `PlotSTEPP()`

#### Step 4:

Estimate the clinical benefit within a biomarker subgroup.

`PlotKM()`, `PlotLong()`, `PlotRspBar()`, `CoxTab()`, `LogRankTab()`
