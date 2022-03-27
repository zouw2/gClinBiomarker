
gClinBiomarker
==============

[![Build Status](https://img.shields.io/badge/build-passing-brightgreen.svg?style=flat-square)]() [![Code Coverage](https://img.shields.io/badge/coverage-0%25-red.svg?style=flat-square)]()

Overview
--------

gClinBiomarker is an R package that contains functions to perform baseline and longitutinal biomarker analyses. This version is wei's private version that fixes bugs and introduces features of critical needs.
 

### Getting the Documentation

Use the command `vignette(package = 'gClinBiomarker')` to view a list of avaialble vignettes for the `gClinBiomarker` package. Then use the command `vignette(<vignett_name>)` to view the relevant documentation.

gClinBiomarker Analysis and Workflows
-------------------------------------
 
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
