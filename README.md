# gClinBiomarker

gClinBiomarker is an R package that contains functions to perform baseline and longitutinal biomarker analyses 

Documentations could be found at:
https://github.roche.com/lengn/gClinbiomarker_documents

The repository includes
- A user vignette demonstrates how gClinBiomarker package may help in your biomarker analysis
- An exapmle use case document which contains more detailed use cases
- A set of Rmarkdown templates that allow you to generate biomarker analysis report by "one click"

To install this package from R, use `install_github()` function from the **devtools** package

In R, type:

```r
## install.packages("devtools")
library(devtools)

install_github("RPackages/gClinBiomarker",
host="https://github.roche.com/api/v3")
```



Note that on bce (r.bas.roche.com), the default R is from an older version. To install, type

```r
install_github("RPackages/gClinBiomarker",
host="github.roche.com/api/v3")
```

## gClinBiomarker overview:

[embed]https://github.roche.com/lengn/gClinbiomarker_documents/blob/master/BiomarkersTools_overview.pdf[/embed]
