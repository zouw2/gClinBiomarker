# gClinBiomarker

gClinBiomarker is an R package that contains functions to perform baseline and longitutinal biomarker analyses 

Source code: https://github.roche.com/Rpackages/gClinBiomarker

Project page: https://pages.github.roche.com/Rpackages/gClinBiomarker/

Documentations could be found under the "article" tab of the project page. Two documents are provided:
- A user vignette demonstrates how gClinBiomarker package may help in your biomarker analysis
- An exapmle use case document which contains more detailed use cases

pdf version of these two documents can be found [here](https://github.roche.com/Rpackages/gClinBiomarker/tree/master/inst/doc).

gClinBiomarker also provide a set of Rmarkdown templates that allow you to generate biomarker analysis report by "one click". The R markdown templates can be found at [here](https://github.roche.com/lengn/gClinbiomarker_documents), along with some slide decks.

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

![screen shot 2017-09-28 at 1 44 01 pm](https://media.github.roche.com/user/48/files/431d398a-a453-11e7-8801-1c6915156185)
![screen shot 2017-09-28 at 1 44 13 pm](https://media.github.roche.com/user/48/files/47cad168-a453-11e7-85f7-deee2f7604ab)
![screen shot 2017-09-28 at 1 44 24 pm](https://media.github.roche.com/user/48/files/4ad58466-a453-11e7-80be-9af0c23fcedd)
![screen shot 2017-09-28 at 1 44 34 pm](https://media.github.roche.com/user/48/files/507dd1a2-a453-11e7-8216-442f15bd500a)
![screen shot 2017-09-28 at 1 44 43 pm](https://media.github.roche.com/user/48/files/53ab85e0-a453-11e7-825d-718f28d1fca3)
