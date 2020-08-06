# blabr

An R library for working in the BLAB. 


### install blabr

First, install the subprocess package. This package has been archived in CRAN, so we will remove it in the near future. Until then, use the following commands:

```R
packageurl <- "https://cran.r-project.org/src/contrib/Archive/subprocess/subprocess_0.8.3.tar.gz"
install.packages(packageurl, repos=NULL, type="source")
```

Then, to install blabr itself:

```R
# install.packages("devtools")
devtools::install_github('BergelsonLab/blabr')
```


### setup environment

You need to have a copy of the **BLAB_DATA** directory in your home folder. Clone the following BLAB_DATA repository (https://github.com/BergelsonLab/BLAB_DATA) to get an updated version of the data used in this package.




### test if it worked

Open up RStudio, and try this out:

```r
library(blabr)

all_bl <- get_all_basiclevel()
```
