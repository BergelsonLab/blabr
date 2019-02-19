# blabr

An R library for working in the BLAB. 


### install blabr

In R:

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
