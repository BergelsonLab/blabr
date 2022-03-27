# blabr

An R library for working in the BLAB. 


### Install

Then, to install blabr itself:

```R
# install.packages("remotes")
remotes::install_github('BergelsonLab/blabr')
```


### Setup environment

You need to have a copy of the **BLAB_DATA** directory in your home folder. Clone the individual dataset repositories into that folder
(see instruction on GitBook [here](https://bergelsonlab.gitbook.io/blab/programming-info/blab_data))




### Test if it worked

Open up RStudio, and try this out:

```r
library(blabr)

all_bl <- get_all_basiclevel()
```
