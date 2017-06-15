# blabr

An R library for working in the BLAB.


### install blabr

In R:

```R
# install.packages("devtools")
devtools::install_github('BergelsonLab/blabr')
```


### setup environment

You need to have a copy of the **BLAB_DATA** directory in your home folder. You can get a copy from Opus (Seedlings/Compiled_Data/BLAB_DATA).


### test if it worked

Open up RStudio, and try this out:

```r
library(blabr)

all_bl <- get_all_basiclevel()
```
