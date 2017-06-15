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
This library expect git to be in different locations depending on your operating system. These are its expectations:

- windows: /bin/git
- mac: /usr/local/bin/git

You can check the location of your installation of git with this command (in the terminal):

```
which git
```

If the path is different than the one listed above, you can just copy/paste it from the current location to where the blabr library expects it to be.

### test if it worked

Open up RStudio, and try this out:

```r
library(blabr)

all_bl <- get_all_basiclevel()
```
