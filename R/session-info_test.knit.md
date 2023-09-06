---
title: "Untitled"
output: github_document
---


```r
library(sessioninfo)
info <- session_info(pkgs = 'installed',
                            info = c('platform', 'python', 'external'),
                            to_file = FALSE)
```

```
## Warning in grDevices::grSoftVersion(): unable to load shared object '/Library/Frameworks/R.framework/Resources/modules//R_X11.so':
##   dlopen(/Library/Frameworks/R.framework/Resources/modules//R_X11.so, 6): Library not loaded: /opt/X11/lib/libSM.6.dylib
##   Referenced from: /Library/Frameworks/R.framework/Versions/4.1/Resources/modules/R_X11.so
##   Reason: image not found
```

```
## Warning in cairoVersion(): unable to load shared object '/Library/Frameworks/R.framework/Resources/library/grDevices/libs//cairo.so':
##   dlopen(/Library/Frameworks/R.framework/Resources/library/grDevices/libs//cairo.so, 6): Library not loaded: /opt/X11/lib/libXrender.1.dylib
##   Referenced from: /Library/Frameworks/R.framework/Versions/4.1/Resources/library/grDevices/libs/cairo.so
##   Reason: image not found
```

```r
print(info)
```

```
## ─ Session info ───────────────────────────────────────────────────────────────
##  setting  value
##  version  R version 4.1.1 (2021-08-10)
##  os       macOS Big Sur 10.16
##  system   x86_64, darwin17.0
##  ui       X11
##  language (EN)
##  collate  en_US.UTF-8
##  ctype    en_US.UTF-8
##  tz       America/New_York
##  date     2023-04-03
##  pandoc   2.19.2 @ /Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/ (via rmarkdown)
## 
## ─ External software ──────────────────────────────────────────────────────────
##  setting        value
##  cairo
##  cairoFT
##  pango
##  png
##  jpeg
##  tiff
##  tcl
##  curl           7.64.1
##  zlib           1.2.11
##  bzlib          1.0.6, 6-Sept-2010
##  xz             5.2.4
##  PCRE           10.34 2019-11-21
##  ICU            66.1
##  TRE            TRE 0.8.0 R_fixes (BSD)
##  iconv          GNU libiconv 1.11
##  readline       5.2
##  BLAS           /opt/R/4.1.1/Resources/lib/libRblas.0.dylib
##  lapack         /opt/R/4.1.1/Resources/lib/libRlapack.dylib
##  lapack_version 3.9.0
## 
## ─ Python configuration ───────────────────────────────────────────────────────
##  Python is not available
## 
## ──────────────────────────────────────────────────────────────────────────────
```



