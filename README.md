# El patron (***WIP***)
<!-- 
![version badge](http://www.r-pkg.org/badges/version/elpatron)
![download badge](http://cranlogs.r-pkg.org/badges/grand-total/elpatron?color=8808ff)
-->
![Build Status](https://travis-ci.org/jmackie4/elpatron.svg?branch=master)

**El patrón**. *Spanish*: *the boss*.

This package aims to facilitate cycling data analysis with `R`. Efficient file reading and support for common (but computationally expensive) calculations are provided. As a companion to this package, I'll try to document various different use cases over on the `gh-pages` branch (*coming soon*). Any contributions to this pseudo-blog are welcomed.

## *IMPORTANT:* CRAN Note

This package was removed from CRAN due to licensing issues with the [Dynastream's FIT SDK](https://www.thisisant.com/resources/fit). It is still usable, albeit with some hiccups in the compiled code. Once I get round to cooking up a pure `R` parser for FIT files it will be back up on CRAN. In the meantime, install from this repo via `devtools` (below).

## Installation

```
devtools::install_github("jmackie4/elpatron")
```

## Acknowledgements

This software is based on pugixml library (http://pugixml.org). pugixml is Copyright (C) 2006-2015 Arseny Kapoulkine. The pugixml license can be found [here](https://github.com/jmackie4/elpatron/blob/master/inst/licenses/pugixml_license.txt).

## TODO

+ Cook up an `R` implementation for parsing FIT files, using [python-fitparse](https://github.com/dtcooper/python-fitparse/tree/ng) as a reference.
+ Convert the `python` module for SRM file parsing to `R`.
