# Changes in 0.1.0
  + FIT file reading is now handled by the `fitdc` package rather than compiled code from the FIT SDK, which threw up some licensing issues.
  + SRM file reading is now handled by pure `R` code, which removes the former `python` dependency and the `readr` suggest.
  + The `clean_bikedata` structure now includes a lap column.

# Changes in 0.0.4
  + Corrected some pesky memory-access bugs in the source code.

# Changes in 0.0.3
  + Code is now compatible with the latest `dplyr` release (>= v0.5.0). Most notably, it is now expected that `contains` is an exported function from `dplyr`.
  + `magrittr` is now an import. This was an implicit dependency anyway, as it is imported by `dplyr`. The pipe operator is now imported from `magrittr` rather than (convolutely) from `dplyr`.
  + This file now uses markdown rather than just plain text.

# Changes in 0.0.2
  + **BUG FIX**: A source file from the FIT SDK assumed a C++11-compatible compiler. Hence, C++11 is now listed in the package description file as a system requirement.
  + Cleaned bike data now has a `"start_time"` attribute *where possible*. This is facilitated by the fact that all importing functions now consistently try to append a `"timestamp.posix"` column.
  + Post-import processing functions (`clean_bikedata` and `uniform_sampling`) now better preserve the attributes of their inputs.

# Changes in 0.0.1
	+ First official version; released on CRAN.
