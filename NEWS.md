# pacotest 0.4.0
## Updates
* Change of default parameters! By default the CCC test is now being computed under consideration of estimation uncertainty of the probability integral transforms, i.e., with options `withEstUncert = TRUE` and `estUncertWithRanks = TRUE`. Before, up to version 0.3.1, both parameters defaulted to `FALSE`.
* Note that when calling `pacotest(U,W,'CCC')`, the default options for the CCC test are used (cf. `pacotestset`), but the two parameters `withEstUncert = FALSE` and `estUncertWithRanks = FALSE` are altered. In contrast when calling `pacotestOptions = pacotestset('CCC')`, the two parameters are set to `withEstUncert = TRUE` and `estUncertWithRanks = TRUE`. For the CCC test, under the default setting, it is assumed that estimated PPITs are provided and the test statistic is computed under consideration of estimation uncertainty of the probability integral transforms, i.e., `withEstUncert = TRUE` and `estUncertWithRanks = TRUE`. To apply `pacotest` with `withEstUncert = TRUE`, three additional inputs have to be provided (`data`, `svcmDataFrame` and `cPitData`).
* In the vine copula context, PPITs are usually estimated and not known. Therefore, in the vine copula context it is recommended to use the functions `pacotestRvineSeq` or `pacotestRvineSingleCopula` instead of `pacotest`. These functions automatically pass through the additional arguments `data`, `svcmDataFrame` and `cPitData` to the function `pacotest` and the CCC test can be applied in its default setting with consideration of estimation uncertainty of the probability integral transforms, i.e., `withEstUncert = TRUE` and `estUncertWithRanks = TRUE`.
* Continuous integration is now done with github actions (https://github.com/MalteKurz/pacotest/actions) instead of travis and appveyor.


## Minor improvements and bug fixes
* Fixed a couple of typos in the documentation.
* Updated the reference to Spanhel, F. and M. S. Kurz (2019), "Simplified vine copula models: Approximations based on the simplifying assumption", Electronic Journal of Statistics 13 (1), 1254-1291.

# pacotest 0.3.1
## Updates
* The default method for generating from a discrete uniform distribution changed (R version >=3.6.0). Regression test results have been adapted accordingly.

# pacotest 0.3
## Updates
* Renaming of `ECORR` test to `CCC` test to be in line with the corresponding paper (Kurz and Spanhel (2017) <https://arxiv.org/abs/1706.02338>)
* Added an additional, more informative, output, `testResultSummary`, to `pacotestRvineSeq()`
* Option, `stopIfRejected`, added to `pacotestRvineSeq()`, which allows the user to stop the sequential test procedure in case of a rejection
* Usage of Bonferroni correction in `pacotestRvineSeq()`
* Default value of `aggInfo` is now set to `meanAll` to be in line with the paper

## Minor improvements and bug fixes
* Bug fix in `extractSubTree`; Added a corresponding unit test
* Stabilization of numerical derivatives in edge cases for the copula parameters
* Don't use the aggregated information for computing the test statistic with the Gamma0 partition
* Fixed an edge case becoming relevant when almost all copulas in a vine copula are set to independence copulas

# pacotest 0.2.2
## Bug fixes
* [#24](https://github.com/MalteKurz/pacotest/issues/18) removed calls of floating-point function floor on integers (caused installation failures on solaris)
* [#23](https://github.com/MalteKurz/pacotest/issues/19) prevent nan's in unsigned int, which is outside the range of representable values (caused memtest note on CRAN)

# pacotest 0.2.1
## Bug fixes
* [#14](https://github.com/MalteKurz/pacotest/issues/14) removed calls of floating-point functions on integers (caused installation failures on solaris)
* [#15](https://github.com/MalteKurz/pacotest/issues/15) prevent nan's in unsigned int, which is outside the range of representable values (caused memtest note on CRAN)
* [#16](https://github.com/MalteKurz/pacotest/issues/16) added a side argument to calls of grad
* [#17](https://github.com/MalteKurz/pacotest/issues/17) completed the omega matrix for asmpt. with ranks
