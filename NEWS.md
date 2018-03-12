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
