## Resubmission
This is a resubmission. The following adaptations are in place

* Shortened the LICENSE file to the necessary part.
* Extended Description entry in the DESCRIPTION file.
* Revised the examples for the functions `pacotestRvineSeq` and `pacotestRvineSingleCopula` to shorten the run-time: Replaced the computational intensive call to the routine `VineCopula::RVineStructureSelect` by directly specifying a regular vine copula model for the data example.

## Test environments

* local ubuntu 14.04 install: R 3.3.2
* ubuntu 12.04 (on travis-ci): R-release (R 3.3.2), R-devel (2017-02-15)
* OS X El Capitan 10.11.6 (on travis-ci): R-release (R 3.3.2)
* Windows Server 2012 R2 x64: R-release (R 3.3.2)

## R CMD check results

0 ERRORs | 0 WARNING | 2 NOTEs

```
checking CRAN incoming feasibility ... NOTE
License components with restrictions and base license permitting such:
MIT + file LICENSE
```
-> The package is MIT licensed.

```
checking installed package size ... NOTE
installed size is  6.7Mb
sub-directories of 1Mb or more:
libs   6.5Mb
```
-> Due to compiled code in the libs sub-directory.

## R CMD check results (Additional third note with R-devel)

0 ERRORs | 0 WARNING | 3 NOTEs

```
checking compiled code ... NOTE

File ‘pacotest/libs/pacotest.so’:

  Found no calls to: ‘R_registerRoutines’, ‘R_useDynamicSymbols’

It is good practice to register native routines and to disable symbol

search.

See ‘Writing portable packages’ in the ‘Writing R Extensions’ manual.
```
-> Known Rcpp issue (https://github.com/RcppCore/Rcpp/issues/636)

