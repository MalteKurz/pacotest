## Submission comments
This submission fixes unit tests in accordance to the new default method for generating from a discrete uniform distribution (R version >=3.6.0).

## Test environments

* local ubuntu 14.04 install: R 3.4.4
* ubuntu 14.04 (on travis-ci): R-release (R 3.5.2), R-devel (2018-03-09)
* macOS Sierra 10.13.3 (on travis-ci): R-release (R 3.5.2)
* Windows Server 2012 R2 x64 (on appveyor): R-release (R 3.5.2)

## R CMD check results

0 ERRORs | 0 WARNING | 1 NOTE

```
checking installed package size ... NOTE
  installed size is  6.9Mb
  sub-directories of 1Mb or more:
    libs   6.7Mb
```
-> This is all compiled code in the libs/ directory.

## Reverse dependencies:

The only reverse dependency `pencopulaCond` was checked successfully.

