## Submission comments
This submission includes a number of updates to align the package with the corresponding paper (Kurz and Spanhel (2017) <arXiv:1706.02338>) and several minor improvements and bug fixes.

## Test environments

* local ubuntu 14.04 install: R 3.4.3
* ubuntu 14.04 (on travis-ci): R-release (R 3.4.2), R-devel (2018-03-12)
* macOS Sierra 10.12.6 (on travis-ci): R-release (R 3.4.3)
* Windows Server 2012 R2 x64 (on appveyor): R-release (R 3.4.4)

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

