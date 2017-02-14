## Test environments

* local ubuntu 14.04 install: R 3.3.2
* ubuntu 12.04 (on travis-ci): R-release (R 3.3.2), R-devel (2017-02-13 r72167)
* OS X El Capitan 10.11.6 (on travis-ci): R-release (R 3.3.2)
* Windows Server 2012 R2 x64: R-release (R 3.3.2)

## R CMD check results

0 ERRORs | 0 WARNING | 4 NOTEs

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

