## Submission comments
This is a patch to fix installation problems on CRAN solaris and memtest notes. The following adaptations have been done:

* Removed calls of floating-point functions on integers (caused installation failures on solaris). Requested ASAP by Prof. Brian Ripley on 12-Mar-2017.
* Prevent nan's in unsigned int, which is outside the range of representable values (caused memtest note on CRAN).

## Test environments

* local ubuntu 14.04 install: R 3.3.3
* local ubuntu 14.04 install: R 3.3.3 with clang compiler
* ubuntu 12.04 (on travis-ci): R-release (R 3.3.2), R-devel (2017-03-20)
* OS X El Capitan 10.11.6 (on travis-ci): R-release (R 3.3.3)

## R CMD check results

0 ERRORs | 0 WARNING | 2 NOTEs

```
checking CRAN incoming feasibility ... NOTE
License components with restrictions and base license permitting such:
MIT + file LICENSE
File 'LICENSE':
  YEAR: 2017
  COPYRIGHT HOLDER: Malte S. Kurz
```
-> The package is MIT licensed.

```
checking installed package size ... NOTE
installed size is  6.7Mb
sub-directories of 1Mb or more:
libs   6.5Mb
```
-> Due to compiled code in the libs sub-directory.

