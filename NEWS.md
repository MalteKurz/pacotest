# pacotest 0.2.1
## Bug fixes
* https://github.com/MalteKurz/pacotest/issues/14 removed calls of floating-point functions on integers (caused installation failures on solaris)
* https://github.com/MalteKurz/pacotest/issues/15 prevent nan's in unsigned int, which is outside the range of representable values (caused memtest note on CRAN)
* https://github.com/MalteKurz/pacotest/issues/16 added a side argument to calls of grad
* https://github.com/MalteKurz/pacotest/issues/17 completed the omega matrix for asmpt. with ranks
