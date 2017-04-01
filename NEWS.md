# pacotest 0.2.1
## Bug fixes
* [#14](https://github.com/MalteKurz/pacotest/issues/14) removed calls of floating-point functions on integers (caused installation failures on solaris)
* [#15](https://github.com/MalteKurz/pacotest/issues/15) prevent nan's in unsigned int, which is outside the range of representable values (caused memtest note on CRAN)
* [#16](https://github.com/MalteKurz/pacotest/issues/16) added a side argument to calls of grad
* [#17](https://github.com/MalteKurz/pacotest/issues/17) completed the omega matrix for asmpt. with ranks

# pacotest 0.2.2
## Bug fixes
* [#24](https://github.com/MalteKurz/pacotest/issues/18) removed calls of floating-point function floor on integers (caused installation failures on solaris)
* [#23](https://github.com/MalteKurz/pacotest/issues/19) prevent nan's in unsigned int, which is outside the range of representable values (caused memtest note on CRAN)
