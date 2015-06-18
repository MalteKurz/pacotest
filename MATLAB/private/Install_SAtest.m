if ispc
    setenv('MINGWROOT','C:\\MinGW\\64')
    mex -setup:mex_C++_mingw-w64.xml
    mex EC.cpp ../../src/Grouping.cpp ../../src/EqualCop.cpp ../../src/EqualRankCorr.cpp -I../../src -DARMA_BLAS_LONG
    mex VI.cpp ../../src/VecIndepTest.cpp -I../../src -DARMA_BLAS_LONG
    mex ERC.cpp ../../src/Grouping.cpp ../../src/EqualRankCorr.cpp ../../src/EqualCop.cpp -I../../src -DARMA_BLAS_LONG
elseif isunix
    mex EC.cpp ../../src/Grouping.cpp ../../src/EqualCop.cpp ../../src/EqualRankCorr.cpp -I../../src -DARMA_BLAS_LONG CXXFLAGS="\$CXXFLAGS -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp" -larmadillo -lmwlapack -lmwblas
    mex VI.cpp ../../src/VecIndepTest.cpp -I../../src -DARMA_BLAS_LONG -larmadillo -lmwlapack -lmwblas
    mex ERC.cpp ../../src/Grouping.cpp ../../src/EqualRankCorr.cpp ../../src/EqualCop.cpp -I../../src -DARMA_BLAS_LONG -larmadillo -lmwlapack -lmwblas
end
