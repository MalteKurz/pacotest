if ispc
    setenv('MINGWROOT','C:\\MinGW\\64')
    mex -setup:mex_C++_mingw-w64.xml
    mex -I./ -I../../src EC.cpp pacotest_Matlab_HelpingFunctions.cpp../../src/Grouping.cpp ../../src/EqualCop.cpp ../../src/EqualRankCorr.cpp -DARMA_BLAS_LONG
    mex -I./ -I../../src VI.cpp pacotest_Matlab_HelpingFunctions.cpp../../src/VecIndepTest.cpp -DARMA_BLAS_LONG
    mex -I./ -I../../src ERC.cpp pacotest_Matlab_HelpingFunctions.cpp ../../src/Grouping.cpp ../../src/EqualRankCorr.cpp ../../src/EqualCop.cpp -DARMA_BLAS_LONG
elseif isunix
    mex  -I./ -I../../src EC.cpp pacotest_Matlab_HelpingFunctions.cpp ../../src/Grouping.cpp ../../src/EqualCop.cpp ../../src/EqualRankCorr.cpp -DARMA_BLAS_LONG CXXFLAGS="\$CXXFLAGS -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp" -larmadillo -lmwlapack -lmwblas
    mex  -I./ -I../../src VI.cpp pacotest_Matlab_HelpingFunctions.cpp ../../src/VecIndepTest.cpp -DARMA_BLAS_LONG -larmadillo -lmwlapack -lmwblas
    mex  -I./ -I../../src ERC.cpp pacotest_Matlab_HelpingFunctions.cpp ../../src/Grouping.cpp ../../src/EqualRankCorr.cpp ../../src/EqualCop.cpp -DARMA_BLAS_LONG -larmadillo -lmwlapack -lmwblas
end
