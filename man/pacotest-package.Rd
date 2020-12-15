\name{pacotest-package}
\alias{pacotest-package}
\docType{package}
\title{
Testing for Partial Copulas and the Simplifying Assumption in Vine Copulas
}
\description{
The \pkg{pacotest} package provides functions, which allow to test for partial copulas and the simplifying assumption in vine copulas.
The package consists of two different test types, the Constant Conditional Correlation (CCC) test and the Vectorial Independence (VI) test.
The function \code{\link{pacotestset}} can be used to create and alter pacotest options lists and the function \code{\link{pacotest}} can be used to test for the partial copula and the simplifying assumption for a single bivariate conditional copula.

The function \code{\link{pacotestRvineSeq}} can be used with a \link[VineCopula]{RVineMatrix} from the \link[VineCopula]{VineCopula-package} to test all pair-copulas being building blocks in a R-vine copula to be (j-1)-th order partial copulas, which is equivalent to testing the simplifying assumption.
A single building blog of a R-vine copula could be tested to be a (j-1)-th order partial copula by applying the function \code{\link{pacotestRvineSingleCopula}} to a \link[VineCopula]{RVineMatrix} from the \link[VineCopula]{VineCopula-package}.
}
\author{
Malte S. Kurz
}

\references{
Hobaek-Haff, I., K. Aas and A. Frigessi (2010), "On the simplified pair-copula construction -- Simply useful or too simplistic?", Journal of Multivariate Analysis 101(5), pp. 1296-1310.

Kojadinovic, I. and M. Holmes (2009), "Tests of independence among continuous random vectors based on Cramer-von Mises functionals of the empirical copula process", Journal of Multivariate Analysis 100(6), pp. 1137-1154.

Kurz, M. S. and F. Spanhel (2017), "Testing the simplifying assumption in high-dimensional vine copulas", ArXiv e-prints \url{https://arxiv.org/abs/1706.02338}.

Quessy, J.-F. (2010), "Applications and asymptotic power of marginal-free tests of stochastic vectorial independence", Journal of Statistical Planning and Inference 140(11), pp. 3058-3075.

Spanhel, F. and M. S. Kurz (2019), "Simplified vine copula models: Approximations based on the simplifying assumption", Electronic Journal of Statistics 13 (1), 1254-1291.

Spanhel, F. and M. S. Kurz (2016), "The partial copula: Properties and associated dependence measures", Statistics & Probability Letters 119, pp. 76-83.
}

\seealso{
Development for \pkg{pacotest} can be followed via the GitHub repository at \url{https://github.com/MalteKurz/pacotest}.
}


