#include "SAtest_header.h"
#include <RcppArmadillo.h>

// [[Rcpp::export]]
Rcpp::List VI(arma::mat Udata, arma::mat Wdata, double NumbBoot) {
    try
    {
        
        int N = (int) NumbBoot;
        
        
        // Associate outputs
        double TestStat;
        double pValue;
        arma::mat S(N,1);
        Rcpp::List out;
        
        VecIndepTest(Udata, Wdata, N, &TestStat, &pValue, S);
        
        out = Rcpp::List::create(Rcpp::Named("pValue")=pValue,Rcpp::Named("TestStat")=TestStat,Rcpp::Named("S")=S);
        return out;
    }
    catch( std::exception& __ex__ ) {
        forward_exception_to_r( __ex__ );
    }
    catch(...) {
        ::Rf_error( "c++ exception" );
    }
}

// [[Rcpp::export]]
Rcpp::List ERC(arma::mat Udata, arma::mat Wdata, double Grouping, double AggPvalsNumbRep=0) {
    try
    {
        
        int grouping = (int) Grouping;
        int aggPvalsNumbRep = (int) AggPvalsNumbRep;
        
        // Associate outputs
        double pValue;
        Rcpp::List out;
        
        if (AggPvalsNumbRep == 0)
        {
            double TestStat;
            arma::mat Xdata(1,2);
            arma::mat Ydata(1,2);
            EqualRankCorrTest(Udata, Wdata, Grouping, &TestStat, &pValue, Xdata, Ydata);
            
            out = Rcpp::List::create(Rcpp::Named("pValue")=pValue,Rcpp::Named("TestStat")=TestStat,Rcpp::Named("Xdata")=Xdata,Rcpp::Named("Ydata")=Ydata);
        }
        else
        {
            arma::mat pValues(AggPvalsNumbRep,1);
            arma::mat Xdata(1,2);
            arma::mat Ydata(1,2);
            EqualRankCorrTest(Udata, Wdata, pValues, &pValue, AggPvalsNumbRep);
            
            out = Rcpp::List::create(Rcpp::Named("pValue")=pValue,Rcpp::Named("pValues")=pValues);
        }
        
        return out;
    }
    catch( std::exception& __ex__ ) {
        forward_exception_to_r( __ex__ );
    }
    catch(...) {
        ::Rf_error( "c++ exception" );
    }
}

// [[Rcpp::export]]
Rcpp::List EC(arma::mat Udata, arma::mat Wdata, double NumbBoot, double Grouping) {
    try
    {
        
        int N = (int) NumbBoot;
        int grouping = (int) Grouping;
        
        
        // Associate outputs
        double TestStat;
        double pValue;
        arma::mat S(N,1);
        arma::mat Xdata(1,2);
        arma::mat Ydata(1,2);
        Rcpp::List out;
        
        EqualCopTest(Udata, Wdata, N, grouping, &TestStat, &pValue, S, Xdata, Ydata);
        
        out = Rcpp::List::create(Rcpp::Named("pValue")=pValue,Rcpp::Named("TestStat")=TestStat,Rcpp::Named("S")=S,Rcpp::Named("Xdata")=Xdata,Rcpp::Named("Ydata")=Ydata);
        return out;
    }
    catch( std::exception& __ex__ ) {
        forward_exception_to_r( __ex__ );
    }
    catch(...) {
        ::Rf_error( "c++ exception" );
    }
}

