#include <pacotest_header.h>

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
Rcpp::List ERC(arma::mat Udata, arma::mat Wdata, double Grouping, double AggPvalsNumbRep=0, double ExpMinSampleSize = 50, double TrainingDataFraction = 0.5) {
    try
    {
        
        int grouping = (int) Grouping;
        int aggPvalsNumbRep = (int) AggPvalsNumbRep;
        
        
        // Associate outputs
        double pValue;
        Rcpp::List out;
        unsigned int i=0;
        
        if (AggPvalsNumbRep == 0)
        {
            double TestStat;
            arma::mat Xdata(1,2);
            arma::mat Ydata(1,2);
            arma::umat SplitVariable(1,4);
            arma::umat SplitQuantile(1,4);
            arma::mat SplitThreshold(1,3);
            SplitVariable.zeros();
            SplitQuantile.zeros();
            SplitThreshold.zeros();
            
            EqualRankCorrTest(Udata, Wdata, Grouping, &TestStat, &pValue, Xdata, Ydata, ExpMinSampleSize, TrainingDataFraction, SplitVariable, SplitQuantile, SplitThreshold);
            
            out = Rcpp::List::create(Rcpp::Named("pValue")=pValue,Rcpp::Named("TestStat")=TestStat,Rcpp::Named("Xdata")=Xdata,Rcpp::Named("Ydata")=Ydata,Rcpp::Named("SplitVariable")=SplitVariable,Rcpp::Named("SplitQuantile")=SplitQuantile,Rcpp::Named("SplitThreshold")=SplitThreshold);
        }
        else
        {
            arma::mat pValues(AggPvalsNumbRep,1);
            arma::mat Xdata(1,2);
            arma::mat Ydata(1,2);
            arma::umat SplitVariable(AggPvalsNumbRep,4);
            arma::umat SplitQuantile(AggPvalsNumbRep,4);
            arma::mat SplitThreshold(AggPvalsNumbRep,3);
            SplitVariable.zeros();
            SplitQuantile.zeros();
            SplitThreshold.zeros();

            EqualRankCorrTest(Udata, Wdata, pValues, &pValue, AggPvalsNumbRep, ExpMinSampleSize, TrainingDataFraction, SplitVariable, SplitQuantile, SplitThreshold);
            
            out = Rcpp::List::create(Rcpp::Named("pValue")=pValue,Rcpp::Named("pValues")=pValues,Rcpp::Named("SplitVariable")=SplitVariable,Rcpp::Named("SplitQuantile")=SplitQuantile,Rcpp::Named("SplitThreshold")=SplitThreshold);
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
Rcpp::List EC(arma::mat Udata, arma::mat Wdata, double NumbBoot, double Grouping, double ExpMinSampleSize = 50, double TrainingDataFraction = 0.5) {
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
        arma::umat SplitVariable(1,4);
        arma::umat SplitQuantile(1,4);
        arma::mat SplitThreshold(1,3);
        SplitVariable.zeros();
        SplitQuantile.zeros();
        SplitThreshold.zeros();
        
        Rcpp::List out;
        
        EqualCopTest(Udata, Wdata, N, grouping, &TestStat, &pValue, S, Xdata, Ydata, ExpMinSampleSize, TrainingDataFraction, SplitVariable, SplitQuantile, SplitThreshold);
        
        out = Rcpp::List::create(Rcpp::Named("pValue")=pValue,Rcpp::Named("TestStat")=TestStat,Rcpp::Named("S")=S,Rcpp::Named("Xdata")=Xdata,Rcpp::Named("Ydata")=Ydata,Rcpp::Named("SplitVariable")=SplitVariable,Rcpp::Named("SplitQuantile")=SplitQuantile,Rcpp::Named("SplitThreshold")=SplitThreshold);
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
Rcpp::List ERC_noRanks(arma::mat Udata, arma::mat Wdata, double Grouping, double AggPvalsNumbRep=0, double ExpMinSampleSize = 50, double TrainingDataFraction = 0.5) {
    try
    {
        
        int grouping = (int) Grouping;
        int aggPvalsNumbRep = (int) AggPvalsNumbRep;
        
        
        // Associate outputs
        double pValue;
        Rcpp::List out;
        unsigned int i=0;
        
        if (AggPvalsNumbRep == 0)
        {
            double TestStat;
            arma::mat Xdata(1,2);
            arma::mat Ydata(1,2);
            arma::umat SplitVariable(1,4);
            arma::umat SplitQuantile(1,4);
            arma::mat SplitThreshold(1,3);
            SplitVariable.zeros();
            SplitQuantile.zeros();
            SplitThreshold.zeros();
            
            EqualRankCorrTest_noRanks(Udata, Wdata, Grouping, &TestStat, &pValue, Xdata, Ydata, ExpMinSampleSize, TrainingDataFraction, SplitVariable, SplitQuantile, SplitThreshold);
            
            out = Rcpp::List::create(Rcpp::Named("pValue")=pValue,Rcpp::Named("TestStat")=TestStat,Rcpp::Named("Xdata")=Xdata,Rcpp::Named("Ydata")=Ydata,Rcpp::Named("SplitVariable")=SplitVariable,Rcpp::Named("SplitQuantile")=SplitQuantile,Rcpp::Named("SplitThreshold")=SplitThreshold);
        }
        else
        {
            arma::mat pValues(AggPvalsNumbRep,1);
            arma::mat Xdata(1,2);
            arma::mat Ydata(1,2);
            arma::umat SplitVariable(AggPvalsNumbRep,4);
            arma::umat SplitQuantile(AggPvalsNumbRep,4);
            arma::mat SplitThreshold(AggPvalsNumbRep,3);
            SplitVariable.zeros();
            SplitQuantile.zeros();
            SplitThreshold.zeros();

            EqualRankCorrTest_noRanks(Udata, Wdata, pValues, &pValue, AggPvalsNumbRep, ExpMinSampleSize, TrainingDataFraction, SplitVariable, SplitQuantile, SplitThreshold);
            
            out = Rcpp::List::create(Rcpp::Named("pValue")=pValue,Rcpp::Named("pValues")=pValues,Rcpp::Named("SplitVariable")=SplitVariable,Rcpp::Named("SplitQuantile")=SplitQuantile,Rcpp::Named("SplitThreshold")=SplitThreshold);
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
Rcpp::List ERC_oracle(arma::mat Udata, arma::mat Wdata, double Grouping, double AggPvalsNumbRep=0, double ExpMinSampleSize = 50, double TrainingDataFraction = 0.5) {
    try
    {
        
        int grouping = (int) Grouping;
        int aggPvalsNumbRep = (int) AggPvalsNumbRep;
        
        
        // Associate outputs
        double pValue;
        Rcpp::List out;
        unsigned int i=0;
        
        if (AggPvalsNumbRep == 0)
        {
            double TestStat;
            arma::mat Xdata(1,2);
            arma::mat Ydata(1,2);
            arma::umat SplitVariable(1,4);
            arma::umat SplitQuantile(1,4);
            arma::mat SplitThreshold(1,3);
            SplitVariable.zeros();
            SplitQuantile.zeros();
            SplitThreshold.zeros();
            
            EqualRankCorrTest_oracle(Udata, Wdata, Grouping, &TestStat, &pValue, Xdata, Ydata, ExpMinSampleSize, TrainingDataFraction, SplitVariable, SplitQuantile, SplitThreshold);
            
            out = Rcpp::List::create(Rcpp::Named("pValue")=pValue,Rcpp::Named("TestStat")=TestStat,Rcpp::Named("Xdata")=Xdata,Rcpp::Named("Ydata")=Ydata,Rcpp::Named("SplitVariable")=SplitVariable,Rcpp::Named("SplitQuantile")=SplitQuantile,Rcpp::Named("SplitThreshold")=SplitThreshold);
        }
        else
        {
            arma::mat pValues(AggPvalsNumbRep,1);
            arma::mat Xdata(1,2);
            arma::mat Ydata(1,2);
            arma::umat SplitVariable(AggPvalsNumbRep,4);
            arma::umat SplitQuantile(AggPvalsNumbRep,4);
            arma::mat SplitThreshold(AggPvalsNumbRep,3);
            SplitVariable.zeros();
            SplitQuantile.zeros();
            SplitThreshold.zeros();

            EqualRankCorrTest_oracle(Udata, Wdata, pValues, &pValue, AggPvalsNumbRep, ExpMinSampleSize, TrainingDataFraction, SplitVariable, SplitQuantile, SplitThreshold);
            
            out = Rcpp::List::create(Rcpp::Named("pValue")=pValue,Rcpp::Named("pValues")=pValues,Rcpp::Named("SplitVariable")=SplitVariable,Rcpp::Named("SplitQuantile")=SplitQuantile,Rcpp::Named("SplitThreshold")=SplitThreshold);
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

