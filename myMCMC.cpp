#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
double logverosimilitud(NumericVector theta, NumericVector X, NumericVector Y){
  double alfa = theta[0];
  double beta = theta[1];
  double sigma = theta[2];
  int n = X.length();
  
  NumericVector yfun(n);
  for(int i=0; i < n; i++){
    yfun[i] = alfa + beta*X[i];
  }
  
  NumericVector verosimilitud(n);
  double primera;
  double segunda;
  
  for (int i=0; i < n; i++){
    primera = exp(-pow(Y[i]-yfun[i], 2.0)*0.5/(sigma*sigma));
    segunda = sqrt(2*M_PI)*sigma;
    verosimilitud[i] = log(primera/segunda);
  }
  
  double suma = sum(verosimilitud);
  return (suma);
}

// [[Rcpp::export]]
double logApriori(NumericVector theta){
  double alfa = theta[0];
  double beta = theta[1];
  double sigma = theta[2];
  
  double a_ = R::dnorm(alfa, 1000, 200, true);
  double b_ = R::dnorm(beta, 1000, 50, true);
  double sigma_ = R::dgamma(sigma, 0.01, 50,true);
  double aux = a_+b_+sigma_;
  return(aux);
}

// [[Rcpp::export]]
double aPosteriori(NumericVector theta, NumericVector X, NumericVector Y){
  double aux1 = logverosimilitud(theta, X, Y);
  double aux2 = logApriori(theta);
  aux2 = aux2 + aux1;
  return(aux2);
}

// [[Rcpp::export]]
NumericVector proposal(NumericVector theta){
  double alfa = theta[0];
  double beta = theta[1];
  double sigma = theta[2];
  
  double p1 = R::rnorm(alfa,  100);
  double p2 = R::rnorm(beta,  100);
  double p3 = R::rnorm(sigma, 100);
  
  NumericVector aux(3);
  aux[0] = p1;
  aux[1] = p2;
  aux[2] = p3;
  return (aux);
}

// [[Rcpp::export]]
NumericMatrix myMCMC(NumericVector x, NumericVector y, NumericVector startValue, int iterations){
  NumericMatrix chain(iterations+1, 3);
  for(int i=0; i < startValue.length(); i++){
    chain(0,i) = startValue[i];
  }
  
  NumericVector prop(3);
  NumericVector aux(3);
  double probab;
  for(int i=0; i < iterations; i++){
    for(int j=0; j < 3; j++){            //auxiliar, vector parametros
      aux[j] = chain(i, j);
    }
    prop = proposal(aux);
    probab = exp(aPosteriori(prop, x, y) - aPosteriori(aux, x, y));
    if(R::runif(0,1) < probab){
      for(int j=0; j < startValue.length(); j++){
        chain(i+1, j) = prop[j];
      }
    }else{
      for(int j=0; j < startValue.length(); j++){
        chain(i+1, j) = chain(i,j);
      }
    }
  }
  return(chain);
}