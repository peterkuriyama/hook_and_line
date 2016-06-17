
#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  // Data
  DATA_MATRIX(X_ij); // Covariates of year, brsec time and intercept
  DATA_VECTOR(num_boc); //number of bocaccio
  DATA_VECTOR(nhooks); //Used in dbinom
  
  // DATA_MATRIX(X_ij); //Covariates, currently year, site, angler
  
  // Parameters
  PARAMETER_VECTOR(b_j);
  
  // PARAMETER_VECTOR(b_j); //
  // PARAMETER_VECTOR(theta_z); //parameter vector with zero prob, and prob binomial

  // Objective funcction
  // Type zero_prob = 1 / (1 + exp(-theta_z(0))); //probability of zero
  // Type logprob = exp(theta_z(1)); //probability of bocaccio
  Type jnll = 0; //total likelihood
  
  int n_data = num_boc.size(); //Find number of data values

  // vector<Type> jnll_i(n_data); //likelihood
  
  //Linear predictor
  vector<Type> linpred_i(n_data); //predictions
  
  linpred_i = X_ij * b_j; //X is deign matrix, b is parameter vector  
  // for(int j = 0; j < n_data; j++){
  //   linpred_i(j) = X_ij(j) * b_j; //X is deign matrix, b is parameter vector  
  // }
  

  // for(int j = 0; j < n_data; j++){
  //   // linpred_i(j) = slope;  
  //   // linpred_i(j) = slope * onbsec(j);  
  //   linpred_i(j) = interc + slope * onbsec(j);  
  // }
  
  //inverse logit transformation
  vector<Type> probs_i(n_data); //probabilities for dbinom
  for(int j = 0; j < n_data; j++){
    probs_i(j) = exp(linpred_i(j)) / (1 + exp(linpred_i(j)));
    // linpred_i(j) = interc + slope * onbsec(j);  
  }
  
  //Probability of data conditional on fixed effect values
  for(int i =  0; i < n_data; i++){
    jnll -= dbinom(num_boc(i), nhooks(i), probs_i(i), TRUE);
  }

  // REPORT(interc);
  // REPORT(slope);
  REPORT(b_j)
  // REPORT(probs_i);
  REPORT(jnll);
  REPORT(linpred_i);

  return jnll;


}
