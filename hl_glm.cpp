
#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  //Toggle delta model
  DATA_IVECTOR(Options_z); //0 = non-delta, 1 = delta

  // Data
  DATA_MATRIX(X_ij); // Covariates, design matrix
  DATA_VECTOR(num_boc); //number of bocaccio
  DATA_VECTOR(num_hooks); //Used in dbinom
  
  // Parameters
  PARAMETER_VECTOR(b_j);
  PARAMETER_VECTOR(theta_z);

  // Objective funcction
  // Type logprob = exp(theta_z(1)); //probability of bocaccio
  Type zero_prob = 1 / (1 + exp(-theta_z(0)));

  Type jnll = 0; //total likelihood

  int n_data = num_boc.size(); //Find number of data values

  //Linear predictor
  vector<Type> linpred_i(n_data); //predictions
  linpred_i = X_ij * b_j; //X is deign matrix, b is parameter vector  
    
  //inverse logit transformation
  vector<Type> probs_i(n_data); //probabilities for dbinom
  for(int j = 0; j < n_data; j++){
    probs_i(j) = exp(linpred_i(j)) / (1 + exp(linpred_i(j)));
  }
  
  //Probability of data conditional on fixed effect values
  for(int i =  0; i < n_data; i++){
    if(num_boc(i) == 0 & Options_z(0) == 1) jnll -= log(zero_prob);
    if(num_boc(i) != 0 & Options_z(0) == 1) jnll -= log(1 - zero_prob) + dbinom(num_boc(i), num_hooks(i), probs_i(i), TRUE);
    if(Options_z(0) == 0) jnll -= dbinom(num_boc(i), num_hooks(i) , probs_i(i), TRUE);
    
  }

  REPORT(b_j);
  REPORT(theta_z);
  REPORT(jnll);
  REPORT(linpred_i);

  return jnll;

}
