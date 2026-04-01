// Two-level (1 hierarchical grouping) `random' intercept model
// Partial pooling on intercepts 
// Updated for new version of Stan (2025!)

data{
int<lower=0> N; 	// number of total observations
int<lower=0> Nspp; 	// number of species (grouping factor)
array[N] int species; 	// species identity, coded as int
int<lower=0> Ntreeid;  // number of tree ids (grouping factor)
array[N] int treeid;   // tree id identity, coded as int
vector[N] eos; 	// eos (predictor for slope)
array[N] real y; 		// ring width (response)
}

parameters{
real a;		// mean intercept across everything
real<lower=0> sigma_atreeid;
real<lower=0> sigma_y; 	// measurement error, noise etc. 	
vector[Ntreeid] atreeid; // variation of intercept across tree ids, no-centered
vector[Nspp] aspp;
vector[Nspp] bspp;
}

transformed parameters{
// vector[Ntreeid] atreeid;
// atreeid = 0 + sigma_atreeid*zatreeid; // non-centered parameterization on atreeid
array[N] real ypred;
for (i in 1:N){ // don't change this for reparameterization
    ypred[i]=
        a + 
        aspp[species[i]] + 
        atreeid[treeid[i]] + 
        bspp[species[i]]*eos[i];

    }
}

model{	
  a ~ normal(2, 3);
  aspp ~ normal(0, 4);
  bspp ~ normal(0, 0.7);
  atreeid ~ normal(0, sigma_atreeid);
  sigma_atreeid ~ normal(0, 1);
  sigma_y ~ normal(0, 1);
  
  y ~ normal(ypred, sigma_y);
}	

generated quantities {
  array[N] real y_rep;
  for (i in 1:N) {
    y_rep[i] = normal_rng(
        a + 
        aspp[species[i]] + 
        atreeid[treeid[i]] + 
        bspp[species[i]]*eos[i], sigma_y);
  }
    // prior predictive samples
  real a_prior = normal_rng(2, 3);
  real aspp_prior = normal_rng(0, 4);
  real bsp_prior = normal_rng(0, 0.7);
  real sigma_atreeid_prior = abs(normal_rng(0, 1));  
  real atreeid_prior = normal_rng(0, sigma_atreeid_prior);
  real sigma_y_prior = abs(normal_rng(0, 1));    
}
