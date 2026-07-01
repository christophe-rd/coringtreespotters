// Two-level (1 hierarchical grouping) `random' intercept model
// Partial pooling on intercepts 
// Updated for new version of Stan (2025!)

data{
int<lower=0> N;     // number of total observations
int<lower=0> Nspp;     // number of species (grouping factor)
array[N] int species;     // species identity, coded as int
int<lower=0> Ntreeid;  // number of tree ids (grouping factor)
array[N] int treeid;   // tree id identity, coded as int
int<lower=0> Nyear;
array[N] int year; 
array[Ntreeid] int treeid_species; // species index for each treeid
array[Nspp] int Ntreeid_per_spp;
int<lower=0> Ngslseq;
vector[Ngslseq] gslseq;
real gslscale; // scale
vector[N] gsl;     // gsl (predictor for slope)
array[N] real y;         // ring width (response)
}

parameters{
real a;        // mean intercept across everything
real<lower=0> sigma_atreeid;
real<lower=0> sigma_y;     // measurement error, noise etc.      
vector[Ntreeid] zatreeid; // variation of intercept across tree ids, non-centered
vector[Nspp] aspp;
vector[Nyear] ayear;
vector[Nspp] bspp;
}

transformed parameters{
vector[Ntreeid] atreeid;
atreeid = 0 + sigma_atreeid*zatreeid; // non-centered parameterization on atreeid

array[N] real ypred;
for (i in 1:N){ // don't change this for reparameterization
    ypred[i]=
        a + 
        aspp[species[i]] + 
        atreeid[treeid[i]] + 
        ayear[year[i]] +
        bspp[species[i]]*gsl[i];

    }
}

model{    
  a ~ normal(1, 3);
  aspp ~ normal(0, 6);
  ayear ~ normal(0, 1);
  
  bspp ~ normal(0, 0.7);
  zatreeid ~ normal(0, 1);
  sigma_atreeid ~ normal(0, 1);
  sigma_y ~ normal(0, 1);
  
  y ~ normal(ypred, sigma_y); // this creates an error model where error is normally distributed
}    

generated quantities {
  array[N] real y_rep;
  for (i in 1:N) {
    y_rep[i] = normal_rng(
        a + 
        aspp[species[i]] + 
        atreeid[treeid[i]] + 
        ayear[year[i]] +
        bspp[species[i]]*gsl[i], sigma_y);
  }
  
  // prior predictive samples
  real a_prior = normal_rng(1, 3);
  real aspp_prior = normal_rng(0, 6);
  real ayear_prior = normal_rng(0, 1);
  
  real bsp_prior = normal_rng(0, 0.7);
  real sigma_y_prior = abs(normal_rng(0, 1));    
  
  real sigma_atreeid_prior = abs(normal_rng(0, 1));  
  real zatreeid_prior = normal_rng(0, 1);
  real atreeid_prior = abs(normal_rng(0, 1)) * zatreeid_prior;

  // For LOO cross-validation
  vector[N] log_lik;
  for (i in 1:N) {
    log_lik[i] = normal_lpdf(y[i] | ypred[i], sigma_y);
  }

  // Recover the full intercept per treeid
  vector[Ntreeid] fullintercept;
  vector[Ntreeid] treeid_slope;
  
  real mean_ayear = mean(ayear);   
  
  for (t in 1:Ntreeid) {
    fullintercept[t] = a +
                       aspp[treeid_species[t]] +
                       atreeid[t] +
                       mean_ayear;
    treeid_slope[t] = bspp[treeid_species[t]];
  }
  
  // Sim for each tree id, at each gslseq
  matrix[Ngslseq, Ntreeid] y_post;

  for (t in 1:Ntreeid) {
    for (g in 1:Ngslseq) {
      y_post[g, t] = normal_rng(fullintercept[t] + (treeid_slope[t]/ gslscale) * gslseq[g], sigma_y);
    }
  }
  
  // Sim for each species
  matrix[Ngslseq, Nspp] spp_mean;
  matrix[Ngslseq, Nspp] spp_post;

  spp_mean = rep_matrix(0, Ngslseq, Nspp);

  for (t in 1:Ntreeid) {
    int s = treeid_species[t];
    for (g in 1:Ngslseq) {
      spp_mean[g, s] += (fullintercept[t] + (treeid_slope[t] / gslscale) * gslseq[g])
                        / Ntreeid_per_spp[s];
    }
  }

  for (s in 1:Nspp) {
    for (g in 1:Ngslseq) {
      spp_post[g, s] = normal_rng(spp_mean[g, s], sigma_y);
    }
  }
}