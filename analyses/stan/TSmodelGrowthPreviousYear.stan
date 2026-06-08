// previous year growth condition as another slope predictor
// mid march 2026
data {
  int<lower=0> N;
  int<lower=0> Nspp;
  array[N] int species;
  int<lower=0> Ntreeid;
  array[N] int treeid;
  int<lower=0> Nyear;
  array[N] int year; 
  vector[N] gdd;
  vector[N] gddyr;
  array[N] real y;
}
parameters {
  real a;
  real<lower=0> sigma_atreeid;
  real<lower=0> sigma_y;
  vector[Ntreeid] zatreeid;
  vector[Nspp-1] aspp_raw;   // sum to zero to fix tree depth issue
  vector[Nyear] ayear;
  vector[Nspp] bsp;
  vector[Nspp] bspyr;
}
transformed parameters {
  vector[Ntreeid] atreeid;
  atreeid = sigma_atreeid * zatreeid;

  vector[Nspp] aspp;          
  aspp[1:Nspp-1] = aspp_raw;
  aspp[Nspp] = -sum(aspp_raw);

  array[N] real ypred;
  for (i in 1:N) {
    ypred[i] =
      a +
      aspp[species[i]] +
      atreeid[treeid[i]] +
      ayear[year[i]] +
      bsp[species[i]] * gdd[i] +
      bspyr[species[i]] * gddyr[i];
  }
}
model {
  a ~ normal(2, 10);
  zatreeid ~ normal(0, 1);
  aspp_raw ~ normal(0, 12);
  ayear ~ normal(0, 1);
  bsp ~ normal(0, 1);
  bspyr ~ normal(0, 1);
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
      ayear[year[i]] +
      bsp[species[i]] * gdd[i] + 
      bspyr[species[i]] * gddyr[i],
      sigma_y);
  }
  real a_prior = normal_rng(2, 10);
  real atreeid_prior = normal_rng(0, 1);    
  real aspp_prior = normal_rng(0, 12);
  real ayear_prior = normal_rng(0, 1);
  real bsp_prior = normal_rng(0, 1);
  real bspyr_prior = normal_rng(0, 1);
  real zatreeid_prior = normal_rng(0, 1);
  real sigma_atreeid_prior = abs(normal_rng(0, 1));
  real sigma_y_prior = abs(normal_rng(0, 1));
}