// One-species version of the ring width model, for simulation
// Partial pooling on tree id intercepts (non-centered) and year intercepts

data{
int<lower=0> N;     // number of total observations
int<lower=0> Ntreeid;  // number of tree ids (grouping factor)
array[N] int treeid;   // tree id identity, coded as int
int<lower=0> Nyear;
array[N] int year; 
vector[N] gdd;     // gdd (predictor for slope)
array[N] real y;         // ring width (response)
}

parameters{
real a;        // mean intercept across everything
real b;        // gdd slope
real<lower=0> sigma_atreeid;
real<lower=0> sigma_y;     // measurement error, noise etc.      
vector[Ntreeid] zatreeid; // variation of intercept across tree ids, non-centered
vector[Nyear] ayear;
}

transformed parameters{
vector[Ntreeid] atreeid;
atreeid = 0 + sigma_atreeid*zatreeid; // non-centered parameterization on atreeid

array[N] real ypred;
for (i in 1:N){
    ypred[i]=
        a + 
        atreeid[treeid[i]] + 
        ayear[year[i]] +
        b*gdd[i];
    }
}

model{    
  a ~ normal(1, 3);
  ayear ~ normal(0, 1);
  b ~ normal(0, 1);
  zatreeid ~ normal(0, 1);
  sigma_atreeid ~ normal(0, 1);
  sigma_y ~ normal(0, 1);
  
  y ~ normal(ypred, sigma_y);
}