
functions {
  // basis function approx. to GPs, developed by Riutort-Mayol et al. arxiv.org/2004.11408
  // implementation adapted from github.com/gabriuma/basis_functions_approach_to_GP

  // square root of m-th eigenvalue (of Laplacian with Dirichlet bnd. conds.)
  real sqrt_lambda(real L, int m){ return m * pi()/(2 * L); }
  
  // m-th eigenvalue
  real lambda(real L, int m){ return sqrt_lambda(L, m)^2;}
  
  // m-th eigenfunction
  vector phi(real L, int m, vector x){
    return (1/sqrt(L)) * sin( sqrt_lambda(L,m) * (L + x));
  }  
  
  // spectral density (spd) of exponentiated quadratic (exp_quad) kernel
  real spd_exp_quad(real alpha, real rho, real w){
    return alpha^2 * sqrt(2 * pi()) * rho * exp(-0.5 * (rho * w)^2);
  }
  
  // diagonal matrix for eigen-decomposition of exponentiated quadratic kernel
  vector diag_exp_quad(real L, int M, real alpha, real rho){
    vector[M] diag_mat;
    for(m in 1:M) diag_mat[m] = spd_exp_quad(alpha, rho, sqrt_lambda(L,m));
    return diag_mat;
  }
}

data {
  int<lower=1> N;
  int<lower=1,upper=N> T;
  int<lower=1> R;
  int<lower=1> r_start[R];
  vector[N] t;
  int<lower=0> g[N];
  int<lower=0> w[N];
  // basis function approx.
  int<lower=1> M; 
}

transformed data {
  vector[N] t_sc;
  vector[R] L;
  matrix[N,M] PHI;
  {
    for(r in 1:R-1){
      vector[(r_start[r+1] - r_start[r])] t_r = t[r_start[r]:(r_start[r+1]-1)];
      real mu_t_r = mean(t_r);
      real sd_t_r = sd(t_r);
      t_sc[r_start[r]:(r_start[r+1]-1)] = (t_r-mu_t_r)/sd_t_r;
      
      L[r] = t[r_start[r]-1] * 5.0/2.0;
    }
    
    vector[(N - r_start[R])] t_r = t[r_start[R]:N];
    real mu_t_r = mean(t_r);
    real sd_t_r = sd(t_r);
    t_sc[r_start[r]:(r_start[r+1]-1)] = (t_r-mu_t_r)/sd_t_r;
    L[R] = t[N] * 5.0/2.0;
    
  }
   
  
  
  {
    for(r in 1:R){
    }
  }
  // center and scale t
  real t_mu = mean(t);
  real t_sd = sd(t);
  vector[N] t_sc = (t-t_mu)/t_sd;
  
  // basis function approx
  real L= max(t_sc - min(t_sc)) * 5.0/2.0;
	
	// GP approximation
	matrix[N,M] PHI;
	for (m in 1:M) PHI[,m] = phi(L, m, t_sc);
}

parameters {
  // GP hyperparameters
	// vector<lower=0>[1] rho;
	// vector<lower=0>[1] alpha;
	real<lower=0> rho;
	real<lower=0> alpha;
	
	// GP centering
	real a;
	
	// latent variables for GP approx.
	vector[M] zeta;
}

transformed parameters{
	vector[N] f = PHI *  (sqrt(diag_exp_quad(L, M, alpha, rho)) .* zeta);
}

model{
	zeta ~ normal(0,1);
	rho ~ normal(0,0.2);				// lengthscale GP
	alpha ~ normal(0,1);			// magnitud GP
	a ~ normal(0,1);          // centering
	
	head(w, T) ~ binomial_logit(head(g,T), a+head(f, T));
}

generated quantities {
  vector[N] af = a + f;
}
