
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
  // int<lower=1,upper=N> T;
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
  real L;
  matrix[N,M] PHI;
  
  {
    real mu_t = mean(t);
    real sd_t = sd(t);
    t_sc = (t-mu_t)/sd_t;
    L = (max(t_sc) - min(t_sc)) * 5.0/2.0;

    // rider GP approximation      
    for(m in 1:M) PHI[,m] = phi(L,m,t_sc);
  }
}

parameters {
  // GP hyperparameters
	vector<lower=0>[R] rho;
	vector<lower=0>[R] alpha;
	
	// GP centering
	real a[R];
	
	// latent variables for GP approx.
	vector[R*M] zeta;
}

transformed parameters{
	vector[N] f;
	vector[N] af;
	  {
    for(r in 1:R-1){
      f[r_start[r]:(r_start[r+1]-1)] =
        PHI[r_start[r]:(r_start[r+1]-1),] *  (sqrt(diag_exp_quad(L, M, alpha[r], rho[r])) .* segment(zeta, 1 + (r-1)*M, M));
        
      af[r_start[r]:(r_start[r+1]-1)] =  a[r] + f[r_start[r]:(r_start[r+1]-1)];
    }
    
    f[r_start[R]:N] =
        PHI[r_start[R]:N,] *  (sqrt(diag_exp_quad(L, M, alpha[R], rho[R])) .* segment(zeta, 1 + (R-1)*M, M));

    af[r_start[R]:N] = a[R] + f[r_start[R]:N];
	  }
}

model{
	zeta ~ normal(0,1);
	rho ~ normal(0,0.2);				// lengthscale GP
	alpha ~ normal(0,1);			// magnitud GP
	a ~ normal(0,1);          // centering
	
	w ~ binomial_logit(g, af);
}
