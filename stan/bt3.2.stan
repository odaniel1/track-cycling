
functions {
  
  real match_logit_lpmf(int[] s, vector alpha){
    for(n in 1:num_elements(s)){
      if (s[n] < 3)
        return binomial_logit_lpmf(s[n] | s[n], alpha);
      else
        return binomial_logit_lpmf(2 | 3, alpha) - log(3) + log(2);
    }
  }
    real match_log_loss(int[] s, vector alpha){
    for(n in 1:num_elements(s)){
      if (s[n] == 1)
        return -binomial_logit_lpmf(1 | 1, alpha);
      else
        return -log_sum_exp(binomial_logit_lpmf(2 | 2, alpha),  binomial_logit_lpmf(2 | 3, alpha) - log(3) + log(2));
    }
  }
  
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
  real spd_exp_quad(real tau, real rho, real w){
    return tau^2 * sqrt(2 * pi()) * rho * exp(-0.5 * (rho * w)^2);
  }
  
  // diagonal matrix for eigen-decomposition of exponentiated quadratic kernel
  vector diag_exp_quad(real L, int B, real tau, real rho){
    vector[B] diag_mat;
    for(b in 1:B) diag_mat[b] = spd_exp_quad(tau, rho, sqrt_lambda(L,b));
    return diag_mat;
  }
}

data {
  int<lower=0> R; // Riders
  int<lower=0> M; // Matches
  int<lower=0,upper = M> T; // Training matches
  int<lower=0> D; // Dates
  int<lower=1,upper=R> winner_id[M]; // ID's specifying riders in match
  int<lower=1,upper=R> loser_id[M];
  int<lower=1,upper=3> sprints[M];
  int<lower=0> winner_date_no[M];
  int<lower=0> loser_date_no[M];
  int<lower=0> date_index_R[R];
  vector[D] rider_dates;
  
  // basis function approx.
  int<lower=1> B; 
}

transformed data {
  vector[D] d_sc;
  real L;
  matrix[D,B] PHI;
  {
    for(r in 1:R-1){
      // center and scale rider t
      vector[(date_index_R[r+1] - date_index_R[r])] d_r = rider_dates[date_index_R[r]:(date_index_R[r+1]-1)];
      real mu_d_r = mean(d_r);
      real sd_d_r = sd(d_r);
      d_sc[date_index_R[r]:(date_index_R[r+1]-1)] = (d_r-mu_d_r)/sd_d_r;
      
      // rider basis function approx
      L[r] = rider_dates[date_index_R[r+1]-1] * 5.0/2.0;
      
      // rider GP approximation
      for(b in 1:B) PHI[date_index_R[r]:(date_index_R[r+1]-1),b] = phi(L[r],b,d_sc[date_index_R[r]:(date_index_R[r+1]-1)]);
    }
    
    {
      // center and scale rider t
      vector[(D - date_index_R[R])] d_r = rider_dates[date_index_R[R]:D];
      real mu_d_r = mean(d_r);
      real sd_d_r = sd(d_r);
      d_sc[date_index_R[R]:D] = (d_r-mu_d_r)/sd_d_r;
      
      // rider basis function approx      
      L[R] = rider_dates[D] * 5.0/2.0;
      
      // rider GP approximation      
      for(b in 1:B) PHI[date_index_R[R]:D,b] = phi(L[R],b,d_sc[date_index_R[R]:D]);
    }
  }
}

parameters {
  // Rider average strength
  real alpha0[R];
  real<lower=0>sigma;
  
  // GP hyperparameters
	vector<lower=0>[R] rho;
	vector<lower=0>[R] tau;
	
	// latent variables for GP approx.
	vector[R*B] zeta;
}

transformed parameters{
	vector[D] f;
	vector[D] alphaD;
  vector[M] delta;
	  {
	    
    for(r in 1:R-1){
      if(date_index_R[r] != (date_index_R[r+1]-1)){
        f[date_index_R[r]:(date_index_R[r+1]-1)] =
          PHI[date_index_R[r]:(date_index_R[r+1]-1),] *  (sqrt(diag_exp_quad(L[r], B, tau[r], rho[r])) .* segment(zeta, 1 + (r-1)*B, B));
        alphaD[date_index_R[r]:(date_index_R[r+1]-1)] =  alpha0[r] + f[date_index_R[r]:(date_index_R[r+1]-1)];
      }
      else{
        f[date_index_R[r]] = 0;
        alphaD[date_index_R[r]] =  alpha0[r];
      }
    }
    
    if(date_index_R[R] != D){
      f[date_index_R[R]:D] =
          PHI[date_index_R[R]:D,] *  (sqrt(diag_exp_quad(L[R], B, tau[R], rho[R])) .* segment(zeta, 1 + (R-1)*B, B));
      
      alphaD[date_index_R[R]:D] = alpha0[R] + f[date_index_R[R]:D];
    }
    else{
      f[date_index_R[R]] = 0;
      alphaD[date_index_R[R]] =  alpha0[R];
    }

	  }
	  
	  for(m in 1:M){
	    delta[m] = alphaD[date_index_R[winner_id[m]] + winner_date_no[m]] - alphaD[date_index_R[loser_id[m]] + loser_date_no[m]];
	  }
}


model{
  sigma ~ gamma(15,40);
	alpha0 ~ normal(0,sigma);
	
	zeta ~ normal(0,1);
	rho ~ normal(0,1);		// lengthscale GP
	tau ~ normal(0,0.2);		// magnitud GP
	
	sprints ~ match_logit(delta);
  head(sprints, T) ~ match_logit(head(delta, T));
}

generated quantities {
  
  real training_avg_log_loss = -inv(T) * bernoulli_logit_lpmf(1 | head(delta, T));
  real evaluation_avg_log_loss = -inv(M-T) * bernoulli_logit_lpmf(1 | tail(delta, M - T));
  
  real training_avg_match_log_loss = inv(T) * match_log_loss(head(sprints, T), head(delta, T));
  real evaluation_avg_match_log_loss = inv(M-T) * match_log_loss( tail(sprints, M - T), tail(delta, M - T));
  
  real training_accuracy = 0;
  real evaluation_accuracy = 0;
  
  for(m in 1:T){
    training_accuracy += (delta[m] > 0);
  }
    for(m in (T+1):M){
    evaluation_accuracy += (delta[m] > 0);
  }
  
  training_accuracy = training_accuracy * inv(T);
  evaluation_accuracy = evaluation_accuracy * inv(M-T);
}