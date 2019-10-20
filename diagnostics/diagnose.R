diagnose <- function(network, cases) {
  
  
  ################## Calculate probabilities for samples ###################
 
  P <- function(sample, known, probs = network) {
    P_pn = probs$P_pn
    P_tb = probs$P_tb
    P_lc = probs$P_lc
    P_br = probs$P_br
    P_te = probs$P_te
    P_vtb = probs$P_vtb
    P_sm = probs$P_sm
    P_xr = probs$P_xr 
    P_dy = probs$P_dy
    
    # Probabilities are found on index value + 1 for given attribute
    # WARNING: Does not apply to temperature
    
    # Unknown 
    pn = sample$pn + 1
    tb = sample$tb + 1
    lc = sample$lc + 1
    br = sample$br + 1
    
    # Known
    te = known$te # This will be input to function, do not add 1
    vtb = known$vtb + 1
    sm = known$sm + 1
    xr = known$xr + 1
    dy = known$dy + 1
    
    
   probability <- P_pn[pn] * P_vtb[vtb] * P_sm[sm] * P_tb[tb, vtb] * P_lc[lc, sm] *
       P_br[br, sm] * P_te(te)[pn] * P_xr[xr, pn, tb, lc] * P_dy[dy, lc, br]

    return(probability)
  }
  
  ########## Candidate function for binary attributes #####################
  
  f <- function(X) {
    return(abs(X-1))
  }
  
  ######################### MCMC Metropolitan in Gibbs ###########################
  
  # Task is to determine: 
  # P(Pn | Te, VTB, Sm, XR, Dy)
  # P(TB | Te, VTB, Sm, XR, Dy)
  # P(LC | Te, VTB, Sm, XR, Dy)
  # P(Br | Te, VTB, Sm, XR, Dy)
  
  # E = {Te, VTB, Sm, XR, Dy}
  # U = {Pn, TB, LC, Br}
  # f_Pn = f_TB = f_LC = f_Br: f(X) = 0 if X = 1, f(X) = 1 otherwise
  
  metro_in_gibbs <- function(te, vtb, sm, xr, dy) {
    
    sample_size <- 1000
    burn_size <- floor(0.1*sample_size)
    zeros <- numeric(sample_size)
    
    known <- list(te = te, vtb = vtb, sm = sm, xr = xr, dy = dy)
    samples <- data.frame(pn = zeros, tb = zeros, lc = zeros, br = zeros)
    
    random_values <- runif(4)
    pn <- as.integer(random_values[1] > 0.5)
    tb <- as.integer(random_values[2] > 0.5)
    lc <- as.integer(random_values[3] > 0.5)
    br <- as.integer(random_values[4] > 0.5)
    samples[1, ] <- c(pn, tb, lc, br)
    #print(samples[1, ])
    n_unknown <- dim(samples)[2]

    
    # For each sample i
    for(i in 2:sample_size){
      
      # Assigned is equal to previous sample 
      assigned <- samples[i-1, ]
      
      # For each unknown parameter p
      for(p in 1:n_unknown) {
        
        # Proposed is equal to assigned, with unknown parameter p replaced by
        # f(p)
        
        proposed <- assigned
        proposed[p] <- f(proposed[p])

        # Calculate p for new and old
        P_old <- P(assigned, known)
        P_new <- P(proposed, known)

        if(P_new > P_old) {
          assigned <- proposed
          
        } else {
          random <- runif(1)
          ratio <- P_new/P_old

          if(random < ratio) {
            assigned <- proposed
          }
          
        }
        
      }
      samples[i, ] <- assigned
      
    }

    samples <- tail(samples, -burn_size)
    #samples <- samples[burn_size:sample_size,]
    n_samples = dim(samples)[1]
    P_pn_1 <- length(which(samples$pn == 1))/n_samples
    P_tb_1 <- length(which(samples$tb == 1))/n_samples
    P_lc_1 <- length(which(samples$lc == 1))/n_samples
    P_br_1 <- length(which(samples$br == 1))/n_samples
    
    P_out = c(P_pn_1, P_tb_1, P_lc_1, P_br_1)
    #print(P_pn_1 <- length(which(samples$pn == 1)))
    return(P_out)
  }

  output <- matrix(0, 10, 4)
  
  for(i in 1:dim(cases)[1]) {
    case <- cases[i, ]
    te <- case$Te
    vtb <- case$VTB
    sm <- case$Sm
    xr <- case$XR
    dy <- case$Dy
    
    
    output[i,] <- metro_in_gibbs(te, vtb, sm, xr, dy)
    
  }
  print(output)
  return(output)
  
}
