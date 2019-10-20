learn <- function(hist) {
  
  pn <- hist$Pn
  te <- hist$Te
  vtb <- hist$VTB
  tb <- hist$TB
  sm <- hist$Sm
  lc <- hist$LC
  br <- hist$Br
  xr <- hist$XR
  dy <- hist$Dy
  
  
  # Number of training examples
  N_train <- dim(hist)[1]
  
  # P(Pn = 1) , P(Pn = 0)
  P_pn <- matrix(0, 1, 2)
  n_pn_0 <- length(which(pn == 0)) + 1
  P_pn[1] <- n_pn_0/(N_train + 2)  # Pn = 0
  P_pn[2] <- 1 - P_pn[1]     # Pn = 1
  
  
  # P(VTB= 1) , P(VTB = 0) 
  P_vtb <- matrix(0, 1, 2)
  n_vtb_0 <- length(which(vtb == 0)) + 1
  P_vtb[1] <- n_vtb_0/(N_train + 2)
  P_vtb[2] <- 1 - P_vtb[1]
  
  # P(Sm = 1) , P(Sm = 0)
  P_sm <- matrix(0, 1, 2)
  n_sm_0 <- length(which(sm == 0)) + 1
  P_sm[1] <- n_sm_0/(N_train + 2)
  P_sm[2] <- 1 - P_sm[1]
  
  
  
  # P(Te = X| Pn = 1) , P(Te = X| Pn = 0) 
  
  # Pn = 0
  pn_0_idx <- which(pn == 0)
  te_pn_0_mean <- mean(te[pn_0_idx])
  te_pn_0_std <- sd(te[pn_0_idx])
  
  # Pn = 1
  pn_1_idx <- which(pn == 1)
  te_pn_1_mean <- mean(te[pn_1_idx])
  te_pn_1_std <- sd(te[pn_1_idx])
  
  P_te <- function(temp, pn0_mean = te_pn_0_mean, pn0_std = te_pn_0_std,
                   pn1_mean = te_pn_1_mean, pn1_std = te_pn_1_std) {

    # Pn = 0
    D_te_pn_0 <- dnorm(temp, pn0_mean, pn0_std)
    
    
    # Pn = 1
    D_te_pn_1 <- dnorm(temp, pn1_mean, pn1_std)
    
    # Convert densities to probabilities
    P_te_pn_0 <- D_te_pn_0/(D_te_pn_1 + D_te_pn_0)
    P_te_pn_1 <- 1 - P_te_pn_0
    
    
    return(c(P_te_pn_0, P_te_pn_1))  
  }
  
  
  # P(TB = 1 | VTB = 1) , P(TB =  1| VTB= 0) and 
  # P(TB = 0 | VTB = 1) , P(TB = 0 | VTB = 0)
  
  P_tb <- matrix(0, 2, 2)
  
  for(i in 1:2) {
    for(j in 1:2){
      tb_idx <- which(tb == i-1)
      vtb_idx <- which(vtb == j-1)
      
      P_tb[i, j] <- (length(intersect(tb_idx, vtb_idx)) + 1)/(length(vtb_idx) + 2)
    }
  }
  
  
  # P(LC = 1 | Sm = 1) , P(LC =  1| Sm = 0) and 
  # P(LC = 0 | Sm = 1) , P(LC = 0 | Sm = 0)
  
  P_lc <- matrix(0, 2, 2)
  
  for(i in 1:2) {
    for(j in 1:2){
      lc_idx <- which(lc == i-1)
      sm_idx <- which(sm == j-1)
      
      P_lc[i, j] <- (length(intersect(lc_idx, sm_idx)) + 1)/(length(sm_idx) + 2)
    }
  }
  
  # P(Br = 1 | Sm = 1) , P(Br =  1| Sm = 0) and 
  # P(Br = 0 | Sm = 1) , P(Br = 0 | Sm = 0)
  
  P_br <- matrix(0, 2, 2)
  
  for(i in 1:2) {
    for(j in 1:2){
      br_idx <- which(br == i-1)
      sm_idx <- which(sm == j-1)
      
      P_br[i, j] <- (length(intersect(br_idx, sm_idx)) + 1)/(length(sm_idx) + 2)
    }
  }
  
  # P(XR| Pn, TB, LC)
  
  P_xr <- array(0, c(2,2,2,2))
  
  for(i in 1:2) {
    for(j in 1:2){
      for(k in 1:2){
        for(l in 1:2){
          xr_idx <- which(xr == i-1) # index position 1 in array
          pn_idx <- which(pn == j-1) # index position 2 in array
          tb_idx <- which(tb == k-1) # index position 3 in array
          lc_idx <- which(lc == l-1) # index position 4 in array
        
          intersection <- intersect(pn_idx, intersect(tb_idx, lc_idx))
          
          P_xr[i, j, k, l] <- (length(intersect(xr_idx, intersection)) + 1)/(length(intersection) + 2)
          
        }
      }
    }
  }

  # P(Dy| LC, Br)
  
  P_dy <- array(0, c(2,2,2))
  
  for(i in 1:2) {
    for(j in 1:2){
      for(k in 1:2){
  
          dy_idx <- which(dy == i-1)
          lc_idx <- which(lc == j-1)
          br_idx <- which(br == k-1)

          
          intersection <- intersect(lc_idx, br_idx)

          P_dy[i, j, k] <- (length(intersect(dy_idx, intersection)) + 1)/(length(intersection) + 2)
          }
    }
  }
  
  output <- list(P_pn = P_pn, P_tb = P_tb, P_lc = P_lc, P_br = P_br,
                 P_te = P_te, P_vtb = P_vtb, P_sm = P_sm, 
                 P_xr = P_xr, P_dy = P_dy)
  
  
  return(output)
}