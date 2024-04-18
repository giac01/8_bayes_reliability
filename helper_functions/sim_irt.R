# rm(list = ls())
# 
# n_pps  = 100
# 
# item_c = c( 0,  0,  0,  0)         # guessing parameter
# item_a = c(.5, .5, .5, .5)  # discrimination
# item_b = c( 0,-.25,.5 ,.25)  # difficulty
# 
# theta_mean  = 0
# theta_sd = 4

sim_irt = function(
    n_pps,                      # Number of pps
    
    # Item parameters 
    item_c,  # guessing parameter
    item_a ,  # discrimination
    item_b, # difficulty
    
    theta_mean ,
    theta_sd
  ){


if (length(item_c)!=length(item_a)) stop("error")
if (length(item_c)!=length(item_b)) stop("error")
if (length(item_a)!=length(item_b)) stop("error")

dat = expand.grid(pps = 1:n_pps, item = 1:length(item_c))

theta = rnorm(n_pps, mean = theta_mean, sd = theta_sd)

dat$theta  = theta[dat$pps]
dat$item_c = item_c[dat$item]
dat$item_a = item_a[dat$item]
dat$item_b = item_b[dat$item]
dat$p      = dat$item_c + (1-dat$item_c)/(1+exp(-dat$item_a*(dat$theta-dat$item_b)))

dat$y      = rbinom(nrow(dat), size = 1, prob = dat$p)

return(dat)
}
