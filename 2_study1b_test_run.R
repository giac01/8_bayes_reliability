rm(list =ls())
Sys.setenv(RUN_REP = 1)
Sys.setenv(SEED_ENV = 99)

timeone = Sys.time()
source("4_sim_factor_tau_inequiv.R")
timetwo = Sys.time()

timetwo - timeone
