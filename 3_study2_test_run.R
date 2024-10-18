rm(list =ls())
Sys.setenv(RUN_REP = 10*8)
Sys.setenv(SEED_ENV = 99)

timeone = Sys.time()
source("6_sim_sdt.R")
timetwo = Sys.time()

timetwo - timeone
