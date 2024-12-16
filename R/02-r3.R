# R3 = 1 / [1 + (d × (Emax − 1) × 10 × Imax,u) / (EC50 + (10 × Imax,u))] using this equation
r3 <- function(d = 1, Emax, Imax, EC50) {
  1 / (1 + (d * (Emax - 1) * 10 * Imax) / (EC50 + (10 * Imax)))
}
r3(Emax = 44, Imax = 1.97, EC50 = 5.2) # 0.5
