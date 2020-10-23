get.jags <- function(Data)
{
	#get initial values
  get.WTA.inits <- function(WTA, CENS) {
      Inits.WTA <- rep(NA, length(WTA))
      Inits.WTA[which(CENS == 0)] <- runif(length(which(CENS == 0)), -2.5, 0)
      Inits.WTA[which(CENS == 2)] <- runif(length(which(CENS == 2)), 2.5, 5)

      return(Inits.WTA)
  }

  inits1 <- list(WTA = get.WTA.inits(Data$WTA, Data$CENS), sig = runif(1, 0, 10), beta_x = runif(Data$Nx, -5, 5), beta_y = runif(Data$Ny, -5, 5))
  inits2 <- list(WTA = get.WTA.inits(Data$WTA, Data$CENS), sig = runif(1, 0, 10), beta_x = runif(Data$Nx, -5, 5), beta_y = runif(Data$Ny, -5, 5))
  inits3 <- list(WTA = get.WTA.inits(Data$WTA, Data$CENS), sig = runif(1, 0, 10), beta_x = runif(Data$Nx, -5, 5), beta_y = runif(Data$Ny, -5, 5))

  #cl <- makeCluster(3) # comment out if not using parallel processing

  # edit methods depending on whether using parallel processing or not
	fit <- run.jags(model="./code/wta-jags-model.txt", monitor = c("beta_x", "beta_y", "sig"), data = Data , n.chains = 3 , inits = list(inits1,inits2,inits3), burnin = 10000, adapt = 1000, sample = 10000, jags = "C:/Program Files/JAGS/JAGS-4.3.0/x64/bin/jags-terminal.exe", ,method = "rjags")  # , method = "rjparallel", cl = cl)

	#stopCluster(cl) # comment out if not using parallel processing

	return(fit)
}
