get.jags <- function(Data)
{
	# get initial values for censored WTA
  get.WTA.inits <- function(WTA, CENS) {
      Inits.WTA <- rep(NA, length(WTA))
      Inits.WTA[which(CENS == 0)] <- runif(length(which(CENS == 0)), -2.5, 0)
      Inits.WTA[which(CENS == 2)] <- runif(length(which(CENS == 2)), 2.5, 5)

      return(Inits.WTA)
  }

	# get initial values for remaining parameters

	inits1 <- list(WTA = get.WTA.inits(Data$WTA, Data$CENS), sig = runif(1, 0, 2), phi = runif(1, 0, 2), beta_x = runif(Data$Nx, -2, 2), beta_y = runif(Data$Ny, -2, 2), beta_z = runif(Data$Nz, -2, 2), betar_x = runif(Data$NxR, -2, 2), betar_y = runif(Data$NyR, -2, 2), betar_z = runif(Data$NzR, -2, 2),	mulvx = runif(1, -2, 2), mulvy = runif(1, -2, 2), mulvz = runif(1, -2, 2), siglvx = runif(1, 0, 2), siglvy = runif(1, 0, 2), siglvz = runif(1, 0, 2))

	inits2 <- list(WTA = get.WTA.inits(Data$WTA, Data$CENS), sig = runif(1, 0, 2), phi = runif(1, 0, 2), beta_x = runif(Data$Nx, -2, 2), beta_y = runif(Data$Ny, -2, 2), beta_z = runif(Data$Nz, -2, 2), betar_x = runif(Data$NxR, -2, 2), betar_y = runif(Data$NyR, -2, 2), betar_z = runif(Data$NzR, -2, 2),	mulvx = runif(1, -2, 2), mulvy = runif(1, -2, 2), mulvz = runif(1, -2, 2), siglvx = runif(1, 0, 2), siglvy = runif(1, 0, 2), siglvz = runif(1, 0, 2))

	inits3 <- list(WTA = get.WTA.inits(Data$WTA, Data$CENS), sig = runif(1, 0, 2), phi = runif(1, 0, 2), beta_x = runif(Data$Nx, -2, 2), beta_y = runif(Data$Ny, -2, 2), beta_z = runif(Data$Nz, -2, 2), betar_x = runif(Data$NxR, -2, 2), betar_y = runif(Data$NyR, -2, 2), betar_z = runif(Data$NzR, -2, 2),	mulvx = runif(1, -2, 2), mulvy = runif(1, -2, 2), mulvz = runif(1, -2, 2), siglvx = runif(1, 0, 2), siglvy = runif(1, 0, 2), siglvz = runif(1, 0, 2))

  cl <- makeCluster(3) # comment out if not using parallel processing

  # edit methods depending on whether using parallel processing or not
	fit <- run.jags(model="wta-jags-model.txt", monitor = c("beta_x", "beta_y", "beta_z", "betar_x", "betar_y", "betar_z", "sig", "phi"), data = Data, n.chains = 3,
								inits = list(inits1,inits2,inits3), burnin = 10000, adapt = 1000, sample = 10000,
								jags = "C:/Program Files/JAGS/JAGS-4.3.0/x64/bin/jags-terminal.exe", method = "rjparallel", cl = cl) # method = "rjags"

	stopCluster(cl) # comment out if not using parallel processing

	return(fit)
}

get.jags.sel <- function(Data)
{
	# get initial values for censored WTA
  get.WTA.inits <- function(WTA, CENS) {
      Inits.WTA <- rep(NA, length(WTA))
      Inits.WTA[which(CENS == 0)] <- runif(length(which(CENS == 0)), -2.5, 0)
      Inits.WTA[which(CENS == 2)] <- runif(length(which(CENS == 2)), 2.5, 5)

      return(Inits.WTA)
  }

	# get initial values for remaining parameters

	inits1 <- list(WTA = get.WTA.inits(Data$WTA, Data$CENS), sig = runif(1, 0, 2), phi = runif(1, 0, 2), b_x = runif(Data$Nx, -2, 2), b_y = runif(Data$Ny, -2, 2), b_z = runif(Data$Nz, -2, 2), br_x = runif(Data$NxR, -2, 2), br_y = runif(Data$NyR, -2, 2), br_z = runif(Data$NzR, -2, 2),	mulvx = runif(1, -2, 2), mulvy = runif(1, -2, 2), mulvz = runif(1, -2, 2), siglvx = runif(1, 0, 2), siglvy = runif(1, 0, 2), siglvz = runif(1, 0, 2))

	inits2 <- list(WTA = get.WTA.inits(Data$WTA, Data$CENS), sig = runif(1, 0, 2), phi = runif(1, 0, 2), b_x = runif(Data$Nx, -2, 2), b_y = runif(Data$Ny, -2, 2), b_z = runif(Data$Nz, -2, 2), br_x = runif(Data$NxR, -2, 2), br_y = runif(Data$NyR, -2, 2), br_z = runif(Data$NzR, -2, 2),	mulvx = runif(1, -2, 2), mulvy = runif(1, -2, 2), mulvz = runif(1, -2, 2), siglvx = runif(1, 0, 2), siglvy = runif(1, 0, 2), siglvz = runif(1, 0, 2))

	inits3 <- list(WTA = get.WTA.inits(Data$WTA, Data$CENS), sig = runif(1, 0, 2), phi = runif(1, 0, 2), b_x = runif(Data$Nx, -2, 2), b_y = runif(Data$Ny, -2, 2), b_z = runif(Data$Nz, -2, 2), br_x = runif(Data$NxR, -2, 2), br_y = runif(Data$NyR, -2, 2), br_z = runif(Data$NzR, -2, 2),	mulvx = runif(1, -2, 2), mulvy = runif(1, -2, 2), mulvz = runif(1, -2, 2), siglvx = runif(1, 0, 2), siglvy = runif(1, 0, 2), siglvz = runif(1, 0, 2))

  cl <- makeCluster(3) # comment out if not using parallel processing

  # edit methods depending on whether using parallel processing or not
	fit <- run.jags(model="wta-jags-model-sel.txt", monitor = c("eta_g_x", "ind_gl_x", "etar_g_x", "beta_x", "betar_x", "eta_g_y", "ind_gl_y", "etar_g_y", "beta_y", "betar_y", "eta_g_z", "ind_gl_z",
								"etar_g_z", "beta_z", "betar_z", "sig", "phi"), data = Data, n.chains = 3,
								inits = list(inits1,inits2,inits3), burnin = 1000, adapt = 10000, sample = 10000,
								jags = "C:/Program Files/JAGS/JAGS-4.3.0/x64/bin/jags-terminal.exe", method = "rjparallel", cl = cl) # method = "rjags"

	stopCluster(cl) # comment out if not using parallel processing

	return(fit)
}
