dogam <- function(dat, fam="gaussian", meth='ML', gamma=1.4, k_lgln=20, wv=TRUE, lstk=TRUE, lat_lon_mon=TRUE, llm_k=NA, llm_cc='cc', wth=TRUE, hbf=TRUE, hkc=TRUE, yrf=TRUE, yr=FALSE) {
  form <- paste0("logwt ~ s(loglen, k=", k_lgln, ") + s(obs_name, bs='re')")
  if(wv) form <- paste0(form, " + wavescale")
  if(lstk) form <- paste0(form," + lightstick")
  if(lat_lon_mon) {
    if(is.na(llm_k)) { 
      form <- paste0(form, " + te(lon, lat, mon, k = c(10,10,4)") 
    } else {
      form <- paste0(form, " + te(lon, lat, mon, k = ", llm_k)
    }
    if(llm_cc=='cc') form <- paste0(form, ", bs = c('cr','cr','cc')")
    if(llm_cc=='cs') form <- paste0(form, ", bs = c('cs','cs','cs')")
    if(llm_cc=='cr') form <- paste0(form, ", bs = c('cr','cr','cr')")
    form <- paste0(form, ")")
  }  
  if(wth) form <- paste0(form, " + weather")
  if(hbf) form <- paste0(form, " + s(hbf,k=15)")
  if(hkc) form <- paste0(form, " + HkCourse")
  if(yrf) form <- paste0(form, " + yrf")
  if(yr) form <- paste0(form, " + s(year, k=6)")
  mod1rgf_ml <- gam(as.formula(form), data = dat, family = fam, method = meth, gamma = gamma)
  return(mod1rgf_ml)
}


doplots <- function(mod, dat, direc, modn, diag=F, loglen=F, maps=F, hbf=F, year=F, yrf=F, wv=F, obs=F, lightstick=F, weather=F, comprehensive=F) {
  # Create individual plots as requested
  if(diag)       plot_diags(mod, dat, direc, modn)
  if(loglen)     plot_loglen(mod, dat, direc, modn)
  if(maps)       plot_maps(mod, dat, direc, modn)
  if(hbf)        plot_hbf(mod, dat, direc, modn)
  if(weather)    plot_weather(mod, dat, direc, modn)
  if(lightstick) plot_lightstick(mod, dat, direc, modn)
  if(year)       plot_year(mod, dat, direc, modn)
  if(yrf)        plot_yrf(mod, dat, direc, modn)
  if(wv)         plot_wv(mod, dat, direc, modn)
  if(obs)        plot_obs(mod, dat, direc, modn)
  
  # Create comprehensive plot if requested
  if(comprehensive) {
    # Check if required plots are also requested
    needed_plots <- c(diag, maps, hbf, year, loglen, obs)
    needed_names <- c("diag", "maps", "hbf", "year", "loglen", "obs")
    
    if(!all(needed_plots)) {
      missing <- needed_names[!needed_plots]
      warning(paste("Some plots needed for comprehensive figure are not requested:",
                    paste(missing, collapse=", "), 
                    "\nThese will still be included in the comprehensive plot."))
    }
    
    # Create comprehensive plot
    create_comprehensive_plot(mod, dat, direc, modn)
  }
}



# plot_diags <- function(mod, dat, direc, modn) {
#   # Convert to a gamViz object
#   o <- getViz(mod)
#   
#   # Create the check plots
#   check_plots <- check.gamViz(o)
#   
#   # Apply theme to each plot in the list
#   for(i in 1:length(check_plots)) {
#     check_plots[[i]] <- check_plots[[i]] + 
#       theme(axis.text = element_text(size = 14),
#             axis.title = element_text(size = 16))
#   }
#   
#   # Arrange plots in a 2x2 grid using gridPrint
#   # This returns a gtable that can be saved with ggsave
#   p <- gridPrint(check_plots, plots = 1:4, ncol = 2)
#   
#   # Use normal ggplot2 saving mechanism
#   ggsave(paste0(direc, "/", modn, "_gamcheck.png"),
#          plot = p,
#          width = 10, 
#          height = 8, 
#          dpi = 300)
# }

plot_diags <- function(mod, dat, direc, modn) {
  # Make sure cowplot is installed
  if (!requireNamespace("cowplot", quietly = TRUE)) {
    install.packages("cowplot")
  }
  library(cowplot)
  library(ggplot2)
  
  # Convert to a gamViz object
  o <- getViz(mod)
  
  # Get the check plots
  check_plots <- check.gamViz(o)
  
  # Apply theme to each plot without trying to convert them
  for(i in 1:length(check_plots)) {
    check_plots[[i]] <- check_plots[[i]] + 
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 16))
  }
  
  # Set up a device for arranging plots
  png(paste0(direc, "/", modn, "_gamcheck.png"), 
      width = 3000, height = 2400, res = 300)
  
  # Use grid.arrange from the grid package which is more compatible
  # with these plot objects
  gridExtra::grid.arrange(
    check_plots[[1]], check_plots[[2]],
    check_plots[[3]], check_plots[[4]],
    ncol = 2
  )
  
  # Close the device
  dev.off()
}


plot_loglen <- function(mod, dat, direc, modn) {
  new_data <- with(dat,
                   expand.grid(hbf=median(hbf),
                               loglen=seq(min(loglen), max(loglen), length.out=200),
                               logwt=median(logwt),
                               obs_name=factor(obs_name[1],levels=levels(obs_name)),
                               mon=median(mon),
                               lat = median(lat),
                               lon = median(lon),
                               lightstick = lightstick[1],
                               weather = weather[1],
                               HkCourse = HkCourse[1],
                               yrf = yrf[1],
                               wavescale=factor(wavescale[1], levels=levels(wavescale)),
                               year = median(year)))
  excl <- c("s(obs_name)", "te(lat,lon,mon)", "s(hbf)", "wavescale", "s(year)", "yrf")
  pred  <- predict(mod, new_data, exclude = excl,
                   type = "link", se.fit = TRUE, unconditional = TRUE)
  pred  <- cbind(pred, new_data)
  pred  <- transform(pred, lwr_ci = (fit - (2 * se.fit)),
                     upr_ci = (fit + (2 * se.fit)),
                     fitted = (fit))
  windows()
  plot(pred$loglen, pred$fitted, xlab = "loglen", ylab = "logwt", type = "l")
  lines(pred$loglen, pred$upr_ci, lty=2)
  lines(pred$loglen, pred$lwr_ci, lty=2)
  lines(x=c(min(new_data$loglen), max(new_data$loglen)), y=c(min(pred$fitted), max(pred$fitted)), col=2)
  savePlot(paste0(direc, "/", modn, "__loglen1.png"), type = "png")
  
  o <- getViz(mod)
  
  print(plot(o, select=1))
  savePlot(paste0(direc, "/", modn, "__loglen2.png"), type = "png")
}

pred_LWR <- function(mod, dat, xlat, xlon, xmon) {
  new_data <- with(dat,
                   expand.grid(hbf=median(hbf),
                               loglen=seq(min(loglen), max(loglen), length.out=200),
                               logwt=median(logwt),
                               obs_name=factor(obs_name[1],levels=levels(obs_name)),
                               mon=xmon,
                               lat = xlat,
                               lon = xlon,
                               lightstick = lightstick[1],
                               weather = weather[1],
                               HkCourse = HkCourse[1],
                               yrf = yrf[1],
                               wavescale=factor(wavescale[1], levels=levels(wavescale)),
                               year = median(year)))
  excl <- c("s(obs_name)", "s(hbf)", "wavescale", "lightstick", "HkCourse", "yrf", "s(year)")
  pred  <- predict(mod, new_data, exclude = excl,
                   type = "link", se.fit = TRUE, unconditional = TRUE)
  pred  <- cbind(pred, new_data)
  pred  <- transform(pred, lwr_ci = (fit - (2 * se.fit)),
                     upr_ci = (fit + (2 * se.fit)),
                     fitted = (fit))
  return(pred)
}


plot_maps <- function(mod, dat, direc, modn) {
  b <- dat[dat$lat < -8 & dat$lon < 170,]
  a <- tapply(b$lat, list(b$lat, b$lon), length)
  windows(20,20)
  par(mfrow=c(2,2), mar = c(5,4,3,1))
  for(mm in c(2,5,8,11)) {
    condlist <- list(mon=mm, hbf=20, year=2012, loglen=log(130), wavescale=2)
    vis.gam.exp(mod, view=c("lon","lat"), cond=condlist, plot.type="contour", color="heat", xlab = "Longitude", 
                ylab = "Latitude", too.far=0.06, main = paste0("Month ", mm), labcex=0.8)
    map(database="world2", add=TRUE, fill=TRUE)
  }
  savePlot(paste0(direc, "/", modn, "_maps1.png"), type = "png")
  
  pac_map <- map_data("world2")
  pac_map <- pac_map[pac_map$long > min(drggf_r$lon-1) & pac_map$long < max(drggf_r$lon+1) & 
                       pac_map$lat > min(drggf_r$lat-1) & pac_map$lat < max(drggf_r$lat+1),]
  
  o <- getViz(mod)
  oo <- list()
  windows(20,20)
  for(mm in c(2,5,8,11)) {
    oo[[mm]] <- plot(sm(o,3), fix=c("mon"=mm), too.far=c(0.06, .5)) + l_fitRaster(noiseup = TRUE, mul = 3) +
      l_fitContour(linetype = 2) + l_points(shape =  2) + 
      geom_polygon(aes(x = long, y = lat, z = 1, group = group), data = pac_map, fill = 'slategray4') 
  }
  gridPrint(oo[[2]], oo[[5]], oo[[8]], oo[[11]])
  savePlot(paste0(direc, "/", modn, "_maps_gg.png"), type = "png")
}

plot_hbf <- function(mod, dat, direc, modn) {
  new_data <- with(dat,
                   expand.grid(hbf=seq(min(hbf), max(hbf), length=100),
                               loglen=log(130),
                               logwt=median(logwt),
                               obs_name=factor(obs_name[1],levels=levels(obs_name)),
                               mon=median(mon),
                               lat = median(lat),
                               lon = median(lon),
                               lightstick = lightstick[1],
                               weather = weather[1],
                               HkCourse = HkCourse[1],
                               wavescale=factor(wavescale[1], levels=levels(wavescale)),
                               yrf = yrf[1],
                               year = median(year)))
  excl <- c("s(obs_name)", "te(lat,lon,mon)", "wavescale", "year", "yrf")
  pred  <- predict(mod , new_data, exclude = excl,
                   type = "link", se.fit = TRUE, unconditional = TRUE)
  pred  <- cbind(pred, new_data)
  pred  <- transform(pred, lwr_ci = exp(fit - (2 * se.fit)),
                     upr_ci = exp(fit + (2 * se.fit)),
                     fitted = exp(fit))
  
  windows()
  g <- ggplot(pred, aes(x = hbf , y = fitted)) +
    geom_ribbon(aes(ymin = lwr_ci, ymax = upr_ci), alpha = 0.2) +
    geom_line(colour = "black", lwd = 1.1) +
    theme_classic() + 
    xlab("hbf") + 
    ylab("Weight (kg)") 
  print(g)
  
  savePlot(paste0(direc, "/", modn, "_hbf.png"), type = "png")
}

plot_year <- function(mod, dat, direc, modn) {
  new_data <- with(dat,
                   expand.grid(hbf = median(hbf),
                               loglen=log(130),
                               logwt=median(logwt),
                               obs_name=factor(obs_name[1],levels=levels(obs_name)),
                               mon=median(mon),
                               lat = median(lat),
                               lon = median(lon),
                               wavescale=factor(levels(wavescale)[1], levels=levels(wavescale)),
                               lightstick = lightstick[1],
                               weather = weather[1],
                               yrf = yrf[1],
                               HkCourse = HkCourse[1],
                               year = seq(min(year), max(year), length=100)))
  excl <- c("s(obs_name)", "te(lat,lon,mon)", "s(hbf)", "wavescale")
  pred  <- predict(mod , new_data, exclude = excl,
                   type = "link", se.fit = TRUE, unconditional = TRUE)
  pred  <- cbind(pred, new_data)
  pred  <- transform(pred, lwr_ci = exp(fit - (2 * se.fit)),
                     upr_ci = exp(fit + (2 * se.fit)),
                     fitted = exp(fit))
  
  windows()
  g <- ggplot(pred, aes(x = year, y = fitted)) +
    geom_ribbon(aes(ymin = lwr_ci, ymax = upr_ci), alpha = 0.2) +
    geom_line(colour = "black", lwd = 1.1) +
    theme_classic() + 
    xlab("Year") + 
    ylab("Weight (kg)") 
  print(g)
  
  savePlot(paste0(direc, "/", modn, "_year.png"), type = "png")
}

plot_yrf <- function(mod, dat, direc, modn) {
  new_data <- with(dat,
                   expand.grid(hbf=median(hbf),
                               loglen=log(130),
                               logwt=median(logwt),
                               obs_name=factor(obs_name[1],levels=levels(obs_name)),
                               mon=median(mon),
                               lat = median(lat),
                               lon = median(lon),
                               lightstick = lightstick[1],
                               weather = weather[1],
                               HkCourse = HkCourse[1],
                               yrf = sort(unique(yrf)),
                               wavescale=factor(wavescale[1], levels=levels(wavescale)),
                               year = median(year)))
  excl <- c("s(obs_name)", "te(lat,lon,mon)", "s(hbf)", "wavescale")
  pred  <- predict(mod, new_data, exclude = excl,
                   type = "link", se.fit = TRUE, unconditional = TRUE)
  pred  <- cbind(pred, new_data)
  pred  <- transform(pred, lwr_ci = exp(fit - (2 * se.fit)),
                     upr_ci = exp(fit + (2 * se.fit)),
                     fitted = exp(fit))
  
  windows()
  g <- ggplot(pred, aes(x = yrf, y = fitted)) +
    geom_point() +
    geom_errorbar(aes(ymin = lwr_ci, ymax = upr_ci), width = 0.2) +
    labs(x = "Year", y = "Weight (kg)")
  print(g)
  
  savePlot(paste0(direc, "/", modn, "_yrf.png"), type = "png")
}

plot_lightstick <- function(mod, dat, direc, modn) {
  new_data <- with(dat,
                   expand.grid(hbf = median(hbf),
                               loglen=log(130),
                               logwt=median(logwt),
                               obs_name=factor(obs_name[1],levels=levels(obs_name)),
                               mon=median(mon),
                               lat = median(lat),
                               lon = median(lon),
                               lightstick = factor(levels(lightstick), levels=levels(lightstick)),
                               weather = factor(weather[1], levels=levels(weather)),
                               HkCourse = HkCourse[1],
                               yrf = yrf[1],
                               wavescale=factor(wavescale[1], levels=levels(wavescale)),
                               year = median(year)))
  excl <- c("s(obs_name)", "te(lat,lon,mon)", "s(hbf)", "s(year)", "yrf")
  pred  <- predict(mod , new_data, exclude = excl,
                   type = "link", se.fit = TRUE, unconditional = TRUE)
  pred  <- cbind(pred, new_data)
  pred  <- transform(pred, lwr_ci = exp(fit - (1.96 * se.fit)),
                     upr_ci = exp(fit + (1.96 * se.fit)),
                     fitted = exp(fit))
  
  windows()
  g <- print(ggplot(pred, aes(x = lightstick, y = fitted)) +
               geom_point() +
               geom_errorbar(aes(ymin = lwr_ci, ymax = upr_ci), width = 0.2) +
               labs(x = "Light stick", y = "Weight (kg)"))
  print(g)
  
  savePlot(paste0(direc, "/", modn, "_lightstick.png"), type = "png")
}

plot_weather <- function(mod, dat, direc, modn) {
  new_data <- with(dat,
                   expand.grid(hbf = median(hbf),
                               loglen=log(130),
                               logwt=median(logwt),
                               obs_name=factor(obs_name[1],levels=levels(obs_name)),
                               mon=median(mon),
                               lat = median(lat),
                               lon = median(lon),
                               lightstick = lightstick[1],
                               weather = factor(levels(weather), levels=levels(weather)),
                               HkCourse = HkCourse[1],
                               yrf = yrf[1],
                               wavescale=factor(wavescale[1], levels=levels(wavescale)),
                               year = median(year)))
  excl <- c("s(obs_name)", "te(lat,lon,mon)", "s(hbf)", "s(year)", "yrf")
  pred  <- predict(mod , new_data, exclude = excl,
                   type = "link", se.fit = TRUE, unconditional = TRUE)
  pred  <- cbind(pred, new_data)
  pred  <- transform(pred, lwr_ci = exp(fit - (1.96 * se.fit)),
                     upr_ci = exp(fit + (1.96 * se.fit)),
                     fitted = exp(fit))
  
  windows()
  g <- print(ggplot(pred, aes(x = weather, y = fitted)) +
               geom_point() +
               geom_errorbar(aes(ymin = lwr_ci, ymax = upr_ci), width = 0.2) +
               labs(x = "Weather", y = "Weight (kg)"))
  print(g)
  
  savePlot(paste0(direc, "/", modn, "_weather.png"), type = "png")
}

plot_wv <- function(mod, dat, direc, modn) {
  new_data <- with(dat,
                   expand.grid(hbf = median(hbf),
                               loglen=log(130),
                               logwt=median(logwt),
                               obs_name=factor(obs_name[1],levels=levels(obs_name)),
                               mon=median(mon),
                               lat = median(lat),
                               lon = median(lon),
                               lightstick = lightstick[1],
                               weather = weather[1],
                               HkCourse = HkCourse[1],
                               yrf = yrf[1],
                               wavescale=factor(levels(wavescale), levels=levels(wavescale)),
                               year = median(year)))
  excl <- c("s(obs_name)", "te(lat,lon,mon)", "s(hbf)", "s(year)", "yrf")
  pred  <- predict(mod , new_data, exclude = excl,
                   type = "link", se.fit = TRUE, unconditional = TRUE)
  pred  <- cbind(pred, new_data)
  pred  <- transform(pred, lwr_ci = exp(fit - (1.96 * se.fit)),
                     upr_ci = exp(fit + (1.96 * se.fit)),
                     fitted = exp(fit))
  
  windows()
  g <- print(ggplot(pred, aes(x = wavescale, y = fitted)) +
               geom_point() +
               geom_errorbar(aes(ymin = lwr_ci, ymax = upr_ci), width = 0.2) +
               labs(x = "Wavescale", y = "Weight (kg)"))
  print(g)
  
  savePlot(paste0(direc, "/", modn, "_wavescale.png"), type = "png")
}

plot_obs <- function(mod, dat, direc, modn) {
  new_data <- with(dat,
                   expand.grid(hbf = median(hbf),
                               loglen=log(130),
                               logwt=median(logwt),
                               obs_name=factor((obs_name)),
                               mon=median(mon),
                               lat = median(lat),
                               lon = median(lon),
                               lightstick = lightstick[1],
                               weather = weather[1],
                               HkCourse = HkCourse[1],
                               yrf = yrf[1],
                               wavescale=factor(levels(wavescale)[1], levels=levels(wavescale)),
                               year = median(year)))
  excl <- c("te(lat,lon,mon)", "s(hbf)", "s(year)", "wavescale", "yrf")
  pred  <- predict(mod, new_data, exclude = excl,
                   type = "link", se.fit = TRUE, unconditional = TRUE)
  pred  <- cbind(pred, new_data)
  pred  <- transform(pred, lwr_ci = exp(fit - (1.96 * se.fit)),
                     upr_ci = exp(fit + (1.96 * se.fit)),
                     fitted = exp(fit))
  windows()
  g <- ggplot(pred, aes(x = obs_name, y = fitted)) +
    geom_point() +
    geom_errorbar(aes(ymin = lwr_ci, ymax = upr_ci), width = 0.2) +
    labs(x = "Observer", y = "Weight (kg)")
  print(g)
  savePlot(paste0(direc, "/", modn, "_observers.png"), type = "png")
  
  b <- getViz(mod)
  windows()
  g <- plot(b, select=2)
  print(g)
  savePlot(paste0(direc, "/", modn, "_observers_qq.png"), type = "png")
}


plot_loglen_vslinear <- function(mod, modlin, dat, direc, modn) {
  new_data <- with(dat,
                   expand.grid(hbf=median(hbf),
                               loglen=seq(min(loglen), max(loglen), length.out=200),
                               logwt=median(logwt),
                               obs_name=factor(obs_name[1],levels=levels(obs_name)),
                               mon=median(mon),
                               lat = median(lat),
                               lon = median(lon),
                               lightstick = lightstick[1],
                               weather = weather[1],
                               HkCourse = HkCourse[1],
                               yrf = yrf[1],
                               wavescale=factor(wavescale[1], levels=levels(wavescale)),
                               year = median(year)))
  # browser()
  excl <- c("s(obs_name)", "te(lat,lon,mon)", "s(hbf)", "wavescale", "yrf")
  pred  <- predict(mod, new_data, exclude = excl,
                   type = "link", se.fit = TRUE, unconditional = TRUE)
  pred.t  <- predict(mod, new_data, exclude = excl,
                   type = "terms", se.fit = TRUE, unconditional = TRUE)
  pred  <- cbind(pred, new_data)
  pred  <- transform(pred, lwr_ci = (fit - (2 * pred.t$se.fit[,2])),
                     upr_ci = (fit + (2 * pred.t$se.fit[,2])),
                     fitted = (fit))
  pred$len <- exp(pred$loglen)
  pred$wt <- exp(pred$fitted)
  pred$up_wt <- exp(pred$upr_ci)
  pred$lw_wt <- exp(pred$lwr_ci)
  predlin  <- predict(modlin, new_data, exclude = excl,
                      type = "link", se.fit = TRUE, unconditional = TRUE)
  
  predlin  <- cbind(predlin, new_data)
  predlin  <- transform(predlin, lwr_ci = (fit - (2 * se.fit)),
                        upr_ci = (fit + (2 * se.fit)),
                        fitted = (fit))
  predlin$len <- exp(predlin$loglen)
  predlin$wt <- exp(predlin$fitted)
  predlin$up_wt <- exp(predlin$upr_ci)
  predlin$lw_wt <- exp(predlin$lwr_ci)
  
  windows(6,6)
  plot(pred$loglen, pred$fitted, xlab = "loglen", ylab = "logwt", type = "l")
  lines(pred$loglen, pred$upr_ci, lty=2)
  lines(pred$loglen, pred$lwr_ci, lty=2)
  lines(predlin$loglen, predlin$fitted, col=2)
  savePlot(paste0(direc, "/", modn, "__loglen1.png"), type = "png")
  savePlot(paste0(direc, "/", modn, "__loglen1.pdf"), type = "pdf")
  
  plot(pred$len, pred$wt, xlab = "Length (cm)", ylab = "logwt", type = "l")
  lines(pred$len, exp(pred$upr_ci), lty=2)
  lines(pred$len, exp(pred$lwr_ci), lty=2)
  lines(predlin$len, exp(predlin$fitted), col=2)
  savePlot(paste0(direc, "/", modn, "__len1.png"), type = "png")
  savePlot(paste0(direc, "/", modn, "__len1.pdf"), type = "pdf")
  
  yl <- range(c(pred$upr_ci - predlin$fitted, pred$lwr_ci - predlin$fitted))
  plot(pred$loglen, pred$fitted - predlin$fitted, xlab = "loglen", ylab = "logwt difference", type = "l", ylim = yl)
  lines(pred$loglen, pred$upr_ci - predlin$fitted, lty=2)
  lines(pred$loglen, pred$lwr_ci - predlin$fitted, lty=2)
  abline(h=0, col=2, lty=2)
  savePlot(paste0(direc, "/", modn, "__loglen2.png"), type = "png")
  savePlot(paste0(direc, "/", modn, "__loglen2.pdf"), type = "pdf")
  
  yl <- range(c(pred$up_wt - predlin$wt, pred$lw_wt - predlin$wt))
  plot(pred$len, pred$wt - predlin$wt, xlab = "Length (cm)", ylab = "Wt difference (kg)", type = "l", ylim = yl)
  lines(pred$len, pred$up_wt - predlin$wt, lty=2)
  lines(pred$len, pred$lw_wt - predlin$wt, lty=2)
  abline(h=0, col=2, lty=2)
  savePlot(paste0(direc, "/", modn, "__len2.png"), type = "png")
  savePlot(paste0(direc, "/", modn, "__len2.pdf"), type = "pdf")
}

plot_M_v_F_V_joint <- function(modF, modM, modJ, datF, datM, direc, modn) {
  new_data <- with(datF,
                   expand.grid(hbf=median(hbf),
                               loglen=seq(min(loglen), max(loglen), length.out=200),
                               logwt=median(logwt),
                               obs_name=factor(obs_name[1],levels=levels(obs_name)),
                               mon=median(mon),
                               lat = median(lat),
                               lon = median(lon),
                            #   lightstick = lightstick[1],
                            #   weather = weather[1],
                            #   HkCourse = HkCourse[1],
                               yrf = yrf[1],
                            #   wavescale=factor(wavescale[1], levels=levels(wavescale)),
                               year = median(year)))
  #excl <- c("s(obs_name)", "te(lat,lon,mon)", "s(hbf)", "wavescale", "yrf")
  excl <- c("s(obs_name)", "te(lon,lat,mon)", "s(hbf)", "yrf")
  pred  <- predict(modF, new_data, exclude = excl,
                   type = "link", se.fit = TRUE, unconditional = TRUE)
  pred.t  <- predict(modF, new_data, exclude = excl,
                   type = "terms", se.fit = TRUE, unconditional = TRUE)
  pred  <- cbind(pred, new_data)
  predF <- transform(pred, lwr_ci = (fit - (2 * pred.t$se.fit[, "s(loglen)"])),
                     upr_ci = (fit + (2 * pred.t$se.fit[, "s(loglen)"])),
                     fitted = (fit),
                     len = exp(pred$loglen),
                     wt = exp(pred$fit),
                     up_wt = exp(fit + (2 * pred.t$se.fit[, "s(loglen)"])),
                     lw_wt = exp(fit - (2 * pred.t$se.fit[, "s(loglen)"])))
  
  pred  <- predict(modM, new_data, exclude = excl,
                   type = "link", se.fit = TRUE, unconditional = TRUE)
  pred.t  <- predict(modM, new_data, exclude = excl,
                     type = "terms", se.fit = TRUE, unconditional = TRUE)
  pred  <- cbind(pred, new_data)
  predM <- transform(pred, lwr_ci = (fit - (2 * pred.t$se.fit[, "s(loglen)"])),
                     upr_ci = (fit + (2 * pred.t$se.fit[, "s(loglen)"])),
                     fitted = (fit),
                     len = exp(pred$loglen),
                     wt = exp(pred$fit),
                     up_wt = exp(fit + (2 * pred.t$se.fit[, "s(loglen)"])),
                     lw_wt = exp(fit - (2 * pred.t$se.fit[, "s(loglen)"])))
  
  pred  <- predict(modJ, new_data, exclude = excl,
                   type = "link", se.fit = TRUE, unconditional = TRUE)
  pred.t  <- predict(modJ, new_data, exclude = excl,
                     type = "terms", se.fit = TRUE, unconditional = TRUE)
  pred  <- cbind(pred, new_data)
  predJ <- transform(pred, lwr_ci = (fit - (2 * pred.t$se.fit[, "s(loglen)"])),
                     upr_ci = (fit + (2 * pred.t$se.fit[, "s(loglen)"])),
                     fitted = (fit),
                     len = exp(pred$loglen),
                     wt = exp(pred$fit),
                     up_wt = exp(fit + (2 * pred.t$se.fit[, "s(loglen)"])),
                     lw_wt = exp(fit - (2 * pred.t$se.fit[, "s(loglen)"])))
  
  windows(6,6)
  plot(predJ$loglen, predJ$fitted, xlab = "loglen", ylab = "logwt", type = "l")
  lines(predJ$loglen, predJ$upr_ci, lty=1)
  lines(predJ$loglen, predJ$lwr_ci, lty=1)
  lines(predM$loglen, predF$fitted, col=2)
  lines(predF$loglen, predM$fitted, col=3)
  legend("topleft", legend=c("Joint", "Male", "Female"), lty=c(1,1,1), col = c(1,3,2))
  savePlot(paste0(direc, "/", modn, "_loglen1.png"), type = "png")
  
  plot(predJ$len, predJ$wt, xlab = "Length (cm)", ylab = "Weight (kg)", type = "l")
  lines(predJ$len, exp(predJ$upr_ci), lty=1)
  lines(predJ$len, exp(predJ$lwr_ci), lty=1)
  lines(predM$len, exp(predM$fitted), col=3)
  lines(predF$len, exp(predF$fitted), col=2)
  legend("topleft", legend=c("Joint", "Male", "Female"), lty=c(1,1,1), col = c(1,3,2))
  savePlot(paste0(direc, "/", modn, "_len1.png"), type = "png")
  
  yl <- range(c(predF$upr_ci - predJ$fitted, predF$lwr_ci - predJ$fitted, predM$upr_ci - predJ$fitted, predM$lwr_ci - predJ$fitted))
  plot(predJ$loglen, predM$fitted - predJ$fitted, xlab = "loglen", ylab = "logwt difference", type = "l", ylim = yl)
  lines(predJ$loglen, predM$upr_ci - predJ$fitted, lty=2)
  lines(predJ$loglen, predM$lwr_ci - predJ$fitted, lty=2)
  lines(predJ$loglen, predF$fitted - predJ$fitted, lty=1, col = 2)
  lines(predJ$loglen, predF$upr_ci - predJ$fitted, lty=2, col = 2)
  lines(predJ$loglen, predF$lwr_ci - predJ$fitted, lty=2, col = 2)
  abline(h=0, col=3, lty=2)
  legend("topright", legend=c("Female vs joint", "Male vs joint"), lty=1, col = c(2,1))
  savePlot(paste0(direc, "/", modn, "_loglen2.png"), type = "png")
  
  yl2 <- range(c(predF$up_wt - predJ$wt, predM$up_wt - predJ$wt, predF$lw_wt - predJ$wt, predM$lw_wt - predJ$wt))
  plot( predJ$len, predM$wt - predJ$wt, xlab = "Length (cm)", ylab = "Wt difference (kg)", type = "l", ylim = yl2)
  lines(predJ$len, predM$up_wt - predJ$wt, lty=2)
  lines(predJ$len, predM$lw_wt - predJ$wt, lty=2)
  lines(predJ$len, predF$wt - predJ$wt, lty = 1, col = 2)
  lines(predJ$len, predF$up_wt - predJ$wt, lty=2, col = 2)
  lines(predJ$len, predF$lw_wt - predJ$wt, lty=2, col = 2)
  abline(h=0, col=3, lty=2)
  legend("topleft", legend=c("Female vs joint", "Male vs joint"), lty=1, col = c(2,1))
  savePlot(paste0(direc, "/", modn, "_len2.pdf"), type = "pdf")
}

#' Quantify nonlinearity: Compare GAM vs Linear models
#' 
#' @param mod_gam GAM model with smooth for length
#' @param mod_linear Model with linear length term
#' @param data Dataset used for fitting
#' @return List with comparison metrics
#' Quantify nonlinearity: Compare GAM vs Linear models
#' 
#' @param mod_gam GAM model with smooth for length
#' @param mod_linear Model with linear length term
#' @param data Dataset used for fitting
#' @return List with comparison metrics
quantify_nonlinearity <- function(mod_gam, mod_linear, data) {
  
  # 1. Model fit comparison
  aic_diff <- AIC(mod_linear) - AIC(mod_gam)  # Positive = GAM better
  
  # 2. Get predictions for both models on the LINK scale
  # (for scat family, type="response" can give NaN)
  pred_gam_link <- predict(mod_gam, type = "link")
  pred_linear_link <- predict(mod_linear, type = "link")
  
  # Back-transform to weight scale
  pred_gam <- exp(pred_gam_link)
  pred_linear <- exp(pred_linear_link)
  
  # Get observed weights (back-transform from logwt if needed)
  if("logwt" %in% names(data)) {
    obs <- exp(data$logwt)
  } else if("weight" %in% names(data)) {
    obs <- data$weight
  } else {
    stop("Cannot find weight column (tried 'logwt' and 'weight')")
  }
  
  # 3. Calculate RMSE and percentage differences
  rmse_gam <- sqrt(mean((obs - pred_gam)^2, na.rm = TRUE))
  rmse_linear <- sqrt(mean((obs - pred_linear)^2, na.rm = TRUE))
  rmse_improvement <- (rmse_linear - rmse_gam) / rmse_linear * 100
  
  # 4. Mean absolute percentage error
  mape_gam <- mean(abs((obs - pred_gam) / obs), na.rm = TRUE) * 100
  mape_linear <- mean(abs((obs - pred_linear) / obs), na.rm = TRUE) * 100
  
  # 5. Maximum deviation across length range
  diff_weights <- pred_gam - pred_linear
  max_abs_diff <- max(abs(diff_weights), na.rm = TRUE)
  max_pct_diff <- max(abs(diff_weights / pred_linear), na.rm = TRUE) * 100
  
  # 6. Effective degrees of freedom for length smooth
  s_table <- summary(mod_gam)$s.table
  loglen_row <- grep("loglen", rownames(s_table), value = TRUE)[1]
  
  if(!is.na(loglen_row)) {
    length_smooth_edf <- s_table[loglen_row, "edf"]
    length_smooth_pval <- s_table[loglen_row, "p-value"]
  } else {
    length_smooth_edf <- NA
    length_smooth_pval <- NA
  }
  
  # 7. Likelihood ratio test
  lr_stat <- -2 * (logLik(mod_linear)[1] - logLik(mod_gam)[1])
  df_diff <- attr(logLik(mod_gam), "df") - attr(logLik(mod_linear), "df")
  lr_pval <- pchisq(lr_stat, df = df_diff, lower.tail = FALSE)
  
  # Compile results
  results <- list(
    model_comparison = data.frame(
      Metric = c("AIC", "ΔAIC (Linear - GAM)", "log-likelihood", "Δ log-likelihood"),
      GAM = c(AIC(mod_gam), NA, logLik(mod_gam)[1], NA),
      Linear = c(AIC(mod_linear), aic_diff, logLik(mod_linear)[1], 
                 logLik(mod_gam)[1] - logLik(mod_linear)[1])
    ),
    
    prediction_metrics = data.frame(
      Metric = c("RMSE (kg)", "RMSE improvement (%)", 
                 "MAPE (%)", "Max absolute diff (kg)", "Max % diff"),
      GAM = c(rmse_gam, rmse_improvement, mape_gam, NA, NA),
      Linear = c(rmse_linear, NA, mape_linear, NA, NA),
      Difference = c(rmse_linear - rmse_gam, NA, 
                     mape_linear - mape_gam, max_abs_diff, max_pct_diff)
    ),
    
    smooth_diagnostics = data.frame(
      Term = loglen_row,
      EDF = length_smooth_edf,
      `P-value` = length_smooth_pval,
      Significant = ifelse(is.na(length_smooth_pval), "NA",
                           ifelse(length_smooth_pval < 0.001, "***", 
                                  ifelse(length_smooth_pval < 0.01, "**",
                                         ifelse(length_smooth_pval < 0.05, "*", "ns"))))
    ),
    
    likelihood_ratio = data.frame(
      Statistic = lr_stat,
      DF = df_diff,
      `P-value` = lr_pval
    ),
    
    # Add sample sizes for reference
    sample_info = data.frame(
      n_observations = nrow(data),
      length_range = paste(round(range(exp(data$loglen)), 1), collapse = "-"),
      weight_range = paste(round(range(obs, na.rm=TRUE), 1), collapse = "-")
    )
  )
  
  return(results)
}

#' Calculate mean differences between GAM and linear predictions by length
#' 
#' @param mod_gam GAM model
#' @param mod_linear Linear model
#' @param data Model data
#' @param bin_width Width of length bins in cm (default = 5)
#' @return Data frame with mean differences by length
calculate_length_differences <- function(mod_gam, mod_linear, data, 
                                         bin_width = 5) {
  
  # Get predictions on link scale
  pred_gam_link <- predict(mod_gam, type = "link")
  pred_linear_link <- predict(mod_linear, type = "link")
  
  # Back-transform to weight scale
  pred_gam <- exp(pred_gam_link)
  pred_linear <- exp(pred_linear_link)
  
  # Calculate differences
  diff_abs <- pred_gam - pred_linear
  diff_pct <- (pred_gam - pred_linear) / pred_linear * 100
  
  # Get lengths in cm
  length_cm <- exp(data$loglen)
  obs_weight <- exp(data$logwt)
  
  # Create length bins
  length_bins <- cut(length_cm, 
                     breaks = seq(floor(min(length_cm)), 
                                  ceiling(max(length_cm)), 
                                  by = bin_width),
                     include.lowest = TRUE)
  
  # Create data frame
  df <- data.frame(
    length_cm = length_cm,
    length_bin = length_bins,
    obs_weight = obs_weight,
    pred_gam = pred_gam,
    pred_linear = pred_linear,
    diff_abs = diff_abs,
    diff_pct = diff_pct
  )
  
  # Calculate summary statistics by length bin
  library(dplyr)
  
  summary_by_bin <- df %>%
    group_by(length_bin) %>%
    summarise(
      n = n(),
      mean_length = mean(length_cm),
      median_length = median(length_cm),
      mean_obs_weight = mean(obs_weight),
      mean_pred_gam = mean(pred_gam),
      mean_pred_linear = mean(pred_linear),
      mean_diff_abs = mean(diff_abs),
      sd_diff_abs = sd(diff_abs),
      mean_diff_pct = mean(diff_pct),
      sd_diff_pct = sd(diff_pct),
      max_diff_abs = max(abs(diff_abs)),
      max_diff_pct = max(abs(diff_pct)),
      min_diff_abs = min(diff_abs),
      max_diff_abs_pos = max(diff_abs)
    ) %>%
    arrange(mean_length)
  
  return(list(
    by_bin = summary_by_bin,
    raw_data = df
  ))
}

