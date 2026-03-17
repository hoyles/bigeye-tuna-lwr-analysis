# comprehensive_plot.R
# Modular functions for creating comprehensive visualization

# 1. Create diagnostic plots (top-left quadrant)
create_diagnostic_plots <- function(mod, dir, width, height) {
  png(paste0(dir, "/diags.png"), width = width, height = height, res = 600)
  par(mfrow = c(2, 2), 
      mar = c(5, 5, 4, 2), 
      cex.lab = 1.5,    
      cex.axis = 1.4,   
      cex.main = 1.4)   
  
  # Use standard gam.check with base R plotting
  gam.check(mod)
  dev.off()
  
  return(paste0(dir, "/diags.png"))
}

# 2. Create contour maps (bottom-left quadrant) with observation locations
# Simplified version that directly handles predictions and too.far
create_contour_maps <- function(mod, dat, dir, width, height) {
  # Set too.far parameter
  too_far_val <- 0.06  # Same value as in original code
  
  png(paste0(dir, "/maps1.png"), width = width, height = height, res = 600)
  
  # Set up 2x2 grid with appropriate margins
  par(mfrow=c(2,2), 
      mar = c(5, 5, 4, 2), 
      cex.lab = 1.5,    
      cex.axis = 1.4,   
      cex.main = 1.4)   
  
  # Loop through months for separate contour plots
  for(mm in c(2,5,8,11)) {
    # Extract observations for this month AND adjacent months (±1)
    # Handle edge cases for months 1 and 12
    month_range <- c(mm-1, mm, mm+1)
    month_range <- month_range[month_range >= 1 & month_range <= 12]
    
    # Get data for this month and adjacent months
    month_data <- dat[dat$mon %in% month_range, ]
    
    # The simplest approach: Just use vis.gam.exp directly
    condlist <- list(mon=mm, hbf=20, year=2020, loglen=log(130), wavescale=2)
    
    if ("obs_name" %in% names(dat)) {
      condlist$obs_name <- dat$obs_name[1]
    }
    if ("lightstick" %in% names(dat)) {
      condlist$lightstick <- dat$lightstick[1]
    }
    if ("weather" %in% names(dat)) {
      condlist$weather <- dat$weather[1]
    }
    if ("HkCourse" %in% names(dat)) {
      condlist$HkCourse <- dat$HkCourse[1]
    }
    if ("yrf" %in% names(dat)) {
      condlist$yrf <- dat$yrf[1]
    }
    
    # Call vis.gam.exp directly to create the plot
    vis.gam.exp(mod, view=c("lon","lat"), cond=condlist, 
                plot.type="contour", color="revheat", 
                xlab="Longitude", ylab="Latitude", 
                too.far=too_far_val, main=paste0("Month ", mm))
    
    # Add map
    map(database="world2", add=TRUE, fill=TRUE)
    
    # Add observation points for adjacent months
    if(nrow(month_data) > 0) {
      # Use different point symbols for different months
      for(m in month_range) {
        m_data <- month_data[month_data$mon == m, ]
        if(nrow(m_data) > 0) {
          # Use filled circle for exact month, open circle for adjacent months
          if(m == mm) {
            points(m_data$lon, m_data$lat, pch = 21, 
                   col = "black", bg = "white", cex = 0.7)
          } else {
            # Use slightly smaller points for adjacent months
            points(m_data$lon, m_data$lat, pch = 1, 
                   col = "gray20", cex = 0.6)
          }
        }
      }
    }
  }
  dev.off()
  
  return(paste0(dir, "/maps1.png"))
}

# 3. Create HBF effect plot
create_hbf_plot <- function(mod, dat, dir, width, height) {
  # Make sure ggplot2 is loaded
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    install.packages("ggplot2")
  }
  library(ggplot2)
  
  png(paste0(dir, "/hbf.png"), width = width, height = height, res = 600)
  
  # Standard ggplot theme for consistency
  standard_theme <- theme_classic() +
    theme(
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14),
      plot.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16),
      plot.margin = ggplot2::margin(10, 20, 10, 10, "pt") # Use ggplot2::margin explicitly
    )
  
  new_data_hbf <- with(dat,
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
  pred_hbf <- predict(mod, new_data_hbf, exclude = excl,
                      type = "link", se.fit = TRUE, unconditional = TRUE)
  pred_hbf <- cbind(pred_hbf, new_data_hbf)
  pred_hbf <- transform(pred_hbf, lwr_ci = exp(fit - (2 * se.fit)),
                        upr_ci = exp(fit + (2 * se.fit)),
                        fitted = exp(fit))
  
  p_hbf <- ggplot(pred_hbf, aes(x = hbf, y = fitted)) +
    geom_ribbon(aes(ymin = lwr_ci, ymax = upr_ci), alpha = 0.2) +
    geom_line(colour = "black", lwd = 1.1) +
    standard_theme +
    labs(x = "HBF", y = "Weight (kg)", title = "HBF Effect")
  print(p_hbf)
  dev.off()
  
  return(paste0(dir, "/hbf.png"))
}

# 4a. Create Year effect plot (for continuous year term)
create_year_plot <- function(mod, dat, dir, width, height) {
  library(ggplot2)
  
  png(paste0(dir, "/year.png"), width = width, height = height, res = 600)
  
  # Standard ggplot theme for consistency
  standard_theme <- theme_classic() +
    theme(
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14),
      plot.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16),
      plot.margin = ggplot2::margin(10, 20, 10, 10, "pt") # Use ggplot2::margin explicitly
    )
  
  new_data_year <- with(dat,
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
  pred_year <- predict(mod, new_data_year, exclude = excl,
                       type = "link", se.fit = TRUE, unconditional = TRUE)
  pred_year <- cbind(pred_year, new_data_year)
  pred_year <- transform(pred_year, lwr_ci = exp(fit - (2 * se.fit)),
                         upr_ci = exp(fit + (2 * se.fit)),
                         fitted = exp(fit))
  
  p_year <- ggplot(pred_year, aes(x = year, y = fitted)) +
    geom_ribbon(aes(ymin = lwr_ci, ymax = upr_ci), alpha = 0.2) +
    geom_line(colour = "black", lwd = 1.1) +
    standard_theme +
    labs(x = "Year", y = "Weight (kg)", title = "Year Effect")
  print(p_year)
  dev.off()
  
  return(paste0(dir, "/year.png"))
}

# 4b. Create Year-as-factor effect plot - simplified approach
create_yrf_plot <- function(mod, dat, dir, width, height) {
  library(ggplot2)
  
  png(paste0(dir, "/year.png"), width = width, height = height, res = 600)
  
  # Standard ggplot theme for consistency
  standard_theme <- theme_classic() +
    theme(
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14),
      plot.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16),
      plot.margin = ggplot2::margin(10, 20, 10, 10, "pt")
    )
  
  # Get all unique years from the data
  years <- sort(unique(as.numeric(as.character(dat$yrf))))
  n_years <- length(years)
  
  # Create a more reliable prediction approach
  # Start with a single reference row
  ref_row <- data.frame(
    hbf = median(dat$hbf),
    loglen = log(130),
    logwt = median(dat$logwt),
    obs_name = factor(dat$obs_name[1], levels=levels(dat$obs_name)),
    mon = median(dat$mon),
    lat = median(dat$lat),
    lon = median(dat$lon),
    wavescale = factor(dat$wavescale[1], levels=levels(dat$wavescale)),
    lightstick = dat$lightstick[1],
    weather = dat$weather[1],
    HkCourse = dat$HkCourse[1],
    year = median(dat$year)
  )
  
  # If the model has additional predictors, add them with sensible defaults
  if ("wavenum" %in% names(dat) && "wavenum" %in% names(mod$model)) {
    ref_row$wavenum <- median(dat$wavenum)
  }
  
  # Replicate the reference row for each year and update the yrf factor
  new_data_yrf <- do.call(rbind, replicate(n_years, ref_row, simplify = FALSE))
  new_data_yrf$yrf <- factor(years, levels = levels(dat$yrf))
  
  # Initialize results dataframe
  results <- data.frame(
    year = years,
    fitted = NA,
    lwr_ci = NA,
    upr_ci = NA
  )
  
  # Exclude irrelevant terms from prediction
  excl <- c("s(obs_name)", "te(lat,lon,mon)", "s(hbf)", "wavescale", "s(year)")
  
  # Loop through each year and predict individually to avoid expansion grid issues
  for (i in 1:n_years) {
    # Create a single row for this year
    pred_row <- ref_row
    pred_row$yrf <- factor(years[i], levels = levels(dat$yrf))
    
    # Predict for this year
    pred <- predict(mod, newdata = pred_row, exclude = excl,
                    type = "link", se.fit = TRUE, unconditional = TRUE)
    
    # Store the results
    results$fitted[i] <- exp(pred$fit)
    results$lwr_ci[i] <- exp(pred$fit - 2 * pred$se.fit)
    results$upr_ci[i] <- exp(pred$fit + 2 * pred$se.fit)
  }
  
  # Create the plot with error bars
  p_yrf <- ggplot(results, aes(x = year, y = fitted, group = 1)) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = lwr_ci, ymax = upr_ci), width = 0.2) +
    geom_line() +
    scale_x_continuous(breaks = seq(min(results$year), max(results$year), by = 2)) +
    standard_theme +
    labs(x = "Year", y = "Weight (kg)", title = "Year Effect (Factor)")
  
  print(p_yrf)
  dev.off()
  
  return(paste0(dir, "/year.png"))
}

# 5. Create Length-Weight plot
create_lw_plot <- function(mod, dat, dir, width, height) {
  library(ggplot2)
  
  png(paste0(dir, "/loglen.png"), width = width, height = height, res = 600)
  
  # Standard ggplot theme for consistency
  standard_theme <- theme_classic() +
    theme(
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14),
      plot.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16),
      plot.margin = ggplot2::margin(10, 20, 10, 10, "pt") # Use ggplot2::margin explicitly
    )
  
  # Create custom Length-Weight relationship plot
  new_data_len <- with(dat,
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
  
  # Calculate actual lengths in cm from log lengths
  new_data_len$length_cm <- exp(new_data_len$loglen)
  
  excl <- c("s(obs_name)", "te(lat,lon,mon)", "s(hbf)", "wavescale", "year", "yrf")
  pred_len <- predict(mod, new_data_len, exclude = excl,
                      type = "link", se.fit = TRUE, unconditional = TRUE)
  
  # Create data frame with predictions and convert to real units
  lw_data <- data.frame(
    loglen = new_data_len$loglen,
    length_cm = new_data_len$length_cm,
    fit = pred_len$fit,
    se = pred_len$se.fit
  )
  
  # Add confidence intervals (on log scale)
  lw_data$lwr_ci <- lw_data$fit - 2 * lw_data$se
  lw_data$upr_ci <- lw_data$fit + 2 * lw_data$se
  
  # Calculate weights in kg
  lw_data$weight_kg <- exp(lw_data$fit)
  lw_data$weight_kg_lower <- exp(lw_data$lwr_ci)
  lw_data$weight_kg_upper <- exp(lw_data$upr_ci)
  
  # Create the plot with log scaling and confidence intervals
  p_loglen <- ggplot(lw_data, aes(x = length_cm, y = weight_kg)) +
    geom_ribbon(aes(ymin = weight_kg_lower, ymax = weight_kg_upper), alpha = 0.2) +
    geom_line(colour = "black", lwd = 1.1) +
    scale_x_log10(
      breaks = c(50, 100, 150, 200, 250),
      labels = c("50", "100", "150", "200", "250")
    ) +
    scale_y_log10() +
    standard_theme +
    labs(x = "Length (cm)", y = "Weight (kg)", title = "Length-Weight Relationship")
  
  print(p_loglen)
  dev.off()
  
  return(paste0(dir, "/loglen.png"))
}

# 6. Create Observer effect plot
create_obs_plot <- function(mod, dat, dir, width, height) {
  library(ggplot2)
  
  png(paste0(dir, "/obs.png"), width = width, height = height, res = 600)
  
  # Standard ggplot theme for consistency
  standard_theme <- theme_classic() +
    theme(
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14),
      plot.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16),
      plot.margin = ggplot2::margin(10, 20, 10, 10, "pt") # Use ggplot2::margin explicitly
    )
  
  # Create weight by observer plot
  new_data_obs <- with(dat,
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
  pred_obs <- predict(mod, new_data_obs, exclude = excl,
                      type = "link", se.fit = TRUE, unconditional = TRUE)
  pred_obs <- cbind(pred_obs, new_data_obs)
  pred_obs <- transform(pred_obs, lwr_ci = exp(fit - (1.96 * se.fit)),
                        upr_ci = exp(fit + (1.96 * se.fit)),
                        fitted = exp(fit))
  
  # Create the plot without x-axis labels
  p_obs <- ggplot(pred_obs, aes(x = obs_name, y = fitted)) +
    geom_point() +
    geom_errorbar(aes(ymin = lwr_ci, ymax = upr_ci), width = 0.2) +
    labs(x = "Observer", y = "Weight (kg)", title = "Observer Effects") +
    standard_theme +
    theme(
      axis.text.x = element_blank(),  # Remove the x-axis text completely
      axis.ticks.x = element_blank()  # Remove the x-axis ticks as well
    )
  print(p_obs)
  dev.off()
  
  return(paste0(dir, "/obs.png"))
}

# 7. Completely redesigned function for combining plots
combine_plots <- function(plot_files, direc, modn, width, height) {
  library(magick)
  
  # Read all images
  diags <- image_read(plot_files$diags)
  maps <- image_read(plot_files$maps1)
  hbf <- image_read(plot_files$hbf)
  year <- image_read(plot_files$year)
  loglen <- image_read(plot_files$loglen)
  obs <- image_read(plot_files$obs)
  
  # Layout proportions - give more space to right column
  left_ratio <- 0.65   # left column takes 65% of width (reduced from 70%)
  right_ratio <- 0.35  # right column takes 35% of width (increased from 30%)
  
  # Calculate actual dimensions
  left_width <- floor(width * left_ratio)
  right_width <- floor(width * right_ratio)
  top_height <- floor(height / 2)
  right_panel_height <- floor(height / 4)
  
  # Create a blank canvas
  canvas <- image_blank(width, height, color = "white")
  
  # Resize images with FIXED dimensions to maintain aspect ratios
  diags_resized <- image_resize(diags, geometry = paste0(left_width, "x", top_height, "!"))
  maps_resized <- image_resize(maps, geometry = paste0(left_width, "x", top_height, "!"))
  
  # Resize right column images
  hbf_resized <- image_resize(hbf, geometry = paste0(right_width, "x", right_panel_height, "!"))
  year_resized <- image_resize(year, geometry = paste0(right_width, "x", right_panel_height, "!"))
  loglen_resized <- image_resize(loglen, geometry = paste0(right_width, "x", right_panel_height, "!"))
  obs_resized <- image_resize(obs, geometry = paste0(right_width, "x", right_panel_height, "!"))
  
  # Compose the canvas
  # Top-left: Diagnostic plots
  canvas <- image_composite(canvas, diags_resized, offset = "+0+0")
  
  # Bottom-left: Contour maps
  canvas <- image_composite(canvas, maps_resized, offset = paste0("+0+", top_height))
  
  # Right column: Effect plots
  canvas <- image_composite(canvas, hbf_resized, offset = paste0("+", left_width, "+0"))
  canvas <- image_composite(canvas, year_resized, offset = paste0("+", left_width, "+", right_panel_height))
  canvas <- image_composite(canvas, loglen_resized, offset = paste0("+", left_width, "+", 2*right_panel_height))
  canvas <- image_composite(canvas, obs_resized, offset = paste0("+", left_width, "+", 3*right_panel_height))
  
  # Add dividing lines between sections
  # Horizontal line between top and bottom sections
  h_line <- image_blank(width, 2, color = "black")
  canvas <- image_composite(canvas, h_line, offset = paste0("+0+", top_height))
  
  # Vertical line between left and right columns
  v_line <- image_blank(2, height, color = "black")
  canvas <- image_composite(canvas, v_line, offset = paste0("+", left_width, "+0"))
  
  # Horizontal lines between right column plots
  for (i in 1:3) {
    h_line_right <- image_blank(right_width, 2, color = "black")
    canvas <- image_composite(canvas, h_line_right, offset = paste0("+", left_width, "+", i*right_panel_height))
  }
  
  # Save the final image
  outfile <- list()
  outfile$png <- paste0(direc, "/", modn, "_comprehensive.png")
  image_write(canvas, outfile$png, quality = 100, density=1000)
  outfile$tiff <- paste0(direc, "/", modn, "_comprehensive.tiff")
  image_write(canvas, outfile$tiff, format = "tiff", compression = "lzw", density=1000)
  
  return(outfile)
}

# Helper function to check if the model uses year as a factor or smooth term
uses_year_as_factor <- function(mod) {
  # Check if model terms include "yrf" (year factor)
  terms <- attr(mod$terms, "term.labels")
  return("yrf" %in% terms)
}

# Main function
create_comprehensive_plot <- function(mod, dat, direc, modn) {
  # Create temporary directory for individual plot files
  temp_dir <- paste0(direc, "/temp_", modn)
  dir.create(temp_dir, showWarnings = FALSE)
  
  # Define standard dimensions - make final plot taller than wide
  std_width <- 7500  # increased from 3000 for more width
  std_height <- 8660
  
  # Calculate dimensions for grid plots
  left_width <- std_width * 0.65  # 65% of width
  right_width <- std_width * 0.35 # 35% of width
  top_height <- std_height / 2
  right_panel_height <- std_height / 4
  
  # Create individual plots and store file paths
  plot_files <- list()
  
  # Create each plot component
  plot_files$diags <- create_diagnostic_plots(mod, temp_dir, left_width, top_height)
  plot_files$maps1 <- create_contour_maps(mod, dat, temp_dir, left_width, top_height)
  
  # Create effect plots
  plot_files$hbf <- create_hbf_plot(mod, dat, temp_dir, right_width, right_panel_height)
  
  # Check if model uses year as factor or smooth term
  if (uses_year_as_factor(mod)) {
    plot_files$year <- create_yrf_plot(mod, dat, temp_dir, right_width, right_panel_height)
  } else {
    plot_files$year <- create_year_plot(mod, dat, temp_dir, right_width, right_panel_height)
  }
  
  plot_files$loglen <- create_lw_plot(mod, dat, temp_dir, right_width, right_panel_height)
  plot_files$obs <- create_obs_plot(mod, dat, temp_dir, right_width, right_panel_height)
  
  # Combine plots
  result <- combine_plots(plot_files, direc, modn, std_width, std_height)
  
  # Clean up temp files
  unlink(temp_dir, recursive = TRUE)
  
  return(result)
}