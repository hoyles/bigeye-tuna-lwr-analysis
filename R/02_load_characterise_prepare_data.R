#dat <- read_csv("./data/BET OBS data.csv")
#dat <- read_csv("./data/BET_new.csv")
#dat <- read_csv("./data/BET_20230223.csv")
datfolder <- "../../LW_other/data/"
dat <- read_csv(paste0(datfolder, "fish_new0224.csv"))
dat_0224 <- read_csv(paste0(datfolder, "BET_new0224.csv"))
datold <- read_csv(paste0(datfolder, "BET_new.csv"))
dat_0223<- read_csv(paste0(datfolder, "BET_20230223.csv"))
#dat_0223$workdate <- dmy(dat_0223$workdate)
dat_0223$year <- year(dat_0223$workdate)

length(unique(dat_0223$tripID))
length(unique(datold$tripID))
length(unique(dat$tripID))
sort(unique(dat_0223$tripID))
sort(unique(dat$tripID))

#a <- dat[dat$tripID=="98/09/28",]
# Prepare data -------------------------------------------
#str(dat)
dat$workdate <- ymd(dat$workdate)
dat$year <- year(dat$workdate)
dat$mon <- month(dat$workdate)
dat$qtr <- factor(floor((dat$mon-1)/3) + 1)
dat$logwt <- log(dat$weight)
dat$loglen <- log(dat$length)
dat <- rename(dat, lunar_illumination=liunar_illumination)
dat$region <- as.factor(dat$region)
dat$HkCourse <- as.factor(dat$HkCourse)
dat$yrf <- as.factor(dat$year)
dat$obs_name <- as.factor(dat$obs_name)

dat$modwt5 <- dat$weight %% 5
dat$modlen5 <- dat$length %% 5
dat$modlen5f <- as.factor(dat$modlen5)
dat$wt5f <- as.factor(dat$modwt5)
dat$setid <- factor(paste(dat$tripID, as.character(dat$workdate), sep="_"))
setid2 <- factor(paste(dat$obs_name, as.character(dat$workdate), sep="_"))

dat$weather <- factor(dat$weather)
dat$HkCourse <- factor(dat$HkCourse)
dat$lightstick <- factor(dat$lightstick)
dat$wavescale <- factor(dat$wavescale)
dat$wavenum <- as.numeric(dat$wavescale)

# Data cleaning -------------------------------------------

dat$processtype[dat$year==2019 & dat$processtype=="RGG"] <- "RGT"


# Check data -------------------------------------------
table(is.na(dat$obs_name)) # 150146 good
table(is.na(dat$tripID)) # 0 NA
table(is.na(dat$workdate)) # 0 NA
table(is.na(dat$processtype)) # 1 NA
table(is.na(dat$weight)) # 0 NA
table(is.na(dat$length)) # 0 NA
table(is.na(dat$sex)) # 968 NA
table(dat$year, is.na(dat$sex)) # All but 5 are pre-2015
table(dat$obs_name, is.na(dat$sex), useNA = "ifany") # 
table(is.na(dat$wavescale)) # 56 NA
table(is.na(dat$baitType)) # 1740 NA
table(dat$year, is.na(dat$baitType)) # mostly 2008 & 2009
table(dat$obs_name, is.na(dat$baitType), useNA = "ifany") # All from obs001 (1560) and obs012 (65), 
table(is.na(dat$lightstick)) # 0 NA
table(is.na(dat$hbf)) # 351 NA
table(dat$year, is.na(dat$hbf)) # all in 2012
table(dat$obs_name, is.na(dat$hbf), useNA = "ifany") # 4 observers
table(is.na(dat$region)) # 0 NA
table(is.na(dat$weather)) # 0 NA
table(is.na(dat$HkCourse)) # 658 NA
table(dat$obs_name, is.na(dat$HkCourse), useNA = "ifany") # various observers
table(is.na(dat$lon)) # 0 NA
table(is.na(dat$lat)) # 0 NA
table(is.na(dat$phase)) # 0 NA
table(is.na(dat$lunar_illumination)) # 0 NA
table(is.na(dat$Cluster)) # 0 NA

table(dat$obs_name, useNA = "ifany") # all good
barplot(table(dat$obs_name))
table(table(dat$obs_name))
hist(table(dat$obs_name), nclass=100)

# do any observers do multiple sets at the same time on different trips? No. 
a <- dat
for(obs in sort(unique(a$obs_name))) {
  aa <- a[a$obs_name==obs,]
  for(wd in sort(unique(aa$workdate))) {
    awd <- aa[aa$workdate==wd & !is.na(aa$workdate),]
    trp <- sort(unique(awd$tripID))
    if(length(trp) > 1) { print(paste(obs, wd, trp)); flush.console() }
  }
  print(obs); flush.console()
}
# Same again a different way - do any observers do multiple sets at the same time on different trips? No. 
a <- dat
a <- a[!is.na(a$obs_name),]
a$obs_date_trip <- paste(a$tripID, a$workdate, a$obs_name)
a$obs_date <- paste(a$workdate, a$obs_name)
all_odt <- sort(unique(a$obs_date_trip))
a1 <- a[match(all_odt, a$obs_date_trip),]
table(table(a1$obs_date))
a2 <- table(a1$obs_date)
a2[a2>1]
sort(unique(a$tripID))

a <- dat
a$set_weather <- paste(a$weather, a$setid)
all_sw <- sort(unique(a$set_weather))
a1 <- a[match(all_sw, a$set_weather),]
table(table(a1$setid))
a2 <- table(a1$setid)
a3 <- names(a2[a2>1])
a[a$setid %in% a3[3], c("setid", "weather")] %>% print(n=100)

sort(unique(a$tripID))
barplot(table(dat$weather), main = "Weather")


# Characterise data -------------------------------------------
a <- table(dat$year, dat$obs_name)
a23 <- table(dat_0223$year, dat_0223$obs_name)
tmp <- function(x) sum(x > 0)
apply(a,1,tmp)
apply(a23,1,tmp)
windows(height=10, width=8); par(mfrow=c(3,2))
barplot(table(dat$year[dat$year > 2008]), xlab = "Year", ylab="Samples per year")
barplot(apply(a[rownames(a) != "2008",],1,tmp), xlab = "Year", ylab="Unique observers per year")

a <- table(dat$year, dat$tripID)
tmp <- function(x) sum(x > 0)
apply(a,1,tmp)
barplot(apply(a[rownames(a) != "2008",],1,tmp), xlab = "Year", ylab="Unique trips per year")

a <- table(dat$year, paste(dat$obs_name, dat$workdate))
tmp <- function(x) sum(x > 0)
barplot(apply(a[rownames(a) != "2008",],1,tmp), xlab = "Year", ylab="Unique sets per year")
barplot(table(dat$year) / apply(a,1,tmp), xlab = "Year", ylab="Mean samples per set")
savePlot("../figures3/summary_plots.png", type = "png")
savePlot("../figures_resubmit/fig3_summary_plots.png", type = "png")
savePlot("../figures_resubmit/fig3_summary_plots.pdf", type = "pdf")


windows(); par(mfrow = c(4,4))
for(y in 2009:2022) with(filter(dat, year == y), hist(table(factor(obs_name)), breaks = seq(0, 2500, 50), main = y, xlim = c(0, 2500)))
savePlot("../figures3/fish_per_obs_per_yr.png", type = "png")

table(dat$obs_name, dat$year, useNA = "ifany") 
length(unique(dat$obs_name))
table(dat$year, is.na(dat$obs_name)) # mostly

table(dat$processtype, useNA = "ifany") # 1 NA
table(dat$year, dat$processtype, useNA = "ifany") # 1 NA
table(dat$year, dat$processtype)
windows()
#par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
barplot(table(dat$processtype, dat$year), legend.text=TRUE, col = c(1:5), args.legend=list(x="topleft", title="Process type"), 
        xlab = "Year", ylab = "Number of samples")
savePlot(file = "../figures3/process_types_by_year.png", type = "png")
# savePlot("../figures_resubmit/fig4b_process_types_by_year.png", type = "png")
# savePlot("../figures_resubmit/fig4b_process_types_by_year.pdf", type = "pdf")

table(dat$sex, useNA = "ifany") # 968 NA
table(dat$sex, dat$year, useNA = "ifany") # 968 NA
windows()
#par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
barplot(table(dat$sex, dat$year), legend.text=TRUE, col = c(1:5), args.legend=list(x="topleft", title="Sex"), 
        xlab = "Year", ylab = "Number of samples")
savePlot(file = "../figures3/sex_prop_by_yr.png", type = "png")

windows(height=7, width=14); par(mfrow=c(1,2))
with(dat[dat$year > 2008,], barplot(table(sex, year), legend.text=TRUE, col = c(1:5), args.legend=list(x="topleft", title="Sex"), xlab = "Year", ylab = "Number of samples", ylim=c(0,15000)))
with(dat[dat$year > 2008,], barplot(table(processtype, year), legend.text=TRUE, col = c(1:5), args.legend=list(x="topleft", title="Process type"), xlab = "Year", ylab = "Number of samples", ylim=c(0,15000)))
savePlot(file = "../figures3/Samples_by_sex_process.png", type = "png")
savePlot(file = "../figures_resubmit/fig4_Samples_by_sex_process.png", type = "png")
savePlot(file = "../figures_resubmit/fig4_Samples_by_sex_process.pdf", type = "pdf")


table(dat$wavescale, useNA = "ifany") # 56 NA
table(dat$wavescale, dat$year, useNA = "ifany") # 56 NA

windows(10,10); par(mfrow=c(2,2))
par(mar=c(3.1, 4.1, 4.1, 4.5), xpd=TRUE)
a <- with(dat[dat$year > 2008,],table(weather, year))
a <- t(a)/apply(a,2,sum)
barplot(t(a), legend.text=TRUE, col = c(1:5), args.legend=list(x="topright", title="Weather", inset = c(-0.2, 0)), 
        xlab = "", ylab = "Proportion of samples", cex.lab=1.3)
#savePlot(file = "../figures3/weather_prop_by_yr.png", type = "png")

par(mar=c(3.1, 3.1, 4.1, 5.1), xpd=TRUE)
a <- with(dat[dat$year > 2008,],table(wavescale, year))
a <- t(a)/apply(a,2,sum)
barplot(t(a), legend.text=TRUE, col = c(1:5), args.legend=list(x="topright", title="Wave scale", inset = c(-0.2, 0)), 
        xlab = "", ylab = "", cex.lab=1.3)
#savePlot(file = "../figures3/wavescale_prop_by_yr.png", type = "png")

table(dat$baitType, useNA = "ifany") # 1737 NA
table(dat$baitType, dat$year, useNA = "ifany") # 1737 NA
par(mar=c(5.1, 4.1, 2.1, 4.5), xpd=TRUE)
a <- with(dat[dat$year > 2008,],table(baitType, year))
a <- t(a)/apply(a,2,sum)
barplot(t(a), legend.text=TRUE, col = c(1:5), args.legend=list(x="topright", title="Bait type", inset = c(-0.2, 0)), 
        xlab = "Year", ylab = "Proportion of samples", cex.lab=1.3)
#savePlot(file = "../figures3/baittype_prop_by_yr.png", type = "png")

par(mar=c(5.1, 3.1, 2.1, 5.1), xpd=TRUE)
a <- with(dat[dat$year > 2008,],table(Cluster, year))
a <- t(a)/apply(a,2,sum)
barplot(t(a), legend.text=TRUE, col = c(1:5), args.legend=list(x="topright", title="Cluster", inset = c(-0.15, 0)), 
        xlab = "Year", ylab = "", cex.lab=1.3)
#savePlot(file = "../figures3/cluster_prop_by_yr.png", type = "png")
savePlot(file = "../figures3/proportions_by_year.png", type = "png")

barplot(table(dat$weather), main = "Weather")
barplot(table(dat$HkCourse), main = "HK Course")
barplot(table(dat$Cluster), main = "Cluster")

table(dat$hbf, useNA = "ifany") # 351 NA
table(dat$year, dat$hbf, useNA = "ifany") # 351 NA
windows(height=6, width=12); par(mfrow=c(1,2))
a <- with(dat[dat$year > 2008,],table(year, hbf)) # 351 NA
aa <- t(a) * (as.numeric(colnames(a)))
a <- apply(aa, 2, sum) / apply(a,1,sum)
plot(2009:2022, a, type = "b", ylim = c(0, 22), ylab = "Mean HBF per year", xlab = "Year")
#savePlot(file = "../figures3/Mean_HBF_per_year.png", type = "png")

table(dat$lightstick, useNA = "ifany") # 0 NA
table(dat$lightstick, dat$year, useNA = "ifany") # 0 NA
a <- with(dat[dat$year > 2008,],table(lightstick, year) / rep(table(year),each = 2)) # 0 NA
plot(2009:2022, a[2,], type = "b", ylim = c(0, 1), ylab = "Proportion of samples from sets reporting lightsticks", xlab = "Year")
#savePlot(file = "../figures3/lightstick_prop_by_yr.png", type = "png")
savePlot(file = "../figures3/HBF_ltstks_per_year.png", type = "png")

table(dat$processtype, dat$year)
dat$processtype[dat$year==2019 & dat$processtype=="RGG"] <- "RGT"

a <- tapply(dat$length, list(dat$lon, dat$lat), length)
windows(width=10, height = 7)
image(sort(unique(dat$lon)),sort(unique(dat$lat)),  log10(a), xlab = "Longitude", ylab = "Latitude")
contour(sort(unique(dat$lon)),sort(unique(dat$lat)),  (a), add=TRUE, labcex=0.8, levels = c(10,100,1000,2000,3000,5000,7000,10000))
map(add=T, database = "world2", fill=TRUE)
savePlot(file = "../figures3/samplemap.png", type = "png")

windows(); par(mfrow=c(2,2))
hist((table(dat$tripID)), nclass=2000, xlab = "Fish measured", main = "Fish per trip")
hist((table(dat$tripID)), nclass=2000, xlim = c(0,30), xlab = "Fish measured", main = "Fish per trip")
hist(table(dat$setid), nclass=30, xlab = "Fish measured", main = "Fish per set")
savePlot(file = "../figures3/fish_per_set_trip.png", type = "png")

# What are the characteristics of process types? 
windows(width=12, height = 11); par(mfrow = c(3,2), mar = c(5,4,3,2))
ptypes <- sort(unique(dat$processtype))
for (pr in ptypes) {
  datx <- filter(dat, processtype == pr)
  a <- tapply(datx$length, list(datx$lon, datx$lat), length)
  image(sort(unique(datx$lon)),sort(unique(datx$lat)),  log10(a), main = pr, xlim = c(120, 250), ylim = c(-40, 40), 
        xlab = "Longitude", ylab = "Latitude")
  contour(sort(unique(datx$lon)),sort(unique(datx$lat)),  (a), add=TRUE, labcex=0.8, levels = c(10,100,1000,2000,3000,5000,7000,10000))
  map(add=T, database = "world2", fill=TRUE)
}
savePlot(file = "../figures3/samplemap_process.png", type = "png")


modmap_process1 %<-% gam(processtype=="RHG" ~ te(lon, lat, k = c(10,10)) + yrf + s(length), data = dat[dat$year > 2015 & dat$length > 50 & dat$length < 200,], family="binomial")
modmap_process2 %<-% gam(processtype=="RHG" ~ te(lon, lat, k = c(10,10)) + s(obs_name, bs="re") + yrf + s(length), data = dat[dat$year > 2015 & dat$length > 50 & dat$length < 200,], family="binomial")
summary(modmap_process1)
AIC(modmap_process1)
save(modmap_process2, file = "./resubmit/store/modmap_process2.RData")

windows()
plot.gam(modmap_process2, pages = 1, all.terms=TRUE, scheme=2)
windows()
plot.gam(modmap_process2, select = 1, scheme=2, too.far=0.01, trans=inv.logit)

windows(width=20, height = 15); 
new_data <- with(filter(dat, year > 2015),
                 expand.grid(hbf=median(hbf),
                             length=130,
                             obs_name=factor(obs_name[1],levels=levels(obs_name)),
                             lat = sort(unique(lat)),
                             lon = sort(unique(lon)),
                             yrf = yrf[1],
                             wavescale=factor(wavescale[1], levels=levels(wavescale)),
                             year = median(year)))
excl <- c("s(obs_name)")
pred  <- predict(modmap_process2, new_data, exclude = excl,
                 type = "link", se.fit = TRUE, unconditional = TRUE)
pred  <- cbind(pred, new_data)
pred  <- transform(pred, lwr_ci = inv.logit(fit - (2 * se.fit)),
                   upr_ci = inv.logit(fit + (2 * se.fit)),
                   fitted = inv.logit(fit))
z <- tapply(pred$fitted, list(pred$lon, pred$lat), mean)

a <- filter(dat, year > 2015)
tf2 <- exclude.too.far(pred$lat, pred$lon, a$lat, a$lon, 0.05)
z2 <- tapply(pred$fitted[!tf2], list(pred$lon[!tf2], pred$lat[!tf2]), mean)
#windows()
image(as.numeric(rownames(z2)),as.numeric(colnames(z2)),  z2, xlim = c(120, 250), ylim = c(-40, 40), xlab = "Longitude", ylab = "Latitude")
contour(as.numeric(rownames(z2)),as.numeric(colnames(z2)), z2, add=TRUE, labcex=0.8)
map(add=T, database = "world2", fill=TRUE)
savePlot(file = "../figures3/modmap_process2.png", type = "png")


windows(width=10, height = 7.5)
i <- 1
plot(1:10, 1:10, xlim = c(60, 200), ylim = c(0, 1), col = i, pch = i, type = "n", 
     xlab = "Fork length (cm)", ylab = "Relative frequency")
for (pr in ptypes) {
  datx <- filter(dat, processtype == pr, length > 60, length < 200)
  a <- table(datx$length)
  a <- a / max(a)
  points(as.numeric(names(a)), a, xlim = c(60, 200), ylim = c(0, 1), col = i, pch = i, type = "b", lty=2)
  i = i + 1
}
legend("topright", legend = ptypes, col = 1:5, pch=1:5, lty=2)
savePlot(file = "../figures3/lenfreq_process.png", type = "png")
savePlot(file = "../figures_resubmit/Fig5_lenfreq_process.png", type = "png")
savePlot(file = "../figures_resubmit/Fig5_lenfreq_process.pdf", type = "pdf")

windows(width=10, height = 7.5); par(mfrow = c(2,2))
i <- 1; j <- 1
for (pr in ptypes) {
  plot(1:10, 1:10, xlim = c(60, 200), ylim = c(0, 1), col = i, pch = i, type = "n", 
       xlab = "Fork length (cm)", ylab = "Relative frequency")
  for (yy in 2009:2022) {
    datx <- filter(dat, processtype == pr & year == yy)
    if(dim(datx)[1] > 1) {
      a <- table(datx$length)
      a <- a / max(a)
      points(as.numeric(names(a)), a, xlim = c(60, 200), ylim = c(0, 1), col = i, pch = i, type = "b", lty=2)
      i = i + 1
    }
    j=j+1
  }
}
legend("topright", legend = ptypes, col = 1:5, pch=1:5, lty=2)
savePlot(file = "../figures3/lenfreq_process_yr.png", type = "png")


windows(width=20, height = 25); par(mfrow = c(5,3), mar=c(4,4,3,1))
years <- sort(unique(dat$year))
for (yy in years) {
  datx <- filter(dat, year == yy)
  datx$latf <- factor(datx$lat, levels = -50:50)
  datx$lonf <- factor(datx$lon, levels = 120:250)
  a <- tapply(datx$length, list(datx$lonf, datx$latf), length)
  image(as.numeric(rownames(a)),as.numeric(colnames(a)),  log10(a), main = yy, xlim = c(120, 250), ylim = c(-40, 40), 
        xlab = "Longitude", ylab = "Latitude")
  contour(as.numeric(rownames(a)),as.numeric(colnames(a)), (a), add=TRUE, labcex=0.8, levels = c(10,100,1000,2000,3000,5000,7000,10000))
  map(add=T, database = "world2", fill=TRUE)
}
savePlot(file = "../figures3/samplemap_year.png", type = "png")

# Check whether RGG and RGT are the same, as TWN suggests
ddd <- dat[dat$year %in% c(2014, 2015) & dat$processtype %in% c("RGG", "RGT") & !is.na(dat$processtype) & dat$hbf > 10 & dat$loglen > 4.3,]
ddd$processtype <- as.factor(ddd$processtype)
ddd_r <- ddd %>% group_by(setid) %>% slice_sample(n=1)
dim(ddd); dim(ddd_r)

# Check differences between sets - use a single year. 
mod_rlw_setre1 %<-% gam(logwt ~ s(loglen) + s(setid, bs="re") + te(lat, lon, mon, k = c(10,10,4), bs = c("cr", "cr", "cc")) + s(obs_name, bs="re") + processtype, data = ddd[ddd$year==2015,], gamma = 1.4, family = 'scat')
mod_rlw_setre2 %<-% gam(logwt ~ s(loglen) + s(setid, bs="re") + te(lat, lon, k = c(10,10), bs = c("cr", "cr")) + s(obs_name, bs="re") + yrf, data = filter(dat, year > 2017 & mon==2 & processtype=="RGT" & length > 50 & length < 180), gamma = 1.4)
mod_rlw_setre3 %<-% gam(logwt ~ s(loglen) + s(setid, bs="re") + te(lat, lon, k = c(10,10), bs = c("cr", "cr")) + s(obs_name, bs="re") + yrf + s(hbf), data = filter(dat, year > 2017 & mon==2 & processtype=="RGT" & length > 50 & length < 180), gamma = 1.4)
summary(mod_rlw_setre2)
summary(mod_rlw_setre3)
AIC(mod_rlw_setre3, mod_rlw_setre2)
windows()
plot.gam(mod_rlw_setre2, pages=1, all.terms=TRUE)
plot.gam(mod_rlw_setre3, pages=1, all.terms=TRUE)
save(mod_rlw_setre2, mod_rlw_setre3, file = "./resubmit/store/mod_rlw_setre.RData")

# check the 2 years of data with both types using a length model
mod_rlen1 %<-% gam(length ~ te(lat, lon, k = c(10,10)) + yrf + processtype, data = ddd_r, gamma = 1.4)
mod_rlen2 %<-% gam(length ~ te(lat, lon, mon, k = c(10,10,4), bs = c("cr", "cr", "cc")) + yrf + processtype, data = ddd_r, gamma = 1.4)
mod_rlen3 %<-% gam(length ~ te(lat, lon, k = c(10,10)) +s(mon) + yrf + processtype, data = ddd_r, gamma = 1.4)
mod_rlen4 %<-% gam(length ~ te(lat, lon, mon, k = c(10,10,4), bs = c("cr", "cr", "cc")) + yrf + s(obs_name, bs="re") + processtype, data = ddd_r, gamma = 1.4)
mod_rlen5 %<-% gam(length ~ te(lat, lon, mon, k = c(10,10,4), bs = c("cr", "cr", "cc")) + yrf + s(obs_name, bs="re") + processtype + s(hbf), data = ddd_r, gamma = 1.4)
mod_rlen6 %<-% gam(length ~ te(lat, lon, mon, k = c(10,10,4), bs = c("cr", "cr", "cc")) + yrf + s(obs_name, bs="re") + processtype + s(hbf) + lightstick, data = ddd_r, gamma = 1.4)
mod_rlen7 %<-% gam(length ~ te(lat, lon, mon, k = c(10,10,4), bs = c("cr", "cr", "cc")) + yrf + s(obs_name, bs="re") + processtype + s(hbf) + lightstick + s(lunar_illumination), data = ddd_r, gamma = 1.4)
mod_rlen8 %<-% gam(length ~ te(lat, lon, mon, k = c(10,10,4), bs = c("cr", "cr", "cc")) + s(obs_name, bs="re") + processtype + s(hbf) + lightstick + s(lunar_illumination), data = ddd_r, gamma = 1.4)
mod_rlen9 %<-% gam(length ~ te(lat, lon, mon, k = c(10,10,4), bs = c("cr", "cr", "cc")) + s(obs_name, bs="re") + s(hbf) + lightstick + s(lunar_illumination), data = ddd_r, gamma = 1.4)
mod_rlen10 %<-% gam(length ~ te(lat, lon, mon, k = c(10,10,4), bs = c("cr", "cr", "cc")) + s(obs_name, bs="re") + lightstick + s(lunar_illumination), data = ddd_r, gamma = 1.4)
mod_rlen11 %<-% gam(length ~ te(lat, lon, mon, k = c(10,10,4), bs = c("cr", "cr", "cc")) + s(obs_name, bs="re") + processtype + s(hbf) + lightstick + s(lunar_illumination)+sex, data = ddd_r, gamma = 1.4)
a <- AIC(mod_rlen1,mod_rlen2,mod_rlen3,mod_rlen4,mod_rlen5,mod_rlen6,mod_rlen7,mod_rlen8,mod_rlen9,mod_rlen10,mod_rlen11)
cbind(a, a$AIC-min(a$AIC))


summary(mod_rlen4)
summary(mod_rlen5)
summary(mod_rlen6)
summary(mod_rlen7)
summary(mod_rlen8) # fits best
# Family: gaussian 
# Link function: identity 
# 
# Formula:
#   length ~ te(lat, lon, mon, k = c(10, 10, 4), bs = c("cr", "cr", 
#                                                       "cc")) + s(obs_name, bs = "re") + processtype + s(hbf) + 
#   lightstick + s(lunar_illumination)
# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     126.562      1.676  75.507  < 2e-16 ***
#   processtypeRGT   -3.049      1.917  -1.590  0.11184    
# lightstick1       9.241      3.443   2.684  0.00732 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df     F p-value    
# te(lat,lon,mon)       46.166 59.381 4.890 < 2e-16 ***
#   s(obs_name)           23.099 37.000 3.188 < 2e-16 ***
#   s(hbf)                 4.223  4.926 2.916 0.01027 *  
#   s(lunar_illumination)  1.083  1.161 7.533 0.00361 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# R-sq.(adj) =  0.218   Deviance explained = 23.8%
# GCV = 390.17  Scale est. = 371.71    n = 2944

summary(mod_rlen8)$s.table
summary(mod_rlen8)$p.table
names(summary(mod_rlen8))
summary(mod_rlen8)$pTerms.table
summary(mod_rlen8)$p.t
summary(mod_rlen8)$p.coeff
names(mod_rlen8)
mod_rlen8$xlevels
mod_rlen8$formula
str(mod_rlen8$terms)

modnames <- paste("mod_rlen", 1:11, sep = "")
termtab <- array(0, dim=c(11, 10))
i=1
tmlist <- c("yrf", "lat", "lon", "mon", "obs_name", "hbf", "lunar_illumination", "processtype", "lightstick")
for(mm in modnames) {
  a <- attr(get(mm)$terms, "term.labels")
  j=1
  for (tm in tmlist) {
    termtab[i, j][tm %in% a] <- 1
    j=j+1
  }
  termtab[i, j] <- AIC(get(mm))
  i=i+1
}
colnames(termtab) <- c(tmlist, "AIC")
rownames(termtab) <- modnames
termtab <- data.frame(termtab)
termtab$dAIC <- termtab$AIC-min(termtab$AIC)
write.csv(termtab, file = "./tables/mod_rlen.csv")

# flextable(head(cars)) %>% 
#   bold(part = "header") %>% 
#   add_footer_lines("The 'cars' dataset")
stable <-  flextable(as.data.frame(summary(mod_rlen8)$s.table)) %>% 
  bold(part = "header")
str(stable)  


summary(mod_rlen9)
windows()
#plot.gam(mod_rlen7, pages = 1, all.terms=TRUE)
plot.gam(mod_rlen8, pages = 1, all.terms=TRUE)
#plot.gam(mod_rlen9, pages = 1, all.terms=TRUE)

# check the 2 years of data with both types using a weight model
mod_rwgt1 %<-% gam(logwt ~ te(lat, lon, k = c(10,10)) + yrf + processtype, data = ddd_r, gamma = 1.4)
mod_rwgt2 %<-% gam(logwt ~ te(lat, lon, mon, k = c(10,10,4), bs = c("cr", "cr", "cc")) + yrf + processtype, data = ddd_r, gamma = 1.4)
mod_rwgt3 %<-% gam(logwt ~ te(lat, lon, k = c(10,10)) +s(mon) + yrf + processtype, data = ddd_r, gamma = 1.4)
mod_rwgt4 %<-% gam(logwt ~ te(lat, lon, mon, k = c(10,10,4), bs = c("cr", "cr", "cc")) + yrf + s(obs_name, bs="re") + processtype, data = ddd_r, gamma = 1.4)
mod_rwgt5 %<-% gam(logwt ~ te(lat, lon, mon, k = c(10,10,4), bs = c("cr", "cr", "cc")) + yrf + s(obs_name, bs="re") + processtype + s(hbf), data = ddd_r, gamma = 1.4)
mod_rwgt6 %<-% gam(logwt ~ te(lat, lon, mon, k = c(10,10,4), bs = c("cr", "cr", "cc")) + yrf + s(obs_name, bs="re") + processtype + s(hbf) + lightstick, data = ddd_r, gamma = 1.4)
mod_rwgt7 %<-% gam(logwt ~ te(lat, lon, mon, k = c(10,10,4), bs = c("cr", "cr", "cc")) + yrf + s(obs_name, bs="re") + processtype + s(hbf) + lightstick + s(lunar_illumination), data = ddd_r, gamma = 1.4)
mod_rwgt8 %<-% gam(logwt ~ te(lat, lon, mon, k = c(10,10,4), bs = c("cr", "cr", "cc")) + s(obs_name, bs="re") + processtype + s(hbf) + lightstick + s(lunar_illumination), data = ddd_r, gamma = 1.4)
mod_rwgt9 %<-% gam(logwt ~ te(lat, lon, mon, k = c(10,10,4), bs = c("cr", "cr", "cc")) + s(obs_name, bs="re") + s(hbf) + lightstick + s(lunar_illumination), data = ddd_r, gamma = 1.4)
mod_rwgt10 %<-% gam(logwt ~ te(lat, lon, mon, k = c(10,10,4), bs = c("cr", "cr", "cc")) + s(obs_name, bs="re") + lightstick + s(lunar_illumination), data = ddd_r, gamma = 1.4)
mod_rwgt11 %<-% gam(logwt ~ te(lat, lon, mon, k = c(10,10,4), bs = c("cr", "cr", "cc")) + s(obs_name, bs="re") + processtype + s(hbf) + lightstick + s(lunar_illumination) + sex, data = ddd_r, gamma = 1.4)
a <- AIC(mod_rwgt1,mod_rwgt2,mod_rwgt3,mod_rwgt4,mod_rwgt5,mod_rwgt6,mod_rwgt7,mod_rwgt8,mod_rwgt9,mod_rwgt10,mod_rwgt11)
cbind(a, a$AIC-min(a$AIC))
summary(mod_rwgt8)  # fits best
# Family: gaussian 
# Link function: identity 
# 
# Formula:
#   logwt ~ te(lat, lon, mon, k = c(10, 10, 4), bs = c("cr", "cr", 
#                                                      "cc")) + s(obs_name, bs = "re") + processtype + s(hbf) + 
#   lightstick + s(lunar_illumination)
# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     3.50726    0.04561  76.905  < 2e-16 ***
#   processtypeRGT -0.09872    0.05029  -1.963  0.04975 *  
#   lightstick1     0.26461    0.08963   2.952  0.00318 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df     F p-value    
# te(lat,lon,mon)       47.974 61.704 5.215  <2e-16 ***
#   s(obs_name)           24.425 37.000 3.843  <2e-16 ***
#   s(hbf)                 4.840  5.630 2.346  0.0435 *  
#   s(lunar_illumination)  1.486  1.813 4.245  0.0109 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# R-sq.(adj) =  0.247   Deviance explained = 26.8%
# GCV = 0.24157  Scale est. = 0.22953   n = 2944

newdat <- rbind(ddd_r[1:5,],ddd_r[1:5,])
newdat[1:5,]$processtype <- "RGT"
exp(predict(mod_rwgt8, newdata=newdat[1:5,])) / 
  exp(predict(mod_rwgt8, newdata=newdat[6:10,]))


windows()
plot.gam(mod_rwgt8, pages = 1, all.terms=TRUE)


modnames <- paste("mod_rwgt", 1:11, sep = "")
termtab <- array(0, dim=c(11, 10))
i=1
tmlist <- c("yrf", "lat", "lon", "mon", "obs_name", "hbf", "lunar_illumination", "processtype", "lightstick")
for(mm in modnames) {
  a <- attr(get(mm)$terms, "term.labels")
  j=1
  for (tm in tmlist) {
    termtab[i, j][tm %in% a] <- 1
    j=j+1
  }
  termtab[i, j] <- AIC(get(mm))
  i=i+1
}
colnames(termtab) <- c(tmlist, "AIC")
rownames(termtab) <- modnames
termtab <- data.frame(termtab)
termtab$dAIC <- termtab$AIC-min(termtab$AIC)
write.csv(termtab, file = "./tables/mod_rwgt.csv")

# check the 2 years of data with both types using LW model
mod_r_lw1 %<-% gam(logwt ~ s(loglen) + te(lat, lon, k = c(10,10)) + yrf + processtype, data = ddd_r, gamma = 1.4)
mod_r_lw2 %<-% gam(logwt ~ s(loglen) + te(lat, lon, mon, k = c(10,10,4), bs = c("cr", "cr", "cc")) + yrf + processtype, data = ddd_r, gamma = 1.4)
mod_r_lw3 %<-% gam(logwt ~ s(loglen) + te(lat, lon, k = c(10,10)) +s(mon) + yrf + processtype, data = ddd_r, gamma = 1.4)
mod_r_lw4 %<-% gam(logwt ~ s(loglen) + te(lat, lon, mon, k = c(10,10,4), bs = c("cr", "cr", "cc")) + yrf + s(obs_name, bs="re") + processtype, data = ddd_r, gamma = 1.4)
mod_r_lw5 %<-% gam(logwt ~ s(loglen) + te(lat, lon, mon, k = c(10,10,4), bs = c("cr", "cr", "cc")) + yrf + s(obs_name, bs="re") + processtype + s(hbf), data = ddd_r, gamma = 1.4)
mod_r_lw6 %<-% gam(logwt ~ s(loglen) + te(lat, lon, mon, k = c(10,10,4), bs = c("cr", "cr", "cc")) + yrf + s(obs_name, bs="re") + processtype + s(hbf) + lightstick, data = ddd_r, gamma = 1.4)
mod_r_lw7 %<-% gam(logwt ~ s(loglen) + te(lat, lon, mon, k = c(10,10,4), bs = c("cr", "cr", "cc")) + yrf + s(obs_name, bs="re") + processtype + s(hbf) + lightstick + s(lunar_illumination), data = ddd_r, gamma = 1.4)
mod_r_lw8 %<-% gam(logwt ~ s(loglen) + te(lat, lon, mon, k = c(10,10,4), bs = c("cr", "cr", "cc")) + s(obs_name, bs="re") + processtype + s(hbf) + lightstick + s(lunar_illumination), data = ddd_r, gamma = 1.4)
mod_r_lw9 %<-% gam(logwt ~ s(loglen) + te(lat, lon, mon, k = c(10,10,4), bs = c("cr", "cr", "cc")) + s(obs_name, bs="re") + s(hbf) + lightstick + s(lunar_illumination), data = ddd_r, gamma = 1.4)
mod_r_lw10 %<-% gam(logwt ~ s(loglen) + te(lat, lon, mon, k = c(10,10,4), bs = c("cr", "cr", "cc")) + s(obs_name, bs="re") + lightstick + s(lunar_illumination), data = ddd_r, gamma = 1.4)
mod_r_lw11 %<-% gam(logwt ~ s(loglen) + te(lat, lon, mon, k = c(10,10,4), bs = c("cr", "cr", "cc")) + s(obs_name, bs="re") + processtype + s(hbf) + lightstick, data = ddd_r, gamma = 1.4)
mod_r_lw12 %<-% gam(logwt ~ s(loglen) + te(lat, lon, mon, k = c(10,10,4), bs = c("cr", "cr", "cc")) + s(obs_name, bs="re") + s(hbf) + lightstick, data = ddd_r, gamma = 1.4)
mod_r_lw13 %<-% gam(logwt ~ s(loglen) + te(lat, lon, mon, k = c(10,10,4), bs = c("cr", "cr", "cc")) + s(obs_name, bs="re") + s(hbf), data = ddd_r, gamma = 1.4)
mod_r_lw14 %<-% gam(logwt ~ s(loglen) + te(lat, lon, mon, k = c(10,10,4), bs = c("cr", "cr", "cc")) + s(obs_name, bs="re") + s(hbf) + s(lunar_illumination), data = ddd_r, gamma = 1.4)
mod_r_lw15 %<-% gam(logwt ~ s(loglen) + sex + te(lat, lon, mon, k = c(10,10,4), bs = c("cr", "cr", "cc")) + s(obs_name, bs="re") + s(hbf) + s(lunar_illumination), data = ddd_r, gamma = 1.4)

a <- AIC(mod_r_lw1,mod_r_lw2,mod_r_lw3,mod_r_lw4,mod_r_lw5,mod_r_lw6,mod_r_lw7,mod_r_lw8,mod_r_lw9,mod_r_lw10,mod_r_lw11,mod_r_lw12,mod_r_lw13,mod_r_lw14)
a
a-min(a)
summary(mod_r_lw9)  # fits best
summary(mod_r_lw11)
summary(mod_r_lw12)
summary(mod_r_lw13)
windows()
plot.gam(mod_r_lw9, pages = 1, all.terms=TRUE)

windows(); par(mfrow=c(2,2))
gam.check(mod_rlen8)
savePlot(file="../figures3/mod_rlen8.png", type = "png")
gam.check(mod_rwgt8)
savePlot(file="../figures3/mod_rwgt8.png", type = "png")
gam.check(mod_r_lw9)
savePlot(file="../figures3/mod_r_lw9.png", type = "png")
gam.check(mod_r_lw9_scat)
savePlot(file="../figures3/mod_r_lw9_scat.png", type = "png")

modnames <- paste("mod_r_lw", 1:14, sep = "")
termtab <- array(0, dim=c(14, 11))
i=1
tmlist <- c("yrf", "loglen", "lat", "lon", "mon", "obs_name", "hbf", "lunar_illumination", "processtype", "lightstick")
for(mm in modnames) {
  a <- attr(get(mm)$terms, "term.labels")
  j=1
  for (tm in tmlist) {
    termtab[i, j][tm %in% a] <- 1
    j=j+1
  }
  termtab[i, j] <- AIC(get(mm))
  i=i+1
}
colnames(termtab) <- c(tmlist, "AIC")
rownames(termtab) <- modnames
termtab <- data.frame(termtab)
termtab$dAIC <- termtab$AIC-min(termtab$AIC)
write.csv(termtab, file = "./tables/mod_rlw.csv")

str(ddd_r)
table(is.na(ddd_r$processtype))
table(is.na(ddd_r$lightstick))
table(is.na(ddd_r$obs_name))
table(is.na(ddd_r$lunar_illumination))
table(is.na(ddd_r$hbf))
table(is.na(ddd_r$mon))
table(is.na(ddd_r$yrf))
table(is.na(ddd_r$logwt))
table(is.na(ddd_r$loglen))
table(is.na(ddd_r$lat))
table(is.na(ddd_r$lon))
table(is.na(ddd_r$lunar_illumination))

# check the 2 years of data with both types using LW model, with scat dbn to fit better
mod_r_lw1_scat %<-% gam(logwt ~ s(loglen) + te(lat, lon, k = c(10,10)) + yrf + processtype, data = ddd_r, gamma = 1.4, family=scat, method='ML')
mod_r_lw2_scat %<-% gam(logwt ~ s(loglen) + te(lat, lon, mon, k = c(10,10,4), bs = c("cr", "cr", "cc")) + yrf + processtype, data = ddd_r, gamma = 1.4, family=scat, method='ML')
mod_r_lw3_scat %<-% gam(logwt ~ s(loglen) + te(lat, lon, k = c(10,10)) +s(mon) + yrf + processtype, data = ddd_r, gamma = 1.4, family=scat, method='ML')
mod_r_lw4_scat %<-% gam(logwt ~ s(loglen) + te(lat, lon, mon, k = c(10,10,4), bs = c("cr", "cr", "cc")) + yrf + s(obs_name, bs="re") + processtype, data = ddd_r, gamma = 1.4, family=scat, method='ML')
mod_r_lw5_scat %<-% gam(logwt ~ s(loglen) + te(lat, lon, mon, k = c(10,10,4), bs = c("cr", "cr", "cc")) + yrf + s(obs_name, bs="re") + processtype + s(hbf), data = ddd_r, gamma = 1.4, family=scat, method='ML')
mod_r_lw6_scat %<-% gam(logwt ~ s(loglen) + te(lat, lon, mon, k = c(10,10,4), bs = c("cr", "cr", "cc")) + yrf + s(obs_name, bs="re") + processtype + s(hbf) + lightstick, data = ddd_r, gamma = 1.4, family=scat, method='ML')
mod_r_lw7_scat %<-% gam(logwt ~ s(loglen) + te(lat, lon, mon, k = c(10,10,4), bs = c("cr", "cr", "cc")) + yrf + s(obs_name, bs="re") + processtype + s(hbf) + lightstick + s(lunar_illumination), data = ddd_r, gamma = 1.4, family=scat, method='ML')
mod_r_lw8_scat %<-% gam(logwt ~ s(loglen) + te(lat, lon, mon, k = c(10,10,4), bs = c("cr", "cr", "cc")) + s(obs_name, bs="re") + processtype + s(hbf) + lightstick + s(lunar_illumination), data = ddd_r, gamma = 1.4, family=scat, method='ML')
mod_r_lw9_scat %<-% gam(logwt ~ s(loglen) + te(lat, lon, mon, k = c(10,10,4), bs = c("cr", "cr", "cc")) + s(obs_name, bs="re") + s(hbf) + lightstick + s(lunar_illumination), data = ddd_r, gamma = 1.4, family=scat, method='ML')
mod_r_lw10_scat %<-% gam(logwt ~ s(loglen) + te(lat, lon, mon, k = c(10,10,4), bs = c("cr", "cr", "cc")) + s(obs_name, bs="re") + lightstick + s(lunar_illumination), data = ddd_r, gamma = 1.4, family=scat, method='ML')
mod_r_lw11_scat %<-% gam(logwt ~ s(loglen) + te(lat, lon, mon, k = c(10,10,4), bs = c("cr", "cr", "cc")) + s(obs_name, bs="re") + processtype + s(hbf) + lightstick, data = ddd_r, gamma = 1.4, family=scat, method='ML')
mod_r_lw12_scat %<-% gam(logwt ~ s(loglen) + te(lat, lon, mon, k = c(10,10,4), bs = c("cr", "cr", "cc")) + s(obs_name, bs="re") + s(hbf) + lightstick, data = ddd_r, gamma = 1.4, family=scat, method='ML')
mod_r_lw13_scat %<-% gam(logwt ~ s(loglen) + te(lat, lon, mon, k = c(10,10,4), bs = c("cr", "cr", "cc")) + s(obs_name, bs="re") + s(hbf), data = ddd_r, gamma = 1.4, family=scat, method='ML')
mod_r_lw14_scat %<-% gam(logwt ~ s(loglen) + te(lat, lon, mon, k = c(10,10,4), bs = c("cr", "cr", "cc")) + s(obs_name, bs="re") + s(hbf) + s(lunar_illumination), data = ddd_r, gamma = 1.4, family=scat, method='ML')
mod_r_lw15_scat %<-% gam(logwt ~ s(loglen) + te(lat, lon, mon, k = c(10,10,4), bs = c("cr", "cr", "cc")) + s(obs_name, bs="re") + s(hbf) + lightstick + sex, data = ddd_r, gamma = 1.4, family=scat, method='ML')

mod_r_lw11b_scat %<-% gam(logwt ~ s(loglen) + te(lat, lon, mon, k = c(11,11,5), bs = c("cr", "cr", "cc")) + s(obs_name, bs="re") + processtype + s(hbf) + lightstick, data = ddd_r, gamma = 1.4, family=scat, method='ML')
mod_r_lw12b_scat %<-% gam(logwt ~ s(loglen) + te(lat, lon, mon, k = c(11,11,5), bs = c("cr", "cr", "cc")) + s(obs_name, bs="re") + s(hbf) + lightstick, data = ddd_r, gamma = 1.4, family=scat, method='ML')


a <- AIC(mod_r_lw1_scat,mod_r_lw2_scat,mod_r_lw3_scat,mod_r_lw4_scat,mod_r_lw5_scat,mod_r_lw6_scat,mod_r_lw7_scat,
         mod_r_lw8_scat,mod_r_lw9_scat,mod_r_lw10_scat,mod_r_lw11_scat,mod_r_lw12_scat,mod_r_lw13_scat,
         mod_r_lw14_scat,mod_r_lw15_scat)
cbind(a, a$AIC - min(a$AIC))
a
a-min(a)
windows(); par(mfrow=c(2,2))
gam.check(mod_r_lw9_scat)
summary(mod_r_lw11_scat)
summary(mod_r_lw12_scat)
summary(mod_r_lw13_scat)

AIC(mod_r_lw11_scat)
AIC(mod_r_lw12_scat)
AIC(mod_r_lw13_scat)

mod_r_lw11_scat$gcv.ubre
mod_r_lw12_scat$gcv.ubre
mod_r_lw13_scat$gcv.ubre

mod_r_lw11_scat$formula
mod_r_lw12_scat$formula

modnames <- paste("mod_r_lw", 1:15, "_scat", sep = "")
termtab <- array(0, dim=c(15, 11))
i=1
tmlist <- c("yrf", "loglen", "lat", "lon", "mon", "obs_name", "hbf", "lunar_illumination", "processtype", "lightstick")
for(mm in modnames) {
  a <- attr(get(mm)$terms, "term.labels")
  j=1
  for (tm in tmlist) {
    termtab[i, j][tm %in% a] <- 1
    j=j+1
  }
  termtab[i, j] <- AIC(get(mm))
  i=i+1
}
colnames(termtab) <- c(tmlist, "AIC")
rownames(termtab) <- modnames
termtab <- data.frame(termtab)
termtab$dAIC <- termtab$AIC-min(termtab$AIC)
write.csv(termtab, file = "./tables/mod_rlw_scat.csv")


ddd$obs_name <- factor(ddd$obs_name)
table(ddd$processtype, ddd$obs_name) # Most observers do one or the other. Only 6 observers do both RGG and RGT. 
# What was the timing of the changeover for those observers? 
a <- ddd[ddd$obs_name %in% c("obs_011","obs_031","obs_052","obs_065","obs_066","obs_069"),]
a$obs_name <- factor(a$obs_name)
table(a$processtype, paste(a$year, a$qtr, sep="_"), a$obs_name) # Most observers do one or the other. Only 6 observers do both RGG and RGT. 

dd2 <- ddd[!ddd$obs_name %in% c("obs_011","obs_031","obs_052","obs_065","obs_066","obs_069"),]
modlen4x <- gam(length ~ te(lon, lat, mon, k = c(10,10,4)) + yf + s(obs_name, bs="re") + processtype, data = dd2)
summary(modlen4x)

table(ddd$processtype, ddd$tripID) # Most observers do one or the other. Only 6 observers do both RGG and RGT. 


# Initial plots & summaries
str(dat)
summary(dat)

windows()
#hist(dat$year, xlab = "Year", ylab = "Samples per year", main = "")
barplot(table(dat$year), xlab = "Year", ylab = "Samples per year", main = "")
savePlot(file = "../figures3/total_records_per_year.png", type = "png")
table(dat$year) # only 7 in 2022 - remove

table(dat$processtype)
hist(dat$length, nclass=200)
table(dat$length)
quantile(dat$length, probs = c(0.1, 1, 99, 99.9)/100)
hist(dat$weight, nclass=250)
hist(dat$weight, nclass=250, xlim = c(0, 100)) # note the many peaks at units of 5. How will this affect precision, and estimation for small fish? 
table(dat$weight %% 1)
table(dat$weight %% 5) / length(dat$weight)
table(dat$weight %% 5 == 4.5) # 3 weights not a whole number
dat[dat$weight %% 5 == 4.5,] # 3 weights not a whole number
table(dat$weight %% 5 == 0.3) # 6 weights not a whole number
dat[dat$weight %% 5 == 0.3,] # 6 weights not a whole number

# Clean data - additional cleaning
da2 <- dat[dat$length > 60 & dat$length < 200,]
da2 <- da2[da2$year > 2008 & da2$year < 2023,]
da2 <- da2[!da2$weight %% 5 %in% c(0.3, 4.5),]

doth <- filter(da2, processtype=="OTH")
drgg <- filter(da2, processtype=="RGG")
drgt <- filter(da2, processtype=="RGT")
drhg <- filter(da2, processtype=="RHG")
drww <- filter(da2, processtype=="RWW")

drggf <- filter(drgg, sex=="F")
drggm <- filter(drgg, sex=="M")
drgtf <- filter(drgt, sex=="F")
drgtm <- filter(drgt, sex=="M")

# Observers
table(da2$obs_name)
# check if any are more likely to a) round or b) give bad estimates
# Proportion of measurements on the 5 should be 20%

dim(drggf)
dim(drggm)
dim(drgtf)
dim(drgtm)
length(table(factor(drggf$setid)))
length(table(factor(drggm$setid)))
length(table(factor(drgtf$setid)))
length(table(factor(drgtm$setid)))

dat %>% filter(year >= 2015) %>% group_by(processtype) %>% summarise(count=n())
dat %>% group_by(processtype, year > 2014, sex) %>% summarise(count=n()) %>% print(n=100)
dat %>% group_by(year > 2014, sex) %>% summarise(count=n()) %>% print(n=100)
dat %>% group_by(sex) %>% summarise(count=n()) %>% print(n=100)

hist(da2$modwt5)
hist(da2$modlen5)

str(da2)

windows()
hist(table())
a <- table(da2$obs_name, da2$modwt5)
table(da2$modwt5) / length(da2$modwt5)
table(da2$modlen5) / length(da2$modlen5)
da2 %>% group_by(yrf, modwt5) %>% summarise(count=n())

apply(a, 1, sum)[order(apply(a, 1, sum))]
obscheck5 <- cbind(a / apply(a, 1, sum), apply(a, 1, sum))
hist(obscheck5[obscheck5[,6] > 100,1], nclass=30, xlim = c(0,1))
obscheck5[obscheck5[,1] > 0.5,]
obscheck5[rownames(obscheck5) %in% c("obs_065", "obs_091", "obs_175", "obs_087", "obs_113", "obs_128"),]

# a <- table(da2$tripID, da2$weight %% 5)
# check5 <- cbind(a / apply(a, 1, sum), apply(a, 1, sum))
# windows()
# hist(check5[check5[,7] > 100,1], nclass=50, xlim = c(0,1), xlab = "Proportion in the 5 bin")
# savePlot(file = "../figures3/prop_weight_samples_mult_of_five.png", type = "png")
# 
# a <- table(da2$tripID, da2$length %% 5)
# check5 <- cbind(a / apply(a, 1, sum), apply(a, 1, sum))
# windows()
# hist(check5[check5[,6] > 100,1], nclass=50, xlim = c(0,1), xlab = "Proportion in the 5 bin")
# savePlot(file = "../figures3/prop_length_samples_mult_of_five.png", type = "png")


a <- a[,1:5]
wv5 <- a / apply(a, 1, sum)
windows(height=7, width = 7); par(mfrow = c(1,2), mar=c(5,2,3,1), oma=c(0,2,0,0))
plot(0:4, wv5[,1], col=1, pch=1, type ="n", ylim = c(0.1,.35), xlab = "Remainder", ylab = "", main="Weights",cex.lab=1.2)
for (i in 1:5) {
  points(0:4, wv5[i,], col=i, pch=as.character(i), type ="b")
}
a <- table(da2$wavescale, da2$modlen5)
a <- a[,1:5]
wv5 <- a / apply(a, 1, sum)
plot(0:4, wv5[,1], col=1, pch=1, type ="n", ylim = c(0.1,.35), xlab = "Remainder", ylab = "", main= "Lengths",cex.lab=1.2)
for (i in 1:5) {
  points(0:4, wv5[i,], col=i, pch=as.character(i), type ="b")
}
legend("topright", title = "Wavescale", legend = 1:5, col = 1:5, lty=1)
mtext("Proportions", side=2, line=0.8, outer=TRUE, cex=1.2)
savePlot("../figures3/Measurement rounding by wavescale.png", type = "png")

nmax=200
a <- table(da2$obs_name, da2$weight %% 5)
a <- a[apply(a, 1, sum) > nmax,]
wv5 <- a / apply(a, 1, sum)
windows(height=7, width = 7); par(mfrow = c(1,2), mar=c(5,2,3,1), oma=c(0,2,0,0))
plot(0:4, wv5[1,], col=1, pch=1, type ="n", ylim = c(0,1), xlab = "Remainder", ylab = "", main = "Weights",cex.lab=1.2)
for (i in 1:dim(wv5)[1]) {
  points(0:4, wv5[i,], col=i, pch=1, type ="b")
}
points(0:4, table(da2$weight %% 5) / length(da2$weight),col=1, pch=0, type = "b", lwd=2, cex=1.5)
a <- table(da2$obs_name, da2$modlen5)
a <- a[apply(a, 1, sum) > nmax,]
wv5 <- a / apply(a, 1, sum)
plot(0:4, wv5[1,], col=1, pch=1, type ="n", ylim = c(0,1), xlab = "Remainder", ylab = "", main = "Lengths",cex.lab=1.2)
for (i in 1:dim(wv5)[1]) {
  points(0:4, wv5[i,], col=i, pch=1, type ="b")
}
points(0:4, table(da2$length %% 5) / length(da2$length),col=1, pch=0, type = "b", lwd=2, cex=1.2)
mtext("Proportions", side=2, line=0.8, outer=TRUE, cex=1.2)
#legend("topright", title = "Wavescale", legend = 1:5, col = 1:5)
savePlot("../figures3/Measurement rounding by observer.png", type = "png")
savePlot("../figures_resubmit/Fig07_Measurement rounding by observer.png", type = "png")
savePlot("../figures_resubmit/Fig07_Measurement rounding by observer.pdf", type = "pdf")

x  <-  c(1:3)
plot(x, x, type="b")
legend("bottomright", c("dinges"), cex=1.2, pch=c(21), lty=1, bty="n")  
legend("bottomright", c("dinges"), cex=1.2, pch = 21,pt.bg = 'white', lty = 1, bty="n")

a <- with(da2[da2$wavescale==1,],table(tripID, weight %% 5))
windows(); par(mfrow = c(3,2))
check5 <- cbind(a / apply(a, 1, sum), apply(a, 1, sum))
hist(check5[check5[,6] > 100,1], nclass=50, xlim = c(0,1))



# mod_rf_wt5 <- randomForest(modwt5==0 ~ obs_name + processtype + as.factor(modlen5) + sex + wavescale + lightstick + region + weather + hbf, data = da2)
# mod_rf_wt5 <- randomForest(as.factor(modwt5==0) ~ obs_name, data = da2[sample(dim(da2)[1], 10000),], na.action=na.omit)
mod_rf_wt5 <- randomForest(as.factor(modwt5==0) ~ obs_name + processtype + modlen5 + sex + wavescale + lightstick + region + weather + hbf + HkCourse + length + yrf, 
                           data = da2[sample(dim(da2)[1], 10000),], na.action=na.omit, ntree=1000, importance = TRUE)
mod_rf_wt5b <- randomForest(as.factor(modwt5==0) ~ processtype + modlen5 + sex + wavescale + lightstick + region + weather + hbf + HkCourse + length + yrf, 
                            data = da2[sample(dim(da2)[1], 10000),], na.action=na.omit, ntree=1000, importance = TRUE)
mod_rf_wt5
mod_rf_wt5b
importance(mod_rf_wt5)
predict(mod_rf_wt5, type="prob")

datx <- da2[sample(dim(da2)[1], 50000),]
datx <- datx[complete.cases(datx),]
table(datx$region)
table(datx$year)
datx <- datx[datx$region != 0,]
mod_gam1 %<-% gam(modwt5==0 ~ s(length) + as.factor(obs_name) + as.factor(modlen5)+ as.factor(processtype) + as.factor(wavescale) + region + yrf, 
                  family = "binomial", data = datx)
summary(mod_gam1)
plot.gam(mod_gam1, all.terms=TRUE, pages = 1)


windows()
plot(dat$loglen, dat$logwt, xlab = "log (length)", ylab = "log (weight)")

windows()
with(dat[dat$processtype=="RGG",],plot(loglen, logwt, cex=0.5, xlab = "log (length)", ylab = "log (weight)"))
with(dat[dat$processtype=="RGT",],points(loglen, logwt, pch=2, col = 2, cex=0.5))
with(dat[dat$processtype=="RHG",],points(loglen, logwt, pch=3, col = 3, cex=0.5))
with(dat[dat$processtype=="RWW",],points(loglen, logwt, pch=4, col = 4, cex=0.5))
with(dat[dat$processtype=="OTH",],points(loglen, logwt, pch=5, col = 5, cex=0.5))

# Check rounding
windows(height=12, width=10); par(mfrow=c(2,1))
hist(da2$weight, nclass=250, xlim = c(0, 120), axes=F, main = "Histogram of weights", xlab = "Weight (kg)") # note the many peaks at units of 5. How will this affect precision, and estimation for small fish? 
axis(1,at=seq(0,120,5))
axis(2); box()
for (xx in seq(0,120,5)) lines(x=c(xx, xx), y=c(0, 10000), lty=2)
hist(da2$length, nclass=141, xlim = c(60, 200), axes=F, main = "Histogram of lengths", xlab = "Length (cm)") # note the many peaks at units of 5. How will this affect precision, and estimation for small fish? 
axis(1,at=seq(60,200,5))
axis(2); box()
for (xx in seq(60,200,5)) lines(x=c(xx, xx), y=c(0, 5000), lty=2)
savePlot(file = "../figures3/Fig08_weight_length_freq.png", type = "png")
savePlot(file = "../figures_resubmit/Fig06_weight_length_freq.png", type = "png")
savePlot(file = "../figures_resubmit/Fig06_weight_length_freq.pdf", type = "pdf")

save(dat,da2, file="../data/processed/dat.Rdata")

# models
# split into measurement types and sexes
table(dat$processtype)

table(doth$sex) # A few sexed
table(drgg$sex) # Almost all sexed
table(drgt$sex) # Most sexed
table(drhg$sex) # Most sexed
table(drww$sex) # Not sexed

# Look at error distributions 
windows(); par(mfrow=c(2,2))
gam.check(mod1)
mod1 <- gam(logwt ~ s(loglen), data = drggf)
windows(); par(mfrow=c(2,2))
gam.check(mod1)
mod1 <- gam(logwt ~ s(loglen), data = drgtm)
windows(); par(mfrow=c(2,2))
gam.check(mod1)
mod1 <- gam(logwt ~ s(loglen), data = drgtf)
windows(); par(mfrow=c(2,2))
gam.check(mod1)
# RGG has many more outliers. Is this linked to processtype or year? 
windows(8,10); par(mfrow=c(3,2))
for(yr in 2009:2014) {
  mod1 <- gam(logwt ~ s(loglen), data = filter(drggf, year == yr))
  qq.gam(mod1, main = yr)
}
windows(8,10); par(mfrow=c(3,3))
for(yr in 2015:2021) {
  mod1 <- gam(logwt ~ s(loglen), data = filter(drgtm, year == yr))
  qq.gam(mod1, main = yr)
}

# with heavy tailed distribution, scat works better - good to reduce the influence of outliers
mod1g <- gam(logwt ~ s(loglen), data = drggf, family = scat)
windows(); par(mfrow=c(2,2))
gam.check(mod1g)
summary(mod1g)
plot(mod1g)

mod1t <- gam(logwt ~ s(loglen), data = drgtf, family = scat)
windows(); par(mfrow=c(2,2))
gam.check(mod1t)
summary(mod1t)
plot(mod1t)

datx <- drggf[sample(dim(drggf)[1], 5000),]

mod_gf_rf <- randomForest(loglen ~ wavescale + lightstick + region + weather + hbf + HkCourse + yrf, 
                          data = datx, na.action=na.omit, ntree=1000, importance = TRUE)
importance(mod_gf_rf)
varImpPlot(mod_gf_rf)

str(drggf)
drggf_r <- drggf %>% group_by(setid) %>% slice_sample(n=1)
drgtf_r <- drgtf %>% group_by(setid) %>% slice_sample(n=1)

mod1gf <- gam(logwt ~ s(loglen, k=20), data = drggf_r, family = scat)
mod1tf <- gam(logwt ~ s(loglen, k=20), data = drgtf_r, family = scat)
drggf_r$resid <- mod1gf$residuals
drgtf_r$resid <- mod1tf$residuals
mod1gf_rf <- randomForest(resid ~ wavescale + region + weather + hbf + yrf + lat + lon, 
                          data = drggf_r, na.action=na.omit, ntree=500, importance = TRUE)
mod1tf_rf <- randomForest(resid ~ wavescale + lightstick + region + weather + hbf + yrf + lat + lon, 
                          data = drgtf_r, na.action=na.omit, ntree=500, importance = TRUE)
importance(mod1gf_rf)
windows()
varImpPlot(mod1gf_rf) # different rankings on Inc%MSE and incNodePurity
savePlot(file = "../figures3/varimpplot_resids_rgg_f.png", type = "png")

windows()
varImpPlot(mod1tf_rf) # different rankings on Inc%MSE and incNodePurity
savePlot(file = "../figures3/varimpplot_resids_rgt_f.png", type = "png")

# final bit of data cleaning before analyses --------------------------------------------------

lengthunique <- function(x) length(unique(x))

drggf <- drggf[drggf$year < 2015,] 
drggf <- drggf[drggf$length >= 70,]
drggf <- drggf[drggf$hbf > 10,]
a <- tapply(drggf$setid, drggf$obs_name, lengthunique) 
a <- a[a > 10 & !is.na(a)] # clean observers at least 10 sets
drggf <- drggf[drggf$obs_name %in% names(a),] 

drggm <- drggm[drggm$year < 2015,] 
drggm <- drggm[drggm$length >= 70,]
drggm <- drggm[drggm$hbf > 10,]
a <- tapply(drggm$setid, drggm$obs_name, lengthunique)
a <- a[a > 10 & !is.na(a)] # clean observers at least 10 sets
drggm <- drggm[drggm$obs_name %in% names(a),] 

drgtf <- drgtf[drgtf$year > 2014,] 
drgtf <- drgtf[drgtf$length >= 70,]
drgtf <- drgtf[drgtf$hbf > 10,]
a <- tapply(drgtf$setid, drgtf$obs_name, lengthunique)
a <- a[a > 10 & !is.na(a)] # clean observers at least 10 sets
drgtf <- drgtf[drgtf$obs_name %in% names(a),] 

drgtm <- drgtm[drgtm$year > 2014,] 
drgtm <- drgtm[drgtm$length >= 70,]
drgtm <- drgtm[drgtm$hbf > 10,]
a <- tapply(drgtm$setid, drgtm$obs_name, lengthunique)
a <- a[a > 10 & !is.na(a)] # clean observers at least 10 sets
drgtm <- drgtm[drgtm$obs_name %in% names(a),] 
table(drgtm$year)

str(drggf)

# # Models using the full dataset - don't use
# mod1gf <- gam(logwt ~ s(loglen, k=20) + obs_name + wavescale + lightstick + te(lat, lon, k = c(10,10)) + 
#                 weather + s(hbf) + HkCourse + yrf, data = drggf, family = gaussian)
# mod2gf <- gam(logwt ~ s(loglen, k=20) + obs_name + wavescale + lightstick + te(lat, lon, k = c(10,10)) + 
#                 weather + s(hbf) + yrf, data = drggf, family = gaussian)
# mod3gf <- gam(logwt ~ s(loglen, k=20) + obs_name + wavescale + lightstick + te(lat, lon, k = c(10,10)) + 
#                 weather + s(hbf), data = drggf, family = gaussian)
# mod4gf <- gam(logwt ~ s(loglen, k=20) + obs_name + wavescale + lightstick + te(lat, lon, mon, k = c(10,10,4)) + 
#                 weather + s(hbf) + yrf, data = drggf, family = gaussian)
# mod5gf <- gam(logwt ~ s(loglen, k=20) + obs_name + wavescale + lightstick + te(lat, lon, mon, k = c(10,10,4)) + 
#                 weather + s(hbf) + yrf + HkCourse, data = drggf, family = gaussian)
# mod5gfx <- gam(logwt ~ s(loglen, k=20) + s(obs_name, bs="re") + wavescale + lightstick + te(lat, lon, mon, k = c(10,10,4)) + 
#                  weather + s(hbf) + yrf + HkCourse, data = drggf, family = gaussian)
# mod6gfx <- gam(logwt ~ s(loglen, k=20) + s(obs_name, bs="re") + wavescale + te(lat, lon, mon, k = c(10,10,4)) + 
#                  weather + s(hbf) + yrf, data = drggf, family = gaussian)
# mod7gfx <- gam(logwt ~ s(loglen, k=20) + s(obs_name, bs="re") + wavescale + qtr + te(lat, lon, k = c(10,10)) + te(lat, lon, k = c(10,10), by = qtr) + 
#                  weather + s(hbf) + yrf, data = drggf, family = gaussian)
# mod7gfx_scat <- gam(logwt ~ s(loglen, k=20) + s(obs_name, bs="re") + wavescale + qtr + te(lat, lon, k = c(10,10)) + te(lat, lon, k = c(10,10), by = qtr) + 
#                       weather + s(hbf) + yrf, data = drggf, family = scat)
# # Add setid as an RE - very slow with 23000 rows
# mod8gfx <- gam(logwt ~ s(loglen, k=20) + s(obs_name, bs="re")  + s(setid, bs="re") + wavescale + lightstick + te(lat, lon, mon, k = c(10,10,4)) + 
#                  weather + s(hbf) + yrf + HkCourse, data = drggf, family = gaussian)
# 
# summary(mod5gfx)
# summary(mod8gfx)
# summary(drggf$lightstick)
# summary(drggf$HkCourse)
# summary(drggf$hbf)
# summary(drggf$wavescale)
# summary(drggf$wavenum)
# AIC(mod1gf,mod2gf,mod3gf,mod4gf,mod5gf,mod5gfx,mod6gfx,mod7gfx,mod7gfx_scat)
# 
# windows(20,20)
# plot.gam(mod7gfx, pages = 1, all.terms = T)
# hist(drggf$length)
# plot(drggf$loglen, drggf$logwt)
# table(drggf$length)
# table(drggf$loglen)

# plot all variables
hists_by_var <- function(dat) {
  hist(dat$length, nclass=100, main = "Length")
  hist(dat$loglen, nclass=100, main = "Log(length)")
  barplot(table(dat$wavescale), main = "Wave scale")
  barplot(table(dat$lightstick), main = "Light sticks")
  barplot(table(dat$weather), main = "Weather")
  hist(dat$hbf, breaks=seq(0:max(dat$hbf, na.rm = TRUE)), main = "HBF")
  barplot(table(dat$HkCourse), main = "HK Course")
  barplot(table(dat$Cluster), main = "Cluster")
  barplot(table(dat$baitType), main = "Bait type")
}
summary(drggf)

windows(12, 12); par(mfrow = c(3,3), oma=c(0,0,3,0))
hists_by_var(drggf); title("DRGGF", outer=TRUE)
savePlot("../figures3/DRGGF summary plots.png", type = "png")

windows(12, 12); par(mfrow = c(3,3), oma=c(0,0,3,0))
hists_by_var(drggm); title("DRGGM", outer=TRUE)
savePlot("../figures3/DRGGM summary plots.png", type = "png")

windows(12, 12); par(mfrow = c(3,3), oma=c(0,0,3,0))
hists_by_var(drgtf); title("DRGTF", outer=TRUE)
savePlot("../figures3/DRGTF summary plots.png", type = "png")

windows(12, 12); par(mfrow = c(3,3), oma=c(0,0,3,0))
hists_by_var(drgtm); title("DRGTM", outer=TRUE)
savePlot("../figures3/DRGTM summary plots.png", type = "png")

# Identify a random effect equivalent to the set variable

# In theory, set itself could be included as a random effect - what happens? (takes forever)
mod7gfx <- gam(logwt ~ s(loglen, k=20) + s(obs_name, bs="re") + wavescale + qtr + te(lat, lon, k = c(10,10)) + 
                 te(lat, lon, k = c(10,10), by = qtr) + weather + s(hbf) + yrf + s(trip_id, bs="re"), 
               data = drggf, family = gaussian)

# Use a $setid random effect? 
drggf$setid <- factor(drggf$setid)
table(table(drggf$setid))
sum(table(table(drggf$setid))) # 6535 sets
table(is.na(drggf$setid)) # 23309 rows
table(table(drggf$tripID))

table(dat$obs_name)
length(unique(drggf$obs_name))

