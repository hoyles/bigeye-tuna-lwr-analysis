

# WGG Females ----------------------------------------------
# choose one random sample from each (load the version from the previous run so as to stay consistent) 
drggf_r <- drggf %>% group_by(setid) %>% slice_sample(n=1)
trms <- c("wavescale", "lightstick", "weather", "HkCourse", "yrf", "loglen", "obs_name", "lon", "lat", "mon", "hbf")
drggf_r <- drggf_r[complete.cases(drggf_r[,trms]),]
drggf_ro <- drggf_r[!drggf_r$obs_name=="obs_065",] # Remove very large outlier 
drggf_ro <- drggf_ro[!drggf_ro$tripID=="2014100442916",]

dim(drggf_r)  
save.image()

rm(list=ls(pattern="mod[1234567890]rgf"))

# models using ML gaussian
mod1_test %<-%  gam(logwt ~ s(loglen, k=2) + obs_name + wavescale + lightstick + te(lat, lon, k = c(10,10)) + weather + s(hbf) + HkCourse + yrf, data = drggf_ro, family = gaussian, gamma = 1.4)
mod1_test2 %<-%  gam(logwt ~ s(loglen, k=20) + obs_name + wavescale + lightstick + te(lat, lon, k = c(10,10)) + weather + s(hbf) + HkCourse + yrf, data = drggf_ro, family = gaussian, gamma = 1.4)
windows()
plot(mod1_test, pages=1, all.terms=TRUE)
windows()
plot(mod1_test2, pages=1, all.terms=TRUE)

summary(mod1_test)
summary(mod1_test2)
AIC(mod1_test,mod1_test2)

mod1rgf_ml %<-%  gam(logwt ~ s(loglen, k=20) + obs_name + wavescale + lightstick + te(lat, lon, k = c(10,10)) + weather + s(hbf) + HkCourse + yrf, data = drggf_r, family = gaussian, method = 'ML', gamma = 1.4)
mod2rgf_ml %<-%  gam(logwt ~ s(loglen, k=20) + obs_name + wavescale + lightstick + te(lat, lon, k = c(10,10)) + weather + s(hbf) + yrf, data = drggf_r, family = gaussian, method = 'ML', gamma = 1.4)
mod3rgf_ml %<-%  gam(logwt ~ s(loglen, k=20) + obs_name + wavescale + lightstick + te(lat, lon, k = c(10,10)) + weather + s(hbf), data = drggf_r, family = gaussian, method = 'ML', gamma = 1.4)
mod4rgf_ml %<-%  gam(logwt ~ s(loglen, k=20) + obs_name + wavescale + lightstick + te(lat, lon, mon, k = c(10,10,4)) + weather + s(hbf) + yrf, data = drggf_r, family = gaussian, method = 'ML', gamma = 1.4)
mod5rgf_ml %<-%  gam(logwt ~ s(loglen, k=20) + obs_name + wavescale + lightstick + te(lat, lon, mon, k = c(10,10,4)) + weather + s(hbf) + yrf + HkCourse, data = drggf_r, family = gaussian, method = 'ML', gamma = 1.4)
mod4rgfx_ml %<-% gam(logwt ~ s(loglen, k=20) + s(obs_name, bs="re") + wavescale + lightstick + te(lat, lon, mon, k = c(10,10,4)) + weather + s(hbf) + yrf, data = drggf_r, family = gaussian, method = 'ML', gamma = 1.4)
mod5rgfx_ml %<-% gam(logwt ~ s(loglen, k=20) + s(obs_name, bs="re") + wavescale + lightstick + te(lat, lon, mon, k = c(10,10,4)) + weather + s(hbf) + yrf + HkCourse, data = drggf_r, family = gaussian, method = 'ML', gamma = 1.4)
mod6rgfx_ml %<-% gam(logwt ~ s(loglen, k=20) + s(obs_name, bs="re") + wavescale +              te(lat, lon, mon, k = c(10,10,4)) + weather + s(hbf) + yrf, data = drggf_r, family = gaussian, method = 'ML', gamma = 1.4)
mod9rgfx_ml %<-%  gam(logwt ~ s(loglen, k=20) + s(obs_name, bs="re") + wavescale +              te(lat, lon, mon, k = c(10,10,4))           + s(hbf) + yrf,          data = drggf_r, family = gaussian, method = 'ML', gamma = 1.4)
mod9rgfxy_ml %<-% gam(logwt ~ s(loglen, k=20) + s(obs_name, bs="re") + wavescale +              te(lat, lon, mon, k = c(10,10,4))           + s(hbf) + s(year, k=6), data = drggf_r, family = gaussian, method = 'ML', gamma = 1.4)
mod10rgfxy_ml %<-% gam(logwt ~ s(loglen, k=20) + s(obs_name, bs="re") +                         te(lat, lon, mon, k = c(10,10,4))           + s(hbf) + s(year, k=6), data = drggf_r, family = gaussian, method = 'ML', gamma = 1.4)
mod11rgfxy_ml %<-% gam(logwt ~ s(loglen, k=20) + s(obs_name, bs="re") +                         te(lat, lon, mon, k = c(10,10,4))           + s(hbf) + s(year, k=6) + s(lunar_illumination), data = drggf_r, family = gaussian, method = 'ML', gamma = 1.4)

# models using gcv.ubre
mod1rgf_gc %<-%  gam(logwt ~ s(loglen, k=20) + obs_name + wavescale + lightstick + te(lat, lon, k = c(10,10)) + weather + s(hbf) + HkCourse + yrf, data = drggf_r, family = gaussian, gamma = 1.4)
mod2rgf_gc %<-%  gam(logwt ~ s(loglen, k=20) + obs_name + wavescale + lightstick + te(lat, lon, k = c(10,10)) + weather + s(hbf) + yrf, data = drggf_r, family = gaussian, gamma = 1.4)
mod3rgf_gc %<-%  gam(logwt ~ s(loglen, k=20) + obs_name + wavescale + lightstick + te(lat, lon, k = c(10,10)) + weather + s(hbf), data = drggf_r, family = gaussian, gamma = 1.4)
mod4rgf_gc %<-%  gam(logwt ~ s(loglen, k=20) + obs_name + wavescale + lightstick + te(lat, lon, mon, k = c(10,10,4)) + weather + s(hbf) + yrf, data = drggf_r, family = gaussian, gamma = 1.4)
mod5rgf_gc %<-%  gam(logwt ~ s(loglen, k=20) + obs_name + wavescale + lightstick + te(lat, lon, mon, k = c(10,10,4)) + weather + s(hbf) + yrf + HkCourse, data = drggf_r, family = gaussian, gamma = 1.4)
mod4rgfx_gc %<-% gam(logwt ~ s(loglen, k=20) + s(obs_name, bs="re") + wavescale + lightstick + te(lat, lon, mon, k = c(10,10,4)) + weather + s(hbf) + yrf, data = drggf_r, family = gaussian, gamma = 1.4)
mod5rgfx_gc %<-% gam(logwt ~ s(loglen, k=20) + s(obs_name, bs="re") + wavescale + lightstick + te(lat, lon, mon, k = c(10,10,4)) + weather + s(hbf) + yrf + HkCourse, data = drggf_r, family = gaussian, gamma = 1.4)
mod6rgfx_gc %<-% gam(logwt ~ s(loglen, k=20) + s(obs_name, bs="re") + wavescale +              te(lat, lon, mon, k = c(10,10,4)) + weather + s(hbf) + yrf, data = drggf_r, family = gaussian, gamma = 1.4)
mod9rgfx_gc %<-% gam(logwt ~ s(loglen, k=20) + s(obs_name, bs="re") + wavescale +              te(lat, lon, mon, k = c(10,10,4))           + s(hbf) + yrf, data = drggf_r, family = gaussian, gamma = 1.4)
mod9rgfxy_gc %<-% gam(logwt ~ s(loglen, k=20) + s(obs_name, bs="re") + wavescale +              te(lat, lon, mon, k = c(10,10,4))           + s(hbf) + s(year, k=6), data = drggf_r, family = gaussian, gamma = 1.4)
mod9rgfxy_gc_scat %<-% gam(logwt ~ s(loglen, k=20) + s(obs_name, bs="re") + wavescale +              te(lat, lon, mon, k = c(10,10,4))           + s(hbf) + s(year, k=6), data = drggf_r, family = scat, gamma = 1.4)

# models using ML scat
# mod4rgfx_ml_scat %<-% dogam(dat=drggf_r, llm_cc = 'cc', hkc=FALSE,  fam = 'scat')
# mod5rgfx_ml_scat %<-% dogam(dat=drggf_r, llm_cc = 'cc',             fam = 'scat')
# mod6rgfx_ml_scat %<-% dogam(dat=drggf_r, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, fam = 'scat')
# mod9rgfx_ml_scat %<-% dogam(dat=drggf_r, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, fam = 'scat')
# mod9rgfxy_ml_scat %<-% dogam(dat=drggf_r, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, yrf=FALSE, yr=TRUE, fam = 'scat')
# mod9rgfxy_ml_scat_rot %<-% dogam(dat=drggf_r, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, yrf=FALSE, yr=TRUE, fam = 'scat')
# mod10rgfxy_ml_scat %<-% dogam(dat=drggf_r, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, yrf=FALSE, yr=TRUE, wv=FALSE, fam = 'scat')

mod4rgfxo_ml %<-% dogam(dat=drggf_ro, llm_cc = 'cc', hkc=FALSE)
mod5rgfxo_ml %<-% dogam(dat=drggf_ro, llm_cc = 'cc')
mod6rgfxo_ml %<-% dogam(dat=drggf_ro, llm_cc = 'cc', lstk=FALSE, hkc=FALSE)
mod9rgfxo_ml %<-% dogam(dat=drggf_ro, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE)
mod9rgfxyo_ml %<-% dogam(dat=drggf_ro, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, yrf=FALSE, yr=TRUE)
mod10rgfxyo_ml %<-% dogam(dat=drggf_ro, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, yrf=FALSE, yr=TRUE, wv=FALSE)
mod10rgfxo_ml %<-%  dogam(dat=drggf_ro, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, yrf=TRUE, yr=FALSE, wv=FALSE)

mod4rgfx_ml_scat %<-% dogam(dat=drggf_ro, llm_cc = 'cc', hkc=FALSE,  fam = 'scat')
mod5rgfx_ml_scat %<-% dogam(dat=drggf_ro, llm_cc = 'cc',             fam = 'scat')
mod6rgfx_ml_scat %<-% dogam(dat=drggf_ro, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, fam = 'scat')
mod9rgfx_ml_scat %<-% dogam(dat=drggf_ro, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, fam = 'scat')
mod9rgfxy_ml_scat %<-%     dogam(dat=drggf_ro, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, yrf=FALSE, yr=TRUE, fam = 'scat')
mod9rgfxy_ml_scat_rot %<-% dogam(dat=drggf_ro, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, yrf=FALSE, yr=TRUE, fam = 'scat')
mod10rgfxy_ml_scat %<-% dogam(dat=drggf_ro, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, yrf=FALSE, yr=TRUE, wv=FALSE, fam = 'scat')
mod10rgfx_ml_scat %<-%  dogam(dat=drggf_ro, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, yrf=TRUE, yr=FALSE, wv=FALSE, fam = 'scat')

AIC(mod9rgfxy_ml_scat_rot, mod9rgfxy_ml_scat)

terms(mod4rgfx_ml)
attr(mod4rgfx_ml$terms, "term.labels")
attr(mod4rgfx_mltst_cc$terms, "term.labels")
attr(mod4rgfx_mltst_cs$terms, "term.labels")
attr(mod4rgfx_mltst$terms, "term.labels")

AIC(mod4rgfx_ml,mod4rgfx_mltst_cc,mod4rgfx_mltst_cs,mod4rgfx_mltst)
# Formula:
#   logwt ~ s(loglen, k = 20) + s(obs_name, bs = "re") + te(lat, 
#   lon, mon, k = c(10, 10, 4), bs = c("cs", "cs", "cs")) + wavescale + 
#   lightstick + weather + s(hbf, k = 10) + yrf
a <- AIC(mod4rgfx_ml,mod5rgfx_ml,mod6rgfx_ml,mod9rgfx_ml,mod9rgfxy_ml,mod10rgfxy_ml)
cbind(a, a$AIC-min(a$AIC))

a <- AIC(mod4rgfxo_ml,mod5rgfxo_ml,mod6rgfxo_ml,mod9rgfxo_ml,mod9rgfxyo_ml,mod10rgfxyo_ml,mod10rgfxo_ml)
cbind(a, a$AIC-min(a$AIC))

a <- AIC(mod1rgf_gc, mod2rgf_gc,mod3rgf_gc,mod4rgf_gc,mod5rgf_gc,mod4rgfx_gc,mod5rgfx_gc,mod6rgfx_gc,mod9rgfx_gc,mod9rgfxy_gc)
cbind(a, a$AIC-min(a$AIC))

a <- AIC(mod4rgfx_ml_scat, mod5rgfx_ml_scat,mod6rgfx_ml_scat,mod9rgfx_ml_scat,mod9rgfxy_ml_scat,mod10rgfxy_ml_scat,mod10rgfx_ml_scat)
cbind(a, a$AIC-min(a$AIC))

cmp.css <- complete.cases(drggf_r[,attr(mod5rgfx_ml_scat$terms, "term.labels")])
sum(cmp.css)
as.data.frame(drggf_r[!cmp.css,attr(mod5rgfx_ml_scat$terms, "term.labels")])
drggf_r2 <- drggf_r[cmp.css,]

summary(mod4rgfx_ml_scat)
summary(mod5rgfx_ml_scat)
summary(mod6rgfx_ml_scat)
summary(mod9rgfx_ml_scat)
summary(mod9rgfxy_ml_scat)
summary(mod10rgfxy_ml_scat)

attr(mod4rgfx_ml_scat$terms, "term.labels")
attr(mod5rgfx_ml_scat$terms, "term.labels")
attr(mod6rgfx_ml_scat$terms, "term.labels")
attr(mod9rgfx_ml_scat$terms, "term.labels")
attr(mod9rgfxy_ml_scat$terms, "term.labels")
attr(mod10rgfxy_ml_scat$terms, "term.labels")


modlist <- mget(ls(pattern="mod[1234567890]rgf_ml"))
sapply(modlist, function(mod) {data.frame(r.sq = (summary(mod)$r.sq), AIC = AIC(mod))})

windows(20,20)
plot.gam(mod9rgfx_ml_scat, pages = 1, all.terms = T)
hist(drggf_r$length)
plot(drggf_r$loglen, drggf_r$logwt)
windows(20,15);
plot.gam(mod9rgfxy_ml_scat, pages = 1, all.terms = T, main = "mod9rgfxy_ml_scat")
windows(20,15);
plot.gam(mod9rgfxy_ml_scat, select=5, ylim=c(-0.15, 0.15))
windows(20,15);
plot.gam(mod9rgfxy_ml_scat, select=4, ylim=c(-0.15, 0.15))
abline(h=0, col = 2)

save(mod1rgf_ml,mod2rgf_ml,mod3rgf_ml, mod4rgf_ml,mod5rgf_ml,
     mod4rgfx_ml,mod5rgfx_ml,mod6rgfx_ml,
     mod9rgfx_ml,mod9rgfxy_ml,mod10rgfxy_ml, 
     file = "../resubmit/store/mods_ggf_ml_gauss.RData")
rm(mod1rgf_ml,mod2rgf_ml,mod3rgf_ml, mod4rgf_ml,mod5rgf_ml,
   mod4rgfx_ml,mod5rgfx_ml,mod6rgfx_ml,
   mod9rgfx_ml,mod9rgfxy_ml,mod10rgfxy_ml)
load(file = "./resubmit/store/mods_ggf_ml_gauss.RData")

save(mod4rgfxo_ml,mod5rgfxo_ml,mod6rgfxo_ml,
     mod9rgfxo_ml,mod9rgfxyo_ml,mod10rgfxyo_ml,mod10rgfxo_ml, 
     file = "../resubmit/store/mods_ggf_ml_gauss_cleanobs.RData")
rm(mod4rgfxo_ml,mod5rgfxo_ml,mod6rgfxo_ml,
   mod9rgfxo_ml,mod9rgfxyo_ml,mod10rgfxyo_ml,mod10rgfxo_ml)
load(file = "./resubmit/store/mods_ggf_ml_gauss_cleanobs.RData")

save(mod1rgf_gc, mod2rgf_gc, mod3rgf_gc, mod4rgf_gc,mod5rgf_gc,
     mod4rgfx_gc,mod5rgfx_gc,mod6rgfx_gc,mod9rgfx_gc,
     mod9rgfxy_gc,mod9rgfxy_gc_scat,
     file = "../resubmit/store/mods_ggf_gc_gauss.RData")
rm(mod1rgf_gc, mod2rgf_gc, mod3rgf_gc, mod4rgf_gc,mod5rgf_gc,
   mod4rgfx_gc,mod5rgfx_gc,mod6rgfx_gc,mod9rgfx_gc,
   mod9rgfxy_gc,mod9rgfxy_gc_scat)

infl <- influence.gam(mod10rgfxy_ml_scat)
ichk <- drggf_r
ichk$infl <- infl
ichk$obs_name <- factor(ichk$obs_name)
a <- aggregate(infl ~ obs_name, data = ichk, FUN=mean)
a[order(a$infl),]
a <- aggregate(infl ~ obs_name, data = ichk, FUN=sum)
a[order(a$infl),]
aggregate(infl ~ obs_name, data = ichk, FUN=sum)
aggregate(infl ~ obs_name, data = ichk, FUN=length)
windows()
plot(aggregate(infl ~ obs_name, data = ichk, FUN=mean))
plot(aggregate(infl ~ obs_name, data = ichk, FUN=sum), ylim = c(0, 6))
a <- coef(mod10rgfxy_ml_scat)
names(a)
a.obs <- a[grep("obs_name", names(a))]
ob <- data.frame(nms = levels(drggf_r$obs_name))
a.obs[order(a.obs)]
obs.re <- extract_ranef(mod10rgfxy_ml_scat)
obs.re[order(-obs.re$value),]

linform <- as.formula(logwt ~ loglen + s(obs_name, bs="re") + te(lon, lat, mon, k = c(10, 10, 4), bs = c("cr", "cr", "cc")) + s(hbf, k = 15) + yrf)
mod10rgtfx_ml_scat_lin %<-%  gam(linform, data = drgtf_ro, family = scat, method = 'ML', gamma = 1.4) # same as best model except linear loglen
mod10rgtfx_ml_scat_lin_gaus %<-%  gam(linform, data = drgtf_ro, family = 'gaussian', method = 'ML', gamma = 1.4) # same as prev model except Gaussian

# mod10rgfxy_ml_scat_lin %<-% dogam(dat=drggf_ro, llm_cc = 'cc', k_lgln=2, lstk=FALSE, 
#         hkc=FALSE, wth=FALSE, yrf=FALSE, yr=TRUE, wv=FALSE, fam = 'scat') 
#mod10rgfxyo_ml_lin %<-% dogam(dat=drggf_ro, llm_cc = 'cc', k_lgln=2, lstk=FALSE, 
#         hkc=FALSE, wth=FALSE, yrf=FALSE, yr=TRUE, wv=FALSE) # same as best model except gaussian & linear loglen
AIC(mod10rgfxy_ml_scat_lin, mod10rgfxy_ml_scat)

plot_loglen_vslinear(mod=mod10rgfxy_ml_scat, modlin=mod10rgfxy_ml_scat_lin, dat = drggf_ro, direc="./figures3/", modn="mod10rgfxyo_ml_scat_linear")
plot_loglen_vslinear(mod=mod10rgfxyo_ml, modlin=mod10rgfxyo_ml_lin, dat = drggf_ro, direc="./figures3/", modn="mod10rgfxyo_ml_linear")
# Figure 13
plot_loglen_vslinear(mod=mod10rgfxo_ml, modlin=mod10rgfxy_ml_scat, dat = drggf_ro, direc="../figures_resubmit/", modn="mod10rgfxyo_ml_gauss_v_scat")



doplots(mod=mod10rgfxy_ml_scat, dat = drggf_ro, direc="./figures3/", modn="mod10rgfxyo_ml_scat",
        diag=T, loglen=T, maps=T, hbf=T, year=T, yrf=F, wv=F, obs=T, comprehensive=TRUE)
doplots(mod=mod10rgfx_ml_scat, dat = drggf_ro, direc="./figures3/", modn="mod10rgfx_ml_scat",
        diag=T, loglen=T, maps=T, hbf=T, year=F, yrf=T, wv=F, obs=T, comprehensive=TRUE)

doplots(mod=mod10rgfxyo_ml, dat = drggf_ro, direc="./figures3/", modn="mod10rgfxyo_ml_gauss",
        diag=T, loglen=T, maps=T, hbf=T, year=T, yrf=F, wv=F, obs=T)

save(mod4rgfx_ml_scat,mod5rgfx_ml_scat,mod6rgfx_ml_scat,
     mod9rgfx_ml_scat,mod9rgfxy_ml_scat,mod9rgfxy_ml_scat_rot, mod10rgfxy_ml_scat_lin,mod10rgfxyo_ml_lin,
     mod10rgfxy_ml_scat, mod10rgfx_ml_scat, file = "../resubmit/store/mods_ggf_ml_scat_cleanobs.RData")
rm(mod4rgfx_ml_scat,mod5rgfx_ml_scat,mod6rgfx_ml_scat,
   mod9rgfx_ml_scat,mod9rgfxy_ml_scat,mod9rgfxy_ml_scat_rot,mod10rgfxy_ml_scat_lin,mod10rgfxyo_ml_lin,
   mod10rgfxy_ml_scat, mod10rgfx_ml_scat)
load("./resubmit/store/mods_ggf_ml_scat_cleanobs.RData")
load("./resubmit/store/mods_ggf_ml_scat.RData")

save.image()

#######################-------------------------------------------------------------
# WGG Males
# choose one random sample from each
drggm_r <- drggm %>% group_by(setid) %>% slice_sample(n=1)
dim(drggm_r)  
trms <- c("wavescale", "lightstick", "weather", "HkCourse", "yrf", "loglen", "obs_name", "lon", "lat", "mon", "hbf")
cmp.css <- complete.cases(drggm_r[,trms])
drggm_r <- drggm_r[cmp.css,]
drggm_ro <- drggm_r[drggm_r$obs_name != "obs_065",]
drggm_ro <- drggm_ro[!drggm_ro$tripID=="2014100442916",]

# models using ML scat
mod4rgmx_ml_scat %<-% dogam(dat=drggm_r, llm_cc = 'cc', hkc=FALSE,  fam = 'scat')
mod5rgmx_ml_scat %<-% dogam(dat=drggm_r, llm_cc = 'cc',             fam = 'scat')
mod6rgmx_ml_scat %<-% dogam(dat=drggm_r, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, fam = 'scat')
mod9rgmx_ml_scat %<-% dogam(dat=drggm_r, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, fam = 'scat')
mod9rgmxy_ml_scat %<-% dogam(dat=drggm_r, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, yrf=FALSE, yr=TRUE, fam = 'scat')
mod9rgmxy_ml_scat_rot %<-% dogam(dat=drggm_r, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, yrf=FALSE, yr=TRUE, fam = 'scat')
mod10rgmxy_ml_scat %<-% dogam(dat=drggm_r, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, yrf=FALSE, yr=TRUE, wv=FALSE, fam = 'scat')

mod4rgmx_ml_scat %<-% dogam(dat=drggm_ro, llm_cc = 'cc', hkc=FALSE,  fam = 'scat')
mod5rgmx_ml_scat %<-% dogam(dat=drggm_ro, llm_cc = 'cc',             fam = 'scat')
mod6rgmx_ml_scat %<-% dogam(dat=drggm_ro, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, fam = 'scat')
mod9rgmx_ml_scat %<-% dogam(dat=drggm_ro, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, fam = 'scat')
mod9rgmxy_ml_scat %<-% dogam(dat=drggm_ro, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, yrf=FALSE, yr=TRUE, fam = 'scat')
mod9rgmxy_ml_scat_rot %<-% dogam(dat=drggm_ro, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, yrf=FALSE, yr=TRUE, fam = 'scat')
mod10rgmxy_ml_scat %<-% dogam(dat=drggm_ro, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, yrf=FALSE, yr=TRUE, wv=FALSE, fam = 'scat')

infl <- influence.gam(mod4rgmx_ml_scat)
ichk <- drggm_r
ichk$infl <- infl
ichk$obs_name <- factor(ichk$obs_name)
a <- aggregate(infl ~ obs_name, data = ichk, FUN=mean)
a[order(a$infl),]
a <- aggregate(infl ~ obs_name, data = ichk, FUN=sum)
a[order(a$infl),]
aggregate(infl ~ obs_name, data = ichk, FUN=sum)
aggregate(infl ~ obs_name, data = ichk, FUN=length)
windows()
plot(aggregate(infl ~ obs_name, data = ichk, FUN=mean))
plot(aggregate(infl ~ obs_name, data = ichk, FUN=sum), ylim = c(0, 6))
a <- coef(mod4rgmx_ml_scat)
names(a)
a.obs <- a[grep("obs_name", names(a))]
ob <- data.frame(nms = levels(drggm_r$obs_name))
a.obs[order(a.obs)]
obs.re <- extract_ranef(mod4rgmx_ml_scat)
a <- obs.re[order(-obs.re$value),]
plot(a$value)
drggm_r[drggm_r$obs_name=="obs_065",]

save(mod4rgmx_ml_scat,mod5rgmx_ml_scat,mod6rgmx_ml_scat,
     mod9rgmx_ml_scat,mod9rgmxy_ml_scat,mod9rgmxy_ml_scat_rot,
     mod10rgmxy_ml_scat, file = "../resubmit/store/mods_ggm_ml_scat_cleanobs.RData")
rm(mod4rgmx_ml_scat,mod5rgmx_ml_scat,mod6rgmx_ml_scat,
     mod9rgmx_ml_scat,mod9rgmxy_ml_scat,mod9rgmxy_ml_scat_rot,
     mod10rgmxy_ml_scat)
load(file = "./resubmit/store/mods_ggm_ml_scat_cleanobs.RData")
load(file = "./resubmit/store/mods_ggm_ml_scat.RData")

a <- AIC(mod4rgmx_ml_scat,mod5rgmx_ml_scat,mod6rgmx_ml_scat,mod9rgmx_ml_scat,mod9rgmxy_ml_scat,mod9rgmxy_ml_scat_rot,
         mod10rgmxy_ml_scat)
cbind(a, a$AIC-min(a$AIC))

a <- summary(mod4rgmx_ml_scat)
names(summary(mod4rgmx_ml_scat))
mod4rgmx_ml_scat$coefficients
length(unique(drggm_r$obs_name))

doplots(mod=mod4rgmx_ml_scat, dat = drggm_ro, direc="./figures3/", modn="mod4rgmxo_ml_scat",
        diag=T, loglen=T, maps=T, hbf=T, lightstick=T, weather=T, year=F, yrf=T, wv=T, obs=T, comprehensive=TRUE)

#######################-------------------------------------------------------------
# WGT Females
# choose one random sample from each
drgtf_r <- drgtf %>% group_by(setid) %>% slice_sample(n=1)
trms <- c("wavescale", "lightstick", "weather", "HkCourse", "yrf", "loglen", "obs_name", "lon", "lat", "mon", "hbf")
cmp.css <- complete.cases(drgtf_r[,trms])
drgtf_r <- drgtf_r[cmp.css,]

mod10rgtfoo_ml_scat %<-% dogam(dat=drgtf_r, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, yrf=TRUE, yr=FALSE,  wv=FALSE, fam = 'scat')

obs.re <- extract_ranef(mod10rgtfoo_ml_scat) 
a <- obs.re[order(obs.re$value),] 
plot(a$value) # remove 2 highest and 1 lowest obs_091, obs_175, obs_113
drgtf_ro <- drgtf_r[!drgtf_r$obs_name %in% c("obs_091", "obs_175", "obs_113"),] 

# Resubmit - check how far these were from the distribution
re_mean <- mean(a$value)
re_sd <- sd(a$value)
a$z_score <- (a$value - re_mean) / re_sd

# Check z-scores for removed observers
a[a$group %in% c("obs_091", "obs_175", "obs_113"), c("group", "value", "z_score")]
print(a[!a$group %in% c("obs_091", "obs_175", "obs_113"), c("group", "value", "z_score")], n=120)

# end of resubmit

# check data in drgtf_r and drgtf_ro
dim(drgtf_r) # 11687 rows
table(drgtf_r$obs_name) # includes obs_091, obs_175, obs_113
table(drgtf_ro$obs_name) # doesn't include them

a <- as.character(drgtf_r$obs_name)
a[is.na(a)] <- "xxx"
drgtf_r$obs_name <- factor(a)
dim(drgtf_r) 
table(drgtf_r$obs_name)
length(unique(drgtf_r$obs_name))

ls(pattern="mod[[:alnum:]]rg")

mod4rgtfx_ml_scat  %<-% dogam(dat=drgtf_ro, llm_cc = 'cc', hkc=FALSE,  fam = 'scat')
mod5rgtfx_ml_scat  %<-% dogam(dat=drgtf_ro, llm_cc = 'cc',             fam = 'scat')
mod6rgtfx_ml_scat  %<-% dogam(dat=drgtf_ro, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, fam = 'scat')
mod9rgtfx_ml_scat  %<-% dogam(dat=drgtf_ro, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, fam = 'scat')
mod9rgtfxy_ml_scat %<-% dogam(dat=drgtf_ro, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, yrf=FALSE, yr=TRUE, fam = 'scat')
mod10rgtfx_ml_scat %<-% dogam(dat=drgtf_ro, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, yrf=TRUE, yr=FALSE,  wv=FALSE, fam = 'scat')
# resubmit
mod10rgtfx_ml_gaus %<-% dogam(dat=drgtf_ro, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, yrf=TRUE, yr=FALSE,  wv=FALSE, fam = 'gaussian')
save(mod10rgtfx_ml_gaus, file="../resubmit/store/mod10rgtfx_ml_gaus.RData")
mod10rgtfxy_ml_scat %<-% dogam(dat=drgtf_ro, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, yrf=FALSE, yr=TRUE, wv=FALSE, fam = 'scat')
# mod11rgtfx_ml_scat %<-% dogam(dat=drgtf_r, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, yrf=TRUE, yr=FALSE, wv=FALSE,  fam = 'scat')
# mod11rgtfxy_ml_scat %<-%dogam(dat=drgtf_r, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, yrf=FALSE, yr=TRUE, wv=FALSE, fam = 'scat')

linform <- as.formula(logwt ~ loglen + s(obs_name, bs="re") + te(lon, lat, mon, k = c(10, 10, 4), bs = c("cr", "cr", "cc")) + s(hbf, k = 15) + yrf)
mod10rgtfx_ml_scat_lin %<-%  gam(linform, data = drgtf_ro, family = scat, method = 'ML', gamma = 1.4)

mod11rgtfx_ml_scat %<-%  gam(logwt ~ s(loglen, k = 20) + s(obs_name, bs="re") + te(lon, lat, mon, k = c(10, 10, 4), bs = c("cr", "cr", "cc")) + s(hbf, k = 15) + s(lunar_illumination) + yrf, data = drgtf_ro, family = scat, method = 'ML', gamma = 1.4)
drgtf_ro$Cluster <- factor(drgtf_ro$Cluster)
mod12rgtfx_ml_scat %<-%  gam(logwt ~ s(loglen, k = 20) + s(obs_name, bs="re") + te(lon, lat, mon, k = c(10, 10, 4), bs = c("cr", "cr", "cc")) + s(hbf, k = 15) + Cluster + yrf, data = drgtf_ro, family = scat, method = 'ML', gamma = 1.4)

AIC(mod10rgtfx_ml_scat, mod11rgtfx_ml_scat, mod12rgtfx_ml_scat)
summary(mod11rgtfx_ml_scat) # Lunar illumination has no effect
summary(mod12rgtfx_ml_scat) # Cluster 

windows()
plot.gam(mod11rgtfx_ml_scat, pages=1)
windows()
plot.gam(mod12rgtfx_ml_scat, pages=1, all.terms=TRUE)
##############
#REML
mod4rgtfx_ml_scat_rml  %<-% dogam(dat=drgtf_ro, llm_cc = 'cc', hkc=FALSE,  fam = 'scat', meth="REML")
mod5rgtfx_ml_scat_rml  %<-% dogam(dat=drgtf_ro, llm_cc = 'cc',             fam = 'scat', meth="REML")
mod6rgtfx_ml_scat_rml  %<-% dogam(dat=drgtf_ro, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, fam = 'scat', meth="REML")
mod9rgtfx_ml_scat_rml  %<-% dogam(dat=drgtf_ro, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, fam = 'scat', meth="REML")
mod9rgtfxy_ml_scat_rml %<-% dogam(dat=drgtf_ro, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, yrf=FALSE, yr=TRUE, fam = 'scat', meth="REML")
mod10rgtfx_ml_scat_rml %<-% dogam(dat=drgtf_ro, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, yrf=TRUE, yr=FALSE,  wv=FALSE, fam = 'scat', meth="REML")
# resubmit
mod10rgtfx_ml_gaus_rml %<-% dogam(dat=drgtf_ro, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, yrf=TRUE, yr=FALSE,  wv=FALSE, fam = 'gaussian', meth="REML")
save(mod10rgtfx_ml_gaus_rml, file="../resubmit/store/mod10rgtfx_ml_gaus_rml.RData")
mod10rgtfxy_ml_scat_rml %<-% dogam(dat=drgtf_ro, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, yrf=FALSE, yr=TRUE, wv=FALSE, fam = 'scat', meth="REML")
# mod11rgtfx_ml_scat %<-% dogam(dat=drgtf_r, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, yrf=TRUE, yr=FALSE, wv=FALSE,  fam = 'scat')
# mod11rgtfxy_ml_scat %<-%dogam(dat=drgtf_r, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, yrf=FALSE, yr=TRUE, wv=FALSE, fam = 'scat')

linform <- as.formula(logwt ~ loglen + s(obs_name, bs="re") + te(lon, lat, mon, k = c(10, 10, 4), bs = c("cr", "cr", "cc")) + s(hbf, k = 15) + yrf)
mod10rgtfx_ml_scat_lin_rml %<-%  gam(linform, data = drgtf_ro, family = scat, method = 'REML', gamma = 1.4)
AIC(mod10rgtfx_ml_scat_rml, mod10rgtfx_ml_scat_lin_rml)
AIC(mod10rgtfx_ml_scat_rml) - AIC(mod10rgtfx_ml_scat_lin_rml)
summary(mod10rgtfx_ml_scat_lin_rml)
str(drgtf_ro)
################

# resubmit: diagnostics of scat versus gaussian
summary(mod10rgtfx_ml_gaus)
windows(); par(mfrow=c(2,2))
plot(mod10rgtfx_ml_gaus)
gam.check(mod10rgtfx_ml_gaus)
gam.check(mod10rgtfx_ml_scat)

results <- compare_distributions(
  mod_gauss = mod10rgtfx_ml_gaus,
  mod_scat = mod10rgtfx_ml_scat,
  output_dir = "./resubmit/"
)

# resubmit: compare analysis without outliers #############
mod10rgtfx_o_ml_scat %<-% dogam(dat=drgtf_r, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, yrf=TRUE, yr=FALSE,  wv=FALSE, fam = 'scat')
mod10rgtmx_o_ml_scat %<-% dogam(dat=drgtm_r, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, yrf=TRUE, yr=FALSE,  wv=FALSE, fam = 'scat')

# Representative lengths
pred_lengths <- c(80, 100, 120, 140, 160)

# Prediction data - standardised covariates
newdata <- data.frame(
  loglen = log(pred_lengths),
  lat = median(drgtf_ro$lat),
  lon = median(drgtf_ro$lon),
  mon = 6,
  hbf = median(drgtf_ro$hbf),
  yrf = levels(drgtf_ro$yrf)[11],
  obs_name = levels(drgtf_ro$obs_name)[145]
)

# Female predictions
pred_f_clean <- predict(mod10rgtfx_ml_scat, newdata, exclude = "s(obs_name)")
pred_f_full <- predict(mod10rgtfx_o_ml_scat, newdata, exclude = "s(obs_name)")

# Male predictions (adjust newdata if needed)
pred_m_clean <- predict(mod10rgtmx_ml_scat, newdata, exclude = "s(obs_name)")
pred_m_full <- predict(mod10rgtmx_o_ml_scat, newdata, exclude = "s(obs_name)")

# Results
results <- data.frame(
  length_cm = pred_lengths,
  female_diff_kg = exp(pred_f_full) - exp(pred_f_clean),
  female_diff_pct = 100 * (exp(pred_f_full) - exp(pred_f_clean)) / exp(pred_f_clean),
  male_diff_kg = exp(pred_m_full) - exp(pred_m_clean),
  male_diff_pct = 100 * (exp(pred_m_full) - exp(pred_m_clean)) / exp(pred_m_clean)
)

print(results)

# End of resubmit outliers #####################

# resubmit check k
windows(); par(mfrow=c(2,2))
gam.check(mod10rgtfx_ml_scat)
mod10rgtfx_k1_ml_scat %<-% gam(logwt ~ s(loglen, k = 20) + s(obs_name, bs = "re") + te(lon,lat, mon, k = c(10, 10, 4), bs = c("cr", "cr", "cc")) + s(hbf, k = 15) + yrf, dat=drgtf_ro, fam = 'scat', gamma=1.4, method='ML')
mod10rgtfx_k_ml_scat %<-% gam(logwt ~ s(loglen, k = 20) + s(obs_name, bs = "re") + te(lon,lat, mon, k = c(20, 20, 8), bs = c("cr", "cr", "cc")) + s(hbf, k = 15) + yrf, dat=drgtf_ro, fam = 'scat', gamma=1.4, method='ML')
mod10rgtfx_k2_ml_scat %<-% gam(logwt ~ s(loglen, k = 20) + s(obs_name, bs = "re") + te(lon,lat, mon, k = c(10, 10, 8), bs = c("cr", "cr", "cc")) + s(hbf, k = 15) + yrf, dat=drgtf_ro, fam = 'scat', gamma=1.4, method='ML')
mod10rgtfx_k3_ml_scat %<-% gam(logwt ~ s(loglen, k = 20) + s(obs_name, bs = "re") + te(lon,lat, mon, k = c(15, 15, 6), bs = c("cr", "cr", "cc")) + s(hbf, k = 15) + yrf, dat=drgtf_ro, fam = 'scat', gamma=1.4, method='ML')
mod10rgtfx_k4_ml_scat %<-% gam(logwt ~ s(loglen, k = 20) + s(obs_name, bs = "re") + te(lon,lat, mon, k = c(20, 10, 4), bs = c("cr", "cr", "cc")) + s(hbf, k = 15) + yrf, dat=drgtf_ro, fam = 'scat', gamma=1.4, method='ML')
mod10rgtfx_k5_ml_scat %<-% gam(logwt ~ s(loglen, k = 20) + s(obs_name, bs = "re") + te(lon,lat, mon, k = c(10, 20, 4), bs = c("cr", "cr", "cc")) + s(hbf, k = 15) + yrf, dat=drgtf_ro, fam = 'scat', gamma=1.4, method='ML')

system.time({
  mod10rgtfx_ml_scat <- dogam(dat=drgtf_ro, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, yrf=TRUE, yr=FALSE,  
  wv=FALSE, fam ='scat')
})

system.time({
  mod10b_rgtfx_ml_scat <- dogam(dat=drgtf_ro, llm_cc = 'cc', lstk=FALSE, llm_k = "c(15,15,6)", hkc=FALSE, wth=FALSE, yrf=TRUE, yr=FALSE,  
  wv=FALSE, fam ='scat')
})
system.time({
  mod10a_rgtfx_ml_scat <- dogam(dat=drgtf_ro, llm_cc = 'cc', lstk=FALSE, llm_k = "c(8,8,4)", hkc=FALSE, wth=FALSE, yrf=TRUE, yr=FALSE,  
  wv=FALSE, fam ='scat')
})

system.time({
  mod10b_rgtfx_ml_scat <- dogam(dat=drgtf_ro, llm_cc = 'cc', lstk=FALSE, llm_k = "c(20,20,8)", hkc=FALSE, wth=FALSE, yrf=TRUE, yr=FALSE,  
  wv=FALSE, fam ='scat')
})

# Rerun alternative models with higher te k values
mod4a_rgtfx_ml_scat  %<-% dogam(dat=drgtf_ro, llm_cc = 'cc', llm_k = "c(15,15,6)", hkc=FALSE,  fam = 'scat')
mod5a_rgtfx_ml_scat  %<-% dogam(dat=drgtf_ro, llm_cc = 'cc', llm_k = "c(15,15,6)",             fam = 'scat')
mod6a_rgtfx_ml_scat  %<-% dogam(dat=drgtf_ro, llm_cc = 'cc', lstk=FALSE, llm_k = "c(15,15,6)", hkc=FALSE, fam = 'scat')
mod9a_rgtfx_ml_scat  %<-% dogam(dat=drgtf_ro, llm_cc = 'cc', lstk=FALSE, llm_k = "c(15,15,6)", hkc=FALSE, wth=FALSE, fam = 'scat')
mod9a_rgtfxy_ml_scat %<-% dogam(dat=drgtf_ro, llm_cc = 'cc', lstk=FALSE, llm_k = "c(15,15,6)", hkc=FALSE, wth=FALSE, yrf=FALSE, yr=TRUE, fam = 'scat')
mod10a_rgtfx_ml_scat %<-% dogam(dat=drgtf_ro, llm_cc = 'cc', lstk=FALSE, llm_k = "c(15,15,6)", hkc=FALSE, wth=FALSE, yrf=TRUE, yr=FALSE,  wv=FALSE, fam = 'scat')

AIC(mod4a_rgtfx_ml_scat, mod5a_rgtfx_ml_scat, mod6a_rgtfx_ml_scat,
    mod9a_rgtfx_ml_scat, mod9a_rgtfxy_ml_scat, mod10a_rgtfx_ml_scat)
# Compare with AIC for models with k=c(10,10,4)
AIC(mod4rgtfx_ml_scat, mod5rgtfx_ml_scat, mod6rgtfx_ml_scat,
    mod9rgtfx_ml_scat, mod9rgtfxy_ml_scat, mod10rgtfx_ml_scat, mod10rgtfxy_ml_scat)

save()

# Compare drgtf_ro with mod10rgtfx_ml_scat$model
names(drgtf_ro)
nm <- names(mod10rgtfx_ml_scat$model)
dim(drgtf_ro)
dim(drgtf_r)
dim(mod10rgtfx_ml_scat$model)

gam.check(mod10rgtfx_k1_ml_scat)
gam.check(mod10rgtfx_k_ml_scat)
gam.check(mod10rgtfx_k2_ml_scat)
gam.check(mod10rgtfx_k3_ml_scat)
gam.check(mod10rgtfx_k4_ml_scat)
gam.check(mod10rgtfx_k5_ml_scat)
AIC(mod10rgtfx_k1_ml_scat,
    mod10rgtfx_k_ml_scat,
    mod10rgtfx_k2_ml_scat,
    mod10rgtfx_k3_ml_scat,
    mod10rgtfx_k4_ml_scat,
    mod10rgtfx_k5_ml_scat)

# Check if the data is the same in mod10rgtfx_ml_scat and mod10rgtfx_k1_ml_scat
names(mod10rgtfx_k1_ml_scat)
dim(mod10rgtfx_k1_ml_scat$model)
dim(mod10rgtfx_ml_scat$model)
# which data rows in mod10rgtfx_ml_scat$model are not in mod10rgtfx_k1_ml_scat$model
a <- setdiff(rownames(mod10rgtfx_ml_scat$model), rownames(mod10rgtfx_k1_ml_scat$model))
mod10rgtfx_ml_scat$model[rownames(mod10rgtfx_ml_scat$model) %in% a, ]


# Figure 12
AIC(mod10rgtfx_ml_scat_lin, mod10rgtfx_ml_scat)
plot_loglen_vslinear(mod=mod10rgtfx_ml_scat, modlin=mod10rgtfx_ml_scat_lin, dat = drgtf_ro, direc="../figures_resubmit/", modn="mod10rgtfx_ml_scat_linear")
plot_loglen_vslinear(mod=mod10rgtbx_ml_scat, modlin=mod10rgtbx_ml_scat_lin, dat = drgtf_ro, direc="../figures_resubmit/", modn="mod10rgtbx_ml_scat_linear")

# Quantify nonlinearity for resubmit
AIC(mod10rgtfx_ml_scat, mod10rgtfx_ml_scat_lin)
AIC(mod10rgtfx_ml_scat) - AIC(mod10rgtfx_ml_scat_lin)
summary(mod10rgtfx_ml_scat_lin)
quantify_nonlinearity(mod_gam=mod10rgtfx_ml_scat, mod_linear=mod10rgtfx_ml_scat_lin, data=mod10rgtfx_ml_scat$model)

# Run the analysis
length_diffs <- calculate_length_differences(
  mod_gam = mod10rgtfx_ml_scat,
  mod_linear = mod10rgtfx_ml_scat_lin,
  data = mod10rgtfx_ml_scat$model,
  bin_width = 5  # 5 cm bins
)

# View summary
print(length_diffs$by_bin, n=30)

# Find which lengths have largest deviations
length_diffs$by_bin %>%
  arrange(desc(abs(mean_diff_abs))) %>%
  head(10)

# Or by percentage
length_diffs$by_bin %>%
  arrange(desc(abs(mean_diff_pct))) %>%
  head(10)


# Save everything
save(mod4rgtfx_ml_scat,mod5rgtfx_ml_scat,mod6rgtfx_ml_scat,
     mod9rgtfx_ml_scat,mod9rgtfxy_ml_scat,mod10rgtfx_ml_scat_lin,
     mod10rgtfxy_ml_scat,mod10rgtfx_ml_scat, file = "../resubmit/store/mods_gtf_ml_scat_cleanobs.RData")
rm(mod4rgtfx_ml_scat,mod5rgtfx_ml_scat,mod6rgtfx_ml_scat,
     mod9rgtfx_ml_scat,mod9rgtfxy_ml_scat,mod10rgtfx_ml_scat_lin,
     mod10rgtfxy_ml_scat,mod10rgtfx_ml_scat, testlin)
load("../resubmit/store/mods_gtf_ml_scat_cleanobs.RData")
load("../resubmit/store/mods_gtf_ml_scat.RData")
save.image()

a <- AIC(mod4rgtfx_ml_scat,mod5rgtfx_ml_scat,mod6rgtfx_ml_scat,mod9rgtfx_ml_scat,mod9rgtfxy_ml_scat,mod10rgtfxy_ml_scat,mod10rgtfx_ml_scat,
         mod11rgtfxy_ml, mod12rgtfxy_ml, mod13rgtfxy_ml,mod12brgtfxy_ml,mod13brgtfxy_ml,mod13crgtfxy_ml)
cbind(a, a$AIC-min(a$AIC))


########## Spatial interaction terms

mod11rgtfxy_ml %<-% gam(logwt ~ ti(loglen, k=20) + s(obs_name, bs="re") + 
                          ti(lat, k = 15) + ti(lon, k = 15) + ti(mon, k=12, bs="cc") +
                          ti(lat, lon, k = c(15,15)) + ti(lon, mon, k = c(10,4), bs=c("cr", "cc")) + ti(lat, mon, k = c(10,4), bs=c("cr", "cc")) +
                          ti(lat, lon, mon, k = c(8,8,3), bs =c("cr","cr","cc")) +
                          ti(lat, lon, loglen, k = c(6,6,5), bs = c("cr", "cr", "cr")) +
                          s(hbf, k=15) + yrf, data = drgtf_ro, family = scat, method = 'ML', gamma = 1.4)
mod12rgtfxy_ml %<-% gam(logwt ~ ti(loglen, k=20) + s(obs_name, bs="re") + 
                          ti(lat, k = 15) + ti(lon, k = 20) + ti(mon, k=12, bs="cc") +
                          ti(lat, lon, k = c(10,10)) + ti(lon, mon, k = c(10,5), bs=c("cr", "cc")) + ti(lat, mon, k = c(10,5), bs=c("cr", "cc")) +
                          ti(lat, lon, mon, k = c(8,8,5), bs =c("cr","cr","cc")) +
                          ti(lat, loglen, k = c(8,4)) + ti(lon,loglen, k = c(10,4)) +
                          ti(lat, lon, loglen, k = c(7,7,4)) +
                          s(hbf, k=15, bs = "cr") + yrf, data = drgtf_ro, family = scat, method = 'ML', gamma = 1.4)
mod12brgtfxy_ml %<-% gam(logwt ~ ti(loglen, k=20) + s(obs_name, bs="re") + 
                          ti(lat, k = 10) + ti(lon, k = 10) + ti(mon, k=4, bs="cc") +
                          ti(lat, lon, k = c(10,10)) + ti(lon, mon, k = c(10,4), bs=c("cr", "cc")) + ti(lat, mon, k = c(10,4), bs=c("cr", "cc")) +
                          ti(lat, lon, mon, k = c(10,10,4), bs =c("cr","cr","cc")) +
                          ti(lat, loglen, k = c(8,4)) + ti(lon,loglen, k = c(10,4)) +
                          ti(lat, lon, loglen, k = c(7,7,4)) +
                          s(hbf, k=15, bs = "cr") + yrf, data = drgtf_ro, family = scat, method = 'ML', gamma = 1.4)
mod13rgtfxy_ml %<-% gam(logwt ~ ti(loglen, k=12) + s(obs_name, bs="re") + 
                          ti(lat, k = 10) + ti(lon, k = 10) + ti(mon, k=8, bs="cc") +
                          ti(lat, lon, k = c(10,10)) + ti(lon, mon, k = c(10,5), bs=c("cr", "cc")) + ti(lat, mon, k = c(10,5), bs=c("cr", "cc")) +
                          ti(lat, lon, mon, k = c(7,7,5), bs =c("cr","cr","cc")) +
                          ti(lat, loglen, k = c(8,4)) + ti(lon,loglen, k = c(8,4)) +
                          ti(lat, lon, loglen, k = c(7,7,4)) +
                          ti(loglen, by = yrf) +
                          s(hbf, k=15, bs = "cr") + yrf, data = drgtf_ro, family = scat, method = 'ML', gamma = 1.4)
mod13brgtfxy_ml %<-% gam(logwt ~ ti(loglen, k=20) + s(obs_name, bs="re") + 
                          ti(lat, k = 10) + ti(lon, k = 10) + ti(mon, k=4, bs="cc") +
                          ti(lat, lon, k = c(10,10)) + ti(lon, mon, k = c(10,4), bs=c("cr", "cc")) + ti(lat, mon, k = c(10,4), bs=c("cr", "cc")) +
                          ti(lat, lon, mon, k = c(10,10,4), bs =c("cr","cr","cc")) +
                          ti(lat, loglen, k = c(8,4)) + ti(lon,loglen, k = c(8,4)) +
                          ti(lat, lon, loglen, k = c(7,7,4)) +
                          ti(loglen, by = yrf) +
                          s(hbf, k=15, bs = "cr") + yrf, data = drgtf_ro, family = scat, method = 'ML', gamma = 1.4)
mod13crgtfxy_ml %<-% gam(logwt ~ ti(loglen, k=20) + s(obs_name, bs="re") + 
                           ti(lat, k = 10) + ti(lon, k = 20) + ti(mon, k=8, bs="cc") +
                           ti(lat, lon, k = c(10,10)) + ti(lon, mon, k = c(10,6), bs=c("cr", "cc")) + ti(lat, mon, k = c(10,6), bs=c("cr", "cc")) +
                           ti(lat, lon, mon, k = c(10,10,4), bs =c("cr","cr","cc")) +
                           ti(lat, loglen, k = c(8,6)) + ti(lon,loglen, k = c(8,6)) +
                           ti(lat, lon, loglen, k = c(8,8,6)) +
                           ti(loglen, by = yrf) +
                           s(hbf, k=15, bs = "cr") + yrf, data = drgtf_ro, family = scat, method = 'ML', gamma = 1.4)
windows(); par(mfrow=c(2,2))
gam.check(mod13crgtfxy_ml)
windows()
plot.gam(mod13crgtfxy_ml, pages=1, all.terms=TRUE)
summary(mod11rgtfxy_ml)
summary(mod12rgtfxy_ml)
summary(mod13crgtfxy_ml)
AIC(mod10rgtfxy_ml_scat, mod11rgtfxy_ml, mod12rgtfxy_ml, mod13rgtfxy_ml,
    mod12brgtfxy_ml, mod13brgtfxy_ml, mod13crgtfxy_ml)

mod12rgtfxy_reml %<-% gam(logwt ~ ti(loglen, k=20) + s(obs_name, bs="re") + 
                            ti(lat, k = 15) + ti(lon, k = 18) + ti(mon, k=12, bs="cc") +
                            ti(lat, lon, k = c(10,10)) + ti(lon, mon, k = c(10,4), bs=c("cr", "cc")) + ti(lat, mon, k = c(10,4), bs=c("cr", "cc")) +
                            ti(lat, lon, mon, k = c(7,7,4), bs =c("cs","cs","cs")) +
                            ti(lat, lon, loglen, k = c(7,7,3), bs = c("cs", "cs", "cs")) +
                            s(hbf, bs = "cs") + yrf, data = drgtf_ro, family = scat, method = 'REML', gamma = 1.4)
mod12rgtfxy_reml_bam %<-% bam(logwt ~ ti(loglen, k=20) + s(obs_name, bs="re") + 
                            ti(lat, k = 15) + ti(lon, k = 18) + ti(mon, k=12, bs="cc") +
                            ti(lat, lon, k = c(10,10)) + ti(lon, mon, k = c(10,4), bs=c("cr", "cc")) + ti(lat, mon, k = c(10,4), bs=c("cr", "cc")) +
                            ti(lat, lon, mon, k = c(7,7,4), bs =c("cs","cs","cs")) +
                            ti(lat, lon, loglen, k = c(7,7,3), bs = c("cs", "cs", "cs")) +
                            s(hbf, bs = "cs") + yrf, data = drgtf_ro, family = scat, method = 'REML', gamma = 1.4, seed=TRUE)

mod11rgtmxy_ml %<-% gam(logwt ~ ti(loglen, k=20) + s(obs_name, bs="re") + 
                          ti(lat, k = 15) + ti(lon, k = 15) + ti(mon, k=12, bs="cc") +
                          ti(lat, lon, k = c(15,15)) + ti(lon, mon, k = c(10,4), bs=c("cr", "cc")) + ti(lat, mon, k = c(10,4), bs=c("cr", "cc")) +
                          ti(lat, lon, mon, k = c(8,8,3), bs =c("cr","cr","cc")) +
                          ti(lat, lon, loglen, k = c(6,6,5), bs = c("cr", "cr", "cr")) +
                          s(hbf) + yrf, data = drgtm_ro, family = scat, method = 'ML', gamma = 1.4)
mod12rgtmxy_ml %<-% gam(logwt ~ ti(loglen, k=20) + s(obs_name, bs="re") + 
                          ti(lat, k = 15) + ti(lon, k = 18) + ti(mon, k=12, bs="cc") +
                          ti(lat, lon, k = c(10,10)) + ti(lon, mon, k = c(10,4), bs=c("cr", "cc")) + ti(lat, mon, k = c(10,4), bs=c("cr", "cc")) +
                          ti(lat, lon, mon, k = c(7,7,4), bs =c("cs","cs","cs")) +
                          ti(lat, lon, loglen, k = c(7,7,3), bs = c("cs", "cs", "cs")) +
                          s(hbf, bs = "cs") + yrf, data = drgtm_ro, family = gaussian, method = 'ML', gamma = 1.4)

save(mod11rgtfxy_ml, mod12rgtfxy_ml, mod13rgtfxy_ml,mod12brgtfxy_ml,mod13brgtfxy_ml,mod13crgtfxy_ml,mod12rgtfxy_reml,mod12rgtfxy_reml_bam,
     mod11rgtmxy_ml,mod12rgtmxy_ml, file = "./resubmit/store/mod11_13_gt.RData")
load("./resubmit/store/mod11_13_gt.RData")
rm(mod11rgtfxy_ml, mod12rgtfxy_ml, mod13rgtfxy_ml,mod12brgtfxy_ml,mod13brgtfxy_ml,mod13crgtfxy_ml,mod12rgtfxy_reml,mod12rgtfxy_reml_bam,
   mod11rgtmxy_ml,mod12rgtmxy_ml)

# Predict LWRs and make plots for different years and locations

pred_LWRx <- function(mod1, dat, xlat, xlon, xmon, xyr) {
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
                               yrf = xyr,
                               wavescale=factor(wavescale[1], levels=levels(wavescale)),
                               year = median(year)))
  excl <- c("s(obs_name)", "s(hbf)", "wavescale", "s(lightstick)", "HkCourse")
  pred  <- predict(mod1, new_data, exclude = excl,
                   type = "link", se.fit = TRUE, unconditional = TRUE)
  pred  <- cbind(pred, new_data)
  pred  <- transform(pred, lwr_ci = (fit - (2 * se.fit)),
                     upr_ci = (fit + (2 * se.fit)),
                     fitted = (fit))
  return(pred)
}

doyrs <- unique(drgtf_ro$yrf)
pred1 <- pred_LWRx(mod1=mod13crgtfxy_ml, dat=drgtf_ro, xlat = 3, xlon=145, xmon=3, xyr=doyrs[1])

windows()
i=1
plot(pred1$loglen, pred1$fitted, type = "l", xlab = "log(length)", ylab = "log(weight)")
for(yr in doyrs) {
  pred1 <- pred_LWRx(mod1=mod13crgtfxy_ml, dat=drgtf_ro, xlat = 3, xlon=145, xmon=3, xyr=yr)
  lines(pred1$loglen, pred1$fitted, type = "l", col= i)
  i = i+1
}

windows()
i=1
locs <- data.frame(lats = c(3,3,3,3,3), lons=c(140, 160, 180, 200, 220))
plot(pred1$loglen, pred1$fitted, type = "l", xlab = "log(length)", ylab = "log(weight)")
for(j in 1:dim(locs)[1]) {
  pred1 <- pred_LWRx(mod1=mod13crgtfxy_ml, dat=drgtf_ro, xlat = locs$lats[j], xlon=locs$lons[j], xmon=3, xyr=doyrs[1])
  lines(pred1$loglen, pred1$fitted, type = "l", col= i)
  i = i+1
}
legend("bottomright", legend =locs$lons, col=1:(i-1), lty=1)

windows()
i=1
locs <- data.frame(lats = c(-30, -5, 10, 30), lons=c(190,190,190,190))
plot(pred1$loglen, pred1$fitted, type = "l", xlab = "log(length)", ylab = "log(weight)")
for(j in 1:dim(locs)[1]) {
  pred1 <- pred_LWRx(mod1=mod13crgtfxy_ml, dat=drgtf_ro, xlat = locs$lats[j], xlon=locs$lons[j], xmon=3, xyr=doyrs[1])
  lines(pred1$loglen, pred1$fitted, type = "l", col= i, lwd=2)
  i = i+1
}
legend("bottomright", legend =locs$lats, col=1:(i-1), lty=1)

windows()
i=1
locs <- data.frame(lats = c(-30, -5, 10, 30), lons=c(190,190,190,190))
plot(pred1$loglen, pred1$fitted, type = "l", xlab = "log(length)", ylab = "log(weight)")
for(j in 1:dim(locs)[1]) {
  pred1 <- pred_LWRx(mod1=mod13crgtfxy_ml, dat=drgtf_ro, xlat = locs$lats[j], xlon=locs$lons[j], xmon=9, xyr=doyrs[1])
  lines(pred1$loglen, pred1$fitted, type = "l", col= i, lwd=2)
  i = i+1
}
legend("bottomright", legend =locs$lats, col=1:(i-1), lty=1)


o <- getViz(mod11rgtfxy_ml)
check.gamViz(o)
AIC(mod11rgtfxy_ml,mod12rgtfxy_ml,mod13rgtfxy_ml,mod10rgtfx_ml_scat)
AIC(mod11rgtmxy_ml,mod12rgtmxy_ml,mod10rgtmx_ml_scat)

summary(mod10rgtfx_ml_scat)
summary(mod11rgtfxy_ml)


windows(20,20)
plot.gam(mod10rgtfx_ml_scat, pages = 1, all.terms = T)
windows(20,15);
plot.gam(mod10rgtfx_ml_scat, select=4, ylim=c(-0.15, 0.15))
abline(h=0, col = 2)
windows(20,15);
plot.gam(mod10rgtfx_ml_scat, select=2, ylim=c(-0.15, 0.15))
abline(h=0, col = 2)
windows(10,10); par(mfrow=c(2,2))
hist(drgtf$length)
plot(drgtf$loglen, drgtf$logwt)

windows(20,20)
plot.gam(mod13rgtfxy_ml, pages = 1, all.terms = T)

cbind(rownames(a), a$AIC, a$AIC-min(a$AIC))

attr(mod4rgtfx_ml_scat$terms, "term.labels")
attr(mod5rgtfx_ml_scat$terms, "term.labels")
attr(mod6rgtfx_ml_scat$terms, "term.labels")
attr(mod9rgtfx_ml_scat$terms, "term.labels")
attr(mod9rgtfxy_ml_scat$terms, "term.labels")
attr(mod10rgtfxy_ml_scat$terms, "term.labels")

windows(20,20)
plot.gam(mod10rgtfx_ml_scat, pages = 1, all.terms = T)
hist(drgtf_r$length)
plot(drgtf_r$loglen, drgtf_r$logwt)
windows(20,15);
plot.gam(mod10rgtfx_ml_scat, pages = 1, all.terms = T, main = "mod10rgtfx_ml_scat")
windows(20,15);
plot.gam(mod10rgtfx_ml_scat, select=5, ylim=c(-0.15, 0.15))

doplots(mod=mod10rgtfx_ml_scat, dat = drgtf_ro, direc="../figures_resubmit/", modn="Fig09_mod10rgtfxo_ml_scat",
        diag=T, loglen=T, maps=T, hbf=T, year=F, yrf=T, wv=F, obs=T, comprehensive=TRUE)
length(unique(mod10rgtfx_ml_scat$model$obs_name))

#######################-------------------------------------------------------------

# WGT Males
# choose one random sample from each
drgtm_r <- drgtm %>% group_by(setid) %>% slice_sample(n=1)
trms <- c("wavescale", "lightstick", "weather", "HkCourse", "yrf", "loglen", "obs_name", "lon", "lat", "mon", "hbf")
cmp.css <- complete.cases(drgtm_r[,trms])
drgtm_r <- drgtm_r[cmp.css,]

obs.re <- extract_ranef(mod10rgtmx_o_ml_scat)
a <- obs.re[order(-obs.re$value),] 
plot(a$value) # remove 3 highest and 2 lowest obs_091, obs_175, obs_087, obs_128, obs_113
drgtm_ro <- drgtm_r[!drgtm_r$obs_name %in% c("obs_091", "obs_175", "obs_087", "obs_113", "obs_128"),] 

# Resubmit - check how far these were from the distibution
re_mean <- mean(a$value)
re_sd <- sd(a$value)
a$z_score <- (a$value - re_mean) / re_sd

dim(drgtm_r) # 11687 rows
a <- table(drgtm_r$obs_name) # includes obs_091, obs_175, obs_113
table(drgtm_ro$obs_name) # doesn't include them
a <- table(mod10rgtmx_ml_scat$model$obs_name) 
a[a<11]

# Check z-scores for removed observers
a[a$group %in% c("obs_091", "obs_175", "obs_087", "obs_113", "obs_128"), c("group", "value", "z_score")]
print(a[!a$group %in% c("obs_091", "obs_175", "obs_087", "obs_113", "obs_128"), c("group", "value", "z_score")], n=120)

# end of resubmit ##############

ls(pattern="mod[[:alnum:]]rgtm$")
rm(list = ls(pattern="mod[[:alnum:]]rgtm$"))
rm(mod10rgtmx)

mod4rgtmx_ml_scat  %<-% dogam(dat=drgtm_r, llm_cc = 'cc', hkc=FALSE,  fam = 'scat')
mod5rgtmx_ml_scat  %<-% dogam(dat=drgtm_r, llm_cc = 'cc',             fam = 'scat')
mod6rgtmx_ml_scat  %<-% dogam(dat=drgtm_r, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, fam = 'scat')
mod9rgtmx_ml_scat  %<-% dogam(dat=drgtm_r, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, fam = 'scat')
mod9rgtmxy_ml_scat %<-% dogam(dat=drgtm_r, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, yrf=FALSE, yr=TRUE, fam = 'scat')
mod10rgtmx_ml_scat %<-% dogam(dat=drgtm_r, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, yrf=TRUE, yr=FALSE,  wv=FALSE, fam = 'scat')
mod10rgtmxy_ml_scat %<-% dogam(dat=drgtm_r, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, yrf=FALSE, yr=TRUE, wv=FALSE, fam = 'scat')

mod4rgtmx_ml_scat  %<-% dogam(dat=drgtm_ro, llm_cc = 'cc', hkc=FALSE,  fam = 'scat')
mod5rgtmx_ml_scat  %<-% dogam(dat=drgtm_ro, llm_cc = 'cc',             fam = 'scat')
mod6rgtmx_ml_scat  %<-% dogam(dat=drgtm_ro, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, fam = 'scat')
mod9rgtmx_ml_scat  %<-% dogam(dat=drgtm_ro, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, fam = 'scat')
mod9rgtmxy_ml_scat %<-% dogam(dat=drgtm_ro, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, yrf=FALSE, yr=TRUE, fam = 'scat')
mod10rgtmx_ml_scat %<-% dogam(dat=drgtm_ro, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, yrf=TRUE, yr=FALSE,  wv=FALSE, fam = 'scat')
mod10rgtmxy_ml_scat %<-%dogam(dat=drgtm_ro, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, yrf=FALSE, yr=TRUE, wv=FALSE, fam = 'scat')

#mod10rgtmx_ml_scat_lin %<-% dogam(dat=drgtm_ro, llm_cc = 'cc', k_lgln=2, lstk=FALSE, hkc=FALSE, wth=FALSE, yrf=TRUE, yr=FALSE,  wv=FALSE, fam = 'scat')
linform <- as.formula(logwt ~ loglen + s(obs_name, bs="re") + te(lon, lat, mon, k = c(10, 10, 4), bs = c("cr", "cr", "cc")) + s(hbf, k = 15) + yrf)
mod10rgtmx_ml_scat_lin %<-%  gam(linform, data = drgtm_ro, family = scat, method = 'ML', gamma = 1.4)
AIC(mod10rgtmx_ml_scat_lin) - AIC(mod10rgtmx_ml_scat)
plot(mod10rgtmx_ml_scat_lin, all.terms=TRUE, select=1)
termplot(mod10rgtmx_ml_scat_lin2, se=TRUE, terms="loglen")
summary(mod10rgtmx_ml_scat_lin)
gam.check(mod10rgtmx_ml_scat)

# Figure 12
plot_loglen_vslinear(mod=mod10rgtmx_ml_scat, modlin=mod10rgtmx_ml_scat_lin, dat = drgtm_ro, direc="../figures3/", modn="mod10rgtmx_ml_scat_linear")

a <- AIC(mod4rgtmx_ml_scat,mod5rgtmx_ml_scat,mod6rgtmx_ml_scat,mod9rgtmx_ml_scat,mod9rgtmxy_ml_scat,
    mod10rgtmxy_ml_scat,mod10rgtmx_ml_scat)
cbind(a, a$AIC-min(a$AIC))

dim(drgtm_r)

save(mod4rgtmx_ml_scat,mod5rgtmx_ml_scat,mod6rgtmx_ml_scat,
     mod9rgtmx_ml_scat,mod9rgtmxy_ml_scat,mod10rgtmx_ml_scat_lin,
     mod10rgtmxy_ml_scat,mod10rgtmx_ml_scat,testlin_gtm, 
     file = "../resubmit/store/mods_gtm_ml_scat_cleanobs.RData")
rm(mod4rgtmx_ml_scat,mod5rgtmx_ml_scat,mod6rgtmx_ml_scat,
     mod9rgtmx_ml_scat,mod9rgtmxy_ml_scat,
     mod10rgtmxy_ml_scat,mod10rgtmx_ml_scat,mod10rgtmx_ml_scat_lin,testlin_gtm)
load("./resubmit/store/mods_gtm_ml_scat_cleanobs.RData")
load("./resubmit/store/mods_gtm_ml_scat.RData")

summary(mod10rgtmx_ml_scat)

doplots(mod=mod10rgtmx_ml_scat, dat = drgtm_ro, direc="../figures_resubmit/", modn="supp_mod10rgtmxo_ml_scat",
        diag=T, loglen=T, maps=T, hbf=T, year=F, yrf=T, wv=F, obs=T, comprehensive=TRUE)

###############################################

save(drggm_r, drggf_r, drgtf_r, drgtm_r, file="../resubmit/store/data_for_gams.RData")
load(file="data_for_gams.RData")
save(drggm_ro, drggf_ro, drgtf_ro, drgtm_ro, drgtb_ro, file="../resubmit/store/data_for_gams_cleanobs2.RData")
load("./resubmit/store/data_for_gams_cleanobs2.RData")

### Males and females joint

drgtb_ro <- rbind(drgtm_ro, drgtf_ro)
  
mod4rgtbx_ml_scat  %<-% dogam(dat=drgtb_ro, llm_cc = 'cc', hkc=FALSE,  fam = 'scat')
mod5rgtbx_ml_scat  %<-% dogam(dat=drgtb_ro, llm_cc = 'cc',             fam = 'scat')
mod6rgtbx_ml_scat  %<-% dogam(dat=drgtb_ro, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, fam = 'scat')
mod9rgtbx_ml_scat  %<-% dogam(dat=drgtb_ro, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, fam = 'scat')
mod9rgtbxy_ml_scat %<-% dogam(dat=drgtb_ro, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, yrf=FALSE, yr=TRUE, fam = 'scat')
mod10rgtbx_ml_scat %<-% dogam(dat=drgtb_ro, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, yrf=TRUE, yr=FALSE,  wv=FALSE, fam = 'scat')
# Linear loglen equivalent of best joint model (for nonlinearity comparison, Fig 10)

linform_b <- as.formula(logwt ~ loglen + s(obs_name, bs="re") + te(lon, lat, mon, k = c(10, 10, 4), bs = c("cr", "cr", "cc")) + s(hbf, k = 15) + yrf)
mod10rgtbx_ml_scat_lin <- gam(linform_b, data = drgtb_ro, family = scat, method = 'ML', gamma = 1.4)

mod10rgtbxy_ml_scat %<-%dogam(dat=drgtb_ro, llm_cc = 'cc', lstk=FALSE, hkc=FALSE, wth=FALSE, yrf=FALSE, yr=TRUE, wv=FALSE, fam = 'scat')
drgtb_ro$sex <- factor(ifelse(drgtb_ro$sex=="M", "M", "F"))
#mod10rgtbxy_ml_scat_lin2 %<-%dogam(dat=drgtb_ro, llm_cc = 'cc', k_lgln=2, lstk=FALSE, hkc=FALSE, wth=FALSE, yrf=FALSE, yr=TRUE, wv=FALSE, fam = 'scat')
mod10rgtbx_2sex_ml_scat %<-% gam(logwt ~ s(loglen, k=20, by = sex) + sex + s(obs_name, bs="re") + te(lon, lat, mon, k = c(10, 10, 4), bs = c("cr", "cr", "cc")) + s(hbf, k = 15) + yrf, data = drgtb_ro, family = scat, method = 'ML', gamma = 1.4)

linform <- as.formula(logwt ~ loglen + s(obs_name, bs="re") + te(lon, lat, mon, k = c(10, 10, 4), bs = c("cr", "cr", "cc")) + s(hbf, k = 15) + yrf)
mod10rgtbxy_ml_scat_lin2 %<-% gam(linform, data = drgtb_ro, family = scat, method = 'ML', gamma = 1.4)

mod13crgtbxy_ml %<-% gam(logwt ~ ti(loglen, k=20) + s(obs_name, bs="re") + 
                           ti(lat, k = 10) + ti(lon, k = 20) + ti(mon, k=8, bs="cc") +
                           ti(lat, lon, k = c(10,10)) + ti(lon, mon, k = c(10,6), bs=c("cr", "cc")) + ti(lat, mon, k = c(10,6), bs=c("cr", "cc")) +
                           ti(lat, lon, mon, k = c(10,10,4), bs =c("cr","cr","cc")) +
                           ti(lat, loglen, k = c(8,6)) + ti(lon,loglen, k = c(8,6)) +
                           ti(lat, lon, loglen, k = c(8,8,6)) +
                           ti(loglen, by = yrf) +
                           s(hbf, bs = "cr") + yrf, data = drgtb_ro, family = scat, method = 'ML', gamma = 1.4)

resolved(futureOf(mod13crgtbxy_ml))
resolved(futureOf(mod10rgtbx_sex_ml_scat))
resolved(futureOf(mod10rgtbx_2sex_ml_scat))

str(mod10rgtbx_2sex_ml_scat)
summary(mod10rgtbx_2sex_ml_scat)
summary(mod10rgtbx_ml_scat)

AIC(mod10rgtbx_2sex_ml_scat, mod10rgtbx_ml_scat)
AIC(mod10rgtbx_2sex_ml_scat) - AIC( mod10rgtbx_ml_scat)

a <- AIC(mod4rgtbx_ml_scat,mod5rgtbx_ml_scat,mod6rgtbx_ml_scat,mod9rgtbx_ml_scat,mod9rgtbxy_ml_scat,
         mod10rgtbxy_ml_scat,mod10rgtbx_ml_scat,mod13crgtbxy_ml)
a <- AIC(mod4rgtbx_ml_scat,mod5rgtbx_ml_scat,mod6rgtbx_ml_scat,mod9rgtbx_ml_scat,mod9rgtbxy_ml_scat,
         mod10rgtbxy_ml_scat,mod10rgtbx_ml_scat)
cbind(a, a$AIC-min(a$AIC))
summary(mod13crgtbxy_ml)

# Compare AIC of combined model with independent males and females
a <- AIC(mod10rgtbx_ml_scat, mod10rgtmx_ml_scat, mod10rgtfx_ml_scat)
c(a[1,2], (a[2,2] + a[3,2]))
a[1,2] - (a[2,2] + a[3,2])


doplots(mod=mod10rgtbx_ml_scat, dat = drgtb_ro, direc="../figures_resubmit/", modn="Fig11_mod10rgtbx_ml_scat",
        diag=T, loglen=T, maps=T, hbf=T, year=F, yrf=T, wv=F, obs=T, comprehensive=TRUE)

logwt ~ s(loglen, k = 20) + s(obs_name, bs = "re") + te(lon, lat, mon, k = c(10, 10, 4), bs = c("cr", "cr", "cc")) + 
  s(hbf, k = 10) + s(year, k = 6)

summary(mod13crgtbxy_ml)

# Plot M vs F vs joint
# Figure 13
plot_M_v_F_V_joint(modF=mod10rgtfx_ml_scat, modM=mod10rgtmx_ml_scat, modJ=mod10rgtbx_ml_scat, 
                   datF=drgtf_ro, datM=drgtm_ro, direc="../figures3/", modn="mod10rgt_joint")

save(mod4rgtbx_ml_scat,mod5rgtbx_ml_scat,mod6rgtbx_ml_scat,mod9rgtbx_ml_scat,
     mod9rgtbxy_ml_scat,mod10rgtbx_ml_scat,mod10rgtbxy_ml_scat,mod10rgtbxy_ml_scat_lin,mod10rgtbx_ml_scat_lin,
     mod13crgtbxy_ml, file = "../resubmit/store/mods_gtb_ml_scat_cleanobs.RData")
rm(mod4rgtbx_ml_scat,mod5rgtbx_ml_scat,mod6rgtbx_ml_scat,mod9rgtbx_ml_scat,mod13crgtbxy_ml,
     mod9rgtbxy_ml_scat,mod10rgtbx_ml_scat,mod10rgtbxy_ml_scat,mod10rgtbxy_ml_scat_lin)
load(file = "./resubmit/store/mods_gtb_ml_scat_cleanobs.RData")

# Predict for several locations and so on
predMF_20_160_2 <- pred_LWR(mod=mod10rgtbxy_ml_scat, dat=drgtb_ro, xlat = 20, xlon=160, xmon=2)
predMF_20_160_5 <- pred_LWR(mod=mod10rgtbxy_ml_scat, dat=drgtb_ro, xlat = 20, xlon=160, xmon=5)
predMF_20_160_8 <- pred_LWR(mod=mod10rgtbxy_ml_scat, dat=drgtb_ro, xlat = 20, xlon=160, xmon=8)
predMF_20_160_11 <- pred_LWR(mod=mod10rgtbxy_ml_scat, dat=drgtb_ro, xlat = 20, xlon=160, xmon=11)

predMF_20_160_2 <- pred_LWR(mod=mod10rgtbxy_ml_scat, dat=drgtb_ro, xlat = 20, xlon=160, xmon=2)
predMF_0_160_2 <- pred_LWR(mod=mod10rgtbxy_ml_scat, dat=drgtb_ro, xlat = 0, xlon=160, xmon=2)
predMF_x15_160_2 <- pred_LWR(mod=mod10rgtbxy_ml_scat, dat=drgtb_ro, xlat = -15, xlon=160, xmon=2)
predMF_x30_160_2 <- pred_LWR(mod=mod10rgtbxy_ml_scat, dat=drgtb_ro, xlat = -30, xlon=160, xmon=2)

predMF_30_190_8 <- pred_LWR(mod=mod10rgtbxy_ml_scat, dat=drgtb_ro, xlat = 30, xlon=190, xmon=8)
predMF_15_190_8 <- pred_LWR(mod=mod10rgtbxy_ml_scat, dat=drgtb_ro, xlat = 15, xlon=190, xmon=8)
predMF_0_190_8 <- pred_LWR(mod=mod10rgtbxy_ml_scat, dat=drgtb_ro, xlat = 0, xlon=190, xmon=8)
predMF_x30_190_8 <- pred_LWR(mod=mod10rgtbxy_ml_scat, dat=drgtb_ro, xlat = -30, xlon=190, xmon=8)

predMF_5_140_11 <- pred_LWR(mod=mod10rgtbxy_ml_scat, dat=drgtb_ro, xlat = 5, xlon=140, xmon=11)
predMF_5_170_11 <- pred_LWR(mod=mod10rgtbxy_ml_scat, dat=drgtb_ro, xlat = 5, xlon=170, xmon=11)
predMF_5_200_11 <- pred_LWR(mod=mod10rgtbxy_ml_scat, dat=drgtb_ro, xlat = 5, xlon=200, xmon=11)
predMF_5_230_11 <- pred_LWR(mod=mod10rgtbxy_ml_scat, dat=drgtb_ro, xlat = 5, xlon=230, xmon=11)

windows(9, 9); par(mfrow=c(2,2), mar=c(2,2,3,1), oma=c(3,2,0,0))
with(predMF_20_160_2, plot(exp(loglen), exp(fitted), xlab = "", ylab = "", type = "l", lwd=2, ylim = c(0,130)), cex.lab=1.1)
with(predMF_20_160_5, lines(exp(loglen), exp(fitted), lty=1, col=2, lwd=2))
with(predMF_20_160_8, lines(exp(loglen), exp(fitted), lty=2, col=2, lwd=2))
with(predMF_20_160_11, lines(exp(loglen), exp(fitted), lty=2, col=1, lwd=2))
mtext(side=2, text="Weight (kg)", line=2.5, cex=1.2)
legend("topleft", legend = paste0("Lat 20, Lon 160, Mon ", c(2, 5, 8, 11)), col=c(1,2,2,1), lty=c(1,1,2,2), lwd=2, cex=1.3)

with(predMF_20_160_2, plot(exp(loglen), exp(fitted), xlab = "", ylab = "", type = "l", lwd=2, ylim = c(0,130)))
with(predMF_0_160_2, lines(exp(loglen), exp(fitted), lty=1, col=2, lwd=2))
with(predMF_x15_160_2, lines(exp(loglen), exp(fitted), lty=2, col=2, lwd=2))
with(predMF_x30_160_2, lines(exp(loglen), exp(fitted), lty=2, col=1, lwd=2))
legend("topleft", legend = c("Lat 20, Lon 160, Mon 2","Lat  0, Lon 160, Mon 2","Lat -15, Lon 160, Mon 2","Lat -30, Lon 160, Mon 2"), col=c(1,2,2,1), lty=c(1,1,2,2), lwd=2, cex=1.3)

with(predMF_30_190_8, plot(exp(loglen), exp(fitted), xlab = "", ylab = "", type = "l", lwd=2, ylim = c(0,130)))
with(predMF_15_190_8, lines(exp(loglen), exp(fitted), lty=1, col=2, lwd=2))
with(predMF_0_190_8, lines(exp(loglen), exp(fitted), lty=2, col=2, lwd=2))
with(predMF_x30_190_8, lines(exp(loglen), exp(fitted), lty=2, col=1, lwd=2))
legend("topleft", legend = c("Lat  20, Lon 160, Mon 8","Lat  0, Lon 160, Mon 8","Lat  0, Lon 190, Mon 8","Lat -30, Lon 190, Mon 8"), col=c(1,2,2,1), lty=c(1,1,2,2), lwd=2, cex=1.3)
mtext(side=2, text="Weight (kg)", line=2.5, cex=1.2)
mtext(side=1, text="Length (cm)", line=2.5, cex=1.2)

with(predMF_5_140_11, plot(exp(loglen), exp(fitted), xlab = "", ylab = "", type = "l", lwd=2, ylim = c(0,130)))
with(predMF_5_170_11, lines(exp(loglen), exp(fitted), lty=1, col=2, lwd=2))
with(predMF_5_200_11, lines(exp(loglen), exp(fitted), lty=2, col=2, lwd=2))
with(predMF_5_230_11, lines(exp(loglen), exp(fitted), lty=2, col=1, lwd=2))
legend("topleft", legend = c("Lat 5, Lon 140, Mon 11","Lat 5, Lon 170, Mon 11","Lat 5, Lon 200, Mon 11","Lat 5, Lon 230, Mon 11"), col=c(1,2,2,1), lty=c(1,1,2,2), lwd=2, cex=1.3)
mtext(side=1, text="Length (cm)", line=2.5, cex=1.2)
savePlot(paste0("../figures3/mod10rgtbxy_ml_scat_loglen_array.png"), type = "png")

windows(9, 9); par(mfrow=c(2,2))
with(predMF_20_160_2, plot(loglen, fitted, xlab = "loglen", ylab = "logwt", type = "l", lwd=2, ylim = c(1.5,5)))
with(predMF_20_160_5, lines(loglen, fitted, lty=1, col=2, lwd=2))
with(predMF_20_160_8, lines(loglen, fitted, lty=2, col=2, lwd=2))
with(predMF_20_160_11, lines(loglen, fitted, lty=2, col=1, lwd=2))
legend("topleft", legend = paste0("Lat=20, Lon=160, Month=", c(2, 5, 8, 11)), col=c(1,2,2,1), lty=c(1,1,2,2), lwd=2, cex=1.3)

with(predMF_20_160_2, plot(loglen, fitted, xlab = "loglen", ylab = "logwt", type = "l", lwd=2, ylim = c(1.5,5)))
with(predMF_0_160_2, lines(loglen, fitted, lty=1, col=2, lwd=2))
with(predMF_0_190_2, lines(loglen, fitted, lty=2, col=2, lwd=2))
with(predMF_x30_190_2, lines(loglen, fitted, lty=2, col=1, lwd=2))
legend("topleft", legend = c("Lat= 20, Lon=160, Month=2","Lat=  0,  Lon=160, Month=2","Lat=  0,  Lon=190, Month=2","Lat=-30, Lon=190, Month=2"), col=c(1,2,2,1), lty=c(1,1,2,2), lwd=2, cex=1.3)

with(predMF_20_160_8, plot(loglen, fitted, xlab = "loglen", ylab = "logwt", type = "l", lwd=2, ylim = c(1.5,5)))
with(predMF_0_160_8, lines(loglen, fitted, lty=1, col=2, lwd=2))
with(predMF_0_190_8, lines(loglen, fitted, lty=2, col=2, lwd=2))
with(predMF_x30_190_8, lines(loglen, fitted, lty=2, col=1, lwd=2))
legend("topleft", legend = c("Lat= 20, Lon=160, Month=8","Lat=  0,  Lon=160, Month=8","Lat=  0,  Lon=190, Month=8","Lat=-30, Lon=190, Month=8"), col=c(1,2,2,1), lty=c(1,1,2,2), lwd=2, cex=1.3)

with(predMF_30_160_2, plot(loglen, fitted, xlab = "loglen", ylab = "logwt", type = "l", lwd=2, ylim = c(1.5,5)))
with(predMF_0_200_5, lines(loglen, fitted, lty=1, col=2, lwd=2))
with(predMF_x10_230_8, lines(loglen, fitted, lty=2, col=2, lwd=2))
with(predMF_35_210_11, lines(loglen, fitted, lty=2, col=1, lwd=2))
legend("topleft", legend = c("Lat= 30, Lon=160, Month=2","Lat=  0,  Lon=200, Month=5","Lat=-10, Lon=230, Month=8","Lat= 35, Lon=210, Month=11"), col=c(1,2,2,1), lty=c(1,1,2,2), lwd=2, cex=1.3)
savePlot(paste0("../figures3/mod10rgtbxy_ml_scat_loglen_array2.png"), type = "png")

# Predict for several locations and so on for mod13cf
predMF_20_160_2 <- pred_LWR(mod=mod13crgtfxy_ml, dat=drgtf_ro, xlat = 20, xlon=160, xmon=2)
predMF_20_160_5 <- pred_LWR(mod=mod13crgtfxy_ml, dat=drgtf_ro, xlat = 20, xlon=160, xmon=5)
predMF_20_160_8 <- pred_LWR(mod=mod13crgtfxy_ml, dat=drgtf_ro, xlat = 20, xlon=160, xmon=8)
predMF_20_160_11 <- pred_LWR(mod=mod13crgtfxy_ml, dat=drgtf_ro, xlat = 20, xlon=160, xmon=11)

predMF_20_160_2 <- pred_LWR(mod=mod13crgtfxy_ml, dat=drgtf_ro, xlat = 20, xlon=160, xmon=2)
predMF_0_160_2 <- pred_LWR(mod=mod13crgtfxy_ml, dat=drgtf_ro, xlat = 0, xlon=160, xmon=2)
predMF_0_190_2 <- pred_LWR(mod=mod13crgtfxy_ml, dat=drgtf_ro, xlat = 0, xlon=190, xmon=2)
predMF_x30_190_2 <- pred_LWR(mod=mod13crgtfxy_ml, dat=drgtf_ro, xlat = -30, xlon=190, xmon=2)

predMF_20_160_8 <- pred_LWR(mod=mod13crgtfxy_ml, dat=drgtf_ro, xlat = 20, xlon=160, xmon=8)
predMF_0_160_8 <- pred_LWR(mod=mod13crgtfxy_ml, dat=drgtf_ro, xlat = 0, xlon=160, xmon=8)
predMF_0_190_8 <- pred_LWR(mod=mod13crgtfxy_ml, dat=drgtf_ro, xlat = 0, xlon=190, xmon=8)
predMF_x30_190_8 <- pred_LWR(mod=mod13crgtfxy_ml, dat=drgtf_ro, xlat = -30, xlon=190, xmon=8)

predMF_30_160_2 <- pred_LWR(mod=mod13crgtfxy_ml, dat=drgtf_ro, xlat = 30, xlon=160, xmon=2)
predMF_0_200_5 <- pred_LWR(mod=mod13crgtfxy_ml, dat=drgtf_ro, xlat = 0, xlon=200, xmon=5)
predMF_x10_230_8 <- pred_LWR(mod=mod13crgtfxy_ml, dat=drgtf_ro, xlat = -10, xlon=230, xmon=8)
predMF_35_210_11 <- pred_LWR(mod=mod13crgtfxy_ml, dat=drgtf_ro, xlat = 35, xlon=210, xmon=11)

windows(8, 8); par(mfrow=c(2,2), mar=c(5,4,4,1), oma=c(0,0,0,0))
with(predMF_20_160_2, plot(exp(loglen), exp(fitted), xlab = "", ylab = "Weight (kg)", type = "l", lwd=2, ylim = c(0,130)))
with(predMF_20_160_5, lines(exp(loglen), exp(fitted), lty=1, col=2, lwd=2))
with(predMF_20_160_8, lines(exp(loglen), exp(fitted), lty=2, col=2, lwd=2))
with(predMF_20_160_11, lines(exp(loglen), exp(fitted), lty=2, col=1, lwd=2))
mtext("By month", side=3, line=-1, outer=TRUE, cex=1.5)
legend("topleft", legend = paste0("Lat=20, Lon=160, Month=", c(2, 5, 8, 11)), col=c(1,2,2,1), lty=c(1,1,2,2), lwd=2, cex=1)

with(predMF_20_160_2, plot(exp(loglen), exp(fitted), xlab = "", ylab = "", type = "l", lwd=2, ylim = c(0,130)))
with(predMF_0_160_2, lines(exp(loglen), exp(fitted), lty=1, col=2, lwd=2))
with(predMF_0_190_2, lines(exp(loglen), exp(fitted), lty=2, col=2, lwd=2))
with(predMF_x30_190_2, lines(exp(loglen), exp(fitted), lty=2, col=1, lwd=2))
mtext("By month", side=3, line=-1, outer=TRUE, cex=1.5)
legend("topleft", legend = c("Lat= 20, Lon=160, Month=2","Lat=  0,  Lon=160, Month=2","Lat=  0,  Lon=190, Month=2","Lat=-30, Lon=190, Month=2"), col=c(1,2,2,1), lty=c(1,1,2,2), lwd=2, cex=1)

with(predMF_20_160_8, plot(exp(loglen), exp(fitted), xlab = "Length (cm)", ylab = "Weight (kg)", type = "l", lwd=2, ylim = c(0,130)))
with(predMF_0_160_8, lines(exp(loglen), exp(fitted), lty=1, col=2, lwd=2))
with(predMF_0_190_8, lines(exp(loglen), exp(fitted), lty=2, col=2, lwd=2))
with(predMF_x30_190_8, lines(exp(loglen), exp(fitted), lty=2, col=1, lwd=2))
legend("topleft", legend = c("Lat= 20, Lon=160, Month=8","Lat=  0,  Lon=160, Month=8","Lat=  0,  Lon=190, Month=8","Lat=-30, Lon=190, Month=8"), col=c(1,2,2,1), lty=c(1,1,2,2), lwd=2, cex=1)

with(predMF_30_160_2, plot(exp(loglen), exp(fitted), xlab = "Length (cm)", ylab = "", type = "l", lwd=2, ylim = c(0,130)))
with(predMF_0_200_5, lines(exp(loglen), exp(fitted), lty=1, col=2, lwd=2))
with(predMF_x10_230_8, lines(exp(loglen), exp(fitted), lty=2, col=2, lwd=2))
with(predMF_35_210_11, lines(exp(loglen), exp(fitted), lty=2, col=1, lwd=2))
legend("topleft", legend = c("Lat= 30, Lon=160, Month=2","Lat=  0,  Lon=200, Month=5","Lat=-10, Lon=230, Month=8","Lat= 35, Lon=210, Month=11"), col=c(1,2,2,1), lty=c(1,1,2,2), lwd=2, cex=1)
savePlot(paste0("../figures3/mod13crgtfxy_ml_loglen_array.png"), type = "png")

windows(9, 9); par(mfrow=c(2,2))
with(predMF_20_160_2, plot(loglen, fitted, xlab = "loglen", ylab = "logwt", type = "l", lwd=2, ylim = c(1.5,5)))
with(predMF_20_160_5, lines(loglen, fitted, lty=1, col=2, lwd=2))
with(predMF_20_160_8, lines(loglen, fitted, lty=2, col=2, lwd=2))
with(predMF_20_160_11, lines(loglen, fitted, lty=2, col=1, lwd=2))
legend("topleft", legend = paste0("Lat=20, Lon=160, Month=", c(2, 5, 8, 11)), col=c(1,2,2,1), lty=c(1,1,2,2), lwd=2, cex=1.3)

with(predMF_20_160_2, plot(loglen, fitted, xlab = "loglen", ylab = "logwt", type = "l", lwd=2, ylim = c(1.5,5)))
with(predMF_0_160_2, lines(loglen, fitted, lty=1, col=2, lwd=2))
with(predMF_0_190_2, lines(loglen, fitted, lty=2, col=2, lwd=2))
with(predMF_x30_190_2, lines(loglen, fitted, lty=2, col=1, lwd=2))
legend("topleft", legend = c("Lat= 20, Lon=160, Month=2","Lat=  0,  Lon=160, Month=2","Lat=  0,  Lon=190, Month=2","Lat=-30, Lon=190, Month=2"), col=c(1,2,2,1), lty=c(1,1,2,2), lwd=2, cex=1.3)

with(predMF_20_160_8, plot(loglen, fitted, xlab = "loglen", ylab = "logwt", type = "l", lwd=2, ylim = c(1.5,5)))
with(predMF_0_160_8, lines(loglen, fitted, lty=1, col=2, lwd=2))
with(predMF_0_190_8, lines(loglen, fitted, lty=2, col=2, lwd=2))
with(predMF_x30_190_8, lines(loglen, fitted, lty=2, col=1, lwd=2))
legend("topleft", legend = c("Lat= 20, Lon=160, Month=8","Lat=  0,  Lon=160, Month=8","Lat=  0,  Lon=190, Month=8","Lat=-30, Lon=190, Month=8"), col=c(1,2,2,1), lty=c(1,1,2,2), lwd=2, cex=1.3)

with(predMF_30_160_2, plot(loglen, fitted, xlab = "loglen", ylab = "logwt", type = "l", lwd=2, ylim = c(1.5,5)))
with(predMF_0_200_5, lines(loglen, fitted, lty=1, col=2, lwd=2))
with(predMF_x10_230_8, lines(loglen, fitted, lty=2, col=2, lwd=2))
with(predMF_35_210_11, lines(loglen, fitted, lty=2, col=1, lwd=2))
legend("topleft", legend = c("Lat= 30, Lon=160, Month=2","Lat=  0,  Lon=200, Month=5","Lat=-10, Lon=230, Month=8","Lat= 35, Lon=210, Month=11"), col=c(1,2,2,1), lty=c(1,1,2,2), lwd=2, cex=1.3)
savePlot(paste0("../figures3/mod13crgtfxy_ml_loglen_array2.png"), type = "png")


# Predict for several locations and so on for mod13cb
predMF_20_160_2 <- pred_LWR(mod=mod13crgtbxy_ml, dat=drgtf_ro, xlat = 20, xlon=160, xmon=2)
predMF_20_160_5 <- pred_LWR(mod=mod13crgtbxy_ml, dat=drgtf_ro, xlat = 20, xlon=160, xmon=5)
predMF_20_160_8 <- pred_LWR(mod=mod13crgtbxy_ml, dat=drgtf_ro, xlat = 20, xlon=160, xmon=8)
predMF_20_160_11 <- pred_LWR(mod=mod13crgtbxy_ml, dat=drgtf_ro, xlat = 20, xlon=160, xmon=11)

predMF_20_160_2 <- pred_LWR(mod=mod13crgtbxy_ml, dat=drgtf_ro, xlat = 20, xlon=160, xmon=2)
predMF_0_160_2 <- pred_LWR(mod=mod13crgtbxy_ml, dat=drgtf_ro, xlat = 0, xlon=160, xmon=2)
predMF_0_190_2 <- pred_LWR(mod=mod13crgtbxy_ml, dat=drgtf_ro, xlat = 0, xlon=190, xmon=2)
predMF_x30_190_2 <- pred_LWR(mod=mod13crgtbxy_ml, dat=drgtf_ro, xlat = -30, xlon=190, xmon=2)

predMF_20_160_8 <- pred_LWR(mod=mod13crgtbxy_ml, dat=drgtf_ro, xlat = 20, xlon=160, xmon=8)
predMF_0_160_8 <- pred_LWR(mod=mod13crgtbxy_ml, dat=drgtf_ro, xlat = 0, xlon=160, xmon=8)
predMF_0_190_8 <- pred_LWR(mod=mod13crgtbxy_ml, dat=drgtf_ro, xlat = 0, xlon=190, xmon=8)
predMF_x30_190_8 <- pred_LWR(mod=mod13crgtbxy_ml, dat=drgtf_ro, xlat = -30, xlon=190, xmon=8)

predMF_30_160_2 <- pred_LWR(mod=mod13crgtbxy_ml, dat=drgtf_ro, xlat = 30, xlon=160, xmon=2)
predMF_0_200_5 <- pred_LWR(mod=mod13crgtbxy_ml, dat=drgtf_ro, xlat = 0, xlon=200, xmon=5)
predMF_x10_230_8 <- pred_LWR(mod=mod13crgtbxy_ml, dat=drgtf_ro, xlat = -10, xlon=230, xmon=8)
predMF_35_210_11 <- pred_LWR(mod=mod13crgtbxy_ml, dat=drgtf_ro, xlat = 35, xlon=210, xmon=11)

windows(9, 9); par(mfrow=c(2,2))
with(predMF_20_160_2, plot(exp(loglen), exp(fitted), xlab = "Length (cm)", ylab = "Weight (kg)", type = "l", lwd=2, ylim = c(0,130)))
with(predMF_20_160_5, lines(exp(loglen), exp(fitted), lty=1, col=2, lwd=2))
with(predMF_20_160_8, lines(exp(loglen), exp(fitted), lty=2, col=2, lwd=2))
with(predMF_20_160_11, lines(exp(loglen), exp(fitted), lty=2, col=1, lwd=2))
legend("topleft", legend = paste0("Lat=20, Lon=160, Month=", c(2, 5, 8, 11)), col=c(1,2,2,1), lty=c(1,1,2,2), lwd=2)

with(predMF_20_160_2, plot(exp(loglen), exp(fitted), xlab = "Length (cm)", ylab = "Weight (kg)", type = "l", lwd=2, ylim = c(0,130)))
with(predMF_0_160_2, lines(exp(loglen), exp(fitted), lty=1, col=2, lwd=2))
with(predMF_0_190_2, lines(exp(loglen), exp(fitted), lty=2, col=2, lwd=2))
with(predMF_x30_190_2, lines(exp(loglen), exp(fitted), lty=2, col=1, lwd=2))
legend("topleft", legend = c("Lat= 20, Lon=160, Month=2","Lat=  0,  Lon=160, Month=2","Lat=  0,  Lon=190, Month=2","Lat=-30, Lon=190, Month=2"),
       col=c(1,2,2,1), lty=c(1,1,2,2), lwd=2)

with(predMF_20_160_8, plot(exp(loglen), exp(fitted), xlab = "Length (cm)", ylab = "Weight (kg)", type = "l", lwd=2, ylim = c(0,130)))
with(predMF_0_160_8, lines(exp(loglen), exp(fitted), lty=1, col=2, lwd=2))
with(predMF_0_190_8, lines(exp(loglen), exp(fitted), lty=2, col=2, lwd=2))
with(predMF_x30_190_8, lines(exp(loglen), exp(fitted), lty=2, col=1, lwd=2))
legend("topleft", legend = c("Lat= 20, Lon=160, Month=8","Lat=  0,  Lon=160, Month=8","Lat=  0,  Lon=190, Month=8","Lat=-30, Lon=190, Month=8"),
       col=c(1,2,2,1), lty=c(1,1,2,2), lwd=2)

with(predMF_30_160_2, plot(exp(loglen), exp(fitted), xlab = "Length (cm)", ylab = "Weight (kg)", type = "l", lwd=2, ylim = c(0,130)))
with(predMF_0_200_5, lines(exp(loglen), exp(fitted), lty=1, col=2, lwd=2))
with(predMF_x10_230_8, lines(exp(loglen), exp(fitted), lty=2, col=2, lwd=2))
with(predMF_35_210_11, lines(exp(loglen), exp(fitted), lty=2, col=1, lwd=2))
legend("topleft", legend = c("Lat= 30, Lon=160, Month=2","Lat=  0,  Lon=200, Month=5","Lat=-10, Lon=230, Month=8","Lat= 35, Lon=210, Month=11"),
       col=c(1,2,2,1), lty=c(1,1,2,2), lwd=2)
savePlot(paste0("../figures3/mod13crgtbxy_ml_loglen_array.png"), type = "png")

windows(9, 9); par(mfrow=c(2,2))
with(predMF_20_160_2, plot(loglen, fitted, xlab = "loglen", ylab = "logwt", type = "l", lwd=2, ylim = c(1.5,5)))
with(predMF_20_160_5, lines(loglen, fitted, lty=1, col=2, lwd=2))
with(predMF_20_160_8, lines(loglen, fitted, lty=2, col=2, lwd=2))
with(predMF_20_160_11, lines(loglen, fitted, lty=2, col=1, lwd=2))
legend("topleft", legend = paste0("Lat=20, Lon=160, Month=", c(2, 5, 8, 11)), col=c(1,2,2,1), lty=c(1,1,2,2), lwd=2)

with(predMF_20_160_2, plot(loglen, fitted, xlab = "loglen", ylab = "logwt", type = "l", lwd=2, ylim = c(1.5,5)))
with(predMF_0_160_2, lines(loglen, fitted, lty=1, col=2, lwd=2))
with(predMF_0_190_2, lines(loglen, fitted, lty=2, col=2, lwd=2))
with(predMF_x30_190_2, lines(loglen, fitted, lty=2, col=1, lwd=2))
legend("topleft", legend = c("Lat= 20, Lon=160, Month=2","Lat=  0,  Lon=160, Month=2","Lat=  0,  Lon=190, Month=2","Lat=-30, Lon=190, Month=2"),
       col=c(1,2,2,1), lty=c(1,1,2,2), lwd=2)

with(predMF_20_160_8, plot(loglen, fitted, xlab = "loglen", ylab = "logwt", type = "l", lwd=2, ylim = c(1.5,5)))
with(predMF_0_160_8, lines(loglen, fitted, lty=1, col=2, lwd=2))
with(predMF_0_190_8, lines(loglen, fitted, lty=2, col=2, lwd=2))
with(predMF_x30_190_8, lines(loglen, fitted, lty=2, col=1, lwd=2))
legend("topleft", legend = c("Lat= 20, Lon=160, Month=8","Lat=  0,  Lon=160, Month=8","Lat=  0,  Lon=190, Month=8","Lat=-30, Lon=190, Month=8"),
       col=c(1,2,2,1), lty=c(1,1,2,2), lwd=2)

with(predMF_30_160_2, plot(loglen, fitted, xlab = "loglen", ylab = "logwt", type = "l", lwd=2, ylim = c(1.5,5)))
with(predMF_0_200_5, lines(loglen, fitted, lty=1, col=2, lwd=2))
with(predMF_x10_230_8, lines(loglen, fitted, lty=2, col=2, lwd=2))
with(predMF_35_210_11, lines(loglen, fitted, lty=2, col=1, lwd=2))
legend("topleft", legend = c("Lat= 30, Lon=160, Month=2","Lat=  0,  Lon=200, Month=5","Lat=-10, Lon=230, Month=8","Lat= 35, Lon=210, Month=11"),
       col=c(1,2,2,1), lty=c(1,1,2,2), lwd=2)
savePlot(paste0("../figures3/mod13crgtbxy_ml_loglen_array2.png"), type = "png")


mod4rgtmx_ml_scat  %<-% dogam(dat=drgtm_r, llm_cc = 'cc', hkc=FALSE,  fam = 'scat')

modbase_ggmx_ml_scat %<-% gam(logwt ~ loglen + s(obs_name, bs="re"), data=drggm_ro, gama=1.4, family = 'scat')
modbase_ggfx_ml_scat %<-% gam(logwt ~ loglen + s(obs_name, bs="re"), data=drggf_ro, gama=1.4, family = 'scat')
drggb_ro <- rbind(drggm_ro,drggf_ro)
modbase_ggbx_ml_scat %<-% gam(logwt ~ loglen + s(obs_name, bs="re"), data=drggb_ro, gama=1.4, family = 'scat')

modbase_gtmx_ml_scat %<-% gam(logwt ~ loglen + s(obs_name, bs="re"), data=drgtm_ro, gama=1.4, family = 'scat')
modbase_gtfx_ml_scat %<-% gam(logwt ~ loglen + s(obs_name, bs="re"), data=drgtf_ro, gama=1.4, family = 'scat')
modbase_gtbx_ml_scat %<-% gam(logwt ~ loglen + s(obs_name, bs="re"), data=drgtb_ro, gama=1.4, family = 'scat')

summary(modbase_gtmx_ml_scat)
summary(modbase_gtfx_ml_scat)
summary(modbase_gtbx_ml_scat)
sum_bx <- summary(modbase_gtbx_ml_scat)
names(sum_bx)
sum_bx$p.coeff
sum_bx$se
sum_bx$se[1]
sum_bx$se[2]/sum_bx$p.coeff[2]
exp(sum_bx$p.coeff[1])

coff <- modbase_gtbx_ml_scat$coefficients[1:2]
summ <- summary(modbase_gtbx_ml_scat)
coffse <- summ$se[1:2]
sum_bx <- summary(modbase_gtbx_ml_scat)
c(i, exp(coff[1]), coffse[1], coff[2], coffse[2]/coff[2], sum_bx$r.sq, sum_bx$n)

coff <- modbase_ggbx_ml_scat$coefficients[1:2]
summ <- summary(modbase_ggbx_ml_scat)
coffse <- summ$se[1:2]
sum_bx <- summary(modbase_ggbx_ml_scat)
c(i, exp(coff[1]), coffse[1], coff[2], coffse[2]/coff[2], sum_bx$r.sq, sum_bx$n)


sum_bg <- summary(modbase_ggbx_ml_scat)
sum_bg$p.coeff
exp(sum_bg$p.coeff[1])

str(drggm_ro)

reg_gg <- list()
reg_gt <- list()
rgg <- table(drggb_ro$region)
rgt <- table(drgtb_ro$region)
for(rr in 1:length(rgg)) {
  if(rgg[rr] > 150) {
    rr1 <- as.numeric(names(rgg[rr]))
    reg_gg[[rr1]] <- gam(logwt ~ loglen + s(obs_name, bs="re"), data=filter(drggb_ro, region==rr1), gama=1.4, family = 'scat')
  }
}

for(rr in 1:length(rgt)) {
  if(rgt[rr] > 150) {
    rr1 <- as.numeric(names(rgt[rr]))
    reg_gt[[rr1]] <- gam(logwt ~ loglen + s(obs_name, bs="re"), data=filter(drgtb_ro, region==rr1), gama=1.4, family = 'scat')
  }
}

names(summary(reg_gg[[2]]))
names(summary(reg_gt[[2]]))

for(i in 1:11) {
  if(!is.null(reg_gg[[i]])) {
    #    print(i)
    coff <- reg_gg[[i]]$coefficients[1:2]
    summ <- summary(reg_gg[[i]])
    coffse <- summ$se[1:2]
    #    print(c(reg_gg[[i]]$coefficients[1:2], summary(reg_gg[[i]])$se[1:2]))
    print(c(i, exp(coff[1]), coffse[1], coff[2], coffse[2]/coff[2], summ$r.sq, summ$n))
  }
}

for(i in 1:11) {
  if(!is.null(reg_gt[[i]])) {
    coff <- reg_gt[[i]]$coefficients[1:2]
    summ <- summary(reg_gt[[i]])
    coffse <- summ$se[1:2]
    #    print(c(reg_gg[[i]]$coefficients[1:2], summary(reg_gg[[i]])$se[1:2]))
    print(c(i, exp(coff[1]), coffse[1], coff[2], coffse[2]/coff[2], summ$r.sq, summ$n))
  }
}

save(reg_gg, reg_gt, mod4rgtmx_ml_scat, 
     modbase_ggmx_ml_scat, modbase_ggfx_ml_scat, modbase_ggbx_ml_scat,
     modbase_gtmx_ml_scat, modbase_gtfx_ml_scat, modbase_gtbx_ml_scat, file = "../resubmit/store/modbase_all.RData")
rm(reg_gg, reg_gt, mod4rgtmx_ml_scat, 
   modbase_ggmx_ml_scat, modbase_ggfx_ml_scat, modbase_ggbx_ml_scat,
   modbase_gtmx_ml_scat, modbase_gtfx_ml_scat, modbase_gtbx_ml_scat)
load("./resubmit/store/modbase_all.RData")

# Resubmit
# Estimate difference from base
rep_lens <- c(80, 100, 120, 140, 160)

# Pooled parameters (example for 2015-2022)
a_poolgg <- 1.108e-5
b_poolgg <- 3.087
a_poolgt <- 1.274e-5
b_poolgt <- 3.058

# For each region, calculate % difference
# % diff = 100 * (a_reg * L^b_reg - a_pool * L^b_pool) / (a_pool * L^b_pool)
for(i in 1:11) {
  if(!is.null(reg_gg[[i]])) {
    coff <- reg_gg[[i]]$coefficients[1:2]
    a_reg <- exp(coff[1])
    b_reg <- coff[2]
    diff_perc <- 100 * (a_reg * rep_lens^b_reg - a_poolgg * rep_lens^b_poolgg) / (a_poolgg * rep_lens^b_poolgg)
    print(c(i, diff_perc))
  }
}

for(i in 1:11) {
  if(!is.null(reg_gt[[i]])) {
    coff <- reg_gt[[i]]$coefficients[1:2]
    a_reg <- exp(coff[1])
    b_reg <- coff[2]
    diff_perc <- 100 * (a_reg * rep_lens^b_reg - a_poolgt * rep_lens^b_poolgt) / (a_poolgt * rep_lens^b_poolgt)
    print(c(i, diff_perc))
  }
}

drggb_ro %>%
  ungroup() %>%
  mutate(len_bin = cut(length, breaks = seq(10, 210, 20), right = FALSE,
                       labels = paste0(seq(10, 190, 20), "-", seq(29, 219, 20)))) %>%
  count(region, len_bin) %>%
  pivot_wider(names_from = len_bin, values_from = n, values_fill = 0, names_sort = TRUE)


rep_lens <- c(100, 120, 140, 160)

# Pooled parameters
pooled <- tibble(
  period = c("2009-2014", "2015-2022"),
  a = c(1.108e-5, 1.274e-5),
  b = c(3.087, 3.058)
)

# Overall comparison
pooled %>%
  expand_grid(length = rep_lens) %>%
  mutate(pred_wt = a * length^b) %>%
  select(period, length, pred_wt) %>%
  pivot_wider(names_from = period, values_from = pred_wt) %>%
  mutate(pct_diff = 100 * (`2015-2022` - `2009-2014`) / `2009-2014`)

# Regional comparison - extract coefficients from both model lists
extract_coefs <- function(model_list, period_label) {
  map_dfr(1:11, function(i) {
    if (!is.null(model_list[[i]])) {
      coef <- model_list[[i]]$coefficients[1:2]
      tibble(period = period_label, region = i, 
             a = exp(coef[1]), b = coef[2])
    }
  })
}

regional <- bind_rows(
  extract_coefs(reg_gg, "2009-2014"),  # adjust name if different
  extract_coefs(reg_gt, "2015-2022")
)

# Regional predictions and comparison
regional %>%
  expand_grid(length = rep_lens) %>%
  mutate(pred_wt = a * length^b) %>%
  select(period, region, length, pred_wt) %>%
  pivot_wider(names_from = period, values_from = pred_wt) %>%
  mutate(pct_diff = 100 * (`2015-2022` - `2009-2014`) / `2009-2014`) %>%
  arrange(region, length) %>% print(n=40)

# Resubmit: predictions on a grid 
# Create 5-degree grid
grid <- expand_grid(
  lon = seq(130, 230, 1),
  lat = seq(-35, 35, 1)
)

# Filter to locations with ≥2 samples within 3 degrees
# Using the female data (adjust data object name if needed)
grid_filtered <- grid %>%
  rowwise() %>%
  filter(sum(abs(drgtb_ro$lon - lon) <= 3 & abs(drgtb_ro$lat - lat) <= 3) >= 2) %>%
  ungroup()

# Create prediction data: grid × lengths × months
pred_data <- grid_filtered %>%
  expand_grid(
    length = c(100, 120, 140, 160),
    mon = 1:12
  ) %>%
  mutate(
    loglen = log(length),
    hbf = mean(drgtb_ro$hbf, na.rm = TRUE),
    yrf = drgtb_ro$yrf[1],
    obs_name = drgtb_ro$obs_name[1]
  )

pred_data$pred_logwt <- predict(mod10rgtfx_ml_scat, newdata = pred_data, 
                                type = "response", exclude = c("s(yrf)", "s(obs_name)"))

# Average across months and back-transform
pred_summary <- pred_data %>%
  group_by(lon, lat, length) %>%
  summarise(mean_logwt = mean(pred_logwt), .groups = "drop") %>%
  mutate(pred_wt = exp(mean_logwt))

# pred_summary
# table(pred_summary$lon)/5
# table(pred_summary$lat)/5
# sum(table(pred_summary$lat)/5)

# Quantiles of length by region
for (i in 1:11) {
  cat("Region", i, ":\n")
  print(quantile(filter(drgtb_ro, region == i)$length, probs = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95)))
}
quantile(drgtb_ro$length, probs = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95))


# Map
library(jsonlite)
library(ggplot2)
library(rnaturalearth)
library(sf)

# Simple overall model predictions (2015-2022 pooled)
a_pool <- 1.274e-5
b_pool <- 3.058

# Filter pred_summary to 80 and 160 cm, add simple model comparison
pred_compare <- pred_summary %>%
  filter(length %in% c(100, 120, 140, 160)) %>%
  mutate(
    simple_wt = a_pool * length^b_pool,
    pct_diff = 100 * (pred_wt - simple_wt) / simple_wt
  )

# Updated regions
regions_json <- '{
  "fishery_regions": [
    {"id": "R1", "nodes": [[120, 50], [170, 50], [170, 10], [140, 10], [140, 20], [120, 20], [120, 50]]},
    {"id": "R2", "nodes": [[170, 50], [-150, 50], [-150, 10], [170, 10], [170, 50]]},
    {"id": "R3", "nodes": [[140, 10], [170, 10], [170, -10], [160, -10], [160, -5], [155, -5], [155, 0], [140, 0], [140, 10]]},
    {"id": "R4", "nodes": [[170, 10], [-150, 10], [-150, -10], [170, -10], [170, 10]]},
    {"id": "R5", "nodes": [[140, -10], [170, -10], [170, -40], [140, -40], [140, -20], [150, -20], [150, -15], [140, -15], [140, -10]]},
    {"id": "R6", "nodes": [[170, -10], [-150, -10], [-150, -40], [170, -40], [170, -10]]},
    {"id": "R7", "nodes": [[110, 20], [140, 20], [140, -10], [110, -10], [110, 20]]},
    {"id": "R8", "nodes": [[140, 0], [155, 0], [155, -5], [160, -5], [160, -10]]},
    {"id": "R9", "nodes": [[140, -15], [150, -15], [150, -20], [140, -20], [140, -15]]},
    {"id": "R10", "nodes": [[-150, 10], [-90, 10], [-90, -40], [-150, -40], [-150, 10]]},
    {"id": "R11", "nodes": [[-150, 50], [-110, 50], [-110, 10], [-150, 10], [-150, 50]]}
  ]
}'

regions_list <- fromJSON(regions_json)$fishery_regions

# Convert regions to df, shift to 0-360
regions_df <- map_dfr(1:nrow(regions_list), function(i) {
  nodes <- regions_list$nodes[[i]]
  tibble(
    region = regions_list$id[i],
    lon = ifelse(nodes[,1] < 0, nodes[,1] + 360, nodes[,1]),
    lat = nodes[,2]
  )
})

# Get world from rnaturalearth, shift to Pacific-centered (0-360)
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_break_antimeridian(lon_0 = 180) %>%
  st_shift_longitude()

# Plot
# Ensure pred_compare uses 0-360 longitude
pred_compare <- pred_compare %>%
  mutate(lon = ifelse(lon < 0, lon + 360, lon))

library(metR)

p <- ggplot() +
  geom_tile(data = pred_compare, aes(x = lon, y = lat, fill = pct_diff)) +
  geom_contour(data = pred_compare, aes(x = lon, y = lat, z = pct_diff),
               breaks = seq(-10, 10, 1), colour = "black", linewidth = 0.2) +
  geom_text_contour(data = pred_compare, aes(x = lon, y = lat, z = pct_diff),
                    breaks = seq(-10, 10, 1), stroke = 0.2, size = 2) +
  geom_sf(data = world, fill = "grey80", colour = "grey50", linewidth = 0.2) +
  geom_path(data = regions_df, aes(x = lon, y = lat, group = region), 
            colour = "salmon", linewidth = 1) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                       name = "% diff") +
  scale_x_continuous(breaks = seq(120, 240, 20)) +
  facet_wrap(~ length, labeller = labeller(length = function(x) paste(x, "cm"))) +
  coord_sf(xlim = c(130, 230), ylim = c(-45, 55), expand = TRUE) +
  labs(x = "Longitude", y = "Latitude", 
       title = "") +
  theme_minimal(base_family = "Arial")
#  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("../figures_resubmit/spatial_lwr_comparison2.png", p, width = 10, height = 6, dpi = 300)
ggsave("../figures_resubmit/Fig12_spatial_lwr_comparison2.tiff", p, 
       width = 10, height = 6, dpi = 1000,
       compression = "lzw")
str(pred_compare)
pred_compare %>% filter(lon==150 & lat==20) 


# Create sf polygons for regions
region_polys <- map_dfr(1:nrow(regions_list), function(i) {
  nodes <- regions_list$nodes[[i]]
  lon <- ifelse(nodes[,1] < 0, nodes[,1] + 360, nodes[,1])
  lat <- nodes[,2]
  # Close polygon if not already closed
  if (lon[1] != lon[length(lon)] | lat[1] != lat[length(lat)]) {
    lon <- c(lon, lon[1])
    lat <- c(lat, lat[1])
  }
  poly <- st_polygon(list(cbind(lon, lat)))
  st_sf(region = regions_list$id[i], geometry = st_sfc(poly, crs = 4326))
})

# Convert pred_summary points to sf
pred_pts <- pred_summary %>%
  mutate(lon360 = ifelse(lon < 0, lon + 360, lon)) %>%
  st_as_sf(coords = c("lon360", "lat"), crs = 4326, remove = FALSE)

# Assign each point to a region
pred_with_region <- st_join(pred_pts, region_polys, join = st_within) %>%
  st_drop_geometry() %>%
  mutate(region_num = as.numeric(gsub("R", "", region)))

# Regional coefficients (2015-2022) - adjust if needed
regional_coefs <- tibble(
  region_num = c(1, 2, 3, 4, 6, 10, 11),
  a_reg = c(1.66e-05, 1.10e-05, 1.26e-05, 1.01e-05, 1.22e-05, 1.07e-05, 2.39e-05),
  b_reg = c(3.001, 3.090, 3.062, 3.104, 3.065, 3.090, 2.931)
)

# Join and calculate regional predictions
pred_compare_reg <- pred_with_region %>%
  filter(length %in% c(100,120, 140, 160)) %>%
  left_join(regional_coefs, by = "region_num") %>%
  filter(!is.na(a_reg)) %>%
  mutate(
    lon = ifelse(lon < 0, lon + 360, lon),
    regional_wt = a_reg * length^b_reg,
    pct_diff = 100 * (pred_wt - regional_wt) / regional_wt
  )

# Plot with contours
p_reg <- ggplot() +
  geom_tile(data = pred_compare_reg, aes(x = lon, y = lat, fill = pct_diff)) +
  # geom_contour(data = pred_compare_reg, aes(x = lon, y = lat, z = pct_diff),
  #              breaks = seq(-20, 20, 1), colour = "black", linewidth = 0.3) +
  # geom_text_contour(data = pred_compare, aes(x = lon, y = lat, z = pct_diff),
  #                   breaks = seq(-10, 10, 1), stroke = 0.2, size = 2) +
  geom_sf(data = world, fill = "grey80", colour = "grey50", linewidth = 0.2) +
  geom_path(data = regions_df, aes(x = lon, y = lat, group = region), 
            colour = "salmon", linewidth = 1) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                       name = "% diff") +
  facet_wrap(~ length, labeller = labeller(length = function(x) paste(x, "cm"))) +
  coord_sf(xlim = c(120, 240), ylim = c(-45, 55), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude", 
       title = "Spatial model vs regional LWR: % difference in predicted weight") +
  theme_minimal()

ggsave("../figures3/spatial_vs_regional_lwr_comparison.png", p_reg, width = 12, height = 6, dpi = 300)

# Resubmit - table of sample szies by region - sets, trips. observers
drgtb_ro %>%
  group_by(region) %>%
  summarise(
    n_sets = n_distinct(setid),
    n_trips = n_distinct(tripID),
    n_obs = n_distinct(obs_name)
  ) %>%
  arrange(region) %>%
  print(n=11)

# Table of sets per year and region in a grid
drgtf_ro %>%
  group_by(region, year) %>%
  summarise(
    n_sets = n_distinct(setid),
  ) %>%
  pivot_wider(names_from = year, values_from = n_sets, values_fill = 0, names_sort = TRUE) %>%
  arrange(region) %>%
  print(n = 11)

# Resubmit HBF effects ------------------------

mod <- mod10rgtfx_ml_scat
dat <- drgtf_ro

# ==============================================================================
# 1. STANDARDIZED EFFECT SIZES WITH CIs
# ==============================================================================

# Get HBF predictions across its range
hbf_seq <- seq(min(dat$hbf), max(dat$hbf), length.out = 100)

# Prediction data at median/mode values
pred_data <- data.frame(
  loglen = median(dat$loglen),
  lon = median(dat$lon),
  lat = median(dat$lat),
  mon = 6,
  hbf = hbf_seq,
  obs_name = factor(names(sort(table(dat$obs_name), decreasing = TRUE))[1], 
                    levels = levels(dat$obs_name)),
  yrf = factor(names(sort(table(dat$yrf), decreasing = TRUE))[1], 
               levels = levels(dat$yrf))
)

# Get predictions for HBF term only
preds <- predict(mod, newdata = pred_data, type = "terms", se.fit = TRUE)
hbf_col <- grep("hbf", colnames(preds$fit))

hbf_effect <- preds$fit[, hbf_col]
hbf_se <- preds$se.fit[, hbf_col]

# Calculate standardized effect
effect_range <- max(hbf_effect) - min(hbf_effect)
idx_min <- which.min(hbf_effect)
idx_max <- which.max(hbf_effect)
diff_se <- sqrt(hbf_se[idx_max]^2 + hbf_se[idx_min]^2)

# Summary table
std_effects <- data.frame(
  term = "s(hbf)",
  hbf_range = paste0(round(min(dat$hbf)), "-", round(max(dat$hbf))),
  effect_logwt = round(effect_range, 4),
  effect_se = round(diff_se, 4),
  CI_lower = round(effect_range - 1.96 * diff_se, 4),
  CI_upper = round(effect_range + 1.96 * diff_se, 4),
  pct_weight_change = round((exp(effect_range) - 1) * 100, 1),
  pct_CI_lower = round((exp(effect_range - 1.96 * diff_se) - 1) * 100, 1),
  pct_CI_upper = round((exp(effect_range + 1.96 * diff_se) - 1) * 100, 1)
)

cat("\n=== STANDARDIZED HBF EFFECT ===\n")
print(std_effects)

# ==============================================================================
# 2. INTERACTION TESTS: HBF x SPACE/SEASON
# ==============================================================================

cat("\n=== FITTING INTERACTION MODELS ===\n")

# Base model (already fitted)
cat("Base model AIC:", AIC(mod), "\n")

# HBF x latitude
cat("Fitting HBF x latitude...\n")
mod_hbf_lat %<-% gam(
  logwt ~ s(loglen, k = 20) + s(obs_name, bs = "re") + 
    te(lon, lat, mon, k = c(10, 10, 4), bs = c("cr", "cr", "cc")) + 
    ti(hbf, k = 8) + ti(lat, k = 8) + ti(hbf, lat, k = c(5, 5)) + yrf,
  data = dat, family = scat(), method = 'ML', gamma = 1.4
)

# HBF x longitude  
cat("Fitting HBF x longitude...\n")
mod_hbf_lon %<-% gam(
  logwt ~ s(loglen, k = 20) + s(obs_name, bs = "re") + 
    te(lon, lat, mon, k = c(10, 10, 4), bs = c("cr", "cr", "cc")) + 
    ti(hbf, k = 8) + ti(lon, k = 8) + ti(hbf, lon, k = c(5, 5)) + yrf,
  data = dat, family = scat(), method = 'ML', gamma = 1.4
)

# HBF x month
cat("Fitting HBF x month...\n")
mod_hbf_mon %<-% gam(
  logwt ~ s(loglen, k = 20) + s(obs_name, bs = "re") + 
    te(lon, lat, mon, k = c(10, 10, 4), bs = c("cr", "cr", "cc")) + 
    ti(hbf, k = 8) + ti(mon, k = 5, bs = "cc") + ti(hbf, mon, k = c(5, 4), bs = c("cr", "cc")) + yrf,
  data = dat, family = scat(), method = 'ML', gamma = 1.4
)

# Model comparison
model_comparison <- data.frame(
  model = c("Base (no interaction)", "HBF x Latitude", "HBF x Longitude", "HBF x Month"),
  AIC = c(AIC(mod), AIC(mod_hbf_lat), AIC(mod_hbf_lon), AIC(mod_hbf_mon)),
  dev_expl = c(summary(mod)$dev.expl, summary(mod_hbf_lat)$dev.expl, 
               summary(mod_hbf_lon)$dev.expl, summary(mod_hbf_mon)$dev.expl)
)
model_comparison$dAIC <- model_comparison$AIC - min(model_comparison$AIC)

cat("\n=== MODEL COMPARISON ===\n")
print(model_comparison)

# Extract interaction term statistics
get_interaction_pval <- function(mod, pattern) {
  s <- summary(mod)
  idx <- grep(pattern, rownames(s$s.table))
  if (length(idx) > 0) {
    # Check column names - scat uses Chi.sq, gaussian uses F
    stat_col <- if ("F" %in% colnames(s$s.table)) "F" else "Chi.sq"
    return(data.frame(
      edf = round(s$s.table[idx, "edf"], 2),
      stat = round(s$s.table[idx, stat_col], 2),
      p_value = signif(s$s.table[idx, "p-value"], 3)
    ))
  }
  return(NULL)
}

interaction_stats <- rbind(
  cbind(interaction = "HBF x Latitude", get_interaction_pval(mod_hbf_lat, "ti\\(hbf,lat\\)")),
  cbind(interaction = "HBF x Longitude", get_interaction_pval(mod_hbf_lon, "ti\\(hbf,lon\\)")),
  cbind(interaction = "HBF x Month", get_interaction_pval(mod_hbf_mon, "ti\\(hbf,mon\\)"))
)

cat("\n=== INTERACTION TERM STATISTICS ===\n")
print(interaction_stats)

# End HBF for resubmit ---------------------------

# ==============================================================================
# 3. SAVE RESULTS
# ==============================================================================

write.csv(std_effects, "tables/hbf_standardized_effect.csv", row.names = FALSE)
write.csv(model_comparison, "tables/hbf_interaction_model_comparison.csv", row.names = FALSE)
write.csv(interaction_stats, "tables/hbf_interaction_statistics.csv", row.names = FALSE)

cat("\nResults saved to tables/\n")



#######################------------------------------------------------------------------------------
rm(list=ls(pattern="^mod"))
save.image()




# Plot spatial patterns of lightsticks
a <- tapply(as.numeric(as.character(drgtf_r$lightstick)), list(drgtf_r$lon, drgtf_r$lat), mean)
windows(width=20, height = 15)
image(sort(unique(drgtf_r$lon)),sort(unique(drgtf_r$lat)),  a)
contour(sort(unique(drgtf_r$lon)),sort(unique(drgtf_r$lat)),  (a), add=TRUE, labcex=0.8, levels = c(10,100,1000,2000,3000,5000,7000,10000))
map(add=T, database = "world2", fill=TRUE)
savePlot(file = "../figures3/lightstick_map.png", type = "png")

# Plot spatial patterns of HBF
drgtm_r$latf <- factor(drgtm_r$lat, levels = -40:40)
drgtm_r$lonf <- factor(drgtm_r$lon, levels = 125:250)
a <- tapply(as.numeric(as.character(drgtm_r$hbf)), list(drgtm_r$lonf, drgtm_r$latf), mean)
windows(width=15, height = 12)
image(125:250,-40:40,  a, xlab = "Longitude", ylab = "Latitude")
contour(125:250,-40:40,  a, add=TRUE, labcex=1, levels = c(10,15,20,25,30))
map(add=T, database = "world2", fill=TRUE)
savePlot(file = "../figures3/hbf_map.png", type = "png")

drggm_r$latf <- factor(drggm_r$lat, levels = -40:40)
drggm_r$lonf <- factor(drggm_r$lon, levels = 125:250)
a <- tapply(as.numeric(as.character(drggm_r$hbf)), list(drggm_r$lonf, drggm_r$latf), mean)
windows(width=15, height = 12)
image(125:250,-40:40,  a, xlab = "Longitude", ylab = "Latitude")
contour(125:250,-40:40,  a, add=TRUE, labcex=1, levels = c(10,15,20,25,30))
map(add=T, database = "world2", fill=TRUE)
savePlot(file = "../figures3/hbf_map_early.png", type = "png")

windows()
hist(a)

table(dat$obs_name)
a <- drgtf_r[drgtf_r$lon > 210 & drgtf_r$lon < 220,]
aa <- table(a$obs_name)
aa[order(aa)]

# are lightsticks linked to observer or to the trip? 
windows(); par(mfrow=c(2,1))
hist(tapply(as.numeric(as.character(drgtf_r$lightstick)), drgtf_r$tripID, mean))
hist(tapply(as.numeric(as.character(drgtf_r$lightstick)), drgtf_r$obs_name, mean))
length(unique(drgtf_r$tripID))
length(unique(drgtf_r$obs_name))
tmp <- tapply(as.numeric(as.character(drgtf_r$lightstick)), drgtf_r$tripID, mean)
length(tmp[tmp> 0.9])
unique(drgtm_r$year)

# Plot results by region
# Plot area and seasonal effects

# Examine sex ratio by year and location
str(drgtf_r)
table(drgtf_r$year, drgtf_r$region)
a <- dat[dat$sex %in% c("F", "M"),]
table(a$year, a$region, a$sex)

str(a)
a$region <- relevel(a$region, ref = "4")
a$yrf <- relevel(a$yrf, ref = "2012")

a <- a[!a$region %in% c(0,7,8),]
a$region <- factor(a$region)
a$yrf <- factor(a$yrf)

sexmo1 <- gam(sex=="F" ~ region * yrf, data = a)
sexmo1
summary(sexmo1)
windows()
plot(sexmo1, pages = 1, all.terms=T)

sr <- tapply(a$sex, list(a$yrf, a$region,a$sex), length)
sr[is.na(sr)] <- 0
sr1 <- sr[,,1]/(sr[,,1]+sr[,,2])
sr1

drgg_set <- drgg %>% group_by(setid) %>% slice_sample(n=1)
drgg_set$ls <- as.numeric(as.character(drgg_set$lightstick))
aggregate(ls ~ year + region, data = drgg_set, FUN=sum)
with(drgg_set, tapply(ls, list(year, region), mean))

drgt_set <- drgt %>% group_by(setid) %>% slice_sample(n=1)
drgt_set$ls <- as.numeric(as.character(drgt_set$lightstick))
aggregate(ls ~ year + region, data = drgt_set, FUN=sum)
with(drgt_set, tapply(ls, list(year, region), mean))

table(drgt_set)

modsize <- gam(length ~ s(year, k=7) + te(lat,lon, k=c(10,10)), data=drgtf)
windows()
latt = seq(min(drgtf$lat), max(drgtf$lat))
lont <- seq(min(drgtf$lon), max(drgtf$lon))
newdat <- expand.grid(yrf = drgtf$yrf[1], lat = latt, lon = lont)
newdat$pred <- predict.gam(modsize, newdata=newdat)
pp <- tapply(newdat$pred, list(newdat$lon, newdat$lat), mean)
image(lont, latt, pp, zlim = c(80, 180))
contour(lont, latt, pp, zlim = c(80, 180), levels = seq(100,180, 5), add=TRUE)
map(add=T, fill=T)
points(drgtf$lon, drgtf$lat, pch=21)
plot.gam(modsize, pages = 1, all.terms = T)

modsize <- gam(length ~ s(year, k=7) + te(lon,lat, k=c(10,10)), data=drgtm)
latt = seq(min(drgtm$lat), max(drgtm$lat))
lont <- seq(min(drgtm$lon), max(drgtm$lon))
newdat <- expand.grid(year = drgtm$year[1], lat = latt, lon = lont)
newdat$pred <- predict.gam(modsize, newdata=newdat)
pp <- tapply(newdat$pred, list(newdat$lon, newdat$lat), mean)
windows()
image(lont, latt, pp, zlim = c(80, 180))
contour(lont, latt, pp, zlim = c(80, 180), levels = seq(100,180, 5), add=TRUE)
map(add=T, fill=T)
points(drgtm$lon, drgtm$lat, pch=21)
plot.gam(modsize, pages = 1, all.terms = T)

modsize <- gam(length ~ s(year, k=13) + te(lat,lon, k=c(10,10)), data=dat)
latt = seq(min(dat$lat), max(dat$lat))
lont <- seq(min(dat$lon), max(dat$lon))
newdat <- expand.grid(year = dat$year[1], lat = latt, lon = lont)
newdat$pred <- predict.gam(modsize, newdata=newdat)
pp <- tapply(newdat$pred, list(newdat$lon, newdat$lat), mean)
windows()
image(lont, latt, pp, zlim = c(80, 180))
contour(lont, latt, pp, zlim = c(80, 180), levels = seq(100,180, 5), add=TRUE)
map(database="world2", add=T, fill=T)
points(dat$lon, dat$lat, pch=21, cex=0.8)
savePlot("../figures3/modsize.png", type = "png")


# WGcomb Females
# choose one random sample from each
drgf_r <- dat %>% filter(sex %in% c("F")) %>% filter(processtype %in% c("RGG", "RGT")) %>% group_by(setid) %>% slice_sample(n=1)
drgf_r <- drgf_r[drgf_r$loglen >= 4.3,]
drgf_r <- drgf_r[drgf_r$hbf > 10,]
a <- as.character(drgf_r$obs_name)
a[is.na(a)] <- "xxx"
drgf_r$obs_name <- factor(a)
drgf_r$processtype <- factor(drgf_r$processtype)
table(is.na(drgf_r$processtype))
drgf_r <- drgf_r[is.na(drgf_r$processtype)==F,]

ls(pattern="mod[[:alnum:]]rg")
rm(list = ls(pattern="mod[[:alnum:]]rgm"))

dim(drgf_r) 
length(unique(drgf_r$obs_name))
mod1rgf %<-%  gam(logwt ~ s(loglen, k=20) + obs_name + wavescale + lightstick + te(lat, lon, k = c(10,10)) + weather + s(hbf) + HkCourse + yrf, data = drgf_r, family = gaussian)
mod2rgf %<-%  gam(logwt ~ s(loglen, k=20) + obs_name + wavescale + lightstick + te(lat, lon, k = c(10,10)) + weather + s(hbf) + yrf, data = drgf_r, family = gaussian)
mod3rgf %<-%  gam(logwt ~ s(loglen, k=20) + obs_name + wavescale + lightstick + te(lat, lon, k = c(10,10)) + weather + s(hbf), data = drgf_r, family = gaussian)
mod4rgf %<-%  gam(logwt ~ s(loglen, k=20) + obs_name + wavescale + lightstick + te(lat, lon, mon, k = c(10,10,4)) + weather + s(hbf) + yrf, data = drgf_r, family = gaussian)
mod5rgf %<-%  gam(logwt ~ s(loglen, k=20) + obs_name + wavescale + lightstick + te(lat, lon, mon, k = c(10,10,4)) + weather + s(hbf) + yrf + HkCourse, data = drgf_r, family = gaussian)
mod4rgfx %<-% gam(logwt ~ s(loglen, k=20) + s(obs_name, bs="re") + wavescale + lightstick + te(lat, lon, mon, k = c(10,10,4)) + weather + s(hbf) + yrf, data = drgf_r, family = gaussian)
mod4rgfxy %<-% gam(logwt ~ s(loglen, k=20) + s(obs_name, bs="re") + wavescale + lightstick + te(lat, lon, mon, k = c(10,10,4)) + weather + s(hbf) + s(year, k=13), data = drgf_r, family = gaussian)
mod4rgfxy_scat %<-% gam(logwt ~ s(loglen, k=20) + s(obs_name, bs="re") + wavescale + lightstick + te(lat, lon, mon, k = c(10,10,4)) + weather + s(hbf) + s(year, k=13), data = drgf_r, family = scat)
mod5rgfx %<-% gam(logwt ~ s(loglen, k=20) + s(obs_name, bs="re") + wavescale + lightstick + te(lat, lon, mon, k = c(10,10,4)) + weather + s(hbf) + yrf + HkCourse, data = drgf_r, family = gaussian)
mod6rgfx %<-% gam(logwt ~ s(loglen, k=20) + s(obs_name, bs="re") + wavescale +              te(lat, lon, mon, k = c(10,10,4)) + weather + s(hbf) + yrf, data = drgf_r, family = gaussian)
mod9rgfx %<-% gam(logwt ~ s(loglen, k=20) + s(obs_name, bs="re") + wavescale +              te(lat, lon, mon, k = c(10,10,4))           + s(hbf) + yrf, data = drgf_r, family = gaussian)
mod7rgfx %<-% gam(logwt ~ s(loglen, k=20) + s(obs_name, bs="re") + wavescale + qtr + te(lat, lon, k = c(10,10)) + te(lat, lon, k = c(10,10), by = qtr) + weather + s(hbf) + yrf, data = drgf_r, family = gaussian)
mod11rgfxy %<-% gam(logwt ~ s(loglen, k=20) + s(obs_name, bs="re") + wavescale + lightstick + te(lat, lon, mon, k = c(10,10,4)) + weather + s(hbf) + s(year, k=13) + processtype, data = drgf_r, family = gaussian)
str(drgf_r)
AIC(mod1rgf,mod2rgf,mod3rgf,mod4rgf,mod4rgfx,mod5rgf,mod5rgfx,mod6rgfx,mod7rgfx,mod9rgfx,mod4rgfxy,mod11rgfxy)
summary(mod4rgfxy)
windows(20,20)
plot.gam(mod4rgfx, pages = 1, all.terms = T)
plot.gam(mod4rgfxy, pages = 1, all.terms = T)
plot.gam(mod4rgfxy_scat, pages = 1, all.terms = T)
windows(20,15);
plot.gam(mod4rgfxy, select=4, ylim=c(-0.15, 0.15))
abline(h=0, col = 2)
windows(20,15);
plot.gam(mod4rgfxy, select=5, ylim=c(-0.15, 0.15))
abline(h=0, col = 2)
windows(20,15);
plot.gam(mod4rgfxy_scat, select=5, ylim=c(-0.15, 0.15))
abline(h=0, col = 2)
windows(10,10); par(mfrow=c(2,2))
hist(drgf$length)
plot(drgf$loglen, drgf$logwt)
windows(20,20)
plot.gam(mod11rgfxy, pages = 1, all.terms = T)
windows(20,15);
plot.gam(mod11rgfxy, select=5, ylim=c(-0.15, 0.15))
abline(h=0, col = 2)

mod4rgfx_scat <- gam(logwt ~ s(loglen, k=20) + s(obs_name, bs="re") + wavescale + lightstick + te(lat, lon, mon, k = c(10,10,4)) + weather + s(hbf) + yrf, data = drgf_r, family = scat)
mod5rgfx_scat <- gam(logwt ~ s(loglen, k=20) + s(obs_name, bs="re") + wavescale + lightstick + te(lat, lon, mon, k = c(10,10,4)) + weather + s(hbf) + yrf + HkCourse, data = drgf_r, family = scat)
mod9rgfx_scat <- gam(logwt ~ s(loglen, k=20) + s(obs_name, bs="re") + wavescale + te(lat, lon, mon, k = c(10,10,4)) + s(hbf) + yrf, data = drgf_r, family = scat)
summary(mod9rgfx_scat)
windows()
plot.gam(mod9rgfx_scat, pages = 1, all.terms = T)
AIC(mod1rgf,mod2rgf,mod3rgf,mod4rgf,mod4rgfx,mod5rgf,mod5rgfx,mod6rgfx,mod7rgfx,mod9rgfx,mod5rgfx_scat,mod9rgfx_scat)

# WGcomb Males
# choose one random sample from each
drgm_r <- dat %>% filter(sex %in% c("M")) %>% filter(processtype %in% c("RGG", "RGT")) %>% group_by(setid) %>% slice_sample(n=1)
drgm_r <- drgm_r[drgm_r$loglen >= 4.3,]
drgm_r <- drgm_r[drgm_r$hbf > 10,]
a <- as.character(drgm_r$obs_name)
a[is.na(a)] <- "xxx"
drgm_r$obs_name <- factor(a)
dim(drgm_r) 
length(unique(drgm_r$obs_name))
mod1rgm <-  gam(logwt ~ s(loglen, k=20) + obs_name + wavescale + lightstick + te(lat, lon, k = c(10,10)) + weather + s(hbf) + HkCourse + yrf, data = drgm_r, family = gaussian)
mod2rgm <-  gam(logwt ~ s(loglen, k=20) + obs_name + wavescale + lightstick + te(lat, lon, k = c(10,10)) + weather + s(hbf) + yrf, data = drgm_r, family = gaussian)
mod3rgm <-  gam(logwt ~ s(loglen, k=20) + obs_name + wavescale + lightstick + te(lat, lon, k = c(10,10)) + weather + s(hbf), data = drgm_r, family = gaussian)
mod4rgm <-  gam(logwt ~ s(loglen, k=20) + obs_name + wavescale + lightstick + te(lat, lon, mon, k = c(10,10,4)) + weather + s(hbf) + yrf, data = drgm_r, family = gaussian)
mod5rgm <-  gam(logwt ~ s(loglen, k=20) + obs_name + wavescale + lightstick + te(lat, lon, mon, k = c(10,10,4)) + weather + s(hbf) + yrf + HkCourse, data = drgm_r, family = gaussian)
mod4rgmx <- gam(logwt ~ s(loglen, k=20) + s(obs_name, bs="re") + wavescale + lightstick + te(lat, lon, mon, k = c(10,10,4)) + weather + s(hbf) + yrf, data = drgm_r, family = gaussian)
mod4rgmxy <- gam(logwt ~ s(loglen, k=20) + s(obs_name, bs="re") + wavescale + lightstick + te(lat, lon, mon, k = c(10,10,4)) + weather + s(hbf) + s(year, k=13), data = drgm_r, family = gaussian)
mod4rgmxy_scat <- gam(logwt ~ s(loglen, k=20) + s(obs_name, bs="re") + wavescale + lightstick + te(lat, lon, mon, k = c(10,10,4)) + weather + s(hbf) + s(year, k=13), data = drgm_r, family = scat)
mod5rgmx <- gam(logwt ~ s(loglen, k=20) + s(obs_name, bs="re") + wavescale + lightstick + te(lat, lon, mon, k = c(10,10,4)) + weather + s(hbf) + yrf + HkCourse, data = drgm_r, family = gaussian)
mod6rgmx <- gam(logwt ~ s(loglen, k=20) + s(obs_name, bs="re") + wavescale +              te(lat, lon, mon, k = c(10,10,4)) + weather + s(hbf) + yrf, data = drgm_r, family = gaussian)
mod9rgmx <- gam(logwt ~ s(loglen, k=20) + s(obs_name, bs="re") + wavescale +              te(lat, lon, mon, k = c(10,10,4))           + s(hbf) + yrf, data = drgm_r, family = gaussian)
mod7rgmx <- gam(logwt ~ s(loglen, k=20) + s(obs_name, bs="re") + wavescale + qtr + te(lat, lon, k = c(10,10)) + te(lat, lon, k = c(10,10), by = qtr) + weather + s(hbf) + yrf, data = drgm_r, family = gaussian)
mod11rgmxy <- gam(logwt ~ s(loglen, k=20) + s(obs_name, bs="re") + wavescale + lightstick + te(lat, lon, mon, k = c(10,10,4)) + weather + s(hbf) + s(year, k=13) + processtype, data = drgm_r, family = gaussian)
AIC(mod1rgm,mod2rgm,mod3rgm,mod4rgm,mod4rgmx,mod5rgm,mod5rgmx,mod6rgmx,mod7rgmx,mod9rgmx,mod4rgmxy)
summary(mod4rgmxy)
windows(20,20)
plot.gam(mod4rgmx, pages = 1, all.terms = T)
plot.gam(mod4rgmxy, pages = 1, all.terms = T)
plot.gam(mod4rgmxy_scat, pages = 1, all.terms = T)
savePlot("../figures3/mod4rgmxy_scat.png", type = "png")
windows(20,15);
plot.gam(mod4rgmxy, select=4, ylim=c(-0.15, 0.15))
abline(h=0, col = 2)
windows(20,15);
plot.gam(mod4rgmxy, select=5, ylim=c(-0.15, 0.15))
abline(h=0, col = 2)
windows(20,15);
plot.gam(mod4rgmxy_scat, select=5, ylim=c(-0.15, 0.15))
abline(h=0, col = 2)
savePlot("../figures3/mod4rgmxy_scat_yr.png", type = "png")
windows(10,10); par(mfrow=c(2,2))
hist(drgm$length)
plot(drgm$loglen, drgm$logwt)

mod4rgmx_scat <- gam(logwt ~ s(loglen, k=20) + s(obs_name, bs="re") + wavescale + lightstick + te(lat, lon, mon, k = c(10,10,4)) + weather + s(hbf) + yrf, data = drgm_r, family = scat)
mod5rgmx_scat <- gam(logwt ~ s(loglen, k=20) + s(obs_name, bs="re") + wavescale + lightstick + te(lat, lon, mon, k = c(10,10,4)) + weather + s(hbf) + yrf + HkCourse, data = drgm_r, family = scat)
mod9rgmx_scat <- gam(logwt ~ s(loglen, k=20) + s(obs_name, bs="re") + wavescale + te(lat, lon, mon, k = c(10,10,4)) + s(hbf) + yrf, data = drgm_r, family = scat)
summary(mod9rgmx_scat)
windows()
plot.gam(mod9rgmx_scat, pages = 1, all.terms = T)
AIC(mod1rgm,mod2rgm,mod3rgm,mod4rgm,mod4rgmx,mod5rgm,mod5rgmx,mod6rgmx,mod7rgmx,mod9rgmx,mod5rgmx_scat,mod9rgmx_scat)
save.image()


mod10rgtfx_ml_scat
wt4 <- exp(pred_LWRx(mod1=mod10rgtfx_ml_scat, dat=drgtf_ro, xlat=0, xlon=250, xmon=6, xyr=2020)$fitted)
  
len <- 1:200
a1 <- 1.08e-5
b1=3.087
a2=1.275e-5
b2=3.058
a3=3.661e-5
b3=2.90182

wt1=a1*len^b1
wt2=a2*len^b2
wt3=a3*len^b3


wt1w <- 1.3264 * wt1^0.969
wt2w <- 1.3264 * wt2^0.969
wt4w <- 1.3264 * wt4^0.969

windows()
plot(len, wt1w, type = "l", xlab = "Length", ylab = "Weight", col = 2, ylim = c(0, 200))
lines(len, wt2w, col = 2, lty=2)
lines(len, wt3, col = 1)
lines(len, wt4w, col = 3)

##########---------------------------------------------------------------------------
# Load LWR relationships into R from a spreadsheet, and plot them
library(readxl)
lwr_all <- read_excel("../tables/Various LWRs 250509.xlsx")
lwr_all <- read_excel("../tables/TableS1_supp_LWR_list.xlsx")
lwr_all <- data.frame(lwr_all)
lwr_all[,1] <- NULL
#lwr_all <- lwr_all[1:30,]
#lwr_all <- lwr_all[-13,]
names(lwr_all)
names(lwr_all) <- tolower(names(lwr_all))
lwr_all <- filter(lwr_all, use.not=="O")

getfl <- function(x,ii=1) {
  if(is.na(x)) {
    return(NA)
  }
  a <- as.numeric(strsplit(x, "-")[[1]])
  return(a[ii])
}

lwr_all$flmax <- lwr_all$flmin <- NA
lwr_all$flmin <- sapply(lwr_all$fl.range, getfl, ii=1)
lwr_all$flmax <- sapply(lwr_all$fl.range, getfl, ii=2)
str(lwr_all)
lwr_all$ocean <- factor(lwr_all$ocean)
# Add the most recent WCPFC LWR estimate to the data frame before plotting

windows(9,9)
plot(c(20, max(lwr_all$flmax,na.rm=TRUE)), c(.31, 250), type = "n", xlab = "Length (cm)", ylab = "Weight (kg)", log="xy", cex.lab=1.2)
for(i in 1:37) {
  if(!is.na(lwr_all$flmin[i])) {
    a <- data.frame(len <- seq(lwr_all$flmin[i], lwr_all$flmax[i], by=1))
    a$wt <- lwr_all$a[i] * a$len^lwr_all$b[i]
    lines(a$len, a$wt, col = i, lty=as.numeric(lwr_all$ocean[i]))
  }
}
use <- !is.na(lwr_all$flmin)
legend("topleft", legend = lwr_all$ref.code[use], col=c(1:37)[use], lty=c(as.numeric(lwr_all$ocean))[use], lwd=1, ncol=2)
savePlot("../figures3/lwr_all_logxy.png", type = "png")

# without logxy
plot(c(20, max(lwr_all$flmax,na.rm=TRUE)), c(.31, 290), type = "n", xlab = "Length (cm)", ylab = "Weight (kg)", cex.lab=1.2)
for(i in 1:37) {
  if(!is.na(lwr_all$flmin[i])) {
    a <- data.frame(len <- seq(lwr_all$flmin[i], lwr_all$flmax[i], by=1))
    a$wt <- lwr_all$a[i] * a$len^lwr_all$b[i]
    lines(a$len, a$wt, col = i, lty=as.numeric(lwr_all$ocean[i]))
  }
}
use <- !is.na(lwr_all$flmin)
legend("topleft", legend = lwr_all$ref.code[use], col=c(1:37)[use], lty=c(as.numeric(lwr_all$ocean))[use], lwd=1, ncol=2)
savePlot("../figures3/lwr_all_nominal.png", type = "png")
