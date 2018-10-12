setwd("~/dropbox/globdev/")
source("src/globdev_functions.R")

vrbs = c("lifexp", "polity2", "avedu", "stature", "gini",
    "real_wage", "homicide_rate", "so2emis_pc")

library("rstan")
library("brms")
library("data.table")
library("writexl")

load(file = "/Users/auke/Downloads/data/unexplained_fits_bygroup.Rda")

pdf("out/unexplained_bygroup_brms.pdf", height = 4, width = 8)
for (nm in vrbs){
    allgroups = ranef(modlist[[nm]], probs = c(0.1, 0.5, 0.9))$groupXtime[, , 1]
    allgroups = as.data.frame(allgroups)
    allgroups$group = tstrsplit(rownames(allgroups), "_")[[1]]
    allgroups$year = as.numeric(tstrsplit(rownames(allgroups), "_")[[2]])
    par(mfrow = c(1, 3), mar =  c(4.5, 4, 3.5, 0.5), font.main = 1)
    for (grp in unique(allgroups$group)){
        onegroup = allgroups[allgroups$group == grp, ]
        matplot(allgroups$year, allgroups[, 3:5], type = 'n', main = paste(nm, grp, sep = ', '),
            xlab = 'year', ylab = '', xlim = c(1820, 2010))
        # for (grp in unique(allgroups$group)) lines(allgroups[allgroups$group == grp, 'year'], allgroups[allgroups$group == grp, '50%ile'], col = 'lightgray')
        abline(h = 0, col = 'gray')
        matlines(onegroup$year, onegroup[, 3:5],
            type = 'l', col = blue, lty = c(2, 1, 2))
    }
}
dev.off()

# load("out/elasticities_fits.Rda")
# load("out/elasticities_dats.Rda")
load(file = "/Users/auke/Downloads/data/unexplained_fits.Rda")

relist = lapply(modlist, function(m) ranef(m, probs = c(0.1, 0.5, 0.9))$y5[, 3:5, "Intercept"])

pdf("out/unexplained_brms.pdf", width = 11, height = 6)
par(mfrow = c(2, 4), mar = mar, font.main = 1)
for (nm in vrbs){
    matplot(rownames(relist[[nm]]), relist[[nm]],
        type = 'l', col = blue, lwd = c(1, 2, 1), lty = 1,
        main = lngvrbs[nm],
        xlab = 'year', ylab = '', xlim = c(1820, 2010))
    abline(h = 0, col = 'gray')
}
dev.off()

writexl::write_xlsx(
    lapply(relist, function(d) data.frame(year = rownames(d), d)), 
    path = 'out/unexpl_dat.xlsx')

ranef(modlist[['real_wage']], probs = c(0.1, 0.5, 0.9))$y5[, 3:5, 'Intercept']
colMeans(ranef(modlist[['real_wage']], summary = F)$y5 > 0)
ranef(modlist[['polity2']], probs = c(0.1, 0.5, 0.9))$y5[, 3:5, 'Intercept']
colMeans(ranef(modlist[['polity2']], summary = F)$y5 > 0)