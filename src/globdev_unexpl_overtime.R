setwd("~/dropbox/globdev")

library("data.table")
library("rstan")
rstan::rstan_options(auto_write = TRUE)
chains = min(parallel::detectCores() - 1, 4)
options(mc.cores = chains)
library("brms")

source("src/globdev_functions.R")

vrbs = c("gdppc2011", "lifexp", "polity2", "avedu", "stature", "gini", "real_wage", "homicide_rate", "so2emis_pc", "co2emis_pc", "biodiv")

clio = data.table::fread("dat/clioannual.csv")
clio5 = data.table::fread("dat/clio5y.csv")

clio = clio[year >= 1820]
clio5 = clio5[y5 >= 1820]

clio5[, polity2 := polity2 + 11]
clio[, polity2 := polity2 + 11]

clio5 = unique(clio[, list(ccode, iso3c)])[clio5, on = 'ccode']

# no devgroup calssification for:
clio5[is.na(devgroup), unique(iso3c)]
# all small or oil-producing outliers so yay! :)
clio5 = clio5[!is.na(devgroup), ]
# and of course it would have been shorter to just to an inner join but whatever

clio[, decade := floor(year / 10) * 10]
clio[, y5 := trunc(year / 5) * 5]

clio5[, clio25 := ccode %in% cliocodes]
clio5[, groupXtime := paste0(devgroup, "_", y5)]


residmods = lapply(vrbs[-1], function(x) lm(scale(get(x)) ~ log(gdppc2011 - 500) + factor(y5), data = clio5))
names(residmods) = vrbs[-1]

ypo = lapply(residmods, function(x) coef(x)[stringi::stri_detect_fixed(names(coef(x)), 'factor(y5)')])
yci = lapply(residmods, function(x) confint(x, level = 0.8)[stringi::stri_detect_fixed(names(coef(x)), 'factor(y5)'), ])
years = lapply(yci, function(x) as.numeric(stringi::stri_extract_last_regex(rownames(x), "\\d{4}")))

# pdf("out/unexplained_byvar.pdf", width = 10, height = 6)
par(mfrow = c(2, 4))
for (nm in vrbs[c(2:9)]){
    matplot(years[[nm]], yci[[nm]], 
        type = 'l', lty = 2, main = nm, pch = 1,
        xlab = 'year', ylab = 'stand. residual', xlim = c(1820, 2010), col = blue)
    lines(years[[nm]], ypo[[nm]], type = 'l', main = nm, col = blue)
    abline(h = 0, col = 'gray')
}
# dev.off()

# fit = brms::brm(scale(lifexp) ~ log(gdppc2011 - 500) + (1 | groupXtime), data = clio5, chains = 3)
# matplot(ranef(fit, probs = c(0.1, 0.9))$groupXtime[, 3:4, ])

modlist = list()
modlist[['real_wage']] = brms::brm(scale(real_wage) ~ log(gdppc2011 - 500) + (1 | groupXtime), data = clio5, chains = 3)
# matplot(ranef(modlist[['real_wage']])$groupXtime[, 3:4, ])

modlist[["lifexp"]] = update(modlist[['real_wage']], 
    formula. = scale(lifexp)      ~ ., newdata = clio5[gdppc2011 != 500], chains = 3)
modlist[["stature"]] = update(modlist[['real_wage']], 
    formula. = scale(stature)   ~ ., newdata = clio5[y5 <= 1980], chains = 3)
modlist[["avedu"]] = update(modlist[['real_wage']], 
    formula. = scale(avedu)     ~ ., newdata = clio5[y5 >= 1870], chains = 3)
modlist[["gini"]] = update(modlist[['real_wage']], 
    formula. = scale(gini)      ~ ., newdata = clio5[ccode %in% cliocodes], chains = 3)
modlist[["polity2"]] = update(modlist[['real_wage']], 
    formula. = scale(polity2)   ~ ., newdata = clio5, chains = 3)
modlist[["homicide_rate"]] = update(modlist[['real_wage']], 
    formula. = scale(homicide_rate) ~ ., newdata = clio5, chains = 3)
modlist[["so2emis_pc"]] = update(modlist[['real_wage']], 
    formula. = scale(so2emis_pc) ~ ., newdata = clio5, chains = 3)
modlist[["biodiv"]] = update(modlist[['real_wage']], 
    formula. = scale(biodiv) ~ ., newdata = clio5, chains = 3)

matplot(ranef(modlist[['gini']], probs = c(0.1, 0.5, 0.9))$y5[, 3:5, ], type = 'l')


# save(modlist, file = "out/unexplained_fits.Rda")
save(modlist, file = "~/downloads/data/unexplained_fits_bygroup.Rda")


# or

modlist = list()
framelist = list(model.frame(scale(real_wage) ~ log(gdppc2011 - 500) + y5 + devgroup, data = clio), 
               model.frame(scale(stature)   ~ log(gdppc2011 - 500) + y5 + devgroup, data = clio[y5 <= 1980]),
               model.frame(scale(lifexp)    ~ log(gdppc2011 - 500) + y5 + devgroup, data = clio),
               model.frame(scale(avedu)     ~ log(gdppc2011 - 500) + y5 + devgroup, data = clio[y5 >= 1870]),
               model.frame(scale(gini)      ~ log(gdppc2011 - 500) + y5 + devgroup, data = clio),
               model.frame(scale(polity2)   ~ log(gdppc2011 - 500) + y5 + devgroup, data = clio),
               model.frame(scale(homicide_rate) ~ log(gdppc2011 - 500) + y5 + devgroup, data = clio),
               model.frame(scale(so2emis_pc) ~ log(gdppc2011 - 500) + y5 + devgroup, data = clio),
               model.frame(scale(biodiv) ~ log(gdppc2011 - 500) + y5 + devgroup, data = clio))

for (d in framelist){
    print(names(d))
    print(dim(d))
    datlist = list(N = nrow(d), Y = c(d[, 1]), X = d[[2]],
        l = as.numeric(as.factor(d$y5)))
        # l = as.numeric(as.factor(paste(d$decade, d$devgroup))))
        # l1 = as.numeric(as.factor(d$decade)), # isn't it much easier to just create one var?
        # l2 = as.numeric(as.factor(d$devgroup)))
    datlist$L = length(unique(datlist$l))
    # datlist$L1 = length(unique(datlist$l1))
    # datlist$L2 = length(unique(datlist$l2))
    # fit  = stan(file = "src/hiernorm_coefmat.stan",
    fit  = stan(file = "src/hiernorm_varinterc.stan",
        data = datlist, chains = 1, pars = 'yhat', include = F)
    modlist[[names(d)[1]]] = fit
}
alphas = as.data.frame(fit, 'alpha')
matplot(t(apply(alphas, 2, quantile, c(0.1, 0.5, 0.9))))

save(modlist, file = "out/unexplained_fits.Rda")

library("brms")



matplot(ranef(modlist[['lifexp']])$y5[, 3:4, 'Intercept'])
matplot(t(apply(alphas, 2, quantile, c(0.1, 0.5, 0.9))))


clio[!is.na(lifexp), log(gdppc2011 - 500)]
clio[order(-log(gdppc2011 - 500)), list(gdppc2011)]

lm(scale(lifexp) ~ log(gdppc2011 - 500) + (1 | y5), data = clio)
plot(model.frame(scale(lifexp) ~ log(gdppc2011 - 500) + (1 | y5), data = clio)[, 1:2])


frmlist = list(brms::make_standata(scale(real_wage) ~ log(gdppc2011 - 500) + y5, data = clio), 
               brms::make_standata(scale(stature)   ~ log(gdppc2011 - 500) + y5, data = clio[y5 <= 1980]),
               brms::make_standata(scale(lifexp)    ~ log(gdppc2011 - 500) + y5, data = clio),
               brms::make_standata(scale(avedu)     ~ log(gdppc2011 - 500) + y5, data = clio[y5 >= 1870]),
               brms::make_standata(scale(gini)      ~ log(gdppc2011 - 500) + y5, data = clio),
               brms::make_standata(scale(polity2)   ~ log(gdppc2011 - 500) + y5, data = clio),
               brms::make_standata(scale(homicide_rate) ~ log(gdppc2011 - 500) + y5, data = clio),
               brms::make_standata(scale(so2emis_pc) ~ log(gdppc2011 - 500) + y5, data = clio),
               brms::make_standata(scale(biodiv) ~ log(gdppc2011 - 500) + y5, data = clio))



brms::make_standata(scale(real_wage) ~ log(gdppc2011 - 500) + (1 | y5), data = clio)
brmsfit = brms::brm(scale(real_wage) ~ log(gdppc2011 - 500) + (1 | y5), 
    data = clio, chains = 1)
brmsfit$model
brmsfit$fit

update(brmsfit, formula. = scale(stature) ~ ., newdata = clio[y5 <= 1980])

matplot(ranef(brmsfit)$y5[, 3:4, 'Intercept'])
