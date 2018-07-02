setwd("~/dropbox/globdev/")

library("data.table")
library("ggplot2")

source("src/globdev_functions.R")

vrbs = c("gdppc2011", "lifexp", "polity2", "avedu", "stature", "gini", "real_wage", "homicide_rate", "so2emis_pc") #, "co2emis_pc", "biodiv")

devctrs = c(chile = 152, argentina = 32, brazil = 76, bolivia = 68, 
    equador = 218, south_africa = 710, botswana = 72, namibia = 516, 
    china = 156,  india = 356, indonesia = 360, thailand = 764,
    malaysia = 458, vietnam = 704, sri_lanka = 144,
    russia = 643, soviet_union = 810, poland = 616, ukraine = 804, 
    latvia = 428, estonia = 233, lithuania = 440)

cliocodes = c(GBR = 826, NLD = 528, FRA = 250, DEU = 276, DDR = 278,
              ITA = 380, ESP = 724, SWE = 752,
              POL = 616, RUS = 643, SUN = 810,
              AUS = 36, CAN = 124, USA = 840, 
              MEX = 484, BRA = 76, ARG = 32,
              EGY = 818, TUR = 792,
              KEN = 404, NGA = 566, ZAF = 710,
              CHN = 156, JPN = 392, IND = 356, IDN = 360, THA = 764)

clio = data.table::fread("dat/clioannual.csv")
clio5 = data.table::fread("dat/clio5y.csv")

# clio5[, polity2 := polity2 + 11]
# clio[, polity2 := polity2 + 11]

clio5 = unique(clio[, list(ccode, iso3c)])[clio5, on = 'ccode']

# loglog always preferable
# AIC(lm(lifexp ~ log(gdppc2011), data = clio5[gdppc2011 < 1e5]))
# AIC(lm(log(lifexp) ~ log(gdppc2011), data = clio5[gdppc2011 < 1e5]))
# AIC(lm(avedu ~ log(gdppc2011), data = clio5[gdppc2011 < 1e5]))
# AIC(lm(log(avedu) ~ log(gdppc2011), data = clio5[gdppc2011 < 1e5]))
# AIC(lm(polity2 ~ log(gdppc2011), data = clio5[gdppc2011 < 1e5]))
# AIC(lm(log(polity2) ~ log(gdppc2011), data = clio5[gdppc2011 < 1e5]))
# AIC(lm(stature ~ log(gdppc2011), data = clio5[gdppc2011 < 1e5]))
# AIC(lm(log(stature) ~ log(gdppc2011), data = clio5[gdppc2011 < 1e5]))
# AIC(lm(gini ~ log(gdppc2011), data = clio5[gdppc2011 < 1e5]))
# AIC(lm(log(gini) ~ log(gdppc2011), data = clio5[gdppc2011 < 1e5]))
# AIC(lm(real_wage ~ log(gdppc2011), data = clio5[gdppc2011 < 1e5]))
# AIC(lm(log(real_wage) ~ log(gdppc2011), data = clio5[gdppc2011 < 1e5]))
# AIC(lm(homicide_rate ~ log(gdppc2011), data = clio5[gdppc2011 < 1e5]))
# AIC(lm(log1p(homicide_rate) ~ log(gdppc2011), data = clio5[gdppc2011 < 1e5]))

# ideally subset the dataset you do the predictions on...
clio5[, paste0(vrbs[-1], '_predicted_loglin') := lapply(.SD, function(x) predict(lm(x ~ log(gdppc2011)), newdata = data.frame(x = x))), .SDcols = vrbs[-1]]
# clio5[, paste0(vrbs[-1], '_predicted_loglog') := lapply(.SD, function(x) exp(predict(lm(log1p(x) ~ log(gdppc2011)), newdata = data.frame(x = x)))), .SDcols = vrbs[-1]]

clio5[iso3c == "USA", list(y5, lifexp, lifexp_predicted_loglin, lifexp - lifexp_predicted_loglin)]
# mentioned in text: ITA, GBR, SWE, USA, DEU, FRA
early_indus = c("GBR", "USA", "DEU", "FRA")
late_indus = c("SWE", "ITA")

clio[, lapply(.SD, sd, na.rm = T), .SDcols = vrbs]
clio5[iso3c %in% early_indus & between(y5, 1820, 1900), list(y5, lifexp, lifexp_predicted_loglin, lifexp - lifexp_predicted_loglin)]
clio5[iso3c %in% late_indus & between(y5, 1820, 1900), list(y5, lifexp, lifexp_predicted_loglin, lifexp - lifexp_predicted_loglin)]
clio5[iso3c %in% late_indus & between(y5, 1820, 1900), list(y5, homicide_rate, homicide_rate_predicted_loglin, homicide_rate - homicide_rate_predicted_loglin)]
clio5[iso3c %in% late_indus & between(y5, 1820, 1900), list(y5, gini, gini_predicted_loglin, gini - gini_predicted_loglin)]
hist(clio5[iso3c %in% late_indus & between(y5, 1820, 1900), gini - gini_predicted_loglin])
hist(clio5[, gini - gini_predicted_loglin])

latam = c("ARG", "CHL", "BRA", "VEN", "PER", "MEX")
africa = c("ZAF", "KEN", "NGA", "GHA", "UGA", "BFA") # , "BWA", "AGO", "TZA", "BFA")
asia = c("CHN", "IND", "IDN", "PHL", "VNM", "THA")
easteu = c("RUS", "POL", "HUN", "ROU", "BGR", "EST")

regionlist = list(latam = latam, africa = africa, asia = asia, easteu = easteu) #, early = early_indus, late = late_indus)

matplot(dcast(clio5[y5 >= 1950 & iso3c %in% regionlist$latam], value.var = 'gdppc2011', formula = y5 ~ iso3c)[, -1])
ggplot(na.omit(clio5[y5 >= 1950 & iso3c %in% regionlist$latam, list(gdppc2011, y5, iso3c)])) + 
    geom_line(aes(y5, gdppc2011, colour = iso3c))
ggplot(na.omit(clio5[y5 >= 1950 & iso3c %in% regionlist$africa, list(gdppc2011, y5, iso3c)])) + 
    geom_line(aes(y5, log(gdppc2011), colour = iso3c))
ggplot(na.omit(clio5[y5 >= 1950 & iso3c %in% regionlist$asia, list(gdppc2011, y5, iso3c)])) + 
    geom_line(aes(y5, gdppc2011, colour = iso3c))

ggplot(na.omit(clio[year <= 1900 & iso3c %in% c(early_indus, late_indus), list(year, iso3c, gdppc2011)])) + 
    geom_line(aes(year, gdppc2011, color = iso3c))
ggplot(na.omit(clio5[y5 <= 1900 & iso3c %in% c(early_indus, late_indus), list(y5, growth = gdppc2011 / shift(gdppc2011)), by = iso3c])) + 
    geom_smooth(aes(y5, growth, color = iso3c), se = F, method = 'gam')


ggplot(na.omit(clio5[y5 >= 1950, list(lifexp, y5, iso3c)])) + 
    geom_line(aes(y5, lifexp, group = iso3c, col = iso3c %in% regionlist$africa))

x = data.table::melt(clio5[y5 >= 1950], id.vars = c("y5", "iso3c", "region"), measure.vars = vrbs[-1])
ggplot(na.omit(x)) + 
    geom_line(aes(y5, value, col = iso3c == "ZAF", group = iso3c)) + 
    facet_wrap( ~ variable, scales = 'free')
x = data.table::melt(clio5[iso3c %in% early_indus], id.vars = c("y5", "iso3c", "region"), measure.vars = vrbs[-1])
ggplot(na.omit(x)) + 
    geom_line(aes(y5, value, col = iso3c, group = iso3c)) + 
    facet_wrap( ~ variable, scales = 'free')

clio[iso3c == "CHN", list(year, lifexp)]

plot(lifexp ~ year, data = clio[iso3c == "IND"])
lines(lifexp ~ year, data = clio[iso3c == "IDN"], type = 'b', col = 2)
plot(lifexp ~ year, data = clio[iso3c == "RUS"])
lines(lifexp ~ year, data = clio[iso3c == "ZAF"], type = 'b', col = 2)

clio5[!is.na(lifexp), list(y5, reduction = lifexp - shift(lifexp)), by = iso3c][reduction <= -2][order(reduction)]



x = data.table::melt(clio5[y5 >= 1950 & iso3c %in% c("CHN", "IND")], id.vars = c("y5", "iso3c", "region"), measure.vars = vrbs[-1])
ggplot(na.omit(x)) + 
    geom_line(aes(y5, value, col = iso3c, group = iso3c)) + 
    facet_wrap( ~ variable, scales = 'free')

x = data.table::melt(
        clio5[y5 >= 1950, lapply(.SD, mean, na.rm = T), .SDcols = vrbs[-1], by = list(region, y5)]
        , id.vars = c("y5", "region"), measure.vars = vrbs[-1])
ggplot(na.omit(x)) + 
    geom_line(aes(y5, value, col = region, group = region)) + 
    facet_wrap( ~ variable, scales = 'free')


for (i in list(regionlist, early_indus, late_indus)) print(i); cat("\n\n")



ctrs = c("GBR", "NLD", "ITA", "JPN", "CHN", "IND", "BRA")
x = melt(clio5[iso3c %in% ctrs], id.vars = c("y5", "iso3c"), measure.vars = c(vrbs[c(2:6, 8)], paste0(vrbs[c(2:6, 8)], '_predicted_loglin')))
x[, predicted := ifelse(grepl("_predicted_loglin", variable), "predicted", "actual")]
x[, variable := gsub("_predicted_loglin", "", variable)]

pdf("out/predpanels_presentation.pdf", width = 11.69, height = 8.27)
plt = ggplot(na.omit(x)) + 
    geom_line(aes(y5, value, col = predicted)) + 
    geom_point(aes(y5, value, col = predicted, alpha = predicted), size = 0.9) + 
    facet_grid(variable ~ iso3c, scales = 'free_y') + # only works in this orientation, facet_wrap otherwise
    scale_x_continuous(breaks = c(1800, 2000)) + 
    theme_bw() +
    scale_colour_manual(values = c(1, blue)) + 
    scale_alpha_manual(values=c(1, 0)) + 
    theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
print(plt)
dev.off()

pdf("out/predpanels_all_sketch.pdf", height = 11.69, width = 8.27)
for (ctrs in list(latam, asia, africa, early_indus, late_indus)){
    x = melt(clio5[iso3c %in% ctrs], id.vars = c("y5", "iso3c"), measure.vars = c(vrbs[-1], paste0(vrbs[-1], '_predicted_loglin')))
    x[, predicted := ifelse(grepl("_predicted_loglin", variable), "predicted", "actual")]
    x[, variable := gsub("_predicted_loglin", "", variable)]

    plt = ggplot(na.omit(x)) + 
        geom_line(aes(y5, value, col = predicted)) + 
        geom_point(aes(y5, value, col = predicted, alpha = predicted), size = 0.9) + 
        facet_grid(variable ~ iso3c, scales = 'free_y') + # only works in this orientation, facet_wrap otherwise
        scale_x_continuous(breaks = c(1800, 2000)) + 
        theme_bw() +
        scale_colour_manual(values = c(1, blue)) + 
        scale_alpha_manual(values=c(1, 0)) + 
        theme(legend.position = "bottom", 
            legend.title = element_blank(), 
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    print(plt)
}
dev.off()

# no more a4 here
pdf("out/predpanels_current_indus.pdf", height = 11, width = 9)
vrbs_currentindus = vrbs[c(-1, -5, -9)]
for (ctrs in regionlist){
    x = melt(clio5[iso3c %in% ctrs & y5 >= 1950], id.vars = c("y5", "iso3c"), measure.vars = c(vrbs_currentindus, paste0(vrbs_currentindus, '_predicted_loglin')))
    x[, predicted := ifelse(grepl("_predicted_loglin", variable), "predicted", "actual")]
    x[, variable := gsub("_predicted_loglin", "", variable)]

    plt = ggplot(na.omit(x)) + 
        geom_line(aes(y5, value, col = predicted)) + 
        geom_point(aes(y5, value, col = predicted, alpha = predicted), size = 0.9) + 
        facet_grid(variable ~ iso3c, scales = 'free_y') + # only works in this orientation, facet_wrap otherwise
        scale_x_continuous(breaks = c(1950, 2000)) + 
        theme_bw() +
        scale_colour_manual(values = c(1, blue)) + 
        scale_alpha_manual(values=c(1, 0)) + 
        theme(legend.position = "bottom", 
            legend.title = element_blank(), 
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    print(plt)
}
dev.off()

# head(clio5[order(-real_wage), list(iso3c, y5, real_wage)], 40)
# head(clio5[y5 <= 1983][order(-real_wage), list(iso3c, y5, real_wage)], 40)
# this should actually be 
# or be fixed, or be confirmed that these figures are corret
# clio5[real_wage > 100, real_wage := NA]

# clio5[, mean(!is.na(real_wage)), by = y5]


pdf("out/predpanels_early_indus.pdf", height = 11.69, width = 8.27)
# for (grp in list(early_indus, late_indus)){
grp = c(early_indus, late_indus)
x = melt(clio5[iso3c %in% grp & y5 >= 1820], id.vars = c("y5", "iso3c"), measure.vars = c(vrbs[-1], paste0(vrbs[-1], '_predicted_loglin')))
x[, predicted := ifelse(grepl("_predicted_loglin", variable), "predicted", "actual")]
x[, variable := gsub("_predicted_loglin", "", variable)]

plt = ggplot(na.omit(x)) + 
    geom_line(aes(y5, value, col = predicted)) + 
    geom_point(aes(y5, value, col = predicted, alpha = predicted), size = 0.9) + 
    facet_grid(variable ~ iso3c, scales = 'free_y') + # only works in this orientation, facet_wrap otherwise
    scale_x_continuous(breaks = c(1820, 2000)) + 
    theme_bw() +
    scale_colour_manual(values = c(1, blue)) + 
    scale_alpha_manual(values=c(1, 0)) + 
    theme(legend.position = "bottom", 
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
print(plt)
dev.off()


# pdf("out/predpanels_gg_loglog.pdf", height = 11.69, width = 8.27)
# for (ctrs in regionlist){
#     x = melt(clio5[iso3c %in% ctrs & y5 >= 1950], id.vars = c("y5", "iso3c"), measure.vars = c(vrbs[-1], paste0(vrbs[-1], '_predicted_loglog')))
#     x[, predicted := ifelse(grepl("_predicted_loglog", variable), "predicted", "actual")]
#     x[, variable := gsub("_predicted_loglog", "", variable)]

#     plt = ggplot(na.omit(x)) + 
#         geom_line(aes(y5, value, col = predicted)) + 
#         geom_point(aes(y5, value, col = predicted, alpha = predicted), size = 0.9) + 
#         facet_grid(variable ~ iso3c, scales = 'free_y') + # only works in this orientation, facet_wrap otherwise
#         scale_x_continuous(breaks = c(1950, 2000)) + 
#         theme_bw() +
#         scale_colour_manual(values = c(1, blue)) + 
#         scale_alpha_manual(values=c(1, 0)) + 
#         theme(legend.position = "bottom", 
#             legend.title = element_blank(), 
#             panel.grid.major = element_blank(), 
#             panel.grid.minor = element_blank())
#     print(plt)
# }
# dev.off()

# clio5[, lapply(.SD, function(x) lm(x ~ log(gdppc2011) + factor(y5))$coef[-1]), .SDcols = vrbs[-1]]



summary(residmods[['polity2']])

for (vrb in vrbs){}

clio5[, paste0(vrbs[-1], '_predicted_loglin') := lapply(.SD, function(x) predict(lm(x ~ log(gdppc2011) + factor(y5)), newdata = data.frame(x = x))), .SDcols = vrbs[-1]]
# clio5[, paste0(vrbs[-1], '_predicted_loglog') := lapply(.SD, function(x) exp(predict(lm(log1p(x) ~ log(gdppc2011)), newdata = data.frame(x = x)))), .SDcols = vrbs[-1]]
