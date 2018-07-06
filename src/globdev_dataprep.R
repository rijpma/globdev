setwd("~/dropbox/globdev/")

library("data.table")
library("readxl")
library("countrycode")

source("src/globdev_functions.R")

oecdregions = data.table::fread("dat/oecdregions.csv")

gdp = readxl::read_excel("~/downloads/Copy of Basic datamaddison2.xlsx", sheet = '2011', skip = 1)
setDT(gdp)
gdp = melt(gdp, id.vars = 'year', variable.name = 'iso3c', value.name = "gdppc2011")
gdp = gdp[year >= 1820]
gdp[, ccode := countrycode(iso3c, 'iso3c', 'iso3n')]
gdp[iso3c == "SUN", ccode := 810]
gdp[iso3c == "YUG", ccode := 890]

hom = as.data.table(read_excel("dat/HomicideRates_Compact.xlsx", sheet = "Data Long Format"))
pol = as.data.table(read_excel("dat/Polity2Index_Compact.xlsx", sheet = "Data Long Format"))
hgt = as.data.table(read_excel("dat/Height_Compact.xlsx", sheet = "Data Long Format"))
ine = as.data.table(read_excel("dat/IncomeInequality_Compact.xlsx", sheet = "Data Long Format"))
lab = as.data.table(read_excel("dat/LabourersRealWage_Compact.xlsx", sheet = "Data Long Format"))
lif = as.data.table(read_excel("dat/LifeExpectancyatBirth(Total)_Compact.xlsx", sheet = "Data Long Format"))
edu = as.data.table(read_excel("dat/AverageYearsofEducation_Compact.xlsx", sheet = "Data Long Format"))
pop = as.data.table(read_excel("dat/TotalPopulation_Compact.xlsx", sheet = "Data Long Format"))
so2 = as.data.table(read_excel("dat/SO2EmissionsperCapita_Compact.xlsx", sheet = "Data Long Format"))
bio = as.data.table(read_excel("dat/Biodiversity-naturalness_Compact.xlsx", sheet = "Data Long Format"))
co2 = as.data.table(read_excel("dat/CO2EmissionsperCapita_Compact.xlsx", sheet = "Data Long Format"))
# co2 = data.table::fread("dat/nation.1751_2014.csv") # problem countries present, fix before using
# co2[, ccode := countrycode(V1, 'country.name', 'iso3n')]

# repair missing observations in latest clio data
cliold = data.table::fread("/users/auke/dropbox/cliodata/allcliodata_raw.csv")
lab = rbindlist(list(lab, 
    cliold[ccode == 704 & !is.na(lab), list(ccode, country.name = country, year = year, value = lab)]))
lab = rbindlist(list(lab, 
    cliold[ccode == 360 & !is.na(lab), list(ccode, country.name = country, year = year, value = lab)]))
edu = rbindlist(list(edu, 
    cliold[ccode == 360 & !is.na(edu), list(ccode, country.name = country, year = year, value = edu)]))

lab[country.name=="Sudan", ccode := 729]
lab = lab[!is.na(ccode)]

setnames(hom, "value", "homicide_rate")
setnames(pol, "value", "polity2")
setnames(hgt, "value", "stature")
setnames(ine, "value", "gini")
setnames(lab, "value", "real_wage")
setnames(lif, "value", "lifexp")
setnames(edu, "value", "avedu")
setnames(so2, "value", "so2emis_pc")
setnames(co2, "value", "co2emis_pc")
setnames(bio, "value", "biodiv")
setnames(pop, "value", "population")

vrbs = c("homicide_rate", "polity2", "stature", "gini", 
    "real_wage", "lifexp", "population", "avedu", 
    "so2emis_pc", "co2emis_pc", "biodiv")

datlist = list(gdp, hom, pol, hgt, ine, lab, lif, edu, so2, co2, bio, pop)
datlist = lapply(datlist, function(x) x[, -2, with = F])

clio = Reduce(function(...) merge(..., on = c("ccode", "year"), all = T),
    datlist)
clio = merge(clio[year >= 1800], CJ(year = 1800:(max(clio$year)), ccode = unique(clio$ccode)), all = T)
clio[, nobs := sum(!is.na(.SD)), .SDcols = vrbs, by = ccode]

clio[year > 1980, stature := NA] # unreliable after 1980

clio[, iso3c := countrycode(ccode, 'iso3n', 'iso3c')]
clio[ccode == 810, iso3c := "SUN"]
clio[ccode == 890, iso3c := "YUG"]
clio[ccode == 729, iso3c := "SDN"]
clio = clio[ccode != 736]

clio = oecdregions[, list(ccode = as.numeric(ccode), region)][clio, on = 'ccode']
clio = devgroups[, list(devgroup = by_point, ccode)][clio, on = 'ccode']

# to make logs possible
clio[, polity2 := polity2 + 11]
clio[, so2emis_pc := so2emis_pc * 1000] # so the unit of analysis becomes kg per
# clio[, co2emis_pc := co2emis_pc * 1000] # data range ok, leave, is ton per year
clio[, biodiv := biodiv * 100]

clio[, .N, by = ccode][, unique(N)]
clio[, .N, by = year][, unique(N)]

datafreq = 5
clio[, y5 := trunc(year / datafreq) * datafreq]
datafreq = 10
clio[, decade := trunc(year / datafreq) * datafreq]

data.table::fwrite(clio, "dat/clioannual.csv")

# clio only has decennial population data <= 2000
# potential fix
# wbpop = jsonlite::fromJSON("http://api.worldbank.org/v2/countries/all/indicators/SP.POP.TOTL?per_page=25000&format=json", flatten = TRUE)
# wbpop = as.data.table(wbpop[[2]])
# clio = wbpop[, list(iso3c = countryiso3code, year = as.numeric(date), pop_wb = value)][clio, on = c("iso3c", "year")]

# pdf("out/popcheck.pdf")
# for (ctr in unique(clio$iso3c)){
#     try(matplot(clio[iso3c %in% ctr, list(population * 1000, pop_wb)], type = 'b', pch = 1, main = ctr))
# }
# dev.off()

# aggregate to 5yearly data
clio5 = Reduce(function(...) merge(..., all = TRUE, by = c("ccode", "y5", "region", "devgroup")),
    # list(da[, lapply(.SD, mean, na.rm = T), .SDcols = c("gdp", "lab", "cra", "hom", "bio", "lif", "hgt") , by = list(y5, ccode)],
    list(clio[, lapply(.SD, mean, na.rm = T), .SDcols = c("real_wage", "homicide_rate", "lifexp", "stature") , by = list(y5, ccode, region, devgroup)],
        clio[year == y5, list(gdppc2011, avedu, polity2, population, co2emis_pc, so2emis_pc, biodiv, ccode, y5, region, devgroup)], 
        clio[y5 != 1930, list(gini, region, devgroup, ccode, y5 = ifelse(year == 1929, 1930, y5), year = ifelse(year == 1929, 1930, year))][
            year == y5, list(ccode, region, devgroup, y5, gini)]))

clio5 = clio5[CJ(ccode = unique(clio$ccode), y5 = unique(clio$y5)), on = c('ccode', 'y5')]

for (nm in names(clio5)) clio5[is.nan(get(nm)), (nm) := NA]

clio5[, .N, by = ccode][, unique(N)]
clio5[, .N, by = y5][, unique(N)]

data.table::fwrite(clio5, "dat/clio5y.csv")
