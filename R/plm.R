#the pln package
library(pln)
data('EmplUK', package = 'plm')
data('Produc', package = 'plm')
data('Grunfeld', package = 'plm')
data('Wages', package = 'plm')
head(Grunfeld)
E <-pdata.frame(EmplUK, index = c('firm', 'year'), drop.index = TRUE, row.names = TRUE)
head(E)
head(attr(E, 'index'))
summary(E$emp)
head(as.matrix(E$emp))
head(lag(E$emp, 0:2))
head(diff(E$emp), 10)
head(lag(E$emp, 2), 10)
# Within transforms variables from means (see below)
head(Within(E$emp))
# between is the mean for each cross section
head(between(E$emp), 4)
# Between is the mean but maintais the original data structure. 
head(Between(E$emp), 4)
# models: fixed effects (within), pooling (pooling), first difference (fd), between (between), error (random)
grun.fe <- plm(inv ~ value + capital, data = Grunfeld, model = 'within')
grun.re <- plm(inv ~ value + capital, data = Grunfeld, model = 'random')
summary(grun.re)
fixef(grun.fe, type = 'dmean')
summary(fixef(grun.fe, type = 'dmean'))
grun.twfe <- plm(inv ~ value + capital, data = 'Grunfeld', model = 'within', effect = 'twoways')
# why does this not work? 