library(plm)
da <- read.csv("Data/pwt61.csv")
head(da)
tail(da)
# form a pdata.frame object.  Identify the columsn that will index. 
daa <- pdata.frame(da, index = c("isocode", "yr"))
head(daa, 10)
tail(daa)
summary(daa$POP)
#------------------------------------
# Download the description of the data
code <- read.csv("Data/vars61.csv")
code <- code[,c(2, 6, 3)]
colnames(code) <- c("Code", "Description", "Unit")
head(code, 4)
#-----------------------------------------------------
#Calculate random effects and fixed effects mocdels
eq1.rf <- plm(cgdp ~ csave, data = daa, model = "random")
summary(eq1.rf)
eq1.fe <- plm(cgdp ~ csave, data = daa, model = "within")
summary(eq1.fe)
# Hauseman test of fixed or random effect models.
phtest(eq1.rf, eq1.fe)

