### PLAYGROUND SCRIPT ###

source("ThreeD_load_packages.R")


# load data
cars
iris
co2

# fix data
str(co2)
names(co2)
head(co2)

co2.df <- co2 
colnames(co2.df) <- c("year", 
                      "jan", "feb", "mar", "apr", "may", "jun", 
                      "jul", "aug", "sep", "oct", "nov", "dec")





plot <- ggplot(data = co2, aes(x = ))
