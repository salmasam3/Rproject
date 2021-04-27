mean_mass <- function (masses)
{
    #Return the mean of a list of masses
    mean_mass <- mean(masses, na.rm = TRUE)
    return(mean_mass)
}

all_of_it <- function ()
{
    all_data <- read.table("../MOMv3.3.txt", sep ="\t", 
                           col.names=c("continent", "status", "order", "family","genus", "species", "log10mass", "mass", "ref"), na.strings = "-999", stringsAsFactors = FALSE)
    
    continents <- all_data$continent
    status <- all_data$status
    masses <-  all_data$mass
    
    continents <- ifelse(continents == "Af", "AF", continents)
    
    results <- NULL
    for (continent in unique(continents))
    {
        extinct_masses <- masses[(status=="extinct") & (continents==continent)]
        extant_masses <- masses[(status=="extant") & (continents==continent)]
        avg_extinct_mass <- mean_mass(extinct_masses)
        avg_extant_mass <- mean_mass(extant_masses)
        diff <-  avg_extant_mass - avg_extinct_mass
        results <- rbind(results, c(continent, avg_extant_mass, avg_extinct_mass, diff))
    }
    
    for (line in 1:dim(results)[2])
    {
        print(results[line,])
    }
    results
}


good <- all_of_it()
good
data.frame(good) -> good
names(good) <- c("continent", "avg_extant_mass", "avg_extinct_mass", "diff")
good$continent <- as.character(good$continent)
good$diff <- as.numeric(as.character(good$diff))
good$avg_extinct_mass <- as.numeric(as.character(good$avg_extinct_mass))
good$avg_extant_mass <- as.numeric(as.character(good$avg_extant_mass))

write.csv(good, "good.csv", row.names = FALSE)
