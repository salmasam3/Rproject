mean_mass <- function (masses)
{
#Return the mean of a list of masses
mean_mass <- mean(masses, na.rm = TRUE)
return(masses)
}

all_of_it <- function ()
{
    all_data <- read.table("MOMv3.3.txt", sep ="\t", 
                           col.names=c("continent", "status", "order", "family","genus", "species", "log10mass", "mass", "ref"))
    
    continents <- all_data$continent
    status <- all_data$status
    masses <-  all_data$mass
    
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
    
    for (line in all_data)
    {
        print(line)
    }
    
}
