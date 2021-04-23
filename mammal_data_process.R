library(dplyr)

read_mass_data <- function(filename, header = TRUE, column_names,
                           na.strings = "-999")
{
    ## Reads in mass data from a tab delimited file
    if(header)
    {
        all_data <- read.table(filename, sep ="\t",
                               na.strings = na.strings, stringsAsFactor = FALSE)
    } else
    {
        all_data <- read.table(filename, sep ="\t",
                               col.names = column_names, na.strings = na.strings, stringsAsFactors = FALSE)
    }
    return(all_data)
}

data_error_correct <- function (df)
{
    ## Corrects known error with some row names having "Af" instead of
    ## "AF"
    df <- df %>% mutate(continent = ifelse(continent == "Af", "AF", continent))
    return(df)
}


calculate_mean_masses <- function(df)
{
    ## Create a data frame that contains columns of average biomass of
    ## extant and extinct mammals and the difference by continent
    
    extinct <- df %>% filter(status == "extinct") %>% group_by(continent) %>%
        summarise(avg_extinct_mass = mean(mass, na.rm = TRUE))
    extant <- df %>% filter(status == "extant") %>% group_by(continent) %>%
        summarise(avg_extant_mass = mean(mass, na.rm = TRUE))
    
    continent_masses <- left_join(extant, extinct) %>%
        mutate(diff = avg_extant_mass - avg_extinct_mass)
    
    return(ungroup(continent_masses))
}

