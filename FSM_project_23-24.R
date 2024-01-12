rm(list=ls(all=TRUE))
setwd("C:/Users/eddie/projects/statistics_project")

#DATASET 1 GlobClus_prop
####ASTRO DATA: 20 different  measures for 147 globular star clusters in the Milky Way Galaxy
#https://astrostatistics.psu.edu/MSMA/datasets/index.html
#Properties include Galactic location, integrated stellar luminosity, metallicity, ellipticity,
#central surface brightness, color, and seven measures of dynamical state
#(core and tidal radius, concentration, central star density and relaxation time,
#central velocity dispersion and escape velocity).
#https://search.r-project.org/CRAN/refmans/astrodatR/html/GlobClus_prop.html
#data description: Appendix C.7 of Feigelson & Babu (2012)
#with statistical analysis in Chapter 8.
options(repos = c(CRAN = "https://cloud.r-project.org"))

# LOAD THE DATASET

install.packages("astrodatR")
require(astrodatR)
data(GlobClus_prop)
dat <- GlobClus_prop

# EXPLORATORY DATA ANALYSIS

str(dat)
summary(dat)
head(dat)

# CORRELATION ANALYSIS FOR NUMERIC COLUMNS ONLY

numeric_cols <- sapply(dat, is.numeric)
cor(dat[, numeric_cols])

# HANDLE MISSING VALUES

sum(is.na(dat))
dat <- na.omit(dat)  # remove rows with missing data

# DATA DISTRIBUTION (HISTOGRAMS)

# Load the required packages and libraries
install.packages("ggplot2")

# Create a directory for the plots
dir.create("plots", showWarnings = FALSE)

# Loop through each numeric variable
for (var in names(dat)[numeric_cols]) {
    # Generate a histogram for the variable
    g <- ggplot(dat, aes(x=!!sym(var))) +
        geom_histogram(binwidth=0.1, fill="blue", color="black") +
        labs(title=paste("Histogram of", var), x=var, y="Frequency") +
        theme_minimal()

    # Save the plot in the 'plots' directory
    ggsave(paste0("plots/", var, ".png"), plot = g, bg = "white")
}

# REGRESSION ANALYSIS

# Multiple Linear Regression
model <- lm(Mv ~ Metal + R.sol + R.GC + log.rho, data = dat)
summary(model)
