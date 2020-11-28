#############################################################
####            Data processing and analysis             ####
#############################################################

# 1 Pre-processing

# enable packages
library(ggplot2)

# import data
df1 <- read.csv("temperature.csv", header = TRUE, sep = ";")

# transform excel dates
df1$date <- as.Date(df1$exceldate, origin = "1899-12-30")

# extract year as new variable
df1$year <- as.numeric(substr(as.character(df1$date), 1, 4))
head(df1)

# aggregate data per year and sites
df2 <- aggregate(value ~ year + site, mean, data = df1)
df2$value <- round(df2$value, 1)
head(df2)

# list unique sites
lst <- as.character(unique(df2$site))
lst

vrb <- "Temperature"

# 2 Main processing

# generate loop to process and analyze data

for (i in 1:length(lst)) {
  
  # i <- 1
  
  # select site
  ste <- lst[i]
  ste
  
  # print site name
  print(ste)
  
  # select data
  df3 <- subset(df2, site == ste)
  
  # extract information about the factor year (duration, start, end, etc.)
  sy <- min(df3$year)    # start year
  ey <- max(df3$year)    # end year
  ny <- length(df3$year) # total number of years
  
  # extract information about the climate data (mean, max, min, etc.) and round to one digit
  av <- round(mean(df3$value), 1)   # average value
  md <- round(median(df3$value), 1) # median value
  mx <- round(max(df3$value), 1)    # maximum value
  mn <- round(min(df3$value), 1)    # minimum value
  
  # extract additional information about the climate (e.g. numbers above the average, etc.)
  av.pl <- nrow(subset(df3, value > av))
  av.mi <- nrow(subset(df3, value < av))
  
  # create new data frame
  dt <- data.frame(ID = i, 
                   site = ste, 
                   start_yr = sy, 
                   end_yr = ey, 
                   n_yr = ny, 
                   avg_val = av,
                   med_val = md,
                   min_val = mn,
                   max_val = mx)
  dt
  
  # estimate trends
  lmd <- lm(value~year, df3)
  lmd
  
  # summarize analysis
  smlmd <- summary(lmd)
  smlmd
  
  # attache statistical information to the new generated data frame
  dt$variable <- vrb
  dt$trend_intercept <- round(smlmd$coefficients[1], 1)   # intercept of the trend
  dt$trend_sensitivity <- round(smlmd$coefficients[2], 3) # sensitivity of the trend
  dt$r2 <- round(smlmd$r.squared, 6)                      # R²
  dt$std_err_yr <- round(smlmd$coefficients[4], 3)        # standard error of the sensitivity of the trend
  dt$p_yr <- round(smlmd$coefficients[8], 4)              # significance of the trend
  dt
  
  # combine data from each run
  if (i == 1) {dt1 <- dt} else {dt1 <- rbind(dt1, dt)}
  
  # plot the trends
  
  if (vrb == "Temperature") {
    
    ggplot(df3, aes(year, value)) +
      theme_light() +
      geom_point(size = 1.5, colour = "red") +
      geom_smooth(method = "lm", formula =  y ~ x, se = FALSE, colour = "orange") +
      theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.5)) +
      theme(plot.caption = element_text(size = 8)) +
      theme(axis.title.x = element_text(size = 14)) +
      theme(axis.title.y = element_text(size = 14)) +
      theme(axis.text = element_text(face = "bold", size = rel(1.2))) +
      scale_x_continuous(limits = c(1950, 2010), breaks = seq(1950, 2010, 10)) + 
      scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, 5)) +
      labs(title = paste(vrb, "development at the site", ste), x = "Year", y = paste(vrb, "[°C]"))
    
  }
  
  if (vrb == "Precipitation") {
    
    ggplot(df3, aes(year, value)) +
      theme_light() +
      geom_bar(stat="identity", colour = "blue", fill = "blue", width=0.5) +
      geom_smooth(method = "lm", formula =  y ~ x, se = FALSE, colour = "orange") +
      theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.5)) +
      theme(plot.caption = element_text(size = 8)) +
      theme(axis.title.x = element_text(size = 14)) +
      theme(axis.title.y = element_text(size = 14)) +
      theme(axis.text = element_text(face = "bold", size = rel(1.2))) +
      scale_x_continuous(limits = c(1950, 2010), breaks = seq(1950, 2010, 10)) + 
      scale_y_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 100)) +
      labs(title = paste(vrb, "development at the site", ste), x = "Year", y = paste(vrb, "[mm]"))
    
  }
  
  # save graphic to folder
  # ggsave(paste(vrb, "Trend", ste, ".tif", sep = "_"), width = 20, height = 20, dpi = 300)
  
}

# 3 Post-processing

head(dt1)

# save table to folder
write.csv(dt1, paste(vrb,"_trends.csv", sep = ""))
