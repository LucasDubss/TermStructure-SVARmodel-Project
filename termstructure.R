#========================================================================================
#                                                                     T E R M  -  S T R U C T U  R E     C O D E 
#                                                                        Lucas Dubois & Myriam  Lamborelle
#========================================================================================

#-----------------------------------------------------------------------------------------------------------------------------------------------------------
#1.0  Libraries :
#-----------------------------------------------------------------------------------------------------------------------------------------------------------
library(readsdmx)
library(tidyverse)
library(lubridate)
library(zoo)
library(readxl)
library(viridis)


#-----------------------------------------------------------------------------------------------------------------------------------------------------------
#2.0  Data Treatment :
#-----------------------------------------------------------------------------------------------------------------------------------------------------------
rm(list = ls())
path<- "/Users/lucasdubois/Desktop/LaTeX/MACRO&FI/Data/germanbonds.xlsx" 
images<- "/Users/lucasdubois/Desktop/LaTeX/MACRO&FI/Images"
raw <- read_excel(path)
names(raw)

# Since we have too many "dates"  we can keep only the first one!
yields <- raw %>%
  select(
    date = Date...1,
    GER1M,
    GER3M,
    GER6M,
    GER1Y,
    GER2Y,
    GER3Y,
    GER5Y,
    GER10Y,
    GER15Y,
    GER20Y,
    GER30Y
  )

#Now, standirizing the date variable:
yields <- yields %>%
  mutate(
    date = as.Date(date),
    date = as.yearmon(date),
    date = as.Date(date, frac = 1)   # end of month
  )

# Check:
str(yields)
head(yields)
summary(yields)
names(yields)

#Check for NA:
sum(is.na(yields)) # No NAs :)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------
# 3.0 First PLOT:
#-----------------------------------------------------------------------------------------------------------------------------------------------------------

maturity_order <- c(
  "GER1M","GER3M","GER6M","GER1Y","GER2Y",
  "GER3Y","GER5Y","GER10Y","GER15Y","GER20Y","GER30Y"
)

yields_long <- yields %>%
  pivot_longer(
    cols = -date,
    names_to = "maturity",
    values_to = "yield"
  ) %>%
  mutate(
    maturity = factor(maturity, levels = maturity_order)
  )

ggplot(yields_long, aes(x = date, y = yield, color = maturity)) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Spectral") +
  labs(
    title = "German Government Bond Yields",
    x = NULL,
    y = "Yield (%)",
    color = "Maturity"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

ggsave(
  filename = file.path(images, "german_yield_curve.png"),
  plot = last_plot(),
  width = 12,
  height = 8,
  dpi = 300
)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------
#.4.0 Principal Components Analysis:
#-----------------------------------------------------------------------------------------------------------------------------------------------------------

#Running our PCA:
yields_PCA <- yields[,-1]
names(yields_PCA)
PCA <-princomp(yields_PCA)

PCA_loadings <- PCA$loadings     # store output
summary(PCA)    

N <- 3  # we chose the same number for N because PC4 only accounts for 1% of the variance!
PCA_loadings <- PCA_loadings[, 1:N]

if (PCA_loadings[1, 1] < 0) {
  PCA_loadings[, 1] <- -PCA_loadings[, 1]
}

#Plotting Loadings:
colnames(PCA_loadings) <- c("PC1", "PC2", "PC3")
PCs <- as.matrix(yields_PCA) %*% as.matrix(PCA_loadings)
PCs <- as.data.frame(PCs)
PCs$date <- yields$date

maturity_months <- c(1, 3, 6, 12, 24, 36, 60, 120, 180, 240, 360)

plot2 <- data.frame(
  maturity_months = maturity_months,
  PCA_loadings
)

png(
  filename = file.path(images, "PCA.png"),
  width = 900,
  height = 600,
  res = 120
)

plot(
  plot2$maturity_months,
  plot2$PC1,
  type = "l",
  lwd = 2,
  col = 2,
  ylim = c(-0.6, 0.6),
  xlab = "Maturity (months)",
  ylab = "",
  main = "PCA Loadings (W)"
)

lines(plot2$maturity_months, plot2$PC2, col = 3, lwd = 2)
lines(plot2$maturity_months, plot2$PC3, col = 4, lwd = 2)

legend(
  "bottomright",
  legend = c("PC1 (Level)", "PC2 (Slope)", "PC3 (Curvature)"),
  col = 2:4,
  lwd = 2,
  bty = "n"
)

dev.off()
#Plotting PCs: