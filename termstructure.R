#========================================================================================
#                                                                     T E R M  -  S T R U C T U  R E     C O D E 
#                                                                        Lucas Dubois & Myriam  Lamborelle
#========================================================================================
rm(list = ls())
#-----------------------------------------------------------------------------------------------------------------------------------------------------------
#1.0  Libraries :
#-----------------------------------------------------------------------------------------------------------------------------------------------------------
library(readsdmx)
library(tidyverse)
library(lubridate)
library(zoo)
library(readxl)
library(viridis)
library(dplyr)
library(tidyr)
library(ggplot2)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------
#2.0  Data Treatment :
#-----------------------------------------------------------------------------------------------------------------------------------------------------------

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
    x = NULL,
    y = "Yield (%)",
    color = "Maturity"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold")
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
  ylab = ""
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

#PCs Plot:
PCs_to_plot <- data.frame(
  date = yields$date,
  PC1 = PCs$PC1,
  PC2 = PCs$PC2,
  PC3 = PCs$PC3
)

covid_start   <- as.Date("2020-03-01")
covid_end     <- as.Date("2021-06-30")

ukraine_start <- as.Date("2022-02-01")
ukraine_end   <- as.Date("2023-12-31")

pc_cols <- viridis::turbo(3)     

y_lim <- range(PCs_to_plot[, 2:4])

png(
  filename = file.path(images, "PCs.png"),
  width = 900,
  height = 600,
  res = 120
)

plot(
  PCs_to_plot$date,
  PCs_to_plot$PC1,
  type = "l",
  lwd = 2.5,
  col = pc_cols[1],
  ylim = y_lim,
  xlab = "",
  ylab = "%"
)

# COVID-19 crisis
rect(
  xleft  = covid_start,
  xright = covid_end,
  ybottom = y_lim[1],
  ytop    = y_lim[2],
  col = rgb(0.6, 0.6, 0.6, 0.35),
  border = NA
)

# Ukraine / energy crisis
rect(
  xleft  = ukraine_start,
  xright = ukraine_end,
  ybottom = y_lim[1],
  ytop    = y_lim[2],
  col = rgb(1, 0.5, 0.5, 0.35),
  border = NA
)

lines(PCs_to_plot$date, PCs_to_plot$PC1, col = pc_cols[1], lwd = 2.5)
lines(PCs_to_plot$date, PCs_to_plot$PC2, col = pc_cols[2], lwd = 2.5)
lines(PCs_to_plot$date, PCs_to_plot$PC3, col = pc_cols[3], lwd = 2.5)

legend(
  "top",
  legend = c(
    "PC1",
    "PC2",
    "PC3",
    "COVID-19 crisis",
    "Ukraine / Energy crisis"
  ),
  col = c(
    pc_cols,
    rgb(0.6, 0.6, 0.6, 0.6),
    rgb(1, 0.5, 0.5, 0.6)
  ),
  lwd = c(2.5, 2.5, 2.5, NA, NA),
  pch = c(NA, NA, NA, 15, 15),
  pt.cex = 2,
  bty = "n",
  cex = 0.9
)

dev.off()

#Plot PCs + Yields (this helps visualize the comparison between the yields and each PC)
y_lim1 <- range(
  c(yields_long$yield, PCs_to_plot$PC1),
  na.rm = TRUE
)

png(
  filename = file.path(images, "PC1_vs_Yields.png"),
  width = 1000,
  height = 600,
  res = 120
)

plot(
  PCs_to_plot$date,
  PCs_to_plot$PC1,
  type = "n",
  ylim = y_lim1,
  xlab = "",
  ylab = "%"
)

for (m in levels(yields_long$maturity)) {
  tmp <- yields_long[yields_long$maturity == m, ]
  lines(
    tmp$date,
    tmp$yield,
    col = rgb(0, 0.55, 0.55, 0.25),
    lwd = 1
  )
}

lines(
  PCs_to_plot$date,
  PCs_to_plot$PC1,
  col = "red3",
  lwd = 3
)

legend(
  "topleft",
  legend = c(
    "All yields",
    "PC1"
  ),
  col = c(
    rgb(0, 0.55, 0.55, 0.6),
    "red3",
    rgb(0.6, 0.6, 0.6, 0.6),
    rgb(1, 0.5, 0.5, 0.6)
  ),
  lwd = c(2, 3, NA, NA),
  pch = c(NA, NA, 15, 15),
  pt.cex = 2,
  bty = "n",
  cex = 0.9
)

dev.off()

y_lim1 <- range(
  c(yields_long$yield, PCs_to_plot$PC1),
  na.rm = TRUE
)

png(
  filename = file.path(images, "PC1_vs_Yields.png"),
  width = 900,
  height = 600,
  res = 120
)

plot(
  PCs_to_plot$date,
  PCs_to_plot$PC1,
  type = "n",
  ylim = y_lim1,
  xlab = "",
  ylab = "%"
)

for (m in levels(yields_long$maturity)) {
  tmp <- yields_long[yields_long$maturity == m, ]
  lines(
    tmp$date,
    tmp$yield,
    col = rgb(0, 0.55, 0.55, 0.25),
    lwd = 1
  )
}

lines(
  PCs_to_plot$date,
  PCs_to_plot$PC1,
  col = "red3",
  lwd = 3
)

legend(
  "topleft",
  legend = c(
    "All yields",
    "PC1"
  ),
  col = c(
    rgb(0, 0.55, 0.55, 0.6),
    "red3",
    rgb(0.6, 0.6, 0.6, 0.6),
    rgb(1, 0.5, 0.5, 0.6)
  ),
  lwd = c(2, 3, NA, NA),
  pch = c(NA, NA, 15, 15),
  pt.cex = 2,
  bty = "n",
  cex = 0.9
)

dev.off()

#PC2
y_lim2 <- range(
  c(yields_long$yield, PCs_to_plot$PC2),
  na.rm = TRUE
)

png(
  filename = file.path(images, "PC2_vs_Yields.png"),
  width = 900,
  height = 600,
  res = 120
)

plot(
  PCs_to_plot$date,
  PCs_to_plot$PC2,
  type = "n",
  ylim = y_lim2,
  xlab = "",
  ylab = "%"
)

for (m in levels(yields_long$maturity)) {
  tmp <- yields_long[yields_long$maturity == m, ]
  lines(
    tmp$date,
    tmp$yield,
    col = rgb(0, 0.55, 0.55, 0.25),
    lwd = 1
  )
}

lines(
  PCs_to_plot$date,
  PCs_to_plot$PC2,
  col = "green",
  lwd = 3
)

legend(
  "topleft",
  legend = c(
    "All yields",
    "PC2"
  ),
  col = c(
    rgb(0, 0.55, 0.55, 0.6),
    "green",
    rgb(0.6, 0.6, 0.6, 0.6),
    rgb(1, 0.5, 0.5, 0.6)
  ),
  lwd = c(2, 3, NA, NA),
  pch = c(NA, NA, 15, 15),
  pt.cex = 2,
  bty = "n",
  cex = 0.9
)

dev.off()

#PC3
y_lim3 <- range(
  c(yields_long$yield, PCs_to_plot$PC3),
  na.rm = TRUE
)

png(
  filename = file.path(images, "PC3_vs_Yields.png"),
  width = 900,
  height = 600,
  res = 120
)

plot(
  PCs_to_plot$date,
  PCs_to_plot$PC3,
  type = "n",
  ylim = y_lim3,
  xlab = "",
  ylab = "%"
)

for (m in levels(yields_long$maturity)) {
  tmp <- yields_long[yields_long$maturity == m, ]
  lines(
    tmp$date,
    tmp$yield,
    col = rgb(0, 0.55, 0.55, 0.25),
    lwd = 1
  )
}

lines(
  PCs_to_plot$date,
  PCs_to_plot$PC3,
  col = "blue",
  lwd = 3
)

legend(
  "topleft",
  legend = c(
    "All yields",
    "PC3"
  ),
  col = c(
    rgb(0, 0.55, 0.55, 0.6),
    "blue",
    rgb(0.6, 0.6, 0.6, 0.6),
    rgb(1, 0.5, 0.5, 0.6)
  ),
  lwd = c(2, 3, NA, NA),
  pch = c(NA, NA, 15, 15),
  pt.cex = 2,
  bty = "n",
  cex = 0.9
)

dev.off()


#-----------------------------------------------------------------------------------------------------------------------------------------------------------
#. 5.0 VAR(1)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------

PCs_mat <- as.matrix(PCs[, c("PC1", "PC2", "PC3")])
colnames(PCs_mat) <- c("PC1", "PC2", "PC3")
T <- nrow(PCs_mat)
Y <- PCs_mat[2:T, ]
X <- cbind(
  1,
  PCs_mat[1:(T-1), ]
)
colnames(X) <- c("Intercept", "PC1_L1", "PC2_L1", "PC3_L1")
coeff_hat <- solve(t(X) %*% X) %*% t(X) %*% Y
mu_hat  <- coeff_hat[1, ]          
phi_hat <- coeff_hat[2:4, ]   

colnames(phi_hat) <- c("PC1","PC2","PC3")
rownames(phi_hat) <- c("PC1(-1)","PC2(-1)","PC3(-1)")

mu_hat
phi_hat

# Stationarity chec;k
eig <- eigen(phi_hat)
eig$values
abs(eig$values)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------
#. 6.0 Affine Pricing Equation:
#-----------------------------------------------------------------------------------------------------------------------------------------------------------

Y <- as.matrix(yields[, -1])
X <- as.matrix(
  cbind(
    1,
    PCs[, c("PC1", "PC2", "PC3")]
  )
)

coeff_hat <- solve(t(X) %*% X) %*% t(X) %*% Y
A <- coeff_hat[1, ]
names(A) <- colnames(Y)
B <- coeff_hat[2:4, ]
rownames(B) <- c("PC1", "PC2", "PC3")
colnames(B) <- colnames(Y)

delta0 <- A[1]   
delta1 <- B[, 1]     

#-----------------------------------------------------------------------------------------------------------------------------------------------------------
#. 7.0 Decomposition:
#------------------------------------------------------------------------------------------------------------------------

PCs_fore <- array(NA, dim = c(N, T, 120))
short_rate_fore <- matrix(NA, nrow = T, ncol = 120)

for (t in 1:T) {
  for (h in 1:120) {
    
    if (h == 1) {
      PCs_fore[, t, h] <- mu_hat + t(phi_hat) %*% PCs_mat[t, ]
    } else {
      PCs_fore[, t, h] <- mu_hat + t(phi_hat) %*% PCs_fore[, t, h - 1]
    }
    
    short_rate_fore[t, h] <- delta0 + t(delta1) %*% PCs_fore[, t, h]
  }
}
J <- ncol(yields) - 1
max_h <- ncol(short_rate_fore)

J <- ncol(yields) - 1

Exp <- matrix(NA, nrow = T, ncol = J)
colnames(Exp) <- colnames(yields)[-1]

max_h <- ncol(short_rate_fore)

for (j in 1:J) {
  h_j <- min(maturity_months[j], max_h)
  
  Exp[, j] <- rowMeans(
    short_rate_fore[, 1:h_j, drop = FALSE]
  )
}


Exp[, 1] <- yields$GER1M

Y_mat <- as.matrix(yields[, -1])  
TP <- Y_mat - Exp


# Plotting: Expectations / Term Premium decomposition
Dates <- yields$date
Y <- as.matrix(yields[, -1])
colnames(Y) <- maturity_order

maturities_to_plot <- c("GER3M", "GER2Y", "GER5Y","GER10Y")

for (m in maturities_to_plot) {
  
  j <- which(maturity_order == m)
  
  Decomp_to_plot <- data.frame(
    date  = Dates,
    Yield = Y[, j],
    Exp   = Exp[, j],
    TP    = TP[, j]
  )
  
  ylim <- range(Decomp_to_plot[, -1], na.rm = TRUE)
  
  png(
    filename = file.path(images, paste0("Decomposition_", m, ".png")),
    width = 900,
    height = 600,
    res = 120
  )
  
  plot(
    Decomp_to_plot$date,
    Decomp_to_plot$Yield,
    type = "l",
    lwd  = 2.5,
    col  = "black",
    ylim = ylim,
    xlab = "",
    ylab = "%"
  )
  
  lines(
    Decomp_to_plot$date,
    Decomp_to_plot$Exp,
    col = "steelblue",
    lwd = 2
  )
  
  lines(
    Decomp_to_plot$date,
    Decomp_to_plot$TP,
    col = "firebrick",
    lwd = 2
  )
  
  legend(
    "top",
    legend = c("Yield", "Expectations", "Term premium"),
    col = c("black", "steelblue", "firebrick"),
    lwd = c(2.5, 2, 2),
    bty = "n",
    cex = 0.9
  )
  
  dev.off()
}

# Plot of signed histograms with correct stacking
bar_width <- median(diff(sort(unique(Dates))))

for (m in maturities_to_plot) {
  
  j <- which(maturity_order == m)
  
  Decomp <- data.frame(
    date = Dates,
    Exp = Exp[, j],
    TP  = TP[, j],
    Yield = Y[, j]
  )
  
  # Build bar limits
  Decomp_long <- Decomp %>%
    mutate(
      Exp_ymin = 0,
      Exp_ymax = Exp,
      
      TP_ymin = ifelse(TP >= 0, Exp, TP),
      TP_ymax = ifelse(TP >= 0, Exp + TP, 0)
    ) %>%
    select(date,
           Exp_ymin, Exp_ymax,
           TP_ymin, TP_ymax,
           Yield) %>%
    pivot_longer(
      cols = c(Exp_ymin, TP_ymin),
      names_to = "component",
      values_to = "ymin"
    ) %>%
    mutate(
      ymax = ifelse(component == "Exp_ymin", Exp_ymax, TP_ymax),
      component = ifelse(component == "Exp_ymin", "Expectations", "TermPremium")
    )
  
  p <- ggplot() +
    
    # Expectations bars
    geom_rect(
      data = Decomp_long %>% filter(component == "Expectations"),
      aes(
        xmin = date - bar_width / 2,
        xmax = date + bar_width / 2,
        ymin = ymin,
        ymax = ymax,
        fill = component
      ),
      alpha = 0.7
    ) +
    
    # Term premium bars (stacked if positive, negative otherwise)
    geom_rect(
      data = Decomp_long %>% filter(component == "TermPremium"),
      aes(
        xmin = date - bar_width / 2,
        xmax = date + bar_width / 2,
        ymin = ymin,
        ymax = ymax,
        fill = component
      ),
      alpha = 0.7
    ) +
    
    # Yield line
    geom_line(
      data = Decomp,
      aes(x = date, y = Yield),
      linewidth = 1.3,
      color = "black"
    ) +
    
    scale_fill_manual(
      values = c(
        "Expectations" = "steelblue",
        "TermPremium"  = "firebrick"
      )
    ) +
    
    labs(
      x = NULL,
      y = "%",
      fill = NULL
    ) +
    
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "top"
    )
  
  ggsave(
    filename = file.path(images, paste0("Decomp_Bars_", m, ".png")),
    plot = p,
    width = 9,
    height = 6,
    dpi = 300
  )
}

#-----------------------------------------------------------------------------------------------------------------------------------------------------------
# 10.0 One bar per event: cumulative 10Y change split into Expectations + Term Premium
#-----------------------------------------------------------------------------------------------------------------------------------------------------------

# --- Event windows ----------------------------------------------------------------------------
events <- tibble::tibble(
  event = c("COVID-19", "Ukraine war", "Monetary policy tightening"),
  start = as.Date(c("2020-03-01", "2022-02-01", "2022-07-01")),
  end   = as.Date(c("2021-06-30", "2023-12-31", "2023-12-31"))
)

# --- Choose the 10Y series --------------------------------------------------------------------
m <- "GER10Y"
j <- which(maturity_order == m)

base <- data.frame(
  date  = Dates,
  Exp   = Exp[, j],
  TP    = TP[, j],
  Yield = Y[, j]
) %>%
  arrange(date)

# --- Cumulative changes per event (end − start), in basis points ------------------------------
event_changes <- events %>%
  rowwise() %>%
  mutate(
    Exp_bp = {
      tmp <- base %>% filter(date >= start & date <= end)
      100 * (last(tmp$Exp) - first(tmp$Exp))
    },
    TP_bp = {
      tmp <- base %>% filter(date >= start & date <= end)
      100 * (last(tmp$TP) - first(tmp$TP))
    },
    Yield_bp = {
      tmp <- base %>% filter(date >= start & date <= end)
      100 * (last(tmp$Yield) - first(tmp$Yield))
    }
  ) %>%
  ungroup()

# --- Enforce event order ----------------------------------------------------------------------
event_order <- c("COVID-19", "Ukraine war", "Monetary  Tightening")

event_changes$event <- factor(event_changes$event, levels = event_order)

# --- Long format for stacked bars -------------------------------------------------------------
bar_df <- event_changes %>%
  select(event, Exp_bp, TP_bp, Yield_bp) %>%
  pivot_longer(
    cols = c(Exp_bp, TP_bp),
    names_to = "component",
    values_to = "change_bp"
  ) %>%
  mutate(
    component = recode(component,
                       Exp_bp = "Expectations",
                       TP_bp  = "Term premium"),
    event = factor(event, levels = event_order)
  )

# --- Plot: ONE bar per event split into Exp and TP ---------------------------------------------
p <- ggplot(bar_df, aes(x = event, y = change_bp, fill = component)) +
  geom_col(width = 0.65) +
  geom_hline(yintercept = 0, linewidth = 0.6) +
  geom_point(
    data = event_changes,
    aes(x = event, y = Yield_bp),
    inherit.aes = FALSE,
    shape = 18,   # diamond
    size = 4
  ) +
  scale_fill_manual(
    values = c(
      "Expectations" = "steelblue",
      "Term premium" = "firebrick"
    )
  ) +
  labs(
    x = NULL,
    y = "Cumulative change (bp)",
    fill = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(face = "bold")
  )

ggsave(
  filename = file.path(images, paste0("EventBars_", m, ".png")),
  plot = p,
  width = 10,
  height = 6,
  dpi = 300
)