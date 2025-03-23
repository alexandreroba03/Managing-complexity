#PART 1
install.packages("readxl")
install.packages("ggplot2")
install.packages("copula") 
install.packages("ggthemes")
install.packages("scales")
install.packages("gridExtra")
install.packages("grid") 
install.packages("dplyr")
library(gridExtra)
library(grid)
library(readxl)
library(ggthemes)  
library(scales) 
library(readxl)
library(ggplot2)
library(copula)
Data_assignment <- read_excel("~/Downloads/Managing complexity/managing complexity.xlsx")
View(Data_assignment)

############################################# QUESTION 2 ############################################
############################################# Scatter Plot ##########################################
# Clean and transform data
Data_assignment <- Data_assignment[-1, ] 
Data_assignment <- Data_assignment[1:506, ]
colnames(Data_assignment) <- trimws(colnames(Data_assignment))

# Convert columns to numeric
Data_assignment$`% of change for SM` <- as.numeric(Data_assignment$`% of change for SM`)
Data_assignment$`% of change for N` <- as.numeric(Data_assignment$`% of change for N`)
Data_assignment$`% of change  for O` <- as.numeric(Data_assignment$`% of change  for O`)
# Corrected the column name "% of change for O"
colnames(Data_assignment) <- gsub("\\s+", " ", colnames(Data_assignment)) 
colnames(Data_assignment) <- trimws(colnames(Data_assignment)) 
# Corrects the column Oil Prices
colnames(Data_assignment) <- gsub("\\s+", " ", colnames(Data_assignment))  
colnames(Data_assignment) <- trimws(colnames(Data_assignment))

#SM vs N
primary_color <- "#0073E6"  
highlight_color <- "#E74C3C" 
background_color <- "#F8F9F9" 

ggplot(Data_assignment, aes(x = `% of change for SM`, y = `% of change for N`)) +
  geom_point(alpha = 0.6, color = primary_color, size = 2) +  
  
  geom_smooth(method = "loess", se = FALSE, color = "black", linetype = "dashed", linewidth = 1.2) +
  
  theme_minimal(base_size = 14) +  
  theme(
    panel.background = element_rect(fill = background_color, color = NA),  
    panel.grid.major = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor = element_blank(),  
    axis.text = element_text(color = "black"),  
    axis.title = element_text(face = "bold"),  
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),  
    plot.subtitle = element_text(hjust = 0.5, size = 13, color = "black")  
  ) +
  
  labs(
    title = "Stock Market vs Nasdaq Returns",
    subtitle = "Visualization of the relationship between returns",  
    x = "Stock Market Returns",
    y = "Nasdaq Returns"
  )

#SM vs O
primary_color <- "#FF7F0E"   
highlight_color <- "#E74C3C" 
background_color <- "#F8F9F9" 

ggplot(Data_assignment, aes(x = `% of change for SM`, y = `% of change for O`)) +
  geom_point(alpha = 0.6, color = primary_color, size = 2) +  
  
  geom_smooth(method = "loess", se = FALSE, color = "black", linetype = "dashed", linewidth = 1.2) +
  
  theme_minimal(base_size = 14) +  
  theme(
    panel.background = element_rect(fill = background_color, color = NA),  
    panel.grid.major = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor = element_blank(),  
    axis.text = element_text(color = "black"),  
    axis.title = element_text(face = "bold"),  
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),  
    plot.subtitle = element_text(hjust = 0.5, size = 13, color = "black") 
  ) +
  
  labs(
    title = "Stock Market vs Oil Prices Returns",
    subtitle = "Visualization of the relationship between returns", 
    x = "Stock Market Returns",
    y = "Oil Prices Returns"
  )

#O vs N
primary_color <- "#2CA02C"   
highlight_color <- "#E74C3C" 
background_color <- "#F8F9F9" 

ggplot(Data_assignment, aes(x = `% of change for O`, y = `% of change for N`)) +
  geom_point(alpha = 0.6, color = primary_color, size = 2) + 
  
  geom_smooth(method = "loess", se = FALSE, color = "black", linetype = "dashed", linewidth = 1.2) +
  
  theme_minimal(base_size = 14) +  
  theme(
    panel.background = element_rect(fill = background_color, color = NA),  
    panel.grid.major = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor = element_blank(),  
    axis.text = element_text(color = "black"),  
    axis.title = element_text(face = "bold"),  
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),  
    plot.subtitle = element_text(hjust = 0.5, size = 13, color = "black") 
  ) +
  
  labs(
    title = "Oil Prices vs Nasdaq Returns",
    subtitle = "Visualization of the relationship between returns", 
    x = "Oil Prices Returns",
    y = "Nasdaq Returns"
  )

############################################# QUESTION 3 ##########################################
############################################# COPULA PLOT ##########################################
# Apply copula transformation (pseudo-observations)
Data_assignment$U_SM_N <- rank(Data_assignment$`% of change for SM`) / nrow(Data_assignment)
Data_assignment$V_SM_N <- rank(Data_assignment$`% of change for N`) / nrow(Data_assignment)

Data_assignment$U_SM_O <- rank(Data_assignment$`% of change for SM`) / nrow(Data_assignment)
Data_assignment$V_SM_O <- rank(Data_assignment$`% of change for O`) / nrow(Data_assignment)

Data_assignment$U_O_N <- rank(Data_assignment$`% of change for O`) / nrow(Data_assignment)
Data_assignment$V_O_N <- rank(Data_assignment$`% of change for N`) / nrow(Data_assignment)

# Select a specific point
selected_index <- 4
selected_point <- Data_assignment[selected_index, ]

#SM vs N
primary_color <- "#0073E6"   
highlight_color <- "#E74C3C" 
background_color <- "#F8F9F9" 

ggplot(Data_assignment, aes(x = U_SM_N, y = V_SM_N)) +
  geom_point(alpha = 0.6, color = primary_color, size = 2) +  
  geom_point(aes(x = selected_point$U_SM_N, y = selected_point$V_SM_N), 
             color = highlight_color, size = 5, shape = 21, fill = "white", stroke = 1.5) +  
  annotate("text", x = selected_point$U_SM_N + 0.02, y = selected_point$V_SM_N,
           label = paste0("(", round(selected_point$U_SM_N, 4), ", ", round(selected_point$V_SM_N, 4), ")"),
           hjust = 0, color = highlight_color, fontface = "bold") +  
  theme_minimal(base_size = 14) +  
  theme(
    panel.background = element_rect(fill = background_color, color = NA),  
    panel.grid.major = element_line(color = "grey80", linetype = "dashed"), 
    panel.grid.minor = element_blank(),  
    axis.text = element_text(color = "black"),  
    axis.title = element_text(face = "bold"),  
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16)  
  ) +
  labs(
    title = "Copula Transformation: Stock Market vs Nasdaq",
    x = "U (Ranked Stock Market Returns)",
    y = "V (Ranked Nasdaq Returns)"
  )

#SM vs O
primary_color <- "#FF7F0E"   
highlight_color <- "#E74C3C" 
background_color <- "#F8F9F9" 

ggplot(Data_assignment, aes(x = U_SM_O, y = V_SM_O)) +
  geom_point(alpha = 0.6, color = primary_color, size = 2) +  
  geom_point(aes(x = selected_point$U_SM_O, y = selected_point$V_SM_O), 
             color = highlight_color, size = 5, shape = 21, fill = "white", stroke = 1.5) +  
  annotate("text", x = selected_point$U_SM_O + 0.02, y = selected_point$V_SM_O,
           label = paste0("(", round(selected_point$U_SM_O, 4), ", ", round(selected_point$V_SM_O, 4), ")"),
           hjust = 0, color = highlight_color, fontface = "bold") +  
  theme_minimal(base_size = 14) +  
  theme(
    panel.background = element_rect(fill = background_color, color = NA),  
    panel.grid.major = element_line(color = "grey80", linetype = "dashed"), 
    panel.grid.minor = element_blank(),  
    axis.text = element_text(color = "black"),  
    axis.title = element_text(face = "bold"),  
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16)  
  ) +
  labs(
    title = "Copula Transformation: Stock Market vs Oil Prices",
    x = "U (Ranked Stock Market Returns)",
    y = "V (Ranked Oil Prices Returns)"
  )

#O vs N
primary_color <- "#2CA02C"   
highlight_color <- "#E74C3C" 
background_color <- "#F8F9F9" 

ggplot(Data_assignment, aes(x = U_O_N, y = V_O_N)) +
  geom_point(alpha = 0.6, color = primary_color, size = 2) + 
  geom_point(aes(x = selected_point$U_O_N, y = selected_point$V_O_N), 
             color = highlight_color, size = 5, shape = 21, fill = "white", stroke = 1.5) + 
  annotate("text", x = selected_point$U_O_N + 0.02, y = selected_point$V_O_N,
           label = paste0("(", round(selected_point$U_O_N, 4), ", ", round(selected_point$V_O_N, 4), ")"),
           hjust = 0, color = highlight_color, fontface = "bold") + 
  theme_minimal(base_size = 14) +  
  theme(
    panel.background = element_rect(fill = background_color, color = NA),  
    panel.grid.major = element_line(color = "grey80", linetype = "dashed"), 
    panel.grid.minor = element_blank(),  
    axis.text = element_text(color = "black"), 
    axis.title = element_text(face = "bold"),  
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16)  
  ) +
  labs(
    title = "Copula Transformation: Oil Prices vs Nasdaq",
    x = "U (Ranked Oil Prices Returns)",
    y = "V (Ranked Nasdaq Returns)"
  )


############################################# QUESTION 4 ##########################################
############################################# VALUE AT RISK #######################################
#4.a
Data_assignment$`Inverted_SM` <- -Data_assignment$`% of change for SM`
Data_assignment$`Inverted_N`  <- -Data_assignment$`% of change for N`
Data_assignment$`Inverted_O`  <- -Data_assignment$`% of change for O`
Data_assignment$`Inverted_SM_Sorted` <- c(
  sort(Data_assignment$`Inverted_SM`, decreasing = TRUE, na.last = NA),
  rep(NA, sum(is.na(Data_assignment$`Inverted_SM`)))
)
Data_assignment$`Inverted_N_Sorted` <- c(
  sort(Data_assignment$`Inverted_N`, decreasing = TRUE, na.last = NA),
  rep(NA, sum(is.na(Data_assignment$`Inverted_N`)))
)
Data_assignment$`Inverted_O_Sorted` <- c(
  sort(Data_assignment$`Inverted_O`, decreasing = TRUE, na.last = NA),
  rep(NA, sum(is.na(Data_assignment$`Inverted_O`)))
)

get_empirical_VaR <- function(inverted_sorted_column, alpha) {
  n <- sum(!is.na(inverted_sorted_column))  
  index <- ceiling(alpha * n)  
  return(inverted_sorted_column[index])  
}

# Empirical 95% and 99% VaR calculation
VaR_95_empirical_SM <- get_empirical_VaR(Data_assignment$`Inverted_SM_Sorted`, 0.05)
VaR_99_empirical_SM <- get_empirical_VaR(Data_assignment$`Inverted_SM_Sorted`, 0.01)

VaR_95_empirical_N <- get_empirical_VaR(Data_assignment$`Inverted_N_Sorted`, 0.05)
VaR_99_empirical_N <- get_empirical_VaR(Data_assignment$`Inverted_N_Sorted`, 0.01)

VaR_95_empirical_O <- get_empirical_VaR(Data_assignment$`Inverted_O_Sorted`, 0.05)
VaR_99_empirical_O <- get_empirical_VaR(Data_assignment$`Inverted_O_Sorted`, 0.01)

# Average and standard deviation of yields
mean_SM <- mean(Data_assignment$`% of change for SM`, na.rm = TRUE)
sd_SM <- sd(Data_assignment$`% of change for SM`, na.rm = TRUE)

mean_N <- mean(Data_assignment$`% of change for N`, na.rm = TRUE)
sd_N <- sd(Data_assignment$`% of change for N`, na.rm = TRUE)

mean_O <- mean(Data_assignment$`% of change for O`, na.rm = TRUE)
sd_O <- sd(Data_assignment$`% of change for O`, na.rm = TRUE)

# Calculation of normal VaR at 95% and 99%
VaR_95_normal_SM <- -(mean_SM + qnorm(0.05) * sd_SM)
VaR_99_normal_SM <- -(mean_SM + qnorm(0.01) * sd_SM)

VaR_95_normal_N <- -(mean_N + qnorm(0.05) * sd_N)
VaR_99_normal_N <- -(mean_N + qnorm(0.01) * sd_N)

VaR_95_normal_O <- -(mean_O + qnorm(0.05) * sd_O)
VaR_99_normal_O <- -(mean_O + qnorm(0.01) * sd_O)

VaR_results <- data.frame(
  Asset = c("Stock Market", "Nasdaq", "Oil Prices"),
  Mean = c(mean_SM, mean_N, mean_O),
  STD = c(sd_SM, sd_N, sd_O),
  VaR_95_Empirical = c(VaR_95_empirical_SM, VaR_95_empirical_N, VaR_95_empirical_O),
  VaR_99_Empirical = c(VaR_99_empirical_SM, VaR_99_empirical_N, VaR_99_empirical_O),
  VaR_95_Normal = c(VaR_95_normal_SM, VaR_95_normal_N, VaR_95_normal_O),
  VaR_99_Normal = c(VaR_99_normal_SM, VaR_99_normal_N, VaR_99_normal_O)
)

print(VaR_results)
############################################# EXPECTED SHORTFALL #######################################
#4.b
# Calculation of the empirical Expected Shortfall (ES)
ES_95_empirical_SM <- mean(Data_assignment$Inverted_SM_Sorted[1:25], na.rm = TRUE)
ES_99_empirical_SM <- mean(Data_assignment$Inverted_SM_Sorted[1:5], na.rm = TRUE)

ES_95_empirical_N <- mean(Data_assignment$Inverted_N_Sorted[1:25], na.rm = TRUE)
ES_99_empirical_N <- mean(Data_assignment$Inverted_N_Sorted[1:5], na.rm = TRUE)

ES_95_empirical_O <- mean(Data_assignment$Inverted_O_Sorted[1:25], na.rm = TRUE)
ES_99_empirical_O <- mean(Data_assignment$Inverted_O_Sorted[1:5], na.rm = TRUE)

# Calculation of ES normal
ES_95_normal_SM <- - (mean_SM - (sd_SM / (1 - 0.95)) * dnorm(qnorm(0.05)))
ES_99_normal_SM <- - (mean_SM - (sd_SM / (1 - 0.99)) * dnorm(qnorm(0.01)))

ES_95_normal_N <- - (mean_N - (sd_N / (1 - 0.95)) * dnorm(qnorm(0.05)))
ES_99_normal_N <- - (mean_N - (sd_N / (1 - 0.99)) * dnorm(qnorm(0.01)))

ES_95_normal_O <- - (mean_O - (sd_O / (1 - 0.95)) * dnorm(qnorm(0.05)))
ES_99_normal_O <- - (mean_O - (sd_O / (1 - 0.99)) * dnorm(qnorm(0.01)))

ES_results <- data.frame(
  Asset = c("Stock Market", "Nasdaq", "Oil Prices"),
  Mean = c(mean_SM, mean_N, mean_O),
  STD = c(sd_SM, sd_N, sd_O),
  VaR_95_Empirical = c(VaR_95_empirical_SM, VaR_95_empirical_N, VaR_95_empirical_O),
  VaR_99_Empirical = c(VaR_99_empirical_SM, VaR_99_empirical_N, VaR_99_empirical_O),
  ES_95_Empirical = c(ES_95_empirical_SM, ES_95_empirical_N, ES_95_empirical_O),
  ES_99_Empirical = c(ES_99_empirical_SM, ES_99_empirical_N, ES_99_empirical_O),
  VaR_95_Normal = c(VaR_95_normal_SM, VaR_95_normal_N, VaR_95_normal_O),
  VaR_99_Normal = c(VaR_99_normal_SM, VaR_99_normal_N, VaR_99_normal_O),
  ES_95_Normal = c(ES_95_normal_SM, ES_95_normal_N, ES_95_normal_O),
  ES_99_Normal = c(ES_99_normal_SM, ES_99_normal_N, ES_99_normal_O)
)

print(ES_results)

############################################# COMPARISON ###########################################
create_var_histogram <- function(data, variable, var95, var99, title, color_fill) {
  ggplot(data, aes(x = !!sym(variable))) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = color_fill, alpha = 0.7, color = "black") +
    geom_density(color = "black", linewidth = 1) +
    geom_vline(aes(xintercept = var95, color = "VaR 95%"), linetype = "dashed", linewidth = 1.2) +
    geom_vline(aes(xintercept = var99, color = "VaR 99%"), linetype = "dashed", linewidth = 1.2) +
    scale_color_manual(values = c("VaR 95%" = "red", "VaR 99%" = "darkred")) +
    labs(title = title, x = "Losses (%)", y = "Density", color = "VaR Levels") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
          legend.position = "bottom")
}

hist_SM_VaR <- create_var_histogram(Data_assignment, "Inverted_SM", VaR_95_empirical_SM, VaR_99_empirical_SM, "Stock Market - Loss Distribution", "#0073E6")
hist_N_VaR  <- create_var_histogram(Data_assignment, "Inverted_N", VaR_95_empirical_N, VaR_99_empirical_N, "Nasdaq - Loss Distribution", "#FF7F0E")
hist_O_VaR  <- create_var_histogram(Data_assignment, "Inverted_O", VaR_95_empirical_O, VaR_99_empirical_O, "Oil Prices - Loss Distribution", "#2CA02C")

grid.arrange(
  arrangeGrob(hist_SM_VaR, hist_N_VaR, hist_O_VaR, ncol = 3,
              top = textGrob("95% and 99% VaR (Value at Risk) analysis\n Comparison of extreme losses for Stock Market, Nasdaq et Oil Prices", 
                             gp = gpar(fontsize = 16, fontface = "bold"))),
  heights = c(1, 0.1)
)

create_es_histogram <- function(data, variable, es95, es99, title, color_fill) {
  ggplot(data, aes(x = !!sym(variable))) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = color_fill, alpha = 0.7, color = "black") +
    geom_density(color = "black", linewidth = 1) +
    geom_vline(aes(xintercept = es95, color = "ES 95%"), linetype = "dashed", linewidth = 1.2) +
    geom_vline(aes(xintercept = es99, color = "ES 99%"), linetype = "dashed", linewidth = 1.2) +
    scale_color_manual(values = c("ES 95%" = "blue", "ES 99%" = "darkblue")) +
    labs(title = title, x = "Losses (%)", y = "Density", color = "Expected Shortfall (ES)") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
          legend.position = "bottom")
}

hist_SM_ES <- create_es_histogram(Data_assignment, "Inverted_SM", ES_95_empirical_SM, ES_99_empirical_SM, "Stock Market - Loss Distribution", "#0073E6")
hist_N_ES  <- create_es_histogram(Data_assignment, "Inverted_N", ES_95_empirical_N, ES_99_empirical_N, "Nasdaq - Loss Distribution", "#FF7F0E")
hist_O_ES  <- create_es_histogram(Data_assignment, "Inverted_O", ES_95_empirical_O, ES_99_empirical_O, "Oil Prices - Loss Distribution", "#2CA02C")

grid.arrange(
  arrangeGrob(hist_SM_ES, hist_N_ES, hist_O_ES, ncol = 3,
              top = textGrob("Expected Shortfall (ES) analysis at 95% and 99%\n Comparison of extreme losses for Stock Market, Nasdaq et Oil Prices", 
                             gp = gpar(fontsize = 16, fontface = "bold"))),
  heights = c(1, 0.1)
)

create_density_plot <- function(data, variable, mean_val, sd_val, title, color_fill) {
  ggplot(data, aes(x = !!sym(variable))) +
    geom_density(fill = color_fill, alpha = 0.5) +
    stat_function(fun = dnorm, args = list(mean = mean_val, sd = sd_val),
                  color = "black", linetype = "dashed", linewidth = 1.2) +
    labs(title = title, x = "Losses (%)", y = "Density", subtitle = "Comparison of the empirical distribution and the normal distribution") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))
}

dens_SM <- create_density_plot(Data_assignment, "Inverted_SM", mean_SM, sd_SM, "Stock Market - Loss Distribution", "#0073E6")
dens_N  <- create_density_plot(Data_assignment, "Inverted_N", mean_N, sd_N, "Nasdaq - Loss Distribution", "#FF7F0E")
dens_O  <- create_density_plot(Data_assignment, "Inverted_O", mean_O, sd_O, "Oil Prices - Loss Distribution", "#2CA02C")

grid.arrange(
  arrangeGrob(dens_SM, dens_N, dens_O, ncol = 3,
              top = textGrob("Comparison of Empirical Losses and Normal Law\nStock Market, Nasdaq et Oil Prices", 
                             gp = gpar(fontsize = 16, fontface = "bold"))),
  heights = c(1, 0.1)
)

VaR_ES_results <- data.frame(
  Asset = rep(c("Stock Market", "Nasdaq", "Oil Prices"), each = 4),
  Indicator = rep(c("VaR 95%", "VaR 99%", "ES 95%", "ES 99%"), times = 3),
  Empirical = c(VaR_95_empirical_SM, VaR_99_empirical_SM, ES_95_empirical_SM, ES_99_empirical_SM,
                VaR_95_empirical_N, VaR_99_empirical_N, ES_95_empirical_N, ES_99_empirical_N,
                VaR_95_empirical_O, VaR_99_empirical_O, ES_95_empirical_O, ES_99_empirical_O)
)

VaR_results <- subset(VaR_ES_results, Indicator %in% c("VaR 95%", "VaR 99%"))
ES_results  <- subset(VaR_ES_results, Indicator %in% c("ES 95%", "ES 99%"))

var_colors <- c("VaR 95%" = "#E67E22", "VaR 99%" = "#D62728")
es_colors  <- c("ES 95%" = "#0073E6", "ES 99%" = "#1F77B4")
all_colors <- c(var_colors, es_colors)

plot_VaR <- ggplot(VaR_results, aes(x = Asset, y = Empirical, fill = Indicator)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black", width = 0.7) +
  geom_text(aes(label = round(Empirical, 2)), position = position_dodge(width = 0.7), vjust = -0.5, size = 4) +
  labs(
    title = "Comparison of VaR (Value at Risk) at 95% and 99%",
    subtitle = "Analysis of expected maximum losses",
    y = "Value (%)", x = "Assets",
    fill = "VaR Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  ) +
  scale_fill_manual(values = var_colors)

print(plot_VaR)

plot_ES <- ggplot(ES_results, aes(x = Asset, y = Empirical, fill = Indicator)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black", width = 0.7) +
  geom_text(aes(label = round(Empirical, 2)), position = position_dodge(width = 0.7), vjust = -0.5, size = 4) +
  labs(
    title = "Comparison of 95% and 99% Expected Shortfall (ES)",
    subtitle = "Analysis of average losses beyond the VaR",
    y = "Value (%)", x = "Assets",
    fill = "ES Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  ) +
  scale_fill_manual(values = es_colors)

print(plot_ES)

plot_combined <- ggplot(VaR_ES_results, aes(x = Asset, y = Empirical, fill = Indicator)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black", width = 0.7) +
  geom_text(aes(label = round(Empirical, 2)), position = position_dodge(width = 0.7), vjust = -0.5, size = 4) +
  labs(
    title = "Global Comparison of Risk Indicators: VaR vs ES",
    subtitle = "Analysis of maximum and average extreme losses",
    y = "Value (%)", x = "Assets",
    fill = "Risk Indicator"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  ) +
  scale_fill_manual(values = all_colors)

print(plot_combined)

############################################# TAIL INDEX #######################################
#4.c
estimate_tail_index_book <- function(returns, top_k = 25) {
  returns <- as.numeric(returns)
  returns <- na.omit(returns)
  
  n <- length(returns)
  if (n < top_k) {
    warning("Not enough valid data to estimate the queue.")
    return(NA)
  }
  
  losses <- sort(-returns, decreasing = TRUE)  
  selected_losses <- losses[1:top_k]  
  
  ranks <- rank(-selected_losses, ties.method = "average")  
  one_minus_Fx <- ranks / n  
  
  X_vec <- log(selected_losses)  # log(x)
  Y_vec <- -log(one_minus_Fx)  # -log(1 - F(x))
  
  model <- lm(Y_vec ~ X_vec)
  slope <- coef(model)[2]  # pente = tail index
  
  return(slope)
}

tail_index_SM <- estimate_tail_index_book(Data_assignment$`% of change for SM`)
tail_index_N  <- estimate_tail_index_book(Data_assignment$`% of change for N`)
tail_index_O  <- estimate_tail_index_book(Data_assignment$`% of change for O`)

tail_data_results <- data.frame(
  Asset     = c("Stock Market", "Nasdaq", "Oil"),
  TailIndex = c(tail_index_SM, tail_index_N, tail_index_O)
)

print(tail_data_results)

ggplot(tail_data_results, aes(x = Asset, y = TailIndex, fill = Asset)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), 
           color = "black", width = 0.6, alpha = 0.8) +
  
  geom_text(aes(label = round(TailIndex, 4)), 
            vjust = -0.3, size = 5, fontface = "bold") +
  
  scale_fill_manual(values = c("Stock Market" = "#1E90FF", 
                               "Nasdaq" = "#F39C12",      
                               "Oil" = "#27AE60")) +  
  
  labs(title = "Tail Index by Asset",
       x = "Asset", y = "Tail Index") +
  
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "none"  
  )

plot_tail_regression <- function(returns, asset_name, color, top_k = 30) {
  returns <- na.omit(as.numeric(returns))
  
  n <- length(returns)
  if (n < top_k) {
    warning("Not enough points to ", asset_name)
    return(NULL)
  }
  
  losses <- sort(-returns, decreasing = TRUE)  
  selected_losses <- losses[1:top_k]  
  
  ranks <- rank(-selected_losses, ties.method = "average")  
  one_minus_Fx <- ranks / n  
  
  X_vec <- log(selected_losses)
  Y_vec <- -log(one_minus_Fx)
  
  model <- lm(Y_vec ~ X_vec)
  
  ggplot(data = data.frame(X_vec, Y_vec), aes(x = X_vec, y = Y_vec)) +
    
    geom_point(color = color, fill = color, shape = 21, size = 2, stroke = 1.5) +
    
    geom_smooth(method = "lm", color = "#E74C3C", se = FALSE, linewidth = 1) +
    
    annotate("text", x = min(X_vec), y = max(Y_vec), label = asset_name, 
             color = color, fontface = "bold", size = 6, hjust = 0) +
    
    labs(title = paste("Tail Index Regression for", asset_name),
         x = "log(x) (log of largest losses)", 
         y = "-log(1 - F(x))") +
    
    theme_minimal() +
    
    theme(
      plot.background = element_rect(fill = "gray95", color = NA), 
      panel.background = element_rect(fill = "white", color = "black"), 
      panel.grid.major = element_line(linetype = "dotted", color = "gray70"),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 15, face = "bold"),
      axis.text = element_text(size = 13)
    )
}

plot_tail_regression(Data_assignment$`% of change for SM`, "Stock Market", color = "#1E90FF")  
plot_tail_regression(Data_assignment$`% of change for N`, "Nasdaq", color = "#F39C12")  
plot_tail_regression(Data_assignment$`% of change for O`, "Oil", color = "#27AE60")  

############################################# EVT #######################################
#4.d
logLikGPD_Excel <- function(par, exceedances) {
  xi   <- max(par[1], 1e-7)
  beta <- max(par[2], 1e-7)
  
  if (any(1 + xi * exceedances / beta <= 0)) {
    return(-1e10)  
  }
  
  M <- length(exceedances)
  
  ll_value <- -M * log(beta) - (1 + 1/xi) * sum(log(1 + xi * exceedances / beta))
  
  return(ll_value)
}

fitGPD_ExcelLike <- function(asset_name, data_inverted, threshold, M_exceed = 25) {
  exceed_all <- data_inverted[data_inverted > threshold] - threshold
  exceed_all <- na.omit(exceed_all)
  
  if (length(exceed_all) < M_exceed) {
    cat("Only", length(exceed_all), "excess forr", asset_name,
        "(<", M_exceed, ").\n")
    return(NULL)
  }
  
  exceedances <- sort(exceed_all, decreasing = TRUE)[1:M_exceed]
  M <- length(exceedances)
  
  init_par <- c(xi = 0.1, beta = sd(exceedances)) 
  
  fit <- optim(
    par     = init_par,
    fn      = logLikGPD_Excel,    
    exceedances = exceedances,
    method  = "L-BFGS-B",
    lower   = c(1e-7, 1e-7),      
    control = list(fnscale = -1) 
  )
  
  xi_hat   <- max(fit$par[1], 1e-7)
  beta_hat <- max(fit$par[2], 1e-7)
  LL <- fit$value  
  
  exceed_sorted  <- sort(exceedances)
  Fu_empirical   <- rank(exceed_sorted, ties.method = "first") / M
  Fu_theoretical <- 1 - (1 + xi_hat * (exceed_sorted / beta_hat))^(-1 / xi_hat)
  
  cat("\n=== GPD Excel-Like for", asset_name, "===\n")
  cat("Threshold  :", threshold, "\n")
  cat("M (exceedances)    :", M, "\n")
  cat("xi estimated          :", xi_hat, "\n")
  cat("beta estimated        :", beta_hat, "\n")
  cat("Log-likelihood  :", LL, "\n")

  return(list(
    asset        = asset_name,
    threshold    = threshold,
    M            = M,
    xi           = xi_hat,
    beta         = beta_hat,
    logLik       = LL,
    Fu_empirical = Fu_empirical,
    Fu_theoretical = Fu_theoretical,
    exceed_sorted = exceed_sorted
  ))
}

res_SM <- fitGPD_ExcelLike(
  asset_name    = "Stock Market",
  data_inverted = Data_assignment$Inverted_SM,
  threshold     = VaR_95_empirical_SM,
  M_exceed      = 25
)

res_N <- fitGPD_ExcelLike(
  asset_name    = "Nasdaq",
  data_inverted = Data_assignment$Inverted_N,
  threshold     = VaR_95_empirical_N,
  M_exceed      = 25
)

res_O <- fitGPD_ExcelLike(
  asset_name    = "Oil",
  data_inverted = Data_assignment$Inverted_O,
  threshold     = VaR_95_empirical_O,
  M_exceed      = 25
)


plot_gpd_fit <- function(gpd_res, point_color) {

  df_plot <- data.frame(
    x           = gpd_res$exceed_sorted,
    Empirical   = gpd_res$Fu_empirical,
    Theoretical = gpd_res$Fu_theoretical
  )
  
  p <- ggplot(df_plot, aes(x = x)) +
    geom_point(aes(y = Empirical), color = point_color, size = 2) +
    geom_line(aes(y = Theoretical), color = "#E74C3C", size = 1) +
    labs(
      title    = paste(gpd_res$asset, "- GPD Fit"),
      subtitle = "Empirical Distribution vs. Theoretical GPD",
      x        = "Exceedance (X - Threshold)",
      y        = "Fu(x)"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title    = element_text(face = "bold", hjust = 0.5, size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 12)
    )
  
  return(p)
}
  
p_SM <- plot_gpd_fit(
  gpd_res    = res_SM,
  point_color = "#1E90FF"
)

p_N <- plot_gpd_fit(
  gpd_res    = res_N,
  point_color = "#F39C12"
)

p_O <- plot_gpd_fit(
  gpd_res    = res_O,
  point_color = "#27AE60"
)

grid.arrange(p_SM, p_N, p_O, ncol = 3)
print(p_SM)
print(p_N)
print(p_O)



