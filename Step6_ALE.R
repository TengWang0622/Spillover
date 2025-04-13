##########################################################################################################
#
#                                    P   R   O   J   E   C   T
#                                                of
#               Healthcare system resilience and adaptation policies under climate hazards
#
###########################################################################################################

# Developed by Teng Wang, Hanxu Shi

# Contact: wang.teng19@alumni.imperial.ac.uk
#          shx@bjmu.edu.cn

# Version - 20240325

# Description: Main Script

############################################
#             Preparation
############################################

library(readxl)
library(tidyverse)
#library(dlnm)
#library(splines)
#library(survival)
#library(mvmeta)
library(dplyr)
library(magrittr)
library(Matrix)

library(foreach)
library(doParallel)
library(progress)

library(lme4)
#library(lmerTest)

library(ape)

library(MatchIt)
library(WeightIt)
library(MASS)
library(cli)

library(openxlsx)

library(ggridges)
library(viridis)

library(googleLanguageR)


library(randomForest)
library(dplyr)
library(ggplot2)
library(caret)
library(pdp)
library(gridExtra)

library(ALEPlot)
library(viridis)

library(akima)


###################################################################
#                     1D ALE Analysis  
###################################################################

var_names <- c(
  "Dim_Hea_A" = "Service delivery",
  "Dim_Hea_B" = "Health workforce",
  "Dim_Hea_C" = "Preventative interventions",
  "Dim_Hea_D" = "Longevity",
  "Dim_Hea_E" = "Financing",
  "Dim_Hea_F" = "Physical health",
  "Dim_Hea_G" = "Mental health",
  "Dim_Hea_H" = "Maternal/child health",
  "Dim_Hea_I" = "Infectious disease"
)

yhat <- function(X.model, newdata) {
  predict(X.model, newdata)
}

top_features <- importance_df$Variable[1:nrow(importance_df)]


rf_data <- as.data.frame(sapply(rf_data, as.numeric))

X_vars <- names(rf_data)[names(rf_data) != "Resilience"]
X_matrix <- as.data.frame(rf_data[, X_vars])  # 确保是数据框
y_vector <- rf_data$Resilience

ale_plots_1d <- list()

for(i in seq_along(top_features)) {
  feature <- top_features[i]
  feature_index <- which(names(X_matrix) == feature)
  
  ale_data <- ALEPlot(
    X = X_matrix,
    X.model = final_rf,
    pred.fun = yhat,
    J = feature_index,
    K = 50
  )
  
  sd_ale <- sd(ale_data$f.values)
  ci_margin <- 1.96 * sd_ale
  
  ale_df <- data.frame(
    x = ale_data$x.values,
    ale = ale_data$f.values,
    upper = ale_data$f.values + ci_margin,
    lower = ale_data$f.values - ci_margin
  )
  
  p <- ggplot(ale_df, aes(x = x, y = ale)) +
    geom_ribbon(aes(ymin = lower, ymax = upper),
                fill = "#8460DA", alpha = 0.2) +
    
    geom_line(color = "#8460DA", size = 1) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12)
    ) +
    #ylim(-0.25,0.3) +       
    labs(
      x = var_names[feature_index],
      y = "ALE value"
    )
  
  ale_plots_1d[[i]] <- p
  
  ale_plots_1d[[i]]
  #eval(parse(text = paste('ggsave("/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result RF PDP/PDP/AC_ALE_STD_',var_names[feature_index],'_1d.png", width = 8, height = 6, dpi = 600)',sep="")))
  
}

grid_1d <- do.call(grid.arrange, c(ale_plots_1d, ncol = 3))

ggsave("/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/Spillover/Result RF/ALE/Recovery/Recovery_ALE_STD_Pillar.png", 
       grid_1d, width = 12, height = 8, dpi = 600)

###############################################################
#                    2D ALE Analysis     
###############################################################

feature_pairs <- combn(top_features[1:5], 2, simplify = FALSE)
ale_plots_2d <- list()


for(i in seq_along(feature_pairs)) {
  pair <- feature_pairs[[i]]
  J_indices <- c(which(names(X_matrix) == pair[1]),
                 which(names(X_matrix) == pair[2]))
  
  ale_2d <- ALEPlot(
    X = X_matrix,
    X.model = final_rf,
    pred.fun = yhat,
    J = J_indices,
    K = 50
  )
  
  x_values <- rep(ale_2d$x.values[[1]], each = length(ale_2d$x.values[[2]]))
  y_values <- rep(ale_2d$x.values[[2]], times = length(ale_2d$x.values[[1]]))
  z_values <- as.vector(ale_2d$f.values)
  

  ale_2d_df <- data.frame(
    x = x_values,
    y = y_values,
    z = z_values
  )
  
  p <- ggplot(ale_2d_df, aes(x = x, y = y, fill = z)) +
    geom_tile() +
    scale_fill_viridis_c(option = "magma", name = "ALE value") +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12),
      legend.position = "right"
    ) +
    labs(
      x = var_names[pair[1]],  # 使用映射后的变量名
      y = var_names[pair[2]],  # 使用映射后的变量名
      title = paste("2D ALE Plot:", var_names[pair[1]], "vs", var_names[pair[2]])
    ) +
    
    geom_contour(aes(z = z), color = "white", alpha = 0.5, bins = 10)
  
  ale_plots_2d[[i]] <- p
}


grid_2d <- do.call(grid.arrange, c(ale_plots_2d, ncol = 2))
ggsave("/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result RF PDP/ALE_2d_plots.png", 
       grid_2d, width = 12, height = 20, dpi = 600)


########################### ALE Interaction strength ###########################  

n_points <- 50

ale_values_list <- list()

for(feature in top_features) {
  feature_index <- which(names(X_matrix) == feature)
  
  ale_result <- ALEPlot(
    X = X_matrix,
    X.model = final_rf,
    pred.fun = yhat,
    J = feature_index,
    K = n_points
  )
  
  x_range <- range(ale_result$x.values)
  x_seq <- seq(x_range[1], x_range[2], length.out = n_points)
  
  interpolated_values <- approx(
    x = ale_result$x.values,
    y = ale_result$f.values,
    xout = x_seq,
    method = "linear"
  )$y
  
  ale_values_list[[feature]] <- scale(interpolated_values)
}

lengths <- sapply(ale_values_list, length)
print("Length of ALE values for each feature:")
print(lengths)

ale_matrix <- do.call(cbind, ale_values_list)
colnames(ale_matrix) <- top_features

print("Dimensions of ALE matrix:")
print(dim(ale_matrix))

cor_matrix <- cor(ale_matrix)^2  # 平方后得到解释方差

# Heatmap
cor_df <- as.data.frame(as.table(cor_matrix))
names(cor_df) <- c("Var1", "Var2", "Correlation")

custom_order <- rev(c("Dim_Hea_A", 
                      "Dim_Hea_B",
                      "Dim_Hea_C",
                      "Dim_Hea_D",
                      "Dim_Hea_E",
                      "Dim_Hea_F",
                      "Dim_Hea_G",
                      "Dim_Hea_H",
                      "Dim_Hea_I"))

cor_df$Var1 <- factor(cor_df$Var1, levels = custom_order)
cor_df$Var2 <- factor(cor_df$Var2, levels = custom_order)

var_names <- c(
  "Dim_Hea_A" = "Service delivery",
  "Dim_Hea_B" = "Health workforce",
  "Dim_Hea_C" = "Preventative interventions",
  "Dim_Hea_D" = "Longevity",
  "Dim_Hea_E" = "Financing",
  "Dim_Hea_F" = "Physical health",
  "Dim_Hea_G" = "Mental health",
  "Dim_Hea_H" = "Maternal/child health",
  "Dim_Hea_I" = "Infectious disease"
)

correlation_plot <- ggplot(cor_df, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(
    colors = c("#FFFFFF", "#E6F3FF", "#CCE7FF", "#99CCFF", "#66B2FF", "#3399FF", "#0080FF", "#0066CC", "#004C99"),
    limits = c(0, 1),
    name = "Explained\nVariance"
  ) +
  geom_text(aes(label = sprintf("%.2f", Correlation)), 
            color = ifelse(cor_df$Correlation > 0.5, "white", "black"),
            size = 3) +
  scale_x_discrete(labels = var_names) +  # 应用新的变量名
  scale_y_discrete(labels = var_names) +  # 应用新的变量名
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
  ) +
  coord_fixed()# +
#labs(title = "ALE Feature Interaction Strength (Explained Variance)")

ggsave("/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/Spillover/Result RF/Correlation/Recovery_ALE_correlation_heatmap.png", 
       correlation_plot, 
       width = 8, 
       height = 8, 
       dpi = 600)

high_correlations <- which(cor_matrix >=0  & cor_matrix <= 1, arr.ind = TRUE)
correlation_summary <- data.frame(
  Feature1 = var_names[rownames(cor_matrix)[high_correlations[,1]]],
  Feature2 = var_names[colnames(cor_matrix)[high_correlations[,2]]],
  ExplainedVariance = cor_matrix[high_correlations]
)
correlation_summary <- correlation_summary[order(-correlation_summary$ExplainedVariance),]


cat("\nStrong Feature Interactions (Explained Variance > 0.3):\n")
print(correlation_summary)

mean_interactions <- colMeans(cor_matrix) - 1
mean_interactions_df <- data.frame(
  Feature = var_names[names(mean_interactions)],
  MeanInteractionStrength = mean_interactions
)
mean_interactions_df <- mean_interactions_df[order(-mean_interactions_df$MeanInteractionStrength),]

cat("\nAverage Feature Interaction Strength:\n")
print(mean_interactions_df)


