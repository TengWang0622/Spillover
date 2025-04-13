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

library(metR)

############################################################################
#                    Random Forest Analysis 
############################################################################

Dim_Hea_A=1:7             # Service delivery
Dim_Hea_B=8:13            # Health workforce
Dim_Hea_C=14:17           # Preventative interventions
Dim_Hea_D=18:19           # Longevity
Dim_Hea_E=20:26           # Financing
Dim_Hea_F=27:30           # Physical health
Dim_Hea_G=31:32           # Mental health
Dim_Hea_H=33:37           # Maternal/child health
Dim_Hea_I=38:39           # Infectious disease

DimALL=39

df_City$Resilience=df_City$Res_Health
# ==============================================================================
#                            Random forest - Pillar
# ==============================================================================

rf_data <- df_City[, c((DimALL+4):(DimALL+4+8), which(names(df_City) == "Resilience"))]


set.seed(123)
train_index <- createDataPartition(rf_data$Resilience, p = 0.8, list = FALSE)
train_data <- rf_data[train_index, ]
test_data <- rf_data[-train_index, ]


ntree_range <- seq(100, 1000, by = 100)
oob_errors <- numeric(length(ntree_range))

for(i in seq_along(ntree_range)) {
  rf_temp <- randomForest(Resilience ~ ., 
                          data = train_data,
                          ntree = ntree_range[i],
                          importance = TRUE)
  oob_errors[i] <- tail(rf_temp$mse, 1)
}


p1 <- ggplot(data.frame(ntree = ntree_range, oob = oob_errors), 
             aes(x = ntree, y = oob)) +
  geom_line(color = "#8460DA", size = 1) +
  geom_point(color = "#8460DA", size = 3) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.text = element_text(color = "black", size = 12,family="Arial")
  ) +
  labs(x = "Number of trees",
       y = "Out-of-bag error")

optimal_ntree <- ntree_range[which.min(oob_errors)]


mtry_values <- seq(2, ncol(train_data)-1, by = 1)
cv_errors <- numeric(length(mtry_values))

ctrl <- trainControl(method = "cv", number = 5)
mtry_search <- train(
  Resilience ~ .,
  data = train_data,
  method = "rf",
  trControl = ctrl,
  tuneGrid = data.frame(mtry = mtry_values)
)

optimal_mtry <- mtry_search$bestTune$mtry


nodesize_values <- c(1, 3, 5, 7, 9)
nodesize_errors <- numeric(length(nodesize_values))

for(i in seq_along(nodesize_values)) {
  rf_temp <- randomForest(
    Resilience ~ .,
    data = train_data,
    ntree = optimal_ntree,
    mtry = optimal_mtry,
    nodesize = nodesize_values[i]
  )
  nodesize_errors[i] <- mean(rf_temp$mse)
}

optimal_nodesize <- nodesize_values[which.min(nodesize_errors)]


final_rf <- randomForest(Resilience ~ .,
                         data = train_data,
                         ntree = optimal_ntree,
                         mtry = optimal_mtry,
                         nodesize = optimal_nodesize,
                         importance = TRUE)


importance_df <- importance(final_rf) %>%
  as.data.frame() %>%
  rownames_to_column("Variable") %>%
  arrange(desc(`%IncMSE`)) %>%
  mutate(relative_contribution = (`%IncMSE` / sum(`%IncMSE`)) * 100)

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

custom_order <- rev(c("Dim_Hea_A", 
                      "Dim_Hea_B",
                      "Dim_Hea_C",
                      "Dim_Hea_D",
                      "Dim_Hea_E",
                      "Dim_Hea_F",
                      "Dim_Hea_G",
                      "Dim_Hea_H",
                      "Dim_Hea_I"))

importance_df <- importance_df %>%
  mutate(
    color_group = case_when(
      grepl("Dim_Hea_A", Variable) ~ "#8460DA",
      grepl("Dim_Hea_B", Variable) ~ "#8460DA",
      grepl("Dim_Hea_C", Variable) ~ "#8460DA",
      grepl("Dim_Hea_D", Variable) ~ "#8460DA",
      grepl("Dim_Hea_E", Variable) ~ "#8460DA",
      grepl("Dim_Hea_F", Variable) ~ "#8460DA",
      grepl("Dim_Hea_G", Variable) ~ "#8460DA",
      grepl("Dim_Hea_H", Variable) ~ "#8460DA",
      grepl("Dim_Hea_I", Variable) ~ "#8460DA"
    )
  )

importance_df$Variable <- factor(importance_df$Variable, levels = custom_order)
importance_df$Variable_label <- var_names[as.character(importance_df$Variable)]
importance_df$Variable_label <- factor(importance_df$Variable_label, 
                                       levels = var_names[custom_order])

ggplot(importance_df, 
       aes(x = reorder(Variable_label, `%IncMSE`), #Variable_label, reorder(Variable, `%IncMSE`),
           y = relative_contribution)) +
  geom_bar(stat = "identity", 
           aes(fill = color_group),
           width = 0.2,
           alpha = 0.6) +
  geom_point(aes(color = color_group),
             size = 7.5,
             alpha = 1,
             shape = 16) +
  scale_fill_identity() +
  scale_color_identity() +
  coord_flip() +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    axis.text.y = element_text(color = "black", size = 12),
    axis.title.y = element_blank(),
    axis.text.x = element_text(color = "black", size = 12),
    axis.title.x = element_text(color = "black", size = 12),
    panel.border = element_rect(color = "black", 
                                fill = NA, 
                                linewidth = 1),
    plot.title = element_text(size = 12, face = "bold"),    
    axis.line = element_line(color = "black", linewidth = 0.3)
  ) +
  scale_y_continuous(limits = c(0, 20)) +
  labs(y = "Relative importance (%)")

# Resilience
#eval(parse(text = paste('ggsave("/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/Spillover/Result RF/Relative importance/Resistance/Pillars_RF.png", width = 8, height = 6, dpi = 600)',sep="")))

#eval(parse(text = paste('ggsave("/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/Spillover/Result RF/Relative importance/Resistance/Pillars_RF_narrow.png", width = 4.5, height = 6, dpi = 600)',sep="")))

OrderALL=importance_df
OrderALL <- OrderALL %>% arrange(match(Variable, rev(custom_order)))
OrderALL=as.data.frame(OrderALL)
print(OrderALL)

eval(parse(text = paste('write.xlsx(OrderALL, "/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/Spillover/Result RF/Relative importance/Resistance/Resistance_Pillar_importance.xlsx", rowNames = FALSE)',sep="")))



test_pred <- predict(final_rf, test_data)
test_rmse <- sqrt(mean((test_data$Resilience - test_pred)^2))
test_r2 <- 1 - sum((test_data$Resilience - test_pred)^2) / 
  sum((test_data$Resilience - mean(test_data$Resilience))^2)


model_results <- list(
  optimal_parameters = data.frame(
    ntree = optimal_ntree,
    mtry = optimal_mtry,
    nodesize = optimal_nodesize
  ),
  performance_metrics = data.frame(
    RMSE = test_rmse,
    R2 = test_r2
  ),
  importance = importance_df
)

print("Optimal Parameters:")
print(model_results$optimal_parameters)
print("\nModel Performance:")
print(model_results$performance_metrics)
print("\nTop 5 Important Variables:")
print(head(model_results$importance, 5))
















