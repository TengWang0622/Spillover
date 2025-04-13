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


######################################################################
#                  Partial dependence plots  STD
######################################################################

######################## 1D PDP ###############################

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


top_vars <- importance_df$Variable[1:nrow(importance_df)]

par(mfrow = c(2,3))
for(var in top_vars) {
  partial(final_rf, pred.var = var, plot = TRUE,
          main = paste("Partial Dependence on", var))
}


pdp_plots <- list()

for(var in top_vars) {
  pd <- partial(final_rf, 
                pred.var = var, 
                grid.resolution = 100)
  
  p <- ggplot(pd, aes(x = get(var), y = yhat)) +
    geom_line(color = "#8460DA", linewidth = 1) +        # #40E0D0
    geom_ribbon(aes(ymin = yhat - 1.96*sd(yhat),
                    ymax = yhat + 1.96*sd(yhat)),
                alpha = 0.2,
                fill = "#8460DA") +                      # #AFEEEE
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      plot.title = element_text(size = 22, face = "bold"),
      axis.text.y = element_text(color = "black", size = 22),
      axis.text.x = element_text(color = "black", size = 22),
      axis.title.x = element_text(color = "black", size = 22),
      axis.title.y = element_text(color = "black", size = 22)
    ) +
    
    ylim(-0.1,0.5)+ 
    labs(
      x = var_names[var],  
      y = "Partial dependence")
  
  pdp_plots[[var]] <- p
  pdp_plots[[var]]
  eval(parse(text = paste('ggsave("/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result RF PDP/PDP/AC_STD_',var,'_PDP_1d.png", width = 8, height = 6, dpi = 600)',sep="")))
  
}


# ======================= Combine all the figs into one ============================

top_vars <- importance_df$Variable[1:20]

top_vars <- importance_df$Variable[1:nrow(importance_df)]
pdp_plots <- list()

for(var in top_vars) {
  pd <- partial(final_rf, 
                pred.var = var, 
                grid.resolution = 100)
  
  p <- ggplot(pd, aes_string(x = var, y = "yhat")) + 
    geom_line(color = "#8460DA", linewidth = 1) +
    geom_ribbon(aes(ymin = yhat - 1.96*sd(yhat),
                    ymax = yhat + 1.96*sd(yhat)),
                alpha = 0.2,
                fill = "#8460DA") +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      plot.title = element_text(size = 11, face = "bold")
    ) +
    #ylim(25, 65) +          
    labs(
      x = var_names[var],  
      y = "Partial dependence")
  
  pdp_plots[[var]] <- p
  pdp_plots[[var]]
  #eval(parse(text = paste('ggsave("/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result RF PDP/PDP/Res',var,'_PDP_1d_Bootstrap.png", width = 8, height = 6, dpi = 600)',sep="")))
  #eval(parse(text = paste('ggsave("/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result RF PDP/PDP/AC_STD_',var,'_PDP_1d.png", width = 8, height = 6, dpi = 600)',sep="")))
  
}

grid_pdp <- do.call(grid.arrange, c(pdp_plots, ncol = 3))


eval(parse(text = paste('ggsave("/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/Spillover/Result RF/PDP/Recovery/Resistance_PDP_1d_STD_Pillar.png", 
       grid_pdp, width = 12, height = 8, dpi = 600)',sep="")))

# Combine
eval(parse(text = paste('ggsave("/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/Spillover/Result RF/PDP/Recovery/Resistance_PDP_1d_STD_Pillar_Combine.png", 
       grid_pdp, width = 8, height = 6, dpi = 600)',sep="")))



####################### 2D PDP ################################


Name_vars=importance_df$Variable_label[1:nrow(importance_df)] # variable name list

top_vars <- importance_df$Variable[1:nrow(importance_df)]

Name_pairs=combn(Name_vars[1:4], 2, simplify = FALSE) # variable name list

feature_pairs <- combn(top_vars[1:4], 2, simplify = FALSE)
pdp_plots_2d <- list()

for(i in seq_along(feature_pairs)) {
  pair <- feature_pairs[[i]]
  
  name_pair=Name_pairs[[i]]
  
  pd_2d <- partial(final_rf,
                   pred.var = as.character(pair),
                   grid.resolution = 20)
  
  names(pd_2d)[1:2] <- c("var1", "var2")
  
  p <- ggplot(pd_2d, aes(x = var1, y = var2, z = yhat)) +
    geom_tile(aes(fill = yhat)) +
    
    #scale_fill_gradientn(
    #  colors = c("#FFFFFF", "#B5A4E4", "#8460DA"),  
    #  name = "Partial\ndependence",
    #  limits = c(33,45)
    #) +
    
    scale_fill_viridis_c(
      option = "viridis",  # 使用magma配色方案 viridis, magma
      name = "Partial\ndependence"  # ,
      #limits = c(0.29,0.37)
    ) +
    
    geom_contour(aes(z = yhat),
                 color = "white", 
                 alpha = 0.5, 
                 bins = 10) +
    
    geom_text_contour(aes(z = yhat),
                      color = "white",
                      size = 3,
                      skip = 1) +  # 每隔一条等高线标注数值
    #  geom_contour_label(aes(z = yhat),
    #                     bins = 8,
    #                     size = 3,
    #                     label.placer = label_placer_fraction(frac = 0.5),
    #                     color = "white",
    #                    label.padding = unit(0.1, "lines")) +
    
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12),
      legend.position = "right",
      aspect.ratio=1
    ) +
    labs(
      x = name_pair[1],    #var_names[pair[1]],
      y = name_pair[2]    #var_names[pair[2]]
    ) +
    coord_fixed()
  
  pdp_plots_2d[[i]] <- p
}

# 保存二维PDP图
grid_pdp_2d <- do.call(grid.arrange, c(pdp_plots_2d, ncol = 6))


ggsave("/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/Spillover/Result RF/PDP/Resistance/Resistance_Pillar_PDP_2d_plots_BOOTSTRAP_1x6.png", 
       grid_pdp_2d, width = 24, height = 4, dpi = 600)


grid_pdp_2d <- do.call(grid.arrange, c(pdp_plots_2d, ncol = 3))
ggsave("/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/Spillover/Result RF/PDP/Resistance/Resistance_Pillar_PDP_2d_plots_BOOTSTRAP_2x3.png", 
       grid_pdp_2d, width = 12, height = 8, dpi = 600)

grid_pdp_2d <- do.call(grid.arrange, c(pdp_plots_2d, ncol = 2))
ggsave("/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/Spillover/Result RF/PDP/Resistance/Resistance_Pillar_PDP_2d_plots_BOOTSTRAP_3x2.png", 
       grid_pdp_2d, width = 8, height = 12, dpi = 600)

grid_pdp_2d <- do.call(grid.arrange, c(pdp_plots_2d, ncol = 2))
ggsave("/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/Spillover/Result RF/PDP/Resistance/Resistance_Pillar_PDP_2d_plots_BOOTSTRAP_3x2_low.png", 
       grid_pdp_2d, width = 8, height = 9, dpi = 600)

########################################################################
#                             Bootstrap
########################################################################

####################### 1D PDP #######################

top_vars <- importance_df$Variable[1:16]

top_vars <- importance_df$Variable_label[1:nrow(importance_df)]

pdp_plots <- list()

calculate_bootstrap_ci <- function(model, data, pred_var, n_boot = 100) {
  n <- nrow(data)
  boot_predictions <- matrix(NA, nrow = 100, ncol = n_boot)
  
  for(i in 1:n_boot) {
    # Bootstrap sample
    boot_idx <- sample(1:n, n, replace = TRUE)
    boot_data <- data[boot_idx, ]
    
    # Calculate PDP for bootstrap sample
    pd <- partial(model, 
                  pred.var = pred_var,
                  train = boot_data,
                  grid.resolution = 100)
    
    boot_predictions[, i] <- pd$yhat
  }
  
  # Calculate CI
  ci_lower <- apply(boot_predictions, 1, quantile, probs = 0.025)
  ci_upper <- apply(boot_predictions, 1, quantile, probs = 0.975)
  
  return(list(lower = ci_lower, upper = ci_upper))
}

top_vars <- importance_df$Variable[1:nrow(importance_df)]

for(var in top_vars) {
  pd <- partial(final_rf, 
                pred.var = var, 
                grid.resolution = 100)
  
  ci <- calculate_bootstrap_ci(final_rf, rf_data, var)
  pd$ci_lower <- ci$lower
  pd$ci_upper <- ci$upper
  
  p <- ggplot(pd, aes_string(x = var, y = "yhat")) +
    
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper),
                fill = "#8460DA", alpha = 0.2) +
    geom_line(color = "#8460DA", linewidth = 1) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      plot.title = element_text(size = 11, face = "bold"),
      axis.text.x = element_text(color = "black", size = 22),
      axis.title.x = element_text(color = "black", size = 22),
      axis.text.y = element_text(color = "black", size = 22),
      axis.title.y = element_text(color = "black", size = 22)
    ) +
    # ylim(0.1, 0.3) +  
    labs(
      x = var_names[var],  
      y = "Partial dependence"
    )
  
  pdp_plots[[var]] <- p
  pdp_plots[[var]]
  #eval(parse(text = paste('ggsave("/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result RF PDP/PDP/Res',var,'_PDP_1d_Bootstrap.png", width = 8, height = 6, dpi = 600)',sep="")))
  #eval(parse(text = paste('ggsave("/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/Spillover/Result RF/PDP/Res',var,'_PDP_2d_Bootstrap.png", width = 8, height = 6, dpi = 600)',sep="")))
  
}

grid_pdp <- do.call(grid.arrange, c(pdp_plots, ncol = 3))

ggsave("/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/Sp/Result RF PDP/PDP/Res_Ele_PDP_1d_BootstrapFULL.png", 
       grid_pdp, width = 25, height = 30, dpi = 600)

######################## 2D PDP ########################

top_vars <- importance_df$Variable_label[1:nrow(importance_df)]
top_vars <- importance_df$Variable[1:nrow(importance_df)]


feature_pairs <- combn(top_vars[1:4], 2, simplify = FALSE)
pdp_plots_2d <- list()

for(i in seq_along(feature_pairs)) {
  pair <- feature_pairs[[i]]
  
  pd_2d <- partial(final_rf,
                   pred.var = pair,
                   grid.resolution = 20)

  p <- ggplot(pd_2d, aes_string(x = pair[1], y = pair[2], z = "yhat")) +
    geom_tile(aes(fill = yhat)) +
    scale_fill_viridis_c(option = "plasma", name = "Partial\ndependence", limits = c(33,45)) +
    geom_contour(color = "white", alpha = 0.5, bins = 10) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12),
      legend.position = "right"
    ) +
    labs(
      x = var_names[pair[1]],
      y = var_names[pair[2]] 
      #title = paste("2D PDP:", var_names[pair[1]], "vs", var_names[pair[2]])
    ) +
    coord_fixed()  
  
  pdp_plots_2d[[i]] <- p
}

grid_pdp_2d <- do.call(grid.arrange, c(pdp_plots_2d, ncol = 5))
ggsave("/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result RF PDP/Res_Ele_PDP_2d_plots_BOOTSTRAP.png", 
       grid_pdp_2d, width = 24, height = 8, dpi = 600)


