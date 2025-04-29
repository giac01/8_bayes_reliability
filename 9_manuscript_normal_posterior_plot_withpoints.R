rm(list = ls(all.names = TRUE))

library(ggplot2)
library(patchwork)
library(dplyr)
library(tidyr)
library(viridis) # Use viridis package for color-blind-friendly palettes
# library(latex2exp)

J = 15

# sds = rep(1, J)

# Function to draw normal density curves and add random samples
plot_normal_densities <- function(reliability, 
                                  sds, 
                                  draw_points = TRUE,
                                  set_col = NULL
                                  
                                  ) {
  # Create a data frame for means and sds
  
  true_variance = reliability/(1-reliability)
  true_sd = sqrt(true_variance)
  
  means = rnorm(J, mean = 10, sd = true_sd)
  # print(sd(means))
  
  means = means/sd(means)*true_sd
  
  cat("true sd \n")
  print(sd(means))
  
  params <- data.frame(mean = means, sd = sds, group = factor(1:length(means)))
  
  # Generate a color palette using viridis
  # colors <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  # colors <- colors[0:(J-1) %% 7 + 1]
  colors <- c("#000000", "#D55E00") 
  colors <- colors[c(rep(1,(J-1)),2)]
  params$color <- colors
  
  if (!is.null(set_col)) params$color = set_col
  
  # Create a base ggplot
  p <- ggplot(
    data = data.frame(x = c(min(means) - 3*max(sds), max(means) + 3*max(sds))), 
    aes(x = x))
  
  sample_data = list()
  
  # Add the normal density curves
  for (i in 1:nrow(params)) {
    p <- p + stat_function(fun = dnorm, 
                           geom = "polygon",
                           args = list(mean = params$mean[i], sd = params$sd[i]), 
                           colour = params$color[i], 
                           fill = params$color[i],
                           alpha = 0,
                           size = 1)
    
    # Draw two random samples from each distribution
    samples <- rnorm(2, mean = params$mean[i], sd = params$sd[i])
    sample_data[[i]] <- data.frame(x = samples, y = dnorm(samples, mean = params$mean[i], sd = params$sd[i]), group = params$group[i], color = params$color[i])
    
    # Add the samples to the plot
    if(draw_points){
      p <- p + geom_point(data = sample_data[[i]], aes(x = x, y = y, color = color), size = 2) 
    }
  }
  
  # Customize the plot
  p <- p + labs(
    x = "θ",
    y = "θ Posterior"
  ) + 
    scale_color_identity() +
    theme_minimal() +
    theme(legend.position = "none") 
  
  sample_data = dplyr::bind_rows(sample_data) %>%
    mutate(group = factor(group)) %>%
    mutate(time = as.numeric(duplicated(group)) + 1) %>%
    pivot_wider(
      values_from = x,
      id_cols = c(group, color),
      names_from = time
    )
  
  point_plot = sample_data %>% 
    ggplot(aes(x = `1`, y = `2`, color = color)) + 
    geom_point(size = 2) + 
    theme_bw() + 
    labs(
      x = "Draw 1",
      y = "Draw 2"
    ) +
    scale_color_identity()
  
  # Return the plot
  return(list(p = p,
              sample_data = sample_data,
              point_plot = point_plot))
}

# Example usage


# debug(plot_normal_densities)
# set.seed(1)
# p1 = plot_normal_densities(reliability = .3, sds = rep(1, J), draw_points = FALSE, set_col = 'black')
# p2 = plot_normal_densities(reliability = .6, sds = rep(1, J), draw_points = FALSE)
# p3 = plot_normal_densities(reliability = .9, sds = rep(1, J), draw_points = FALSE)
# 
# p1$p + p2$p 

set.seed(1)
p1 = plot_normal_densities(reliability = .1, sds = rep(1, J), draw_points = TRUE)
set.seed(7)
p2 = plot_normal_densities(reliability = .6, sds = rep(1, J), draw_points = TRUE)
set.seed(2)
p3 = plot_normal_densities(reliability = .9, sds = rep(1, J), draw_points = TRUE)

p1$p = p1$p + labs(subtitle = "RMU Reliability = .10")
p2$p = p2$p + labs(subtitle = "RMU Reliability = .60", y = NULL)
p3$p = p3$p + labs(subtitle = "RMU Reliability = .90", y = NULL)
p1$p          = p1$p          + labs(title = expression(bold("Panel A:")~" Latent Variable (θ) Posterior Distributions for 15 Example Subjects"))
p1$point_plot = p1$point_plot + labs(title = expression(bold("Panel B:")~" Distribution of Pairs of Posterior Draws from each Subject"))
p2$point_plot = p2$point_plot + labs(y = NULL)
p3$point_plot = p3$point_plot + labs(y = NULL)


p1$p + p1$point_plot +
p2$p + p2$point_plot +
p3$p + p3$point_plot + plot_layout(nrow = 2, byrow = FALSE) + plot_annotation(tag_levels = c('A', '1'))

(p1$p + p2$p + p3$p ) /
(p1$point_plot + p2$point_plot + p3$point_plot) 


ggsave(file = file.path("plots","9_posteriors.png"), width = 8, height = 6, dpi = 300)
ggsave(file = file.path("plots","9_posteriors.pdf"), width = 8, height = 6, dpi = 300)

