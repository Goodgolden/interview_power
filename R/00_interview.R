
#' Title Interview Lecture Power function
#'
#' @param n sample size
#' @param alpha significance level
#' @param sigma standard deviation
#' @param d effect size (in SD units)
#'
#' @returns power and a plot

power_plot_left <- function(n = 3,
                            alpha = 0.05, 
                            sigma = 1,
                            d = 1) {
  ## Parameters
  mu0 <- 0
  mu1 <- mu0 - d * sigma
  sd_xbar <- sigma / sqrt(n)
  
  ## Critical value (LEFT-tailed)
  z_alpha <- qnorm(alpha)
  crit <- mu0 + z_alpha * sd_xbar
  
  ## Power calculation
  power <- pnorm(crit, 
                 mean = mu1, 
                 sd = sd_xbar)
  
  # Data
  x <- seq(mu1 - 4 * sd_xbar, 
           mu0 + 4 * sd_xbar, 
           length.out = 1000)
  df <- data.frame(x = x,
                   H0 = dnorm(x, mean = mu0, 
                              sd = sd_xbar),
                   H1 = dnorm(x, mean = mu1, 
                              sd = sd_xbar))
  
  ## Final plot
  p <- ggplot(df, aes(x = x)) +
    geom_line(aes(y = H0), 
              linetype = "dashed", 
              linewidth = 1) +
    geom_line(aes(y = H1), 
              linewidth = 1) +
    ## Type I error (LEFT)
    geom_area(data = subset(df,
                            x <= crit),
              aes(y = H0),
              fill = "red", 
              alpha = 0.4) +
    ## Power (LEFT under H1)
    geom_area(data = subset(df, x <= crit),
              aes(y = H1),
              fill = "blue", alpha = 0.4) +
    geom_vline(xintercept = crit, 
               linetype = "dotted") +
    ## mu labels
    annotate("text",
             x = mu0, 
             y = max(df$H0) * 1.05,
             label = expression(mu[0]), 
             size = 5) +
    annotate("text",
             x = mu1, 
             y = max(df$H1) * 1.05,
             label = expression(mu[1]), 
             size = 5) +
    ## Top-right annotation
    annotate("text",
             x = Inf, 
             y = Inf,
             label = paste0("n = ", n,
                            "\n d = ", d,
                            "\n \u03B1 = ", alpha,
                            "\n Power = ", 
                            round(power, 2)),
             hjust = 1.1, 
             vjust = 1.1,
             size = 5) +
    ## Remove axis labels
    labs(x = NULL, y = NULL) +
    theme_bw(base_size = 15)
  
  return(list(power = power, 
              plot = p))
}

power_plot1 <- power_plot_left(n = 10, alpha = 0.10, d = 1)
power_plot1$plot

power_plot2 <- power_plot_left(n = 10, alpha = 0.05, d = 1)
power_plot2$plot

power_plot3 <- power_plot_left(n = 10, alpha = 0.01, d = 1)
power_plot3$plot

library(gridExtra)

combined_plot <- grid.arrange(power_plot1$plot,
                              power_plot2$plot,
                              power_plot3$plot,
                              ncol = 3)

# Save
ggsave("figure/power_plots_row.png",
       combined_plot,
       width = 15, height = 5, dpi = 300)

