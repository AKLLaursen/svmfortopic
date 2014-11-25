#' Function for plotting the maximal margin classifier
#' @param set_seed An integer, setting the seed. Default value matches graph in
#' topic
#' @param save_path A path string. If given, saves an .eps file to local hard
#' drive
#' @param do_print A string, yes or no
#' @export
#' 
max_marg_class <- function(set_seed = 1001, save_path = NULL, do_print = "Yes")
{
  # Create separable points.
  set.seed(set_seed)
  
  x = matrix(rnorm(40), 20, 2)
  y = rep(c(-1, 1), c(10, 10))
  x[y == 1, ] = x[y == 1, ] + 2
  x[y == -1, ] = x[y == -1, ] - 2
  
  data_frame <- data.frame(x,
                           y = y %>% as.factor)
  
  # Fit model
  model_out <- svm(y ~ .,
                   data = data_frame,
                   kernel = "linear",
                   gamma = 0,
                   scale = FALSE)
  
  # Calculate separating hyperplane and margins
  w <- t(model_out$coefs) %*% model_out$SV
  slope <- -w[1] / w[2]
  hyper_inter <- model_out$rho / w[2]
  
  b_down <- head(model_out$SV, 1)
  lower_inter <- b_down[2] - slope * b_down[1]
  b_up <- tail(model_out$SV, 1)
  upper_inter <- b_up[2] - slope * b_up[1]
  
  # Plot graph
  p <- ggplot(data_frame,
              aes(x = X1, y = X2, colour = y)) +
    geom_point(size = 3,
               shape = 19) +
    scale_color_manual(values = c("#E4001B", "#003366")) +
    geom_point(data = data_frame[model_out$index, ],
               aes(x = X1, y = X2),
               size = 6,
               shape = 1,
               colour = "black") +
    geom_abline(intercept = hyper_inter,
                slope = slope) +
    geom_abline(intercept = lower_inter,
                slope = slope,
                linetype = "dashed") +
    geom_abline(intercept = upper_inter,
                slope = slope,
                linetype = "dashed")
  
  # Print plot
  if (tolower(do_print) == "yes") print(p)
  
  # Possibly save graphs
  if (!is.null(save_path)) {
    ggsave(filename = "3_2_max_margin.eps",
           plot = p,
           path = save_path,
           scale = 1,
           width = 21,
           height = 21 * 0.5,
           units = "cm",
           dpi = 1000)
  }
}

#' Function for plotting the the epsilon-intensity linear loss function and
#' Huber's loss function.
#' @param save_path A path string. If given, saves an .eps file to local hard
#' drive
#' @param do_print A string, yes or no
#' @export
loss_function <- function(save_path = NULL, do_print = "Yes") {
  # Epsilon intensity loss function
  eps <- function(x, epsilon = 1.5) {
    ifelse(abs(x) < epsilon, 0, abs(x) - epsilon)
  }
  
  # Huber's loss function
  hub <- function(x, c = 3) {
    ifelse(abs(x) <= c, x ** 2 / 2, c * abs(x) - c ** 2 / 2)
  }
  
  plots <- list(
    p1 = ggplot(data.frame(x = c(-6, 6)), aes(x)) +
      stat_function(fun = eps, colour = "#003366") +
      geom_hline(yintercept = 0, colour = "black", linetype = "dashed") + 
      geom_vline(xintercept = 0, colour = "black", linetype = "dashed") +
      annotate("segment", x = 1.5, xend = 1.5, y = 0, yend = -1,
               colour = "#E4001B", linetype = "dashed") +
      annotate("segment", x = -1.5, xend = -1.5, y = 0, yend = -1,
               colour = "#E4001B", linetype = "dashed") +
      annotate("text", x = 2, y = -.5, parse = TRUE,
               label = "epsilon", size = 5) +
      annotate("text", x = -2.3, y = -.5, parse = TRUE,
               label = "-epsilon", size = 5) +
      xlab(expression("|y-f(x,a)|")) +
      ylab(expression("|y - f(x,a)|"[epsilon])),
    p2 = ggplot(data.frame(x = c(-6, 6)), aes(x)) +
      stat_function(fun = hub, colour = "#003366") +
      stat_function(fun = function(x) x ** 2 / 2, colour = "black",
                    linetype = "dashed") +
      geom_hline(yintercept = 0, colour = "black", linetype = "dashed") + 
      geom_vline(xintercept = 0, colour = "black", linetype = "dashed") +
      annotate("segment", x = 3, xend = 3, y = 3 ** 2 / 2, yend = -1,
               colour = "#E4001B", linetype = "dashed") +
      annotate("segment", x = -3, xend = -3, y = (-3) ** 2 / 2, yend = -1,
               colour = "#E4001B", linetype = "dashed") +
      annotate("text", x = 3.5, y = 2.5, parse = TRUE,
               label = "c", size = 5) +
      annotate("text", x = -3.8, y = 2.5, parse = TRUE,
               label = "-c", size = 5) +
      xlab("|y-f(x,a)|") +
      ylab(expression("|y-f(x,a)|"["H"])))
  p <- arrangeGrob(plots$p1, plots$p2, ncol = 2)
  
  # Print plot
  if (tolower(do_print) == "yes") print(p)
  
  # Possibly save graphs
  if (!is.null(save_path)) {
    ggsave(filename = "3_9_loss_function.eps",
           plot = p,
           path = save_path,
           scale = 1,
           width = 21,
           height = 21 * 0.5,
           units = "cm",
           dpi = 1000)
  }
}