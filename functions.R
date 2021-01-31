# Load Libraries
cat('Loading packages')
lapply(
  c('ggplot2', 'broom', 'dplyr', 'scales', 'patchwork', 'purrr', 'directlabels', 'readr'),
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

m.lin <- function(data,
                  x,
                  y,
                  z = NULL,
                  type = c('lin', 'quad1', 'quad2'),
                  anova = F,
                  plot = T,
                  file = 'lin_model_plot'){
  if(type=='lin'){
    if(!is.null(z)){
      cat('Constructing linear model:\n', y, '~', x, '+', z, '\n', sep = '')
      # Construct model
      m <- lm(get(y) ~ get(x) + get(z), data = data)
      # Coefficients
      cf <- m %>% tidy() %>% mutate(term = c('Intercept', x, z))
      # Fit
      ft <- glance(m)
      # Residuals
      rs <- augment(m) %>% rename(!!x := `get(x)`, !!y := `get(y)`, !!z := `get(z)`)
      # Plot label
      p.lab <- bquote(atop(z==.(scientific(cf$estimate[2], digits = 3))~x+
                             .(round(cf$estimate[3], 3))~y+
                             .(round(cf$estimate[1], 1)),
                           R^2==.(round(ft$r.squared, 2))))
    } else {
      cat('Constructing linear model:\n', y, '~', x, '\n', sep = '')
      # Construct model
      m <- lm(get(y) ~ get(x), data = data)
      # Coefficients
      cf <- m %>% tidy() %>% mutate(term = c('Intercept', x))
      # Fit
      ft <- glance(m)
      # Residuals
      rs <- augment(m) %>% rename(!!x := `get(x)`, !!y := `get(y)`)
      # Plot label
      p.lab <- bquote(atop(y==.(scientific(cf$estimate[2], digits = 3))~x+
                             .(round(cf$estimate[1], 1)),
                           R^2==.(round(ft$r.squared, 2))))
    }
  } else if(type=='quad1'){
    if(!is.null(z)){
      cat('Constructing linear model:\n', y, '~', x, '^2+', z, '\n', sep = '')
      # Construct model
      m <- lm(get(y) ~ I(get(x)^2) + get(z), data = data)
      # Coefficients
      cf <- m %>% tidy() %>% mutate(term = c('Intercept', paste0(x, '^2'), z))
      # Fit
      ft <- glance(m)
      # Residuals
      rs <- augment(m) %>% rename(!!x := `I(get(x)^2)`, !!y := `get(y)`, !!z := `get(z)`)
      # Plot label
      p.lab <- bquote(atop(z==.(scientific(cf$estimate[2], digits = 3))~x^2+
                             .(round(cf$estimate[3], 3))~y+
                             .(round(cf$estimate[1], 1)),
                           R^2==.(round(ft$r.squared, 2))))
    } else {
      cat('Constructing linear model:\n', y, '~', x, '^2\n', sep = '')
      # Construct model
      m <- lm(get(y) ~ I(get(x)^2), data = data)
      # Coefficients
      cf <- m %>% tidy() %>% mutate(term = c('Intercept', paste0(x, '^2')))
      # Fit
      ft <- glance(m)
      # Residuals
      rs <- augment(m) %>% rename(!!x := `I(get(x)^2)`, !!y := `get(y)`)
      # Plot label
      p.lab <- bquote(atop(y==.(scientific(cf$estimate[2], digits = 3))~x^2+
                             .(round(cf$estimate[1], 1)),
                           R^2==.(round(ft$r.squared, 2))))
    }
  } else if(type=='quad2'){
    if(!is.null(z)){
      cat('Constructing linear model:\n', y, '~', x, '+', x, '^2+', z, '\n', sep = '')
      # Construct model
      m <- lm(get(y) ~ poly(get(x), 2) + get(z), data = data)
      # Coefficients
      cf <- m %>% tidy() %>% mutate(term = c('Intercept', x, paste0(x, '^2'), z))
      # Fit
      ft <- glance(m)
      # Residuals
      rs <- augment(m) %>% 
        rename(!!x := `poly(get(x), 2)`, !!y := `get(y)`, !!z := `get(z)`)
      # Plot label
      p.lab <- bquote(atop(z==.(round(cf$estimate[2], 1))~x+
                             .(round(cf$estimate[3], 1))~x^2+
                             .(round(cf$estimate[4], 3))~y+
                             .(round(cf$estimate[1], 1)),
                           R^2==.(round(ft$r.squared, 2))))
    } else {
      cat('Constructing linear model:\n', y, '~', x, '+', x, '^2\n', sep = '')
      # Construct model
      m <- lm(get(y) ~ poly(get(x), 2), data = data)
      # Coefficients
      cf <- m %>% tidy() %>% mutate(term = c('Intercept', x, paste0(x, '^2')))
      # Fit
      ft <- glance(m)
      # Residuals
      rs <- augment(m) %>% 
        rename(!!x := `poly(get(x), 2)`, !!y := `get(y)`)
      # Plot label
      p.lab <- bquote(atop(y==.(round(cf$estimate[3], 1))~x+
                             .(round(cf$estimate[2], 1))~x^2+
                             .(round(cf$estimate[1], 1)),
                           R^2==.(round(ft$r.squared, 2))))
    }
  }
  
  # ANOVA
  if(anova == T){
    cat('ANOVA test summary:\n')
    # Anova
    aov(get(y) ~ as.factor(get(x)), data = data) %>%
      tidy() %>%
      mutate(term = c(x, 'Residuals'))
    # Print
    print(m.anova)
    # Bartlet test for equal variance among groups
    cat('Bartlett test for equal variances among groups:\nNull hypothesis is that variance is the same among groups\npvalue > 0.05 means variances are equal')
    bartlett.test(get(y) ~ as.factor(get(x)), data = data) %>%
      tidy() %>% 
      print()
    # Tukey ad-hoc to test pairwise differences bw means
    cat('Pair-wise Tukey test for differences in means among groups:')
    m.anova <- aov(get(y) ~ as.factor(get(x)), data = data) %>% 
      TukeyHSD() %>%
      tidy() %>% 
      mutate('term' = x)
    print(m.anova)
  }
  
  # Print regression summary
  cat('Model summary:\n')
  print(cf)
  print(ft)
  
  if(plot==TRUE){
    cat('Saving plots to: ', file, '.png\n')
    # Plot
    if(!is.null(z)){
      grid <- expand.grid(x = seq(0, 150, length.out = 100),
                          y = seq(0, 150, length.out = 100),
                          stringsAsFactors = F) %>% 
        tibble() %>% 
        rename(!!x := x, !!z := y)
      p.grid <- grid %>%
        mutate(!!y := predict(m, newdata = grid))
      p <- p.grid %>% 
        ggplot(aes(y = get(x), x = get(z), z = get(y))) +
        geom_raster(aes(fill = get(y)), alpha = 0.5, show.legend = F) +
        geom_contour(aes(color = ..level..), size = 0.3, binwidth = 10, show.legend = F) +
        geom_point(data = data, aes(y = get(x), x = get(z)), size = 2) +
        scale_color_gradient(low = 'black', high = 'black') +
        scale_fill_viridis_c(option = 'D') +
        labs(x = x, y = z, color = y) +
        annotate('text',
                 x = -Inf,
                 y = -Inf,
                 hjust = -0.25,
                 vjust = -0.5,
                 label = p.lab,
                 size = 3) +
        theme_classic()
      p1 <- direct.label(p, list(fontface = 'plain', cex = 0.6, 'last.bumpup'))
    } else {
      p1 <- data %>% 
        ggplot() +
        geom_point(aes(x = get(x), y = get(y))) +
        geom_line(aes(x = get(x), y = predict(m))) +
        labs(x = x, y = y) +
        annotate('text',
                 x = -Inf,
                 y = Inf,
                 hjust = -0.1,
                 vjust = 1,
                 label = p.lab,
                 size = 3) +
        theme_classic()
    }
    # Check residuals
    p2 <- rs %>% 
      ggplot() +
      geom_histogram(aes(x = .std.resid), bins = 4, alpha = 0.3, color = 'black') +
      labs(x = 'Residuals', y = NULL) +
      theme_classic()
    
    p3 <- rs %>%
      ggplot() +
      geom_qq(aes(sample = .std.resid)) +
      geom_qq_line(aes(sample = .std.resid)) +
      theme_classic()
    
    # Composition
    p <- p1 + (p2/p3) +
      plot_annotation(tag_levels = 'a') +
      plot_layout(widths = c(2,1))
    
    # Save
    ggsave(filename = paste0(file, '.png'), plot = p, device = 'png', type = 'cairo', width = 7, height = 4)
  }
  if(anova == T){
    return(list(model = cf, fit = ft, anova = m.anova))
  } else{
    return(list(model = cf, fit = ft))
  }
}