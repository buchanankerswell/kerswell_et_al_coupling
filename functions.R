# Load Libraries
cat("Loading packages\n")
suppressMessages(lapply(c("progress", "metR", "knitr", "kableExtra", "directlabels",
  "patchwork", "scales", "ggplot2", "broom", "dplyr", "purrr", "readr", "tidyr", "forcats",
  "ggrepel"),
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }))
m.lin <- function(data, x, y, z = NULL, type = c("lin", "quad1", "quad2"), anova = F,
  plot = T, file = "lin_model_plot") {
  if (type == "lin") {
    if (!is.null(z)) {
      cat("Constructing linear model:\n", y, "~", x, "+", z, "\n", sep = "")
      # Construct model
      m <- lm(get(y) ~ get(x) + get(z), data = data)
      # Coefficients
      cf <- m %>% tidy() %>% mutate(term = c("Intercept", x, z))
      # Fit
      ft <- glance(m)
      # Residuals
      rs <- augment(m) %>% rename(`:=`(!!x, `get(x)`), `:=`(!!y, `get(y)`),
        `:=`(!!z, `get(z)`))
      # Plot label
      p.lab <- bquote(atop(z == .(scientific(cf$estimate[2], digits = 3)) ~
        x + .(round(cf$estimate[3], 3)) ~ y + .(round(cf$estimate[1], 1)),
        R^2 == .(round(ft$r.squared, 2))))
    } else {
      cat("Constructing linear model:\n", y, "~", x, "\n", sep = "")
      # Construct model
      m <- lm(get(y) ~ get(x), data = data)
      # Coefficients
      cf <- m %>% tidy() %>% mutate(term = c("Intercept", x))
      # Fit
      ft <- glance(m)
      # Residuals
      rs <- augment(m) %>% rename(`:=`(!!x, `get(x)`), `:=`(!!y, `get(y)`))
      # Plot label
      p.lab <- bquote(atop(y == .(scientific(cf$estimate[2], digits = 3)) ~
        x + .(round(cf$estimate[1], 1)), R^2 == .(round(ft$r.squared, 2))))
    }
  } else if (type == "quad1") {
    if (!is.null(z)) {
      cat("Constructing linear model:\n", y, "~", x, "^2+", z, "\n", sep = "")
      # Construct model
      m <- lm(get(y) ~ I(get(x)^2) + get(z), data = data)
      # Coefficients
      cf <- m %>% tidy() %>% mutate(term = c("Intercept", paste0(x, "^2"),
        z))
      # Fit
      ft <- glance(m)
      # Residuals
      rs <- augment(m) %>% rename(`:=`(!!x, `I(get(x)^2)`), `:=`(!!y, `get(y)`),
        `:=`(!!z, `get(z)`))
      # Plot label
      p.lab <- bquote(atop(z == .(scientific(cf$estimate[2], digits = 3)) ~
        x^2 + .(round(cf$estimate[3], 3)) ~ y + .(round(cf$estimate[1], 1)),
        R^2 == .(round(ft$r.squared, 2))))
    } else {
      cat("Constructing linear model:\n", y, "~", x, "^2\n", sep = "")
      # Construct model
      m <- lm(get(y) ~ I(get(x)^2), data = data)
      # Coefficients
      cf <- m %>% tidy() %>% mutate(term = c("Intercept", paste0(x, "^2")))
      # Fit
      ft <- glance(m)
      # Residuals
      rs <- augment(m) %>% rename(`:=`(!!x, `I(get(x)^2)`), `:=`(!!y, `get(y)`))
      # Plot label
      p.lab <- bquote(atop(y == .(scientific(cf$estimate[2], digits = 3)) ~
        x^2 + .(round(cf$estimate[1], 1)), R^2 == .(round(ft$r.squared, 2))))
    }
  } else if (type == "quad2") {
    if (!is.null(z)) {
      cat("Constructing linear model:\n", y, "~", x, "+", x, "^2+", z, "\n",
        sep = "")
      # Construct model
      m <- lm(get(y) ~ poly(get(x), 2, raw = T) + get(z), data = data)
      # Coefficients
      cf <- m %>% tidy() %>% mutate(term = c("Intercept", x, paste0(x, "^2"),
        z))
      # Fit
      ft <- glance(m)
      # Residuals
      rs <- augment(m) %>% rename(`:=`(!!x, names(.)[2]), `:=`(!!y, `get(y)`),
        `:=`(!!z, `get(z)`))
      # Plot label
      p.lab <- bquote(atop(z == .(round(cf$estimate[2], 1)) ~ x + .(scientific(cf$estimate[3],
        digits = 3)) ~ x^2 + .(round(cf$estimate[4], 3)) ~ y + .(round(cf$estimate[1],
        1)), R^2 == .(round(ft$r.squared, 2))))
    } else {
      cat("Constructing linear model:\n", y, "~", x, "+", x, "^2\n", sep = "")
      # Construct model
      m <- lm(get(y) ~ poly(get(x), 2, raw = T), data = data)
      # Coefficients
      cf <- m %>% tidy() %>% mutate(term = c("Intercept", x, paste0(x, "^2")))
      # Fit
      ft <- glance(m)
      # Residuals
      rs <- augment(m) %>% rename(`:=`(!!x, names(.)[2]), `:=`(!!y, `get(y)`))
      # Plot label
      p.lab <- bquote(atop(y == .(round(cf$estimate[3], 1)) ~ x + .(round(cf$estimate[2],
        1)) ~ x^2 + .(round(cf$estimate[1], 1)), R^2 == .(round(ft$r.squared,
        2))))
    }
  }

  # ANOVA
  if (anova == T) {
    cat("ANOVA test summary:\n")
    # Anova
    aov(get(y) ~ as.factor(get(x)), data = data) %>% tidy() %>% mutate(term = c(x,
      "Residuals")) %>% print()
    # Bartlet test for equal variance among groups
    cat("Bartlett test for equal variances among groups:\nNull hypothesis is that variance is the same among groups\npvalue > 0.05 means variances are equal")
    bartlett.test(get(y) ~ as.factor(get(x)), data = data) %>% tidy() %>% print()
    # Tukey ad-hoc to test pairwise differences bw means
    cat("Pair-wise Tukey test for differences in means among groups:")
    m.anova <- aov(get(y) ~ as.factor(get(x)), data = data) %>% TukeyHSD() %>%
      tidy() %>% mutate(term = x)
    print(m.anova)
  }

  # Print regression summary
  cat("Model summary:\n")
  print(cf)
  print(ft)

  if (plot == TRUE) {
    cat("Saving plots to: ", file, ".png\n")
    # Plot
    if (!is.null(z)) {
      grid <- expand.grid(x = seq(0, 150, length.out = 100), y = seq(0, 150,
        length.out = 100), stringsAsFactors = F) %>% tibble() %>% rename(`:=`(!!x,
        x), `:=`(!!z, y))
      p.grid <- grid %>% mutate(`:=`(!!y, predict(m, newdata = grid)))
      p <- p.grid %>% ggplot(aes(y = get(x), x = get(z), z = get(y))) + geom_raster(aes(fill = get(y)),
        alpha = 0.5, show.legend = F) + geom_contour(aes(color = ..level..),
        size = 0.3, binwidth = 10, show.legend = F) + geom_point(data = data,
        aes(y = get(x), x = get(z)), size = 2) + scale_color_gradient(low = "black",
        high = "black") + scale_fill_viridis_c(option = "D") + labs(x = x,
        y = z, color = y) + # annotate('text', x = -Inf, y = -Inf, hjust = -0.25, vjust = -0.5, label =
      # p.lab, size = 3) +
      theme_classic()
      p1 <- direct.label(p, list(fontface = "plain", cex = 0.6, "last.bumpup"))
    } else {
      p1 <- data %>% ggplot() + geom_point(aes(x = get(x), y = get(y))) + geom_line(aes(x = get(x),
        y = predict(m))) + labs(x = x, y = y) + # annotate('text', x = -Inf, y = Inf, hjust = -0.1, vjust = 1, label = p.lab,
      # size = 3) +
      theme_classic()
    }
    # Check residuals
    p2 <- rs %>% ggplot() + geom_histogram(aes(x = .std.resid), bins = 4, alpha = 0.3,
      color = "black") + labs(x = "Residuals", y = NULL) + theme_classic()

    p3 <- rs %>% ggplot() + geom_qq(aes(sample = .std.resid)) + geom_qq_line(aes(sample = .std.resid)) +
      theme_classic()

    # Composition
    p <- p1 + (p2/p3) + plot_annotation(tag_levels = "a") + plot_layout(widths = c(2,
      1))

    # Save
    ggsave(filename = paste0(file, ".png"), plot = p, device = "png", type = "cairo",
      width = 7, height = 4)
  }
  if (anova == T) {
    return(list(model = cf, fit = ft, anova = m.anova))
  } else {
    return(list(model = cf, fit = ft))
  }
}

# Read binary (.prn) files and save grids
read_nodes <- function(prn) {
  # Read binary, save grid Filename
  fname <- prn
  # Open connection
  f.prn <- file(fname, "rb")
  # Read sizes of variables
  readBin(f.prn, "integer", 4, 1, signed = F)
  # Read model parameters Grid resolution
  xnumx <- readBin(f.prn, "integer", 1, 8)
  znumz <- readBin(f.prn, "integer", 1, 8)
  # Markers per cell
  mnumx <- readBin(f.prn, "integer", 1, 8)
  mnumz <- readBin(f.prn, "integer", 1, 8)
  # Number of markers
  marknum <- readBin(f.prn, "integer", 1, 8)
  # Model sizes
  xsize <- readBin(f.prn, "numeric", 1, 8)
  zsize <- readBin(f.prn, "numeric", 1, 8)
  # Pressure value
  pinit <- readBin(f.prn, "numeric", 5, 8)
  # Gravity
  gx <- readBin(f.prn, "numeric", 1, 8)
  gz <- readBin(f.prn, "numeric", 1, 8)
  # Number of rocks
  rocknum <- readBin(f.prn, "integer", 1, 4)
  # Number of Boundary conditions
  boundnum <- readBin(f.prn, "integer", 1, 8)
  # Stage, time
  stg <- readBin(f.prn, "integer", 1, 4)
  timesum <- readBin(f.prn, "numeric", 1, 8)
  # Skip rock properties
  curpos <- 4 + 2 * 4 + 16 * 8 + rocknum * (8 * 24 + 4)
  seek(f.prn, curpos, "start")
  # Initialize Matrices
  pr <- matrix(NA, znumz, xnumx)
  vx <- matrix(NA, znumz, xnumx)
  vz <- matrix(NA, znumz, xnumx)
  exx <- matrix(NA, znumz, xnumx)
  ezz <- matrix(NA, znumz, xnumx)
  exz <- matrix(NA, znumz, xnumx)
  sxx <- matrix(NA, znumz, xnumx)
  szz <- matrix(NA, znumz, xnumx)
  sxz <- matrix(NA, znumz, xnumx)
  ro <- matrix(NA, znumz, xnumx)
  nu <- matrix(NA, znumz, xnumx)
  nd <- matrix(NA, znumz, xnumx)
  mu <- matrix(NA, znumz, xnumx)
  ep <- matrix(NA, znumz, xnumx)
  et <- matrix(NA, znumz, xnumx)
  pr0 <- matrix(NA, znumz, xnumx)
  prb <- matrix(NA, znumz, xnumx)
  dv <- matrix(NA, znumz, xnumx)
  tk <- matrix(NA, znumz, xnumx)
  cp <- matrix(NA, znumz, xnumx)
  kt <- matrix(NA, znumz, xnumx)
  ht <- matrix(NA, znumz, xnumx)
  eii <- matrix(1, znumz, xnumx) * 1e-16
  sii <- matrix(1, znumz, xnumx) * 10000
  dis <- matrix(1, znumz, xnumx) * 1e-10
  # Progress bar
  pb.nodes <- progress_bar$new(format = paste0("Reading Nodes [", stringr::str_extract(fname,
    "cd.[0-9]{2}.[0-9]+"), "] [:bar] :percent in: :elapsed"), total = xnumx *
    znumz, clear = FALSE, width = 60)
  # Read nodes information
  for (i in seq_len(xnumx)) {
    for (j in seq_len(znumz)) {
      vbuf <- readBin(f.prn, "numeric", 3, 4)
      pr[j, i] <- vbuf[1]
      vx[j, i] <- vbuf[2]
      vz[j, i] <- vbuf[3]
      vbuf1 <- readBin(f.prn, "integer", 3, 8)
      vbuf2 <- readBin(f.prn, "numeric", 16, 4)
      exx[j, i] <- vbuf2[1]
      ezz[j, i] <- vbuf2[2]
      exz[j, i] <- vbuf2[3]
      sxx[j, i] <- vbuf2[4]
      szz[j, i] <- vbuf2[5]
      sxz[j, i] <- vbuf2[6]
      ro[j, i] <- vbuf2[7]
      nu[j, i] <- vbuf2[8]
      nd[j, i] <- vbuf2[9]
      mu[j, i] <- vbuf2[10]
      ep[j, i] <- vbuf2[11]
      et[j, i] <- vbuf2[12]
      pr0[j, i] <- vbuf2[13]
      prb[j, i] <- vbuf2[14]
      dv[j, i] <- vbuf2[15]
      tk[j, i] <- vbuf2[16]
      vbuf3 <- readBin(f.prn, "integer", 1, 8)
      vbuf4 <- readBin(f.prn, "numeric", 3, 4)
      cp[j, i] <- vbuf4[1]
      kt[j, i] <- vbuf4[2]
      ht[j, i] <- vbuf4[3]
      pb.nodes$tick()
    }
  }
  # Skip all nodes
  curpos2 <- curpos + (4 * 22 + 8 * 4) * xnumx * znumz
  seek(f.prn, curpos2, "start")
  # Read gridline positions
  gx <- readBin(f.prn, "numeric", xnumx, 4)
  gz <- readBin(f.prn, "numeric", znumz, 4)
  # Close connection
  close(f.prn)
  # Progress bar
  pb.eii <- progress_bar$new(format = "Calc. Stress, Strain, & Shear Heating [:bar] :percent in: :elapsed",
    total = ((xnumx - 2) * (znumz - 2)), clear = FALSE, width = 60)
  # Calculate stress & strain
  for (i in seq_len(xnumx - 2)) {
    for (j in seq_len(znumz - 2)) {
      eii[j + 1, i + 1] = (exz[j + 1, i + 1]^2 + ((exx[j + 1, i + 1] + exx[j +
        2, i + 1] + exx[j + 1, i + 2] + exx[j + 2, i + 2])/4)^2)^0.5
      sii[j + 1, i + 1] = (sxz[j + 1, i + 1]^2 + ((sxx[j + 1, i + 1] + sxx[j +
        2, i + 1] + sxx[j + 1, i + 2] + sxx[j + 2, i + 2])/4)^2)^0.5
      dis[j+1,i+1]=(2*sxz[j+1,i+1]*exz[j+1,i+1]+2*((sxx[j+1,i+1]*exx[j+1,i+1]+sxx[j+2,i+1]*exx[j+2,i+1]+sxx[j+1,i+2]*exx[j+1,i+2]+sxx[j+2,i+2]*exx[j+2,i+2])/4))
      pb.eii$tick()
    }
  }
  # Save grid
  g <- purrr::map2(list(pr, vx, vz, exx, ezz, exz, eii, sxx, sxz, sii, dis, ro, nu,
    nd, mu, ep, et, pr0, prb, dv, tk, cp, kt, ht), c("pr", "vx", "vz", "exx",
    "ezz", "exz", "eii", "sxx", "sxz", "sii", "dis", "ro", "nu", "nd", "mu", "ep", "et",
    "pr0", "prb", "dv", "tk", "cp", "kt", "ht"), ~{
    rownames(.x) <- gz
    colnames(.x) <- gx
    .x %>% as_tibble(rownames = "z", .name_repair = ~vctrs::vec_as_names(...,
      repair = "unique", quiet = T)) %>% tidyr::pivot_longer(-z, names_to = "x",
      values_to = .y) %>% mutate(x = x %>% as.integer(), z = z %>% as.integer()) %>%
      mutate(z = z - 18000)
  }) %>% purrr::reduce(left_join, by = c("z", "x")) %>% mutate(w = c(0, diff(x)),
    .before = z)
  t <- timesum
  return(list(nodes = g, time = t))
}

# Read .txt files and save rock type grid
read_rock_nodes <- function(txt) {
  # Read binary, save grid Filename
  fname <- txt
  # Open connection and read file
  d <- scan(fname)
  # Time
  t <- d[1]
  # x nodes
  x.coord <- d[2]
  # z nodes
  z.coord <- d[3]
  # rock type
  r <- d[4:length(d)]
  # initialize colorgrid
  colors <- vector("numeric", z.coord * x.coord)
  # color map
  c.map <- tibble(type = seq_len(40), r = c(0.792, 0.50588, 1, 0.68235, 1, 0.75294,
    0.50196, 0, 0, 0, 0.3, 0.14118, 0, 0.9, 0.4, 0.8549, 0.95294, 0.35294, 0.1,
    0, 0, 0, 0, 1, 1, 0.46667, 0.50196, 0.72549, 0.82549, 0.6, 1, 0.99216, 0.84706,
    0.9, 0.8, 1, 0.6, 0.6, 0.1, 0.6), g = c(0.862, 0.99608, 0.50196, 0.34118, 0.50196,
    0.75294, 0.50196, 0.50196, 0.84314, 0, 0.3, 0.72157, 0.50196, 0.4, 0, 0.59608,
    0.20392, 0.16863, 0.6, 0, 0, 0, 0, 1, 0.90196, 0.46667, 0.50196, 0.015686,
    0.43922, 0, 0, 0.38824, 0.078431, 0.2, 0, 0.6, 0.4, 0.8, 0.8, 0.5), b = c(0.988,
    0.78824, 0, 0, 0, 0.75294, 0.50196, 0, 0, 0.71765, 0.9, 0.99216, 1, 1, 0,
    0.36078, 0.086275, 0.027451, 0, 0, 0, 0, 0, 0.31765, 0.18824, 0.23529, 0,
    0.78431, 0.99608, 0, 0, 0.30196, 0.15294, 0.2, 0, 0, 0, 0, 0, 0))
  num <- 1
  ind <- 1
  while (num < length(r)) {
    value <- r[num]
    if (value == -2) {
      n.color <- r[num + 1]
      mat <- r[num + 2]
      ind_vec <- seq(ind, ind + n.color - 1, 1)
      ind <- ind + n.color
      num <- num + 3
    } else {
      if (value == -1) {
        mat <- NA
      } else {
        mat <- value
      }
      ind_vec <- ind
      ind <- ind + 1
      num <- num + 1
    }
    colors[ind_vec] <- mat
  }
  color.grid <- expand.grid(z = seq_len(z.coord), x = seq_len(x.coord)) %>% tibble() %>%
    mutate(type = colors + 1) %>% filter(type >= 0 && type <= 40) %>% left_join(c.map,
    by = "type") %>% drop_na() %>% mutate(color = rgb(r, g, b, 1), z = z - 18)
  return(list(grid = color.grid, time = t))
}

draw_grid <- function(nodes = NULL, rocks = NULL, hf = NULL, time, box = c(up = -18,
  down = 200, left = 0, right = 2000), arrows = FALSE, leg.pos = "right", base.size = 11, p.type = c("rocks",
  "stress", "strain", "density", "temperature", "viscosity", "heat", "stream", "hf"), v.pal = "magma",
  v.direction = 1, transparent = TRUE) {
  n <- nodes
  r <- rocks
  h <- hf
  if (p.type == "rocks") {
    if (!is.null(r)) {
      p <- r %>% ggplot() + geom_raster(aes(x = x/2, y = z/2, fill = as.factor(type)),
        na.rm = T) + geom_contour(data = n, aes(x = x/1000, y = z/1000, z = tk -
        273), size = 0.15, color = "white", na.rm = T, breaks = c(0, seq(100,
        1900, 200))) + geom_text_contour(data = n, aes(x = x/1000, y = z/1000,
        z = tk - 273), stroke = 0.2, size = 3, breaks = c(0, seq(100, 1900,
        200))) + labs(x = "km", y = "km", fill = "Rock Type", title = paste0("Rock Type  ",
        time, " Ma")) + coord_equal(expand = F) + scale_y_reverse(limits = c(box[2],
        box[1])) + scale_x_continuous(limits = c(box[3], box[4])) + scale_fill_manual(values = unique(r$color)[as.factor(r$type) %>%
        unique() %>% order()], breaks = unique(r$type)[as.factor(r$type) %>%
        unique() %>% order()], na.value = 'white') + theme_minimal(base_size = base.size) + theme(legend.position = leg.pos,
        axis.text = element_text(color = "black"))
      if(arrows == TRUE){
        p <- p + geom_arrow(aes(x = x/1000, y = z/1000, dx = vx, dy = (-vz)), skip = 5, alpha = 0.3, show.legend = F) +
          geom_contour(aes(x = x/1000, y = z/1000, z = tk - 273),
                       size = 0.15,
                       color = "white",
                       na.rm = T,
                       breaks = c(0, seq(100, 1900, 200))) +
          geom_text_contour(aes(x = x/1000, y = z/1000, z = tk - 273), stroke = 0.2, size = 3, breaks = c(0, seq(100, 1900,200)))
      }
    }
  } else if (p.type == "temperature") {
    if (!is.null(n)) {
      p <- n %>% ggplot() + geom_contour_fill(aes(x = x/1000, y = z/1000, z = tk -
        273), size = 0.1, color = NA, breaks = c(0, seq(100, 1900, 200))) +
        geom_text_contour(aes(x = x/1000, y = z/1000, z = tk - 273), stroke = 0.2,
          size = 3, breaks = c(0, seq(100, 1900, 200))) + labs(x = "km",
        y = "km", fill = bquote(degree * C), title = paste0("Temperature  ",
          time, " Ma")) + coord_equal(expand = F) + scale_y_reverse(limits = c(box[2],
        box[1])) + scale_x_continuous(limits = c(box[3], box[4])) + scale_fill_viridis_c(option = v.pal,
        direction = v.direction, na.value = "transparent") + theme_minimal(base_size = base.size) +
        theme(legend.position = leg.pos, axis.text = element_text(color = "black"))
      if(arrows == TRUE){
        p <- p + geom_arrow(aes(x = x/1000, y = z/1000, dx = vx, dy = (-vz)), skip = 5, alpha = 0.3, show.legend = F) +
          geom_text_contour(aes(x = x/1000, y = z/1000, z = tk - 273), stroke = 0.2, size = 3, breaks = c(0, seq(100, 1900,200)))
      }
    }
  } else if (p.type == "stress") {
    if (!is.null(n)) {
      p <- n %>% ggplot() + geom_contour_fill(aes(x = x/1000, y = z/1000, z = log10(sii)),
        size = 0.1, color = NA) + geom_contour(aes(x = x/1000, y = z/1000,
        z = tk - 273), size = 0.15, color = "white", na.rm = T, breaks = c(0,
        seq(100, 1900, 200))) + geom_text_contour(aes(x = x/1000, y = z/1000,
        z = tk - 273), stroke = 0.2, size = 3, breaks = c(0, seq(100, 1900,
        200))) + labs(x = "km", y = "km", fill = bquote(Log(Pa)), title = paste0("Log Stress  ",
        time, " Ma")) + coord_equal(expand = F) + scale_y_reverse(limits = c(box[2],
        box[1])) + scale_x_continuous(limits = c(box[3], box[4])) + scale_fill_viridis_c(option = v.pal,
        direction = v.direction, na.value = "transparent") + theme_minimal(base_size = base.size) +
        theme(legend.position = leg.pos, axis.text = element_text(color = "black"))
      if(arrows == TRUE){
        p <- p + geom_arrow(aes(x = x/1000, y = z/1000, dx = vx, dy = (-vz)), skip = 5, alpha = 0.3, show.legend = F) +
          geom_contour(aes(x = x/1000, y = z/1000, z = tk - 273),
                       size = 0.15,
                       color = "white",
                       na.rm = T,
                       breaks = c(0, seq(100, 1900, 200))) +
          geom_text_contour(aes(x = x/1000, y = z/1000, z = tk - 273), stroke = 0.2, size = 3, breaks = c(0, seq(100, 1900,200)))
      }
    }
  } else if (p.type == "strain") {
    if (!is.null(n)) {
      p <- n %>% ggplot() + geom_contour_fill(aes(x = x/1000, y = z/1000, z = log10(eii)),
        size = 0.1, color = NA) + geom_contour(aes(x = x/1000, y = z/1000,
        z = tk - 273), size = 0.15, color = "white", na.rm = T, breaks = c(0,
        seq(100, 1900, 200))) + geom_text_contour(aes(x = x/1000, y = z/1000,
        z = tk - 273), stroke = 0.2, size = 3, breaks = c(0, seq(100, 1900,
        200))) + labs(x = "km", y = "km", fill = bquote(Log(s^-1)), title = paste0("Log Strain Rate  ",
        time, " Ma")) + coord_equal(expand = F) + scale_y_reverse(limits = c(box[2],
        box[1])) + scale_x_continuous(limits = c(box[3], box[4])) + scale_fill_viridis_c(option = v.pal,
        direction = v.direction, na.value = "transparent") + theme_minimal(base_size = base.size) +
        theme(legend.position = leg.pos, axis.text = element_text(color = "black"))
      if(arrows == TRUE){
        p <- p + geom_arrow(aes(x = x/1000, y = z/1000, dx = vx, dy = (-vz)), skip = 5, alpha = 0.3, show.legend = F) +
          geom_contour(aes(x = x/1000, y = z/1000, z = tk - 273),
                       size = 0.15,
                       color = "white",
                       na.rm = T,
                       breaks = c(0, seq(100, 1900, 200))) +
          geom_text_contour(aes(x = x/1000, y = z/1000, z = tk - 273), stroke = 0.2, size = 3, breaks = c(0, seq(100, 1900,200)))
      }
    }
  } else if (p.type == "density") {
    if (!is.null(n)) {
      p <- n %>% ggplot() + geom_contour_fill(aes(x = x/1000, y = z/1000, z = ro),
        size = 0.1, color = NA) + geom_contour(aes(x = x/1000, y = z/1000,
        z = tk - 273), size = 0.15, color = "white", na.rm = T, breaks = c(0,
        seq(100, 1900, 200))) + geom_text_contour(aes(x = x/1000, y = z/1000,
        z = tk - 273), stroke = 0.2, size = 3, breaks = c(0, seq(100, 1900,
        200))) + labs(x = "km", y = "km", fill = bquote(kg ~ m^-3), title = paste0("Density  ",
        time, " Ma")) + coord_equal(expand = F) + scale_y_reverse(limits = c(box[2],
        box[1])) + scale_x_continuous(limits = c(box[3], box[4])) + scale_fill_viridis_c(option = v.pal,
        direction = v.direction, na.value = "transparent") + theme_minimal(base_size = base.size) +
        theme(legend.position = leg.pos, axis.text = element_text(color = "black"))
      if(arrows == TRUE){
        p <- p + geom_arrow(aes(x = x/1000, y = z/1000, dx = vx, dy = (-vz)), skip = 5, alpha = 0.3, show.legend = F) +
          geom_contour(aes(x = x/1000, y = z/1000, z = tk - 273),
                       size = 0.15,
                       color = "white",
                       na.rm = T,
                       breaks = c(0, seq(100, 1900, 200))) +
          geom_text_contour(aes(x = x/1000, y = z/1000, z = tk - 273), stroke = 0.2, size = 3, breaks = c(0, seq(100, 1900,200)))
      }
    }
  } else if (p.type == "viscosity") {
    if (!is.null(n)) {
      p <- n %>% ggplot() + geom_contour_fill(aes(x = x/1000, y = z/1000, z = log10(nu)),
        size = 0.1, color = NA) + geom_contour(aes(x = x/1000, y = z/1000,
        z = tk - 273), size = 0.15, color = "white", na.rm = T, breaks = c(0,
        seq(100, 1900, 200))) + geom_text_contour(aes(x = x/1000, y = z/1000,
        z = tk - 273), stroke = 0.2, size = 3, breaks = c(0, seq(100, 1900,
        200))) + labs(x = "km", y = "km", fill = bquote(log(Pa %.% s)), title = paste0("Log Viscosity  ",
        time, " Ma")) + coord_equal(expand = F) + scale_y_reverse(limits = c(box[2],
        box[1])) + scale_x_continuous(limits = c(box[3], box[4])) + scale_fill_viridis_c(option = v.pal,
        direction = v.direction, na.value = "transparent") + theme_minimal(base_size = base.size) +
        theme(legend.position = leg.pos, axis.text = element_text(color = "black"))
      if(arrows == TRUE){
        p <- p + geom_arrow(aes(x = x/1000, y = z/1000, dx = vx, dy = (-vz)), skip = 5, alpha = 0.3, show.legend = F) +
          geom_contour(aes(x = x/1000, y = z/1000, z = tk - 273),
                       size = 0.15,
                       color = "white",
                       na.rm = T,
                       breaks = c(0, seq(100, 1900, 200))) +
          geom_text_contour(aes(x = x/1000, y = z/1000, z = tk - 273), stroke = 0.2, size = 3, breaks = c(0, seq(100, 1900,200)))
      }
    }
  } else if(p.type == 'heat') {
    if (!is.null(n)) {
      p <-
        n %>% ggplot() + geom_contour_fill(aes(
          x = x / 1000,
          y = z / 1000,
          z = ht
        ),
        size = 0.1,
        color = NA) + geom_contour(
          aes(
            x = x / 1000,
            y = z / 1000,
            z = tk - 273
          ),
          size = 0.15,
          color = "white",
          na.rm = T,
          breaks = c(0,
                     seq(100, 1900, 200))
        ) + geom_text_contour(
          aes(
            x = x / 1000,
            y = z / 1000,
            z = tk - 273
          ),
          stroke = 0.2,
          size = 3,
          breaks = c(0, seq(100, 1900,
                            200))
        ) + labs(
          x = "km",
          y = "km",
          fill = bquote(Wm^-3),
          title = paste0("Heat Sources  ",
                         time, " Ma")
        ) + coord_equal(expand = F) + scale_y_reverse(limits = c(box[2],
                                                                 box[1])) + scale_x_continuous(limits = c(box[3], box[4])) + scale_fill_viridis_c(option = v.pal,
                                                                                                                                                  direction = v.direction,
                                                                                                                                                  na.value = "transparent") + theme_minimal(base_size = base.size) +
        theme(legend.position = leg.pos,
              axis.text = element_text(color = "black"))
      if(arrows == TRUE){
        p <- p + geom_arrow(aes(x = x/1000, y = z/1000, dx = vx, dy = (-vz)), skip = 5, alpha = 0.3, show.legend = F) +
          geom_contour(aes(x = x/1000, y = z/1000, z = tk - 273),
                       size = 0.15,
                       color = "white",
                       na.rm = T,
                       breaks = c(0, seq(100, 1900, 200))) +
          geom_text_contour(aes(x = x/1000, y = z/1000, z = tk - 273), stroke = 0.2, size = 3, breaks = c(0, seq(100, 1900,200)))
      }
    }
  } else if (p.type == "stream") {
    if (!is.null(n)) {
      p <- n %>% ggplot() +
        geom_contour_fill(
          aes(
            x = x / 1000,
            y = z / 1000,
            z = tk -
              273
          ),
          size = 0.1,
          color = 'white',
          alpha = 0.5,
          breaks = c(0, seq(100, 1900, 200)),
          show.legend = F
        ) +
        geom_streamline(
          aes(
            x = x / 1000,
            y = z / 1000,
            dx = vx,
            dy = (-vz),
            color = sqrt(..dx.. ^ 2 +
                           ..dy.. ^ 2) * 31540000 * 100,
            alpha = ..step..
          ),
          S = 5,
          dt = 31540000 *
            1000 / 5,
          arrow = NULL,
          L = 10,
          res = 1,
          skip = 5,
          lineend = "round",
          size = 0.3
        ) + geom_contour(
          aes(
            x = x / 1000,
            y = z / 1000,
            z = tk -
              273
          ),
          size = 0.15,
          color = "white",
          na.rm = T,
          breaks = c(0, seq(100,
                            1900, 200))
        ) + geom_text_contour(
          aes(
            x = x / 1000,
            y = z / 1000,
            z = tk -
              273
          ),
          stroke = 0.2,
          size = 3,
          breaks = c(0, seq(100, 1900, 200))
        ) +
        labs(
          x = "km",
          y = "km",
          color = bquote(cm ~ yr ^ -1),
          title = paste0("Streams  ",
                         time, " Ma")
        ) + guides(alpha = F, fill = F) + coord_equal(expand = F) +
        scale_y_reverse(limits = c(box[2], box[1])) + scale_x_continuous(limits = c(box[3],
                                                                                    box[4])) + scale_fill_gradient(low = "grey90", high = "grey10") +
        scale_color_viridis_c(option = v.pal,
                              direction = v.direction,
                              na.value = "transparent") +
        theme_minimal(base_size = base.size) + theme(legend.position = leg.pos, panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                     axis.text = element_text(color = "black"))
    }
  } else if(p.type == 'shear'){
    if (!is.null(n)) {
      p <- n %>%
        ggplot() +
        geom_contour_fill(aes(x = x/1000, y = z/1000, z = log10(dis)), size = 0.1, color = NA) +
        geom_contour(aes(x = x/1000, y = z/1000, z = tk - 273),
                     size = 0.15,
                     color = "white",
                     na.rm = T,
                     breaks = c(0, seq(100, 1900, 200))) +
        geom_text_contour(aes(x = x/1000, y = z/1000, z = tk - 273), stroke = 0.2, size = 3, breaks = c(0, seq(100, 1900,200))) +
        labs(x = "km", y = "km", fill = bquote(log(Wm^-3)), title = paste0("Log Shear Heating  ", time, " Ma")) +
        coord_equal(expand = F) + scale_y_reverse(limits = c(box[2], box[1])) +
        scale_x_continuous(limits = c(box[3], box[4])) +
        scale_fill_viridis_c(option = v.pal, direction = v.direction, na.value = "transparent") +
        theme_minimal(base_size = base.size) +
        theme(legend.position = leg.pos, axis.text = element_text(color = "black"))
      if(arrows == TRUE){
        p <- p + geom_arrow(aes(x = x/1000, y = z/1000, dx = vx, dy = (-vz)), skip = 5, alpha = 0.3, show.legend = F) +
          geom_contour(aes(x = x/1000, y = z/1000, z = tk - 273),
                       size = 0.15,
                       color = "white",
                       na.rm = T,
                       breaks = c(0, seq(100, 1900, 200))) +
          geom_text_contour(aes(x = x/1000, y = z/1000, z = tk - 273), stroke = 0.2, size = 3, breaks = c(0, seq(100, 1900,200)))
      }
    }
  } else if (p.type == "hf") {
    if (!is.null(h)) {
      p <- h %>% ggplot() + geom_point(aes(x = x/1000, y = qs * 1000), size = 0.5,
        alpha = 0.5, na.rm = T) + geom_line(aes(x = x/1000, y = qs * 1000),
        na.rm = T) + labs(x = "km", y = bquote(Heat ~ Flow ~ ~mWm^-2)) +
        scale_y_continuous(limits = c(0, 200)) + scale_x_continuous(limits = c(box[3],
        box[4]), expand = c(0, 0)) + theme_classic(base_size = base.size) +
        theme(axis.text = element_text(color = "black"))
    }
  }
  if (transparent == TRUE) {
    p <- p + theme(plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA), legend.background = element_rect(fill = "transparent",
        color = NA))
  }
  return(p)
}

plot_grid <- function(nodes = NULL, rocks = NULL, hf = NULL, rock.rows = 3, n.row = NULL, n.col = 1, leg.pos = "bottom",
  leg.collect = TRUE, leg.box = "horizontal", leg.dir = "vertical", leg.title.pos = "top", leg.title.vjust = 0.5,
  leg.title.hjust = 0.5, transparent = TRUE, tag = TRUE) {
  # Nodes plots
  if (!is.null(nodes)) {
    if ("list" %in% class(nodes)) {
      p.nodes <- purrr::map(nodes, ~{
        .x + guides(fill = guide_colorbar(title.position = leg.title.pos,
          title.hjust = leg.title.hjust, title.vjust = leg.title.vjust)) +
          theme(axis.text.x = element_blank(), axis.title.x = element_blank())
      }) %>% set_names(names(nodes))
    } else {
      p.nodes <- nodes + guides(fill = guide_colorbar(title.position = leg.title.pos,
        title.hjust = leg.title.hjust, title.vjust = leg.title.vjust)) +
        theme(axis.text.x = element_blank(), axis.title.x = element_blank())
    }
  } else {
    p.nodes <- NULL
  }
  # Rocks plot
  if (!is.null(rocks)) {
    p.rocks <- rocks + guides(fill = guide_legend(nrow = rock.rows, title.position = leg.title.pos,
      title.hjust = leg.title.hjust, title.vjust = leg.title.vjust)) + theme(axis.text.x = element_blank(),
      axis.title.x = element_blank())
  } else {
    p.rocks <- NULL
  }
  # Heat flow plot
  if (!is.null(hf)) {
    p.hf <- hf
  } else {
    p.hf <- NULL
  }
  # Wrap plots and gather legends
  if('gg' %in% class(p.nodes)){p.nodes <- list(p.nodes)}
  if(tag == TRUE){
    p <-
      wrap_plots(list(hf = p.hf, rocks = p.rocks) %>% append(p.nodes) %>% compact()) + theme(axis.text.x = element_text(color = "black"),
                                                                                             axis.title.x = element_text(color = "black")) + plot_layout(
                                                                                               ncol = n.col,
                                                                                               nrow = n.row,
                                                                                               guides = "collect",
                                                                                               heights = 1,
                                                                                               widths = 1
                                                                                             ) + plot_annotation(tag_levels = 'a',
                                                                                                                 theme = theme(plot.margin = margin(0,
                                                                                                                                                    10, 0, 10))) &
      theme(
        legend.position = leg.pos,
        legend.box = leg.box,
        legend.direction = leg.dir,
        plot.title = element_text(hjust = 1)
      )
  } else {
    p <- wrap_plots(list(hf = p.hf, rocks = p.rocks) %>% append(p.nodes) %>% compact()) + theme(axis.text.x = element_text(color = "black"),
                                                                                                axis.title.x = element_text(color = "black")) + plot_annotation(theme = theme(plot.margin = margin(0,
                                                                                                                                                                                                                                                            10, 0, 10))) & theme(legend.position = leg.pos, legend.box = leg.box, legend.direction = leg.dir,
                                                                                                                                                                                                                                                                                 plot.title = element_text(hjust = 1))
    if(leg.collect == T){
      p <- p + plot_layout(ncol = n.col, nrow = n.row, guides = "collect",
                           heights = 1, widths = 1)
    } else {
      p <- p + plot_layout(ncol = n.col, nrow = n.row, guides = 'keep',
                         heights = 1, widths = 1)
    }
  }
  # Transparent background
  if (transparent == TRUE) {
    p <- p &
      theme(plot.background = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = "transparent", color = NA), panel.background = element_rect(fill = "transparent",
        color = NA))
  }
  return(p)
}
