library(shiny)
library(shinythemes)
library(tidyverse)
library("PerformanceAnalytics")
library(cowplot)
library(viridis)
library(directlabels)
library(ggrepel)
load('regression_data.RData')
load('reduced_coupling_data.RData')
# Define UI ----
ui <- fluidPage(
    theme = shinytheme('yeti'),
    withMathJax(),
    # Application title
    titlePanel("Predicting Coupling Depth"),
    fluidRow(
        column(width = 4,
               tabsetPanel(
                   tabPanel(
                       'Segments',
                       selectInput(
                           'segment',
                           label = 'Select Specific Segment',
                           choices = c(
                               'NA',
                               'N. Cascadia',
                               'Nankai',
                               'Mexico',
                               'Columbia-Ecuador',
                               'SC Chile',
                               'Kyushu',
                               'N. Sumatra',
                               'Alaska',
                               'N. Chile',
                               'N. Costa Rica',
                               'Aleutians',
                               'N. Hikurangi',
                               'Mariana',
                               'Kermadec',
                               'Kamchatka',
                               'Izu',
                               'NE Japan',
                               'All'
                           )
                       )
                   ),
                   tabPanel(
                       'Custom',
                       helpText('Rock Thermal Properties'),
                       numericInput(
                           'kcrust',
                           label = 'Thermal Conductivity (crust) \\(W/mK\\)',
                           min = 0,
                           max = 5,
                           step = 0.1,
                           value = 2.5
                       ),
                       numericInput(
                           'kmantle',
                           label = 'Thermal Conductivity (mantle) \\(W/mK\\)',
                           min = 0,
                           max = 5,
                           step = 0.1,
                           value = 3.1
                       ),
                       numericInput(
                           'radcrust',
                           label = 'Radiogenic heat (crust) \\(µW/m^3\\)',
                           min = 0,
                           max = 1.5,
                           step = 0.01,
                           value = 1.3
                       ),
                       numericInput(
                           'radmantle',
                           label = 'Radiogenic heat (mantle) \\(µW/m^3\\)',
                           min = 0,
                           max = 1.3,
                           step = 0.01,
                           value = 0.02
                       ),
                       helpText('Subduction Zone Parameters'),
                       numericInput('qs', label = 'Backarc Surface Heat Flow \\(mW/m^3\\)', value = 65),
                       numericInput('zcrust', label = 'Crustal thickness \\(km\\)', value = 35),
                       numericInput('inTparam', label = 'Thermal Parameter \\(km/100\\)', value = 50),
                       actionButton('reset', 'Reset to Defaults')
                   ),
                   tabPanel(
                       'Plot Download',
                       helpText('Configure Plot'),
                       selectInput(
                           'select_plot',
                           'Select Plot for Download',
                           choices = c('Geotherm',
                                       'Predicted Coupling Depth')
                       ),
                       textInput('filename', label = 'Filename', placeholder = 'Plot1'),
                       numericInput(
                           'xlim',
                           'x-axis limit',
                           min = 500,
                           max = 2000,
                           step = 100,
                           value = 1300
                       ),
                       numericInput(
                           'ylim',
                           'y-axis limit',
                           min = 10,
                           max = 300,
                           step = 10,
                           value = 150
                       ),
                       numericInput(
                           'pdf_width',
                           'Plot width (inches)',
                           min = 5.5,
                           max = 17,
                           step = 0.25,
                           value = 5.5
                       ),
                       numericInput(
                           'pdf_height',
                           'Plot height (inches)',
                           min = 4.25,
                           max = 11,
                           step = 0.25,
                           value = 4.25
                       ),
                       downloadButton('dlplot', 'Download Plot')
                   )
               )),
        column(width = 8,
               fluidRow(
                   column(width = 6,
                          plotOutput("plot1")),
                   column(width = 6,
                          plotOutput(
                              "plot2",
                              brush = brushOpts(id = "plot2_brush", resetOnNew = FALSE),
                              dblclick = "plot2_dblclick",
                          ))
               ),
               fluidRow(column(
                   width = 12,
                   verbatimTextOutput('info')
               )))
    )
)

# Define server  ----
server <- function(input, output, session) {
    vals <- reactiveValues()
    vals$df <- NULL
    vals$qs <- NULL
    vals$kcrust <- NULL
    vals$kmantle <- NULL
    vals$radcrust <- NULL
    vals$radmantle <- NULL
    vals$zcrust <- NULL
    vals$z1100 <- NULL
    vals$zcouple <- NULL
    vals$plot2x <- c(0,150)
    vals$plot2y <- c(0,150)
    observe({
        vals$qs <- input$qs / 1e3
        vals$kcrust <- input$kcrust
        vals$kmantle <- input$kmantle
        vals$radcrust <- input$radcrust / 1e6
        vals$radmantle <- input$radmantle / 1e6
        vals$zcrust <- input$zcrust * 1e3
        # Geotherm model params
        surfacetemp <- 273.15
        zsize <- 300000
        dz <- 100
        znum <- (zsize / dz) + 1
        z <- seq.int(0, zsize, dz)
        T <- rep.int(0, znum)
        # Layer 1 (crust)
        A1 <- vals$radcrust
        k1 <- vals$kcrust
        D1 <- vals$zcrust
        # Layer 2 (lithospheric mantle)
        A2 <- vals$radmantle
        k2 <- vals$kmantle
        D2 <- 300000 - input$zcrust
        # Heat flow at the top of each layer
        qt1 <- vals$qs
        qt2 <- vals$qs - (A1 * D1)
        # Temp at the top of each layer
        Tt1 <- surfacetemp
        Tt2 <- Tt1 + (qt1 * D1 / k1) - (A1 / 2 / k1 * D1 ^ 2)
        #Calculate T within each layer
        for (i in 1:length(z)) {
            if (z[i] <= D1) {
                T[i] <- Tt1 + (qt1 / k1 * z[i]) - (A1 / 2 / k1 * z[i] ^ 2)
            }
            else if (z[i] > D1 & z[i] < D2 + D1) {
                T[i] <-
                    Tt2 + (qt2 / k2 * (z[i] - D1)) - (A2 / 2 / k2 * (z[i] - D1) ^ 2)
            }
        }
        vals$df <- data.frame('depth' = z, 'T' = T - 273.15)
        vals$z1100 <-
            vals$df$depth[which.min(abs(vals$df$T - 1100))] / 1000
        if (input$segment == 'N. Cascadia') {
            updateNumericInput(session, 'qs', value = 75)
            updateNumericInput(session, 'zcrust', value = 35)
            updateNumericInput(session, 'inTparam', value = 3.4)
        }
        if (input$segment == 'Nankai') {
            updateNumericInput(session, 'qs', value = 69)
            updateNumericInput(session, 'zcrust', value = 35)
            updateNumericInput(session, 'inTparam', value = 6.9)
        }
        if (input$segment == 'Mexico') {
            updateNumericInput(session, 'qs', value = 72)
            updateNumericInput(session, 'zcrust', value = 40)
            updateNumericInput(session, 'inTparam', value = 7.2)
        }
        if (input$segment == 'Columbia-Ecuador') {
            updateNumericInput(session, 'qs', value = 80)
            updateNumericInput(session, 'zcrust', value = 40)
            updateNumericInput(session, 'inTparam', value = 10.4)
        }
        if (input$segment == 'SC Chile') {
            updateNumericInput(session, 'qs', value = 80)
            updateNumericInput(session, 'zcrust', value = 40)
            updateNumericInput(session, 'inTparam', value = 20.0)
        }
        if (input$segment == 'Kyushu') {
            updateNumericInput(session, 'qs', value = 69)
            updateNumericInput(session, 'zcrust', value = 30)
            updateNumericInput(session, 'inTparam', value = 13.5)
        }
        if (input$segment == 'N. Sumatra') {
            updateNumericInput(session, 'qs', value = 120)
            updateNumericInput(session, 'zcrust', value = 30)
            updateNumericInput(session, 'inTparam', value = 25.0)
        }
        if (input$segment == 'Alaska') {
            updateNumericInput(session, 'qs', value = 80)
            updateNumericInput(session, 'zcrust', value = 40)
            updateNumericInput(session, 'inTparam', value = 25.3)
        }
        if (input$segment == 'N. Chile') {
            updateNumericInput(session, 'qs', value = 85)
            updateNumericInput(session, 'zcrust', value = 60)
            updateNumericInput(session, 'inTparam', value = 38.4)
        }
        if (input$segment == 'N. Costa Rica') {
            updateNumericInput(session, 'qs', value = 80)
            updateNumericInput(session, 'zcrust', value = 30)
            updateNumericInput(session, 'inTparam', value = 20.4)
        }
        if (input$segment == 'Aleutians') {
            updateNumericInput(session, 'qs', value = 75)
            updateNumericInput(session, 'zcrust', value = 10)
            updateNumericInput(session, 'inTparam', value = 39.6)
        }
        if (input$segment == 'N. Hikurangi') {
            updateNumericInput(session, 'qs', value = 80)
            updateNumericInput(session, 'zcrust', value = 30)
            updateNumericInput(session, 'inTparam', value = 41.0)
        }
        if (input$segment == 'Mariana') {
            updateNumericInput(session, 'qs', value = 80)
            updateNumericInput(session, 'zcrust', value = 10)
            updateNumericInput(session, 'inTparam', value = 54.6)
        }
        if (input$segment == 'Kermadec') {
            updateNumericInput(session, 'qs', value = 80)
            updateNumericInput(session, 'zcrust', value = 10)
            updateNumericInput(session, 'inTparam', value = 60.0)
        }
        if (input$segment == 'Kamchatka') {
            updateNumericInput(session, 'qs', value = 70)
            updateNumericInput(session, 'zcrust', value = 30)
            updateNumericInput(session, 'inTparam', value = 77.0)
        }
        if (input$segment == 'Izu') {
            updateNumericInput(session, 'qs', value = 80)
            updateNumericInput(session, 'zcrust', value = 10)
            updateNumericInput(session, 'inTparam', value = 77.0)
        }
        if (input$segment == 'NE Japan') {
            updateNumericInput(session, 'qs', value = 88)
            updateNumericInput(session, 'zcrust', value = 30)
            updateNumericInput(session, 'inTparam', value = 107.9)
        }
    })
    observeEvent(input$plot2_dblclick, {
        brush <- input$plot2_brush
        if (!is.null(brush)) {
            vals$plot2x <- c(brush$xmin, brush$xmax)
            vals$plot2y <- c(brush$ymin, brush$ymax)
        } else {
            vals$plot2x <- c(0,150)
            vals$plot2y <- c(0,150)
        }
    })
    observeEvent(input$reset, {
        updateNumericInput(session, 'kcrust', value = 2.5)
        updateNumericInput(session, 'kmantle', value = 3.1)
        updateNumericInput(session, 'radcrust', value = 1.3)
        updateNumericInput(session, 'radmantle', value = 0.02)
        updateNumericInput(session, 'zcrust', value = 35)
        updateNumericInput(session, 'inTparam', value = 50)
        updateNumericInput(session, 'qs', value = 80)
    })
    plot1 <- reactive({
        req(vals$qs)
        p1 <- ggplot(data = vals$df) +
            geom_vline(xintercept = 1100, color = 'grey') +
            annotate(
                'text',
                x = 1075,
                y = 250,
                size = 4,
                color = 'grey',
                label = expression(z[1100]),
                angle = 90
            ) +
            theme_bw(base_size = 16) +
            geom_hline(yintercept = input$zcrust, color = 'peru') +
            annotate(
                'text',
                x = 1000,
                y = input$zcrust - 3,
                size = 4,
                label = 'crust',
                color = 'peru'
            ) +
            geom_path(aes(x = T, y = depth / 1000),
                      size = 1.5,
                      color = 'deeppink4') +
            xlim(0, input$xlim) +
            ylim(input$ylim, 0) +
            xlab(expression('Temperature' ~ (degree * C))) +
            ylab('Depth (km)') +
            theme(
                axis.ticks.length = unit(-0.25, "cm"),
                axis.text.x = element_text(margin = unit(c(0.5, 0, 0, 0), "cm")),
                axis.text.y = element_text(margin = unit(c(0, 0.5, 0, 0), "cm")),
                panel.border = element_rect(size = 2, color = 'black'),
                axis.text = element_text(face = 'plain', color = 'black'),
                axis.ticks = element_line(size = 1, color = 'black'),
                legend.position = 'none',
                axis.title = element_text(size = 14, face = 'plain')
            )
        pbase <-
            if (abs(vals$df$T[which.min(abs(vals$df$T - 1100))] - 1100) < 10) {
                list(
                    geom_hline(yintercept = vals$z1100),
                    annotate(
                        'text',
                        x = 500,
                        y = vals$z1100 - 0.5,
                        size = 4,
                        label = paste0('Lithospheric Thickness\n', vals$z1100)
                    )
                )
            }
        else {
            pbase <- NULL
        }
        return(p1 + pbase)
    })
    plot2 <- reactive({
        req(vals$qs)
        #### Countour plot ######
        p2 <-
            ggplot(data = cubicContourData, aes(x = tparam, y = z1100, z = z.coupling)) +
            geom_contour(aes(color = ..level..),
                         size = 0.5,
                         binwidth = 10) +
            xlab(expression(paste('Slab Thermal Parameter ', Phi, '/100 (km/100)'))) +
            ylab(expression(paste('Lithospheric Thickness, ', z[1100], ' (km)'))) +
            scale_color_gradient(low = 'grey', high = 'black') +
            coord_cartesian(xlim = vals$plot2x, ylim = vals$plot2y) +
            theme_bw(base_size = 14) +
            theme(
                axis.ticks.length = unit(-0.25, "cm"),
                axis.text.x = element_text(margin = unit(c(0.5, 0, 0, 0), "cm")),
                axis.text.y = element_text(margin = unit(c(0, 0.5, 0, 0), "cm")),
                panel.border = element_rect(size = 2, color = 'black'),
                axis.text = element_text(face = 'plain', color = 'black'),
                axis.ticks = element_line(size = 1, color = 'black'),
                legend.position = 'none',
                axis.title = element_text(size = 12, face = 'plain')
            )
        dl.config <<- list('calc.boxes', box.color = 'white', 'draw.rects')
        p2 <-
            direct.label(p2, list(fontface = 'plain', cex = 0.8, 'top.pieces', vjust=0.5, 'dl.config'))
        if (!is.null(input$inTparam)) {
            req(vals$z1100)
            if (input$segment == 'All') {
                pcouple <- list(
                    geom_point(
                        data = segs,
                        aes(x = tparam, y = z1100),
                        size = 2,
                        color = 'deeppink4'
                    ),
                    geom_text_repel(data = segs,
                               aes(
                                   x = tparam,
                                   y = z1100,
                                   label = segment
                               ),
                               nudge_y = 4)
                )
            }
            else {
                vals$zcouple <- round(
                    (0.00495 * vals$z1100 ^ 2) + (-0.0927 * input$inTparam) +
                        63.6
                    ,
                    1
                )
                dfpoint <-
                    data.frame(
                        'tparam' = input$inTparam,
                        'z1100' = vals$z1100,
                        'z.coupling' = vals$zcouple
                    )
                pcouple <- list(
                    geom_point(
                        data = dfpoint,
                        aes(x = tparam, y = z1100),
                        color = 'deeppink4',
                        size = 2
                    ),
                    annotate(
                        'text',
                        x = dfpoint$tparam,
                        y = dfpoint$z1100 - 5,
                        label = round(dfpoint$z.coupling),
                        size = 4
                    )
                )
            }
        }
        return(p2 + pcouple)
    })
    info <- eventReactive(plot2(), {
        if(input$segment != 'All'){
            paste0(
                "Lithospheric Thickness: ",
                value = vals$z1100,
                ' | Thermal Parameter: ',
                input$inTparam,
                ' | Coupling Depth: ',
                vals$zcouple
            )
        }
        else{
            return(NULL)
        }
        
    })
    output$info <- renderText({
        info()
    })
    observe({
        output$dlplot <-
            if (input$select_plot == 'Geotherm') {
                downloadHandler(filename <-
                                    function() {
                                        paste0(input$filename, '.pdf')
                                    }, content <-
                                    function(filename) {
                                        ggsave(
                                            filename,
                                            plot = plot1(),
                                            device = pdf(
                                                width = input$pdf_width,
                                                height = input$pdf_height
                                            )
                                        )
                                    })
            }
        else if (input$select_plot == 'Predicted Coupling Depth') {
            downloadHandler(filename <-
                                function() {
                                    paste0(input$filename, '.pdf')
                                }, content <-
                                function(filename) {
                                    ggsave(
                                        filename,
                                        plot = plot2(),
                                        device = pdf(
                                            width = input$pdf_width,
                                            height = input$pdf_height
                                        )
                                    )
                                })
        }
    })
    output$plot1 <- renderPlot({
        plot1_debounce <- plot1 %>% debounce(0)
        plot1_debounce()
    })
    output$plot2 <- renderPlot({
        plot2_debounce <- plot2 %>% debounce(150)
        plot2_debounce()
    })
}

# Run the application
shinyApp(ui = ui, server = server)
