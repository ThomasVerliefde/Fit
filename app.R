# Title: Fit
# Author: Thomas Verliefde
# Date: 2018/05/09
# Version: 1.0

# Checks whether packages are installed, and installs the ones which are not.
# It should not reinstall already installed packages.
# Note that it does not check for version compatibility.
list.of.packages = c("shiny","tidyverse","shinyjs","magrittr",'cowplot');new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])];if(length(new.packages)){install.packages(new.packages,repos="http://cran.us.r-project.org")};lapply(list.of.packages,require,character.only=T);rm(list.of.packages,new.packages)

shinyApp(
  
  ui = fluidPage(
    fluidRow(
      column(
        2,
        actionButton(
          style="margin-top:18px;margin-left:10px;",
          'refresh','Resample',icon('refresh'))
      ),
      column(
        8,
        HTML('<h1> <div align="center"> Generalizability of Fit <div></h1>')
      ),
      column(
        2
      )
    ),
    fluidRow(
      column(
        12,
        div(
          style = "height:100%;",
          plotOutput('plot',height = '800px')
        )
      )
    )
  ),
  
  server = function(input, output, session) {
    
    
    trueDist = . %>% poly(2,raw=T,simple=T) %>% multiply_by_matrix(matrix(1,2))
    xmin = -2
    xmax = 2
    ymargin = 2
    n = 15
    iter = 2
    ymin = seq(xmin,xmax,length.out=101) %>%
      trueDist(.) %>% min %>% subtract(ymargin) %>% floor
    ymax = seq(xmin,xmax,length.out=101) %>%
      trueDist(.) %>% max %>% add(ymargin) %>% ceiling
    degrees = c(1,2,8)
    
    data = eventReactive(
      input$refresh,
      {replicate(
        iter,
        runif(n,xmin,xmax)
      ) %>%
          as_tibble %>%
          rename_all(
            funs(
              gsub('V','x',.)
            )
          ) %>%
          bind_cols(
            transmute_all(
              .,
              funs(
                trueDist(.) %>% add(rnorm(n))
              )
            ) %>%
              rename_all(
                funs(
                  gsub('x','y',.)
                )
              )
          )
      },ignoreNULL = FALSE
    )
    
    models = reactive(
      lapply(
        degrees,
        function(l) {
          data() %$%
            lm(y1 ~ poly(x1,l,raw=T))
        }
      )
    )
    
    Rsq = function(model,y,x) {
      model %>% predict(newdata = tibble(x1=x)) %>%
        cor(y,.) %>% raise_to_power(2) %>% as.numeric
    }
    
    Rsq1 = reactive(
      lapply(
        1:3,
        function(l) {
          data() %$%
            Rsq(models()[[l]],y1,x1)
        }
      )
    )
    
    Rsq2 = reactive(
      lapply(
        1:3,
        function(l) {
          data() %$%
            Rsq(models()[[l]],y2,x2)
        }
      )
    )
    
    p1 = reactive(
      data() %>% ggplot(aes(x=x1,y=y1)) +
        geom_point() +
        stat_function(fun = trueDist,col='grey') +
        stat_smooth(geom='line',method = "lm", formula = y ~ poly(x,degrees[1],raw=TRUE,coefs=NULL)) +
        lims(x = c(xmin,xmax), y = c(ymin,ymax)) +
        labs(x = sprintf('R² = %.3f',Rsq1() %>% extract2(1)))
    )
    
    p2 = reactive(
      data() %>% ggplot(aes(x=x1,y=y1)) +
      geom_point() +
      stat_function(fun = function(x) {x+x^2},col='grey') +
      stat_smooth(geom='line',method = "lm", formula = y ~ poly(x,degrees[2],raw=TRUE,coefs=NULL)) +
      lims(x = c(xmin,xmax), y = c(ymin,ymax)) +
      labs(x = sprintf('R² = %.3f',Rsq1() %>% extract2(2)))
    )

    p3 = reactive(
      data() %>% ggplot(aes(x=x1,y=y1)) +
      geom_point() +
      stat_function(fun = function(x) {x+x^2},col='grey') +
      stat_smooth(geom='line',method = "lm", formula = y ~ poly(x,degrees[3],raw=TRUE,coefs=NULL)) +
      lims(x = c(xmin,xmax), y = c(ymin,ymax)) +
      labs(x = sprintf('R² = %.3f',Rsq1() %>% extract2(3)))
    )

    p4 = reactive(
      data() %>% ggplot(aes(x=x1,y=y1)) +
      geom_point(aes(x=x2,y=y2)) +
      stat_function(fun = trueDist,col='grey') +
      stat_smooth(geom='line',method = "lm", formula = y ~ poly(x,degrees[1],raw=TRUE,coefs=NULL)) +
      lims(x = c(xmin,xmax), y = c(ymin,ymax)) +
      labs(x = sprintf('R² = %.3f',Rsq2() %>% extract2(1)))
    )

    p5 = reactive(
      data() %>% ggplot(aes(x=x1,y=y1)) +
      geom_point(aes(x=x2,y=y2)) +
      stat_function(fun = function(x) {x+x^2},col='grey') +
      stat_smooth(geom='line',method = "lm", formula = y ~ poly(x,degrees[2],raw=TRUE,coefs=NULL)) +
      lims(x = c(xmin,xmax), y = c(ymin,ymax)) +
      labs(x = sprintf('R² = %.3f',Rsq2() %>% extract2(2)))
    )

    p6 = reactive(
      data() %>% ggplot(aes(x=x1,y=y1)) +
      geom_point(aes(x=x2,y=y2)) +
      stat_function(fun = function(x) {x+x^2},col='grey') +
      stat_smooth(geom='line',method = "lm", formula = y ~ poly(x,degrees[3],raw=TRUE,coefs=NULL)) +
      lims(x = c(xmin,xmax), y = c(ymin,ymax)) +
      labs(x = sprintf('R² = %.3f',Rsq2() %>% extract2(3)))
    )
    
    theme_update(
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.title.y = element_blank()
    )
    
    output$plot = renderPlot({
      plot_grid(
        p1(),p2(),p3(),p4(),p5(),p6(),
        align = 'h',
        nrow = 2
      ) +
        draw_label('old', angle = 90, size = 13,x = 0.005,y = .75,fontface = 'bold') +
        draw_label('new', angle = 90, size = 13,x = 0.005,y = .25,fontface = 'bold') +
        draw_label('Linear',size = 14, x = .17, y = .99, fontface = 'bold') +
        draw_label('Quadratic',size = 14, x = .5, y = .99, fontface = 'bold') +
        draw_label(paste0(degrees[3],'-th Degree Polynomial'),size = 14, x = .84, y = .99, fontface = 'bold') +
        theme(plot.margin = unit(c(.5,.5,.5,.5), "cm"))
    })
    
  }
)