# Title: Fit
# Author: Thomas Verliefde
# Date: 2018/09/19
# Version: 1.1

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
        4,
        HTML('<h1> <div align="center"> Generalizability of Fit <div></h1>')
      ),
      column(
        2,
        numericInput(
          "oldN","Initial Sample Size",15,min=15,max=100
        )
        ),
      column(
        2,
        numericInput(
          'newN','Retest Sample Size',15,min=15,max=100
        )
      ),
      column(
        2,
        numericInput(
          'polyD','# Degrees Polynomial',6,min=3,max=15
        )
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
    ymargin = 3
    ymin = seq(xmin,xmax,length.out=101) %>%
      trueDist(.) %>% min %>% subtract(ymargin) %>% floor
    ymax = seq(xmin,xmax,length.out=101) %>%
      trueDist(.) %>% max %>% add(ymargin) %>% ceiling
    degrees = reactive(c(1,2,input$polyD))
    
    data1 = eventReactive(
      input$refresh,
      {
        tibble(
          x1 = runif(input$oldN,xmin,xmax)
          ) %>%
          mutate(
          y1 = trueDist(x1) %>% add(rnorm(input$oldN))
        )
      },
      ignoreNULL = FALSE
    )
    
    data2 = eventReactive(
      input$refresh,
      {
        tibble(
          x2 = runif(input$newN,xmin,xmax)
          ) %>%
          mutate(
          y2 = trueDist(x2) %>% add(rnorm(input$newN))
        )
      },
      ignoreNULL = FALSE
    )
        
        
        
    #   data = eventReactive(
    #     input$refresh,
    #     {replicate(
    #     iter,
    #     runif(input$oldN,xmin,xmax)
    #   ) %>%
    #       as_tibble %>%
    #       rename_all(
    #         funs(
    #           gsub('V','x',.)
    #         )
    #       ) %>%
    #       bind_cols(
    #         transmute_all(
    #           .,
    #           funs(
    #             trueDist(.) %>% add(rnorm(input$oldN))
    #           )
    #         ) %>%
    #           rename_all(
    #             funs(
    #               gsub('x','y',.)
    #             )
    #           )
    #       )
    #   },ignoreNULL = FALSE
    # )
    
    models = reactive(
      lapply(
        degrees(),
        function(l) {
          data1() %$%
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
          data1() %$%
            Rsq(models()[[l]],y1,x1)
        }
      )
    )
    
    Rsq2 = reactive(
      lapply(
        1:3,
        function(l) {
          data2() %$%
            Rsq(models()[[l]],y2,x2)
        }
      )
    )
    
    p1 = reactive(
      data1() %>% ggplot(aes(x=x1,y=y1)) +
        geom_point() +
        stat_function(fun = trueDist,col='grey') +
        stat_smooth(geom='line',method = "lm", formula = y ~ poly(x,degrees()[1],raw=TRUE,coefs=NULL)) +
        lims(x = c(xmin,xmax), y = c(ymin,ymax)) +
        labs(x = sprintf('R² = %.3f',Rsq1() %>% extract2(1)),
             y = 'old',
             title = 'Linear') +
        theme_light() +
        theme(
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(
              colour='black',fill=NA
            )
          )
    )
    
    p2 = reactive(
      data1() %>% ggplot(aes(x=x1,y=y1)) +
        geom_point() +
        stat_function(fun = function(x) {x+x^2},col='grey') +
        stat_smooth(geom='line',method = "lm", formula = y ~ poly(x,degrees()[2],raw=TRUE,coefs=NULL)) +
        lims(x = c(xmin,xmax), y = c(ymin,ymax)) +
        labs(x = sprintf('R² = %.3f',Rsq1() %>% extract2(2)),
             y = '',
             title = 'Quadratic') +
        theme_light() +
        theme(
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(
            colour='black',fill=NA
          )
        )
    )
    
    p3 = reactive(
      data1() %>% ggplot(aes(x=x1,y=y1)) +
        geom_point() +
        stat_function(fun = function(x) {x+x^2},col='grey') +
        stat_smooth(geom='line',method = "lm", formula = y ~ poly(x,degrees()[3],raw=TRUE,coefs=NULL)) +
        lims(x = c(xmin,xmax), y = c(ymin,ymax)) +
        labs(x = sprintf('R² = %.3f',Rsq1() %>% extract2(3)),
             y = '',
             title = 'Polynomial') +
        theme_light() +
        theme(
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(
            colour='black',fill=NA
          )
        )
    )
    
    p4 = reactive(
      data1() %>% ggplot(aes(x=x1,y=y1)) +
        geom_point(data = data2(),aes(x=x2,y=y2)) +
        stat_function(fun = trueDist,col='grey') +
        stat_smooth(geom='line',method = "lm", formula = y ~ poly(x,degrees()[1],raw=TRUE,coefs=NULL)) +
        lims(x = c(xmin,xmax), y = c(ymin,ymax)) +
        labs(x = sprintf('R² = %.3f',Rsq2() %>% extract2(1)),
             y = 'new',
             title = '') +
        theme_light() +
        theme(
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(
            colour='black',fill=NA
          )
        )
    )
    
    p5 = reactive(
      data1() %>% ggplot(aes(x=x1,y=y1)) +
        geom_point(data = data2(),aes(x=x2,y=y2)) +
        stat_function(fun = function(x) {x+x^2},col='grey') +
        stat_smooth(geom='line',method = "lm", formula = y ~ poly(x,degrees()[2],raw=TRUE,coefs=NULL)) +
        lims(x = c(xmin,xmax), y = c(ymin,ymax)) +
        labs(x = sprintf('R² = %.3f',Rsq2() %>% extract2(2)),
             y = '',
             title = '') +
        theme_light() +
        theme(
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(
            colour='black',fill=NA
          )
        )
    )
    
    p6 = reactive(
      data1() %>% ggplot(aes(x=x1,y=y1)) +
        geom_point(data = data2(),aes(x=x2,y=y2)) +
        stat_function(fun = function(x) {x+x^2},col='grey') +
        stat_smooth(geom='line',method = "lm", formula = y ~ poly(x,degrees()[3],raw=TRUE,coefs=NULL)) +
        lims(x = c(xmin,xmax), y = c(ymin,ymax)) +
        labs(x = sprintf('R² = %.3f',Rsq2() %>% extract2(3)),
             y = '',
             title = '') +
        theme_light() +
        theme(
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(
            colour='black',fill=NA
          )
        )
    )
    
    output$plot = renderPlot({
      plot_grid(
        p1(),p2(),p3(),p4(),p5(),p6(),
        align = 'h',
        nrow = 2
       )
    })
    
  }
)
