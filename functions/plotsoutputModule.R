plotsoutputUI <- function(id) {
  ns <- NS(id)

  tagList(fluidRow(
    box(
      column(
        2,
        numericInput(
          ns('outb_Thhold'),
          'Probability to have at least n nosocomial infection:',
          value = 1,
          min = 0,
          step = 1
        )
      ),
      column(5,
             plotOutput(ns("pOutbreak"))),
      column(5,
             plotOutput(ns("nosoHazard")))
    ),
    box(
      column(
        4,
        selectInput(
          inputId = ns("scaleInc"),
          label = "Display incidence for (scale):",
          choices = c("The whole facility" = 0,
                      "Each ward" = 1),
          selected = FALSE
        ),
        conditionalPanel(
          condition =
            paste0('input[\'', ns('scaleInc'), "\'] == 1"),
          checkboxInput(
            inputId = ns("wardInc"),
            label = "Display incidence for a specific ward",
            value = FALSE,
            width = NULL
          ),
          conditionalPanel(condition =
                             paste0('input[\'', ns('wardInc'), "\'] == 1"),
                           uiOutput(ns("ward_choiceInc")))
        ),
        selectInput(
          inputId = ns("popInc"),
          label = "Display incidence for (population):",
          choices = c(
            "Both: patients and professionals" = "P+H",
            "Patients" = "P",
            "Professionals" = "H"
          ),
          selected = FALSE
        ),
        conditionalPanel(condition =
                           paste0('input[\'', ns('iterInc'), "\'] == 0"),
        checkboxInput(
          inputId = ns("display_sdInc"),
          label = "Display standard deviation",
          value = FALSE,
          width = NULL
        )),
        checkboxInput(
          inputId = ns("iterInc"),
          label = "Display incidence for only one simulation",
          value = FALSE,
          width = NULL
        ),
        conditionalPanel(condition =
                           paste0('input[\'', ns('iterInc'), "\'] == 1"),
                         uiOutput(ns("iter_choiceInc")))
      ),
      column(8,
             plotOutput(ns("plotIncidence")))
    ),
    br(),
    box(
      column(
        4,
        selectInput(
          inputId = ns("scaleTest"),
          label = "Display number of tests for (scale):",
          choices = c("The whole facility" = 0,
                      "Each ward" = 1),
          selected = FALSE
        ),
        conditionalPanel(
          condition =
            paste0('input[\'', ns('scaleTest'), "\'] == 1"),
          checkboxInput(
            inputId = ns("wardTest"),
            label = "Display number of tests for a specific ward",
            value = FALSE,
            width = NULL
          ),
          conditionalPanel(condition =
                             paste0('input[\'', ns('wardTest'), "\'] == 1"),
                           uiOutput(ns(
                             "ward_choiceTest"
                           )))
        ),
        selectInput(
          inputId = ns("popTest"),
          label = "Display number of tests for (population):",
          choices = c(
            "Both: patients and professionals" = "P+H",
            "Patients" = "P",
            "Professionals" = "H"
          ),
          selected = FALSE
        ),
        checkboxInput(
          inputId = ns("iterTest"),
          label = "Display incidence for only one simulation",
          value = FALSE,
          width = NULL
        ),
        conditionalPanel(condition =
                           paste0('input[\'', ns('iterTest'), "\'] == 1"),
                         uiOutput(ns(
                           "iter_choiceTest"
                         ))),
        checkboxInput(
          inputId = ns("agrtest"),
          label = "Median over aggregated period",
          value = FALSE,
          width = NULL
        ),
        conditionalPanel(condition =
                           paste0('input[\'', ns('agrtest'), "\'] == 1"),
                         uiOutput(ns(
                           "daysint_choiceTest"
                         )))
      ),
      column(8,
             plotOutput(ns("plottest")))
    )
  ))
}

plotsoutput <- function(input, output, session, model, variable, ndays) {
  # model()
  ns <- session$ns

  #########
  ######### Network plot
  #########

  output$pOutbreak <- renderPlot({
    plot_pOutbreak(
      model(),
      variable$matContact,
      variable$pop_size_P,
      outb_Thhold = input$outb_Thhold,
      addtitle = TRUE,
      verbose = FALSE
    )
  })

  output$nosoHazard <- renderPlot({

    plot_nosoHazard(trajmwss = model(),
                    ward_names = variable$ward_names,
                    pop_size_P = variable$pop_size_P,
                    LS = variable$LS,
                    matContact = variable$matContact,
                    addtitle = TRUE,
                    verbose = FALSE)

  })


  #########
  ######### Cummulative incidences
  #########

  output$iter_choiceInc <- renderUI({
    selectInput(
      inputId = ns("iter_inc"),
      label = "Select a simulation",
      choices = seq(length(model()))
    )
  })

  output$ward_choiceInc <- renderUI({
    selectInput(
      inputId = ns("ward_inc"),
      label = "Select a ward",
      choices = variable$ward_names
    )
  })

  output$plotIncidence <- renderPlot({

    ward = FALSE

    if(isTRUE(input$iterInc) |  length(input$display_sdInc) == 0)
      display_sdInc <- FALSE else
        display_sdInc <- input$display_sdInc

    if (input$scaleInc == 1 & isTRUE(input$wardInc) & length(input$ward_inc) > 0)
      ward = input$ward_inc

    if (input$iterInc == 1 & length(input$iter_inc) > 0)
      iter = input$iter_inc %>% as.numeric
    else
      iter = FALSE

    if (input$popInc == "P+H" | length(input$popInc) == 0)
      pop = FALSE
    else
      pop = input$popInc

      scale = input$scaleInc
      display_sd = display_sdInc

    plot_incidence(
      model(),
      scale = scale,
      pop = pop,
      iter = iter,
      ward = ward,
      display_sd = display_sdInc
    )
  })

  #########
  ######### Daily test boxplot
  #########

  output$iter_choiceTest <- renderUI({
    selectInput(
      inputId = ns("iter_test"),
      label = "Select a simulation",
      choices = seq(length(model()))
    )
  })

  output$ward_choiceTest <- renderUI({
    selectInput(
      inputId = ns("ward_test"),
      label = "Select a ward",
      choices = variable$ward_names
    )
  })

  output$daysint_choiceTest <- renderUI({
    numericInput(
      ns('daysint'),
      'Calculate median of daily number of test over n-days periods',
      value = 1,
      min = 0,
      max = ndays(),
      step = 1
    )
  })


  output$plottest <- renderPlot({
    ward = FALSE

    if (input$scaleTest == 1 & isTRUE(input$wardTest) & length(input$ward_test) > 0)
      ward = input$ward_test


    if (input$iterTest == 1 & length(input$iter_test) > 0)
      iter = input$iter_test %>% as.numeric
    else
      iter = FALSE

    if (input$popTest == "P+H")
      pop = NULL
    else
      pop = input$popTest


    if (input$agrtest == 0 | length(input$daysint) == 0)
      daysint = 1
    else
      daysint = input$daysint

    scale = input$scaleTest %>% as.numeric

    plot_test(
      model(),
      daysint = daysint,
      iter = iter,
      ward = ward,
      pop = pop,
      scale = scale
    )

  })

}
