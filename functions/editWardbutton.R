editWardbutton  <- function(input, output, session, wardchoices){

  observeEvent(input$editbut, {
    showModal(modalDialog(
      title = "Here you can add new wards or buildings",
      selectizeInput("wardtomod", "Ward/building name",
                     choices = wardchoices,
                     options = list(
                       placeholder = 'Select ward',
                       onInitialize = I('function() { this.setValue(""); }')
                     )),
      conditionalPanel(
        "input.wardtomod != ''",
        numericInput(
          'P_pop_sizeNEW',
          'Number of beds',
          value = 1,
          min = 1,
          step = 1
        ),
        numericInput(
          'H_pop_sizeNEW',
          'Number of healthcare workers',
          value = 1,
          min = 1,
          step = 1
        ),
        numericInput(
          'turnoverNEW',
          'Average stay (days)',
          value = 1,
          min = 1,
          step = 1
        ),
        numericInput(
          'V_pop_sizeNEW',
          'Average number of visits per week',
          value = 0,
          min = 0,
          step = 1
        )
      ),
      actionButton("modWbutton", "Modify ward/building"),
      easyClose = TRUE
    ))
  })
    }
