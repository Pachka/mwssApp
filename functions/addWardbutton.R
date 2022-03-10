addWardbutton <- function(input, output, session){
  observeEvent(input$addbut, {
    showModal(modalDialog(
      title = "Here you can add new wards or buildings",
      textInput("ward", "Ward/building name", ""),
      tags$p(
        "Note: Each ward/building has collective areas and dedicated medical staff. \n
      Two wards cannot have the same name."
        # )
      ),
      conditionalPanel(
        "input.ward != ''",
        numericInput(
          'P_pop_size',
          'Number of beds',
          value = 1,
          min = 1,
          step = 1
        ),
        numericInput(
          'H_pop_size',
          'Number of healthcare workers',
          value = 1,
          min = 1,
          step = 1
        ),
        numericInput(
          'turnover',
          'Average stay (days)',
          value = 1,
          min = 1,
          step = 1
        ),
        numericInput(
          'V_pop_size',
          'Average number of visits per week',
          value = 0,
          min = 0,
          step = 1
        )
      ),
      uiOutput("addBut"),
      easyClose = TRUE
    ))
  })
}
