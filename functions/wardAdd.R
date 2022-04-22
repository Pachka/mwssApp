wardAddPanel <- function(input, output, session){
  observeEvent(input$addbut, {
    showModal(modalDialog(
      title = "Here you can add new wards or buildings",
      textInput("wardname", "Ward/building name", ""),
      tags$p(
        "Note: Each ward/building has collective areas and dedicated medical staff. \n
      Two wards cannot have the same name."
      ),
      conditionalPanel(
        "input.wardname != ''",
        numericInput(
          'pop_size_P',
          'Number of beds',
          value = 1,
          min = 1,
          step = 1
        ),
        numericInput(
          'pop_size_H',
          'Number of healthcare workers',
          value = 1,
          min = 1,
          step = 1
        ),
        numericInput(
          'LS',
          'Average length of stay (days)',
          value = 1,
          min = 1,
          step = 1
        ),
        numericInput(
          'nVisits',
          'Average number of visits per day',
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
