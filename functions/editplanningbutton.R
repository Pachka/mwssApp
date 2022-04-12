editplanningbutton  <- function(input, output, session, workerchoices, wardchoices){
  observeEvent(input$editbut, {
    showModal(modalDialog(
      title = "Here you can add new wards or buildings",
      selectizeInput("workertomod", "Caregivers name",
                     choices = workerchoices,
                     options = list(
                       placeholder = 'Select worker',
                       onInitialize = I('function() { this.setValue(""); }')
                     )),
      conditionalPanel(
        "input.workertomod != ''",
        div(DT::DTOutput("TSedit"), style = "font-size: 70%;"),
        selectizeInput("wardtomod", "Change time spend in one of the Ward/building",
                       choices = wardchoices,
                       options = list(
                         placeholder = 'Select ward',
                         onInitialize = I('function() { this.setValue(""); }')
                       )),
        conditionalPanel(
          "input.wardtomod != ''",
          numericInput(
            'ptimeinWard',
            '% of work time in the ward',
            value = 0,
            min = 0,
            max = 100,
            step = 1
          )
        )
      ),
      actionButton("modPlanbutton", "Modify planning"),
      easyClose = TRUE
    ))
  })
}
