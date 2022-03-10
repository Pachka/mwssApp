

selectVariantUI <- function(id, choices) {
  ns <- NS(id)

  tagList(
    selectizeInput(ns("variant_id"), "SARS-CoV-2 variants",
                   choices =  choices,
                   options = list(
                     placeholder = 'Choose a SARS-CoV-2 variant',
                     onInitialize = I('function() { this.setValue(""); }')
                   )
  )
  )

}





selectVariantServ <- function(id) {

  moduleServer(
    id,
    function(input, output, session) {

    observeEvent(input$variant_id,{
    print("test")
    params_dataset <- paste0(input$variant_id, "Params") %>% get
    updateNumericInput(session, inputId = "PrevCom", value = params_dataset$PrevCom)

  })


    }
  )
}


