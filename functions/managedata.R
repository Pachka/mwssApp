

managedataUI <- function(id) {
  ns <- NS(id)

  tagList(
    fileInput(ns("loadwards"), "Upload a pre-recorded set of wards/buildings",
              accept = c("rda", ".Rda")),
    actionButton(inputId = ns("apply"), label = "Load new dataset", icon = icon("play"),
                 style = "color: #fff; background-color: red; border-color: #fff;width:130;padding: 5px 5px 5px 5px;margin: -20px 5px 5px 15px; "),
    h5("Download the current structure.", style = "margin: 15px 5px 5px 5px; "),
    downloadButton(ns("downloadData"), "Download",
                   style = "color: #fff; background-color: #27ae60; border-color: #fff;padding: 5px 5px 5px 5px;margin: 5px 5px 5px 15px; ")
  )

}

managedataServ <- function(id, df) {
  moduleServer(
    id,
    function(input, output, session) {
      ############
      ############
      ### Upload/Download dataset
      ############
      ############

      ########
      ## Upload dataset
      ########

      observeEvent(input$apply, {
        ask_confirmation(
          inputId = "myconfirmation",
          title = "Want to confirm ?",
          type = "warning",
          btn_labels = c("Cancel", "Confirm"),
          text = "Note that loading a dataset will erase the currently recorded structure."
        )
      })

      observeEvent(eventExpr = input$myconfirmation,
                   handlerExpr = {
                     if(isTRUE(input$myconfirmation)){
                       req(input$loadwards)

                       load(input$loadwards$datapath)

                       df$df <- savedstrdf$structure
                       df$TS <- savedstrdf$matContact
                       df$IMMstate <- savedstrdf$IMMstate

                       print(df)
                       }
                   }, ignoreNULL = FALSE)


      ########
      ## Download dataset
      ########

      output$downloadData <- downloadHandler(
        filename = function() {
          paste("data-", Sys.Date(), ".rda", sep="")
        },
        content = function(filename) {
          savedstrdf <- list(structure = df$df, matContact = df$TS, IMMstate = df$IMMstate)
          save(savedstrdf, file = filename)
        }
      )

    }
  )
}
