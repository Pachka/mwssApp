deleteWardbutton  <- function(input, output, session, dataset, rows){

  observeEvent(input$deletebut,{
      remWard <- dataset$str[as.numeric(rows), "Ward"]
            dataset$str <- dataset$str[-as.numeric(rows),]
            dataset$TS <- dataset$TS[dataset$TS$Ward != remWard,]
            dataset$TS <- dataset$TS[, names(dataset$TS) != remWard]

            if(!is.null(dataset$intro))
              if(remWard %in% dataset$intro$ward)
                dataset$intro <- dataset$intro[dataset$intro$ward != remWard, ]
        })
    }
