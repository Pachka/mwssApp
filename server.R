# Define server logic required to draw a histogram
server <- function(input, output, session) {


  ######################################
  #### Sidebar panels and subpanels ####
  ######################################

  observeEvent(input$sidebarItemExpanded, {
    if(input$sidebarItemExpanded == "STR")
      updateTabItems(session, "sidebarMenu", selected = "hiddenSTR") else
        if(input$sidebarItemExpanded == "EPI")
          updateTabItems(session, "sidebarMenu", selected = "hiddenEPI") else
            if(input$sidebarItemExpanded == "SIM")
              updateTabItems(session, "sidebarMenu", selected = "hiddenSIM")

  })

  ###############################
  ####  Tables initialization ###
  ###############################

  # make reactive to record wards
  data <- shiny::reactiveValues(
    # Wards structure
    str = data.frame(
      "Ward" = character(),
      "P" = numeric(),
      "H" = numeric(),
      "Visits" = numeric(),
      "Turnover" = numeric(),
      stringsAsFactors = F
    ),
    # Prop of time per HCWS in each unit as data.frame
    TS = data.frame("Worker" = character(),
                    "Ward" = numeric(),
                    # "total" = numeric(),
                    stringsAsFactors = F),
    intro = NULL,
    IMMstate = NULL,
    EPIstate = NULL
  )


  #####################################
  #######    Structure panel     ######
  #####################################

  ###############################
  ####    Add wards           ###
  ###############################


  ###
  ### Use add Button
  ###

  # when addward is activated (see body_ui) -- open remote window structured as follow
  callModule(addWardbutton, "addward")

  ## If a new ward name is written in the dedicated field, the add button appears
  output$addBut <- renderUI({
    if (!input$ward %in% data$str$Ward & input$ward != "") {
      actionButton("addWbutton", "Add ward/building")
    }
  })

  ## When addBut is activated
  # Add ward/holding and HCWS to the tables: str, TS, IMMstate
  observeEvent(input$addWbutton, {
    isolate({

      #####
      ##### Update structure table
      #####

      # add ward to wards structure data.frame
      data$str[nrow(data$str) + 1,] <- c(
        input$ward,
        input$P_pop_size,
        input$H_pop_size,
        input$V_pop_size,
        input$turnover
      )

      #####
      ##### Update TS table
      #####

      ## add HCWS to contact matrix data.frame
      # already recorded HCWS
      recH <- nrow(data$TS)

      # set to 0 the time spent in other wards for all new HCWS
      data$TS[(recH + 1):(recH + input$H_pop_size), ] <- 0

      # set to 100 the time spent in the new wards for all new HCWS
      data$TS[(recH + 1):(recH + input$H_pop_size), input$ward] <-
        rep(100, input$H_pop_size %>% as.numeric)

      # set new HCWS names and affiliation
      data$TS[(recH + 1):(recH + input$H_pop_size), "Ward"] <-
        input$ward
      data$TS[(recH + 1):(recH + input$H_pop_size), "Worker"] <-
        paste0(paste0(input$ward, "_"), seq(input$H_pop_size))

      # set time of other P into new ward to 0
      data$TS[is.na(data$TS)] <- 0

      #####
      ##### Update IMMstate table
      #####

      # add ward to wards structure data.frame
      data$IMMstate %<>% rbind(. ,
                               data.table(ward = input$ward,
                                          pop = c("p","h"),
                                          imm = "NI",
                                          n = c(input$P_pop_size,
                                                input$H_pop_size)))

    })
  })

  ##########################
  ####    Modify ward    ###
  ##########################

  # when addward is activated (see body_ui) -- open remote window structured as follow

  callModule(editWardbutton, "editward", data$str$Ward)

  observeEvent(input$wardtomod, {
    updateNumericInput(session, "P_pop_sizeNEW", value = data$str[data$str$Ward == input$wardtomod, "P"])
    updateNumericInput(session, "H_pop_sizeNEW", value = data$str[data$str$Ward == input$wardtomod, "H"])
    updateNumericInput(session, "turnoverNEW", value = data$str[data$str$Ward == input$wardtomod, "Turnover"])
    updateNumericInput(session, "V_pop_sizeNEW", value = data$str[data$str$Ward == input$wardtomod, "Visits"])
  })

  ## When modWbutton is activated
  # Add ward/holding and HCWS to the tables: str, TS, IMMstate
  observeEvent(input$modWbutton, {
    isolate({

      #####
      ##### Update structure table
      #####

      popH <- data$str[data$str$Ward == input$wardtomod,"H"] %>% as.numeric

      # add ward to wards structure data.frame
      data$str[data$str$Ward == input$wardtomod,"P"] <- input$P_pop_sizeNEW
      data$str[data$str$Ward == input$wardtomod,"H"] <- input$H_pop_sizeNEW
      data$str[data$str$Ward == input$wardtomod,"Turnover"] <- input$turnoverNEW
      data$str[data$str$Ward == input$wardtomod,"Visits"] <- input$V_pop_sizeNEW

      #####
      ##### Update TS table
      #####
      if(popH < input$H_pop_sizeNEW){

        nHCWSsup <- input$H_pop_sizeNEW - popH

        ## add HCWS to contact matrix data.frame
        # already recorded HCWS
        recH <- nrow(data$TS)

        # set to 0 the time spent in other wards for all new HCWS
        data$TS[(recH + 1):(recH + nHCWSsup), ] <- 0

        # set to 100 the time spent in the new wards for all new HCWS
        data$TS[(recH + 1):(recH + nHCWSsup), input$wardtomod] <- rep(100, nHCWSsup %>% as.numeric)

        # set new HCWS names and affiliation
        data$TS[(recH + 1):(recH + nHCWSsup), "Ward"] <- input$wardtomod

        # set names
        data$TS[(recH + 1):(recH + nHCWSsup), "Worker"] <- paste0(paste0(input$wardtomod, "_"), seq(nHCWSsup))

        # set time of other P into new ward to 0
        data$TS[is.na(data$TS)] <- 0
      }

      if(popH > input$H_pop_sizeNEW){

        data$TS %<>% .[data$TS$Ward != input$wardtomod, ]

        ## add HCWS to contact matrix data.frame
        # already recorded HCWS
        recH <- nrow(data$TS)

        # set to 0 the time spent in other wards for all new HCWS
        data$TS[(recH + 1):(recH + input$H_pop_sizeNEW), ] <- 0

        # set to 100 the time spent in the new wards for all new HCWS
        data$TS[(recH + 1):(recH + input$H_pop_sizeNEW), input$wardtomod] <- rep(100, input$H_pop_sizeNEW %>% as.numeric)

        # set new HCWS names and affiliation
        data$TS[(recH + 1):(recH + input$H_pop_sizeNEW), "Ward"] <- input$wardtomod

        # set names
        data$TS[(recH + 1):(recH + input$H_pop_sizeNEW), "Worker"] <- paste0(paste0(input$wardtomod, "_"), seq(input$H_pop_sizeNEW))

        # set time of other P into new ward to 0
        data$TS[is.na(data$TS)] <- 0
      }
      #####
      ##### Update IMMstate table
      #####

      # add ward to wards structure data.frame
      # data$IMMstate %<>% rbind(. ,
      #                          data.table(ward = input$ward,
      #                                     pop = c("p","h"),
      #                                     imm = "NI",
      #                                     n = c(input$P_pop_size,
      #                                           input$H_pop_size)))

    })
  })

  ################################
  ####    Remove HCWS in TS    ###
  ################################

  ### FIX ME: to do

  #######################################
  ####    Edit HCWS planning in TS    ###
  #######################################


  callModule(editplanningbutton, "editplanning", data$TS$Worker, data$str$Ward)

  output$TSedit <- DT::renderDT({

    planninglayout <- data$TS[which(data$TS$Worker == input$workertomod),]

    DT::datatable(
      planninglayout,
      rownames = F,
      editable = FALSE,
      escape = FALSE
    )

  })

  observeEvent(input$workertomod, {
    # updateNumericInput(session, "P_pop_sizeNEW", value = data$str[data$str$Ward == input$wardtomod, "P"])
    # updateNumericInput(session, "H_pop_sizeNEW", value = data$str[data$str$Ward == input$wardtomod, "H"])
    # updateNumericInput(session, "turnoverNEW", value = data$str[data$str$Ward == input$wardtomod, "Turnover"])
    # updateNumericInput(session, "V_pop_sizeNEW", value = data$str[data$str$Ward == input$wardtomod, "Visits"])
  })

  ## When modWbutton is activated
  # Add ward/holding and HCWS to the tables: str, TS, IMMstate
  observeEvent(input$modPlanbutton, {
    isolate({

      #####
      ##### Update TS table
      #####
      data$TS[which(data$TS$Worker == input$workertomod),input$wardtomod] <- input$ptimeinWard

    })

  })

  #########################################
  #######    Parameters subpanel     ######
  #########################################


  #####################################
  #####    Parameters subpanel     ####
  #####################################

  ######################################################
  #### Specify initial Immunity levels in population ###
  ######################################################

  #####
  ##### Immunity level
  #####

  ####
  ### Patients
  ####

  # when addIMMP is activated (see body_ui)
  observeEvent(input$addIMMPs, {
    showModal(modalDialog(
      title = "Immunity state of individuals in the recorded wards",
      h3("The number of non immune individuals is deduced from the number of partially and fully immune individuals."),
      selectizeInput("IMMPWard", "Ward/building name",
                     choices = data$str$Ward,
                     options = list(
                       placeholder = 'Type to search for facilty',
                       onInitialize = I('function() { this.setValue(""); }')
                     )

      ),
      conditionalPanel(
        "input.IMMPWard != ''",
        knobInput(
          inputId = "W_pFI",
          label = "Number of fully immune patients",
          value = 0,
          min = 0,
          max = data$str %>% .[.$Ward == input$IMMPWard,"P"],
          # post = "%",
          displayPrevious = TRUE,
          lineCap = "round",
          fgColor = "#D9FF66",
          inputColor = "#C6FF1A"
        ),
        knobInput(
          inputId = "W_pPI",
          label = "Number of partially immune patients",
          value = 0,
          min = 0,
          max = data$str %>% .[.$Ward == input$IMMPWard,"P"],
          # post = "%",
          displayPrevious = TRUE,
          lineCap = "round",
          fgColor = "#FFD966",
          inputColor = "#FFC61A"
        ),
        knobInput(
          inputId = "W_pNI",
          label = "Number of non immune patients",
          value = data$str %>% .[.$Ward == input$IMMPWard,"P"],
          min = 0,
          max = data$str %>% .[.$Ward == input$IMMPWard,"P"],
          # post = "%",
          displayPrevious = TRUE,
          lineCap = "round",
          fgColor = "#FF6666",
          inputColor = "#FF1A1A",
          readOnly = T
        )
      ),
      conditionalPanel(
        "input.IMMPWard != ''",
        uiOutput("addIMMPButton")),
      easyClose = TRUE
    ))
  })


  observeEvent(input$IMMPWard,{
    pNI <- data$str %>% .[.$Ward == input$IMMPWard,"P"]
    pPI <- 0
    pFI <- 0

    updateKnobInput(session, 'W_pFI',
                    value = pFI,
                    options = list(max = pNI))

    updateKnobInput(session, 'W_pPI',
                    value = pPI,
                    options = list(max = pNI))

    updateKnobInput(session, 'W_pNI',
                    value = pNI,
                    options = list(max = pNI))
  })

  observeEvent(input$W_pFI, {
    isolate({
      pop <- data$str %>% .[.$Ward == input$IMMPWard,"P"]

      pFI <- input$W_pFI
      pPI <- input$W_pPI
      pNIup <- pop - input$W_pFI - input$W_pPI

      # print(c( pop = pop, pFI = pFI, pPI = pPI, pNI = pNIup))

      updateKnobInput(session, 'W_pPI',
                      options = list(max = pop - pFI))

      updateKnobInput(session, 'W_pNI',
                      value = pNIup,
                      options = list(max = pNIup))
    })
  })

  observeEvent(input$W_pPI,{
    pop <- data$str %>% .[.$Ward == input$IMMPWard,"P"]
    pFI <- input$W_pFI
    pPI <- input$W_pPI
    pNI <- pop - input$W_pFI - input$W_pPI

    updateKnobInput(session, 'W_pNI',
                    value = pNI,
                    options = list(max = pNI))

  })

  ## when addBut in the remote window is activated
  output$addIMMPButton <- renderUI({
    actionButton("addIMMP", "Add")
  })

  # Update immunity level states
  observeEvent(input$addIMMP, {
    isolate({
      # add ward to wards structure data.frame
      data$IMMstate %<>% .[(.$ward != input$IMMHWard | .$pop != "p"),]
      data$IMMstate %<>% rbind(.,data.frame(ward = input$IMMPWard,
                                            pop = "p",
                                            imm = c("NI","PI","FI"),
                                            n = c(input$W_pNI,
                                                  input$W_pPI,
                                                  input$W_pFI)
      )
      )
      data$IMMstate %<>% .[.$n != 0, ]

    })
  })

  ########### Set immunity levels

  ######## Generic levels

  output$updated_pNI_NL <- renderUI({
    pNI <- (1- input$PrevVacc) * 100

    valueBox(paste(pNI, "%"), "of non immune patients", icon = NULL,
             color = "red", width = 4,
             href = NULL)
  })

  output$updated_pPI <- renderUI({
    pPI <- input$PrevVacc * 100 * input$propV1
    valueBox(paste(pPI, "%"), "of partially immune patients", icon = NULL,
             color = "orange", width = 4,
             href = NULL)
  })


  output$updated_pFI <- renderUI({
    pFI <- input$PrevVacc * 100 * input$propV2
    valueBox(paste(pFI, "%"), "of fully immune patients", icon = NULL,
             color = "green", width = 4,
             href = NULL)
  })


  observeEvent(input$immuStatP, {
    if(input$immuStatP == "NL" & length(data$str$Ward) > 0){

      pNI <- (1- input$PrevVacc)
      pPI <- input$PrevVacc * input$propV1
      pFI <- input$PrevVacc * input$propV2

      IMMstateP <- expand.grid(
        ward = data$str$Ward,
        imm = c("NI","PI","FI"))

      IMMstateP %<>%  merge(., data$str[, c("Ward", "P")], all = T, by.x = "ward", by.y = "Ward")

      setDT(IMMstateP)

      apply(data$str, 1, function(W){

        nP <- sample(c("NI", "PI", "FI"), W["P"], prob = c(pNI, pPI, pFI), replace = T) %>% table

        if("NI" %in% names(nP))
          IMMstateP[ward == W["Ward"] & imm == "NI", n := nP[["NI"]]]
        if("PI" %in% names(nP))
          IMMstateP[ward == W["Ward"] & imm == "PI", n := nP[["PI"]]]
        if("FI" %in% names(nP))
          IMMstateP[ward == W["Ward"] & imm == "FI", n := nP[["FI"]]]
        IMMstateP[, pop := "p"]
      })

      IMMstateP[, P:=NULL]

      data$IMMstate <- rbind(data$IMMstate[pop != "p",], IMMstateP)
      data$IMMstate %<>% .[.$n != 0, ]
    }
  })

  ########
  ######## Modified levels
  ########
  #  Patients
  output$updated_slider_pPI <- renderUI({

    knobInput(
      inputId = "pPI",
      label = "Population partially immune",
      value = 0,
      min = 0,
      max = 100 - input$pFI,
      post = "%",
      displayPrevious = TRUE,
      lineCap = "round",
      fgColor = "#FFD966",
      inputColor = "#FFC61A"
    )
  })

  output$updated_pNI_ML <- renderUI({
    pNI <- 100 - input$pFI - input$pPI

    valueBox(paste(pNI, "%"), "of non immune patients", icon = NULL,
             color = "red", width = 4,
             href = NULL)
  })

  observeEvent(input$updateimmuStatPML, {

    pNI <- (100 - input$pFI - input$pPI)/100
    pPI <- (input$pPI / 100)
    pFI <- (input$pFI / 100)

    IMMstateP <- expand.grid(
      ward = data$str$Ward,
      imm = c("NI","PI","FI"))

    IMMstateP %<>%  merge(., data$str[, c("Ward", "P")], all = T, by.x = "ward", by.y = "Ward")

    setDT(IMMstateP)

    apply(data$str, 1, function(W){

      nP <- sample(c("NI", "PI", "FI"), W["P"], prob = c(pNI, pPI, pFI), replace = T) %>% table

      if("NI" %in% names(nP))
        IMMstateP[ward == W["Ward"] & imm == "NI", n := nP[["NI"]]]
      if("PI" %in% names(nP))
        IMMstateP[ward == W["Ward"] & imm == "PI", n := nP[["PI"]]]
      if("FI" %in% names(nP))
        IMMstateP[ward == W["Ward"] & imm == "FI", n := nP[["FI"]]]
      IMMstateP[, pop := "p"]
    })

    IMMstateP[, P:=NULL]

    data$IMMstate <- rbind(data$IMMstate[pop != "p",], IMMstateP)
    data$IMMstate %<>% .[.$n != 0, ]

  })


  ####
  ### Healthcare workers
  ####

  ####### Generic prop

  observeEvent(input$immuStatH, {
    if(input$immuStatH == "NL" & length(data$str$Ward) > 0){

      hNI <- (1 - input$PrevVacc)
      hPI <- input$PrevVacc * input$propV1
      hFI <- input$PrevVacc * input$propV2

      IMMstateH <- expand.grid(
        ward = data$str$Ward,
        imm = c("NI","PI","FI"))

      IMMstateH %<>%  merge(., data$str[, c("Ward", "H")], all = T, by.x = "ward", by.y = "Ward")

      setDT(IMMstateH)

      apply(data$str, 1, function(W){

        nH <- sample(c("NI", "PI", "FI"), W["H"], prob = c(hNI, hPI, hFI), replace = T) %>% table

        if("NI" %in% names(nH))
          IMMstateH[ward == W["Ward"] & imm == "NI", n := nH[["NI"]]]

        if("PI" %in% names(nH))
          IMMstateH[ward == W["Ward"] & imm == "PI", n := nH[["PI"]]]

        if("FI" %in% names(nH))
          IMMstateH[ward == W["Ward"] & imm == "FI", n := nH[["FI"]]]

        IMMstateH[, pop := "h"]
      })

      IMMstateH[, H:=NULL]

      data$IMMstate <- rbind(data$IMMstate[pop != "h",], IMMstateH)
      data$IMMstate %<>% .[.$n != 0, ]
    }
  })


  output$updated_hNI_NL <- renderUI({
    hNI <- (1- input$PrevVacc) * 100

    valueBox(paste(hNI, "%"), "of non immune healthcare workers", icon = NULL,
             color = "red", width = 4,
             href = NULL)
  })

  #######  Modified immunity prop at hospital-scale

  output$updated_hNI_ML <- renderUI({
    hNI <- 100 - input$hFI - input$hPI

    valueBox(paste(hNI, "%"), "of non immune healthcare workers", icon = NULL,
             color = "red", width = 4,
             href = NULL)
  })

  output$updated_hPI <- renderUI({
    hPI <- input$PrevVacc * 100 * input$propV1
    valueBox(paste(hPI, "%"), "of partially immune healthcare workers", icon = NULL,
             color = "orange", width = 4,
             href = NULL)
  })

  output$updated_hFI <- renderUI({
    hFI <- input$PrevVacc * 100 * input$propV2
    valueBox(paste(hFI, "%"), "of fully immune healthcare workers", icon = NULL,
             color = "green", width = 4,
             href = NULL)
  })

  observeEvent(input$updateimmuStatHML, {

    hNI <- (100 - input$hFI - input$hPI)/100
    hPI <- (input$hPI / 100)
    hFI <- (input$hFI / 100)

    IMMstateH <- expand.grid(
      ward = data$str$Ward,
      imm = c("NI","PI","FI"))

    IMMstateH %<>%  merge(., data$str[, c("Ward", "H")], all = T, by.x = "ward", by.y = "Ward")

    setDT(IMMstateH)

    apply(data$str, 1, function(W){

      nH <- sample(c("NI", "PI", "FI"), W["H"], prob = c(hNI, hPI, hFI), replace = T) %>% table

      if("NI" %in% names(nH))
        IMMstateH[ward == W["Ward"] & imm == "NI", n := nH[["NI"]]]

      if("PI" %in% names(nH))
        IMMstateH[ward == W["Ward"] & imm == "PI", n := nH[["PI"]]]

      if("FI" %in% names(nH))
        IMMstateH[ward == W["Ward"] & imm == "FI", n := nH[["FI"]]]

      IMMstateH[, pop := "h"]
    })

    IMMstateH[, H:=NULL]

    data$IMMstate <- rbind(data$IMMstate[pop != "h",], IMMstateH)
    data$IMMstate %<>% .[.$n != 0, ]
  })


  ####### Modified immunity prop at ward-scale

  # when addIMMH is activated (see body_ui)
  observeEvent(input$addIMMHs, {
    showModal(modalDialog(
      title = "Here you can specify the state of individuals in the recorded wards",
      selectizeInput("IMMHWard", "Ward/building name",
                     choices = data$str$Ward,
                     options = list(
                       placeholder = 'Select ward',
                       onInitialize = I('function() { this.setValue(""); }')
                     )

      ),
      conditionalPanel(
        "input.IMMHWard != ''",
        knobInput(
          inputId = "W_hFI",
          label = "Number of fully immune healthcare warkers",
          value = 0,
          min = 0,
          max = data$str %>% .[.$Ward == input$IMMHWard,"H"],
          # post = "%",
          displayPrevious = TRUE,
          lineCap = "round",
          fgColor = "#D9FF66",
          inputColor = "#C6FF1A"
        ),
        knobInput(
          inputId = "W_hPI",
          label = "Number of partially immune healthcare warkers",
          value = 0,
          min = 0,
          max = data$str %>% .[.$Ward == input$IMMHWard,"H"],
          # post = "%",
          displayPrevious = TRUE,
          lineCap = "round",
          fgColor = "#FFD966",
          inputColor = "#FFC61A"
        ),
        knobInput(
          inputId = "W_hNI",
          label = "Number of non immune healthcare warkers",
          value = data$str %>% .[.$Ward == input$IMMHWard,"H"],
          min = 0,
          max = data$str %>% .[.$Ward == input$IMMHWard,"H"],
          # post = "%",
          displayPrevious = TRUE,
          lineCap = "round",
          fgColor = "#FF6666",
          inputColor = "#FF1A1A",
          readOnly = T
        )
      ),
      conditionalPanel(
        "input.IMMHWard != ''",
        uiOutput("addIMMHButton")),
      easyClose = TRUE
    ))
  })


  observeEvent(input$IMMHWard,{

    hNI <- data$str %>% .[.$Ward == input$IMMHWard,"H"]
    hPI <- 0
    hFI <- 0

    updateKnobInput(session, 'W_hFI',
                    value = hFI,
                    options = list(max = hNI))

    updateKnobInput(session, 'W_hPI',
                    value = hPI,
                    options = list(max = hNI))

    updateKnobInput(session, 'W_hNI',
                    value = hNI,
                    options = list(max = hNI))
  })


  observeEvent(input$W_hFI,{

    pop <- data$str %>% .[.$Ward == input$IMMHWard,"H"]
    hFI <- input$W_hFI
    hPI <- input$W_hPI
    hNI <- pop - input$W_hFI - input$W_hPI

    updateKnobInput(session, 'W_hPI',
                    options = list(max = pop - hFI))

    updateKnobInput(session, 'W_hNI',
                    value = hNI,
                    options = list(max = hNI))

  })

  observeEvent(input$W_hPI,{


    pop <- data$str %>% .[.$Ward == input$IMMHWard,"H"]
    hFI <- input$W_hFI
    hPI <- input$W_hPI
    hNI <- pop - input$W_hFI - input$W_hPI

    updateKnobInput(session, 'W_hNI',
                    value = hNI,
                    options = list(max = hNI))

  })

  ## when addBut in the remote window is activated
  output$addIMMHButton <- renderUI({
    actionButton("addIMMH", "Add")
  })

  # Update immunity level states
  observeEvent(input$addIMMH, {
    isolate({

      IMMstateH <- expand.grid(
        ward = input$IMMHWard,
        imm = c("NI","PI","FI"))

      nH <- data$str %>% .[.$Ward == input$IMMHWard,"H"]

      hNI <- input$W_hNI/100
      hPI <- input$W_hPI/100
      hFI <- input$W_hFI/100

      sample(c("NI", "PI", "FI"), nH, prob = c(hNI, hPI, hFI), replace = T) %>% table %>% data.frame

      IMMstateH %<>% merge(., prop, by.x = "imm", by.y = ".")

      setDT(IMMstateH)

      setnames(IMMstateH, "Freq", "n")
      IMMstateH[, pop := "h"]

      data$IMMstate <- rbind(data$IMMstate[pop != "h" | ward != input$IMMHWard,], IMMstateH)

      data$IMMstate %<>% .[.$n != 0, ]

    })
  })


  output$updated_slider_hPI <- renderUI({

    knobInput(
      inputId = "hPI",
      label = "Population partially immune",
      value = 0,
      min = 0,
      max = 100 - input$hFI,
      post = "%",
      displayPrevious = TRUE,
      lineCap = "round",
      fgColor = "#428BCA",
      inputColor = "#428BCA"
    )
  })


  ###############################################
  #### Specify initial epidemiological states ###
  ###############################################

  # when addinf is activated (see body_ui)
  observeEvent(input$addinf, {
    showModal(modalDialog(
      title = "Here you can specify the state of individuals in the recorded wards",
      selectizeInput("wardinf", "Ward/building name",
                     choices = data$str$Ward,
                     options = list(
                       placeholder = 'Type to search for facilty',
                       onInitialize = I('function() { this.setValue(""); }')
                     )

      ),
      conditionalPanel(
        "input.wardinf != ''",
        selectInput(
          inputId = "epiType",
          label = "Population",
          choices = c("Health care workers" = "H", "Patients" = "P")
        ),
        uiOutput("epiStat"),
        selectInput(
          inputId = "epiStat",
          label = "Epidemiological state",
          choices = c("Incubating but not contagious" = "E",
                      "Incubating and contagious to become asymptomatic" = "EA",
                      "Incubating and contagious to become symptomatic" = "ES",
                      "Infectious asymptomatic" = "IA",
                      "Infectious with middle symptoms" = "I1",
                      "Infectious with severe symptoms" =  "I2",
                      "Recovered" = "R",
                      "Vaccinated with 1 dose (susceptible)" = "V1",
                      "Vaccinated with 2 doses (susceptible)" = "V2",
                      "Vaccinated and incubating but not contagious" = "VE",
                      "Vaccinated and contagious to become asymptomatic" = "VEA",
                      "Vaccinated and contagious to become symptomatic" = "VES",
                      "Vaccinated and infectious with middle symptoms" = "VI1",
                      "Vaccinated and infectious with severe symptoms" = "VI2")
        )
      ),
      uiOutput("addinfBut"),
      easyClose = TRUE
    ))
  })

  output$epiStat <- renderUI({
    intro <- data$intro

    if(input$wardinf %in% intro$ward & input$epiType %in% intro[intro$ward == input$wardinf, "type"]){

      alreadyattr <- intro[intro$ward == input$wardinf & intro$type == input$epiType, "number"] %>% as.numeric  %>% sum

      val <- data$str[data$str$Ward == input$wardinf, input$epiType] %>% as.numeric %>% subtract(alreadyattr)
    } else
      # If no epidemiological state is specified, all individuals are susceptible

      val <- data$str[data$str$Ward == input$wardinf, input$epiType] %>% as.numeric

    numericInput("epiSize", "Number of individuals",
                 value = 0,
                 min = 0,
                 max = val,
                 step = 1)
  })

  ## when addBut in the remote window is activated
  output$addinfBut <- renderUI({
    if (length(data$str$Ward) > 0) {
      actionButton("addepistates", "Add")
    }
  })

  # Update epidemiological states
  observeEvent(input$addepistates, {
    isolate({
      # add ward to wards structure data.frame
      data$intro %<>% rbind(.,
                            data.frame(ward =  input$wardinf,
                                       number = input$epiSize,
                                       type = ifelse(input$epiType == "P", "P", "H"),
                                       epiStat = input$epiStat))

    })
  })

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

                   data$str <- savedstrdf$structure
                   data$TS <- savedstrdf$matContact
                   data$IMMstate <- savedstrdf$IMMstate}
               }, ignoreNULL = FALSE)


  ########
  ## Download dataset
  ########

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".rda", sep="")
    },
    content = function(filename) {
      savedstrdf <- list(structure = data$str, matContact = data$TS, IMMstate = data$IMMstate)
      save(savedstrdf, file = filename)
    }
  )

  ############
  ############
  ### Display and edit tables
  ############
  ############

  ########
  ## Structure
  ########

  output$data <- DT::renderDT({

    structurelayout <- data$str
    ##rename column names to (inside renderDT)
    if(!is.null(structurelayout))
      colnames(structurelayout)[2:5] <- c("Patients", "Health care Workers", "Avg number of visits", "Avg length of stay")

    DT::datatable(
      structurelayout,
      rownames = F,
      escape = FALSE,
      options = list(pageLength = 5)
    )
  })

  # rows <- reactive({input$data_rows_selected})
  # if (!is.null(rows)){
  #   callModule(deleteWardbutton, "deleteward", dataset = data, rows = rows)
  #   }

  observeEvent(input$deletebut,{

    if (!is.null(input$data_rows_selected)) {
      remWard <- data$str[as.numeric(input$data_rows_selected), "Ward"]
      data$str <- data$str[-as.numeric(input$data_rows_selected),]
      data$TS <- data$TS[data$TS$Ward != remWard,]
      data$TS <- data$TS[, names(data$TS) != remWard]

      if(!is.null(data$intro))
        if(remWard %in% data$intro$ward)
          data$intro <- data$intro[data$intro$ward != remWard, ]
    }
  })

  ##########
  ## Time sharing
  ##########

  output$TS <- renderDataTable({

    TS <- data$TS
    wardN <- data$str$Ward
    TS$total <- TS[, wardN] %>% rowSums

    datatable(TS) %>% formatStyle(
      'total',
      backgroundColor = styleInterval(c(99.9, 100.1), c('green', 'white', 'red')),
      fontWeight = 'bold'
    )

  })

  # output$TS = renderDT(data$TS,
  #                      # filter = 'top',
  #                      selection = 'none',
  #                      rownames = FALSE,
  #                      editable = TRUE,
  #                      options = list(pageLength = 10))

  proxyTS = dataTableProxy('TS')

  observeEvent(input$TS_cell_edit, {
    info = input$TS_cell_edit
    str(info)
    i = info$row
    j = info$col + 1
    v = info$value

    isolate(
      data$TS[i, j] <<- DT::coerceValue(v, data$TS[i, j])
    )

    replaceData(proxyTS, data$TS, resetPaging = FALSE)  # replaces data displayed by the updated table
  })

  #   ############
  #   ## Initial Epidemiological state
  #   ############
  #
  output$intro <- DT::renderDT({

    ##rename column names to (inside renderDT)
    if(!is.null(data$intro))
      colnames(data$intro)[2:3] <- c("Patients", "Health care Workers")

    DT::datatable(
      data$intro,
      rownames = F,
      escape = FALSE,
      # editable = TRUE,
      options = list(pageLength = 10)
    )
  })

  output$intro = renderDT(data$intro,
                          rownames = FALSE,
                          # editable = TRUE,
                          options = list(pageLength = 10))

  proxyintro = dataTableProxy('intro')

  # observeEvent(input$intro_cell_edit, {
  #   info = input$intro_cell_edit
  #   str(info)
  #   i = info$row
  #   j = info$col + 1
  #   v = info$value
  #
  #   isolate(
  #     data$intro[i, j] <<- DT::coerceValue(v, data$intro[i, j])
  #   )
  #
  #   replaceData(proxyintro, data$intro, resetPaging = FALSE)  # replaces data displayed by the updated table
  # })

  observeEvent(input$deleteintro,{

    if (!is.null(input$intro_rows_selected)) {

      data$intro <- data$intro[-as.numeric(input$intro_rows_selected),]
    }
  })

  ###########
  ##### Plot the network
  ###########

  output$network_plot <- renderPlot({
    num_nodes <- nrow(data$str)

    if (num_nodes == 1) {

      my_sociomatrix <- matrix(rep(0, num_nodes * num_nodes),
                               # edge values
                               nrow = num_nodes,
                               #nrow must be same as ncol
                               ncol = num_nodes)

      diag(my_sociomatrix) <- 0

      net <- as.network(
        x = my_sociomatrix,
        # the network object
        directed = TRUE,
        # specify whether the network is directed
        loops = FALSE,
        # do we allow self ties (should not allow them)
        matrix.type = "adjacency" # the type of input
      )

      network.vertex.names(net) <- data$str$Ward

      vertex_size <-
        as.numeric(data$str$P) + as.numeric(data$str$H)

      plot.network(
        net,
        # our network object
        vertex.col = "grey",
        # color nodes by gender
        vertex.cex = (vertex_size / max(vertex_size)) * 3,
        # size nodes by their age
        displaylabels = T,
        # show the node names
        label.pos = 0 # display the names directly over nodes
      )

    }

    if (num_nodes > 1) {
      vertex_size <- as.numeric(data$str$P) + as.numeric(data$str$H)
      names(vertex_size) <- data$str$Ward

      contacts <- t(data$TS[, ! names(data$TS) %in% c("Worker", "Ward")])
      colnames(contacts) <- data$TS[,1]

      matContact <- mwss::timeShare(contacts, namesincol1 = FALSE)

      shinyWidgets::updateProgressBar(session = session, id = "pb", value = 0) # reinitialize to 0 if you run the calculation several times
      session$sendCustomMessage(type = 'launch-modal', "my-modal") # launch the modal

      mwss::plot_connectivity(matContact, vertex_size, verbose = FALSE)
    }

  })


  ###########
  ##### Plot the network
  ###########

  output$disease_states <- renderPlot({
    # create a dataset
    state <- c(rep("tE" , 3), rep("tEi" , 3) , rep("tI" , 3)) %>% factor(., levels = c("tI", "tEi", "tE"))
    condition <- rep(c("Asymptomatic" , "Mild symptoms" , "Severe symptoms") , 3) %>%
      factor(., levels = rev(c("Asymptomatic" , "Mild symptoms" , "Severe symptoms")))
    value <- c(rep(input$tE , 3) , input$tEA, rep(input$tES , 2) , input$tIA, input$tIM, input$tIS)
    data <- data.frame(state, condition, value)

    ggplot(data, aes(fill= as.factor(state), y=value, x=condition)) +
      geom_bar(position="stack", stat="identity") + coord_flip() +
      xlab("") + ylab("Duration (days)") +
      scale_fill_manual(values=c("#8b2312", "#E69F00", "#128b23"),
                        name="Epidemiological\nstate",
                        breaks= c("tI", "tEi", "tE"),
                        labels=c("Infectious", "Incubating infectious", "Incubating non infectious"))


  })




  ############
  ### Simulation section
  ############

  runmodel <- eventReactive(input$run, {


    if(data$str$Ward %>% length >= 2){

      if(input$testPW == "Ag-RDT"){
      senW = input$sensAg
      speW = input$speAg
      ttestPW = (as.numeric(strftime(input$tAg, "%M"))/60 + as.numeric(strftime(input$tAg, "%H")))/24
      } else {
        senW = input$sensPCR
        speW = input$spePCR
        ttestPW = (as.numeric(strftime(input$tPCR, "%M"))/60 + as.numeric(strftime(input$tPCR, "%H")))/24
      }

      if(input$testH == "Ag-RDT"){
        senH = input$sensAg
        speH = input$speAg
        ttestHW = (as.numeric(strftime(input$tAg, "%M"))/60 + as.numeric(strftime(input$tAg, "%H")))/24
      } else {
        senH = input$sensPCR
        speH = input$spePCR
        ttestHW = (as.numeric(strftime(input$tPCR, "%M"))/60 + as.numeric(strftime(input$tPCR, "%H")))/24
      }

      if(input$testsymp == "Ag-RDT"){
        sensymp = input$sensAg
        spesymp = input$speAg
        ttestsymp = (as.numeric(strftime(input$tAg, "%M"))/60 + as.numeric(strftime(input$tAg, "%H")))/24
      } else {
        sensymp = input$sensPCR
        spesymp = input$spePCR
        ttestsymp = (as.numeric(strftime(input$tPCR, "%M"))/60 + as.numeric(strftime(input$tPCR, "%H")))/24
      }

      if(input$testSA == "Ag-RDT"){
        senSA = input$sensAg
        speSA = input$speAg
      } else {
        senSA = input$sensPCR
        speSA = input$spePCR
      }
      # ttestSA
      # as.numeric(strftime(t, "%H"))/60 + strftime(t, "%M")

      # if no screening strategy >> probability to be tested is null
      if(input$screenstrP == 1){
        ptestPWNI = input$ptestPWNI # probability to test NI patients in the ward
        ptestPWLI = input$ptestPWLI # probability to test PI patients in the ward
        ptestPWHI = input$ptestPWHI # probability to test FI patients in the ward
      } else {
        ptestPWNI = 0 # probability to test NI patients in the ward
        ptestPWLI = 0 # probability to test PI patients in the ward
        ptestPWHI = 0 # probability to test FI patients in the ward
      }

      if(input$screenstrH == 1){
        ptestHNI = input$ptestHNI # probability to test NI HCWS
        ptestHLI = input$ptestHLI # probability to test PI HCWS
        ptestHHI = input$ptestHHI # probability to test FI HCWS
      } else {
        ptestHNI = 0 # probability to test NI HCWS
        ptestHLI = 0 # probability to test PI HCWS
        ptestHHI = 0 # probability to test FI HCWS
      }


      gdata <- build_gdata(
        ##### Infection
        n_ctcH_PSA = input$n_ctcH_PSA,
        t_ctcH_PSA = (as.numeric(strftime(input$t_ctcH_PSA, "%M"))/60 + as.numeric(strftime(input$t_ctcH_PSA, "%H")))/24,
        n_ctcP_PSA = input$n_ctcH_PSA,
        t_ctcP_PSA = (as.numeric(strftime(input$t_ctcP_PSA, "%M"))/60 + as.numeric(strftime(input$t_ctcP_PSA, "%H")))/24,
        n_ctcH_PW = input$n_ctcH_PW,
        t_ctcH_PW = (as.numeric(strftime(input$t_ctcH_PW, "%M"))/60 + as.numeric(strftime(input$t_ctcH_PW, "%H")))/24,
        n_ctcP_PW = input$n_ctcP_PW,
        t_ctcP_PW = (as.numeric(strftime(input$t_ctcP_PW, "%M"))/60 + as.numeric(strftime(input$t_ctcP_PW, "%H")))/24,
        n_ctcH_H = input$n_ctcH_H,
        t_ctcH_H = (as.numeric(strftime(input$t_ctcH_H, "%M"))/60 + as.numeric(strftime(input$t_ctcH_H, "%H")))/24,
        t_ctcV_PW = (as.numeric(strftime(input$t_ctcV_PW, "%M"))/60 + as.numeric(strftime(input$t_ctcV_PW, "%H")))/24,
        # daily incidence (https://www.gouvernement.fr/info-coronavirus/carte-et-donnees)
        I = input$I/100000,
        # disease duration (days)
        d = 10,
        #  basic reproduction number
        R0 = input$R0, # https://www.gouvernement.fr/info-coronavirus/carte-et-donnees
        tSA  = (as.numeric(strftime(input$tSA, "%M"))/60 + as.numeric(strftime(input$tSA, "%H")))/24, # average duration before full admission (in screening area for clinical exam, administrative procedure, etc)
        tISO = input$tISO,   # average duration of confinement (isolated ward or contact restriction)
        tIC  = input$tIC,   # average duration of stay in intensive care
        tSL  = input$tSLs[1],   # average duration of sick leave
        tESL = input$tSLs[2],   # average duration of extended sick leave
        tE  = input$tE, # duration epidemiological state E
        tEA = input$tEA, # duration epidemiological state EA
        tES = input$tES, # duration epidemiological state ES
        tIA = input$tIA, # duration epidemiological state IA
        tIM = input$tIM, # duration epidemiological state IM
        tIS = input$tIS, # duration epidemiological state IS
        tLI = input$tLI, # duration of partial immunity before return to non immune status
        tHI = input$tHI, # duration of full immunity before return to partial immune status
        # patient protection
        epsPPSA = input$epsPPSA %>% as.numeric,
        epsHPSA = input$epsHPSA %>% as.numeric,
        epsHPW = input$epsHPW %>% as.numeric,
        epsPPW = input$epsPPW %>% as.numeric,
        epsVPW = input$epsVPW %>% as.numeric,
        # healthcare workers protection
        epsPHSA = input$epsPHSA %>% as.numeric, #from patient in SA
        epsPHW = input$epsPHW %>% as.numeric, #from patient in W
        epsHHW = input$epsHHW %>% as.numeric, #from HW in W
        ## Test in ward
        ttestSA = (as.numeric(strftime(input$ttestSA, "%M"))/60 + as.numeric(strftime(input$ttestSA, "%H")))/24, # test duration in screening area
        ttestPW = ttestPW, # test duration in ward for screening patients
        ttestHW = ttestHW, # test duration in ward for screening professionals
        ttestsymp = ttestsymp, # test duration for symptomatic

        tbtwtestP = input$tbtwtestP, # duration between two tests for patient
        tbtwtestH = input$tbtwtestH, # duration between two tests for HCWS

        tbeftestPsymp = input$tbeftestPsymp/24, # duration before test of symp patient
        tbeftestHsymp = input$tbeftestHsymp/24, # duration before test of symp HCWS

        psympNI = input$psympNI, # probability to be symptomatic when non immune
        psympLI = input$psympLI, # probability to be symptomatic when partially immune
        psympHI = input$psympHI, # probability to be symptomatic when fully immune

        psevNI = input$psevNI, # probability to develop severe symptoms when non immune
        psevLI = input$psevLI, # probability to develop severe symptoms when partially immune
        psevHI = input$psevHI, # probability to develop severe symptoms when fully immune

        pSL = input$pSL/100, # probability to take sick leave
        pESL = input$pESL/100, # probability to take extended sick leave
        pSLT = input$pSLT/100, # probability to take EL/ESL after positive test

        pIC = input$pIC/100, # probability to be transfer in intensive care
        pdieIC = input$pdieIC/100, # probability to die in intensive care

        ###################################
        pLI = input$pLI, # probability to be PI at the admission (proportion of PI in the population)
        pHI = input$pHI, # probability to be FI at the admission (proportion of FI in the population)
        hNI2LI = input$hNI2LI, # daily probability to become partially immune
        hLI2HI = input$hLI2HI, # daily probability to become fully immune

        rinfLI = input$rinfLI, # partial immunity efficiency % FIX ME better explain that this is the ratio of reduction of probability to be infected compared to non immune
        rinfHI = input$rinfHI, # partial immunity efficiency % FIX ME better explain that this is the ratio of reduction of probability to be infected compared to non immune

        rsymp = input$rsymp, # Ratio adjusting probability of symptoms for patients compared to general population (professionals)
        rsev = input$rsev, # Ratio adjusting probability of severity if symptoms for patients compared to general population (professionals)

        ptestPSAsymp = input$ptestPSAsymp, # probability to test symptomatic patients in the screening area
        ptestPSANI = input$ptestPSANI,  # probability to test NI patients in the screening area
        ptestPSALI = input$ptestPSALI, # probability to test PI patients in the screening area
        ptestPSAHI = input$ptestPSAHI, # probability to test FI patients in the screening area

        ptestPWsymp = input$ptestPWsymp/100, # probability to test symptomatic patients in the ward
        ptestPWNI = ptestPWNI,# probability to test NI patients in the ward
        ptestPWLI = ptestPWLI, # probability to test PI patients in the ward
        ptestPWHI = ptestPWHI, # probability to test FI patients in the ward

        ptestHsymp = input$ptestHsymp/100, # probability to test symptomatic HCWS in the ward
        ptestHNI = ptestHNI, # probability to test NI HCWS
        ptestHLI = ptestHLI, # probability to test PI HCWS
        ptestHHI = ptestHHI, # probability to test FI HCWS

        senSA = senSA,
        speSA = speSA,
        senW = senW,
        speW = speW,
        senH = senH,
        speH = senH,
        sensymp = sensymp,
        spesymp = spesymp
      )

      ward_names <- data$str$Ward
      pop_size_P <- data$str$P %>% as.numeric
      pop_size_H <- data$str$H %>% as.numeric
      nVisits <- data$str$Visits %>% as.numeric %>% divide_by(7) # FIX ME check accuracy
      LS <- data$str$Turnover %>% as.numeric %>% divide_by(7) # FIX ME adjust

      # IMMstate <- data$IMMstate ## FIX ME: check and use

      contacts <- t(data$TS[, ! names(data$TS) %in% c("Worker", "Ward")])
      colnames(contacts) <- data$TS[,1]

      matContact <- mwss::timeShare(contacts, namesincol1 = FALSE)

      n_days <- input$n_days %>% seq

      IMMstate = NULL
      EPIstate = NULL

      if(input$SA == 1) SA = TRUE else SA = FALSE
      nH_SA = input$nH_SA

      test = TRUE

      save(ward_names, file = "tmpdata/ward_names.Rda")
      save(pop_size_P, file = "tmpdata/pop_size_P.Rda")
      save(pop_size_H, file = "tmpdata/pop_size_H.Rda")
      save(nVisits, file = "tmpdata/nVisits.Rda")
      save(LS, file = "tmpdata/LS.Rda")
      save(matContact, file = "tmpdata/matContact.Rda")
      save(IMMstate, file = "tmpdata/IMMstate.Rda")
      save(EPIstate,  file = "tmpdata/EPIstate.Rda")
      save(SA,  file = "tmpdata/SA.Rda")
      save(nH_SA,  file = "tmpdata/nH_SA.Rda")
      save(test,  file = "tmpdata/test.Rda")
      save(gdata,  file = "tmpdata/gdata.Rda")
      save(n_days,  file = "tmpdata/n_days.Rda")

      mwssmodel <- mwss(ward_names,
                    pop_size_P,
                    pop_size_H,
                    nVisits,
                    LS,
                    matContact = matContact,
                    IMMstate = NULL,
                    EPIstate = NULL,
                    SA = SA,
                    nH_SA = nH_SA,
                    gdata = gdata,
                    tspan =  n_days,
                    verbose = FALSE)

      n_sim <- input$n_sim

      save(n_sim,  file = "tmpdata/n_sim.Rda")

      trajmwss <- multisim(mwssmodel, n_sim)

      save(trajmwss,  file = "tmpdata/trajmwss.Rda")

      trajmwss

    } else {
      # FIX ME POP UP WINDOW
      print("A minimum of two wards is required to run the model")
    }

  })

  # value boxes


  ###### Probability of outbreak
  ## nosocomial infections

    output$nosoH <- renderValueBox({
      valueBox(
        ifelse(class(runmodel()) == "mwss", summary(runmodel(),
                                                    scale = 0,
                                                    focus = "infections")$H$quantiles_noso[["50%"]], ""),
        HTML("Number of nosocomial  <br/> infection among professionals"),
        icon = icon("user-md"), color = "red", width=NULL)
    })

    output$nosoP <- renderValueBox({
      valueBox(
        ifelse(class(runmodel()) == "mwss", summary(runmodel(),
                                                    scale = 0,
                                                    focus = "infections")$P$quantiles_noso[["50%"]], ""),
        HTML("Number of nosocomial  <br/> infection among patients"),
        icon = icon("bed"), color = "red", width=NULL)
    })

    # number of test FIX ME split patient / HCWS
    output$ntestP <- renderValueBox({
      valueBox(
        ifelse(class(runmodel()) == "mwss", summary(runmodel(),
                                                    scale = 0,
                                                    focus = "test")$quantilesP[["50%"]], ""),
        HTML("Number of tests of patients <br/> "),
        icon = icon("exclamation-triangle"), color = "yellow", width=NULL)
    })

    output$ntestH <- renderValueBox({
      valueBox(
        ifelse(class(runmodel()) == "mwss", summary(runmodel(),
                                                    scale = 0,
                                                    focus = "test")$quantilesH[["50%"]], ""),
        HTML("Number of tests of professionals <br/> "),
        icon = icon("exclamation-triangle"), color = "yellow", width=NULL)
    })


    # number of severe cases
    output$nSev <- renderValueBox({
      valueBox(
        ifelse(class(runmodel()) == "mwss", summary(runmodel(),
                                                    scale = 0,
                                                    focus = "incidence")$incidence[, incPS] %>%  median, ""),
        HTML("Number of severe cases <br/>among patients"),
        icon = icon("fire"), color = "red", width=NULL)
    })

    # number of severe cases
    output$ISO <- renderValueBox({
      valueBox(
        ifelse(class(runmodel()) == "mwss", summary(runmodel(),
                                                    scale = 0)$ISO$quantiles[["50%"]], ""),
        HTML("Maximal number of beds <br/>simulataneously under confinement"),
        # "Maximal number of beds simulataneously under confinement",
        icon = icon("bed"), color = "green", width=NULL)
    })

    # number of severe cases
    output$SL <- renderValueBox({
      valueBox(
        ifelse(class(runmodel()) == "mwss", summary(runmodel(),
                                                    scale = 0)$SL$quantiles[["50%"]], ""),
        HTML("Maximal number of professionals <br/>simulataneously in sick leave"),
        icon = icon("user-md"), color = "green", width=NULL)
    })

  #########
  ######### Network plot
  #########

  output$pOutbreak <- renderPlot({

    contacts <- t(data$TS[, ! names(data$TS) %in% c("Worker", "Ward")])
    colnames(contacts) <- data$TS[,1]

    matContact <- mwss::timeShare(contacts, namesincol1 = FALSE)

    plot_pOutbreak(runmodel(), matContact, data$str$P,
                   outb_Thhold = input$outb_Thhold,
                   addtitle = TRUE, verbose = FALSE)
  })

    output$nosoHazard <- renderPlot({

      contacts <- t(data$TS[, ! names(data$TS) %in% c("Worker", "Ward")])
      colnames(contacts) <- data$TS[,1]

      matContact <- mwss::timeShare(contacts, namesincol1 = FALSE)

      plot_nosoHazard(runmodel(), matContact, data$str$P,
                     addtitle = TRUE, verbose = FALSE)
    })


    #########
    ######### Cummulative incidences
    #########

    output$iter_choiceInc <- renderUI({
      selectInput(inputId = "iter_inc",
                  label = "Select a simulation",
                  choices = seq(input$n_sim))
    })

    output$ward_choiceInc <- renderUI({
      selectInput(inputId="ward_inc",
                  label="Select a ward",
                  choices = data$str$Ward)
    })

    output$plotIncidence <- renderPlot({

      ward = FALSE

      if(input$scaleInc == 1 & isTRUE(input$wardInc))
        ward = input$ward_inc

      if(input$iterInc == 1)
        iter = input$iter_inc %>% as.numeric else
          iter = FALSE

      if(input$popInc == "P+H")
        pop = FALSE else
          pop = input$popInc

      plot_incidence(runmodel(),
                     scale = input$scaleInc,
                     pop = pop,
                     iter = iter,
                     ward = ward,
                     display_sd = input$display_sdInc)
    })

  #########
  ######### Daily test boxplot
  #########

    output$iter_choiceTest <- renderUI({
      selectInput(inputId = "iter_test",
                  label = "Select a simulation",
                  choices = seq(input$n_sim))
    })

    output$ward_choiceTest <- renderUI({
      selectInput(inputId="ward_test",
                  label="Select a ward",
                  choices = data$str$Ward)
    })

    output$daysint_choiceTest <- renderUI({
      numericInput(
        'daysint',
        'Calculate median of daily number of test over n-days periods',
        value = 1,
        min = 0,
        max = input$n_days,
        step = 1
      )
    })


    output$plottest <- renderPlot({

      ward = FALSE

      if(input$scaleTest == 1 & isTRUE(input$wardTest))
        ward = input$ward_test


      if(input$iterTest == 1)
        iter = input$iter_test %>% as.numeric else
          iter = FALSE

        if(input$popTest == "P+H")
          pop = NULL else
            pop = input$popTest


        if(input$agrtest == 0)
        daysint = 1 else
          daysint = input$daysint

        plot_test(runmodel(), daysint = daysint, iter = iter, ward = ward, pop = pop, scale = input$scaleTest)

    })


  ############
  ### Test section // parameters
  ############


  # observeEvent(input$variant_id,{
  #
  #   params_dataset <- paste0(input$variant_id, "Params") %>% get
  #
  #   # EPI
  #   updateNumericInput(session, 'PrevCom', value = params_dataset$PrevCom)
  #   updateSliderInput(session, "p", value = params_dataset$p*100)
  #   updateSliderInput(session, "p2", value = params_dataset$p2*100)
  #   updateSliderInput(session, "p2p", value = params_dataset$p2p*100)
  #   updateSliderInput(session, 'pDIC', value = params_dataset$pDIC*100)
  #   updateSliderInput(session, 'pD', value = params_dataset$pD*100)
  #   updateSliderInput(session, 'pT', value = params_dataset$pT)
  #   updateSliderInput(session, 'pSL', value = params_dataset$pSL)
  #   updateSliderInput(session, 'pESL', value = params_dataset$pESL)
  #   updateNumericInput(session, 'gamma1', value = params_dataset$gamma1)
  #   updateNumericInput(session, 'gamma2', value = params_dataset$gamma2)
  #   updateNumericInput(session, 'gamma3', value = params_dataset$gamma3)
  #   updateNumericInput(session, 'gamma4', value = params_dataset$gamma4)
  #   updateNumericInput(session, 'gamma5', value = params_dataset$gamma5)
  #   updateNumericInput(session, 'gammaT', value = params_dataset$gammaT)
  #   updateNumericInput(session, 'gammaBSL', value = params_dataset$gammaBSL)
  #   updateSliderInput(session, "gammaSL", value = c(params_dataset$gammaSL, params_dataset$gammaESL))
  #   updateNumericInput(session, 'Ta', value = params_dataset$Ta)
  #   updateNumericInput(session, 'betaPP', value = params_dataset$betaPP)
  #   updateNumericInput(session, 'betaPH', value = params_dataset$betaPH)
  #   updateNumericInput(session, 'betaHH', value = params_dataset$betaHH)
  #   updateNumericInput(session, 'betaHP', value = params_dataset$betaHP)
  #   updateNumericInput(session, 'betaVP', value = params_dataset$betaVP)
  #
  #   # Test
  #   updateNumericInput(session, 'sensInf', value = params_dataset$sensInf)
  #   updateNumericInput(session, 'sensNInf', value = params_dataset$sensNInf)
  #   updateNumericInput(session, 'sensInfAnt', value = params_dataset$sensInfAnt)
  #   updateNumericInput(session, 'sensIncInfAnt', value = params_dataset$sensIncInfAnt)
  #   updateNumericInput(session, 'sensIncInf', value = params_dataset$sensIncInf)
  #
  #   # Vaccination
  #   updateNumericInput(session, 'PrevVacc', value = params_dataset$PrevVacc)
  #   updateNumericInput(session, 'propV1', value = params_dataset$propV1)
  #   updateNumericInput(session, 'propV2', value = params_dataset$propV2)
  #   updateNumericInput(session, 'pv1p', value = params_dataset$pv1p)
  #   updateNumericInput(session, 'pv1h', value = params_dataset$pv1h)
  #   updateNumericInput(session, 'pv2p', value = params_dataset$pv2p)
  #   updateNumericInput(session, 'pv2h', value = params_dataset$pv2h)
  #   updateNumericInput(session, 'gammav1', value = params_dataset$gammav1)
  #   updateNumericInput(session, 'gammav2', value = params_dataset$gammav2)
  #   updateNumericInput(session, 'pInfV1', value = params_dataset$pInfV1)
  #   updateNumericInput(session, 'pInfV2', value = params_dataset$pInfV2)
  #   updateNumericInput(session, 'psv', value = params_dataset$psv)
  #   updateNumericInput(session, 'p2pv', value = params_dataset$p2pv)
  #   updateNumericInput(session, 'p2v', value = params_dataset$p2v)
  #   updateNumericInput(session, 'pDv', value = params_dataset$pDv)
  #   updateNumericInput(session, 'pTv', value = params_dataset$pTv)
  #
  #   # Surveillance and control
  #   updateNumericInput(session, 'gammapreIso', value = params_dataset$gammapreIso)
  #   updateNumericInput(session, 'gammaIso', value = params_dataset$gammaIso)
  #   updateNumericInput(session, 'nHCWS_AL', value = params_dataset$nHCWS_AL)
  #   updateNumericInput(session, 'timeExCli', value = params_dataset$timeExCli)
  #   updateNumericInput(session, 'timeTest', value = params_dataset$timeTest)
  #   updateNumericInput(session, 'betaPP_AL', value = params_dataset$betaPP_AL)
  #   updateNumericInput(session, 'betaPH_AL', value = params_dataset$betaPH_AL)
  #   updateNumericInput(session, 'betaHH_AL', value = params_dataset$betaHH_AL)
  #   updateNumericInput(session, 'betaHP_AL', value = params_dataset$betaHP_AL)
  #   # updateNumericInput(session, 'regHtint', value = params_dataset$regHtint)
  #   # updateNumericInput(session, 'regPtint', value = params_dataset$regPtint)
  #   updateNumericInput(session, 'sensInf', value = params_dataset$sensInf)
  #   updateNumericInput(session, 'sensNInf', value = params_dataset$sensNInf)
  #
  # })


  ############
  ### Test section
  ############


  output$IMMstateDT <- DT::renderDT({

    if(!is.null(data$IMMstate)){

      IMMstateDT <- data$IMMstate

      IMMstateDT[, n := sum(n), by = list(ward, pop, imm)]
      IMMstateDT %<>% unique
      IMMstateDT[, prop := n %>% divide_by(sum(n)) %>% multiply_by(100) %>% round, by = list(ward, pop)]
      IMMstateDT[, prop := paste(prop, "%")]

      IMMstateDT[imm == "NI", imm := "Non immune"]
      IMMstateDT[imm == "PI", imm := "Partially immune"]
      IMMstateDT[imm == "FI", imm := "Fully immune"]

      IMMstateDT %<>% .[order(ward,pop, imm),]

      colnames(IMMstateDT) <- c("ward", "Population","Immunity level", "Number of ind.", "Proportion")

      DT::datatable(
        IMMstateDT,
        rownames = F,
        escape = FALSE,
        # editable = TRUE,
        options = list(pageLength = 10)
      )


    }

  })

  observeEvent(input$deleteIMMstate,{

    if (!is.null(input$IMMstateDT_rows_selected)) {

      data$IMMstateDT <- data$IMMstateDT[-as.numeric(input$IMMstateDT_rows_selected),]
    }
  })

  ########### Help boxes

  observe_helpers(session = shiny::getDefaultReactiveDomain(),
                  help_dir = "helpfiles", withMathJax = FALSE)


  ####### Generating downloadable reports

  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)

      # Set up parameters to pass to Rmd document

      mwssoutput <- runmodel()

      contacts <- t(data$TS[, ! names(data$TS) %in% c("Worker", "Ward")])
      colnames(contacts) <- data$TS[,1]

      matContact <- mwss::timeShare(contacts, namesincol1 = FALSE)
      test_str <- input$test_str

      params <- list(mwssoutput = mwssoutput,
                     matContact = matContact,
                     test_str = test_str,
                     P = data$str$P
      )


      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )

  #### Perspectives
  # Un panneau pour le partage des soignants
  # Un panneau pour les param?tres ?pid?mio
  # upload and download data

}
