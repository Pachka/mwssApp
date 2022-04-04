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


  # params <- shiny::reactiveValues(
  # ls = list(
  #   betaPP     = numeric(),
  #   betaHH     = numeric(),
  #   betaPH     = numeric(),
  #   betaHP     = numeric(),
  #   betaVP     = numeric(),
  #   betaPP_AL  = numeric(),
  #   betaHH_AL  = numeric(),
  #   betaPH_AL  = numeric(),
  #   betaHP_AL  = numeric(),
  #   Ta = numeric(),
  #   gamma1 = numeric(),
  #   gamma2 = numeric(),
  #   gamma3 = numeric(),
  #   gamma4 = numeric(),
  #   gamma5 = numeric(),
  #   gammaT = numeric(),
  #   gammaBSL = numeric(),
  #   gammaSL = numeric(),
  #   gammaESL = numeric(),
  #   gammapreIso = numeric(),
  #   gammaIso= numeric(),
  #   timeExCli = numeric(),
  #   timeTest = numeric(),
  #   nHCWS_AL = numeric(),
  #   p = numeric(),
  #   p2 = numeric(),
  #   p2p = numeric(),
  #   pDIC = numeric(),
  #   pD = numeric(),
  #   pT = numeric(),
  #   pSL = numeric(),
  #   pESL = numeric(),
  #   sensInf = numeric(),
  #   sensNInf = numeric(),
  #   sensInfAnt = numeric(),
  #   sensIncInfAnt = numeric(),
  #   sensIncInf = numeric(),
  #   PrevCom = numeric(),
  #   PrevVacc = numeric(),
  #   propV1 = numeric(),
  #   propV2 = numeric(),
  #   pv1p = input$pv1p,
  #   pv1h = numeric(),
  #   pv2p = numeric(),
  #   pv2h = numeric(),
  #   gammav1 = numeric(),
  #   gammav2 = numeric(),
  #   pInfV1 = numeric(),
  #   pInfV2 = numeric(),
  #   psv = numeric(),
  #   p2pv = numeric(),
  #   p2v = numeric(),
  #   pDv = numeric(),
  #   pTv = numeric(),
  #   gammaVin = numeric(),
  #   gammaVout = numeric()
  # )
  # )

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

      # print(input$H_pop_sizeNEW - popH)

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

  output$TS = renderDT(data$TS,
                       # filter = 'top',
                       selection = 'none',
                       rownames = FALSE,
                       editable = TRUE,
                       options = list(pageLength = 10))

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
  #
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


  ############
  ### Simulation section
  ############

  runmodel <- eventReactive(input$run, {

    if(data$str$Ward %>% length >= 2){
      ward <- data$str$Ward
      pop_size_P <- data$str$P %>% as.numeric
      pop_size_H <- data$str$H %>% as.numeric
      nVisits <- data$str$Visits %>% as.numeric %>% divide_by(7)
      LS <- data$str$Turnover %>% as.numeric # FIX ME adjust
      if(input$airlockEffective)
        nHCWS_AL <- input$nHCWS_AL

      IMMstate <- data$IMMstate

      save(ward, file = "ward.Rda")
      save(pop_size_P, file = "pop_size_P.Rda")
      save(pop_size_H, file = "pop_size_H.Rda")
      save(nVisits, file = "nVisits.Rda")
      save(LS, file = "LS.Rda")
      save(IMMstate, file = "IMMstate.Rda")

      xstart <- startvec(ward_names = ward,
                         pop_size_P = pop_size_P,
                         pop_size_H = pop_size_H,
                         nVisits = nVisits,
                         LS = LS,
                         IMMstate = IMMstate %>% as.data.frame,
                         # EPIstate = data$EPIstate,
                         AL = input$airlockEffective,
                         AL_nHCWS = nHCWS_AL)

      # print(xstart)

      # nombre de contacts
      # βPP = βPH = β, βHH = 0.25 × β and βHP = (Nh/Np) × β
      # 0.25 >> un soignant est là 25% du temps (42h / semaine)
      # Np x βHP = Nh × βPH >> nombre de contacts reciproque soignants vers patient et patients vers soignants
      # FIX ME:
      # β >> nombre de contact/pers x durée moyenne de contact x proba de transmission / unité de temps

      if(isFALSE(input$allBetaW)){
        betaPP     = input$betaPP
        betaHH     = input$betaPP * 0.25
        betaPH     = input$betaPP
        betaHP     = input$betaPP
      } else {
        betaPP     = input$betaPP
        betaHH     = input$betaHH
        betaPH     = input$betaPH
        betaHP     = input$betaHP
      }

      if(isFALSE(input$allBetaAL)){
        betaPP_AL     = input$betaPP_AL
        betaHH_AL     = input$betaPP_AL * 0.25
        betaPH_AL     = input$betaPP_AL
        betaHP_AL     = input$betaPP_AL
      } else {
        betaPP_AL     = input$betaPP_AL
        betaHH_AL     = input$betaHH_AL
        betaPH_AL     = input$betaPH_AL
        betaHP_AL     = input$betaHP_AL
      }

      params_dataset = list(
        betaPP     = betaPP,
        betaHH     = betaHH,
        betaPH     = betaPH,
        betaHP     = betaHP,
        betaVP     = input$betaVP,
        betaPP_AL  = betaPP_AL,
        betaHH_AL  = betaHH_AL,
        betaPH_AL  = betaPH_AL,
        betaHP_AL  = betaHP_AL,
        Ta = input$Ta,
        gamma1 = 1/ input$gamma1,
        gamma2 = 1/ input$gamma2,
        gamma3 = 1/ input$gamma3,
        gamma4 = 1/ input$gamma4,
        gamma5 = 1/ input$gamma5,
        gammaT = 1/ input$gammaT,
        gammaBSL = 1/ input$gammaBSL,
        gammaSL = 1/ input$gammaSL[1],
        gammaESL = 1/ input$gammaSL[2],
        gammapreIso = 1 / input$gammapreIso,
        gammaIso = 1 / input$gammaIso,
        timeExCli = input$timeExCli,
        timeTest = input$timeTest,
        nHCWS_AL = input$nHCWS_AL,
        p = input$p / 100,
        p2 = input$p2 / 100,
        p2p = input$p2p / 100,
        pDIC = input$pDIC / 100,
        pD = input$pD / 100,
        pT = input$pT / 100,
        pSL = input$pSL / 100,
        pESL = input$pESL / 100,
        sensInf = input$sensInf,
        sensNInf = input$sensNInf,
        sensInfAnt = input$sensInfAnt,
        sensIncInfAnt = input$sensIncInfAnt,
        sensIncInf = input$sensIncInf,
        PrevCom = input$PrevCom,
        PrevVacc = input$PrevVacc,
        propV1 = input$propV1,
        propV2 = input$propV2,
        pv1p = input$pv1p,
        pv1h = input$pv1h,
        pv2p = input$pv2p,
        pv2h = input$pv2h,
        gammav1 = input$gammav1,
        gammav2 = input$gammav2,
        pInfV1 = input$pInfV1,
        pInfV2 = input$pInfV2,
        psv = input$psv,
        p2pv = input$p2pv,
        p2v = input$p2v,
        pDv = input$pDv,
        pTv = input$pTv,
        gammaVin = input$gammaVin, ### FIX ME
        gammaVout = input$gammaVout ### FIX ME
      )

      contacts <- t(data$TS[, ! names(data$TS) %in% c("Worker", "Ward")])
      colnames(contacts) <- data$TS[,1]

      matContact <- mwss::timeShare(contacts, namesincol1 = FALSE)

      params_dataset %<>% lapply(., as.numeric)

      # print(params_dataset)
      # print(xstart)
      # print(matContact)
      # print(input$n_sim)
      # print(input$n_days)
      # print(input$isoWard)
      # print(input$airlockEffective)
      # print(input$scenario)
      # print(input$test_str)
      # print(input$Vaccination)

      save(params_dataset, file = "data/test_data/params_dataset.Rda" )
      save(xstart, file = "data/test_data/xstart.Rda")
      save(matContact, file = "data/test_data/matContact.Rda")
      n_sim <- input$n_sim
      save(n_sim, file = "data/test_data/n_sim.Rda")
      n_days <- input$n_days
      save(n_days, file = "data/test_data/n_days.Rda")
      isoWard <- input$isoWard
      save(isoWard, file = "data/test_data/isoWard.Rda")
      airlockEffective <- input$airlockEffective
      save(airlockEffective, file = "data/test_data/airlockEffective.Rda")
      scenario <- input$scenario
      save(scenario, file = "data/test_data/scenario.Rda")
      test_str <- input$test_str
      save(test_str, file = "data/test_data/test_str.Rda")
      Vaccination <- input$Vaccination
      save(Vaccination, file = "data/test_data/Vaccination.Rda")

      if(input$scenario == "FALSE")
        output <- mwss(params_dataset,
                       xstart,
                       matContact,
                       nSimulations = input$n_sim,
                       duration = input$n_days,
                       isoWard = input$isoWard,
                       airlockEffective = input$airlockEffective,
                       scenario =  FALSE,
                       test_str = input$test_str,
                       Vaccination = input$Vaccination) else
                         output <- mwss(params_dataset,
                                        xstart,
                                        matContact,
                                        nSimulations = input$n_sim,
                                        duration = input$n_days,
                                        isoWard = input$isoWard,
                                        airlockEffective = input$airlockEffective,
                                        scenario =  input$scenario,
                                        test_str = input$test_str,
                                        Vaccination = input$Vaccination)

      save(output, file = "data/test_data/output.Rda")
      output

    } else {
      # FIX ME POP UP WINDOW
      print("A minimum of two wards is required to run the model")
    }

  })

  # value boxes

  ###### Probability of outbreak


  ###### Average number of transmissions
  output$nTransmission <- renderValueBox({
    valueBox(summary(runmodel(), input$test_str)$TrSec$quantiles[["50%"]],
             "Median number of contaminations inside the facility",
             icon = icon("fire"), color = "yellow", width=NULL)
  })

  ###### Number of test
  output$nTest <- renderValueBox({
    valueBox(summary(runmodel(), input$test_str)$nTestsHosp$quantiles[["50%"]],
             "Median number of tests performed in the facility", icon = icon("exclamation-triangle"), color = "orange", width=NULL)
  })

  ###### Incidences
  #### Patients
  output$incidenceP <- renderValueBox({
    valueBox(summary(runmodel(), input$test_str)$incidence$quantiles["50%","P"],
             "Median incidence among patients",
             icon = icon("bed"), color = "green", width=NULL)
  })

  #### HCWS
  output$incidenceH <- renderValueBox({
    valueBox(summary(runmodel(), input$test_str)$incidence$quantiles["50%","H"],
             "Median incidence among healthcare workers",
             icon = icon("user-md"), color = "green", width=NULL)
  })

  #########
  ######### Network plot
  #########

  output$pOutbreak <- renderPlot({

    contacts <- t(data$TS[, ! names(data$TS) %in% c("Worker", "Ward")])
    colnames(contacts) <- data$TS[,1]

    matContact <- mwss::timeShare(contacts, namesincol1 = FALSE)

    plot_pOutbreak(runmodel(), matContact, data$str$P)#, verbose = FALSE)

  })

  #########
  ######### Daily test boxplot
  #########

  output$plotDailyTest <- renderPlot({

    plotDailyTest(summary(runmodel(), input$test_str)$nDailyTestsHosp$perSim, startday = FALSE)

  })

  output$plotIncidence <- renderPlot({
    plotcumInc(runmodel())
  })

  #########
  ######### Trajectories of cumulative incidences ?
  #########



  ############
  ### Test section
  ############


  observeEvent(input$variant_id,{

    params_dataset <- paste0(input$variant_id, "Params") %>% get

    # EPI
    updateNumericInput(session, 'PrevCom', value = params_dataset$PrevCom)
    updateSliderInput(session, "p", value = params_dataset$p*100)
    updateSliderInput(session, "p2", value = params_dataset$p2*100)
    updateSliderInput(session, "p2p", value = params_dataset$p2p*100)
    updateSliderInput(session, 'pDIC', value = params_dataset$pDIC*100)
    updateSliderInput(session, 'pD', value = params_dataset$pD*100)
    updateSliderInput(session, 'pT', value = params_dataset$pT)
    updateSliderInput(session, 'pSL', value = params_dataset$pSL)
    updateSliderInput(session, 'pESL', value = params_dataset$pESL)
    updateNumericInput(session, 'gamma1', value = params_dataset$gamma1)
    updateNumericInput(session, 'gamma2', value = params_dataset$gamma2)
    updateNumericInput(session, 'gamma3', value = params_dataset$gamma3)
    updateNumericInput(session, 'gamma4', value = params_dataset$gamma4)
    updateNumericInput(session, 'gamma5', value = params_dataset$gamma5)
    updateNumericInput(session, 'gammaT', value = params_dataset$gammaT)
    updateNumericInput(session, 'gammaBSL', value = params_dataset$gammaBSL)
    updateSliderInput(session, "gammaSL", value = c(params_dataset$gammaSL, params_dataset$gammaESL))
    updateNumericInput(session, 'Ta', value = params_dataset$Ta)
    updateNumericInput(session, 'betaPP', value = params_dataset$betaPP)
    updateNumericInput(session, 'betaPH', value = params_dataset$betaPH)
    updateNumericInput(session, 'betaHH', value = params_dataset$betaHH)
    updateNumericInput(session, 'betaHP', value = params_dataset$betaHP)
    updateNumericInput(session, 'betaVP', value = params_dataset$betaVP)

    # Test
    updateNumericInput(session, 'sensInf', value = params_dataset$sensInf)
    updateNumericInput(session, 'sensNInf', value = params_dataset$sensNInf)
    updateNumericInput(session, 'sensInfAnt', value = params_dataset$sensInfAnt)
    updateNumericInput(session, 'sensIncInfAnt', value = params_dataset$sensIncInfAnt)
    updateNumericInput(session, 'sensIncInf', value = params_dataset$sensIncInf)

    # Vaccination
    updateNumericInput(session, 'PrevVacc', value = params_dataset$PrevVacc)
    updateNumericInput(session, 'propV1', value = params_dataset$propV1)
    updateNumericInput(session, 'propV2', value = params_dataset$propV2)
    updateNumericInput(session, 'pv1p', value = params_dataset$pv1p)
    updateNumericInput(session, 'pv1h', value = params_dataset$pv1h)
    updateNumericInput(session, 'pv2p', value = params_dataset$pv2p)
    updateNumericInput(session, 'pv2h', value = params_dataset$pv2h)
    updateNumericInput(session, 'gammav1', value = params_dataset$gammav1)
    updateNumericInput(session, 'gammav2', value = params_dataset$gammav2)
    updateNumericInput(session, 'pInfV1', value = params_dataset$pInfV1)
    updateNumericInput(session, 'pInfV2', value = params_dataset$pInfV2)
    updateNumericInput(session, 'psv', value = params_dataset$psv)
    updateNumericInput(session, 'p2pv', value = params_dataset$p2pv)
    updateNumericInput(session, 'p2v', value = params_dataset$p2v)
    updateNumericInput(session, 'pDv', value = params_dataset$pDv)
    updateNumericInput(session, 'pTv', value = params_dataset$pTv)

    # Surveillance and control
    updateNumericInput(session, 'gammapreIso', value = params_dataset$gammapreIso)
    updateNumericInput(session, 'gammaIso', value = params_dataset$gammaIso)
    updateNumericInput(session, 'nHCWS_AL', value = params_dataset$nHCWS_AL)
    updateNumericInput(session, 'timeExCli', value = params_dataset$timeExCli)
    updateNumericInput(session, 'timeTest', value = params_dataset$timeTest)
    updateNumericInput(session, 'betaPP_AL', value = params_dataset$betaPP_AL)
    updateNumericInput(session, 'betaPH_AL', value = params_dataset$betaPH_AL)
    updateNumericInput(session, 'betaHH_AL', value = params_dataset$betaHH_AL)
    updateNumericInput(session, 'betaHP_AL', value = params_dataset$betaHP_AL)
    # updateNumericInput(session, 'regHtint', value = params_dataset$regHtint)
    # updateNumericInput(session, 'regPtint', value = params_dataset$regPtint)
    updateNumericInput(session, 'sensInf', value = params_dataset$sensInf)
    updateNumericInput(session, 'sensNInf', value = params_dataset$sensNInf)

  })


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
