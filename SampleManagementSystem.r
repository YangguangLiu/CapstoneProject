# cleaning R memory
rm(list=ls())

library(shiny)
library(DBI)
library(RMySQL)
library(RSQLite)
library(gWidgets)
library(shinythemes)
library(dbConnect)
library(DT)
library(ggplot2)
library(rmarkdown)
library(tinytex)

Logged = FALSE

sqlitePath <- "db/SampleManagementSystem.db"

`%then%` <- shiny:::`%OR%`

# Submitter's UI
ui_submitter = function(){
  fluidPage(theme = shinytheme("united"),
           # Application title
           navbarPage("Experimental Result Query System for Submitter",
                      tabPanel("Result Query",
                               sidebarPanel(
                                 selectInput(inputId = "selectAnalyte_user",
                                             label = "Select Analyte", 
                                             choices = c("HDL", "LDL", "Triglycerides")),
                                 actionButton("query_Analyteresult","Result Query",
                                              width = "100%",
                                              icon = icon("table")),
                                 br(), br(),
                                 downloadButton("downloadQueryData", 
                                                "Download Query Result", 
                                                style = "width:100%")
                               ),
                               mainPanel(
                                 br(),
                                 dataTableOutput("queryAnalyteinfo")
                               )),
                      tabPanel("Log Out",
                               actionButton("logoutbtn","Log Out", 
                                            width = "300px", 
                                            icon = icon("sign-out-alt"))
                      )),
           
           mainPanel()
)
}

# Analyst's UI
ui_analyst = function(){
  fluidPage(theme = shinytheme("cerulean"),
            # Application title
           navbarPage("Data Management System for Analyst",
                      tabPanel("Standard Data",
                               sidebarPanel(
                                 fileInput(inputId = "stddatafile", 
                                           label = em("Choose a standard data file"), 
                                           accept = c('.csv')),
                                 helpText(em("Note: please do not click the below button 
                                          if you already submitted your standard data 
                                          to the database.")),
                                 actionButton("submit_stddata","Submit To Database",
                                              width = "100%",
                                              icon = icon("save")),
                                 br(),
                                 hr(),
                                 selectInput(inputId = "stdAnalyte", 
                                             label = "Select Analyte",
                                             choices = c("HDL", "LDL", "Triglycerides")),
                                 actionButton("plot_stdAnalytedatabtn",
                                              "Plot Standard Analyte", 
                                              width = "100%",
                                              icon = icon("bar-chart-o"))
                               ),
                               mainPanel(
                                 dataTableOutput("stddatainfo"),
                                 br(),
                                 plotOutput("stdAnalytedataplot")
                               )),
                      tabPanel("Raw Data",
                               sidebarPanel(
                                 actionButton("show_allrawdatabtn",
                                              "Display Raw Data", 
                                              width = "100%",
                                              icon = icon("table"))
                               ),
                               mainPanel(
                                 br(),
                                 dataTableOutput("allrawdatainfo")
                               )),
                      tabPanel("Data Analysis",
                               sidebarPanel(
                                 selectInput(inputId = "selectAnalyte", 
                                             label = "Select Analyte", 
                                             choices = c("HDL", "LDL", "Triglycerides")),
                                 actionButton("linear_regressionbtn",
                                              "Linear Regression Model",
                                              width = "100%",
                                              icon = icon("list-alt")),
                                 br(),
                                 br(),
                                 actionButton("pred_resultbtn",
                                              "Predict Results", 
                                              width = "100%",
                                              icon = icon("table")),
                                 br(),
                                 br(),
                                 actionButton("submit_preddatabtn",
                                              "Submit To Database",
                                              width = "100%",
                                              icon = icon("save"))
                               ),
                               mainPanel(
                                 verbatimTextOutput("summary_lm"),
                                 dataTableOutput("pred_resultinfo")
                               )),
                      tabPanel("Results",
                               sidebarPanel(
                                 actionButton("allfinalresultsbtn",
                                              "Show Analyzed Results",
                                              width = "100%",
                                              icon = icon("table")),
                                 br(),
                                 br(),
                                 downloadButton("analystdownloadData", 
                                                "Download Final Result", 
                                                style = "width:100%"),
                                 br(),
                                 hr(),
                                 radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                                              inline = TRUE),
                                 downloadButton("download_analyst_report", 
                                                "Download Final Report", 
                                                style = "width:100%")),
                               mainPanel(
                                 dataTableOutput("allfinaldatainfo")
                               )
                      ),
                      tabPanel("Log Out",
                               actionButton("logoutbtn","Log Out",
                                            width = "300px", 
                                            icon = icon("sign-out-alt"))
                      )),
           
           mainPanel()
)
}

# Staff's UI
ui_staff = function(){
  fluidPage(theme = shinytheme("yeti"),
            # Application title
           navbarPage("Data Management System for Staff",
                      tabPanel("Submit Data",
                               sidebarPanel(
                                 fileInput(inputId = "rawdatafile", 
                                           label = em("Select a raw data file"), 
                                           accept = c('.csv')),
                                 actionButton("submit_rawdata",
                                              "Submit To Database", 
                                              width = "100%", 
                                              icon = icon("save"))
                               ),
                               mainPanel(
                                 br(),
                                 dataTableOutput("rawdatainfo")
                               )),
                      tabPanel("Results",
                               sidebarPanel(
                                 actionButton("finalresultsbtn",
                                              "Show Data Results", 
                                              icon = icon("th"), 
                                              width = "100%"),
                                 br(),
                                 br(),
                                 downloadButton("downloadData", 
                                                "Download Data Results", 
                                                style = "width:100%")
                                 ),
                               mainPanel(
                                 dataTableOutput("finaldatainfo")
                               )
                      ),
                      tabPanel("Log Out",
                               actionButton("logoutbtn","Log Out", 
                                            width = "300px",
                                            icon = icon("sign-out-alt"))
                      )),
           
           mainPanel()
)
}

# Log In/Sign Up UI  
ui_loginsignup = function(){
  fluidPage(theme = shinytheme("superhero"),
            # Application title
           navbarPage("Sample Management System",
                      tabPanel("Log In",
                               textInput("userName",  placeholder="Username", 
                                         label = tagList(icon("user"), "Username")),
                               passwordInput("passwd", placeholder="Password", 
                                             label = tagList(icon("unlock-alt"), "Password")),
                               br(),
                               actionButton("login","Log in", width = "300px", 
                                            icon = icon("sign-in-alt"), class = "btn-primary")
                               ),
                      tabPanel("Sign Up",
                               textInput("newUserId", "User ID",""),
                               textInput("firstName", "First Name",""),
                               textInput("lastName", "Last Name",""),
                               passwordInput("password", "Password"),
                               selectInput("role", "Role", choices = c("Submitter", "Analyst","Staff")),
                               actionButton("submit","Submit", width = "300px", class = "btn-primary"),
                               dataTableOutput("tableshow")
                      )),
           
           mainPanel()
  )
}
  
server = function(input, output, session) {
  USER <- reactiveValues(Logged = Logged)

  data <- eventReactive(input$submit, {
    con <- dbConnect(SQLite(), dbname=sqlitePath)
    
    validate(
      need(input$newUserId != "", "Please input your User ID") %then%
        need(!is.null(input$newUserId), "User ID is an empty string!"),
      
      need(input$firstName != "", "Please input your First Name") %then%
        need(!is.null(input$firstName), "First Name is an empty string!"),
      
      need(input$lastName != "", "Please input your Last Name") %then%
        need(!is.null(input$lastName), "Last Name is an empty string!"),
      
      need(input$password != "", "Please input your Password") %then%
        need(!is.null(input$password), "Password is an empty string!"),
      
      errorClass = "myClass"
    )
    
    input_username <- isolate(input$newUserId)
    input_role <- isolate(input$role)
    
    queryUser <- sprintf({"SELECT rowid FROM userdata WHERE userID='%s'"}, 
                     input_username,
                     serialize = F)
    queryRole_Analyst <- sprintf({"SELECT rowid FROM userdata WHERE role='Analyst'"},
                                 serialize = F)

    user <- dbGetQuery(con, queryUser)
    role_Analyst <- dbGetQuery(con, queryRole_Analyst)

    if (length(user$rowid) == 1)
      showModal(modalDialog(
        title = "Warning message",
        "User ID already exists, please choose a different userID.",
        easyClose = TRUE,
        footer = NULL
      ))
    else if (input_role == 'Analyst' && length(role_Analyst$rowid) == 1)
      showModal(modalDialog(
        title = "Warning message",
        "Analyst already exists, only one Analyst role is allowed. Please choose another role.",
        easyClose = TRUE,
        footer = NULL
      ))
    else {
      # put the user's information into the table "userdata" of database project
      dbWriteTable(con, 
                   "userdata", 
                   data.frame(userID = input$newUserId,
                              first_Name = input$firstName,
                              last_Name = input$lastName,
                              password = input$password,
                              role = input$role,
                              created_data = format(Sys.Date(),"20%y-%m-%d"),
                              stringsAsFactors = FALSE), 
                   append = TRUE)
      
      data <- dbReadTable(con, "userdata")
      dbDisconnect(con)
      
      # USER$Logged <-  TRUE
      if (input$role == "Staff"){
        output$page <- renderUI({
          div(class="outer", do.call(bootstrapPage,c("",ui_staff()))) # Staff
        })
      } else if (input$role == "Submitter"){
        output$page <- renderUI({
          div(class="outer", do.call(bootstrapPage,c("",ui_submitter()))) # Submitter
        })
      } else {
        output$page <- renderUI({
          div(class="outer", do.call(bootstrapPage,c("",ui_analyst()))) # Analyst
        })
      }
        
      return(data)

     }

  })
  
  output$tableshow <- renderDataTable({data()},
                                      options = list(
                                        pageLength = 10,
                                        scrollX = TRUE,
                                        dom = 'lftip',
                                        initComplete = JS(
                                          "function(settings, json) {",
                                          "$(this.api().table().header()).css({
                                          'background-color': '#000', 'color': '#fff'});",
                                          "}")
                                      )
                                  )
  
  observe({
    if(USER$Logged == FALSE){
      if(!is.null(input$login)){
        if(input$login>0){
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          inputquery <- sprintf({"SELECT rowid 
            FROM userdata 
            WHERE userID='%s' and password = '%s'"}, 
                                Username,Password,serialize = F)
          db <- dbConnect(SQLite(), dbname=sqlitePath)
          inputuser <- dbGetQuery(db, inputquery)
          
          
          if (length(inputuser$rowid) == 1){
            # USER$Logged <-  TRUE
            
            # Staff
            inputRolequery_Staff <- sprintf({"SELECT rowid 
            FROM userdata 
            WHERE userID='%s' and role = 'Staff'"}, 
                                      Username, serialize = F)
            inputuserRole_Staff <- dbGetQuery(db, inputRolequery_Staff)
            
            # Submitter
            inputRolequery_Submitter <- sprintf({"SELECT rowid 
            FROM userdata 
            WHERE userID='%s' and role = 'Submitter'"}, 
                                                Username, serialize = F)
            inputuserRole_Submitter <- dbGetQuery(db, inputRolequery_Submitter)
            
            if (length(inputuserRole_Staff$rowid) == 1){
              output$page <- renderUI({
                div(class="outer", do.call(bootstrapPage,c("",ui_staff()))) # Staff
              })
            } else if (length(inputuserRole_Submitter$rowid) == 1){
              output$page <- renderUI({
                div(class="outer", do.call(bootstrapPage,c("",ui_submitter()))) # Submitter
              })
            } else {
              output$page <- renderUI({
                div(class="outer", do.call(bootstrapPage,c("",ui_analyst()))) # Analyst
              })
            }

          }
          dbDisconnect(db)
        }
      }
    }
  })
  
###########Submit Raw Data ######################
  # determine username
  submitter_name <- reactive({
    if(!is.null(input$userName))
      input$userName
    else
      input$newUserId
  })

  # Read Raw Data 
  rawdataInput <- reactive({
    rawdatainFile <- input$rawdatafile
    
    if(is.null(rawdatainFile))
      return(NULL)
    read.csv(rawdatainFile$datapath)
  })
  
  #Display Raw Data
  output$rawdatainfo <- renderDataTable({
    if(is.null(rawdataInput())) 
      {return ()}
    rawdataInput()
  },
  options = list(
    pageLength = 10,
    scrollX = TRUE,
    dom = 'lftip',
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
      "}")
  )
)
  
  #Save Raw Data to database.
  observeEvent(input$submit_rawdata, {
    if(is.null(rawdataInput()))
    {
      showModal(modalDialog(
      title = "Warning message",
      "Please submit your raw data first.",
      easyClose = TRUE,
      footer = NULL
    ))
      return ()
      }
    else
    {
      db_sampledata <- dbConnect(SQLite(), dbname=sqlitePath)

      rawdata_1 <- read.csv(input$rawdatafile$datapath)
      submit_rawdata_df <- cbind(rawdata_1, 
                                 date_submitted = rep(format(Sys.Date(),"20%y-%m-%d")))
      # table of dampledata already created in SQLite database by the following syntax
      # CREATE TABLE sampledata (sample_id TEXT, analyte_name TEXT, expected_raw NUMERIC, submitter TEXT, date_submitted TEXT);
      dbWriteTable(db_sampledata, "sampledata", submit_rawdata_df, append =TRUE, row.names=FALSE)
      dbDisconnect(db_sampledata)
      
      showModal(modalDialog(
        title = "Warning message",
        "Sample data stored in database.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  # Show data analysis results provided by Analyst 
  # retrieve analyzed data
  staff_analysis_result <- eventReactive(input$finalresultsbtn, {
    db_analyzeddata <- dbConnect(SQLite(), dbname=sqlitePath)

    result_query <- sprintf({"SELECT * FROM finalresultdata"},
                            serialize = F)
    
    result_query_values <- dbGetQuery(db_analyzeddata, result_query)
    dbDisconnect(db_analyzeddata)
    result_query_values
  })
  
  # Display analyzed data
  output$finaldatainfo <- renderDataTable({
    staff_analysis_result()
  },
  options = list(
    pageLength = 10,
    scrollX = TRUE,
    dom = 'lftip',
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
      "}")
  )
)

  # Download analyzed data
  output$downloadData <- downloadHandler(
    
    filename = function() {
      paste(submitter_name(), ".csv", sep = "")
    },
    
    content = function(file) {
      write.csv(staff_analysis_result(), file, row.names = FALSE)
    }
  )

################################################# 
  
########### Analyst Account ######################    
########### Submit Standard Data ######################
  # Read Standard Data
  stddataInput <- reactive({
    stddatainFile <- input$stddatafile
    
    if(is.null(stddatainFile))
      return(NULL)
    read.csv(stddatainFile$datapath)
  })
  
  #Display Standard Data
  output$stddatainfo <- renderDataTable({
    if(is.null(stddataInput())) 
    {return ()}
    stddataInput()
  },
  options = list(
    pageLength = 5,
    scrollX = TRUE,
    dom = 'lftip',
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
      "}")
  )
)
  
  #Save Standard Data to database.
  observeEvent(input$submit_stddata, {
    if(is.null(stddataInput()))
    {return ()}
    else
    {
      db_stddata <- dbConnect(SQLite(), dbname=sqlitePath)

      stddata_1 <- read.csv(input$stddatafile$datapath)
      submit_stddata_df <- cbind(stddata_1, date_submitted = rep(format(Sys.Date(),"20%y-%m-%d")))
      # table of standarddata already created in SQLite database by the following syntax
      # CREATE TABLE standarddata (std_id TEXT, expected_raw NUMERIC, result NUMERIC, result_unit Text, date_submitted TEXT);
      dbWriteTable(db_stddata, "standarddata", submit_stddata_df, append =TRUE, row.names=FALSE)
      dbDisconnect(db_stddata)
      
      showModal(modalDialog(
        title = "Warning message",
        "Standard data successfully saved to database.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
  })
  
  selectedAnalyte <- reactive({
    input$stdAnalyte
  })
  
  # Draw plot for the selected standard Analyte
  p_std <- eventReactive(input$plot_stdAnalytedatabtn, {
    if(is.null(stddataInput()))
      {return ()}
    else
    {
      stddata_selected <- dplyr::filter(stddataInput(), grepl(selectedAnalyte(), std_id))
      
      p <- ggplot(data = stddata_selected, 
             aes(stddata_selected$expected_raw, stddata_selected$result)) + 
        geom_point() + 
        labs(title = paste("Standard plot of Analyte", selectedAnalyte()),
             x = paste("Expected raw data of Analyte", selectedAnalyte()),
             y = paste("Final result of Analyte", selectedAnalyte()))
      return(p)
    }
  })
  
  output$stdAnalytedataplot <- renderPlot({
    p_std()
  })

#################################################
######################## Show all raw data for Analyst #############
  all_raw_result <- reactive({
    db_sampledata <- dbConnect(SQLite(), dbname=sqlitePath)
    
    all_raw_query <- sprintf({"SELECT * FROM sampledata"},
                            serialize = F)
    
    all_raw_values <- dbGetQuery(db_sampledata, all_raw_query)
    dbDisconnect(db_sampledata)
    all_raw_values
  })
  
  # Display analyzed data
  observeEvent(input$show_allrawdatabtn, {
    output$allrawdatainfo <- renderDataTable({
      all_raw_result()
    },
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      dom = 'lftip',
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
        "}")
    )
  )
  })

#########################################################
################### Data Analysis by Analyst ####################
  # Read standard data from database
  standard_data <- reactive({
    db_std_data <- dbConnect(SQLite(), dbname=sqlitePath)
    
    std_data_query <- sprintf({"SELECT * FROM standarddata"},
                             serialize = F)
    
    all_std_values <- dbGetQuery(db_std_data, std_data_query)
    dbDisconnect(db_std_data)
    all_std_values
  })
  
  # Select Analyte
  select_Analyte <- reactive({
    input$selectAnalyte
  })
  
  # Standard data in database:
  stddata_analyte <- reactive({
    dplyr::filter(standard_data(), grepl(select_Analyte(), std_id))
  }) 
  
  # Raw data in database: all_raw_result()
  sampledata_analyte <- reactive({
    dplyr::filter(all_raw_result(), grepl(select_Analyte(), analyte_name))
  }) 
  
  # building the linear regression model
  linearMod <-eventReactive(input$linear_regressionbtn, {
    lm(result ~ expected_raw, data = stddata_analyte())
  })
  
  # expected_raw data used to predict final result
  newdata <- reactive({
    data.frame(expected_raw = sampledata_analyte()$expected_raw)
  }) 
  
  # predict final result
  resultPred <- reactive({
    predict(linearMod(), newdata = newdata())
  }) 
  
  # predicted final result table
  pred_dataresult_df <- eventReactive(input$pred_resultbtn,{
    cbind(sampledata_analyte(), 
          analyst = submitter_name(), 
          date_analyzed = rep(format(Sys.Date(),"20%y-%m-%d")),
          result = resultPred())
  }) 
  
  # Linear Regression: Summary linear model
  output$summary_lm <- renderPrint({
    summary(linearMod())
  })
  
  # Linear Regression: Predict results
  output$pred_resultinfo <- renderDataTable({
    pred_dataresult_df()
  },
  options = list(
    pageLength = 10,
    scrollX = TRUE,
    dom = 'lftip',
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
      "}")
  )
 )
  
  # Data Analysis: Submit results to database
  observeEvent(input$submit_preddatabtn, {
    
    db_result_data <- dbConnect(SQLite(), dbname=sqlitePath)
    # table of finalresultdata already created in SQLite database by the following syntax
    # CREATE TABLE finalresultdata (sample_id TEXT, analyte_name TEXT, expected_raw NUMERIC, submitter TEXT, date_submitted TEXT, analyst TEXT, date_analyzed TEXT, result NUMERIC);
    dbWriteTable(db_result_data, "finalresultdata", pred_dataresult_df(), append =TRUE, row.names=FALSE)
    dbDisconnect(db_result_data)
    
    showModal(modalDialog(
      title = "Warning message",
      "Final results successfully saved to database.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
#########################################################    
################### Show results for Analyst ####################
  # retrieve analyzed data
  analyst_analysis_result <- eventReactive(input$allfinalresultsbtn, {
    db_finalresult_data <- dbConnect(SQLite(), dbname=sqlitePath)

    all_result_query <- sprintf({"SELECT * FROM finalresultdata"},
                            serialize = F)
    
    all_result_values <- dbGetQuery(db_finalresult_data, all_result_query)
    dbDisconnect(db_finalresult_data)
    all_result_values
  })
  
  # Display analyzed data
  output$allfinaldatainfo <- renderDataTable({
    analyst_analysis_result()
  },
  options = list(
    pageLength = 10,
    scrollX = TRUE,
    dom = 'lftip',
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
      "}")
  )
 )
  
  # Download analyzed data
  output$analystdownloadData <- downloadHandler(
    
    filename = function() {
      paste(submitter_name(), ".csv", sep = "")
    },
    
    content = function(file) {
      write.csv(analyst_analysis_result(), file, row.names = FALSE)
    }
  )
  
  # Analyst: Download final report in the format of pdf, html, or docx
  output$download_analyst_report <- downloadHandler(
    
    filename = function() {
      paste('Analyst-report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      src <- normalizePath('report_Analyst.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report_Analyst.Rmd', overwrite = TRUE)
      
      library(rmarkdown)
      out <- render('report_Analyst.Rmd', switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      
      file.rename(out, file)
    }
  )

#################### Submitter Query Result ###############################
  # Select Analyte to query
  query_analyte <- reactive({
    input$selectAnalyte_user
  })
  
  # retrieve selected analyzed data
  submitter_analysis_result <- eventReactive(input$query_Analyteresult, {
    db_analyzeddata <- dbConnect(SQLite(), dbname=sqlitePath)

    result_query <- sprintf({"SELECT * FROM finalresultdata WHERE submitter='%s' and analyte_name='%s'"}, 
                            submitter_name(), query_analyte(),
                            serialize = F)
    
    result_query_values <- dbGetQuery(db_analyzeddata, result_query)
    dbDisconnect(db_analyzeddata)
    result_query_values
  })
  
  # retrieve all analyzed data
  submitter_allanalyzed_result <- reactive({
    db_allanalyzeddata <- dbConnect(SQLite(), dbname=sqlitePath)

    all_result_query <- sprintf({"SELECT * FROM finalresultdata WHERE submitter='%s'"},
                            submitter_name(),
                            serialize = F)

    all_result_query_values <- dbGetQuery(db_allanalyzeddata, all_result_query)
    dbDisconnect(db_allanalyzeddata)
    all_result_query_values
  })
  
  # Display analyzed data
  output$queryAnalyteinfo <- renderDataTable({
    submitter_analysis_result()
  },
  options = list(
    pageLength = 10,
    scrollX = TRUE,
    dom = 'lftip',
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
      "}")
  )
 )
  
  # Download query data
  output$downloadQueryData <- downloadHandler(

    filename = function() {
      paste(submitter_name(), ".csv", sep = "")
    },

    content = function(file) {
      write.csv(submitter_analysis_result(), file, row.names = FALSE)
    }
  )

#####################################################################    
  observe({
    if(USER$Logged == FALSE){
      output$page <- renderUI({
        div(class="outer", do.call(bootstrapPage,c("",ui_loginsignup())))
      })
    }
  })
  
####################### Log Out Button #######################################    
  observeEvent(input$logoutbtn, {
    output$page <- renderUI({
      div(class="outer", do.call(bootstrapPage,c("",ui_loginsignup())))
    })
    USER$Logged <- FALSE
  })
    
}

ui = (htmlOutput("page"))

shinyApp(ui = ui, server = server)