#install.packages('shiny')

#Before you install the 'rJava' package, make sure the latest version of Java is installed in your system. 
#Download from here - https://www.java.com/en/download/manual.jsp
#Restart R Studio for changes to take effect and then install 'rJava' package.
#install.packages('rJava')

#install.packages('xlsx')

library(shiny)
library(rJava)
library(xlsx)
library(DT)
library(data.table)

#Forcing the max upload size of Excel file to be 300 MB.
options(shiny.maxRequestSize=400*1024^2) 
options(max.print=200000000) 
demographicdata = fread('demo_data.csv')
demographicdata = demographicdata[-1,]

ui <- fluidPage(
  
  titlePanel(title = "NCRC HMDA App"),
  
  sidebarLayout(
    sidebarPanel(
      
      fileInput(inputId='file1', label='Choose CSV File for Lender A', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
      actionButton(inputId="SliceA", label="View HMDA data for Lender A"),br(),
      
      hr(),br(),
      
      fileInput(inputId='file2', label='Choose CSV File for Lender B', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
      
      actionButton(inputId="SliceB", label="View HMDA data for Lender B"),br(),
      
      hr(),br(),
      
      selectizeInput(inputId = "demography", 'Choose MSA/MD for calculating Demographics', multiple = TRUE,
                     choices = c("Select a geography",sort((demographicdata$Geography)))
      ),
      
      hr(),br(),
      
      h5(strong("Data By Page")), br(),
      actionButton(inputId="DatabyPage", label="Calculate HMDA Statistics for Lender A & Lender B", icon("flash"),
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      
      helpText(strong("When you click the button above, you should see",
                      "the outputs on Pages 1 - 7 update to reflect the correponding statistics for Lender A, Lender B and the chosen demography")),
      
      
      br(),br(), downloadButton("HMDAdataByPage", "Download tables as .xlsx file"),
      hr(), br()
    ), 
    mainPanel(
      tabsetPanel(
        tabPanel("HMDA Data - Lender A", DT::dataTableOutput("HMDAtableA")),
        tabPanel("HMDA Data - Lender B", DT::dataTableOutput("HMDAtableB")),
        tabPanel("Page 1 - Demographic Summary",
                 fluidRow(
                   htmlOutput("selected_demo")),
                 fluidRow(
                   column(width =6, tableOutput("Population")),
                   
                   column(width = 6, tableOutput("Geography"))
                 )),
        
        
        tabPanel("Page 2 - Lender:A Originations", 
                 fluidRow(
                   htmlOutput("lenderA1")),
                 
                 fluidRow(
                   column(width =6, tableOutput("Applications1")),
                   
                   column(width = 6, tableOutput("Orig1"))
                   
                   
                 )),
        
        tabPanel("Page 3 - Lender:A Denials", 
                 fluidRow(
                   htmlOutput("lenderA2")),
                 
                 fluidRow(
                   
                   column(width =6, tableOutput("Approvals1")),
                   
                   column(width = 6, tableOutput("Den1")))
                 
        ),
        
        tabPanel("Page 4 - Lender:B Originations", 
                 fluidRow(
                   htmlOutput("lenderB1")),
                 fluidRow(
                   column(width =6, tableOutput("Applications2")),
                   
                   column(width = 6, tableOutput("Orig2"))
                   
                 )),
        tabPanel("Page 5 - Lender:B Denials", 
                 fluidRow(
                   htmlOutput("lenderB2")),
                 fluidRow(
                   
                   column(width =6, tableOutput("Approvals2")),
                   
                   column(width = 6, tableOutput("Den2")))
                 
        ),
        tabPanel("Page 6 - Lender A vs Lender B Originations", 
                 fluidRow(
                   htmlOutput("lenderAB1")),
                 fluidRow(
                   column(width =6, tableOutput("Applications3")),
                   
                   column(width = 6, tableOutput("Orig3")))),
        
        tabPanel("Page 7 - Lender A vs Lender B Denials", 
                 fluidRow(
                   htmlOutput("lenderAB2")),
                 fluidRow(
                   column(width =6, tableOutput("Den3")),
                   
                   column(width = 6, tableOutput("Volume"))))
        
      )
    )
  ))


server <- shinyServer(function(input, output, session) {
  
  #To simulate the loading of the application
  
  # When the user uploads a file, save it to a dataframe within the app for future use
  myData1 <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    hmdasliceA <- read.csv(inFile$datapath, header = TRUE)
    hmdasliceA$LMIT = ifelse(hmdasliceA$tract_to_msamd_income <= 80,1,0)
    hmdasliceA$LMIT[is.na(hmdasliceA$LMIT)] <- 0
    
    hmdasliceA$LMIB = ifelse(hmdasliceA$applicant_income_000s <= as.numeric(hmdasliceA$hud_median_family_income)*0.0008, 1, 0)
    hmdasliceA$LMIB[is.na(hmdasliceA$LMIB)] <- 0
    
    hmdasliceA$MINT = ifelse(hmdasliceA$minority_population > 50, 1, 0)
    hmdasliceA$MINT[is.na(hmdasliceA$MINT)] <- 0
    
    hmdasliceA$MINB = ifelse(hmdasliceA$applicant_ethnicity == 1 | hmdasliceA$applicant_race_1 <5, 1, 0)
    hmdasliceA$MINB[is.na(hmdasliceA$MINB)] <- 0
    
    hmdasliceA$Loans = ifelse(hmdasliceA$action_taken == 1, 1, 0)
    hmdasliceA$Loans[is.na(hmdasliceA$Loans)] <- 0 
    
    hmdasliceA$'No Demographic data' = ifelse(hmdasliceA$applicant_ethnicity < 2, 1, 0)
    hmdasliceA$'No Demographic data'[is.na(hmdasliceA$'No Demographic data')] <- 0 
    
    
    hmdasliceA
  })
  
  myData2 <- reactive({
    inFile <- input$file2
    if (is.null(inFile)) return(NULL)
    hmdasliceB <- read.csv(inFile$datapath, header = TRUE)
    hmdasliceB$LMIT = ifelse(hmdasliceB$tract_to_msamd_income <= 80,1,0)
    hmdasliceB$LMIT[is.na(hmdasliceB$LMIT)] <- 0
    
    hmdasliceB$LMIB = ifelse(hmdasliceB$applicant_income_000s <= as.numeric(hmdasliceB$hud_median_family_income)*0.0008, 1, 0)
    hmdasliceB$LMIB[is.na(hmdasliceB$LMIB)] <- 0
    
    hmdasliceB$MINT = ifelse(hmdasliceB$minority_population > 50, 1, 0)
    hmdasliceB$MINT[is.na(hmdasliceB$MINT)] <- 0
    
    hmdasliceB$MINB = ifelse(hmdasliceB$applicant_ethnicity == 1 | hmdasliceB$applicant_race_1 <5, 1, 0)
    hmdasliceB$MINB[is.na(hmdasliceB$MINB)] <- 0
    
    hmdasliceB$Loans = ifelse(hmdasliceB$action_taken == 1, 1, 0)
    hmdasliceB$Loans[is.na(hmdasliceB$Loans)] <- 0 
    
    hmdasliceB$'No Demographic data' = ifelse(hmdasliceB$applicant_ethnicity < 2, 1, 0)
    hmdasliceB$'No Demographic data'[is.na(hmdasliceB$'No Demographic data')] <- 0 
    
    #hmdasliceB$`Bank Name` = lenderids$`Bank Name`[match(hmdasliceB$respondent_id, lenderids$respondent_id)]
    #hmdasliceB$`Bank Name` = ifelse(is.na(hmdasliceB$`Bank Name`), hmdasliceB$respondent_id, hmdasliceB$`Bank Name`)
    
    hmdasliceB
  })
  
  # When the user clicks the button for displaying slice, print them
  observeEvent(input$SliceA, {
    output$HMDAtableA <- DT::renderDataTable(DT::datatable({
      myData1()
    }))
  })
  
  observeEvent(input$SliceB, {
    output$HMDAtableB <- DT::renderDataTable(DT::datatable({
      myData2()
    }))
  })
  
  # More logic involving the HMDA dataset
  
  observeEvent(input$DatabyPage, {
    
    output$selected_demo <- renderText({ 
      paste("Geography name: ", (strong(input$demography))," <br>", "Year(s): ", strong(unique(myData1()$as_of_year)),", ", strong(unique(myData2()$as_of_year)),
            "<br> Group A Lender(s): <br>","Group B Lender(s): <br>", "Report Date: ",strong(Sys.Date()), "<br> Date: ", strong(Sys.Date()), br(), br())
    })
    
    
    Population = c("Total Population",
                   "Total Families",
                   "White",
                   "Black",
                   "Hispanic/Latino",
                   "Native American",
                   "Asian",
                   "HOPI",
                   "Total Minority",
                   "Number of Females",
                   "Population in LMI Tracts",
                   "Population in Minority Tracts"
    )
    
    Values = c(sum(demographicdata$`Sum of Total Persons`[demographicdata$Geography %in% input$demography]), 
               sum(demographicdata$`Sum of Total Families2`[demographicdata$Geography %in% input$demography]),
               sum(demographicdata$`Sum of Total Pop Non-Hispanic White`[demographicdata$Geography %in% input$demography]),
               sum(demographicdata$`Sum of Total Pop Non-Hispanic Black/African American`[demographicdata$Geography %in% input$demography]),
               sum(demographicdata$`Sum of Total Pop Hispanic only`[demographicdata$Geography %in% input$demography]),
               sum(demographicdata$`Sum of Total Pop Non-Hispanic American Indian/Alaska Native`[demographicdata$Geography %in% input$demography]),
               sum(demographicdata$`Sum of Total Pop Non-Hispanic Asian`[demographicdata$Geography %in% input$demography]),
               sum(demographicdata$`Sum of Total Pop Non-Hispanic Native Hawaiian/other Pacific Islander`[demographicdata$Geography %in% input$demography]),
               (sum(demographicdata$`Sum of Total Persons`[demographicdata$Geography %in% input$demography])) - (sum(demographicdata$`Sum of Total Pop Non-Hispanic White`[demographicdata$Geography %in% input$demography])),
               sum(demographicdata$`Sum of Total Female Pop2`[demographicdata$Geography %in% input$demography]), 
               sum(demographicdata$`Sum of LMI Pop`[demographicdata$Geography %in% input$demography]),
               sum(demographicdata$`Sum of MIN Pop`[demographicdata$Geography %in% input$demography]))
    
    
    Percent = c(0,0,
                Values[3]/Values[1],
                Values[4]/Values[1],
                Values[5]/Values[1],
                Values[6]/Values[1],
                Values[7]/Values[1],
                Values[8]/Values[1],
                Values[9]/Values[1],
                Values[10]/Values[1],
                Values[11]/Values[1],
                Values[12]/Values[1])
    
    

    Population = data.frame(Population, Values, Percent)
    colnames(Population) = c("Population Demographics", "Number", "Percent")
    rownames(Population) = NULL
    rm(Values, Percent)
    
    output$Population <- renderTable({
      
      Population
    })
    

  Geography = c("Median Family Income", 
                  "LMI/MUI Income Point", 
                  "Number of families", 
                  "Families living in poverty", 
                  "Poverty Rate",
                  "Average Age",
                  "LMI Tracts in Geography",
                  "Minority/Majority Tracts",
                  "Average Housing Year Built",
                  "Owner Occupied",
                  "Occupied Units")
    
    Values = c(mean(demographicdata$`Average of MSA MFI`[demographicdata$Geography %in% input$demography]), 
               mean((demographicdata$`Average of MSA MFI`[demographicdata$Geography %in% input$demography])*0.8), 
               sum(demographicdata$`Sum of Total Families2`[demographicdata$Geography %in% input$demography]), 
               sum(demographicdata$`Sum of Total Families with Income Below Poverty`[demographicdata$Geography %in% input$demography]),
               mean((demographicdata$`Sum of Total Families with Income Below Poverty`[demographicdata$Geography %in% input$demography])/
                 (demographicdata$`Sum of Total Families2`[demographicdata$Geography %in% input$demography])),
               mean(demographicdata$`Average of Persons Median Age`[demographicdata$Geography %in% input$demography]),
               sum(demographicdata$`Sum of LMI Tract`[demographicdata$Geography %in% input$demography]),
               sum(demographicdata$`Sum of MIN Tract`[demographicdata$Geography %in% input$demography]),
               mean(demographicdata$`Average of Median Year Structure Built`[demographicdata$Geography %in% input$demography]), 
               sum(demographicdata$`Sum of Total Owner Occupied Housing Units - Tenure`[demographicdata$Geography %in% input$demography]), 
               sum(demographicdata$`Sum of Total Occupied Housing Units - Tenure`[demographicdata$Geography %in% input$demography]))  
    
    Percent = c(0,0, 0, 0, 0, 0,
                Values[7]/sum(demographicdata$`Count of 1`[demographicdata$Geography %in% input$demography]),
                Values[8]/sum(demographicdata$`Count of 1`[demographicdata$Geography %in% input$demography]),
                0, 0, 0)
    Values[9] = round((Values[9]),0)
    Values[1] = round(as.numeric(Values[1]))
    Values[2] = round(as.numeric(Values[2]))
    
    
    Values[6] = round(as.numeric(Values[6]))
    
    
    Geography = data.frame(Geography,Values, Percent)
    colnames(Geography) = c("Geography Demographics", "Number", "Percent")
    
    rm(Values, Percent)
    
    output$Geography <- renderTable({
      
      Geography
      
    })
    
    output$lenderA1 <- renderText({ 
      paste("Geography name: ", strong(input$demography)," <br>", "Year(s): ", strong(unique(myData1()$as_of_year)), "<br>", "Group A Lender(s): <br>",
            "Group B Lender(s): <br>", "Report Date: ",strong(Sys.Date()), "<br> Date: ",strong(Sys.Date()), br(), br())
    })
    
    Lender = c("White",
               "Black",
               "Hispanic/Latino",
               "Native American",
               "Asian",
               "HOPI",
               "Minority",
               "No Demographic Data",
               "Female",
               "LMI Applicant",
               "LMI Census Tract",
               "Minority/Majority Tract",
               "Total")
    
    Number = c(sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 5)),
               sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 3)),
               sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 1)),
               sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 1)),
               sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 2)),
               sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 4)),
               (sum(myData1()$action_taken <= 5)) - sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 5)) - 
                 sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 4)) -sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 3)),
               sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 3)),
               sum((myData1()$action_taken <= 5) & (myData1()$applicant_sex_name == "Female")),
               sum((myData1()$action_taken <= 5) & (myData1()$LMIB == 1)),
               sum((myData1()$action_taken <= 5) & (myData1()$LMIT == 1)),
               sum((myData1()$action_taken <= 5) & (myData1()$MINT == 1)),
               sum(myData1()$action_taken <= 5)
    )
    Percent = c(Number[1]/sum(myData1()$action_taken <= 5),
                Number[2]/sum(myData1()$action_taken <= 5),
                Number[3]/sum(myData1()$action_taken <= 5),
                Number[4]/sum(myData1()$action_taken <= 5),
                Number[5]/sum(myData1()$action_taken <= 5),
                Number[6]/sum(myData1()$action_taken <= 5),
                Number[7]/sum(myData1()$action_taken <= 5),
                Number[8]/sum(myData1()$action_taken <= 5),
                Number[9]/sum(myData1()$action_taken <= 5),
                Number[10]/sum(myData1()$action_taken <= 5),
                Number[11]/sum(myData1()$action_taken <= 5),
                Number[12]/sum(myData1()$action_taken <= 5),
                100)
    
    
    Volume = c(sum(myData1()[which((myData1()$action_taken <=5) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 ==5)), "loan_amount_000s"]),
               sum(myData1()[which((myData1()$action_taken <=5) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 ==3)), "loan_amount_000s"]),
               sum(myData1()[which((myData1()$action_taken <=5) & (myData1()$applicant_ethnicity == 1)), "loan_amount_000s"]),
               sum(myData1()[which((myData1()$action_taken <=5) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 ==1)), "loan_amount_000s"]),
               sum(myData1()[which((myData1()$action_taken <=5) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 ==2)), "loan_amount_000s"]),
               sum(myData1()[which((myData1()$action_taken <=5) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 ==4)), "loan_amount_000s"]),
               sum(myData1()[which((myData1()$action_taken <=5)), "loan_amount_000s"]) - sum(myData1()[which((myData1()$action_taken <=5) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 ==5)), "loan_amount_000s"]) -
                 sum(myData1()[which((myData1()$action_taken <=5) & (myData1()$applicant_ethnicity == 4)), "loan_amount_000s"]) - sum(myData1()[which((myData1()$action_taken <=5) & (myData1()$applicant_ethnicity == 3)), "loan_amount_000s"]),
               sum(myData1()[which((myData1()$action_taken <=5) & (myData1()$applicant_ethnicity == 3)), "loan_amount_000s"]),
               sum(myData1()[which((myData1()$action_taken <=5) & (myData1()$applicant_sex_name == "Female")), "loan_amount_000s"]),
               sum(myData1()[which((myData1()$action_taken <=5) & (myData1()$LMIB == 1)), "loan_amount_000s"]),
               sum(myData1()[which((myData1()$action_taken <=5) & (myData1()$LMIT == 1)), "loan_amount_000s"]),
               sum(myData1()[which((myData1()$action_taken <=5) & (myData1()$MINT == 1)), "loan_amount_000s"]),
               0)
    
    Percent1 = c(Volume[1]/ sum(myData1()[which((myData1()$action_taken <=5)), "loan_amount_000s"]),
                 Volume[2]/ sum(myData1()[which((myData1()$action_taken <=5)), "loan_amount_000s"]),
                 Volume[3]/ sum(myData1()[which((myData1()$action_taken <=5)), "loan_amount_000s"]),
                 Volume[4]/ sum(myData1()[which((myData1()$action_taken <=5)), "loan_amount_000s"]),
                 Volume[5]/ sum(myData1()[which((myData1()$action_taken <=5)), "loan_amount_000s"]),
                 Volume[6]/ sum(myData1()[which((myData1()$action_taken <=5)), "loan_amount_000s"]),
                 Volume[7]/ sum(myData1()[which((myData1()$action_taken <=5)), "loan_amount_000s"]),
                 Volume[8]/ sum(myData1()[which((myData1()$action_taken <=5)), "loan_amount_000s"]),
                 Volume[9]/ sum(myData1()[which((myData1()$action_taken <=5)), "loan_amount_000s"]),
                 Volume[10]/ sum(myData1()[which((myData1()$action_taken <=5)), "loan_amount_000s"]),
                 Volume[11]/ sum(myData1()[which((myData1()$action_taken <=5)), "loan_amount_000s"]),
                 Volume[12]/ sum(myData1()[which((myData1()$action_taken <=5)), "loan_amount_000s"]),
                 0
    )
    
    
    Volume = round(1*as.numeric(Volume), 2)
    
    
    ApplicationsA = data.frame(Lender, Number, Percent, Volume, Percent1)
    colnames(ApplicationsA) = c("Applicant characteristics",
                                "Applications", "Percent", "Volume of Loans", "Percent")
    rm(Number, Percent, Volume, Percent1)
    
    output$Applications1 <- renderTable({
      
      ApplicationsA
    })
    

    Number = c(sum((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 5)),
               sum((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 3)),
               sum((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 1)),
               sum((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 1)),
               sum((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 2)),
               sum((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 4)),
               (sum(myData1()$action_taken == 1)) - sum((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 5)) - 
                 sum((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 4)) -sum((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 3)),
               sum((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 3)),
               sum((myData1()$action_taken == 1) & (myData1()$applicant_sex_name == "Female")),
               sum((myData1()$action_taken == 1) & (myData1()$LMIB == 1)),
               sum((myData1()$action_taken == 1) & (myData1()$LMIT == 1)),
               sum((myData1()$action_taken == 1) & (myData1()$MINT == 1)),
               sum(myData1()$action_taken == 1)
    )
    
    Percent = c(Number[1]/sum(myData1()$action_taken == 1),
                Number[2]/sum(myData1()$action_taken == 1),
                Number[3]/sum(myData1()$action_taken == 1),
                Number[4]/sum(myData1()$action_taken == 1),
                Number[5]/sum(myData1()$action_taken == 1),
                Number[6]/sum(myData1()$action_taken == 1),
                Number[7]/sum(myData1()$action_taken == 1),
                Number[8]/sum(myData1()$action_taken == 1),
                Number[9]/sum(myData1()$action_taken == 1),
                Number[10]/sum(myData1()$action_taken == 1),
                Number[11]/sum(myData1()$action_taken == 1),
                Number[12]/sum(myData1()$action_taken == 1),
                100)
    
    Volume = c(sum(myData1()[which((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 ==5)), "loan_amount_000s"]),
               sum(myData1()[which((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 ==3)), "loan_amount_000s"]),
               sum(myData1()[which((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 1)), "loan_amount_000s"]),
               sum(myData1()[which((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 ==1)), "loan_amount_000s"]),
               sum(myData1()[which((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 ==2)), "loan_amount_000s"]),
               sum(myData1()[which((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 ==4)), "loan_amount_000s"]),
               sum(myData1()[which((myData1()$action_taken == 1)), "loan_amount_000s"]) - sum(myData1()[which((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 ==5)), "loan_amount_000s"]) -
                 sum(myData1()[which((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 4)), "loan_amount_000s"]) - sum(myData1()[which((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 3)), "loan_amount_000s"]),
               sum(myData1()[which((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 3)), "loan_amount_000s"]),
               sum(myData1()[which((myData1()$action_taken == 1) & (myData1()$applicant_sex_name == "Female")), "loan_amount_000s"]),
               sum(myData1()[which((myData1()$action_taken == 1) & (myData1()$LMIB == 1)), "loan_amount_000s"]),
               sum(myData1()[which((myData1()$action_taken == 1) & (myData1()$LMIT == 1)), "loan_amount_000s"]),
               sum(myData1()[which((myData1()$action_taken == 1) & (myData1()$MINT == 1)), "loan_amount_000s"]),
               0)
    
    Percent1 = c(Volume[1]/ sum(myData1()[which((myData1()$action_taken == 1)), "loan_amount_000s"]),
                 Volume[2]/ sum(myData1()[which((myData1()$action_taken == 1)), "loan_amount_000s"]),
                 Volume[3]/ sum(myData1()[which((myData1()$action_taken == 1)), "loan_amount_000s"]),
                 Volume[4]/ sum(myData1()[which((myData1()$action_taken == 1)), "loan_amount_000s"]),
                 Volume[5]/ sum(myData1()[which((myData1()$action_taken == 1)), "loan_amount_000s"]),
                 Volume[6]/ sum(myData1()[which((myData1()$action_taken == 1)), "loan_amount_000s"]),
                 Volume[7]/ sum(myData1()[which((myData1()$action_taken == 1)), "loan_amount_000s"]),
                 Volume[8]/ sum(myData1()[which((myData1()$action_taken == 1)), "loan_amount_000s"]),
                 Volume[9]/ sum(myData1()[which((myData1()$action_taken == 1)), "loan_amount_000s"]),
                 Volume[10]/ sum(myData1()[which((myData1()$action_taken == 1)), "loan_amount_000s"]),
                 Volume[11]/ sum(myData1()[which((myData1()$action_taken == 1)), "loan_amount_000s"]),
                 Volume[12]/ sum(myData1()[which((myData1()$action_taken == 1)), "loan_amount_000s"]),
                 0
    )
    
    
    Volume = round(1*as.numeric(Volume))
    
    OriginationsA = data.frame(Lender, Number, Percent, Volume, Percent1)
    colnames(OriginationsA) = c("Applicant characteristics",
                                "Originations", "Percent", "Volume of Loans", "Percent")
    rm(Number, Percent, Volume, Percent1)
    
    output$Orig1 <- renderTable({
      
      OriginationsA
    })
    
    
    output$lenderA2 <- renderText({ 
      paste("Geography name: ", strong(input$demography)," <br>", "Year(s): ", strong(unique(myData1()$as_of_year)), "<br>", "Group A Lender(s): <br>",
            "Group B Lender(s): <br>", "Report Date: ",strong(Sys.Date()), "<br> Date: ",strong(Sys.Date()), br(), br())
    })
    
    Lender = c("White",
               "Black",
               "Hispanic/Latino",
               "Native American",
               "Asian",
               "HOPI",
               "Minority",
               "No Demographic Data",
               "Female",
               "LMI Applicant",
               "LMI Census Tract",
               "Minority/Majority Tract")
    
    
    
    Percent = c(
      sum((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 5))/
        sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 5)),
      sum((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 3))/
        sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 3)),
      sum((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 1))/sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 1)),
      sum((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 1))/
        sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 1)),
      sum((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 2))/
        sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 2)),
      sum((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 4))/
        sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 4)),
      ((sum(myData1()$action_taken == 1)) - sum((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 5)) - 
         sum((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 4)) -sum((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 3)))/
        ((sum(myData1()$action_taken <= 5)) - sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 5)) - 
           sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 4)) -sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 3))),
      sum((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 3))/sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 3)),
      sum((myData1()$action_taken == 1) & (myData1()$applicant_sex_name == "Female"))/sum((myData1()$action_taken <= 5) & (myData1()$applicant_sex_name == "Female")),
      sum((myData1()$action_taken == 1) & (myData1()$LMIB == 1)) / sum((myData1()$action_taken <= 5) & (myData1()$LMIB == 1)),
      sum((myData1()$action_taken == 1) & (myData1()$LMIT == 1))/ sum((myData1()$action_taken <= 5) & (myData1()$LMIT == 1)),
      sum((myData1()$action_taken == 1) & (myData1()$MINT == 1)) /  sum((myData1()$action_taken <= 5) & (myData1()$MINT == 1))
    )
    
    Malepercent = sum((myData1()$action_taken == 1) & (myData1()$applicant_sex_name == "Male"))/sum((myData1()$action_taken <= 5) & (myData1()$applicant_sex_name == "Male"))
    MUIApproval = (sum(myData1()$action_taken == 1) - sum((myData1()$action_taken == 1) & (myData1()$LMIB == 1)))/
      (sum(myData1()$action_taken <= 5) - sum((myData1()$action_taken <= 5) & (myData1()$LMIB == 1)))
    
    MUiCensus  = (sum(myData1()$action_taken == 1) - sum((myData1()$action_taken == 1) & (myData1()$LMIT == 1)))/
      (sum(myData1()$action_taken <= 5) - sum((myData1()$action_taken <= 5) & (myData1()$LMIT == 1)))
    
    Minority = (sum(myData1()$action_taken == 1) - sum((myData1()$action_taken == 1) & (myData1()$MINT == 1)))/
      (sum(myData1()$action_taken <= 5) - sum((myData1()$action_taken <= 5) & (myData1()$MINT == 1)))
    
    
    Disparity = c(Percent[1]/Percent[1], Percent[2]/Percent[1], Percent[3]/Percent[1], Percent[4]/Percent[1],
                  Percent[5]/Percent[1], Percent[6]/Percent[1], Percent[7]/Percent[1], Percent[8]/Percent[1],
                  Percent[9]/Malepercent, Percent[10]/MUIApproval, Percent[11]/ MUiCensus, Percent[12]/Minority)
    
    
    Disparity = round(as.numeric(Disparity),2)
    Percent = (as.numeric(Percent))
    ApprovalsA = data.frame(Lender, Percent, Disparity)
    colnames(ApprovalsA) = c("Applicant characteristics",
                             "Approvals %", "Disparity Index")
    rm(Lender, Percent, Malepercent, MUIApproval, MUiCensus, Minority)
    
    output$Approvals1 <- renderTable({
      
      ApprovalsA
      
    })
    Lender = c("White",
               "Black",
               "Hispanic/Latino",
               "Native American",
               "Asian",
               "HOPI",
               "Minority",
               "No Demographic Data",
               "Female",
               "LMI Applicant",
               "LMI Census Tract",
               "Minority/Majority Tract")
    
    Number = c(
      sum((myData1()$action_taken == 3) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 5)),
      sum((myData1()$action_taken == 3) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 3)),
      sum((myData1()$action_taken == 3) & (myData1()$applicant_ethnicity == 1)),
      sum((myData1()$action_taken == 3) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 1)),
      sum((myData1()$action_taken == 3) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 2)),
      sum((myData1()$action_taken == 3) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 4)),
      (sum(myData1()$action_taken == 3)) - sum((myData1()$action_taken == 3) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 5)) - 
        sum((myData1()$action_taken == 3) & (myData1()$applicant_ethnicity == 4)) -sum((myData1()$action_taken == 3) & (myData1()$applicant_ethnicity == 3)),
      sum((myData1()$action_taken == 3) & (myData1()$applicant_ethnicity == 3)),
      sum((myData1()$action_taken == 3) & (myData1()$applicant_sex_name == "Female")),
      sum((myData1()$action_taken == 3) & (myData1()$LMIB == 1)),
      sum((myData1()$action_taken == 3) & (myData1()$LMIT == 1)),
      sum((myData1()$action_taken == 3) & (myData1()$MINT == 1))
    )
    
    
    Percent = c(
      sum((myData1()$action_taken == 3) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 5))/
        sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 5)),
      sum((myData1()$action_taken == 3) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 3))/
        sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 3)),
      sum((myData1()$action_taken == 3) & (myData1()$applicant_ethnicity == 1))/sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 1)),
      sum((myData1()$action_taken == 3) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 1))/
        sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 1)),
      sum((myData1()$action_taken == 3) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 2))/
        sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 2)),
      sum((myData1()$action_taken == 3) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 4))/
        sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 4)),
      ((sum(myData1()$action_taken == 3)) - sum((myData1()$action_taken == 3) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 5)) - 
         sum((myData1()$action_taken == 3) & (myData1()$applicant_ethnicity == 4)) -sum((myData1()$action_taken == 3) & (myData1()$applicant_ethnicity == 3)))/
        ((sum(myData1()$action_taken <= 5)) - sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 5)) - 
           sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 4)) -sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 3))),
      sum((myData1()$action_taken == 3) & (myData1()$applicant_ethnicity == 3))/sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 3)),
      sum((myData1()$action_taken == 3) & (myData1()$applicant_sex_name == "Female"))/sum((myData1()$action_taken <= 5) & (myData1()$applicant_sex_name == "Female")),
      sum((myData1()$action_taken == 3) & (myData1()$LMIB == 1)) / sum((myData1()$action_taken <= 5) & (myData1()$LMIB == 1)),
      sum((myData1()$action_taken == 3) & (myData1()$LMIT == 1))/ sum((myData1()$action_taken <= 5) & (myData1()$LMIT == 1)),
      sum((myData1()$action_taken == 3) & (myData1()$MINT == 1)) /  sum((myData1()$action_taken <= 5) & (myData1()$MINT == 1))
    )
    
    Malepercent = sum((myData1()$action_taken == 3) & (myData1()$applicant_sex_name == "Male"))/sum((myData1()$action_taken <= 5) & (myData1()$applicant_sex_name == "Male"))
    MUIApproval = (sum(myData1()$action_taken == 3) - sum((myData1()$action_taken == 3) & (myData1()$LMIB == 1)))/
      (sum(myData1()$action_taken <= 5) - sum((myData1()$action_taken <= 5) & (myData1()$LMIB == 1)))
    
    MUiCensus  = (sum(myData1()$action_taken == 3) - sum((myData1()$action_taken == 3) & (myData1()$LMIT == 1)))/
      (sum(myData1()$action_taken <= 5) - sum((myData1()$action_taken <= 5) & (myData1()$LMIT == 1)))
    
    Minority = (sum(myData1()$action_taken == 3) - sum((myData1()$action_taken == 3) & (myData1()$MINT == 1)))/
      (sum(myData1()$action_taken <= 5) - sum((myData1()$action_taken <= 5) & (myData1()$MINT == 1)))
    
    
    Disparity = c(Percent[1]/Percent[1], Percent[2]/Percent[1], Percent[3]/Percent[1], Percent[4]/Percent[1],
                  Percent[5]/Percent[1], Percent[6]/Percent[1], Percent[7]/Percent[1], Percent[8]/Percent[1],
                  Percent[9]/Malepercent, Percent[10]/MUIApproval, Percent[11]/ MUiCensus, Percent[12]/Minority)
    
    
    Disparity = round(as.numeric(Disparity),2)
    Percent = (as.numeric(Percent))
    DenialsA = data.frame(Lender, Number, Percent, Disparity)
    colnames(DenialsA) = c("Applicant characteristics",
                           "Denials", "Percent", "Disparity Index")
    
    
    rm(Lender, Percent,Number, Malepercent, MUIApproval, MUiCensus, Minority, Disparity)
    
    output$Den1 <- renderTable({
      
      DenialsA
      
    })
    
    output$lenderB1 <- renderText({ 
      paste("Geography name: ", strong(input$demography)," <br>", "Year(s): ", strong(unique(myData2()$as_of_year)), "<br>", "Group A Lender(s): <br>",
            "Group B Lender(s): <br>", "Report Date: ",strong(Sys.Date()), "<br> Date: ",strong(Sys.Date()), br(), br())
    })
    
    Lender = c("White",
               "Black",
               "Hispanic/Latino",
               "Native American",
               "Asian",
               "HOPI",
               "Minority",
               "No Demographic Data",
               "Female",
               "LMI Applicant",
               "LMI Census Tract",
               "Minority/Majority Tract",
               "Total")
    
    Number = c(sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 5)),
               sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 3)),
               sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 1)),
               sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 1)),
               sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 2)),
               sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 4)),
               (sum(myData2()$action_taken <= 5)) - sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 5)) - 
                 sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 4)) -sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 3)),
               sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 3)),
               sum((myData2()$action_taken <= 5) & (myData2()$applicant_sex_name == "Female")),
               sum((myData2()$action_taken <= 5) & (myData2()$LMIB == 1)),
               sum((myData2()$action_taken <= 5) & (myData2()$LMIT == 1)),
               sum((myData2()$action_taken <= 5) & (myData2()$MINT == 1)),
               sum(myData2()$action_taken <= 5)
    )
    Percent = c(Number[1]/sum(myData2()$action_taken <= 5),
                Number[2]/sum(myData2()$action_taken <= 5),
                Number[3]/sum(myData2()$action_taken <= 5),
                Number[4]/sum(myData2()$action_taken <= 5),
                Number[5]/sum(myData2()$action_taken <= 5),
                Number[6]/sum(myData2()$action_taken <= 5),
                Number[7]/sum(myData2()$action_taken <= 5),
                Number[8]/sum(myData2()$action_taken <= 5),
                Number[9]/sum(myData2()$action_taken <= 5),
                Number[10]/sum(myData2()$action_taken <= 5),
                Number[11]/sum(myData2()$action_taken <= 5),
                Number[12]/sum(myData2()$action_taken <= 5),
                100)
    
    Volume = c(sum(myData2()[which((myData2()$action_taken <=5) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 ==5)), "loan_amount_000s"]),
               sum(myData2()[which((myData2()$action_taken <=5) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 ==3)), "loan_amount_000s"]),
               sum(myData2()[which((myData2()$action_taken <=5) & (myData2()$applicant_ethnicity == 1)), "loan_amount_000s"]),
               sum(myData2()[which((myData2()$action_taken <=5) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 ==1)), "loan_amount_000s"]),
               sum(myData2()[which((myData2()$action_taken <=5) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 ==2)), "loan_amount_000s"]),
               sum(myData2()[which((myData2()$action_taken <=5) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 ==4)), "loan_amount_000s"]),
               sum(myData2()[which((myData2()$action_taken <=5)), "loan_amount_000s"]) - sum(myData2()[which((myData2()$action_taken <=5) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 ==5)), "loan_amount_000s"]) -
                 sum(myData2()[which((myData2()$action_taken <=5) & (myData2()$applicant_ethnicity == 4)), "loan_amount_000s"]) - sum(myData2()[which((myData2()$action_taken <=5) & (myData2()$applicant_ethnicity == 3)), "loan_amount_000s"]),
               sum(myData2()[which((myData2()$action_taken <=5) & (myData2()$applicant_ethnicity == 3)), "loan_amount_000s"]),
               sum(myData2()[which((myData2()$action_taken <=5) & (myData2()$applicant_sex_name == "Female")), "loan_amount_000s"]),
               sum(myData2()[which((myData2()$action_taken <=5) & (myData2()$LMIB == 1)), "loan_amount_000s"]),
               sum(myData2()[which((myData2()$action_taken <=5) & (myData2()$LMIT == 1)), "loan_amount_000s"]),
               sum(myData2()[which((myData2()$action_taken <=5) & (myData2()$MINT == 1)), "loan_amount_000s"]),
               0)
    
    Percent1 = c(Volume[1]/ sum(myData2()[which((myData2()$action_taken <=5)), "loan_amount_000s"]),
                 Volume[2]/ sum(myData2()[which((myData2()$action_taken <=5)), "loan_amount_000s"]),
                 Volume[3]/ sum(myData2()[which((myData2()$action_taken <=5)), "loan_amount_000s"]),
                 Volume[4]/ sum(myData2()[which((myData2()$action_taken <=5)), "loan_amount_000s"]),
                 Volume[5]/ sum(myData2()[which((myData2()$action_taken <=5)), "loan_amount_000s"]),
                 Volume[6]/ sum(myData2()[which((myData2()$action_taken <=5)), "loan_amount_000s"]),
                 Volume[7]/ sum(myData2()[which((myData2()$action_taken <=5)), "loan_amount_000s"]),
                 Volume[8]/ sum(myData2()[which((myData2()$action_taken <=5)), "loan_amount_000s"]),
                 Volume[9]/ sum(myData2()[which((myData2()$action_taken <=5)), "loan_amount_000s"]),
                 Volume[10]/ sum(myData2()[which((myData2()$action_taken <=5)), "loan_amount_000s"]),
                 Volume[11]/ sum(myData2()[which((myData2()$action_taken <=5)), "loan_amount_000s"]),
                 Volume[12]/ sum(myData2()[which((myData2()$action_taken <=5)), "loan_amount_000s"]),
                 0
    )
    
    Volume = round(1*as.numeric(Volume), 2)
    
    ApplicationsB = data.frame(Lender, Number, Percent, Volume, Percent1)
    colnames(ApplicationsB) = c("Applicant characteristics",
                                "Applications", "Percent", "Volume of Loans", "Percent")
    rm(Number, Percent, Volume, Percent1)
    
    output$Applications2 <- renderTable({
      
      ApplicationsB
    })
    
    Number = c(sum((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 5)),
               sum((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 3)),
               sum((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 1)),
               sum((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 1)),
               sum((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 2)),
               sum((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 4)),
               (sum(myData2()$action_taken == 1)) - sum((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 5)) - 
                 sum((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 4)) -sum((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 3)),
               sum((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 3)),
               sum((myData2()$action_taken == 1) & (myData2()$applicant_sex_name == "Female")),
               sum((myData2()$action_taken == 1) & (myData2()$LMIB == 1)),
               sum((myData2()$action_taken == 1) & (myData2()$LMIT == 1)),
               sum((myData2()$action_taken == 1) & (myData2()$MINT == 1)),
               sum(myData2()$action_taken == 1)
    )
    
    Percent = c(Number[1]/sum(myData2()$action_taken == 1),
                Number[2]/sum(myData2()$action_taken == 1),
                Number[3]/sum(myData2()$action_taken == 1),
                Number[4]/sum(myData2()$action_taken == 1),
                Number[5]/sum(myData2()$action_taken == 1),
                Number[6]/sum(myData2()$action_taken == 1),
                Number[7]/sum(myData2()$action_taken == 1),
                Number[8]/sum(myData2()$action_taken == 1),
                Number[9]/sum(myData2()$action_taken == 1),
                Number[10]/sum(myData2()$action_taken == 1),
                Number[11]/sum(myData2()$action_taken == 1),
                Number[12]/sum(myData2()$action_taken == 1),
                100)
    
    Volume = c(sum(myData2()[which((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 ==5)), "loan_amount_000s"]),
               sum(myData2()[which((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 ==3)), "loan_amount_000s"]),
               sum(myData2()[which((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 1)), "loan_amount_000s"]),
               sum(myData2()[which((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 ==1)), "loan_amount_000s"]),
               sum(myData2()[which((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 ==2)), "loan_amount_000s"]),
               sum(myData2()[which((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 ==4)), "loan_amount_000s"]),
               sum(myData2()[which((myData2()$action_taken == 1)), "loan_amount_000s"]) - sum(myData2()[which((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 ==5)), "loan_amount_000s"]) -
                 sum(myData2()[which((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 4)), "loan_amount_000s"]) - sum(myData2()[which((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 3)), "loan_amount_000s"]),
               sum(myData2()[which((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 3)), "loan_amount_000s"]),
               sum(myData2()[which((myData2()$action_taken == 1) & (myData2()$applicant_sex_name == "Female")), "loan_amount_000s"]),
               sum(myData2()[which((myData2()$action_taken == 1) & (myData2()$LMIB == 1)), "loan_amount_000s"]),
               sum(myData2()[which((myData2()$action_taken == 1) & (myData2()$LMIT == 1)), "loan_amount_000s"]),
               sum(myData2()[which((myData2()$action_taken == 1) & (myData2()$MINT == 1)), "loan_amount_000s"]),
               0)
    
    Percent1 = c(Volume[1]/ sum(myData2()[which((myData2()$action_taken == 1)), "loan_amount_000s"]),
                 Volume[2]/ sum(myData2()[which((myData2()$action_taken == 1)), "loan_amount_000s"]),
                 Volume[3]/ sum(myData2()[which((myData2()$action_taken == 1)), "loan_amount_000s"]),
                 Volume[4]/ sum(myData2()[which((myData2()$action_taken == 1)), "loan_amount_000s"]),
                 Volume[5]/ sum(myData2()[which((myData2()$action_taken == 1)), "loan_amount_000s"]),
                 Volume[6]/ sum(myData2()[which((myData2()$action_taken == 1)), "loan_amount_000s"]),
                 Volume[7]/ sum(myData2()[which((myData2()$action_taken == 1)), "loan_amount_000s"]),
                 Volume[8]/ sum(myData2()[which((myData2()$action_taken == 1)), "loan_amount_000s"]),
                 Volume[9]/ sum(myData2()[which((myData2()$action_taken == 1)), "loan_amount_000s"]),
                 Volume[10]/ sum(myData2()[which((myData2()$action_taken == 1)), "loan_amount_000s"]),
                 Volume[11]/ sum(myData2()[which((myData2()$action_taken == 1)), "loan_amount_000s"]),
                 Volume[12]/ sum(myData2()[which((myData2()$action_taken == 1)), "loan_amount_000s"]),
                 0
    )
    
    
    Volume = round(1*as.numeric(Volume), 2)
    
    OriginationsB = data.frame(Lender, Number, Percent, Volume, Percent1)
    colnames(OriginationsB) = c("Applicant characteristics",
                                "Originations", "Percent", "Volume of Loans", "Percent")
    rm(Number, Percent, Volume, Percent1)
    
    output$Orig2 <- renderTable({
      
      OriginationsB
    })
    
    
    output$lenderB2 <- renderText({ 
      paste("Geography name: ", strong(input$demography)," <br>", "Year(s): ", strong(unique(myData2()$as_of_year)), "<br>", "Group A Lender(s): <br>",
            "Group B Lender(s): <br>", "Report Date: ",strong(Sys.Date()),"<br>", "Date: ",strong(Sys.Date()), br(), br())
    })
    
    Lender = c("White",
               "Black",
               "Hispanic/Latino",
               "Native American",
               "Asian",
               "HOPI",
               "Minority",
               "No Demographic Data",
               "Female",
               "LMI Applicant",
               "LMI Census Tract",
               "Minority/Majority Tract")
    
    Percent = c(
      sum((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 5))/
        sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 5)),
      sum((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 3))/
        sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 3)),
      sum((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 1))/sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 1)),
      sum((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 1))/
        sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 1)),
      sum((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 2))/
        sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 2)),
      sum((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 4))/
        sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 4)),
      ((sum(myData2()$action_taken == 1)) - sum((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 5)) - 
         sum((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 4)) -sum((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 3)))/
        ((sum(myData2()$action_taken <= 5)) - sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 5)) - 
           sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 4)) -sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 3))),
      sum((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 3))/sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 3)),
      sum((myData2()$action_taken == 1) & (myData2()$applicant_sex_name == "Female"))/sum((myData2()$action_taken <= 5) & (myData2()$applicant_sex_name == "Female")),
      sum((myData2()$action_taken == 1) & (myData2()$LMIB == 1)) / sum((myData2()$action_taken <= 5) & (myData2()$LMIB == 1)),
      sum((myData2()$action_taken == 1) & (myData2()$LMIT == 1))/ sum((myData2()$action_taken <= 5) & (myData2()$LMIT == 1)),
      sum((myData2()$action_taken == 1) & (myData2()$MINT == 1)) /  sum((myData2()$action_taken <= 5) & (myData2()$MINT == 1))
    )
    
    Malepercent = sum((myData2()$action_taken == 1) & (myData2()$applicant_sex_name == "Male"))/sum((myData2()$action_taken <= 5) & (myData1()$applicant_sex_name == "Male"))
    MUIApproval = (sum(myData2()$action_taken == 1) - sum((myData2()$action_taken == 1) & (myData2()$LMIB == 1)))/
      (sum(myData2()$action_taken <= 5) - sum((myData2()$action_taken <= 5) & (myData2()$LMIB == 1)))
    
    MUiCensus  = (sum(myData2()$action_taken == 1) - sum((myData2()$action_taken == 1) & (myData2()$LMIT == 1)))/
      (sum(myData2()$action_taken <= 5) - sum((myData2()$action_taken <= 5) & (myData2()$LMIT == 1)))
    
    Minority = (sum(myData2()$action_taken == 1) - sum((myData2()$action_taken == 1) & (myData2()$MINT == 1)))/
      (sum(myData2()$action_taken <= 5) - sum((myData2()$action_taken <= 5) & (myData2()$MINT == 1)))
    
    
    Disparity = c(Percent[1]/Percent[1], Percent[2]/Percent[1], Percent[3]/Percent[1], Percent[4]/Percent[1],
                  Percent[5]/Percent[1], Percent[6]/Percent[1], Percent[7]/Percent[1], Percent[8]/Percent[1],
                  Percent[9]/Malepercent, Percent[10]/MUIApproval, Percent[11]/ MUiCensus, Percent[12]/Minority)
    
    
    Disparity = round(as.numeric(Disparity),2)
    Percent = (as.numeric(Percent))
    ApprovalsB = data.frame(Lender, Percent, Disparity)
    colnames(ApprovalsB) = c("Applicant characteristics",
                             "Approvals %", "Disparity Index")
    rm(Lender, Percent, Malepercent, MUIApproval, MUiCensus, Minority, Disparity)
    
    output$Approvals2 <- renderTable({
      
      ApprovalsB
      
    })
    Lender = c("White",
               "Black",
               "Hispanic/Latino",
               "Native American",
               "Asian",
               "HOPI",
               "Minority",
               "No Demographic Data",
               "Female",
               "LMI Applicant",
               "LMI Census Tract",
               "Minority/Majority Tract")
    
    Number = c(
      sum((myData2()$action_taken == 3) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 5)),
      sum((myData2()$action_taken == 3) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 3)),
      sum((myData2()$action_taken == 3) & (myData2()$applicant_ethnicity == 1)),
      sum((myData2()$action_taken == 3) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 1)),
      sum((myData2()$action_taken == 3) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 2)),
      sum((myData2()$action_taken == 3) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 4)),
      (sum(myData2()$action_taken == 3)) - sum((myData2()$action_taken == 3) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 5)) - 
        sum((myData2()$action_taken == 3) & (myData2()$applicant_ethnicity == 4)) -sum((myData2()$action_taken == 3) & (myData2()$applicant_ethnicity == 3)),
      sum((myData2()$action_taken == 3) & (myData2()$applicant_ethnicity == 3)),
      sum((myData2()$action_taken == 3) & (myData2()$applicant_sex_name == "Female")),
      sum((myData2()$action_taken == 3) & (myData2()$LMIB == 1)),
      sum((myData2()$action_taken == 3) & (myData2()$LMIT == 1)),
      sum((myData2()$action_taken == 3) & (myData2()$MINT == 1))
    )
    
    Percent = c(
      sum((myData2()$action_taken == 3) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 5))/
        sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 5)),
      sum((myData2()$action_taken == 3) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 3))/
        sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 3)),
      sum((myData2()$action_taken == 3) & (myData2()$applicant_ethnicity == 1))/sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 1)),
      sum((myData2()$action_taken == 3) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 1))/
        sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 1)),
      sum((myData2()$action_taken == 3) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 2))/
        sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 2)),
      sum((myData2()$action_taken == 3) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 4))/
        sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 4)),
      ((sum(myData2()$action_taken == 3)) - sum((myData2()$action_taken == 3) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 5)) - 
         sum((myData2()$action_taken == 3) & (myData2()$applicant_ethnicity == 4)) -sum((myData2()$action_taken == 3) & (myData2()$applicant_ethnicity == 3)))/
        ((sum(myData2()$action_taken <= 5)) - sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 5)) - 
           sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 4)) -sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 3))),
      sum((myData2()$action_taken == 3) & (myData2()$applicant_ethnicity == 3))/sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 3)),
      sum((myData2()$action_taken == 3) & (myData2()$applicant_sex_name == "Female"))/sum((myData2()$action_taken <= 5) & (myData2()$applicant_sex_name == "Female")),
      sum((myData2()$action_taken == 3) & (myData2()$LMIB == 1)) / sum((myData2()$action_taken <= 5) & (myData2()$LMIB == 1)),
      sum((myData2()$action_taken == 3) & (myData2()$LMIT == 1))/ sum((myData2()$action_taken <= 5) & (myData2()$LMIT == 1)),
      sum((myData2()$action_taken == 3) & (myData2()$MINT == 1)) /  sum((myData2()$action_taken <= 5) & (myData2()$MINT == 1))
    )
    
    Malepercent = sum((myData2()$action_taken == 3) & (myData2()$applicant_sex_name == "Male"))/sum((myData2()$action_taken <= 5) & (myData1()$applicant_sex_name == "Male"))
    MUIApproval = (sum(myData2()$action_taken == 3) - sum((myData2()$action_taken == 3) & (myData2()$LMIB == 1)))/
      (sum(myData2()$action_taken <= 5) - sum((myData2()$action_taken <= 5) & (myData2()$LMIB == 1)))
    
    MUiCensus  = (sum(myData2()$action_taken == 3) - sum((myData2()$action_taken == 3) & (myData2()$LMIT == 1)))/
      (sum(myData2()$action_taken <= 5) - sum((myData2()$action_taken <= 5) & (myData2()$LMIT == 1)))
    
    Minority = (sum(myData2()$action_taken == 3) - sum((myData2()$action_taken == 3) & (myData2()$MINT == 1)))/
      (sum(myData2()$action_taken <= 5) - sum((myData2()$action_taken <= 5) & (myData2()$MINT == 1)))
    
    
    Disparity = c(Percent[1]/Percent[1], Percent[2]/Percent[1], Percent[3]/Percent[1], Percent[4]/Percent[1],
                  Percent[5]/Percent[1], Percent[6]/Percent[1], Percent[7]/Percent[1], Percent[8]/Percent[1],
                  Percent[9]/Malepercent, Percent[10]/MUIApproval, Percent[11]/ MUiCensus, Percent[12]/Minority)
    
    
    Disparity = round(as.numeric(Disparity),2)
    Percent = (as.numeric(Percent))
    DenialsB = data.frame(Lender, Number, Percent, Disparity)
    colnames(DenialsB) = c("Applicant characteristics",
                           "Denials", "Percent", "Disparity Index")
    
    
    rm(Lender, Number, Percent, Malepercent, MUIApproval, MUiCensus, Minority, Disparity)
    
    output$Den2 <- renderTable({
      
      DenialsB
      
    })
    
    
    
    output$lenderAB1 <- renderText({ 
      paste("Geography name: ", strong(input$demography)," <br>", "Year(s): ", strong(unique(myData1()$as_of_year)),", ", strong(unique(myData2()$as_of_year)),
            "<br>", "Group A Lender(s): <br>",
            "Group B Lender(s): <br>", "Report Date: ",strong(Sys.Date()),"<br>","Date: ", strong(Sys.Date()), br(), br())
    })
    
    Lender = c("White",
               "Black",
               "Hispanic/Latino",
               "Native American",
               "Asian",
               "HOPI",
               "Minority",
               "No Demographic Data",
               "Female",
               "LMI Applicant",
               "LMI Census Tract",
               "Minority/Majority Tract")
    
    NumberA = c(sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 5)),
                sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 3)),
                sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 1)),
                sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 1)),
                sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 2)),
                sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 4)),
                (sum(myData1()$action_taken <= 5)) - sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 5)) - 
                  sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 4)) -sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 3)),
                sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 3)),
                sum((myData1()$action_taken <= 5) & (myData1()$applicant_sex_name == "Female")),
                sum((myData1()$action_taken <= 5) & (myData1()$LMIB == 1)),
                sum((myData1()$action_taken <= 5) & (myData1()$LMIT == 1)),
                sum((myData1()$action_taken <= 5) & (myData1()$MINT == 1))
    )
    
    A = c(NumberA[1]/sum(myData1()$action_taken <= 5),
          NumberA[2]/sum(myData1()$action_taken <= 5),
          NumberA[3]/sum(myData1()$action_taken <= 5),
          NumberA[4]/sum(myData1()$action_taken <= 5),
          NumberA[5]/sum(myData1()$action_taken <= 5),
          NumberA[6]/sum(myData1()$action_taken <= 5),
          NumberA[7]/sum(myData1()$action_taken <= 5),
          NumberA[8]/sum(myData1()$action_taken <= 5),
          NumberA[9]/sum(myData1()$action_taken <= 5),
          NumberA[10]/sum(myData1()$action_taken <= 5),
          NumberA[11]/sum(myData1()$action_taken <= 5),
          NumberA[12]/sum(myData1()$action_taken <= 5))
    
    rm(NumberA)
    
    NumberB = c(sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 5)),
                sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 3)),
                sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 1)),
                sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 1)),
                sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 2)),
                sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 4)),
                (sum(myData2()$action_taken <= 5)) - sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 5)) - 
                  sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 4)) -sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 3)),
                sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 3)),
                sum((myData2()$action_taken <= 5) & (myData2()$applicant_sex_name == "Female")),
                sum((myData2()$action_taken <= 5) & (myData2()$LMIB == 1)),
                sum((myData2()$action_taken <= 5) & (myData2()$LMIT == 1)),
                sum((myData2()$action_taken <= 5) & (myData2()$MINT == 1))
    )
    
    B = c(NumberB[1]/sum(myData2()$action_taken <= 5),
          NumberB[2]/sum(myData2()$action_taken <= 5),
          NumberB[3]/sum(myData2()$action_taken <= 5),
          NumberB[4]/sum(myData2()$action_taken <= 5),
          NumberB[5]/sum(myData2()$action_taken <= 5),
          NumberB[6]/sum(myData2()$action_taken <= 5),
          NumberB[7]/sum(myData2()$action_taken <= 5),
          NumberB[8]/sum(myData2()$action_taken <= 5),
          NumberB[9]/sum(myData2()$action_taken <= 5),
          NumberB[10]/sum(myData2()$action_taken <= 5),
          NumberB[11]/sum(myData2()$action_taken <= 5),
          NumberB[12]/sum(myData2()$action_taken <= 5))
    
    
    rm(NumberB)
    
    Disparity = c(B[1] - A[1], B[2] - A[2], B[3] - A[3], B[4] - A[4],
                  B[5] - A[5], B[6] - A[6], B[7] - A[7], B[8] - A[8],
                  B[9] - A[9], B[10] - A[10], B[11] - A[11], B[12] - A[12])
    
    
    
    Applications = data.frame(Lender, A, B, Disparity)
    
    colnames(Applications) = c("Applications", "Group A Lender(s) %", "Group B Lender(s) %", "Difference %")
    rm(A, B, Disparity)
    
    output$Applications3 <- renderTable({
      
      Applications
    })
    
    Number1 = c(sum((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 5)),
                sum((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 3)),
                sum((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 1)),
                sum((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 1)),
                sum((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 2)),
                sum((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 4)),
                (sum(myData1()$action_taken == 1)) - sum((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 5)) - 
                  sum((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 4)) -sum((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 3)),
                sum((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 3)),
                sum((myData1()$action_taken == 1) & (myData1()$applicant_sex_name == "Female")),
                sum((myData1()$action_taken == 1) & (myData1()$LMIB == 1)),
                sum((myData1()$action_taken == 1) & (myData1()$LMIT == 1)),
                sum((myData1()$action_taken == 1) & (myData1()$MINT == 1))
    )
    
    A = c(Number1[1]/sum(myData1()$action_taken == 1),
          Number1[2]/sum(myData1()$action_taken == 1),
          Number1[3]/sum(myData1()$action_taken == 1),
          Number1[4]/sum(myData1()$action_taken == 1),
          Number1[5]/sum(myData1()$action_taken == 1),
          Number1[6]/sum(myData1()$action_taken == 1),
          Number1[7]/sum(myData1()$action_taken == 1),
          Number1[8]/sum(myData1()$action_taken == 1),
          Number1[9]/sum(myData1()$action_taken == 1),
          Number1[10]/sum(myData1()$action_taken == 1),
          Number1[11]/sum(myData1()$action_taken == 1),
          Number1[12]/sum(myData1()$action_taken == 1))
    
    rm(Number1)
    Number2 = c(sum((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 5)),
                sum((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 3)),
                sum((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 1)),
                sum((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 1)),
                sum((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 2)),
                sum((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 4)),
                (sum(myData2()$action_taken == 1)) - sum((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 5)) - 
                  sum((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 4)) -sum((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 3)),
                sum((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 3)),
                sum((myData2()$action_taken == 1) & (myData2()$applicant_sex_name == "Female")),
                sum((myData2()$action_taken == 1) & (myData2()$LMIB == 1)),
                sum((myData2()$action_taken == 1) & (myData2()$LMIT == 1)),
                sum((myData2()$action_taken == 1) & (myData2()$MINT == 1))
    )
    
    B = c(Number2[1]/sum(myData2()$action_taken == 1),
          Number2[2]/sum(myData2()$action_taken == 1),
          Number2[3]/sum(myData2()$action_taken == 1),
          Number2[4]/sum(myData2()$action_taken == 1),
          Number2[5]/sum(myData2()$action_taken == 1),
          Number2[6]/sum(myData2()$action_taken == 1),
          Number2[7]/sum(myData2()$action_taken == 1),
          Number2[8]/sum(myData2()$action_taken == 1),
          Number2[9]/sum(myData2()$action_taken == 1),
          Number2[10]/sum(myData2()$action_taken == 1),
          Number2[11]/sum(myData2()$action_taken == 1),
          Number2[12]/sum(myData2()$action_taken == 1))
    
    rm(Number2)
    
    Disparity = c(B[1] - A[1], B[2] - A[2], B[3] - A[3], B[4] - A[4],
                  B[5] - A[5], B[6] - A[6], B[7] - A[7], B[8] - A[8],
                  B[9] - A[9], B[10] - A[10], B[11] - A[11], B[12] - A[12])
    
    
    Originations = data.frame(Lender, A, B, Disparity)
    
    colnames(Originations) = c("Originations", "Group A Lender(s) %", "Group B Lender(s) %", "Difference %")
    
    rm(Lender, A, B, Disparity)
    
    output$Orig3 <- renderTable({
      
      Originations
    })
    
    output$lenderAB2 <- renderText({ 
      paste("Geography name: ", strong(input$demography)," <br>", "Year(s): ", strong(unique(myData1()$as_of_year)),", ", strong(unique(myData2()$as_of_year)), "<br>", "Group A Lender(s): <br>",
            "Group B Lender(s): <br>", "Report Date: ",strong(Sys.Date()), "<br> Date: ", strong(Sys.Date()), br(),br())
    })
    
    Lender = c("White",
               "Black",
               "Hispanic/Latino",
               "Native American",
               "Asian",
               "HOPI",
               "Minority",
               "No Demographic Data",
               "Female",
               "LMI Applicant",
               "LMI Census Tract",
               "Minority/Majority Tract")
    
    
    
    Volume1 = c(sum(myData1()[which((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 ==5)), "loan_amount_000s"]),
                sum(myData1()[which((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 ==3)), "loan_amount_000s"]),
                sum(myData1()[which((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 1)), "loan_amount_000s"]),
                sum(myData1()[which((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 ==1)), "loan_amount_000s"]),
                sum(myData1()[which((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 ==2)), "loan_amount_000s"]),
                sum(myData1()[which((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 ==4)), "loan_amount_000s"]),
                sum(myData1()[which((myData1()$action_taken == 1)), "loan_amount_000s"]) - sum(myData1()[which((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 ==5)), "loan_amount_000s"]) -
                  sum(myData1()[which((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 4)), "loan_amount_000s"]) - sum(myData1()[which((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 3)), "loan_amount_000s"]),
                sum(myData1()[which((myData1()$action_taken == 1) & (myData1()$applicant_ethnicity == 3)), "loan_amount_000s"]),
                sum(myData1()[which((myData1()$action_taken == 1) & (myData1()$applicant_sex_name == "Female")), "loan_amount_000s"]),
                sum(myData1()[which((myData1()$action_taken == 1) & (myData1()$LMIB == 1)), "loan_amount_000s"]),
                sum(myData1()[which((myData1()$action_taken == 1) & (myData1()$LMIT == 1)), "loan_amount_000s"]),
                sum(myData1()[which((myData1()$action_taken == 1) & (myData1()$MINT == 1)), "loan_amount_000s"])    )
    
    A = c(Volume1[1]/ sum(myData1()[which((myData1()$action_taken == 1)), "loan_amount_000s"]),
          Volume1[2]/ sum(myData1()[which((myData1()$action_taken == 1)), "loan_amount_000s"]),
          Volume1[3]/ sum(myData1()[which((myData1()$action_taken == 1)), "loan_amount_000s"]),
          Volume1[4]/ sum(myData1()[which((myData1()$action_taken == 1)), "loan_amount_000s"]),
          Volume1[5]/ sum(myData1()[which((myData1()$action_taken == 1)), "loan_amount_000s"]),
          Volume1[6]/ sum(myData1()[which((myData1()$action_taken == 1)), "loan_amount_000s"]),
          Volume1[7]/ sum(myData1()[which((myData1()$action_taken == 1)), "loan_amount_000s"]),
          Volume1[8]/ sum(myData1()[which((myData1()$action_taken == 1)), "loan_amount_000s"]),
          Volume1[9]/ sum(myData1()[which((myData1()$action_taken == 1)), "loan_amount_000s"]),
          Volume1[10]/ sum(myData1()[which((myData1()$action_taken == 1)), "loan_amount_000s"]),
          Volume1[11]/ sum(myData1()[which((myData1()$action_taken == 1)), "loan_amount_000s"]),
          Volume1[12]/ sum(myData1()[which((myData1()$action_taken == 1)), "loan_amount_000s"])
    )
    Volume1 = round(1*as.numeric(Volume1))
    
    
    Volume2 = c(sum(myData2()[which((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 ==5)), "loan_amount_000s"]),
                sum(myData2()[which((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 ==3)), "loan_amount_000s"]),
                sum(myData2()[which((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 1)), "loan_amount_000s"]),
                sum(myData2()[which((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 ==1)), "loan_amount_000s"]),
                sum(myData2()[which((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 ==2)), "loan_amount_000s"]),
                sum(myData2()[which((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 ==4)), "loan_amount_000s"]),
                sum(myData2()[which((myData2()$action_taken == 1)), "loan_amount_000s"]) - sum(myData2()[which((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 ==5)), "loan_amount_000s"]) -
                  sum(myData2()[which((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 4)), "loan_amount_000s"]) - sum(myData2()[which((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 3)), "loan_amount_000s"]),
                sum(myData2()[which((myData2()$action_taken == 1) & (myData2()$applicant_ethnicity == 3)), "loan_amount_000s"]),
                sum(myData2()[which((myData2()$action_taken == 1) & (myData2()$applicant_sex_name == "Female")), "loan_amount_000s"]),
                sum(myData2()[which((myData2()$action_taken == 1) & (myData2()$LMIB == 1)), "loan_amount_000s"]),
                sum(myData2()[which((myData2()$action_taken == 1) & (myData2()$LMIT == 1)), "loan_amount_000s"]),
                sum(myData2()[which((myData2()$action_taken == 1) & (myData2()$MINT == 1)), "loan_amount_000s"])    )
    
    B = c(Volume2[1]/ sum(myData2()[which((myData2()$action_taken == 1)), "loan_amount_000s"]),
          Volume2[2]/ sum(myData2()[which((myData2()$action_taken == 1)), "loan_amount_000s"]),
          Volume2[3]/ sum(myData2()[which((myData2()$action_taken == 1)), "loan_amount_000s"]),
          Volume2[4]/ sum(myData2()[which((myData2()$action_taken == 1)), "loan_amount_000s"]),
          Volume2[5]/ sum(myData2()[which((myData2()$action_taken == 1)), "loan_amount_000s"]),
          Volume2[6]/ sum(myData2()[which((myData2()$action_taken == 1)), "loan_amount_000s"]),
          Volume2[7]/ sum(myData2()[which((myData2()$action_taken == 1)), "loan_amount_000s"]),
          Volume2[8]/ sum(myData2()[which((myData2()$action_taken == 1)), "loan_amount_000s"]),
          Volume2[9]/ sum(myData2()[which((myData2()$action_taken == 1)), "loan_amount_000s"]),
          Volume2[10]/ sum(myData2()[which((myData2()$action_taken == 1)), "loan_amount_000s"]),
          Volume2[11]/ sum(myData2()[which((myData2()$action_taken == 1)), "loan_amount_000s"]),
          Volume2[12]/ sum(myData2()[which((myData2()$action_taken == 1)), "loan_amount_000s"])
    )
    Volume2 = round(1*as.numeric(Volume2))
    
    
    Disparity = c(B[1] - A[1], B[2] - A[2], B[3] - A[3], B[4] - A[4],
                  B[5] - A[5], B[6] - A[6], B[7] - A[7], B[8] - A[8],
                  B[9] - A[9], B[10] - A[10], B[11] - A[11], B[12] - A[12])
    
    Disparity = (as.numeric(Disparity))
    Volumes = data.frame(Lender, Volume1, Volume2, Disparity)
    colnames(Volumes) = c("Volume of Loans", "Group A Lender(s)", "Group B Lender(s)", "Difference %")
    rm(A,B,Volume1, Volume2, Disparity)
    
    output$Volume <- renderTable({
      
      Volumes
      
    })
    
    
    NumberA = c(
      sum((myData1()$action_taken == 3) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 5)),
      sum((myData1()$action_taken == 3) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 3)),
      sum((myData1()$action_taken == 3) & (myData1()$applicant_ethnicity == 1)),
      sum((myData1()$action_taken == 3) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 1)),
      sum((myData1()$action_taken == 3) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 2)),
      sum((myData1()$action_taken == 3) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 4)),
      (sum(myData1()$action_taken == 3)) - sum((myData1()$action_taken == 3) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 5)) - 
        sum((myData1()$action_taken == 3) & (myData1()$applicant_ethnicity == 4)) -sum((myData1()$action_taken == 3) & (myData1()$applicant_ethnicity == 3)),
      sum((myData1()$action_taken == 3) & (myData1()$applicant_ethnicity == 3)),
      sum((myData1()$action_taken == 3) & (myData1()$applicant_sex_name == "Female")),
      sum((myData1()$action_taken == 3) & (myData1()$LMIB == 1)),
      sum((myData1()$action_taken == 3) & (myData1()$LMIT == 1)),
      sum((myData1()$action_taken == 3) & (myData1()$MINT == 1))
    )
    
    A = c(
      sum((myData1()$action_taken == 3) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 5))/
        sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 5)),
      sum((myData1()$action_taken == 3) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 3))/
        sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 3)),
      sum((myData1()$action_taken == 3) & (myData1()$applicant_ethnicity == 1))/sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 1)),
      sum((myData1()$action_taken == 3) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 1))/
        sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 1)),
      sum((myData1()$action_taken == 3) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 2))/
        sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 2)),
      sum((myData1()$action_taken == 3) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 4))/
        sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 4)),
      ((sum(myData1()$action_taken == 3)) - sum((myData1()$action_taken == 3) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 5)) - 
         sum((myData1()$action_taken == 3) & (myData1()$applicant_ethnicity == 4)) -sum((myData1()$action_taken == 3) & (myData1()$applicant_ethnicity == 3)))/
        ((sum(myData1()$action_taken <= 5)) - sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 2) & (myData1()$applicant_race_1 == 5)) - 
           sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 4)) -sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 3))),
      sum((myData1()$action_taken == 3) & (myData1()$applicant_ethnicity == 3))/sum((myData1()$action_taken <= 5) & (myData1()$applicant_ethnicity == 3)),
      sum((myData1()$action_taken == 3) & (myData1()$applicant_sex_name == "Female"))/sum((myData1()$action_taken <= 5) & (myData1()$applicant_sex_name == "Female")),
      sum((myData1()$action_taken == 3) & (myData1()$LMIB == 1)) / sum((myData1()$action_taken <= 5) & (myData1()$LMIB == 1)),
      sum((myData1()$action_taken == 3) & (myData1()$LMIT == 1))/ sum((myData1()$action_taken <= 5) & (myData1()$LMIT == 1)),
      sum((myData1()$action_taken == 3) & (myData1()$MINT == 1)) /  sum((myData1()$action_taken <= 5) & (myData1()$MINT == 1))
    )
    
    
    
    NumberB = c(
      sum((myData2()$action_taken == 3) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 5)),
      sum((myData2()$action_taken == 3) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 3)),
      sum((myData2()$action_taken == 3) & (myData2()$applicant_ethnicity == 1)),
      sum((myData2()$action_taken == 3) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 1)),
      sum((myData2()$action_taken == 3) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 2)),
      sum((myData2()$action_taken == 3) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 4)),
      (sum(myData2()$action_taken == 3)) - sum((myData2()$action_taken == 3) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 5)) - 
        sum((myData2()$action_taken == 3) & (myData2()$applicant_ethnicity == 4)) -sum((myData2()$action_taken == 3) & (myData2()$applicant_ethnicity == 3)),
      sum((myData2()$action_taken == 3) & (myData2()$applicant_ethnicity == 3)),
      sum((myData2()$action_taken == 3) & (myData2()$applicant_sex_name == "Female")),
      sum((myData2()$action_taken == 3) & (myData2()$LMIB == 1)),
      sum((myData2()$action_taken == 3) & (myData2()$LMIT == 1)),
      sum((myData2()$action_taken == 3) & (myData2()$MINT == 1))
    )
    
    B = c(
      sum((myData2()$action_taken == 3) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 5))/
        sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 5)),
      sum((myData2()$action_taken == 3) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 3))/
        sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 3)),
      sum((myData2()$action_taken == 3) & (myData2()$applicant_ethnicity == 1))/sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 1)),
      sum((myData2()$action_taken == 3) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 1))/
        sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 1)),
      sum((myData2()$action_taken == 3) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 2))/
        sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 2)),
      sum((myData2()$action_taken == 3) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 4))/
        sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 4)),
      ((sum(myData2()$action_taken == 3)) - sum((myData2()$action_taken == 3) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 5)) - 
         sum((myData2()$action_taken == 3) & (myData2()$applicant_ethnicity == 4)) -sum((myData2()$action_taken == 3) & (myData2()$applicant_ethnicity == 3)))/
        ((sum(myData2()$action_taken <= 5)) - sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 2) & (myData2()$applicant_race_1 == 5)) - 
           sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 4)) -sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 3))),
      sum((myData2()$action_taken == 3) & (myData2()$applicant_ethnicity == 3))/sum((myData2()$action_taken <= 5) & (myData2()$applicant_ethnicity == 3)),
      sum((myData2()$action_taken == 3) & (myData2()$applicant_sex_name == "Female"))/sum((myData2()$action_taken <= 5) & (myData2()$applicant_sex_name == "Female")),
      sum((myData2()$action_taken == 3) & (myData2()$LMIB == 1)) / sum((myData2()$action_taken <= 5) & (myData2()$LMIB == 1)),
      sum((myData2()$action_taken == 3) & (myData2()$LMIT == 1))/ sum((myData2()$action_taken <= 5) & (myData2()$LMIT == 1)),
      sum((myData2()$action_taken == 3) & (myData2()$MINT == 1)) /  sum((myData2()$action_taken <= 5) & (myData2()$MINT == 1))
    )
    
    Disparity = c(B[1] - A[1], B[2] - A[2], B[3] - A[3], B[4] - A[4],
                  B[5] - A[5], B[6] - A[6], B[7] - A[7], B[8] - A[8],
                  B[9] - A[9], B[10] - A[10], B[11] - A[11], B[12] - A[12])
    
    
    Disparity = (as.numeric(Disparity))
    Denials = data.frame(Lender, NumberA, NumberB, Disparity)
    colnames(Denials) = c("Denials", "Group A Lender(s)", "Group B Lender(s)", "Difference %")
    
    rm(A,B,NumberA, NumberB, Disparity, Lender)
    
    output$Den3 <- renderTable({
      
      Denials
      
    })
    
    output$HMDAdataByPage <- downloadHandler(
      filename = function() {
        "HMDAdataByPage.xlsx"
      },
      content = function(file) {
        write.xlsx(Population, file, sheetName = "Population Demographics", append = FALSE)
        write.xlsx(Geography, file, sheetName = "Geography Demographics", append = TRUE)
        write.xlsx(ApplicationsA, file, sheetName = "Lender A Applications", append = TRUE)
        write.xlsx(OriginationsA, file, sheetName = "Lender A Origination", append = TRUE)
        write.xlsx(ApprovalsA, file, sheetName = "Lender A Approvals", append = TRUE)
        write.xlsx(DenialsA, file, sheetName = "Lender A Denials", append = TRUE)
        write.xlsx(ApplicationsB, file, sheetName = "Lender B Applications", append = TRUE)
        write.xlsx(OriginationsB, file, sheetName = "Lender B Origination", append = TRUE)
        write.xlsx(ApprovalsB, file, sheetName = "Lender B Approvals", append = TRUE)
        write.xlsx(DenialsB, file, sheetName = "Lender B Denials", append = TRUE)
        write.xlsx(Applications, file, sheetName = "Lender A vs Lender B Applications", append = TRUE)
        write.xlsx(Originations, file, sheetName = "Lender A vs Lender B Originations", append = TRUE)
        write.xlsx(Volumes, file, sheetName = "Lender A vs Lender B Volumes", append = TRUE)
        write.xlsx(Denials, file, sheetName = "Lender A vs Lender B Denials", append = TRUE)
        
        
        
      }
    )
    
  })
  
  
  session$onSessionEnded(stopApp)
})
shinyApp(ui = ui, server = server)