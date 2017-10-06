library(shiny)
library(ggplot2)
library(datasets)
library(data.table)
library(shinydashboard)
library(shinythemes)

load("n_loan_status.RData")
plot.new()


fluidPage(
    
#link to css
tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "main.css")
),

#page title
titlePanel(title=div(img(src="logo.JPG"))),
tabsetPanel(
    tabPanel("Attributes Dashboard", 
        dashboardPage(
            dashboardHeader(title=""),
            dashboardSidebar( width = 350,
                  list(sliderInput("int_rate_range", "Select Interst Rate Range:",
                                   min = 5, max = 30, step=1,
                                   value = c(5,30)),
                       sliderInput("loan_amnt_range", "Select Loan Amount Range:", min=500, max=35000, value = c(500,35000)),
                       checkboxGroupInput("loan_term_check", "Select Loan Term:",
                                          c("36 Months"=" 36 months","60 Months"=" 60 months"),
                                          selected =c(" 36 months"," 60 months") ),
                       dateRangeInput("issue_date_range", label="Select Issued Date Range", start = "2006-12-01", end = "2016-08-01", min = "2006-12-01",
                                      max = "2016-08-01", format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                                      language = "en", separator = " to ", width = NULL),
                       checkboxGroupInput("loan_status_check", "Select Loan Status:",
                                          n_loan_status,
                                          selected = n_loan_status)
                       
                       
                  )               
                              
                              
                              
            ),
            dashboardBody(    fluidRow( box(dataTableOutput("show_plot1")),
                          box(plotOutput("show_plot2"), height = "auto",width = 2,title="Loan Amount Boxplot",solidHeader = TRUE, status = "primary"),
                          box(plotOutput("show_plot3"), width=2,title="Payment Amounts Boxplot",solidHeader = TRUE, status = "primary"),
                          box(plotOutput("show_plot4"),width = 2,title="Recieved Interest Boxplot",solidHeader = TRUE, status = "primary"),
                          box(plotOutput("show_plot6"), title="Interest Rate Histogram",solidHeader = TRUE, status = "primary"),
                          box(plotOutput("show_plot7"),title="Loan Amount Histogram",solidHeader = TRUE, status = "primary"),
                          box(plotOutput("show_plot8"),title="Time-Series for Number of New Loans",solidHeader = TRUE, status = "primary"),
                          box(plotOutput("show_plot9"),title="Loan Term Pie Chart",solidHeader = TRUE, status = "primary"),
                          box(plotOutput("show_plot10"),title="Loan Status Barchart",solidHeader = TRUE, status = "primary"),
                          box(plotOutput("show_plot11"),title="Borrower's Home Ownership",solidHeader = TRUE, status = "primary"),
                          box(plotOutput("show_plot12"),title="Borrower's Employment Length",solidHeader = TRUE, status = "primary")
            )
                          
                          )
        )    
    ),
    tabPanel("Loan-Related vs Borrower-Related Attributes",
        dashboardPage(
            dashboardHeader(title=""),
            dashboardSidebar( radioButtons("Loan_vs_borrower", 
                                "Select Correlation Plot:", 
                                selected="loan_borrower",
                                 c("Loan-Related vs. Borrower-Related" = "loan_borrower",
                                  "Borrower-Related vs. Borrower-Related" = "borrower_borrower",
                                    "Loan-Related vs. Loan-Related" = "loan_loan"))),#,
                                #sliderInput("confLevel",label="Select confidence level:",min=0.8, max=0.95, step = 0.05, value = 0.9)),
            dashboardBody( fluidRow( box(htmlOutput("show_text1")),
            plotOutput("show_cor_plot", width = 1200, height=1300)))
        )
    )
)
        

)