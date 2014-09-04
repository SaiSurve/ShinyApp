#HML Query -- Shiny implementation
#User Interface

#Load relevant libraries
library(shiny)
library(stringr)



shinyUI(
        navbarPage("Retail Analytics Interface",
                   tabPanel("HML Segments"       
                            ,titlePanel(HTML('<p><img src="logo.png"/></p>'))
                            ,sidebarLayout(
                                          sidebarPanel(
                                                       # Pure Variables of Select Input
                                                       h4("Select the Required Columns:")
													   ,checkboxInput("pur_var_01", "Loyalty Card Number", TRUE)
#													   ,checkboxInput("pur_var_02", "Customer Number", FALSE)

													   # Derived/Aggregate Variables of Select Input
													   ,checkboxInput("agg_var_01", "Rank Result", FALSE)
													   ,checkboxInput("agg_var_02", "Decile of Customer", FALSE)
													   ,checkboxInput("agg_var_03", "Segment Name", FALSE)
													   ,br()
													   # Case Variables of Select Input
                                                       ,h4("Options Available Based on Selected Columns:")
													   ,conditionalPanel(condition = "input.agg_var_01 |input.agg_var_02|input.agg_var_03|input.agg_var_04"
                                                                         ,selectInput("cas_var_01", "Ranking of shoppers based on:"
                                                                                     ,list("Sales" = "SALES","Units" = "UNITS","Visits" = "VISITS")
																					 )
                                                                        )
                                                       ,conditionalPanel(condition = "input.agg_var_03"
                                                                         ,h5("Choose Decile Threshod for Segments"),numericInput("cas_var_01_02", label = "Heavy Shopper if Decile <",value = "2")
																		 ,numericInput("cas_var_02_02", label = "Light Shopper if Decile >=",value = "5")
                                                                        )
													   ,br()
                                                       # Filters Required
                                                       ,h4("Put Filters on:")
                                                       ,checkboxInput("fil_bool_01", "Division Number", FALSE)
                                                       ,conditionalPanel(condition = "input.fil_bool_01"
                                                                        ,textInput("fil_input_cd_01", label = "Division Number in (comma delimited):",value = "1005")
                                                                        )
                                                       ,checkboxInput("fil_bool_02", "Product Number", FALSE)
                                                       ,conditionalPanel(condition = "input.fil_bool_02"
                                                                        ,textInput("fil_input_cd_02", label = "Product Number in (comma delimited):",value = "380000-EA")
                                                                        )
                                                       ,checkboxInput("fil_bool_03", "Start Transaction Date", FALSE)
                                                       ,conditionalPanel(condition = "input.fil_bool_03","Range:"
                                                                        ,div(class='row',div(class="span2 offset1"
																		                    ,dateInput("fil_input_dt_01_03", label = "End Date",value = Sys.Date()-1)
																		                    )
                                                                                        ,div(class="span2 offset1"
																						    ,numericInput("fil_input_in_02_03", label = "# Weeks",value = 54)
																							)
                                                                            )                                                                        
                                                                        )
                                                       ,checkboxInput("fil_bool_04", "Product Segment Code", FALSE)
                                                       ,conditionalPanel(condition = "input.fil_bool_04"
                                                                        ,textInput("fil_input_cd_04", label = "Segment Code in (comma delimited):",value = "1005")
                                                                        )                                                 
                                                       ,tags$hr()
                                                      )#end of sidebarPanel
										  ,mainPanel(
                                                     tabsetPanel(
                                                                 tabPanel("Code",verbatimTextOutput("TestQuery"))
                                                                ,tabPanel("Results",tableOutput("table"))
                                                                ,tabPanel("Summary",plotOutput("plot"))
                                                                )
                                                    )#end of mainPanel																
                                         )#end of sidebarLayout
                           )#end of tabPanel
                  )#end of navbarPage 
      )#end of shinyUI


