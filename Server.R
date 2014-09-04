#HML Query -- Shiny implementation
#Server

library(shiny)
source("Functions.R")
shinyServer(function(input, output){
                                    output$TestQuery <- renderText({
									                                #Function to create a query -- used later in the code
																	fun_makequery = function(inp_vec) #individual parts of inp_vec - sel,frm,inj,jon,whr,gby,hav 
																	{
																	 # Get a list of all variable names
																	 inp_ops        <- str_sub(str_trim(inp_vec),-3,-1)
																	 
																	 # Create a vector of the functions to apply to each sql operator statement
																	 custom_fun_operator_apply     <- str_c("fun_operator_", inp_ops, collapse = NULL)
																	 
																	 ###DZ code (slight modifications - KK)###
																	 #string slicing function
																	 sss = c(strsplit(inp_vec, "_"))
																	 
																	 fun.list = as.vector(fun_names()[grep("^fun", fun_names())])
																	 
																	 sss2 = strsplit(fun.list, "_")
																	 
																	 fun.df = data.frame("fun_name" = fun.list, "suffix" = str_slice_fun(sss2, 3),stringsAsFactors = F)
																	 
																	 str_list = list()
																	 for (z in 1:length(sss)) {
																	 if(nchar(eval(parse(text = paste0(inp_vec[z]))))== 0) {str_list[[z]] = c("")}
																	 else{
																	 str_list[[z]] =
																	 eval(parse(text = paste0(as.name(fun.df[fun.df$suffix == str_slice_fun(sss, 6)[z], "fun_name"]), "(", inp_vec[z], ")")))
																	                         }}
																							 
																	 # This is the final HML String created
																	 str_c(unlist(str_list), sep = "", collapse = "\n")
																	 
																	 # Easy to view format for the final constructed query
																	 return(str_c(unlist(str_list), sep = "", collapse = "\n"))
																	 }
																	 
																	 #Processing the user inputs starts from here
																	 #User input validation 
																	 validate(
																	          need(all(as.numeric(input$cas_var_01_02)<=10,as.numeric(input$cas_var_01_02)>0,as.numeric(input$cas_var_02_02)<=10,as.numeric(input$cas_var_02_02)>0),"Enter a valid percentile value "),
																	          need(all(input$cas_var_02_02>input$cas_var_01_02),"the second value should be greater than the first threshold value"),
																	          need(all(as.numeric(fun_Vec_create(input$fil_input_cd_01))>1000,as.numeric(fun_Vec_create(input$fil_input_cd_01))<9999),"Division Number has an invalid argument"),
																	          need(all(as.Date(input$fil_input_dt_01_03,'%Y-%m-%d')<Sys.Date())," End date cannot be in the Future"),
																	          need(all(as.numeric(input$fil_input_in_02_03)>0,as.numeric(input$fil_input_in_02_03)<999),"Please check the number of weeks entered please make sure its a valid integer"),
																			  need(all(as.numeric(substr(fun_Vec_create(input$fil_input_cd_02),1,nchar(fun_Vec_create(input$fil_input_cd_02))-3))>0,nchar(fun_Vec_create(input$fil_input_cd_02))<10,nchar(fun_Vec_create(input$fil_input_cd_02))>3,substr(fun_Vec_create(input$fil_input_cd_02),nchar(fun_Vec_create(input$fil_input_cd_02))-2,nchar(fun_Vec_create(input$fil_input_cd_02))-2) == "-",utf8ToInt(paste0(substr(fun_Vec_create(input$fil_input_cd_02),nchar(fun_Vec_create(input$fil_input_cd_02))-1,nchar(fun_Vec_create(input$fil_input_cd_02))),collapse="")) >=65,utf8ToInt(paste0(substr(fun_Vec_create(input$fil_input_cd_02),nchar(fun_Vec_create(input$fil_input_cd_02))-1,nchar(fun_Vec_create(input$fil_input_cd_02))),collapse="")) <=90), "Product Number has an invalid argument")
																	         )
																	 
																	 
																	 
									
                                                                    #Assigning/constructing user inputs based on their types
																	#Pure variable vectors
                                                                    Sel_pur_01                     <- c(if(input$pur_var_01){"LINKED_LYLTY_CARD_NBR"})
                                                                    Sel_pur_02                     <- c(if(input$pur_var_01){"LINKED_LYLTY_CARD_NBR"})
                                                                    Sel_pur_03                     <- c(if(input$pur_var_01){"LINKED_LYLTY_CARD_NBR"})
                                                                    # Aggregate variable Vectors
                                                                    Sel_agg_01                     <- c(paste0(if(input$cas_var_01 == "SALES"){paste0("SUM(TOT_AMT_INCLD_GST)")} 
                                                                                                      else {if(input$cas_var_01 == "UNITS"){paste0("SUM(PROD_QTY)")} else{paste0("COUNT(DISTINCT IDX_TXN_ID)")}},"AS RANK_RESULT"))																	
                                                                    Sel_agg_02                     <- c(if(input$agg_var_01){"RANK_RESULT"},paste0("(ROW_NUMBER() OVER (ORDER BY RANK_RESULT DESC) *10 / (COUNT (*) OVER () + 1) ) AS DECILE"))
                                                                    Sel_agg_03                     <- c(if(input$agg_var_01){"RANK_RESULT"},if(input$agg_var_02){"Decile"},paste0("CAST('",input$cas_var_01,"' AS CHAR(20)) AS RANK_TYPE"))
																	
                                                                    # Case Variable Vectors
                                                                    Sel_cas_01                     <- c()
																	Sel_cas_02                     <- c()
																	Sel_cas_03                     <- c(if(input$agg_var_03){fun_operator_case("SEGMENT_NAME",paste0("WHEN (DECILE < ",input$cas_var_01_02,") THEN 'Heavy'"),
                                                                                                      paste0("WHEN (DECILE < ",input$cas_var_02_02, ") THEN 'Medium'"),paste0("ELSE 'Light'" ))})
                                                                                                                                        
                                                                    # Where Filters
                                                                    
                                                                    Fil_whr_01                     <- c(if(input$fil_bool_01){paste0("A.DIVISION_NBR in(",c("'"), paste0(fun_Vec_create(input$fil_input_cd_01), collapse = "', '"), c("')"))},
                                                                                                      c("TOT_AMT_INCLD_GST > 0"),c("PROD_QTY > 0"),
                                                                                                      if(input$fil_bool_03){paste0("START_TXN_DATE BETWEEN DATE_ADD_WEEK(-",input$fil_input_in_02_03,", CAST(",input$fil_input_dt_01_03," AS DATE)) \n AND (CAST (",input$fil_input_dt_01_03," AS DATE))")},
                                                                                                      if(input$fil_bool_04|input$fil_bool_02){paste0(c("(",paste0(c(if(input$fil_bool_04){paste0("POSITION(TRIM(SGMNT_CODE) IN (",c("'"), paste0(fun_Vec_create(input$fil_input_cd_04), collapse = " "), c("')) > 0"))},if(input$fil_bool_02){paste0("A.PROD_NBR in(",c("'"), paste0(fun_Vec_create(fun_Vec_create(input$fil_input_cd_02)), collapse = "', '"), c("')"))}),collapse = " OR "),")"),collapse = " ")})
																	Fil_whr_02                     <- c()
                                                                    Fil_whr_03                     <- c()								  
                                                                    # Having Filters
                                                                    
                                                                    Fil_hav_01                     <- c()
																	Fil_hav_02                     <- c()
                                                                    Fil_hav_03                     <- c()
																	
                                                                    #Generate operator level variables																	
                                                                    #Select
                                                                    sqlvec_mod_01_opr_01_sel    <- paste0(c(Sel_pur_01, Sel_agg_01, Sel_cas_01),collapse = ", \n")
																	sqlvec_mod_02_opr_01_sel    <- paste0(c(Sel_pur_02, Sel_agg_02, Sel_cas_02),collapse = ", \n")
																	sqlvec_mod_03_opr_01_sel    <- paste0(c(Sel_pur_03, Sel_agg_03, Sel_cas_03),collapse = ", \n")
																	
																	#Inner Join
																	sqlvec_mod_01_opr_03_inj    <- c("PROD_EDW_WIL.DIM_PROD_ARTICLE_CURR_V PROD")
																	sqlvec_mod_02_opr_03_inj    <- c("") #function call
																	sqlvec_mod_03_opr_03_inj    <- c("") #function call
																	
																	#Join on
																	sqlvec_mod_01_opr_04_jon    <- c("A.PROD_NBR = PROD.PROD_NBR \nAND A.DIVISION_NBR = PROD.DIVISION_NBR")
																	sqlvec_mod_02_opr_04_jon    <- c("") #function call
																	sqlvec_mod_03_opr_04_jon    <- c("") #function call
																	
																	#Where
																	sqlvec_mod_01_opr_05_whr    <- paste0(c(Fil_whr_01),collapse = "\n AND \n")
																	sqlvec_mod_02_opr_05_whr    <- paste0(c(Fil_whr_02),collapse = "\n AND \n")
																	sqlvec_mod_03_opr_05_whr    <- paste0(c(Fil_whr_03),collapse = "\n AND \n")
																	
																	#Group By
																	sqlvec_mod_01_opr_06_gby    <- paste0(c(Sel_pur_01),collapse = ", \n")
																	sqlvec_mod_02_opr_06_gby    <- c("") #paste0(c(Sel_pur_02),collapse = ", \n")
																	sqlvec_mod_03_opr_06_gby    <- c("") #paste0(c(Sel_pur_03),collapse = ", \n")
																	
																	#Having
																	sqlvec_mod_01_opr_07_hav    <- paste0(c(Fil_hav_01),collapse = "\n AND \n")
																	sqlvec_mod_02_opr_07_hav    <- paste0(c(Fil_hav_02),collapse = "\n AND \n")
																	sqlvec_mod_03_opr_07_hav    <- paste0(c(Fil_hav_03),collapse = "\n AND \n")
																	
																	#Constructing lower most query and upwards -- using query fun_makequery() and other operator level functions in Functions.R
																	sqlvec_mod_01_opr_02_frm    <- c("PROD_EDW_WIL.IDX_CUST_PROD_SALE_SUMMARY_V")
																	varnames    <- unlist(ls())
																	inp_vec01          <- sort(unique(varnames[str_detect(varnames, "^sql") & str_length(varnames) == 24 & str_sub(str_trim(varnames),12,13) == "01"]))
																	sqlvec_mod_02_opr_02_frm    <- fun_makequery(inp_vec01)
																	
																	varnames    <- unlist(ls())
																	inp_vec02          <- sort(unique(varnames[str_detect(varnames, "^sql") & str_length(varnames) == 24 & str_sub(str_trim(varnames),12,13) == "02"]))
																	sqlvec_mod_03_opr_02_frm    <- fun_makequery(inp_vec02)
																	
																	varnames    <- unlist(ls())
																	inp_vec03          <- sort(unique(varnames[str_detect(varnames, "^sql") & str_length(varnames) == 24 & str_sub(str_trim(varnames),12,13) == "03"]))
																	TestQuery          <- fun_makequery(inp_vec03)
																	
																	TestQuery
																	
  
                                                                   }
																  )
                                   }
		   )
