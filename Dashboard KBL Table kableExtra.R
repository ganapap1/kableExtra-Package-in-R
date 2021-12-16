# Load required Libraries
library(shiny)
library(shinyjs)           
library(shinydashboard)
library(shinydashboardPlus)
library(kableExtra)
library(shinyWidgets)
library(shinyalert)
library(dplyr)
library(DT)

detach("package:shinydashboardPlus", unload = TRUE)


#################################################################################
#Actionbutton style function height 30px and width 155px
#################################################################################
styleButtonBlue<- function(){
  "white-space: normal;
                        text-align:center;
                        color: #ffffff; 
                        background-color:#4682B4;
                        border-color: #ffffff;
                        border-width:3px;
                        height:30px;
                        width:155px;
                        font-size: 13px;"
}

'%!in%' <- Negate('%in%')  # define 'not in' func
options(scipen=999)  # prevents printing scientific notatio



#################################################################################
# header which is part of ui 
#################################################################################
header <- shinydashboardPlus::dashboardHeader(
  title = "Design Beautiful Table with kableExtra Package in R",
  titleWidth = '550px',
  tags$li(
    a( 
      splitLayout(
        cellWidths = c("50%", "50%"),
        actionButton(inputId = "mRscriptImport", label = "Upload R Script!",style=styleButtonBlue()),
        actionButton(inputId = "mDatasetImport", label = "Upload Dataset!",style=styleButtonBlue())
      ),
      href = NULL,
      style = "cursor: pointer;height:15px;"
    ),
    class = "dropdown"
    
  )
)



sidebar <- shinydashboardPlus::dashboardSidebar(
  useShinyjs(),
  # Remove the sidebar toggle element
  tags$script(JS("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")),
  minified = TRUE,  #this option will completely close the side bar  and expand the header text
  id = 'msidebarid',
  collapsed = TRUE
  
)

body <- dashboardBody(
  shinyjs::useShinyjs(),
  useShinyalert(),
  uiOutput(outputId = 'mCompleteUI')
  
)#dashboard body


ui <- dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body
)




server <- function(input, output, session) {
  # #######################################################################
  # this is to hide right side bar
  # #######################################################################
  shinyjs::addCssClass(selector = "body", class = "sidebar-collapse")
  onevent("mouseenter", "sidebarCollapsed", shinyjs::removeCssClass(selector = "body", class = "sidebar-collapse"))
  onevent("mouseleave", "sidebarCollapsed", shinyjs::addCssClass(selector = "body", class = "sidebar-collapse"))

  disable(id = 'mDatasetImport') 
  
  
  ########################################################################
  # Declare reactive variable
  ########################################################################
  vmy <- reactiveValues(mydata=NULL,myrscriptdf=NULL)
  

  
  
  
  output$mCompleteUI <- renderUI({
    validate(
      need(vmy$myrscriptdf, 'import R Script!'),
      # need(vmy$mydata != '', 'Import Dataset')
    )
    
    
    fluidRow(
      tags$head(
        tags$style(
          "#box_parameter{color:black; font-size:11px; font-style:normal;height: 475px;
overflow:auto; max-height: 475px; background: #ffffff;text-align: left;}"
        )
      ),
      box(
        id="box_parameter",
        width = 3,
        height = '525px',
        title = 'Play with Parameters',
        status = "warning",
        solidHeader = TRUE,
        collapsible = FALSE,
        tabsetPanel(
          tabPanel(title = "Table",
                   tags$br(),
                   prettyCheckboxGroup(
                     inputId  = "mtablerelated",
                     label    = 'Table Options', 
                     choices  = c(dplyr::filter(vmy$myrscriptdf, Grp_Code=='TABLE')$Steps),
                     selected = c(dplyr::filter(vmy$myrscriptdf, Grp_Code=='TABLE')$Steps)[1],
                     inline   = FALSE,
                     status   = "danger"
                   ),
                   
                   prettyCheckboxGroup(
                     inputId = "mformatrelated",
                     label   = 'Format Options', 
                     choices = c(dplyr::filter(vmy$myrscriptdf, Grp_Code=='PIPE')$Steps),
                     inline  = FALSE,
                     status  = "danger"
                   ),
                   
                   prettyRadioButtons(
                     inputId = "mformatNumberColor",
                     label   = 'Format Number & Color', 
                     choices = c(dplyr::filter(vmy$myrscriptdf, Grp_Code=='NUM_FORMAT')$Steps),
                     inline  = FALSE,
                     status  = "danger"
                   ),
                   
                   prettyRadioButtons(
                     inputId = "mCaptionOptions",
                     label   = 'Header Options', 
                     choices = c(dplyr::filter(vmy$myrscriptdf, Grp_Code=='CAPTION')$Steps),
                     inline  = FALSE,
                     status  = "danger"
                   ),
                   
                   prettyCheckboxGroup(
                     inputId = "mFooterOptions",
                     label   = 'Footer Options', 
                     choices = c(dplyr::filter(vmy$myrscriptdf, Grp_Code=='FOOTER')$Steps),
                     inline  = FALSE,
                     status  = "danger"
                   ),
                   
                   prettyCheckboxGroup(
                     inputId = "mscrollingFilesave",
                     label   = 'Others', 
                     choices = c(dplyr::filter(vmy$myrscriptdf, Grp_Code=='OTHERS')$Steps),
                     inline  = FALSE,
                     status  = "danger"
                   )
          ), #tabpanel closure
          tabPanel(
            title = 'Grouping',
            tags$br(),
            align='center',
            prettyCheckbox(inputId = 'mColGrouping',label = "I need Column Grouping",
                           value = FALSE,status = 'danger',shape = 'square'),
            
            splitLayout(cellWidths = c('25%','25%','25%','25%'),
                        HTML(paste(" Lower Level", '<br>', "Group ->")),
                        numericInput(inputId = 'm1stSubGrp',label = NULL,value = 2,width = '75px'),
                        numericInput(inputId = 'm2ndSubGrp',label = NULL,value = 2,width = '75px'),
                        numericInput(inputId = 'm3rdSubGrp',label = NULL,value = 2,width = '75px')
            ),
            align='center',
            splitLayout(cellWidths = c('26%','37%','37%'),
                        HTML(paste(" Middle Level", '<br>', "Group ->")),
                        numericInput(inputId = 'm1stGroup',label = NULL,value = 3,width = '100px'),
                        numericInput(inputId = 'm2ndGroup',label = NULL,value = 3,width = '100px')
                        
            ),
            align='center',
            splitLayout(cellWidths = c('26%','74%'),
                        HTML(paste(" Top Level", '<br>', "Group ->")),
                        numericInput(inputId = 'mtopGroup',label = NULL,value = (6),width = '200px'),
            ),
            
            prettyCheckboxGroup(
              inputId = "mRowGrouping",
              label   = 'Grouping Row Based Option 1',
              choices = c(dplyr::filter(vmy$myrscriptdf, Grp_Code=='ROW_GROUP')$Steps),
              inline  = FALSE,
              status  = "danger"
            ),
            
            HTML(paste('<h6><b>',"Grouping Rows Option2: first check the box and", "then select the element to group",'</b>')),
            tags$br(),    tags$br(),
            prettyCheckbox(inputId = 'mGrpColumnYesNo',label = 'Click to get rows groupped',inline = TRUE,bigger = TRUE,
                           value = FALSE,status ='danger'),
            
            prettyRadioButtons(
              inputId = "mColumnAutoGroup",
              label = NULL,
              choices = c('upload dataset','options will appear '),
              inline = FALSE,
              status = "danger"
            )
          ), #tabpanel closure
          tabPanel(
            title = 'Cleansing',
            tags$br(),
            selectInput(
              inputId  = 'mMovetoFront',
              label    = "Move Column to Front",
              choices  = NULL,
              selected = NULL,
              multiple = FALSE),
            
            prettyCheckboxGroup(
              inputId = "mColumnHide",
              label   = 'Hide Column(s)', 
              choices = c('upload dataset','options will appear '),
              inline  = FALSE,
              status  = "danger"
            )
          ) #tabpanel closure
        )# tabsetpanel closure
      ),
      box(
        width = 9,
        height = '525px',
        status='warning',
        tags$style(HTML("
        .tabs-above > .nav > li[class=active] > a {
           background-color: #000;
           color: #FFF;
        }")),
        tabsetPanel(
          tabPanel(
            title = "KBL Table",
            tags$head(
              tags$style(
                paste0("#mKblTable{color:black; font-size:11px; font-style:bold;overflow-y:scroll;font-family: 'Lucida Sans Unicode';
                                            width: '100%';height: 460px;max-height: 460px; background: #ffffff;text-align: left;}")
              )
            ),
            htmlOutput(outputId = 'mKblTable')
          ),
          tabPanel(
            title = "Code-KBL Table Related",
            tags$head(
              tags$style(
                paste0("#mKblTableTXT{color:black; font-size:11px; font-style:bold;overflow-y:scroll;font-family: 'Lucida Sans Unicode';
                                            width: '100%';height: 460px;max-height: 460px; background: #ffffcd;text-align: left;}")
              )
            ),
            textAreaInput(inputId ='mKblTableTXT',label = NULL,width = '100%',height = '460px',value = NULL )
          ),
          tabPanel(
            title = "Code-Initialization",
            tags$head(
              tags$style(
                paste0("#mInitializeTXT{color:black; font-size:11px; font-style:bold;overflow-y:scroll;font-family: 'Lucida Sans Unicode';
                                            width: '100%';height: 460px;max-height: 460px; background: #ffffe7;text-align: left;}")
              )
            ),
            textAreaInput(inputId ='mInitializeTXT',label = NULL,width = '100%',height = '460px',value = NULL ) 
          ), #tabpanel closure
          tabPanel(
            title = "Code-Data Cleansing",
            tags$head(
              tags$style(
                paste0("#mCleansingTXT{color:black; font-size:11px; font-style:bold;overflow-y:scroll;font-family: 'Lucida Sans Unicode';
                                            width: '100%';height: 460px;max-height: 460px; background: #d4ebf2;text-align: left;}")
              )
            ),
            textAreaInput(inputId ='mCleansingTXT',label = NULL,width = '100%',height = '460px',value = NULL ) 
          ) #tabpanel closure
        )
      ) #column closure
    ) #fluidrow closure
  })
  
  
  
  
  
  

  ########################################################################
  # uploading R script Excel file
  ########################################################################
  observeEvent(input$mRscriptExcel,{
    ext <- tools::file_ext(input$mRscriptExcel$name)
    if (ext != "xls" & ext != 'xlsx'){
      shinyalert("Oops!", "Hi R Script is in Excel file ...!", type = "error")
      return()
    }else
    {
      tryCatch({      
        vmy$myrscriptdf<- data.frame(readxl::read_excel(input$mRscriptExcel$datapath))
      },error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))})
      #### delete NA rows code start
      vmy$myrscriptdf <- na.omit(vmy$myrscriptdf)
      vmy$myrscriptdf <- vmy$myrscriptdf[complete.cases(vmy$myrscriptdf), ]
      
      removeModal()
      enable(id = 'mDatasetImport')
    } 
    
  })
  
  
  ########################################################################
  # uploading csv file
  ########################################################################
  observeEvent(input$file,{
    if (length(input$mRscriptExcel)==0){
      shinyalert("Oops!", "Hi first import R Script Excel file ...!", type = "error")
      return()
    }
    
    ext <- tools::file_ext(input$file$name)
    
    if (ext != "csv"){
      shinyalert("Oops!", "Hi import only csv file...!", type = "error")
      return()
    }else
    {
      tryCatch({           
        vmy$mydata <- as.data.frame(read.csv(input$file$datapath,
                                             header = TRUE)[1:100,]
        )
      },error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))})

    
    #### delete NA rows code start
    vmy$mydata <- na.omit(vmy$mydata)
    vmy$mydata <- vmy$mydata[complete.cases(vmy$mydata), ]
    x_RowName <- rownames(vmy$mydata)
    rownames(vmy$mydata) <- NULL
    vmy$mydata <- cbind(x_RowName,vmy$mydata)
    
    
    muniqueCol <- c()
    for (i in names(vmy$mydata)){
      if (length(unique(vmy$mydata[,i])) <= 10){
        muniqueCol<- c(muniqueCol,i)
        
      }
    }
    updatePrettyRadioButtons(session,inputId = 'mColumnAutoGroup',choices = c(muniqueCol) ,selected = NULL,inline = FALSE,
                             prettyOptions = list(animation = "pulse", status = "danger"))
    updatePrettyCheckboxGroup(session,inputId = 'mColumnHide',choices = names(vmy$mydata) ,selected = NULL,inline = FALSE,
                              prettyOptions = list(animation = "pulse", status = "danger"))
    updateSelectInput(session,inputId = 'mMovetoFront',choices =  names(vmy$mydata),selected =  names(vmy$mydata)[1])
    removeModal()
    }
  })
  
  
  observeEvent(c(input$m1stGroup,input$m2ndGroup),{
    updateNumericInput(session,inputId = 'mtopGroup',value = (input$m1stGroup + input$m2ndGroup))
  })
  
  
  
  ########################################################################
  # every input for these variables will run the function fnDataProcessing
  ########################################################################
  observeEvent(
    c(input$file,input$mtablerelated,input$mformatrelated,input$mformatNumberColor,
      input$mColGrouping,input$mRowGrouping,input$mGrpColumnYesNo,
      input$mscrollingFilesave,input$mColumnAutoGroup,input$mMovetoFront,
      input$mColumnHide,input$mCaptionOptions,input$mFooterOptions),{
        
        fnDataProcessing()
      })

  
  fnputdoublehash <- function(mtext){
    paste0("#",mtext ,'\n','#####################################','\n')
  }
  
  ########################################################################
  # Data Processing and building table
  ########################################################################
  fnDataProcessing <- function(){
    minitialize <- ""
    mnumbercode <- ""
    mkblcode <- ""
    mcleanse <- ""
    mautogrp <- ""
    
    ww <-filter(vmy$myrscriptdf, Grp_Code %in% c("GET_FILE"))
    if (nrow(ww)!=0){
      for (i in 1:nrow(ww)){
        minitialize <- paste0(minitialize,'\n\n',fnputdoublehash(ww[i,3]), ww[i,4])
      }
    }
    
    xx <-filter(vmy$myrscriptdf, Steps %in% c(input$mformatNumberColor))
    if (nrow(xx)!=0){
      mnumbercode <- paste0('\n',fnputdoublehash(xx[1,3]),xx[1,4])
    }
    
    
    xx22 <-filter(vmy$myrscriptdf, Grp_Code %in% c("HIDE_MOVE"))
    if (nrow(xx22)!=0){
      for (i in 1:nrow(xx22)){
        mcleanse <- paste0(mcleanse,'\n\n',fnputdoublehash(xx22[i,3]), xx22[i,4])
      }
    }
    
    if (input$mGrpColumnYesNo==TRUE){
      xx55 <-filter(vmy$myrscriptdf, Grp_Code %in% c("AUTOGROUP"))
      if (nrow(xx55)!=0){
        for (i in 1:nrow(xx55)){
          mautogrp <- paste0(mautogrp,'\n\n',fnputdoublehash(xx55[i,3]), xx55[i,4])
        }
      }  
    }
    
    
    yy <-filter(vmy$myrscriptdf, Steps %in% c(input$mtablerelated))
    if (nrow(yy)!=0){
      for (i in 1:nrow(yy)){
        if (i==nrow(yy)){
          xxcc <-filter(vmy$myrscriptdf, Steps %in% c(input$mCaptionOptions))
          if (nrow(xxcc)!=0){
            mcaption <- paste0('\n\n',fnputdoublehash(xxcc[1,3]),xxcc[1,4])
          }
          mkblcode <- paste0("#TABLE SCRIPT STARTS HERE",mkblcode,'\n\n',fnputdoublehash(yy[i,3]),yy[i,4],",",mcaption,")")
        }else
        {
          mkblcode <- paste0(mkblcode,'\n\n',fnputdoublehash(yy[i,3]),yy[i,4],",")
        }
      }
    }
    
    yy77 <-filter(vmy$myrscriptdf, Steps %in% c(input$mformatrelated))
    if (nrow(yy77)!=0){
      for (i in 1:nrow(yy77)){
        mkblcode <- paste0(mkblcode," %>% ",'\n\n',fnputdoublehash(yy77[i,3]),yy77[i,4])
      }
    }
    
    ff1 <-filter(vmy$myrscriptdf, Steps %in% c(input$mRowGrouping))
    if (nrow(ff1)!=0){
      for (i in 1:nrow(ff1)){
        mkblcode <- paste0(mkblcode," %>% ",'\n\n',fnputdoublehash(ff1[i,3]),ff1[i,4])
      }
    }
    
    if (input$mColGrouping==TRUE){
      ff2 <-filter(vmy$myrscriptdf, Grp_Code %in% c("COL_GROUP"))
      if (nrow(ff2)!=0){
        for (i in 1:nrow(ff2)){
          mkblcode <- paste0(mkblcode," %>% ",'\n\n',fnputdoublehash(ff2[i,3]),ff2[i,4])
        }
      }      
    }
    
    
    if (input$mGrpColumnYesNo == TRUE & length(input$mColumnAutoGroup)>0){
      # mgrptxt <- paste0("pack_rows(index = table(paste(input$mColumnAutoGroup,'-',as.character(mydf$",input$mColumnAutoGroup,"))))")
      # mkblcode <- paste0(mkblcode," %>% ",'\n\n',"#","Row Automatic Grouping",'\n','#########################','\n', mgrptxt)
      # 
      ff5 <-filter(vmy$myrscriptdf, Grp_Code %in% c("COLORSUBTOTAL"))
      if (nrow(ff5)!=0){
        for (i in 1:nrow(ff5)){
          mkblcode <- paste0(mkblcode," %>% ",'\n\n',fnputdoublehash(ff5[i,3]),ff5[i,4])
        }
      }   
    }
    
    gg <-filter(vmy$myrscriptdf, Steps %in% c(input$mFooterOptions))
    if (nrow(gg)!=0){
      for (i in 1:nrow(gg)){
        mkblcode <- paste0(mkblcode," %>% ",'\n\n',fnputdoublehash(gg[i,3]),gg[i,4])
      }
    }
    
    
    
    kk <-filter(vmy$myrscriptdf, Steps %in% c(input$mscrollingFilesave))
    if (nrow(kk)!=0){
      for (i in 1:nrow(kk)){
        mkblcode <- paste0(mkblcode," %>% ",'\n\n',fnputdoublehash(kk[i,3]),kk[i,4])
      }
    }
    
    updateTextAreaInput(session,inputId = 'mInitializeTXT',value =paste(minitialize,'\n\n',mautogrp,'\n\n', mnumbercode))
    updateTextAreaInput(session,inputId = 'mCleansingTXT',value =paste(mcleanse))
    updateTextAreaInput(session,inputId = 'mKblTableTXT',value =mkblcode)
    
  }
  
  
  ########################################################################
  # run code in the text area in put field in all three locations
  ########################################################################
  output$mKblTable <- renderUI({
    if (length(input$file)==0){
      return()
    }
    HTML(eval(parse(text = input$mInitializeTXT)))
    HTML(eval(parse(text = input$mCleansingTXT)))
    HTML(eval(parse(text = input$mKblTableTXT)))
  })  
  
 
  
  #################################################################################
  # File dataset upload code starts here
  #################################################################################
  
  observeEvent(input$mRscriptImport,{
    showModal(
      modalDialog(
        size = 's',title = 'Import R Script',
        align = "center",
        box(
          width = 12,
          background = 'yellow',
          fileInput("mRscriptExcel",
                    label = 'Select R Script Excel',
                    multiple = FALSE,
                    accept = c("Excel",".xlsx,.xls"))
        ),
        easyClose = TRUE

      )
    )
  })
  
  observeEvent(input$mDatasetImport,{
    showModal(
      modalDialog(
        size = 's',
        align = "center",
        title = 'Upload Dataset',
        box(
          width = 12,
          background = 'yellow',
        fileInput("file",
                  label = 'Select Dataset ONLY CSV',
                  multiple = FALSE,
                  accept = c("csv",".csv"))
        ),
        easyClose = TRUE
      )
    )
  })
   
  
  
}

shinyApp(ui, server)
