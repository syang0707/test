library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(DT)
library(dplyr)
library(stringr)
library(shinyjs)
# setwd("~/OneDrive - Universal Weather and Aviation/R/Demo/BeautyEngine")
setwd("./")
Contact <- read.csv("Contact.csv", stringsAsFactors = F) 
# ui.R ####
ui <- dashboardPage(
    skin = "black",
    dashboardHeader(
    title = "Beauty Engine"
            ),
    dashboardSidebar(
        sidebarMenu(
            menuItem(
                "Contact", icon=icon("phone"), tabName = "tab_contact"), 
            menuItem(
                "AC DATA", icon=icon("plane-departure"), tabName = "tab_engine")
        )
    ),
    dashboardBody(
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"),
        # tags$head(includeScript("google-analytics.js")),
        tags$head(includeHTML("google-analytics.html")),
        # tags$img(align="left",src="https://d39dp4bekf8wag.cloudfront.net/wp-content/uploads/2020/06/universal-aviation-fbo-ground-services-from-universal-weather-and-aviation.svg",height="70px"),
        tags$li(class = "dropdown",
                tags$a(href="https://insyght.universalweather.com/UWA/fbo1#/movements/", target="_blank", 
                       tags$img(height = "70px", alt="Universal Aviation", src="https://d39dp4bekf8wag.cloudfront.net/wp-content/uploads/2020/06/universal-aviation-fbo-ground-services-from-universal-weather-and-aviation.svg")
                )
        ),
        tabItems(
            tabItem(tabName = "tab_contact",
                    fluidPage(
                        fluidRow(column(6, verbatimTextOutput("pollText"))),
                        fluidRow(
                            pickerGroupUI(
                                id = "contact.filters",
                                btn_label = NULL,
                                params = list(
                                    Airport = list(inputId = "Airport", label = "空港:"),
                                    Company.Name = list(inputId = "Company.Name", label = "会社名:"),
                                    Purpose = list(inputId = "Purpose", label = "関連:")
                                ),
                                options = pickerOptions(hideDisabled = TRUE, 
                                                        liveSearch = TRUE,
                                                        liveSearchPlaceholder = "Type it",
                                                        liveSearchStyle = "contains")
                            ),
                            actionLink("button", "Reset", icon = icon("redo"))),
                        infoBox(title = textOutput("company"), subtitle = textOutput("purpose"),
                                icon = icon("phone-alt", lib = "font-awesome"), color = "olive", width = 12,
                                value = uiOutput("call")),
                        column(DT::dataTableOutput("contact"), width = 12)
                    )
            ),
            tabItem(tabName = "tab_engine",
                    fluidPage(
                        fluidRow(h6("使い方："),
                                 h6("1、入力欄で「レジ番」を入力して、「Enter」をクリックする"),
                                 h6("2、下に、航空機国籍、騒音値と諸元資料は出る")),
                        searchInput(
                            inputId = "ac.data",
                            label = "Click search icon to update or hit 'Enter'", 
                            placeholder = "Type an A/C Registration",
                            btnSearch = icon("search"), 
                            btnReset = icon("remove"),
                            width = "100%"
                        ),
                        hr(),
                        fluidRow(
                            infoBox(title = "Registration Prefix", subtitle = "If incorrect data/no data is shown, please contact Beauty.",
                                    value = DT::dataTableOutput("ac.data.country"),
                                    icon = icon("flag"),
                                    color = "red", width = 12),
                            infoBox(title = "MTOW & NOISE VALUE", subtitle = "Unit of Noise Level is in decibels (EPNdB).", 
                                    value = DT::dataTableOutput("ac.data.noise"),
                                    icon = icon("broadcast-tower"),
                                    color = "yellow", width = 12),
                            infoBox(title = "TYPE INFO", subtitle = "All of registered data on Insyght will be shown.", 
                                    value = DT::dataTableOutput("ac.data.type"),
                                    icon = icon("ruler"),
                                    color = "green", width = 12)
                        )
                    )
            ) 
        ) 
    )
)

server <- function(input, output, session) {
    
    output$ac.data.country <- renderDT({
        data.country <- read.csv("AC.data.country.csv", stringsAsFactors = F)
        Result <- data.country[1,] %>% select(Registration.Country, Alpha.2.Code, Alpha.3.Code)
        for (i in seq_along(data.country$Reg.Prefix)) {
            if (str_detect(toupper(input$ac.data), pattern = data.country$Reg.Prefix[i]) == T){
                Result <- data.country[i,] %>% select(Registration.Country, Alpha.2.Code, Alpha.3.Code)
            }
        }
        Result %>% DT::datatable(options = list(scrollY = 50,
                                                scrollX = 600,
                                                deferRender = F,
                                                scroller = F,
                                                paging = F,
                                                dom = 'ltipr',
                                                fixedColumns = T,
                                                info = F),
                                 rownames = FALSE,
                                 style = 'bootstrap')
    })
    output$ac.data.noise <- renderDT({
        data.noise <- read.csv("AC.data.noise.csv", stringsAsFactors = F) 
        names(data.noise)[6] <-  "NOISE.VALUE"
        for (i in seq_along(data.noise$REG)) {
            if (toupper(input$ac.data) == data.noise$REG[i]){
                Result <- data.noise[i,] %>% 
                    select(MTOW, KG, NOISE.VALUE, FLYOVER, APPROACH, LATERAL) 
            }
        }
        Result %>% DT::datatable(options = list(scrollY = 50,
                                                scrollX = 600,
                                                deferRender = F,
                                                scroller = F,
                                                paging = F,
                                                dom = 'ltipr',
                                                fixedColumns = T,
                                                info = F), 
                                 rownames = FALSE,
                                 style = 'bootstrap')
    })
    output$ac.data.type <-  renderDT({
        data.noise <- read.csv("AC.data.noise.csv", stringsAsFactors = F)
        names(data.noise)[3] <-  "TYPE"
        for (i in seq_along(data.noise$REG)) {
            if (toupper(input$ac.data) == data.noise$REG[i]){
                TYPE <- data.noise$TYPE[i]
            }
        }
        ac.data.type <- read.csv("AC.data.type.csv")
        Result <- ac.data.type %>% 
            subset(ICAO.code == TYPE) %>% 
            select(ICAO.code, Manufacturer, Wing.span.in.meter, Wing.area.in.m2, Length.in.meter, Height.in.meter, Number.of.seats, Number.of.engines) %>% 
            arrange()
        Result %>% DT::datatable(options = list(scrollY = 100,
                                                scrollX = 600,
                                                deferRender = F,
                                                scroller = F,
                                                paging = F,
                                                dom = 'ltipr',
                                                fixedColumns = T,
                                                info = F), 
                                 rownames = FALSE,
                                 style = 'bootstrap')
    })
    res_mod <- callModule(
        module = pickerGroupServer,
        id = "contact.filters",
        data = Contact,
        vars = c("Airport", "Company.Name", "Purpose")
    )
    
    output$contact <- renderDT({
        res_mod() %>% 
            DT::datatable(filter = 'top', extensions = c('Scroller'), editable = F,
                          selection = "single",
                          options = list(scrollY = 500,
                                         scrollX = 600,
                                         deferRender = F,
                                         scroller = T,
                                         paging = TRUE,
                                         pageLength = 10,
                                         searchHighlight = TRUE,
                                         info = F,
                                         dom = 'lBfrtip',
                                         fixedColumns = TRUE),
                          rownames = FALSE,
                          colnames = c("空港", "会社名", "関連", "電話", "備考"),
                          style = 'bootstrap') 
    })
    
    enginelog <- tempfile('logfile', fileext = '.txt')
    
    an_observe_func = observe(suspended=T, {
        input$contact_rows_selected
        isolate({
            company <- paste(res_mod()[input$contact_rows_selected, 1], res_mod()[input$contact_rows_selected, 2])
            purpose <- res_mod()[input$contact_rows_selected, 3]
            tel <- res_mod()[input$contact_rows_selected, 4]
            call <- res_mod()[input$contact_rows_selected, 4] %>% 
                str_replace_all(pattern = "^0{1}", "tel:+81") %>% 
                str_replace_all(pattern = "-", "")
            output$company <- renderText(company)
            output$purpose <- renderText(purpose)
            output$call <- renderUI(a(href=call, tel))
            cat(paste(Sys.time(), "/", company, "/", purpose, "/", tel), '\n', file = enginelog, append = T)
        })
    })
    
    pollData <- reactivePoll(1000, session,
                             checkFunc = function() {
                                 if (file.exists(enginelog))
                                     file.info(enginelog)$mtime[1]
                                 else
                                     ""
                             },
                             valueFunc = function() {
                                 if (file.exists(enginelog))
                                 readLines(enginelog)
                                 else
                                     readLines("")
                             }
    )
    
    output$pollText <- renderText({
        text <- pollData()
        text[is.na(text)] <- ""
        paste(text, collapse = '\n')
        filename <- str_remove_all(as.character(Sys.time()), pattern = "2020|-|:| |JST") %>% str_sub(1,8)
        write.csv(text, paste0("Log.", filename, ".csv"))
    })
    
    an_observe_func$resume()
    
    observeEvent(input$button, {
        session$reload()
    })
}
# RunAPP ####
shinyApp(ui, server)

