library(shiny)
library(shinythemes)
library(shinycssloaders)
source("src/helper_county.R")
# Global variables can go here
fips <- ''
zip <- "94587"
nppl_hi <- 20
nppl_lo <- 5

#css <- HTML(".html-widget.gauge_hi svg {height: 350px; width: 400px}
#            .html-widget.gauge_lo svg {height: 350px; width: 400px}")

# Define the UI
ui <- fluidPage(theme=shinytheme("superhero"),
  titlePanel("COVID-19 Risk Score Calculator"),
  #tags$head(tags$style(css)),
  #INPUT
  sidebarLayout(
    sidebarPanel(
      helpText("Answer a few questions to see your COVID-19 risk score:", class = "lead"),
      textInput('fips', label =  '5-digit FIPS code of your county', fips),
      textInput('zip', label =  "If you don't know your county FIPS code, what's your 5-digit zip code?", zip),
      numericInput('nppl_hi', 'Max number of contacts you have had in the past two weeks', nppl_hi),
      numericInput('nppl_lo', 'Min number of contacts you have had in the past two weeks', nppl_lo),
      #sliderInput('fac_underreport', "Choose what percentage of cases are tested?", min = 0.01, max = 1, value = 0.15, step = 0.01),
      checkboxInput('is_sick', "Are you sick already?"),
      #checkboxInput('in_hosp', "Do you work in a hospital?"),
      checkboxInput('in_highriskzone', "Do you live in or have you visited in the past two weeks an area where the transmission is widespread?"),
      actionButton('go', "Calculate my risk score", class = "btn-primary"),
      width =3
    ),
    #OUTPUT
    mainPanel(
      tabsetPanel(
        tabPanel("Plot",
                 fluidRow(
                   column(width=6, withSpinner(gaugeOutput("gauge_hi", width = "40%"), type = 1)),
                   column(width=6, gaugeOutput("gauge_lo", width = "40%"))),
                 fluidRow(textOutput("res"), style = "width:800px")),
        #tabPanel("Map"),
        tabPanel("Methodology",
                 textOutput("methods"))),
      width = 9
    )
  )
)


# Define the server code
server <- function(input, output) {
  temp<- eventReactive(input$go, {
    #validate input types
    validate(
      need(input$fips!="" | input$zip!="", 'Provide at least one of the two: FIPS and zip code.'),
      need(input$nppl_hi & input$nppl_lo, 'Please provide the number of contacts.'),
      need(input$nppl_hi >0, 'The max number of people should be >0.')
    )
    #read in FIPS or get it from ZIP
    if(input$fips!=""){
      fips<-input$fips
    }else{
      fips<-get_fips_from_zip(input$zip)
    }
    #get county-level characteristics
    county_pop <- get_county_pop(fips)
    county_name <- get_county_name(fips)
    county_casecount <- get_county_casecount(fips, latest_day)
    county_underreport <- calc_county_underreport(fips)
    
    risk_hi <- 1-(1-get_county_casecount(fips, latest_day)/get_county_pop(fips)/calc_county_underreport(fips))^input$nppl_hi
    if (input$nppl_lo>0){
      risk_lo <- 1-(1-get_county_casecount(fips, latest_day)/get_county_pop(fips)/calc_county_underreport(fips))^input$nppl_lo
    } else{
      risk_lo <- 0
    }
    g<-function(x){
      # a mapping function to address nonlinearity between probability and score
      assertthat::assert_that(x>=0 && x<=1)
      return((log(x)+8)/8)
    }
    score_hi<-if_else(risk_hi>0, g(risk_hi)*100, 0)
    score_lo<-if_else(risk_lo>0, g(risk_lo)*100, 0)
    
    unlist(list("fips" = fips,
                "county_pop" = county_pop,
                "county_name" = county_name,
                "county_casecount" = county_casecount,
                "county_underreport" = county_underreport,
                "risk_hi"= risk_hi,
                "risk_lo" = risk_lo,
                "score_hi" = score_hi,
                "score_lo" = score_lo
    ))
  })
  output$gauge_hi <-renderGauge({
    temp <- temp()
    score_hi <- round(as.numeric(temp['score_hi']))
    gauge(score_hi, 
          min = 0, max = 100, 
          sectors = gaugeSectors(success = c(0, 30),
                                 warning = c(30, 70),
                                 danger = c(70, 100)),
          label = "Upper bound")
  })
  output$gauge_lo <-renderGauge({
    temp <- temp()
    score_lo <- round(as.numeric(temp['score_lo']))
    gauge(score_lo, 
          min = 0, max = 100, 
          sectors = gaugeSectors(success = c(0, 30),
                                 warning = c(30, 70),
                                 danger = c(70, 100)),
          label = "Lower bound")
  })
  output$res <-renderText({
    temp <- temp()
    paste('You live in county:', temp['county_name'], '.',
          'Your county has', temp['county_casecount'], 'cases out of a population of', 
          format(temp['county_pop']%>%as.numeric(), big.mark = ','), '.',
          'We estimated the under-reporting factor is', scales::percent(temp['county_underreport']%>%as.numeric()), '.',
          "The probability of you contracting COVID-19 ranges", 
          scales::percent(temp['risk_lo']%>%as.numeric()), 'to', scales::percent(temp['risk_hi']%>%as.numeric()), '.',
          "On a scale of 0 to 100, your risk score ranges", 
          round(temp['score_lo']%>%as.numeric()), 'to', round(temp['score_hi']%>%as.numeric()), ".")
  })
  output$methods <-renderText({
    "Methodology placeholder (George)."
  })
}


# Return a Shiny app object
shinyApp(ui = ui, server = server)