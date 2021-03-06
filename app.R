library(shiny)
library(shinythemes)
library(shinycssloaders)
source("src/helper_county.R")
# Global variables can go here
zip <- "94587"
nppl <- 20

css <- HTML(".html-widget.gauge svg {height: 350px;width: 900px;}")

# Define the UI
ui <- fluidPage(theme=shinytheme("superhero"),
  titlePanel("COVID-19 Risk Score Calculator"),
  tags$head(tags$style(css)),
  #INPUT
  sidebarLayout(
    sidebarPanel(
      helpText("Answer a few questions to see your COVID-19 risk score:", class = "lead"),
      #textInput('fips', label =  '5-digit FIPS code of your county', fips),
      textInput('zip', label =  "What's your 5-digit zip code?", zip),
      sliderInput('nppl', 'How many people will you see in person in a week? (Play with me after you click the button!)', min = 0, max = 50, value = nppl, step =1),
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
                 fluidRow(withSpinner(gaugeOutput("gauge"), type = 1), style = "height:350px;"),
                 fluidRow(textOutput("res"), style = "width:800px")),
        #tabPanel("Map"),
        tabPanel("Methodology",
                 htmlOutput("methods"))),
      width = 9
    )
  )
)


# Define the server code
server <- function(input, output) {
  temp<- eventReactive(input$go, {
    #validate input types
    validate(
      need(input$zip!="", 'Please provide a zip code.'),
      need(input$nppl, 'Please provide the number of contacts.')
    )
    #read in FIPS or get it from ZIP
    fips<-get_fips_from_zip(input$zip)
    #get county-level characteristics
    #county_pop<-1e6
    #county_name<-"Alameda"
    #county_casecount<-500
    #county_underreport<-0.2
    county_pop <- get_county_pop(fips)
    county_name <- get_county_name(fips)
    county_casecount <- get_county_casecount(fips, latest_day)
    county_underreport <- calc_county_underreport(fips)
    
    unlist(list("fips" = fips,
                "county_pop" = county_pop,
                "county_name" = county_name,
                "county_casecount" = county_casecount,
                "county_underreport" = county_underreport
    ))
  })
  
  output$gauge <-renderGauge({
    temp <- temp()
    county_casecount<-temp['county_casecount']%>%as.numeric()
    county_pop<-temp['county_pop']%>%as.numeric()
    county_underreport<-temp['county_underreport']%>%as.numeric()
    if (input$nppl>0){
      risk <- 1-(1-county_casecount/county_pop/county_underreport)^input$nppl
    } else{
      risk <- 0
    }
    g<-function(x){
      # a mapping function to address nonlinearity between probability and score
      prob_flu<- 35.5/327.2/52
      normalized<-log10(x/prob_flu)*25+50 
      # 50 means equal likelihood of flu
      # 1 means 1/100 times probability of flu
      # 100 means 100 times probability of flu
      return(normalized)
    }
    score<-if_else(risk>0, g(risk), 0)
    # use the checkboxInput
    if(input$is_sick | input$in_highriskzone){
      score<-max(50, score)
    }
   
    gauge(case_when(score > 100 ~ 100,
                    score < 0 ~ 0,
                    TRUE ~ round(score)), 
          min = 0, max = 100, 
          sectors = gaugeSectors(success = c(0, 30),
                                 warning = c(30, 70),
                                 danger = c(70, 100)),
          label = "")
  })
  
  output$res <-renderText({
    temp <- temp()
    county_casecount<-temp['county_casecount']%>%as.numeric()
    county_pop<-temp['county_pop']%>%as.numeric()
    county_underreport<-temp['county_underreport']%>%as.numeric()
    if (input$nppl>0){
      risk <- 1-(1-county_casecount/county_pop/county_underreport)^input$nppl
    } else{
      risk <- 0
    }
    g<-function(x){
      # a mapping function to address nonlinearity between probability and score
      prob_flu<- 35.5/327.2/52
      normalized<-log10(x/prob_flu)*25+50 
      # 50 means equal likelihood of flu
      # 1 means 1/100 times probability of flu
      # 100 means 100 times probability of flu
      return(normalized)
    }
    score<-if_else(risk>0, g(risk), 0)
    # use the checkboxInput
    if(input$is_sick | input$in_highriskzone){
      score<-max(50, score)
    }
    prob_flu<- 35.5/327.2/52
    paste('You live in county:', temp['county_name'], '.',
          'Your county has', temp['county_casecount'], 'cases out of a population of', 
          format(temp['county_pop']%>%as.numeric(), big.mark = ','), '.',
          "We estimated that your county's sepcific under-reporting factor is", scales::percent(temp['county_underreport']%>%as.numeric()), '.',
          "Our estimation of the probability of you being exposed to COVID-19 through community transmission is", scales::percent(risk), '.',
          "For comparison, your risk of being exposed to flu is", scales::percent(prob_flu), '.', 
          "On a scale of 0  (low risk) to 100 (high risk), your risk score is", round(score), '.')
  })
  
  output$methods <-renderUI({
    tagList(tags$p(""),
            div(
              "We used published ",
              tags$a("county-level data of COVID-19 cases & deaths", href="https://www.nytimes.com/article/coronavirus-county-data-us.html"),
              " to estimate the prevalence of infected people within your county. Based on this likely prevalence, and the amount of social distancing you're able to accomplish, we can determine the likelihood you'll be exposed to COVID-19."
            ),
            tags$p(""),
            tags$h3("Assumptions:"),
            tags$li(
              "Above and beyond the official cases reported by your county, there are additional unreported cases of COVID-19. We followed methodology reported",
              tags$a("by Russell et al (2020)", href="https://cmmid.github.io/topics/covid19/severity/global_cfr_estimates.html"),
              "to calculate the % of cases that are current detected. This goes into our calculation of the estimated number of cases distributed throughout your community."),
            tags$li("Other methods of becoming infected (e.g. touching an infected surface) are not accounted for by this calculator."),
            tags$p(""),
            tags$p("We'll be doing our best to update these assumptions as additional knowledge about the virus becomes available."),
    )
#      
#      "",
#      "Sources:",
#      "County-level COVID-19 data: ",
#      "Under-reporting factor: "
  })
}


# Return a Shiny app object
shinyApp(ui = ui, server = server)