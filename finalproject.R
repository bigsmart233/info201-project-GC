library(shiny)
library(tidyverse)
library(tidyr)
library(rsconnect)

totalPop <- read_delim("data1/Region-Table 1.csv")

clean_duration <- read_delim("data1/Duration of homelessness-Table 1.csv")
clean <- clean_duration[1:4]

char <- read_delim("data1/Demographics-Table 1.csv")
newChar <- char %>% 
  select_if(~ !any(is.na(.)))

shelter <- read_delim('data1/Shelter-Table 1.csv')
newShelter <- shelter %>% 
  select_if(~ !any(is.na(.)))

ui <- fluidPage(h1("Homelessness in Seattle, WA"),tabsetPanel(
  tabPanel("Introduction",titlePanel("Homelessness in Seattle"), sidebarLayout(
    sidebarPanel(h3("Project Overview:"),
      p("Homelessness is a growing issue not just in Washington state, but across the United States. 
        According to the 2020 Annual Homelessness Assessment Report to Congress, the state of Washington 
        had an estimated 21,312 people experiencing homelessness on a single night in January 2020. 
        Of those 21,312 people, nearly half (46%) were unsheltered, meaning they were sleeping outside, 
        in a vehicle, or in another place not meant for people to stay comfortably in. Which brings about urgency."),
      h3("Data Source:"),
      p("The data that we collect were from the website", tags$a(href= "https://data.world/sasha/count-us-in-report-homelessness-data", 
                                                                 "Homelessness Data for Seattle/King County 1998-2020"), "by Sasha Anderson.
                                                                  These datasets provide the homeless percentage/individual counts from Washington State by the federal 
                                                                  government from 1998 to 2020 and US demographic characteristics and it's rate by region."),
      
      
      
      
      
      
      h3("Audience:"),
      p("We believe that our target audience is everyday citizens. We want this project to focus on bringing awareness towards the prevalence of homelessness
        and how many individuals struggle with financial stability throughout their lifetime. Further, we want individuals to acknowledge that people who are classified as 
        homeless come from multiple backgrounds and social economicstatus."),
      h3("Questions:"),
      p("1) How has homelessness impacted Seattle overtime?"),
      p("2) What is the percentile of homelessness in different regions (East Coast, West Coat, King County, Seattle)?"),
      p("3) What demographics are being affected by homelessness the most?(veterans, young adults, etc)")),
    mainPanel(imageOutput("imageMap"),
              h3("Creators:"),
              p("1) James Tenorio"),
              p("2) Christian Baldoza"),
              p("3) Wangyin Hong"),
              br(),
              p("The graph above displays homelessness by state, in this case the darker the shade the more prevalent homelessness is."))),
  ), 
  
  tabPanel("Duration of Homelessness Statistics",
            titlePanel("Duration of Homelessness"),
            sidebarLayout(
              sidebarPanel(
                fluidRow(p("The data below displays a sample of the", em("duration"), "of", strong("homelessness"), "percentage and count for each year."),
                         br(),
                         p("The Data Set contains information given to The Department of Health and Human Services by KingCounty WA's, All Home Lead Agency."),
                         uiOutput("checkboxDuration")
                  
                )),
                mainPanel(tableOutput("sampletable"), p("The data table above, displays the span of time in different catagories that homeless individuals live through. 
                                                        It goes to show that homelessness doesn't have a set end or begining. 
                                                        Throughout analyzing the data, we sadly see a stead increase of homelessness by 5% within a three year span.
                                                        The data shows that long term homelessness has increased, but short-term homelessness has decreased overall.")))
),
  tabPanel("Shelter for Homelessness Plot Analysis",
           titlePanel("Types of Shelters for the Homelessness"),
           sidebarLayout(
             sidebarPanel(
               fluidRow( p("The Data below displays the main types of shelters for Homelessness within Washington State "),
                         p("Further, the data is organized yearly census data that refers to the total count of individuals"),
                         column(6,
                                radioButtons("color", "Choose color",
                                             choices = c("skyblue", "pink", "beige","green", 
                                                                  "purple1", "steelblue3", "lightgrey"))
                         ),
                         column(6, 
                                uiOutput("checkboxShelter")
                         )
               )
             ),
             mainPanel(
               plotOutput("plot"),
               textOutput("years"), 
               br(),
               p("The plot above, focuses on answering the question on what shelters are most used by those who struggle with homelessness.
                 It focuses on providing evidence that could be used to improve on previous solutions, and displays which have an impact in reducing 
                 homelessness overall.")
             )
           )),
  tabPanel("Display Table for each Region & Year",
           titlePanel("Homeless Percentage/Count for each Region in Washington State"),
           sidebarLayout(
             sidebarPanel(
               fluidRow(
                 p("The Data below displays the Homelessness percentage within each Region in Washington State."),
                 p("Further, the data is organized by each year for both unsheltered and sheltered indivudals."),
                 column(6,
                        uiOutput("checkboxRegion")))
             ),
             mainPanel(tableOutput("rdata"),textOutput("counts"),
                       br(),
                       p('The table above focuses on answering which areas in Washington State need more help and support than others. So resources
                         are allocated properly to combat such a large global issue on a smaller scale. It also, emphasises that larger cities tend to have larger
                         populations of homelessness due to living price standards and job scarcity.'))
           )),
  tabPanel("Homeless Demographic Table", titlePanel("Homeless Demographic Characteristics in Washington State"),
           sidebarLayout(
             sidebarPanel(
               fluidRow(
                 p("The data displayed below is the total demographics characteristics for homelessness in washington State."),
                 column(6, 
                        uiOutput("checkboxDemo")))
             ),
             mainPanel(tableOutput("chartable"), textOutput("chartext"), 
                 br(),
                 p("The data above, brings significance towards which individuals with specific backgrounds suffer from homelessness more than others. This
                   allows programs to be created specifically to help offer more support, and allows questioning towards the causes and effects of living situations
                   in modern society."))
             )),
  
  tabPanel("Summary", sidebarLayout(
    sidebarPanel(h3("Overall Takeaways:"),
                 p("After analyzing our data, we have to came to the conclusion that throughout Washington State, Seattle
                  has the highest cases of homelessness. Futher, we have realized that the highest cases of homelessness were 
                  from 2018 with an estimate of 37 percent. We believe this is because as rent increased to its highest point in 2020,
                  compared to previous years there was an increase in evictions with many losing their homes due to not being able to pay
                  for their rent consistently."),
                 p("With our final project, we hope to understand the data collected throughout Seattle's homeless population due to
                              multiple ranges of issues from housing to lack of support from family. With our data we strive to study the different
                              contributing factors and its impacts on different regions within Washington State specifically to help bring awareness
                              to such a large issue and to give support to individuals in such situations."),
                 br(),
                 h3("Data Quality:"),
                 p("We believe that our data set was of good quality and was clear to understand. 
                   The dataset itself was organized and specific in its data. 
                   Further, it was easy to navigate and gave unbiased statistical data while taking 
                   into multiple influential factors when it comes to homelessness. However, we do want you to notice that there are a good amount of 
                   na values, which is possibly due individuals having different preferences about classifying their situations."),
                 h3("Future Ideas:"),
                 p("Future Ideas we had in mind was looking towards health professional output on homelessness and their suggestions towards sustainable
                   programs and ways to seek medical attention. In addition, looking towards gathering personal stories of individuals dealing with homessness 
                   currently, to understand how they got into their situation and possibly ways to avoid it.")),
    mainPanel(br(), p('The graph displays the specific statistic number of homessless people in each state in 2020.'), 
                imageOutput("StatImage")
             ),
  ))
)
)

server <- function(input, output) {
 
  # For the introduction
  output$imageMap <- renderImage({
    list(alt= "Homeless population by state", src= "homeless.png",  width = "100%")
  })
  
  ## For the Duration of Homelessness 
  output$checkboxDuration <- renderUI({
    selectInput("Year", "Choose a specific Year",
                choices = unique(clean$Year))
  })
  output$sampletable <- renderTable({
    clean %>%
      filter(Year == (input$Year))
  })
  ## For the plot
  
  cleaned_shelter <- shelter %>% 
    select_if(~!any(is.na(.)))
  
  cleaned_shelter[rowSums(is.na(cleaned_shelter)) == 0, ]
  
  cleaned_shelter %>% 
    filter(!is.na(Count), !is.na(Percent))
 
  
  output$checkboxShelter <- renderUI({
    radioButtons("Shelter", "Shelter Types for homelessness",
                 choices = unique(newShelter$`Shelter type`),
    )
  })
sample2 <- reactive({
  cleaned_shelter %>%
      filter(`Shelter type` %in% input$Shelter)
  
  })
  # Reactive Plot
  output$plot <- renderPlot({
    p <- sample2() %>%
      ggplot(aes(x = factor(Year), y = Count, fill = factor(`Shelter type`))) +
      geom_col() +
      labs(x = "Year", y = "Count", fill = "Shelter type") +
      scale_fill_manual(values = input$color)
    
    if(nrow(sample2()) == 0) {
      p <- p + labs(title = "Please select a type of Shelter")
    } 
    p
  })
  output$years <- renderText({
    years <- sample2() %>% pull(Year) %>% unique()
    if (length(years) == 1) {
      paste("This data is from", years)
    } else {
      paste("The data ranges from the year", min(years), "to the maximum year", max(years))
    }
  })
  
  ## For the table
  output$checkboxRegion <- renderUI({
    radioButtons("Region", "Choose region in Washington State",
                 choices = unique(totalPop$Region),
                 selected = NULL)
    
  })
  # Filtered reactive data table for chosen Region.
  sample <- reactive({
    s1 <- totalPop %>% 
    filter(Region %in% input$Region)
    s1
  })
  output$rdata <- renderTable({
    totalPop %>% 
      filter(Region == (input$Region))
  })
  sample3 <- reactive({
    s3 <- totalPop %>%
      filter(Region == (input$Region))
  })
  output$rdata <- renderTable({
    totalPop %>% 
      filter(Region == (input$Region))
  })
  # Reactive text which shows the max and min years for the graph.
  output$counts <- renderText({
    counts <- sample3() %>% pull(Count) %>% unique()
    if (length(counts) == 1) {
      paste("This data has a count", counts)
    } else {
      paste("The data has a minimum population count of", min(counts), "and a maximum population count", max(counts))
    }
  })
  # Reactive characteristic table.
  output$chartable <- renderTable({
    newChar %>% 
      filter(Characteristic == input$Demo)
  })
  
  output$checkboxDemo <- renderUI({
    selectInput("Demo", "Choose a Characteristic",
                 choices = unique(newChar$Characteristic))
  })
  sample4 <- reactive({
    s4 <- newChar %>% 
      filter(!is.na()) %>% 
      filter(Characteristic %in% input$Demo)
    s4
  })
  # Reactive text that changes based on what is chosen in the table.
  output$chartext <- renderText({
    selectChar <- newChar %>% 
      filter(Characteristic == input$Demo)

    obvsNum <- sum(!is.na(selectChar$Count))
    output_text <- paste("Selected Characteristics table contains", obvsNum, "years of 
    observed data.")
    
  })
  
  ## For summary page.
  output$StatImage <- renderImage({
    list(alt= "Statistics of homeless count per state ", src= "stat.webp",
         width = "100%")
  })

}

shinyApp(ui = ui, server = server)