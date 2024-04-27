
library(dplyr)
data_i <- read_delim("IHME_Data.csv")
pathogen_data <- data_i %>% select(-sex_id, -sex_name, -metric_id, -upper, -lower, -measure_id, -cause_id, cause_name) %>% filter(!(location_name %in% c(
                                                                                                       "Southeast Asia, East Asia, and Oceania", 
                                                                                                       "East Asia", "Democratic People's Republic of Korea",
                                                                                                       "Southeast Asia", "Lao People's Democratic Republic", "Marshall Islands",
                                                                                                       "Central Europe, Eastern Europe, and Central Asia", 
                                                                                                       "Central Asia", "Central Europe", "Eastern Europe", "Republic of Moldova", 
                                                                                                       "High-income", "High-income Asia Pacific", "Western Europe",
                                                                                                       "Southern Sub-Saharan Africa", "Southern Latin America", 
                                                                                                       "High-income North America", "Latin America and Caribbean")), metric_name == "Number",measure_name == "Deaths")
options_pathogens <- as.list(unique(pathogen_data$pathogen))

# Define server logic 
server <- function(input, output) {
  #sample data table on about page output
  output$sample <- renderTable({
    sample <- pathogen_data %>% sample_n(5)
    sample
  })
#Symptoms and age group page 
  #Options vectors 
  options_age <- as.list(unique(pathogen_data$age_group_name))
  options_symptoms <- as.list(unique(pathogen_data$infectious_syndrome))

  #Age options widget 
  output$age <- renderUI({
    selectInput("age", label = "Select Age Group", choices = options_age, selected = "All Ages")
  })
  #Symptoms options widget 
  output$symptoms <- renderUI({
    checkboxGroupInput("symptoms", label = "Select Symptoms", choices = options_symptoms, selected = options_symptoms) 
  })

  #visual options widget 
  output$visual <- renderUI({
    radioButtons("Plot type", label = "Plot type", choices = c("Bar Graph", "Dot Plot")) 
  })
  
  # Plot output 
  output$plot <- renderPlot({
    #Data filter by age selected 
    if(input$age == "All Ages"){
      df <- pathogen_data %>% select(age_group_name,infectious_syndrome,pathogen) %>% filter(infectious_syndrome %in% input$symptoms) %>% group_by(age_group_name, infectious_syndrome) %>%  summarize(num = n()) 
    }
    else{
      df <- pathogen_data %>% select(age_group_name,infectious_syndrome,pathogen) %>% filter(age_group_name %in% input$age, infectious_syndrome %in% input$symptoms) %>% group_by(age_group_name,infectious_syndrome) %>%  summarize(num = n())
    }
    
    #reactive text output for symptoms vs age page
    output$react_plot <- renderText({
      n_sum <- sum(df$num)
      paste("There are",toString(n_sum),"total observations in the subset you have selected")
    })
    
    #ggplot for graph type choices  
    if(input$visual == "Bar Graph"){
      ggplot(df, aes(num, color = infectious_syndrome))+ 
        geom_bar(position = "Dodge", fill = "white")+
        ggtitle("Symptoms Instances by Age Group")+
        labs(x = "Number of Observations", y = "# Age groups", color = "Symptoms")
    } 
    else{
      ggplot(df, aes(age_group_name,num, color = infectious_syndrome))+ 
        geom_count()+
        ggtitle("Symptoms Instances by Age Group")+
        labs(x = "Age Group", y = "# of Observations", color = "Symptoms")
    }
  })

  
#Pathogen vs Symptoms page 
  output$bars <- renderPlot ({ 
    
    pathogen_data %>%  
      select(pathogen, infectious_syndrome) %>% 
      filter(pathogen %in% input$pathogenss) %>% 
      group_by(pathogen) %>% 
      summarize(num = n(),uniq = n_distinct(infectious_syndrome)) %>% 
      ggplot(aes(num,uniq, color = pathogen)) + 
      geom_col(position = "dodge", fill = "white")+
      ggtitle("Pathogens and Associated Bacterial Infections")+
      labs(x = "Number of Observations", y = "#Infections associated with pathogen", color = "pathogen")
    
    
  })
  #Reactive text for pathogen vs symptoms  
  output$react_pathogen <- renderText({
    path <- pathogen_data %>%  
      select(pathogen, infectious_syndrome) %>% 
      filter(pathogen %in% input$pathogenss)
    unique_symp <- unique(path$infectious_syndrome)
    result <- toString(unique_symp)
    paste("The symptoms associated with the pathogens selected are:", result)
  })

#Pathogen and deaths based on location page  
  # Filter the data based on user inputs
  filtered_data <- reactive({
    pathogen_data %>%
      filter(location_name == input$location,
             pathogen %in% input$pathogen_checkbox,
             age_group_name == input$age_group)
  })
  
  # Render the bargraph and scatterplot
  output$pathogen_plot <- renderPlot({
    if(input$location == "Global"){
    data <- pathogen_data %>%
      group_by(pathogen) %>%
      summarize(deaths = sum(val)) %>%
      arrange(desc(deaths))
    }
    else{
      data <- pathogen_data %>%
        filter(pathogen %in% input$pathogen_checkbox) %>%
        group_by(pathogen) %>%
        summarize(deaths = sum(val)) %>%
        arrange(desc(deaths))
    }
    
    if (input$plot_type == "Bar Graph") {
      ggplot(data, aes(x = pathogen, y = deaths, fill = pathogen)) +
        geom_col() +
        ggtitle(paste("Pathogens Responsible for Deaths in", input$location)) +
        xlab("Pathogen") +
        ylab("Number of Deaths") +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      ggplot(data, aes(x = pathogen, y = deaths, color = pathogen)) +
        geom_point() +
        ggtitle(paste("Pathogens Responsible for Deaths in", input$location)) +
        xlab("Pathogen") +
        ylab("Number of Deaths") +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  # Render the max deaths text
  output$pathogen_max_deaths <- renderText({
    data <- pathogen_data %>%
      filter(location_name == input$location,
             pathogen %in% input$pathogen_checkbox) %>%
      group_by(pathogen) %>%
      summarize(deaths = sum(val)) %>%
      arrange(desc(deaths))
    
    max_deaths <- max(data$deaths)
    max_pathogen <- data %>%
      filter(deaths == max_deaths) %>%
      pull(pathogen)
    
    paste("The pathogen that causes the most deaths is", max_pathogen, "with", 
          round(max_deaths, 2), "deaths.")
  })
#Pathogen and deaths based on age group page
  # Render the table on table page
  output$pathogen_table <- renderTable({
    data <- pathogen_data %>%
      filter(age_group_name == input$age_group) %>%
      group_by(pathogen) %>%
      summarize(deaths = sum(val)) %>%
      arrange(desc(deaths))
    
  })
  
  # Render the death range text
  output$death_range <- renderText({
    data <- pathogen_data %>%
      group_by(pathogen) %>%
      summarize(deaths = sum(val)) %>%
      arrange(desc(deaths))
    
    min_deaths <- round(min(data$deaths), 2)
    max_deaths <- round(max(data$deaths), 2)
    
    if (input$age_group == "All Ages") {
      paste("The minimum deaths caused by the 33 pathogens across all age groups is", 
            round(min_deaths, 2), "and the maximum is", round(max_deaths, 2), ".")
    } else {
      age_data <- filtered_data() %>%
        group_by(pathogen, age_group_name) %>%
        summarize(deaths = sum(val)) %>%
        arrange(desc(deaths))
      
      min_age_deaths <- round(min(age_data$deaths), 2)
      max_age_deaths <- round(max(age_data$deaths), 2)
      
      paste("The minimum deaths caused by the 33 pathogens within the selected age group is", 
            round(min_age_deaths, 2), "and the maximum is", round(max_age_deaths, 2),".")
    }
  })
}

# Run the application 
#shinyApp(ui = ui, server = server)
