#<<<<<<< HEAD
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(tidyverse)
library(data.table)

#read in data 
data_i <- read_delim("IHME_Data.csv")
pathogen_data <- data_i %>% select(-sex_id, -sex_name, -metric_id, -upper, -lower, -measure_id, -cause_id, cause_name) %>% filter(!(location_name %in% c(
                                                                                                          "Southeast Asia, East Asia, and Oceania", 
                                                                                                          "East Asia", "Democratic People's Republic of Korea",
                                                                                                          "Southeast Asia", "Lao People's Democratic Republic", "Marshall Islands",
                                                                                                          "Central Europe, Eastern Europe, and Central Asia", 
                                                                                                          "Central Asia", "Central Europe", "Eastern Europe", "Republic of Moldova", 
                                                                                                          "High-income", "High-income Asia Pacific", "Western Europe",
                                                                                                          "Southern Sub-Saharan Africa", "Southern Latin America", 
                                                                                                          "High-income North America", "Latin America and Caribbean")),metric_name == "Number", measure_name == "Deaths")
options_pathogens <- as.list(unique(pathogen_data$pathogen))

# Define UI for application
ui <- fluidPage(
  tabsetPanel(
    #About page 
    tabPanel("About", 
             h1("Pathogen Data App"), 
             h2("Data Summary"), 
             p("The dataset we are working with is one that provides the estimates of deaths and years of life lost due to various bacterial infections, caused by", strong(33),"pathogens across", strong(204), "locations in 2019. The estimates in this dataset were made by using a total of 343 million individual records and 11,361 study location years. These records analyzed data from hospital discharges, cause of death, tissue sampling, literature reviews, microbiology lab results from hospitals nationally as well as muti-national surveillance systems."),
             p("\n The data was collected by researchers at the", em("Institute for Health Metrics and Evaluation (IHME)")," as well as University of Oxford. It is being accessed through the GHDx: Global Health Data Exchange which is a catalog of vital statistics and health related data, available to the public."),
             h2("\n Purpose"),
             img(src = "pathogen.png",height = "50%", width = "50%"),
             p("As shown in the image above, viral pathogens impact areas such as vaccine development, agriculture,immune responses,theraputics, and epidemiology.The purpose of this app is to create a platform where users can easily go through consolidated forms of data related to fatal pathogens in order to answer questions pertinent to healthcare and research around the world. The questions we are focused on relate to, fatal pathogens by age group and location, as well as common symptoms caused by pathogens in terms of helping with pathogen identification, and common symptoms caused by pathogens by age group. We hope that by answering these questions with the data we have, users will be able to better understand how they can actively work to reduce the extremity of these pathogens. This app will allow users to easily pick and choose what they would like to include in their analysis and allows them to visualize the findings. Our target audience consists mainly of health professionals who may be interested in understanding the underlying causes of deaths occurring globally and what measures they can take/improve upon to prevent these infections from spreading. This group includes clinicians, epidemiologists, and even public health organizations such as the World Health Organization (WHO) or the National Association of Country and City Health Officials (NACCHO)."),
             h2("\n Figure"),
             p("The filtered dataset contains a total of 332,258 observations and 11 different variables regarding the different pathogens, age groups, infections/symptoms, and their associated death estimates."),
             p("\n Below is a small random sample of the pathogen data set where val is equal to the # of estimated deaths."), 
             tableOutput("sample"), 
    ), 
    #Pathogen and Death Rate by Age page
    tabPanel("Pathogen and Associated Number of Deaths by Age",
             fluidRow(
               column(
                 width = 4,
                 p("Select a specific age group to display a table that provides the type of pathogen and the number of deaths it causes by age. This can help answer questions such as, which age groups are more prone to a given set of pathogens"),
                 selectInput(inputId = "age_group", 
                             label = "Select age group", 
                             choices = c(unique(pathogen_data$age_group_name)), selected = "All Ages"),
                 textOutput(outputId = "death_range")
               ),
               column(
                 width = 8,
                 tableOutput(outputId = "pathogen_table")
               )
             ),
    ), 

    #Pathogen and Death rate based on Location page
    tabPanel("Pathogen and Associated Number of Deaths by Location ",
             fluidRow(
               column(
                 width = 4,
                 p("Select a location, various pathogens, and the type of plot to visually 
                   see how many deaths each pathogen causes based on location. This can help sort data based on location so that users only see the country relevant to their point of interest."),
                 selectInput(inputId = "location", 
                             label = "Select location", 
                             choices = unique(pathogen_data$location_name)),
                 checkboxGroupInput(inputId = "pathogen_checkbox",
                                    label = "Select pathogens to display:",
                                    choices = unique(pathogen_data$pathogen),
                                    selected = unique(pathogen_data$pathogen))
               ),
               column(
                 width = 8,
                 radioButtons(inputId = "plot_type",
                              label = "Select plot type:",
                              choices = c("Bar Graph", "Scatterplot"),
                              selected = "Bar Graph"),
                 plotOutput(outputId = "pathogen_plot", width = "800px"),
                 fluidRow(
                   column(
                     width = 12,
                     textOutput(outputId = "pathogen_max_deaths")
                   )
                 )
               )
             )
    ),
    #Infection and Age group page
    tabPanel("Bacterial Infections and Age Groups",
             sidebarLayout(
               sidebarPanel(
                 p("Use the widgets below to select different subsets of data based on certain age groups, or certain combinations of infections/symptoms. This plot helps identify which age groups are prone to given bacterial infections and can help backround information to handle diagnosis and appropriate treatment development."),
                 radioButtons("visual", "Plot type", c("Bar Graph", "Dot plot")),
                 uiOutput("age"),
                 uiOutput("symptoms")), 
               mainPanel(
                 textOutput("react_plot"),
                 plotOutput("plot")),
             ),  
    ),
    #Pathogen and Bacterial Infections page
    tabPanel("Pathogens and Bacterial Infections", 
             sidebarLayout( 
               sidebarPanel( 
                 p("Select various combinations of pathogens and see how many symptoms are associated with each pathogen in that set. The x-axis shows the number of observations for each pathogen and the text output changes to show exactly which bacterial infections are associated with the selected pathogens. This data can help answer questions related to pathogen identification based on patient symptoms."),
                 checkboxGroupInput("pathogenss", "Select Pathogen", choices = options_pathogens, selected = options_pathogens),
               ),
               mainPanel ( 
                 plotOutput("bars"),
                 textOutput(("react_pathogen"))
               )
             ),
    ),
    #Conclusion page
    tabPanel("Conclusion and Takeaways",
             h2("Description of a Notable Insight/Pattern Discovered in Project"),
              p("Overall, an insight/pattern discovered from this project and dataset is that these bacterial infections and pathogens are highly prevalent across all age groups and locations globally. While there are certain bacterial syndromes and pathogens that cause the most deaths in certain age groups and locations, it is mainly consistent. This illustrates that these infections are caused by similar issues in these countries and the health community can take measures against the pathogens and bacterial infections to combat the large number of deaths occurring. Examples of such measures include vaccine development for the pathogens, or medicine development for the infections caused by the pathogens."), 
             
             h2("\n Specific Data to Demonstrate Insight"),
              p("From the estimated infectious-related deaths in 2019, there were approximately 7.7 million deaths associated with the 33 bacterial pathogens from the dataset. Below, are the questions we were aiming to specifically address in this project (for our target audience):"), 

             h3(" \n Which pathogens are responsible for the largest number of deaths by location?"),
              p("The top five pathogens that were responsible for the largest number of deaths by location were Staphylococcus auereus, Escherichia coli, Streptococcus pneumoniae, Klebsiella pneumoniae, and Pseudomonas aeruginosa. These five pathogens contributed to approximately 54.9% of deaths globally."), 
             h3("\n Which pathogens are responsible for the largest number of deaths by age?"), 
              p("Across all age groups, the maximum number of deaths is approximately 12535446.23, with the leading pathogens being the same five that are listed above (Staphylococcus auereus, Escherichia coli, Streptococcus pneumoniae, Klebsiella pneumoniae, and Pseudomonas aeruginosa). More specifically, among children younger than 5 years, S pneumoniae caused the most deaths."), 
             h3("\n Are pathogens easily identifiable by their symptoms?"),
              p("Unfortunatly, these 33 pathogens are difficult to diagnose by the symptoms they cause because they often result in multiple symptoms. There are only 9 pathogens (Aeromonas spp, Camplylobacter spp, Clostridioides difficile,Legionella spp, Listeria monocytogenes,Salmonella Paratyphi, Shigella spp, Vibrio cholerae, Neisseria gonorrhoeae), that have only 2 symptoms/bacterial infections each. All the other pathogens have multiple combinations of bacterial infections possible making it difficult to identify a pathogen based off of the infections it causes,alone."),
             h3("\n Do bacterial infections/symptoms differ by age group?"),
              p("Almost all bacterial infections were present across all age groups and in mostly equal amounts except for gonorrhea and chlamydia which were only present in ages 5+. Additionally, this suggests that symptoms do not vary based on age for the 33 pathogens, however, certain symptoms such as bloodstream infections are more common than others (ex: typhoid fever)."),
             
             h2("\n Broader Implications of Insights"),
             p("The insights and dataset demonstrate that there is a significant amount of health loss globally due to these various infectious syndromes and pathogens. The deaths caused by these bacteria were the second leading cause of global deaths in 2019, and therefore, infectious disease must be prioritized. Health organizations such as World Health Organization (WHO) or the National Association of Country and City Health Officials (NACCHO) can take action to address and prevent the spread of such infections across so many locations. Moreover, health professionals and epidemiologists can work towards studying this data to develop antibiotics, vaccines, and conduct analysis on how to combat the spread of these pathogens. These developments will aid in reducing deaths in various locations and age groups across the world."), 

            h2("\n Data Quality"),
             p("The data quality of the dataset is reliable as the estimates were made by using a total of 343 million individual records and 11,361 study location years. Additionally, the records were analyzed data from hospital discharges, cause of death, tissue sampling, literature reviews, microbiology lab results from hospitals nationally as well as muti-national surveillance systems. The data was collected by researchers at the Institute for Health Metrics and Evaluation (IHME) as well as University of Oxford. Therefore, taking these factors into account, there is substantial credibility behind the estimates provided and it does its best to be as unbiased as possible. 
             However, there is an ethical issue present as this data is accessed through the GHDx: Global Health Data Exchange which is a catalog of vital statistics and health related data, fully available to the public, which can be harmful to the populations that the data was taken from. There is a lack of confidentiality for these populations, and there may be intentional misuse of the data that can affect the locations and age groups from the data. Additionally, it is unclear whether or not certain pathogens have a large impact on smaller nations and certain age groups or if there is simply less data collected from those nations. Therefore, these populations may be harmed because they will not be well represented in the steps our target audience will take based on the results from this data set."),
             
             h2("\n How to Advance the Project"), 
             p("To advance the project, the project could display what pathogen they are in most danger of depending on their location, symptoms, and age group by including widgets in which they can input this information, which the app will thus compute. Additionally, after providing the pathogen they are in most danger of, the project could provide important measures to take to stay safe and continue prevention of the spread of these infections so that the user can stay informed through an accessible application."),
    ), 
  )
)
  
# Run the application 
#shinyApp(ui = ui, server = server)
