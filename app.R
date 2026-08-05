#packages
library(DT)
library(shinydashboard)
library(shinycssloaders)
library(tidyverse)
library(rio)
library(reactable)
library(shiny)
library(rvest)
library(htmltools)
library(vtable)
library(stringi)
library(reactablefmtr)
library(rsconnect)
library(readxl)


#load données et fonctions
playoffs<- read_excel("data/Playoffs.xlsx")
nf <- function(x) if (is.na(x)) " " else x



ui<- shinyUI(fluidPage(
  tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
  
  # load custom stylesheet
  includeCSS("style.css"),
  
  # remove shiny "red" warning messages on GUI
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  # load page layout
  dashboardPage(
    
    skin = "red",
    
    dashboardHeader(title="Revivre les Playoffs", titleWidth = 300),
    
    dashboardSidebar(width = 300,
                     sidebarMenu(
                       HTML(paste0(
                         "<br>",
                         "<img style = 'display: block; margin-left: auto; margin-right: auto;' src='C:/Users/theom/OneDrive/Bureau/revivrelesplayoffs/bioNPS-master/revivrelesplayoffs/nbaplayoffs.jpg' width = '250'></a>",
                         "<br>"
                       )),
                       menuItem("Bienvenue", tabName = "home", icon = icon("home")),
                       menuItem("Actualités", tabName = "charts", icon = icon("table")),
                       menuItem("Playoffs", tabName = "table", icon = icon("chart-bar")),
                       menuItem("À propos", tabName = "choropleth", icon = icon("circle-info"))
                     )
                     
    ), # end dashboardSidebar
    
    dashboardBody(
      
      tabItems(
        
        tabItem(tabName = "home",
                includeMarkdown("home.md")
        ),
        
        tabItem(tabName = "charts",
                includeMarkdown("charts.md")
        ),
        
        tabItem(tabName = "table", 
                includeMarkdown("playoffs.md"),
                fluidPage(
                  titlePanel("Séries de Playoffs"),
                  
                  fluidRow(
                    column(4,
                           selectInput("Année",
                                       "Année:",
                                       c("All",
                                         unique(as.character(playoffs$Année))))
                    ),
                    column(4,
                           selectInput("Round",
                                       "Round:",
                                       c("All",
                                         unique(as.character(playoffs$Round))))
                    )
                  ),
                  # Create a new row for the table.
                  reactableOutput("table")
                )
                
        ),
        
        
        tabItem(tabName = "charts",
                includeMarkdown("charts.md")
        ), 
        
        tabItem(tabName = "choropleth",
                fluidRow(
                  # About - About Me - start ------------------------------------------------
                  box(
                    title = "À propos de moi",
                    width = "6 col-lg-6",
                    tags$p(
                      class = "text-center",
                      tags$strong("Salut moi c'est Théo"),
                      HTML(paste0("(", tags$a(href = "https://x.com/theomdeig", "@theomdeig", target="_blank"), ")"))
                    ),
                    tags$p(
                      "J'ai 24 ans, je suis le basket depuis 2022 environ, mon joueur préféré est Jamal Crawford."),
                    tags$p("
                      J'ai voulu coder ce site afin de rendre plus facile le visionnage de séries de playoffs passées.
                      Il s'agit de mon 1er site, j'espère que la navigation sera agréable."),
                    tags$p("
                      Je voulais à l'origine avoir la possibilité de cacher les résultats des séries pour vivre les séries et 
                      découvrir les vainqueurs au fur et à mesure. Malheureusement étant donné le nombre de matchs manquants, 
                      je préfère me concentrer sur l'archivage et la recherche."),
                    tags$p(
                      "En cas de problème, question ou remarque vous pouvez m'envoyer un message sur",
                      HTML(paste0(tags$a(href = "https://x.com/theomdeig", "Twitter", target = "_blank"))),
                      ", j'essaierai d'être réactif et d'y poster les nouvelles séries ajoutées."
                    )
                  ),
                  # About - About Me - end --------------------------------------------------
                  # About - About Dashboard - start -----------------------------------------
                  box(
                    title = "À propos du site",
                    # status = "primary",
                    width = "6 col-lg-6",
                    tags$p(
                      "Ce site a été crée avec les logiciels",
                      tags$a(href = "https://r-project.org", target = "_blank", "R"),
                      "et", tags$a(href = "https://rstudio.com", target = "_blank", "RStudio"),", et est hébergé sur Posit Cloud Connect."
                    ),
                    tags$p(
                      "Aucune intelligence artificielle n'a été utilisée pour générer le code ou les images du site."),
                    tags$p(
                      "Deux modèles de sites développés en Shiny m'ont beaucoup servi : "),
                    tags$p(
                      "- Le Dashboard de Garrick Aden-Buie",
                      HTML(paste0(tags$a(href = "https://garrickadenbuie.com", "(garrickadenbuie.com)", target = "_blank"), ",")),
                      "dont le code est accesible sur",
                      tags$a(href = "https://github.com/gadenbuie/tweet-conf-dash", target = "_blank", "Github")),
                    tags$p(
                      "- L'application d'Alessio Benedetti", "dont le code est accessible sur",
                      tags$a(href = "https://github.com/abenedetti/bioNPS/", target = "_blank", "Github"),
                    )
                    
                  )
                  
                )
                
        ) # end dashboardBody
        
      )# end dashboardPage
      
    ))))


server<-shinyServer(function(input, output) {
  
  output$table <- renderReactable(reactable({
    data <- playoffs
    data[is.na(data)] <- " "
    if (input$Année != "All") {
      data <- data[data$Année == input$Année,]
    }
    if (input$Round != "All") {
      data <- data[data$Round == input$Round,]
    }
    data
  }, theme = reactableTheme(headerStyle = list(display = "none"), cellPadding = "4px 4px"),
  columns = list(Année = colDef(width = 60, style=list(fontWeight = "bold")),
                 équipes = colDef(width = 150, style=list(fontWeight = "bold")),
                 Game1 = colDef(html = TRUE, cell = function(value, index) {
                   stri_sprintf(ifelse(value == "Manquant", "Manquant", ifelse(value == " ", " ",'<a href="%s" target="_blank">Game 1</a>')), data$Game1[index], na_string = NA_character_)
                 }),Game2 = colDef(html = TRUE, cell = function(value, index) {
                   stri_sprintf(ifelse(value == "Manquant", "Manquant", ifelse(value == " ", " ",'<a href="%s" target="_blank">Game 2</a>')), data$Game2[index], na_string = NA_character_)
                 }),Game3 = colDef(html = TRUE, cell = function(value, index) {
                   stri_sprintf(ifelse(value == "Manquant", "Manquant", ifelse(value == " ", " ",'<a href="%s" target="_blank">Game 3</a>')), data$Game3[index], na_string = NA_character_)
                 }),Game4 = colDef(html = TRUE, cell = function(value, index) {
                   stri_sprintf(ifelse(value == "Manquant", "Manquant", ifelse(value == " ", " ",'<a href="%s" target="_blank">Game 4</a>')), data$Game4[index], na_string = NA_character_)
                 }),Game5 = colDef(html = TRUE, cell = function(value, index) {
                   stri_sprintf(ifelse(value == "Manquant", "Manquant", ifelse(value == " ", " ",'<a href="%s" target="_blank">Game 5</a>')), data$Game5[index], na_string = NA_character_)
                 }),Game6 = colDef(html = TRUE, cell = function(value, index) {
                   stri_sprintf(ifelse(value == "Manquant", "Manquant", ifelse(value == " ", " ",'<a href="%s" target="_blank">Game 6</a>')), data$Game6[index], na_string = NA_character_)
                 }),Game7 = colDef(html = TRUE, cell = function(value, index) {
                   stri_sprintf(ifelse(value == "Manquant", "Manquant", ifelse(value == " ", " ",'<a href="%s" target="_blank">Game 7</a>')), data$Game7[index], na_string = NA_character_)
                 }))
  , defaultPageSize = 50, compact = TRUE, striped = TRUE, searchable = TRUE,
  rowStyle = function(value) {
    if (data[value, "équipes"] %in% c("Lakers - Sixers", "Sixers - Celtics") & data[value, "Année"] == "1980" |
        data[value, "équipes"] %in% c("Pistons - Bulls", "Pistons - Lakers", "Knicks - Sixers", "Bulls - Knicks", "Bulls - Cavs") & data[value, "Année"] == "1989" |
        data[value, "équipes"] %in% c("Spurs - Pistons", "Spurs - Suns", "Suns - Grizzlies", "Sonics - Kings", "Spurs - Sonics") & data[value, "Année"] == "2005") list(background = "#AACDAE")
  }
  ))
  
})

# Create the Shiny app
shinyApp(ui = ui, server = server)




rsconnect::writeManifest()

