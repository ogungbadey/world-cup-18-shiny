library(shiny)
library(glue)
all.shots = NULL
all.passes = NULL
# competition.stages = NULL
all.fixtures = NULL

world.cup.match_events = readRDS("world_cup_match_events.RData")
for(j in 1:length(world.cup.match_events)){
  match.event = world.cup.match_events[j][[1]]
    
  shots.temp = as.data.frame(match.event[[4]])
  passes.temp = as.data.frame(match.event[[3]])
  shots.temp$shot.match.id = match.event[[1]]["match.id"]
  shots.temp$xg_dbl = shots.temp$shot.statsbomb_xg * 5 
  
  passes.temp$pass.match.id = match.event[[1]]["match.id"] 
  
  all.shots = rbind(all.shots,shots.temp)
  all.passes = rbind(all.passes,passes.temp)
  fixture = as.data.frame(match.event[[1]])
  fixture = fixture %>% mutate(match_title = glue("{home.team} - {away.team}"))
  all.fixtures = rbind(all.fixtures, fixture)
  # competition.stages = rbind(competition.stages,stage.temp)
}
all.shots = all.shots %>% mutate_at(c(5:6,14:15), as.numeric)

all.stages = unique(all.passes$competition.stage)
all.teams = unique(all.passes$team.name)
all.players = unique(all.passes$player.name)
campo.x <- seq(from=0,to=120,by=1)
campo.y <- seq(from=0,to=80,by=1)
campo.coordinates <- expand.grid(campo.x,campo.y)
colnames(campo.coordinates) <- c("X","Y")
# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Shot Map!"),
  
  helpText("Shot map based on world cup games"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
      column(4, wellPanel(
        selectInput(
          "stage",
          label = "Select competition stage",
          choices = all.stages
          ),
        selectInput(
          "match",
          label = "Select match",
          choices = all.fixtures$match_title,
        ),
        # selectInput(
        #   "teams",
        #   label = "Select Team",
        #   choices = all.teams,
        #   selected = all.teams[1]
        # ),
        selectInput(
          "players1",
          label = "Select Player",
          choices = all.players,
        ),
        selectInput(
          "players2",
          label = "Select Player",
          choices = all.players,
          selected = NULL
        ),
        sliderInput(
          "xG",
          label = "Filter shots based on xG",
          min= round(min(all.shots$shot.statsbomb_xg, na.rm = TRUE),1),
          max= round(max(all.shots$shot.statsbomb_xg, na.rm = TRUE),1),
          value=c(0,0.9)
        ),
        plotOutput("assistmap", brush = "assist_brush")
      )),
      # column(5, wellPanel(
      #   
      # )),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "shotPlot"),
      
    verbatimTextOutput("info")
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output,clientData,session) {
  values = reactiveValues(trigger=0,x=NA, y=NA)
  observeEvent(input$stage, {
    stageInput = input$stage
    temp.match = all.fixtures %>% filter(stage == stageInput) 
    match.options = unique(temp.match$match_title)
    updateSelectInput(session, "match", choices = match.options)
  })
  
  observeEvent(input$match, {
    teams = input$match
    teams.split = strsplit(teams, " - ")
    # browser()
    teamA = teams.split[[1]][1]
    teamB = teams.split[[1]][2]
    match.id = all.fixtures %>% 
      filter(match_title == teams) %>% 
      pull(match.id)
    teamA.players = all.shots %>% 
      filter(shot.match.id == match.id & team.name == teamA) %>% 
      pull(player.name) %>% unique()
    teamB.players = all.shots %>% 
      filter(shot.match.id == match.id & team.name == teamB) %>% 
      pull(player.name) %>% unique()
    updateSelectInput(session, "players1", choices = append("All",teamA.players))
    updateSelectInput(session, "players2", choices = append("All", teamB.players))
  })
  
  observe({
    if(!is.null(input$assist_brush)){
      
    } else {
      selectFixture = all.fixtures %>% filter(match_title == input$match)
      
      values$shots.selected = all.shots %>% 
        filter(shot.match.id == selectFixture$match.id)
    }
  })
  
  observe({
    req(input$plot_click)
    isolate(values$trigger <- values$trigger + 1)
    values$x <- input$plot_click$x
    values$y <- input$plot_click$y
  })
  output$assistmap <- renderPlot({hori5})
  
  output$shotPlot = renderPlot({
    
    shots.data <- values$shots.selected
    teams = input$match
    teams.split = strsplit(teams, " - ")
    # browser()
    teamA = teams.split[[1]][1]
    teamB = teams.split[[1]][2]
    
    
    
    plotB = shots.data %>% filter(team.name == teamB) 
    plotA = shots.data %>% filter(team.name == teamA) %>%
      mutate(X.Shot = 120 - X.Shot,Y.Shot=80-Y.Shot) 
    if(input$players1 != "All") {
      plotA = plotA %>% filter(player.name == input$players1)
    }
    
    if(input$players2 != "All") {
      plotB = plotB %>% filter(player.name == input$players2)
    }
    
    if(values$trigger %% 2 == 0) {
      
      hori5 + geom_point(data = plotB , aes(x=X.Shot,y=Y.Shot,
                                            size=xg_dbl),
                         color = "yellow")+
        geom_point(data=plotA, aes(x=X.Shot,y=Y.Shot, size=xg_dbl), color="blue") + 
        geom_point(data = plotA %>% 
                     filter(shot.outcome.name == "Goal"), 
                   aes(x=X.Shot, y=Y.Shot, size=xg_dbl/2), color="green") + 
        geom_point(data = plotB %>% 
                     filter(shot.outcome.name == "Goal"), 
                   aes(x=X.Shot, y=Y.Shot, size=xg_dbl/2), color="green") + 
        geom_label(aes(x=15, y=10, label=teamA)) +
        geom_text(aes(x=15, y=5, label=glue("shots: {nrow(plotA)}"))) + 
        geom_text(aes(x=15, y=0, label=glue("xG: {round(sum(plotA$shot.statsbomb_xg),2)}"))) + 
        geom_label(aes(x=105, y=10, label=teamB)) + 
        geom_text(aes(x=105, y=5, label=glue("shots: {nrow(plotB)}"))) + 
        geom_text(aes(x=105, y=0, label=glue("xG: {round(sum(plotB$shot.statsbomb_xg),2)}"))) 
    } else {
      # browser()
      shot.info = shots.data %>% filter(X.Shot == round(values$x,0), Y.Shot == round(values$y, 0))
      shot.freeze_frame=NULL
      if(!is.null(shot.info)) {
        shot.freeze_frame = as.data.frame(shot.info$shot.freeze_frame)
        
        shot.freeze_frame = shot.freeze_frame %>% 
          mutate(location = sapply(location, toString)) %>%
          separate(location, into=c("player.x", "player.y"), sep="\\,") %>% 
          mutate_at(c(1:2), as.numeric)
          
        
        hori5 + 
          geom_point(data = shot.info, aes(x=X.Shot, y=Y.Shot)) +
        geom_point(data=shot.freeze_frame, aes(x=player.x, y=player.y))
        }
      
    }
   
    })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
#This small Shiny application demonstrates Shiny's automatic UI updates.

