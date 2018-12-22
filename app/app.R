options(warn = -1)

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(tools)
library(reshape2)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(r2d3)

source("algorithm.R")

# \
#  > Preloadinng data
# /

df <- read.csv("data/whisky.csv")
df.pca <- read.csv("data/pca.csv")
df.pca.rotation <- read.csv("data/pca_rotation.csv")
df.cluster <- read.csv("data/cluster.csv")
df.distillery <- read.csv("data/distillery.csv")
df.flavor <- read.csv("data/flavor_vocabulary.csv")
df.color <- read.csv("data/color.csv", check.names = F, as.is = T)
df.exp.profile <- read.csv("data/exp_profile.csv")

row.names(df) <- df$id
df$type <- as.character(df$type)
df$name <- as.character(df$name)
df$distillery <- as.character(df$distillery)
df$origin <- as.character(df$origin)
df$cost <- as.character(df$cost)
df$rating <- as.numeric(df$rating)
df$abv <- as.numeric(df$abv)

choice.mood <- c(Classic = 1, Mild = 2, Adventurous = 3)
choice.exp <- c(Novice = 1, Appcretiator = 2, Connoisseur = 3)
choice.flavor <- sort.by.name(setNames(1:14, toTitleCase(names(df[, seq(9, 22)]))))
choice.fav <- sort.by.name(setNames(df$id, df$name))
choice.type <- sort(unique(df$type))
choice.distillery <- sort(unique(df$distillery))
choice.origin <- sort(unique(df$origin))
choice.cost <- sort(unique(df$cost))

# \
#  > UI
# /

ui <- dashboardPage(
  # header
  dashboardHeader(titleWidth = 0),
  # sidebar
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      id = "tabs",
      menuItem("Recommender", tabName = "recommender", icon = icon("glass"), selected = T),
      menuItem("Distillery Map", tabName = "distillery", icon = icon("map-signs")),
      menuItem("Insight", tabName = "insight", icon = icon("binoculars")),
      menuItem("Data", tabName = "data", icon = icon("cube")),
      menuItem("Ideas Behind", tabName = "ideas", icon = icon("coffee")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  # body
  dashboardBody(
    # custom title
    tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<div id="title"> Whisky Recommender </div>\');
      })
     ')),
    # page style
    includeCSS("css/style.css"),
    # initialize library
    shinyjs::useShinyjs(),
    shinyWidgets::useSweetAlert(),
    # pages
    tabItems(
      # recommender page
      tabItem(
        tabName = "recommender",
        fluidRow(
          tabBox(
            width = 12,
            tabPanel(
              "Input",
              fluidRow(
                box(
                  width = 4, height = 330, solidHeader = T,
                  selectizeInput(
                    "mood", "Mood",
                    choices = choice.mood,
                    options = list(
                      placeholder = "How do you feel today",
                      onInitialize = I('function() { this.setValue(""); }')
                    ),
                    selected = "2",
                    multiple = F
                  ),
                  selectizeInput(
                    "exp", "Experience Level",
                    choices = choice.exp,
                    options = list(
                      placeholder = "Your experience with whisky",
                      onInitialize = I('function() { this.setValue(""); }')
                    ),
                    selected = "2",
                    multiple = F
                  ),
                  selectizeInput(
                    "flavor", "Preferred Flavor",
                    choices = choice.flavor,
                    options = list(
                      placeholder = "The flavors you want to taste",
                      onInitialize = I('function() { this.setValue(""); }')
                    ),
                    multiple = T
                  ),
                  selectizeInput(
                    "fav", "Favorite Whisky",
                    choices = choice.fav,
                    options = list(
                      placeholder = "Choose some beloved ones",
                      onInitialize = I('function() { this.setValue(""); }')
                    ),
                    multiple = T
                  )
                ),
                box(
                  width = 4, height = 330, solidHeader = T,
                  selectizeInput(
                    "type", "Spirit Type",
                    choices = choice.type,
                    options = list(
                      placeholder = "What types are you looking for",
                      onInitialize = I('function() { this.setValue(""); }')
                    ),
                    multiple = T
                  ),
                  selectizeInput(
                    "distillery", "Distillery",
                    choices = choice.distillery,
                    options = list(
                      placeholder = "Where is it produced",
                      onInitialize = I('function() { this.setValue(""); }')
                    ),
                    multiple = T
                  ),
                  selectizeInput(
                    "origin", "Origin",
                    choices = choice.origin,
                    options = list(
                      placeholder = "The region it comes from",
                      onInitialize = I('function() { this.setValue(""); }')
                    ),
                    multiple = T
                  ),
                  selectizeInput(
                    "cost", "Cost",
                    choices = choice.cost,
                    options = list(
                      placeholder = "How much do you want to spend",
                      onInitialize = I('function() { this.setValue(""); }')
                    ),
                    multiple = T
                  )
                ),
                box(
                  width = 4, height = 330, solidHeader = T,
                  div(
                    style = "margin-bottom: -20px;",
                    sliderInput("rating", "Rating",
                      min = 0, max = 100,
                      value = c(0, 100)
                    )
                  ),
                  div(
                    style = "margin-bottom: -20px;",
                    sliderInput("abv", "ABV",
                      min = 0, max = 100,
                      value = c(0, 100)
                    )
                  ),
                  div(
                    style = "margin-bottom: 2px;",
                    sliderInput("display", "Display",
                      min = 1, max = 30,
                      value = 15
                    )
                  ),
                  fluidRow(
                    align = "center",
                    actionButton("searchButton", "", icon = icon("search")),
                    actionButton("resetButton", "", icon = icon("repeat"))
                  )
                )
              )
            ),
            tabPanel(
              "Flavor Profile",
              htmlOutput("profileText"),
              plotlyOutput("profile")
            ),
            tabPanel(
              "Principal Flavor",
              htmlOutput("pcaText"),
              plotlyOutput("pca")
            )
          )
        ),
        fluidRow(
          box(
            id = "whiskyCard", width = 12, solidHeader = T,
            br(),
            valueBoxOutput("whisky1"), valueBoxOutput("whisky2"), valueBoxOutput("whisky3"),
            valueBoxOutput("whisky4"), valueBoxOutput("whisky5"), valueBoxOutput("whisky6"),
            valueBoxOutput("whisky7"), valueBoxOutput("whisky8"), valueBoxOutput("whisky9"),
            valueBoxOutput("whisky10"), valueBoxOutput("whisky11"), valueBoxOutput("whisky12"),
            valueBoxOutput("whisky13"), valueBoxOutput("whisky14"), valueBoxOutput("whisky15"),
            valueBoxOutput("whisky16"), valueBoxOutput("whisky17"), valueBoxOutput("whisky18"),
            valueBoxOutput("whisky19"), valueBoxOutput("whisky20"), valueBoxOutput("whisky21"),
            valueBoxOutput("whisky22"), valueBoxOutput("whisky23"), valueBoxOutput("whisky24"),
            valueBoxOutput("whisky25"), valueBoxOutput("whisky26"), valueBoxOutput("whisky27"),
            valueBoxOutput("whisky28"), valueBoxOutput("whisky29"), valueBoxOutput("whisky30")
          )
        )
      ),
      # distillery page
      tabItem(
        tabName = "distillery",
        fluidRow(box(
          width = 12, solidHeader = T,
          leafletOutput("distilleryMap")
        ))
      ),
      # insight page
      tabItem(
        tabName = "insight",
        fluidRow(
          tabBox(
            width = 12,
            tabPanel(
              "Cluster",
              fluidRow(
                column(
                  width = 12, align = "center",
                  prettyRadioButtons(
                    label = "",
                    inputId = "clusterNum",
                    choices = c("3 clusters", "4 clusters", "5 clusters"),
                    inline = T
                  ),
                  plotlyOutput("cluster")
                )
              )
            ),
            tabPanel(
              "Flavor Vocabulary",
              fluidRow(
                column(
                  width = 12,
                  d3Output("flavorVocab")
                )
              )
            )
          )
        )
      ),
      # data page
      tabItem(
        tabName = "data",
        fluidRow(tabBox(
          width = 12,
          tabPanel("Whisky", DT::dataTableOutput("whiskyData")),
          tabPanel("Distillery", DT::dataTableOutput("distilleryData")),
          tabPanel("Flavor Vocabulary", DT::dataTableOutput("flavorData"))
        ))
      ),
      # ideas behind page
      tabItem(
        tabName = "ideas",
        fluidRow(tabBox(
          width = 12,
          tabPanel("Background", includeMarkdown("md/background.md")),
          tabPanel("Data Collection", includeMarkdown("md/data_collection.md")),
          tabPanel("EDA", includeMarkdown("md/eda.md")),
          tabPanel("Recommender", includeMarkdown("md/recommender.md")),
          tabPanel("Limitation", includeMarkdown("md/limitation.md"))
        ))
      ),
      # about page
      tabItem(
        tabName = "about",
        fluidRow(tabBox(
          width = 12,
          tabPanel("Resource", includeMarkdown("md/resource.md")),
          tabPanel("Reference", includeMarkdown("md/reference.md"))
        ))
      )
    )
  )
)

# \
#  > Server
# /

server <- function(input, output, session) {
  # hide item after server start
  shinyjs::hide(id = "profile")
  shinyjs::hide(id = "pca")
  shinyjs::hide(id = "whiskyCard")
  # empty profile page
  output$profileText <- renderText({
    "<center><font face='Indie Flower' color='#888888' size='4'>
    <br>
    This tab shows the flavor profiles of recommended whiskies!
    <br>
    Seems it's still empty...
    <br>
    Why not try entering some inputs?
    <br><br>
    </font></center>"
  })
  # empty principal flavor page
  output$pcaText <- renderText({
    "<center><font face='Indie Flower' color='#888888' size='4'>
    <br>
    This tab presents the principal flavors of recommended whiskies in 3D!
    <br>
    But it's not here yet...
    <br>
    Maybe click the search button and witness magic happens?
    <br><br>
    </font></center>"
  })
  # recommender
  observeEvent(input$searchButton, {
    # show and hide elements
    shinyjs::hide(id = "profileText")
    shinyjs::hide(id = "pcaText")
    shinyjs::show(id = "profile")
    shinyjs::show(id = "pca")
    # main progress
    withProgress(message = "Generating recommendations", value = 0, {
      n <- 5
      # sub progress: filtering data
      incProgress(1 / n, detail = "Filtering data...")

      if (length(input$type) != 0) {
        df <- df[df$type %in% input$type, ]
      }
      if (length(input$distillery) != 0) {
        df <- df[df$distillery %in% input$distillery, ]
      }
      if (length(input$origin) != 0) {
        df <- df[df$origin %in% input$origin, ]
      }
      if (length(input$cost) != 0) {
        df <- df[df$cost %in% input$cost, ]
      }
      df <- df[df$rating >= input$rating[1] & df$rating <= input$rating[2], ]
      df <- df[df$abv >= input$abv[1] & df$abv <= input$abv[2], ]

      if (dim(df)[1] == 0) {
        empty <- T
        return()
      }
      # sub progress: analyze user profile
      incProgress(1 / n, detail = "Analyzing user profile...")

      if (input$mood == "1") {
        w.diversity <- c(7, 3, 0)
      } else if (input$mood == "2") {
        w.diversity <- c(6, 3, 1)
      } else if (input$mood == "3") {
        w.diversity <- c(0, 3, 7)
      } else {
        w.diversity <- c(4, 4, 2)
      }
      if (input$exp != "") {
        exp.level <- as.numeric(input$exp)
      } else {
        exp.level <- 0
      }
      pref.flavor <- rep(0, 14)
      if (!is.null(input$flavor)) {
        pref.flavor[as.numeric(input$flavor)] <- 1
      }
      if (!is.null(input$fav)) {
        fav.id <- as.numeric(input$fav)
      } else {
        fav.id <- c()
      }

      profile <- df[, c(1, 2, 7, seq(9, 22))]
      profile$rating <- standardize(profile$rating)
      user.profile <- get.user.profile(profile, fav.id, exp.level, df.exp.profile, pref.flavor, c(8, 2))
      # sub progress: select top items
      incProgress(1 / n, detail = "Selecting top items...")

      topk <- get.topk(profile, user.profile, fav.id, 200)

      if (dim(topk)[1] == 0) {
        empty <- T
        return()
      }
      # sub progress: calculate matching score
      incProgress(1 / n, detail = "Calculating matching scores...")

      recommend <- get.score(topk, input$display, w.diversity)
      recommend <- merge(x = df, y = recommend[c("id", "score")], by = "id", all.y = T)
      row.names(recommend) <- recommend$id
      recommend <- recommend[order(recommend$score, decreasing = T), ]
      # sub progress: prepare whisky cards
      incProgress(1 / n, detail = "Preparing whisky cards...")

      for (i in 1:30) {
        output[[paste0("whisky", i)]] <- output.valuebox(recommend, i, input$display, df.color)
      }

      shinyjs::show(id = "whiskyCard")

      empty <- F
    })
    # error display
    if (empty) {
      sendSweetAlert(
        session = session,
        title = "Oups!",
        text = "No whisky found, maybe try a different combination?",
        type = "warning"
      )
      return()
    }
    # flavor profile figure
    output$profile <- renderPlotly({
      withProgress(message = "Drawing flavor profile", value = 0, {
        n <- 2
        incProgress(1 / n, detail = "Preparing data...")
        df <- recommend[, c(1, seq(9, 22))]
        df.melt <- melt(df, id.vars = c("id"))
        user.profile$id <- "yours"
        user.profile.melt <- melt(user.profile, id.vars = c("id"))
        incProgress(1 / n, detail = "Plotting...")
        plot_ly(
          data = df.melt, x = ~as.numeric(variable), y = ~value,
          color = ~as.factor(id), mode = "lines", type = "scatter"
        ) %>%
          add_trace(
            data = user.profile.melt, x = ~as.numeric(variable), y = ~value,
            line = list(dash = "dot")
          ) %>%
          layout(
            hovermode = "compare",
            xaxis = list(
              title = "",
              tickmode = "array",
              ticktext = names(df[, -1]),
              tickvals = seq(1, 14)
            ),
            yaxis = list(
              title = ""
            ),
            legend = list(orientation = "h"),
            margin = list(
              l = 0,
              r = 23,
              t = 0,
              b = 0,
              pad = 0
            )
          )
      })
    })
    # principal flavor figure
    output$pca <- renderPlotly({
      withProgress(message = "Drawing principal flavors", value = 0, {
        n <- 2
        incProgress(1 / n, detail = "Preparing data...")
        df.plot <- merge(recommend[, -9:-22], df.pca[, 1:4], by = "id")
        incProgress(1 / n, detail = "Plotting...")
        plot_ly(
          data = df.plot,
          x = ~PC1, y = ~(-PC2), z = ~PC3,
          type = "scatter3d", mode = "markers",
          color = ~type, size = ~standardize(df.plot$score),
          marker = list(sizeref = 0.01, sizemode = "area"),
          text = ~paste0(
            floor(score * 100), "%", "<br>",
            id, ": ", name, " - ", abv, "%", "<br>",
            type, "<br>",
            distillery, " - ", origin, "<br>",
            cost, " - ", rating, "/100"
          ),
          hoverinfo = "text"
        ) %>%
          layout(
            scene = list(
              xaxis = list(title = "Fruity & Floral"),
              yaxis = list(title = "Peaty & Smoky"),
              zaxis = list(title = "Rich & Full")
            ),
            legend = list(orientation = "h"),
            margin = list(
              l = 0,
              r = 0,
              t = 0,
              b = 0,
              pad = 0
            )
          )
      })
    })
  })
  # reset button
  observeEvent(input$resetButton, {
    shinyjs::reset("mood")
    shinyjs::reset("exp")
    shinyjs::reset("flavor")
    shinyjs::reset("fav")
    shinyjs::reset("type")
    shinyjs::reset("distillery")
    shinyjs::reset("origin")
    shinyjs::reset("cost")
    shinyjs::reset("rating")
    shinyjs::reset("abv")
    shinyjs::reset("display")
    shinyjs::hide(id = "profile")
    shinyjs::hide(id = "pca")
    shinyjs::hide(id = "whiskyCard")
    shinyjs::show(id = "profileText")
    shinyjs::show(id = "pcaText")

    output$profile <- NULL
    output$pca <- NULL
  })
  # distillery map
  output$distilleryMap <- renderLeaflet({
    withProgress(message = "Drawing distillery map", value = 0, {
      n <- 2
      incProgress(1 / n, detail = "Preparing data...")
      icon <- makeIcon(
        iconUrl = "img/distillery.png",
        iconWidth = 30, iconHeight = 30,
        iconAnchorX = 15, iconAnchorY = 21
      )
      incProgress(1 / n, detail = "Plotting...")
      leaflet(df.distillery, options = leafletOptions(minZoom = 2, maxZoom = 18)) %>%
        addProviderTiles(providers$Esri.WorldStreetMap) %>%
        addMarkers(~lng, ~lat,
          label = ~as.character(name),
          popup = paste("<div class='leaflet-popup-scrolled' style='max-width:250px;max-height:200px'>", df.distillery$about, "</div>"),
          group = "distillery",
          clusterOptions = markerClusterOptions(),
          icon = icon
        ) %>%
        addFullscreenControl() %>%
        addResetMapButton() %>%
        addSearchFeatures(
          targetGroups = "distillery",
          options = searchFeaturesOptions(
            zoom = 16, openPopup = T, firstTipSubmit = T,
            autoCollapse = T, hideMarkerOnCollapse = T,
            textPlaceholder = "Enter distillery names...", position = "topright"
          )
        ) %>%
        setView(lat = 5.85, lng = 0, zoom = 2) %>%
        setMaxBounds(
          lng1 = -180
          , lat1 = -90
          , lng2 = 200
          , lat2 = 90
        )
    })
  })
  # cluster
  observeEvent(input$clusterNum, {
    withProgress(message = "Drawing clusters", value = 0, {
      n <- 2
      incProgress(1 / n, detail = "Preparing data...")
      df.data.cluster <- merge(df[, 1:8], df.pca, by = "id") %>%
        merge(df.cluster, by = "id")
      df.data.cluster$cluster <- as.factor(df.data.cluster[, paste0("cluster.", substr(input$clusterNum, 1, 1))])
      incProgress(1 / n, detail = "Plotting...")
      output$cluster <- renderPlotly({
        plot_ly(df.data.cluster,
          x = ~PC1, y = ~(-PC2), z = ~PC3,
          type = "scatter3d", mode = "markers",
          size = 6,
          color = ~cluster, marker = list(sizemode = "area"),
          opacity = 0.9,
          text = ~paste0(
            id, ": ", name, " - ", abv, "%", "<br>",
            type, "<br>",
            distillery, " - ", origin, "<br>",
            cost, " - ", rating, "/100"
          ),
          hoverinfo = "text"
        ) %>%
          layout(
            scene = list(
              xaxis = list(title = "PC1: Fruity & Floral"),
              yaxis = list(title = "PC2: Peaty & Smoky"),
              zaxis = list(title = "PC3: Rich & Full")
            ),
            legend = list(orientation = "h"),
            margin = list(
              l = 0,
              r = 0,
              t = 0,
              b = 0,
              pad = 0
            )
          )
      })
    })
  })
  # flavor vocabulary
  output$flavorVocab <- renderD3({
    withProgress(message = "Drawing flavor vocabulary", value = 0, {
      incProgress(1 / 1, detail = "Analyzing flavors...")
      r2d3(script = "chart/flavor.js")
    })
  })
  # whisky data
  output$whiskyData <- DT::renderDataTable({
    DT::datatable(df[, 1:8],
      style = "bootstrap",
      class = "table-condensed",
      extensions = "Responsive",
      rownames = F,
      options = list(
        pageLength = 10,
        lengthMenu = c(5, 10, 20, 50),
        pagingType = "numbers",
        columnDefs = list(
          list(className = "dt-left", targets = 0),
          list(className = "dt-center", targets = 1:6),
          list(className = "dt-right", targets = 7)
        ),
        language = list(
          lengthMenu = "_MENU_",
          search = "_INPUT_",
          searchPlaceholder = "Search..."
        )
      )
    )
  })
  # distillery data
  output$distilleryData <- DT::renderDataTable({
    DT::datatable(df.distillery[, c(1, 2, 5:12)],
      style = "bootstrap",
      class = "table-condensed",
      extensions = "Responsive",
      rownames = F,
      options = list(
        pageLength = 10,
        lengthMenu = c(5, 10, 20, 50),
        pagingType = "numbers",
        columnDefs = list(
          list(className = "dt-left", targets = 0),
          list(className = "dt-center", targets = 1:8),
          list(className = "dt-right", targets = 9)
        ),
        language = list(
          lengthMenu = "_MENU_",
          search = "_INPUT_",
          searchPlaceholder = "Search..."
        )
      )
    )
  })
  # flavor vocabulary data
  output$flavorData <- DT::renderDataTable({
    DT::datatable(df.flavor,
      style = "bootstrap",
      class = "table-condensed",
      extensions = "Responsive",
      rownames = F,
      options = list(
        pageLength = 10,
        lengthMenu = c(5, 10, 20, 50),
        pagingType = "numbers",
        columnDefs = list(
          list(className = "dt-left", targets = 0),
          list(className = "dt-center", targets = 1),
          list(className = "dt-right", targets = 2)
        ),
        language = list(
          lengthMenu = "_MENU_",
          search = "_INPUT_",
          searchPlaceholder = "Search..."
        )
      )
    )
  })
}

# \
#  > App
# /

shinyApp(ui = ui, server = server)
