library(shiny)
BioComp <<- BioComp

ui <- fluidPage(
  #titlePanel("Generalizable Pest and Pathogen Model")
  tabsetPanel(id = "tabsPanel",
              tabPanel(title = "Plot", plotOutput("bigsur", height = "100%", width = "100%")
                       # selectInput(inputId = "plotDataSelect", label = "Select data to display", choices = names(dataForPlot)[2:(length(names(dataForPlot))-1)])
                       # absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                       #               draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                       #               width = 330, height = "auto",
                       #               selectInput(inputId = "plotDataSelect", label = "Select data to display", choices = names(dataForPlot)[2:(length(names(dataForPlot))-1)])
                       )
              )
              # tabPanel(title = "State Summary", leafletOutput("stateData", height = "600px")),
              # tabPanel(title = "County Summary", leafletOutput("countyData", height = "600px"))
  )



server <- function(input, output) {
  output$bigsur <- renderPlot({
    # data <- data.frame(Year = dataForPlot$Year, Variable = dataForPlot[[input$plotDataSelect]], Host = dataForPlot$Host)
    # if (input$plotDataSelect == "Area"){
    #   yName <- expression("Infected Area "*~(ha))
    # } else {
    #   yName <- expression("Number of Infected Trees (in thousands)")
    # }
    # if(!is.null(input$pest)){
    #   title = input$pest
    # } else {
    #   title = "Model Output"
    # }
    # theme = theme_set(theme_classic())
    # theme = theme_update(legend.position="top", legend.title=element_blank(),legend.spacing=unit(-0.5,"lines"), plot.background = element_rect(fill = "#3F3E3E", colour = "#3F3E3E"), panel.background = element_rect(fill = "#3F3E3E", colour = "#3F3E3E"), legend.background = element_rect(fill = "#3F3E3E"))
    # theme = theme_update(axis.text = element_text(size = 12, colour="white",family = "Helvetica"), axis.ticks=element_blank(), plot.title = element_text(hjust = 0.5,colour="white", size =18,family = "Helvetica"), axis.line = element_line(colour="white"),axis.title=element_text(size=16, vjust=0,35,colour="white", family = "Helvetica"),legend.text=element_text(size=12,colour="white",family = "Helvetica"))
    # ggplot(data, aes(x=Year, y=Variable, color=factor(Host)))+geom_line(aes(Year, Variable), size = 1.5)+scale_color_manual(values=c("#54ACC1", "#ADBD60"))+scale_fill_manual(values=c("#54ACC1", "#ADBD60"))+
    #   ggtitle(title)+theme(text = element_text(family = "sans"))+
    #   scale_x_continuous(name="Year", breaks=seq(input$start, input$end, 2))+
    #   scale_y_continuous(name=yName)+guides(col=guide_legend(ncol=3),shape=guide_legend(ncol = 1))
    theme = theme_set(theme_minimal())
    theme = theme_update(legend.position="top", legend.title=element_blank())
    theme = theme_update(axis.text = element_text(colour="black"), axis.ticks=element_blank(), plot.title = element_text(hjust = 0.5), axis.line = element_line())
    ggplot(BioComp, aes(Time, Bay2Tanoak, color=factor(Model)))+geom_line(aes(color = factor(Model)), size=1)+
    scale_color_manual(values=c("#CC0000","#7BAFD4", "#CD660D"))+scale_fill_manual(values=c("#CC0000","#7BAFD4", "#CD660D"))+ggtitle(title)+
    theme(axis.text=element_text(size=10),axis.title=element_text(size=16,vjust=0.35),legend.text=element_text(size=12),plot.title=element_text(size=22))+
    scale_x_continuous(name="Date", breaks=seq(1990,2090,20))+
    scale_y_continuous(name=expression("Bay to Tanoak Ratio"))+
    geom_ribbon(data = BioComp, aes(ymin=minB2T, ymax=maxB2T, fill = factor(Model), colour = NA), alpha=0.3)
  })

}

shinyApp(ui = ui, server = server)