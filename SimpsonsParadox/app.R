#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(gridExtra)

# Define the UI for the application

ui <- fluidPage(
  titlePanel("Explaining the Simpson's Paradox using Shiny"),
  
  h1("Short Documentation"),
  
  p(strong("
        Use of Two Slider inputs , which gives inputs to the graphs The top Slider being for the input to the easy department 
        The bottom Slider being the input to the difficult department
        There are two Graphs on the top Row, that are reactive to the input sliders 
        The left bar graph represents the change % of Male and Female admits in the easy department with variance of the easy department input slider
        The right bar graph represents the change % of Male and Female admits in the Hard department with variance of the Hard department input slider.")),
        
  p(strong("The final Bar graph in Green that is the combined bar plot, we see that , the combined plot varies as such: 
        If % Male admitted (easy/Hard) > % Male admitted (easy/Hard) then % Male admitited [Combined (easy+Hard)] can be less than % FeMale admitited [Combined (easy+Hard)]
        If % Male admitted (easy/Hard) < % Male admitted (easy/Hard) then % Male admitited [Combined (easy+Hard)] can be greater than % FeMale admitited [Combined (easy+Hard)]   
       ")),
  
  sidebarLayout(position = "left",
                sidebarPanel(
                  sliderInput("percentEInput","% Applied to the Easy Department",min=1, max=100, value = runif(1)*100),
                  sliderInput("percentHInput","% Applied to the Hard Department",min=1, max=100, value = runif(1)*100)
                ),
                mainPanel(plotOutput(
                  outputId = "linearplot", width = "100%"
                ),
                textOutput("Decision"),
                tags$head(tags$style("#Decision{color: green;
                                     font-size: 40px;
                                     font-style: bold;
                                     }"
                         )
                ))
                )
                )

# Define server logic required to draw a plot
library(ggplot2)

server<-function(input, output)
{
  
  output$linearplot <- renderPlot(
    {
      
      ####
      
      #Code can be placed here, where we can have values for the reactive plot
      data(UCBAdmissions)
      dimnames(UCBAdmissions)
      
      admindata <- data.frame(UCBAdmissions)
      
      aggdata <- setNames(aggregate(admindata$Freq, list(admindata$Admit,admindata$Dept), sum), c("Decision", "Group","Count"))
      
      grade <- c()
      
      
      # New data frame to categorize Easy or Hard
      dept.id<-c(unique(as.character(aggdata$Group)))
      
      for (i in seq(1,length(aggdata$Decision),2)){
        if(as.character(aggdata$Group[i]) == as.character(dept.id[as.integer(i/2+1)])) {
          if(as.integer(aggdata$Count[i]) > as.integer(aggdata$Count[i+1])) {
            grade[as.integer(i/2+1)] <- "Easy"
          } else if (as.integer(aggdata$Count[i] <= as.character(aggdata$Count[i+1]))) {
            grade[as.integer(
              i/2+1)] <- "Hard"
          }
        }
      }
      
      newgrade.df <- cbind.data.frame(dept.id,grade)
      newgrade.df$grade <- as.factor(newgrade.df$grade)
      
      # Making Final Data frame with only easy and hard departments
      lev <- with(admindata, levels(admindata$Dept))
      lev[lev == as.character(newgrade.df$dept.id)] <- as.character(newgrade.df$grade)
      admindata.mod <- within(admindata, levels(admindata$Dept)<-lev)
      drop<-c("Admit", "Gender", "Dept", "Freq")
      admindata.mod.clean <- admindata.mod[,!(names(admindata.mod) %in% drop)]
      
      # Collapse them 
      aggdata2 <- setNames(aggregate(admindata.mod.clean$Freq, list(admindata.mod.clean$Admit, admindata.mod.clean$Gender, admindata.mod.clean$Dept), sum),
                           c("Decision", "Gender", "Category", "Count"))
      
      TotalApp <- sum(aggdata2$Count)
      TotalApp.easy <- sum(aggdata2[which(as.character(aggdata2$Category)=="Easy"),]$Count)
      TotalApp.hard <- sum(aggdata2[which(as.character(aggdata2$Category)=="Hard"),]$Count)
      
      aggdata2$percentage <- lapply(aggdata2$Count, function(x){(x*100)/TotalApp})
      aggdata2$percentage <- as.integer(aggdata2$percentage)
      
      # Another data frame to get the Percentage Admitted only
      aggdata5<-aggdata2[which(as.character(aggdata2$Decision)=="Admitted"),]
      aggdata5$perc <- mapply(aggdata5$Category, aggdata5$Count, FUN=function(x, y){
        if (x == "Easy")
        {
          (y*100)/TotalApp.easy  
        } else {
          (y*100)/TotalApp.hard
        }
      })
      
      aggdata5$perc <- as.integer(aggdata5$perc)
      
      aggdata6<-rbind(
        aggdata5[which(as.character(aggdata5$Gender)=="Male"),],
        aggdata5[which(as.character(aggdata5$Gender)=="Female"),]
      )
      
      library(ggplot2)
      aggdata3<-aggdata2[which(as.character(aggdata2$Category) == "Easy"),]
      aggdata4<-aggdata2[which(as.character(aggdata2$Category) == "Hard"),]
      
      percentage_easy <- as.integer(sum(aggdata3$Count)*100/TotalApp)
      
      #create a new data frame, with random values
      decision1 <- c("% of Male Admitted", "% of Male Rejected")
      countofperc1 <- c(runif(1)*100, runif(1)*100)
      aggdata_final_male_easy <- setNames(cbind.data.frame(decision1, countofperc1) , c("decision", "Percentage"))
      aggdata_final_male_easy
      
      decision2 <- c("% of Female Admitted", "% of Female Rejected")
      #aggdata_final_female_easy <- setNames(cbind.data.frame(decision2, (c(100,100)-aggdata_final_male_easy$Percentage)),c("decision", "Percentage"))
      aggdata_final_female_easy <- setNames(cbind.data.frame(decision2, c(runif(1)*100, runif(1)*100)),c("decision", "Percentage"))
      aggdata_final_female_easy
      
      aggdata_final_whole_easy <- rbind(aggdata_final_male_easy, aggdata_final_female_easy)
      
      #create a new data frame, with random values
      decision3 <- c("% of Male Admitted", "% of Male Rejected")
      countofperc2 <- c(runif(1)*100, runif(1)*100)
      aggdata_final_male_hard <- setNames(cbind.data.frame(decision3, countofperc2) , c("decision", "Percentage"))
      aggdata_final_male_hard
      
      decision4 <- c("% of Female Admitted", "% of Female Rejected")
      aggdata_final_female_hard <- setNames(cbind.data.frame(decision4, c(runif(1)*100, runif(1)*100)),c("decision", "Percentage"))
      aggdata_final_female_hard
      
      aggdata_final_whole_hard <- rbind(aggdata_final_male_hard, aggdata_final_female_hard)
      
      
      
      pt1<-ggplot(aggdata_final_whole_easy, aes(x=aggdata_final_whole_easy$decision,
                                                y = aggdata_final_whole_easy$Percentage*(input$percentEInput/100)))+
        geom_bar(stat = "identity",fill="#DD8888", color="black")
      
      pt2<-ggplot(aggdata_final_whole_hard, aes(x=aggdata_final_whole_hard$decision,
                                                y = aggdata_final_whole_hard$Percentage*(input$percentHInput/100)))+
        geom_bar(stat = "identity",fill="#AA3333", color="black") 
      
      # Create the Combined Frame
      ####
      decision5 <- c("% of Male Admitted", "% of Female Admitted")
      countofperc3 <- c((aggdata_final_whole_easy$Percentage[1]*(input$percentEInput/100) + aggdata_final_whole_hard$Percentage[1]*(input$percentHInput/100))/2, 
                        (aggdata_final_whole_easy$Percentage[3]*(input$percentEInput/100) + aggdata_final_whole_hard$Percentage[3]*(input$percentHInput/100))/2)
      aggdata_final_combined <- setNames(cbind.data.frame(decision5, countofperc3) , c("decision", "Percentage"))
      
      ####
      
      pt3<-ggplot(aggdata_final_combined, aes(x=aggdata_final_combined$decision,
                                              y=aggdata_final_combined$Percentage)) +
        geom_bar(stat='identity', fill="#BBB333", color="black")
      
      
      p1<-list(pt1,pt2,pt3)      
      marrangeGrob(p1, nrow=2, ncol=2)
      
    },
    width = "auto", height = "auto")
  
  
  output$Decision <- renderText(
    {
      paste("Simpson's Paradox")
      
    }
  )
  
}

shinyApp(ui, server)

