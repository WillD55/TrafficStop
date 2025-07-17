require(ggplot2)
require(dplyr)
require(tidyverse)
require(tidygeocoder)
require(leaflet)
require(riverplot)
require(RColorBrewer)
require(pals)
require(patchwork)


Traffic2022 = read.csv("/Users/willdrake/Desktop/DataViz/FinalProject/Data/2022-middletown.csv")
Traffic2021 = read.csv("/Users/willdrake/Desktop/DataViz/FinalProject/Data/2021-middletown.csv")
Traffic2020 = read.csv("/Users/willdrake/Desktop/DataViz/FinalProject/Data/2020-middletown.csv")
Traffic2019 = read.csv("/Users/willdrake/Desktop/DataViz/FinalProject/Data/2019-middletown.csv")
Traffic2018 = read.csv("/Users/willdrake/Desktop/DataViz/FinalProject/Data/2018-middletown.csv")

Traffic2020 %>% 
  rename("ReasonForStop" = "ResonForStop") -> Traffic2020

Traffic2022 %>% 
  rename("ReasonForStop" = "ReasonforStop") -> Traffic2022

df_list <- list(
  Traffic2018, Traffic2019, Traffic2020,
  Traffic2021, Traffic2022
)

df_list <- map(df_list, ~ .x %>%
               mutate(
                   InterventionIdentificationID = as.character(InterventionIdentificationID),
                   SourceReferenceId           = as.character(SourceReferenceId))
)

allTraffic <- bind_rows(df_list)

allTraffic %>% 
  mutate(InterventionDateTime = gsub(pattern = " .*", "", InterventionDateTime)) -> allTraffic

allTraffic$InterventionDateTime = as.Date(allTraffic$InterventionDateTime,
                                       format="%m/%d/%y")
allTraffic %>%
  mutate(Year = gsub(pattern = "-.*", "", InterventionDateTime)) -> allTraffic
  
# Visualizations related to general stops vs date time and cops

ggplot(data=allTraffic)+
  geom_density(aes(x=InterventionDateTime), fill = "purple")+
  ylab("Density")+
  xlab("Traffic Stop Date")+
  ggtitle("Frequency of Traffic Stops")

ggplot(data=allTraffic)+
  geom_density(aes(x=InterventionDateTime), fill = "indianred3")+
  facet_wrap(.~Year, scales = "free_x")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8))+
  xlab("Time")+
  ylab("Density")+
  ggtitle("Frequency of Traffic Stops over Time")+
  theme(
    axis.title.x = element_blank()
  )

allTraffic %>% 
  group_by(ReportingOfficerIdentificationID) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  head(5)

allTraffic %>% 
  filter(ReportingOfficerIdentificationID == c(163,160,3235,3194,170)) -> Top5Officers

ggplot(data=Top5Officers)+
  geom_density(aes(x=InterventionDateTime), fill = "purple")+
  facet_wrap(.~ReportingOfficerIdentificationID)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8))+
  xlab("Time")+
  ylab("Density")

ggplot(data=Top5Officers)+
  geom_bar(aes(x=SubjectSexCode, fill=SubjectSexCode), position="dodge")+
  facet_wrap(.~ReportingOfficerIdentificationID)

allTraffic %>% 
  mutate(SubjectRaceCode = case_when(
    SubjectRaceCode == "W" ~ "White",
    SubjectRaceCode == "B" ~ "Black",
    SubjectRaceCode == "A" ~ "Asian",
    SubjectRaceCode == "I" ~ "American Indian"
  )) -> allTraffic

Top5Officers %>% 
  mutate(SubjectRaceCode = case_when(
    SubjectRaceCode == "W" ~ "White",
    SubjectRaceCode == "B" ~ "Black",
    SubjectRaceCode == "A" ~ "Asian",
    SubjectRaceCode == "I" ~ "American Indian"
  )) -> Top5Officers

officers <- ggplot(data=Top5Officers)+
  geom_bar(aes(x=SubjectRaceCode, fill=SubjectRaceCode), position="dodge")+
  facet_wrap(.~ReportingOfficerIdentificationID)+
  theme(
    axis.title.y = element_blank(),
    legend.position = "none")+
  xlab("Race")+
  scale_fill_manual(values = c("White" = "steelblue4","Black"="slategray3","American Indian"="slategray4","Asian"="slategray1"))

total <- ggplot(data=allTraffic)+
  geom_bar(aes(x=SubjectRaceCode, fill=SubjectRaceCode), position = "dodge")+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none")+
  ggtitle("Racial Breakdown of Traffic Stops: Overall vs. Top 5 Officers")+
  scale_fill_manual(values = c("White" = "steelblue4","Black"="slategray3","American Indian"="slategray4","Asian"="slategray1"))

total / officers

# make tick marks say the full race and then get rid of get the legend
  
# Riverplot visualization

allTraffic %>% 
  group_by(ReasonForStop) %>% 
  summarise(RecordCount = n())

unique(allTraffic$InterventionDispositionCode)

# M - Misdemeanor Summons, V - Verbal Warning, I - Infraction Ticket, W - Written Warning,
# U - Arrest, N - No Disposition

allTraffic %>% 
  group_by(InterventionDispositionCode) %>% 
  summarise(RecordCount = n())

nodes <-data.frame(ID=c("Adminstrative Offense","Cell Phone","Defective Lights","Display of Plates",
                        "Equipment Violation","Moving Violation","Registration","STC Violation",
                        "Seatbelt","Speed Related","Stop Sign","Traffic Control Signal",
                        "Unlicensed Operation","Window Tint","Other","All Stops", "N", "V", "W", "I", "M", "U"),
                   x=c(rep(0,15), 1.5, rep(3,6)),
                   y=c(
                   # offence nodes, top to bottom
                   70,65,60,55,50,45,40,35,30,25,20,15,10,5,0,
                   # All Stops in the middle
                   35,
                   # dispositions spread evenly
                   70,56,42,28,14,0),
                   col=c(alphabet()[1:15], "#000000",
                         brewer.pal(6,"Pastel2")),
                   labels=c("Adminstrative Offense","Cell Phone","Defective Lights","Display of Plates",
                            "Equipment Violation","Moving Violation","Registration","STC Violation",
                            "Seatbelt","Speed Related","Stop Sign","Traffic Control Signal",
                            "Unlicensed Operation","Window Tint","Other", "All Stops",
                            "No Disposition", "Verbal Warning", "Written Warning", "Infraction Ticket", "Misdemeanor Summons", "Arrest"))

nodes$col<-paste(nodes$col, 95, sep="")

edges <-data.frame(N1=c("Adminstrative Offense","Cell Phone","Defective Lights","Display of Plates",
                        "Equipment Violation","Moving Violation","Registration","STC Violation",
                        "Seatbelt","Speed Related","Stop Sign","Traffic Control Signal","Unlicensed Operation",
                        "Window Tint","Other", rep("All Stops", 6)),
                   N2=c(rep("All Stops", 15),"N","V","W","I",
                        "M","U"),
                   Value=c(459,1307,1354,613,23,840,282,
                           1075,33,94,1280,1530,902,32,279,
                           1464,880,240,220,5771,1528))

river_data<-makeRiver(nodes, edges)

riverplot(river_data, lty = 0, srt = 30, default_style = NULL, gravity = "top",
          node_margin = 1, nodewidth = 1, plot_area = 0.8, nsteps = 50, yscale = "auto")

title("Middletown, CT Traffic Stops Reasons & Outcomes")


# Riverplot Visualization not working yet without central node to make it clear where 
# things are going ask Nazzaro about this

allTraffic %>%
  group_by(ReasonForStop, InterventionDispositionCode) %>%
  summarise(Value = n(), .groups="drop") -> Numbers

nodes2 <-data.frame(ID=c("Adminstrative Offense","Cell Phone","Defective Lights","Display of Plates",
                        "Equipment Violation","Moving Violation","Registration","STC Violation",
                        "Seatbelt","Speed Related","Stop Sign","Traffic Control Signal",
                        "Unlicensed Operation","Window Tint","Other", "N", "V", "W", "I", "M", "U"),
                   x=c(rep(0,15), rep(4,6)),
                   y=c(seq(1,100, length.out=15), seq(1,100, length.out=6)),
                   col=c(alphabet()[1:15],
                         brewer.pal(6,"Pastel2")),
                   labels=c("Adminstrative Offense","Cell Phone","Defective Lights","Display of Plates",
                            "Equipment Violation","Moving Violation","Registration","STC Violation",
                            "Seatbelt","Speed Related","Stop Sign","Traffic Control Signal",
                            "Unlicensed Operation","Window Tint","Other",
                            "No Disposition", "Verbal Warning", "Written Warning", "Infraction Ticket", "Misdemeanor Summons", "Arrest"))

nodes$col<-paste(nodes$col, 95, sep="")

edges2 <-data.frame(N1=c(rep("Adminstrative Offense",6),rep("Cell Phone",6),rep("Defective Lights",6),rep("Display of Plates",6),
                        rep("Equipment Violation",6),rep("Moving Violation",6),rep("Registration",6),rep("STC Violation",3),
                        rep("Seatbelt",6),rep("Speed Related",6),rep("Stop Sign",6),rep("Traffic Control Signal",6),rep("Unlicensed Operation",6),
                        rep("Window Tint",6),rep("Other",6)),
                   N2=c("N","V","W","I","M","U",
                        "N","V","W","I","M","U",
                        "N","V","W","I","M","U",
                        "N","V","W","I","M","U",
                        "N","V","W","I","M","U",
                        "N","V","W","I","M","U",
                        "N","V","W","I","M","U",
                        "V","W","I",
                        "N","V","W","I","M","U",
                        "N","V","W","I","M","U",
                        "N","V","W","I","M","U",
                        "N","V","W","I","M","U",
                        "N","V","W","I","M","U",
                        "N","V","W","I","M","U",
                        "N","V","W","I","M","U"
                        ),
                   Value=Numbers$Value)

river_data<-makeRiver(nodes2, edges2)

riverplot(river_data, srt=0, plot_area = 1)

#work on combining variable to get this less messy

allTraffic %>%
  mutate(
    InterventionDispositionCode = case_when(
      InterventionDispositionCode == "V" ~ "Warning (Verbal or Written)",
      InterventionDispositionCode == "W" ~ "Warning (Verbal or Written)",
      InterventionDispositionCode == "N" ~ "No Action",
      InterventionDispositionCode == "M" ~ "Court Summons or Arrest",
      InterventionDispositionCode == "U" ~ "Court Summons or Arrest",
      InterventionDispositionCode == "I" ~ "Ticket",
      TRUE ~ InterventionDispositionCode)) -> allTraffic2

allTraffic2 %>% 
  mutate(
    ReasonForStop = case_when(
      ReasonForStop %in% c("Speed Related", "Moving Violation",
                           "Stop Sign", "Traffic Control Signal") ~ "Moving & Traffic",
          
      ReasonForStop %in% c("Cell Phone", "Seatbelt") ~ "Safety & Distraction",
          
      ReasonForStop %in% c("Unlicensed Operation", "Registration") ~ "License & Registration",
          
      ReasonForStop %in% c("Defective Lights", "Window Tint","Display of Plates",
                           "Equipment Violation", "STC Violation") ~ "Equipment & Inspection",
          
      ReasonForStop == "Administrative Offense" ~ "Administrative",
      TRUE ~ "Other")) -> allTraffic2

allTraffic2 %>%
  group_by(ReasonForStop, InterventionDispositionCode) %>%
  summarise(Value = n(), .groups="drop") -> Numbers2

nodes2 <-data.frame(ID=c("Moving & Traffic","Safety & Distraction","License & Registration","Equipment & Inspection","Administrative",
                         "Other", "No Action", "Warning (Verbal or Written)", "Ticket", "Court Summons or Arrest"),
                    x=c(rep(0,6), rep(1,4)),
                    y=c(18,15,12,9,6,3,18,13.5,9,4.5),
                    col=c(brewer.pal(6,"Blues"),
                          brewer.pal(4,"Greens")),
                    labels=c("Moving & Traffic","Safety & Distraction","License & Registration","Equipment & Inspection","Administrative","Other",
                             "No Action Taken", "Warning Verbal or Written", "Ticket", "Court Summons or Arrest"))

nodes$col<-paste(nodes$col, 95, sep="")

edges2 <-data.frame(N1=c(rep("Moving & Traffic",4),rep("Safety & Distraction",4),rep("License & Registration",4),rep("Equipment & Inspection",4),
                         rep("Administrative",4),rep("Other",4)),
                    N2=c("No Action", "Warning (Verbal or Written)", "Ticket", "Court Summons or Arrest",
                         "No Action", "Warning (Verbal or Written)", "Ticket", "Court Summons or Arrest",
                         "No Action", "Warning (Verbal or Written)", "Ticket", "Court Summons or Arrest",
                         "No Action", "Warning (Verbal or Written)", "Ticket", "Court Summons or Arrest",
                         "No Action", "Warning (Verbal or Written)", "Ticket", "Court Summons or Arrest",
                         "No Action", "Warning (Verbal or Written)", "Ticket", "Court Summons or Arrest"
                    ),
                    Value=Numbers2$Value)

river_data<-makeRiver(nodes2, edges2)

myStyle <- riverplot::default.style()
myStyle$textcex <- 0.7

riverplot(river_data,default_style = myStyle, srt=30, plot_area = 0.9)

title("Traffic Stop Reasons & Outcomes (Middletown, CT)")


# Working Shiny App maybe add more 

allTraffic %>% 
  mutate(age_group = case_when(
    SubjectAge >= 16 & SubjectAge <= 20 ~ "16 to 20",
    SubjectAge >= 21 & SubjectAge <= 24 ~ "21 to 24",
    SubjectAge >= 25 & SubjectAge <= 34 ~ "25 to 34",
    SubjectAge >= 35 & SubjectAge <= 44 ~ "35 to 44",
    SubjectAge >= 45 & SubjectAge <= 54 ~ "45 to 54",
    SubjectAge >= 55 ~ "55 and up"
  )) -> allTraffic

allTraffic$Year = as.numeric(allTraffic$Year)

allTraffic %>%
  filter(!is.na(age_group)) -> allTrafficsub

allTrafficsub %>% mutate(Outcomes = case_when(
  InterventionDispositionCode %in% c("V", "W") ~ "Warning",
  InterventionDispositionCode == "U" ~ "Arrest",
  InterventionDispositionCode == "N" ~ "No Action",
  InterventionDispositionCode == "M" ~ "Court Summons",
  InterventionDispositionCode == "I" ~ "Ticket"
)) -> allTrafficsub

allTrafficsub$Outcomes = factor(allTrafficsub$Outcomes, levels=c("No Action", "Warning", "Ticket", "Court Summons", "Arrest", ordered=TRUE))

ggplot(data = allTrafficsub)+
  geom_bar(aes(x=age_group, fill=fct_rev(Outcomes)), position="fill")+
  scale_fill_discrete(name = "Outcomes")+
  coord_flip()

ggplot(data=allTrafficsub)+
  geom_bar(aes(x=SubjectSexCode))

ggplot(data = allTrafficsub)+
  geom_density(aes(x=InterventionDateTime))+
  facet_wrap(.~age_group)

allTrafficsub %>% 
  rename(
    "Age Group" = "age_group",
    "Sex" = "SubjectSexCode",
    "Race" = "SubjectRaceCode"
  ) -> allTrafficsub
  

library(shiny)
library(bslib)
library(forcats)



ui <- page_sidebar(
  title = "Dashboard",
  sidebar = sidebar(
    selectInput(
      inputId = "demographic",
      label   = "Choose a Factor: ",
      choices = c("Age Group" = "Age Group","Sex" = "Sex", "Race" = "Race")),
    sliderInput(
      inputId = "year",
      label   = "Choose a Year Range: ",
      min     = min(allTraffic$Year),
      max     = max(allTraffic$Year),
      value   = c(min(allTraffic$Year), max(allTraffic$Year)),
      step    = 1,
      sep = "")
  ),
  
  navset_card_underline(
    title = "Demographic",
    nav_panel("Demographic Stops", plotOutput(outputId = "bar")),
    nav_panel("Outcomes Proportionally",  plotOutput(outputId = "bar2")),
  )
)

server <- function(input, output, session) {
  output$bar = renderPlot({
    allTrafficsub %>% 
      filter(!is.na(.data[[input$demographic]])) %>% 
      filter(Year <= input$year[2] & Year >= input$year[1]) -> allTrafficsub
    
    ggplot(data = allTrafficsub)+
      geom_bar(aes(x=.data[[input$demographic]], fill=.data[[input$demographic]]))+
      scale_fill_brewer(palette="Pastel1")+
      ylab("Number of Traffic Stops")+
      theme(
        axis.title.x = element_blank()    
      )
  })
  
  output$bar2 = renderPlot({
    allTrafficsub %>% 
      filter(!is.na(.data[[input$demographic]])) %>% 
      filter(Year <= input$year[2] & Year >= input$year[1]) -> allTrafficsub
    
    pastel_cols <- c("#D2B4DE", "#F1948A", "#F0B27A", "#FCF3CF", "#ABEBC6")
    
    ggplot(data = allTrafficsub)+
      geom_bar(aes(x=.data[[ input$demographic ]], fill = fct_rev(Outcomes)), position = "fill")+
      scale_fill_manual(name = "Outcomes", values = pastel_cols)+
      #facet_wrap(.~(.data[[input$demographic]]), scales = "free_x")+
      ylab("Proportion")+
      coord_flip()+
      theme(  
        axis.title.y = element_blank()    
      )
    
  })
}

shinyApp(ui, server)

# Dumbbell Plot

allTraffic %>% 
  mutate(period = case_when(
    Year %in% c(2018,2019) ~ "Pre Covid",
    Year %in% c(2020,2021,2022) ~ "During/After Covid"
  )) -> allTraffic

allTraffic %>% 
  select(ReasonForStop, VehicleSearchedIndicator, InterventionDispositionCode, period) %>% 
  group_by(period, ReasonForStop) %>% 
  summarise(stops = n(),
            searches = sum(VehicleSearchedIndicator == "True", na.rm = TRUE),
            arrests_from_searches = sum(VehicleSearchedIndicator == "True" & InterventionDispositionCode == "U", na.rm = TRUE),
            .groups = "drop") %>% 
  mutate(search_rate = searches / stops,
         arrest_rate = arrests_from_searches / searches) -> dumbbell_traffic

dumbbell_traffic %>% 
  arrange(desc(search_rate)) %>%
  arrange(period)

dumbbell_traffic_df = data.frame(
  ReasonForStop = c("Other","Unlicensed Operation","Window Tint", "Administrative Offense",
                    "Moving Violation","Seatbelt","Display of Plates","Equipment Violation","STC Violation", "Defective Lights",
                    "Stop Sign","Speed Related","Registration","Traffic Control Signal","Cell Phone"),
  search_rate_pre = c(.3125,.25,.2285,.2095,.1462,.1447,.1443,.1176,.1,.0974,.0840,.0791,.0613,.0589,.0195),
  search_rate_during_after = c(.3114,.1666,.0289,.2032,.0898,.1111,.0560,.1666,.0434,.0909,.0266,.0254,.0684,.0317,.0053)
)

dumbbell_traffic_df$ReasonForStop = factor(dumbbell_traffic_df$ReasonForStop, 
                             levels=c("Other","Unlicensed Operation","Window Tint", "Administrative Offense",
                                      "Moving Violation","Seatbelt","Display of Plates","Equipment Violation","STC Violation", "Defective Lights",
                                      "Stop Sign","Speed Related","Registration","Traffic Control Signal","Cell Phone"), ordered = TRUE)

dumbbell_traffic_df$search_rate_pre = dumbbell_traffic_df$search_rate_pre*100
dumbbell_traffic_df$search_rate_during_after = dumbbell_traffic_df$search_rate_during_after*100

ggplot(data=dumbbell_traffic_df)+
  geom_segment(aes(x=search_rate_pre, xend=search_rate_during_after, y=ReasonForStop, yend=ReasonForStop))+
  geom_point(aes(x=search_rate_pre, y=ReasonForStop, color="Before Covid"))+
  geom_point(aes(x=search_rate_during_after, y=ReasonForStop, color="During Covid"))+
  scale_color_manual("Searches",values=c("Before Covid"="red","During Covid"="blue"))+
  ylab("Reason for Stop")+
  xlab("Search Rate (%)")+
  ggtitle("Impact of the Pandemic on Search Rates")



