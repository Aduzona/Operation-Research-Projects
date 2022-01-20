#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# https://fontawesome.com/
# 

library(shiny)
library(shinydashboard)
library(DT)
library(kableExtra)
library(knitr)
library(gtools)
library(ROI)
library(ROI.plugin.alabama)
library(readxl)

#setwd

# Data
product_data <- read_excel("Data_ordering.xlsx",sheet = "product data")
box_data <- read_excel("Data_ordering.xlsx",sheet = "box data")

Total_racks=8
levels_per_rack=4
rack_length= 6000
rack_width=1750
rack_height=300

Racks<- data.frame(Total_racks,levels_per_rack,rack_length,rack_width,rack_height)
colnames(Racks)<- c("Total_racks","levels_per_rack","length(mm)","rack_width(mm)","rack_height(mm)")
rownames(Racks)<-"Rack Information"

## Product data 

product_data<-cbind(1:62,product_data)
colnames(product_data)<- c("N0", "material ID","demand per day","box ID","pieces/box","price")

product_data$demand_per_year= ceiling((product_data$`demand per day` *262)/ product_data$`pieces/box`) 
product_data$box_cost= product_data$`pieces/box`*product_data$price

product_data <- merge(product_data, box_data[,c("box ID", "ordering cost (€)")], by = "box ID")
colnames(product_data)[9] <- "ordering_cost"

product_data<- product_data[order(product_data$N0),]
row.names(product_data)<-1:62


so_eoq_fun <- Vectorize(function(dei,cori,cord,pri,h){
    eoq<- sqrt((2*dei*(cori+cord))/(pri*h))
    return(eoq)
})

# maximum EOQ provided there is no coordination of ordering cycles (i.e., cord are not shared among parallely ordered items)
vec_so_eoq_fun.max <- so_eoq_fun(dei = product_data$demand_per_year,cori=product_data$ordering_cost,cord=1500,pri=product_data$box_cost,h=0.10)
# minimum EOQ disregarding common ordering cost cord
vec_so_eoq_fun.min <- so_eoq_fun(dei = product_data$demand_per_year,cori=product_data$ordering_cost,cord=0,pri=product_data$box_cost,h=0.10)

product_data$eoq.min <- round(vec_so_eoq_fun.min)
product_data$eoq.max <- round(vec_so_eoq_fun.max)


b_sorting <- double(length(product_data$`box ID`)) 
b_not_sorting <-double(length(product_data$`box ID`))

for(j in 1: length(box_data$`box ID`)){
    for (k in 1:length(product_data$`box ID`)) {
        if(box_data$`box ID`[j]==product_data$`box ID`[k]){
            
            if(box_data$sorting[j]=="width"){
                b_sorting[k] <- box_data$width[j]
                b_not_sorting[k]<- box_data$length[j]
                
            }else{
                b_sorting[k] <- box_data$length[j]
                b_not_sorting[k]<- box_data$width[j]
            }
        }
    }
    
}

product_data$b_sorting <- b_sorting 
product_data$b_not_sorting <- b_not_sorting


# Mathematical model

## Separate Ordering (SO)
### Not Constained using EOQ


n <- length(product_data$`material ID`) #number of materials
cori <- product_data$ordering_cost   #ordering cost for each items
cord <-  1500    #ordering cost whenever there is an order
dei <- product_data$demand_per_year
h<-0.10
box_cost <- product_data$box_cost #pri

# objective function --> I dropped the 1500*62 as it is not decision relevant
obj.fun <- function(q, d= dei, c.or = cori, c.h = h*box_cost  ) (1500*62)+(sum((dei/q)*c.or)+ sum(c.h*(q/2)))
# benchmarks
#obj.fun(product_data$eoq.max)
#obj.fun(product_data$eoq.min)
# constraint function --> also contains the ceiling of lanes and a sum was missing
const.fun <- function(q, bns = product_data$b_not_sorting, bs = product_data$b_sorting, rl = rack_length) {
    sum(bs * ceiling( bns * q / rl))
}

#const.fun(product_data$eoq.max)
#const.fun(product_data$eoq.min)
# try to figure out a freasible starting solution
#const.fun(product_data$eoq.min/10) -  rack_total_width


obj.const<- data.frame(rbind(c(obj.fun(product_data$eoq.max),const.fun(product_data$eoq.max)),c(obj.fun(product_data$eoq.min),const.fun(product_data$eoq.min) )))

colnames(obj.const)<-c("Cost Function (€)", "Capacity in (mm)") 
rownames(obj.const)<-c("Eoq Max","Eoq Min")

#kable(obj.const,caption = "Cost Values and capacities", format="pandoc")


### Seperate Ordering With Constraint
rack_total_width <- 51200 #rack_width * Total_racks * levels_per_rack

qopt <- OP(
    objective = F_objective(F=obj.fun ,n=n),
    types = rep("C",n),
    bounds = V_bound(ub= product_data$eoq.min , lb= rep(1, n)),
    constraints = F_constraint(F=const.fun,
                               dir="<=",
                               rhs = rack_total_width)
)


copt_sol <- ROI_solve(qopt, start = rep(min(product_data$eoq.max)/10,n), solver = "alabama" )

copt_solution<-data.frame(obj.fun(floor(copt_sol$solution)),rbind(floor(copt_sol$solution)))
colnames(copt_solution)<-c("cost",1:62)
rownames(copt_solution)< "constrained"

#kable(copt_solution,caption = "Capacitated values for cost and Quantity",format = "pandoc")

copt_solution


product_data$b_sorting <- b_sorting 
product_data$b_not_sorting <- b_not_sorting


header<-dashboardHeader(
    title = "Synchronized lot sizing with capacity constraint",
    dropdownMenu( type = 'message',
                  messageItem(
                      from = 'LinkedIn',
                      message = "Diego Uchendu",
                      icon = icon("linkedin"),
                      href = "https://www.linkedin.com/in/diego-uchendu-1970188b/"
                  ),messageItem(
                      from = 'LinkedIn',
                      message = "Eric Michael Djoko Polla",
                      icon = icon("linkedin"),
                      href = "https://www.linkedin.com/in/eric-michael-djoko-polla-460a09179/"
                  )
    )
)

sidebar<- dashboardSidebar(
    sidebarMenu(
        menuItem("Data",
                 tabName = "data",
                 icon = icon("database") ,
                 menuSubItem("Rack Dimensions",
                             tabName = "rack_dimensions",
                             icon = icon("warehouse")),
                 menuSubItem("Product Data",
                             tabName="product_data",
                             icon = icon("dolly-flatbed"))
        ),
        menuItem("Ordering",
                 tabName = "ordering",
                 icon = icon("shopping-cart"),
                 
                 menuSubItem("Seperate Ordering",
                             tabName = "seperate_ordering",
                             icon = icon("shopping-basket")),
                 menuSubItem("Joint Ordering",
                             tabName="joint_ordering",
                             icon = icon("cart-plus"))
                 )
    )
)

body<-dashboardBody(
    tabItems(
        tabItem(tabName = "rack_dimensions",
                fluidRow(column(12,DT::dataTableOutput("rack_table"))),
                fluidRow(column(12,DT::dataTableOutput("box_table")))
            
        ),
        tabItem(tabName = "product_data",
                fluidRow(
                    DT::dataTableOutput("product_data"))
                ),
        tabItem(tabName = "seperate_ordering",
            tabPanel("Capacity Constrained",
                "Limited rack capacity",
                fluidRow(
                    fluidRow(column(12,DT::dataTableOutput("constrained_table")))
                )
            )
        ),
        tabItem(tabName = "joint_ordering"
                
        )
        
    )
)

ui<- dashboardPage(skin = "blue",
                   header = header,
                   sidebar = sidebar,
                   body = body)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$rack_table <- DT::renderDataTable(DT::datatable({
        
        Racks
    }))
    output$box_table <- DT::renderDataTable(DT::datatable({
        
        box_data
    }))
    
    output$product_data <- DT::renderDataTable(DT::datatable({
        product_data
    
    }))
    
    output$constrained_table <-DT::renderDataTable(DT::datatable({
        t(copt_solution)
        
    }))
    
}

# Run the application 
shinyApp(ui = ui, server = server)
