library(shiny)
library(shinydashboard)
library(kableExtra)
library(pracma)
library(xlsx)
library(ggplot2)
library(tidyverse)
library("openxlsx")


contract <- function(mu){
  
  #mu<-2
  eta<-c(-mu/2,mu/2)
  a<- -mu/2
  b<- mu/2
  
  p= c(1/2,1/2); # probability of eta (xi)
  
  n=length(p);
  
  c=0;
  ck=0.5;
  w=1;
  r=2.5;
  #R=0;
  #Sout=0;
  
  #th= c(.1,.1);
  #CR=(r-c-ck)/(r-c)
  #}
  K<- vector(mode = "numeric",2)
  Z<- vector(mode = "numeric",2)
  K[1] <- mu+ eta[1]+a
  #paste0("Min Capacity ",K[1])
  K[2] <- mu + eta[2]+b
  #paste0("Max Capacity ",K[2])
  
  
  Kmin <- K[1]
  etamin <- eta[1]
  min.fee <- function(r,w,a,b,Kmin,mu,etamin){
    tmp1<- function(s) punif(s,a,b)
    
    Zmin=(r-w)*(Kmin-integrate(tmp1,lower= -Inf,upper = (Kmin-mu-etamin))$value)
    return(Zmin)
  }
  Zmin <- min.fee(r,w,a,b,Kmin,mu,etamin)
  #paste0("Min Fees ",Zmin)
  
  funk25=function(s) punif(s,a,b)
  
  Kmax <- K[2]
  etamax <- eta[2]
  max.fee <- function(r,w,a,b,Kmax,mu,etamax){
    
    tmp1<- function(s) punif(s,a,b)
    
    Zmax=(r-w)*(Kmax-integrate(tmp1,-Inf, (Kmax-mu-etamax))$value)
    return(Zmax)
  }
  Zmax=max.fee(r,w,a,b,Kmax,mu,etamax)
  Zmax=ceiling(Zmax)
  #paste0("Max Fees ",Zmax)
  
  
  Schrittweite_k = 1
  #Schrittweite_z = 1
  Schrittweite_z=0.25
  Schrittfaktor_k = 1/Schrittweite_k
  Schrittfaktor_z = 1/Schrittweite_z
  
  
  k_set<- seq(Kmin,Kmax,Schrittweite_k)
  z_set<- seq(Zmin,ceil(Zmax),Schrittweite_z)
  
  rows= (length(k_set)*length(z_set))*2#(2* ((ceil(Zmax)*Schrittfaktor_z)+1)*((Kmax+1)*Schrittfaktor_k)) + 1 # +1 includes header
  #rows
  con = matrix(0,nrow =rows ,ncol = 6) # only the head column is created
  #con[1,] <- c(paste(c('b','c','K','Z','v','r'), sep= " "))
  colnames(con)<-c('b','c','K','Z','v','r')
  
  
  # Main code
  #i=2
  i=1
  
  for(type in 1:2){
    if(type==1){
      name= 'b1'
      etas=eta[1]
      connum=1
    }else{
      name = 'b2'
      etas=eta[2]
      connum=1;
    }
    
    
    for(k in k_set){
      #loop from 0 to 20 for z, thus the highest z value is 23 accouting to Zmax * Schrittfactor_k
      for(z in z_set){
        #loop from 0 to 23 for z, thus the highest z value is 23 accouting to Zmax * Schrittfactor_z
        
        seit= z  # Steps of Discritization of set payment 
        #if(seit > 2*k){
        #next
        #}
        #if(seit < 0.5* k){
        #next
        #}
        
        B=  (r-w) * (k-integrate(funk25,-Inf,k-mu-etas)$value )-seit
        S= (w-c)*(k-integrate(funk25,-Inf,k-mu-etas)$value)+seit-ck*k
        SC=(r-c)*(k-integrate(funk25,-Inf,k-mu-etas)$value)-ck*k
        con[i,1]=name
        con[i,2]=connum
        con[i,3]=paste(k)
        con[i,4]=paste(seit)
        con[i,5]=round(B,6)
        con[i,6]=round(S,6)
        #con= rbind(con,c(con[i,1],con[i,2],con[i,3],con[i,4],con[i,5],con[i,6]))
        
        i= i+1 # next line
        connum=connum+1 # next contract
        
        
        
      }
      
    }
    
    
  }
  #head(con)
  # con[1:20,]
  #kable(unique(con),digits = c(1,1,1,1,1,1),"pandoc",caption = "Discretized Candidate contracts")
  
  --#excel_file<- paste0("contract_set",b,".xlsx")
    --#write.xlsx(data.frame(b=c("b1","b2"),row.names = NULL), file = excel_file,sheetName = "buyer",row.names = FALSE, append = FALSE)
    
    --#write.xlsx(data.frame(K=k_set,row.names = NULL), file = excel_file,sheetName = "capacity",row.names = FALSE, append = TRUE)
    # Add a second data set in a new worksheet
    --#write.xlsx(data.frame(Z=z_set,row.names = NULL), file = excel_file, sheetName="sidepayment", row.names = FALSE, append=TRUE)
    # Add a third data set
    #con[,c(1,4,3,5)]
    --#write.xlsx(unique(data.frame(con[,c(1,4,3,5)],row.names = NULL)), file = excel_file,sheetName="utility", row.names = FALSE, append=TRUE)
    
    --#write.xlsx(unique(data.frame(con[,c(1,4,3,6)],row.names = NULL)), file = excel_file,sheetName="revenue", row.names = FALSE, append=TRUE)
    return(data.frame(con))
}



Data<- read.xlsx("New_irrational_12.xlsx",sheet = 5)
#Data[,1:2]
contract_set1<- read.xlsx("contract_set1.xlsx",sheet = 6)
#head(contract_set1)


Data2<- read.xlsx("Existing_irrational_12_p1.xlsx",sheet = 5)
#Data#[,1:2]
#contract_set1<- read.xlsx("contract_set1.xlsx",sheet = 6)

select_contract<- function(oc,ce){
  
  contract_by_offered<-Data2%>% filter(Offered_Contract==oc & Profit_Estimate== ce)
  
  contract_low_high<-contract_set1 %>% inner_join(contract_by_offered,by=c("Z"="Res_Fees","K"="Capacity"))%>% select(b,Z,K,v,r)
  
  
  contract_low_high<-contract_low_high%>%filter(b=="b1")%>%inner_join(contract_low_high%>%filter(b=="b2"),by=c("Z"="Z","K"="K"))%>%select(K,Z,v.x,v.y,r.x,r.y)%>%mutate(sc=v.x+r.x,sc_h=v.y+r.y)
  #%>%`colnames<-`(c("$K_j$","$Z_l$","$Buyer (??=-1)$", "$Buyer(??=1)$","$Supplier(??=-1)$","$Supplier(??=1)$"))
  
  return(contract_low_high)
}


select_contract2<- function(oc,ce){
  
  contract_by_offered<-Data%>% filter(Offered_Contract==oc & Contract_Estimate== ce)
  
  contract_low_high<-contract_set1 %>% inner_join(contract_by_offered,by=c("Z"="Res_Fees","K"="Capacity"))%>% select(b,Z,K,v,r)
  
  
  contract_low_high<-contract_low_high%>%filter(b=="b1")%>%inner_join(contract_low_high%>%filter(b=="b2"),by=c("Z"="Z","K"="K"))%>%select(K,Z,v.x,v.y,r.x,r.y)%>%mutate(sc=v.x+r.x,sc_h=v.y+r.y)
  #ξ %>%kable()
  
  return(contract_low_high)
}


Data_prob<- read.xlsx("New_irrational_12.xlsx",sheet = 4)




probabilities_New<-function(oc,ce,type){
  contract_by_offered<-Data_prob%>% filter(Offered_Contract==oc & Contract_Estimate== ce)
  
  probs<-contract_by_offered%>%filter(b==type)%>%subset(X>0)
  return(round(sum(probs$X),3))
}


prob_fun<-function(type){
  sub_prob<-Data_prob%>%select(Offered_Contract,Contract_Estimate,b,X)%>%filter(b==type)%>%subset(X>0 & Contract_Estimate<9)
  sub_prob$Offered_Contract<- factor(sub_prob$Offered_Contract,levels = c('1','2','3','4','5','6','7','8'))
  sub_prob$Contract_Estimate<- factor(sub_prob$Contract_Estimate,levels = c('1','2','3','4','5','6','7','8'))
  
  return(ggplot(sub_prob,aes(x=Offered_Contract  ,y= X))+geom_bar(aes(fill= Contract_Estimate ),stat = "identity",width = 0.25)+scale_fill_brewer(palette = "Dark2")+labs(y="Choice Probability of Buyer",x="Offered Contract")+facet_grid(~Contract_Estimate)+ theme_bw()+ggtitle("Offered Contract Vs Choice probability with Contract Estimate") )
}



header<-dashboardHeader(
  title = "Choice Set Size Effect on Contracting in Supply Chain",
  dropdownMenu( type = 'message',
                messageItem(
                  from = 'LinkedIn',
                  message = "",
                  icon = icon("linkedin"),
                  href = "https://www.linkedin.com/in/diego-uchendu-1970188b/"
                )
  )
)
sidebar<- dashboardSidebar(
  sidebarMenu(
    menuItem("Contract Menu",
             tabName = "contract_menu",
             icon = icon("table")),
    menuItem("Offered Contracts",
             tabName = "Offered_Contracts",
             icon = icon("file-contract") ,
    menuSubItem("Offered Contracts",
                tabName = "Offered_Contracts",
                icon = icon("file-contract")),
    menuSubItem("Choice Probability",
                tabName="choice_prob",
                icon = icon("percent"))
    ),
    menuItem("Supplier Profit Graph",
             tabName = "Graph",
             icon = icon("chart-line")),
    
    sliderInput(
      inputId = "Offered_Contract2",
      label = "Offered Contract:",
      min=1 , max = 8,value = 4
    ),
    sliderInput("Estimate2",
                "Contract Estimate",
                min=1,
                max=8,
                value=1)
  )
)
body<- dashboardBody(
  tabItems(
    tabItem(tabName = "contract_menu",
          fluidRow(
            box(sliderInput(inputId ="Demand",
                        label = "Average Market Demand",
                        min=2,
                        max=10,
                        value = 2)),
            box(valueBoxOutput("unique_contracts"))
          )
            ,
            fluidRow(column(12,DT::dataTableOutput("table")))
    ),
    tabItem(tabName = "Offered_Contracts",
            
            #fluidRow(
              tabBox(
                #title = "first TabBox",
                id="tabset1",width = 500,
              
              tabPanel("Choice set Size",
                     "Contract and Profit Attributes",
                fluidRow(
                #box(
                  valueBoxOutput("supplier_profit")
               ,# ),
              fluidRow(column(12,DT::dataTableOutput("table_menu2")))
              
              )),
              
            tabPanel(
              "Profit Only", "No Contract Estimate",
            fluidRow(
              valueBoxOutput("supplier_profit_only"),
              sliderInput("Estimate",
                          "Profit Estimate",
                          min=1,
                          max=8,
                          value=1),
              fluidRow(column(12,DT::dataTableOutput("table_menu")))
              
            )
              ) )),
    tabItem(tabName="choice_prob",
            fluidRow(
              selectInput("buyer_type",
                              "Buyer Type",
                              choices = list("Low type","High type")
                              
                              
              ),
              
              valueBoxOutput("choice_prob")
              ,
              valueBoxOutput("no_choice_prob"))
              
            ,
            plotOutput('prob_gragh')
            ),
    tabItem(tabName = "Graph",
      plotOutput('plot2'),
      plotOutput('plot1')
      
      )
      
  )
  
)

  
ui<- dashboardPage(skin = "blue",
                   header = header,
                   sidebar = sidebar,
                   body = body)


server <- function(input, output,session) {
  
  output$table <- DT::renderDataTable(DT::datatable({
    
    data<- contract(mu=input$Demand)
    data<-data[,c("b","Z","K","v","r")]
    colnames(data)<-c("Buyer Type(b)","Reservation Fee(Z)","Capacity(K)","Buyer_Profit(v)","Supplier_Profit(r)")
    #if(input$Buyer_type=="Rational_buyer"){
    #data<- contract(mu=input$Demand)
    #data<-data[,c("b","Z","K","v","r")]
    #data<-subset(data,v>0 & r>0)
    #}
    
    data
  }))
  
  output$unique_contracts <- renderValueBox({
    data<- contract(mu=input$Demand)
    unique_contract<-nrow(data)/2
    valueBox(
      value = unique_contract,
      subtitle = "unique contracts",
      icon=icon("file-contract"),
      color = "yellow"
    )
  })
  
  output$supplier_profit <- renderValueBox({
    
    #if(input$graph=="Choice Cardinality"){
    Data_profit<- read.xlsx("New_irrational_12.xlsx",sheet = 1)
    data3_2<-Data_profit%>% filter(Offered_Contract==input$Offered_Contract2 & Contract_Estimate== input$Estimate2)%>% select(Supplier_Profit,N0_of_Contract)
    profit<-round(data3_2$Supplier_Profit,3)
    #}
    #data3_2
    valueBox(
      value = profit,
      subtitle = "Expected Supplier Profit",
      icon = icon("euro-sign"),
      color = if (profit < 0.5) {
        "blue"
      } else {
        "fuchsia"
      }
    )
  })
  
  #supplier_profit_only
  output$supplier_profit_only <- renderValueBox({
    
    #if(input$graph=="Choice Cardinality"){
    Data_profit<- read.xlsx("Existing_irrational_12_p1.xlsx",sheet = 1)
    data3<-Data_profit%>% filter(Offered_Contract==input$Offered_Contract2 & Profit_Estimate== input$Estimate)%>% select(Supplier_Profit,N0_of_Contract)
    profit<-round(data3$Supplier_Profit,3)
    #}
    #data3_2
    valueBox(
      value = profit,
      subtitle = "Expected Supplier Profit",
      icon = icon("euro-sign"),
      color = if (profit < 0.5) {
        "blue"
      } else {
        "fuchsia"
      }
    )
  })
  
  output$table_menu2 <- DT::renderDataTable(DT::datatable({
    
    
    #if(input$graph=="Choice Cardinality"){
    data2_2<- data.frame(select_contract2(input$Offered_Contract2,input$Estimate2))
    colnames(data2_2)<- c("K","Z","Buyer profit(Low Forecast)","Buyer profit(high Forecast)","Supplier profit(Low Forecast)","Supplier profit(high Forecast)","Supply Chain profit(Low)","Supply Chain profit(High)")
    #"Buyer(Low Forecast)","Buyer(high Forecast)","Supplier(Low Forecast)","Supplier(high Forecast)
    
    
    data2_2
    
  }))
  
  output$table_menu <- DT::renderDataTable(DT::datatable({
    
    
    #if(input$graph=="No Choice Cardinality"){
    data2<- data.frame(select_contract(input$Offered_Contract2,input$Estimate))
    colnames(data2)<- c("K","Z","Buyer profit(Low Forecast)","Buyer profit(high Forecast)","Supplier profit(Low Forecast)","Supplier profit(high Forecast)","Supply Chain profit(Low)","Supply Chain profit(High)")
    
    #}
    
    data2
    
  }))
  
  output$choice_prob <- renderValueBox({
    
    if(input$buyer_type=="Low type"){
      choiceP<-probabilities_New(oc=input$Offered_Contract2,ce=input$Estimate2,type="b1")
    }
    if(input$buyer_type=="High type"){
      choiceP<-probabilities_New(oc=input$Offered_Contract2,ce=input$Estimate2,type="b2")
    }
    valueBox(
      value = choiceP,
      subtitle = "Choice Probability",
      color = "green"
    )
    #1-probabilities_New(oc,ce,type)
  })
  
  output$no_choice_prob <- renderValueBox({
    
    if(input$buyer_type=="Low type"){
      NoChoiceP<- 1-probabilities_New(oc=input$Offered_Contract2,ce=input$Estimate2,type="b1")
      
    }
    if(input$buyer_type=="High type"){
      NoChoiceP<- 1-probabilities_New(oc=input$Offered_Contract2,ce=input$Estimate2,type="b2")
    }
    valueBox(
      value = NoChoiceP,
      subtitle = "No Choice Probability",
      color = "red"
    )
    #1-probabilities_New(oc,ce,type)
  })
  
  output$prob_gragh<-renderPlot({
    if(input$buyer_type=="Low type"){
    #prob_fun(type="b1")
      sub_prob<-Data_prob%>%select(Offered_Contract,Contract_Estimate,b,X)%>%filter(b=="b1")%>%subset(X>0 & Contract_Estimate<9)
      sub_prob$Offered_Contract<- factor(sub_prob$Offered_Contract,levels = c('1','2','3','4','5'))
      sub_prob$Contract_Estimate<- factor(sub_prob$Contract_Estimate,levels = c('1','2','3','4','5'))
      
      return(ggplot(sub_prob,aes(x=Offered_Contract  ,y= X))+geom_bar(aes(fill= Contract_Estimate ),stat = "identity",width = 0.25)+scale_fill_brewer(palette = "Dark2")+labs(y="Choice Probability of Buyer",x="Offered Contract")+facet_grid(~Contract_Estimate)+ theme_bw()+ggtitle("Offered Contract Vs Choice probability with Contract Estimate") )
      
    }
    if(input$buyer_type=="High type"){
      prob_fun(type="b2")
      }
    
  })
  
  output$plot2 <- renderPlot({
    
    Data_profit<- read.xlsx("New_irrational_12.xlsx",sheet = 1)
    #New_irrational_5$Offered_Contract<- factor(New_irrational_5$Offered_Contract,levels = c('1','2','3','4','5'))
    #ggplot(New_irrational_5,aes(x=Offered_Contract  ,y= Supplier_Profit))+geom_bar(aes(fill= Contract_Estimate ),stat = "identity",width = 0.25)+scale_fill_brewer(palette = "Dark2")+labs(y="Expected Supplier Profit",x="Offered Contract")+facet_grid(~Contract_Estimate)+ theme_bw()+ggtitle("Offered Contract Vs Supplier Profit with Contract Estimate")
    
    Data_profit$Offered_Contract<-as.numeric(Data_profit$Offered_Contract)
    Data_profit$Contract_Estimate<-as.numeric(Data_profit$Contract_Estimate)
    sub_profit<- subset(Data_profit,Offered_Contract<=8 & Contract_Estimate<=8)
    sub_profit$Offered_Contract<- factor(sub_profit$Offered_Contract,levels = c('1','2','3','4','5','6','7','8'))
    sub_profit$Contract_Estimate<- factor(sub_profit$Contract_Estimate,levels = c('1','2','3','4','5','6','7','8'))
    
    ggplot(sub_profit,aes(x=Offered_Contract  ,y= Supplier_Profit))+geom_bar(aes(fill= Contract_Estimate ),stat = "identity",width = 0.25)+scale_fill_brewer(palette = "Dark2")+labs(y="Expected Supplier Profit",x="Offered Contract")+facet_grid(~Contract_Estimate)+ theme_bw()+ggtitle("Offered Contract Vs Supplier Profit with Contract Estimate")
    
    
  })
  
  output$plot1 <- renderPlot({
    
    Data_profit2<- read.xlsx("Existing_irrational_12_p1.xlsx",sheet = 1)
    #Existing_irrational_5$Offered_Contract<- factor(Existing_irrational_5$Offered_Contract,levels = c('1','2','3','4','5'))
    #ggplot(Existing_irrational_5,aes(x=Offered_Contract  ,y= Supplier_Profit))+geom_bar(aes(fill= Profit_Estimate ),stat = "identity",width = 0.25)+scale_fill_brewer(palette = "Dark2")+labs(y="Expected Supplier Profit",x="Number of Contract")+facet_grid(~Profit_Estimate)+ theme_bw()+ggtitle("Offered Contract Vs Supplier Profit with Profit Estimate")
    
    
    Data_profit2$Offered_Contract<-as.numeric(Data_profit2$Offered_Contract)
    Data_profit2$Profit_Estimate<-as.numeric(Data_profit2$Profit_Estimate)
    sub_profit<- subset(Data_profit2,Offered_Contract<=8 & Profit_Estimate<=8)
    sub_profit$Offered_Contract<- factor(sub_profit$Offered_Contract,levels = c('1','2','3','4','5','6','7','8'))
    sub_profit$Profit_Estimate<- factor(sub_profit$Profit_Estimate,levels = c('1','2','3','4','5','6','7','8'))
    
    ggplot(sub_profit,aes(x=Offered_Contract  ,y= Supplier_Profit))+geom_bar(aes(fill= Profit_Estimate ),stat = "identity",width = 0.25)+scale_fill_brewer(palette = "Dark2")+labs(y="Expected Supplier Profit",x="Number of Contract")+facet_grid(~Profit_Estimate)+ theme_bw()+ggtitle("Offered Contract Vs Supplier Profit with Profit Estimate")
    
    
    
    
  })
}

shinyApp(ui=ui,server = server)

