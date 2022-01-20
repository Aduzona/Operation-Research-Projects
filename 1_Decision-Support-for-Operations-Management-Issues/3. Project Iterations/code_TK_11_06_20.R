library("readxl")
product_data <- read_excel("Data_ordering.xlsx",sheet = "product data")
box_data <- read_excel("Data_ordering.xlsx",sheet = "box data")

#rack data
Total_racks=8
levels_per_rack=4
rack_length= 6000
rack_width=1750
rack_height=300

product_data$demand_per_year= ceiling((product_data$`demand per day` *365)/ product_data$`pieces/box`)

product_data$box_cost= product_data$`pieces/box` * product_data$price

product_data

# shortcut ordering cost

product_data <- merge(product_data, box_data[,c("box ID", "ordering cost (€)")], by = "box ID")

colnames(product_data)[8] <- "ordering_cost"

# order_cost <- double(length(product_data$`box ID`))
# 
# for(j in 1: length(box_data$`box ID`)){
#   for (k in 1:length(product_data$`box ID`)) {
#     if(box_data$`box ID`[j]==product_data$`box ID`[k]){
#       order_cost[k] <- box_data$`ordering cost (€)`[j]
#     }
#   }
#   
# }
# 
# product_data$ordering_cost <- order_cost 
# product_data

# EOQ ##########################################
#dei= demand_per_year, cori= ordering cost for box i
#cord= ordering cost, pri= box cost, h= interest rate

# directly vectorized
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

# constraints #####################################
# this can be formulated more elegantly, but it works and that suffices
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


# Lane occupation ######################################

lane.min <- ceiling(product_data$eoq.min * product_data$b_not_sorting / rack_length)
lane.max <- ceiling(product_data$eoq.max * product_data$b_not_sorting / rack_length)

# total rack width 

rack_total_width <- rack_width * 4 * 8

# overflow in mm
sum(lane.min*product_data$b_sorting) - rack_total_width
sum(lane.max*product_data$b_sorting) - rack_total_width
# relative overflow in %
(sum(lane.min*product_data$b_sorting) - rack_total_width)/rack_total_width*100
(sum(lane.max*product_data$b_sorting) - rack_total_width)/rack_total_width*100


##################################
# Constrained optimization
#################################

library(ROI)
library(ROI.plugin.alabama)


n <- length(product_data$`material ID`) #number of materials
cori <- product_data$ordering_cost   #ordering cost for each items
cord <-  1500    #ordering cost whenever there is an order
dei <- product_data$demand_per_year
h<-0.10
box_cost <- product_data$box_cost #pri

# objective function --> I dropped the 1500*62 as it is not decision relevant
obj.fun <- function(q, d= dei, c.or = cori, c.h = h*box_cost  ) (sum((dei/q)*c.or)+ sum(c.h*(q/2)))
# benchmarks
obj.fun(product_data$eoq.max)
obj.fun(product_data$eoq.min)
# constraint function --> also contains the ceiling of lanes and a sum was missing
const.fun <- function(q, bns = product_data$b_not_sorting, bs = product_data$b_sorting, rl = rack_length) {
  sum(bs * ceiling( bns * q / rl))
  }

const.fun(product_data$eoq.max)
const.fun(product_data$eoq.min)
# try to figure out a freasible starting solution
const.fun(product_data$eoq.min/10) -  rack_total_width

qopt <- OP(
  objective = F_objective(F=obj.fun ,n=n),
  types = rep("C",n),
  bounds = V_bound(ub= product_data$eoq.min , lb= rep(1, n)),
  constraints = F_constraint(F=const.fun,
                             dir="<=",
                             rhs = rack_total_width)
)

# solve the problem with appropriate starting vlaue and proper solver
copt_sol <- ROI_solve(qopt, start = product_data$eoq.min/10, solver = "alabama" )
# always check whether the algorithm converged
copt_sol
# solution
copt_sol$solution

#########################################
# Now you need to fine tune the results 
# --> there are rounding issues -> exactly determining the lane configuration per shelf level
# --> idea to coping with the common ordering cost
