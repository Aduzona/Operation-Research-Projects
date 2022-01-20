library(kableExtra)
library(knitr)
library(gtools)

#######################################################################
# Data preparation 
#######################################################################
library(readxl)

product_data <- read_excel("Data_ordering.xlsx",sheet = "product data")
box_data <- read_excel("Data_ordering.xlsx",sheet = "box data")
#rack data
Total_racks=8
levels_per_rack=4
rack_length= 6000
rack_width=1750
rack_height=300

init<- data.frame(Total_racks,levels_per_rack,rack_length,rack_width,rack_height)
colnames(init)<- c("Total_racks","levels_per_rack","rack_length","rack_width","rack_height")
rownames(init)<-"Rack Information"
kable(init,digits = c(1,1,1,1,1), caption= "Storage Information", format= "pandoc")
#product_data
product_data<-cbind(1:62,product_data)
colnames(product_data)<- c("N0", "material ID","demand per day","box ID","pieces/box","price")

kable(product_data[1:5,],format = "pandoc")

###############################################################
## Convert to Boxes
###############################################################

product_data$demand_per_year= ceiling((product_data$`demand per day` *262)/ product_data$`pieces/box`) 

product_data$box_cost= product_data$`pieces/box`*product_data$price

#product_data



product_data <- merge(product_data, box_data[,c("box ID", "ordering cost (€)")], by = "box ID")

colnames(product_data)[9] <- "ordering_cost"


kable(product_data[1:5,c(1,2,3,8,9)],digits = c(1,1,1,1,1,1,1,1), caption= "Merged data with yearly demand", format= "pandoc")


product_data<- product_data[order(product_data$N0),]

row.names(product_data)<-1:62
kable(product_data[1:5,c(1,2,3,4,8,9)],format = "pandoc")


###########################################################
# Separate Ordering (SO)
###########################################################

#########################
## EOQ modelling
#########################

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
#product_data

kable(product_data[1:5,c(1,2,3,4,9,10,11)],digits = c(1,1,1,1,1,1,1,1), caption= "Both EOQs added to the table", format= "pandoc")

#product_data

###############################
## Lane Occupation
###############################

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
#product_data

kable(product_data[1:5,c(2,3,9:ncol(product_data))],digits = c(1,1,1,1,1,1,1,1), caption= "Both EOQs added to the table", format= "pandoc")

#product_data

lane <- ceiling(product_data$eoq.max * (product_data$b_not_sorting/rack_length))
laneso<- data.frame(rbind(lane))
rownames(laneso)<- "lanes"
colnames(laneso)<- 1:62
kable(laneso[1:8],format = "pandoc",caption = "number of lanes per item")



rack_total_width <-51200  #rack_width * 4 * 8
#rack_total_width #56000

lane.min <- ceiling(product_data$eoq.min * product_data$b_not_sorting / rack_length)
#lane.min
lane.max <- ceiling(product_data$eoq.max * product_data$b_not_sorting / rack_length)

#lane.max

# total rack width 

rack_total_width <- 51200 # rack_width * 4 * 8

lane.df<- data.frame(rbind(lane.min,lane.max))
rownames(lane.df)<-c("Min lane","Max lane")
colnames(lane.df)<-1:62
kable(lane.df[,1:12],caption = "Number of lanes that minimizes cost",format = "pandoc")

#product_data

# overflow in mm
overflow.mm.lane.min<- sum(lane.min*product_data$b_sorting) - rack_total_width
overflow.mm.lane.max<-sum(lane.max*product_data$b_sorting) - rack_total_width
# relative overflow in %
overflow.per.lane.min<-(sum(lane.min*product_data$b_sorting) - rack_total_width)/rack_total_width*100
overflow.per.lane.max<- (sum(lane.max*product_data$b_sorting) - rack_total_width)/rack_total_width*100

overflow<- data.frame(overflow.mm.lane.min,overflow.mm.lane.max,overflow.per.lane.min,overflow.per.lane.max)

colnames(overflow)<-c("mm_lane_min","mm_lane_max","% lane_min","% lane.max")
rownames(overflow)<- "Overflow"

kable(overflow,caption = "Rack Capacity overflow in mm and percentage",format = "pandoc")

#product_data

#####################################
##Introducing capacity constraint
#####################################

#get the width of the lanes in mm
rack_width_occupied=sum(lane.min*product_data$b_sorting)
kable(paste(rack_width_occupied,"mm"," is the total expected to be occupied"))

if(rack_width_occupied <= rack_total_width){
  print(paste0("Capacity constraint fulfilled: ", rack_width_occupied, "<=",rack_total_width))
  
}else {
  violated <- rack_width_occupied - rack_total_width
  kable((paste0("Capacity constraint violated by: ",violated )), format = "pandoc")
}

###########################################
## Constrained EOQ optimization
###########################################

library(ROI)
library(ROI.plugin.alabama)

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

kable(obj.const,caption = "Cost Values and capacities", format="pandoc")

#########################################
##Now adding capacity constraint
#########################################

qopt <- OP(
  objective = F_objective(F=obj.fun ,n=n),
  types = rep("C",n),
  bounds = V_bound(ub= product_data$eoq.min , lb= rep(1, n)),
  constraints = F_constraint(F=const.fun,
                             dir="<=",
                             rhs = rack_total_width)
)

#This shows that minimum EOQ is too big and therefore will not meet the rack space constraint.
#const.fun(min(product_data$eoq.max))#366272
#const.fun(min(product_data$eoq.max)/8)# 61428  still > 56000
#const.fun(min(product_data$eoq.max)/8.7)#52716 < 56000
#const.fun(min(product_data$eoq.max)/10)# 33098 < 56000 is much lower but this can still be the starting value
#round(min(product_data$eoq.max)/10) #15


copt_sol <- ROI_solve(qopt, start = rep(min(product_data$eoq.max)/10,n), solver = "alabama" )
# always check whether the algorithm converged
#copt_sol# The objective value is: 101625.9
# solution
#copt_sol$solution #vector of optimal Quantity that meets the space constraints and minimizes the Obj function.
#round(copt_sol$solution)
#copt_sol$objval #101625.9



#const.fun(copt_sol$solution)#55884
#const.fun(floor(copt_sol$solution))#55092


#obj.fun(copt_sol$solution)#101625.9
#obj.fun(floor(copt_sol$solution))#104639.7  rounded q values

copt_solution<-data.frame(obj.fun(floor(copt_sol$solution)),rbind(floor(copt_sol$solution)))
colnames(copt_solution)<-c("cost",1:62)
rownames(copt_solution)< "constrained"

kable(copt_solution[,1:15],caption = "Capacitated values for cost and Quantity",format = "pandoc")

###################
## No constraint
###################

rack_total_width <- 51200
qopt1 <- OP(
  objective = F_objective(F=obj.fun ,n=n),
  types = rep("C",n),
  bounds = V_bound(ub= product_data$eoq.min , lb= rep(1, n)),
)

copt_sol1 <- ROI_solve(qopt1, start = rep(min(product_data$eoq.max)/10,n), solver = "alabama" )
# always check whether the algorithm converged
#copt_sol1$objval

####################
## Compare results
####################

obj.fun <- function(q, d= dei, c.or = cori, c.h = h*box_cost  ) (1500*62)+(sum((dei/q)*c.or)+ sum(c.h*(q/2)))
obj.fun(product_data$eoq.max)#172279.7

#const.fun(copt_sol$solution)#51132
#const.fun(floor(copt_sol$solution))#50340  we rounded down to avoid exceeding capacity


#obj.fun(copt_sol$solution)#109279.9
#obj.fun(floor(copt_sol$solution))#112079.8 rounded q values

const.unconst <- data.frame(rbind(c(round(obj.fun(product_data$eoq.max)),round(const.fun(product_data$eoq.max))),c(round( obj.fun(copt_sol$solution)),round(const.fun(copt_sol$solution)))))
colnames(const.unconst)<-c("Cost Function (€)","capacitaty")
rownames(const.unconst)<-c("Eoq Max","Optimized Q")

kable(const.unconst,caption = "costs",format = "pandoc")


obj.fun(copt_sol1$solution)#124505.1
obj.fun(floor(copt_sol1$solution))#124508.6 rounded q values

# demand vs quantity

#product_data$`demand per day`
#product_data$demand_per_year  #

#floor(copt_sol1$solution)#  optimum quantities with no capacity constraint same as eoq.min 
#floor(copt_sol$solution) # optimum quantities with capacity constraint


#EOQ cost

#with no constraint
paste("with no capacity constraint, cost is  €", round(obj.fun(product_data$eoq.max)))#172280

#with no capacity constraint optimized.
paste("with no capacity constraint optimized, cost is  €",round(obj.fun(copt_sol1$solution)))# eoq.min

#with constraint
paste("with capacity constraint, cost is  €",round(obj.fun(copt_sol$solution))) #202280



kable(paste("It costs €",floor(obj.fun(floor(copt_sol$solution)))-floor(obj.fun(floor(product_data$eoq.max)))," extra due to lack of capacity"),format = "pandoc")


x.val <- seq.int(1,62, length.out = 62)

cost.fun <- function(q, d= dei, c.or = cori, c.h = h*box_cost  ){
  (1500 +(dei/q)*c.or)+ (c.h*(q/2))
} 

y.vec.no.const <- cost.fun(q=product_data$eoq.max)
y.vec.yes.const<- cost.fun(copt_sol$solution)

kable(paste(length(which((round(y.vec.yes.const)>=round(y.vec.no.const))==TRUE))," Items costs more or same when capacity becomes a problem"),format = "pandoc")



#par(mfrow=c(1,1))
{ 
  plot(x.val,round(y.vec.no.const), xlab ="lot size q", ylab = "total cost", type = "p", lwd =2, ylim=c(12,max(y.vec.no.const)), xaxp=c(1, 10, 1) )
  axis(1, 1:62)
  axis(2, 1:62)
  abline(h=1:62, v=1:62, col="gray", lty=3)
  
  points(x=x.val,y=round(y.vec.yes.const),lwd=2, col="blue")
  
  legend("bottomleft",lty = c(1,1),col = c("black","blue"), legend = c("No constraint","with constraint"), bty="n" , horiz = T)
}

###################################################################
# Joint Ordering (JO)
###################################################################

n <- length(product_data$`box ID`)
c.or0 <- cord
c.or <- product_data$ordering_cost
#H.vec <- 0.5 * dei * c.sh
# based on the previous definition, I think this should be the vector of holding cost multipliers
H.vec <- 0.1 * product_data$box_cost * dei * 0.5

####################################################
## Objective Function with no capacity constraint
####################################################

jrp.obj.fun <- function(m , B, cor, Hvec, cor0) cor0/B + sum(cor/B/m) + sum(Hvec*B*m)

# calculate cycle times
T.vec <- sqrt(c.or/H.vec)
# order products
reo.id <- order(T.vec)
T.vec <- T.vec[reo.id]
c.or <- c.or[reo.id]
H.vec <- H.vec[reo.id]

cost_cycle.mat <- t(cbind(c.or,H.vec,T.vec))
costs.cycle <- data.frame(cost_cycle.mat)
colnames(costs.cycle) <- reo.id 
rownames(costs.cycle) <- c(  "$c_i^{or}$" ,"$H_i$","$T_i$" )
kable(costs.cycle[,1:4],"pandoc", row.names = T)

library(kableExtra)
# calculate T^2 and cumu. cost shares
res.mat <- t(cbind(c.or/H.vec,(c.or0 + cumsum(c.or))/cumsum(H.vec)))
df <- data.frame(res.mat)
colnames(df) <- reo.id 
rownames(df) <- c(  "$T_i^2$" ,"$T^C$"  )
kable(df[,1:4],"pandoc", row.names = T)

# identify break
which(res.mat[1,] > res.mat[2,])# break occured after 13
id.comb <- min(which(res.mat[1,] > res.mat[2,])) - 1 #3
#id.comb
# calculate B
B <- min(T.vec) #0.01955164
# solution with m - integers #######################
m.vec.int <- round(T.vec/B,0)

m.vec.int[1:id.comb] <- 1  
# re-optimize B for fixed m.vec
B.int <- sqrt(sum(c.or/m.vec.int)/sum(m.vec.int*H.vec))#0.02014868
# total cost 
c.cost.int <- jrp.obj.fun( m = m.vec.int, B=B.int, cor = c.or,Hvec=H.vec, cor0 = c.or0)#192606
df <- data.frame(rbind(round(T.vec/B.int,2), round(T.vec/B.int), m.vec.int))
colnames(df) <- reo.id 

rownames(df) <- c("$m_i= {T_i}/{B}$" ,"$[m_i]$", "$[\\tilde{m}_i]$" )
kable(df[,1:6],"pandoc", row.names = T)


dei <- dei[reo.id]
df <- data.frame(rbind( m.vec.int, round(m.vec.int*B.int, 2), round(m.vec.int*B.int*dei, 2) ))
colnames(df) <- reo.id 
rownames(df) <- c("$[\\tilde{m}_i]$", "$T_i$", "$q_i$" )
kable(df[,1:6],"pandoc", row.names = T)


q.vec <- m.vec.int*B.int*dei
#paste(round(c.cost.int)," is total cost with no capacity constraint")# 49943
#const.fun(q.vec) > rack_total_width

jrp.no_cap <- data.frame(round(c.cost.int),const.fun(q.vec))

colnames(jrp.no_cap)<-c("Cost(€)","Capacity (mm)")
rownames(jrp.no_cap)<-"Not constrained"

kable(jrp.no_cap,digit=c(1,1),caption = "cost with no capacity constraint", format = "pandoc")


library(ROI)
jrp.obj.fun2 <- function(Tvec, cor, Hvec, cor0) sum(cor0/Tvec) + sum(cor/Tvec) + sum(Hvec*Tvec)
# you need to initialize the parameters in the objective function --> Tvec is to be optimized over, the rest is given
jrp.obj.fun2 <- function(Tvec, cor=c.or, Hvec = H.vec, cor0 = c.or0) sum(cor0/Tvec) + sum(cor/Tvec) + sum(Hvec*Tvec)

qopt2 <- OP(
  objective = F_objective(F=jrp.obj.fun2 , n=62),
  types = rep("C",n),
  bounds = V_bound(ub= rep(30, 62), lb= rep(.001,62))
) 

#jrp.obj.fun2(m.vec.int*B)
#jrp.obj.fun2(rep(.00001,62))
#jrp.obj.fun2(rep(15,62))
# you don't need Alabama here as there is no constraint. Basically you can also optimize this problem by taking derivatives
copt_sol2 <- ROI_solve(qopt2, start = m.vec.int*B)
#copt_sol2

#copt_sol2$solution


jrp.no_cap.cycleTime <- data.frame(round(copt_sol2$objval),const.fun(copt_sol2$solution*dei))

colnames(jrp.no_cap.cycleTime)<-c("Cost(€)","Capacity (mm)")
rownames(jrp.no_cap.cycleTime)<-"Not constrained"

kable(jrp.no_cap.cycleTime,digit=c(1,1),caption = "cost with no capacity constraint using cycle Time", format = "pandoc")



m.vec.int*B*dei

paste(round(copt_sol2$objval), "Total cost with no Constraint for T")

##################################
## Including capacity constraint
##################################

# we reinitialize the vectors
n <- length(product_data$`box ID`)
c.or0 <- cord
c.or <- product_data$ordering_cost
dei <- product_data$demand_per_year
H.vec <- 0.1 * product_data$box_cost * dei * 0.5


bnotsort <- product_data$b_not_sorting
bsort <- product_data$b_sorting
copt_sol.solution <- copt_sol$solution



# calculate cycle times
T.vec <- sqrt(c.or/H.vec)
# order products
reo.id <- order(T.vec)
T.vec <- T.vec[reo.id]
c.or <- c.or[reo.id]
H.vec <- H.vec[reo.id]
bnotsort <- bnotsort[reo.id]
bsort <- bsort[reo.id]
copt_sol.solution <- copt_sol.solution[reo.id]
dei <- dei[reo.id]

const.fun2 <- function(m, B = B.start, y= dei, bns = bnotsort, bs = bsort, rl = rack_length) {
  sum(bs * ceiling( bns * m*B*y / rl))
}



#rack_total_width<-51200

#library(ROI.plugin.deoptim)

T.feas <- copt_sol.solution/dei
B.start <- min(T.feas)

jrp.obj.fun <- function(m , B = B.start, cor = c.or, Hvec = H.vec, cor0 = c.or0) cor0/B + sum(cor/B/m) + sum(Hvec*B*m)
#jrp.obj.fun(m= rep(1, 62))
#const.fun2(m= rep(1, 62))

qopt3 <- OP(
  objective = F_objective(F=jrp.obj.fun ,n=n),
  # now integer decision variables
  types = rep("C",n),
  bounds = V_bound(li = 1:n, ui = 1:n, ub= rep(50, n) , lb= rep(1, n)),
  constraints = F_constraint(F=const.fun2,
                             dir="<=",
                             rhs = rack_total_width)
)
# Good starting point essential --> m = T.feas/B.start
copt_sol3 <- ROI_solve(qopt3, start = T.feas/B.start , solver = "alabama")  # "deoptimr"

#copt_sol3

#copt_sol3$solution

#jrp.obj.fun(m= copt_sol3$solution)
#const.fun2(m= copt_sol3$solution) <= rack_total_width

jrp.cap.m <- data.frame(round(copt_sol3$objval),const.fun2(m= copt_sol3$solution))

colnames(jrp.cap.m)<-c("Cost(€)","Capacity (mm)")
rownames(jrp.cap.m)<-"Capacitated"

kable(jrp.cap.m,digit=c(1,1),caption = "cost with capacity constraint using Basic period approach", format = "pandoc")


# not feasible
#const.fun2(round(copt_sol3$solution)) <= rack_total_width
# feasible
#const.fun2(floor(copt_sol3$solution)) <= rack_total_width
# potential starting solution
m.start <- floor(copt_sol3$solution) # multiplier m
q.start <- ceiling(m.start * B.start * dei)   # order quantity q (in boxes, rounded up)
#q.start


#Order according to m in ascending order
#reo.id <- order(m.start)
#dei <- dei[reo.id]
#m.start <- m.start[reo.id]

df2 <- data.frame(rbind( m.start, round(m.start*B.start, 2), ceiling(m.start*B.start*dei) ))
colnames(df2) <- reo.id 
rownames(df2) <- c("$[\\tilde{m}_i]$", "$T_i$", "$q_i$" )
kable(df2[,1:6],caption = "First 7 items in order before reordering","pandoc", row.names = T)

library(gtools)
df2<-df2[mixedorder(colnames(df2))]


kable(df2[,1:6],caption = "First 7 items in order After reordering","pandoc", row.names = T)

q.start<- as.vector(df2[3,1:62])


kable(q.start[,1:9],caption = "After reordering","pandoc", row.names = T)


l.start <- ceiling(q.start/floor(rack_length/product_data$b_not_sorting))
rownames(l.start) <- "lanes"


kable(l.start[,1:10],caption = "number of lanes for each items ", row.names = T,format = "pandoc")


unique(round(product_data$b_sorting/100)*100)



patterns <- rbind(
  c(8,0,0),
  c(0,4,0),
  c(0,1,2),
  c(2,0,2),
  c(6,1,0),
  c(4,2,0),
  c(2,3,0),
  c(3,1,1),
  c(1,2,1)
)
colnames(patterns) <- c("200","400","600")



kable(as.data.frame(patterns) ,caption = "Total of 9 patterns ", row.names = T,format = "pandoc")




# assign lane width as names
names(l.start) <- round(product_data$b_sorting/100)*100 

# number of items with lane types 200, 400, 600 that is demand for each lane type
ld <- c(sum(l.start[names(l.start) == "200"]),
        sum(l.start[names(l.start) == "400"]),
        sum(l.start[names(l.start) == "600"]))
ld <-data.frame(rbind(t(ld)))
colnames(ld)<-c("200","400","600")

vec<-unlist(l.start)

product_data$number_of_lanes <- vec


kable(ld ,caption = "number of lanes per required of each type", row.names = T,format = "pandoc")
#ld


tmp<- q.start/floor(rack_length/product_data$b_not_sorting)
rownames(tmp)<- "l(qi)"
kable(as.data.frame(rbind(tmp[1:6])),format = "pandoc",caption = "lanes in fraction")


tmp<-as.data.frame(rbind(tmp))
kable(tmp[,c(15,17,18,50,57,58)],format = "pandoc",caption = "selected items")


#cycle_demand== Lot_size_q
#cycle_demand <- product_data$cycle_time_in_days*product_data$deman_per_day_boxes

#l.start2 <- (cycle_demand *product_data$b_not_sorting)/rack_length
#l.start2 <- ceiling(l.start)
l.start[15]<-1
l.start[17]<- 1
l.start[18]<-1
l.start[50]<- 1
l.start[57]<- 2
l.start[58]<-2

#l.start2 <- ceiling(l.start)
vec<-unlist(l.start)

product_data$number_of_lanes <- vec


kable(product_data[c(15,17,18,50,57,58),c(2,3,12:ncol(product_data))],format = "pandoc")

#product_data



# assign lane width as names
names(l.start) <- round(product_data$b_sorting/100)*100 

# number of items with lane types 200, 400, 600 that is demand for each lane type
ld <- c(sum(l.start[names(l.start) == "200"]),
        sum(l.start[names(l.start) == "400"]),
        sum(l.start[names(l.start) == "600"]))
kable(as.data.frame(ld),format = "pandoc",caption = "Number of lanes for 200, 400 and 600")


###################################
##Changes made for lane assignment
###################################

pat<-data.frame(patterns[3:4,]) 
colnames(pat)<-c("200","400","600")

kable(pat,caption = "Patterns needed", row.names = T, escape = F,format = "pandoc")

##Assign Patterns



fitted.pattern <- patterns[3:4,]
# loop through 32 levels to assign patterns

levels.pattern.mat <- matrix(0,  nrow = 33,ncol = 3)

for (i in 1:32) {
  for (j in 1:3) {
    
    if(i<= 16){
      levels.pattern.mat[i,j] <- t(fitted.pattern[1,j])
    } else {
      levels.pattern.mat[i,j] <- t(fitted.pattern[2,j])
    }
  }
}




levels.pattern.mat<-data.frame(rbind(levels.pattern.mat)) 
colnames(levels.pattern.mat)<-c("200","400","600")
levels.pattern.mat[33,]<-rbind(sum(levels.pattern.mat[1:32,1]),sum(levels.pattern.mat[1:32,2]),sum(levels.pattern.mat[1:32,3]))
rownames(levels.pattern.mat)<-c(1:32,"sum")
kable(levels.pattern.mat,caption = "Pattern Assigned for each level", row.names = T, escape = F,format = "pandoc")

# confirm if lane demand is met.

#sum(levels.pattern.mat[1:32,1]) >= sum(ld[1]) #TRUE  for 200
#sum(levels.pattern.mat[1:32,2]) >= sum(ld[2]) #TRUE  for 400
#sum(levels.pattern.mat[1:32,3]) >= sum(ld[3]) #TRUE  for 600


## Illustration

#m.start

product_data$Order_frequency_M<- unlist(df2[1,1:62])
product_data$Lot_size_q <- unlist(q.start)
#product_data

#m.start <- sort(unique(product_data$Order_frequency_M))
m.start <- product_data$Order_frequency_M
#sort(unique(m.start)) # 21 unique values meaning 22 columns

#max(m.start)

name.m<- paste("Mi=",sort(unique(m.start)), sep = "")

cycle.mat <- matrix(0,  nrow = max(m.start)+1,ncol = length(unique(m.start)))

colnames(cycle.mat)<-name.m

#cycle.mat
# for first row
i<- NULL
j<-1


for(i in sort(unique(m.start))){
  
  tmp <- which(m.start==i)
  tmp<- paste(tmp,collapse = ",")
  
  cycle.mat[1,name.m[j]] <- tmp
  j<- j+1
  
}



for (k in 1:max(m.start)+1) {
  j<- 1
  for(i in sort(unique(m.start))){
    if((k-1)%%i==0){
      tmp <- which(m.start==i)
      tmp<- paste(tmp,collapse = ",")
      
      #print(tmp)
      
      cycle.mat[k,name.m[j]] <- tmp
      j<- j+1 
    }else {
      j<- j+1  
    }
    
  }
  
}

cycle.df <- as.data.frame(cycle.mat)  
#cycle.df

rownames(cycle.df)<-1:nrow(cycle.df)

#[1:6,1:3]
kable(cycle.df, row.names = T, format = "pandoc")


paste("We have ", ncol(cycle.df)," Order frequencies using Joint Ordering JRP")


kable(product_data[4,10:ncol(product_data)],caption = "item 4", format = "pandoc")


# Demand per day in boxes
#its still okay to leave the values in fraction as we are not ordering now.
product_data$deman_per_day_boxes <- product_data$`demand per day`/product_data$`pieces/box`

#This is number of days it takes before a new order "product_data$Lot_size_q" is placed
product_data$cycle_time_in_days <- (m.start*B.start*product_data$demand_per_year)/product_data$deman_per_day_boxes
#product_data$cycle_time_in_days

kable(product_data[4,14:ncol(product_data)],caption = "item 4", format = "pandoc")

cyc<- as.data.frame(rbind(product_data$Order_frequency_M,product_data$cycle_time_in_days))
colnames(cyc)<-1:62
rownames(cyc)<-c("M","days")
#[,1:4]
kable(cyc,caption = "cycle time for each item", format = "pandoc")

product_data$cycle_time_in_days<-floor(product_data$cycle_time_in_days)

cyc<- as.data.frame(rbind(product_data$Order_frequency_M,product_data$cycle_time_in_days))
colnames(cyc)<-1:62
rownames(cyc)<-c("M","days")
kable(cyc,caption = "cycle time for each item", format = "pandoc")

#########################
##Convert Cycles to days
#########################

#min(product_data$cycle_time_in_days)#2.472104 days is equivalent to M=1
#sort(sort(unique(product_data$Order_frequency_M)))
#sort(unique(product_data$Order_frequency_M))*min(product_data$cycle_time_in_days)

cycle.time.frac <-(m.start*B.start*product_data$demand_per_year)/product_data$deman_per_day_boxes

cycle.time <- data.frame(rbind(sort(unique(product_data$Order_frequency_M))*min(cycle.time.frac)   ))

colnames(cycle.time) <- sort(unique(product_data$Order_frequency_M))
rownames(cycle.time) <- c("Days M." )

kable(round(cycle.time,2), format = "pandoc")

########################
##(T,S) policy
########################

kable(product_data[4,c("material ID","box ID","number_of_lanes","b_not_sorting")],format = "pandoc")

#install.packages("writexl")
#library("writexl")

#write_xlsx(product_data,"C:\\Users\\aduzo\\Desktop\\Scientific Project R\\Scientific Project Main\\Decision-Support-for-Operations-Management-Issues\\product_data.xlsx")

stock.level <- floor((product_data$number_of_lanes*rack_length)/product_data$b_not_sorting)

#product_data$number_of_lanes
#floor(stock.level)
stock.level<- data.frame(rbind(floor(stock.level)))
colnames(stock.level)<-1:62
rownames(stock.level)<- "Stock level"

kable(stock.level[,1:10],caption = "Stock level derived from lanes assigned per item", format = "pandoc")

stock.level<-floor(unlist(stock.level))

Periodic.inventory <-function(item){
  
  #Yt demand
  dem <- product_data$deman_per_day_boxes
  #dem[item]
  
  #qt quantity
  quant <- product_data$Lot_size_q
  #quant[item]
  
  #Initial Stock level
  stock.level <- floor((product_data$number_of_lanes*rack_length)/product_data$b_not_sorting)
  #stock.level[item]
  
  #inventory level
  #inventory.level <- inventory.level[t-1]- dem[t]
  
  period.mat <- matrix(0.00000,  nrow = 3,ncol = 262)
  
  colnames(period.mat)<-1:262
  m <- product_data$cycle_time_in_days
  
  #period.mat[3,0]<-  stock.level[1]
  t<- 1
  #for (t in 1:262) {
  while(t<=262){
    
    
    if(t==1){
      period.mat[1,t]<-dem[item]
      period.mat[2,t]<- stock.level[item]
      period.mat[3,t]<- stock.level[item]-dem[item] #stock.level[1]
      
    }else {
      period.mat[1,t]<-dem[item]
      if(t %% floor(m[item])==1){
        #b<-t+1
        #print(t+1)
        period.mat[2,t]<- quant[item]
        
        # fill the capacity
        period.mat[3,t]<- stock.level[item] #quant[item]+period.mat[3,t-1]-dem[item]
      }else{
        period.mat[2,t]<- 0
        period.mat[3,t]<- period.mat[3,t-1]-dem[item]
      }
    }
    
    
    #}
    
    t=t+1
    
  }
  
  
  return (period.mat)
  
}

period.df <- as.data.frame(Periodic.inventory(4))
row.names(period.df)<- c("yt","qt","lt")

#[,1:11]
kable(period.df,caption = "Periodic Inventory TS Policy", row.names = T, escape = F,format = "pandoc")


Ave.phy.inv <-c(vector(),1:62)
alpha.sl <- c(vector(),1:62)
for (i in 1:62) {
  period.df <- as.data.frame(Periodic.inventory(i))
  row.names(period.df)<- c("yt","qt","lt")
  Ave.phy.inv[i]<- sum(period.df[3,][period.df[3,]>0])/length(period.df[3,])
  alpha.sl[i] <- (length(period.df[3,][period.df[3,]>=0])/length(period.df[3,])) *100
}

ts.summary <- data.frame(rbind(Ave.phy.inv,alpha.sl))

colnames(ts.summary)<- 1:62
rownames(ts.summary)<-c("Average Inventory","Alpha Service Level")

kable(ts.summary[,c(1,17,18,50)],caption = "Periodic Inventory TS Policy",format = "pandoc")


Ave.phy.inv <-c(vector(),1:62)
alpha.sl <- c(vector(),1:62)
for (i in 1:62) {
  period.df <- as.data.frame(Periodic.inventory(i))
  row.names(period.df)<- c("yt","qt","lt")
  Ave.phy.inv[i]<- sum(period.df[3,][period.df[3,]>0])/length(period.df[3,])
  alpha.sl[i] <- (length(period.df[3,][period.df[3,]>=0])/length(period.df[3,])) *100
}

ts.summary <- data.frame(rbind(Ave.phy.inv,alpha.sl))

colnames(ts.summary)<- 1:62
rownames(ts.summary)<-c("Average Inventory","Alpha Service Level")

kable(ts.summary[,c(1,17,18,50)],caption = "Periodic Inventory TS Policy",format = "pandoc")



names(product_data)
kable(product_data[c(17,18,50),c("deman_per_day_boxes","Order_frequency_M","cycle_time_in_days")],format = "pandoc")


names(product_data)
kable(product_data[c(17,18,50),c("deman_per_day_boxes","Order_frequency_M","cycle_time_in_days")],format = "pandoc")

product_data$cycle_time_in_days[c(17,18)]<-37
product_data$cycle_time_in_days[c(50)]<-15


#Alpha service level after adjustments


Ave.phy.inv <-c(vector(),1:62)
alpha.sl <- c(vector(),1:62)
for (i in 1:62) {
  period.df <- as.data.frame(Periodic.inventory(i))
  row.names(period.df)<- c("yt","qt","lt")
  Ave.phy.inv[i]<- sum(period.df[3,][period.df[3,]>0])/length(period.df[3,])
  alpha.sl[i] <- (length(period.df[3,][period.df[3,]>0])/length(period.df[3,])) *100
}

ts.summary <- data.frame(rbind(Ave.phy.inv,alpha.sl))

colnames(ts.summary)<- 1:62
rownames(ts.summary)<-c("Average Inventory","Alpha Service Level")

#[,1:4]

kable(ts.summary[,c(1,17,18,50)],caption = "Periodic Inventory TS Policy",format = "pandoc")


period.df <- as.data.frame(Periodic.inventory(50))
row.names(period.df)<- c("yt","qt","lt")


kable(period.df[,1:7],caption = "Periodic Inventory TS Policy", row.names = T, escape = F,format = "pandoc")


item <- 4
par(family="serif", mar = c(4.25,4.25,.1,.1), bg ="white")
{plot(Periodic.inventory(item)[3,], xlab="Time", type="l", lwd=2, ylab="demand", cex.lab = 1.5, cex.axis = 1.25)
  abline(h=stock.level[item], col="red", lwd=2)
  title(main=paste("item ",item),outer = TRUE)}


################################
## Using S,Q Policy 
################################

# Check if Lot size q is less than or equal to Stock level

j<- 0
k<-0
for(i in 1:62){
  if(product_data$Lot_size_q[i]<= stock.level[i]){
    j<-j+1
    k<-k+1
  }else{
    j<-j+1
    
    print(paste("item ", j," Stock level greater Lot size"))
  }
}

paste(k," items have Quantity less than or equal to capacity")



continous.inventory <-function(item){
  
  #Yt demand
  dem <- product_data$deman_per_day_boxes
  #dem[item]
  
  #qt quantity
  quant <- product_data$Lot_size_q
  #quant[item]
  
  #Initial Stock level
  stock.level <- floor((product_data$number_of_lanes*rack_length)/product_data$b_not_sorting)
  #stock.level[item]
  
  #inventory level
  #inventory.level <- inventory.level[t-1]- dem[t]
  
  continous.mat <- matrix(0.00000,  nrow = 3,ncol = 262)
  
  colnames(continous.mat)<-1:262
  m <- product_data$cycle_time_in_days
  
  #period.mat[3,0]<-  stock.level[1]
  t<- 1
  #for (t in 1:262) {
  while(t<=262){
    if(product_data$Lot_size_q[i]<= stock.level[i]){
      
      if(t==1){
        continous.mat[1,t]<-dem[item]
        continous.mat[2,t]<- quant[item]
        continous.mat[3,t]<- quant[item]-dem[item] #stock.level[1]
        
      }else {
        continous.mat[1,t]<-dem[item]
        if(t %% floor(m[item])==1){
          #b<-t+1
          #print(t+1)
          
          
          
          if(continous.mat[3,t-1]>= continous.mat[1,t-1]){
            continous.mat[2,t]<- quant[item]
          }else{
            continous.mat[2,t]<- quant[item]-(continous.mat[1,t-1]-continous.mat[3,t-1])
          }
          continous.mat[3,t]<- quant[item]-dem[item] #period.mat[3,t-1] + quant[item]-dem[item]
        }else{
          continous.mat[2,t]<- 0
          continous.mat[3,t]<- continous.mat[3,t-1]-dem[item]
        }
      }
      
      
    }else {
      
      
      
      if(t==1){
        continous.mat[1,t]<-dem[item]
        continous.mat[2,t]<- stock.level[item]
        continous.mat[3,t]<- stock.level[item]-dem[item] #stock.level[1]
        
      }else {
        continous.mat[1,t]<-dem[item]
        if(t %% floor(m[item])==1){
          #b<-t+1
          #print(t+1)
          if(continous.mat[3,t-1]>= continous.mat[1,t-1]){
            continous.mat[2,t]<- quant[item]
          }else{
            continous.mat[2,t]<- quant[item]-(continous.mat[1,t-1]-continous.mat[3,t-1])
          }
          # fill the capacity
          continous.mat[3,t]<- stock.level[item]-dem[item] #period.mat[3,t-1] + quant[item]-dem[item]
        }else{
          continous.mat[2,t]<- 0
          continous.mat[3,t]<- continous.mat[3,t-1]-dem[item]
        }
      }
      
      
    }
    
    
    
    
    #}
    
    t=t+1
    
  }
  
  
  return (continous.mat)
  
}

item<- 4
continous.df <- as.data.frame(continous.inventory(item))
row.names(period.df)<- c("yt","qt","lt")

#[,1:11]
kable(continous.df[,1:20],caption = paste("Continous Review Inventory  Policy item",item), row.names = T, escape = F,format = "pandoc")



### Service Level

Ave.phy.inv <-c(vector(),1:62)
alpha.sl <- c(vector(),1:62)
for (i in 1:62) {
  continous.df <- as.data.frame(continous.inventory(i))
  row.names(continous.df)<- c("yt","qt","lt")
  Ave.phy.inv[i]<- sum(continous.df[3,][continous.df[3,]>0])/length(continous.df[3,])
  alpha.sl[i] <- (length(continous.df[3,][continous.df[3,]>0])/length(continous.df[3,])) *100
}

sq.summary <- data.frame(rbind(Ave.phy.inv,alpha.sl))

colnames(sq.summary)<- 1:62
rownames(sq.summary)<-c("Average Inventory","Alpha Service Level")

#[,1:4]
kable(sq.summary[,6],caption = "Continous Inventory SQ Policy",format = "pandoc")


item <- 4
par(family="serif", mar = c(4.25,4.25,.1,.1), bg ="white")
{plot(continous.inventory(item)[3,], xlab="Time", type="l", lwd=2, ylab="demand", cex.lab = 1.5, cex.axis = 1.25)
  abline(h=product_data$Lot_size_q[item]%% product_data$deman_per_day_boxes[item], col="red", lwd=2)
  title(main=paste("item ",item),outer = TRUE)}


kable(round(cycle.time,2), format = "pandoc")



product_data$cycle_time_in_days[1]

product_data$Order_frequency_M


# Analysis

## ABC Analysis

#remember box cost is demand times price
value.share <- product_data$box_cost/sum(product_data$box_cost)
#as.data.frame(value.share)  
#value.share


id.vec <- 1:62

#id.vec.ord contains the indexes of materials in decreasing order.
id.vec.ord <- id.vec[order(value.share, decreasing = T)]

#val.vec.ord contains values of the materials in decreasing order
val.vec.ord <- sort(value.share,decreasing = T)


cum.val.vec.ord <-cumsum(val.vec.ord)
rel.cum.val.vec.ord <- cum.val.vec.ord / sum(val.vec.ord)



class.vec <- rep("A", n)
class.vec[rel.cum.val.vec.ord > 0.8 & rel.cum.val.vec.ord <= 0.95 ] <- "B"
class.vec[rel.cum.val.vec.ord > 0.95 ] <- "C"

tab <- data.frame("ord.material.id" = id.vec.ord, "price (ord.)" = product_data$box_cost[order(value.share, decreasing = T)] , "demand (ord.)" = product_data$deman_per_day_boxes[order(value.share, decreasing = T)], "mat.values" = val.vec.ord, "cum.mat.values" = cum.val.vec.ord, "rel.cum.value.shares" = round(rel.cum.val.vec.ord * 100, 1), "class" = class.vec )

kable(tab, digits = c(0,2,0,2,2,1,0),  caption = "ABC analysis results (values classified by boxes)", format = "pandoc")


# a plot
plot(1:n, rel.cum.val.vec.ord, type="b", xaxt = "n", pch =16, xlab = "material id", ylab = "rel. cum. value share")
axis(1, at = 1:n, labels = id.vec.ord)
abline(h = c(.8,.95), lwd=2, lty=2, col="darkgrey")
text(x = rep(1,3) , y = c(.5,.875,.99), labels = LETTERS[1:3] )



## Cost Analysis

#with no constraint
##paste("Seperate Ordering with no capacity constraint, cost is  €", round(obj.fun(product_data$eoq.max)))#172280

#with no capacity constraint optimized.
#paste("Seperate Ordering with no capacity constraint optimized, cost is  €",round(obj.fun(copt_sol1$solution)))# eoq.min

#with constraint
##paste("Seperate Ordering with capacity constraint, cost is  €",round(obj.fun(copt_sol$solution))) #202280

#JRP

##paste("Joint Ordering with no capacity constraint, cost is  €",round(c.cost.int)," for m")# € 49943


##paste("Joint Ordering with no capacity constraint, cost is  €",round(copt_sol2$objval)," for T")# € 49943


##paste("Joint Ordering with capacity constraint, cost is  €",round(copt_sol3$objval)," for m")# € 49943


cost.matrix <- data.frame("unconstrained"=c(round(obj.fun(product_data$eoq.max)),round(c.cost.int)),
                          "constrained"=c(round(obj.fun(copt_sol$solution)),round(copt_sol3$objval)))
rownames(cost.matrix)=c("Separate Ordering","Joint Ordering")

kable(cost.matrix,digits = c(0,1,1,1),caption = "Total cost Matrix", row.names = T, escape = F,format = "pandoc")



#?seq.int
#Seperate ordering sequence
k<- 0
for (j in 1:62) {
  if(round(product_data$eoq.max[j])>round(copt_sol$solution[j])){
    k<-k+1
    
  }
}

#print(k)




so.mat<- matrix(0,nrow = 62, ncol = 40)

for (i in 1:62) {
  
  so.mat[i,]<- seq.int(round(product_data$eoq.max[i]),round(copt_sol$solution[i]) ,length.out = 40)
  
}

so.mat.df<-data.frame("Items"=1:62,rbind(so.mat))
rownames(so.mat.df)<-paste("part",1:62)
colnames(so.mat.df)<-c("Items",1:40)

kable(so.mat.df[1:5,1:6],caption = "Items and samples",format = "pandoc")


## FOR Separate Ordering

so.df <- data.frame(rbind(round(product_data$eoq.max,0),floor(copt_sol$solution)))

so.df <-cbind(sum=c(sum(product_data$eoq.max),sum(copt_sol$solution)),so.df)
rownames(so.df)<- c("Non Constrained","Constrained")
colnames(so.df)<-c("sum",c(1:62))
kable(so.df[,1:8], row.names = T, escape = F,format = "pandoc")


## JRP

#JRP
# lower cost
#q.vec

# looking for m values unconstrained Jo vs constrained JO.
#unconstrained

df<-df[mixedorder((colnames(df)))]


unconst.m<- unlist(df[1,])
unconst.m

#constrained

const.m<- unlist(df2[1,])

k<-0
for (i in 1:62) {
  if(const.m[i]>=unconst.m[i]){
    k<-k+1
  }
}

print(k)



mvals<- data.frame(rbind(unconst.m,const.m))

mvals<-cbind(sum=c(sum(unconst.m),sum(const.m)), mvals)
colnames(mvals)<- c("sum",1:62)
kable(mvals,format = "pandoc")


unconst.q <- unconst.m*B.int*product_data$demand_per_year

const.q <- product_data$Lot_size_q



k<-0
for (i in 1:62) {
  if(const.q[i]<unconst.q[i]){
    k<-k+1
  }
}

print(k)




qvals<- data.frame(rbind(round(unconst.q),round(const.q)))

qvals<-cbind(sum=c(sum(unconst.q),sum(const.q)), qvals)
colnames(qvals)<- c("sum",1:62)
kable(qvals,format = "pandoc")
