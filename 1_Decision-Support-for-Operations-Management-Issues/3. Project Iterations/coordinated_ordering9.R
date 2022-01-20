---
title: "coordinated_ordering9"
author: "GroupB"
date: "28/07/2020"
output: 
  html_document:
    df_print: paged
    toc: yes
    toc_float: true
    number_sections: false
  pdf_document: default
---



```{r setup, include=TRUE, cache=F}
library(kableExtra)
library(knitr)
```

# Data preparation 

```{r}
library(readxl)

product_data <- read_excel("Data_ordering.xlsx",sheet = "product data")
box_data <- read_excel("Data_ordering.xlsx",sheet = "box data")
#rack data
Total_racks=8
levels_per_rack=4
rack_length= 6000
rack_width=1750
rack_height=300


```


## Convert to Boxes

Convert demand $d_i$, quantity $q_i$ and unit price $p_i$ all from part $i$ to box $bc_i$ which is the capacity of a box for part $i$. 

Demand for box with part $i$ . $$y_i=\frac {d_i*262}{bc_i} $$

Price for box with material $i$ .$$pr_i= bc_i \cdot p_i$$

$y_i = demand\_per\_year \\ ,$
$pr_i = box\_cost=$ value of box



```{r}

product_data$demand_per_year= ceiling((product_data$`demand per day` *262)/ product_data$`pieces/box`) 

product_data$box_cost= product_data$`pieces/box` * product_data$price

product_data



product_data <- merge(product_data, box_data[,c("box ID", "ordering cost (€)")], by = "box ID")

colnames(product_data)[8] <- "ordering_cost"
product_data
```


## Lane Occupation

$b_i=$ sorting column lenght or width.This determines the lane width

$b_i^{-1}=$if sorted by length then the value is width and vice versa, used for rack lenght.

Add $b_i$ and $b_i^{-1}$ to product_data table

```{r}
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
product_data

```

E.g Using box_ID 6203060, sorted by length
$b_i=396$, $b_i^{-1}=297$
Also for material ID 7305667+74 with $q_i=214$ therefore,
$rack_{length}=6000 \\$

*1.* Number of boxes in a lane:

$$n_i=\frac {rack_{length}}{b_i^{-1}}=\frac {6000}{297}=20.20=20boxes$$





*2.* How many lanes part $i$ will occupy if you order some number of boxes

$$lane_i=\frac {q_i}{n_i}\\lane_i=q_i \cdot \frac {b_i^{-1}}{rack_{length}}=214 \cdot \frac {297}{6000}=10.593= 11 \space lanes $$

$$lane_i= \left\lceil\frac{b^{-1}_i \cdot q_i}{rl} \right \rceil$$

```{r}
lane <- ceiling(product_data$eoq * (product_data$b_not_sorting/rack_length))
lane
```

(C) Do we meet the capacity contraint?

*3.* Total number of lanes constraints

Collapsing Rack 4 levels in a rack and joining the 8 racks to become 1 level.

Total rack width available: 

$$rack_{total_{Width}}=(rack_{width} \times 4 \times 8)$$



```{r}
rack_total_width <-51200  #rack_width * 4 * 8
rack_total_width #56000
```


```{r}
lane.min <- ceiling(product_data$eoq.min * product_data$b_not_sorting / rack_length)
lane.max <- ceiling(product_data$eoq.max * product_data$b_not_sorting / rack_length)

# total rack width 

rack_total_width <- 51200 # rack_width * 4 * 8

```

Overflow in mm

$$[\sum_{i=1}^n lane_i \cdot b_i]- rack_{{total}_{width}}$$
relative overflow in %

$$\frac{[\sum_{i=1}^n lane_i \cdot b_i]- rack_{{total}_{width}}} {rack_{{total}_{width}} } \cdot 100$$

```{r}
# overflow in mm
sum(lane.min*product_data$b_sorting) - rack_total_width
sum(lane.max*product_data$b_sorting) - rack_total_width
# relative overflow in %
(sum(lane.min*product_data$b_sorting) - rack_total_width)/rack_total_width*100
(sum(lane.max*product_data$b_sorting) - rack_total_width)/rack_total_width*100
```




Please note the coefficients are  are different for every ith item except racklength which is the same for all items.Also the left and right hand side of the equation need to be in mm.
$$\sum_{i=1}^{n=62}lane_i \cdot b_i\le rack_{Total_{width}} \\ \sum_{i=1}^{n=62} q_i \cdot \frac {b_i^{-1} \cdot b_i}{rack_{length}} \le rack_{Total_{width}} \quad \quad (1)$$

```{r}
#get the with of the lanes in mm
rack_width_occupied=sum(lane.min*product_data$b_sorting)
rack_width_occupied
if(rack_width_occupied <= rack_total_width){
  print(paste0("Capacity constraint fulfilled: ", rack_width_occupied, "<=",rack_total_width))
 
}else {
  violated <- rack_width_occupied - rack_total_width
  print(paste0("Capacity constraint violated by: ",violated ))
}

```
As seen above contraint was voilated by $1387664 mm$, this means that we ordered too much, therefore we need to optimize $q_i$


Using our example to get number of the total width i.e (summation of lanes) in $mm$: 
Summation of lanes can simply be: 
$$lanes_i \cdot b_i= 10.593 \cdot 396=4194.828$$
Using equation (1) to prove this concept.

$$214 \cdot \frac {396 \cdot 297}{6000}=4194.828 \\ b_i= \frac {4194.828}{10.593}=396$$

(D) Adjust Q by reducing it:

$\frac {y_i}{q_i}=$ no of orders for part $i \\$
$\frac {q_i}{2}=$ average inventory for part $i$


# Separate Ordering (SO)

$c_i^{or}=$ ordering cost for part $i ,\space$

$c_i^{sh}=$ stock holding cost rate based on unit price $p_i$ and interest rate $h, \qquad$.
$c_i^{sh}=pr_i \cdot h$

calculate EOQ for each part i:
$$ q_i^* = \sqrt \frac {2 \cdot y_i \cdot (c_i^{or}+c^{-or})} {pr_i \cdot h}$$

Min EOQ, used in starting feasible solution.
$$ q_i^* = \sqrt \frac {2 \cdot y_i \cdot (c_i^{or})} {pr_i \cdot h}$$



## EOQ modelling


```{r}
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
product_data
```


## Constrained EOQ optimization

$$ min \rightarrow (\sum_{i=1}^{n=62}c^{-or} + \sum_{i=1}^{n=62} \frac {y_i}{q_i} \cdot c_i^{or}) + (h \cdot \sum_{i=1}^{n=62} \frac {q_i}{2}\cdot pr_i ) $$
Subject to:
$$\sum_{i=1}^{n=62} q_i \cdot \frac {b_i^{-1} \cdot b_i}{rack_{length}} \le rack_{Total_{width}} $$



```{r}
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

```



```{r}
qopt <- OP(
  objective = F_objective(F=obj.fun ,n=n),
  types = rep("C",n),
  bounds = V_bound(ub= product_data$eoq.min , lb= rep(1, n)),
  constraints = F_constraint(F=const.fun,
                             dir="<=",
                             rhs = rack_total_width)
)

#This shows that minimum EOQ is too big and therefore will not meet the rack space constraint.
const.fun(min(product_data$eoq.max))#366272
const.fun(min(product_data$eoq.max)/8)# 61428  still > 56000
const.fun(min(product_data$eoq.max)/8.7)#52716 < 56000
const.fun(min(product_data$eoq.max)/10)# 33098 < 56000 is much lower but this can still be the starting value
round(min(product_data$eoq.max)/10) #15

```



```{r}
copt_sol <- ROI_solve(qopt, start = rep(min(product_data$eoq.max)/10,n), solver = "alabama" )
# always check whether the algorithm converged
copt_sol# The objective value is: 101625.9
# solution
copt_sol$solution #vector of optimal Quantity that meets the space constraints and minimizes the Obj function.
round(copt_sol$solution)
copt_sol$objval #101625.9



const.fun(copt_sol$solution)#55884
const.fun(floor(copt_sol$solution))#55092


obj.fun(copt_sol$solution)#101625.9
obj.fun(floor(copt_sol$solution))#104639.7  rounded q values
```


removing waste of 150 for every 32 levels

## No constraint

```{r}

rack_total_width <- 51200
qopt1 <- OP(
  objective = F_objective(F=obj.fun ,n=n),
  types = rep("C",n),
  bounds = V_bound(ub= product_data$eoq.min , lb= rep(1, n)),
)

copt_sol1 <- ROI_solve(qopt, start = rep(min(product_data$eoq.max)/10,n), solver = "alabama" )
# always check whether the algorithm converged
copt_sol1$objval
```




## Compare results

Remember, $1500*62$  was dropped from the objective function as it is not decision relevant.

```{r}
obj.fun <- function(q, d= dei, c.or = cori, c.h = h*box_cost  ) (1500*62)+(sum((dei/q)*c.or)+ sum(c.h*(q/2)))
obj.fun(product_data$eoq.max)#172279.7
```



```{r}

const.fun(copt_sol$solution)#55884
const.fun(floor(copt_sol$solution))#55092  we rounded down to avoid exceeding capacity


obj.fun(copt_sol$solution)#194625.9
obj.fun(floor(copt_sol$solution))#197639.7 rounded q values


```

Optimum costs with no capacity constraint considers is €172279.7 with capacity constraint, it costs €194625.9 This cost is expected to be higher when capacity constraint is included, as limited capacity will not allow to take advantage of cost savings.

```{r}
# demand vs quantity

product_data$`demand per day`
product_data$demand_per_year  #


```


```{r}
floor(copt_sol$solution)
```

Plot EOQ cost vs cost

cost function per item
$$ C(q_i) \rightarrow (c^{-or} +  \frac {y_i}{q_i} \cdot c_i^{or}) + (h \cdot  \frac {q_i}{2}\cdot pr_i ) $$

```{r}
#EOQ cost

#with no constraint
paste("with no capacity constraint, cost is  €", round(obj.fun(product_data$eoq.max)))#173539.2

#with constraint
paste("with capacity constraint, cost is  €",round(obj.fun(copt_sol$solution))) #202866.4

paste("It costs €",round(obj.fun(copt_sol$solution))-round(obj.fun(product_data$eoq.max))," extra due to lack of capacity")

```
The above result shows that If there is no capacity constraint, the cost will be lower. 


```{r}
x.val <- seq.int(1,62, length.out = 62)

cost.fun <- function(q, d= dei, c.or = cori, c.h = h*box_cost  ){
  (1500 +(dei/q)*c.or)+ (c.h*(q/2))
} 

y.vec.no.const <- cost.fun(q=product_data$eoq.max)
y.vec.yes.const<- cost.fun(copt_sol$solution)

paste(length(which((round(y.vec.yes.const)>round(y.vec.no.const))==TRUE))," Items costs more when capacity becomes a problem")


```


```{r}
#par(mfrow=c(1,1))
{ 
  plot(x.val,round(y.vec.no.const), xlab ="lot size q", ylab = "total cost", type = "p", lwd =2, ylim=c(12,max(y.vec.no.const)), xaxp=c(1, 10, 1) )
  axis(1, 1:62)
axis(2, 1:62)
abline(h=1:62, v=1:62, col="gray", lty=3)

points(x=x.val,y=round(y.vec.yes.const),lwd=2, col="blue")

legend("bottomleft",lty = c(1,1),col = c("black","blue"), legend = c("No constraint","with constraint"), bty="n" , horiz = T)
}



```

* Notice that 34 blue dots which represents the more expensive items when capacity is accounted for.

* Also the notice , for most items on blue larger than black (non constraint items) the difference is much larger. the reverse is the case when black is larger than blue.


# Joint Ordering (JO)

Using Joint replenishment problem

Assumptions:

- One supplier with outbound storage.
- $i=\{1,...,n\}$ products
- Demand rates: $de_i$
- Stock_holding cost rates: $c_i^{sh}$
- Specific setup costs: $c_i^{or}$
- General setup costs: $c^{-or}$
- Cycle time of product $i$: $T_i$


```{r}
n <- length(product_data$`box ID`)
c.or0 <- cord
c.or <- product_data$ordering_cost
#H.vec <- 0.5 * dei * c.sh
# based on the previous definition, I think this should be the vector of holding cost multipliers
H.vec <- 0.1 * product_data$box_cost * dei * 0.5
```


## Objective Function with no capacity constraint

- $B=$basic cycle time
- $H_i=0.5 \cdot y_i \cdot c_i^{sh}$
- $H_0=0$  Holding cost multiplier are $H_0 \space and  \space H_i$
- $$C(m_i B)=\sum_{i=0}^n (\frac {c_i^{or}}{m_i \cdot B}+H_i \cdot m_i \cdot B) == \frac {c_o^{or}}{B}+ \sum_{i=1}^n (\frac {c_i^{or}}{m_i \cdot B}+H_i \cdot m_i \cdot B)  \\ subject \space to: \quad \quad \quad \quad \quad \quad \quad \quad \quad  \\ m_i \ge m_0 \quad \forall i >0 \\m_i \in \mathbb N \qquad \forall i   $$

--> an important side note is that the JRP is basically a quite standard EOQ problem which uses the substitution $T_i = \frac{q_i}{y_i}$, i.e., $q_i = T_i \cdot y_i$. Then, you substitute $T_i = m_i \cdot B$ ans result in the JRP (adding the common ordering cost).

```{r}
jrp.obj.fun <- function(m , B, cor, Hvec, cor0) cor0/B + sum(cor/B/m) + sum(Hvec*B*m)
```



- *Cycle Time*

we determine $T_i=\sqrt (\frac {c_i^{or}}{H_i})$ and $T^C=\frac {\sum_{j=0}^{i´} c_j^{or}}{\sum_{j=0}^{i´}H_j}$
```{r}
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
kable(costs.cycle ,"pandoc", row.names = T)
```


```{r}
library(kableExtra)
# calculate T^2 and cumu. cost shares
res.mat <- t(cbind(c.or/H.vec,(c.or0 + cumsum(c.or))/cumsum(H.vec)))
df <- data.frame(res.mat)
colnames(df) <- reo.id 
rownames(df) <- c(  "$T_i^2$" ,"$T^C$"  )
kable(df,"pandoc", row.names = T)
```


$$B=\sqrt \frac {\sum_{i=0}^n \frac {c_i^{or}}{m_i}}{\sum_{i=0}^n m_i \cdot H_i} $$


```{r}
# identify break
which(res.mat[1,] > res.mat[2,])# break occured after 13
id.comb <- min(which(res.mat[1,] > res.mat[2,])) - 1 #3
id.comb
# calculate B
B <- min(T.vec) #0.01955164
# solution with m - integers #######################
m.vec.int <- round(T.vec/B,0)

m.vec.int[1:id.comb] <- 1  
# re-optimize B for fixed m.vec
B.int <- sqrt(sum(c.or/m.vec.int)/sum(m.vec.int*H.vec))#0.02014868
# total cost 
c.cost.int <- jrp.obj.fun( m = m.vec.int, B=B.int, cor = c.or,Hvec=H.vec, cor0 = c.or0)#192606
df <- data.frame(rbind(round(T.vec/B,2), round(T.vec/B), m.vec.int))
colnames(df) <- reo.id 
rownames(df) <- c("$m_i=\\frac{T_i}{B}$" ,"$[m_i]$", "$[\\tilde{m}_i]$" )
kable(df,"pandoc", row.names = T)
```

Reoptimizing basic cycle time yields $B=$  $`r round(B.int,2)`$ and the total cost are `r round(c.cost.int,2)` 

The order quantities are given by multiplying the cycle times $T_i$ with demand rates $y_i$, i.e. $q_i = T_i \cdot y_i$ such that

```{r}
dei <- dei[reo.id]
df <- data.frame(rbind( m.vec.int, round(m.vec.int*B, 2), round(m.vec.int*B*dei, 2) ))
colnames(df) <- reo.id 
rownames(df) <- c("$[\\tilde{m}_i]$", "$T_i$", "$q_i$" )
kable(df,"pandoc", row.names = T)
```

--> Now the question is: Is this feasible? --> no:

```{r}
q.vec <- m.vec.int*B*dei
const.fun(q.vec) > rack_total_width
```


## Including capacity constraint


When you try to optimize the JRP directly, without introducing the substitution $T_i = m_i \cdot B$ you need to update the JRP function as follows

```{r}
library(ROI)
jrp.obj.fun2 <- function(Tvec, cor, Hvec, cor0) sum(cor0/Tvec) + sum(cor/Tvec) + sum(Hvec*Tvec)
# you need to initialize the parameters in the objective function --> Tvec is to be optimized over, the rest is given
jrp.obj.fun2 <- function(Tvec, cor=c.or, Hvec = H.vec, cor0 = c.or0) sum(cor0/Tvec) + sum(cor/Tvec) + sum(Hvec*Tvec)

qopt2 <- OP(
  objective = F_objective(F=jrp.obj.fun2 , n=62),
  types = rep("C",n),
  bounds = V_bound(ub= rep(30, 62), lb= rep(.001,62))
) 

jrp.obj.fun2(m.vec.int*B)
jrp.obj.fun2(rep(.00001,62))
jrp.obj.fun2(rep(15,62))
# you don't need Alabama here as there is no constraint. Basically you can also optimize this problem by taking derivatives
copt_sol2 <- ROI_solve(qopt2, start = m.vec.int*B)
copt_sol2

copt_sol2$solution

```

```{r}
m.vec.int*B*dei
```


--> You should  integrate the shelf capacity constraint 
--> I also recommmend to stick with the basic period approach

This is formally defined as 
$$ \sum_{i=1}^n b_i \cdot \left\lceil\frac{b^{-1}_i \cdot q_i}{rl} \right \rceil \leq \text{tot_rack_length}$$
Thus, with the substitution above, the left-hand side changes to
$$ \sum_{i=1}^n b_i \cdot \left\lceil\frac{b^{-1}_i \cdot T_i \cdot y_i}{rl} \right \rceil = \sum_{i=1}^n b_i \cdot \left\lceil\frac{b^{-1}_i \cdot m_i \cdot B \cdot y_i}{rl} \right \rceil$$



Thus, you can change the constraint function quite easily

```{r}

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
```

Then, you can use the original JRP function and update the model by including the capacity constraint. Therefore I recommend that you fix $B$ to some appropriate value based on the feasible solution above

$rack_{Totalwidth}=51200$

```{r}

#rack_total_width<-51200

#library(ROI.plugin.deoptim)

T.feas <- copt_sol.solution/dei
B.start <- min(T.feas)

jrp.obj.fun <- function(m , B = B.start, cor = c.or, Hvec = H.vec, cor0 = c.or0) cor0/B + sum(cor/B/m) + sum(Hvec*B*m)
jrp.obj.fun(m= rep(1, 62))
const.fun2(m= rep(1, 62))

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

copt_sol3

copt_sol3$solution

jrp.obj.fun(m= copt_sol3$solution)
const.fun2(m= copt_sol3$solution) <= rack_total_width

```


```{r}
# not feasible
const.fun2(round(copt_sol3$solution)) <= rack_total_width
# feasible
const.fun2(floor(copt_sol3$solution)) <= rack_total_width
# potential starting solution
m.start <- floor(copt_sol3$solution) # multiplier m
q.start <- ceiling(m.start * B.start * dei)   # order quantity q (in boxes, rounded up)
q.start

```


```{r}
#Order according to m in ascending order
#reo.id <- order(m.start)
#dei <- dei[reo.id]
#m.start <- m.start[reo.id]

df2 <- data.frame(rbind( m.start, round(m.start*B.start, 2), ceiling(m.start*B.start*dei) ))
colnames(df2) <- reo.id 
rownames(df2) <- c("$[\\tilde{m}_i]$", "$T_i$", "$q_i$" )
kable(df2,"pandoc", row.names = T)
```


```{r}

library(gtools)
df2<-df2[mixedorder(colnames(df2))]
df2
```


```{r}

q.start<- as.vector(df2[3,1:62])
q.start
```


Afterwards, you have to find a layout scheme such that a precise shelf layout results.

As you rightly point out you need to calculate the number of lanes $l(q_i)$ required for each product given a certain order quantity $q_i$ (integer number of boxes):

$$l(q_i) =\left\lceil \frac{q_i}{n_i} \right\rceil$$
whereby $n_i$ is the number of boxes per lane dedicated to product $i$. I.e., $$n_i = \left\lfloor \frac{rl}{b_i^{-1}} \right\rfloor$$. Thus, for the solution of the JRP we have:

```{r}
l.start <- ceiling(q.start/floor(rack_length/product_data$b_not_sorting))
rownames(l.start) <- "lanes"
l.start
```



Now, these lanes have to be assigned to the levels of the shelves. Therefore, we first need to determine the types of lanes required. The lanes are just described by their width. Due to the safety margins we should round the lane width to full centimeter. I.e., we need to assign lanes with the following widths

```{r}
unique(round(product_data$b_sorting/100)*100)
```
Now comes the tricky part: We have to decide how many lanes of a certain width should be assigned for each level. Luckily, there is only a small number of useful patterns of lanes per level. To be precise there are 9 efficient patterns to arrange these 3 lane types (assuming we use each level exhaustively):

```{r}
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

patterns
```


Let $p_{k,j}$ indicate the number of lanes of type $j$ associated to pattern $k$. Now we need to assess the number of lanes per required of each type. As outlined above, we know the number of lanes per product $l(q_i)$, thus we can deduce the demand for lane type $j$ ($ld_j$) by summing up the $l(q_i)$  for each lane type,  i.e., $ld_j = \sum_{i \in P|b_i=j} l(q_i)$:

```{r}
# assign lane width as names
names(l.start) <- round(product_data$b_sorting/100)*100 

# number of items with lane types 200, 400, 600 that is demand for each lane type
ld <- c(sum(l.start[names(l.start) == "200"]),
sum(l.start[names(l.start) == "400"]),
sum(l.start[names(l.start) == "600"]))
ld
```




Need to reduce 70 units to 64


## Illustration

```{r}

#m.start

product_data$Order_frequency_M<- unlist(df2[1,1:62])
product_data$Lot_size_q <- unlist(q.start)
product_data

#m.start <- sort(unique(product_data$Order_frequency_M))
m.start <- product_data$Order_frequency_M
sort(unique(m.start)) # 21 unique values meaning 22 columns

max(m.start)

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
cycle.df

rownames(cycle.df)<-1:nrow(cycle.df)

kable(cycle.df,"html", row.names = T, escape = F) %>%
  kable_styling("striped")
```

*  *Demand vs Quantity*

$bc_i$ which is the capacity of a box for part $i$. 
$y_i=$ Demand in Years for item $i$
$yb_i=$ Demand per day in boxes for item $i$.
$yd_i=$ Demand per day in items for item $i$
$yb_i= \frac {yd_i}{bc_i} $

$ct =$cycle time is number of days it takes before a new order.

E.g Item 4 has lot size quantity of 18 boxes, and daily demand in boxes is 1.2
$$ct= \frac {q}{yb_i}= \frac  {m_i \cdot B \cdot y_i}{yb_i} \\ ct= \frac {18}{1.2}=15days$$
The example above shows that for item 4, it takes about 15 day to make another order.

e.g assuming we order 17 items and demand pay day in boxes is 1.6667  this mean it will take about 10 day to make another other.

```{r}
# Demand per day in boxes
#its still okay to leave the values in fraction as we are not ordering now.
product_data$deman_per_day_boxes <- product_data$`demand per day`/product_data$`pieces/box`

#This is number of days it takes before a new order "product_data$Lot_size_q" is placed
product_data$cycle_time_in_days <- (m.start*B.start*product_data$demand_per_year)/product_data$deman_per_day_boxes
```

*  *Convert Cycles to days*
```{r}
#min(product_data$cycle_time_in_days)#2.472104 days is equivalent to M=1
#sort(sort(unique(product_data$Order_frequency_M)))
#sort(unique(product_data$Order_frequency_M))*min(product_data$cycle_time_in_days)



cycle.time <- data.frame(rbind(sort(unique(product_data$Order_frequency_M))*min(product_data$cycle_time_in_days)   ))

colnames(cycle.time) <- sort(unique(product_data$Order_frequency_M))
rownames(cycle.time) <- c("Days M." )

kable(cycle.time,"html", row.names = T, escape = F) %>%
  kable_styling("striped")
```


* *Calculate demand after first cycle*

Assuming that demand All Quantity has been supplied for all items in the first cycle

reduce lane assignment while fulfilling demand

```{r}
#cycle_demand== Lot_size_q
#cycle_demand <- product_data$cycle_time_in_days*product_data$deman_per_day_boxes

#l.start2 <- (cycle_demand *product_data$b_not_sorting)/rack_length
l.start2 <- ceiling(l.start)
l.start2[5]<-1
l.start2[10]<- 2
l.start2[11]<-2
l.start2[12]<- 2
l.start2[20]<- 1
l.start2[30]<-1

#l.start2 <- ceiling(l.start)
vec<-unlist(l.start2)

product_data$number_of_lanes <- vec

l.start2


```

```{r}
# assign lane width as names
names(l.start2) <- round(product_data$b_sorting/100)*100 

# number of items with lane types 200, 400, 600 that is demand for each lane type
ld <- c(sum(l.start2[names(l.start2) == "200"]),
sum(l.start2[names(l.start2) == "400"]),
sum(l.start2[names(l.start2) == "600"]))
ld
```


*  *Prove that demand is always met*


Using (T,S) policy

We are using periodic review as order intervals can be derived from period order frequency $m$ for each of the $i^{th}$ items

* Assuming demand is uniformly distributed thus the same quantity of demand repeats each time.

* Initial stock level for the 62 items are given.

* Each Item has some form of order frequency m going over a time period of 262 days.

* Lead time is zero.

* No Backorder

* Stock is filled to the capacity in period 1.


*Notations and Formulars*

On-hand stock S(t)

Outstanding orders O(t)

Backorders B(t)

Inventory level I(t)= S(t) - B(t): Potentially available units

Inventory position P(t) = I(t) + O(t): basis for ordering decisions.


for item 30, we changed the cycle time in days to 39 instead of 42.
```{r}
#item 39
product_data$cycle_time_in_days[30]<- 39
```


In the code below, Shows (T,S) Policy, how demand is met over 262 time period.
Reason for TS policy is that we can not exceed capacity, thus we order upto Lane capacity of each item. Also each item has its cycle time in days which was derived from order frequency M.



```{r}
#install.packages("writexl")
library("writexl")

#write_xlsx(product_data,"C:\\Users\\...\\product_data.xlsx")

```

```{r}
Periodic.inventory <-function(item){
  
  #Yt demand
dem <- product_data$deman_per_day_boxes
dem[item]

#qt quantity
quant <- product_data$Lot_size_q
quant[item]

#Initial Stock level
stock.level <- (product_data$number_of_lanes*rack_length)/product_data$b_not_sorting
stock.level[item]

#inventory level
#inventory.level <- inventory.level[t-1]- dem[t]

period.mat <- matrix(0.00000,  nrow = 3,ncol = 262)

colnames(period.mat)<-1:262
m <- product_data$cycle_time_in_days

#period.mat[3,0]<-  stock.level[1]
t<- 1
#for (t in 1:262) {
while(t<=262){
  if(item==12){
    
    if(t==1){
    period.mat[1,t]<-dem[item]
    #period.mat[2,t]<- quant[item]
    period.mat[3,t]<- stock.level[item] #stock.level[1]
    
  }else {
    period.mat[1,t]<-dem[item]
    if(t %% floor(m[item])==0){
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
    
    
  }else if(item== 30){
    
    
    if(t==1){
    period.mat[1,t]<-dem[item]
    #period.mat[2,t]<- quant[item]
    period.mat[3,t]<- stock.level[item] #stock.level[1]
    
  }else {
    period.mat[1,t]<-dem[item]
    if(t %% floor(m[item])==0){
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
    
    
  }else {
    
    
    if(t==1){
    period.mat[1,t]<-dem[item]
    #period.mat[2,t]<- quant[item]
    period.mat[3,t]<- stock.level[item] #stock.level[1]
    
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
  
  
}

 t=t+1
    
  }
  
  
  row.names(period.mat)<- c("yt","qt","lt")  
  return (period.mat)
  
}

as.data.frame(Periodic.inventory(30))
```



## Changes made on q

1. We assumed that there are 262 working days for the year 2020 according to https://hr.uiowa.edu/pay/payroll-services/payroll-calendars/working-day-payroll-calendar-2020 


2. rack_total_width which was  56,000 mm, looking at the 9 patterns all of which have 150 mm in waste, meaning  all 32 levels will have 150 mm waste each. Therefor $56,000 - (150 *32) = 51,200$ thereby changing rack total width 51,200.

3. These changes then affects the values of q  for all the 62 items involved.


To fulfill lane demands from the items $ld$ are 15 lanes with with 200, 15 lanes with width 400 and 64 lanes with width 600. To fulfull this demand, only 2 patterns are needed.

```{r}
patterns[3:4,]
```
Assign Patterns

```{r}

fitted.pattern <- patterns[3:4,]
# loop through 32 levels to assign patterns

levels.pattern.mat <- matrix(0,  nrow = 32,ncol = 3)

for (i in 1:32) {
  for (j in 1:3) {
    
  if(i<= 16){
    levels.pattern.mat[i,j] <- t(fitted.pattern[1,j])
  } else {
     levels.pattern.mat[i,j] <- t(fitted.pattern[2,j])
   }
  }
}


levels.pattern.mat

# confirm if lane demand is met.

sum(levels.pattern.mat[1:32,1]) >= sum(ld[1]) #TRUE  for 200
sum(levels.pattern.mat[1:32,2]) >= sum(ld[2]) #TRUE  for 400
sum(levels.pattern.mat[1:32,3]) >= sum(ld[3]) #TRUE  for 600

```
