** CONTRACT MODEL**


Set
b,k,z;

alias(k,kk);
alias(z,zz);

Parameter
v(b,z,k)
r(b,z,k)
vmax
nc(b)
beta_pi
beta_m
m;

Variable
F;

binary variable
Y(z,k);


positive variable
X(b,z,k);

Equation
ZF;
ZF.. F =e= sum((b,z,k), X(b,z,k)*r(b,z,k));


Equation
CP;
CP(b,z,k).. X(b,z,k) =e= exp((beta_pi*v(b,z,k))-(beta_m*m))*Y(z,k) / ( sum((zz,kk), exp((beta_pi*v(b,zz,kk))-(beta_m*m))*Y(zz,kk)) + exp(beta_pi*nc(b)) );


Equation
SK;
SK(k).. sum(z, Y(z,k)) =l= 1;

Equation
SZ;
SZ(z).. sum(k, Y(z,k)) =l= 1;

Equation
Max;
Max.. sum((z,k), Y(z,k)) =l= m;


model contract_nlp /all/;


** SOLVE AND OUT LOOP PARAMETER**
parameter
mv          v(bc) scaled with mu
mnc         nc(b) scaled with mu
noc         numer of contracts
muval       value of choice_set size
Fval        Objective Value
yval        Which contracts are offered per iteration
xnoval      no choice probability
xval        choice prob buyer
bval        choosen buyer value
sval        choosen supplier profit
help        help
incr;


$onecho > contracts.txt
dset=b rng=buyer!a2 rdim=1
dset=k rng=capacity!a2 rdim=1
dset=z rng=sidepayment!a2 rdim=1
par=v  rng=utility!a2 rdim=3
par=r  rng=revenue!a2 rdim=3
$offecho

*$call GDXXRW  Input_data_Ozer_and_Wei_2006_solution.xlsx  index = index!a1

$call GDXXRW  contract_set1.xlsx trace=3 @contracts.txt

$GDXIN contract_set1.gdx

*$CALL GDXXRW  contract_set1.xlsx  index = index!a1
*$gdxin contract_set1.gdx



$loaddc b, k , z, v, r
vMax   = smax((b,z,k),abs(v(b,z,k)));
nc(b)  = 0 ;
v(b,z,k) = v(b,z,k);
r(b,z,k) = r(b,z,k) / 2;


** offer only contracts with positive profit for buyer and supplier
*y.fx(z,k)$(v('b1',z,k)<=0 or v('b2',z,k)<=0)=0;
*y.fx(z,k)$(r('b1',z,k)<=0 or r('b2',z,k)<=0)=0;



option optcr =0;
option minlp=bonmin;
*sbb
set l_beta_m /1*1/;
set l_m /1*1/;

incr=1;
beta_pi = 1;
beta_m=1;
m=1;



loop(l_m ,
    loop(l_beta_m,
    



*    mv(b,z,k)      = beta_pi*v(b,z,k)-beta_m*m;
*    mnc(b)         = beta_pi*nc(b);

    
** offer only contracts with positive profit for buyer and supplier
    solve contract_nlp maximizing F using MINLP;

*generate output Data 
   
*    mv(l_m,l_beta_m,'b1',z,k)$(x.l('b1',z,k)>0.01)                    = v('b1',z,k);
*    mv(l_m,l_beta_m,'b2',z,k)$(x.l('b2',z,k)>0.01)                    = v('b2',z,k);
    muval(l_m,l_beta_m)                                               = m;
    xval(l_m,l_beta_m,b,z,k)                                          = eps;
    xval(l_m,l_beta_m,'b1',z,k)$(x.l('b1',z,k)>0.01)                  = x.l('b1',z,k);
    xval(l_m,l_beta_m,'b2',z,k)$(x.l('b2',z,k)>0.01)                  = x.l('b2',z,k);
*    bval(l_m,l_beta_m,z,k)$(x.l('b1',z,k)>0.01 or x.l('b2',z,k)>0.01) = v(b,z,k);
   yval(l_m,l_beta_m,z,k)$(x.l('b1',z,k)>0.01 or x.l('b2',z,k)>0.01) = y.l(z,k);
    noc(l_m,l_beta_m)                                                 = sum((z,k), yval(l_m,l_beta_m,z,k));
    noc(l_m,l_beta_m)$(noc(l_m,l_beta_m)=0)                                     = eps;
    Fval(l_m,l_beta_m)                                                = eps;  
    Fval(l_m,l_beta_m)$(F.l>0)                                        = sum((b,z,k), r(b,z,k) * x.l(b,z,k));
 
    beta_m                                                      =beta_m+incr;
   
*m                                                       =m+incr;
    
    );
   beta_m =                                                   1;
    m                                                    = m + incr;

);

execute_unload "contract_set1.gdx";

*Output Data
execute 'gdxxrw.exe contract_set1.gdx o=New_irrational_12_contract1.xlsx EpsOut=0 par=Fval.L rng=Fval!A1 rdim=2 cdim=0 par=noc.L rng=numbercontracts!A1 rdim=2 cdim=0 par=muval.l rng=Mu!A2 rdim=2 cdim=0 par=xval.l rng=Probability!A1 rdim=5 cdim=0 par=yval.l rng=chosen_contract!A1 rdim=4 cdim=0 '

* par=mv.l rng=buyer_profit!A1 rdim=2 cdim=0

display F.l, y.l, X.l,v,r;


