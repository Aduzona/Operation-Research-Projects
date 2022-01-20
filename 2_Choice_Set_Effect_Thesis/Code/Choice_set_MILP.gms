Set
m choice cardinality /1*5/
b
k
z;

*alias(m,mm);
alias(k,kk);
alias(z,zz);

Parameter
v(b,z,k)
r(b,z,k)
vmax
nc(b)
beta_pi
beta_m
ZC
*/1 1,2 2, 3 3, 4 4, 5 5, 6 6,7 7,8 8,9 9,10 10/
mm(m)  /1 1,2 2, 3 3, 4 4, 5 5/
Ratio(b,z,k,m)
;


Variable
F;

binary variable
Y(z,k,m);


positive variable
X(b,z,k,m)
ZX(b);

Equation
ZF;
ZF.. F =e= sum((b,z,k), r(b,z,k)* sum( (m),X(b,z,k,m)));


Equation
CC;
CC(b)..  ZX(b) + sum((zz,kk,m), X(b,zz,kk,m)) =l=1;


Equation
LC;
LC(b,z,k,m).. X(b,z,k,m)=l= Y(z,k,m);

Equation
CP;
CP(b,z,k,m)..  X(b,z,k,m) =l= Ratio(b,z,k,m) * ZX(b);

Equation
SK;
SK(k).. sum((z,m), Y(z,k,m)) =l= 1;

Equation
SZ;
SZ(z).. sum((k,m), Y(z,k,m)) =l= 1;

Equation
Max;
Max.. sum((z,k,m), Y(z,k,m)) =l= ZC;


Equation
NAC;
NAC(z,k)..  sum(m, mm(m)*Y(z,k,m)) =l= ZC;


model contract_mip /all/;

** SOLVE AND OUT LOOP PARAMETER**
parameter
mv          v(bc) scaled with mu
mnc         nc(b) scaled with mu
noc         numer of contracts
muval       value of scale parameter
Fval        Objective Value
yval        Which contracts are offered per iteration
xnoval      no choice probability
xval        choice prob buyer
help        help
incr;



$onecho > contracts.txt
dset=b rng=buyer!a2 rdim=1
dset=k rng=capacity!a2 rdim=1
dset=z rng=sidepayment!a2 rdim=1
par=v  rng=utility!a2 rdim=3
par=r  rng=revenue!a2 rdim=3
$offecho

$call GDXXRW  contract_set1.xlsx trace=3 @contracts.txt
$GDXIN contract_set1.gdx


$loaddc b, k , z, v, r
vMax   = smax((b,z,k),abs(v(b,z,k)));
nc(b)  = 0;
v(b,z,k) = v(b,z,k);
r(b,z,k) = r(b,z,k) / 2;


option optcr =0;
option mip=cplex;

beta_pi = 1;
beta_m=1;

ZC=card(m);




mv(b,z,k)      = beta_pi*v(b,z,k)- (beta_m* ZC);
mnc(b)         = (beta_pi*nc(b)) ;
Ratio(b,z,k,m)= exp(mv(b,z,k)- mnc(b) );
*    P(b,z,k)= exp(mv(b,z,k)) / (exp(mv(b,z,k)) + exp(mnc(b) ) );

    
** offer only contracts with positive profit for buyer and supplier
solve contract_mip maximizing F using mip;



display F.l, y.l, X.l,ZX.l,v,r, vmax;

















