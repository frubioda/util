function bnu,lambda,t ; calculates bnu(lambda,t), lambda in angstrom
;+
;   bnu(lambda,t)
;
;            calculates bnu(lambda,t), lambda in angstrom.
;
;-
;  88-04-25  mats carlsson
;
c=2.99792458e10
h=6.626176e-27
k=1.380662e-16
l=lambda*1.e-8
 
return, 2.*h*c/l/l/l/(exp(h*c/l/k/t)-1.)
end
