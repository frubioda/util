FUNCTION ferret, x, y
;
;+
; NAME:
;      FERRET
; PURPOSE:
;       Calculates the Ferret diameter of a structure
; CALLING SEQUENCE:
;       Result = ferret(x,y)
; INPUTS:
;       x = vector containing x-values
;       y = vector containing y-values
; OUTPUTS:
;       Result = 2 elements array 
;        [0] = Ferret diameter
;        [1] = max. diameter perpendicular to Ferret axis
; COMMON BLOCKS:
;       none       
; SIDE EFFECTS:
;       
; RESTRICTIONS:
;             
; PROCEDURE:
;             
; MODIFICATION HISTORY:
;       20-Nov-00 J. Hirzberger, IGAM
;-
;

; on_error, 2

nx = n_elements(x)
ny = n_elements(y)

IF nx NE ny THEN MESSAGE,'X and Y have different number of elements'

dsup=0
isup=0
jsup=0

for i=0,nx-1 do begin
    for j=i+1,nx-1 do begin
;
        dist=sqrt((x(i)-x(j))^2. + (y(i)-y(j))^2)
;
        if dist gt dsup then begin
           dsup=rfix(dist)
           isup=i
           jsup=j
        endif
;
    endfor
endfor
;
deltax=float((x(isup)-x(jsup)))
if deltax lt 0 then begin
   deltax=abs(deltax)
   deltay=float(y(jsup)-y(isup))
endif else begin
   deltay=float(y(isup)-y(jsup))
endelse   
;
if deltay ge 0 then begin
   alpha1=-atan(abs(deltay/deltax))
endif else begin
   alpha1=atan(abs(deltay/deltax))
endelse
;
mnx=fix(mean(x))
mny=fix(mean(y))
x=x-mnx
y=y-mny
;
mat=[[cos(alpha1),sin(alpha1)],[-sin(alpha1),cos(alpha1)]]
;
x1=intarr(nx)
y1=intarr(nx)
;
for i=0,nx-1 do begin
;
    vec=[x(i),y(i)]
    vec1=mat#vec
    x1(i)=rfix(vec1(0))
    y1(i)=rfix(vec1(1))
;
endfor 
;
x=x+mnx
y=y+mny
x1=x1+mnx
y1=y1+mnx
;
d2=max(y1)-min(y1)
;
fer=[dsup,d2]    
;
RETURN, fer

END
       


 
  





