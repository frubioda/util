pro cons_ipol,y1t,y2t
;+
;   cons_ipol,y1t,y2t
;
;            interpolates y1t onto new cmass-scale cmast0
;            conserving the integral y1t*d1t*dzt
;
;-
@common_radyn
@common_movie

if(n_params(0) lt 2) then begin
  print,'cons_ipol,y1t,y2t
  return
endif

nt=n_elements(cmass1t(0,*))
x2=cmast0                             ; new x-scale
nx1=n_elements(cmass1t(*,0))
nx2=n_elements(x2)
ynew=fltarr(nx1+nx2+1)
inew=intarr(nx1+nx2)
y2t=y1t-y1t
for it=0,nt-1 do begin             ; loop over time
  y1=y1t(*,it)                     ; original y-values
  x1=cmass1t(*,it)                  ; original x-values
  xmerge=[x1,x2]                   ; merge x-scales
  imerge=indgen(nx1+nx2)           ; new indices
  iold=sort(xmerge)                ; iold(inew) is old index for new index inew
  inew(iold)=imerge                ; inew(iold) is new index for old index iold
  xnew=xmerge(iold)                ; new sorted merged x values
  ynew(nx1+nx2)=0.0                ; store 0.0 outside ynew array for extrapol.
  ynew(inew(0:nx1-1))=y1           ; original y-values stored at new indices
  for i=nx1+nx2-1,nx1,-1 do begin  ; store closest original y-value 
    ynew(inew(i))=ynew(inew(i)+1)
  endfor
  for i=1,nx2-1 do begin           ; loop through new cells
    ii=i+nx1
    xint=xnew(inew(ii-1):inew(ii))
    yint=ynew(inew(ii-1):inew(ii))
    nint=n_elements(xint)
; integral of y*d1t*dzt over new cell
    int=total((xint(1:nint-1)-xint(0:nint-2))*yint(1:nint-1))
    y2t(i,it)=int/(x2(i)-x2(i-1))  ; new value
  endfor
endfor

end



