pro colorbar,xfrac=xfrac,x0=x0,ymargin=ymargin,$
 background=background,reverse=reverse,yrange=yrange
;+
;   colorbar,xfrac=xfrac,x0=x0,ymargin=ymargin,$
;     background=background,reverse=reverse
;
;            makes colorbar to right of plot
;            xfrac       width of colorbar in normalized coordinates  (0.03)
;            x0          position of colorbar, left side         (1.0-xfrac)
;            ymargin     ymargin                                 (!y.margin)
;            background  background color                          (default)
;            reverse     revers colorbar
;            yrange      range in y. if set, y-axis is produced
;
;-
; set default values
;
if(n_elements(xfrac) eq 0) then xfrac=0.03
if(n_elements(x0) eq 0) then x0=1.0-xfrac
if(n_elements(ymargin) eq 0) then ymargin=!y.margin
if(n_elements(background) eq 0) then begin
  if(!d.name eq 'PS') then background=255 else background=0
endif

w=10
bar=bytscl(indgen(256),top=!d.n_colors)
if(n_elements(reverse) ne 0) then bar=reverse(bar)
tmp=bytarr(w,256)
for i=0,w-1 do tmp(i,*)=bar
old_region=!p.region
!p.region=[x0,0.,x0+xfrac,1.0]
if(n_elements(yrange) ge 2) then begin
  plot_image,tmp,[0,1],yrange,xstyle=5,ystyle=1,xmargin=[0,0],$
   ymargin=ymargin,background=background,/noerase
endif else begin
  plot_image,tmp,[0,1],[0,255],xstyle=5,ystyle=5,xmargin=[0,0],$
   ymargin=ymargin,background=background,/noerase
endelse
x=[0,1,1,0,0]
y=[!cymin,!cymin,!cymax,!cymax,!cymin]
oplot,x,y,color=0
!p.region=old_region

end
