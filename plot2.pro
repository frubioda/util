PRO plot2,x,y1,y2,xtitle=xtitle,y1title=y1title,y2title=y2title,title=title,$
          y1range=y1range,y2range=y2range,$
          line1=line1,line2=line2,psym1=psym1,psym2=psym2,_extra=e
;
;  overplot two variables with own axes
;
if(n_params(0) lt 3) then begin
  print,'plot2,x,y1,y2,xtitle=xtitle,y1title=y1title,y2title=y2title,$'
  print,' y1range=y1range,y2range=y2range,$'
  print,' title=title,line1=line1,line2=line2,psym1=psym1,psym2=psym2'
  return
endif

if n_elements(xtitle)  eq 0 then xtitle=' '
if n_elements(y1title) eq 0 then y1title=' '
if n_elements(y2title) eq 0 then y2title=' '
if n_elements(title)   eq 0 then title=' '
if n_elements(y1range) eq 0 then y1range=!y.range
if n_elements(y2range) eq 0 then y2range=!y.range
if n_elements(line1)   eq 0 then line1=0
if n_elements(line2)   eq 0 then line2=2
if n_elements(psym1)   eq 0 then psym1=0
if n_elements(psym2)   eq 0 then psym2=0

pmulti=!p.multi
plot,x,y1,yst=8+!y.style,line=line1,psym=psym1, $
     title=title,ytitle=y1title,xtitle=xtitle,xmargin=[10,10],yrange=y1range,$
     _extra=e
pmulti2=!p.multi
!p.multi=pmulti
plot,x,y2,/noerase,yst=4+!y.style,line=line2,psym=psym2,xmargin=[10,10],$
 yrange=y2range,_extra=e
axis,yaxis=1,ystyle=4+!y.style,ytitle=y2title,yrange=y2range
!p.multi=pmulti2

return
end

