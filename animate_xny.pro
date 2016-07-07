pro animate_xny,frame1,frame2
;+
;   animate_xny,frame1,frame2
;
;            load animation with xinteranimate
;
;-
@common_multit
common cxnyt,x_nyt,tauq_nyt

last_possible=n_elements(x_nyt(0,0,*))-1

if(n_params() lt 2) then begin
  print,'animate_xny,frame1,frame2'
  print,'last possible frame is: ',last_possible
  return
endif
if(frame2 le frame1) then begin
  print,'frame2 must be larger than frame1'
  return
endif
if(frame1 lt 0) then begin
  print,'frame numbers must be ge 0'
  return
endif
if(frame2 gt last_possible) then begin
  print,'last possible frame is: ',last_possible
  return
endif

nframe=frame2-frame1+1

window,0,xsize=500,ysize=440
xinteranimate,set=[500,440,nframe]
for i=frame1,frame2 do begin
  xny_image,i
  xinteranimate,frame=i-frame1,window=[0]
endfor

end
