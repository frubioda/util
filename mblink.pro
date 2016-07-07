pro mblink_ehandler,ev
widget_control, /destroy, ev.top
end

pro mblink ,windows ,true=true
;+
;   mblink ,windows
;
;            blink windows using cw_animate
;            set true=0 if not true color display
;
;-
if(n_elements(windows) eq 0) then windows=[0,2]
if(n_elements(true) eq 0) then true=3

n_windows=n_elements(windows)
wset,windows[0]
base=widget_base(title='mBlink')
animate=cw_animate(base,!d.x_size,!d.y_size,n_windows)
widget_control,/realize,base
for i=0,n_windows-1 do begin
  wset,windows[i]
  cw_animate_load,animate,frame=i,image=tvrd(true=true)
endfor

cw_animate_run,animate,4
xmanager,'mblink',base,event_handler='mblink_ehandler'
wset,windows[0]
end
