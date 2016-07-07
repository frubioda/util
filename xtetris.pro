;+
; NAME:
;       XTETRIS
;
; PURPOSE:
;
;	This is an IDL version of the old computer game called TETRIS.
;
; 	Adapted from a non-widget version of TETRIS originally written by
;	Ray Sterner.  The control keys below are implemented using a trick
;	where a text widget is behind a draw widget on a bulletin-board type
;	base.  The input focus is given to the text widget so that it
;	recognizes alpha-numeric input.  The events generated in the text
;	widget are interpreted as listed below.  This nice trick
;	was posted on the IDL newsgroup by JD Smith.
;
; AUTHOR:
;
;       Robert M. Dimeo, Ph.D.
;	NIST Center for Neutron Research
;       100 Bureau Drive
;	Gaithersburg, MD 20899
;       Phone: (301) 975-8135
;       E-mail: robert.dimeo@nist.gov
;       http://www.ncnr.nist.gov/staff/dimeo
;
; CATEGORY:
;
;       Widgets, games
;
; CALLING SEQUENCE:
;
;       XTETRIS
;
;
; CONTROLS:
;
;	A: left
;	F: right
;	P: pause
;	R: resume
;	D: down
;	SPACE: rotate
;	Q: quit
;
; REQUIREMENTS:
;
;	Uses the object class "PRINTOBJ" for scoring updates.  In the
;	original program by Sterner this was performed using a procedure
;	called SPRINT which used common blocks.  Use of this object class
;	eliminates the need for any common blocks.
;
; COMMON BLOCKS:
;
;	None
;
; DISCLAIMER
;
;	This software is provided as is without any warranty whatsoever.
;	Permission to use, copy, modify, and distribute modified or
;	unmodified copies is granted, provided this disclaimer
;	is included unchanged.
;
; MODIFICATION HISTORY:
;
;       Written by Rob Dimeo, December 7, 2002.
;	12/12/02 (RMD): Added the checkmarks next to the skill level,
;			renamed the skill levels, and added an intermediate level.
;			The checkmarks are a new feature of IDL 5.6 and the program
;			determines if you are running this on 5.6.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro xtCleanup,tlb
widget_control,tlb,get_uvalue = pState
wdelete,(*pState).winPix
ptr_free,(*pState).t_pxa,(*pState).t_pya,(*pState).t_pfxa,(*pState).t_pfya
ptr_free,(*pState).t_brd,(*pState).top,(*pState).t_pfx,(*pState).t_pfy
ptr_free,(*pState).t_px,(*pState).t_py
obj_destroy,(*pState).oText
ptr_free,pState
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro xtHelp,event
widget_control,event.top,get_uvalue = pState
strout = ' '
strout = [strout,' Tetris has 7 different playing pieces which drop down']
strout = [strout,' from the top of the screen. Points are scored by']
strout = [strout,' fitting these pieces together to form horizontal rows']
strout = [strout,' having no gaps. Such complete rows dissolve away and add']
strout = [strout," to the player's score. Pieces may be moved left and right"]
strout = [strout,' and rotated to fit together. The more rows completed the']
strout = [strout,' higher the score each newly completed row is worth.']
strout = [strout,' Extra credit is given for completing 4 rows at the same']
strout = [strout,' time.  Upper or lower case key commands may be used. ']
strout = [strout,' Both the current game scores and the highest score during']
strout = [strout,' the current session of IDL are displayed.']
strout = [strout,' ']
strout = [strout,' The first version of this project was written using PC IDL']
strout = [strout,' in an afternoon as a test of the capabilities of IDL on a']
strout = [strout,' 386 class machine. (Ray Sterner-1991)']
strout = [strout,'']
strout = [strout,' The widget version of this project was written using IDL 5.4']
strout = [strout,' and updated a bit for release 5.6.']
strout = [strout,' (Rob Dimeo-2002)']
strout = [strout,' ']
void = dialog_message(dialog_parent = event.top,strout,/information)
widget_control,(*pState).hiddenTextId,/input_focus
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro xtQuit,event
widget_control,event.top,/destroy
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro xtStart,event
widget_control,event.top,get_uvalue = pState
(*pState).t_lpc = (*pState).t_pc
(*pState).t_lln = (*pState).t_ln
(*pState).t_lsc = (*pState).t_sc
(*pState).oText->changeText,index = 20,text = strtrim((*pState).t_lpc,2)
(*pState).oText->changeText,index = 21,text = strtrim((*pState).t_lln,2)
(*pState).oText->changeText,index = 22,text = strtrim((*pState).t_lsc,2)

(*pState).t_hpc = (*pState).t_hpc > (*pState).t_pc
(*pState).t_hln = (*pState).t_hln > (*pState).t_ln
(*pState).t_hsc = (*pState).t_hsc > (*pState).t_sc
(*pState).oText->changeText,index = 23,text = strtrim((*pState).t_hpc,2)
(*pState).oText->changeText,index = 24,text = strtrim((*pState).t_hln,2)
(*pState).oText->changeText,index = 25,text = strtrim((*pState).t_hsc,2)

xtInit,event

top = [-1]
t_ny = (*pState).t_ny
t_brd = *(*pState).t_brd
tmp = fltarr(t_ny)
for j = 0,t_ny-1 do tmp[j] = total(t_brd[*,j])
mx = 1+max(where(tmp ne 0))
top = [top,mx]
*(*pState).top = top
widget_control,(*pState).timerId,timer = (*pState).duration
(*pState).loop = 1
xt_next,event
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro xt_plot,event,flag = flag
widget_control,event.top,get_uvalue = pState

c = 0
if flag eq 1 then c = (*pState).t_c
if max((*pState).t_y+(*(*pState).t_pfy)) lt (*pState).t_ny then begin
    polyfill, (*pState).t_x+(*(*pState).t_pfx), $
      (*pState).t_y+(*(*pState).t_pfy), color=c
endif

return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro xt_drop,event,done = done,range = range
widget_control,event.top,get_uvalue = pState
t_brd = *(*pState).t_brd

xt_plot, event,flag = 0       ; Erase current position.
(*pState).t_y = (*pState).t_y - 1   ; Drop one position.

flag = 0                                ; Undo flag.
if min((*pState).t_y + (*(*pState).t_py)) lt 0 then flag = 1   ; Hit bottom.
if max(t_brd((*pState).t_x+(*(*pState).t_px), $
  (*pState).t_y+(*(*pState).t_py))) gt 0 then flag = 1    ; Collision.

done = 0                                ; Assume not done yet.
if flag eq 1 then begin                 ; Done.
      (*pState).t_y = (*pState).t_y + 1                        ; Can't move down.
      t_brd((*pState).t_x+(*(*pState).t_px), $
        (*pState).t_y+(*(*pState).t_py)) = (*pState).t_c       ; Update board with color.
      done = 1                              ; Set done flag.
      range = [min((*pState).t_y+(*(*pState).t_py)), $
        max((*pState).t_y+(*(*pState).t_py))]  ; Range to check.
endif

; Update the "COMMON" variables
*(*pState).t_brd = t_brd
xt_plot,event,flag = 1       ; Plot new position.
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro xt_next,event,pn = pn
widget_control,event.top,get_uvalue = pState

if n_elements(pn) eq 0 then begin
  (*pState).t_p = byte(randomu(s)*7)  ; Pick a random piece #.
endif else (*pState).t_p = pn         ; Use selected piece number.
(*pState).t_r = 0                     ; Start in standard position.
(*pState).t_c = (*pState).t_ca[(*pState).t_p]   ; Look up piece color.
; Pull out correct offsets.
*(*pState).t_px = (*(*pState).t_pxa)(*, (*pState).t_r, (*pState).t_p)
*(*pState).t_py = (*(*pState).t_pya)(*, (*pState).t_r, (*pState).t_p)
; Extract outline
*(*pState).t_pfx = (*(*pState).t_pfxa)(0:(*pState).t_pflst((*pState).t_p), $
  (*pState).t_r,(*pState).t_p)
*(*pState).t_pfy = (*(*pState).t_pfya)(0:(*pState).t_pflst((*pState).t_p), $
  (*pState).t_r,(*pState).t_p)
(*pState).t_x = (*pState).t_nx/2                 ; Starting position.
(*pState).t_y = (*pState).t_ny
 return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro xt_score, event,r = r
widget_control,event.top,get_uvalue = pState
; Pull out all of the "COMMON" variables
t_nx = (*pState).t_nx & t_ny = (*pState).t_ny & t_brd = *(*pState).t_brd
t_p = (*pState).t_p & t_r = (*pState).t_r & t_x = (*pState).t_x
t_y = (*pState).t_y & t_pxa = *(*pState).t_pxa & t_pya = *(*pState).t_pya
t_px = *(*pState).t_px & t_py = *(*pState).t_py & t_ca = (*pState).t_ca
t_c = (*pState).t_c & t_wait = (*pState).t_wait & t_pflst = (*pState).t_pflst
t_pfxa = *(*pState).t_pfxa & t_pfya = *(*pState).t_pfya & t_pfx = *(*pState).t_pfx
t_pfy = *(*pState).t_pfy & t_pc = (*pState).t_pc & t_lpc = (*pState).t_lpc
t_hpc = (*pState).t_hpc & t_ln = (*pState).t_ln & t_lln = (*pState).t_lln
t_hln = (*pState).t_hln & t_sc = (*pState).t_sc & t_lsc = (*pState).t_lsc
t_hsc = (*pState).t_hsc

        ;---------  Add score for this piece  --------

t_sc = t_sc + 7      ; Each piece worth 7 pts.
(*pState).oText->changeText,index = 19,text = strtrim(t_sc, 2)
(*pState).t_sc = t_sc
count = 0                                 ; Lines scored on piece.
rn = (r(0)+indgen(r(1)-r(0)+1))<(t_ny-1)  ; Range to check.
        for i = 0, n_elements(rn)-1 do begin      ; Check each line.
          if total(t_brd(*,rn(i)) eq 0) eq 0 then begin  ; Score.
           ;---  light up score line  ----
            xp = [0.01,.99,.99,0.01]*(t_nx-1)
            yp = [0.05,0.05,.99,.99]+rn(i)
            polyfill, xp,yp,color=0,spacing=.1,orient=0
            polyfill, xp,yp,color=0,spacing=.1,orient=90
;            wait, 0
;            ;---  ring bell  -----
;            if t_bell then print,string(7b),form='($,a1)'
            ;---  Collapse board  -------
            t_brd(0,rn(i)) = t_brd(*,(rn(i)+1):*)
            *(*pState).t_brd = t_brd
            ;---  Repaint screen board  -----
            tmp = fltarr(t_ny)
            for j = 0, t_ny-1 do tmp(j) = total(t_brd(*,j))
            mx = 1+max(where(tmp ne 0))
            for z = 0.8, 0., -.2 do begin
            for iy = rn(i), mx do begin
              for ix = 0, t_nx-2 do begin
                c = t_brd(ix,iy)
                polyfill, [0,1,1,0]+ix, (z+[0,0,1,1]+iy)<(t_ny-1), color=c
              endfor
            endfor
            endfor  ; Z
            ;---  Decrement range  ------
            rn = rn - 1
            ;----  Count scored line  -----
            count = count + 1
            ;---  Update score board  -----

            t_ln = t_ln + 1
            (*pState).t_ln = t_ln
            (*pState).oText->changeText,index = 18,text = strtrim(t_ln,2)

            t_sc = t_sc + 22      ; Each line worth 22 pts.
            (*pState).t_sc = t_sc
            (*pState).oText->changeText,index = 19,text = strtrim(t_sc,2)

           endif
        endfor

        ;--------  Check for a tetris (4 lines scored on 1 piece) ----
        if count eq 4 then begin
          t_sc = t_sc + 48      ; 48 extra points.
          (*pState).t_sc = t_sc
          (*pState).oText->changeText,index = 19,text = strtrim(t_sc,2)
        endif

        return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro xtFinish,event, r = r
widget_control,event.top,get_uvalue = pState

tmp = fltarr((*pState).t_ny)
for j = 0, (*pState).t_ny-1 do tmp(j) = total((*(*pState).t_brd)(*,j))
mx = 1+max(where(tmp ne 0))
*(*pState).top = [*(*pState).top,mx]
if min(r) ge (*pState).t_ny-1 then begin  ; Game over?
  (*pState).loop = 0
  polyfill, [0,1,1,0]*((*pState).t_nx-1), [0,0,1,1]*((*pState).t_ny-1),$
          color=0, spacing=.1, orient=0
  polyfill, [0,1,1,0]*((*pState).t_nx-1), [0,0,1,1]*((*pState).t_ny-1),$
          color=0, spacing=.1, orient=90
  return
endif
;------  Update current piece count  ------
(*pState).t_pc = (*pState).t_pc + 1
(*pState).oText->changeText,index = 17,text = strtrim((*pState).t_pc,2)
xt_score,event,r = r                      ; Update score.
xt_next,event
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro xtLeft,event
widget_control,event.top,get_uvalue = pState

xt_plot, event,flag = 0                  ; Erase current position.
(*pState).t_x = (*pState).t_x - 1                           ; Shift left 1.

flag = 0                                ; Undo flag.
if min((*pState).t_x + (*(*pState).t_px)) lt 0 then flag = 1   ; Out of bounds.
if max((*(*pState).t_brd)((*pState).t_x + (*(*pState).t_px), $
  (*pState).t_y + (*(*pState).t_py))) gt 0 then flag = 1    ; Collision.

if flag eq 1 then (*pState).t_x = (*pState).t_x + 1         ; Undo.
xt_plot, event,flag = 1                               ; Plot new position.
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro xtRight,event
widget_control,event.top,get_uvalue = pState

xt_plot, event, flag = 0             ; Erase current position.
(*pState).t_x = (*pState).t_x + 1         ; Shift right 1.
flag = 0              ; Undo flag.
if max((*pState).t_x + (*(*pState).t_px)) gt ((*pState).t_nx-2) then flag = 1 ; Out of bounds.
if max((*(*pState).t_brd)((*pState).t_x + (*(*pState).t_px), $
  (*pState).t_y + (*(*pState).t_py))) gt 0 then flag = 1    ; Collision.
if flag eq 1 then (*pState).t_x = (*pState).t_x - 1         ; Undo.
xt_plot, event,flag = 1                                     ; Plot new position.

end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro xtRotate,event
widget_control,event.top,get_uvalue = pState
; Pull out all of the "COMMON" variables
t_nx = (*pState).t_nx & t_ny = (*pState).t_ny & t_brd = *(*pState).t_brd
t_p = (*pState).t_p & t_r = (*pState).t_r & t_x = (*pState).t_x
t_y = (*pState).t_y & t_pxa = *(*pState).t_pxa & t_pya = *(*pState).t_pya
t_px = *(*pState).t_px & t_py = *(*pState).t_py & t_ca = (*pState).t_ca
t_c = (*pState).t_c & t_wait = (*pState).t_wait & t_pflst = (*pState).t_pflst
t_pfxa = *(*pState).t_pfxa & t_pfya = *(*pState).t_pfya & t_pfx = *(*pState).t_pfx
t_pfy = *(*pState).t_pfy & t_pc = (*pState).t_pc & t_lpc = (*pState).t_lpc
t_hpc = (*pState).t_hpc & t_ln = (*pState).t_ln & t_lln = (*pState).t_lln
t_hln = (*pState).t_hln & t_sc = (*pState).t_sc & t_lsc = (*pState).t_lsc
t_hsc = (*pState).t_hsc

xt_plot,event,flag = 0               ; Erase current position.

 t_r = (t_r + 1) mod 4   ; Rotate.
 t_px = t_pxa(*,t_r,t_p) ; Extract new offsets.
 t_py = t_pya(*,t_r,t_p)
 t_pfx = t_pfxa(0:t_pflst(t_p),t_r,t_p)    ; Extract outline.
 t_pfy = t_pfya(0:t_pflst(t_p),t_r,t_p)

;----  Check for out of bounds or collision. -----
flag = 0                ; Undo flag.
;------  Don't rotate out the sides  ---------
if (min(t_x+t_px) lt 0) or (max(t_x+t_px) gt (t_nx-2)) then flag = 1
;------  Don't rotate out the bottom  -----
if (min(t_y+t_py) lt 0) then flag = 1
;------  Check collision with another piece  ------
if max(t_brd(t_x+t_px, t_y+t_py)) gt 0 then flag = 1    ; Collision.
if flag eq 1 then begin    ; Undo.
     t_r = (t_r + 3) mod 4    ; Rotate 270 = -90.
     t_px = t_pxa(*,t_r,t_p)  ; Extract new offsets.
     t_py = t_pya(*,t_r,t_p)
     t_pfx = t_pfxa(0:t_pflst(t_p),t_r,t_p)    ; Extract outline.
     t_pfy = t_pfya(0:t_pflst(t_p),t_r,t_p)
endif

(*pState).t_r = t_r
*(*pState).t_px = t_px
*(*pState).t_py = t_py
*(*pState).t_pfx = t_pfx
*(*pState).t_pfy = t_pfy

xt_plot, event,flag = 1               ; Plot new position.

end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro xtEvents,event
widget_control,event.top,get_uvalue = pState
thisEvent = tag_names(event,/structure_name)
case thisEvent of
'WIDGET_BUTTON': $
		begin
		  uname = widget_info(event.id,/uname)
		  if uname eq 'NOVICE' then begin
		  	(*pState).duration = 0.5
		  	if (!version).release ge 5.6 then $
		  		widget_control,event.id,set_button = 1
		  		id1 = widget_info(event.top,find_by_uname = $
		  		  'INTERMEDIATE')
		  		id2 = widget_info(event.top,find_by_uname = $
		  		  'EXPERT')
		  		widget_control,id1,set_button = 0
		  		widget_control,id2,set_button = 0
		  endif
		  if uname eq 'INTERMEDIATE' then begin
		  	(*pState).duration = 0.25
		  	if (!version).release ge 5.6 then $
		  		widget_control,event.id,set_button = 1
		 		id1 = widget_info(event.top,find_by_uname = $
		  		  'NOVICE')
		  		id2 = widget_info(event.top,find_by_uname = $
		  		  'EXPERT')
		  		widget_control,id1,set_button = 0
		  		widget_control,id2,set_button = 0
		  endif
		  if uname eq 'EXPERT' then begin
		  	(*pState).duration = 0.1
		  	if (!version).release ge 5.6 then $
		  		widget_control,event.id,set_button = 1
		  		id1 = widget_info(event.top,find_by_uname = $
		  		  'INTERMEDIATE')
		  		id2 = widget_info(event.top,find_by_uname = $
		  		  'NOVICE')
		  		widget_control,id1,set_button = 0
		  		widget_control,id2,set_button = 0
		  endif
		end
'WIDGET_TEXT_CH': $
		begin
		  case strupcase(event.ch) of

		  ' ':  begin
		  		if (*pState).loop eq 1 then xtRotate,event
		  		end
		  'H':	xtHelp,event
		  'P':	(*pState).loop = 0
		  'R':	(*pState).loop = 1
		  'Q':	begin
		  	  xtQuit,event
		  	  return
		  	end
		  'A':	begin
		  		if (*pState).loop eq 1 then xtLeft,event
	 		end
		  'F':	begin
		  		if (*pState).loop eq 1 then xtRight,event;(*pState).direction = 'E'
	  		end
		  else:
		  endcase
		end
else:  widget_control,(*pState).hiddenTextId,/input_focus
endcase

if (*pState).loop eq 1 then begin	; update display
  xt_drop,event, done = d, range = r
  if d eq 1 then begin	; piece is done moving
	xtFinish,event,r = r
  endif

  widget_control,(*pState).timerId,timer = (*pState).duration
endif

end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro xtInit,event
widget_control,event.top,get_uvalue = pState

t_pxa = intarr(4,4,7)
        t_pxa(0,0,0) = [[0,1,0,1], $   ; Piece # 0:  X X
                        [0,1,0,1], $   ;             X X
                        [0,1,0,1], $
                        [0,1,0,1]]

        t_pxa(0,0,1) = [[-2,-1,0,1], $ ; Piece # 1: X X X X
                        [0,0,0,0], $
                        [-2,-1,0,1],$
                        [0,0,0,0]]

        t_pxa(0,0,2) = [[-1,0,1,0], $  ; Piece # 2:  X X X
                        [0,0,0,1], $   ;               X
                        [-1,0,1,0], $
                        [0,0,0,-1]]

        t_pxa(0,0,3) = [[-1,0,0,1], $  ; Piece # 3:    X X
                        [0,0,-1,-1], $ ;                 X X
                        [0,-1,-1,-2], $
                        [-1,-1,0,0]]

        t_pxa(0,0,4) = [[-1,0,0,1], $  ; Piece # 4:      X X
                        [-1,-1,0,0], $ ;               X X
                        [0,-1,-1,-2], $
                        [0,0,-1,-1]]

        t_pxa(0,0,5) = [[-1,0,1,1],$   ; Piece # 5:    X X X
                        [0,0,0,1], $   ;                   X
                        [1,0,-1,-1], $
                        [0,0,0,-1]]

        t_pxa(0,0,6) = [[1,0,-1,-1],$  ; Piece # 6:    X X X
                        [0,0,0,1],$    ;               X
                        [-1,0,1,1],$
                        [0,0,0,-1]]

        ;---  Set up Y offsets for 7 4 part pieces, each with 4 rotations --
        t_pya = intarr(4,4,7)

        t_pya(0,0,0) = [[0,0,1,1],[0,0,1,1],[0,0,1,1],[0,0,1,1]]
        t_pya(0,0,1) = [[0,0,0,0],[-2,-1,0,1],[0,0,0,0],[-2,-1,1,0]]
        t_pya(0,0,2) = [[0,0,0,-1],[-1,0,1,0],[0,0,0,1],[1,0,-1,0]]
        t_pya(0,0,3) = [[0,0,-1,-1],[0,-1,-1,-2],[-1,-1,0,0],[-1,0,0,1]]
        t_pya(0,0,4) = [[-1,-1,0,0],[0,-1,-1,-2],[0,0,-1,-1],[-1,0,0,1]]
        t_pya(0,0,5) = [[0,0,0,-1],[-1,0,1,1],[0,0,0,1],[1,0,-1,-1]]
        t_pya(0,0,6) = [[0,0,0,-1],[1,0,-1,-1],[0,0,0,1],[-1,0,1,1]]

        ;------  Setup pieces as outlines  ---------
        t_pfxa = intarr(8,4,7)
        t_pfxa(0,0,0) = [[0,2,2,0,0,0,0,0],$
                         [0,2,2,0,0,0,0,0],$
                         [0,2,2,0,0,0,0,0],$
                         [0,2,2,0,0,0,0,0]]
        t_pfxa(0,0,1) = [[-2,2,2,-2,0,0,0,0],$
                         [0,1,1,0,0,0,0,0],$
                         [-2,2,2,-2,0,0,0,0],$
                         [0,1,1,0,0,0,0,0]]
        t_pfxa(0,0,2) = [[-1,0,0,1,1,2,2,-1],$
                         [0,1,1,2,2,1,1,0],$
                         [-1,2,2,1,1,0,0,-1],$
                         [-1,0,0,1,1,0,0,-1]]
        t_pfxa(0,0,3) = [[-1,0,0,2,2,1,1,-1],$
                         [-1,0,0,1,1,0,0,-1],$
                         [-2,-1,-1,1,1,0,0,-2],$
                         [-1,0,0,1,1,0,0,-1]]
        t_pfxa(0,0,4) = [[-1,1,1,2,2,0,0,-1],$
                        [0,1,1,0,0,-1,-1,0],$
                        [-2,0,0,1,1,-1,-1,-2],$
                        [0,1,1,0,0,-1,-1,0]]
        t_pfxa(0,0,5) = [[-1,1,1,2,2,-1,0,0],$
                         [0,1,1,2,2,0,0,0],$
                         [-1,2,2,0,0,-1,0,0],$
                         [-1,1,1,0,0,-1,0,0]]
        t_pfxa(0,0,6) = [[-1,0,0,2,2,-1,0,0],$
                         [0,2,2,1,1,0,0,0],$
                         [-1,2,2,1,1,-1,0,0],$
                         [0,1,1,-1,-1,0,0,0]]
        t_pfya = intarr(8,4,7)
        t_pfya(0,0,0) = [[0,0,2,2,0,0,0,0],$
                         [0,0,2,2,0,0,0,0],$
                         [0,0,2,2,0,0,0,0],$
                         [0,0,2,2,0,0,0,0]]
        t_pfya(0,0,1) = [[0,0,1,1,0,0,0,0],$
                         [-2,-2,2,2,0,0,0,0],$
                         [0,0,1,1,0,0,0,0],$
                         [-2,-2,2,2,0,0,0,0]]
        t_pfya(0,0,2) = [[0,0,-1,-1,0,0,1,1],$
                         [-1,-1,0,0,1,1,2,2],$
                         [0,0,1,1,2,2,1,1],$
                         [0,0,-1,-1,2,2,1,1]]
        t_pfya(0,0,3) = [[0,0,-1,-1,0,0,1,1],$
                         [-2,-2,-1,-1,1,1,0,0],$
                         [0,0,-1,-1,0,0,1,1],$
                         [-1,-1,0,0,2,2,1,1]]
        t_pfya(0,0,4) = [[-1,-1,0,0,1,1,0,0],$
                         [-2,-2,0,0,1,1,-1,-1],$
                         [-1,-1,0,0,1,1,0,0],$
                         [-1,-1,1,1,2,2,0,0]]
        t_pfya(0,0,5) = [[0,0,-1,-1,1,1,0,0],$
                         [-1,-1,1,1,2,2,0,0],$
                         [0,0,1,1,2,2,0,0],$
                         [-1,-1,2,2,0,0,0,0]]
        t_pfya(0,0,6) = [[-1,-1,0,0,1,1,0,0],$
                         [-1,-1,0,0,2,2,0,0],$
                         [0,0,2,2,1,1,0,0],$
                         [-1,-1,2,2,1,1,0,0]]
*(*pState).t_pxa = t_pxa
*(*pState).t_pya = t_pya
*(*pState).t_pfxa = t_pfxa
*(*pState).t_pfya = t_pfya

t_brd = bytarr((*pState).t_nx-1,(*pState).t_ny)

if abs((*pState).lev) gt 0 then begin
	lset = (byte(randomu(i,(*pState).t_nx-1,abs((*pState).lev))*8)<7B)* $
	        byte(randomu(i,(*pState).t_nx-1,abs((*pState).lev)) gt .5)
    if (*pState).lev lt 0 then lset = 8*(lset ne 0)
	t_brd(0,0) = lset
endif
*(*pState).t_brd = t_brd

wset,(*pState).winPix
erase
t_nx = (*pState).t_nx
t_ny = (*pState).t_ny
lev = (*pState).lev
        ;-------  Scale board to screen  --------
        plot,[0,(*pState).t_nx-1],[0,(*pState).t_ny-1],position=[.1,.1,.4,.9],/xsty,/ysty,/nodata
        ;-------  Outline board  -----------------
        erase
        polyfill, [-1,t_nx, t_nx, -1], [-1, -1, t_ny, t_ny], $
          color=10
        polyfill, [-1,t_nx, t_nx, -1], [-1, -1, t_ny, t_ny], $
          color=9, spacing=.15, orient=0
        polyfill, [-1,t_nx, t_nx, -1], [-1, -1, t_ny, t_ny], $
          color=9, spacing=.15, orient=90
        polyfill,[-.2,t_nx-.8,t_nx-.8,-.2],$
          [-.2,-.2,t_ny-.8,t_ny-.8]
        polyfill,[0,1,1,0]*(t_nx-1),[0,0,1,1]*(t_ny-1), color=0
	plots,[-1,t_nx,t_nx,-1,-1],[-1,-1,t_ny,t_ny,-1],thick=3

	;------  Show starting board  --------
	if abs(lev) gt 0 then begin
	  for iy = 0, abs(lev) do begin
	    for ix = 0, t_nx-2 do begin
	      c = t_brd(ix,iy)
	      polyfill, [0,1,1,0]+ix, [0,0,1,1]+iy, color=c
	    endfor
	  endfor
	endif

        ;------  Menu  -----
        if (*pState).t_init_flag eq 0 then begin
          (*pState).oText->addText,txtSize=1.2, x=325, y=310, text='A = Move left'	;1
          (*pState).oText->addText,txtSize=1.2, x=475, y=310, text='F = Move right'	;2
          (*pState).oText->addText,txtSize=1.2, x=325, y=290, text='SPACE = Rotate'	;3
          (*pState).oText->addText,txtSize=1.2, x=475, y=290, text='H = Help'
          (*pState).oText->addText,txtSize=1.2, x=325, y=270, text='P = Pause'
          (*pState).oText->addText,txtSize=1.2, x=475, y=270, text='Q = Quit'
          (*pState).oText->addText,txtSize=1.2, x=325, y=250, text='R = Resume'		;7

          (*pState).oText->addText,txtSize=1.8, x=310, y=160, text='Pieces'	;8
          (*pState).oText->addText,txtSize=1.8, x=310, y=130, text='Lines'
          (*pState).oText->addText,txtSize=1.8, x=310, y=100, text='Score'
          (*pState).oText->addText,txtSize=1.2, x=410, y=210, text='This'
          (*pState).oText->addText,txtSize=1.2, x=410, y=190, text='Game'
          (*pState).oText->addText,txtSize=1.2, x=480, y=210, text='Last'
          (*pState).oText->addText,txtSize=1.2, x=480, y=190, text='Game'
          (*pState).oText->addText,txtSize=1.2, x=550, y=210, text='Session'
          (*pState).oText->addText,txtSize=1.2, x=550, y=190, text='High'	;16

          (*pState).oText->addText,txtSize=1.2, x=410, y=160, $
            text=strtrim((*pState).t_pc,2);17
          (*pState).oText->addText,txtSize=1.2, x=410, y=130, $
            text=strtrim((*pState).t_ln,2);18
          (*pState).oText->addText,txtSize=1.2, x=410, y=100, $
            text=strtrim((*pState).t_sc,2);19
          (*pState).oText->addText,txtSize=1.2, x=480, y=160, $
            text=strtrim((*pState).t_lpc,2);20
          (*pState).oText->addText,txtSize=1.2, x=480, y=130, $
            text=strtrim((*pState).t_lln,2);21
          (*pState).oText->addText,txtSize=1.2, x=480, y=100, $
            text=strtrim((*pState).t_lsc,2);22
          (*pState).oText->addText,txtSize=1.2, x=550, y=160, $
            text=strtrim((*pState).t_hpc,2);23
          (*pState).oText->addText,txtSize=1.2, x=550, y=130, $
            text=strtrim((*pState).t_hln,2);24
          (*pState).oText->addText,txtSize=1.2, x=550, y=100, $
            text=strtrim((*pState).t_hsc,2);25

          (*pState).t_init_flag = 1
        endif else (*pState).oText->displayAll

       (*pState).t_pc = 0	; Current score.
       (*pState).t_ln = 0
       (*pState).t_sc = 0
       (*pState).oText->changeText,index = 17,text = strtrim((*pState).t_pc,2)
       (*pState).oText->changeText,index = 18,text = strtrim((*pState).t_ln,2)
       (*pState).oText->changeText,index = 19,text = strtrim((*pState).t_sc,2)

        ;-------  Load color table  --------
        tvlct, $
          [0,255,255,127,255,127,255,127,128,255,  0,  0,255,255,255,255],$
          [0,127,127,255,255,127,189,255,128,255,  0,255,  0,255,255,255],$
          [0,127,255,255,127,255,127,127,128,255,255,233,  0,  0,  0,255]

        ;---------  Make title  --------
        xshft = 30
        xyouts, 25+300+xshft, 400+15, /dev, size=3, '!17IDL Tetris', color=10
        xyouts, 25+301+xshft, 401+15, /dev, size=3, '!17IDL Tetris', color=10
        xyouts, 25+302+xshft, 402+15, /dev, size=3, '!17IDL Tetris', color=11
        xyouts, 25+303+xshft, 403+15, /dev, size=3, '!17IDL Tetris', color=11
        xyouts, 25+304+xshft, 404+15, /dev, size=3, '!17IDL Tetris', color=12
        xyouts, 25+305+xshft, 405+15, /dev, size=3, '!17IDL Tetris', color=12
        xyouts, 25+306+xshft, 406+15, /dev, size=3, '!17IDL Tetris', color=13
        xyouts, 25+307+xshft, 407+15, /dev, size=3, '!17IDL Tetris', color=13
		xval1 = 80
		xval2 = 50
        xyouts, /dev, size=2, 23+392-xval1, 370+13, $
          '!13Adaptation/Original by', color=12
        xyouts, /dev, size=2, 23+342-xval2, 336+13, $
          '!13Rob Dimeo/Ray Sterner!3', color=12
        xyouts, /dev, size=2, 24+392-xval1, 370+14, $
          '!13Adaptation/Original by', color=12
        xyouts, /dev, size=2, 24+342-xval2, 336+14, $
          '!13Rob Dimeo/Ray Sterner!3', color=12
        xyouts, /dev, size=2, 25+392-xval1, 370+15, $
          '!13Adaptation/Original by', color=6
        xyouts, /dev, size=2, 25+342-xval2, 336+15, $
          '!13Rob Dimeo/Ray Sterner!3', color=6

wset,(*pState).winVis
device,copy = [0,0,!d.x_size,!d.y_size,0,0,(*pState).winPix]

end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro xTetris
; Widget definition module
device,decomposed = 0
registerName = 'xtetris'
if xregistered(registerName) then return
tlb = widget_base(/col,title = 'xTetris',mbar = bar,/tlb_frame_attr)
skill = widget_button(bar,value = 'Skill Level',/menu)
if (!version).release lt 5.6 then begin
  void = widget_button(skill,value = 'NOVICE',uname = 'NOVICE')
  void = widget_button(skill,value = 'INTERMEDIATE',uname = 'INTERMEDIATE')
  void = widget_button(skill,value = 'EXPERT',uname = 'EXPERT')
endif else begin
  void = widget_button(skill,value = 'NOVICE',uname = 'NOVICE',/checked_menu)
  void = widget_button(skill,value = 'INTERMEDIATE',uname = 'INTERMEDIATE',/checked_menu)
  void = widget_button(skill,value = 'EXPERT',uname = 'EXPERT',/checked_menu)
endelse
void = widget_button(tlb,value = 'START',event_pro = 'xtStart')
xsize = 650 & ysize = 500
; Now create a "bulletin board" type base and put a draw widget and
; a text widget on it.
bulBase = widget_base(tlb);		bulletin board base
win = widget_draw(bulBase,xsize = xsize,ysize = ysize)
hiddenTextId = widget_text(bulBase,scr_xsize = 1,scr_ysize = 1,/all_events)

widget_control,tlb,/realize
if (!version).release ge 5.6 then begin
  id = widget_info(tlb,find_by_uname = 'EXPERT')
  widget_control,id,set_button = 2
endif
widget_control,win,get_value = winVis
window,/free,/pixmap,xsize = xsize,ysize = ysize
winPix = !d.window
xpos = fix(0.5*xsize)
ypos = ysize

; Define the filled circle symbol
th = (2.0*!pi/20.)*findgen(21)
xc = cos(th) & yc = sin(th)
usersym,xc,yc,/fill

t_nx = 11
t_ny = 21

state = {	winPix:winPix,		$
		win:win,			$
		winVis:winVis,		$
		duration:0.05,		$
		symsize:2.0,		$
		hiddenTextId:hiddenTextId,	$
		direction:'S',		$
		xincrement:8,		$
		yincrement:8,		$
		xpos:xpos,			$
		ypos:ypos,			$
		loop:0,				$
		timerID:bulBase,	$
		oText:obj_new('printObj'),	$

		t_init_flag:0,				$
		wt:(-1),	$
		t_wait:0.1,	$
		lev:0,		$
		t_ln:0,	$
		t_pc:0,	$
		t_sc:0,	$
		t_lln:0,	$
		t_lpc:0,	$
		t_lsc:0,	$
		t_hln:0,	$
		t_hpc:0,	$
		t_hsc:0,	$

		t_p:0,		$
		t_r:0,	$
		t_x:0,	$
		t_y:0,	$
		t_px:ptr_new(0),	$
		t_py:ptr_new(0),	$
		t_c:0,	$
		t_pfx:ptr_new(0),	$
		t_pfy:ptr_new(0),	$

		top:ptr_new(/allocate_heap),	$
		t_nx:t_nx,					$
		t_ny:t_ny,					$
		t_brd:ptr_new(/allocate_heap),	$
		t_ca:(1+indgen(8)),			$
		t_pxa:ptr_new(/allocate_heap),		$
		t_pya:ptr_new(/allocate_heap),		$
		t_pflst:[3,3,7,7,7,5,5],	$
		t_pfxa:ptr_new(/allocate_heap),		$
		t_pfya:ptr_new(/allocate_heap)		$

		}

pState = ptr_new(state,/no_copy)
widget_control,tlb,set_uvalue = pState
pseudoEvent = {pEvent,id:win,top:tlb,handler:0L}
xtInit,pseudoEvent
xt_next,pseudoEvent
widget_control,(*pState).hiddenTextId,/input_focus

xmanager,registerName,tlb,event_handler = 'xtEvents', $
  cleanup = 'xtCleanup',/no_block

end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






























;+
; NAME:
;       PRINTOBJ__DEFINE
;
; PURPOSE:
;
;       This object class mimics the functionality of the routine SPRINT.PRO
;	written by Ray Sterner for use in the program TETRIS.PRO.  This object
;	class removes the common block.  This object class is used in the
;	widget program XTETRIS.PRO.
;
; AUTHOR:
;
;       Robert M. Dimeo, Ph.D.
;	NIST Center for Neutron Research
;       100 Bureau Drive
;	Gaithersburg, MD 20899
;       Phone: (301) 975-8135
;       E-mail: robert.dimeo@nist.gov
;       http://www.ncnr.nist.gov/staff/dimeo
;
; CATEGORY:
;
;       Objects, widgets
;
; CALLING SEQUENCE:
;
;       object = obj_new('PRINTOBJ')
;
;
; INPUT PARAMETERS:
;
;       NONE
;
; INPUT KEYWORDS:
;
;       NONE
;
; REQUIRED PROGRAMS:
;
;       NONE
;
; COMMON BLOCKS:
;
;       NONE
;
; RESTRICTIONS
;
;       NONE
;
; OBJECT METHODS:
;
; There are no explicitly private object methods in IDL but the
; methods used in this class are divided into PUBLIC and PRIVATE
; to indicate which ones should be used by users who wish to run
; the program from the command line, for instance.
;
; PUBLIC OBJECT PROCEDURE METHODS:
;
;   addText --		initializes text at device coordinates (x,y).
;	USAGE: o->addText,x = x,y = y,text = text, txtSize = txtSize, $
;			  color = color, erase = erase
;
;   changeText --	changes text whose index is given by the order in which
;			it was added using the addText method
;	USAGE: o->changeText,text = text, index = index, color = color, $
;			txtSize = txtSize,erase = erase
;
;   displayAll --	displays all of the text that has been added using addText
;	USAGE: o->displayAll
;
;   clear --		clears all of the text that has been added using addText
;
;
; PRIVATE OBJECT PROCEDURE METHODS:
;
;   cleanup -- frees the pointers
;
;   init -- standard object class initialization
;
; EXAMPLE
;
;	IDL>	o = obj_new('printobj')		; instantiate the object class
;	IDL>	window,0,xsize = 400,ysize = 400
;		Initialize a text string, 'Hi there', to appear at (325,310) in device coordinates
;	IDL>	o-> addText,txtSize=1.2, x=325, y=310, text='Hi'
;		Initialize a text string, 'Hello', to appear at (125,100) in device coordinates
;	IDL>	o-> addText,txtSize = 3.4,x = 125,y = 100,text = 'Hello'
;		Change the first string from 'Hi' to 'Oh no'
;	IDL>	o -> changeText,index = 1,text = 'Oh no'
;		Destroy the object
;	IDL>	obj_destroy,o
;
;
; DISCLAIMER
;
;	This software is provided as is without any warranty whatsoever.
;	Permission to use, copy, modify, and distribute modified or
;	unmodified copies is granted, provided this disclaimer
;	is included unchanged.
;
; MODIFICATION HISTORY:
;
;       Written by Rob Dimeo, December 7, 2002.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro printObj::cleanup
compile_opt idl2,hidden
ptr_free,self.xPtr,self.yPtr
ptr_free,self.sizePtr,self.textPtr
ptr_free,self.colorPtr,self.indexPtr
ptr_free,self.erasePtr
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro printObj::addText,	x = x, $
			y = y, $
			txtSize = txtSize,	$
			text = text, $
			color = color, $
			erase = erase
compile_opt idl2,hidden
if n_elements(erase) eq 0 then erase = 0
if n_elements(txtSize) eq 0 then txtSize = 1
if n_elements(color) eq 0 then color = !p.color

if n_elements(*self.xPtr) eq 0 then begin	; first one
  *self.xPtr = [x]
  *self.yPtr = [y]
  *self.textPtr = [text]
  *self.colorPtr = [color]
  *self.sizePtr = [txtSize]
  *self.indexPtr = [1]
  *self.erasePtr = [erase]
endif else begin	; adding another
  *self.xPtr = [*self.xPtr,x]
  *self.yPtr = [*self.yPtr,y]
  *self.textPtr = [*self.textPtr,text]
  *self.colorPtr = [*self.colorPtr,color]
  *self.sizePtr = [*self.sizePtr,txtSize]
  *self.erasePtr = [*self.erasePtr,erase]
  newIndex = n_elements(*self.indexPtr) + 1
  *self.indexPtr = [*self.indexPtr,newIndex]
endelse

; Display the latest addition
xyouts,/dev,x,y,text,color = color,charSize = txtSize

end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro printObj::changeText,	text = text, $
				index = index, $
				color = color, $
				txtSize = txtSize, $
				erase = erase
compile_opt idl2,hidden
if n_elements(erase) eq 0 then erase = (*self.erasePtr)[index - 1]
; First erase the old text
x = (*self.xPtr)[index - 1]
y = (*self.yPtr)[index - 1]
oldText = (*self.textPtr)[index - 1]
xyouts,/dev,x,y,oldText,color = erase, $
		charsize = (*self.sizePtr)[index - 1]

; Now display the new text
if n_elements(color) eq 0 then color = (*self.colorPtr)[index - 1]
if n_elements(txtSize) eq 0 then txtSize = (*self.sizePtr)[index - 1]
xyouts,/dev,x,y,text,charsize = txtSize,color = color
(*self.colorPtr)[index-1] = color
(*self.sizePtr)[index-1] = txtSize
(*self.textPtr)[index-1] = text
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro printObj::displayAll
compile_opt idl2,hidden
n = n_elements(*self.xPtr)
if n eq 0 then return
for i = 0,n-1 do begin
	x = (*self.xPtr)[i]
	y = (*self.yPtr)[i]
	text = (*self.textPtr)[i]
	color = (*self.colorPtr)[i]
	txtSize = (*self.sizePtr)[i]
	xyouts,/dev,x,y,text,color = color,charSize = txtSize
endfor
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro printObj::clear
ptr_free,self.xPtr,self.yPtr
ptr_free,self.sizePtr,self.textPtr
ptr_free,self.colorPtr,self.indexPtr
ptr_free,self.erasePtr
self.xPtr = ptr_new(/allocate_heap)
self.yPtr = ptr_new(/allocate_heap)
self.sizePtr = ptr_new(/allocate_heap)
self.textPtr = ptr_new(/allocate_heap)
self.colorPtr = ptr_new(/allocate_heap)
self.indexPtr = ptr_new(/allocate_heap)
self.erasePtr = ptr_new(/allocate_heap)
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function printObj::init
compile_opt idl2,hidden
self.xPtr = ptr_new(/allocate_heap)
self.yPtr = ptr_new(/allocate_heap)
self.sizePtr = ptr_new(/allocate_heap)
self.textPtr = ptr_new(/allocate_heap)
self.colorPtr = ptr_new(/allocate_heap)
self.indexPtr = ptr_new(/allocate_heap)
self.erasePtr = ptr_new(/allocate_heap)
return,1
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro printObj__define
compile_opt idl2,hidden
define = {	printObj,		$
		xPtr:ptr_new(),		$
		yPtr:ptr_new(),		$
		sizePtr:ptr_new(),	$
		textPtr:ptr_new(),	$
		colorPtr:ptr_new(),	$
		erasePtr:ptr_new(),	$
		indexPtr:ptr_new()	$
		 }
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
