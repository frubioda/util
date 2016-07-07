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