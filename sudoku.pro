pro sudoku ,file, verbose=verbose
;+
;   sudoku
;
;            program to solve sudoku puzzles
;
;-
if(n_elements(file) eq 0) then file='sudoku.dat'
openr,lur,file,/get_lun
arr=intarr(6,6)
row=intarr(6)
for j=5,0,-1 do begin
  readf,lur,row
  arr[*,j]=row
endfor
free_lun,lur

win,500,500
sudoku_plot,arr    ; plot given numbers
text=''
read,'<cr> to continue',text

; set up possible numbers. Each position has a maximum of six possible numbers
; parr[i,j,k] is 1 if k+1 is a possible number at position i,j, zero otherwise

iredo=0
squares_redo=0
arr0=arr

redo:
parr=intarr(6,6,6)+1

; go through given numbers

for i=0,5 do begin
  for j=0,5 do begin
    if(arr[i,j] ne 0) then begin
      parr[i,j,*]=0
    endif
  endfor
endfor

fill_numbers:

; exclude duplicate numbers in rows, columns

for i=0,5 do begin
  for j=0,5 do begin
    if(arr[i,j] ne 0) then begin
      if(j gt 0) then parr[i,0:j-1,arr[i,j]-1]=0
      if(j lt 5) then parr[i,j+1:5,arr[i,j]-1]=0
      if(i gt 0) then parr[0:i-1,j,arr[i,j]-1]=0
      if(i lt 5) then parr[i+1:5,j,arr[i,j]-1]=0
    endif
  endfor
endfor

; exclude duplicate numbers in small rectangles

for i0=0,3,3 do begin  ; horizontal small rectangles
  for j0=0,4,2 do begin  ; vertical small rectangles
    for i=i0,i0+2 do begin
      for j=j0,j0+1 do begin
        if(arr[i,j] ne 0) then begin
          parr[i0:i0+2,j0:j0+1,arr[i,j]-1]=0
;          parr[i,j,arr[i,j]-1]=1
        endif
      endfor
    endfor
  endfor
endfor

; change arr where there is only one possibility

count=0
for i=0,5 do begin
  for j=0,5 do begin
    if((arr[i,j] eq 0) and (total(parr[i,j,*]) eq 1)) then begin
      iw=where(parr[i,j,*] eq 1)
      arr[i,j]=iw[0]+1
      count=count+1
    endif
  endfor
endfor

if(count gt 0) then begin
  if(keyword_set(verbose)) then begin
    sudoku_plot,arr,parr=parr
    read,'1: <cr> to continue',text
  endif
  goto,fill_numbers
endif

; add in numbers where there is only one possibility in a row

for i=0,5 do begin
  for k=0,5 do begin
    iw=where(parr[i,*,k] eq 1,count2)
    if(count2 eq 1) then begin
      arr[i,iw[0]]=k+1
      parr[i,iw[0],k]=0
      count=count+1
    endif
  endfor
endfor

if(count gt 0) then begin
  if(keyword_set(verbose)) then begin
    sudoku_plot,arr,parr=parr
    read,'2: <cr> to continue',text
  endif
  goto,fill_numbers
endif

; add in numbers where there is only one possibility in a column

for j=0,5 do begin
  for k=0,5 do begin
    iw=where(parr[*,j,k] eq 1,count2)
    if(count2 eq 1) then begin
      arr[iw[0],j]=k+1
      parr[iw[0],j,k]=0
      count=count+1
    endif
  endfor
endfor

if(count gt 0) then begin
  if(keyword_set(verbose)) then begin
    sudoku_plot,arr,parr=parr
    read,'3: <cr> to continue',text
  endif
  goto,fill_numbers
endif

; add in numbers where there is only one possibility in a small rectangle

for i0=0,3,3 do begin  ; horizontal small rectangles
  for j0=0,4,2 do begin  ; vertical small rectangles
    for k=0,5 do begin
      iw=where(parr[i0:i0+2,j0:j0+1,k] eq 1,count2)
      if(count2 eq 1) then begin
        j=iw[0]/3
        i=iw[0]-j*3
        arr[i,j]=k+1
        parr[i,j,k]=0
        count=count+1
      endif
    endfor
  endfor
endfor

iw=where(arr eq 0,nremains)
if(nremains eq 0) then begin
  sudoku_plot,arr
  return
endif else begin
  sudoku_plot,arr,parr=parr
  return
endelse

; trial and error part is not working correctly, not used

if(keyword_set(verbose)) then print,'trial and error'
if(count eq 0) then begin
  arr=arr0
  if(iredo eq 0) then begin
    if(squares_redo eq 0) then begin
      parr0=parr                   ; keep track of tried squares
      parr_save=parr               ; keep track of original possibilities
    endif
    for i=0,5 do begin
      for j=0,5 do begin
        if(total(parr0[i,j,*]) gt 1) then begin ; save unfinished square indices
          ir=i
          jr=j
          max_redo=total(parr0[i,j,*])
          parr1=where(parr0[i,j,*] ne 0)+1 ; possible values
          goto,ind_saved
        endif
      endfor
    endfor
    ind_saved:
  endif
  iredo=iredo+1
  if(iredo le max_redo) then begin
    arr[ir,jr]=parr1[iredo-1]       ; new try
    parr[ir,jr,arr[ir,jr]-1]=0
;    print,ir,jr,arr[ir,jr]
    goto,redo
  endif else begin                  ; tried that square, move on
    squares_redo=squares_redo+1
    parr0[ir,jr,*]=0
    parr=parr_save
    print,'no more possibilities'
    sudoku_plot,arr,parr=parr
    read,'<cr> to continue',text
    goto,redo
  endelse
endif

if(count gt 0) then begin
  if(keyword_set(verbose)) then begin
    sudoku_plot,arr,parr=parr
    read,'<cr> to continue',text
  endif
  goto,fill_numbers
endif

end
