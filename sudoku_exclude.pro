pro sudoku_exclude,arr,parr

; exclude duplicate numbers in rows, columns

for i=0,8 do begin
  for j=0,8 do begin
    if(arr[i,j] ne 0) then begin
       parr[i,*,arr[i,j]-1]=0
       parr[*,j,arr[i,j]-1]=0
    endif
  endfor
endfor

; exclude duplicate numbers in small squares

for i0=0,6,3 do begin  ; horizontal small rectangles
  for j0=0,6,3 do begin  ; vertical small rectangles
    for i=i0,i0+2 do begin
      for j=j0,j0+2 do begin
        if(arr[i,j] ne 0) then begin
          parr[i0:i0+2,j0:j0+2,arr[i,j]-1]=0
        endif
      endfor
    endfor
  endfor
endfor

end
