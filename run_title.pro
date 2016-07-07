function run_title,ext
;+
;   run_title,ext
;
;            returns short title for run
;
;-
run=fix(ext)
case run of
  193: title=' Slit 132'
  194: title=' Slit 110'
  195: title=' h4 helios'
  196: title=' Slit 132, M+H'
  197: title=' Slit 132, L'
  198: title=' Slit 110, M'
  199: title=' Slit 030'
  200: title=' Slit 110, L'
  201: title=' Slit 110, H'
  202: title=' Slit 149'
  203: title=' Slit 110, M+H'
  204: title=' Slit 132'
  205: title=' Slit 149, L'
  206: title=' Slit 030, H'
  207: title=' Smooth Vp'
  208: title=' Slit 030, new Vp'
  209: title=' Slit 110, L+M'
  210: title=' Slit 110, L+H'
  211: title=' Slit 030, L+M'
  212: title=' Slit 030, 3rd Vp'
  300: title=' Sinus, P=180, amp=0.025'
  301: title=' Sinus, P=180, amp=0.0025'
  302: title=' Sinus, P=120, amp=0.0025'
  303: title=' Sinus, P= 60, amp=0.0025'
  304: title=' Sinus, P= 60, amp=0.00125'
  305: title=' Sinus, P=120, amp=0.00125'
  306: title=' Sinus, P=120, amp=0.00125, z1(0)=2.3Mm'
  307: title=' Sinus, P= 60, amp=0.0001, fixed grid'
  308: title=' Sinus, P= 60, amp=0.0001'
  309: title=' Sinus, P= 60, amp=0.0001, wv=1,1'
  310: title=' Sinus, P= 60, amp=0.0001, wv=6,2'
  311: title=' Sinus, P= 60, amp=0.0001, wv=12,4'
  312: title=' Sinus, P= 60, amp=0.00125, wv=30,10'
  313: title=' Sinus, P= 60, amp=0.0001, wv=30,10, z1(0)=2.3Mm'
  314: title=' Sinus, P=300, amp=0.0001, wv=30,10'
  315: title=' Sinus, P=300, amp=0.0001, fixed grid'
  316: title=' Sinus, P= 60, amp=0.0001, wv=30,10'
  317: title=' Flat frequency spectrum 5.07-10.1 mHz, fixed grid'
  else: title=' Run '+strtrim(string(run),2)
endcase

return,title
end
