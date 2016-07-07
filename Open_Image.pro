;************************
;** PROGRAM OPEN_IMAGE **
;*******************************************************************
;** This program opens JPEG and PNG planispheres saved with PLIA. **
;** It cannot open polar projections. Better use HDF files.       **
;*******************************************************************

PRO Open_Image, event

  WIDGET_CONTROL, event.top, Get_UValue = info
  WIDGET_CONTROL, event.id,  Get_Value  = buttonValue

  path   = Info.Work_path
  Planet = ''

  Result = DIALOG_READ_IMAGE(FILE=file,FILTER_TYPE='.jpg , .png',     $
                             GET_PATH=path,IMAGE=Image,PATH=path,TITLE='Open Image')


  IF (Result EQ 0) THEN BEGIN
     PRINT,'Returning to Main Program'
     WIDGET_CONTROL, event.top, Set_UValue=info, /No_Copy
     RETURN
  ENDIF
  Info.Work_path = path

  size_plan    = SIZE(Image)
  sx = size_plan(1)
  sy = size_plan(2)

  WIDGET_CONTROL,/hourglass


; Extract Information
  file_name = STRSPLIT(file,'\',/EXTRACT)
  file_size = SIZE(file_name)
  file_name = file_name[file_size(1)-1]
  file_name = STRSPLIT(file_name,'.',/EXTRACT)
  name_size = SIZE(file_name)
  file_name = file_name[name_size(1)-2]
  imag_name = STRSPLIT(file_name,'_',/EXTRACT)


; "Depending on Planet" Settings
  Planet = '--'
  IF (STRMATCH(imag_name[0],'*Venus*'  ) EQ 1) THEN Planet='Venus'
  IF (STRMATCH(imag_name[0],'*Jupiter*') EQ 1) THEN Planet='Jupiter'
  IF (STRMATCH(imag_name[0],'*Saturn*' ) EQ 1) THEN Planet='Saturn'

  IF (Planet EQ 'Venus') THEN BEGIN
   ; Venus reference ellipsoid defined in the IAU 2000 standard (sphere with radius = 6051.8 ± 1.0 km)
   ; We add an atmospheric height corresponding to the clouds altitude
     Info.Radius_Equat = 6051.8 + 70.0
     Info.Radius_Polar = 6051.8 + 70.0
  ENDIF

  IF (Planet EQ 'Jupiter') THEN BEGIN
     Info.Radius_Equat = 71492.0
     Info.Radius_Polar = 66854.0
  ENDIF

  IF (Planet EQ 'Saturn') THEN BEGIN
     Info.Radius_Equat = 60268.0   ; Reference valid at 1 bar (Vasavada et al. 2005, Sanchez-Lavega et al. 2000)
     Info.Radius_Polar = 64364.0
   ; The Req and Rp at 100 mbar are Req=60367.0 and Rp=54438.0
  ENDIF

  IF (Planet EQ '')  OR  ( (Info.Planet NE '--') AND (Planet NE Info.Planet) )  THEN BEGIN
     Result = DIALOG_MESSAGE( 'Invalid Image Format' , /CENTER, TITLE='WARNING !!' )
     WIDGET_CONTROL, event.top, Set_UValue=info, /No_Copy
     RETURN
  ENDIF ELSE BEGIN
     Info.Planet = Planet
  ENDELSE




  Day    = FLOAT (STRTRIM(STRMID(imag_name[1],0,2)))
  Month  = FLOAT (STRTRIM(STRMID(imag_name[1],3,2)))
  Year   = FLOAT (STRTRIM(STRMID(imag_name[1],6,4)))
  Hour   = FLOAT (STRTRIM(STRMID(imag_name[2],0,2)))  ;11
  Minute = FLOAT (STRTRIM(STRMID(imag_name[2],3,2)))  ;14
  Second = FLOAT (STRTRIM(STRMID(imag_name[2],6,2)))  ;17

  Label  = Info.Planet + '  ' + STRCOMPRESS(STRING(FIX(Day))  + '/' + STRING(FIX(Month))  + '/' +     $
           STRING(FIX(Year)),/REMOVE_ALL) + '  '  +  STRING(FIX(Hour)) + ':' +                        $
           STRCOMPRESS(STRING(FIX(Minute)) + ':' + STRING(FIX(Second)),/REMOVE_ALL)
  Time   = JULDAY(Month,Day,Year,Hour,Minute,Second)   ;day*86400 + hour*3600 + minute*60 + second



; Define Resolution and Delta for template
  Resolution = 360./sx
  Resolution = ( ROUND(Resolution * 100.) ) / 100.
;  Delta      = Info.Box_Dim / Resolution

  Info.Resolution = Resolution
;  Info.Delta      = Delta


; Define arrays for Longitude and Latitude
  Longitude = FLTARR(sx,sy)
  Latitude  = FLTARR(sx,sy)
  FOR i=0,sx-1 DO Longitude[i,*]=360.0D*(i)/(sx-1)
  FOR j=0,sy-1 DO Latitude [*,j]=180.0D*(j)/(sy-1)
  Latitude = Latitude - 90.0D


; Determine Bits of the Image
  histogr = HISTOGRAM(Image)
  nonzero = WHERE(histogr,count)

  bit = 1.
  WHILE NOT ( (count GT 2.^(bit-1.)) AND (count LE 2.^bit) ) DO  bit = bit + 1.
  Bits = FLOAT(bit)

  IF (Bits GT 8. ) AND (Bits LE 16.) THEN  Bits_work = 16.
  IF (Bits LE 8. ) THEN Bits_work = 8.
  IF (Bits GT 16.) THEN BEGIN
     Bits_work = 16.
     Image     = BYTSCL(Image,/NAN,TOP=65535.)                 ; Scale image to a maximum of 16 bits
  ENDIF

  IF (Info.Bits LT Bits_work) THEN Info.Bits = Bits_work


  CASE buttonValue OF

       "Image in Left Window": BEGIN
          IF (Info.Is_Rght_Image EQ 'Yes') THEN BEGIN
           ; Comprobamos que las imágenes son del mismo tamaño
             IF ((sx NE Info.sx2) OR (sy NE Info.sy2)) THEN BEGIN
                PRINT,' Images have different size and cannot be measured!'
                PRINT,' Returning'
                RETURN
             ENDIF
          ENDIF
          Info.Is_Left_Image = 'Yes'
         *Info.Image1        = Image
         *Info.Image1_orig   = Image
          Info.sx1           = sx
          Info.sy1           = sy
         *Info.Lat1          = Latitude
         *Info.Lon1          = Longitude
         *Info.Lon1_orig     = Longitude
         *Info.Lat1_orig     = Latitude
          Info.File1_Name    = file_name
          Info.Label1        = Label
          Info.Time1         = Time
         *Info.Time_Vector1  = [Year,Month,Day,Hour,Minute,Second]
          Info.Bits1         = Bits
          Info.Process_Image = 'Left'

          WIDGET_CONTROL, Info.title_left   , SET_VALUE  = Label
          WIDGET_CONTROL, Info.window_left  , DRAW_XSIZE = sx , DRAW_YSIZE = sy
          WIDGET_CONTROL, Info.window_left  , GET_VALUE  = windowID
          WIDGET_CONTROL, Info.hist_left    , GET_VALUE  = histogramID
          WIDGET_CONTROL, Info.Bit_info_left, SET_VALUE  = 'Bits = ' + STRCOMPRESS(STRING(FIX(Bits)),/REMOVE_ALL)
       END

       "Image in Right Window": BEGIN
          IF (Info.Is_Left_Image EQ 'Yes') THEN BEGIN
           ; Comprobamos que las imágenes son del mismo tamaño
             IF ((sx NE Info.sx1) OR (sy NE Info.sy1)) THEN BEGIN
                PRINT,' Images have different size and cannot be measured!'
                PRINT,' Returning'
                RETURN
             ENDIF
          ENDIF
          Info.Is_Rght_Image = 'Yes'
         *Info.Image2        = SHIFT(Image,-10,-10)
         *Info.Image2_orig   = Image
          Info.sx2           = sx
          Info.sy2           = sy
         *Info.Lat2          = Latitude
         *Info.Lon2          = Longitude
         *Info.Lon1_orig     = Longitude
         *Info.Lat1_orig     = Latitude
          Info.File2_Name    = file_name
          Info.Label2        = Label
          Info.Time2         = Time
         *Info.Time_Vector2  = [Year,Month,Day,Hour,Minute,Second]
          Info.Bits2         = Bits
          Info.Process_Image = 'Right'

          WIDGET_CONTROL, Info.title_rght   , SET_VALUE  = Label
          WIDGET_CONTROL, Info.window_rght  , DRAW_XSIZE = sx , DRAW_YSIZE = sy
          WIDGET_CONTROL, Info.window_rght  , GET_VALUE  = windowID
          WIDGET_CONTROL, Info.hist_rght    , GET_VALUE  = histogramID
          WIDGET_CONTROL, Info.Bit_info_rght, SET_VALUE  = 'Bits = ' + STRCOMPRESS(STRING(FIX(Bits)),/REMOVE_ALL)
       END


  ENDCASE

  WSET, windowID
  IF (Bits GT 8) THEN TVIMAGE,BYTSCL(Image,/NAN) ELSE TVIMAGE,Image

  WSET, histogramID
  histogr   = HISTOGRAM(Image)
  Max_Value = FIX(2.^(Bits)-1.)
  POS=[0.05, 0.10, 0.95, 0.90]
  PLOT,HISTOGRAM(Image,MIN=0,MAX=Max_Value),/Ys,/Xs,Title='Histogram',           $
       YRange=[0,MAX( histogr[1:N_ELEMENTS(histogr)-2] )],XRange=[0,Max_Value],    $
       YTicks=4,YTickName=REPLICATE(' ',5),CHARSIZE=0.75,POSITION=POS


; RETURN after setting info values and destroying local variables
  WIDGET_CONTROL, event.top, Set_UValue=info, /No_Copy


END