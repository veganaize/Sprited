' SpriteEd 0.23 '
DECLARE SUB SaveToFile ()
DECLARE SUB GetMouseState ()
DECLARE SUB ResetMouse ()
DECLARE SUB ShowMouse ()
DECLARE SUB HideMouse ()
DECLARE SUB DrawBackground ()
DECLARE SUB InstMouseRoutine ()
DECLARE SUB savebsv ()
DECLARE SUB loadbsv ()
DECLARE SUB SaveGridState ()
DECLARE SUB undo ()
DECLARE SUB InitButtonColors ()
DECLARE SUB RefreshButtons ()
DECLARE SUB ClearGrid ()
DECLARE SUB ErasePixelGrid ()
DECLARE SUB RefreshGrid ()
DECLARE SUB loadgrid ()
DECLARE SUB savegrid ()

DEFINT A-Z
ON ERROR GOTO errcode
    
TYPE button
    x1 AS INTEGER
    x2 AS INTEGER
    y1 AS INTEGER
    y2 AS INTEGER
    clr AS INTEGER
    style AS STRING * 2
END TYPE
DIM SHARED buttons(31) AS button

TYPE imagetype
    high AS INTEGER
    wide AS INTEGER
END TYPE
DIM SHARED image AS imagetype
image.high = 16
image.wide = 16

DIM SHARED gCellSize:  gCellSize = 8

TYPE mousetype
    x AS INTEGER
    y AS INTEGER
    lmb AS INTEGER
    rmb AS INTEGER
    mmb AS INTEGER
END TYPE
DIM SHARED mouse AS mousetype

DIM SHARED MouseResetShowHideRoutine AS STRING
DIM SHARED MouseStateRoutine AS STRING
DIM SHARED lmbcolor AS INTEGER: lmbcolor = 3  '     LMB color
DIM SHARED rmbcolor AS INTEGER: rmbcolor = 15 '     RMB color
DIM SHARED kee AS STRING: kee = ""
DIM SHARED putArray(129) AS INTEGER


DIM SHARED bsvArray(130) AS INTEGER
DIM SHARED pixelgrid(image.high - 1, image.wide - 1) AS INTEGER
DIM SHARED oldgrid(image.high - 1, image.wide - 1) AS INTEGER
DIM SHARED redogrid(image.high - 1, image.wide - 1) AS INTEGER


SCREEN 13
CLS

InstMouseRoutine
ResetMouse
ShowMouse

DrawBackground
ErasePixelGrid
RefreshGrid
InitButtonColors
RefreshButtons


'''Begin Main Loop'''
DO
    GetMouseState
    COLOR lmbcolor

    '''OVER GRID?'''
    IF mouse.x < (16 * gCellSize) AND mouse.y < (16 * gCellSize) THEN
        LOCATE 19, 18: PRINT "true "
        LOCATE 18, 1: PRINT USING "###"; INT(mouse.x / gCellSize);
        LOCATE 18, 5: PRINT USING "###"; INT(mouse.y / gCellSize);
        LOCATE 18, 9: PRINT "  ";
        LOCATE 18, 11: PRINT USING "#"; mouse.lmb;
        LOCATE 18, 12: PRINT USING "#"; mouse.mmb;
        LOCATE 18, 13: PRINT USING "#"; mouse.rmb
       
        
        '''LMB DOWN OVER GRID?'''
        IF mouse.lmb = 1 THEN
            mouse.x = mouse.x - (mouse.x MOD gCellSize - 1)
            mouse.y = mouse.y - (mouse.y MOD gCellSize - 1)
           
            IF lmbcolor = -1 THEN
                HideMouse
                    LINE (mouse.x, mouse.y)-STEP(6, 6), 0, BF
                    LINE (mouse.x + 3, mouse.y + 2)-STEP(1, 1), 8, BF
                ShowMouse
            ELSE
                HideMouse
                    LINE (mouse.x, mouse.y)-STEP(6, 6), lmbcolor, BF
                ShowMouse
            END IF
           
            SaveGridState
            pixelgrid(INT(mouse.y / gCellSize), INT(mouse.x / gCellSize)) = lmbcolor
        'END IF  'LMB OVER GRID

        '''RMB DOWN OVER GRID?'''
        ELSEIF mouse.rmb = 1 THEN
            mouse.x = mouse.x - (mouse.x MOD gCellSize - 1)
            mouse.y = mouse.y - (mouse.y MOD gCellSize - 1)
          
            IF rmbcolor = -1 THEN
                HideMouse
                    LINE (mouse.x, mouse.y)-STEP(6, 6), 0, BF
                    LINE (mouse.x + 3, mouse.y + 2)-STEP(1, 1), 8, BF
                ShowMouse
            ELSE
                HideMouse
                    LINE (mouse.x, mouse.y)-STEP(6, 6), rmbcolor, BF
                ShowMouse
            END IF
          
            SaveGridState
            pixelgrid(INT(mouse.y / gCellSize), INT(mouse.x / gCellSize)) = rmbcolor
        END IF  'MB DOWN OVER GRID

    ELSE  'NOT OVER GRID
        LOCATE 19, 18: PRINT "false"
        LOCATE 18, 1: PRINT USING "###"; INT(mouse.x);
        LOCATE 18, 5: PRINT USING "###"; INT(mouse.y);
        LOCATE 18, 9: PRINT "  ";
        LOCATE 18, 11: PRINT USING "#"; INT(mouse.lmb);
        LOCATE 18, 12: PRINT USING "#"; INT(mouse.mmb);
        LOCATE 18, 13: PRINT USING "#"; INT(mouse.rmb)
 
    END IF 'OVER GRID

    '''LMB DOWN ?'''
    IF mouse.lmb = 1 THEN
        ' *** Slow way? FOR LOOP -- use table instead **
        FOR i = 0 TO 16
            IF mouse.x >= buttons(i).x1 AND mouse.x <= buttons(i).x2 AND mouse.y >= buttons(i%).y1 AND mouse.y <= buttons(i).y2 THEN
                lmbcolor = buttons(i).clr
                RefreshButtons
            END IF
        NEXT i

    '''RMB DOWN ?'''
    ELSEIF mouse.rmb = 1 THEN
        ' *** Slow way? FOR LOOP -- use table instead **
        FOR i = 0 TO 16
            IF mouse.x >= buttons(i).x1 AND mouse.x <= buttons(i).x2 AND mouse.y >= buttons(i).y1 AND mouse.y <= buttons(i).y2 THEN
                rmbcolor = buttons(i).clr
                RefreshButtons
            END IF
        NEXT i
   
    '''MMB DOWN ?'''
    ELSEIF mouse.mmb = 1 THEN
        SYSTEM
    END IF 'MB DOWN

    kee = INKEY$
    IF kee <> "" THEN
        IF kee = "l" OR kee = "L" THEN
            loadgrid
        ELSEIF kee = "s" OR kee = "S" THEN
            SaveToFile
        ELSEIF kee = "c" OR kee = "C" THEN
            ClearGrid
        ELSEIF kee = "u" OR kee = "U" THEN
            undo
        ELSEIF kee = "b" OR kee = "B" THEN
            loadbsv
        ELSEIF kee = "x" OR kee = "X" THEN
            savebsv
        END IF
    END IF

LOOP UNTIL kee = CHR$(27) 'Main Loop.

SYSTEM

errcode:
    'PRINT "Error "; ERR; " on line "; ER
    SELECT CASE ERR
        CASE 53
            LOCATE 22, 2: PRINT "File Not Found          ";
            LOCATE 23, 2: PRINT "press any key..."
            WHILE INKEY$ = "": WEND
            LOCATE 22, 2: PRINT "                  ";
            LOCATE 23, 2: PRINT "                   ";
            'LOCATE 23, 22: PRINT "error";
    END SELECT
RESUME NEXT

SUB ClearGrid

    '''ERASE GRID'''
    FOR r = 0 TO 128 STEP 8
        FOR c = 0 TO 128 STEP 8
            HideMouse
                LINE (c + 1, r + 1)-(c + 7, r + 7), 0, BF
            ShowMouse
        NEXT c
    NEXT r

    '''FILL W/ -1'''
    FOR r = 0 TO image.high - 1
        FOR c = 0 TO image.high - 1
            HideMouse
                LINE (c * gCellSize + 4, r * gCellSize + 3)-STEP(1, 1), 8, BF 'Draw Boxes
            ShowMouse
        NEXT c
    NEXT r

END SUB

SUB DrawBackground
    '''INITIAL GRID ANIMATION'''
    FOR k = 0 TO gCellSize
        mytime! = TIMER + .01
       
        '''ERASE PREVIOUS HORIZONTAL GRIDLINES'''
        FOR r = 0 TO image.high
            LINE (0, r * (k - 1))-(image.wide * (k - 1), r * (k - 1)), 0
        NEXT r
       
        '''ERASE PREVIOUS VERTICAL GRIDLINES'''
        FOR c = 0 TO image.wide
             LINE (c * (k - 1), 0)-(c * (k - 1), image.high * (k - 1)), 0
        NEXT c

        '''DRAW HORIZONTAL GRIDLINES'''
        FOR r = 0 TO image.high
             LINE (0, r * k)-(image.wide * k, r * k), 8
        NEXT r
       
        '''DRAW VERTICAL GRIDLINES'''
        FOR c = 0 TO image.wide
             LINE (c * k, 0)-(c * k, image.high * k), 8
        NEXT c

        WHILE TIMER < mytime!
        WEND
    NEXT k

    FOR i% = 0 TO 7
        '''Assign color to button's structure.'''
        buttons(i%).clr = i%
        buttons(i% + 8).clr = i% + 8
        buttons(i%).x1 = (20 * 7 + i% * 7)
        buttons(i% + 8).x1 = (20 * 7 + i% * 7)
        buttons(i%).x2 = buttons(i%).x1 + 7
        buttons(i% + 8).x2 = buttons(i% + 8).x1 + 7
        buttons(i%).y1 = (2 * 7)
        buttons(i% + 8).y1 = (2 * 7) + 8
        buttons(i%).y2 = (2 * 7 + 7)
        buttons(i% + 8).y2 = (2 * 7 + 7) + 8
        LINE (CSNG(buttons(i%).x1), CSNG(buttons(i%).y1))-(CSNG(buttons(i%).x2), CSNG(buttons(i%).y2)), i%, BF
        LINE (CSNG(buttons(i% + 8).x1), CSNG(buttons(i% + 8).y1))-(CSNG(buttons(i% + 8).x2), CSNG(buttons(i% + 8).y2)), i% + 8, BF
    NEXT i%

    LOCATE 19, 1: PRINT "mouse over grid:"

    LOCATE 18, 4: PRINT ",";
    LOCATE 18, 8: PRINT ",";
END SUB

SUB ErasePixelGrid
'''Fill grid with -1'''
    SaveGridState
    FOR r = 0 TO image.high - 1
        FOR c = 0 TO image.wide - 1
            pixelgrid(r, c) = -1
        NEXT c
    NEXT r
END SUB

SUB GetMouseState
   
    DIM buttonState AS INTEGER
   
    DEF SEG = VARSEG(MouseStateRoutine)
        CALL ABSOLUTE(3, buttonState, mouse.x, mouse.y, SADD(MouseStateRoutine))
    DEF SEG

    mouse.x = mouse.x / 2

    SELECT CASE buttonState
    CASE 0
        mouse.lmb = 0
        mouse.rmb = 0
        mouse.mmb = 0
    CASE 1
        mouse.lmb = 1
        mouse.rmb = 0
        mouse.mmb = 0
    CASE 2
        mouse.lmb = 0
        mouse.rmb = 1
        mouse.mmb = 0
    CASE 3
        mouse.lmb = 1
        mouse.rmb = 1
        mouse.mmb = 0
    CASE 4
        mouse.lmb = 0
        mouse.rmb = 0
        mouse.mmb = 1
    CASE 5
        mouse.lmb = 1
        mouse.rmb = 0
        mouse.mmb = 1
    CASE 6
        mouse.lmb = 0
        mouse.rmb = 1
        mouse.mmb = 1
    CASE 7
        mouse.lmb = 1
        mouse.rmb = 1
        mouse.mmb = 1
    END SELECT

END SUB

SUB HideMouse
    DEF SEG = VARSEG(MouseResetShowHideRoutine)
        CALL ABSOLUTE(2, SADD(MouseResetShowHideRoutine))
    DEF SEG
END SUB

SUB InitButtonColors
    buttons(16).x1 = 128 + 7         ' Transparancy Button
    buttons(16).x2 = 128 + 21
    buttons(16).y1 = 64 + 7
    buttons(16).y2 = 64 + 21
    buttons(16).clr = -1
    buttons(16).style = "BF"
   
    buttons(17).x1 = 128 + 7         ' LMB Color
    buttons(17).x2 = 128 + 21
    buttons(17).y1 = 32 + 7
    buttons(17).y2 = 32 + 21
    buttons(17).clr = lmbcolor
    buttons(17).style = "BF"

    buttons(18).x1 = 128 + 7 + 14       ' RMB Color
    buttons(18).x2 = 128 + 21 + 14
    buttons(18).y1 = 32 + 7
    buttons(18).y2 = 32 + 21
    buttons(18).clr = rmbcolor
    buttons(18).style = "BF"
END SUB

SUB InstMouseRoutine
    ml$ = ""
    ml$ = ml$ + CHR$(&H55)                          ' push bp
    ml$ = ml$ + CHR$(&H89) + CHR$(&HE5)             ' mov  bp, sp
    ml$ = ml$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H6) '   mov bx, [bp+6]
    ml$ = ml$ + CHR$(&H8B) + CHR$(&H7)              '   mov ax, [bx]
    ml$ = ml$ + CHR$(&HCD) + CHR$(&H33)             '   int  33
    ml$ = ml$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H6) '   mov bx, [bp+6]
    ml$ = ml$ + CHR$(&H89) + CHR$(&H7)              '   mov [bx], ax
    ml$ = ml$ + CHR$(&H5D)                          ' pop bp
    ml$ = ml$ + CHR$(&HCA) + CHR$(&H2) + CHR$(&H0)  ' retf 2
    MouseResetShowHideRoutine = ml$

    ml$ = ""
    ml$ = ml$ + CHR$(&H55)                          ' push bp
    ml$ = ml$ + CHR$(&H89) + CHR$(&HE5)             ' mov  bp, sp
    ml$ = ml$ + CHR$(&HB8) + CHR$(&H3) + CHR$(&H0)  '   mov ax, 3
    ml$ = ml$ + CHR$(&HCD) + CHR$(&H33)             '   int  33
    ml$ = ml$ + CHR$(&H53)                          '   push bx
    ml$ = ml$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H6) '   mov  bx, [bp+6]
    ml$ = ml$ + CHR$(&H89) + CHR$(&H17)             '   mov  [bx], dx
    ml$ = ml$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H8) '   mov  bx, [bp+8]
    ml$ = ml$ + CHR$(&H89) + CHR$(&HF)              '   mov  [bx], cx
    ml$ = ml$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&HC) '   mov  bx, [bp+C]
    ml$ = ml$ + CHR$(&H89) + CHR$(&HF7)             '   mov  [bx], ax
    ml$ = ml$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&HA) '   mov  bx, [bp+A]
    ml$ = ml$ + CHR$(&H58)                          '   pop  ax
    ml$ = ml$ + CHR$(&H89) + CHR$(&H7)              '   mov  [bx], ax
    ml$ = ml$ + CHR$(&H5D)                          ' pop  bp
    ml$ = ml$ + CHR$(&HCA) + CHR$(&H8) + CHR$(&H0)  ' retf 8
    MouseStateRoutine = ml$
END SUB

SUB loadbsv
    LOCATE 22, 2: PRINT "                        ";
    LOCATE 22, 2: INPUT "Load File"; filename$
    SaveGridState

    DEF SEG = VARSEG(pixelgrid(0, 0))
        BLOAD filename$, VARPTR(pixelgrid(0, 0))
    DEF SEG
    LOCATE 22, 2: PRINT "                      ";
    RefreshGrid

END SUB

SUB loadgrid
    LOCATE 22, 2: PRINT "                        ";
    LOCATE 22, 2: INPUT "Load File"; filename$
    SaveGridState

    OPEN filename$ FOR RANDOM AS #1 LEN = 2
        FOR r = 0 TO image.high - 1
            FOR c = 0 TO image.wide - 1
                GET #1, (r * 16 + c + 1), pixelgrid(r, c)
            NEXT c
        NEXT r
    CLOSE #1

    'DEF SEG = VARSEG(pixelgrid(0, 0))
    '    ON ERROR GOTO errcode
    '    BLOAD filename$, VARPTR(pixelgrid(0, 0))
    'DEF SEG
    LOCATE 22, 2: PRINT "                      ";
    RefreshGrid
END SUB

SUB RefreshButtons
    HideMouse
    LINE (buttons(16).x1, buttons(16).y1)-(buttons(16).x2, buttons(16).y2), 8, BF
    LINE (buttons(17).x1, buttons(17).y1)-(buttons(17).x2, buttons(17).y2), lmbcolor, BF
    LINE (buttons(18).x1, buttons(18).y1)-(buttons(18).x2, buttons(18).y2), rmbcolor, BF
    ShowMouse
END SUB

SUB RefreshGrid

    '''ERASE GRID'''
    FOR r = 0 TO 128 STEP 8
        FOR c = 0 TO 128 STEP 8
            HideMouse
                LINE (c + 1, r + 1)-(c + 7, r + 7), 0, BF
            ShowMouse
        NEXT c
    NEXT r

    '''DRAW IMAGE'''
    FOR r = 0 TO image.high - 1
        FOR c = 0 TO image.high - 1
            IF pixelgrid(r, c) = -1 THEN
                HideMouse
                    LINE (c * 8 + 4, r * 8 + 3)-STEP(1, 1), 8, BF
                ShowMouse
            ELSE
                HideMouse
                    LINE (c * 8 + 1, r * 8 + 1)-(c * 8 + 7, r * 8 + 7), pixelgrid(r, c), BF
                ShowMouse
            END IF
        NEXT c
    NEXT r

END SUB

DEFSNG A-Z
SUB ResetMouse
    DEF SEG = VARSEG(MouseResetShowHideRoutine)
        CALL ABSOLUTE(0, SADD(MouseResetShowHideRoutine))
    DEF SEG
END SUB

DEFINT A-Z
SUB savebsv
    bsvArray(0) = 16 * 8 + 255
    bsvArray(1) = 16 + 255
  
    FOR r = 0 TO image.high - 1
        FOR c = 0 TO image.wide - 1 STEP 2
            tv% = (pixelgrid(r, c) + &HFF) + pixelgrid(r, c + 1)
            bsvArray(r * 16 + 2 + c) = pixelgrid(r, c)
        NEXT c
    NEXT r
  
    DEF SEG = VARSEG(pixelgrid(0, 0))
        BSAVE filename$, VARPTR(bsvArray), 130
    '    BLOAD filename$, VARPTR(putArray(0))
    DEF SEG

    'PUT (160, 100), putArray(0), XOR

END SUB

SUB SaveGridState
   
    FOR row = 0 TO image.high - 1
        FOR col = 0 TO image.wide - 1
            oldgrid(row, col) = pixelgrid(row, col)
        NEXT col
    NEXT row

END SUB

SUB SaveToFile
    LOCATE 22, 2: INPUT "Save As: (8.3) "; filename$
   
    OPEN filename$ FOR RANDOM AS #1 LEN = 2
        FOR row = 0 TO image.high - 1
            FOR col = 0 TO image.wide - 1
                PUT #1, (row * (image.high - 1) + col), pixelgrid(row, col)
            NEXT col
        NEXT row
    CLOSE #1
   
    LOCATE 22, 2: PRINT "                            ";
END SUB

SUB ShowMouse
    DEF SEG = VARSEG(MouseResetShowHideRoutine)
        CALL ABSOLUTE(1, SADD(MouseResetShowHideRoutine))
    DEF SEG
END SUB

SUB undo

FOR r = 0 TO image.high - 1
    FOR c = 0 TO image.wide - 1
        pixelgrid(r, c) = oldgrid(r, c)
    NEXT c
NEXT r

FOR r = 0 TO image.high * 8 STEP 8   'ERASE GRID
    FOR c = 0 TO image.wide * 8 STEP 8
        HideMouse
            LINE (c + 1, r + 1)-(c + 7, r + 7), 0, BF
        ShowMouse
    NEXT c
NEXT r

FOR r = 0 TO image.high - 1   'DRAW IMAGE
    FOR c = 0 TO image.wide - 1
        IF pixelgrid(r, c) = -1 THEN
            HideMouse
                LINE (c * 8 + 4, r * 8 + 3)-STEP(1, 1), 8, BF
            ShowMouse
        ELSE
            HideMouse
                LINE (c * 8 + 1, r * 8 + 1)-(c * 8 + 7, r * 8 + 7), pixelgrid(r, c), BF
            ShowMouse
        END IF
    NEXT c
NEXT r

END SUB

