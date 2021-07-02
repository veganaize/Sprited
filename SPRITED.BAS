' SpriteEd 0.21
DECLARE SUB GetMouseState ()
DECLARE SUB ResetMouse ()
DECLARE SUB ShowMouse ()
DECLARE SUB HideMouse ()
DECLARE SUB DrawBackground ()
DECLARE SUB SetupMouseRoutines ()

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

DIM SHARED MouseRoutine0$
DIM SHARED MouseRoutine1$
DIM SHARED MouseRoutine2$
DIM SHARED MouseRoutine3$

DIM SHARED ax AS INTEGER: ax = 0  'mouse sub function
DIM SHARED bx AS INTEGER: bx = 0  'mouse button status
DIM SHARED cx AS INTEGER: cx = 0  'mouse column (x co-ord)
DIM SHARED dx AS INTEGER: dx = 0  'mouse row    (y co-ord)

DIM SHARED lmbcolor AS INTEGER: lmbcolor = 3  '     LMB color
DIM SHARED rmbcolor AS INTEGER: rmbcolor = 15 '     RMB color
DIM SHARED kee AS STRING: kee = ""
DIM SHARED putArray(129) AS INTEGER

TYPE buttons
    x1 AS INTEGER
    x2 AS INTEGER
    y1 AS INTEGER
    y2 AS INTEGER
    clr AS INTEGER
    style AS STRING * 2
END TYPE
DIM SHARED button(31) AS buttons

DIM SHARED bsvArray(130) AS INTEGER
DIM SHARED pixelgrid(15, 15) AS INTEGER
DIM SHARED oldgrid(15, 15) AS INTEGER
DIM SHARED redogrid(15, 15) AS INTEGER


SCREEN 13
CLS

SetupMouseRoutines
ResetMouse
ShowMouse
HideMouse

DrawBackground
ErasePixelGrid
RefreshGrid
InitButtonColors
RefreshButtons

ShowMouse

'''Begin Main Loop'''
DO
    GetMouseState
    x! = cx / 2
    y! = dx
    COLOR lmbcolor

    '''OVER GRID?'''
    IF x! < (16 * 8) AND y! < (16 * 8) THEN
        LOCATE 19, 18: PRINT "true "
        LOCATE 18, 1: PRINT USING "###"; INT(x! / 8);
        LOCATE 18, 5: PRINT USING "###"; INT(y! / 8);
        LOCATE 18, 9: PRINT bx
        
        '''LMB DOWN OVER GRID?'''
        IF bx = 1 THEN
            x! = x! - (x! MOD 8 - 1)
            y! = y! - (y! MOD 8 - 1)
           
            IF lmbcolor = -1 THEN
                HideMouse
                LINE (x!, y!)-STEP(6, 6), 0, BF
                LINE (x! + 3, y! + 2)-STEP(1, 1), 8, BF
                ShowMouse
            ELSE
                HideMouse
                LINE (x!, y!)-STEP(6, 6), lmbcolor, BF
                ShowMouse
            END IF
           
            SaveGridState
            pixelgrid(INT(x! / 8), INT(y! / 8)) = lmbcolor
        'END IF  'LMB OVER GRID

        '''RMB DOWN OVER GRID?'''
        ELSEIF bx = 2 THEN
            x! = x! - (x! MOD 8 - 1)
            y! = y! - (y! MOD 8 - 1)
          
            IF rmbcolor = -1 THEN
                HideMouse
                LINE (x!, y!)-STEP(6, 6), 0, BF
                LINE (x! + 3, y! + 2)-STEP(1, 1), 8, BF
                ShowMouse
            ELSE
                HideMouse
                LINE (x!, y!)-STEP(6, 6), rmbcolor, BF
                ShowMouse
            END IF
          
            SaveGridState
            pixelgrid(INT(x! / 8), INT(y! / 8)) = rmbcolor
        END IF  'MB DOWN OVER GRID

    ELSE  'NOT OVER GRID
        LOCATE 19, 18: PRINT "false"
        LOCATE 18, 1: PRINT USING "###"; INT(x!);
        LOCATE 18, 5: PRINT USING "###"; INT(y!);
        LOCATE 18, 9: PRINT bx
    END IF 'OVER GRID

    '''LMB DOWN ?'''
    IF bx = 1 THEN
        ' *** Slow way? FOR LOOP -- use table instead **
        FOR i = 0 TO 16
            IF x! >= button(i).x1 AND x! <= button(i).x2 AND y! >= button(i%).y1 AND y! <= button(i).y2 THEN
                lmbcolor = button(i).clr
                RefreshButtons
            END IF
        NEXT i

    '''RMB DOWN ?'''
    ELSEIF bx = 2 THEN
        ' *** Slow way? FOR LOOP -- use table instead **
        FOR i = 0 TO 16
            IF x! >= button(i).x1 AND x! <= button(i).x2 AND y! >= button(i).y1 AND y! <= button(i).y2 THEN
                rmbcolor = button(i).clr
                RefreshButtons
            END IF
        NEXT i
   
    '''MMB DOWN ?'''
    ELSEIF bx = 4 THEN
        SYSTEM
    END IF 'MB DOWN

    kee = INKEY$

    IF kee = "L" OR kee = "l" THEN loadgrid '       "L"
    IF kee = "S" OR kee = "s" THEN savegrid '       "S"
    IF kee = "C" OR kee = "c" THEN ClearGrid'       "C"
    IF kee = "U" OR kee = "u" THEN undo     '       "U"
    IF kee = "B" OR kee = "b" THEN loadbsv  '       "B"
    IF kee = "X" OR kee = "x" THEN savebsv  '       "X"

LOOP UNTIL kee = CHR$(27) '                         End of Main Loop.

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
    HideMouse
   
    '''ERASE GRID'''
    FOR r = 0 TO 128 STEP 8
        FOR c = 0 TO 128 STEP 8
            LINE (c + 1, r + 1)-(c + 7, r + 7), 0, BF
        NEXT c
    NEXT r

    '''FILL W/ -1'''
    FOR r = 0 TO 15
        FOR c = 0 TO 15
            LINE (c * 8 + 4, r * 8 + 3)-STEP(1, 1), 8, BF 'Draw Boxes
        NEXT c
    NEXT r

    ShowMouse
END SUB

SUB DrawBackground

'''INITIAL GRID ANIMATION'''
FOR k = 0 TO 8
    mytime! = TIMER + .01
   
    '''ERASE PREVIOUS HORIZONTAL GRIDLINES'''
    FOR i = 0 TO 16
        LINE (0, i * (k - 1))-(16 * (k - 1), i * (k - 1)), 0
    NEXT i
   
    '''ERASE PREVIOUS VERTICAL GRIDLINES'''
    FOR i = 0 TO 16
        LINE (i * (k - 1), 0)-(i * (k - 1), 16 * (k - 1)), 0
    NEXT i

    '''DRAW HORIZONTAL GRIDLINES'''
    FOR i = 0 TO 16
        LINE (0, i * k)-(16 * k, i * k), 15
    NEXT i
   
    '''DRAW VERTICAL GRIDLINES'''
    FOR i = 0 TO 16
        LINE (i * k, 0)-(i * k, 16 * k), 15
    NEXT i

    WHILE TIMER < mytime!
    WEND
NEXT k

FOR i% = 0 TO 7
    '''Assign color to button's structure.'''
    button(i%).clr = i%
    button(i% + 8).clr = i% + 8
    button(i%).x1 = (20 * 7 + i% * 7)
    button(i% + 8).x1 = (20 * 7 + i% * 7)
    button(i%).x2 = button(i%).x1 + 7
    button(i% + 8).x2 = button(i% + 8).x1 + 7
    button(i%).y1 = (2 * 7)
    button(i% + 8).y1 = (2 * 7) + 8
    button(i%).y2 = (2 * 7 + 7)
    button(i% + 8).y2 = (2 * 7 + 7) + 8
    LINE (CSNG(button(i%).x1), CSNG(button(i%).y1))-(CSNG(button(i%).x2), CSNG(button(i%).y2)), i%, BF
    LINE (CSNG(button(i% + 8).x1), CSNG(button(i% + 8).y1))-(CSNG(button(i% + 8).x2), CSNG(button(i% + 8).y2)), i% + 8, BF
NEXT i%

LOCATE 19, 1: PRINT "mouse over grid:"

LOCATE 18, 4: PRINT ",";
LOCATE 18, 8: PRINT ",";

END SUB

SUB ErasePixelGrid
'''Paint grid with -1's'''
    SaveGridState
    FOR i = 0 TO 15
        FOR j = 0 TO 15
            pixelgrid(i, j) = -1
        NEXT j
    NEXT i
END SUB

SUB GetMouseState
    DEF SEG = VARSEG(MouseRoutine3$)
    CALL absolute(ax, bx, cx, dx, SADD(MouseRoutine3$))
    DEF SEG
END SUB

SUB HideMouse
    DEF SEG = VARSEG(MouseRoutine2$)
    CALL absolute(ax, bx, cx, dx, SADD(MouseRoutine2$))
    DEF SEG
END SUB

SUB InitButtonColors
    button(16).x1 = 128 + 7         ' Transparancy Button
    button(16).x2 = 128 + 21
    button(16).y1 = 64 + 7
    button(16).y2 = 64 + 21
    button(16).clr = -1
    button(16).style = "BF"
   
    button(17).x1 = 128 + 7         ' LMB Color
    button(17).x2 = 128 + 21
    button(17).y1 = 32 + 7
    button(17).y2 = 32 + 21
    button(17).clr = lmbcolor
    button(17).style = "BF"

    button(18).x1 = 128 + 7 + 14       ' RMB Color
    button(18).x2 = 128 + 21 + 14
    button(18).y1 = 32 + 7
    button(18).y2 = 32 + 21
    button(18).clr = rmbcolor
    button(18).style = "BF"
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
        FOR r = 0 TO 15
        FOR c = 0 TO 15
            GET #1, (r * 16 + c + 1), pixelgrid(c, r)
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
    LINE (button(16).x1, button(16).y1)-(button(16).x2, button(16).y2), 8, BF
    LINE (button(17).x1, button(17).y1)-(button(17).x2, button(17).y2), lmbcolor, BF
    LINE (button(18).x1, button(18).y1)-(button(18).x2, button(18).y2), rmbcolor, BF
    ShowMouse
END SUB

SUB RefreshGrid
    HideMouse

    '''ERASE GRID'''
    FOR r = 0 TO 128 STEP 8
        FOR c = 0 TO 128 STEP 8
            LINE (c + 1, r + 1)-(c + 7, r + 7), 0, BF
        NEXT c
    NEXT r

    '''DRAW IMAGE'''
    FOR r = 0 TO 15
        FOR c = 0 TO 15
            IF pixelgrid(c, r) = -1 THEN
                LINE (c * 8 + 4, r * 8 + 3)-STEP(1, 1), 8, BF
            ELSE
                LINE (c * 8 + 1, r * 8 + 1)-(c * 8 + 7, r * 8 + 7), pixelgrid(c, r), BF
            END IF
        NEXT c
    NEXT r

    ShowMouse
END SUB

DEFSNG A-Z
SUB ResetMouse
    DEF SEG = VARSEG(MouseRoutine0$)
    CALL absolute(ax, bx, cx, dx, SADD(MouseRoutine0$))
    DEF SEG
END SUB

DEFINT A-Z
SUB savebsv
    bsvArray(0) = 16 * 8 + 255
    bsvArray(1) = 16 + 255
  
    FOR r = 0 TO 15
        FOR c = 0 TO 15 STEP 2
            tv% = (pixelgrid(c, r) + &HFF) + pixelgrid(c + 1, r)
            bsvArray(r * 16 + 2 + c) = pixelgrid(c, r)
        NEXT c
    NEXT r
  
    DEF SEG = VARSEG(pixelgrid(0, 0))
        BSAVE filename$, VARPTR(bsvArray), 130
    '    BLOAD filename$, VARPTR(putArray(0))
    DEF SEG

    'PUT (160, 100), putArray(0), XOR

END SUB

SUB savegrid
    LOCATE 22, 2: INPUT "Save As: (8.3) "; filename$
   
    OPEN filename$ FOR RANDOM AS #1 LEN = 2
        FOR r = 0 TO 15
        FOR c = 0 TO 15
            PUT #1, (r * 16 + c + 1), pixelgrid(c, r)
        NEXT c
        NEXT r
    CLOSE #1
   
    LOCATE 22, 2: PRINT "                            ";
END SUB

SUB SaveGridState
FOR i = 0 TO 15
    FOR j = 0 TO 15
        oldgrid(j, i) = pixelgrid(j, i)
    NEXT j
NEXT i
END SUB

SUB SetupMouseRoutines
'''Installs machine language routines for mouse```
    FOR a% = 0 TO 3
        ml$ = ""
        ml$ = ml$ + CHR$(&H55)                          ' push bp
        ml$ = ml$ + CHR$(&H89) + CHR$(&HE5)             ' mov  bp, sp
        ml$ = ml$ + CHR$(&HB8) + CHR$(a%) + CHR$(&H0)   '   mov  ax, #
        ml$ = ml$ + CHR$(&HCD) + CHR$(&H33)             '   int  33
        ml$ = ml$ + CHR$(&H53)                          '   push bx
        ml$ = ml$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H6) '   mov  bx, [bp+6]
        ml$ = ml$ + CHR$(&H89) + CHR$(&H17)             '   mov  (bx), dx
        ml$ = ml$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H8) '   mov  bx, [bp+8]
        ml$ = ml$ + CHR$(&H89) + CHR$(&HF)              '   mov  [bx], cx
        ml$ = ml$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&HC) '   mov  bx, [bp+C]
        ml$ = ml$ + CHR$(&H89) + CHR$(&HF7)             '   mov  [bx], ax
        ml$ = ml$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&HA) '   mov  bx, [bp+A]
        ml$ = ml$ + CHR$(&H58)                          '   pop  ax
        ml$ = ml$ + CHR$(&H89) + CHR$(&H7)              '   mov  [bx], ax
        ml$ = ml$ + CHR$(&H5D)                          ' pop  bp
        ml$ = ml$ + CHR$(&HCA) + CHR$(&H8) + CHR$(&H0)  ' retf 8
    
        SELECT CASE a%
        CASE 0
            MouseRoutine0$ = ml$
        CASE 1
            MouseRoutine1$ = ml$
        CASE 2
            MouseRoutine2$ = ml$
        CASE 3
            MouseRoutine3$ = ml$
        END SELECT
    NEXT a%
END SUB

SUB ShowMouse
    DEF SEG = VARSEG(MouseRoutine1$)
    CALL absolute(ax, bx, cx, dx, SADD(MouseRoutine1$))
    DEF SEG
END SUB

SUB undo

FOR i = 0 TO 15
FOR j = 0 TO 15
    pixelgrid(j, i) = oldgrid(j, i)
NEXT j
NEXT i

HideMouse

FOR r = 0 TO 128 STEP 8     'ERASE GRID
    FOR c = 0 TO 128 STEP 8
        LINE (c + 1, r + 1)-(c + 7, r + 7), 0, BF
    NEXT c
NEXT r
FOR r = 0 TO 15     'DRAW IMAGE
    FOR c = 0 TO 15
        IF pixelgrid(c, r) = -1 THEN
            LINE (c * 8 + 4, r * 8 + 3)-STEP(1, 1), 8, BF
        ELSE
            LINE (c * 8 + 1, r * 8 + 1)-(c * 8 + 7, r * 8 + 7), pixelgrid(c, r), BF
        END IF
    NEXT c
NEXT r

ShowMouse
   
END SUB

