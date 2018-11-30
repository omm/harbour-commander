/* Copyright 2017-2018 Rafał Jopek ( rafaljopek at hotmail com ) */

/* Harbour Commander */

#include "box.ch"
#include "directry.ch"
#include "fileio.ch"
#include "hbgtinfo.ch"
#include "inkey.ch"
#include "setcurs.ch"

#define _HC_nTop        1
#define _HC_nLeft       2
#define _HC_nBottom     3
#define _HC_nRight      4
#define _HC_cDir        5
#define _HC_nArray      6
#define _HC_nRowBar     7
#define _HC_nRowNo      8
#define _HC_cComdLine   9
#define _HC_nComdCol    10
#define _HC_nColNo      11

#define _HC_nElements   11

/* FError() */
#define _HC_Meaning     2

STATIC aPanelLeft
STATIC aPanelRight
STATIC aPanelSelect

PROCEDURE Main()

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )
   Set( _SET_SCOREBOARD, .F. )
   Set( _SET_EVENTMASK, hb_bitOr( INKEY_KEYBOARD, HB_INKEY_GTEVENT ) )
   Set( _SET_INSERT, .T. )

   /* Setup input CP of the translation */
   hb_cdpSelect( "UTF8EX" )
   hb_gtInfo( HB_GTI_COMPATBUFFER, .F. )
   hb_gtInfo( HB_GTI_BOXCP, hb_cdpSelect() )

   /* Configure terminal and OS codepage */
   hb_SetTermCP( hb_cdpTerm() )
   Set( _SET_OSCODEPAGE, hb_cdpOS() )
   Set( _SET_DBCODEPAGE, "EN" )  /* do not switch RDD CP to UTF-8 till it's fully operational */

   hb_gtInfo( HB_GTI_RESIZEMODE, HB_GTI_RESIZEMODE_ROWS )
   hb_gtInfo( HB_GTI_WINTITLE, "Harbour Commander" )

   aPanelLeft := PanelInit()
   aPanelRight := PanelInit()

   /* hb_cwd() zwraca pełny bieżący katalog roboczy zawierający dysk nPos końcowy separator ścieżki */
   PanelFetchList( aPanelLeft, hb_cwd() )
   PanelFetchList( aPanelRight, hb_cwd() )

   AutoSize()

   aPanelSelect := aPanelLeft

   Prompt()

   hb_Scroll()
   SetPos( 0, 0 )

   RETURN

STATIC FUNCTION PanelInit()

   LOCAL aInit

   aInit := Array( _HC_nElements )

   aInit[ _HC_nTop      ] := 0
   aInit[ _HC_nLeft     ] := 0
   aInit[ _HC_nBottom   ] := 0
   aInit[ _HC_nRight    ] := 0
   aInit[ _HC_cDir      ] := ""
   aInit[ _HC_nArray    ] := {}
   aInit[ _HC_nRowBar   ] := 1
   aInit[ _HC_nRowNo    ] := 0
   aInit[ _HC_cComdLine ] := ""
   aInit[ _HC_nComdCol  ] := 0
   aInit[ _HC_nColNo    ] := 0

   RETURN aInit

STATIC PROCEDURE PanelFetchList( aPanel, cDir )

   aPanel[ _HC_cDir ] := hb_defaultValue( cDir, hb_cwd() )
   aPanel[ _HC_nArray ] := hb_vfDirectory( aPanel[ _HC_cDir ], "HSD" )

   hb_ADel( aPanel[ _HC_nArray ], 1, .T. )
   ASort( aPanel[ _HC_nArray ], 2, , {| x, y | x[ F_NAME ] < y[ F_NAME ] .OR. x[ F_ATTR ] > y[ F_ATTR ] } )

   RETURN

STATIC PROCEDURE AutoSize()

   Resize( aPanelLeft, 0, 0, MaxRow() - 2, MaxCol() / 2 )
   Resize( aPanelRight, 0, MaxCol() / 2 + 1, MaxRow() - 2, MaxCol() )

   RETURN

STATIC PROCEDURE Resize( aPanel, nTop, nLeft, nBottom, nRight )

   aPanel[ _HC_nTop    ] := nTop
   aPanel[ _HC_nLeft   ] := nLeft
   aPanel[ _HC_nBottom ] := nBottom
   aPanel[ _HC_nRight  ] := nRight

   RETURN

STATIC PROCEDURE Prompt()

   LOCAL lContinue := .T.
   LOCAL nMaxRow := 0, nMaxCol := 0
   LOCAL nKey, nKeyStd
   LOCAL cNewDir
   LOCAL nError
   LOCAL aError := { ;
      { 0,  "Successful" }, ;
      { 2,  "File not found" }, ;
      { 3,  "Path not found" }, ;
      { 4,  "Too many files open" }, ;
      { 5,  "Access denied" }, ;
      { 6,  "Invalid handle" }, ;
      { 8,  "Insufficient memory" }, ;
      { 15, "Invalid drive specified" }, ;
      { 19, "Attempted to write to a write-protected disk" }, ;
      { 21, "Drive not ready" }, ;
      { 23, "Data CRC error" }, ;
      { 29, "Write fault" }, ;
      { 30, "Read fault" }, ;
      { 32, "Sharing violation" }, ;
      { 33, "Lock Violation" } }

   DO WHILE lContinue

      DispBegin()

      IF nMaxRow != MaxRow() .OR. nMaxCol != MaxCol()

         hb_Scroll()
         AutoSize()

         PanelDisplay( aPanelLeft )
         PanelDisplay( aPanelRight )

         ComdLineDisplay( aPanelSelect )

         BottomBar()

         nMaxRow := MaxRow()
         nMaxCol := MaxCol()
      ENDIF

      DispEnd()

      nKey := Inkey( 0, hb_bitOr( Set( _SET_EVENTMASK, INKEY_ALL ), HB_INKEY_EXT ) )
      nKeyStd := hb_keyStd( nKey )

      SWITCH nKeyStd

      CASE K_ESC
         lContinue := .F.
         EXIT

      CASE K_ENTER

         ChangeDir()

         EXIT

      CASE K_TAB
         IF aPanelSelect == aPanelLeft
            aPanelSelect := aPanelRight
         ELSE
            aPanelSelect := aPanelLeft
         ENDIF
         PanelDisplay( aPanelLeft )
         PanelDisplay( aPanelRight )
         EXIT

      CASE K_UP
         IF aPanelSelect[ _HC_nRowBar ] > 1
            --aPanelSelect[ _HC_nRowBar ]
         ELSE
            IF aPanelSelect[ _HC_nRowNo ] >= 1
               --aPanelSelect[ _HC_nRowNo ]
            ENDIF
         ENDIF
         PanelDisplay( aPanelSelect )
         EXIT

      CASE K_DOWN
         IF aPanelSelect[ _HC_nRowBar ] < aPanelSelect[ _HC_nBottom ] - 1 .AND. aPanelSelect[ _HC_nRowBar ] <= Len( aPanelSelect[ _HC_nArray ] ) - 1
            ++aPanelSelect[ _HC_nRowBar ]
         ELSE
            IF aPanelSelect[ _HC_nRowNo ] + aPanelSelect[ _HC_nRowBar ] <= Len( aPanelSelect[ _HC_nArray ] ) - 1
               ++aPanelSelect[ _HC_nRowNo ]
            ENDIF
         ENDIF
         PanelDisplay( aPanelSelect )
         EXIT

      CASE K_LEFT

         IF aPanelSelect[ _HC_nComdCol ] > 0
            aPanelSelect[ _HC_nComdCol ]--
         ELSE
            IF aPanelSelect[ _HC_nColNo ] >= 1
               aPanelSelect[ _HC_nColNo ]--
            ENDIF
         ENDIF

         ComdLineDisplay( aPanelSelect )

         EXIT

      CASE K_RIGHT

         IF aPanelSelect[ _HC_nComdCol ] < 57 .AND. aPanelSelect[ _HC_nComdCol ] <= Len( aPanelSelect[ _HC_cComdLine ] )
            aPanelSelect[ _HC_nComdCol ]++
         ELSE
            IF aPanelSelect[ _HC_nColNo ] + aPanelSelect[ _HC_nComdCol ] <= Len( aPanelSelect[ _HC_cComdLine ] )
               aPanelSelect[ _HC_nColNo ]++
            ENDIF
         ENDIF

         ComdLineDisplay( aPanelSelect )

         EXIT

      CASE K_DEL

         IF aPanelSelect[ _HC_nComdCol ] >= 0
            aPanelSelect[ _HC_cComdLine ] := Stuff( aPanelSelect[ _HC_cComdLine ], aPanelSelect[ _HC_nComdCol ] + 1, 1, "" )
         ENDIF

         ComdLineDisplay( aPanelSelect )

         EXIT

      CASE K_BS

         IF aPanelSelect[ _HC_nComdCol ] > 0
            aPanelSelect[ _HC_cComdLine ] := Stuff( aPanelSelect[ _HC_cComdLine ], aPanelSelect[ _HC_nComdCol ], 1, "" )
            aPanelSelect[ _HC_nComdCol ]--
         ENDIF

         ComdLineDisplay( aPanelSelect )

         EXIT

      CASE K_F3

         HCView( aPanelSelect )

         EXIT

      CASE K_F7

         cNewDir := MsgBox( "Create the directory", { "Yes", "No!" } )
         IF hb_DirCreate( cNewDir ) == F_ERROR
            IF ( nError := AScan( aError, {| x | x[ 1 ] == FError() } ) ) > 0
               hb_Alert( "Cannot make directory, error: ; ;" + aError[ nError ][ _HC_Meaning ],, 0x70 )
            ELSE
               /* Wyświetl numer błędu, którego nie ma w tablicy błędu */
               hb_Alert( "Cannot make directory, error: ; ;" + hb_ntos( FError() ),, 0x70 )
            ENDIF
         ELSE

            PanelFetchList( aPanelLeft, hb_cwd() )
            PanelFetchList( aPanelRight, hb_cwd() )

            PanelDisplay( aPanelLeft )
            PanelDisplay( aPanelRight )

         ENDIF

         EXIT

      OTHERWISE

         IF ( nKeyStd >= 32 .AND. nKeyStd <= 126 ) .OR. ( nKeyStd >= 160 .AND. nKeyStd <= 255 ) .OR. ! hb_keyChar( nKeyStd ) == ""

            aPanelSelect[ _HC_cComdLine ] := Stuff( aPanelSelect[ _HC_cComdLine ], aPanelSelect[ _HC_nComdCol ] + 1, 0, hb_keyChar( nKeyStd ) )
            aPanelSelect[ _HC_nComdCol ]++

            ComdLineDisplay( aPanelSelect )

         ENDIF

      ENDSWITCH

   ENDDO

   RETURN

STATIC PROCEDURE PanelDisplay( aPanel )

   LOCAL nRow, nPos := 1
   LOCAL nLengthName := 0, nLengthSize := 0

   AScan( aPanelSelect[ _HC_nArray ], {| x | ;
      nLengthName := Max( nLengthName, Len( x[ 1 ] ) ), ;
      nLengthSize := Max( nLengthSize, Len( Str( x[ 2 ] ) ) ) } )

   DispBegin()
   hb_DispBox( aPanel[ _HC_nTop ], aPanel[ _HC_nLeft ], aPanel[ _HC_nBottom ], aPanel[ _HC_nRight ], HB_B_SINGLE_UNI + " ", 0x17 )

   nPos += aPanel[ _HC_nRowNo ]
   FOR nRow := aPanel[ _HC_nTop ] + 1 TO aPanel[ _HC_nBottom ] - 1

      IF nPos <= Len( aPanel[ _HC_nArray ] )
         hb_DispOutAt( nRow, aPanel[ _HC_nLeft ] + 1, ;
            PadR( Expression( ;
            nLengthName, nLengthSize, ;
            aPanel[ _HC_nArray ][ nPos ][ F_NAME ], ;
            aPanel[ _HC_nArray ][ nPos ][ F_SIZE ], ;
            aPanel[ _HC_nArray ][ nPos ][ F_DATE ], ;
            aPanel[ _HC_nArray ][ nPos ][ F_TIME ], ;
            aPanel[ _HC_nArray ][ nPos ][ F_ATTR ] ), ;
            aPanel[ _HC_nRight ] - aPanel[ _HC_nLeft ] - 1 ), ;
            iif( aPanelSelect == aPanel .AND. nPos == aPanel[ _HC_nRowBar ] + aPanel[ _HC_nRowNo ], 0x30, ColoringSyntax( ;
            aPanel[ _HC_nArray ][ nPos ][ F_ATTR ] ) ) )
         ++nPos
      ELSE
         EXIT
      ENDIF

   NEXT

   DispEnd()

   RETURN

STATIC PROCEDURE ComdLineDisplay( aPanel )

   LOCAL nMaxRow := MaxRow(), nMaxCol := MaxCol()

   DispBegin()

   hb_DispOutAt( nMaxRow - 1, 0, PadR( aPanel[ _HC_cComdLine ], nMaxCol + 1 ) )

   SetPos( nMaxRow - 1, aPanel[ _HC_nComdCol ] )

   DispEnd()

   RETURN

STATIC FUNCTION Expression( nLengthName, nLengthSize, aExp1, aExp2, aExp3, aExp4, aExp5 )

   LOCAL cSubString1, cSubString2, cSubString3, cSubString4, cSubString5

   cSubString1 := PadR( aExp1 + Space( nLengthName ), nLengthName ) + " "

   IF aExp1 == ".."
      cSubString1 := PadR( "[" + AllTrim( cSubString1 ) + "]" + Space( nLengthName ), nLengthName ) + " "
   ENDIF

   IF aExp5 == "D" .OR. aExp5 == "HD" .OR. aExp5 == "HSD" .OR. aExp5 == "HSDL" .OR. aExp5 == "RHSA" .OR. aExp5 == "RD"
      cSubString2 := PadL( "DIR", nLengthSize + 3 ) + " "
   ELSE
      cSubString2 := PadL( Transform( aExp2, "9 999 999 999" ), nLengthSize + 3 ) + " "
   ENDIF

   cSubString3 := DToC( aExp3 ) + " "
   cSubString4 := aExp4 + " "
   cSubString5 := PadL( aExp5, 3 )

   RETURN cSubString1 + cSubString2 + cSubString3 + cSubString4 + cSubString5

STATIC FUNCTION ColoringSyntax( aExp5 )

   LOCAL nColor

   IF aExp5 == "HD" .OR. aExp5 == "HSD" .OR. aExp5 == "HSDL" .OR. aExp5 == "RHSA" .OR. aExp5 == "RD"
      nColor := 0x1d
   ELSE
      nColor := 0x17
   ENDIF

   RETURN nColor

STATIC PROCEDURE ChangeDir()

   LOCAL nPos, cDir, cDir0
   LOCAL nPosLast

   nPos := aPanelSelect[ _HC_nRowBar ] + aPanelSelect[ _HC_nRowNo ]
   IF At( "D", aPanelSelect[ _HC_nArray ][ nPos ][ F_ATTR ] ) == 0
      RETURN
   ENDIF
   IF aPanelSelect[ _HC_nArray ][ nPos ][ F_NAME ] == ".."
      cDir := aPanelSelect[ _HC_cDir ]
      cDir0 := SubStr( cDir, RAt( hb_ps(), Left( cDir, Len( cDir ) - 1 ) ) + 1 )
      cDir0 := SubStr( cDir0, 1, Len( cDir0 ) - 1 )
      cDir  := Left( cDir, RAt( hb_ps(), Left( cDir, Len( cDir ) - 1 ) ) )
      PanelFetchlist( aPanelSelect, cDir )
      nPosLast := Max( AScan( aPanelSelect[ _HC_nArray ], {| x | x[ F_NAME ] = cDir0 } ), 1 )

      IF nPosLast > aPanelSelect[ _HC_nBottom ] - 1

         aPanelSelect[ _HC_nRowNo ] := nPosLast

         DO WHILE .T.
            aPanelSelect[ _HC_nRowNo ] -= ( aPanelSelect[ _HC_nBottom ] - 1 )
            IF aPanelSelect[ _HC_nRowNo ] < aPanelSelect[ _HC_nBottom ] - 1
               EXIT
            ELSE
               aPanelSelect[ _HC_nRowNo ]  := 0
               aPanelSelect[ _HC_nRowBar ] := nPosLast
            ENDIF
         ENDDO

         aPanelSelect[ _HC_nRowBar ] := aPanelSelect[ _HC_nBottom ] - 1

      ENDIF

   ELSE
      cDir := aPanelSelect[ _HC_cDir ] + aPanelSelect[ _HC_nArray ][ nPos ][ F_NAME ] + hb_ps()
      aPanelSelect[ _HC_nRowBar ] := 1
      aPanelSelect[ _HC_nRowNo  ] := 0
      PanelFetchlist( aPanelSelect, cDir )
   ENDIF
   PanelDisplay( aPanelSelect )

   RETURN

STATIC PROCEDURE BottomBar()

   LOCAL nRow := MaxRow()
   LOCAL cSpaces
   LOCAL nCol := Int( MaxCol() / 10 ) + 1

   cSpaces := Space( nCol - 8 )

   hb_DispOutAt( nRow, 0,        " 1", 0x7 )
   hb_DispOutAt( nRow, 2,            "Help  " + cSpaces, 0x30 )
   hb_DispOutAt( nRow, nCol,     " 2", 0x7 )
   hb_DispOutAt( nRow, nCol + 2,     "Menu  " + cSpaces, 0x30 )
   hb_DispOutAt( nRow, nCol * 2, " 3", 0x7 )
   hb_DispOutAt( nRow, nCol * 2 + 2, "View  " + cSpaces, 0x30 )
   hb_DispOutAt( nRow, nCol * 3, " 4", 0x7 )
   hb_DispOutAt( nRow, nCol * 3 + 2, "Edit  " + cSpaces, 0x30 )
   hb_DispOutAt( nRow, nCol * 4, " 5", 0x7 )
   hb_DispOutAt( nRow, nCol * 4 + 2, "Copy  " + cSpaces, 0x30 )
   hb_DispOutAt( nRow, nCol * 5, " 6", 0x7 )
   hb_DispOutAt( nRow, nCol * 5 + 2, "RenMov" + cSpaces, 0x30 )
   hb_DispOutAt( nRow, nCol * 6, " 7", 0x7 )
   hb_DispOutAt( nRow, nCol * 6 + 2, "MkDir " + cSpaces, 0x30 )
   hb_DispOutAt( nRow, nCol * 7, " 8", 0x7 )
   hb_DispOutAt( nRow, nCol * 7 + 2, "Delete" + cSpaces, 0x30 )
   hb_DispOutAt( nRow, nCol * 8, " 9", 0x7 )
   hb_DispOutAt( nRow, nCol * 8 + 2, "PullDn" + cSpaces, 0x30 )
   hb_DispOutAt( nRow, nCol * 9, "10", 0x7 )
   hb_DispOutAt( nRow, nCol * 9 + 2, "Quit  " + cSpaces, 0x30 )

   RETURN

STATIC PROCEDURE HCView( aPanel )

   LOCAL nPos
   LOCAL cString, aString
   LOCAL lContinue := .T.
   LOCAL nMaxRow := 0, nMaxCol := 0
   LOCAL nRow := 1, nCol := 0, nNextRow := 0
   LOCAL nKey, nKeyStd
   LOCAL nRowOld, nColOld
   LOCAL cScreen

   nRowOld := Row()
   nColOld := Col()
   cScreen := SaveScreen( 0, 0, MaxRow(), MaxCol() )

   nPos := aPanel[ _HC_nRowBar ] + aPanel[ _HC_nRowNo ]
   IF At( "D", aPanel[ _HC_nArray ][ nPos ][ F_ATTR ] ) == 0

      IF ( cString := hb_MemoRead( aPanel[ _HC_nArray ][ nPos ][ 1 ] ) ) == ""
         hb_Alert( "Error reading: " + aPanel[ _HC_nArray ][ nPos ][ 1 ] )
         RETURN
      ELSE

         aString := hb_ATokens( cString, .T. )

         DO WHILE lContinue

            IF nMaxRow != MaxRow() .OR. nMaxCol != MaxCol()
               nMaxRow := MaxRow()
               nMaxCol := MaxCol()

               IF nRow > nMaxRow - 1
                  nRow := nMaxRow - 1
               ENDIF

               StringDisplay( aPanel, aString, nRow, nCol, nNextRow )

            ENDIF

            nKey := Inkey( 0, hb_bitOr( Set( _SET_EVENTMASK, INKEY_ALL ), HB_INKEY_EXT ) )

            nKeyStd := hb_keyStd( nKey )

            SWITCH nKeyStd

            CASE K_ESC
               lContinue := .F.
               EXIT

            CASE K_LBUTTONDOWN

               IF MRow() > 0 .AND. MRow() < MaxRow() .AND. MCol() > 0 .AND. MCol() < MaxCol() .AND. MRow() < Len( aString ) + 1
                  nRow := MRow()
                  nCol := MCol()
               ENDIF

               StringDisplay( aPanel, aString, nRow, nCol, nNextRow )
               EXIT

            CASE K_MWFORWARD

               IF nNextRow >= 1
                  nNextRow--
               ENDIF

               StringDisplay( aPanel, aString, nRow, nCol, nNextRow )
               EXIT

            CASE K_MWBACKWARD

               IF nNextRow < Len( aString ) - 1
                  nNextRow++
               ENDIF

               StringDisplay( aPanel, aString, nRow, nCol, nNextRow )
               EXIT

            CASE K_UP

               IF nRow > 1
                  nRow--
               ELSE
                  IF nNextRow >= 1
                     nNextRow--
                  ENDIF
               ENDIF

               StringDisplay( aPanel, aString, nRow, nCol, nNextRow )
               EXIT

            CASE K_LEFT

               IF nCol > 1
                  nCol--
               ENDIF

               StringDisplay( aPanel, aString, nRow, nCol, nNextRow )
               EXIT

            CASE K_DOWN

               IF nRow < nMaxRow - 1 .AND. nRow + nNextRow < Len( aString )
                  nRow++
               ELSE
                  IF nRow + nNextRow < Len( aString )
                     nNextRow++
                  ENDIF
               ENDIF

               StringDisplay( aPanel, aString, nRow, nCol, nNextRow )
               EXIT

            CASE K_RIGHT

               nCol++

               StringDisplay( aPanel, aString, nRow, nCol, nNextRow )
               EXIT

            OTHERWISE

            ENDSWITCH

         ENDDO

      ENDIF

   ELSE
      RETURN
   ENDIF

   RestScreen( 0, 0, MaxRow(), MaxCol(), cScreen )
   SetPos( nRowOld, nColOld )

   RETURN

STATIC PROCEDURE StringDisplay( aPanel, aString, nRow, nCol, nNextRow )

   LOCAL i
   LOCAL nMaxRow := MaxRow(), nMaxCol := MaxCol()
   LOCAL nLine
   LOCAL nPos

   nPos := aPanel[ _HC_nRowBar ] + aPanel[ _HC_nRowNo ]

   DispBegin()
   hb_Scroll()

   hb_DispOutAt( 0, 0, ;
      PadR( aPanel[ _HC_nArray ][ nPos ][ 1 ], nMaxCol + 1 ), 0x30 )

   FOR i := 1 TO nMaxRow

      nLine := i + nNextRow

      IF nLine <= Len( aString )
         hb_DispOutAt( i, 0, ;
            PadR( aString[ nLine ], nMaxCol + 1 ), ;
            iif( i == nRow, 0x70, 0x7 ) )
      ELSE
         hb_Scroll( i, 0, nMaxRow, nMaxCol + 1 )
         hb_DispOutAt( i, 1, ">> EOF <<", 0x01 )
         EXIT
      ENDIF

   NEXT

   hb_DispOutAt( nMaxRow, 0, ;
      PadR( " Row(" + hb_ntos( nRow + nNextRow ) + ") Col(" + hb_ntos( nCol ) + ")", nMaxCol + 1 ), 0x30 )

   DispEnd()

   SetPos( nRow, nCol )

   RETURN
