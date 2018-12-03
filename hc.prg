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
#define MEANING         2

STATIC aPanelLeft
STATIC aPanelRight
STATIC aPanelSelect

PROCEDURE Main()

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )
   Set( _SET_SCOREBOARD, .F. )
   Set( _SET_EVENTMASK, hb_bitOr( INKEY_KEYBOARD, HB_INKEY_GTEVENT, INKEY_ALL ) )
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

   /* hb_cwd() zwraca pełny bieżący katalog roboczy zawierający dysk i końcowy separator ścieżki */
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

   hb_ADel( aPanel[ _HC_nArray ], AScan( aPanel[ _HC_nArray ], {| x | x[ F_NAME ] == "." } ), .T. )
   ASort( aPanel[ _HC_nArray ],,, {| x, y | ( ISDIR( cDir + hb_ps() + x[ F_NAME ] ) == ISDIR( cDir + hb_ps() + y[ F_NAME ] ) .AND. ( x[ F_NAME ] < y[ F_NAME ] ) ) .OR. ;
      ( ISDIR( cDir + hb_ps() + x[ F_NAME ] ) .AND. ! ISDIR( cDir + hb_ps() + y[ F_NAME ] ) ) } )

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
   LOCAL nPos
   LOCAL cFileName
   LOCAL nHandle
   LOCAL nFError
   LOCAL aFError := { ;
      { 0,  "The operation completed successfully." }, ;
      { 2,  "The system cannot find the file specified." }, ;
      { 3,  "The system cannot find the path specified." }, ;
      { 4,  "The system cannot open the file." }, ;
      { 5,  "Access is denied." }, ;
      { 6,  "The handle is invalid." }, ;
      { 8,  "Not enough storage is available to process this command." }, ;
      { 15, "The system cannot find the drive specified." }, ;
      { 18, "There are no more files." }, ;
      { 19, "Attempted to write to a write-protected disk." }, ;
      { 21, "The device is not ready." }, ;
      { 23, "Data error (cyclic redundancy check)." }, ;
      { 29, "The system cannot write to the specified device." }, ;
      { 30, "The system cannot read from the specified device." }, ;
      { 32, "The process cannot access the file because it is being used by another process." }, ;
      { 33, "The process cannot access the file because another process has locked a portion of the file." } }

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

         nPos := aPanelSelect[ _HC_nRowBar ] + aPanelSelect[ _HC_nRowNo ]
         IF Empty( aPanelSelect[ _HC_cComdLine ] )
            IF At( "D", aPanelSelect[ _HC_nArray ][ nPos ][ F_ATTR ] ) == 0 
                  hb_run( aPanelSelect[ _HC_nArray ][ nPos ][ F_NAME ] )
               ELSE
                  ChangeDir( aPanelSelect )
            ENDIF
         ELSE

            hb_run( aPanelSelect[ _HC_cComdLine ] )
            aPanelSelect[ _HC_cComdLine ] := NIL

         ENDIF

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

         IF aPanelSelect[ _HC_nComdCol ] < nMaxCol .AND. aPanelSelect[ _HC_nComdCol ] < Len( aPanelSelect[ _HC_cComdLine ] )
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

         HCViewer( aPanelSelect, .F. )

         EXIT

      CASE K_F4

         HCViewer( aPanelSelect, .T. )

         EXIT


      CASE K_F6

         EXIT

      CASE K_F7

         cNewDir := MsgBox( "Create the directory.", { "Yes", "No!" } )
         IF ( nFError := hb_DirCreate( cNewDir ) ) == 0

            PanelFetchList( aPanelLeft, hb_cwd() )
            PanelFetchList( aPanelRight, hb_cwd() )

            PanelDisplay( aPanelLeft )
            PanelDisplay( aPanelRight )

         ELSE
            IF ( nFError := AScan( aFError, {| x | x[ 1 ] == nFError } ) ) > 0
               hb_Alert( "Cannot make directory, error: ; ;" + aFError[ nFError ][ MEANING ],, 0x70 )
            ELSE
               /* Wyświetl numer błędu, którego nie ma w tablicy błędu */
               hb_Alert( "Cannot make directory, error: ; ;" + hb_ntos( FError() ),, 0x70 )
            ENDIF
         ENDIF

         EXIT

      CASE K_F8

         nPos := aPanelSelect[ _HC_nRowBar ] + aPanelSelect[ _HC_nRowNo ]
         IF aPanelSelect[ _HC_nArray ][ nPos ][ F_NAME ] == ".."
            hb_Alert( "The item to be deleted has not been selected.",, 0x70 )
         ELSE
            nPos := aPanelSelect[ _HC_nRowBar ] + aPanelSelect[ _HC_nRowNo ]
            IF At( "D", aPanelSelect[ _HC_nArray ][ nPos ][ F_ATTR ] ) == 0
               IF hb_Alert( "Do you really want to delete the selected file ;" + aPanelSelect[ _HC_nArray ][ nPos ][ F_NAME ], { "Yes", "No!" }, 0x70 ) == 1
                  IF FErase( aPanelSelect[ _HC_nArray ][ nPos ][ F_NAME ] ) == F_ERROR
                     IF ( nFError := AScan( aFError, {| x | x[ 1 ] == FError() } ) ) > 0
                        hb_Alert( "File erase error: ; ;" + aFError[ nFError ][ MEANING ],, 0x70 )
                     ELSE
                        hb_Alert( "File erase error: ; ;" + hb_ntos( FError() ),, 0x70 )
                     ENDIF
                  ELSE

                     PanelFetchList( aPanelLeft, hb_cwd() )
                     PanelFetchList( aPanelRight, hb_cwd() )

                     PanelDisplay( aPanelLeft )
                     PanelDisplay( aPanelRight )

                  ENDIF
               ENDIF
            ELSE
               IF hb_Alert( "Do you really want to delete the selected directory ;" + aPanelSelect[ _HC_nArray ][ nPos ][ F_NAME ], { "Yes", "No!" }, 0x70 ) == 1
                  IF ( nFError := hb_DirDelete( aPanelSelect[ _HC_nArray ][ nPos ][ F_NAME ] ) ) == 0

                     PanelFetchList( aPanelLeft, hb_cwd() )
                     PanelFetchList( aPanelRight, hb_cwd() )

                     PanelDisplay( aPanelLeft )
                     PanelDisplay( aPanelRight )

                  ELSE
                     IF ( nFError := AScan( aFError, {| x | x[ 1 ] == nFError } ) ) > 0
                        hb_Alert( "Cannot make directory, error: ; ;" + aFError[ nFError ][ MEANING ],, 0x70 )
                     ELSE
                        hb_Alert( "Cannot make directory, error: ; ;" + hb_ntos( FError() ),, 0x70 )
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDIF

         EXIT

      CASE K_SH_F4

         cFileName := MsgBox( "Create file." )
         IF nHandle := ( FCreate( cFileName, FC_NORMAL ) ) == F_ERROR

            IF ( nFError := AScan( aFError, {| x | x[ 1 ] == FError() } ) ) > 0
               hb_Alert( "File cannot be created: ; ;" + aFError[ nFError ][ MEANING ],, 0x70 )
            ELSE
               hb_Alert( "File cannot be created: ; ;" + hb_ntos( FError() ),, 0x70 )
            ENDIF
         ELSE
            IF ! FClose( nHandle )
               IF ( nFError := AScan( aFError, {| x | x[ 1 ] == FError() } ) ) > 0
                  hb_Alert( "File cannot be created: ; ;" + aFError[ nFError ][ MEANING ],, 0x70 )
               ELSE
                  hb_Alert( "File cannot be created: ; ;" + hb_ntos( FError() ),, 0x70 )
               ENDIF
            ENDIF

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

   AScan( aPanel[ _HC_nArray ], {| x | ;
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

   LOCAL cStr1, cStr2, cStr3, cStr4, cStr5

   cStr1 := PadR( aExp1 + Space( nLengthName ), nLengthName ) + " "

   IF aExp1 == ".."
      cStr1 := PadR( "[" + AllTrim( cStr1 ) + "]" + Space( nLengthName ), nLengthName ) + " "
   ENDIF

   IF aExp5 == "D" .OR. aExp5 == "HD" .OR. aExp5 == "HSD" .OR. aExp5 == "HSDL" .OR. aExp5 == "RHSA" .OR. aExp5 == "RD"
      cStr2 := PadL( "DIR", nLengthSize + 3 ) + " "
   ELSE
      cStr2 := PadL( Transform( aExp2, "9 999 999 999" ), nLengthSize + 3 ) + " "
   ENDIF

   cStr3 := DToC( aExp3 ) + " "
   cStr4 := aExp4 + " "
   cStr5 := PadL( aExp5, 3 )

   RETURN cStr1 + cStr2 + cStr3 + cStr4 + cStr5

STATIC FUNCTION ColoringSyntax( aExp5 )

   LOCAL nColor

   IF aExp5 == "HD" .OR. aExp5 == "HSD" .OR. aExp5 == "HSDL" .OR. aExp5 == "RHSA" .OR. aExp5 == "RD"
      nColor := 0x1d
   ELSE
      nColor := 0x17
   ENDIF

   RETURN nColor

STATIC PROCEDURE ChangeDir( aPanel )

   LOCAL nPos, cDir, cDir0
   LOCAL nPosLast

   nPos := aPanel[ _HC_nRowBar ] + aPanel[ _HC_nRowNo ]
   IF At( "D", aPanel[ _HC_nArray ][ nPos ][ F_ATTR ] ) == 0
      RETURN
   ENDIF
   IF aPanel[ _HC_nArray ][ nPos ][ F_NAME ] == ".."
      cDir := aPanel[ _HC_cDir ]
      cDir0 := SubStr( cDir, RAt( hb_ps(), Left( cDir, Len( cDir ) - 1 ) ) + 1 )
      cDir0 := SubStr( cDir0, 1, Len( cDir0 ) - 1 )
      cDir  := Left( cDir, RAt( hb_ps(), Left( cDir, Len( cDir ) - 1 ) ) )
      PanelFetchlist( aPanel, cDir )
      nPosLast := Max( AScan( aPanel[ _HC_nArray ], {| x | x[ F_NAME ] = cDir0 } ), 1 )

      IF nPosLast > aPanel[ _HC_nBottom ] - 1

         aPanel[ _HC_nRowNo ] := nPosLast

         DO WHILE .T.
            aPanel[ _HC_nRowNo ] -= ( aPanel[ _HC_nBottom ] - 1 )
            IF aPanel[ _HC_nRowNo ] < aPanel[ _HC_nBottom ] - 1
               EXIT
            ELSE
               aPanel[ _HC_nRowNo ]  := 0
               aPanel[ _HC_nRowBar ] := nPosLast
            ENDIF
         ENDDO

         aPanel[ _HC_nRowBar ] := aPanel[ _HC_nBottom ] - 1

      ENDIF

   ELSE
      cDir := aPanel[ _HC_cDir ] + aPanel[ _HC_nArray ][ nPos ][ F_NAME ] + hb_ps()
      aPanel[ _HC_nRowBar ] := 1
      aPanel[ _HC_nRowNo  ] := 0
      PanelFetchlist( aPanel, cDir )
   ENDIF

   PanelDisplay( aPanel )

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

STATIC FUNCTION MsgBox( cMessage, aOptions )

   LOCAL nMaxRow := 0, nMaxCol := 0
   LOCAL cScreen
   LOCAL aOptionsOK := {}, aPosButtons
   LOCAL lContinue := .T.
   LOCAL i
   LOCAL nChoice := 1
   LOCAL nOpWidth, nWidth, nInitCol, expValue
   LOCAL nRowOld, nColOld
   LOCAL nKey, nKeyStd
   LOCAL cString := ""
   LOCAL nCol := 0, nColNo := 0

   nRowOld := Row()
   nColOld := Col()

   FOR EACH i IN hb_defaultValue( aOptions, {} )
      IF HB_ISSTRING( i ) .AND. ! i == ""
         AAdd( aOptionsOK, i )
      ENDIF
   NEXT

   IF Len( aOptionsOK ) == 0
      aOptionsOK := { "Ok" }
   ENDIF

   DO WHILE lContinue

      DispBegin()
      IF nMaxRow != Int( MaxRow() / 2 ) .OR. nMaxCol != Int( MaxCol() / 2 )

         nMaxRow := Int( MaxRow() / 2 )
         nMaxCol := Int( MaxCol() / 2 )

         aPosButtons := {}
         nOpWidth := 0

         cScreen := SaveScreen( nMaxRow - 2, nMaxCol - 30, nMaxRow + 3, nMaxCol + 30 )

         AEval( aOptionsOK, {| x | nOpWidth += Len( x ) + 4 } )

         nWidth := nOpWidth + 2
         nInitCol := Int( ( ( MaxCol() - ( nWidth + 2 ) ) / 2 ) + 0.5 )
         expValue := nInitCol + Int( ( nWidth - nOpWidth ) / 2 ) + 2
         AEval( aOptionsOK, {| x | AAdd( aPosButtons, expValue ), expValue += Len( x ) + 4 } )

         hb_DispBox( nMaxRow - 2, nMaxCol - 30, nMaxRow + 3, nMaxCol + 30, HB_B_SINGLE_UNI + " ", 0x70 )
         hb_DispOutAt( nMaxRow - 1, nMaxCol - 28, cMessage, 0x70 )

         FOR i := 1 TO Len( aOptionsOK )
            hb_DispOutAt( nMaxRow + 2, aPosButtons[ i ], " " + aOptionsOK[ i ] + " ", iif( i == nChoice, 0x07, 0x70 ) )
         NEXT

         MsgBoxDisplay( cString, nCol, nColNo )

      ENDIF
      DispEnd()

      nKey := Inkey( 0, hb_bitOr( Set( _SET_EVENTMASK, INKEY_ALL ), HB_INKEY_EXT ) )
      nKeyStd := hb_keyStd( nKey )

      SWITCH nKeyStd
      CASE K_ESC
         lContinue := .F.
         nChoice := 0
         EXIT

      CASE K_ENTER
         lContinue := .F.
         EXIT

      CASE K_F1
         EXIT

      CASE K_F10
         lContinue := .F.
         nChoice := 0
         EXIT

      CASE K_LEFT

         IF nCol > 0
            nCol--
         ELSE
            IF nColNo >= 1
               nColNo--
            ENDIF
         ENDIF

         MsgBoxDisplay( cString, nCol, nColNo )

         EXIT

      CASE K_RIGHT

         IF nCol < 56 .AND. nCol < Len( cString )
            nCol++
         ELSE
            IF nColNo + nCol < Len( cString )
               nColNo++
            ENDIF
         ENDIF

         MsgBoxDisplay( cString, nCol, nColNo )

         EXIT

      CASE K_DEL

         IF nCol >= 0
            cString := Stuff( cString, nCol + 1, 1, "" )
         ENDIF

         MsgBoxDisplay( cString, nCol, nColNo )

         EXIT

      CASE K_BS

         IF nCol > 0
            cString := Stuff( cString, nCol, 1, "" )
            nCol--
         ENDIF

         MsgBoxDisplay( cString, nCol, nColNo )

         EXIT

      CASE K_TAB
         IF Len( aOptionsOK ) > 1
            nChoice++
            IF nChoice > Len( aOptionsOK )
               nChoice := 1
            ENDIF
         ENDIF

         FOR i := 1 TO Len( aOptionsOK )
            hb_DispOutAt( nMaxRow + 2, aPosButtons[ i ], " " + aOptionsOK[ i ] + " ", iif( i == nChoice, 0x07, 0x70 ) )
         NEXT

         EXIT

      OTHERWISE

         IF ( nKeyStd >= 32 .AND. nKeyStd <= 126 ) .OR. ( nKeyStd >= 160 .AND. nKeyStd <= 255 ) .OR. ! hb_keyChar( nKeyStd ) == ""

            cString := Stuff( cString, nCol + nColNo + 1, 0, hb_keyChar( nKeyStd ) )
            IF nCol < 56
               nCol++
            ELSE
               nColNo++
            ENDIF
            MsgBoxDisplay( cString, nCol, nColNo )

         ENDIF

      ENDSWITCH

   ENDDO

   RestScreen( nMaxRow - 2, nMaxCol - 30, nMaxRow + 3, nMaxCol + 30, cScreen )
   SetPos( nRowOld, nColOld )

   RETURN iif( nChoice == 1, cString, 0 )

STATIC PROCEDURE MsgBoxDisplay( cString, nCol, nColNo )

   LOCAL nMaxRow := Int( MaxRow() / 2 ), nMaxCol := Int( MaxCol() / 2 )

   DispBegin()

   hb_DispOutAt( nMaxRow, nMaxCol - 28, PadR( SubStr( cString, 1 + nColNo, 57 + nColNo ), 57 ) )

   SetPos( nMaxRow, nMaxCol - 28 + nCol )

   DispEnd()

   RETURN

STATIC PROCEDURE HCViewer( aPanel, lArg )

   LOCAL nPos
   LOCAL cFileName
   LOCAL cString
   LOCAL aString
   LOCAL lContinue := .T.
   LOCAL nMaxRow := 0, nMaxCol := 0
   LOCAL nRow := 1, nCol := 0, nRowNo := 0, nColNo := 0
   LOCAL cStringEditingRow
   LOCAL cSubString
   LOCAL lToggleInsert := .F.
   LOCAL nKey, nKeyStd
   LOCAL nRowOld, nColOld
   LOCAL cScreen

   nRowOld := Row()
   nColOld := Col()
   cScreen := SaveScreen( 0, 0, MaxRow(), MaxCol() )

   nPos := aPanel[ _HC_nRowBar ] + aPanel[ _HC_nRowNo ]
   IF At( "D", aPanel[ _HC_nArray ][ nPos ][ F_ATTR ] ) == 0

      /* Returns the contents of a text file as a character string. */
      cFileName := aPanel[ _HC_nArray ][ nPos ][ F_NAME ]
      IF ( cString := hb_MemoRead( cFileName ) ) == ""
         hb_Alert( "Error reading: " + cFileName )
         RETURN
      ELSE

         /* A character array, filled with the individual tokens found. */
         aString := hb_ATokens( cString, .T. )

         DO WHILE lContinue

            IF nMaxRow != MaxRow() .OR. nMaxCol != MaxCol()
               nMaxRow := MaxRow()
               nMaxCol := MaxCol()

               IF nRow > nMaxRow - 1
                  nRow := nMaxRow - 1
               ENDIF

               StringDisplay( aPanel, aString, nRow, nCol, nRowNo )

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

               StringDisplay( aPanel, aString, nRow, nCol, nRowNo )
               EXIT

            CASE K_MWFORWARD

               IF nRowNo >= 1
                  nRowNo--
               ENDIF

               StringDisplay( aPanel, aString, nRow, nCol, nRowNo )
               EXIT

            CASE K_MWBACKWARD

               IF nRow + nRowNo < Len( aString )
                  nRowNo++
               ENDIF

               StringDisplay( aPanel, aString, nRow, nCol, nRowNo )
               EXIT

            CASE K_UP

               IF nRow > 1
                  nRow--
               ELSE
                  IF nRowNo >= 1
                     nRowNo--
                  ENDIF
               ENDIF

               IF aString[ nRowNo + nRow ] == ""
                  nCol  := 0
               ELSE
                  IF nCol > Len( aString[ nRowNo + nRow ] )
                     nCol := Len( aString[ nRowNo + nRow ] )
                  ENDIF
               ENDIF

               StringDisplay( aPanel, aString, nRow, nCol, nRowNo )
               EXIT

            CASE K_LEFT

               IF nCol > 0
                  nCol--
               ELSE
                  IF nColNo > 0
                     nColNo--
                  ENDIF
               ENDIF

               StringDisplay( aPanel, aString, nRow, nCol, nRowNo )
               EXIT

            CASE K_DOWN

               IF nRow < nMaxRow - 1 .AND. nRow < Len( aString )
                  nRow++
               ELSE
                  IF nRowNo + nRow < Len( aString )
                     nRowNo++
                  ENDIF
               ENDIF

               IF aString[ nRowNo + nRow ] == ""
                  nCol := 0
               ELSE
                  IF nCol > Len( aString[ nRowNo + nRow ] )
                     nCol := Len( aString[ nRowNo + nRow ] )
                  ENDIF
               ENDIF

               StringDisplay( aPanel, aString, nRow, nCol, nRowNo )
               EXIT

            CASE K_RIGHT

               IF nCol < Len( aString[ nRowNo + nRow ]  )
                  nCol++
               ENDIF

               StringDisplay( aPanel, aString, nRow, nCol, nRowNo )
               EXIT

            CASE K_HOME

               nCol := 0

               StringDisplay( aPanel, aString, nRow, nCol, nRowNo )
               EXIT

            CASE K_END

               nCol := Len( aString[ nRowNo + nRow ]  )

               StringDisplay( aPanel, aString, nRow, nCol, nRowNo )
               EXIT

            CASE K_PGUP

               IF nRow <= 1
                  IF nRowNo - nMaxRow >= 0
                     nRowNo -= nMaxRow
                  ENDIF
               ENDIF
               nRow := 1

               StringDisplay( aPanel, aString, nRow, nCol, nRowNo )
               EXIT

            CASE K_PGDN

               IF nRow >= nMaxRow - 1
                  IF nRowNo + nMaxRow  <= Len( aString )
                     nRowNo += nMaxRow
                  ENDIF
               ENDIF
               nRow := Min( nMaxRow - 1, Len( aString ) - nRowNo )

               hb_Scroll( 1, 0, nMaxRow, nMaxCol )

               StringDisplay( aPanel, aString, nRow, nCol, nRowNo )
               EXIT

            CASE K_CTRL_PGUP

               nRow := 0
               nRowNo := 0

               StringDisplay( aPanel, aString, nRow, nCol, nRowNo )
               EXIT

            CASE K_CTRL_PGDN

               nRow := nMaxRow - 1
               nRowNo := Len( aString ) - nMaxRow + 1

               StringDisplay( aPanel, aString, nRow, nCol, nRowNo )
               EXIT

            CASE K_ENTER
               AltD()
               IF lArg
                  IF aString[ nRowNo + nRow ] == "" .OR. nCol == 0

                     hb_AIns( aString, nRowNo + nRow, "", .T. )
                     nRow++
                  ELSE
                     IF nCol == Len( aString[ nRowNo + nRow ] )
                        hb_AIns( aString, nRowNo + nRow + 1, "", .T. )
                        nRow++
                        nCol := 0
                     ELSE
                        cSubString := Right( aString[ nRowNo + nRow ], Len( aString[ nRowNo + nRow ] ) - nCol )
                        cStringEditingRow := aString[ nRowNo + nRow ]
                        aString[ nRowNo + nRow ] := Stuff( cStringEditingRow, nCol + 1, Len( aString[ nRowNo + nRow ] ) - nCol, "" )
                        hb_AIns( aString, nRowNo + nRow, cSubString + 1, .T. )
                        nRow++
                        nCol := 0
                     ENDIF
                  ENDIF

                  SaveFile( aString, cFileName )
                  StringDisplay( aPanel, aString, nRow, nCol, nRowNo )
               ENDIF
               EXIT

            CASE K_INS
               IF lArg
                  IF lToggleInsert
                     SetCursor( SC_NORMAL )
                     lToggleInsert := .F.
                  ELSE
                     SetCursor( SC_INSERT )
                     lToggleInsert := .T.
                  ENDIF
               ENDIF
               EXIT

            CASE K_DEL
               IF lArg
                  IF aString[ nRowNo + nRow ] == ""
                     IF nRow >= 0
                        hb_ADel( aString, nRowNo + nRow, .T. )
                     ENDIF
                  ELSE
                     IF nCol == Len( aString[ nRowNo + nRow ] )

                        aString[ nRowNo + nRow ] += aString[ nRowNo + nRow + 1 ]

                        hb_ADel( aString, nRowNo + nRow + 1, .T. )
                     ELSE
                        cStringEditingRow := aString[ nRowNo + nRow ]
                        aString[ nRowNo + nRow ] := Stuff( cStringEditingRow, nCol + 1, 1, "" )
                     ENDIF
                  ENDIF

                  SaveFile( aString, cFileName )
                  StringDisplay( aPanel, aString, nRow, nCol, nRowNo )
               ENDIF
               EXIT

            CASE K_BS
               IF lArg
                  IF aString[ nRowNo + nRow ] == ""
                     IF nRow > 1
                        hb_ADel( aString, nRowNo + nRow, .T. )
                        nRow--
                        nCol := Len( aString[ nRowNo + nRow ] )
                     ENDIF
                  ELSE
                     IF nCol > 0
                        cStringEditingRow := aString[ nRowNo + nRow ]
                        aString[ nRowNo + nRow ] := Stuff( cStringEditingRow, nCol, 1, "" )
                        nCol--
                     ELSE
                        IF nRow > 1
                           IF aString[ nRowNo + nRow - 1 ] == ""
                              nCol := 0
                           ELSE
                              nCol := Len( aString[ nRowNo + nRow - 1 ] )
                           ENDIF

                           aString[ nRowNo + nRow - 1 ] += aString[ nRowNo + nRow ]

                           hb_ADel( aString, nRowNo + nRow, .T. )
                           nRow--
                        ENDIF
                     ENDIF
                  ENDIF

                  SaveFile( aString, cFileName )
                  StringDisplay( aPanel, aString, nRow, nCol, nRowNo )
               ENDIF
               EXIT

            CASE K_TAB
               IF lArg
                  cStringEditingRow := aString[ nRowNo + nRow ]

                  aString[ nRowNo + nRow ] := Stuff( cStringEditingRow, nCol + 1, iif( lToggleInsert, 1, 0 ), "   " )
                  nCol += 3

                  SaveFile( aString, cFileName )
                  StringDisplay( aPanel, aString, nRow, nCol, nRowNo )
               ENDIF
               EXIT

            OTHERWISE

               IF lArg
                  IF ( nKeyStd >= 32 .AND. nKeyStd <= 126 ) .OR. ( nKeyStd >= 160 .AND. nKeyStd <= 255 ) .OR. ! hb_keyChar( nKeyStd ) == ""

                     cStringEditingRow := aString[ nRowNo + nRow ]
                     aString[ nRowNo + nRow ] := Stuff( cStringEditingRow, nCol + 1, iif( lToggleInsert, 1, 0 ), hb_keyChar( nKeyStd ) )
                     nCol++

                     SaveFile( aString, cFileName )
                     StringDisplay( aPanel, aString, nRow, nCol, nRowNo )
                  ENDIF
               ENDIF

            ENDSWITCH

         ENDDO

      ENDIF

   ELSE
      RETURN
   ENDIF

   RestScreen( 0, 0, MaxRow(), MaxCol(), cScreen )
   SetPos( nRowOld, nColOld )

   RETURN

STATIC PROCEDURE StringDisplay( aPanel, aString, nRow, nCol, nRowNo )

   LOCAL i
   LOCAL nMaxRow := MaxRow(), nMaxCol := MaxCol()
   LOCAL nLine
   LOCAL nPos

   nPos := aPanel[ _HC_nRowBar ] + aPanel[ _HC_nRowNo ]

   DispBegin()
   hb_Scroll()

   hb_DispOutAt( 0, 0, ;
      PadR( aPanel[ _HC_nArray ][ nPos ][ F_NAME ] + "  " + hb_ntos( hb_FSize( aPanel[ _HC_nArray ][ nPos ][ F_NAME ] ) ), nMaxCol + 1 ), 0x30 )

   FOR i := 1 TO nMaxRow

      nLine := i + nRowNo

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
      PadR( " Row(" + hb_ntos( nRow + nRowNo ) + ") Col(" + hb_ntos( nCol ) + ")", nMaxCol + 1 ), 0x30 )

   DispEnd()

   SetPos( nRow, nCol )

   RETURN

STATIC FUNCTION SaveFile( aString, cFileName )

   LOCAL cString := ""

   AEval( aString, {| e | cString += e + hb_eol() } )
   hb_MemoWrit( cFileName, cString )

   RETURN NIL

// ====================================
FUNCTION Q( xPar )
   RETURN Alert( hb_ValToExp( xPar ) )
// ====================================
