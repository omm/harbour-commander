/* Copyright 2017 Rafał Jopek ( rafaljopek at hotmail com ) */

/* Harbour Commander */

#include "box.ch"
#include "directry.ch"
#include "hbgtinfo.ch"
#include "inkey.ch"
#include "setcurs.ch"

STATIC aPanel_Left
STATIC aPanel_Right
STATIC aPanel_Selected

PROCEDURE Main()

   Scroll()
   Set( _SET_SCOREBOARD, .F. )
   Set( _SET_EVENTMASK, hb_bitOr( INKEY_KEYBOARD, HB_INKEY_GTEVENT ) )
   Set( _SET_INSERT, .T. )

   hb_gtInfo( HB_GTI_RESIZEMODE, HB_GTI_RESIZEMODE_ROWS )
   AltD()

   aPanel_Left  := Panel_Init()
   aPanel_Right := Panel_Init()

   Panel_Fetchlist( aPanel_Left, hb_cwd() )
   Panel_Fetchlist( aPanel_Right, hb_cwd() )

   Autosize()

   Panel_Display( aPanel_Left )
   Panel_Display( aPanel_Right )

   aPanel_Selected := aPanel_Left

   Prompt()

   RETURN

STATIC PROCEDURE Autosize()

   Resize( aPanel_Left, 0, 0, MaxRow() -1, Int( MaxCol() / 2 ) )
   Resize( aPanel_Right, 0, Int( MaxCol() / 2 ) + 1, MaxRow() -1, MaxCol() )

   RETURN

STATIC PROCEDURE Prompt()

   LOCAL GetList
   LOCAL cLine, lResize
   LOCAL nMaxRow := 0, nMaxCol := 0
   LOCAL bKeyIns, bKeyCompletion, bKeyResize, bKeyUp, bKeyDown
   LOCAL cCommand

   LOCAL cPrompt

   DO WHILE .T.

      IF cLine == NIL
         cLine := Space( 1024 )
      ENDIF

      DispBegin()

      IF nMaxRow != MaxRow() .OR. nMaxCol != MaxCol()
         Scroll()
         Autosize()

         Panel_Display( aPanel_Left )
         Panel_Display( aPanel_Right )
         BottomBar()

         nMaxRow := MaxRow()
         nMaxCol := MaxCol()
      ENDIF

      hb_DispOutAt( nMaxRow - 1, 0, cPrompt := "$ ", 0x7 )

      GetList := { ;
         Get():New( nMaxRow - 1, Len( cPrompt ), ;
         {| v | iif( PCount() == 0, cLine, cLine := v ) }, ;
         "cLine", ;
         "@KS" + hb_ntos( MaxCol() - Len( cPrompt ) + 1 ) ):colorDisp( hb_NToColor( 0x7 ) ) }

      ATail( GetList ):display()

      DispEnd()

      SetCursor( iif( ReadInsert(), SC_NORMAL, SC_INSERT ) )

      bKeyIns        := SetKey( K_INS,       {|| SetCursor( iif( ReadInsert( ! ReadInsert() ), SC_NORMAL, SC_INSERT ) ) } )
      bKeyCompletion := SetKey( K_TAB,       {|| iif( aPanel_Selected == aPanel_Left, aPanel_Selected := aPanel_Right, aPanel_Selected := aPanel_Left ), ;
         Panel_Display( aPanel_Left ), Panel_Display( aPanel_Right ) } )
      bKeyResize     := SetKey( HB_K_RESIZE, {|| lResize := .T., hb_keyPut( K_ENTER ) } )
      bKeyUp         := SetKey( K_UP,        {|| --aPanel_Selected[ "h" ], Panel_Display( aPanel_Selected ) } )
      bKeyDown       := SetKey( K_DOWN,      {|| ++aPanel_Selected[ "h" ], Panel_Display( aPanel_Selected ) } )

      lResize := .F.

      ReadModal( GetList )

      SetKey( K_INS, bKeyIns )
      SetKey( K_TAB, bKeyCompletion )
      SetKey( HB_K_RESIZE, bKeyResize )
      SetKey( K_UP, bKeyUp )
      SetKey( K_DOWN, bKeyDown )

      DO CASE
      CASE LastKey() == K_ESC
         EXIT
      CASE LastKey() == K_TAB
         LOOP
      CASE lResize .AND. LastKey() == K_ENTER
         LOOP
      CASE LastKey() == K_UP
         LOOP
      CASE LastKey() == K_DOWN
         LOOP
      ENDCASE

      cCommand := AllTrim( cLine )
      cLine := NIL

      IF ! Empty( cCommand )
         Scroll()
         hb_run( RTrim( cCommand ) )
         Inkey( 0 )
         nMaxCol := 0
      ENDIF
   ENDDO

   CLS

   RETURN

STATIC PROCEDURE Resize( aPanel, nTop, nLeft, nBottom, nRight )

   aPanel[ "nTop" ]    := nTop
   aPanel[ "nLeft" ]   := nLeft
   aPanel[ "nBottom" ] := nBottom
   aPanel[ "nRight" ]  := nRight

   RETURN

STATIC PROCEDURE Panel_Display( aPanel )

   LOCAL nRow
   LOCAL i

   DispBegin()
   SetColor( hb_NToColor( 0x1f ) )
   hb_DispBox( aPanel[ "nTop" ], aPanel[ "nLeft" ], aPanel[ "nBottom"  ] -1, aPanel[ "nRight" ], HB_B_SINGLE_UNI + ' ' /*, 0x1f */ )

   i := aPanel[ "f" ]
   FOR nRow := aPanel[ "nTop" ] + 2 TO aPanel[ "nBottom" ] -2
      IF i <= Len( aPanel[ "aArray" ] )
         hb_DispOutAt( nRow, aPanel[ "nLeft" ] + 1, ;
            PadR( aPanel[ "aArray" ][ i ][ F_NAME ], aPanel[ "nRight" ] - aPanel[ "nLeft" ] -1 ), ;
            iif( aPanel_Selected == aPanel .AND. i == aPanel[ "h" ], 0x30, NIL ) )
         ++i
      ELSE
         EXIT
      ENDIF
   NEXT

   DispEnd()

   RETURN

STATIC PROCEDURE Panel_Fetchlist( aPanel, cDir )

   aPanel[ "cDir" ]   := hb_defaultValue( cDir, hb_cwd() )
   aPanel[ "aArray" ] := hb_vfDirectory( aPanel[ "cDir" ] )

   RETURN

STATIC FUNCTION Panel_Init()
   RETURN { ;
      "nTop"    => 0,  ;
      "nLeft"   => 0,  ;
      "nBottom" => 0,  ;
      "nRight"  => 0,  ;
      "cDir"    => '', ;
      "aArray"  => {}, ;
      "f"       => 1,  ;
      "h"       => 1 }


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
