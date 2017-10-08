/* Copyright 2017 Rafał Jopek ( rafaljopek at hotmail com ) */

/* Harbour Commander */


#include "box.ch"
#include "directry.ch"
#include "hbgtinfo.ch"
#include "inkey.ch"
#include "setcurs.ch"

STATIC aSelect_Left
STATIC aSelect_Right
STATIC aSelected

PROCEDURE Main()

   Scroll()
   Set( _SET_SCOREBOARD, .F. )
   Set( _SET_EVENTMASK, hb_bitOr( INKEY_KEYBOARD, HB_INKEY_GTEVENT ) )
   Set( _SET_INSERT, .T. )

   hb_gtInfo( HB_GTI_RESIZEMODE, HB_GTI_RESIZEMODE_ROWS )

   aSelect_Left  := Panel_Init()
   aSelect_Right := Panel_Init()

   Panel_Fetchlist( aSelect_Left, hb_cwd() )
   Panel_Fetchlist( aSelect_Right, hb_cwd() )

   Autosize()

   Panel_Display( aSelect_Left )
   Panel_Display( aSelect_Right )

   aSelected := aSelect_Left

   Prompt()

   RETURN

STATIC PROCEDURE Autosize()

   Resize( aSelect_Left, 0, 0, MaxRow() - 1, MaxCol() / 2 )
   Resize( aSelect_Right, 0, MaxCol() / 2 + 1, MaxRow() - 1, MaxCol() )

   RETURN

STATIC PROCEDURE Prompt()

   LOCAL GetList
   LOCAL cLine, lResize
   LOCAL nMaxRow := 0, nMaxCol := 0
   LOCAL bKeyIns, bKeyCompletion, bKeyResize
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

         Panel_Display( aSelect_Left )
         Panel_Display( aSelect_Right )
         BottomBar()

         nMaxRow := MaxRow()
         nMaxCol := MaxCol()
      ENDIF

      hb_DispOutAt( nMaxRow - 1, 0, cPrompt := "$ " )

      GetList := { ;
         Get():New( nMaxRow - 1, Len( cPrompt ), ;
         {| v | iif( PCount() == 0, cLine, cLine := v ) }, ;
         "cLine", ;
         "@KS" + hb_ntos( MaxCol() - Len( cPrompt ) + 1 ) ) }
      ATail( GetList ):display()

      DispEnd()

      SetCursor( iif( ReadInsert(), SC_INSERT, SC_NORMAL ) )

      bKeyIns        := SetKey( K_INS,       {|| SetCursor( iif( ReadInsert( ! ReadInsert() ), SC_NORMAL, SC_INSERT ) ) } )
      bKeyCompletion := SetKey( K_TAB,       {|| iif( aSelected == aSelect_Left, aSelected := aSelect_Right, aSelected := aSelect_Left ), ;
         Panel_Display( aSelect_Left ), Panel_Display( aSelect_Right ) } )
      bKeyResize     := SetKey( HB_K_RESIZE, {|| lResize := .T., hb_keyPut( K_ENTER ) } )

      lResize := .F.

      ReadModal( GetList )

      SetKey( K_INS, bKeyIns )
      SetKey( K_TAB, bKeyCompletion )
      SetKey( HB_K_RESIZE, bKeyResize )

      DO CASE
      CASE LastKey() == K_ESC
         EXIT
      CASE LastKey() == K_TAB
         LOOP
      CASE lResize .AND. LastKey() == K_ENTER
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

STATIC PROCEDURE Resize( p, nTop, nLeft, nBottom, nRight )

   p[ "nTop" ]    := nTop
   p[ "nLeft" ]   := nLeft
   p[ "nBottom" ] := nBottom
   p[ "nRight" ]  := nRight

   RETURN

STATIC PROCEDURE Panel_Display( p )

   LOCAL r, i

   DispBegin()

   hb_DispBox( p[ "nTop" ], p[ "nLeft" ], p[ "nBottom"  ] - 1, p[ "nRight" ], HB_B_SINGLE_UNI + ' ' /* , 0x1f */ )

   i := p[ 'f' ]
   FOR r := p[ "nTop" ] + 1 TO p[ "nBottom" ] - 1
      IF i <= Len( p[ "aArray" ] )
         hb_DispOutAt( r, p[ "nLeft" ] + 1, PadR( p[ "aArray" ][ i ][ F_NAME ], p[ "nRight" ] - p[ "nLeft" ] - 1 ), ;
            iif( aSelected == p .AND. i == p[ 'h' ], 0xf0, NIL ) )
         ++i
      ELSE
         EXIT
      ENDIF
   NEXT

   DispEnd()

   RETURN

STATIC PROCEDURE Panel_Fetchlist( p, cDir )

   p[ "cDir" ]   := hb_defaultValue( cDir, hb_cwd() )
   p[ "aArray" ] := hb_vfDirectory( p[ "cDir" ] )

   RETURN

STATIC FUNCTION Panel_Init()
   RETURN { ;
      "nTop"    => 0,  ;
      "nLeft"   => 0,  ;
      "nBottom" => 0,  ;
      "nRight"  => 0,  ;
      "cDir"    => '', ;
      "aArray"  => {}, ;
      'f'       => 1,  ;
      'h'       => 1 }


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
