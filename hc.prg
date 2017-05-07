/* Copyright 2017 Rafał Jopek ( rafaljopek at hotmail com ) */

/* Harbour Commander */

#include "box.ch"
#include "hbclass.ch"
#include "hbgtinfo.ch"
#include "inkey.ch"
#include "setcurs.ch"

PROCEDURE Main()

   LOCAL nOldCursor := SetCursor( SC_NONE )
   LOCAL lContinue := .T.
   LOCAL oHC
   LOCAL nKey, nKeyStd

   hb_gtInfo( HB_GTI_ICONFILE, "docs/img/harb_osx.icns" )
   hb_gtInfo( HB_GTI_WINTITLE, "Harbour Commander" )
   hb_gtInfo( HB_GTI_CLOSABLE, .F. )

   oHC := HarbourCommander():New()
   oHC:BottomBar()

   DO WHILE lContinue

      oHC:Refresh()

      nKey := Inkey( 0.1, hb_bitOr( hb_bitAnd( INKEY_ALL, hb_bitNot( INKEY_MOVE ) ), HB_INKEY_GTEVENT, HB_INKEY_EXT ) )

      IF nKey == 0
         LOOP
      ENDIF

      nKeyStd := hb_keyStd( nKey )

      SWITCH nKeyStd

      CASE K_ESC
         lContinue := .F.
         EXIT

      CASE K_LBUTTONDOWN
         oHC:LButtonDown()
         EXIT

      CASE HB_K_RESIZE
         oHC:BottomBar()
         EXIT

      CASE HB_K_CLOSE
         IF Alert( "Close Application", { "Yes", "No" } ) == 1
            RETURN
         ENDIF
         EXIT

      ENDSWITCH

   ENDDO

   SetCursor( nOldCursor )

RETURN

/*
   Class Harbour Commander
*/
CLASS HarbourCommander

   VAR nP

   METHOD New()
   METHOD Refresh()
   METHOD LeftPanel()
   METHOD RightPanel()
   METHOD LButtonDown()
   METHOD BottomBar()

   METHOD Help()
   METHOD Menu()
   METHOD View()
   METHOD Edit()
   METHOD Copy()
   METHOD RenMov()
   METHOD Mkdir()
   METHOD Delete()
   METHOD PullDn()
   METHOD Quit()

ENDCLASS

METHOD New() CLASS HarbourCommander
RETURN Self

METHOD Refresh() CLASS HarbourCommander

   ::LeftPanel()
   ::RightPanel()

RETURN Self

METHOD LeftPanel() CLASS HarbourCommander

   hb_DispBox( 0, 0, MaxRow() - 2, Int( MaxCol() / 2 ), HB_B_SINGLE_UNI + " ", 0x1f )
   hb_DispOutAt( MaxRow() - 4, 0, Chr( 195 ) + Replicate( Chr( 196 ), Int( MaxCol() / 2 ) - 1 ) + Chr( 180 ), 0x1f )

   #if defined( __PLATFORM__WINDOWS )
      hb_DispOutAt( MaxRow() - 1, 0, DiskName() + ":\" + CurDir() + Space( MaxCol() ), 0x7 )
   #elif defined( __PLATFORM__UNIX )
      hb_DispOutAt( MaxRow() - 1, 0, CurDir() + Space( MaxCol() ), 0x7 )
   #endif


RETURN Self

METHOD RightPanel() CLASS HarbourCommander

   hb_DispBox( 0, Int( MaxCol() / 2 ) + 1, MaxRow() - 2, MaxCol(), HB_B_SINGLE_UNI + " ", 0x1f )
   hb_DispOutAt( MaxRow() - 4, Int( MaxCol() / 2 ) + 1, Chr( 195 ) + Replicate( Chr( 196 ), Int( MaxCol() / 2 ) - 1 ) + Chr( 180 ), 0x1f )

   #if defined( __PLATFORM__WINDOWS )
      hb_DispOutAt( MaxRow() - 1, 0, DiskName() + ":\" + CurDir() + Space( MaxCol() ), 0x7 )
   #elif defined( __PLATFORM__UNIX )
      hb_DispOutAt( MaxRow() - 1, 0, CurDir() + Space( MaxCol() ), 0x7 )
   #endif

RETURN Self

METHOD LButtonDown() CLASS HarbourCommander

   LOCAL nMRow := MRow()
   LOCAL nMCol := MCol()
   LOCAL nMPos

   IF nMRow == 0
      Alert("Menu")
   ENDIF

   IF nMRow > 0 .AND. nMRow < MaxRow() - 1 .AND. nMCol < Int( MaxCol() / 2 ) - 1
      Alert("Left")
   ENDIF

   IF nMRow > 0 .AND. nMRow < MaxRow() - 1 .AND. nMCol > Int( MaxCol() / 2 ) + 1
      Alert("Right")
   ENDIF

   IF nMRow == MaxRow() - 1 
      Alert(" C:\ ")
   ENDIF

   IF nMRow > MaxRow() - 1 

      nMPos := Int( nMCol / ::nP ) + 1

      SWITCH nMPos

      CASE 1
         Alert("1")
         EXIT

      CASE 2
         Alert("2")
         EXIT

      CASE 3
         Alert("3")
         EXIT

      CASE 4
         Alert("4")      
         EXIT

      CASE 5
         Alert("5")      
         EXIT

      CASE 6
         Alert("6")      
         EXIT

      CASE 7
         Alert("7")      
         EXIT

      CASE 8
         Alert("8")      
         EXIT

      CASE 9
         Alert("9")      
         EXIT

      CASE 10
         Alert("10")      
         EXIT

      ENDSWITCH

   ENDIF 

RETURN Self

METHOD BottomBar() CLASS HarbourCommander

   LOCAL nRow := MaxRow()
   LOCAL nCol := MaxCol()
   LOCAL cSpaces

   ::nP := Int( nCol / 10 ) + 1
   cSpaces := Space( ::nP - 6 - 1 )

   hb_DispOutAt( nRow, 0, " 1", 0x7 )
   hb_DispOutAt( nRow, 2, "Help  " + cSpaces, 0x30 )
   hb_DispOutAt( nRow, ::nP, " 2",    0x7 )
   hb_DispOutAt( nRow, ::nP + 2, "Menu  " + cSpaces, 0x30 )
   hb_DispOutAt( nRow, ::nP * 2, " 3",    0x7 )
   hb_DispOutAt( nRow, ::nP * 2 + 2, "View  " + cSpaces, 0x30 )
   hb_DispOutAt( nRow, ::nP * 3, " 4",    0x7 )
   hb_DispOutAt( nRow, ::nP * 3 + 2, "Edit  " + cSpaces, 0x30 )
   hb_DispOutAt( nRow, ::nP * 4, " 5",    0x7 )
   hb_DispOutAt( nRow, ::nP * 4 + 2, "Copy  " + cSpaces, 0x30 )
   hb_DispOutAt( nRow, ::nP * 5, " 6",    0x7 )
   hb_DispOutAt( nRow, ::nP * 5 + 2, "RenMov" + cSpaces, 0x30 )
   hb_DispOutAt( nRow, ::nP * 6, " 7",    0x7 )
   hb_DispOutAt( nRow, ::nP * 6 + 2, "Mkdir " + cSpaces, 0x30 )
   hb_DispOutAt( nRow, ::nP * 7, " 8",    0x7 )
   hb_DispOutAt( nRow, ::nP * 7 + 2, "Delete" + cSpaces, 0x30 )
   hb_DispOutAt( nRow, ::nP * 8, " 9",    0x7 )
   hb_DispOutAt( nRow, ::nP * 8 + 2, "PullDn" + cSpaces, 0x30 )
   hb_DispOutAt( nRow, ::nP * 9, "10",    0x7 )
   hb_DispOutAt( nRow, ::nP * 9 + 2, "Quit  " + cSpaces, 0x30 )

RETURN Self

METHOD Help() CLASS HarbourCommander
RETURN Self

METHOD Menu() CLASS HarbourCommander
RETURN Self

METHOD View() CLASS HarbourCommander
RETURN Self

METHOD Edit() CLASS HarbourCommander
RETURN Self

METHOD Copy() CLASS HarbourCommander
RETURN Self

METHOD RenMov() CLASS HarbourCommander
RETURN Self

METHOD Mkdir() CLASS HarbourCommander
RETURN Self

METHOD Delete() CLASS HarbourCommander
RETURN Self

METHOD PullDn() CLASS HarbourCommander
RETURN Self

METHOD Quit() CLASS HarbourCommander
RETURN Self
