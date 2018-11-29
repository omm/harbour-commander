/* Copyright 2017-2018 Rafał Jopek ( rafaljopek at hotmail com ) */

/* Harbour Commander */

#include "box.ch"
#include "inkey.ch"
#include "setcurs.ch"

FUNCTION MsgBox( cMessage, aOptions )

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

      nKey := Inkey( 1, hb_bitOr( Set( _SET_EVENTMASK, INKEY_ALL ), HB_INKEY_EXT ) )
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

         IF nCol < 57 .AND. nCol <= Len( cString )
            nCol++
         ELSE
            IF nColNo + nCol <= Len( cString )
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

            cString := Stuff( cString, nCol + 1, 0, hb_keyChar( nKeyStd ) )
            nCol++

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

   hb_DispOutAt( nMaxRow, nMaxCol - 28, PadR( SubStr( cString, nColNo, 57 ), 57 ), hb_NToColor( 0x30 ) )

   SetPos( nMaxRow, nMaxCol - 28 + nCol )

   DispEnd()

   RETURN
