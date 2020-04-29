/* Copyright 2017-2019 Rafał Jopek ( rafaljopek at hotmail com ) */

/* Harbour Commander */

#include "box.ch"
#include "directry.ch"
#include "fileio.ch"
#include "hbgtinfo.ch"
#include "inkey.ch"
#include "setcurs.ch"

#define DIR_PREFIX( v )  iif( "D" $ v[ F_ATTR ], "A", "B" )

#if defined( __PLATFORM__WINDOWS )
#define OSUPPER( x )  Upper( x )
#else
#define OSUPPER( x )  ( x )
#endif

#define _nTop        1
#define _nLeft       2
#define _nBottom     3
#define _nRight      4
#define _cCurrentDir 5
#define _aDirectory  6
#define _nRowBar     7
#define _nRowNo      8
#define _cComdLine   9
#define _nComdCol    10
#define _nComdColNo  11
#define _nIdPanel    12
#define _nWinHndl    13

#define _nElements   13

STATIC aConfig
#define _nElementConfig 5
#define _aStackWindow   1
#define _nMaxRow        2  // here save MaxRow() of physical screen
#define _nMaxCol        3  // here save MaxCol() of physical screen
#define nCmdHndl        4  // handle of window for cmd
#define nBBarHndl       5  // handle of window for bottom bar

/* FError() */
#define MEANING       2

#define F_STATUS      6  /* select file, directory */

#define FXO_SHARELOCK  0x4000  /* emulate DOS SH_DENY* mode in POSIX OS */

STATIC aWinStack
#define nWinSession           1
#define GetSession            aWinStack[ nWinSession ]
#define GetPanels             aWinStack[ GetSession ]
#define GetPanelActive        aWinStack[ GetSession, GetPanels[ idPanelActive ] ]
#define SetPanelActive( xId ) GetPanels[ idPanelActive ] := xId
#define IsPanelActive( xId )  iif( GetPanelActive[ _nIdPanel ] == xId, .T., .F. )

STATIC aPanels          // array of panels and id of active panel
#define idPanelLeft     1
#define idPanelRight    2
#define idPanelActive   3   // Active panel { 1 - left, 2 - right }

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
   Set( _SET_DBCODEPAGE, "EN" )

   hb_gtInfo( HB_GTI_RESIZEMODE, HB_GTI_RESIZEMODE_ROWS )
   hb_gtInfo( HB_GTI_WINTITLE, "Harbour Commander" )

   /* hb_cwd () returns the full current working directory containing the disk and final path separator */
   /* restore configuration*/
   aConfig := hb_deserialize( hb_memoread( "hc.cfg" ) )

   IF ! HB_ISARRAY( aConfig )
      ConfigInit()
      /* add new sessins to windows stack */
      IF !hb_IsArray( aWinStack )
         aWinStack := {}
         /* in first element we save active session number, initial is 2 (should be greater then 1) */
         AAdd( aWinStack, 2 )
         /* next elements of aWinStack is array( 3 ), { left panel, right panel , active panel (default: left) } */
         AAdd( aWinStack, { PanelInit( idPanelLeft ), PanelInit( idPanelRight ), idPanelLeft } )
      ENDIF
      aConfig[ _aStackWindow ] := aWinStack
   ELSE
      aWinStack := aConfig[ _aStackWindow ]
   ENDIF

   aConfig[ _nMaxRow ] := MaxRow()
   aConfig[ _nMaxCol ] := MaxCol()

   Prompt()

   hb_Scroll()
   SetPos( 0, 0 )

   RETURN

STATIC PROCEDURE ConfigInit()

   aConfig := Array( _nElementConfig )

   aConfig[ _aStackWindow ] := {}
   aConfig[ _nMaxRow ] := NIL
   aConfig[ _nMaxCol ] := NIL
   aConfig[ nCmdHndl ] := NIL
   aConfig[ nBBarHndl ] := NIL

   RETURN

STATIC FUNCTION PanelInit( IdPanel )

   LOCAL aInit

   aInit := Array( _nElements )

   aInit[ _nTop        ] := 0
   aInit[ _nLeft       ] := 0
   aInit[ _nBottom     ] := 0
   aInit[ _nRight      ] := 0
   aInit[ _cCurrentDir ] := hb_cwd()
   aInit[ _aDirectory  ] := {}
   aInit[ _nRowBar     ] := 1
   aInit[ _nRowNo      ] := 0
   aInit[ _cComdLine   ] := ""
   aInit[ _nComdCol    ] := 0
   aInit[ _nComdColNo  ] := 0
   aInit[ _nIdPanel    ] := IdPanel
   aInit[ _nWinHndl    ] := NIL     // handle to window

   RETURN aInit

STATIC PROCEDURE FetchData()

   PanelFetchList( GetPanels[ idPanelLeft ] )
   PanelFetchList( GetPanels[ idPanelRight ] )

   AutoSize()

   RETURN

STATIC PROCEDURE PanelFetchList( aPanel )

   LOCAL i, nPos, a2Dot, aDots := { ".", ".." }, iDot

   CheckDirExists( @aPanel[ _cCurrentDir ] )

   aPanel[ _aDirectory ] := hb_vfDirectory( aPanel[ _cCurrentDir ], "HSD" )

   /* Add .T. to each element of the array. */
   FOR i := 1 TO Len( aPanel[ _aDirectory ] )  // ? na AEval()
      AAdd( aPanel[ _aDirectory ][ i ], .T. )
   NEXT

   FOR EACH iDot in aDots
      IF ( nPos := AScan( aPanel[ _aDirectory ], {| x | x[ F_NAME ] == iDot } ) ) > 0
         IF iDot:__enumIndex == 2
            a2Dot := aPanel[ _aDirectory ][ nPos ]
         ENDIF
         hb_ADel( aPanel[ _aDirectory ], nPos, .T. )
      ENDIF
   NEXT
   ASort( aPanel[ _aDirectory ],,, {| x, y | DIR_PREFIX( x ) + OSUPPER( x[ F_NAME ] ) < DIR_PREFIX( y ) + OSUPPER( y[ F_NAME ] ) } )
   IF HB_ISARRAY( a2Dot )
      hb_AIns( aPanel[ _aDirectory ], 1, a2Dot, .T. )
   ENDIF

   RETURN

STATIC PROCEDURE CheckDirExists( cDir )

   LOCAL nPs

   WHILE ! hb_DirExists( cDir )
      nPs := hb_Rat( hb_ps(), cDir,, iif( Right( cDir, 1 ) == hb_ps(), Len( cDir ) - 1, NIL ) )
      cDir := SubStr( cDir, 1, nPs )
      CheckDirExists( @cDir )
   END

   RETURN

STATIC PROCEDURE AutoSize()

   Resize( GetPanels[ idPanelLeft ], 0, 0, aConfig[ _nMaxRow ] - 2, aConfig[ _nMaxCol ] / 2 )
   Resize( GetPanels[ idPanelRight ], 0, aConfig[ _nMaxCol ] / 2 + 1, aConfig[ _nMaxRow ] - 2, aConfig[ _nMaxCol ] )

   RETURN

STATIC PROCEDURE Resize( aPanel, nTop, nLeft, nBottom, nRight )

   aPanel[ _nTop    ] := nTop
   aPanel[ _nLeft   ] := nLeft
   aPanel[ _nBottom ] := nBottom
   aPanel[ _nRight  ] := nRight

   RETURN

STATIC PROCEDURE Prompt()

   LOCAL lContinue := .T.
   LOCAL nMaxRow := 0, nMaxCol := 0
   LOCAL nKey, nKeyStd
   LOCAL nPos
   LOCAL cFileName
   LOCAL pHandle
   LOCAL nMRow, nMCol
   LOCAL nCol
   LOCAL cSpaces
   LOCAL nErrorCode
   LOCAL cNewDrive
   LOCAL i
   LOCAL lCtrlO := .F.
   LOCAL _nPanelActive := 0
   LOCAL _nPanelRowBar := 0

#if defined( __PLATFORM__WINDOWS )

   // optimized, PLATFORM_LINUX ?
#else
   LOCAL result := ""
   LOCAL ctuxCmd := "xdg-open "
   LOCAL cTerminal := ""
#endif

   /* Create the first window (handle is 1), it's for commands, outputs, etc...
      It has initial size is one row from 0 to maxcol() at maxrow() - 1 position
   */
   IF ( aConfig[ nCmdHndl ] := wOpen( aConfig[ _nMaxRow ] - 1, 0, aConfig[ _nMaxRow ] - 1, aConfig[ _nMaxCol ] ) ) == -1
      /* if can't create a window, stop app */
      RETURN
   else
//      SetCursor( SC_NORMAL )
/*
      @0,0 say hb_ntos( wselect() ) + ", " + hb_ntos( wnum() ) + ", " + hb_ntos( maxrow() ) + ", " + hb_ntos( maxcol() )
      wformat()
      wbox(1)
      wformat()
*/
   ENDIF

   /* Create the window for bottom bar */
   aConfig[ nBBarHndl ] := wOpen( aConfig[ _nMaxRow ], 0, aConfig[ _nMaxRow ], aConfig[ _nMaxCol ] )

   /*
      Ok, now fetch the data
   */
   FetchData()

   DO WHILE lContinue

      /* */
      DispBegin()
/*
      nwrow1 := wlastrow()
      nSelectWin := wSelect( 0 )
      nwlastrow := wlastrow()
*/
      
      IF ( GetPanels[ idPanelLeft ][ _nWinHndl ] == NIL .OR. GetPanels[ idPanelRight ][ _nWinHndl ] == NIL ) .OR.;
         _nPanelActive != GetPanels[ idPanelActive ] .OR.;
         _nPanelRowBar != GetPanelActive[ _nRowBar ]

         IF _nPanelActive != GetPanels[ idPanelActive ]
            _nPanelActive := GetPanels[ idPanelActive ]
         ENDIF
         IF _nPanelRowBar != GetPanelActive[ _nRowBar ]
            _nPanelRowBar := GetPanelActive[ _nRowBar ]
         ENDIF

         AutoSize()

         PanelDisplay( GetPanels[ idPanelLeft ] )
         PanelDisplay( GetPanels[ idPanelRight ] )

         BottomBar()

         nMaxRow := MaxRow()
         nMaxCol := MaxCol()
      ENDIF
      ComdLineDisplay( )
      DispEnd()
      WSelect( aConfig[ nCmdHndl ] )
      SetCursor( SC_NORMAL )
      nKey := Inkey( 0 )
      nKeyStd := hb_keyStd( nKey )

      SWITCH nKeyStd

      CASE K_ESC

         IF ! Empty( GetPanelActive[ _cComdLine ] )
            GetPanelActive[ _cComdLine ] := ""
            GetPanelActive[ _nComdCol ] := 0
            ComdLineDisplay( )
         ELSE
            lContinue := .F.
            /* save configuration on exit*/
            GetPanels[ idPanelLeft ][ _nWinHndl ] := NIL
            GetPanels[ idPanelRight ][ _nWinHndl ] := NIL
            aConfig[ _aStackWindow ] := aWinStack
            hb_MemoWrit( StartUpPath() + "hc.cfg", hb_Serialize( aConfig ) )
         ENDIF
         EXIT

      CASE K_ENTER

         nPos := GetPanelActive[ _nRowBar ] + GetPanelActive[ _nRowNo ]
         IF Empty( GetPanelActive[ _cComdLine ] )
            /* if we stand on a file */
            IF At( "D", GetPanelActive[ _aDirectory ][ nPos ][ F_ATTR ] ) == 0
#if defined( __PLATFORM__WINDOWS )
               hb_run( Chr( 34 ) + GetPanelActive[ _cCurrentDir ] + GetPanelActive[ _aDirectory ][ nPos ][ F_NAME ] + Chr( 34 ) )
#else
               IF hb_vfExists( "/usr/bin/nautilus" )
                  cTerminal = "gnome-terminal"
               ELSEIF hb_vfExists( "/usr/bin/thunar" )
                  cTerminal = "xfce4-terminal"
               ELSEIF hb_vfExists( "/usr/bin/pcmanfm" )
                  cTerminal = "lxterminal"
               ELSEIF hb_vfExists( "/usr/bin/caja" )
                  cTerminal = "mate-terminal"
               ELSEIF hb_vfExists( "/usr/bin/dolphin" )
                  cTerminal = "konsole"
               ENDIF
               hb_processRun( "sh -c 'file " + GetPanelActive[ _cCurrentDir ] + GetPanelActive[ _aDirectory ][ nPos ][ F_NAME ] + "'" + Chr( 34 ),, @result )
               IF At( "ELF", result ) > 0
                  ctuxCmd = ""
               ELSEIF At( "script", result ) > 0
                  ctuxCmd = cterminal + " -x "
               ENDIF
               hb_run( ctuxCmd + Chr( 34 ) + GetPanelActive[ _cCurrentDir ] + GetPanelActive[ _aDirectory ][ nPos ][ F_NAME ] + Chr( 34 ) )
#endif
            ELSE
               ChangeDir( GetPanelActive )
            ENDIF
         ELSE

            // GetPanelActive[ _cComdLine ] := GetPanelActive[ _cCurrentDir ] + GetPanelActive[ _cComdLine ]

            hb_Scroll()
            hb_run( GetPanelActive[ _cComdLine ] )
            GetPanelActive[ _cComdLine ] := ""
            Inkey( 0 )
            nMaxRow := 0
            GetPanelActive[ _nComdCol ] := 0

            PanelRefresh( GetPanelActive )
         ENDIF

         EXIT

      CASE K_TAB

         IF IsPanelActive( idPanelLeft )
            SetPanelActive( idPanelRight )
            GetPanelActive[ _cComdLine ] := GetPanels[ idPanelLeft, _cComdLine ]
            GetPanelActive[ _nComdCol ] := GetPanels[ idPanelLeft, _nComdCol ]
            GetPanels[ idPanelLeft, _cComdLine ] := ""
            GetPanels[ idPanelLeft, _nComdCol ] := 0
         ELSE
            SetPanelActive( idPanelLeft )
            GetPanelActive[ _cComdLine ] := GetPanels[ idPanelRight, _cComdLine ]
            GetPanelActive[ _nComdCol ] := GetPanels[ idPanelRight, _nComdCol ]
            GetPanels[ idPanelRight, _cComdLine ] := ""
            GetPanels[ idPanelRight, _nComdCol ] := 0
         ENDIF

         //PanelDisplay( aPanels[ idPanelLeft ] )
         //PanelDisplay( aPanels[ idPanelRight ] )
         ShowSession()
         EXIT

      CASE K_MOUSEMOVE

         DispBegin()

         nMRow := MRow()
         nCol := Int( nMaxCol / 10 ) + 1

         BottomBar()
         IF nMRow > nMaxRow - 1

            cSpaces := Space( nCol - 8 )

            SWITCH Int( MCol() / nCol ) + 1
            CASE 1  ; hb_DispOutAt( nMRow, 2,            "Help  " + cSpaces, 0xb0 ) ; EXIT
            CASE 2  ; hb_DispOutAt( nMRow, nCol + 2,     "Menu  " + cSpaces, 0xb0 ) ; EXIT
            CASE 3  ; hb_DispOutAt( nMRow, nCol * 2 + 2, "View  " + cSpaces, 0xb0 ) ; EXIT
            CASE 4  ; hb_DispOutAt( nMRow, nCol * 3 + 2, "Edit  " + cSpaces, 0xb0 ) ; EXIT
            CASE 5  ; hb_DispOutAt( nMRow, nCol * 4 + 2, "Copy  " + cSpaces, 0xb0 ) ; EXIT
            CASE 6  ; hb_DispOutAt( nMRow, nCol * 5 + 2, "RenMov" + cSpaces, 0xb0 ) ; EXIT
            CASE 7  ; hb_DispOutAt( nMRow, nCol * 6 + 2, "MkDir " + cSpaces, 0xb0 ) ; EXIT
            CASE 8  ; hb_DispOutAt( nMRow, nCol * 7 + 2, "Delete" + cSpaces, 0xb0 ) ; EXIT
            CASE 9  ; hb_DispOutAt( nMRow, nCol * 8 + 2, "PullDn" + cSpaces, 0xb0 ) ; EXIT
            CASE 10 ; hb_DispOutAt( nMRow, nCol * 9 + 2, "Quit  " + cSpaces, 0xb0 ) ; EXIT
            ENDSWITCH

         ENDIF

         DispEnd()
         EXIT

      CASE K_LDBLCLK

         nPos := GetPanelActive[ _nRowBar ] + GetPanelActive[ _nRowNo ]
         /* if we stand on a file */
         IF At( "D", GetPanelActive[ _aDirectory ][ nPos ][ F_ATTR ] ) == 0
#if defined( __PLATFORM__WINDOWS )
            hb_run( Chr( 34 ) + GetPanelActive[ _cCurrentDir ] + GetPanelActive[ _aDirectory ][ nPos ][ F_NAME ] + Chr( 34 ) )
#else
            IF hb_vfExists( "/usr/bin/nautilus" )
               cTerminal = "gnome-terminal"
            ELSEIF hb_vfExists( "/usr/bin/thunar" )
               cTerminal = "xfce4-terminal"
            ELSEIF hb_vfExists( "/usr/bin/pcmanfm" )
               cTerminal = "lxterminal"
            ELSEIF hb_vfExists( "/usr/bin/caja" )
               cTerminal = "mate-terminal"
            ELSEIF hb_vfExists( "/usr/bin/dolphin" )
               cTerminal = "konsole"
            ENDIF
            hb_processRun( "sh -c 'file " + GetPanelActive[ _cCurrentDir ] + GetPanelActive[ _aDirectory ][ nPos ][ F_NAME ] + "'" + Chr( 34 ),, @result )
            IF At( "ELF", result ) > 0
               ctuxCmd = ""
            ELSEIF At( "script", result ) > 0
               ctuxCmd = cterminal + " -x "
            ENDIF
            hb_run( ctuxCmd + Chr( 34 ) + GetPanelActive[ _cCurrentDir ] + GetPanelActive[ _aDirectory ][ nPos ][ F_NAME ] + Chr( 34 ) )
#endif
         ELSE
            ChangeDir( GetPanelActive )
         ENDIF

         EXIT

      CASE K_RBUTTONDOWN

         nMRow := MRow()
         nMCol := MCol()

         IF nMRow > 0 .AND. nMRow < MaxRow() - 1 .AND. nMCol < Int( MaxCol() / 2 ) + 1
            GetPanelActive := aPanels[ idPanelLeft ]
            IF nMRow <= Len( GetPanelActive[ _aDirectory ] )
               GetPanelActive[ _nRowBar ] := nMRow
            ENDIF
         ENDIF

         IF nMRow > 0 .AND. nMRow < MaxRow() - 1 .AND. nMCol > Int( MaxCol() / 2 )
            GetPanelActive := aPanels[ idPanelRight ]
            IF nMRow <= Len( GetPanelActive[ _aDirectory ] )
               GetPanelActive[ _nRowBar ] := nMRow
            ENDIF
         ENDIF

         nPos := GetPanelActive[ _nRowBar ] + GetPanelActive[ _nRowNo ]
         IF GetPanelActive[ _aDirectory ][ nPos ][ F_NAME ] != ".." // ?

            /* Marking the deletion status of the current item in the table */
            IF GetPanelActive[ _aDirectory ][ nPos ][ F_STATUS ]
               GetPanelActive[ _aDirectory ][ nPos ][ F_STATUS ] := .F.
            ELSE
               /* return the delete status of the current item in the array */
               GetPanelActive[ _aDirectory ][ nPos ][ F_STATUS ] := .T.
            ENDIF

         ENDIF

         PanelDisplay( aPanels[ idPanelLeft ] )
         PanelDisplay( aPanels[ idPanelRight ] )
         EXIT

      CASE K_LBUTTONDOWN

         nMRow := MRow()
         nMCol := MCol()

         IF nMRow > 0 .AND. nMRow < MaxRow() - 1 .AND. nMCol < Int( MaxCol() / 2 ) + 1
            GetPanelActive := aPanels[ idPanelLeft ]
            IF nMRow <= Len( GetPanelActive[ _aDirectory ] )
               GetPanelActive[ _nRowBar ] := nMRow
            ENDIF
         ENDIF

         IF nMRow > 0 .AND. nMRow < MaxRow() - 1 .AND. nMCol > Int( MaxCol() / 2 )
            GetPanelActive := aPanels[ idPanelRight ]
            IF nMRow <= Len( GetPanelActive[ _aDirectory ] )
               GetPanelActive[ _nRowBar ] := nMRow
            ENDIF
         ENDIF

         /* BottomBar */
         nCol := Int( nMaxCol / 10 ) + 1
         IF nMRow > nMaxRow - 1
            SWITCH Int( MCol() / nCol ) + 1
            CASE 1  ; FunctionKey_F1() ; EXIT
            CASE 2  ; FunctionKey_F2() ; EXIT
            CASE 3  ; FunctionKey_F3( GetPanelActive ) ; EXIT
            CASE 4  ; FunctionKey_F4( GetPanelActive ) ; EXIT
            CASE 5  ; FunctionKey_F5( GetPanelActive ) ; EXIT
            CASE 6  ; FunctionKey_F6( GetPanelActive ) ; EXIT
            CASE 7  ; FunctionKey_F7( GetPanelActive ) ; EXIT
            CASE 8  ; FunctionKey_F8( GetPanelActive ) ; EXIT
            CASE 9  ; EXIT
            CASE 10
               IF HC_Alert( "The Harbour Commander", "Do you want to quit the Harbour Commander?", { "Yes", "No!" }, 0x8f ) == 1
                  lContinue := .F.
               ENDIF
               EXIT

            ENDSWITCH

         ENDIF

         PanelDisplay( aPanels[ idPanelLeft ] )
         PanelDisplay( aPanels[ idPanelRight ] )
         EXIT

      CASE K_MWFORWARD

         IF GetPanelActive[ _nRowBar ] > 1
            --GetPanelActive[ _nRowBar ]
         ELSE
            IF GetPanelActive[ _nRowNo ] >= 1
               --GetPanelActive[ _nRowNo ]
            ENDIF
         ENDIF

         EXIT

      CASE K_MWBACKWARD

         IF GetPanelActive[ _nRowBar ] < GetPanelActive[ _nBottom ] - 1 .AND. GetPanelActive[ _nRowBar ] <= Len( GetPanelActive[ _aDirectory ] ) - 1
            ++GetPanelActive[ _nRowBar ]
         ELSE
            IF GetPanelActive[ _nRowNo ] + GetPanelActive[ _nRowBar ] <= Len( GetPanelActive[ _aDirectory ] ) - 1
               ++GetPanelActive[ _nRowNo ]
            ENDIF
         ENDIF

         EXIT

      CASE K_UP

         IF GetPanelActive[ _nRowBar ] > 1
            --GetPanelActive[ _nRowBar ]
         ELSE
            IF GetPanelActive[ _nRowNo ] >= 1
               --GetPanelActive[ _nRowNo ]
            ENDIF
         ENDIF

         EXIT

      CASE K_DOWN

         IF GetPanelActive[ _nRowBar ] < GetPanelActive[ _nBottom ] - 1 .AND. GetPanelActive[ _nRowBar ] <= Len( GetPanelActive[ _aDirectory ] ) - 1
            ++GetPanelActive[ _nRowBar ]
         ELSE
            IF GetPanelActive[ _nRowNo ] + GetPanelActive[ _nRowBar ] <= Len( GetPanelActive[ _aDirectory ] ) - 1
               ++GetPanelActive[ _nRowNo ]
            ENDIF
         ENDIF

         EXIT

      CASE K_LEFT

         IF GetPanelActive[ _nComdCol ] > 0
            GetPanelActive[ _nComdCol ]--
         ELSE
            IF GetPanelActive[ _nComdColNo ] >= 1
               GetPanelActive[ _nComdColNo ]--
            ENDIF
         ENDIF

         EXIT

      CASE K_RIGHT

         IF GetPanelActive[ _nComdCol ] < nMaxCol - Len( GetPanelActive[ _cCurrentDir ] ) .AND. GetPanelActive[ _nComdCol ] < Len( GetPanelActive[ _cComdLine ] )
            GetPanelActive[ _nComdCol ]++
         ELSE
            IF GetPanelActive[ _nComdColNo ] + GetPanelActive[ _nComdCol ] < Len( GetPanelActive[ _cComdLine ] )
               GetPanelActive[ _nComdColNo ]++
            ENDIF
         ENDIF

         EXIT

      CASE K_HOME

         GetPanelActive[ _nComdCol ] := 0

         EXIT

      CASE K_END

         GetPanelActive[ _nComdCol ] := Len( GetPanelActive[ _cComdLine ] )

         EXIT

      CASE K_PGUP

         IF GetPanelActive[ _nRowBar ] <= 1
            IF GetPanelActive[ _nRowNo ] - nMaxRow >= 0
               GetPanelActive[ _nRowNo ] -= nMaxRow
            ENDIF
         ENDIF

         GetPanelActive[ _nRowBar ] := 1
         EXIT

      CASE K_PGDN

         IF GetPanelActive[ _nRowBar ] >= nMaxRow - 3
            IF GetPanelActive[ _nRowNo ] + nMaxRow  <= Len( GetPanelActive[ _aDirectory ] )
               GetPanelActive[ _nRowNo ] += nMaxRow
            ENDIF
         ENDIF

         GetPanelActive[ _nRowBar ] := Min( nMaxRow - 3, Len( GetPanelActive[ _aDirectory ] ) - GetPanelActive[ _nRowNo ] )
         EXIT

      CASE K_INS

         nPos := GetPanelActive[ _nRowBar ] + GetPanelActive[ _nRowNo ]
         IF GetPanelActive[ _aDirectory ][ nPos ][ F_NAME ] != ".." // ?

            /* Marking the deletion status of the current item in the table */
            IF GetPanelActive[ _aDirectory ][ nPos ][ F_STATUS ]
               GetPanelActive[ _aDirectory ][ nPos ][ F_STATUS ] := .F.
            ELSE
               /* return the delete status of the current item in the array */
               GetPanelActive[ _aDirectory ][ nPos ][ F_STATUS ] := .T.
            ENDIF

            IF GetPanelActive[ _nRowBar ] < GetPanelActive[ _nBottom ] - 1 .AND. GetPanelActive[ _nRowBar ] <= Len( GetPanelActive[ _aDirectory ] ) - 1
               ++GetPanelActive[ _nRowBar ]
            ELSE
               IF GetPanelActive[ _nRowNo ] + GetPanelActive[ _nRowBar ] <= Len( GetPanelActive[ _aDirectory ] ) - 1
                  ++GetPanelActive[ _nRowNo ]
               ENDIF
            ENDIF

         ENDIF

         EXIT

      CASE K_DEL

         IF GetPanelActive[ _nComdCol ] >= 0
            GetPanelActive[ _cComdLine ] := Stuff( GetPanelActive[ _cComdLine ], GetPanelActive[ _nComdCol ] + 1, 1, "" )
         ENDIF

         EXIT

      CASE K_BS

         IF GetPanelActive[ _nComdCol ] > 0
            GetPanelActive[ _cComdLine ] := Stuff( GetPanelActive[ _cComdLine ], GetPanelActive[ _nComdCol ], 1, "" )
            GetPanelActive[ _nComdCol ]--
         ENDIF

         EXIT

      CASE K_CTRL_O

         IF ! lCtrlO
            lCtrlO := .T.
            ShowSession( 1 )
         ELSE
            lCtrlO := .F.
            ShowSession()
         ENDIF
         EXIT

      CASE K_F1

         FunctionKey_F1()

         EXIT

      CASE K_F2

         FunctionKey_F2()

         EXIT

      CASE K_F3

         FunctionKey_F3( GetPanelActive )

         EXIT

      CASE K_F4

         FunctionKey_F4( GetPanelActive )

         EXIT

      CASE K_F5

         FunctionKey_F5( GetPanelActive )

         EXIT

      CASE K_F6

         FunctionKey_F6( GetPanelActive )

         EXIT

      CASE K_F7

         FunctionKey_F7( GetPanelActive )

         EXIT

      CASE K_F8

         FunctionKey_F8( GetPanelActive )

         EXIT

      CASE K_F9

         FunctionKey_F9( )

         EXIT

      CASE K_F10
         lContinue := .F.
         EXIT

#if defined( __PLATFORM__WINDOWS )

      CASE K_ALT_F1

         /* change directory to the left panel mount point,
            the last parameter sets the dialog: NIL center, 0x0 to the left and 0x1 to the right
            AllDrives () returns an array */
         IF ( cNewDrive := HC_Alert( "Drive letter", "Choose left drive:", AllDrives(), 0x8a, 0x0 ) ) != 0

               hb_CurDrive( AllDrives()[ cNewDrive ] )
               PanelFetchList( aPanels[ idPanelLeft ], hb_cwd() )
               PanelDisplay( aPanels[ idPanelLeft ] )

         ENDIF

         EXIT

      CASE K_ALT_F2
         /* change directory to the right panel mount point,
            the last parameter sets the dialog: NIL center, 0x0 to the left and 0x1 to the right
            AllDrives () returns an array */
         IF ( cNewDrive := HC_Alert( "Drive letter", "Choose right drive:", AllDrives(), 0x8a, 0x1 ) ) != 0

            hb_CurDrive( AllDrives()[ cNewDrive ] )
            PanelFetchList( aPanels[ idPanelRight ], hb_cwd() )
            PanelDisplay( aPanels[ idPanelRight ] )

         ENDIF

         EXIT
#endif

      CASE K_SH_F4

         nPos := GetPanelActive[ _nRowBar ] + GetPanelActive[ _nRowNo ]
         /* if we stand on a file */
         IF At( "D", GetPanelActive[ _aDirectory ][ nPos ][ F_ATTR ] ) == 0

            IF HB_ISSTRING( cFileName := MsgBox( "Create file.", GetPanelActive[ _cCurrentDir ] + GetPanelActive[ _aDirectory ][ nPos ][ F_NAME ], { "Yes", "No!" } ) )
               IF hb_vfExists( cFileName )

                  HCEdit( cFileName, .T. )

               ELSE
                  IF ( pHandle := hb_vfOpen( cFileName, FO_CREAT + FO_TRUNC + FO_WRITE ) ) != NIL

                     IF ! hb_vfClose( pHandle )
                        IF ( nErrorCode := AScan( FileError(), {| x | x[ 1 ] == FError() } ) ) > 0
                           HC_Alert( "Error", "Test for errors after a binary file operation.;Cannot make file, error:;" + FileError()[ nErrorCode ][ MEANING ] )
                        ELSE
                           HC_Alert( "Error", "Test for errors after a binary file operation.;Cannot make file, error:;" + hb_ntos( FError() ) )
                        ENDIF
                     ENDIF

                     PanelRefresh( GetPanelActive )

                  ELSE

                     IF ( nErrorCode := AScan( FileError(), {| x | x[ 1 ] == FError() } ) ) > 0
                        HC_Alert( "Error", "Test for errors after a binary file operation.;Cannot make file, error:;" + FileError()[ nErrorCode ][ MEANING ] )
                     ELSE
                        HC_Alert( "Error", "Test for errors after a binary file operation.;Cannot make file, error:;" + hb_ntos( FError() ) )
                     ENDIF

                  ENDIF
               ENDIF

            ENDIF

         ELSE
            /* if we stand on the catalog */
            IF HB_ISSTRING( cFileName := MsgBox( "Create file.", NIL, { "Yes", "No!" } ) )
               IF ( pHandle := hb_vfOpen( GetPanelActive[ _cCurrentDir ] + cFileName, FO_CREAT + FO_TRUNC + FO_WRITE ) ) != NIL

                  IF ! hb_vfClose( pHandle )
                     IF ( nErrorCode := AScan( FileError(), {| x | x[ 1 ] == FError() } ) ) > 0
                        HC_Alert( "Error", "Test for errors after a binary file operation.;Cannot make file, error:;" + FileError()[ nErrorCode ][ MEANING ] )
                     ELSE
                        HC_Alert( "Error", "Test for errors after a binary file operation.;Cannot make file, error:;" + hb_ntos( FError() ) )
                     ENDIF
                  ENDIF

                  PanelRefresh( GetPanelActive, cFileName )

               ELSE

                  IF ( nErrorCode := AScan( FileError(), {| x | x[ 1 ] == FError() } ) ) > 0
                     HC_Alert( "Error", "Test for errors after a binary file operation.;Cannot make file, error:;" + FileError()[ nErrorCode ][ MEANING ] )
                  ELSE
                     HC_Alert( "Error", "Test for errors after a binary file operation.;Cannot make file, error:;" + hb_ntos( FError() ) )
                  ENDIF

               ENDIF
            ENDIF

         ENDIF

         EXIT

      CASE 43 /* select a group of files. */
         FOR i := 1 TO Len( GetPanelActive[ _aDirectory ] )
            IF GetPanelActive[ _aDirectory ][ i ][ F_NAME ] != ".."
               GetPanelActive[ _aDirectory ][ i ][ F_STATUS ] := .F.
            ENDIF
         NEXT
         EXIT

      CASE 95 /* Unselect a group of files. */
         FOR i := 1 TO Len( GetPanelActive[ _aDirectory ] )
            IF GetPanelActive[ _aDirectory ][ i ][ F_NAME ] != ".."
               GetPanelActive[ _aDirectory ][ i ][ F_STATUS ] := .T.
            ENDIF
         NEXT
         EXIT

      OTHERWISE

         IF ( nKeyStd >= 32 .AND. nKeyStd <= 126 ) .OR. ( nKeyStd >= 160 .AND. nKeyStd <= 255 ) .OR. ! hb_keyChar( nKeyStd ) == ""

            GetPanelActive[ _cComdLine ] := Stuff( GetPanelActive[ _cComdLine ], GetPanelActive[ _nComdCol ] + GetPanelActive[ _nComdColNo ] + 1, 0, hb_keyChar( nKeyStd ) )
            IF GetPanelActive[ _nComdCol ] < nMaxCol - Len( GetPanelActive[ _cCurrentDir ] )
               GetPanelActive[ _nComdCol ]++
            ELSE
               GetPanelActive[ _nComdColNo ]++
            ENDIF

         ENDIF

      ENDSWITCH

   ENDDO

   RETURN

STATIC PROCEDURE FunctionKey_F1()

   LOCAL cAddr := "https://github.com/omm/harbour-commander/issues/new"
   LOCAL cStdOut, cOutErr

   IF HC_Alert( "Report an error", ;
         "Creating an issue;" + ;
         ";Issues can be used to keep track of bugs, enhancements, or;" + ;
         "other requests.;" + ;
         ";Any GitHub user can create an issue in a public repository;" + ;
         "where issues have not been disabled.;" + ;
         ";You can open a new issue based on code from an existing pull;" + ;
         "request.;", ;
         { "Click New issue." } ) == 1

#if defined( __PLATFORM__WINDOWS )
      hb_processRun( "start " + cAddr,, @cStdOut, @cOuterr )
      hb_alert( cStdOut + hb_eol() + "and" + hb_eol() + cOutErr )
      //hb_run( "start " + cAddr )
#else
      hb_run( "xdg-open " + cAddr )
#endif

   ENDIF

   RETURN

STATIC PROCEDURE FunctionKey_F2()

   HC_MenuF2()

   RETURN

STATIC PROCEDURE FunctionKey_F3( aPanel )

   LOCAL nPos
   LOCAL /* aTarget := {}, */ aItem /*, aDirScan */
/*   LOCAL nLengthName := 0 */
   LOCAL nSizeDir

   nPos := aPanel[ _nRowBar ] + aPanel[ _nRowNo ]

   /* if we stand on a file */
   IF At( "D", aPanel[ _aDirectory ][ nPos ][ F_ATTR ] ) == 0

      HCEdit( aPanel[ _cCurrentDir ] + aPanel[ _aDirectory ][ nPos ][ F_NAME ], .F. )

   ELSE
      IF aPanel[ _aDirectory ][ nPos ][ F_NAME ] == ".."
         nSizeDir := 0
         FOR EACH aItem IN aPanel[ _aDirectory ]
            nSizeDir += aItem[ 2 ]
         NEXT
      ENDIF
      MsgBox( "Size of files in " + aPanel[ _cCurrentDir ],;
         { transform( nSizeDir, "999,999,999" ) +   " bytes",;
           transform( nSizeDir/ 1024, "999,999,999" ) + "   KiB",;
           transform( nSizeDir/ 1024/ 1024, "999,999.999" ) + "   MiB" }, { "OK" } )

/*
      hb_alert( "Size of files in " + aPanel[ _cCurrentDir ] + hb_eol();
         + transform( nSizeDir, "999,999,999" ) +   " bytes" + hb_eol();
         + transform( nSizeDir/ 1024, "999,999,999" ) + "   KiB" + hb_eol();
         + transform( nSizeDir/ 1024/ 1024, "999,999.999" ) + "   MiB" )
*/
/*
      aDirScan := hb_DirScan( aPanel[ _aDirectory ][ nPos ][ F_NAME ], hb_osFileMask() )
      AScan( aDirScan, {| x | nLengthName := Max( nLengthName, Len( x[ 1 ] ) ) } )

      FOR EACH aItem IN aDirScan
         AAdd( aTarget, ;
            PadR( aItem[ F_NAME ], nLengthName ) + " " + ;
            Transform( hb_ntos( aItem[ F_SIZE ] ), "9 999 999 999" ) + " " + ;
            hb_TToC( aItem[ F_DATE ] ) + " " + ;
            aItem[ F_ATTR ] )
      NEXT

      SaveFile( aTarget, "DirScan.txt" ) // where to save ?

      HCEdit( "DirScan.txt", .F. )
*/
   ENDIF

   RETURN

STATIC PROCEDURE FunctionKey_F4( aPanel )

   LOCAL nPos

   nPos := aPanel[ _nRowBar ] + aPanel[ _nRowNo ]
   /* if we stand on a file go to editing */
   IF At( "D", aPanel[ _aDirectory ][ nPos ][ F_ATTR ] ) == 0
      HCEdit( aPanel[ _cCurrentDir ] + aPanel[ _aDirectory ][ nPos ][ F_NAME ], .T. )
   ELSE
      HC_Alert( "No file selected", "Select the file to edit",, 0x8f )
   ENDIF

   RETURN

STATIC PROCEDURE FunctionKey_F5( aPanel )

   LOCAL nPos
   LOCAL nErrorCode

   nPos := aPanel[ _nRowBar ] + aPanel[ _nRowNo ]
   IF aPanel[ _aDirectory ][ nPos ][ F_NAME ] == ".."
      HC_Alert( "Copy", "The item to be copy has not been selected.",, 0x8f )
   ELSE

      IF aPanel == aPanels[ idPanelLeft ]
         /* if we stand on a file */
         IF At( "D", aPanel[ _aDirectory ][ nPos ][ F_ATTR ] ) == 0
            IF HB_ISSTRING( MsgBox( "Copy file " + '"' + aPanels[ idPanelLeft ][ _aDirectory ][ nPos ][ F_NAME ] + '"' + " to", ;
                  aPanels[ idPanelRight ][ _cCurrentDir ] + aPanels[ idPanelLeft ][ _aDirectory ][ nPos ][ F_NAME ], { "Yes", "No!" } ) )

               // IF hb_vfCopyFile( aPanels[ idPanelLeft ][ _cCurrentDir ] + aPanels[ idPanelLeft ][ _aDirectory ][ nPos ][ F_NAME ], ;
               IF HC_CopyFile( aPanels[ idPanelLeft ][ _cCurrentDir ] + aPanels[ idPanelLeft ][ _aDirectory ][ nPos ][ F_NAME ], ;
                     aPanels[ idPanelRight ][ _cCurrentDir ] + aPanels[ idPanelLeft ][ _aDirectory ][ nPos ][ F_NAME ] ) == 0

                  PanelRefresh( aPanels[ idPanelRight ] )

               ELSE
                  IF ( nErrorCode := AScan( FileError(), {| x | x[ 1 ] == FError() } ) ) > 0
                     HC_Alert( "Error", "Test for errors after a binary file operation.;The file can not be copied;" + FileError()[ nErrorCode ][ MEANING ] )

                  ELSE
                     HC_Alert( "Error", "Test for errors after a binary file operation.;The file can not be copied;" + hb_ntos( FError() ) )
                  ENDIF
               ENDIF

            ENDIF
         ELSE
            IF HB_ISSTRING( MsgBox( "Copy directory " + '"' + aPanels[ idPanelLeft ][ _aDirectory ][ nPos ][ F_NAME ] + '"' + " to", ;
                  aPanels[ idPanelRight ][ _cCurrentDir ], { "Yes", "No!" } ) )

               IF HC_CopyDirectory( aPanels[ idPanelLeft ][ _cCurrentDir ] + aPanels[ idPanelLeft ][ _aDirectory ][ nPos ][ F_NAME ], ;
                     aPanels[ idPanelRight ][ _cCurrentDir ] ) == 0 // ?

                  PanelRefresh( aPanels[ idPanelRight ] )

               ELSE
                  IF ( nErrorCode := AScan( FileError(), {| x | x[ 1 ] == FError() } ) ) > 0
                     HC_Alert( "Error", "Test for errors after a binary file operation.;The directory can not be copied;" + FileError()[ nErrorCode ][ MEANING ] )
                  ELSE
                     HC_Alert( "Error", "Test for errors after a binary file operation.;The directory can not be copied;" + hb_ntos( FError() ) )
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ELSE
         /* if we stand on a file */
         IF At( "D", aPanel[ _aDirectory ][ nPos ][ F_ATTR ] ) == 0
            IF HB_ISSTRING( MsgBox( "Copy file " + '"' + aPanels[ idPanelRight ][ _aDirectory ][ nPos ][ F_NAME ] + '"' + " to", ;
                  aPanels[ idPanelLeft ][ _cCurrentDir ] + aPanels[ idPanelRight ][ _aDirectory ][ nPos ][ F_NAME ], { "Yes", "No!" } ) )

               IF HC_CopyFile( aPanels[ idPanelRight ][ _cCurrentDir ] + aPanels[ idPanelRight ][ _aDirectory ][ nPos ][ F_NAME ], ;
                     aPanels[ idPanelLeft ][ _cCurrentDir ] + aPanels[ idPanelRight ][ _aDirectory ][ nPos ][ F_NAME ] ) == 0

                  PanelRefresh( aPanels[ idPanelLeft ] )

               ELSE
                  IF ( nErrorCode := AScan( FileError(), {| x | x[ 1 ] == FError() } ) ) > 0
                     HC_Alert( "Error", "Test for errors after a binary file operation.;The file can not be copied;" + FileError()[ nErrorCode ][ MEANING ] )
                  ELSE
                     HC_Alert( "Error", "Test for errors after a binary file operation.;The file can not be copied;" + hb_ntos( FError() ) )
                  ENDIF
               ENDIF
            ENDIF
         ELSE
            IF HB_ISSTRING( MsgBox( "Copy directory " + '"' + aPanels[ idPanelRight ][ _aDirectory ][ nPos ][ F_NAME ] + '"' + " to", ;
                  aPanels[ idPanelLeft ][ _cCurrentDir ], { "Yes", "No!" } ) )

               IF HC_CopyDirectory( aPanels[ idPanelRight ][ _cCurrentDir ] + aPanels[ idPanelRight ][ _aDirectory ][ nPos ][ F_NAME ], ;
                     aPanels[ idPanelLeft ][ _cCurrentDir ] ) == 0 // ?

                  PanelRefresh( aPanels[ idPanelLeft ] )

               ELSE
                  IF ( nErrorCode := AScan( FileError(), {| x | x[ 1 ] == FError() } ) ) > 0
                     HC_Alert( "Error", "Test for errors after a binary file operation.;The directory can not be copied;" + FileError()[ nErrorCode ][ MEANING ] )
                  ELSE
                     HC_Alert( "Error", "Test for errors after a binary file operation.;The directory can not be copied;" + hb_ntos( FError() ) )
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   RETURN

STATIC PROCEDURE FunctionKey_F6( aPanel )

   LOCAL nPos
   LOCAL nErrorCode

   nPos := aPanel[ _nRowBar ] + aPanel[ _nRowNo ]
   IF aPanel[ _aDirectory ][ nPos ][ F_NAME ] == ".."
      HC_Alert( "Rename or move", "The item to be copy has not been selected.",, 0x8f )
   ELSE

      IF aPanel == aPanels[ idPanelLeft ]
         /* if we stand on a file */
         IF At( "D", aPanel[ _aDirectory ][ nPos ][ F_ATTR ] ) == 0
            IF HB_ISSTRING( MsgBox( "Move file " + '"' + aPanels[ idPanelLeft ][ _aDirectory ][ nPos ][ F_NAME ] + '"' + " to", ;
                  aPanels[ idPanelRight ][ _cCurrentDir ], { "Yes", "No!" } ) )

               IF HC_CopyFile( aPanels[ idPanelLeft ][ _cCurrentDir ] + aPanels[ idPanelLeft ][ _aDirectory ][ nPos ][ F_NAME ], ;
                     aPanels[ idPanelRight ][ _cCurrentDir ] + aPanels[ idPanelLeft ][ _aDirectory ][ nPos ][ F_NAME ] ) == 0

                  PanelRefresh( aPanels[ idPanelRight ] )

                  IF hb_DirRemoveAll( aPanels[ idPanelLeft ][ _cCurrentDir ] + aPanels[ idPanelLeft ][ _aDirectory ][ nPos ][ F_NAME ] ) == .T.

                     PanelRefresh( aPanels[ idPanelLeft ] )

                  ELSE
                     IF ( nErrorCode := AScan( FileError(), {| x | x[ 1 ] == FError() } ) ) > 0
                        HC_Alert( "Error", "Test for errors after a binary file operation.;Cannot make file, error:;" + FileError()[ nErrorCode ][ MEANING ] )
                     ELSE
                        HC_Alert( "Error", "Test for errors after a binary file operation.;Cannot make file, error:;" + hb_ntos( FError() ) )
                     ENDIF
                  ENDIF

               ELSE
                  IF ( nErrorCode := AScan( FileError(), {| x | x[ 1 ] == FError() } ) ) > 0
                     HC_Alert( "Error", "Test for errors after a binary file operation.;Cannot make file, error:;" + FileError()[ nErrorCode ][ MEANING ] )
                  ELSE
                     HC_Alert( "Error", "Test for errors after a binary file operation.;annot make file, error:;" + hb_ntos( FError() ) )
                  ENDIF
               ENDIF

            ENDIF
         ELSE
            IF HB_ISSTRING( MsgBox( "Move directory " + '"' + aPanels[ idPanelLeft ][ _aDirectory ][ nPos ][ F_NAME ] + '"' + " to", ;
                  aPanels[ idPanelRight ][ _cCurrentDir ], { "Yes", "No!" } ) )

               IF HC_CopyDirectory( aPanels[ idPanelLeft ][ _cCurrentDir ] + aPanels[ idPanelLeft ][ _aDirectory ][ nPos ][ F_NAME ], ;
                     aPanels[ idPanelRight ][ _cCurrentDir ] ) == 0 // ?

                  PanelRefresh( aPanels[ idPanelRight ] )

                  IF hb_DirRemoveAll( aPanels[ idPanelLeft ][ _cCurrentDir ] + aPanels[ idPanelLeft ][ _aDirectory ][ nPos ][ F_NAME ] ) == .T.

                     PanelRefresh( aPanels[ idPanelLeft ] )

                  ELSE
                     IF ( nErrorCode := AScan( FileError(), {| x | x[ 1 ] == FError() } ) ) > 0
                        HC_Alert( "Error", "Test for errors after a binary file operation.;Cannot make directory, error:;" + FileError()[ nErrorCode ][ MEANING ] )
                     ELSE
                        HC_Alert( "Error", "Test for errors after a binary file operation.;Cannot make directory, error:;" + hb_ntos( FError() ) )
                     ENDIF
                  ENDIF

               ELSE
                  IF ( nErrorCode := AScan( FileError(), {| x | x[ 1 ] == FError() } ) ) > 0
                     HC_Alert( "Error", "Test for errors after a binary file operation.;Cannot make directory, error:;" + FileError()[ nErrorCode ][ MEANING ] )
                  ELSE
                     HC_Alert( "Error", "Test for errors after a binary file operation.;Cannot make directory, error:;" + hb_ntos( FError() ) )
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ELSE
         /* if we stand on a file */
         IF At( "D", aPanel[ _aDirectory ][ nPos ][ F_ATTR ] ) == 0
            IF HB_ISSTRING( MsgBox( "Move file " + '"' + aPanels[ idPanelRight ][ _aDirectory ][ nPos ][ F_NAME ] + '"' + " to", ;
                  aPanels[ idPanelLeft ][ _cCurrentDir ], { "Yes", "No!" } ) )

               IF HC_CopyFile( aPanels[ idPanelRight ][ _cCurrentDir ] + aPanels[ idPanelRight ][ _aDirectory ][ nPos ][ F_NAME ], ;
                     aPanels[ idPanelLeft ][ _cCurrentDir ] + aPanels[ idPanelRight ][ _aDirectory ][ nPos ][ F_NAME ] ) == 0

                  PanelRefresh( aPanels[ idPanelLeft ] )

                  IF hb_DirRemoveAll( aPanels[ idPanelRight ][ _cCurrentDir ] + aPanels[ idPanelRight ][ _aDirectory ][ nPos ][ F_NAME ] ) == .T.

                     PanelRefresh( aPanels[ idPanelRight ] )

                  ELSE
                     IF ( nErrorCode := AScan( FileError(), {| x | x[ 1 ] == FError() } ) ) > 0
                        HC_Alert( "Error", "Test for errors after a binary file operation.;Cannot make file, error:;" + FileError()[ nErrorCode ][ MEANING ] )
                     ELSE
                        HC_Alert( "Error", "Test for errors after a binary file operation.;Cannot make file, error:;" + hb_ntos( FError() ) )
                     ENDIF
                  ENDIF

               ELSE
                  IF ( nErrorCode := AScan( FileError(), {| x | x[ 1 ] == FError() } ) ) > 0
                     HC_Alert( "Error", "Test for errors after a binary file operation.;Cannot make file, error:;" + FileError()[ nErrorCode ][ MEANING ] )
                  ELSE
                     HC_Alert( "Error", "Test for errors after a binary file operation.;Cannot make file, error:;" + hb_ntos( FError() ) )
                  ENDIF
               ENDIF
            ENDIF
         ELSE
            IF HB_ISSTRING( MsgBox( "Move directory " + '"' + aPanels[ idPanelRight ][ _aDirectory ][ nPos ][ F_NAME ] + '"' + " to", ;
                  aPanels[ idPanelLeft ][ _cCurrentDir ], { "Yes", "No!" } ) )

               IF HC_CopyDirectory( aPanels[ idPanelRight ][ _cCurrentDir ] + aPanels[ idPanelRight ][ _aDirectory ][ nPos ][ F_NAME ], ;
                     aPanels[ idPanelLeft ][ _cCurrentDir ] ) == 0 // ?

                  PanelRefresh( aPanels[ idPanelLeft ] )

                  IF hb_DirRemoveAll( aPanels[ idPanelRight ][ _cCurrentDir ] + aPanels[ idPanelRight ][ _aDirectory ][ nPos ][ F_NAME ] ) == .T.

                     PanelRefresh( aPanels[ idPanelRight ] )

                  ELSE
                     IF ( nErrorCode := AScan( FileError(), {| x | x[ 1 ] == FError() } ) ) > 0
                        HC_Alert( "Error", "Test for errors after a binary file operation.;Cannot make directory, error:;" + FileError()[ nErrorCode ][ MEANING ] )
                     ELSE
                        HC_Alert( "Error", "Test for errors after a binary file operation.;Cannot make directory, error:;" + hb_ntos( FError() ) )
                     ENDIF
                  ENDIF

               ELSE
                  IF ( nErrorCode := AScan( FileError(), {| x | x[ 1 ] == FError() } ) ) > 0
                     HC_Alert( "Error", "Test for errors after a binary file operation.;Cannot make directory, error:;" + FileError()[ nErrorCode ][ MEANING ] )
                  ELSE
                     HC_Alert( "Error", "Test for errors after a binary file operation.;Cannot make directory, error:;" + hb_ntos( FError() ) )
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   /* if we remove being the RowBar indicator in the last position then - 1 */
// IF Len( aPanel[ _aDirectory ] ) < aPanel[ _nRowBar ] + aPanel[ _nRowNo ]
// aPanel[ _nRowBar ] -= 1
// ENDIF

   RETURN

STATIC PROCEDURE FunctionKey_F7( aPanel )

   LOCAL cNewDir
   LOCAL nErrorCode

   IF HB_ISSTRING( cNewDir := MsgBox( "Create the directory.", NIL, { "Yes", "No!" } ) )

      IF hb_vfDirMake( aPanel[ _cCurrentDir ] + cNewDir ) == 0

         PanelRefresh( aPanel, cNewDir )

      ELSE
         IF ( nErrorCode := AScan( FileError(), {| x | x[ 1 ] == FError() } ) ) > 0
            HC_Alert( "Error", "Test for errors after a binary file operation.;Cannot make directory, error:;" + FileError()[ nErrorCode ][ MEANING ] )
         ELSE
            HC_Alert( "Error", "Test for errors after a binary file operation.;Cannot make directory, error:;" + hb_ntos( FError() ) )
         ENDIF
      ENDIF
   ENDIF

   RETURN

STATIC PROCEDURE FunctionKey_F8( aPanel )

   LOCAL nPos
   LOCAL nErrorCode

   nPos := aPanel[ _nRowBar ] + aPanel[ _nRowNo ]

   IF aPanel[ _aDirectory ][ nPos ][ F_NAME ] == ".."
      HC_Alert( "Up Directory", "The item to be deleted has not been selected." )
   ELSE
      nPos := aPanel[ _nRowBar ] + aPanel[ _nRowNo ]
      /* if we stand on a file */
      IF At( "D", aPanel[ _aDirectory ][ nPos ][ F_ATTR ] ) == 0

         /* Marking the deletion status of the current item in the table */
         IF GetPanelActive[ _aDirectory ][ nPos ][ F_STATUS ]
            GetPanelActive[ _aDirectory ][ nPos ][ F_STATUS ] := .F.
         ENDIF

         PanelDisplay( aPanel )

         IF HC_Alert( "Delete file", "Do you really want to delete the selected file:;" + '"' + aPanel[ _aDirectory ][ nPos ][ F_NAME ] + '"', { "Yes", "No!" } ) == 1

            // IF hb_vfErase( aPanel[ _cCurrentDir ] + aPanel[ _aDirectory ][ nPos ][ F_NAME ] ) == 0
            IF HC_DeleteFile( GetPanelActive ) == 0

               PanelRefresh( aPanel )

            ELSE
               IF ( nErrorCode := AScan( FileError(), {| x | x[ 1 ] == FError() } ) ) > 0
                  HC_Alert( "Error", "Test for errors after a binary file operation.;Cannot make file, error:;" + FileError()[ nErrorCode ][ MEANING ] )
               ELSE
                  HC_Alert( "Error", "Test for errors after a binary file operation.;Cannot make file, error:;" + hb_ntos( FError() ) )
               ENDIF
            ENDIF
         ELSE
            /* return the delete status of the current item in the array */
            GetPanelActive[ _aDirectory ][ nPos ][ F_STATUS ] := .T.
         ENDIF

      ELSE
         /* if we stand on the catalog */
         IF HC_Alert( "Down Directory", "Do you really want to delete the selected directory:;" + '"' + aPanel[ _aDirectory ][ nPos ][ F_NAME ] + '"', { "Yes", "No!" }, 0x9f ) == 1
            IF hb_vfDirRemove( aPanel[ _cCurrentDir ] + aPanel[ _aDirectory ][ nPos ][ F_NAME ] ) == 0

               PanelRefresh( aPanel )

            ELSE
               IF HC_Alert( "Down Directory", "The following subdirectory is not empty. ;" + ;
                     '"' + aPanel[ _aDirectory ][ nPos ][ F_NAME ] + '"' + ";" + ;
                     "Do you still wish to delete it?", { "Delete", "No!" } ) == 1

                  IF hb_DirRemoveAll( aPanel[ _cCurrentDir ] + aPanel[ _aDirectory ][ nPos ][ F_NAME ] ) == .T.

                     PanelRefresh( aPanel )

                  ELSE
                     IF ( nErrorCode := AScan( FileError(), {| x | x[ 1 ] == FError() } ) ) > 0
                        HC_Alert( "Error", "Test for errors after a binary file operation.;Cannot make directory, error:;" + FileError()[ nErrorCode ][ MEANING ] )
                     ELSE
                        HC_Alert( "Error", "Test for errors after a binary file operation.;Cannot make directory, error:;" + hb_ntos( FError() ) )
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDIF


   /* if we remove being the RowBar + RowNo indicator in the last position then - 1 */
// IF Len( aPanel[ _aDirectory ] ) < aPanel[ _nRowBar ] + aPanel[ _nRowNo ]
// aPanel[ _nRowBar ] -= 1
// ENDIF

   RETURN

STATIC PROCEDURE FunctionKey_F9( )

   LOCAL nTop, nLeft, nRight
   LOCAL /*cScreen,*/ cScreenSub
   LOCAL nKey, nKeyStd
   LOCAL lReDraw := .T.
   LOCAL lContinue := .T.
   LOCAL i
   LOCAL iMenu := 1
   LOCAL aF9Menu := { "Left", "Option", "Right" }
   LOCAL aF9MenuPos := { 1 }
   LOCAL aF9SubMenu := { "Option" => { "Panel settings", "Save settings" } }
   LOCAL nResult
   LOCAL nBottomSub
   LOCAL nLeftSub

   nTop := 0
   nLeft := 0

   WSelect( 0 )
   nRight := MaxCol()

   aEval( aF9Menu, {| x, y | Aadd( aF9MenuPos, aF9MenuPos[ y ] + Len( x ) + 1 ) },, Len( aF9Menu ) - 1 )

//   cScreen := SaveScreen( nTop, nLeft, nTop, nRight )

//   hb_DispBox( nTop, nLeft, nTop, nRight, hb_UTF8ToStrBox( " █       " ), 0x22 )
   WOpen( nTop, nLeft, nTop, nRight )
   hb_DispBox( 0, 0, nTop, nRight, hb_UTF8ToStrBox( " █       " ), 0x22 )

   DO WHILE lContinue
      IF lReDraw
         DispBegin()
         FOR EACH i IN aF9Menu
            hb_DispOutAt( nTop, aF9MenuPos[ i:__enumIndex ], i, iif( i:__enumIndex == iMenu, 0x8f, 0x20 ) )
         NEXT
         DispEnd()
         lReDraw := .F.
      ENDIF

      nKey := Inkey( 0 )
      nKeyStd := hb_keyStd( nKey )

      SWITCH nKeyStd
      CASE K_ESC
         lContinue := .F.
         ShoWSession()
         EXIT

      CASE K_LEFT
         iMenu --
         IF iMenu < 1
            iMenu := Len( aF9Menu )
         ENDIF
         lReDraw := .T.
         EXIT

      CASE K_RIGHT
         iMEnu ++
         IF iMenu > Len( aF9Menu )
            iMenu := 1
         ENDIF
         lReDraw := .T.
         EXIT

      CASE K_ENTER
         IF HB_HHasKey( aF9SubMenu, aF9Menu[ iMenu ] )
            nLeftSub := 0
            nBottomSub := Min( nTop + 1 + Len( aF9SubMenu[ aF9Menu[ iMenu ] ] ) + 1, MaxRow() )
            aEval( aF9SubMenu[ aF9Menu[ iMenu ] ], {| x | nLeftSub := Max( Len( x ), nLeftSub ) } )
            nLeftSub := Min( aF9MenuPos[ iMenu ] + nLeftSub + 1, MaxCol() )
            cScreenSub := SaveScreen( nTop + 1, aF9MenuPos[ iMenu ], nBottomSub, nLeftSub )
            hb_DispBox( nTop + 1, aF9MenuPos[ iMenu ], nBottomSub, nLeftSub, HB_B_DOUBLE_UNI + " " /*hb_UTF8ToStrBox( " _       " )*/, 0x8a )
            nResult := Achoice( nTop + 2, aF9MenuPos[ iMenu ] + 1, nBottomSub -1, nLeftSub - 1, aF9SubMenu[ aF9Menu[ iMenu ] ] )
            if nResult > 0
               HC_ALERT( "SubMenu", "Your choice is '" + aF9SubMenu[ aF9Menu[ iMenu ] ][ nResult ] + "'" )
            endif
            RestScreen( nTop + 1, aF9MenuPos[ iMenu ], nBottomSub, nLeftSub, cScreenSub )
         ELSE
            HC_ALERT( "Warning", "Not implemented yet",, 0x8f )
         ENDIF
         EXIT
      ENDSWITCH

   ENDDO

//   RestScreen( nTop, nLeft, nTop, nRight, cScreen )

   RETURN

// STATIC PROCEDURE FunctionKey_F10( aPanel )
// RETURN

STATIC FUNCTION HC_CopyFile( cSourceFile, cTargetFile )

   LOCAL nRow, nCol
   LOCAL nWidth
   LOCAL nTop, nLeft, nBottom, nRight
   LOCAL cScreen
   LOCAL nReturn := 0
   LOCAL nMaxRow := MaxRow(), nMaxCol := MaxCol()
   LOCAL pSource
   LOCAL pTarget
   LOCAL nPosition
   LOCAL nBufferSize := 65536
   LOCAL cBuffer
   LOCAL tsDateTime
   LOCAL nFileSize

   nWidth := Max( Len( cSourceFile ), Len( cTargetFile ) ) + 2

   nRow := Int( nMaxRow / 3 )
   nCol := Int( ( nMaxCol - nWidth ) / 2 )

   nTop    := nRow
   nLeft   := nCol - 1
   nBottom := nRow + 7
   nRight  := nCol + nWidth

   cScreen := SaveScreen( nTop, nLeft, nBottom + 1, nRight + 2 )
   hb_DispBox( nTop, nLeft, nBottom, nRight, HB_B_DOUBLE_UNI + " ", 0x8f )
   hb_Shadow( nTop, nLeft, nBottom, nRight )

   cBuffer := Space( nBufferSize )

   /* file is opened for reading, do not deny any further attempts to open the file */
   IF ( pSource := hb_vfOpen( cSourceFile, FO_READ + FO_SHARED + FXO_SHARELOCK ) ) != NIL

      /* shared lock, file is opened for writing, deny further attempts to open the file, emulate DOS SH_DENY* mode in POSIX OS */
      IF ( pTarget := hb_vfOpen( cTargetFile, FO_CREAT + FO_WRITE + FO_EXCLUSIVE + FXO_SHARELOCK ) ) != NIL

         hb_DispOutAt( ++nRow, nCol, PadC( "Copying the file", nWidth ), 0x8f )
         hb_DispOutAt( ++nRow, nCol, PadC( cSourceFile, nWidth ), 0x8f )
         hb_DispOutAt( ++nRow, nCol, PadC( "to", nWidth ), 0x8f )
         hb_DispOutAt( ++nRow, nCol, PadC( cTargetFile, nWidth ), 0x8f )

         /* FS_SET Seek from beginning of file, FS_END Seek from end of file */
         nPosition := hb_vfSeek( pSource, FS_SET, FS_END )
         nFileSize := nPosition

         hb_vfSeek( pSource, FS_SET )

         DO WHILE ( nPosition > FS_SET )

            IF nPosition < nBufferSize
               nBufferSize := nPosition
            ENDIF

            IF nBufferSize != hb_vfRead( pSource, @cBuffer, nBufferSize )
               nReturn := FError()
               EXIT
            ENDIF
            IF nBufferSize != hb_vfWrite( pTarget, @cBuffer, nBufferSize )
               nReturn := FError()
               EXIT
            ENDIF

            nPosition -= nBufferSize

            DispBegin()
            hb_DispOutAt( ++nRow, nCol, PadC( hb_ntos( 100 * ( nFileSize - nPosition ) / nFileSize ) + " %", nWidth ), 0x8a )

            hb_DispOutAt( ++nRow, nCol, Replicate( " ", nWidth ), 0x0 )
            hb_DispOutAt(   nRow, nCol, Replicate( " ", nWidth * ( nFileSize - nPosition ) / nFileSize ), 0x22 )

            DispEnd()
            nRow := Int( nMaxRow / 3 ) + 4

         ENDDO

         hb_vfClose( pTarget )
         hb_vfClose( pSource )

         /* get date file time, set date file time */
         hb_vfTimeGet( cSourceFile, @tsDateTime )
         hb_vfTimeSet( cTargetFile, tsDateTime )

      ELSE

         hb_vfClose( pSource )
         nReturn := FError()

      ENDIF
   ENDIF

   RestScreen( nTop, nLeft, nBottom + 1, nRight + 2, cScreen )

   RETURN nReturn

STATIC FUNCTION HC_CopyDirectory( cSourceFile, cTargetFile )

   LOCAL aCatalog
   LOCAL nRows
   LOCAL i
   LOCAL cSubCat

   cSubCat := hb_FNameNameExt( cSourceFile )
   IF hb_DirCreate( cTargetFile + cSubCat ) != 0
      RETURN FError()
   ENDIF

   aCatalog := hb_vfDirectory( cSourceFile + hb_ps(), "HSD" )
   nRows    := Len( aCatalog )

   FOR i := 1 TO nRows
      IF aCatalog[ i ][ F_NAME ] == "." .OR. aCatalog[ i ][ F_NAME ] == ".."

      ELSEIF "D" $ aCatalog[ i ][ F_ATTR ]
         IF HC_CopyDirectory( cSourceFile + hb_ps() + aCatalog[ i ][ F_NAME ], cTargetFile + cSubCat + hb_ps() ) == -1
            RETURN FError()
         ENDIF
      ELSE

         IF HC_CopyFile( cSourceFile + hb_ps() + aCatalog[ i ][ F_NAME ], cTargetFile + cSubCat + hb_ps() + aCatalog[ i ][ F_NAME ] ) != 0
            RETURN FError()
         ENDIF

      ENDIF

   NEXT

   RETURN 0

STATIC FUNCTION HC_DeleteFile( aPanel )  // ?

   LOCAL i

   FOR i := 1 TO Len( aPanel[ _aDirectory ] )

      IF aPanel[ _aDirectory ][ i ][ F_STATUS ] == .F.

         DO WHILE hb_vfErase( aPanel[ _cCurrentDir ] + aPanel[ _aDirectory ][ i ][ F_NAME ] ) == -1 // ?
            RETURN FError()
         ENDDO

         /* if we remove being the RowBar or RowBar + RowNo indicator in the last position then - 1 */
         // IF Len( aPanel[ _aDirectory ] ) < aPanel[ _nRowBar ] + aPanel[ _nRowNo ]       // how to remove nRowNo nRowBar?
         aPanel[ _nRowBar ] -= 1
         // ENDIF

      ENDIF

   NEXT

   RETURN 0

STATIC PROCEDURE PanelDisplay( aPanel )

   LOCAL nRow, nPos := 1
   LOCAL nLengthName := 4 /* 4 is len of top element ".." plus brackets "[" and "]" */
   LOCAL nLengthSize := 0

   AScan( aPanel[ _aDirectory ], {| x | ;
      nLengthName := Max( nLengthName, Len( x[ 1 ] ) ), ;
      nLengthSize := Max( nLengthSize, Len( Str( x[ 2 ] ) ) ) } )

   IF aPanel[ _nWinHndl ] == NIL
      SetColor( NToColor( 0x1f ) )
      aPanel[ _nWinHndl ] := WOpen( aPanel[ _nTop ], aPanel[ _nLeft ], aPanel[ _nBottom ], aPanel[ _nRight ] )
//      SetCursor( SC_NONE )
      //@-1,1 say hb_ntos( aPanel[ _nWinHndl ] ) + ", " + hb_ntos( wselect() )
   ELSE
      WSelect( aPanel[ _nWinHndl ] )
      hb_Scroll()
   ENDIF
   WFormat()
   IF GetPanels[ idPanelActive ] == aPanel[ _nIdPanel ]
      WBox( )
   ELSE
      WBox( 1 )
   ENDIF

   DispBegin()
   nPos += aPanel[ _nRowNo ]
   FOR nRow := aPanel[ _nTop ] TO aPanel[ _nBottom ] - 1

      IF nPos <= Len( aPanel[ _aDirectory ] )
         hb_DispOutAt( nRow, 0, ;
            PadR( Expression( nLengthName, nLengthSize, ;
            aPanel[ _aDirectory ][ nPos ][ F_NAME ], ;
            aPanel[ _aDirectory ][ nPos ][ F_SIZE ], ;
            aPanel[ _aDirectory ][ nPos ][ F_DATE ], ;
            aPanel[ _aDirectory ][ nPos ][ F_ATTR ] ), ;
            aPanel[ _nRight ] - aPanel[ _nLeft ] - 1 ), ;
            iif( GetPanelActive[ _nIdPanel ] == aPanel[ _nIdPanel ] .AND. nPos == aPanel[ _nRowBar ] + aPanel[ _nRowNo ], ;
            iif( ! aPanel[ _aDirectory ][ nPos ][ F_STATUS ], 0x3e, 0x30 ), ;
            ColoringSyntax( aPanel[ _aDirectory ][ nPos ][ F_ATTR ], aPanel[ _aDirectory ][ nPos ][ F_STATUS ] ) ) )
         ++nPos
      ELSE
         EXIT
      ENDIF

   NEXT

   PanelTitleDisplay( aPanel )

   DispEnd()

   RETURN

STATIC PROCEDURE ComdLineDisplay( )

//   LOCAL nMaxRow, nMaxCol
   LOCAL cPromptEnd
   LOCAL aPanel := GetPanelActive
/*
   LOCAL nWin0 := wselect()
   local nwnum := wnum()
   local wrow := wlastrow()
   local wcol := wlastcol()
   local wlastrow := wlastrow()
   local wlastcol := wlastcol()
*/

   IF "Windows" $ os()
      cPromptEnd := ">"
   ELSE
      cPromptEnd := "$"
   ENDIF
   WSelect( aConfig[ nCmdHndl ] )
/*
   nwin0 := wselect()
   wrow := wrow()
   wcol := wcol()
   wlastrow := wlastrow()
   wlastcol := wlastcol()
*/
   DispBegin()

   hb_DispOutAt( 0, 0, ;
      PadR( aPanel[ _cCurrentDir ] + cPromptEnd + SubStr( aPanel[ _cComdLine ], 1 + aPanel[ _nComdColNo ], MaxCol() + aPanel[ _nComdColNo ] ), MaxCol() ), 0x7 )

   //SetPos( WLastRow() - 1, aPanel[ _nComdCol ] + Len( aPanel[ _cCurrentDir ] ) + 1 )
   SetPos( 0, aPanel[ _nComdCol ] + Len( aPanel[ _cCurrentDir ] ) + 1 )

   DispEnd()
   
   RETURN

STATIC PROCEDURE PanelTitleDisplay( aPanel )

   LOCAL cPanelTitle := aPanel[ _cCurrentDir ]
   LOCAL nWidthPanel

   nWidthPanel := WLastCol() - WCol() - 1 /*aPanel[ _nRight ] - aPanel[ _nLeft ] - 2*/
   IF Len( cPanelTitle ) < nWidthPanel
      cPanelTitle := PadR( aPanel[ _cCurrentDir ], Min( Len( aPanel[ _cCurrentDir ] ), nWidthPanel ), Space( 1 ) )
   ELSE
      cPanelTitle := SubStr( cPanelTitle, 1, 3 ) + "..." + Right( cPanelTitle, nWidthPanel - 5 )
   ENDIF

   DispBegin()

   hb_DispOutAt( -1, 0, cPanelTitle, "W/B" )

   DispEnd()

   RETURN

STATIC FUNCTION Expression( nLengthName, nLengthSize, cName, cSize, dDate, cAttr )

   LOCAL cFileName, cFileSize, dFileDate, cFileAttr

   cFileName := PadR( cName + Space( nLengthName ), nLengthName ) + " "

   IF cName == ".."
      cFileName := PadR( "[" + AllTrim( cFileName ) + "]" + Space( nLengthName ), nLengthName ) + " "
   ENDIF

   IF cAttr == "D" .OR. cAttr == "HD" .OR. cAttr == "HSD" .OR. cAttr == "HSDL" .OR. cAttr == "RHSA" .OR. cAttr == "RD" .OR. cAttr == "AD" .OR. cAttr == "RHD"
      cFileSize := PadL( Iif( rTrim( cFileName ) == "[..]", "UP--DIR", "DIR"), nLengthSize + 3 ) + " "
   ELSE
      cFileSize := PadL( Transform( cSize, "9 999 999 999" ), nLengthSize + 3 ) + " "
   ENDIF

   dFileDate := hb_TToC( dDate ) + " "
   cFileAttr := PadL( cAttr, 3 )

   RETURN cFileName + cFileSize + dFileDate + cFileAttr

STATIC FUNCTION ColoringSyntax( cAttr, lStatus )

   LOCAL nColor

   IF cAttr == "HD" .OR. cAttr == "HSD" .OR. cAttr == "HSDL" .OR. cAttr == "RHSA" .OR. cAttr == "RD"
      nColor := 0x13
   ELSE
      nColor := 0x1f
   ENDIF

   IF ! lStatus
      nColor := 0x1e
   ENDIF

   RETURN nColor

STATIC PROCEDURE PanelRefresh( aPanel, nRowBar2cDir )

   LOCAL _nRowBarNew

   IF aPanels[ idPanelLeft ][ _cCurrentDir ] == aPanels[ idPanelRight ][ _cCurrentDir ]

      PanelFetchList( aPanels[ idPanelLeft ], aPanels[ idPanelLeft ][ _cCurrentDir ] )
      PanelFetchList( aPanels[ idPanelRight ], aPanels[ idPanelRight ][ _cCurrentDir ] )

      IF HB_ISSTRING( nRowBar2cDir )
         _nRowBarNew := GetRowBarPos( aPanels[ idPanelLeft ], nRowBar2cDir )
         aPanels[ idPanelLeft ][ _nRowBar ] := _nRowBarNew
         aPanels[ idPanelRight ][ _nRowBar ] := _nRowBarNew
      ENDIF

      PanelDisplay( aPanels[ idPanelLeft ] )
      PanelDisplay( aPanels[ idPanelRight ] )

   ELSE

      PanelFetchList( aPanel, aPanel[ _cCurrentDir ] )

      IF HB_ISSTRING( nRowBar2cDir )
         _nRowBarNew := GetRowBarPos( aPanel, nRowBar2cDir )
         aPanel[ _nRowBar ] := _nRowBarNew
      ENDIF

      PanelDisplay( aPanel )

   ENDIF

   RETURN

STATIC FUNCTION GetRowBarPos( aPanel, nRowBar2cDir )

   LOCAL nPos

   nPos := Ascan( aPanel[ _aDirectory ], {| x | x[ F_NAME ] == nRowBar2cDir } )

   RETURN iif( nPos == 0, 1, nPos )

PROCEDURE ShowSession( nMode )

   LOCAL aWin

   IF nMode == NIL
      aWin := {;
            iif( IsPanelActive( idPanelLeft ), GetPanels[ idPanelRight, _nWinHndl ], GetPanels[ idPanelLeft, _nWinHndl ] ),;
            iif( IsPanelActive( idPanelLeft ), GetPanels[ idPanelLeft, _nWinHndl ], GetPanels[ idPanelRight, _nWinHndl ] ) }

      WSelect( aWin[ 1 ] )
      WSelect( aWin[ 2 ] )
   ELSEIF nMode == 1
      WSelect( 0, .T. )
   ENDIF
   WSelect( aConfig[ nCmdHndl ] )

   RETURN

STATIC PROCEDURE ChangeDir( aPanel )

   LOCAL nPos, cDir, cDir0
   LOCAL nPosLast

   nPos := aPanel[ _nRowBar ] + aPanel[ _nRowNo ]
   IF At( "D", aPanel[ _aDirectory ][ nPos ][ F_ATTR ] ) == 0
      RETURN
   ENDIF
   IF aPanel[ _aDirectory ][ nPos ][ F_NAME ] == ".."
      cDir := aPanel[ _cCurrentDir ]
      cDir0 := SubStr( cDir, RAt( hb_ps(), Left( cDir, Len( cDir ) - 1 ) ) + 1 )
      cDir0 := SubStr( cDir0, 1, Len( cDir0 ) - 1 )
      /*cDir*/ aPanel[ _cCurrentDir ]  := Left( cDir, RAt( hb_ps(), Left( cDir, Len( cDir ) - 1 ) ) )
      PanelFetchList( aPanel )
      nPosLast := Max( AScan( aPanel[ _aDirectory ], {| x | x[ F_NAME ] == cDir0 } ), 1 )

      IF nPosLast > GetPanelActive[ _nBottom ] - 1
         GetPanelActive[ _nRowNo ] := nPosLast % ( GetPanelActive[ _nBottom ] - 1 )
         GetPanelActive[ _nRowBar ] := GetPanelActive[ _nBottom ] - 1
      ELSE
         GetPanelActive[ _nRowNo ]  := 0
         GetPanelActive[ _nRowBar ] := nPosLast
      ENDIF

   ELSE
      aPanel[ _cCurrentDir ] += aPanel[ _aDirectory ][ nPos ][ F_NAME ] + hb_ps()
      aPanel[ _nRowBar ] := 1
      aPanel[ _nRowNo  ] := 0
      PanelFetchList( aPanel )
   ENDIF

   RETURN

STATIC FUNCTION AllDrives()

   LOCAL i
   LOCAL aArrayDrives := {}

   FOR i := 1 TO 26
      IF DiskChange( Chr( i + 64 ) )
         AAdd( aArrayDrives, Chr( i + 64 ) )
      ENDIF
   NEXT

   RETURN aArrayDrives

STATIC PROCEDURE BottomBar()

   LOCAL nRow
   LOCAL cSpaces
   LOCAL nCol

   WSelect( aConfig[ nBBarHndl ] )
   nRow := MaxRow()
   nCol := Int( MaxCol() / 10 ) + 1

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

STATIC FUNCTION MsgBox( cMessage, aMessage, aOptions )

   LOCAL nMaxRow := 0, nMaxCol := 0
   LOCAL cScreen
   LOCAL aOptionsOK := {}, aPosButtons
   LOCAL lContinue := .T.
   LOCAL i
   LOCAL nChoice := 1
   LOCAL nOpWidth, nWidth, nInitCol, expValue
   LOCAL nOldRow, nOldCol
   LOCAL nKey, nKeyStd
   LOCAL cString
   LOCAL nCol := 0, nColNo := 0

   nOldRow := Row()
   nOldCol := Col()

   FOR EACH i IN hb_defaultValue( aOptions, {} )
      IF HB_ISSTRING( i ) .AND. ! i == ""
         AAdd( aOptionsOK, i )
      ENDIF
   NEXT

   IF Len( aOptionsOK ) == 0
      aOptionsOK := { "Ok" }
   ENDIF

   /* aMessage is not currently an array */
   IF Empty( aMessage )
      cString := ""
   ELSE
      cString := aMessage
      nCol := Len( aMessage )
   ENDIF

   DO WHILE lContinue

      DispBegin()
      IF nMaxRow != Int( MaxRow() / 3 ) .OR. nMaxCol != Int( MaxCol() / 2 )

         nMaxRow := Int( MaxRow() / 3 )
         nMaxCol := Int( MaxCol() / 2 )

         aPosButtons := {}
         nOpWidth := 0

         cScreen := SaveScreen( nMaxRow - 2, nMaxCol - 36, nMaxRow + 4, nMaxCol + 38 )

         AEval( aOptionsOK, {| x | nOpWidth += Len( x ) + 4 } )

         nWidth := nOpWidth + 2
         nInitCol := Int( ( ( MaxCol() - ( nWidth + 2 ) ) / 2 ) + 0.5 )
         expValue := nInitCol + Int( ( nWidth - nOpWidth ) / 2 ) + 2
         AEval( aOptionsOK, {| x | AAdd( aPosButtons, expValue ), expValue += Len( x ) + 4 } )

         hb_DispBox( nMaxRow - 2, nMaxCol - 36, nMaxRow + 3, nMaxCol + 36, HB_B_SINGLE_UNI + " ", 0x8f )
         hb_Shadow( nMaxRow - 2, nMaxCol - 36, nMaxRow + 3, nMaxCol + 36 )
         hb_DispOutAt( nMaxRow - 1, nMaxCol - 34, cMessage, 0x8f )

         FOR i := 1 TO Len( aOptionsOK )
            hb_DispOutAt( nMaxRow + 2, aPosButtons[ i ], " " + aOptionsOK[ i ] + " ", iif( i == nChoice, 0x07, 0x8f ) )
         NEXT

         MsgBoxDisplay( cString, nCol, nColNo )

      ENDIF
      DispEnd()

      MsgBoxDisplay( cString, nCol, nColNo )

      nKey := Inkey( 0 )
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

         EXIT

      CASE K_RIGHT

         IF nCol < 68 .AND. nCol < Len( cString )
            nCol++
         ELSE
            IF nColNo + nCol < Len( cString )
               nColNo++
            ENDIF
         ENDIF

         EXIT

      CASE K_HOME

         nCol := 0

         EXIT

      CASE K_END

         nCol := Len( cString )

         EXIT

      CASE K_DEL

         IF nCol >= 0
            cString := Stuff( cString, nCol + 1, 1, "" )
         ENDIF

         EXIT

      CASE K_BS

         IF nCol > 0
            cString := Stuff( cString, nCol, 1, "" )
            nCol--
         ENDIF

         EXIT

      CASE K_TAB
         IF Len( aOptionsOK ) > 1
            nChoice++
            IF nChoice > Len( aOptionsOK )
               nChoice := 1
            ENDIF
         ENDIF

         FOR i := 1 TO Len( aOptionsOK )
            hb_DispOutAt( nMaxRow + 2, aPosButtons[ i ], " " + aOptionsOK[ i ] + " ", iif( i == nChoice, 0x07, 0x8f ) )
         NEXT

         EXIT

      CASE HB_K_RESIZE

         hb_Scroll()
         AutoSize()

         PanelDisplay( aPanels[ idPanelLeft ] )
         PanelDisplay( aPanels[ idPanelRight ] )

         ComdLineDisplay( )

         BottomBar()

         EXIT

      OTHERWISE

         IF ( nKeyStd >= 32 .AND. nKeyStd <= 126 ) .OR. ( nKeyStd >= 160 .AND. nKeyStd <= 255 ) .OR. ! hb_keyChar( nKeyStd ) == ""

            cString := Stuff( cString, nCol + nColNo + 1, 0, hb_keyChar( nKeyStd ) )
            IF nCol < 68
               nCol++
            ELSE
               nColNo++
            ENDIF

         ENDIF

      ENDSWITCH

   ENDDO

   RestScreen( nMaxRow - 2, nMaxCol - 36, nMaxRow + 4, nMaxCol + 38, cScreen )
   SetPos( nOldRow, nOldCol )

   RETURN iif( nChoice == 1, iif( Empty( cString ), 0, cString ), 0 ) // ??

STATIC PROCEDURE MsgBoxDisplay( cString, nCol, nColNo )

   LOCAL nMaxRow := Int( MaxRow() / 3 ), nMaxCol := Int( MaxCol() / 2 )

   DispBegin()

   hb_DispOutAt( nMaxRow, nMaxCol - 34, PadR( SubStr( cString, 1 + nColNo, 69 + nColNo ), 69 ) )

   SetPos( nMaxRow, nMaxCol - 34 + nCol )

   DispEnd()

   RETURN

STATIC FUNCTION HC_Alert( cTitle, xMessage, xOptions, nColorNorm, nArg )

   LOCAL nOldCursor := SetCursor( SC_NONE )

   // LOCAL nRowPos := Row(), nColPos := Col()
   LOCAL aMessage, aOptions, aPos
   LOCAL nColorHigh
   LOCAL nLenOptions, nLenMessage
   LOCAL nWidth := 0
   LOCAL nLenght := 0
   LOCAL nPos
   LOCAL i
   LOCAL nMaxRow := 0, nMaxCol := 0
   LOCAL nRow, nCol
   LOCAL nKey, nKeyStd
   LOCAL nTop, nLeft, nBottom, nRight
   LOCAL nChoice := 1
   LOCAL nMRow, nMCol
/*   LOCAL nWinAlert */

   DO CASE
   CASE ValType( cTitle ) == "U"
      cTitle := "OK"
   ENDCASE

   DO CASE
   CASE ValType( xMessage ) == "U"
      aMessage := { "" }
   CASE ValType( xMessage ) == "C"
      aMessage := hb_ATokens( xMessage, ";" )
   CASE ValType( xMessage ) == "A"
      aMessage := xMessage
   CASE ValType( xMessage ) == "N"
      aMessage := hb_ATokens( hb_CStr( xMessage ) )
   ENDCASE

   DO CASE
   CASE ValType( xOptions ) == "U"
      aOptions := { "OK" }
   CASE ValType( xOptions ) == "C"
      aOptions := hb_ATokens( xOptions, ";" )
   CASE ValType( xOptions ) == "A"
      aOptions := xOptions
   ENDCASE

   DO CASE
   CASE ValType( nColorNorm ) == "U"
      nColorNorm := 0x4f
      nColorHigh := 0x1f
   CASE ValType( nColorNorm ) == "N"
      nColorNorm := hb_bitAnd( nColorNorm, 0xff )
      nColorHigh := hb_bitAnd( hb_bitOr( hb_bitShift( nColorNorm, - 4 ), hb_bitShift( nColorNorm, 4 ) ), 0x77 )
   ENDCASE

   nLenOptions := Len( aOptions )
   FOR i := 1 TO nLenOptions
      nWidth += Len( aOptions[ i ] ) + 2
      nLenght += Len( aOptions[ i ] ) + 2
   NEXT

   /* in the loop I go through nWidth, I choose what is bigger */
   nLenMessage := Len( aMessage )
   FOR i := 1 TO nLenMessage
      nWidth := Max( nWidth, Len( aMessage[ i ] ) )
   NEXT

   DO WHILE .T.

      DispBegin()

      /* save the second setting! */
      IF nMaxRow != MaxRow( .T. ) .OR. nMaxCol != iif( nArg == NIL, MaxCol( .T. ), iif( nArg == 0x0, Int( MaxCol( .T. ) / 2 ), MaxCol( .T. ) + Int( MaxCol( .T. ) / 2 ) ) )

         WSelect( 1 )

         nMaxRow := MaxRow( .T. )
         /* the last parameter sets the dialog box: NIL middle, 0x0 to the left and 0x1 to the right */
         nMaxCol := iif( nArg == NIL, MaxCol( .T. ), iif( nArg == 0x0, Int( MaxCol( .T. ) / 2 ), MaxCol( .T. ) + Int( MaxCol( .T. ) / 2 ) ) )

         nTop    := Int( nMaxRow / 3 ) - 3
         nLeft   := Int( ( nMaxCol - nWidth ) / 2 ) - 2
         nBottom := nTop + 4 + nLenMessage
         nRight  := Int( ( nMaxCol + nWidth ) / 2 ) - 1 + 2

         //WClose( 1 )
         WSetShadow( 0x8 )
         WOpen( nTop, nLeft, nBottom, nRight, .T. )

         hb_DispBox( 0, 0, nMaxRow, nMaxCol, hb_UTF8ToStrBox( " █       " ), nColorNorm )
         hb_DispOutAt( 0, 0, Center( cTitle ), hb_bitShift( nColorNorm, 4 ) )

         FOR nPos := 1 TO Len( aMessage )
            hb_DispOutAt( 1 + nPos, 0, Center( aMessage[ nPos ] ), nColorNorm )
         NEXT

      ENDIF

      /* saves the coordinates of the aOptions buttons */
      aPos := {}
      nRow := nPos + 2
      nCol := Int( ( MaxCol() + 1 - nLenght - nLenOptions + 1 ) / 2 )

      FOR i := 1 TO nLenOptions
         AAdd( aPos, nCol )
         hb_DispOutAt( nRow, nCol, " " + aOptions[ i ] + " ", iif( i == nChoice, nColorHigh, nColorNorm ) )
         nCol += Len( aOptions[ i ] ) + 3
      NEXT

      DispEnd()

      nKey := Inkey( 0 )
      nKeyStd := hb_keyStd( nKey )

      DO CASE
      CASE nKeyStd == K_ESC
         nChoice := 0
         EXIT

      CASE nKeyStd == K_ENTER .OR. nKeyStd == K_SPACE
         EXIT

      CASE nKeyStd == K_MOUSEMOVE

         FOR i := 1 TO nLenOptions
            IF MRow() == nPos + 2 .AND. MCol() >= aPos[ i ] .AND. MCol() <= aPos[ i ] + Len( aOptions[ i ] ) + 1
               nChoice := i
            ENDIF
         NEXT

      CASE nKeyStd == K_LBUTTONDOWN

         nMCol := MCol()
         nMRow := MRow()

         IF MRow() == 0 .AND. MCol() >= 0 .AND. MCol() <= MaxCol()

            DO WHILE MLeftDown()
               WMove( WRow() + MRow() - nMRow, WCol() + MCol() - nMCol )
            ENDDO

         ENDIF

         FOR i := 1 TO nLenOptions
            IF MRow() == nPos + 2 .AND. MCol() >= aPos[ i ] .AND. MCol() <= aPos[ i ] + Len( aOptions[ i ] ) + 1
               nChoice := i
               EXIT
            ENDIF
         NEXT

         IF nChoice == i
            EXIT
         ENDIF

      CASE ( nKeyStd == K_LEFT .OR. nKeyStd == K_SH_TAB ) .AND. nLenOptions > 1

         nChoice--
         IF nChoice == 0
            nChoice := nLenOptions
         ENDIF

      CASE ( nKeyStd == K_RIGHT .OR. nKeyStd == K_TAB ) .AND. nLenOptions > 1

         nChoice++
         IF nChoice > nLenOptions
            nChoice := 1
         ENDIF

      CASE nKeyStd == K_CTRL_UP
         WMove( WRow() - 1, WCol() )

      CASE nKeyStd == K_CTRL_DOWN
         WMove( WRow() + 1, WCol() )

      CASE nKeyStd == K_CTRL_LEFT
         WMove( WRow(), WCol() - 1 )

      CASE nKeyStd == K_CTRL_RIGHT
         WMove( WRow(), WCol() + 1 )

      CASE nKeyStd == HB_K_RESIZE

         //WClose( 1 )

         AutoSize()

         PanelDisplay( aPanels[ idPanelLeft ] )
         PanelDisplay( aPanels[ idPanelRight ] )
         ComdLineDisplay( )

         BottomBar()

      ENDCASE

   ENDDO

   WClose( )
   SetCursor( nOldCursor )
   // SetPos( nRowPos, nColPos )

   RETURN iif( nKey == 0, 0, nChoice )

STATIC FUNCTION HC_MenuF2()

   LOCAL cFile := "hc.menu"
   LOCAL cCopyExample := ""
   LOCAL i, aLine, aMenu := {}, cLine
   LOCAL nMaxRow := 0, nMaxCol := 0
   LOCAL nRow := 1
   LOCAL nKey, nKeyStd
   LOCAL nColLength := 0
   LOCAL nTop, nLeft, nBottom, nRight
   LOCAL nChoice := 1
   LOCAL nRowPos := 1, nColPos := 2
   LOCAL nMRow, nMCol
/*   LOCAL nWinMnu */

   IF ! hb_vfExists( StartUpPath() + "hc.menu" )

      /* Example of menu structure, saves string to disk file, editing from file hc.menu */
      cCopyExample += "F1:Compilation of my project in Harbour" + hb_eol()
      cCopyExample += Space( 8 ) + "hbmk2 hc.prg" + hb_eol()
      cCopyExample += "F2:Checking version of GCC compiler" + hb_eol()
      cCopyExample += Space( 8 ) + "gcc --version" + hb_eol()
      cCopyExample += "F3:What is my OS version" + hb_eol()
      cCopyExample += Space( 8 ) + "uname -a"

      /* If no path is specified, hb_MemoWrit () saves cCopyExample in the current directory */
      hb_MemoWrit( StartUpPath() + "hc.menu", cCopyExample )

   ENDIF

   aLine := hb_ATokens( hb_MemoRead( cFile ), .T. )

   FOR i := 1 TO Len( aLine )
      IF SubStr( aLine[ i ], 1, 1 ) == "F"
         AAdd( aMenu, aLine[ i ] )
      ENDIF
   NEXT

   AScan( aMenu, {| str | nColLength := Max( nColLength, Len( str ) ) } )

   DO WHILE .T.

      DispBegin()

      IF nMaxRow != MaxRow( .T. ) .OR. nMaxCol != MaxCol( .T. )

         WSelect( 0 )

         nMaxRow := MaxRow( .T. )
         nMaxCol := MaxCol( .T. )

         nTop    := Int( nMaxRow / 3 ) - 3
         nLeft   := Int( ( nMaxCol - nColLength ) / 2 ) - 2
         nBottom := nTop + 2 + Len( aMenu )
         nRight  := Int( ( nMaxCol + nColLength ) / 2 ) - 1 + 2

         WSetShadow( 0x8 )
         WOpen( nTop, nLeft, nBottom, nRight, .T. )

         hb_DispBox( 0, 0, nMaxRow, nMaxCol, hb_UTF8ToStrBox( " █       " ), 0x9f )
         hb_DispOutAt( 0, 0, Center( /*hb_UserName() +*/ "Main menu" ), 0xf0 )

         hb_DispBox( 1, 0, MaxRow(), MaxCol(), HB_B_SINGLE_UNI, 0x9f )

      ENDIF

      DispEnd()

      FOR EACH cLine IN aMenu

         hb_DispOutAt( nRowPos + cLine:__enumIndex(), nColPos, ;
            PadR( cLine, nColLength ), ;
            iif( cLine:__enumIndex() == nRow, 0xf, 0x9f ) )

         iif( cLine:__enumIndex() == nRow, nChoice := cLine:__enumIndex(), NIL )

      NEXT

      DispEnd()

      SetCursor( SC_NONE )
      nKey := Inkey( 0 )
      nKeyStd := hb_keyStd( nKey )

      DO CASE
      CASE nKeyStd == K_ESC
         nChoice := 0
         EXIT

      CASE nKeyStd == K_ENTER .OR. nKeyStd == K_SPACE
         EXIT

      CASE nKeyStd == K_MOUSEMOVE

         IF nRowPos < MRow() .AND. nColPos < MCol() .AND. nRowPos + Len( aMenu ) >= MRow() .AND. nColPos + nColLength > MCol()
            nRow := MRow() - nRowPos
         ENDIF

      CASE nKeyStd == K_LBUTTONDOWN

         nMCol := MCol()
         nMRow := MRow()

         IF MRow() == 0 .AND. MCol() >= 0 .AND. MCol() <= MaxCol()

            DO WHILE MLeftDown()
               WMove( WRow() + MRow() - nMRow, WCol() + MCol() - nMCol )
            ENDDO

         ENDIF

         IF nRowPos < MRow() .AND. nColPos < MCol() .AND. nRowPos + Len( aMenu ) >= MRow() .AND. nColPos + nColLength > MCol()
            RETURN nChoice
         ENDIF

      CASE nKeyStd == K_MWFORWARD

         IF nRowPos < MRow() .AND. nColPos < MCol() .AND. nRowPos + Len( aMenu ) >= MRow() .AND. nColPos + nColLength > MCol()
            IF nRow > 1
               nRow--
            ELSE
               nRow := Len( aMenu )
            ENDIF
         ENDIF

      CASE nKeyStd == K_MWBACKWARD

         IF nRowPos < MRow() .AND. nColPos < MCol() .AND. nRowPos + Len( aMenu ) >= MRow() .AND. nColPos + nColLength > MCol()
            IF nRow < Len( aMenu )
               nRow++
            ELSE
               nRow := 1
            ENDIF
         ENDIF

      CASE nKeyStd == K_UP

         IF nRow > 1
            nRow--
         ELSE
            nRow := Len( aMenu )
         ENDIF

      CASE nKeyStd == K_DOWN

         IF nRow < Len( aMenu )
            nRow++
         ELSE
            nRow := 1
         ENDIF

      CASE nKeyStd == K_CTRL_UP
         WMove( WRow() - 1, WCol() )

      CASE nKeyStd == K_CTRL_DOWN
         WMove( WRow() + 1, WCol() )

      CASE nKeyStd == K_CTRL_LEFT
         WMove( WRow(), WCol() - 1 )

      CASE nKeyStd == K_CTRL_RIGHT
         WMove( WRow(), WCol() + 1 )

      CASE nKeyStd == HB_K_RESIZE

         WClose( )

         AutoSize()

         PanelDisplay( aPanels[ idPanelLeft ] )
         PanelDisplay( aPanels[ idPanelRight ] )
         ComdLineDisplay( )

         BottomBar()

      ENDCASE

   ENDDO

   WClose( )

   RETURN iif( nKey == 0, 0, nChoice )

STATIC PROCEDURE HCEdit( cFileName, lArg )

   LOCAL cString
   LOCAL aString
   LOCAL lContinue := .T.
   LOCAL nMaxRow := 0, nMaxCol := 0
   LOCAL nRow := 1, nCol := 0, nRowNo := 0, nColNo := 0
   LOCAL cStringEditingRow
   LOCAL cSubString
   LOCAL lToggleInsert := .F.
   LOCAL nKey, nKeyStd
/*   LOCAL nOldRow, nOldCol */
/*   LOCAL cScreen */
   LOCAL tsDateTime
/*   LOCAL nEdtHndl */

/*   nOldRow := Row()
   nOldCol := Col()
*/
   WOpen( 0, 0, aConfig[ _nMaxRow ], aConfig[ _nMaxCol ] )

   IF HB_ISSTRING( cFileName ) // ?

      cString := hb_MemoRead( cFileName )

      aString := hb_ATokens( cString, .T. )

      DO WHILE lContinue

         IF nMaxRow != MaxRow() .OR. nMaxCol != MaxCol()
            nMaxRow := MaxRow()
            nMaxCol := MaxCol()

            IF nRow > nMaxRow - 1
               nRow := nMaxRow - 1
            ENDIF

            HCEditDisplay( aString, nRow, nCol, nRowNo )

         ENDIF

         DispBegin()
         hb_DispOutAt( 0, 0, ;
            PadR( cFileName + "  ", nMaxCol + 1 ), 0x30 )

         HCEditDisplay( aString, nRow, nCol, nRowNo )

         hb_vfTimeGet( cFileName, @tsDateTime )
         hb_DispOutAt( nMaxRow, 0, ;
            PadR( " Row(" + hb_ntos( nRow + nRowNo ) + ") Col(" + hb_ntos( nCol + 1 ) + ") Size(" + hb_ntos( hb_vfSize( cFileName ) ) + ") Date(" + hb_TToC( tsDateTime ) + ")", nMaxCol + 1 ), 0x30 )
         DispEnd()

         nKey := Inkey( 0 )
         nKeyStd := hb_keyStd( nKey )

         SWITCH nKeyStd

         CASE K_ESC
            lContinue := .F.
            EXIT

         CASE K_LBUTTONDOWN

            IF MRow() > 0 .AND. MCol() > 0 .AND. MRow() < Len( aString ) + 1 .AND. MCol() < nMaxCol
               nRow := MRow()
               nCol := Len( aString[ nRowNo + nRow ] )
            ENDIF

            EXIT

         CASE K_MWFORWARD

            IF nRowNo >= 1
               nRowNo--
            ENDIF

            EXIT

         CASE K_MWBACKWARD

            IF nRow + nRowNo < Len( aString )
               nRowNo++
            ENDIF

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

            EXIT

         CASE K_LEFT

            IF nCol > 0
               nCol--
            ELSE
               IF nColNo > 0
                  nColNo--
               ENDIF
            ENDIF

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

            EXIT

         CASE K_RIGHT

            IF nCol < Len( aString[ nRowNo + nRow ] )
               nCol++
            ENDIF

            EXIT

         CASE K_HOME

            nCol := 0

            EXIT

         CASE K_END

            nCol := Len( aString[ nRowNo + nRow ] )

            EXIT

         CASE K_PGUP

            IF nRow <= 1
               IF nRowNo - nMaxRow >= 0
                  nRowNo -= nMaxRow
               ENDIF
            ENDIF
            nRow := 1

            EXIT

         CASE K_PGDN

            IF nRow >= nMaxRow - 1
               IF nRowNo + nMaxRow  <= Len( aString )
                  nRowNo += nMaxRow
               ENDIF
            ENDIF
            nRow := Min( nMaxRow - 1, Len( aString ) - nRowNo )

            hb_Scroll( 1, 0, nMaxRow, nMaxCol )

            EXIT

         CASE K_CTRL_PGUP

            nRow := 0
            nRowNo := 0

            EXIT

         CASE K_CTRL_PGDN

            nRow := nMaxRow - 1
            nRowNo := Len( aString ) - nMaxRow + 1

            EXIT

         CASE K_ENTER

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
                     hb_AIns( aString, nRowNo + nRow + 1, cSubString, .T. )
                     nRow++
                     nCol := 0
                  ENDIF
               ENDIF

               SaveFile( aString, cFileName )

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

            ENDIF
            EXIT

         CASE K_TAB
            IF lArg
               cStringEditingRow := aString[ nRowNo + nRow ]

               aString[ nRowNo + nRow ] := Stuff( cStringEditingRow, nCol + 1, iif( lToggleInsert, 1, 0 ), "   " )
               nCol += 3

               SaveFile( aString, cFileName )

            ENDIF
            EXIT

         OTHERWISE

            IF lArg
               IF ( nKeyStd >= 32 .AND. nKeyStd <= 126 ) .OR. ( nKeyStd >= 160 .AND. nKeyStd <= 255 ) .OR. ! hb_keyChar( nKeyStd ) == ""

                  cStringEditingRow := aString[ nRowNo + nRow ]
                  aString[ nRowNo + nRow ] := Stuff( cStringEditingRow, nCol + 1, iif( lToggleInsert, 1, 0 ), hb_keyChar( nKeyStd ) )
                  nCol++

                  SaveFile( aString, cFileName )

               ENDIF
            ENDIF

         ENDSWITCH

      ENDDO

   ELSE
      HC_Alert( "Error reading:;" + cFileName )
      RETURN
   ENDIF

   WClose()

   RETURN

STATIC PROCEDURE HCEditDisplay( aString, nRow, nCol, nRowNo )

   LOCAL i
   LOCAL nMaxRow := MaxRow(), nMaxCol := MaxCol()
   LOCAL nLine

   hb_Scroll( 2, 0, nMaxRow - 2, nMaxCol )

   FOR i := 1 TO nMaxRow

      nLine := i + nRowNo

      IF nLine <= Len( aString )
         hb_DispOutAt( i, 0, ;
            PadR( aString[ nLine ], nMaxCol + 1 ), ;
            iif( i == nRow, 0x8f, 0x7 ) )
      ELSE
         hb_Scroll( i, 0, nMaxRow, nMaxCol + 1 )
         hb_DispOutAt( i, 1, ">> EOF <<", 0x01 )
         EXIT
      ENDIF

   NEXT

   SetPos( nRow, nCol )

   RETURN

STATIC PROCEDURE SaveFile( aString, cFileName )

   LOCAL cString := ""

   AEval( aString, {| e | cString += e + hb_eol() } )
   hb_MemoWrit( cFileName, cString )

   RETURN

STATIC FUNCTION FileError()
   RETURN { ;
      {   0, "The operation completed successfully." }, ;
      {   2, "The system cannot find the file specified." }, ;
      {   3, "The system cannot find the path specified." }, ;
      {   4, "The system cannot open the file." }, ;
      {   5, "Access is denied." }, ;
      {   6, "The handle is invalid." }, ;
      {   8, "Not enough storage is available to process this command." }, ;
      {  15, "The system cannot find the drive specified." }, ;
      {  16, "The directory cannot be removed." }, ;
      {  17, "The system cannot move the file to a different disk drive." }, ;
      {  18, "There are no more files." }, ;
      {  19, "Attempted to write to a write-protected disk." }, ;
      {  21, "The device is not ready." }, ;
      {  23, "Data error (cyclic redundancy check)." }, ;
      {  29, "The system cannot write to the specified device." }, ;
      {  30, "The system cannot read from the specified device." }, ;
      {  32, "The process cannot access the file because ; it is being used by another process." }, ;
      {  33, "The process cannot access the file because ; another process has locked a portion of the file." }, ;
      {  36, "Too many files opened for sharing." }, ;
      {  38, "Reached the end of the file." }, ;
      {  62, "Space to store the file waiting to be printed ; is not available on the server." }, ;
      {  63, "Your file waiting to be printed was deleted." }, ;
      {  80, "The file exists." }, ;
      {  82, "The directory or file cannot be created." }, ;
      { 110, "The system cannot open the device or file specified." }, ;
      { 111, "The file name is too long" }, ;
      { 113, "No more internal file identifiers available." }, ;
      { 114, "The target internal file identifier is incorrect." }, ;
      { 123, "The filename, directory name, ; or volume label syntax is incorrect." }, ;
      { 130, "Attempt to use a file handle to an open disk ; partition for an operation other than raw disk I/O." }, ;
      { 131, "An attempt was made to move the file pointer ; before the beginning of the file." }, ;
      { 132, "The file pointer cannot be set on the specified ; device or file." }, ;
      { 138, "The system tried to join a drive ; to a directory on a joined drive." }, ;
      { 139, "The system tried to substitute a drive ; to a directory on a substituted drive." }, ;
      { 140, "The system tried to join a drive ; to a directory on a substituted drive." }, ;
      { 141, "The system tried to SUBST a drive ; to a directory on a joined drive." }, ;
      { 143, "The system cannot join or substitute a drive ; to or for a directory on the same drive." }, ;
      { 144, "The directory is not a subdirectory of the root directory." }, ;
      { 145, "The directory is not empty." }, ;
      { 150, "System trace information was not specified ; in your CONFIG.SYS file, or tracing is disallowed." }, ;
      { 154, "The volume label you entered exceeds ; the label character limit of the target file system." }, ;
      { 167, "Unable to lock a region of a file." }, ;
      { 174, "The file system does not support ; atomic changes to the lock type." }, ;
      }

// ====================================
FUNCTION Q( xPar )
   RETURN Alert( hb_ValToExp( xPar ) )
// ====================================

FUNCTION StartUpPath()

   LOCAL cDir := ""
   
   hb_FNameSplit( hb_argv( 0 ), @cDir )

   RETURN cDir
