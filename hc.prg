/* Copyright 2017-2018 Rafał Jopek ( rafaljopek at hotmail com ) */

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

#define _HC_nTop          1
#define _HC_nLeft         2
#define _HC_nBottom       3
#define _HC_nRight        4
#define _HC_cDir          5
#define _HC_aArray        6
#define _HC_nRowBar       7
#define _HC_nRowNo        8
#define _HC_cComdLine     9
#define _HC_nComdCol     10
#define _HC_nComdColNo   11

#define _HC_nElements    11

/* FError() */
#define MEANING           2

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

   aInit[ _HC_nTop       ] := 0
   aInit[ _HC_nLeft      ] := 0
   aInit[ _HC_nBottom    ] := 0
   aInit[ _HC_nRight     ] := 0
   aInit[ _HC_cDir       ] := ""
   aInit[ _HC_aArray     ] := {}
   aInit[ _HC_nRowBar    ] := 1
   aInit[ _HC_nRowNo     ] := 0
   aInit[ _HC_cComdLine  ] := ""
   aInit[ _HC_nComdCol   ] := 0
   aInit[ _HC_nComdColNo ] := 0

   RETURN aInit

STATIC PROCEDURE PanelFetchList( aPanel, cDir )

   aPanel[ _HC_cDir ] := hb_defaultValue( cDir, hb_cwd() )
   aPanel[ _HC_aArray ] := hb_vfDirectory( aPanel[ _HC_cDir ], "HSD" )

   hb_ADel( aPanel[ _HC_aArray ], AScan( aPanel[ _HC_aArray ], {| x | x[ F_NAME ] == "." } ), .T. )
   ASort( aPanel[ _HC_aArray ],,, {| x, y | DIR_PREFIX( x ) + OSUPPER( x[ F_NAME ] ) < DIR_PREFIX( y ) + OSUPPER( y[ F_NAME ] ) } )

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
   LOCAL aTarget := {}, aItem, aDirScan
   LOCAL nLengthName := 0
   LOCAL nHandle
   LOCAL nErrorCode
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

      ComdLineDisplay( aPanelSelect )
      PanelDisplay( aPanelSelect )

      nKey := Inkey( 0 )
      nKeyStd := hb_keyStd( nKey )

      SWITCH nKeyStd

      CASE K_ESC
         lContinue := .F.
         EXIT

      CASE K_ENTER

         nPos := aPanelSelect[ _HC_nRowBar ] + aPanelSelect[ _HC_nRowNo ]
         IF Empty( aPanelSelect[ _HC_cComdLine ] )
            IF At( "D", aPanelSelect[ _HC_aArray ][ nPos ][ F_ATTR ] ) == 0
               hb_run( aPanelSelect[ _HC_aArray ][ nPos ][ F_NAME ] )
            ELSE
               ChangeDir( aPanelSelect )
            ENDIF
         ELSE

            /* aPanelSelect[ _HC_cComdLine ] := aPanelSelect[ _HC_cComdLine ] + " " + aPanelSelect[ _HC_cDir ] */

            hb_run( aPanelSelect[ _HC_cComdLine ] )
            aPanelSelect[ _HC_cComdLine ] := ""
            nMaxRow := 0
            aPanelSelect[ _HC_nComdCol ] := 0

            PanelRefresh( aPanelSelect )
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

         EXIT

      CASE K_DOWN

         IF aPanelSelect[ _HC_nRowBar ] < aPanelSelect[ _HC_nBottom ] - 1 .AND. aPanelSelect[ _HC_nRowBar ] <= Len( aPanelSelect[ _HC_aArray ] ) - 1
            ++aPanelSelect[ _HC_nRowBar ]
         ELSE
            IF aPanelSelect[ _HC_nRowNo ] + aPanelSelect[ _HC_nRowBar ] <= Len( aPanelSelect[ _HC_aArray ] ) - 1
               ++aPanelSelect[ _HC_nRowNo ]
            ENDIF
         ENDIF

         EXIT

      CASE K_LEFT

         IF aPanelSelect[ _HC_nComdCol ] > 0
            aPanelSelect[ _HC_nComdCol ]--
         ELSE
            IF aPanelSelect[ _HC_nComdColNo ] >= 1
               aPanelSelect[ _HC_nComdColNo ]--
            ENDIF
         ENDIF

         EXIT

      CASE K_RIGHT

         IF aPanelSelect[ _HC_nComdCol ] < nMaxCol .AND. aPanelSelect[ _HC_nComdCol ] < Len( aPanelSelect[ _HC_cComdLine ] )
            aPanelSelect[ _HC_nComdCol ]++
         ELSE
            IF aPanelSelect[ _HC_nComdColNo ] + aPanelSelect[ _HC_nComdCol ] < Len( aPanelSelect[ _HC_cComdLine ] )
               aPanelSelect[ _HC_nComdColNo ]++
            ENDIF
         ENDIF

         EXIT

      CASE K_HOME

         aPanelSelect[ _HC_nComdCol ] := 0

         EXIT

      CASE K_END

         aPanelSelect[ _HC_nComdCol ] := Len( aPanelSelect[ _HC_cComdLine ] )

         EXIT

      CASE K_PGUP

         IF aPanelSelect[ _HC_nRowBar ] <= 1
            IF aPanelSelect[ _HC_nRowNo ] - nMaxRow >= 0
               aPanelSelect[ _HC_nRowNo ] -= nMaxRow
            ENDIF
         ENDIF

         aPanelSelect[ _HC_nRowBar ] := 1
         EXIT

      CASE K_PGDN

         IF aPanelSelect[ _HC_nRowBar ] >= nMaxRow - 3
            IF aPanelSelect[ _HC_nRowNo ] + nMaxRow  <= Len( aPanelSelect[ _HC_aArray ] )
               aPanelSelect[ _HC_nRowNo ] += nMaxRow
            ENDIF
         ENDIF

         aPanelSelect[ _HC_nRowBar ] := Min( nMaxRow - 3, Len( aPanelSelect[ _HC_aArray ] ) - aPanelSelect[ _HC_nRowNo ] )
         EXIT

      CASE K_DEL

         IF aPanelSelect[ _HC_nComdCol ] >= 0
            aPanelSelect[ _HC_cComdLine ] := Stuff( aPanelSelect[ _HC_cComdLine ], aPanelSelect[ _HC_nComdCol ] + 1, 1, "" )
         ENDIF

         EXIT

      CASE K_BS

         IF aPanelSelect[ _HC_nComdCol ] > 0
            aPanelSelect[ _HC_cComdLine ] := Stuff( aPanelSelect[ _HC_cComdLine ], aPanelSelect[ _HC_nComdCol ], 1, "" )
            aPanelSelect[ _HC_nComdCol ]--
         ENDIF

         EXIT

      CASE K_F3

         nPos := aPanelSelect[ _HC_nRowBar ] + aPanelSelect[ _HC_nRowNo ]
         IF At( "D", aPanelSelect[ _HC_aArray ][ nPos ][ F_ATTR ] ) == 0

            HCEdit( aPanelSelect[ _HC_cDir ] + aPanelSelect[ _HC_aArray ][ nPos ][ F_NAME ], .F. )

         ELSE
            aDirScan := hb_DirScan( aPanelSelect[ _HC_aArray ][ nPos ][ F_NAME ], hb_osFileMask() )
            AScan( aDirScan, {| x | nLengthName := Max( nLengthName, Len( x[ 1 ] ) ) } )

            FOR EACH aItem IN aDirScan
               AAdd( aTarget, ;
                  PadR( aItem[ F_NAME ], nLengthName ) + " " + ;
                  Transform( hb_ntos( aItem[ F_SIZE ] ), "9 999 999 999" ) + " " + ;
                  hb_TToC( aItem[ F_DATE ] ) + " " + ;
                  aItem[ F_ATTR ] )
            NEXT

            SaveFile( aTarget, "DirScan.txt" ) // ??

            HCEdit( "DirScan.txt", .F. )
         ENDIF

         EXIT

      CASE K_F4

         nPos := aPanelSelect[ _HC_nRowBar ] + aPanelSelect[ _HC_nRowNo ]
         IF At( "D", aPanelSelect[ _HC_aArray ][ nPos ][ F_ATTR ] ) == 0
            HCEdit( aPanelSelect[ _HC_cDir ] + aPanelSelect[ _HC_aArray ][ nPos ][ F_NAME ], .T. )
         ELSE
            HC_Alert( "No file selected!",, 0x70 )
         ENDIF

         EXIT

      CASE K_F5

         nPos := aPanelSelect[ _HC_nRowBar ] + aPanelSelect[ _HC_nRowNo ]
         IF aPanelSelect[ _HC_aArray ][ nPos ][ F_NAME ] == ".."
            HC_Alert( "The item to be copy has not been selected.",, 0x70 )
         ELSE

            IF aPanelSelect == aPanelLeft

               IF At( "D", aPanelSelect[ _HC_aArray ][ nPos ][ F_ATTR ] ) == 0
                  IF HB_ISSTRING( MsgBox( "Copy file " + '"' + aPanelLeft[ _HC_aArray ][ nPos ][ F_NAME ] + '"' + " to", ;
                        aPanelRight[ _HC_cDir ], { "Yes", "No!" } ) )

                     IF hb_vfCopyFile( aPanelLeft[ _HC_cDir ] + aPanelLeft[ _HC_aArray ][ nPos ][ F_NAME ], ;
                           aPanelRight[ _HC_cDir ] + aPanelLeft[ _HC_aArray ][ nPos ][ F_NAME ] ) == 0

                        PanelRefresh( aPanelRight )

                     ELSE
                        IF ( nErrorCode := AScan( aFError, {| x | x[ 1 ] == FError() } ) ) > 0
                           HC_Alert( "Cannot make file, error: ; ;" + aFError[ nErrorCode ][ MEANING ] )
                        ELSE
                           HC_Alert( "Cannot make file, error: ; ;" + hb_ntos( FError() ) )
                        ENDIF
                     ENDIF

                  ENDIF
               ELSE
                  IF HB_ISSTRING( MsgBox( "Copy directory " + '"' + aPanelLeft[ _HC_aArray ][ nPos ][ F_NAME ] + '"' + " to", ;
                        aPanelRight[ _HC_cDir ], { "Yes", "No!" } ) )

                     IF CopyDirectory( aPanelLeft[ _HC_cDir ] + aPanelLeft[ _HC_aArray ][ nPos ][ F_NAME ], ;
                           aPanelRight[ _HC_cDir ] ) == 0

                        PanelRefresh( aPanelRight )

                     ELSE
                        IF ( nErrorCode := AScan( aFError, {| x | x[ 1 ] == FError() } ) ) > 0
                           HC_Alert( "Cannot make file, error: ; ;" + aFError[ nErrorCode ][ MEANING ] )
                        ELSE
                           HC_Alert( "Cannot make file, error: ; ;" + hb_ntos( FError() ) )
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
            ELSE
               IF At( "D", aPanelSelect[ _HC_aArray ][ nPos ][ F_ATTR ] ) == 0
                  IF HB_ISSTRING( MsgBox( "Copy file " + '"' + aPanelRight[ _HC_aArray ][ nPos ][ F_NAME ] + '"' + " to", ;
                        aPanelLeft[ _HC_cDir ], { "Yes", "No!" } ) )

                     IF hb_vfCopyFile( aPanelRight[ _HC_cDir ] + aPanelRight[ _HC_aArray ][ nPos ][ F_NAME ], ;
                           aPanelLeft[ _HC_cDir ] + aPanelRight[ _HC_aArray ][ nPos ][ F_NAME ] ) == 0

                        PanelRefresh( aPanelLeft )

                     ELSE
                        IF ( nErrorCode := AScan( aFError, {| x | x[ 1 ] == FError() } ) ) > 0
                           HC_Alert( "Cannot make file, error: ; ;" + aFError[ nErrorCode ][ MEANING ] )
                        ELSE
                           HC_Alert( "Cannot make file, error: ; ;" + hb_ntos( FError() ) )
                        ENDIF
                     ENDIF
                  ENDIF
               ELSE
                  IF HB_ISSTRING( MsgBox( "Copy directory " + '"' + aPanelRight[ _HC_aArray ][ nPos ][ F_NAME ] + '"' + " to", ;
                        aPanelLeft[ _HC_cDir ], { "Yes", "No!" } ) )

                     IF CopyDirectory( aPanelRight[ _HC_cDir ] + aPanelRight[ _HC_aArray ][ nPos ][ F_NAME ], ;
                           aPanelLeft[ _HC_cDir ] ) == 0

                        PanelRefresh( aPanelLeft )

                     ELSE
                        IF ( nErrorCode := AScan( aFError, {| x | x[ 1 ] == FError() } ) ) > 0
                           HC_Alert( "Cannot make file, error: ; ;" + aFError[ nErrorCode ][ MEANING ] )
                        ELSE
                           HC_Alert( "Cannot make file, error: ; ;" + hb_ntos( FError() ) )
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDIF

         EXIT

      CASE K_F6

         nPos := aPanelSelect[ _HC_nRowBar ] + aPanelSelect[ _HC_nRowNo ]
         IF aPanelSelect[ _HC_aArray ][ nPos ][ F_NAME ] == ".."
            HC_Alert( "The item to be rename or move has not been selected.",, 0x70 )
         ELSE

            IF At( "D", aPanelSelect[ _HC_aArray ][ nPos ][ F_ATTR ] ) == 0
               IF HB_ISSTRING( MsgBox( "Rename or move file " + '"' + aPanelSelect[ _HC_aArray ][ nPos ][ F_NAME ] + '"' + " to", ;
                     aPanelSelect[ _HC_cDir ] + aPanelSelect[ _HC_aArray ][ nPos ][ F_NAME ], { "Yes", "No!" } ) )

               ENDIF
            ELSE
               IF HB_ISSTRING( MsgBox( "Rename or move directory  " + '"' + aPanelSelect[ _HC_aArray ][ nPos ][ F_NAME ] + '"' + " to", ;
                     aPanelSelect[ _HC_cDir ] + aPanelSelect[ _HC_aArray ][ nPos ][ F_NAME ], { "Yes", "No!" } ) )

               ENDIF
            ENDIF

         ENDIF

         EXIT

      CASE K_F7

         IF HB_ISSTRING( cNewDir := MsgBox( "Create the directory.", NIL, { "Yes", "No!" } ) )
            IF hb_vfDirMake( aPanelSelect[ _HC_cDir ] + cNewDir ) == 0

               PanelRefresh( aPanelSelect )

            ELSE
               IF ( nErrorCode := AScan( aFError, {| x | x[ 1 ] == FError() } ) ) > 0
                  HC_Alert( "Cannot make file, error: ; ;" + aFError[ nErrorCode ][ MEANING ] )
               ELSE
                  HC_Alert( "Cannot make file, error: ; ;" + hb_ntos( FError() ) )
               ENDIF
            ENDIF
         ENDIF

         EXIT

      CASE K_F8

         nPos := aPanelSelect[ _HC_nRowBar ] + aPanelSelect[ _HC_nRowNo ]

         IF aPanelSelect[ _HC_aArray ][ nPos ][ F_NAME ] == ".."
            HC_Alert( "The item to be deleted has not been selected.",, 0x70 )
         ELSE
            nPos := aPanelSelect[ _HC_nRowBar ] + aPanelSelect[ _HC_nRowNo ]
            IF At( "D", aPanelSelect[ _HC_aArray ][ nPos ][ F_ATTR ] ) == 0
               IF HC_Alert( "Do you really want to delete the selected file ;" + '"' + aPanelSelect[ _HC_aArray ][ nPos ][ F_NAME ] + '"', { "Yes", "No!" }, 0x70 ) == 1
                  IF hb_vfErase( aPanelSelect[ _HC_cDir ] + aPanelSelect[ _HC_aArray ][ nPos ][ F_NAME ] ) == 0

                     PanelRefresh( aPanelSelect )

                  ELSE
                     IF ( nErrorCode := AScan( aFError, {| x | x[ 1 ] == FError() } ) ) > 0
                        HC_Alert( "Cannot make file, error: ; ;" + aFError[ nErrorCode ][ MEANING ] )
                     ELSE
                        HC_Alert( "Cannot make file, error: ; ;" + hb_ntos( FError() ) )
                     ENDIF
                  ENDIF
               ENDIF
            ELSE
               IF HC_Alert( "Do you really want to delete the selected directory ;" + '"' + aPanelSelect[ _HC_aArray ][ nPos ][ F_NAME ] + '"', { "Yes", "No!" }, 0x70 ) == 1
                  IF hb_vfDirRemove( aPanelSelect[ _HC_cDir ] + aPanelSelect[ _HC_aArray ][ nPos ][ F_NAME ] ) == 0

                     PanelRefresh( aPanelSelect )

                  ELSE
                     IF HC_Alert( "The following subdirectory is not empty. ;" + ;
                           '"' + aPanelSelect[ _HC_aArray ][ nPos ][ F_NAME ] + '"' + ";" + ;
                           "Do you still wish to delete it?", { "Delete", "No!" } ) == 1

                        IF hb_DirRemoveAll( aPanelSelect[ _HC_cDir ] + aPanelSelect[ _HC_aArray ][ nPos ][ F_NAME ] ) == .T.

                           PanelRefresh( aPanelSelect )

                        ELSE
                           IF ( nErrorCode := AScan( aFError, {| x | x[ 1 ] == FError() } ) ) > 0
                              HC_Alert( "Cannot make file, error: ; ;" + aFError[ nErrorCode ][ MEANING ] )
                           ELSE
                              HC_Alert( "Cannot make file, error: ; ;" + hb_ntos( FError() ) )
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDIF

         EXIT

      CASE K_SH_F4

         IF HB_ISSTRING( cFileName := MsgBox( "Create file.", NIL, { "Yes", "No!" } ) )
            IF ( nHandle := hb_vfOpen( aPanelSelect[ _HC_cDir ] + cFileName, FO_CREAT + FO_TRUNC + FO_WRITE ) ) != NIL

               IF ! hb_vfClose( nHandle )
                  IF ( nErrorCode := AScan( aFError, {| x | x[ 1 ] == FError() } ) ) > 0
                     HC_Alert( "Cannot make file, error: ; ;" + aFError[ nErrorCode ][ MEANING ] )
                  ELSE
                     HC_Alert( "Cannot make file, error: ; ;" + hb_ntos( FError() ) )
                  ENDIF
               ENDIF

               PanelRefresh( aPanelSelect )

            ELSE

               IF ( nErrorCode := AScan( aFError, {| x | x[ 1 ] == FError() } ) ) > 0
                  HC_Alert( "Cannot make file, error: ; ;" + aFError[ nErrorCode ][ MEANING ] )
               ELSE
                  HC_Alert( "Cannot make file, error: ; ;" + hb_ntos( FError() ) )
               ENDIF

            ENDIF
         ENDIF

         EXIT

      OTHERWISE

         IF ( nKeyStd >= 32 .AND. nKeyStd <= 126 ) .OR. ( nKeyStd >= 160 .AND. nKeyStd <= 255 ) .OR. ! hb_keyChar( nKeyStd ) == ""

            aPanelSelect[ _HC_cComdLine ] := Stuff( aPanelSelect[ _HC_cComdLine ], aPanelSelect[ _HC_nComdCol ] + aPanelSelect[ _HC_nComdColNo ] + 1, 0, hb_keyChar( nKeyStd ) )
            IF aPanelSelect[ _HC_nComdCol ] < nMaxCol
               aPanelSelect[ _HC_nComdCol ]++
            ELSE
               aPanelSelect[ _HC_nComdColNo ]++
            ENDIF

         ENDIF

      ENDSWITCH

   ENDDO

   RETURN

STATIC FUNCTION CopyDirectory( cSourceFile, cTargetFile )

   LOCAL aCatalog
   LOCAL nRows
   LOCAL i
   LOCAL cSubCat

   cSubCat := hb_FNameNameExt( cSourceFile )
   IF hb_DirCreate( cTargetFile + cSubCat ) != 0
      // komunikat błędu
      RETURN - 1
   ENDIF

   aCatalog := hb_vfDirectory( cSourceFile + hb_ps(), "HSD" )
   nRows    := Len( aCatalog )

   FOR i := 1 TO nRows
      IF aCatalog[ i ][ F_NAME ] == "." .OR. aCatalog[ i ][ F_NAME ] == ".."

      ELSEIF aCatalog[ i ][ F_ATTR ] == "D"
         IF CopyDirectory( cSourceFile + hb_ps() + aCatalog[ i ][ F_NAME ], cTargetFile + cSubCat + hb_ps() ) == - 1
            // komunikat błędu
            RETURN - 1
         ENDIF
      ELSE
         IF hb_vfCopyFile( cSourceFile + hb_ps() + aCatalog[ i ][ F_NAME ], cTargetFile + cSubCat + hb_ps() + aCatalog[ i ][ F_NAME ] ) != 0
            // komunikat błędu
            RETURN - 1
         ENDIF
      ENDIF
   NEXT

   RETURN 0

STATIC PROCEDURE PanelDisplay( aPanel )

   LOCAL nRow, nPos := 1
   LOCAL nLengthName := 0, nLengthSize := 0

   AScan( aPanel[ _HC_aArray ], {| x | ;
      nLengthName := Max( nLengthName, Len( x[ 1 ] ) ), ;
      nLengthSize := Max( nLengthSize, Len( Str( x[ 2 ] ) ) ) } )

   DispBegin()
   IF aPanelSelect == aPanel
      hb_DispBox( aPanel[ _HC_nTop ], aPanel[ _HC_nLeft ], aPanel[ _HC_nBottom ], aPanel[ _HC_nRight ], HB_B_DOUBLE_UNI + " ", 0x1f )
   ELSE
      hb_DispBox( aPanel[ _HC_nTop ], aPanel[ _HC_nLeft ], aPanel[ _HC_nBottom ], aPanel[ _HC_nRight ], HB_B_SINGLE_UNI + " ", 0x1f )
   ENDIF

   nPos += aPanel[ _HC_nRowNo ]
   FOR nRow := aPanel[ _HC_nTop ] + 1 TO aPanel[ _HC_nBottom ] - 1

      IF nPos <= Len( aPanel[ _HC_aArray ] )
         hb_DispOutAt( nRow, aPanel[ _HC_nLeft ] + 1, ;
            PadR( Expression( ;
            nLengthName, nLengthSize, ;
            aPanel[ _HC_aArray ][ nPos ][ F_NAME ], ;
            aPanel[ _HC_aArray ][ nPos ][ F_SIZE ], ;
            aPanel[ _HC_aArray ][ nPos ][ F_DATE ], ;
            aPanel[ _HC_aArray ][ nPos ][ F_ATTR ] ), ;
            aPanel[ _HC_nRight ] - aPanel[ _HC_nLeft ] - 1 ), ;
            iif( aPanelSelect == aPanel .AND. nPos == aPanel[ _HC_nRowBar ] + aPanel[ _HC_nRowNo ], 0x30, ColoringSyntax( ;
            aPanel[ _HC_aArray ][ nPos ][ F_ATTR ] ) ) )
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

   hb_DispOutAt( nMaxRow - 1, 0, PadR( aPanel[ _HC_cDir ] + SubStr( aPanel[ _HC_cComdLine ], 1 + aPanel[ _HC_nComdColNo ], nMaxCol + aPanel[ _HC_nComdColNo ] ), nMaxCol ) )

   SetPos( nMaxRow - 1, aPanel[ _HC_nComdCol ] + Len( aPanel[ _HC_cDir ] ) )

   DispEnd()

   RETURN

STATIC FUNCTION Expression( nLengthName, nLengthSize, aExp1, aExp2, aExp3, aExp4 )

   LOCAL cStr1, cStr2, cStr3, cExp4

   iif( nLengthName == 2, nLengthName := 4, nLengthName ) // ??

   cStr1 := PadR( aExp1 + Space( nLengthName ), nLengthName ) + " "

   IF aExp1 == ".."
      cStr1 := PadR( "[" + AllTrim( cStr1 ) + "]" + Space( nLengthName ), nLengthName ) + " "
   ENDIF

   IF aExp4 == "D" .OR. aExp4 == "HD" .OR. aExp4 == "HSD" .OR. aExp4 == "HSDL" .OR. aExp4 == "RHSA" .OR. aExp4 == "RD" .OR. aExp4 == "AD" .OR. aExp4 == "RHD"
      cStr2 := PadL( "DIR", nLengthSize + 3 ) + " "
   ELSE
      cStr2 := PadL( Transform( aExp2, "9 999 999 999" ), nLengthSize + 3 ) + " "
   ENDIF

   cStr3 := hb_TToC( aExp3 ) + " "
   cExp4 := PadL( aExp4, 3 )

   RETURN cStr1 + cStr2 + cStr3 + cExp4

STATIC FUNCTION ColoringSyntax( aExp4 )

   LOCAL nColor

   IF aExp4 == "HD" .OR. aExp4 == "HSD" .OR. aExp4 == "HSDL" .OR. aExp4 == "RHSA" .OR. aExp4 == "RD"
      nColor := 0x13
   ELSE
      nColor := 0x1f
   ENDIF

   RETURN nColor

STATIC PROCEDURE PanelRefresh( aPanel )

   IF aPanelLeft[ _HC_cDir ] == aPanelRight[ _HC_cDir ]

      PanelFetchList( aPanelLeft, aPanelLeft[ _HC_cDir ] )
      PanelFetchList( aPanelRight, aPanelRight[ _HC_cDir ] )

      PanelDisplay( aPanelLeft )
      PanelDisplay( aPanelRight )

   ELSE

      PanelFetchList( aPanel, aPanel[ _HC_cDir ] )
      PanelDisplay( aPanel )

   ENDIF

   RETURN

STATIC PROCEDURE ChangeDir( aPanel )

   LOCAL nPos, cDir, cDir0
   LOCAL nPosLast

   nPos := aPanel[ _HC_nRowBar ] + aPanel[ _HC_nRowNo ]
   IF At( "D", aPanel[ _HC_aArray ][ nPos ][ F_ATTR ] ) == 0
      RETURN
   ENDIF
   IF aPanel[ _HC_aArray ][ nPos ][ F_NAME ] == ".."
      cDir := aPanel[ _HC_cDir ]
      cDir0 := SubStr( cDir, RAt( hb_ps(), Left( cDir, Len( cDir ) - 1 ) ) + 1 )
      cDir0 := SubStr( cDir0, 1, Len( cDir0 ) - 1 )
      cDir  := Left( cDir, RAt( hb_ps(), Left( cDir, Len( cDir ) - 1 ) ) )
      PanelFetchlist( aPanel, cDir )
      nPosLast := Max( AScan( aPanel[ _HC_aArray ], {| x | x[ F_NAME ] = cDir0 } ), 1 )

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
      cDir := aPanel[ _HC_cDir ] + aPanel[ _HC_aArray ][ nPos ][ F_NAME ] + hb_ps()
      aPanel[ _HC_nRowBar ] := 1
      aPanel[ _HC_nRowNo  ] := 0
      PanelFetchlist( aPanel, cDir )
   ENDIF

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

   /* aMessage obecnie nie jest tablicą */
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

         hb_DispBox( nMaxRow - 2, nMaxCol - 36, nMaxRow + 3, nMaxCol + 36, HB_B_SINGLE_UNI + " ", 0x70 )
         hb_Shadow( nMaxRow - 2, nMaxCol - 36, nMaxRow + 3, nMaxCol + 36 )
         hb_DispOutAt( nMaxRow - 1, nMaxCol - 34, cMessage, 0x70 )

         FOR i := 1 TO Len( aOptionsOK )
            hb_DispOutAt( nMaxRow + 2, aPosButtons[ i ], " " + aOptionsOK[ i ] + " ", iif( i == nChoice, 0x07, 0x70 ) )
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
            hb_DispOutAt( nMaxRow + 2, aPosButtons[ i ], " " + aOptionsOK[ i ] + " ", iif( i == nChoice, 0x07, 0x70 ) )
         NEXT

         EXIT

      CASE HB_K_RESIZE

         hb_Scroll()
         AutoSize()

         PanelDisplay( aPanelLeft )
         PanelDisplay( aPanelRight )

         ComdLineDisplay( aPanelSelect )

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

FUNCTION HC_Alert( cMessage, aOptions )

   LOCAL nOldCursor := SetCursor( SC_NONE )
   LOCAL nRowPos, nColPos
   LOCAL aMessage
   LOCAL nLenMessage, nLenOptions
   LOCAL nCurrent, nWidth1, nWidth2
   LOCAL i, nPos
   LOCAL nCol, nRow
   LOCAL cScreen
   LOCAL nTop, nLeft, nBottom, nRight
   LOCAL nMaxRow := 0, nMaxCol := 0
   LOCAL nKey, nKeyStd
   LOCAL nChoice := 1

   nRowPos := Row()
   nColPos := Col()

   IF ValType( aOptions ) != "A"
      aOptions := { "OK" }
   ENDIF

   aMessage := hb_ATokens( cMessage, ";" )

   nWidth1 := 2
   nWidth2 := 0

   nLenOptions := Len( aOptions )
   FOR i := 1 TO nLenOptions
      nWidth1 += Len( aOptions[ i ] ) + 4
      nWidth2 += Len( aOptions[ i ] ) + 2
   NEXT

   nLenMessage := Len( aMessage )
   FOR i := 1 TO nLenMessage
      nWidth1 := Max( nWidth1, Len( aMessage[ i ] ) + 2 )
   NEXT

   DO WHILE .T.

      DispBegin()
      nRow := Int( ( MaxRow() - nLenMessage ) / 3 )
      nCol := Int( ( MaxCol() - nWidth1 ) / 2 )

      /* Zachowuje wycinek ekranu do późniejszego użycia */
      nTop    := nRow
      nLeft   := nCol
      nBottom := nRow + 3 + nLenMessage
      nRight  := nCol + nWidth1 + 1

      IF nMaxRow != MaxRow() .OR. nMaxCol != MaxCol()

         cScreen := SaveScreen( nTop, nLeft, nBottom + 1, nRight + 2 )
         hb_DispBox( nTop, nLeft, nBottom, nRight, HB_B_SINGLE_UNI + " ", 0x70 )
         hb_Shadow( nTop, nLeft, nBottom, nRight )

         FOR nPos := 1 TO nLenMessage
            hb_DispOutAt( nRow + nPos, nCol + 1, PadC( aMessage[ nPos ], nWidth1 ), 0x70 )
         NEXT

         nMaxRow := MaxRow()
         nMaxCol := MaxCol()

      ENDIF

      nRow += nPos + 1
      nCurrent := Int( ( nWidth1 - nWidth2 ) / ( nLenOptions  + 1 ) )

      nCol += nCurrent + 1

      FOR i := 1 TO nLenOptions
         hb_DispOutAt( nRow, nCol, " " + aOptions[ i ] + " ", iif( i == nChoice, 0x7, 0x70 ) )
         nCol += Len( aOptions[ i ] ) + nCurrent + 2
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

      CASE HB_K_RESIZE

         hb_Scroll()
         AutoSize()

         PanelDisplay( aPanelLeft )
         PanelDisplay( aPanelRight )

         ComdLineDisplay( aPanelSelect )

         BottomBar()

      ENDCASE

   ENDDO

   RestScreen( nTop, nLeft, nBottom + 1, nRight + 2, cScreen )
   SetCursor( nOldCursor )
   SetPos( nRowPos, nColPos )

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
   LOCAL nOldRow, nOldCol
   LOCAL cScreen
   LOCAL tsDateTime

   nOldRow := Row()
   nOldCol := Col()
   cScreen := SaveScreen( 0, 0, MaxRow(), MaxCol() )

   /* Returns the contents of a text file as a character string. */
   IF ( cString := hb_MemoRead( cFileName ) ) == ""
      HC_Alert( "Error reading: " + cFileName )
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

            StringDisplay( aString, nRow, nCol, nRowNo )

         ENDIF

         DispBegin()
         hb_DispOutAt( 0, 0, ;
            PadR( cFileName + "  ", nMaxCol + 1 ), 0x30 )

         StringDisplay( aString, nRow, nCol, nRowNo )

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

            IF MRow() > 0 .AND. MRow() < MaxRow() .AND. MCol() > 0 .AND. MCol() < MaxCol() .AND. MRow() < Len( aString ) + 1
               nRow := MRow()
               nCol := MCol()
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

            IF nCol < Len( aString[ nRowNo + nRow ]  )
               nCol++
            ENDIF

            EXIT

         CASE K_HOME

            nCol := 0

            EXIT

         CASE K_END

            nCol := Len( aString[ nRowNo + nRow ]  )

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

   ENDIF

   RestScreen( 0, 0, MaxRow(), MaxCol(), cScreen )
   SetPos( nOldRow, nOldCol )

   RETURN

STATIC PROCEDURE StringDisplay( aString, nRow, nCol, nRowNo )

   LOCAL i
   LOCAL nMaxRow := MaxRow(), nMaxCol := MaxCol()
   LOCAL nLine

   hb_Scroll( 2, 0, nMaxRow - 2, nMaxCol )

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

   SetPos( nRow, nCol )

   RETURN

STATIC PROCEDURE SaveFile( aString, cFileName )

   LOCAL cString := ""

   AEval( aString, {| e | cString += e + hb_eol() } )
   hb_MemoWrit( cFileName, cString )

   RETURN

// ====================================
FUNCTION Q( xPar )
   RETURN Alert( hb_ValToExp( xPar ) )
// ====================================
