Unit fos_ocrt;

{$H-}

{---------------------------------------------------------------------------
                                 CncWare
                         (c) Copyright 1999-2000
 ---------------------------------------------------------------------------
  Filename..: ocrt.pp
  Programmer: Ken J. Wright, ken@cncware.com
  Date......: 03/01/99

  Purpose - crt unit replacement plus OOP windows using ncurses.

  NOTE: All of the crt procedures & functions have been replaced with ncurses
  driven versions. This makes the ncurses library a little easier to use in a
  Pascal program and benefits from terminal independence.

-------------------------------<< REVISIONS >>--------------------------------
  Ver  |   Date   | Prog| Description
-------+----------+-----+-----------------------------------------------------
  1.00 | 03/01/99 | kjw | Initial Release.
       | 03/22/99 | kjw | Added nDelWindow(), delwin() does not nil pointer.
  1.01 | 11/22/99 | kjw | Added the following: nEcho, ClrEol, ClrBot, InsLine,
                        | DelLine, Delay, nClrEol, nClrBot, nInsLine, nDelLine,
                        | nRefresh, nScroll, nDrawBox, nNewWindow, nWinColor,
                        | nWriteScr, nFrame & some functions for returning
                        | line drawing character values.
  1.02 | 11/26/99 | kjw | Added nKeypressed().
  1.03 | 12/01/99 | kjw | Added global boolean nIsActive.
  1.04 | 12/03/99 | kjw | 1) Added procedures nHline, nVLine, & nWriteAC.
                        | 2) Changed all the line draw character functions
                        | (i.e., nHL, nVL) to return the longint value from
                        | ncurses rather than the character value (which was
                        | not very useful!). Now these can be passed to
                        | nWriteAC() to correctly write the line drawing
                        | characters.
                        | 3) Added more of the ACS characters.
  1.05 | 12/08/99 | kjw | 1) StartCurses() is now done as part of the unit
                        | initialization block. EndCurses() is done via an
                        | exit procedure.
                        | 2) nIsActive is now a function (safer!).
                        | 3) Added panel unit for windowing.
                        | 4) Added tnWindow object.
  1.10 | 12/12/99 | kjw | Added nSEdit().
  1.11 | 12/12/99 | kjw | Added Special property to tEC object. Now any normal
                        | character can trigger sedit to exit.
------------------------------------------------------------------------------
  2.00 | 12/13/99 | kjw | nCrt renamed to oCrt. A new nCrt has been created
                        | which is a drop-in replacement for the FPC crt unit.
                        | oCrt contains all of nCrt plus the OOP extensions.
                        | All of the common code is in ncrt.inc.
  2.01 | 12/15/99 | kjw | 1) A tnWindow object now becomes the target for
                        | stdout following Init & Show. A Hide will put the
                        | target back to stdscr.
                        | 2) Added nSetActiveWin() to manually pick a target
                        | window for stdout.
  2.02 | 12/15/99 | kjw | 1) PutFrame applied keypad to stdscr instead of sub.
                        | 2) See ncrt.inc
  2.03 | 12/16/99 | kjw | 1) See ncrt.inc
                        | 2) Added shift/f-key constants.
  2.04 | 01/04/00 | kjw | See ncrt.inc
  2.05 | 01/06/00 | kjw | 1) See ncrt.inc.
                        | 2) Added boolean internal_fwrite. FWrite was failing
                        | when trying to write outside of the active window.
                        | 3) nSEdit was not handling tec.firsttime correctly
                        | when a tec.special was processed.
  2.06 | 01/11/00 | kjw | See ncrt.inc.
  2.07 | 01/31/00 | kjw | 1) See ncrt.inc.
                        | 2) Added getcolor, getframecolor, getheadercolor
                        | methods to tnWindow.
  2.08 | 06/09/00 | kjw | 1) Added Picture property to tEC object. This is
                        | used for picture input masking in nSEdit.
                        | 2) Added nCheckPxPicture() function.
                        | 3) nSEdit() changed to use picture input masking.
                        | See pxpic.txt for a description of the picture
                        | string format.

  2.08.01 | 06/11/2000 | kjw
          | Fixed the spin cycle problem in nCheckPXPicture.
  2.09.00 | 06/16/2000 | kjw
          | 1) nSEdit renamed to nEdit. Now nSEdit just calls nEdit() for
          | compatibility.
          | 2) Added overloaded nEdit functions for Integer, LongInt, and
          | Real types.
          | 3) Changed nEdit() embedding of control characters to preface
          | with a ^P. Also now uses a highlight attribute for the control
          | characters.
          | 4) Added control character cursor control to nEdit().
          | 5) Added Esc/1..0 = F1..F10 to nEdit().
          | 6) Added '@' to match set in pxpic.inc.
          | 7) tnWindow.Align was not positioning properly. Off by one.
          | 8) tnWindow.Init used wrong pointer for keypad and intrflush.
          | 9) tnWindow.Edit was messing up ec.Special.
  2.09.01 | 06/16/2000 | kjw
          | 1) nStdScr (tnWindow) added and initialized at unit startup.
          | nStdScr can be used for a default full screen window.
          | 2) nEdit overloaded to work without a window pointer. It works
          | with the currently active window.
  2.10.00 | 06/23/2000 | kjw
          | 1) Added character mapping to the tEC object. This includes the
          | ChMap property and the AddChMap() and ClrChMap() methods.
          | 2) Added AppendMode property to the tEC object. The character
          | typed in nEdit() is always appended to the current string
          | regardless of cursor position. Useful when ExitMode is true.
          | 3) tnWindow.Done was not re-assigning an ActiveWn.
          | 4) nEdit LeftArrow was allowing < x.
          | 5) Added nEditNumber() function.
          | 6) Added nEditDate() function.
          | 7) I made a command decision and renamed the tEC.FirstTime
          | property to tEC.ClearMode as it is more descriptive.
  2.11.00 | 1) Cleaned up some loose ends with 2.10.
          | 2) Some more overloading
          | 3) Removed tnWindow.readln, write, and writeln methods.
          | 4) See ncrt.inc.
  2.12.00 | 1) Remove the "n" from the tnWindow.editxxx functions for
          | consistancy. Procedurals are prefaced with an "n". Object methods
          | are not.
          | 2) Procedural FWrite renamed to nFWrite.
          | 3) tEC object type renamed to tnEC.
          | 4) Added nMakeWindow(), a one line procedural wrapper for
          | tnWindow.Init and tnWindow.PutHeader.
          | 5) Added GetX, GetY, IsFramed methods to tnWindow;
          | 6) Fixed nFWrite for too long strings;
          | 7) tnWindow.Align was wrong when justify was none.
  2.13.00 | 06/30/00 | kjw | See ncrt.inc
  2.14.00 | 07/05/00 | kjw | See ncrt.inc
  2.15.00 | 07/12/00 | kjw |
          | 1) Renamed IsBold to nIsBold. Renamed SetColorPair to nSetColorPair.
          | 2) Added tnMenu object (not functional);
          | 07/17/00 | kjw |
          | 2) Argh!! Align method had another mistake. Changed x/y=1 to =0.
          | 3) Added nShowMessage() function.
          | 4) tnMenu is now minimally functional.
          | 07/25/00 | kjw |
          | 1) tnMenu fully functional for current level.
  2.16.00 | 08/14/2000 | kjw |
          | 1) Added Get/SetMark(), IsActive(), IsValid(), IsAssigned(),
          | SetIndex() to tnMenu.
          | 08/18/2000 | kjw |
          | 1) Added nkXXX constants for all(?) extended keys.
          | 2) Changed all uses of extended keys to use new nkXXX's.
          | 3) Edit overloaded to return a nkXXX in ch rather that a char.
          | 4) Resize method added to tnWindow.
          | 5) AddChMap overloaded for preferred (easier) use with nkXXX's.
          | 08/24/2000 | kjw |
          | 1) Added nReadScr, nReadScrStr, nReadScrColor, nWriteScrStr,
          | nGrabScreen, nPopScreen, nReleaseScreen.
          | 2) Fixed some trouble with PrevWn accuracy.
  2.16.01 | 05/26/2009 | kjw |
          | 1) Corrected error with tnWindow.PutFrame and wattr_get. Recent
          | updates to ncurses and ocrt by the FreePascal team introduced an
          | error with tnWindow.PutFrame's use of wattr_get.
------------------------------------------------------------------------------
}
Interface

Uses
{$ifdef unix}
    baseunix,
    termio,
{$endif}
  fos_ncurses,dos,strings;  {dos needed for TextRec}

Const

   { decimal number format, us or european }
   nUS = 0;
   nEURO = 1;
   nDecFmt : byte = nUS;

   { border styles for text boxes }
   btNone : integer = 0;
   btSingle : integer = 1;
   btDouble : integer = 2;

   { ordinal keycodes, new style, preferred }
   nkEnter     = 13;       { Enter key }
   nkEsc       = 27;       { Home key }
   nkHome      = -71;      { Home key }
   nkUp        = -72;      { Up arrow }
   nkPgUp      = -73;      { PgUp key }
   nkLeft      = -75;      { Left arrow }
   nkRight     = -77;      { Right arrow }
   nkEnd       = -79;      { End key }
   nkDown      = -80;      { Down arrow }
   nkPgDn      = -81;      { PgDn key }
   nkIns       = -82;      { Insert key }
   nkDel       = -83;      { Delete key }
   nkCtrlLeft  = -115;     { Ctrl/left arrow }
   nkCtrlRight = -116;     { Ctrl/right arrow }
   nkF1        = -59;      { f1 key }
   nkF2        = -60;      { f2 key }
   nkF3        = -61;      { f3 key }
   nkF4        = -62;      { f4 key }
   nkF5        = -63;      { f5 key }
   nkF6        = -64;      { f6 key }
   nkF7        = -65;      { f7 key }
   nkF8        = -66;      { f8 key }
   nkF9        = -67;      { f9 key }
   nkF10       = -68;      { f10 key }
   nkF11       = -84;      { shift/f1 key }
   nkF12       = -85;      { shift/f2 key }
   nkF13       = -86;      { shift/f3 key }
   nkF14       = -87;      { shift/f4 key }
   nkF15       = -88;      { shift/f5 key }
   nkF16       = -89;      { shift/f6 key }
   nkF17       = -90;      { shift/f7 key }
   nkF18       = -91;      { shift/f8 key }
   nkF19       = -92;      { shift/f9 key }
   nkF20       = -93;      { shift/f10 key }
   nkAltA      = -30;      { alt/a }
   nkAltB      = -48;      { alt/b }
   nkAltC      = -46;      { alt/c }
   nkAltD      = -32;      { alt/d }
   nkAltE      = -18;      { alt/e }
   nkAltF      = -33;      { alt/f }
   nkAltG      = -34;      { alt/g }
   nkAltH      = -35;      { alt/h }
   nkAltI      = -23;      { alt/i }
   nkAltJ      = -36;      { alt/j }
   nkAltK      = -37;      { alt/k }
   nkAltL      = -38;      { alt/l }
   nkAltM      = -50;      { alt/m }
   nkAltN      = -49;      { alt/n }
   nkAltO      = -24;      { alt/o }
   nkAltP      = -25;      { alt/p }
   nkAltQ      = -16;      { alt/q }
   nkAltR      = -19;      { alt/r }
   nkAltS      = -31;      { alt/s }
   nkAltT      = -20;      { alt/t }
   nkAltU      = -22;      { alt/u }
   nkAltV      = -47;      { alt/v }
   nkAltW      = -17;      { alt/w }
   nkAltX      = -45;      { alt/x }
   nkAltY      = -21;      { alt/y }
   nkAltZ      = -44;      { alt/z }
   nkAlt1      = -120;     { alt/1 }
   nkAlt2      = -121;     { alt/2 }
   nkAlt3      = -122;     { alt/3 }
   nkAlt4      = -123;     { alt/4 }
   nkAlt5      = -124;     { alt/5 }
   nkAlt6      = -125;     { alt/6 }
   nkAlt7      = -126;     { alt/7 }
   nkAlt8      = -127;     { alt/8 }
   nkAlt9      = -128;     { alt/9 }
   nkAlt0      = -129;     { alt/0 }
   nkAltMinus  = -130;     { alt/- }
   nkAltEqual  = -131;     { alt/= }
   nkAltTab    = -15;      { alt/tab }

   { ordinal key codes (old style, don't break any apps!) }
   nKeyEnter     = nkEnter;
   nKeyEsc       = nkEsc;
   nKeyHome      = abs(nkHome);
   nKeyUp        = abs(nkUp);
   nKeyPgUp      = abs(nkPgUp);
   nKeyLeft      = abs(nkLeft);
   nKeyRight     = abs(nkRight);
   nKeyEnd       = abs(nkEnd);
   nKeyDown      = abs(nkDown);
   nKeyPgDn      = abs(nkPgDn);
   nKeyIns       = abs(nkIns);
   nKeyDel       = abs(nkDel);
   nKeyCtrlLeft  = abs(nkCtrlLeft);
   nKeyCtrlRight = abs(nkCtrlRight);
   nKeyF1        = abs(nkF1);
   nKeyF2        = abs(nkF2);
   nKeyF3        = abs(nkF3);
   nKeyF4        = abs(nkF4);
   nKeyF5        = abs(nkF5);
   nKeyF6        = abs(nkF6);
   nKeyF7        = abs(nkF7);
   nKeyF8        = abs(nkF8);
   nKeyF9        = abs(nkF9);
   nKeyF10       = abs(nkF10);
   nKeyF11       = abs(nkF11);
   nKeyF12       = abs(nkF12);
   nKeyF13       = abs(nkF13);
   nKeyF14       = abs(nkF14);
   nKeyF15       = abs(nkF15);
   nKeyF16       = abs(nkF16);
   nKeyF17       = abs(nkF17);
   nKeyF18       = abs(nkF18);
   nKeyF19       = abs(nkF19);
   nKeyF20       = abs(nkF20);

   { character mapping }
   nMaxChMaps    = 255;     { maximun index for character mapping }

   { menus }
   nMAXMENUITEMS = 100;

Type
   {*** structures to save a screen via nGrabScreen ***}
   pnOneRow = pchtype;
   { a buffer for a max of 256 chtype items accessed via pchar }
   tnOneRow = array [0..1023] of char;
   { a one way linked list of screen rows }
   pnRowBuf = ^tnRowBuf;
   tnRowBuf = Record
      row : pnOneRow;    { one row of a screen }
      next : pnRowBuf;   { next row in the list }
   End;
   { the header record of a saved screen }
   pnScreenBuf = ^tnScreenBuf;
   tnScreenBuf = Record
      x,                 { column origin }
      y,                 { row origin }
      n : integer;       { number of columns }
      first : pnRowBuf;  { pointer to first row in list }
   End;

   tnS10 = string[10];

   { for scrolling a window }
   tnUpDown = (up,down);
   { for window & header positioning }
   tnJustify = (none,left,center,right,top,bottom);
   { used for nEC character mapping }
   (********* Note : these are obsolete *******)
   nChMapStr = string[4];
   {nChMap = array [1..nMaxChMaps] of nChMapStr;}
   (*******************************************)
   nChMap = array [1..nMaxChMaps,1..2] of integer;

   { used for nSEdit }
   {------------------------------------------------------------------------
     ClearMode = true : passed string is initialized to ''.
      IsHidden = true : causes a string of '*' to display in place of
                        the actual characters typed.
       InsMode        : toggle for insert/overwrite mode.
      ExitMode = true : sedit exits after every keystroke.
               = false: sedit only exits when #27,#13, or any extended
                        key *except* for Home,End,RArrow,LArrow.
       Special        : If a pressed key is found in this string, then
                        sedit exits without processing.
       Picture        : An input mask string. See pxpic.txt for an
                        explanation of picture strings.
     CtrlColor        : The highlight color for embedded control characters.
         ChMap        : An array of character triplets describing a character
                        that is typed and what it should map to.
    ------------------------------------------------------------------------}
   tnEC = Object
      ClearMode,
      IsHidden,
      InsMode,
      ExitMode,
      AppendMode : boolean;
      Special : string;
      Picture : string;
      CtrlColor : integer;
      ChMap : nChMap;
      Constructor Init(ft,ih,im,em,ap : boolean;
                                  s,p : string;
                                   cc : integer;
                                   mp : nChMap);
      Destructor Done;
      Function AddChMap(_in,_out : integer) : integer;
      Function AddChMap(mp : nChMapStr) : integer;
      Procedure ClrChMap(idx : integer);
   End;

   pwin = PWindow;

   pnWindow = ^tnWindow;
   tnWindow = Object
      Private
          wn : pwindow;       { pointer to win or sub to read/write to }
         win : pwindow;       { pointer to main window record }
         sub : pwindow;       { sub window if a bordered window }
         pan : ppanel;        { pointer to panel record }
         subp : ppanel;       { sub panel if a bordered window }
         visible : boolean;   { is the window visible? }
         hasframe : boolean;
         wincolor,            { window color }
         framecolor,          { frame color }
         hdrcolor : integer;  { header color }
         hdrpos : tnJustify;  { header alignment }
         header : string[80]; { header string }
         Procedure init_wins(x,y,x1,y1 : integer);
         Procedure done_wins;
      Public
         data : pointer;      { a pointer to user defined data }
         ec : tnEC;           { edit control settings }
         Constructor Init(x,y,x1,y1,wcolor : integer;
                                    border : boolean;
                                    fcolor : integer);
         Destructor Done;
         Procedure Resize(cols_,rows_ : integer);
         Procedure Active;  { make this the current window }
         Procedure Show;    { display the window }
         Procedure Hide;    { hide the window }
         Procedure ClrScr;
         Procedure ClrEol;
         Procedure ClrBot;
         Procedure InsLine;
         Procedure DelLine;
         Procedure GotoXY(x,y : integer);
          Function WhereX : integer;
          Function WhereY : integer;
          Function ReadKey : char;
         Procedure WriteAC(x,y,att,c : longint);
         Procedure FWrite(x,y,att,z : integer; s : string);
         Procedure DrawBox(LineStyle,x1,y1,x2,y2,att : Integer);
          Function GetHeader : string;
         Procedure PutHeader(hdr : string; hcolor : integer; hpos : tnJustify);
         Procedure SetColor(att : integer);
          Function GetColor : integer;
          Function GetFrameColor : integer;
          Function GetHeaderColor : integer;
         Procedure PutFrame(att : integer);
         Procedure Move(x,y : integer);
         Procedure Scroll(ln : integer; dir : tnUpDown);
         Procedure Align(hpos,vpos : tnJustify);
          Function Rows : integer;
          Function Cols : integer;
          Function GetX : integer;
          Function GetY : integer;
          Function IsFramed : boolean;
          Function IsVisible : Boolean;
          Function Edit(x,y,att,z,CursPos:Integer;es:String;Var ch : integer) : String;
          Function Edit(x,y,att,z,CursPos:Integer;es:LongInt;Var ch : integer) : LongInt;
          Function Edit(x,y,att,z,CursPos:Integer;es:Real;Var ch : integer) : Real;
          Function Edit(x,y,att,z,CursPos:Integer;es:String;Var ch : Char) : String;
          Function Edit(x,y,att,z,CursPos:Integer;es:LongInt;Var ch : Char) : LongInt;
          Function Edit(x,y,att,z,CursPos:Integer;es:Real;Var ch : Char) : Real;
          Function EditNumber(x,y,att,wid,decm : integer;bgd : string;initv,minv,maxv : real;var esc : boolean) : real;
          Function EditNumber(x,y,att,wid,decm : integer;bgd : string;initv,minv,maxv : longint;var esc : boolean) : longint;
          Function EditDate(x,y,att : integer;initv : string;var esc : boolean) : string;
   End;

   pnMenuStr = ^tnMenuStr;
   tnMenuStr = array [0..79] of char; { storage for menu item text }
   pnMenu = ^tnMenu;
   tnMenu = Object
      Private
         tc,   { text (item) color }
         cc,   { cursor (current item) color }
         fc,   { frame color }
         hc,   { header Color }
         gc,   { non-selectable color }
         x,y,  { top,left corner of window }
         r,c,  { how many rows & columns of items to display }
         wid,  { minimum window width }
         iidx, { item index }
         merr  { menu error code }
               : integer;
         loopon,
         framed,
         posted : boolean; { is the menu posted? }
         mark : tnS10;
         items : array[1..nMAXMENUITEMS] of pnMenuStr;
         pi : array[1..nMAXMENUITEMS] of pItem;
         pm : pMenu;
         win : pnWindow;
         Procedure InitWin;
         Procedure ClearItem(idx : integer);
         Procedure AddItem(i : integer; s : string);
         Function Selectable(idx : integer) : boolean;
         Function IsValid(idx : integer) : boolean;
      Public
         Constructor Init(_x,_y,_w,_r,_c,_tc,_cc,_gc : integer;
                          _fr : boolean; _fc : integer);
         Destructor Done;
         Procedure Post;            { create the menu of current items }
         Procedure UnPost;          { unbind the items and free the menu }
         Procedure Start;           { start user input, includes show }
         Procedure Stop;            { a shortcut for hide,unpost }
         Procedure Show;            { display the menu, includes post }
         Procedure Hide;            { remove the menu from the display }
          Function Wind : pnWindow; { pointer to the window object }
         Procedure Move(_x,_y : integer);       { shortcut window move }
         Procedure Align(hpos,vpos : tnJustify);{ shortcut window align }
         Procedure PutHeader(hdr : string; hcolor : integer; hpos : tnJustify);
         Procedure Clear;           { unpost and clear the menu item list }
          Function Add(s : string) : integer; { append a menu item }
         Procedure Insert(idx : integer; s : string); { insert a menu item }
         Procedure Remove(idx : integer);     { delete a menu item }
         Procedure Change(idx : integer; s : string); { change an item }
         Procedure Active(idx : integer; b : boolean); { toggle gray }
          Function IsActive(idx : integer) : boolean; { item active ? }
         Procedure Spin(b : boolean);{ toggle item looping }
          Function Status : integer;{ return the current error/status code }
          Function Index : integer; { return the current item index }
         Procedure SetIndex(idx : integer); { set the item index }
          Function Count : integer; { number of items in the menu }
          Function Rows(_r : integer) : integer; {get/set menu rows }
          Function Cols(_c : integer) : integer; {get/set menu columns }
          Function IsAssigned(idx : integer) : boolean; { valid & assigned }
          Function GetMark : string; { return the item mark string }
         Procedure SetMark(ms : string); { set the mark string }
         Procedure Refresh;
         Procedure SetColor(att : byte);       { change text color }
         Procedure SetCursorColor(att : byte); { change cursor color }
         Procedure SetFrameColor(att : byte);  { change frame color }
         Procedure SetGrayColor(att : byte);   { change inactive color }
   End;

Var
   nStdScr : tnWindow; { default window created at unit initialization }
   nscreen : pwin;     { pointer to ncurses stdscr }
   nEC : tnEC;         { global edit control object }

Procedure nSetActiveWin(win : pwindow);
Procedure nDoNow(donow : boolean);
 Function nKeypressed(timeout : word) : boolean;
Procedure nEcho(b : boolean);
Procedure nWindow(var win : pWindow; x,y,x1,y1 : integer);
Procedure nNewWindow(var win : pWindow; x,y,x1,y1 : integer);
Procedure nDelWindow(var win : pWindow);
Procedure nWinColor(win : pWindow; att : integer);
Procedure nClrScr(win : pWindow; att : integer);
Procedure nClrEol(win : pWindow);
Procedure nClrBot(win : pWindow);
Procedure nInsLine(win : pWindow);
Procedure nDelLine(win : pWindow);
Procedure nGotoXY(win : pWindow; x,y : integer);
 Function nWhereX(win : pWindow) : integer;
 Function nWhereY(win :  pWindow) : integer;
 Function nReadkey(win : pWindow) : char;
 Function nReadln(win : pWindow) : string;
Procedure nWrite(win : pWindow; s : string);
Procedure nWriteln(win : pWindow; s : string);
Procedure nWriteScr(win : pWindow; x,y,att : integer; s : string);
Procedure nRefresh(win : pWindow);
Procedure nScroll(win : pWindow; lines : integer; dir : tnUpDown);
Procedure nDrawBox(win : pWindow; LineStyle,x1,y1,x2,y2,att : Integer);
Procedure nFrame(win : pWindow);
 Function nRows(win : pWindow) : integer;
 Function nCols(win : pWindow) : integer;
 Function nHL : longint; { horizontal line }
 Function nVL : longint; { vertical line }
 Function nUL : longint; { upper left corner }
 Function nLL : longint; { lower loft corner }
 Function nUR : longint; { upper right corner }
 Function nLR : longint; { lower right corner }
 Function nLT : longint; { left tee }
 Function nRT : longint; { right tee }
 Function nTT : longint; { top tee }
 Function nBT : longint; { bottom tee }
 Function nPL : longint; { plus, + }
 Function nLA : longint; { left arrow }
 Function nRA : longint; { right arrow }
 Function nUA : longint; { up arror }
 Function nDA : longint; { down arrow }
 Function nDI : longint; { diamond }
 Function nCB : longint; { checkerboard }
 Function nDG : longint; { degree }
 Function nPM : longint; { plus/minus }
 Function nBL : longint; { bullet }
Procedure nHLine(win : pwindow; col,row,attr,x : integer);
Procedure nVLine(win : pwindow; col,row,attr,y : integer);
Procedure nWriteAC(win : pwindow; x,y : integer; att,acs_char : longint);
 Function nIsBold(att : integer) : boolean;
 Function nSetColorPair(att : integer) : integer;
Procedure nFWrite(win : pwindow; col,row,attrib : integer; clear : integer; s : string);
Procedure nFWrite(col,row,attrib : integer; clear : integer; s : string);
 Function nSEdit(win : pwindow; x,y,att,z,CursPos:Integer;es:String;Var ch : Char) : String;
 Function nEdit(win : pwindow; x,y,att,z,CursPos:Integer;es:String;Var ch : Char) : String;
 Function nEdit(win : pwindow; x,y,att,z,CursPos:Integer;es:LongInt;Var ch : Char) : LongInt;
 Function nEdit(win : pwindow; x,y,att,z,CursPos:Integer;es:Real;Var ch : Char) : Real;
 Function nEdit(x,y,att,z,CursPos:Integer;es:String;Var ch : Char) : String;
 Function nEdit(x,y,att,z,CursPos:Integer;es:LongInt;Var ch : Char) : LongInt;
 Function nEdit(x,y,att,z,CursPos:Integer;es:Real;Var ch : Char) : Real;
 Function nEdit(win : pwindow; x,y,att,z,CursPos:Integer;es:String;Var chv : integer) : String;
 Function nEdit(win : pwindow; x,y,att,z,CursPos:Integer;es:LongInt;Var ch : integer) : LongInt;
 Function nEdit(win : pwindow; x,y,att,z,CursPos:Integer;es:Real;Var ch : integer) : Real;
 Function nEdit(x,y,att,z,CursPos:Integer;es:String;Var ch : integer) : String;
 Function nEdit(x,y,att,z,CursPos:Integer;es:LongInt;Var ch : integer) : LongInt;
 Function nEdit(x,y,att,z,CursPos:Integer;es:Real;Var ch : integer) : Real;
 Function nEditNumber(win : pwindow; x,y,att,wid,decm : integer;bgd : string;initv,minv,maxv : real;var esc : boolean) : real;
 Function nEditNumber(win : pwindow; x,y,att,wid,decm : integer;bgd : string;initv,minv,maxv : longint;var esc : boolean) : longint;
 Function nEditNumber(x,y,att,wid,decm : integer;bgd : string;initv,minv,maxv : real;var esc : boolean) : real;
 Function nEditNumber(x,y,att,wid,decm : integer;bgd : string;initv,minv,maxv : longint;var esc : boolean) : longint;
 Function nEditDate(win : pwindow; x,y,att : integer;initv : string;var esc : boolean) : string;
 Function nEditDate(x,y,att : integer;initv : string;var esc : boolean) : string;
Procedure nMakeWindow(var win : tnWindow;x1,y1,x2,y2,ta,ba,ha : integer;hasframe : boolean;hdrpos : tnJustify;hdrtxt : string);
Procedure nMakeWindow(var win : pnWindow;x1,y1,x2,y2,ta,ba,ha : integer;hasframe : boolean;hdrpos : tnJustify;hdrtxt : string);
Procedure nMakeMenu(var mnu : tnMenu;x,y,_w,_r,_c,ta,ca,ga,ba,ha : integer;hasframe : boolean;hdrpos : tnJustify;hdrtxt : string);
Procedure nMakeMenu(var mnu : pnMenu;x,y,_w,_r,_c,ta,ca,ga,ba,ha : integer;hasframe : boolean;hdrpos : tnJustify;hdrtxt : string);
 Function nShowMessage(msg : string;matt : byte;hdr : string;hatt : byte;ack : boolean) : pnWindow;
 Function nReadScr(win : pWindow; x,y,n : integer) : string;
 Function nReadScr(x,y,n : integer) : string;
 Function nReadScrStr(win : pWindow; x,y,n : integer; buf : pchtype) : pchtype;
 Function nReadScrStr(x,y,n : integer; buf : pchtype) : pchtype;
 Function nReadScrColor(win : pWindow; x,y : integer) : integer;
 Function nReadScrColor(x,y : integer) : integer;
Procedure nWriteScrStr(win : pWindow; x,y : integer; s : pchtype);
Procedure nWriteScrStr(x,y : integer; s : pchtype);
Procedure nGrabScreen(var p : pnScreenBuf; x,y,c,r : integer; win : pWindow);
Procedure nGrabScreen(var p : pnScreenBuf; x,y,c,r : integer);
Procedure nGrabScreen(var p : pnScreenBuf);
Procedure nPopScreen(p : pnScreenBuf; x,y : integer; win : pWindow);
Procedure nPopScreen(p : pnScreenBuf; x,y : integer);
Procedure nPopScreen(p : pnScreenBuf);
Procedure nReleaseScreen(p : pnScreenBuf);
 Function nCheckPxPicture(var s, Pic : string; var CPos : integer) : word;

 {---------------------------------------------------------------------------
                                  CncWare
                          (c) Copyright 1999-2000
                    Portions copyright the FreePascal Team
  ---------------------------------------------------------------------------
   Filename..: ncrt.inc
   Programmer: Ken J. Wright, ken@cncware.com
   Date......: 03/01/99

   Purpose - Code that is common to nCrt and oCrt.

 -------------------------------<< REVISIONS >>--------------------------------
   Ver  |   Date   | Prog| Description
 -------+----------+-----+-----------------------------------------------------
   2.00 | 12/13/99 | kjw | Initial Release.
   2.02 | 12/15/99 | kjw | Removed use of endwin. Replaced with tcget/setattr.
   2.03 | 12/16/99 | kjw | 1) Added shifted f-keys to nReadkey.
                         | 2) Added raw & scrollok to StartCurses.
                         | 3) Added alt'd keyset support.
   2.04 | 01/04/00 | kjw | keypressed changed back to method of using getch
                         | rather than select.
   2.05 | 01/06/00 | kjw | 1) StartCurses now defaults to echo. Readkey sets to
                         | noecho. This allows nCrt to handle echoing in the
                         | default manor, but allows oCrt to control echoing
                         | in the app with nEcho. Note: Read(ln) will always
                         | echo as normal, regardless of any setting by nEcho.
                         | Also set DoRefresh to true.
                         | 2) nDelWindow now checks for stdscr or curscr and
                         | makes sure that ActiveWn is not nil.
                         | 3) Window() now moves to 1,1 and does not do a
                         | clrscr.
   2.06 | 01/11/00 | kjw | 1) Oops! 2.04 change went back to stdscr vs. ActiveWn.
                         | Keypressed works correctly with windows again.
                         | 2) ClrEol works correctly now with color.
   2.07 | 01/31/00 | kjw | 1) Added NCRT_VERSION constants.
                         | 2) Added prev_textattr to detect a change in
                         | TextAttr value so current color gets updated.
                         | 3) See ocrt.pp
   2.08 | 06/09/00 | kjw | See ocrt.pp

   2.08.01 | 06/11/2000 | kjw | See ocrt.pp
   2.09.00 | 06/16/2000 | kjw | See ocrt.pp
   2.10.00 | 06/16/2000 | kjw | See ocrt.pp
   2.11.00 | 06/27/2000 | kjw
           | 1) See ocrt.pp
           | 2) Now uses ncurses for CrtRead so console control characters
           | work correctly (i.e., <ctrl/h>, <backspace>, etc.).
   2.12.00 | 06/29/2000 | kjw | See ocrt.pp
   2.13.00 | 06/30/2000 | kjw
           | Added nStop and nStart procedures.
   2.14.00 | 07/05/2000 | kjw
           | 1) Added nCursor and nEscDelay functions.
           | 2) Added nInit and moved code from ncrt.pp & ocrt.pp to it.
           | 3) KEY_ALTMINUS & KEYALTEQUAL were reversed, but mapping ended
           | up correct.
   2.15.00 | 1) Added nMaxRows & nMaxCols constants.
           | 2) See ocrt.pp
   2.16.00 | 08/14/2000 | kjw | See ocrt.pp
           | 08/24/2000 | kjw |
           | 1) Added nTermName.
           | 2) Added CursesFailed.
           | 3) Moved all common initialization code to nInit.
           | 4) prev_textattr more reliable.
 ------------------------------------------------------------------------------
 }

 Procedure AssignCrt(var F: Text);
 Procedure ClrEol;
 Procedure ClrScr;
 Procedure ClrBot;
 Procedure Delay(DTime: Word);
 Procedure DelLine;
 Procedure GotoXY(x,y : integer);
 Procedure HighVideo;
 Procedure InsLine;
  Function Keypressed : boolean;
 Procedure LowVideo;
 Procedure NormVideo;
 Procedure NoSound;
  Function Readkey : char;
 Procedure Sound(hz : word);
 Procedure TextBackground(att : byte);
 Procedure TextColor(att : byte);
 Procedure TextMode(mode : word);
  Function WhereX : integer;
  Function WhereY : integer;
 Procedure Window(x,y,x1,y1 : integer);
 Procedure nStop;
 procedure nExit;
 Procedure nStart;
  Function nCursor(c : integer) : integer;
  Function nEscDelay(d : longint) : longint;
  Function nTermName : string;

 Const

    NCRT_VERSION_MAJOR = 2;
    NCRT_VERSION_MINOR = 16;
    NCRT_VERSION_PATCH = 0;
    NCRT_VERSION = '2.16.00';

  { CRT modes }
    BW40          = 0;            { 40x25 B/W on Color Adapter }
    CO40          = 1;            { 40x25 Color on Color Adapter }
    BW80          = 2;            { 80x25 B/W on Color Adapter }
    CO80          = 3;            { 80x25 Color on Color Adapter }
    Mono          = 7;            { 80x25 on Monochrome Adapter }
    Font8x8       = 256;          { Add-in for ROM font }

  { Mode constants for 3.0 compatibility }
    C40           = CO40;
    C80           = CO80;

    Black        =  0;
    Blue         =  1;
    Green        =  2;
    Cyan         =  3;
    Red          =  4;
    Magenta      =  5;
    Brown        =  6;
    LightGray    =  7;
    DarkGray     =  8;
    LightBlue    =  9;
    LightGreen   = 10;
    LightCyan    = 11;
    LightRed     = 12;
    LightMagenta = 13;
    Yellow       = 14;
    White        = 15;
    Blink        = 128;

    TextAttr : Byte = $07;
    LastMode : Word = 3;
    WindMin  : Word = $0;
    WindMax  : Word = $184f;

    { support for the alt'd characters }
    { these get initialized by StartCurses }
    KEY_ALTA = 465; { alt/a }
    KEY_ALTB = 466;
    KEY_ALTC = 467;
    KEY_ALTD = 468;
    KEY_ALTE = 469;
    KEY_ALTF = 470;
    KEY_ALTG = 471;
    KEY_ALTH = 472;
    KEY_ALTI = 473;
    KEY_ALTJ = 474;
    KEY_ALTK = 475;
    KEY_ALTL = 476;
    KEY_ALTM = 477;
    KEY_ALTN = 478;
    KEY_ALTO = 479;
    KEY_ALTP = 480;
    KEY_ALTQ = 481;
    KEY_ALTR = 482;
    KEY_ALTS = 483;
    KEY_ALTT = 484;
    KEY_ALTU = 485;
    KEY_ALTV = 486;
    KEY_ALTW = 487;
    KEY_ALTX = 488;
    KEY_ALTY = 489;
    KEY_ALTZ = 490; { alt/z }
    KEY_ALT1 = 491; { alt/1 }
    KEY_ALT2 = 492; { alt/2 }
    KEY_ALT3 = 493; { alt/3 }
    KEY_ALT4 = 494; { alt/4 }
    KEY_ALT5 = 495; { alt/5 }
    KEY_ALT6 = 496; { alt/6 }
    KEY_ALT7 = 497; { alt/7 }
    KEY_ALT8 = 498; { alt/8 }
    KEY_ALT9 = 499; { alt/9 }
    KEY_ALT0 = 500; { alt/0 }
    KEY_ALTMINUS = 501; { alt/- }
    KEY_ALTEQUAL = 502; { alt/= }
    KEY_ALTTAB   = 503; { alt/tab }

    { cursor type }
    cOFF = 0; { invisible cursor }
    cON  = 1; { normal cursor }
    cBIG = 2; { very visible cursor }

    { fullscreen size }
    nMaxRows : word = 25; { reset at startup to terminal setting }
    nMaxCols : word = 80; { for columns and rows }

  var
    CheckBreak,
    CheckEOF,
    CheckSnow,
    DirectVideo: Boolean;

 procedure FOS_Init_OCRT;

 Implementation

 Const
    { standard file descriptors }
    STDIN  = 0;
    STDOUT = 1;
    STDERR = 2;

 Var
    ExitSave : pointer;                { pointer to original exit proc }
    fg,bg : integer;                   { foreground & background }
    cp : array [0..7,0..7] of integer; { color pair array }
    ps : array [0..255] of char;       { for use with pchars }
    doRefresh : boolean;               { immediate refresh toggle }
    SubWn,                             { window created from window() }
    PrevWn,                            { previous window when active changes }
    ActiveWn : pwindow;                { current active window for stdout }
    tmp_b : boolean;
    isEcho : boolean;                  { keeps track of echo status }
    MaxRows,                           { set at startup to terminal values }
    MaxCols : longint;                 { for columns and rows }
    tios : TermIOS;                    { saves the term settings at startup }
    prev_textattr : integer;           { detect change in TextAttr }

 {==========================================================================}

 { set the active window for write(ln), read(ln) }
 Procedure SetActiveWn(win : pwindow);
 Begin
    If win <> ActiveWn Then PrevWn := ActiveWn;
    { don't set to a nil window! }
    If win <> Nil Then
       ActiveWn := win
    Else
       ActiveWn := stdscr;
 End;

 {--------------------------------------------
   initialize ncurses screen & keyboard, and
   return a pointer to stdscr.
   NOTE: This is done at unit initialization.
  --------------------------------------------}
 Function StartCurses(var win : pWindow) : Boolean;
 Var
    i : integer;
    s : string[3];
 Begin
    { save the current terminal settings }
    tcGetAttr(STDIN,tios);
    if initscr=Nil then Begin
       StartCurses := false;
       win := nil;
       Exit;
    End Else Begin
       StartCurses := true;
       start_color;
       cbreak; { disable keyboard buffering }
       raw;    { disable flow control, etc. }
       echo;   { echo keypresses }
       nonl;   { don't process cr in newline }
       intrflush(stdscr,bool(false));
       keypad(stdscr,bool(true));
       scrollok(stdscr,bool(true));
       win := stdscr;
       isEcho := true;
       doRefresh := true;
       getmaxyx(stdscr,MaxRows,MaxCols);
       { make these values visible to apps }
       nMaxRows := MaxRows;
       nMaxCols := MaxCols;
       { define the the alt'd keysets for ncurses }
       { alt/a .. atl/z }
       for i := ord('a') to ord('z') do Begin
          s := #27+chr(i)+#0;
          define_key(@s[1],(KEY_ALTA-97)+i);
       End;
       { alt/1 .. alt/9 }
       for i := 1 to 9 do Begin
          s := #27+chr(i)+#0;
          define_key(@s[1],(KEY_ALT1-1)+i);
       End;
       s := #27+'0'+#0; define_key(@s[1],KEY_ALT0);     { alt/0 }
       s := #27+'-'+#0; define_key(@s[1],KEY_ALTMINUS); { alt/- }
       s := #27+'='+#0; define_key(@s[1],KEY_ALTEQUAL); { alt/= }
       s := #27+#9+#0;  define_key(@s[1],KEY_ALTTAB);   { alt/tab }
    End;
 End;

 {----------------------------------
   Shutdown ncurses.
   NOTE: This is done via ExitProc.
  ----------------------------------}
 Procedure EndCurses;
 Begin
    { restore the original terminal settings }
    { and leave the screen how the app left it }
    tcSetAttr(STDIN,TCSANOW,tios);
 End;

 {--------------------------------------------------------
   This disables any curses activity until a refresh.
   Use this BEFORE any shelling (shell,exec,execv,etc)
   to put the terminal temporarily back into cooked mode.
  --------------------------------------------------------}
 Procedure nStop;
 Begin
    endwin;
 End;

 {---------------------------------------------
   Simply a refresh to re-establish the curses
   terminal settings following an nStop.
  ---------------------------------------------}
 Procedure nStart;
 Begin
    refresh;
 End;

 { see if the specified attribute is high intensity }
 Function nIsBold(att : integer) : boolean;
 Begin
    bg := att div 16;
    fg := att - (bg * 16);
    nisbold := (fg > 7);
 End;

 { map a curses color to an ibm color }
 Function c2ibm(c : integer) : integer;
 { ncurses constants
    COLOR_BLACK   = 0;
    COLOR_RED     = 1;
    COLOR_GREEN   = 2;
    COLOR_YELLOW  = 3;
    COLOR_BLUE    = 4;
    COLOR_MAGENTA = 5;
    COLOR_CYAN    = 6;
    COLOR_WHITE   = 7;
 }
 Var
    att : integer;
 Begin
    Case c of
       COLOR_BLACK   : att := black;
       COLOR_RED     : att := red;
       COLOR_GREEN   : att := green;
       COLOR_YELLOW  : att := brown;
       COLOR_BLUE    : att := blue;
       COLOR_MAGENTA : att := magenta;
       COLOR_CYAN    : att := cyan;
       COLOR_WHITE   : att := lightgray;
       else att := c;
    End;
    c2ibm := att;
 End;

 { map an ibm color to a curses color }
 Function ibm2c(c : integer) : integer;
 Var
    att : integer;
 Begin
    Case c of
       black     : att := COLOR_BLACK;
       red       : att := COLOR_RED;
       green     : att := COLOR_GREEN;
       brown     : att := COLOR_YELLOW;
       blue      : att := COLOR_BLUE;
       magenta   : att := COLOR_MAGENTA;
       cyan      : att := COLOR_CYAN;
       lightgray : att := COLOR_WHITE;
       else att := c;
    End;
    ibm2c := att;
 End;

 { initialize a color pair }
 Function nSetColorPair(att : integer) : integer;
 var
    i : integer;
 Begin
    bg := att div 16;
    fg := att - (bg * 16);
    While bg > 7 Do dec(bg,8);
    While fg > 7 Do dec(fg,8);
    bg := ibm2c(bg);
    fg := ibm2c(fg);
    i := cp[bg,fg];
    init_pair(i,fg,bg);
    nSetColorPair := i;
 End;

 { map a standard color attribute to an ncurses attribute }
 Function CursesAtts(att : byte) : longint;
 Var
    atts : longint;
 Begin
    atts := COLOR_PAIR(nSetColorPair(att));
    If nIsBold(att) Then atts := atts or A_BOLD;
    If (att and $80) = $80 Then atts := atts or A_BLINK;
    CursesAtts := atts;
 End;

 {------------------------------------------------
   Delete a window.
   NOTE: This does not clear it from the display.
  ------------------------------------------------}
 Procedure nDelWindow(var win : pWindow);
 Begin
    If (win = stdscr) or (win = curscr) Then Exit;
    If win <> Nil Then delwin(win);
    win := Nil;
    If ActiveWn = Nil Then SetActiveWn(stdscr);
 End;

 {-----------------------------------------
   Set the current text color of a window,
   delayed until next refresh.
  -----------------------------------------}
 Procedure nWinColor(win : pWindow; att : integer);
 Begin
    wattrset(win,CursesAtts(att));
    prev_textattr := att;
 End;

 { clear the specified window }
 procedure nClrScr(win : pWindow; att : integer);
 Begin
    wbkgd(win,CursesAtts(att));
    TouchWin(win);
    werase(win);
    If doRefresh Then wrefresh(win);
    prev_textattr := att;
 End;

 { clear from the cursor to the end of line in a window }
 Procedure nClrEol(win : pWindow);
 Var
    tmp : pwindow;
    x,y,
    xb,yb,
    xm,ym : longint;
 Begin
    {--------------------------------------------------------
      In order to have the correct color, we must define and
      clear a temporary window. ncurses wclrtoeol() uses the
      window background color rather that the current color
      attribute ;-(
     --------------------------------------------------------}
    getyx(win,y,x);
    getbegyx(win,yb,xb);
    getmaxyx(win,ym,xm);
    tmp := subwin(win,1,xm-x,yb+y,xb+x);
    If tmp = nil then Exit;
    wbkgd(tmp,CursesAtts(TextAttr));
    werase(tmp);
 {   wclrtoeol(win);}
    If doRefresh Then wrefresh(tmp);
    delwin(tmp);
 End;

 { clear from the cursor to the bottom in a window }
 Procedure nClrBot(win : pWindow);
 Begin
    wclrtobot(win);
    If doRefresh Then wrefresh(win);
 End;

 { insert a line at the cursor line in a window }
 Procedure nInsLine(win : pWindow);
 Begin
    winsertln(win);
    If doRefresh Then wrefresh(win);
 End;

 { delete line at the cursor in a window }
 Procedure nDelLine(win : pWindow);
 Begin
    wdeleteln(win);
    If doRefresh Then wrefresh(win);
 End;

 { position cursor in a window }
 Procedure nGotoXY(win : pWindow; x,y : integer);
 Begin
    wmove(win,y-1,x-1);
    touchwin(win);
    If doRefresh Then wrefresh(win);
 End;

 { find cursor x position in a window }
 Function nWhereX(win : pWindow) : integer;
 var x,y : longint;
 Begin
    getyx(win,y,x);
    nWhereX := x+1;
 End;

 { find cursor y position in a window }
 Function nWhereY(win : pWindow) : integer;
 var x,y : longint;
 Begin
    getyx(win,y,x);
    nWhereY := y+1;
 End;

 {---------------------------------------------------------------------
  read a keystroke from a window, including function keys and extended
  keys (arrows, etc.)
  Note: Make sure that keypad(win,true) has been issued prior to use.
        ( nWindow does this )
  ---------------------------------------------------------------------}
 Function nReadkey(win : pWindow) : char;
 var
    c : char;
    l : longint;
    xtnded : boolean;
 Begin
    l := wgetch(win);
    { if it's an extended key, then map to the IBM values }
    if l > 255 then begin
       xtnded := true;
       c := #27;
       Case l of
          KEY_BREAK : Begin xtnded := false; c := #3; End;
          KEY_BACKSPACE : Begin xtnded := false; c := #8; End;
          KEY_IC    : c := #82; { insert }
          KEY_DC    : c := #83; { delete }
          KEY_HOME  : c := #71; { home }
          KEY_END   : c := #79; { end }
          KEY_UP    : c := #72; { up arrow }
          KEY_DOWN  : c := #80; { down arrow }
          KEY_LEFT  : c := #75; { left arrow }
          KEY_RIGHT : c := #77; { right arrow }
          KEY_NPAGE : c := #81; { page down }
          KEY_PPAGE : c := #73; { page up }
          KEY_ALTA  : c := #30; { alt/a }
          KEY_ALTB  : c := #48;
          KEY_ALTC  : c := #46;
          KEY_ALTD  : c := #32;
          KEY_ALTE  : c := #18;
          KEY_ALTF  : c := #33;
          KEY_ALTG  : c := #34;
          KEY_ALTH  : c := #35;
          KEY_ALTI  : c := #23;
          KEY_ALTJ  : c := #36;
          KEY_ALTK  : c := #37;
          KEY_ALTL  : c := #38;
          KEY_ALTM  : c := #50;
          KEY_ALTN  : c := #49;
          KEY_ALTO  : c := #24;
          KEY_ALTP  : c := #25;
          KEY_ALTQ  : c := #16;
          KEY_ALTR  : c := #19;
          KEY_ALTS  : c := #31;
          KEY_ALTT  : c := #20;
          KEY_ALTU  : c := #22;
          KEY_ALTV  : c := #47;
          KEY_ALTW  : c := #17;
          KEY_ALTX  : c := #45;
          KEY_ALTY  : c := #21;
          KEY_ALTZ  : c := #44;  { alt/z }
          KEY_ALT1  : c := #120; { alt/1 }
          KEY_ALT2  : c := #121; { alt/2 }
          KEY_ALT3  : c := #122; { alt/3 }
          KEY_ALT4  : c := #123; { alt/4 }
          KEY_ALT5  : c := #124; { alt/5 }
          KEY_ALT6  : c := #125; { alt/6 }
          KEY_ALT7  : c := #126; { alt/7 }
          KEY_ALT8  : c := #127; { alt/8 }
          KEY_ALT9  : c := #128; { alt/9 }
          KEY_ALT0  : c := #129; { alt/0 }
          KEY_ALTMINUS : c := #130; { alt/- }
          KEY_ALTEQUAL : c := #131; { alt/= }
          KEY_ALTTAB : c := #15; { alt/tab }
       Else
          Begin
             If l = Key_f(1) Then c := #59 Else
             If l = Key_f(2) Then c := #60 Else
             If l = Key_f(3) Then c := #61 Else
             If l = Key_f(4) Then c := #62 Else
             If l = Key_f(5) Then c := #63 Else
             If l = Key_f(6) Then c := #64 Else
             If l = Key_f(7) Then c := #65 Else
             If l = Key_f(8) Then c := #66 Else
             If l = Key_f(9) Then c := #67 Else
             If l = Key_f(10) Then c := #68 Else
             If l = Key_f(11) Then c := #84 Else
             If l = Key_f(12) Then c := #85 Else
             If l = Key_f(13) Then c := #86 Else
             If l = Key_f(14) Then c := #87 Else
             If l = Key_f(15) Then c := #88 Else
             If l = Key_f(16) Then c := #89 Else
             If l = Key_f(17) Then c := #90 Else
             If l = Key_f(18) Then c := #91 Else
             If l = Key_f(19) Then c := #92 Else
             If l = Key_f(20) Then c := #93;
          End;
       End;
       If xtnded Then Begin
          nReadKey := #0;
          ungetch(ord(c));
          Exit;
       End Else
          nReadkey := c;
    End Else
       nReadkey := chr(ord(l));
 End;

 { write a string to a window at the current cursor position }
 Procedure nWrite(win : pWindow; s : string);
 Begin
    If TextAttr <> prev_textattr Then
       nWinColor(win,TextAttr);
    waddstr(win,StrPCopy(ps,s));
    If doRefresh Then wrefresh(win);
 End;

 {=========================================================================
   CrtWrite, CrtRead, CrtReturn, CrtClose, CrtOpen, AssignCrt.
   These functions come from the FPC distribution rtl/linux/crt unit.
   These are the hooks into the input/output stream needed for write(ln)
   and read(ln).
  =========================================================================}

 { used by CrtWrite }
 Procedure DoWrite(temp : string);
 Begin
    nWrite(ActiveWn,temp);
 End;

 Function CrtWrite(Var F: TextRec): Integer;
 {
   Top level write function for CRT
 }
 Var
   Temp : String;
   idx,i : Longint;
 {  oldflush : boolean;}
 Begin
 {  oldflush:=ttySetFlush(Flushing);}
   idx:=0;
   while (F.BufPos>0) do
    begin
      i:=F.BufPos;
      if i>255 then
       i:=255;
      system.Move(F.BufPTR^[idx],Temp[1],F.BufPos);
      Temp[0]:=Chr(i);
      DoWrite(Temp);
      dec(F.BufPos,i);
      inc(idx,i);
    end;
 {  ttySetFlush(oldFLush);}
   CrtWrite:=0;
 End;

 Function CrtRead(Var F: TextRec): Integer;
 {
   Read from CRT associated file.
 }
 Begin
   { let's use ncurses instead! }
   FillChar(F.BufPtr^, F.BufSize, #0);
   wgetnstr(ActiveWn,F.BufPtr^, F.BufSize-1);
   F.BufEnd := Length(StrPas(F.BufPtr^))+1;
   F.BufPtr^[F.BufEnd-1] := #10;
   F.BufPos:=0;
 {  CrtWrite(F);}
   CrtRead:=0;
 End;

 Function CrtReturn(Var F:TextRec):Integer;
 Begin
   F.BufEnd := 0;
   F.BufPos:= 0;
   CrtReturn:=0;
 end;

 Function CrtClose(Var F: TextRec): Integer;
 {
   Close CRT associated file.
 }
 Begin
   F.Mode:=fmClosed;
   CrtClose:=0;
 End;

 Function CrtOpen(Var F: TextRec): Integer;
 {
   Open CRT associated file.
 }
 Begin
   If F.Mode=fmOutput Then
    begin
      TextRec(F).InOutFunc:=@CrtWrite;
      TextRec(F).FlushFunc:=@CrtWrite;
    end
   Else
    begin
      F.Mode:=fmInput;
      TextRec(F).InOutFunc:=@CrtRead;
      TextRec(F).FlushFunc:=@CrtReturn;
    end;
   TextRec(F).CloseFunc:=@CrtClose;
   CrtOpen:=0;
 End;

 procedure AssignCrt(var F: Text);
 {
   Assign a file to the console. All output on file goes to console instead.
 }
 begin
   Assign(F,'');
   TextRec(F).OpenFunc:=@CrtOpen;
 end;

 {==========================================================================
                       Standard crt unit replacements
  ==========================================================================}
 { set the text background color }
 Procedure TextBackground(att : byte);
 Begin
    TextAttr:=
     ((att shl 4) and ($f0 and not Blink)) or (TextAttr and ($0f OR Blink) );
    nWinColor(ActiveWn,TextAttr);
 End;

 { set the text foreground color }
 Procedure TextColor(att : byte);
 Begin
    TextAttr := (att and $8f) or (TextAttr and $70);
    nWinColor(ActiveWn,TextAttr);
 End;

 { set to high intensity }
 Procedure HighVideo;
 Begin
   TextColor(TextAttr Or $08);
 End;

 { set to low intensity }
 Procedure LowVideo;
 Begin
   TextColor(TextAttr And $77);
 End;

 { set to normal display colors }
 Procedure NormVideo;
 Begin
   TextColor(7);
   TextBackGround(0);
 End;

 { clear stdscr }
 Procedure ClrScr;
 Begin
    nClrScr(ActiveWn,TextAttr);
 End;

 { clear from the cursor to the end of line in stdscr }
 Procedure ClrEol;
 Begin
    nClrEol(ActiveWn);
 End;

 { clear from the cursor to the bottom of stdscr }
 Procedure ClrBot;
 Begin
    nClrBot(ActiveWn);
 End;

 { insert a line at the cursor line in stdscr }
 Procedure InsLine;
 Begin
    nInsLine(ActiveWn);
 End;

 { delete line at the cursor in stdscr }
 Procedure DelLine;
 Begin
    nDelLine(ActiveWn);
 End;

 { position cursor in stdscr }
 Procedure GotoXY(x,y : integer);
 Begin
    nGotoXY(ActiveWn,x,y);
 End;

 { find cursor x position in stdscr }
 Function WhereX : integer;
 Begin
    WhereX := nWhereX(ActiveWn);
 End;

 { find cursor y position in stdscr }
 Function WhereY : integer;
 Begin
    WhereY := nWhereY(ActiveWn);
 End;

 { Wait for DTime milliseconds }
 Procedure Delay(DTime: Word);
 Begin
   fpselect(0,nil,nil,nil,DTime);
 End;

 { create a new subwindow of stdscr }
 Procedure Window(x,y,x1,y1 : integer);
 Begin
    nDelWindow(SubWn);
    SubWn := subwin(stdscr,y1-y+1,x1-x+1,y-1,x-1);
    If SubWn = nil then Exit;
    intrflush(SubWn,bool(false));
    keypad(SubWn,bool(true));
    scrollok(SubWn,bool(true));
    SetActiveWn(SubWn);
    GotoXY(1,1);
 End;

 {------------------------------------------------------
  Check if a key has been pressed.
  Note: this is best used along with select() on STDIN,
  as it can suck up lots of cpu time.
  Better yet, use nKeypressed instead if you don't need
  to include file descriptors other than STDIN.
  ------------------------------------------------------}
 function  Keypressed : boolean;
 var
    l : longint;
 {   fd : fdSet;}
 Begin
    Keypressed := FALSE;
    nodelay(ActiveWn,bool(TRUE));
    l := wgetch(ActiveWn);
    If l <> ERR Then Begin { ERR = -(1) from unit ncurses }
       ungetch(l);
       Keypressed := TRUE;
    End;
    nodelay(ActiveWn,bool(FALSE));

 { Below is more efficient code, but does not work well with
   nReadkey & extended keys because nReadkey's ungetch does not
   force a change in STDIN. So, a "while keypressed" block does
   not produce the expected results when trapping for char(0)
   followed by a second scan code.

    FD_Zero(fd);
    fd_Set(STDIN,fd);
    Keypressed := (Select(STDIN+1,@fd,nil,nil,0) > 0);
 }
 End;

 { silently read a key from stdscr }
 Function Readkey : char;
 Begin
    tmp_b := IsEcho;
    noecho;
    Readkey := nReadkey(ActiveWn);
    If tmp_b Then echo;
 End;

 { a cheap replacement! }
 Procedure Sound(hz : word);
 Begin
    Beep;
    wrefresh(ActiveWn);
 End;

 Procedure NoSound;
 Begin
 End;

 Procedure TextMode(mode : word);
 Begin
    nDelWindow(SubWn);
    SetActiveWn(stdscr);
    LastMode := mode;
    DirectVideo := true;
    CheckSnow := true;
    NormVideo;
    ClrScr;
 End;

 { Set the cursor visibility. Returns the previous value }
 { or (-1) if value c is not supported by the terminal. }
 Function nCursor(c : integer) : integer;
 Begin
    nCursor := curs_set(c);
 End;

 { Set the <esc> key delay time in milliseconds. }
 { Use d=(-1) to return current value without updating. }
 Function nEscDelay(d : longint) : longint;
 Begin
    nEscDelay := ESCDELAY;
    If d >= 0 Then ESCDELAY := d;
 End;

 { return the current terminal name (same as $TERM env variable) }
 Function nTermName : string;
 Begin
    nTermName := StrPas(termname);
 End;

 { could not initialize ncurses }
 Procedure CursesFailed;
 Begin
    { give 'em a clue! }
    Writeln('StartCurses() failed');
    Halt;
 End;

 { exit procedure to ensure curses is closed up cleanly }
 Procedure nExit;
 Begin
    ExitProc := ExitSave;
    EndCurses;
 End;

 Procedure nInit;
 Begin
    { set the unit exit procedure }
    ExitSave := ExitProc;
    ExitProc := @nExit;
    { load the color pairs array with color pair indices (0..63 }
    For bg := 0 to 7 Do For fg := 0 to 7 do cp[bg,fg] := (bg*8)+fg;
    { initial window pointers }
    SubWn := nil;
    PrevWn := ActiveWn;
    { basic gray on black screen }
    TextMode(LastMode);
    { Redirect the standard output }
    assigncrt(Output);
    Rewrite(Output);
    TextRec(Output).Handle:=StdOutputHandle;
    { Redirect the standard input }
    assigncrt(Input);
    Reset(Input);
    TextRec(Input).Handle:=StdInputHandle;
    { some defaults }
    nEscDelay(500); { default is 1000 (1 second) }
    nCursor(cON);   { normal cursor }
 End;

 {---------------------------------------------------------------------------
                                  CncWare
               Created and Copyright (c) 1991  J. John Sprenger
 ----------------------------------------------------------------------------
   Filename..: pxpic.inc
   Programmer: Ken J. Wright, ken@cncware.com
   Date......: 06/09/2000

   Purpose - Duplicates the functionality of the TPXPictureValidator.IsValid
             method from Turbo Vision's validate unit. This function was
             extracted from a unit called fmtline written by J. John Sprenger.
             It was actually written before the validate unit was available
             from Borland in TV2.0.

 -------------------------------<< REVISIONS >>--------------------------------
   Ver  |   Date   | Prog| Description
 -------+----------+-----+-----------------------------------------------------
   1.00 | 06/10/00 | kjw | Initial Release.
   1.01 | 06/11/00 | kjw | Finally debugged the spin cycle! The AnyLeft function
                         | missed a condition that left it an endless loop.
                         | Added the boolean "done" to fix it.
   1.02 | 06/15/00 | kjw | Added '@' to the match set.
 ------------------------------------------------------------------------------}

   {    Created and Copyright (c) 1991  J. John Sprenger    }

   { tFormatLine.CheckPicture is the function that inspects }
   { the input string passed as S against the Pic string    }
   { which holds the Paradox-form Picture.  If an error is  }
   { found the position of the error is placed in CPos.     }

 function nCheckPxPicture(var s, Pic : string;
                       var CPos : integer) : word;
   const
     { flError, flCharOk and flFormatOK are constants used  }
     { by tFormatLine.CheckPicture.  flError is returned    }
     { when an error is found,  flCharOk when an character  }
     { is found to be appropriate,  And flFormatOk when the }
     { entire input string is found acceptable.             }
     flError    = $0000;
     flCharOK   = $0001;
     flFormatOK = $0002;

   var
     Resolved  : integer;
     TempIndex : integer;

   { Function Copy represents a bit of syntactic sugar for  }
   { the benefit of the author.  It changes the Copy func.  }
   { so that its parameters represent start and end points  }
   { rather than a start point followed by a quantity.      }
   function Copy(s : string; start, stop : integer) : string;
   begin
     if stop < start then Copy:=''
     else Copy:=System.Copy(s,start,stop-start+1);
   end;

   { Function FindMatch recursively locates the matching   }
   (* grouping characters for "{" and "[".                *)
   function FindMatch(P : string) : integer;
   var
     i:integer;
     match:boolean;
   begin
     i:=2;
     match:=false;
     while (i<=length(P)) and not match do begin
       if ((p[i]=']') and (p[1]='[')) or ((p[i]='}') and
         (p[1]='{')) then
         match:=true;
       if p[i]='{' then
         i:=i+FindMatch(Copy(p,i,length(p)))
       else
         if p[i]='[' then
           i:=i+FindMatch(Copy(p,i,length(P)))
         else inc(i);
     end;
     FindMatch:=i-1;
   end;

   { Function CP is the heart of tFormatLine.  It           }
   { determines if the string, s, passed to it fits the     }
   { requirements of the picture, Pic.  The number of       }
   { characters successfully resolved is returned in the    }
   { parameter resolved. When groups or repetitions are     }
   { encountered CP will call itself recursively.           }
   function CP(var s : string; Pic : string; var CPos :
               integer; var Resolved : integer) : word;
   const
      CharMatchSet = ['#', '?', '&', '''', '@', '!'];
   var
     i          : integer;
     index      : integer;
     result_     : word;
     commit     : boolean;
     Groupcount : integer;

   { Procedure Succeed resolves defaults and <Space>        }
   { default requests                                       }

   { Note:
     The little patch below to exclude group end checking during
     expansion lets autofill work as it should, however it also
     autofills prematurely when there are more optionals or
     alternates. I haven't quite figured how to make this work
     correctly within the current recursion scheme.
     kjw
   }
     procedure Succeed;
     var
       t     : integer;
       found : boolean;
     begin
       if (i <= Length(s)) and
          (s[i]=' ') and
          (Pic[index]<>' ') and
          (Pic[index]<>',')
       then begin
         t:=index;
         found:=false;
         while (t<=length(pic)) and not found do begin
           if not (Pic[t] in (CharMatchSet+
                  ['*','[','{',',',']','}'])) then begin
             if pic[t]=';' then inc(t);
             s[i]:=Pic[t];
             found:=true;
           end;
           inc(t);
         end;
       end;
       if (i>length(s)) then
         {----------------------}
         { Expand with defaults }
         while not (Pic[index] in
               (CharMatchSet+['*','[','{',',',']','}'])) and
               (index<=length(Pic)) and
               not(Pic[index-1] in [(*'}',*)','(*,']'*)]) do begin {kjw}
           if Pic[index]=';' then inc(index);
           s[i]:=Pic[index];
           if i>length(s) then begin
             CPos:=i;
             s[0]:=char(i);
           end;
           inc(i);
           inc(index);
         end;
     end;

   { Function AnyLeft returns true if there are no required }
   { characters left in the Picture string.                 }
     function AnyLeft : boolean;
     var
        TempIndex : integer;
        done : boolean; {kjw, 06/11/2000}
     begin
       done := false;
       TempIndex:=index;
       while ((Pic[TempIndex]='[') or (Pic[TempIndex]='*'))
             and (TempIndex<=Length(Pic))
             and (Pic[TempIndex]<>',')
             and not done do begin
         if Pic[TempIndex]='[' then
           Tempindex:=Tempindex+FindMatch(Copy(Pic,index, Length(Pic)))
         else begin
           if not (Pic[TempIndex+1] in ['0'..'9']) then begin
             inc(TempIndex);
             if Pic[TempIndex] in ['{','['] then
               tempIndex:=TempIndex+ FindMatch(Copy(pic,index,length(pic)))
             else inc(TempIndex);
           end else done := true;
         end;
       end;
       AnyLeft:=(TempIndex<=length(Pic)) and
                (Pic[TempIndex]<>',');
     end;

   { Function CharMatch determines if the current character }
   { matches the corresponding character mask in the        }
   { Picture string. Alters the character if necessary.     }
     function CharMatch : word;
     var result_ : word;
     begin
       result_:=flError;
       case Pic[index] of
         '#': if s[i] in ['0'..'9'] then result_:=flCharOk;
         '?': if s[i] in ['A'..'Z','a'..'z'] then
           result_:=flCharOk;
         '&': if s[i] in ['A'..'Z','a'..'z'] then
           begin
             result_:=flCharOk;
             s[i]:=upcase(s[i]);
           end;
        '''': result_:=flCharOk;
         '@': result_:=flCharOk;
         '!': begin
            result_:=flCharOk;
            s[i]:=upcase(s[i]);
         end;
       end;
       if result_<>flError then commit:=true;
       CharMatch:=result_;
     end;

   { Function Literal handles characters which are needed    }
   { by the picture but otherwise used as format specifiers. }
   { All such characters are preceded by the ';' in the      }
   { picture string.                                         }
     function Literal : word;
     var result_ : word;
     begin
       inc(index);
       if s[i]=Pic[index] then result_:=flCharOk
       else result_:=flError;
       if result_<>flError then commit:=true;
       Literal:=result_;
     end;

   { Function Group handles required and optional groups    }
   { in the picture string.  These are designated by the    }
   (* "{","}" and "[","]" character pairs.                 *)
     function Group:word;
     var
       result_: word;
       TempS: string;
       TempPic: string;
       TempCPos: integer;
       PicEnd: integer;
       TempIndex: integer;
       SwapIndex:integer;
       SwapPic : string;
     begin
       TempPic:=Copy(Pic,index,length(Pic));
       PicEnd:=FindMatch(TempPic);
       TempPic:=Copy(TempPic,2,PicEnd-1);
       TempS:=Copy(s,i,length(s));
       TempCPos:=1;

       result_:=CP(TempS,TempPic,TempCPos,TempIndex);

       if result_=flCharOK then inc(GroupCount);
       if (result_=flFormatOK) and (groupcount>0) then
         dec(GroupCount);
       if result_<>flError then result_:=flCharOk;

       SwapIndex:=index;
       index:=TempIndex;
       SwapPic:=Pic;
       Pic:=TempPic;
       if not AnyLeft then result_:=flCharOk;
       pic:=SwapPic;
       index:=SwapIndex;
       if i>1 then s:=copy(s,1,i-1)+TempS else s:=TempS;

       CPos:=Cpos+TempCPos-1;
       if Pic[index]='[' then begin
         if result_<>flError then
            i:=i+TempCPos-1
         else dec(i);
         result_:=flCharOK;
       end
       else i:=i+TempCPos-1;
       index:=index+PicEnd-1;
       Group:=result_;
     end;

   { Function Repetition handles characters that may be     }
   { repeated in the input string.  The picture string      }
   { indicates this possiblity with "*" character.          }
     function Repetition:word;
     var
       result_:word;
       count:integer;
       TempPic:string;
       TempS:string;
       TempCPos:integer;
       TempIndex:integer;
       SwapIndex:integer;
       SwapPic:string;
       PicEnd:integer;
       commit:boolean;

       procedure MakeCount;
       var nstr:string;
           code:integer;
       begin
         if Pic[index] in ['0'..'9'] then begin
           nstr:='';
           repeat
             nstr:=nstr+Pic[index];
             inc(index);
           until not(Pic[index] in ['0'..'9']);
           val(nstr,count,code);
         end
         else count:=512;
       end;

       procedure MakePic;
       begin
         if Pic[index] in ['{','['] then begin
           TempPic:=copy(Pic,index,length(Pic));
           PicEnd:=FindMatch(TempPic);
           TempPic:=Copy(TempPic,2,PicEnd-1);
         end
       else begin
         if Pic[index]<>';' then begin
           TempPic:=''+Pic[index];
           PicEnd:=3;
           if index=1 then
             pic:='{'+pic[index]+'}'+ copy(pic,index+1,length(pic))
           else pic:=copy(pic,1,index-1)+
                          '{'+pic[index]+'}'+
                          copy(pic,index+1,length(pic));
         end
         else begin
           TempPic:=Pic[index]+Pic[index+1];
           PicEnd:=4;
           if index=1 then
             pic:='{' + pic[index] + pic[index+1]+'}' +
                  copy(pic,index+1,length(pic))
           else pic:=copy(pic,1,index-1) + '{' + pic[index] +
                     pic[index+1] + '}' + copy(pic,index+1,length(pic));
         end;
       end;
     end;

     begin
       inc(index);
       MakeCount;
       MakePic;
       result_:=flCharOk;
       while (count<>0) and (result_<>flError) and
             (i<=length(s)) do begin
         commit:=false;
         TempS:=Copy(s,i,length(s));
         TempCPos:=1;
         result_:=CP(TempS,TempPic,TempCPos,TempIndex);

         if result_=flCharOK then inc(GroupCount);
         if (result_=flFormatOK) and (groupcount > 0) then
           dec(GroupCount);
         if result_<>flError then result_:=flCharOk;

         SwapIndex:=Index;
         Index:=TempIndex;
         SwapPic:=Pic;
         Pic:=TempPic;
         if (not AnyLeft) then result_:=flCharOk;
         Pic:=SwapPic;
         index:=SwapIndex;
         if i>1 then s:=copy(s,1,i-1)+TempS else s:=TempS;
         Cpos:=Cpos+TempCpos-1;
         if (count>255) then begin
           if result_<>flError then begin
             i:=i+TempCpos-1;
             if not commit then commit:=true;
             result_:=flCharOk;
           end
           else dec(i);
         end
           else i:=i+TempCPos-1;
         inc(i);
         dec(count);
       end;
       dec(i);
       index:=index+PicEnd-1;
       if result_=flError then
          if (count>255) and not commit
            then result_:=flCharOk;
       repetition:=result_;
     end;

     begin { of function CP}
       i:=1;
       index:=1;
       result_:=flCharOk;
       commit:=false;
       Groupcount:=0;
       while (i<=length(s)) and (result_<>flError) do begin
         if index>length(Pic) then result_:=flError
         else begin
           if s[i]=' ' then Succeed;
           if Pic[index] in CharMatchSet then
             result_:=CharMatch
           else
             if Pic[index]=';' then
               result_:=Literal
             else
               if (Pic[index]='{') or (Pic[index]='[') then
                 result_:=Group
               else
                 if Pic[index]='*' then
                   result_:=Repetition
                 else
                   if Pic[index] in [',','}',']'] then
                     result_:=flError
                   else
                     if Pic[index]=s[i] then begin
                       result_:=flCharOk;
                       commit:=true;
                     end
                     else result_:=flError;
           if (result_ = flError) and not commit then begin
             TempIndex:=Index;
             while (TempIndex<=length(Pic)) and
                   ((Pic[TempIndex]<>',') and
                   (Pic[TempIndex-1]<>';'))  do begin
               if (Pic[TempIndex]='{') or
                  (Pic[TempIndex]=']') then
                 Index:=FindMatch(Copy( Pic,
                                  TempIndex,length(Pic)))+TempIndex-1;
                 inc(TempIndex);
             end;
             if Pic[TempIndex]=',' then begin
               if Pic[TempIndex-1]<>';' then begin
                 result_:=flCharOk;
                 index:=TempIndex;
                 inc(index);
               end;
             end;
           end
           else if result_<>flError then begin
             inc(i);
             inc(index);
             Succeed;
           end;

         end;
       end;
       Resolved:=index;

       if (result_=flCharOk) and
          (GroupCount=0) and
          (not AnyLeft or ((Pic[index-1]=',') and
          (Pic[index-2]<>';'))) then
          result_:=flFormatOk;

       CPos:=i-1;
       CP:=result_;
     end;

 begin{ of function CheckPicture}
    Resolved:=0;
    CPos := 0;
    If (Pic = '') or (s = '') Then
       nCheckPxPicture := flFormatOk
    Else
       nCheckPxPicture:=CP(s,Pic,CPos,Resolved);
 end;

Var
   _chmap : nChMap;

{---------------------------------------------------------------------
  tnWindow.Init

  Create a new window.
       x = upper left corner x, screen relative
       y = upper left corner y, screen relative
      x1 = lower right corner x, screen relative
      y1 = lower right corner y, screen relative
  wcolor = window/text color
  border = include a frame?
  fcolor = frame color
 ---------------------------------------------------------------------}
Constructor tnWindow.Init(x,y,x1,y1,wcolor : integer;
                           border : boolean;
                           fcolor : integer);
Var
   mp : nChMap;
Begin
   hasframe := border;
   wincolor := wcolor;
   framecolor := fcolor;
   hdrcolor := wcolor;
   header := '';
   data := nil;
   visible := false;
   init_wins(x,y,x1,y1);
   FillChar(mp,SizeOf(mp),#0);
   ec.Init(false,false,false,false,false,'','',15,mp);
   ec.ClrChMap(0);
   SetActiveWn(wn);
End;

{ deallocate the window }
Destructor tnWindow.Done;
Begin
   done_wins;
   ec.Done;
   SetActiveWn(nscreen);
End;

Procedure tnWindow.init_wins(x,y,x1,y1 : integer);
Begin
   win := nil;
   sub := nil;
   pan := nil;
   subp := nil;
   win := newwin(y1-y+1,x1-x+1,y-1,x-1);
   pan := new_panel(win);
   hide_panel(pan);
   If hasframe Then
      PutFrame(framecolor)
   Else Begin
      wn := win;
      wbkgd(win,COLOR_PAIR(nSetColorPair(wincolor)));
      If nisbold(wincolor) then wattr_on(win,A_BOLD,nil);
      scrollok(win,bool(true));
      intrflush(win,bool(false));
      keypad(win,bool(true));
   End;
End;

Procedure tnWindow.done_wins;
Begin
   If subp <> nil Then del_panel(subp);
   If pan <> nil Then del_panel(pan);
   If sub <> nil Then delwin(sub);
   If (win <> nil) and (win <> stdscr) Then delwin(win);
   subp := nil;
   pan := nil;
   sub := nil;
   If win <> stdscr Then win := nil;
End;

Procedure tnWindow.ReSize(cols_,rows_ : integer);
Var
   xx,yy,
   mx,my : integer;
   vis : boolean;
Begin
   xx := GetX;
   yy := GetY;
   { can't be larger than full screen }
   If cols_ > nMaxCols Then cols_ := nMaxCols;
   If rows_ > nMaxRows Then rows_ := nMaxRows;
   { set the bottom, right corner }
   mx := xx+cols_-1;
   my := yy+rows_-1;
   { expand left? }
   If mx > nMaxCols Then xx := nMaxCols-cols_+1;
   { expand up? }
   If my > nMaxRows Then yy := nMaxRows-rows_+1;
   If xx < 1 Then xx := 1;
   If yy < 1 Then yy := 1;
   { reset the bottom, right corner }
   mx := xx+cols_-1;
   my := yy+rows_-1;
   { constrain to full screen }
   If mx > nMaxCols Then mx := nMaxCols;
   If my > nMaxRows Then my := nMaxRows;
   vis := visible;
   Hide;
   visible := vis;
   done_wins;
   init_wins(xx,yy,mx,my);
   If visible Then Show;
End;

{ make the window current for all normal crt requests }
Procedure tnWindow.Active;
Begin
   SetActiveWn(wn);
End;

{ display the window and move to the top }
Procedure tnWindow.Show;
Begin
   SetActiveWn(wn);
   visible := true;
   show_panel(pan);
   If subp <> nil Then show_panel(subp);
   update_panels;
   doupdate;
End;

{ hide the window }
Procedure tnWindow.Hide;
Begin
   { don't go back to yourself }
   If PrevWn <> wn Then
      SetActiveWn(PrevWn)
   Else
      SetActiveWn(stdscr);
   visible := false;
   If subp <> nil Then hide_panel(subp);
   hide_panel(pan);
   update_panels;
   doupdate;
   GotoXY(WhereX,WhereY);
End;

Procedure tnWindow.ClrScr;
Begin
   tmp_b := dorefresh;
   dorefresh := visible;
   nClrScr(wn,wincolor);
   dorefresh := tmp_b;
End;

Procedure tnWindow.ClrEol;
Begin
   tmp_b := dorefresh;
   dorefresh := visible;
   nClrEol(wn);
   dorefresh := tmp_b;
End;

Procedure tnWindow.ClrBot;
Begin
   tmp_b := dorefresh;
   dorefresh := visible;
   nClrBot(wn);
   dorefresh := tmp_b;
End;

Procedure tnWindow.InsLine;
Begin
   tmp_b := dorefresh;
   dorefresh := visible;
   nInsLine(wn);
   dorefresh := tmp_b;
End;

Procedure tnWindow.DelLine;
Begin
   tmp_b := dorefresh;
   dorefresh := visible;
   nDelLine(wn);
   dorefresh := tmp_b;
End;

{ return the window border header string }
Function tnWindow.GetHeader : string;
Begin
   GetHeader := header;
End;

{----------------------------------------------------------------------
  put/replace a header string at the top of a bordered window

     hdr = header string (top line of window, only if hasframe = true)
  hcolor = header line color
    hpos = justfication of header string, left, center, or right
 ----------------------------------------------------------------------}
Procedure tnWindow.PutHeader(hdr : string; hcolor : integer; hpos : tnJustify);
Var
   cp,
   hx,
   len : integer;
   att,
   mx,my : longint;
Begin
   If Hasframe Then Begin
      If hdr <> '' Then Begin
         header := hdr;
         hdrcolor := hcolor;
         hdrpos := hpos;
         getmaxyx(win,my,mx);
         nHline(win,2,1,framecolor,mx-1);
         len := mx-2;
         hdr := Copy(hdr,1,len);
         len := Length(hdr);
         Case hpos of
           left   : hx := 1;
           center : hx := (mx - len) div 2;
           right  : hx := (mx - len) - 1;
         End;
         mvwaddstr(win,0,hx,StrPCopy(ps,hdr));
         cp := nSetColorPair(hcolor);
         If nIsBold(hcolor) Then
            att := A_BOLD
         Else
            att := A_NORMAL;
         mvwchgat(win,0,hx,len,att,cp,Nil);
      End;
   End;
End;

{ set the the color of the writable window }
Procedure tnWindow.SetColor(att : integer);
Begin
   wbkgd(wn,COLOR_PAIR(nSetColorPair(att)));
   If nisbold(att) then 
     wattr_set(wn,A_BOLD,0,Nil);
   wincolor := att;
   If visible Then wrefresh(wn);
End;

{ get the writeable window color }
Function tnWindow.GetColor : integer;
Begin
   GetColor := wincolor;
End;

{ get the frame color }
Function tnWindow.GetFrameColor : integer;
Begin
   GetFrameColor := framecolor;
End;

{ get the header color }
Function tnWindow.GetHeaderColor : integer;
Begin
   GetHeaderColor := hdrcolor;
End;

{ frame an un-framed window, or update the frame color of a framed window }
Procedure tnWindow.PutFrame(att : integer);
Var
   x,y,
   mx,my,
   atts : longint;
   junk : smallint;
   
Begin
   wbkgd(win,COLOR_PAIR(nSetColorPair(att)));
   wattr_get(win,@atts,@junk,nil);
   If nisbold(att) then wattr_on(win,atts or A_BOLD,Nil);
   box(win,ACS_VLINE,ACS_HLINE);
   framecolor := att;
   If framecolor = -1 Then framecolor := wincolor;
   hasframe := true;
   If header <> '' Then PutHeader(header,hdrcolor,hdrpos);
   If sub = nil Then Begin
      getbegyx(win,y,x);
      getmaxyx(win,my,mx);
      sub := newwin(my-2,mx-2,y+1,x+1);
      If sub <> nil Then Begin
         subp := new_panel(sub);
         hide_panel(subp);
         wbkgd(sub,COLOR_PAIR(nSetColorPair(wincolor)));
         If nisbold(wincolor) then wattr_on(sub,A_BOLD,Nil);
         scrollok(sub,bool(true));
         intrflush(sub,bool(false));
         keypad(sub,bool(true));
         wn := sub;
      End;
   End;
   touchwin(sub);
   If visible Then Begin
      wrefresh(win);
      wrefresh(sub);
   End;
End;

{ move the window }
Procedure tnWindow.Move(x,y : integer);
Begin
   move_panel(pan,y-1,x-1);
   If subp <> nil Then move_panel(subp,y,x);
   If visible Then Begin
      update_panels;
      doupdate;
   End;
End;

Procedure tnWindow.Align(hpos,vpos : tnJustify);
Var
   x,y,
   bx,by : longint;
Begin
   getmaxyx(win,y,x);
   getbegyx(win,by,bx);
   Case hpos of
      none   : x := bx;
      left   : x := 0;
      right  : x := MaxCols - x;
      center : x := (MaxCols - x) div 2;
   End;
   Case vpos of
      none   : y := by;
      top    : y := 0;
      bottom : y := MaxRows - y;
      center : y := (MaxRows - y) div 2;
   End;
   move(x+1,y+1);
End;

Procedure tnWindow.Scroll(ln : integer; dir : tnUpDown);
Begin
   nScroll(wn,ln,dir);
End;

Procedure tnWindow.GotoXY(x,y : integer);
Begin
   tmp_b := dorefresh;
   dorefresh := visible;
   nGotoXY(wn,x,y);
   dorefresh := tmp_b;
End;

Function tnWindow.WhereX : integer;
Begin
   WhereX := nWhereX(wn);
End;

Function tnWindow.WhereY : integer;
Begin
   WhereY := nWhereY(wn);
End;

Function tnWindow.ReadKey : char;
Begin
   ReadKey := nReadKey(wn);
End;

Procedure tnWindow.WriteAC(x,y,att,c : longint);
Begin
   tmp_b := dorefresh;
   dorefresh := visible;
   nWriteAC(wn,x,y,att,c);
   dorefresh := tmp_b;
End;

Procedure tnWindow.FWrite(x,y,att,z : integer; s : string);
Begin
   tmp_b := dorefresh;
   dorefresh := visible;
   nFWrite(wn,x,y,att,z,s);
   dorefresh := tmp_b;
End;

Procedure tnWindow.DrawBox(LineStyle,x1,y1,x2,y2,att : Integer);
Begin
   tmp_b := dorefresh;
   dorefresh := visible;
   nDrawBox(wn,LineStyle,x1,y1,x2,y2,att);
   dorefresh := tmp_b;
End;

Function tnWindow.Rows : integer;
Begin
   Rows := nRows(wn);
End;

Function tnWindow.Cols : integer;
Begin
   Cols := nCols(wn);
End;

Function tnWindow.GetX : integer;
Var
   x,y : longint;
Begin
   getbegyx(win,y,x);
   GetX := x+1;
End;

Function tnWindow.GetY : integer;
Var
   x,y : longint;
Begin
   getbegyx(win,y,x);
   GetY := y+1;
End;

Function tnWindow.IsFramed : boolean;
Begin
   IsFramed := hasframe;
End;

Function tnWindow.IsVisible : boolean;
Begin
   IsVisible := visible;
End;

Function tnWindow.Edit(x,y,att,z,CursPos:Integer;es:String;Var ch : integer) : String;
var
   tmp_ec : tnec;
Begin
   { save global ec}
   tmp_ec := nEC;
   { init global ec to window ec }
   nEC := ec;
   Edit := nEdit(wn,x,y,att,z,CursPos,es,ch);
   { re-init window ec to possible changed values }
   ec.ClearMode := nEC.ClearMode;
   ec.InsMode := nEC.InsMode;
   { init global ec to saved }
   nEC := tmp_ec;
End;

Function tnWindow.Edit(x,y,att,z,CursPos:Integer;es:String;Var ch : Char) : String;
var
   i : integer;
Begin
   Edit := Edit(x,y,att,z,CursPos,es,i);
   ch := chr(abs(i));
End;

{ overload for longint }
Function tnWindow.Edit(x,y,att,z,CursPos:Integer;es:LongInt;Var ch : integer) : LongInt;
var
   tmp_ec : tnec;
Begin
   tmp_ec := nEC;
   nEC := ec;
   Edit := nEdit(wn,x,y,att,z,CursPos,es,ch);
   ec.ClearMode := nEC.ClearMode;
   ec.InsMode := nEC.InsMode;
   nEC := tmp_ec;
End;

Function tnWindow.Edit(x,y,att,z,CursPos:Integer;es:LongInt;Var ch : Char) : LongInt;
var
   i : integer;
Begin
   Edit := Edit(x,y,att,z,CursPos,es,i);
   ch := chr(abs(i));
End;

{ overload for real }
Function tnWindow.Edit(x,y,att,z,CursPos:Integer;es:Real;Var ch : integer) : Real;
var
   tmp_ec : tnec;
Begin
   tmp_ec := nEC;
   nEC := ec;
   Edit := nEdit(wn,x,y,att,z,CursPos,es,ch);
   ec.ClearMode := nEC.ClearMode;
   ec.InsMode := nEC.InsMode;
   nEC := tmp_ec;
End;

Function tnWindow.Edit(x,y,att,z,CursPos:Integer;es:Real;Var ch : Char) : Real;
var
   i : integer;
Begin
   Edit := Edit(x,y,att,z,CursPos,es,i);
   ch := chr(abs(i));
End;

Function tnWindow.EditNumber(x,y,att,wid,decm : integer;bgd : string;initv,minv,maxv : real;var esc : boolean) : real;
var
   tmp_ec : tnec;
Begin
   tmp_ec := nEC;
   nEC := ec;
   EditNumber := nEditNumber(wn,x,y,att,wid,decm,bgd,initv,minv,maxv,esc);
   ec.ClearMode := nEC.ClearMode;
   ec.InsMode := nEC.InsMode;
   nEC := tmp_ec;
End;

Function tnWindow.EditNumber(x,y,att,wid,decm : integer;bgd : string;initv,minv,maxv : longint;var esc : boolean) : longint;
var
   tmp_ec : tnec;
Begin
   tmp_ec := nEC;
   nEC := ec;
   EditNumber := nEditNumber(wn,x,y,att,wid,decm,bgd,initv,minv,maxv,esc);
   ec.ClearMode := nEC.ClearMode;
   ec.InsMode := nEC.InsMode;
   nEC := tmp_ec;
End;

Function tnWindow.EditDate(x,y,att : integer;initv : string;var esc : boolean) : string;
var
   tmp_ec : tnec;
Begin
   tmp_ec := nEC;
   nEC := ec;
   EditDate := nEditDate(wn,x,y,att,initv,esc);
   ec.ClearMode := nEC.ClearMode;
   ec.InsMode := nEC.InsMode;
   nEC := tmp_ec;
End;

{--------------------------- tnEC -------------------------------}

Constructor tnEC.Init(ft,ih,im,em,ap : boolean;
                                 s,p : string;
                                  cc : integer;
                                  mp : nChMap);
Begin
   ClearMode := ft;
   IsHidden := ih;
   InsMode := im;
   ExitMode := em;
   AppendMode := ap;
   Special := s;
   Picture := p;
   CtrlColor := cc;
   ChMap := mp;
End;

Destructor tnEC.Done;
Begin
End;

{ Add or replace a character map }
{ Preferred }
Function tnEC.AddChMap(_in,_out : integer) : integer;
Var
   i : integer;
Begin
   i := 0;
   Repeat
      inc(i);
   Until (i > nMaxChMaps) or (ChMap[i,1] = _in) or (ChMap[i,1] = 0);
   If i <= nMaxChMaps Then Begin
      AddChMap := i;
      ChMap[i,1] := _in;
      ChMap[i,2] := _out;
   End Else
      AddChMap := 0;
End;

{ Add or replace a character map }
{ Obsolete, overloaded }
Function tnEC.AddChMap(mp : nChMapStr) : integer;
Var
   i : integer;
   _in,_out : integer;
Begin
   { convert to new type }
   If mp[1] = #0 Then
      _in := ord(mp[2]) * (-1)
   Else
      _in := ord(mp[1]);
   If mp[3] = #0 Then
      _out := ord(mp[4]) * (-1)
   Else
      _out := ord(mp[3]);
   AddChMap := AddChMap(_in,_out);
End;

Procedure tnEC.ClrChMap(idx : integer);
Begin
   Case idx of
      0 : FillChar(ChMap,SizeOf(ChMap),0);
      1..nMaxChMaps : Begin
         ChMap[idx,1] := 0;
         ChMap[idx,2] := 0;
      End;
   End;
End;

{==========================================================================}

{ set the active window for write(ln), read(ln) }
Procedure nSetActiveWin(win : pwindow);
Begin
   SetActiveWn(win);
End;

{----------------------------------------------------------------
  Set the refresh toggle.
  If true, then all changes to a window are immediate. If false,
  then changes appear following the next call to nRefresh.
 ----------------------------------------------------------------}
Procedure nDoNow(donow : boolean);
Begin
   dorefresh := donow;
End;

{-----------------------------------------------------
  Set the echo flag.
  This determines whether or not, characters are
  echoed to the display when entered via the keyboard.
 -----------------------------------------------------}
Procedure nEcho(b : boolean);
Begin
   Case b of
      true : echo;
      false: noecho;
   End;
   isEcho := b;
End;

{ create a new subwindow of stdscr }
Procedure nWindow(var win : pWindow; x,y,x1,y1 : integer);
Begin
   nDelWindow(win);
   win := subwin(stdscr,y1-y+1,x1-x+1,y-1,x-1);
   If win = nil then Exit;
   intrflush(win,bool(false));
   keypad(win,bool(true));
   scrollok(win,bool(true));
   SetActiveWn(win);
End;

{ create a new window }
Procedure nNewWindow(var win : pWindow; x,y,x1,y1 : integer);
Begin
   nDelWindow(win);
   win := newwin(y1-y+1,x1-x+1,y-1,x-1);
   If win = nil then Exit;
   intrflush(win,bool(false));
   keypad(win,bool(true));
   scrollok(win,bool(true));
   SetActiveWn(win);
End;

{ repaint a window }
Procedure nRefresh(win : pWindow);
Begin
   touchwin(win);
   wrefresh(win);
End;

{----------------------------------------------
 Wait for a key to be pressed, with a timeout.
 If a key is pressed, then nKeypressed returns
 immediately as true, otherwise it return as
 false after the timeout period.
 ----------------------------------------------}
function nKeypressed(timeout : word) : boolean;
var
   fds : TFDSet;
   maxFD : longint;
Begin
   fpFD_Zero(fds);
   maxFD := 1;
   { turn on stdin bit }
   If fpFD_IsSet(STDIN,fds)=0 Then
     fpFD_Set(STDIN,fds);
   { wait for some input }
   If fpSelect(maxFD,@fds,nil,nil,timeout) > 0 Then
      nKeypressed := TRUE
   Else
      nKeypressed := FALSE;
End;

{---------------------------------
  read input string from a window
 ---------------------------------}
Function nReadln(win : pWindow) : string;
Begin
   wgetstr(win,ps);
   nReadln := StrPas(ps);
End;

{ write a string to a window without refreshing screen }
{ DON'T update PrevWn! }
Procedure nWriteScr(win : pWindow; x,y,att : integer; s : string);
Var
   tmp : pwindow;
Begin
   tmp := ActiveWn;
   tmp_b := doRefresh;
   ActiveWn := win;
   doRefresh := false;
   nFWrite(win,x,y,att,0,s);
   ActiveWn := tmp;
   doRefresh := tmp_b;
End;

{----------------------------------------------------------
  Scroll a window, up or down, a specified number of lines.
  lines = number of lines to scroll.
  dir = direction, up or down.
 ----------------------------------------------------------}
Procedure nScroll(win : pWindow; lines : integer; dir : tnUpDown);
Begin
   ScrollOk(win,bool(True));
   Case dir of
        up : lines := abs(lines);
      down : lines := abs(lines) * (-1);
   End;
   wscrl(win,lines);
   If doRefresh Then wRefresh(win);
End;

{ draw a colored box, with or without a border }
Procedure nDrawBox(win : pWindow; LineStyle,x1,y1,x2,y2,att : Integer);
Var
   sub : pWindow;
   x,y : longint;
Begin
   getbegyx(win,y,x);
   sub := subwin(win,y2-y1+1,x2-x1+1,y+y1-1,x+x1-1);
   If sub = nil Then exit;
   wbkgd(sub,CursesAtts(att));
   werase(sub);
   case LineStyle of
      1,2 : box(sub, ACS_VLINE, ACS_HLINE);
   End;
   If doRefresh Then wrefresh(sub);
   nDelWindow(sub);
End;

{---------------------------
  add a border to a window,
  waits for a refresh
 ---------------------------}
Procedure nFrame(win : pWindow);
Begin
   box(win, ACS_VLINE, ACS_HLINE);
End;

{-----------------------------------------------------------
  write a string to a window at the current cursor position
  followed by a newline
 -----------------------------------------------------------}
Procedure nWriteln(win : pWindow; s : string);
Begin
   waddstr(win,StrPCopy(ps,s+#10));
   If doRefresh Then wrefresh(win);
End;

{ return then number of rows in a window }
Function nRows(win : pWindow) : integer;
Var
   x,y : longint;
Begin
   getmaxyx(win,y,x);
   nRows := y;
End;

{ return then number of columns in a window }
Function nCols(win : pWindow) : integer;
Var
   x,y : longint;
Begin
   getmaxyx(win,y,x);
   nCols := x;
End;

{-------------------------------------------------------
 Line drawing characters have to be handled specially.
 Use nWriteAC() to write these characters. They cannot
 be simply included as characters in a string.
 -------------------------------------------------------}

{ returns horizontal line character }
Function nHL : longint;
Begin
   nHL := ACS_HLINE;
End;

{ returns vertical line character }
Function nVL : longint;
Begin
   nVL := ACS_VLINE;
End;

{ returns upper left corner character }
Function nUL : longint;
Begin
   nUL := ACS_ULCORNER;
End;

{ returns lower left corner character }
Function nLL : longint;
Begin
   nLL := ACS_LLCORNER;
End;

{ returns upper right corner character }
Function nUR : longint;
Begin
   nUR := ACS_URCORNER;
End;

{ returns lower right corner character }
Function nLR : longint;
Begin
   nLR := ACS_LRCORNER;
End;

{ returns left tee character }
Function nLT : longint;
Begin
   nLT := ACS_LTEE;
End;

{ returns right tee character }
Function nRT : longint;
Begin
   nRT := ACS_RTEE;
End;

{ returns top tee character }
Function nTT : longint;
Begin
   nTT := ACS_TTEE;
End;

{ returns bottom tee character }
Function nBT : longint;
Begin
   nBT := ACS_BTEE;
End;

{ returns plus/cross character }
Function nPL : longint;
Begin
   nPL := ACS_PLUS;
End;

{ returns left arrow character }
Function nLA : longint;
Begin
   nLA := ACS_LARROW;
End;

{ returns right arrow character }
Function nRA : longint;
Begin
   nRA := ACS_RARROW;
End;

{ returns up arrow character }
Function nUA : longint;
Begin
   nUA := ACS_UARROW;
End;

{ returns down arrow character }
Function nDA : longint;
Begin
   nDA := ACS_DARROW;
End;

{ returns diamond character }
Function nDI : longint;
Begin
   nDI := ACS_DIAMOND;
End;

{ returns checkerboard character }
Function nCB : longint;
Begin
   nCB := ACS_CKBOARD;
End;

{ returns degree character }
Function nDG : longint;
Begin
   nDG := ACS_DEGREE;
End;

{ returns plus/minus character }
Function nPM : longint;
Begin
   nPM := ACS_PLMINUS;
End;

{ returns bullet character }
Function nBL : longint;
Begin
   nBL := ACS_BULLET;
End;

{ draw a horizontal line with color and a start & end position }
Procedure nHLine(win : pwindow; col,row,attr,x : integer);
var
   sub : pwindow;
   bx,by : longint;
Begin
   getbegyx(win,by,bx);
   sub := subwin(win,1,x-col+1,by+row-1,bx+col-1);
   If sub = nil Then Exit;
   x := getmaxx(sub);
   wbkgd(sub,CursesAtts(attr));
   mvwhline(sub,0,0,ACS_HLINE,x);
   If doRefresh Then wrefresh(sub);
   delwin(sub);
End;

{ draw a vertical line with color and a start & end position }
Procedure nVLine(win : pwindow; col,row,attr,y : integer);
var sub : pwindow;
Begin
   sub := subwin(win,y-row+1,1,row-1,col-1);
   If sub = nil Then Exit;
   wbkgd(sub,CursesAtts(attr));
   mvwvline(sub,0,0,ACS_VLINE,y);
   If doRefresh Then wrefresh(sub);
   delwin(sub);
End;

{----------------------------------------------------------------
  Write a character from the alternate character set. A normal
  value from the alternate character set is larger than $400000.
  If the value passed here is 128..255, then we assume it to be
  the ordinal value from the IBM extended character set, and try
  to map it to curses correctly. If it does not map, then we just
  make it an alternate character and hope the output is what the
  programmer expected. Note: this will work on the Linux console
  just fine, but for other terminals the passed value must match
  the termcap definition for the alternate character.
  Note: The cursor returns to it's original position.
 ----------------------------------------------------------------}
Procedure nWriteAC(win : pwindow; x,y : integer; att,acs_char : longint);
var
   xx,yy,
   cp : longint;
Begin
   If acs_char in [0..255] Then Begin
      Case acs_char of
         176 : acs_char := ACS_CKBOARD;
         179 : acs_char := ACS_VLINE;
         180 : acs_char := ACS_RTEE;
         191 : acs_char := ACS_URCORNER;
         192 : acs_char := ACS_LLCORNER;
         193 : acs_char := ACS_BTEE;
         194 : acs_char := ACS_TTEE;
         195 : acs_char := ACS_LTEE;
         196 : acs_char := ACS_HLINE;
         197 : acs_char := ACS_PLUS;
         218 : acs_char := ACS_ULCORNER;
         217 : acs_char := ACS_LRCORNER;
         241 : acs_char := ACS_PLMINUS;
         248 : acs_char := ACS_DEGREE;
         249 : acs_char := ACS_BULLET;
         else  acs_char := acs_char or A_ALTCHARSET;
      End;
   End;
   { save the current cursor position }
   getyx(win,yy,xx);
   cp := nSetColorPair(att);
   { write character with current attributes }
   mvwaddch(win,y-1,x-1,acs_char);
   { update with new attributes }
   If nIsBold(att) Then
      att := A_BOLD or A_ALTCHARSET
   Else
      att := A_NORMAL or A_ALTCHARSET;
   mvwchgat(win,y-1,x-1,1,att,cp,Nil);
   { return cursor to saved position }
   wmove(win,yy,xx);
   If doRefresh Then wrefresh(win);
End;

{-------------------------------------------------------------------
 write a string to stdscr with color, without moving the cursor

   Col    = x start position
   Row    = y start position
   Attrib = color (0..127), note color = (background*16)+foreground
   Clear  = clear line up to x position
   s      = string to write
 -------------------------------------------------------------------}
Procedure nFWrite(win : pwindow; col,row,attrib : integer; clear : integer; s : string);
var
   clr : array [0..255] of char;
   cs : string;
   sub : pWindow;
   x,y,
   mx,my,
   xx,yy : longint;
   ctrl : boolean;
Begin
   if Clear > 0 Then Begin
      FillChar(clr,SizeOf(clr),' ');
      clr[SizeOf(clr)-1] := #0;
      If Clear > MaxCols Then Clear := MaxCols;
      cs := Copy(StrPas(clr),1,(Clear-Col)-Length(s)+1);
   End Else
      cs := '';
   s := s+cs;
   If s = '' Then Exit;
   getyx(win,yy,xx);
   getbegyx(win,y,x);
   getmaxyx(win,my,mx);
   If Length(s) > mx Then s := Copy(s,1,mx);
   sub := subwin(win,1,Length(s),y+row-1,x+col-1);
   If sub = nil Then Exit;
   cs := s;
   ctrl := false;
   { look for embedded control characters }
   For x := 1 to Length(s) Do Begin
      If s[x] in [#0..#31] Then Begin
         s[x] := ' ';
         ctrl := true;
      End;
   End;
   wbkgd(sub,COLOR_PAIR(nSetColorPair(Attrib)));
   If nisbold(Attrib) then
      wattr_on(sub,A_BOLD,Nil);
   mvwaddstr(sub,0,0,StrPCopy(ps,s));
   { highlight the embedded control characters substitutes }
   If ctrl Then Begin
      { nEC is always the current edit control object }
      If Attrib <> nEC.CtrlColor Then
         nWinColor(sub,nEC.CtrlColor)
      Else Begin
         { reverse the highlight color if same as current attribute }
         bg := nEC.CtrlColor div 16;
         fg := nEC.CtrlColor - (bg * 16);
         While bg > 7 Do dec(bg,8);
         While fg > 7 Do dec(fg,8);
         nWinColor(sub,(fg*16)+bg);
      End;
      For x := 1 to Length(cs) Do Begin
         If cs[x] in [#0..#31] Then
            mvwaddch(sub,0,x-1,ord(cs[x])+64);
      End;
   End;
   If doRefresh Then wrefresh(sub);
   delwin(sub);
   wmove(win,yy,xx);
End;

{ overload - no pointer }
Procedure nFWrite(col,row,attrib : integer; clear : integer; s : string);
Begin
   nFWrite(ActiveWn,col,row,attrib,clear,s);
End;

{ compatibility for the old function name }
Function nSEdit(win : pwindow; x,y,att,z,CursPos:integer;
                es:string;var ch : char) : string;
Var
   s : string;
Begin
   s := nEdit(win,x,y,att,z,CursPos,es,ch);
   nSEdit := s;
End;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
{                            String Editor                           }
Function nEdit(win : pwindow;     { window to work in }
               x,y,               { base x,y coordinates of edit region }
               att,               { color attribute }
               z,                 { right-most column of edit region }
               CursPos:integer;   { place cursor on this column at start }
               es:string;         { initial value of string }
               var chv : integer  { ordinal value of character typed, }
                                  { negative for extended keys }
               ) : string;
Var
   ZMode,
   AppendMode,
   SEditExit : boolean;
   prvx,
   prvy,
   pidx,
   pres,
   Index : integer;
   ts,
   hes : string;
   isextended : boolean;
   ch : char;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
Procedure NewString;
BEGIN
   nEdit := es;
   hes := es;
   FillChar(hes[1],Length(hes),'*');
END;

Procedure WriteString;
Var
   xx,yy : integer;
Begin
   xx := nWhereX(win);
   yy := nWhereY(win);
   If nEC.IsHidden Then
      nFWrite(win,x,y,att,z,hes)
   Else
      nFWrite(win,x,y,att,z,es);
   nGotoXY(win,xx,yy);
End;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
Procedure EInsMode;
Begin
   nEC.InsMode := (not nEC.InsMode)
End;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
Procedure WriteChar;
var s : string;
Begin
   ts := es;
   If AppendMode Then Begin
      es := es + ' ';
      Index := Length(es);
   End Else Begin
      If nWhereX(win) >= Length(es)+x Then Repeat
         es := es + ' ';
      Until Length(es)+x-1 = nWhereX(win);
      If es = '' Then es := ' ';
      If Length(es)+x-1 = nWhereX(win) Then Index := Length(es);
   End;
   es[Index] := ch;
   s := Copy(es,1,Index);
   If nCheckPxPicture(s,nEC.Picture,pidx) <> 0 Then Begin
      { no error, picture satisfied }
      If (Length(s) > Length(es)) or
         ((Length(s) = Length(es)) and (s <> es)) Then Begin
         { expanded/changed by picture }
         es := s;
      End;
      If pidx > Index Then Begin
         If pidx > Length(es) Then pidx := Length(es);
         If pidx > Index Then Index := pidx;
      End;
   End Else Begin
      { error, did not fit the picture }
      Sound(1000);
      Delay(50);
      NoSound;
      es := ts;
      Dec(Index);
   End;
   NewString;
   WriteString;
   If (Index < z-x+1) or not ZMode Then Begin
      Index := Index+1;
      nGotoXY(win,x+Index-1,y);
   End;
End;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
Procedure EInsert;            { Insert      }
Begin
   If Length(es) < Z-X+1 Then Begin
      ts := es;
      Insert(' ',es,Index);
      If nCheckPXPicture(es,nEC.Picture,pidx) = 0 Then Begin
         Sound(1000);
         Delay(50);
         NoSound;
         es := ts;
         ch := #255;
      End;
      NewString;
      WriteString;
   End;
End;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
Procedure EDelete;            { Delete      }
Begin
   ts := es;
   Delete(es,Index,1);
   If nCheckPXPicture(es,nEC.Picture,pidx) = 0 Then Begin
      Sound(1000);
      Delay(50);
      NoSound;
      es := ts;
      ch := #255;
   End;
   NewString;
   WriteString;
End;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
Procedure ECtrlEnd;           { <CTRL> End  }
Begin
   Delete(es,Index,Length(es));
   NewString;
   WriteString;
End;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
Procedure EHome;             { Home        }
Begin
   Index := 1;
   nGotoXY(win,x,y);
End;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
Procedure ELeftArrow;         { Left Arrow  }
Begin
   If nWhereX(win) > x Then Begin
      dec(Index);
      nGotoXY(win,nWhereX(win)-1,nWhereY(win));
   End;
End;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
Procedure ERightArrow;       { Right Arrow }
Begin
   If Index < z-x+1 Then Begin
      nGotoXY(win,nWhereX(win)+1,nWhereY(win));
      Index := Index + 1;
   End;
End;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
Procedure EEnd;               { End         }
Begin
   Index := Length(es)+1;
   If Index > z-x+1 Then Index := Length(es);
   If Index < 1 Then Index := 1;
   If Index > MaxCols Then Index := MaxCols;
   nGotoXY(win,x+(Index-1),y);
End;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
Procedure EBackSpace;          { Backspace  }
Begin
   Index := Index - 1;
   If Index < 1 Then Begin
      Index := 1;
      Exit;
   End Else
      If nWhereX(win) > x Then nGotoXY(win,nWhereX(win) - 1,nWhereY(win));
   Delete(es,Index,1);
   NewString;
   WriteString;
   nGotoXY(win,x+(Index-1),y);
End;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
Procedure ETurboBackSpace; { Ctrl/Backspace  }
Begin
   If Index = 1 Then Exit;
   Delete(es,1,Index-1);
   NewString;
   Index := 1;
   If nWhereX(win) > x Then nGotoXY(win,1,nWhereY(win));
   WriteString;
   nGotoXY(win,x,y);
END;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
Procedure ECtrlLeftArrow;{ Ctrl Left Arrow }
Begin
   If nEC.IsHidden Then Begin
      EHome;
      Exit;
   End;
   If es[Index-1] = ' ' Then Index := Index-1;
   If es[Index] <> ' ' Then Begin
      While (Index > 1) And (es[Index] <> ' ') Do
         Index := Index-1;
   End Else
   If es[Index] = ' ' Then Begin
      While (Index > 1) And (es[Index] = ' ') Do
         Index := Index-1;
      While (Index > 1) And (es[Index] <> ' ') Do
         Index := Index-1;
   End;
   If Index = 1 Then
      nGotoXY(win,x,y)
   Else Begin
      nGotoXY(win,x+Index,y);
      Index := Index+1;
   End;
End;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
Procedure ECtrlRightArrow;{ Ctrl Right Arrow  }
Begin
   If nEC.IsHidden Then Begin
      EEnd;
      Exit;
   End;
   While (Index < Length(es)) And (es[Index] <> ' ') Do
   Begin
        Index := Index+1;
   End;
   While (Index < Length(es)) And (es[Index] = ' ') Do
   Begin
        Index := Index+1;
   End;
   nGotoXY(win,x+Index-1,y);
End;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
Procedure CheckForWriteChar(embed : boolean);
Begin
   If embed or Not (Ch In [#27,#255]) Then Begin
      If (ch in [#10,#13]) and (not embed) {and not ControlKey} Then exit;
      If nEC.ClearMode Then Begin
         es := '';
         WriteString;
         nGotoXY(win,X,Y);
         Index := 1;
         WriteChar;
         nEC.ClearMode := False;
      End Else Begin
         If nEC.InsMode Then Begin
            EInsert;
            WriteChar;
         End Else WriteChar;
      End;
   End;
End;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
Procedure ProcessSpecialKey;
begin
   If ch = #129 Then ch := #68; { Linux, map Esc/0 to F10 }
   chv := ord(ch) * (-1);       { set the return value }

   Case ch of
   #16..#25,
   #30..#38,
   #44..#50,
   #59..#68,
   #84..#90,
  #92..#113,
       #118,
       #132,
        #72,
        #73,
        #80,
        #81 : Begin SEditExit:=True;Exit;End;
        #71 : EHome;
        #75 : ELeftArrow;
        #77 : ERightArrow;
        #79 : EEnd;
        #82 : EInsMode;
        #83 : EDelete;
        #15,
       #115 : ECtrlLeftArrow;
       #116 : ECtrlRightArrow;
       #117 : ECtrlEnd;
   End;
End;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
Procedure ProcessNormalKey;
Var
   i : integer;
   ctrl : boolean;
begin
   chv := ord(ch); { set the return value }
   For i := 1 to Length(nEC.Special) Do Begin
      If ch = nEC.Special[i] Then Begin
         SEditExit:=True;
         Exit;
      End;
   End;
   ctrl := false;
   { standard control key assignments }
   case ch of
      #0..#15,
      #17..#31 : Begin
         nEC.ClearMode := False;
         Case ch of
            #1 : EHome;
            #5 : EEnd;
            #2 : ELeftArrow;
            #6 : ERightArrow;
           #19 : ECtrlLeftArrow;
            #4 : ECtrlRightArrow;
            #7 : EDelete;
            #9 : EInsMode;
            #8 : EBackSpace;
           #10 : ch := #13;
           #13 : Begin
                    pres := nCheckPxPicture(es,nEC.Picture,pidx);
                    If pres <> 2 Then Begin
                       Sound(1000);
                       Delay(50);
                       NoSound;
                       ch := #255;
                    End;
                 End;
           #27 : If KeyPressed Then Begin
                    { covers up a Linux peculiarity where the next }
                    { character typed bleeds through with esc/1..9 }
                    nGotoXY(win,prvx,prvy);
                    WriteString;
                    ch := ReadKey;
                    { make it a function key }
                    If ch in ['1'..'9'] Then Begin
                       ch := Char(Ord(ch)+10);
                       chv := ord(ch) * (-1);
                    End Else ch := #27;
                    SEditExit := true;
                 End;
         End;
         Exit;
      End;
      #16 : Begin
               { embed control characters in the string }
               ch := UpCase(ReadKey);
               If ch in ['@','2','A'..'Z'] Then Begin
                  ctrl := true;
                  If ch = '2' Then ch := '@';
                  ch := Char(Ord(ch)-64);
                  chv := ord(ch);
               End;
            End;
     #127 : Begin nEC.ClearMode := False;ETurboBackSpace;Exit;End;
   end;
   CheckForWriteChar(ctrl);
   ch := #0;
end;

{-----------------------------------------------------------------------
  Map a keystroke to another character, normal or extended.

  The maps are 4 character strings interpreted as 2 sets of character
  pairs that represent the following:

  1st char - If it is #0 then it is an extended char. Use the 2nd
             character to identify.
  2nd char - Only used if 1st char is #0.

  The first pair of the string is the actual key pressed.
  The second pair is what that key should be become.

  #0#59 = F1, extended key
  #59#0 = ; , normal key

  So a map of #0#59#59#0 maps the F1 key to the ; key,
          and #0#59#0#60 maps the F1 key to the F2 key,
          and #0#59#0#0 maps the F1 key to a null.

   Examples:
     #0#59#0#60 = map F1 to F2
     #1#0#0#59  = map ^A to F1
     #0#59#1#0  = map F1 to ^A
     #0#59#0#0  = map F1 to ^@ (null)
     #0#0#0#59  = map ^@ to F1
     #97#0#65#0 = map a to A
}
Procedure MapKey(var ch : char;var eflag : boolean);
Var
   i,
   cv : integer;
   s2 : string[2];
   s4 : string[4];
Begin
   cv := Ord(ch);
   If eflag Then cv := cv * (-1);
   i := 0;
   { look for a character map assignment }
   Repeat
      inc(i);
   Until (i > nMaxChMaps) or (nEC.ChMap[i,1] = cv);
   { if found, then re-assign ch to the mapped key }
   If i <= nMaxChMaps Then Begin
      cv := nEC.ChMap[i,2];
      eflag := (cv < 0);
      ch := chr(abs(cv));
   End;
(*
   { look for a character map assignment }
   i := 0;
   s4 := #0#0#0#0;
   Case eflag of
      true  : s2 := #0+ch;
      false : s2 := ch+#0;
   End;
   Repeat
      inc(i);
   Until (i > nMaxChMaps) or (pos(s2,nEC.ChMap[i]) = 1);
   { if found, then re-assign ch to the mapped key }
   If i <= nMaxChMaps Then Begin
      system.Move(nEC.ChMap[i,1],s4[1],Length(nEC.ChMap[i]));
      s2 := Copy(s4,3,2);
      eflag := (s2[1] = #0);
      Case eflag of
         true  : ch := s2[2];
         false : ch := s2[1];
      End;
      If ch = #0 Then eflag := false;
   End;
*)
End;

{============================================================================}
Begin
   SEditExit := nEC.ExitMode;
   AppendMode := nEC.AppendMode;
   ZMode := z <> 0;
   If CursPos > Length(es)+x Then
      Index := Length(es)+1                { End Of String    }
   Else Index := CursPos+1-x;              { Inside Of String }
   If Not ZMode then z := x+length(es);
   Newstring;
   WriteString;
   nGotoXY(win,CursPos,y);
   Repeat
      prvx := nWhereX(win); { save for ProcessNormalKey }
      prvy := nWhereY(win);
      If Not ZMode then z := x+length(es);
      ch := ReadKey;
      isextended := (ch = #0);
      If isextended Then
         ch := ReadKey;
      MapKey(ch,isextended);
      If isextended Then
         ProcessSpecialKey
      Else
         ProcessNormalKey;
   Until (ch In [#13,#27]) or SEditExit;
   nEC.ClearMode := False;
   NewString;
End;{ of nEdit }

{ compatibility for old ch type }
Function nEdit(win : pwindow; x,y,att,z,CursPos:integer;
                es:string;var ch : char) : string;
Var i : integer;
Begin
   nEdit := nEdit(win,x,y,att,z,CursPos,es,i);
   ch := chr(abs(i));
End;

{ nEdit using currently active window }
Function nEdit(x,y,att,z,CursPos:integer;
               es:string;var ch : integer) : string;
Begin
   nEdit := nEdit(ActiveWn,x,y,att,z,CursPos,es,ch);
End;

Function nEdit(x,y,att,z,CursPos:integer;
               es:string;var ch : char) : string;
Var i : integer;
Begin
   nEdit := nEdit(ActiveWn,x,y,att,z,CursPos,es,i);
   ch := chr(ord(i));
End;

{ overload for longint type }
Function nEdit(x,y,att,z,CursPos:integer;
               es:longint;var ch : integer) : longint;
Begin
   nEdit := nEdit(ActiveWn,x,y,att,z,CursPos,es,ch);
End;

Function nEdit(x,y,att,z,CursPos:integer;
               es:longint;var ch : char) : longint;
Begin
   nEdit := nEdit(ActiveWn,x,y,att,z,CursPos,es,ch);
End;

{ longint with pointer }
Function nEdit(win : pwindow; x,y,att,z,CursPos:integer;
                es:LongInt;var ch : integer) : LongInt;
Var
   savpic,
   ess : string;
   esv,
   err : longint;
Begin
   Str(es:0,ess);
   savpic := nEC.Picture;
   If savpic = '' Then nEC.Picture := '[-]#*#';
   ess := nEdit(win,x,y,att,z,CursPos,ess,ch);
   nEC.Picture := savpic;
   val(ess,esv,err);
   nEdit := esv;
End;

Function nEdit(win : pwindow; x,y,att,z,CursPos:integer;
               es:longint;var ch : char) : longint;
Var i : integer;
Begin
   nEdit := nEdit(win,x,y,att,z,CursPos,es,i);
   ch := chr(abs(i));
End;

{ overload for real type }
Function nEdit(x,y,att,z,CursPos:integer;
               es:real;var ch : integer) : real;
Begin
   nEdit := nEdit(ActiveWn,x,y,att,z,CursPos,es,ch);
End;

Function nEdit(x,y,att,z,CursPos:integer;
               es:real;var ch : char) : real;
Var i : integer;
Begin
   nEdit := nEdit(ActiveWn,x,y,att,z,CursPos,es,i);
   ch := chr(abs(i));
End;

{ with pointer }
Function nEdit(win : pwindow; x,y,att,z,CursPos:integer;
                es:Real;var ch : integer) : Real;
Var
   savpic,
   ess : string;
   esv : real;
   i,
   err : Integer;
Begin
   Str(es:0:12,ess);
   While ess[Length(ess)] = '0' Do Delete(ess,Length(ess),1);
   savpic := nEC.Picture;
   If savpic = '' Then Begin
      Case nDecFmt of
         nUS   : nEC.Picture := '[+,-]#*#[[.*#][{E,e}[+,-]#[#][#][#]]]';
         nEURO : Begin
                    nEC.Picture := '[+,-]#*#[[;,*#][{E,e}[+,-]#[#][#][#]]]';
                    For i := 1 to Length(ess) Do
                       If ess[i] = '.' Then ess[i] := ',';
                 End;
      End;
   End;
   ess := nEdit(win,x,y,att,z,CursPos,ess,ch);
   nEC.Picture := savpic;
   For i := 1 to Length(ess) Do If ess[i] = ',' Then ess[i] := '.';
   val(ess,esv,err);
   nEdit := esv;
End;

Function nEdit(win : pwindow; x,y,att,z,CursPos:integer;
               es:real;var ch : char) : real;
Var i : integer;
Begin
   nEdit := nEdit(win,x,y,att,z,CursPos,es,i);
   ch := chr(abs(i));
End;

{ And now some sugar for Rainer Hantsch! }
{------------------------------------------------------------------------
  This is a right justified number editor. As a digit is typed, the
  existing number string gets pushed left and the new digit is appended.
  If decimal columns are specified, then pressing <space> will enter the
  decimal character (. or ,). A background string can be specified that
  fills the empty spaces.
 ------------------------------------------------------------------------}
Function nEditNumber(
       win : pwindow;
         x,              { edit field start column }
         y,              { edit field start row }
       att,              { edit field color attribute }
       wid,              { edit field width }
      decm : integer;    { number of decimal columns }
       bgd : string;     { background string -
                           if bgd = '', then no background
                           if bgd = a single character, then is used as the
                           background fill character.
                           if bgd length is longer than wid, then the entire
                           bgd string is used as the background.}
     initv,              { initial value }
      minv,              { range minimum value }
      maxv  : real;      { range maximum value }
   var esc : boolean     { if Esc key pressed = true, else = false }
) : real;

Const
   { up to 12 decimal places }
   decs : string = '[#][#][#][#][#][#][#][#][#][#][#][#]';
Var
   r : real;
   s,s1,s2 : string;
   i,
   e,
   bc,
   bx : integer;
   ch : char;
   fill : array [0..255] of char;
   tmp_ec : tnEC;
Begin
   tmp_ec := nEC;
   nEC.ExitMode := true;
   nEC.AppendMode := true;
   nEC.ClrChMap(0);
   nEC.AddChMap(#7#0#0+Char(nKeyDel));
   nEC.AddChMap(#8#0#0+Char(nKeyDel));
   If decm > (Length(decs) div 3) Then
      decm := (Length(decs) div 3);
   If decm >= wid Then decm := (wid - 1);
   If decm > 0 Then Begin
      nEC.Picture := '[-]*#[{.}'+Copy(decs,1,(decm*3))+']';
      If nDecFmt = nEURO Then Begin
         nEC.Picture[8] := ',';
         Insert(';',nEC.Picture,8);
         nEC.AddChMap('.'+#0+','+#0);
      End;
   End Else
      nEC.Picture := '[-]*#';
   If bgd = '' Then Begin
      bgd := ' ';
      bc := att;
   End Else
      bc := nEC.CtrlColor;
   If Length(bgd) < wid Then Begin
      FillChar(fill,wid,bgd[1]);
      fill[wid] := #0;
      bgd := StrPas(fill);
   End;
   bx := x;
   If Length(bgd) > wid Then inc(x);
   str(initv:wid:decm,s);
   While s[1] = ' ' Do Delete(s,1,1);
   If Pos('.',s) <> 0 Then
      While s[Length(s)] = '0' Do Delete(s,Length(s),1);
   If decm = 0 Then Delete(s,Pos('.',s),1);
   If nDecFmt = nEURO Then For i := 1 to Length(s) Do
      If s[i] = '.' Then s[i] := ',';
   Repeat
      nFWrite(win,bx,y,bc,bx+Length(bgd)-(x-bx),copy(bgd,1,wid-length(s)+(x-bx)));
      If x > bx Then
         nFWrite(win,x+wid,y,bc,0,copy(bgd,wid+2,length(bgd)));
      s1 := nEdit(win,x+wid-Length(s),y,att,x+wid-1,x+wid-1,s,ch);
      s2 := s1;
      If nDecFmt = nEURO Then For i := 1 to Length(s2) Do
         If s2[i] = ',' Then s2[i] := '.';
      val(s2,r,e);
      If (s1 = '') or ((e = 0) and (r >= minv) and (r <= maxv)) Then
         s := s1
      Else
         If ch <> #27 then Begin
            ch := #0;
            Sound(1000);
            Delay(50);
            NoSound;
         End;
      nEC.AppendMode := Length(s) < wid;
   Until ch in [#13,#27];
   esc := (ch = #27);
   nEditNumber := r;
   nEC := tmp_ec;
End;

{ overload - real, no pointer }
Function nEditNumber(
   x,y,att,wid,decm : integer;
                bgd : string;
              initv,
               minv,
               maxv : real;
            var esc : boolean) : real;
Begin
   nEditNumber := nEditNumber(ActiveWn,x,y,att,wid,decm,bgd,initv,minv,maxv,esc);
End;

{ overload for longint }
Function nEditNumber(
                win : pwindow;
   x,y,att,wid,decm : integer;
                bgd : string;
              initv,
               minv,
               maxv : longint;
            var esc : boolean) : longint;
Var
   r : real;
Begin
   r := nEditNumber(win,x,y,att,wid,0,bgd,Real(initv),Real(minv),Real(maxv),esc);
   nEditNumber := Trunc(r);
End;

{ overload - longint, no pointer }
Function nEditNumber(
   x,y,att,wid,decm : integer;
                bgd : string;
              initv,
               minv,
               maxv : longint;
            var esc : boolean) : longint;
Var
   r : real;
Begin
   r := nEditNumber(ActiveWn,x,y,att,wid,0,bgd,Real(initv),Real(minv),Real(maxv),esc);
   nEditNumber := Trunc(r);
End;

{ More sugar for Rainer }
{------------------------------------------------------------------------
  A date string editor.
 ------------------------------------------------------------------------}
Function nEditDate(
       win : pwindow;
         x,           { edit field start column }
         y,           { edit field start row }
       att : integer; { edit field color attribute }
     initv : string;     { initial value }
   var esc : boolean     { if Esc key pressed = true, else = false }
) : string;

Var
   s : string;
   i : integer;
   ch : char;
   tmp_ec : tnEC;

Begin
   tmp_ec := nEC;
   nEC.InsMode := false;
   nEC.ClearMode := false;
   nEC.ExitMode := false;
   nEC.AppendMode := false;
   Case nDecFmt of
      nUS :  Begin
         nEC.Picture := '{#,m,M}{#,m,M}/{#,d,D}{#,d,D}/{#,y,Y}{#,y,Y}{#,y,Y}{#,y,Y}';
         s := 'mm/dd/yyyy';
      End;
      nEURO : Begin
         nEC.Picture := '{#,d,D}{#,d,D}/{#,m,M}{#,m,M}/{#,y,Y}{#,y,Y}{#,y,Y}{#,y,Y}';
         s := 'dd/mm/yyyy';
      End;
   End;
   If nCheckPxPicture(initv,nEC.Picture,i) <> 0 Then
      system.move(initv[1],s[1],Length(initv));
   nEC.AddChMap(#7#0#0+Char(nKeyLeft));
   nEC.AddChMap(#8#0#0+Char(nKeyLeft));
   nEC.AddChMap(#0+Char(nKeyDel)+#0+Char(nKeyLeft));
   Repeat
      s := nEdit(win,x,y,att,x+9,x,s,ch);
      If ch = #13 Then Begin
         For i := 1 to Length(s) Do
            If s[i] in ['m','d','y'] Then ch := #0;
      End;
   Until ch in [#13,#27];
   esc := (ch = #27);
   nEditDate := s;
   nEC := tmp_ec;
End;

{ overload - no pointer }
Function nEditDate(x,y,att : integer;initv : string;var esc : boolean) : string;
Begin
   nEditDate := nEditDate(ActiveWn,x,y,att,initv,esc);
End;

{ A one-line procedural wrapper }
Procedure nMakeWindow(
    var win : tnWindow;
    x1,y1,
    x2,y2,
    ta,ba,ha : integer;
    hasframe : boolean;
    hdrpos : tnJustify;
    hdrtxt : string);
Begin
   win.init(x1,y1,x2,y2,ta,hasframe,ba);
   If hdrtxt <> '' Then win.PutHeader(hdrtxt,ha,hdrpos);
End;

{ And with a window pointer }
Procedure nMakeWindow(
    var win : pnWindow;
    x1,y1,
    x2,y2,
    ta,ba,ha : integer;
    hasframe : boolean;
    hdrpos : tnJustify;
    hdrtxt : string);
Begin
   New(win,init(x1,y1,x2,y2,ta,hasframe,ba));
   If hdrtxt <> '' Then win^.PutHeader(hdrtxt,ha,hdrpos);
End;

{--------------------------------------------------------------------
  Display a message in a centered and framed box. With ack set to
  false, the window remains active for further use in the program.

  Inputs:
     msg = message to display
     matt = message color
     hdr  = header text at frame top
     hatt = header/frame color
     ack  = TRUE : display ftr text and wait for a keypress, then
                   remove the window.
            FALSE: don't display ftr, don't wait for a keypress, and
                   don't remove the window.
  Output:
     a nil pointer if ack = true,
     a pointer to the tnWindow object if ack = false
 --------------------------------------------------------------------}
Function nShowMessage(msg : string;
                     matt : byte;
                      hdr : string;
                     hatt : byte;
                      ack : boolean) : pnWindow;
const
   ftr = 'Press Any Key';
   acklns : shortint = 0;
var
   i,j,
   cr,
   wid,
   maxwid,
   lines : integer;
   mwin : pnWindow;
Begin
   wid := 0;
   maxwid := Length(hdr);
   If ack and (Length(ftr) > maxwid) Then
      maxwid := Length(ftr);
   lines := 1;
   { how many rows does this window need ? }
   For i := 1 to Length(msg) Do Begin
      inc(wid);
      { let's be consistant! }
      If msg[i] = #13 Then msg[i] := #10;
      { either a forced line break or we need to word-wrap }
      If (msg[i] = #10) or (wid >= (MaxCols-2)) Then Begin
         inc(lines);
         j := 0;
         If not (msg[i] in [#10,#32]) Then Begin
            { we're in a word, so find the previous space (if any) }
            Repeat
               inc(j);
            Until (j=wid) or ((i-j) <= 0) or (msg[i-j] = #32);
            If ((i-j) > 0) and (msg[i-j] = #32) Then Begin
               wid := wid-j;
               msg[i-j] := #10  { force a line break }
            End Else
               j := 0;
         End;
         If wid > maxwid Then maxwid := wid;
         wid := j; { either 0 or word-wrap remnent }
      End;
   End;
   If wid > maxwid Then maxwid := wid;
   If ack Then acklns := 1 else acklns := 0;
   { make the message window }
   New(mwin,Init(1,1,maxwid+2,lines+acklns+2,matt,true,hatt));
   With mwin^ Do Begin
      PutHeader(hdr,hatt,center);
      Align(center,center);
      If lines = 1 Then
         { one-liners get centered }
         Write(msg:Length(msg)+((maxwid-Length(msg)) div 2))
      Else
         Write(msg);
      Show;
      If ack Then Begin
         cr := nCursor(cOff);
         FWrite(((cols-Length(ftr)) div 2)+1,rows,matt,0,ftr);
{
  The following line can be used in place of the line above to place the
  footer text in the frame instead of with the message body. Make sure to
  keep acklns=0.

         nFWrite(win,((ncols(win)-Length(ftr)) div 2)+1,nrows(win),hatt,0,ftr);
}
         Readkey;
         While Keypressed Do Readkey;
         Hide;
         nCursor(cr);
      End;
   End;
   If ack Then Begin
      Dispose(mwin,Done);
      mwin := nil;
   End;
   nShowMessage := mwin;
End;

{---------------------------------------
  Read a character string from a window
  win - window to extract info from.
    x - starting column.
    y - starting row.
    n - number of characters to read.
 ---------------------------------------}
Function nReadScr(win : pWindow; x,y,n : integer) : string;
Var
   i,idx : integer;
   s : string;
   c : longint;
   { array of char/attr values, 4 bytes each, max 256 }
   buf : array[0..1023] of char;
   p : pchtype;
Begin
   s := '';
   p := nReadScrStr(win,x,y,n,@buf);
   If p <> nil Then Begin
      idx := 0;
      For i := 1 to n Do Begin
         system.move(buf[idx],c,SizeOf(c));
         s := s + chr(c and A_CHARTEXT);
         inc(idx,SizeOf(c));
      End;
   End;
   nReadScr := s;
End;

{ overload for current window }
Function nReadScr(x,y,n : integer) : string;
Begin
   nReadScr := nReadScr(ActiveWn,x,y,n);
End;

Function nReadScrStr(win : pWindow; x,y,n : integer; buf : pchtype) : pchtype;
Var
   cx,cy : integer;
   mx,my : longint;
Begin
   cx := nWhereX(win);
   cy := nWhereY(win);
   If win <> nil Then Begin
      getmaxyx(win,my,mx);
      If (x in [1..mx]) and (y in [1..my]) Then Begin
         { n is contrained to the right margin, so no need to range check }
         mvwinchnstr(win,y-1,x-1,buf,n);
         nGotoXY(win,cx,cy);
      End;
   End;
   nReadScrStr := buf;
End;

{ overload for current window }
Function nReadScrStr(x,y,n : integer; buf : pchtype) : pchtype;
Begin
   nReadScrStr := nReadScrStr(ActiveWn,x,y,n,buf);
End;

Function nReadScrColor(win : pWindow; x,y : integer) : integer;
Var
   cl,
   fg,bg,
   cx,cy : integer;
   c,cv,
   mx,my : longint;
Begin
   cl := -1;
   cx := nWhereX(win);
   cy := nWhereY(win);
   If win <> nil Then Begin
      getmaxyx(win,my,mx);
      If (x in [1..mx]) and (y in [1..my]) Then Begin
         c := mvwinch(win,y-1,x-1);
         nGotoXY(win,cx,cy);
         cv := PAIR_NUMBER(c and A_COLOR);
         pair_content(cv,@fg,@bg);
         fg := c2ibm(fg);
         bg := c2ibm(bg);
         cv := (c and A_ATTRIBUTES);
         If A_BOLD and cv = A_BOLD Then inc(fg,8);
         cl := (bg*16)+fg;
      End;
   End;
   nReadScrColor := cl;
End;

{ overload for current window }
Function nReadScrColor(x,y : integer) : integer;
Begin
   nReadScrColor := nReadScrColor(ActiveWn,x,y);
End;

{ write a string with attributes, previously saved with nReadScrStr }
Procedure nWriteScrStr(win : pWindow; x,y : integer; s : pchtype);
Begin
   mvwaddchstr(win,y-1,x-1,s);
   If doRefresh Then wrefresh(win);
End;

{ overload for current window }
Procedure nWriteScrStr(x,y : integer; s : pchtype);
Begin
   mvwaddchstr(ActiveWn,y-1,x-1,s);
   If doRefresh Then wrefresh(ActiveWn);
End;

{---------------------------------------
 save a rectangular portion of a window
   x = start column
   y = start row
   c = number of columns
   r = number of rows
 ---------------------------------------}
Procedure nGrabScreen(var p : pnScreenBuf; x,y,c,r : integer; win : pWindow);
Var
   mx,my : longint;
   i,
   cx,cy : integer;
   prb,trb : pnRowBuf;
Begin
   nReleaseScreen(p);
   getmaxyx(win,my,mx);
   If not (x in [1..mx]) or Not (y in [1..my]) Then Begin
      p := nil;
      Exit;
   End;
   cx := nWhereX(win);
   cy := nWhereY(win);
   New(p);
   p^.x := x;
   p^.y := y;
   p^.n := c;
   p^.first := nil;
   trb := nil;
   For i := 0 to r-1 Do Begin
      If (y+i in [1..my]) Then Begin
         New(prb);
         GetMem(prb^.row,c*SizeOf(chtype));
         mvwinchnstr(win,y-1+i,x-1,prb^.row,c);
         If trb <> nil Then trb^.Next := prb;
         prb^.next := nil;
         trb := prb;
         If i = 0 Then p^.First := prb;
      End;
   End;
   nGotoXY(win,cx,cy);
End;

{ overload for current window }
Procedure nGrabScreen(var p : pnScreenBuf; x,y,c,r : integer);
Begin
   nGrabScreen(p,x,y,c,r,ActiveWn);
End;

{ overload for current full window }
Procedure nGrabScreen(var p : pnScreenBuf);
Var
   c,r : longint;
Begin
   getmaxyx(ActiveWn,r,c);
   nGrabScreen(p,1,1,c,r,ActiveWn);
End;

{-----------------------------------------
 restore a window saved with nGrabScreen
   p = pointer to the saved buffer
   x = start restore to this column
   y = start restore to this row
   win = restore to this window
 -----------------------------------------}
Procedure nPopScreen(p : pnScreenBuf; x,y : integer; win : pWindow);
Var
   cx,cy : integer;
   mx,my : longint;
   pb : pnRowBuf;
Begin
   If p = nil Then Exit;
   getmaxyx(win,my,mx);
   If Not (x in [1..mx]) or Not (y in [1..my]) Then Exit;
   dec(x);
   cx := nWhereX(win);
   cy := nWhereY(win);
   pb := p^.First;
   While pb <> nil Do Begin
      If (pb^.row <> nil) and (y in [1..my]) Then
         mvwaddchnstr(win,y-1,x,pb^.row,p^.n);
      inc(y);
      pb := pb^.next;
   End;
   nGotoXY(win,cx,cy);
   If doRefresh Then wrefresh(win);
End;

{ overload for current window, defined position }
Procedure nPopScreen(p : pnScreenBuf; x,y : integer);
Begin
   nPopScreen(p,x,y,ActiveWn);
End;

{ overload for current window, saved position }
Procedure nPopScreen(p : pnScreenBuf);
Begin
   If p = nil Then Exit;
   nPopScreen(p,p^.x,p^.y,ActiveWn);
End;

{ free up the memory used to store a grabbed screen }
Procedure nReleaseScreen(p : pnScreenBuf);
Var
   cur,tmp : pnRowBuf;
Begin
   If p = nil Then Exit;
   If p^.first <> nil Then Begin
      cur := p^.first;
      While cur <> nil Do Begin
         tmp := cur^.next;
         If cur^.row <> nil Then FreeMem(cur^.row,p^.n * SizeOf(chtype));
         Dispose(cur);
         cur := tmp;
      End;
   End;
   Dispose(p);
End;

{============================== tnMenu ====================================}

{ A one-line procedural wrapper }
Procedure nMakeMenu(
    var mnu : tnMenu;
    x,y,
    _w,_r,_c,
    ta,ca,ga,ba,ha : integer;
    hasframe : boolean;
    hdrpos : tnJustify;
    hdrtxt : string);
Begin
   mnu.init(x,y,_w,_r,_c,ta,ca,ga,hasframe,ba);
   If hdrtxt <> '' Then mnu.PutHeader(hdrtxt,ha,hdrpos);
End;

{ And with a menu pointer }
Procedure nMakeMenu(
    var mnu : pnMenu;
    x,y,
    _w,_r,_c,
    ta,ca,ga,ba,ha : integer;
    hasframe : boolean;
    hdrpos : tnJustify;
    hdrtxt : string);
Begin
   New(mnu,init(x,y,_w,_r,_c,ta,ca,ga,hasframe,ba));
   If hdrtxt <> '' Then mnu^.PutHeader(hdrtxt,ha,hdrpos);
End;

Constructor tnMenu.Init(_x,_y,_w,_r,_c,_tc,_cc,_gc : integer;
                         _fr : boolean; _fc : integer);
Begin
   x := _x;
   y := _y;
   wid := _w;
   r := _r;
   c := _c;
   tc := _tc;
   cc := _cc;
   gc := _gc;
   framed := _fr;
   fc := _fc;
   hc := fc;
   iidx := 0;
   mark := '';
   posted := false;
   If wid > MaxCols Then wid := MaxCols;
   InitWin;
   Spin(false);
End;

Destructor tnMenu.Done;
Begin
   UnPost;
   Clear;
   Dispose(win,Done);
End;

Procedure tnMenu.InitWin;
Const
   xhgt : shortint = 0;
Begin
   If framed Then xhgt := 2 Else xhgt := 0;
   New(win,Init(x,y,(x+wid-1),(y+r+xhgt-1),tc,framed,fc));
End;

Procedure tnMenu.Post;
Var
   bx,by,
   mx,my : longint;
   p : pchar;
   a : array[0..SizeOf(tnS10)-1] of char;
Begin
   { could already be posted }
   UnPost;
   { see if the window size has changed (a new longer item added?) }
   getmaxyx(win^.win,my,mx);
   If (wid <> mx) Then Begin
      getbegyx(win^.win,by,bx);
      Dispose(win,Done);
      x := bx+1;
      y := by+1;
      InitWin;
   End;
   { create the new menu }
   pm := new_menu(@pi);
   { only show item text }
   menu_opts_off(pm,O_SHOWDESC);
   { bind the windows }
   set_menu_win(pm,win^.win);
   set_menu_sub(pm,win^.wn);
   { set the rows and columns }
   set_menu_format(pm,r,c);
   { set the colors }
   set_menu_fore(pm,CursesAtts(cc));
   set_menu_back(pm,CursesAtts(tc));
   set_menu_grey(pm,CursesAtts(gc));
   p := StrPCopy(a,mark);
   set_menu_mark(pm,p);
   merr := post_menu(pm);
   posted := (merr = E_OK);
   Spin(loopon);
End;

Procedure tnMenu.UnPost;
Begin
   merr := unpost_menu(pm);
   merr := free_menu(pm);
   pm := nil;
   posted := false;
End;

Procedure tnMenu.Show;
Begin
   If not posted Then Post;
   win^.Show;
End;

{ Start user interaction loop }
Procedure tnMenu.Start;
Const
   select = #13;
   cancel = #27;
Var
   key : char;
   i,cnt,
   prev,
   savecurs,
   xkey : integer;
   direction : longint;
Begin
   Show;
   iidx := 0;
   savecurs := nCursor(cOFF);
   Repeat
      prev := iidx;
      win^.Show;
      key := readkey;
      xkey := 0;
      case key of
         #0 : xkey := ord(readkey);
         ^F : xkey := nKeyHome;
         ^L : xkey := nKeyEnd;
         #9,
         ^N : xkey := nKeyDown;
         ^P : xkey := nKeyUp;
         else menu_driver(pm,ord(key));
      end;
      case xkey of
         nKeyHome : menu_driver(pm,REQ_FIRST_ITEM);
         nKeyEnd  : menu_driver(pm,REQ_LAST_ITEM);
         nKeyRight,
         nKeyDown : menu_driver(pm,REQ_NEXT_ITEM);
         nKeyLeft,
         nKeyUp   : menu_driver(pm,REQ_PREV_ITEM);
      end;
      iidx := item_index(current_item(pm)) + 1;
      If (not Selectable(iidx)) and (key <> cancel) Then Begin
         cnt := Count;
         If cnt > 1 Then Begin
            { temporarily enable spinning }
            If not loopon Then
               menu_opts_off(pm,O_NONCYCLIC);
            { which way to another item? }
            If iidx > prev Then
               direction := REQ_NEXT_ITEM
            Else
               direction := REQ_PREV_ITEM;
            Repeat
               menu_driver(pm,direction);
               i := item_index(current_item(pm)) + 1;
            Until Selectable(i) or (i = iidx);
            { reset spin }
            Spin(loopon);
            { keep prev honest }
            iidx := item_index(current_item(pm)) + 1;
         End;
      End;
   Until key in [select,cancel];
   menu_driver(pm,REQ_CLEAR_PATTERN);
   If iidx = ERR Then merr := iidx;
   If key = cancel Then iidx := 0;
   nCursor(savecurs);
End;

Procedure tnMenu.Stop;
Begin
   Hide;
   UnPost;
End;

Procedure tnMenu.Hide;
Begin
   win^.Hide;
End;

Function tnMenu.Wind : pnWindow;
Begin
   Wind := win;
End;

Procedure tnMenu.Align(hpos,vpos : tnJustify);
Begin
   win^.Align(hpos,vpos);
End;

Procedure tnMenu.Move(_x,_y : integer);
Begin
   win^.Move(_x,_y);
End;

Procedure tnMenu.PutHeader(hdr : string; hcolor : integer; hpos : tnJustify);
Begin
   win^.PutHeader(hdr,hcolor,hpos);
End;

Procedure tnMenu.Clear;
Var
   i : integer;
Begin
   UnPost;
   For i := 1 to nMAXMENUITEMS Do ClearItem(i);
End;

{ is this menu item selectable }
Function tnMenu.Selectable(idx : integer) : boolean;
Begin
   Selectable := IsAssigned(idx) and
   ((O_SELECTABLE and item_opts(pi[idx])) = O_SELECTABLE);
End;

Function tnMenu.IsValid(idx : integer) : boolean;
Begin
   IsValid := ((idx >= 1) and (idx <= nMAXMENUITEMS));
End;

Function tnMenu.IsAssigned(idx : integer) : boolean;
Begin
   IsAssigned := IsValid(idx) and (pi[idx] <> nil);
End;

Procedure tnMenu.ClearItem(idx : integer);
Begin
   If IsValid(idx) Then Begin
      If items[idx] <> nil Then Begin
         merr := free_item(pi[idx]);
         If merr = E_OK Then Begin
            FreeMem(items[idx],StrLen(items[idx]^)+1);
            pi[idx] := nil;
            items[idx] := nil;
         End;
      End;
   End Else merr := E_BAD_ARGUMENT;
End;

Procedure tnMenu.AddItem(i : integer; s : string);
Const
   fwid : shortint = 0;
   iwid : shortint = 1;
Var
   rl : integer;
   sp1,sp2,sp3 : plongint;
Begin
   If IsValid(i) Then Begin
      sp1:=nil; sp2:=nil; sp3:=nil;
      ClearItem(i);
      GetMem(items[i],Length(s)+1);
      StrPCopy(items[i]^,s);
      pi[i] := new_item(pchar(items[i]),nil);
      If pi[i] <> Nil Then Begin
         merr := E_OK;
         { Expand the window width if necessary. Limit to screen width.
           Add possibly 2 for the frame, the item indicator length, and
           the item spacing value. }
         If framed Then fwid := 2;
         if c > 1 Then Begin
            If posted Then Begin
               { need a valid pm }
               menu_spacing(pm,sp1,sp2,sp3);
               iwid := Length(GetMark) + sp3^;
            End Else
               iwid := Length(GetMark) + 1;
         End Else
            iwid := 0;
         { required length }
         rl := ((Length(s)+iwid)*c)+fwid;
         { expand? }
         If rl > wid Then wid := rl;
         If wid > MaxCols Then wid := MaxCols;
      End Else merr := E_REQUEST_DENIED;
   End Else merr := E_BAD_ARGUMENT;
End;

Function tnMenu.Add(s : string) : integer;
Var
   i : integer;
Begin
   i := 0;
   Add := 0;
   Repeat
      inc(i);
   Until (i > nMAXMENUITEMS) or (items[i] = nil);
   AddItem(i,s);
   If merr = E_OK Then Add := i;
End;

Procedure tnMenu.Insert(idx : integer; s : string);
Begin
   If IsValid(idx) Then Begin
      ClearItem(nMAXMENUITEMS);
      If idx < nMAXMENUITEMS Then Begin
         { shift the pointer list up and keep lists syncronized }
         system.Move(pi[idx],pi[idx+1],SizeOf(pnMenuStr)*(nMAXMENUITEMS-idx));
         system.Move(items[idx],items[idx+1],SizeOf(pItem)*(nMAXMENUITEMS-idx));
         pi[idx] := nil;
         items[idx] := nil;
      End;
      AddItem(idx,s);
   End Else merr := E_BAD_ARGUMENT;
End;

Procedure tnMenu.Remove(idx : integer);
Begin
   If IsValid(idx) Then Begin
      ClearItem(idx);
      { shift the pointer list down and keep lists syncronized }
      system.Move(pi[idx+1],pi[idx],SizeOf(pnMenuStr)*(nMAXMENUITEMS-idx));
      system.Move(items[idx+1],items[idx],SizeOf(pItem)*(nMAXMENUITEMS-idx));
      pi[nMAXMENUITEMS] := nil;
      items[nMAXMENUITEMS] := nil;
   End Else merr := E_BAD_ARGUMENT;
End;

Procedure tnMenu.Change(idx : integer; s : string);
Begin
   AddItem(idx,s);
End;

{ toggle a menu item's selectability }
Procedure tnMenu.Active(idx : integer; b : boolean);
Begin
   Case b of
      true : item_opts_on(pi[idx],O_SELECTABLE);
      false : item_opts_off(pi[idx],O_SELECTABLE);
   End;
End;

{ is the item selectable? }
Function tnMenu.IsActive(idx : integer) : boolean;
Begin
   IsActive := Selectable(idx);
End;

{ Toggle item looping. Moves to first/last when bottom/top is reached }
Procedure tnMenu.Spin(b : boolean);
Begin
   loopon := b;
   If posted Then
    Case b of
       true : menu_opts_off(pm,O_NONCYCLIC);
       false : menu_opts_on(pm,O_NONCYCLIC);
    End;
End;

{ return most recent error status }
Function tnMenu.Status : integer;
Begin
   Status := merr;
End;

Function tnMenu.Index : integer;
Begin
   Index := iidx;
End;

Procedure tnMenu.SetIndex(idx : integer);
Begin
   If IsValid(idx) and IsAssigned(idx) and Selectable(idx) Then Begin
      set_current_item(pm,pi[idx]);
      iidx := idx;
   End;
End;

Function tnMenu.Count : integer;
Begin
   Count := item_count(pm);
End;

Function tnMenu.Rows(_r : integer) : integer;
Begin
   Rows := r;
   If _r > 0 Then r := _r;
End;

Function tnMenu.Cols(_c : integer) : integer;
Begin
   Cols := c;
   If _c > 0 Then c := _c;
End;

{ get the item indicator prefix string }
Function tnMenu.GetMark : string;
Begin
   If posted Then
      GetMark := StrPas(menu_mark(pm))
   Else
      GetMark := mark;
End;

{ set the item indicator prefix string }
Procedure tnMenu.SetMark(ms : string);
Begin
   mark := ms;
End;

Procedure tnMenu.Refresh;
Begin
   Post;
   Show;
End;

Procedure tnMenu.SetColor(att : byte);
Begin
   tc := att;
   If posted Then set_menu_back(pm,CursesAtts(tc));
End;

Procedure tnMenu.SetCursorColor(att : byte);
Begin
   cc := att;
   If posted Then set_menu_fore(pm,CursesAtts(cc));
End;

Procedure tnMenu.SetFrameColor(att : byte);
Begin
   fc := att;
   If posted Then Wind^.PutFrame(att);
End;

Procedure tnMenu.SetGrayColor(att : byte);
Begin
   gc := att;
   If posted Then set_menu_grey(pm,CursesAtts(gc));
End;

{----------------------- initialize the unit!------------------------- }
procedure FOS_Init_OCRT;
Begin
   FillChar(_chmap,SizeOf(_chmap),0);
   nEC.Init(false,false,false,false,false,'','',15,_chmap);
   { load the color pairs array with color pair indices (0..63) }
   For bg := 0 to 7 Do For fg := 0 to 7 do cp[bg,fg] := (bg*8)+fg;
   { initialize ncurses }
   If StartCurses(ActiveWn) Then Begin
      { save pointer to ncurses stdscr }
      nscreen := ActiveWn;
      { defaults, crtassign, etc. }
      nInit;
      { create the default full screen, non-bordered window object }
      nStdScr.Init(1,1,MaxCols,MaxRows,7,false,0);
      { default read/write to stdscr }
      ActiveWn := nscreen;
   End Else Begin
      CursesFailed;
   End;
End; { of Unit oCrt }

end.
