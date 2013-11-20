unit fos_ncurses_core;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,fos_ncurses;

type

  { TFOS_NCURSES_SCREEN }

  TFOS_NCURSES_SCREEN=class(Tobject)
  public
    procedure Test;
  end;

var
  GFOS_CONSOLE : TFOS_NCURSES_SCREEN;

procedure FOS_Init_Console;

implementation

procedure FOS_Init_Console;
begin
  GFOS_CONSOLE := TFOS_NCURSES_SCREEN.Create;
end;

procedure FOS_Finalize_Console;
begin
  GFOS_CONSOLE.free;
end;

{ TFOS_NCURSES_SCREEN }

procedure TFOS_NCURSES_SCREEN.Test;
var ch : longint;
begin
  initscr;
  keypad(stdscr, TRUE);
  noecho;
  start_color;
  refresh;
  init_pair(1, COLOR_YELLOW, COLOR_BLUE);
  wbkgd(stdscr,COLOR_PAIR(1));
  addch(ACS_ULCORNER);
  Refresh;
  //init_pair(1, COLOR_RED, COLOR_BLACK);
  attron(COLOR_PAIR(1));
  printw('Viola !!! In color ...');
  attroff(COLOR_PAIR(1));
  printw('Hello World !!!');
  refresh;
  ch := getch;
  if ch=KEY_F(1) then
    Printw('F1 Presssed')
  else
    printw('%c',ch);
  getch;
  endwin;
  exit;
end;


initialization

end.

