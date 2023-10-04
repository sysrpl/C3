(********************************************************)
(*                                                      *)
(*  Codebot Cloud Client                                *)
(*  http://www.getlazarus.org/apps/s3                   *)
(*  Modified October 2023                               *)
(*                                                      *)
(*  Released under GNU GPLv3 license                    *)
(*                                                      *)
(********************************************************)

unit DialogTools;

{$i c3.inc}

interface

uses
  Classes, SysUtils, Graphics, Controls, Forms, Dialogs, ExtCtrls,
  Codebot.Graphics.Types;

type
  TShowDialogProc = procedure of object;

procedure ShowDialog(Proc: TShowDialogProc);
procedure Undecorate(Window: TCustomForm);

implementation

const
  GtkLib = 'gtk-x11-2.0';

procedure gtk_window_set_decorated(Handle: THandle; Value: Integer); cdecl; external GtkLib;

procedure Undecorate(Window: TCustomForm);
begin
  gtk_window_set_decorated(Window.Handle, 0);
end;

procedure SetTrueBounds(Parent, Child: TCustomForm);
const
  Edge = 10;
var
  A, B: TPoint;
  W, H: Integer;
  R: TRectI;
begin
  Undecorate(Child);
  A.X := Parent.Left;
  A.Y := Parent.Top;
  B.X := 0;
  B.Y := 0;
  B := Parent.ClientToScreen(B);
  W := (B.X - A.X) * 2 + Parent.ClientWidth;
  H := (B.Y - A.Y) + (B.X - A.X) + Parent.ClientHeight;
  R.X := A.X;
  R.Y := A.Y;
  R.Width := W;
  R.Height := H;
  R.Inflate(Edge, Edge);
  Child.BoundsRect := R;
end;

{ TShadowWindow }

type
  TShadowWindow = class(TForm)
  private
    FTimer: TTimer;
    FProc: TShowDialogProc;
    procedure TimerExpired(Sender: TObject);
    procedure Shadow(Sender: TObject);
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
  end;

constructor TShadowWindow.CreateNew(AOwner: TComponent; Num: Integer = 0);
begin
  inherited CreateNew(AOwner, Num);
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 100;
  FTimer.OnTimer  := TimerExpired;
  OnPaint := Shadow;
  AlphaBlend := True;
  AlphaBlendValue := 128;
  BoundsRect := Application.MainForm.BoundsRect;
  Color := clBlack;
  ShowInTaskBar := stNever;
end;

procedure TShadowWindow.Shadow(Sender: TObject);
begin
  OnPaint := nil;
  FTimer.Enabled := True;
  SetTrueBounds(Application.MainForm, Self);
end;

procedure TShadowWindow.TimerExpired(Sender: TObject);
begin
  FTimer.Enabled := False;
  FProc;
  ModalResult := mrOK;
  Release;
end;

{ ShowDialog }

procedure ShowDialog(Proc: TShowDialogProc);
var
  Shadow: TShadowWindow;
begin
  Shadow := TShadowWindow.CreateNew(Application);
  Shadow.FProc := Proc;
  Shadow.ShowModal;
end;

end.

