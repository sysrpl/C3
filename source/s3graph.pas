(********************************************************)
(*                                                      *)
(*  Codebot Simple Storage                              *)
(*  http://www.getlazarus.org/apps/s3                   *)
(*  Modified October 2023                               *)
(*                                                      *)
(*  Released under GNU GPLv3 license                    *)
(*                                                      *)
(********************************************************)

unit S3Graph;

{$i c3.inc}

interface

uses
  Classes, SysUtils, Graphics,
  S3Objects,
  Codebot.System,
  Codebot.Collections,
  Codebot.Text.Xml,
  Codebot.Graphics,
  Codebot.Graphics.Types;

const
  iconBucket = '󱐗';
  iconBucketSelect = '󱐖';
  iconFolderPlus = '󰜄';
  iconFolderMinus = '󰛲';
  iconFolderRight = '󰍟';
  iconFolderDown = '󰍝';
  iconFolderClosed = '󰉋';
  iconFolderOpened = '󰝰';
  iconFolderSelect = '󰉖';
  iconRefresh = '󱍷';
  iconContent = '󰈔';
  iconContentSelect = '󰈤';
  iconImage = '󰈟';
  iconText = '󰈙';
  iconVideo = '󰈫';
  iconBusy = '󰦖';
  iconSuccess = '󰆀';
  iconFail = '󰅾';
  iconCanceled = '󰆇';
  iconUnchecked = '󰄱';
  iconChecked = '󰄵';
  iconUncheckedPressed = '󰄮';
  iconCheckedPressed = '󰄲';


  colorBucket: TColor = $E5AA1E;
  colorBusy: TColor = $28CAFF;
  colorSuccess: TColor = $4AC38B;
  colorFail: TColor = $3539E5;
  colorCanceled: TColor = $007CF5;
  colorFolder: TColor = $00B9FF;
  colorContent: TColor = $9AA626;

{ TS3Renderer }

type
  TS3Renderer = class
  private
    FFont: IFont;
    FIcon: IFont;
    FSmallIcon: IFont;
  public
    constructor Create(Font: TFont);
    procedure ObjectsButtonCalc(Obj: IStorageObject; Index: Integer;
      Rect: TRectI; var Buttons: TButtonRects);
    procedure DrawBucket(Surface: ISurface; Bucket: IBucket; Rect: TRectI;
      State: TDrawState);
    procedure DrawObjectEmpty(Surface: ISurface; Rect: TRectI; Index: Integer);
    procedure DrawObject(Surface: ISurface; Obj: IStorageObject; Rect: TRectI;
      ColWidths: IntArray;  State: TDrawState);
    procedure DrawObjectButton(Surface: ISurface; Obj: IStorageObject;
      ItemIndex, Button: Integer; Rect: TRectI; State: TDrawState);
    procedure DrawTask(Surface: ISurface; Task: IAsyncTask; Rect: TRectI;
      ColWidths: IntArray; State: TDrawState);
  end;

implementation

{ TS3Renderer }

constructor TS3Renderer.Create(Font: TFont);
const
  IconName = 'Material Design Icons';
  IconScale = 1.5;
  SmallIconScale = 1;
begin
  inherited Create;
  FFont := NewFont(Font);
  FIcon := NewFont(Font);
  FIcon.Name := IconName;
  FIcon.Size := FIcon.Size * IconScale;
  FSmallIcon := NewFont(Font);
  FSmallIcon.Name := IconName;
  FSmallIcon.Size := FSmallIcon.Size * SmallIconScale;
end;

procedure DrawRectState(Surface: ISurface; const Rect: TRectI; State: TDrawState; Radius: Float = 0);
var
  C: TColorB;
  G: IGradientBrush;
  B: IBrush;
  P: IPen;
begin
  B := NewBrush(clWindow);
  Surface.FillRect(B, Rect);
  if dsSelected in State then
  begin
    C := clHighlight;
    if dsFocused in State then
    begin
      G := NewBrush(0, Rect.Top, 0, Rect.Bottom);
      G.AddStop(C.Fade(0.1), 0);
      G.AddStop(C.Fade(0.4), 0.8);
      if Radius > 1 then
        Surface.FillRoundRect(G, Rect, Radius)
      else
        Surface.FillRect(G, Rect);
      P := NewPen(C.Fade(0.8));
      if Radius > 1 then
        Surface.StrokeRoundRect(P, Rect, Radius)
      else
        Surface.StrokeRect(P, Rect);
    end
    else
    begin
      B := NewBrush(C.Fade(0.1));
      if Radius > 1 then
        Surface.FillRoundRect(B, Rect, Radius)
      else
        Surface.FillRect(B, Rect);
      P := NewPen(C.Fade(0.4));
      if Radius > 1 then
        Surface.StrokeRoundRect(P, Rect, Radius)
      else
        Surface.StrokeRect(P, Rect);
    end;
  end
  { TODO: Test more }
  else if dsDefaulted in State then
  begin
    C := clHighlight;
    if dsFocused in State then
      C := C.Fade(0.8)
    else
      C := C.Fade(0.3);
    StrokeRectColor(Surface, Rect, C, Radius);
  end
  else if dsHot in State then
  begin
    C := clHighlight;
    P := NewPen(C.Fade(0.5));
    if Radius > 1 then
      Surface.StrokeRoundRect(P, Rect, Radius)
    else
      Surface.StrokeRect(P, Rect);
  end;
end;

const
  IndentRight = 4;
  RectRadius = 4;

procedure Inflate(var Rect: TRectI);
begin
  Rect.Inflate(-2, -1);
end;

function GetLevel(Obj: IStorageObject): Integer;
begin
  Result := 0;
  while Obj <> nil do
  begin
    Obj := Obj.Parent;
    Inc(Result);
  end;
end;

function GetIndent(Obj: IStorageObject): Integer;
const
  Space = 22;
var
  I: Integer;
begin
  I := 0;
  while Obj <> nil do
  begin
    Obj := Obj.Parent;
    Inc(I);
  end;
  Result := I * Space;
end;

function GetColumn(const Rect: TRect; ColWidths: IntArray; Index: Integer): TRectI;
var
  I: Integer;
begin
  Result := Rect;
  Result.Left := 0;
  for I := 0 to Index  - 1 do
    Result.Left := Result.Left + ColWidths[I];
  Result.Width := ColWidths[Index];
  Inflate(Result);
end;


procedure TS3Renderer.DrawBucket(Surface: ISurface; Bucket: IBucket; Rect: TRectI;
  State: TDrawState);
const
  Indent = 28;
  TextIndent = 6;

  procedure DrawText(Rect: TRectI);
  var
    S: string;
  begin
    Inflate(Rect);
    Rect.Left := Rect.Left + Indent;
    DrawRectState(Surface, Rect, State, RectRadius);
    if Bucket = nil then
      Exit;
    Rect.Left := Rect.Left + TextIndent;
    Rect.Right := Rect.Right - IndentRight;
    FFont.Style := FFont.Style + [fsBold];
    Rect.Bottom := Rect.Top + Rect.Height div 2;
    Rect.Top := Rect.Top + 3;
    Surface.TextOut(FFont, Bucket.Name, Rect, drLeft);
    FFont.Style := FFont.Style - [fsBold];
    Rect.Offset(0, Rect.Height);
    Rect.Top := Rect.Top - 3;
    S := Bucket.Region;
    if S = '' then
      S := '(locating region)';
    Surface.TextOut(FFont, S, Rect, drLeft);
  end;

  procedure DrawIcon(Rect: TRectI);
  var
    S: string;
  begin
    Rect.Right := Rect.Left + Indent + 8;
    FIcon.Color := colorBucket;
    if dsSelected in State then
      S := iconBucketSelect
    else
      S := iconBucket;
    Surface.TextOut(FIcon, S, Rect, drCenter);
  end;

begin
  DrawText(Rect);
  DrawIcon(Rect);
end;

procedure TS3Renderer.ObjectsButtonCalc(Obj: IStorageObject; Index: Integer;
  Rect: TRectI; var Buttons: TButtonRects);
begin
  if Obj is IFolder then
  begin
    Rect.Left := GetIndent(Obj);
    Rect.Width := Rect.Height;
    Rect.X := Rect.Left - Rect.Width;
    Buttons.Length := 1;
    Buttons[0] := Rect;
  end;
end;

procedure TS3Renderer.DrawObjectEmpty(Surface: ISurface; Rect: TRectI; Index: Integer);
begin
  FillRectState(Surface, Rect, []);
  if Index = 0 then
  begin
    Inflate(Rect);
    Rect.Left := 12;
    Surface.TextOut(FFont, 'This container is empty', Rect, drLeft);
  end;
end;

procedure TS3Renderer.DrawObject(Surface: ISurface; Obj: IStorageObject; Rect: TRectI; ColWidths: IntArray;
  State: TDrawState);
var
  IconRect: TRectI;
  TextRect: TRectI;
  Folder: IFolder;
  Content: IContent;
  S: string;
  B: TColorB;
begin
  {if dsDefaulted in State then
  begin
    Inflate(Rect);
    DrawRectState(Surface, Rect, State, RectRadius)
  end
  else if dsSelected in State then
  begin
    B := clHighlight;
    Surface.FillRect(NewBrush(B.Fade(0.2)), Rect);
    Inflate(Rect);
  end
  else
    Inflate(Rect);}
  //Rect.Left := GetIndent(Obj);
  IconRect := Rect;
  IconRect.Left := GetIndent(Obj);
  IconRect.Width := Rect.Height;
  if Obj is IFolder then
  begin
    Folder := Obj as IFolder;
    if dsSelected in State then
      S := iconFolderSelect
    else if Folder.Opened then
      S := iconFolderOpened
    else
      S := iconFolderClosed;
    FIcon.Color := colorFolder;
    Surface.TextOut(FIcon, S, IconRect, drLeft);
  end
  else if Obj is IContent then
  begin
    Content := Obj as IContent;
    if dsSelected in State then
      S := iconContentSelect
    else
      S := iconContent;
    FIcon.Color := colorContent;
    Surface.TextOut(FIcon, S, IconRect, drLeft);
  end
  else
  begin
    FFont.Style := FFont.Style + [fsItalic];
    IconRect.Right := IconRect.Left;
    TextRect := IconRect;
    TextRect.Left := IconRect.Right - 4;
    TextRect.Width := Round(Surface.TextSize(FFont, Obj.Name).X + 16);
    Inflate(TextRect);
    DrawRectState(Surface, TextRect, State - [dsSelected], RectRadius);
    Surface.TextOut(FFont, Obj.Name, TextRect, drCenter);
    FFont.Style := FFont.Style - [fsItalic];
    Exit;
  end;
  TextRect := IconRect;
  TextRect.Left := IconRect.Right - 4;
  TextRect.Right := Rect.Right;
  Inflate(TextRect);
  DrawRectState(Surface, TextRect, State, RectRadius);
  TextRect.Inflate(-4, 0);
  Surface.TextOut(FFont, Obj.Name, TextRect, drLeft);
end;

procedure TS3Renderer.DrawObjectButton(Surface: ISurface; Obj: IStorageObject;
  ItemIndex, Button: Integer; Rect: TRectI; State: TDrawState);
var
  Folder: IFolder;
  S: string;
begin
  if Obj is IFolder then
  begin
    Folder := Obj as IFolder;
    if Folder.Opened then
      S := iconFolderDown
    else
      S := iconFolderRight;
    FIcon.Color := clWindowText;
    Surface.TextOut(FIcon, S, Rect, drRight);
  end;
end;

procedure TS3Renderer.DrawTask(Surface: ISurface; Task: IAsyncTask; Rect: TRectI;
  ColWidths: IntArray; State: TDrawState);
var
  Data: TTaskData;

  procedure DrawText(Rect: TRectI);
  begin
    Rect.Left := ColWidths[0] - 4;
    Inflate(Rect);
    DrawRectState(Surface, Rect, State, RectRadius);
    FFont.Style := FFont.Style - [fsBold];
    Rect.Left := Rect.Left + 4;
    Rect.Width := ColWidths[1] - 8;
    Surface.TextOut(FFont, Data.Message, GetColumn(Rect, ColWidths, 1), drLeft);
    Surface.TextOut(FFont, TimeToStr(Task.StartTime), GetColumn(Rect, ColWidths,  2), drLeft);
    Surface.TextOut(FFont, Format('%.0f ms', [Task.Duration * 1000]), GetColumn(Rect, ColWidths, 3), drLeft);
  end;

  procedure DrawIcon;
  var
    R: TRectI;
    S: string;
  begin
    case Task.Status of
      asyncBusy:
        begin
          FSmallIcon.Color := colorBusy;
          S := iconBusy;
        end;
      asyncSuccess:
        begin
          FSmallIcon.Color := colorSuccess;
          S := iconSuccess;
        end;
      asyncFail:
        begin
          FSmallIcon.Color := colorFail;
          S := iconFail;
        end;
      asyncCanceled:
        begin
          FSmallIcon.Color := colorCanceled;
          S := iconCanceled;
        end;
    end;
    R := GetColumn(Rect, ColWidths, 0);
    R.Offset(-2, 0);
    Surface.TextOut(FSmallIcon, S, R, drCenter);
  end;

begin
  Data := Task.Data as TTaskData;
  FillRectState(Surface, Rect, []);
  DrawText(Rect);
  DrawIcon;
end;

end.

