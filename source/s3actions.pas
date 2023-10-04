(********************************************************)
(*                                                      *)
(*  Codebot Cloud Client                                *)
(*  http://www.getlazarus.org/apps/s3                   *)
(*  Modified October 2023                               *)
(*                                                      *)
(*  Released under GNU GPLv3 license                    *)
(*                                                      *)
(********************************************************)

unit S3Actions;

{$i c3.inc}

interface

uses
  Classes, SysUtils, Controls, Graphics, Forms, Dialogs, ExtCtrls,
  DialogTools, S3Objects, S3Graph, BlackFrm,
  Codebot.System,
  Codebot.Controls,
  Codebot.Controls.Extras,
  Codebot.Graphics,
  Codebot.Graphics.Types;

{ TS3ActionManager }

type
  TS3ActionManager = class
  private
    FManager: TS3Manager;
    FActions: TStrings;
    FBox: TPanel;
    FFont: IFont;
    FIcon: IFont;
    FObj: IInterface;
    procedure Dialog;
    procedure BoxClick(Sender: TObject);
    procedure DrawBucket(Surface: ISurface; Bucket: IBucket; Rect: TRectI);
    procedure DrawFolder(Surface: ISurface; Folder: IFolder; Rect: TRectI);
    procedure DrawObject(Surface: ISurface; Content: IContent; Rect: TRectI);
    procedure DrawTask(Surface: ISurface; Task: IAsyncTask; Rect: TRectI);
    procedure DrawNull(Surface: ISurface; Null: INullObject; Rect: TRectI);
    procedure DrawNothing(Surface: ISurface; Rect: TRectI);
    procedure Render(Sender: TObject);
  public
    constructor Create(Manager: TS3Manager; Box: TPanel);
    destructor Destroy; override;
    procedure Select(Obj: IInterface);
  end;

implementation

{ TS3ActionManager }

constructor TS3ActionManager.Create(Manager: TS3Manager; Box: TPanel);
begin
  inherited Create;
  FManager := Manager;
  FActions := TStringList.Create;
  FBox := Box;
  FFont := NewFont(FBox.Font);
  FIcon := NewFont(FBox.Font);
  FIcon.Name := IconFontName;
  FIcon.Size := FIcon.Size * IconScale;
  FBox.OnPaint := Render;
  FBox.OnClick  := BoxClick;
end;

destructor TS3ActionManager.Destroy;
begin
  inherited Create;
  FActions.Free;
end;

procedure TS3ActionManager.Select(Obj: IInterface);
begin
  FActions.Clear;
  FObj := Obj;
  FBox.Invalidate;
end;

procedure TS3ActionManager.DrawBucket(Surface: ISurface; Bucket: IBucket; Rect: TRectI);

  procedure BuildActions;
  begin
    FActions.Add('Create new bucket');
    FActions.Add('Create new folder');
    FActions.Add('Delete bucket');
    FActions.Add('Edit ACL');
    FActions.Add('Edit CORS');
    FActions.Add('Properties');
    FActions.Add('- General -');
    FActions.Add('Accounts');
    FActions.Add('Options');
  end;

begin
  if FActions.Count = 0 then
    BuildActions;
  Surface.TextOut(FFont, 'Actions for bucket'#10#10 + Bucket.Name + #10#10 + FActions.Text, Rect, drLeft);
end;

procedure TS3ActionManager.Dialog;
begin
  ShowMessage('This is a dialog');
end;

procedure TS3ActionManager.BoxClick(Sender: TObject);
begin
  ShowDialog(Dialog);
  {with TBlackForm.Create(Application) do
  begin
    ShowModal;
    Free;

  end;}
end;

procedure TS3ActionManager.DrawFolder(Surface: ISurface; Folder: IFolder; Rect: TRectI);
begin
  Surface.TextOut(FFont, 'Folder : ' + Folder.Name, Rect, drCenter);

end;

procedure TS3ActionManager.DrawObject(Surface: ISurface; Content: IContent; Rect: TRectI);
begin
  Surface.TextOut(FFont, 'Object : ' + Content.Name, Rect, drCenter);
end;

procedure TS3ActionManager.DrawTask(Surface: ISurface; Task: IAsyncTask; Rect: TRectI);
begin
  Surface.TextOut(FFont, 'Task', Rect, drCenter);
end;

procedure TS3ActionManager.DrawNull(Surface: ISurface; Null: INullObject; Rect: TRectI);
begin
  Surface.TextOut(FFont, 'Null', Rect, drCenter);
end;

procedure TS3ActionManager.DrawNothing(Surface: ISurface; Rect: TRectI);
begin
  Surface.TextOut(FFont, 'Nothing', Rect, drCenter);
end;

procedure TS3ActionManager.Render(Sender: TObject);
var
  Surface: ISurface;
  Rect: TRectI;
begin
  Surface := NewSurface(FBox);
  Rect := FBox.ClientRect;
  Surface.FillRect(NewBrush(FBox.ParentCurrentColor), Rect);
  if FObj = nil then
    DrawNothing(Surface, Rect)
  else if FObj is IBucket then
    DrawBucket(Surface, FObj as IBucket, Rect)
  else if FObj is IFolder then
    DrawFolder(Surface, FObj as IFolder, Rect)
  else if FObj is IContent then
    DrawObject(Surface, FObj as IContent, Rect)
  else if FObj is IAsyncTask then
    DrawTask(Surface, FObj as IAsyncTask, Rect)
  else if FObj is INullObject then
    DrawNull(Surface, FObj as INullObject, Rect);
end;

end.

