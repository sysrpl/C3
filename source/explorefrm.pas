(********************************************************)
(*                                                      *)
(*  Codebot Simple Storage                              *)
(*  http://www.getlazarus.org/apps/s3                   *)
(*  Modified October 2023                               *)
(*                                                      *)
(*  Released under GNU GPLv3 license                    *)
(*                                                      *)
(********************************************************)

unit ExploreFrm;

{$mode delphi}
{$WARN 5024 off : Parameter "$1" not used}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, ExtCtrls, PairSplitter,
  S3Objects, S3Graph,
  Codebot.System,
  Codebot.Text.Xml,
  Codebot.Networking.Storage,
  Codebot.Controls.Scrolling,
  Codebot.Graphics,
  Codebot.Graphics.Types;

{ TExploreForm }

type
  TExploreForm = class(TForm)
    BucketsBox: TDrawList;
    BucketsLabel: TLabel;
    Label1: TLabel;
    TasksHeader: THeaderBar;
    ObjectsLabel: TLabel;
    HorzSplit: TPairSplitter;
    LeftSide: TPairSplitterSide;
    ObjectsBox: TDrawList;
    BucketTimer: TTimer;
    ObjectsHeader: THeaderBar;
    TasksLabel: TLabel;
    RightSide: TPairSplitterSide;
    TasksBox: TDrawList;
    VertSplit: TPairSplitter;
    TopSide: TPairSplitterSide;
    BottomSide: TPairSplitterSide;
    BusyTimer: TTimer;
    QuitTimer: TTimer;
    procedure BottomSideResize(Sender: TObject);
    procedure BucketsBoxDrawItem(Sender: TObject; Surface: ISurface;
      Index: Integer; Rect: TRectI; State: TDrawState);
    procedure BoxFocusMouseAction(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LabelClick(Sender: TObject);
    procedure ObjectsBoxButtonCalc(Sender: TObject; ItemIndex: Integer;
      Rect: TRectI; var Buttons: TButtonRects);
    procedure ObjectsBoxButtonClick(Sender: TObject; ItemIndex, Button: Integer);
    procedure ObjectsBoxButtonDraw(Sender: TObject; Surface: ISurface;
      ItemIndex, Button: Integer; Rect: TRectI; State: TDrawState);
    procedure ObjectsBoxDrawItem(Sender: TObject; Surface: ISurface;
      Index: Integer; Rect: TRectI; State: TDrawState);
    procedure TasksBoxDrawItem(Sender: TObject; Surface: ISurface;
      Index: Integer; Rect: TRectI; State: TDrawState);
    procedure BoxEnter(Sender: TObject);
    procedure BoxExit(Sender: TObject);
    procedure BucketsBoxSelectItem(Sender: TObject);
    procedure BucketTimerTimer(Sender: TObject);
    procedure BusyTimerTimer(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HorzSplitChangeBounds(Sender: TObject);
    procedure QuitTimerTimer(Sender: TObject);
    procedure TasksHeaderColumnResize(Sender: TObject; Column: THeaderColumn);
    procedure VertSplitChangeBounds(Sender: TObject);
  private
    FManager: TS3Manager;
    FBucket: IBucket;
    FRenderer: TS3Renderer;
    FTaskHeight: Integer;
    procedure HandleBusyChange(Sender: TObject; IsBusy: Boolean);
    procedure HandleTask(Sender: TObject; Data: TTaskData);
  end;

var
  ExploreForm: TExploreForm;

implementation

{$R *.lfm}

{ TExploreForm }

procedure TExploreForm.FormCreate(Sender: TObject);
const
  BucketHeight = 48;
  ObjectHeight = 28;
  HistoryHeight = 24;
begin
  ClientWidth := VertSplit.Left * 2 + VertSplit.Width;
  ClientHeight := VertSplit.Top * 2 + VertSplit.Height;
  VertSplit.Anchors := [akLeft, akTop, akRight, akBottom];
  BucketsBox.ItemHeight := BucketHeight;
  BucketsBox.Tag := PtrInt(BucketsLabel);
  BucketsLabel.Tag := PtrInt(BucketsBox);
  ObjectsBox.ItemHeight := ObjectHeight;
  ObjectsBox.Tag := PtrInt(ObjectsLabel);
  ObjectsLabel.Tag := PtrInt(ObjectsBox);
  ObjectsHeader.Tag := PtrInt(ObjectsBox);
  ObjectsBox.Count := 1;
  TasksBox.ItemHeight := HistoryHeight;
  TasksBox.Tag := PtrInt(TasksLabel);
  TasksLabel.Tag := PtrInt(TasksBox);
  TasksHeader.Tag := PtrInt(TasksBox);
  FManager := TS3Manager.Create(S3Configs.Amazon);
  FManager.OnBusyChange.Add(HandleBusyChange);
  FManager.OnTask.Add(HandleTask);
  FRenderer := TS3Renderer.Create(Font);
end;

procedure TExploreForm.FormDestroy(Sender: TObject);
begin
  FRenderer.Free;
  FManager.Free;
end;

procedure TExploreForm.FormResize(Sender: TObject);
begin
  HorzSplit.Invalidate;
end;

procedure TExploreForm.FormShow(Sender: TObject);
begin
  OnShow := nil;
  ObjectsBox.MultiSelect := True;
  FManager.Refresh;
  FTaskHeight := VertSplit.Height - VertSplit.Position;
end;

procedure TExploreForm.HorzSplitChangeBounds(Sender: TObject);
begin
  HorzSplit.Invalidate;
end;

procedure TExploreForm.QuitTimerTimer(Sender: TObject);
begin
  if not FManager.Busy then
    Close;
end;

procedure TExploreForm.TasksHeaderColumnResize(Sender: TObject;
  Column: THeaderColumn);
begin
  TasksBox.Invalidate;
end;

procedure TExploreForm.BusyTimerTimer(Sender: TObject);
begin
  TasksBox.Invalidate;
end;

procedure TExploreForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if FManager.Busy then
  begin
    FManager.Shutdown;
    CanClose := False;
    QuitTimer.Enabled := True;
  end
  else
    CanClose := True;
end;

procedure TExploreForm.HandleBusyChange(Sender: TObject; IsBusy: Boolean);
begin
  BusyTimer.Enabled := IsBusy;
  TasksBox.Invalidate;
end;

procedure TExploreForm.HandleTask(Sender: TObject; Data: TTaskData);
var
  Bucket: IBucket;
begin
  case Data.Kind of
    taskListBuckets,
    taskGetBucketLocation:
      begin
        BucketsBox.Count := FManager.Buckets.Count;
        BucketsBox.Invalidate;
      end;
    taskListBucketObjects:
      begin
        Bucket := Data.Target as IBucket;
        if Bucket = FBucket then
          ObjectsBox.Count := Bucket.Count;
        if ObjectsBox.Count = 0 then
          ObjectsBox.Count := 1;
        ObjectsBox.Invalidate;
      end;
  end;
  TasksBox.Count := FManager.Tasks.Count;
  TasksBox.Invalidate;
end;

procedure TExploreForm.BucketsBoxDrawItem(Sender: TObject; Surface: ISurface;
  Index: Integer; Rect: TRectI; State: TDrawState);
var
  Bucket: IBucket;
begin
  if Index < FManager.Buckets.Count then
    Bucket := FManager.Buckets[Index]
  else
    Bucket := nil;
  FRenderer.DrawBucket(Surface, Bucket, Rect, State);
end;

procedure TExploreForm.BoxFocusMouseAction(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TWinControl((Sender as TComponent).Tag).SetFocus;
end;

procedure TExploreForm.LabelClick(Sender: TObject);
begin
  TWinControl((Sender as TComponent).Tag).SetFocus;
end;

procedure TExploreForm.ObjectsBoxButtonCalc(Sender: TObject;
  ItemIndex: Integer; Rect: TRectI; var Buttons: TButtonRects);
begin
  Buttons.Length := 1;
  Rect.Right := 20;
  Rect.X := 4;
  Buttons[0] := Rect;
end;

procedure TExploreForm.ObjectsBoxButtonClick(Sender: TObject; ItemIndex,
  Button: Integer);
var
  Obj: IStorageObject;
  I: Integer;
begin
  if (FBucket <> nil) and (ItemIndex < FBucket.Count) then
  begin
    Obj := FBucket[ItemIndex];
    Obj.Checked := not Obj.Checked;
    for I := 0 to ObjectsBox.Count - 1 do
      if ObjectsBox.IsSelected(I) then
        FBucket[I].Checked := Obj.Checked;
    ObjectsBox.Invalidate;
  end;
end;

procedure TExploreForm.ObjectsBoxButtonDraw(Sender: TObject; Surface: ISurface;
  ItemIndex, Button: Integer; Rect: TRectI; State: TDrawState);
var
  Obj: IStorageObject;
begin
  if (FBucket <> nil) and (ItemIndex < FBucket.Count) then
  begin
    Obj := FBucket[ItemIndex];
    if Obj.Checked or ObjectsBox.IsSelected(ItemIndex) or (ObjectsBox.ItemIndex = ItemIndex) then
      FRenderer.DrawObjectButton(Surface, Obj, ItemIndex, Button, Rect, State);
  end;
end;

procedure TExploreForm.ObjectsBoxDrawItem(Sender: TObject; Surface: ISurface;
  Index: Integer; Rect: TRectI; State: TDrawState);
var
  Obj: IStorageObject;
begin
  if (FBucket = nil) or (Index > FBucket.Count - 1) then
    FRenderer.DrawObjectEmpty(Surface, Rect, Index)
  else
  begin
    Obj := FBucket[Index];
    FRenderer.DrawObject(Surface, Obj, Rect, ObjectsHeader.GetColWidths, State);
  end;
end;

procedure TExploreForm.TasksBoxDrawItem(Sender: TObject; Surface: ISurface;
  Index: Integer; Rect: TRectI; State: TDrawState);
var
  Task: IAsyncTask;
  I: Integer;
begin
  I := FManager.Tasks.Count;
  if Index < I then
    Task := FManager.Tasks[I - Index - 1]
  else
    Task := nil;
  FRenderer.DrawTask(Surface, Task, Rect, TasksHeader.GetColWidths, State);
end;

function InactiveColor: TColor;
var
  C: TColorB;
begin
  C := clActiveCaption;
  Result := C.Blend(clInactiveCaption, 0.25).Color;
end;

procedure TExploreForm.BoxEnter(Sender: TObject);
var
  L: TLabel;
begin
  L := TLabel((Sender as TComponent).Tag);
  L.Color := InactiveColor;
  L.Font.Color := clCaptionText;
end;

procedure TExploreForm.BoxExit(Sender: TObject);
var
  L: TLabel;
begin
  L := TLabel((Sender as TComponent).Tag);
  L.Color := clInactiveCaption;
  L.Font.Color := clInactiveCaptionText;
end;

procedure TExploreForm.BucketsBoxSelectItem(Sender: TObject);
begin
  BucketTimer.Enabled := False;
  BucketTimer.Enabled := True;
end;

procedure TExploreForm.BucketTimerTimer(Sender: TObject);
var
  I: Integer;
begin
  BucketTimer.Enabled := False;
  I := BucketsBox.ItemIndex;
  if (I > -1) and (I < FManager.Buckets.Count) then
  begin
    FBucket := FManager.Buckets[I];
    ObjectsLabel.Caption := '   Objects in bucket ' + FBucket.Name;
    if FBucket.Region = '' then
      BucketTimer.Enabled := True
    else
      FBucket.Query;
    if FBucket.Count > 0 then
      ObjectsBox.Count := FBucket.Count
    else
      ObjectsBox.Count := 1;
  end
  else
  begin
    FBucket := nil;
    ObjectsLabel.Caption := '   No bucket selected';
    ObjectsBox.Count := 1;
  end;
end;

procedure TExploreForm.BottomSideResize(Sender: TObject);
begin
  FTaskHeight := VertSplit.Height - VertSplit.Position;
end;

procedure TExploreForm.VertSplitChangeBounds(Sender: TObject);
begin
  if FTaskHeight > 0 then
  begin
    VertSplit.Position := VertSplit.Height - FTaskHeight;
    VertSplit.Invalidate;
    HorzSplit.Invalidate;
  end;
end;

end.

