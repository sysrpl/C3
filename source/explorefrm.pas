(********************************************************)
(*                                                      *)
(*  Codebot Cloud Client                                *)
(*  http://www.getlazarus.org/apps/s3                   *)
(*  Modified October 2023                               *)
(*                                                      *)
(*  Released under GNU GPLv3 license                    *)
(*                                                      *)
(********************************************************)

unit ExploreFrm;

{$i c3.inc}

interface

uses
  Classes, SysUtils, Forms, Dialogs , Controls, Graphics, StdCtrls, ExtCtrls,
  PairSplitter, LCLType, LCLIntf, S3Actions, S3Graph, S3Objects, DialogTools,
  Codebot.System,
  Codebot.Collections,
  Codebot.Text.Xml,
  Codebot.Networking.Storage,
  Codebot.Controls.Scrolling,
  Codebot.Controls.Containers,
  Codebot.Controls.Extras,
  Codebot.Graphics,
  Codebot.Graphics.Types;

{ TExploreForm }

type
  TExploreForm = class(TForm)
    BucketsBox: TDrawList;
    BucketsLabel: TLabel;
    ObjectsBox: TDrawList;
    ObjectsHeader: THeaderBar;
    ObjectsLabel: TLabel;
    ObjectsPanel: TPanel;
    ActionsBox: TPanel;
    TasksHeader: THeaderBar;
    HorzSplit: TPairSplitter;
    LeftSide: TPairSplitterSide;
    BucketTimer: TTimer;
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
    procedure FormDblClick(Sender: TObject);
    procedure LabelClick(Sender: TObject);
    procedure ObjectsBoxButtonCalc(Sender: TObject; ItemIndex: Integer;
      Rect: TRectI; var Buttons: TButtonRects);
    procedure ObjectsBoxButtonClick(Sender: TObject; ItemIndex, Button: Integer);
    procedure ObjectsBoxButtonDraw(Sender: TObject; Surface: ISurface;
      ItemIndex, Button: Integer; Rect: TRectI; State: TDrawState);
    procedure ObjectsBoxDblClick(Sender: TObject);
    procedure ObjectsBoxDrawItem(Sender: TObject; Surface: ISurface;
      Index: Integer; Rect: TRectI; State: TDrawState);
    procedure ObjectsBoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SelectItem(Sender: TObject);
    procedure ObjectsHeaderColumnResize(Sender: TObject; Column: THeaderColumn);
    procedure RenderBox1Click(Sender: TObject);
    procedure TasksBoxDblClick(Sender: TObject);
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
    FActionManager: TS3ActionManager;
    FBucket: IBucket;
    FObjects: IList<IStorageObject>;
    FRenderer: TS3Renderer;
    FTaskHeight: Integer;
    procedure ObjectsRebuild;
    function ObjectsGet(Index: Integer): IStorageObject;
    function ObjectsCount: Integer;
    function ObjectsIndex(Item: IStorageObject): Integer;
    procedure FolderExpand(Folder: IFolder; Open: Boolean);
    procedure HandleBusyChange(Sender: TObject; IsBusy: Boolean);
    procedure HandleTask(Sender: TObject; Data: TTaskData);
  end;

var
  ExploreForm: TExploreForm;

implementation

{$R *.lfm}

{ TExploreForm }

const
  DefCapcity = 1000;

procedure TExploreForm.FormCreate(Sender: TObject);
const
  BucketHeight = 48;
  ObjectHeight = 24;
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
  FObjects := TInterfaces<IStorageObject>.Create;
  FObjects.Capacity := DefCapcity;
  FManager := TS3Manager.Create(S3Configs.Amazon);
  FManager.OnBusyChange.Add(HandleBusyChange);
  FManager.OnTask.Add(HandleTask);
  FRenderer := TS3Renderer.Create(Font);
end;

procedure TExploreForm.FormDestroy(Sender: TObject);
begin
  FRenderer.Free;
  FManager.Free;
  FActionManager.Free;
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
  FActionManager := TS3ActionManager.Create(FManager, ActionsBox);
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

procedure TExploreForm.ObjectsRebuild;

  procedure AddItem(Item: IStorageObject);
  var
    Folder: IFolder;
    Child: IStorageObject;
  begin
    FObjects.Add(Item);
    if Item is IFolder then
    begin
      Folder := Item as IFolder;
      if Folder.Opened then
        if Folder.Count = 0 then
          AddItem(Folder.Null)
        else for Child in Folder do
          AddItem(Child);
    end;
  end;

var
  Item: IStorageObject;
begin
  FObjects.Clear;
  FObjects.Capacity := DefCapcity;
  if FBucket = nil then
    Exit;
  for Item in FBucket do
    AddItem(Item);
end;

function TExploreForm.ObjectsGet(Index: Integer): IStorageObject;
begin
  Result := nil;
  if (Index < 0) or (Index > FObjects.Count - 1) then
    Exit;
  Result := FObjects[Index];
end;

function TExploreForm.ObjectsCount: Integer;
begin
  Result := FObjects.Count;
end;

function TExploreForm.ObjectsIndex(Item: IStorageObject): Integer;
begin
  Result := FObjects.IndexOf(Item);
end;

procedure TExploreForm.HandleBusyChange(Sender: TObject; IsBusy: Boolean);
begin
  BusyTimer.Enabled := IsBusy;
  TasksBox.Invalidate;
end;

procedure TExploreForm.HandleTask(Sender: TObject; Data: TTaskData);
var
  Bucket: IBucket;
  Folder: IFolder;
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
        begin
          ObjectsRebuild;
          ObjectsBox.Count := ObjectsCount;
          if ObjectsBox.Count = 0 then
            ObjectsBox.Count := 1;
          ObjectsBox.ItemIndex := 0;
          ObjectsBox.Invalidate;
        end;
      end;
    taskListFolderObjects:
      begin
        Folder := Data.Target as IFolder;
        Bucket := Folder.Bucket;
        if Bucket = FBucket then
          FolderExpand(Folder, Folder.Opened);
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

procedure TExploreForm.FormDblClick(Sender: TObject);
begin
  //Caption := 'Dimmed';
  //DimWindow(Self);
  //ShowMessage('Hello world');
  //UndimWindow;
end;

procedure TExploreForm.LabelClick(Sender: TObject);
begin
  TWinControl((Sender as TComponent).Tag).SetFocus;
end;

procedure TExploreForm.FolderExpand(Folder: IFolder; Open: Boolean);

  function IsChild(Obj: IStorageObject): Boolean;
  begin
    Result := False;
    if Obj = nil then
      Exit;
    if Obj.Parent = Folder then
      Result := True
    else
      Result := IsChild(Obj.Parent);
  end;

var
  Obj: IStorageObject;
  Top, Index: Integer;
begin
  Folder.Opened := Open;
  if Folder.Bucket <> FBucket then
    Exit;
  Top := ObjectsBox.TopIndex;
  Index := ObjectsBox.ItemIndex;
  Obj := nil;
  if Index > -1 then
  begin
    Obj := ObjectsGet(Index);
    if IsChild(Obj) then
      Obj := Folder;
  end;
  ObjectsRebuild;
  ObjectsBox.Count := ObjectsCount;
  ObjectsBox.TopIndex := Top;
  Index := ObjectsIndex(Obj);
  if Index < 0 then
    Index := 0;
  ObjectsBox.ItemIndex := Index;
  ObjectsBox.Invalidate;
end;

procedure TExploreForm.ObjectsBoxButtonCalc(Sender: TObject;
  ItemIndex: Integer; Rect: TRectI; var Buttons: TButtonRects);
var
  Obj: IStorageObject;
begin
  Obj := ObjectsGet(ItemIndex);
  if Obj <> nil then
    FRenderer.ObjectsButtonCalc(Obj, ItemIndex, Rect, Buttons);
end;

procedure TExploreForm.ObjectsBoxButtonClick(Sender: TObject; ItemIndex,
  Button: Integer);
var
  Obj: IStorageObject;
  Folder: IFolder;
  // I: Integer;
begin
  Obj := ObjectsGet(ItemIndex);
  if (Obj <> nil) and (Obj is IFolder) then
  begin
    Folder := Obj as IFolder;
    FolderExpand(Folder, not Folder.Opened);
    {Obj.Checked := not Obj.Checked;

    for I := 0 to ObjectsBox.Count - 1 do
      if ObjectsBox.IsSelected(I) then
        FObjects[I].Checked := Obj.Checked;}

  end;
end;

procedure TExploreForm.ObjectsBoxButtonDraw(Sender: TObject; Surface: ISurface;
  ItemIndex, Button: Integer; Rect: TRectI; State: TDrawState);
var
  Obj: IStorageObject;
begin
  Obj := ObjectsGet(ItemIndex);
  if Obj <> nil then
  begin
    FRenderer.DrawObjectButton(Surface, Obj, ItemIndex, Button, Rect, State);
  end;
end;

procedure TExploreForm.ObjectsBoxDblClick(Sender: TObject);
var
  Obj: IStorageObject;
  Folder: IFolder;
  Content: IContent;
begin
  Obj := ObjectsGet(ObjectsBox.ItemIndex);
  if Obj is IFolder then
  begin
    Folder := Obj as IFolder;
    FolderExpand(Folder, not Folder.Opened);
  end
  else if Obj is IContent then
  begin
    Content := Obj as IContent;
    OpenUrl(Content.Presign);
  end;
end;

procedure TExploreForm.ObjectsBoxDrawItem(Sender: TObject; Surface: ISurface;
  Index: Integer; Rect: TRectI; State: TDrawState);
var
  Obj: IStorageObject;
begin
  Obj := ObjectsGet(Index);
  if Obj <> nil then
    FRenderer.DrawObject(Surface, Obj, Rect, ObjectsHeader.GetColWidths, State)
  else
    FRenderer.DrawObjectEmpty(Surface, Rect, Index);
end;

procedure TExploreForm.ObjectsBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Obj: IStorageObject;
  Folder: IFolder;
begin
  Obj := ObjectsGet(ObjectsBox.ItemIndex);
  if Obj is IFolder then
  begin
    Folder := Obj as IFolder;
    case Key of
      VK_LEFT:
        if Folder.Opened then
          FolderExpand(Folder, False)
        else if Obj.Parent is IFolder then
          ObjectsBox.ItemIndex := ObjectsIndex(Obj.Parent);
      VK_RIGHT: FolderExpand(Folder, True);
      VK_SPACE: FolderExpand(Folder, not Folder.Opened);
    end;
  end
  else if (Obj is IContent) and (Key = VK_LEFT) then
    ObjectsBox.ItemIndex := ObjectsIndex(Obj.Parent);
end;

procedure TExploreForm.SelectItem(Sender: TObject);
var
  I: Integer;
begin
  if BucketsBox.Focused then
  begin
    I := BucketsBox.ItemIndex;
    if I > -1 then
      FActionManager.Select(FManager.Buckets[I])
    else
      FActionManager.Select(nil);
  end
  else if ObjectsBox.Focused then
  begin
    I := ObjectsBox.ItemIndex;
    if I > -1 then
      FActionManager.Select(ObjectsGet(I))
    else if FBucket <> nil then
      FActionManager.Select(FBucket.Null)
    else
      FActionManager.Select(nil);
  end
  else if TasksBox.Focused then
  begin
    I := TasksBox.ItemIndex;
    if I > -1 then
      FActionManager.Select(FManager.Tasks[FManager.Tasks.Count - I - 1])
    else
      FActionManager.Select(nil);
  end;
end;

procedure TExploreForm.ObjectsHeaderColumnResize(Sender: TObject;
  Column: THeaderColumn);
begin
  ObjectsBox.Invalidate;
end;

procedure TExploreForm.RenderBox1Click(Sender: TObject);
begin

end;

procedure TExploreForm.TasksBoxDblClick(Sender: TObject);
var
  Task: IAsyncTask;
  data: TTaskData;
  I: Integer;
begin
  I := TasksBox.ItemIndex;
  if I < 0 then
    Exit;
  Task := FManager.Tasks[FManager.Tasks.Count - I - 1];
  if Task.Status = asyncFail then
  begin
    Data := Task.Data as TTaskData;
    Data.Error.Beautify;
    ShowMessage('Task Error Details:'#10#10 + Data.Error.Xml)
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
  SelectItem(Sender);
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
    ObjectsRebuild;
    I := ObjectsCount;
    if I > 0 then
      ObjectsBox.Count := I
    else
      ObjectsBox.Count := 1;
    SelectItem(BucketsBox);
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

