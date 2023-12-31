(********************************************************)
(*                                                      *)
(*  Codebot Cloud Client                                *)
(*  http://www.getlazarus.org/apps/s3                   *)
(*  Modified October 2023                               *)
(*                                                      *)
(*  Released under GNU GPLv3 license                    *)
(*                                                      *)
(********************************************************)

unit S3Objects;

{$i c3.inc}

interface

uses
  Classes, SysUtils, DateUtils,
  Codebot.System,
  Codebot.Collections,
  Codebot.Text.Xml,
  Codebot.Networking.Storage;

{ Forward declarations }

type
  IStorageObject = interface;
  IStorageContainer = interface;
  IBucket = interface;
  IFolder = interface;
  IContent = interface;
  INullObject = interface;

{ List types }

  IBuckets = IList<IBucket>;
  ITasks = IList<IAsyncTask>;

{ IStorageObject }

  IStorageObject = interface
  ['{C508CA02-A75F-4CC6-87DD-F444C56AE30C}']
    function GetBucket: IBucket;
    function GetAcl: string;
    procedure SetAcl(const Value: string);
    function GetChecked: Boolean;
    procedure SetChecked(Value: Boolean);
    function GetName: string;
    function GetParent: IStorageContainer;
    procedure Refresh;
    property Bucket: IBucket read GetBucket;
    property Acl: string read GetAcl write SetAcl;
    property Name: string read GetName;
    property Checked: Boolean read GetChecked write SetChecked;
    property Parent: IStorageContainer read GetParent;
  end;

{ IStorageContainer }

  IStorageContainer = interface(IStorageObject)
  ['{6A2E9834-36CF-4268-B431-1DA84DC289A2}']
    function GetQueried: Boolean;
    function GetCount: Integer;
    function GetItem(Index: Integer): IStorageObject;
    function GetEnumerator: IEnumerator<IStorageObject>;
    function Contains(Obj: IStorageObject): Boolean;
    procedure Query;
    property Queried: Boolean read GetQueried;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: IStorageObject read GetItem; default;
  end;

{ IBucket }

  IBucket = interface(IStorageContainer)
  ['{769BB824-88B0-4619-9A85-EBDACBD14146}']
    function GetEndPoint: string;
    function GetRegion: string;
    function GetNull: INullObject;
    property EndPoint: string read GetEndPoint;
    property Region: string read GetRegion;
    property Null: INullObject read GetNull;
  end;

{ IFolder }

  IFolder = interface(IStorageContainer)
  ['{170A5139-953C-4AE0-9C15-47C27F368D1A}']
    function GetOpened: Boolean;
    procedure SetOpened(Value: Boolean);
    function GetNull: INullObject;
    property Opened: Boolean read GetOpened write SetOpened;
    property Null: INullObject read GetNull;
  end;

{ IContent is a file, but file is a keyword so we use IContent as a synonym }

  IContent = interface(IStorageObject)
  ['{02235761-F628-4717-B175-7CF38CBE8B6B}']
    function GetSize: Cardinal;
    function GetModified: TDateTime;
    function GetStorageClass: string;
    function Presign(Expires: Integer = 0; Shorten: Boolean = False): string;
    property Size: Cardinal read GetSize;
    property Modified: TDateTime read GetModified;
    property StorageClass: string read GetStorageClass;
  end;

{ INullObject is used as a placeholder item for an empty folder }

  INullObject = interface(IStorageObject)
  ['{72BEAD59-D842-45ED-9F59-CED51E899223}']
  end;

{ TTaskData }

  TTaskKind = (
    taskListBuckets,
    taskGetBucketLocation,
    taskListBucketObjects,
    taskListFolderObjects
  );

  TTaskData = class
  private
    FTarget: IStorageObject;
    FKind: TTaskKind;
    FMessage: string;
    FError: IDocument;
  public
    constructor Create(Target: IStorageObject; Kind: TTaskKind; const Message: string);
    property Target: IStorageObject read FTarget;
    property Kind: TTaskKind read FKind;
    property Message: string read FMessage write FMessage;
    property Error: IDocument read FError write FError;
  end;

{ Event prototypes }

  TBusyChangeEvent = procedure (Sender: TObject; IsBusy: Boolean) of object;
  TTaskDataEvent = procedure (Sender: TObject; Data: TTaskData) of object;

{ TS3Manager }

  TS3Manager = class
  private
    FS3: TS3Methods;
    FBuckets: IBuckets;
    FTasks: ITasks;
    FBusyCount: Integer;
    FOnBusyChange: TDelegate<TBusyChangeEvent>;
    FOnTask: TDelegate<TTaskDataEvent>;
    function GetOnBusyChange: IDelegate<TBusyChangeEvent>;
    function GetOnTask: IDelegate<TTaskDataEvent>;
    function GetBusy: Boolean;
    function GetBuckets: IBuckets;
    function GetTasks: ITasks;
    { These response handlers implement all task completion logic }
    procedure ListBuckets(Data: TTaskData; Response: IDocument);
    procedure GetBucketLocation(Data: TTaskData; Response: IDocument);
    procedure ListBucketObjects(Data: TTaskData; Response: IDocument);
    procedure ListFolderObjects(Data: TTaskData; Response: IDocument);
  protected
    procedure TaskStart(const Request: TS3Request; Data: TObject);
    procedure TaskComplete(Task: IAsyncTask; Result: IDocument);
    procedure DoTask(Data: TTaskData);
    procedure DoBusyChange(IsBusy: Boolean);
    property S3: TS3Methods read FS3;
  public
    constructor Create(Config: TS3ConfigFactory);
    destructor Destroy; override;
    procedure Refresh;
    procedure Shutdown;
    property Busy: Boolean read GetBusy;
    property Buckets: IBuckets read GetBuckets;
    property Tasks: ITasks read GetTasks;
    property OnBusyChange: IDelegate<TBusyChangeEvent> read GetOnBusyChange;
    property OnTask: IDelegate<TTaskDataEvent> read GetOnTask;
  end;

implementation

{ IChildObject }

type
  IChildObject = interface
  ['{93EC0DB0-9D56-4E67-AD21-C2FA7C04A08C}']
    function GetParent: IStorageContainer;
    procedure SetParent(Value: IStorageContainer);
    property Parent: IStorageContainer read GetParent write SetParent;
  end;

{ TS3Object }

  TS3Object = class(TInterfacedObject)
  private
    FOwner: TS3Manager;
    FName: string;
    FChecked: Boolean;
  protected
    function GetBucket: IBucket;
    function GetChecked: Boolean;
    procedure SetChecked(Value: Boolean);
    function Contains(Obj: IStorageObject): Boolean;
    property Owner: TS3Manager read FOwner;
  public
    constructor Create(Owner: TS3Manager; Name: string); virtual;
  end;

{ TBucket }

  TBucket = class(TS3Object, IStorageObject, IStorageContainer, IBucket)
  private
    FEndPoint: string;
    FRegion: string;
    FNull: INullObject;
    FQueried: Boolean;
    FList: IList<IStorageObject>;
    { IStorageObject }
    function GetAcl: string;
    procedure SetAcl(const Value: string);
    function GetName: string;
    function GetParent: IStorageContainer;
    procedure Refresh;
    { IStorageContainer }
    function GetQueried: Boolean;
    function GetCount: Integer;
    function GetItem(Index: Integer): IStorageObject;
    function GetEnumerator: IEnumerator<IStorageObject>;
    procedure Query;
    { IBucket }
    function GetEndPoint: string;
    function GetRegion: string;
    function GetNull: INullObject;
  public
    constructor Create(Owner: TS3Manager; Name: string); override;
  end;

{ TFolder }

  TFolder = class(TS3Object, IStorageObject, IStorageContainer, IFolder, IChildObject)
  private
    FParent: IStorageContainer;
    FQueried: Boolean;
    FOpened: Boolean;
    FNull: INullObject;
    FList: IList<IStorageObject>;
    { IStorageObject }
    function GetAcl: string;
    procedure SetAcl(const Value: string);
    function GetName: string;
    function GetParent: IStorageContainer;
    procedure Refresh;
    { IStorageContainer }
    function GetQueried: Boolean;
    function GetCount: Integer;
    function GetItem(Index: Integer): IStorageObject;
    function GetEnumerator: IEnumerator<IStorageObject>;
    procedure Query;
    { IFolder }
    function GetOpened: Boolean;
    procedure SetOpened(Value: Boolean);
    function GetNull: INullObject;
    { IChildObject }
    procedure SetParent(Value: IStorageContainer);
  public
    constructor Create(Owner: TS3Manager; Name: string); override;
  end;

{ TContent }

  TContent = class(TS3Object, IStorageObject, IContent, IChildObject)
  private
    FParent: IStorageContainer;
    FSize: Cardinal;
    FModified: TDateTime;
    FStorageClass: string;
    { IStorageObject }
    function GetAcl: string;
    procedure SetAcl(const Value: string);
    function GetName: string;
    function GetParent: IStorageContainer;
    procedure Refresh;
    { IContent }
    function GetSize: Cardinal;
    function GetModified: TDateTime;
    function GetStorageClass: string;
    function Presign(Expires: Integer = 0; Shorten: Boolean = False): string;
    { IChildObject }
    procedure SetParent(Value: IStorageContainer);
  end;

{ TNullObject }

  TNullObject = class(TS3Object, IStorageObject, IChildObject, INullObject)
  private
    FParent: IStorageContainer;
    { IStorageObject }
    function GetAcl: string;
    procedure SetAcl(const Value: string);
    function GetName: string;
    function GetParent: IStorageContainer;
    procedure Refresh;
    { IChildObject }
    procedure SetParent(Value: IStorageContainer);
  end;

{ TS3Object }

constructor TS3Object.Create(Owner: TS3Manager; Name: string);
begin
  inherited Create;
  FOwner := Owner;
  FName := Name;
end;

function TS3Object.GetBucket: IBucket;
var
  Obj: IStorageObject;
begin
  Obj := Self as IStorageObject;
  while Obj.Parent <> nil do
    Obj := Obj.Parent;
  Result := Obj as IBucket;
end;

function TS3Object.GetChecked: Boolean;
begin
  Result := FChecked;
end;

procedure TS3Object.SetChecked(Value: Boolean);
begin
  FChecked := Value;
end;

function TS3Object.Contains(Obj: IStorageObject): Boolean;
var
  Folder: IFolder;
  Container: IStorageContainer;
  Item: IStorageObject;
begin
  Result := False;
  if Obj = nil then
    Exit;
  if (Obj is INullObject) and (Self is IFolder) then
  begin
    Folder := Self as IFolder;
    Result := Folder.Null = Obj as INullObject;
    if Result then
      Exit;
  end;
  if Self is IStorageContainer then
  begin
    Container := Self as IStorageContainer;
    for Item in Container do
      if Item = Obj then
        Exit(True)
      else if Item is IStorageContainer then
      begin
        Result := (Item as IStorageContainer).Contains(Obj);
        if Result then
          Exit(True);
      end;
  end;
end;

{ TBucket }

constructor TBucket.Create(Owner: TS3Manager; Name: string);
begin
  inherited Create(Owner, Name);
  FList := TInterfaces<IStorageObject>.Create;
end;

function TBucket.GetAcl: string;
begin
  Result := '';
end;

procedure TBucket.SetAcl(const Value: string);
begin

end;

function TBucket.GetName: string;
begin
  Result := FName;
end;

function TBucket.GetParent: IStorageContainer;
begin
  Result := nil;
end;

procedure TBucket.Refresh;
begin
  FQueried := False;
  FList.Clear;
  Query;
end;

function TBucket.GetQueried: Boolean;
begin
  Result := FQueried;
end;

function TBucket.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TBucket.GetItem(Index: Integer): IStorageObject;
begin
  Result := FList[Index];
end;

function TBucket.GetEnumerator: IEnumerator<IStorageObject>;
begin
  Result := FList.GetEnumerator;
end;

procedure TBucket.Query;
var
  Request: TS3Request;
  Data: TTaskData;
begin
  if FQueried then
    Exit;
  if FRegion = '' then
    Exit;
  FQueried := True;
  Request := Owner.S3.ListObjects(FName, '', '', '/');
  Data := TTaskData.Create(Self as IBucket, taskListBucketObjects, 'List ' + FName);
  Owner.TaskStart(Request, Data);
end;

function TBucket.GetEndPoint: string;
begin
  Result := FEndPoint;
end;

function TBucket.GetRegion: string;
begin
  Result := FRegion;
end;

function TBucket.GetNull: INullObject;
var
  Child: IChildObject;
begin
  if FNull = nil then
  begin
    FNull := TNullObject.Create(Owner, 'Empty');
    Child := FNull as IChildObject;
    Child.Parent := Self;
  end;
  Result := FNull;
end;

{ TFolder }

constructor TFolder.Create(Owner: TS3Manager; Name: string);
begin
  inherited Create(Owner, Name);
  FList := TInterfaces<IStorageObject>.Create;
end;

{ IStorageObject.TFolder }

function TFolder.GetAcl: string;
begin
  Result := '';
end;

procedure TFolder.SetAcl(const Value: string);
begin

end;

function TFolder.GetName: string;
begin
  Result := FName;
end;

function TFolder.GetParent: IStorageContainer;
begin
  Result := FParent;
end;

procedure TFolder.Refresh;
begin
  FList.Clear;
  FQueried := False;
  Query;
end;

{ TFolder.IStorageContainer }

function TFolder.GetQueried: Boolean;
begin
  Result := FQueried;
end;

function TFolder.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TFolder.GetItem(Index: Integer): IStorageObject;
begin
  Result := FList[Index];
end;

function TFolder.GetEnumerator: IEnumerator<IStorageObject>;
begin
  Result := FList.GetEnumerator;
end;

function GetPrefix(Folder: IFolder): string;
begin
  Result := Folder.Name + '/';
  if Folder.Parent is IFolder then
    Result := GetPrefix(Folder.Parent as IFolder) + Result;
end;

function GetPrefixMin(Folder: IFolder): string;
begin
  Result := GetPrefix(Folder);
  Result.Length := Result.Length - 1;
end;

procedure TFolder.Query;
var
  Request: TS3Request;
  Folder: IFolder;
  Bucket: IBucket;
  Region: string;
  Prefix: string;
  Data: TTaskData;
begin
  if FQueried then
    Exit;
  Folder := Self as IFolder;
  Bucket := Folder.Bucket;
  Region := Bucket.Region;
  if Region = '' then
    Exit;
  FQueried := True;
  Prefix := GetPrefix(Folder);
  Request := Owner.S3.ListObjects(Bucket.Name, '', Prefix, '/');
  Prefix.Length := Prefix.Length - 1;
  Data := TTaskData.Create(Self as IFolder, taskListFolderObjects, 'Listing objects in folder ' +
    GetBucket.Name + '/' + GetPrefixMin(Folder));
  Owner.TaskStart(Request, Data);
end;

{ TFolder.IFolder }

function TFolder.GetOpened: Boolean;
begin
  Result := FOpened;
end;

procedure TFolder.SetOpened(Value: Boolean);
begin
  FOpened := Value;
  if FOpened then
    Query;
end;

function TFolder.GetNull: INullObject;
var
  Child: IChildObject;
begin
  if FNull = nil then
  begin
    FNull := TNullObject.Create(Owner, 'Empty');
    Child := FNull as IChildObject;
    Child.Parent := Self;
  end;
  Result := FNull;
end;

{ TFolder.IChild }

procedure TFolder.SetParent(Value: IStorageContainer);
begin
  FParent := Value;
end;

{ TContent.IStorageObject }

function TContent.GetAcl: string;
begin
  Result := '';
end;

procedure TContent.SetAcl(const Value: string);
begin

end;

function TContent.GetName: string;
begin
  Result := FName;
end;

function TContent.GetParent: IStorageContainer;
begin
  Result := FParent;
end;

procedure TContent.Refresh;
begin

end;

{ TContent.IFile }

function TContent.GetSize: Cardinal;
begin
  Result := FSize;
end;

function TContent.GetModified: TDateTime;
begin
  Result := FModified;
end;

function TContent.GetStorageClass: string;
begin
  Result := FStorageClass;
end;

function TContent.Presign(Expires: Integer = 0; Shorten: Boolean = False): string;
var
  Path: string;
begin
  Path := FName;
  if GetParent is IFolder then
    Path := GetPrefixMin(GetParent as IFolder) + '/' + Path;
  Result := Owner.FS3.Presign(GetBucket.Name, Path, Expires);
  if Shorten then
  begin

  end;
end;

{ TContent.IChildObject }

procedure TContent.SetParent(Value: IStorageContainer);
begin
  FParent := Value;
end;

{ TNullObject.IStorageObject }

function TNullObject.GetAcl: string;
begin
  REsult := '';
end;

procedure TNullObject.SetAcl(const Value: string);
begin
end;

function TNullObject.GetName: string;
begin
  Result := 'Empty';
end;

function TNullObject.GetParent: IStorageContainer;
begin
  Result := FParent;
end;

procedure TNullObject.Refresh;
begin
end;

{ TNullObject.IChildObject }

procedure TNullObject.SetParent(Value: IStorageContainer);
begin
  FParent := Value;
end;

{ TTaskData }

constructor TTaskData.Create(Target: IStorageObject; Kind: TTaskKind; const Message: string);
begin
  inherited Create;
  FTarget := Target;
  FKind := Kind;
  FMessage := Message;
end;

{ TS3Manager }

constructor TS3Manager.Create(Config: TS3ConfigFactory);
begin
  inherited Create;
  FS3 := TS3Methods.Create(Config);
end;

destructor TS3Manager.Destroy;
begin
  Shutdown;
  while FBusyCount > 0 do
    PumpMessages;
  inherited Destroy;
end;

procedure TS3Manager.Refresh;
var
  Request: TS3Request;
  Data: TTaskData;
begin
  Buckets.Clear;
  Request := S3.ListBuckets;
  Data := TTaskData.Create(nil, taskListBuckets, 'List buckets on service ' + S3.Config.EndPoint);
  TaskStart(Request, Data);
end;

procedure TS3Manager.Shutdown;
var
  T: IAsyncTask;
begin
  if FBusyCount > 0 then
    for T in FTasks do
      T.Cancel;
end;

procedure TS3Manager.DoTask(Data: TTaskData);
var
  Event: TTaskDataEvent;
begin
  for Event in FOnTask do Event(Self, Data);
end;

procedure TS3Manager.DoBusyChange(IsBusy: Boolean);
var
  Event: TBusyChangeEvent;
begin
  for Event in FOnBusyChange do Event(Self, IsBusy);
end;

procedure TS3Manager.TaskStart(const Request: TS3Request; Data: TObject);
var
  Task: IAsyncDocTask;
begin
  Task := NewDocTask(TaskComplete, Data, True);
  Tasks.Add(Task);
  S3.SendAsync(Request, Task);
  DoTask(Data as TTaskData);
  Inc(FBusyCount);
  if FBusyCount = 1 then
    DoBusyChange(True);
end;

{ All completion logic is handled by the manager }

procedure TS3Manager.ListBuckets(Data: TTaskData; Response: IDocument);
var
  Bucket: IBucket;
  Request: TS3Request;
  List: INodeList;
  N: INode;
  S: string;
begin
  List := Response.Root.SelectList('//Bucket/Name');
  if List.Count = 1 then
    S := ''
  else
    S := 's';
  Data.Message := Format('Service %s has %d bucket%s', [S3.Config.EndPoint, List.Count, S]);
  for N in List do
  begin
    Bucket := TBucket.Create(Self, N.Text);
    Request := S3.GetBucketLocation(Bucket.Name);
    Data := TTaskData.Create(Bucket, taskGetBucketLocation, 'Querying region of bucket ' + Bucket.Name);
    Buckets.Add(Bucket);
    TaskStart(Request, Data);
  end;
end;

procedure TS3Manager.GetBucketLocation(Data: TTaskData; Response: IDocument);
var
  Bucket: IBucket;
  BucketObj: TBucket;
  Region: string;
begin
  Bucket := Data.Target as IBucket;
  BucketObj := Bucket as TBucket;
  Region := Response.Document.Root.Text;
  if Region = '' then
    Region := S3.Config.Region;
  BucketObj.FRegion := Region;
  BucketObj.FEndPoint := S3.Config.EndPoint(Region);
  S3.AddRegion(Bucket.Name, Region);
  Data.Message := Format('Bucket %s is located in region %s', [Bucket.Name, Bucket.Region]);
end;

function CleanName(Name: string): string;
begin
  Result := Trim(Name);
  if Result.EndsWith('/') then
    SetLength(Result, Length(Result) - 1);
  Result := Result.LastOf('/');
end;

function ParseDateTime(const Value: string): TDateTime;
var
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
begin
  Year := StrToInt(Copy(Value, 1, 4));
  Month := StrToInt(Copy(Value, 6, 2));
  Day := StrToInt(Copy(Value, 9, 2));
  Hour := StrToInt(Copy(Value, 12, 2));
  Min := StrToInt(Copy(Value, 15, 2));
  Sec := StrToInt(Copy(Value, 18, 2));
  MSec := StrToInt(Copy(Value, 21, 3));
  Result := UniversalTimeToLocal(EncodeDateTime(Year, Month, Day, Hour, Min, Sec, MSec));
end;

procedure TS3Manager.ListBucketObjects(Data: TTaskData; Response: IDocument);
var
  Bucket: IBucket;
  BucketObj: TBucket;
  Folder: IFolder;
  Content: IContent;
  ContentObj: TContent;
  N: INode;
  F: IFiler;
  S: string;
begin
  Bucket := Data.Target as IBucket;
  BucketObj := Bucket as TBucket;
  for N in Response.Root.SelectList('//CommonPrefixes/Prefix') do
  begin
    S := CleanName(N.Text);
    if S = '' then
      Continue;
    Folder := TFolder.Create(Self, S);
    (Folder as IChildObject).SetParent(Bucket);
    BucketObj.FList.Add(Folder) ;
  end;
  for N in Response.Root.SelectList('//Contents') do
  begin
    F := N.Filer;
    S := CleanName(F.ReadStr('Key'));
    if S = '' then
      Continue;
    Content := TContent.Create(Self, S);
    (Content as IChildObject).SetParent(Bucket);
    ContentObj := Content as TContent;
    ContentObj.FSize := F.ReadInt64('Size');
    ContentObj.FModified := ParseDateTime(F.ReadStr('LastModified'));
    ContentObj.FStorageClass := F.ReadStr('StorageClass');
    BucketObj.FList.Add(Content) ;
  end;
  if Bucket.Count = 1 then
    S := ''
  else
    S := 's';
  Data.Message := Format('Found %d object%s in bucket %s', [Bucket.Count, S, Bucket.Name]);
end;

procedure TS3Manager.ListFolderObjects(Data: TTaskData; Response: IDocument);
var
  Folder: IFolder;
  FolderObj: TFolder;
  NewFolder: IFolder;
  Content: IContent;
  ContentObj: TContent;
  N: INode;
  F: IFiler;
  S: string;
begin
  Folder := Data.Target as IFolder;
  FolderObj := Folder as TFolder;
  for N in Response.Root.SelectList('//CommonPrefixes/Prefix') do
  begin
    S := CleanName(N.Text);
    if S = '' then
      Continue;
    NewFolder := TFolder.Create(Self, S);
    (NewFolder as IChildObject).SetParent(Folder);
    FolderObj.FList.Add(NewFolder);
  end;
  for N in Response.Root.SelectList('//Contents') do
  begin
    F := N.Filer;
    S := CleanName(F.ReadStr('Key'));
    if S = '' then
      Continue;
    if (F.ReadInt64('Size') = 0) and (S = Folder.Name) then
      Continue;
    Content := TContent.Create(Self, S);
    (Content as IChildObject).SetParent(Folder);
    ContentObj := Content as TContent;
    ContentObj.FSize := F.ReadInt64('Size');
    ContentObj.FModified := ParseDateTime(F.ReadStr('LastModified'));
    ContentObj.FStorageClass := F.ReadStr('StorageClass');
    FolderObj.FList.Add(Content);
  end;
  if Folder.Count = 1 then
    S := ''
  else
    S := 's';
  Data.Message := Format('Found %d object%s in folder %s/%s', [Folder.Count, S, Folder.Bucket.Name, GetPrefixMin(Folder)]);
end;

procedure TS3Manager.TaskComplete(Task: IAsyncTask; Result: IDocument);
var
  Data: TTaskData;
begin
  Data := Task.Data as TTaskData;
  if Task.Status = asyncCanceled then
    Data.Error := Result
  else if Task.Status = asyncFail then
    Data.Error := Result
  else if Task.Status = asyncSuccess then
    case Data.Kind of
      taskListBuckets: ListBuckets(Data, Result);
      taskGetBucketLocation: GetBucketLocation(Data, Result);
      taskListBucketObjects: ListBucketObjects(Data, Result);
      taskListFolderObjects: ListFolderObjects(Data, Result);
    end;
  DoTask(Data);
  Dec(FBusyCount);
  if FBusyCount = 0 then
    DoBusyChange(False);
end;

function TS3Manager.GetOnBusyChange: IDelegate<TBusyChangeEvent>;
begin
  Result := FOnBusyChange;
end;

function TS3Manager.GetOnTask: IDelegate<TTaskDataEvent>;
begin
  Result := FOnTask;
end;

function TS3Manager.GetBusy: Boolean;
begin
  Result := FBusyCount > 0;
end;

function TS3Manager.GetBuckets: IBuckets;
begin
  if FBuckets = nil then
    FBuckets := TInterfaces<IBucket>.Create;
  Result := FBuckets;
end;

function TS3Manager.GetTasks: ITasks;
begin
  if FTasks = nil then
    FTasks := TInterfaces<IAsyncTask>.Create;
  Result := FTasks;
end;

end.

