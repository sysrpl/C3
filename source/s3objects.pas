(********************************************************)
(*                                                      *)
(*  Codebot Simple Storage                              *)
(*  http://www.getlazarus.org/apps/s3                   *)
(*  Modified October 2023                               *)
(*                                                      *)
(*  Released under GNU GPLv3 license                    *)
(*                                                      *)
(********************************************************)

unit S3Objects;

{$mode delphi}

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

{ List types }

  IBuckets = IList<IBucket>;
  ITasks = IList<IAsyncTask>;

{ IStorageObject }

  IStorageObject = interface
  ['{C508CA02-A75F-4CC6-87DD-F444C56AE30C}']
    function GetAcl: string;
    procedure SetAcl(const Value: string);
    function GetChecked: Boolean;
    procedure SetChecked(Value: Boolean);
    function GetName: string;
    function GetParent: IStorageContainer;
    procedure Refresh;
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
    property EndPoint: string read GetEndPoint;
    property Region: string read GetRegion;
  end;

{ IFolder }

  IFolder = interface(IStorageContainer)
  ['{170A5139-953C-4AE0-9C15-47C27F368D1A}']
    function GetOpened: Boolean;
    procedure SetOpened(Value: Boolean);
    property Opened: Boolean read GetOpened write SetOpened;
  end;

{ IContent is a file, but file is a keyword so we use IContent as a synonym }

  IContent = interface(IStorageObject)
  ['{02235761-F628-4717-B175-7CF38CBE8B6B}']
    function GetSize: Cardinal;
    function GetModified: TDateTime;
    function GetStorageClass: string;
    property Size: Cardinal read GetSize;
    property Modified: TDateTime read GetModified;
    property StorageClass: string read GetStorageClass;
  end;

{ TTaskData }

  TTaskKind = (
    taskListBuckets,
    taskGetBucketLocation,
    taskListBucketObjects
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
    procedure ListBuckets(Response: IDocument);
    procedure GetBucketLocation(Data: TTaskData; Response: IDocument);
    procedure ListBucketObjects(Data: TTaskData; Response: IDocument);
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
    procedure SetParent(Value: IStorageContainer);
  end;

{ TS3Object }

  TS3Object = class(TInterfacedObject)
  private
    FOwner: TS3Manager;
    FName: string;
    FChecked: Boolean;
  protected
    function GetChecked: Boolean;
    procedure SetChecked(Value: Boolean);
    property Owner: TS3Manager read FOwner;
  public
    constructor Create(Owner: TS3Manager; Name: string); virtual;
  end;

{ TBucket }

  TBucket = class(TS3Object, IStorageObject, IStorageContainer, IBucket)
  private
    FEndPoint: string;
    FRegion: string;
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
  public
    constructor Create(Owner: TS3Manager; Name: string); override;
  end;

{ TFolder }

  TFolder = class(TS3Object, IStorageObject, IStorageContainer, IFolder, IChildObject)
  private
    FParent: IStorageContainer;
    FQueried: Boolean;
    FOpened: Boolean;
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

function TS3Object.GetChecked: Boolean;
begin
  Result := FChecked;
end;

procedure TS3Object.SetChecked(Value: Boolean);
begin
  FChecked := Value;
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
  Data := TTaskData.Create(Self as IBucket, taskListBucketObjects, 'Listing objects in ' +
    Owner.S3.Config.EndPoint(FRegion) + '/' + FName);
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

procedure TFolder.Query;
begin

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

procedure TFolder.SetParent(Value: IStorageContainer);
begin
  FParent := Value;
end;

{ TFile.IStorageObject }

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

{ TFile.IFile }

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

{ TFile.IChildObject }

procedure TContent.SetParent(Value: IStorageContainer);
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
  Data := TTaskData.Create(nil, taskListBuckets, 'Listing buckets on ' + S3.Config.EndPoint);
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

procedure TS3Manager.ListBuckets(Response: IDocument);
var
  Bucket: IBucket;
  Request: TS3Request;
  Data: TTaskData;
  N: INode;
begin
  for N in Response.Root.SelectList('//Bucket/Name') do
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
end;

function CleanName(Name: string): string;
begin
  Result := Trim(Name);
  if Result.EndsWith('/') then
    SetLength(Result, Length(Result) - 1);
  Result := Result.LastOf('/');
  if Name <> Result then
    WriteLn('Name cleaned: ', Name);
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
    S := F.ReadStr('Key');
    if S = '' then
      Continue;
    Content := TContent.Create(Self, CleanName(S));
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
  Data.Message := Data.Message + Format(' (found %d object%s)', [Bucket.Count, S]);
end;

procedure TS3Manager.TaskComplete(Task: IAsyncTask; Result: IDocument);
var
  Data: TTaskData;
begin
  Data := Task.Data as TTaskData;
  if Task.Status = asyncCanceled then
  begin
    Data.Error := NewDocument;
    Data.Error.Force('Cancelled');
  end
  else if Task.Status = asyncFail then
    Data.Error := Result
  else if Task.Status = asyncSuccess then
    case Data.Kind of
      taskListBuckets: ListBuckets(Result);
      taskGetBucketLocation: GetBucketLocation(Data, Result);
      taskListBucketObjects: ListBucketObjects(Data, Result);
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

