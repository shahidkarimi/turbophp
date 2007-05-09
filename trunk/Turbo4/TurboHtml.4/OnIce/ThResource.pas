unit ThResource;

interface

uses
	Messages, SyncObjs, SysUtils, Classes, Controls, Graphics,
	ThComponent, ThComponentIterator, ThContent, ThAction;

type
	TThResource = class;
	//
	TThResourceEvent = procedure(inSender: TThResource;
		inAction: TThAction) of object;
	//
	TThResource = class(TThCustomResource)
	private
		FOnBeforeFulfill: TThResourceEvent;
		FPrefix: string;
		FSession: TThSession;
		FUrl: string;
	protected
		function GetPrefix: string; override;
		procedure SetSession(const Value: TThSession);
		procedure SetUrl(const Value: string); virtual;
	protected
		procedure DoBeforeFulfill(inAction: TThAction);
		procedure PopUrl(inAction: TThAction); virtual;
		procedure UnassignComponent(AComponent: TComponent); override;
	protected
		property Prefix: string read GetPrefix write FPrefix;
		property OnBeforeFulfill: TThResourceEvent read FOnBeforeFulfill
			write FOnBeforeFulfill;
		property Session: TThSession read FSession write SetSession;
	public
		//procedure Fulfill(inAction: TThAction); virtual; abstract;
		procedure FulfillRequest(inAction: TThAction); virtual;
		function Requested(inAction: TThAction): Boolean; virtual;
	public
		property Url: string read FUrl write SetUrl;
	end;
	//
	TThResourceList = class(TList)
	private
		FContainer: TComponent;
	protected
		procedure ListResources;
		procedure SetContainer(const Value: TComponent);
	protected
		property Container: TComponent read FContainer write SetContainer;
	public
		constructor Create(inContainer: TComponent = nil);
	end;
	//
	TThResourceIterator = class(TThIterator)
	private
		FResourceList: TThResourceList;
	protected
		function GetResource: TThResource;
		function Eof: Boolean; override;
		property ResourceList: TThResourceList read FResourceList
			write FResourceList;
	public
		constructor Create(inList: TThResourceList);
		property Resource: TThResource read GetResource;
	end;
	//
	TThResourceDispatcher = class(TThComponent)
	private
		FResourceList: TThResourceList;
	protected
		procedure Clear;
		function CreateIterator: TThResourceIterator;
		function GetResourceList: TThResourceList;
		procedure NeedResourceList;
		property ResourceList: TThResourceList read GetResourceList
			write FResourceList;
	public
		destructor Destroy; override;
//		procedure DispatchAction(inAction: TThAction);
	end;
	//
	TThFileResourceBase = class(TThResource)
	private
		FFolder: string;
		FFilename: string;
		FContentType: string;
	protected
		function GetPath: string; virtual;
		function GetFilename: string; virtual;
		procedure SetContentType(const Value: string);
		procedure SetFilename(const Value: string);
		procedure SetFolder(const Value: string);
	protected
		property ContentType: string read FContentType write SetContentType;
		property Filename: string read GetFilename write SetFilename;
		property Folder: string read FFolder write SetFolder;
	public
//		procedure Fulfill(inAction: TThAction); override;
	public
		property Path: string read GetPath;
	end;
	//
	TThFileResource = class(TThFileResourceBase)
	published
		property ContentType;
		property Filename;
		property Folder;
//		property OnBeforeFulfill;
		property Url;
	end;
	//
	TThFolderResource = class(TThFileResourceBase)
	public
//		procedure Fulfill(inAction: TThAction); override;
	published
		property ContentType;
		property Folder;
//		property OnBeforeFulfill;
		property Url;
	end;
	//
	TThImageFileResource = class(TThFileResourceBase)
	published
		property Filename;
		property Folder;
//		property OnBeforeFulfill;
		property Url;
	end;
	//
	TThPictureResourceBase = class(TThResource)
	private
		FPicture: TPicture;
	protected
		procedure SetPicture(const Value: TPicture);
	public
		constructor Create(inOwner: TComponent); override;
		destructor Destroy; override;
//		procedure Fulfill(inAction: TThAction); override;
	public
		property Picture: TPicture read FPicture write SetPicture;
	end;
	//
	TThPictureResource = class(TThPictureResourceBase)
	published
//		property OnBeforeFulfill;
		property Picture;
		property Url;
	end;
	//
{
	TThModuleClass = class of TComponent;
	TThGetModuleClassEvent = function(
		inSender: TThResource): TThModuleClass of object;
	//
	TThModuleResource = class(TThResource)
	private
		FAction: TThAction;
		FOnGetModuleClass: TThGetModuleClassEvent;
		FSection: TCriticalSection;
		FUsesVcl: Boolean;
	protected
		procedure SetOnGetModuleClass(const Value: TThGetModuleClassEvent);
		procedure SetUsesVcl(const Value: Boolean);
	protected
		function DoGetModuleClass: TThModuleClass;
		procedure FulfillByModule(inAction: TThAction);
		procedure SyncFulfill;
		procedure VclFulfill(inAction: TThAction);
	protected
		property Action: TThAction read FAction write FAction;
		property Section: TCriticalSection read FSection;
	public
		constructor Create(inOwner: TComponent); override;
		destructor Destroy; override;
		procedure Fulfill(inAction: TThAction); override;
	published
		property OnBeforeFulfill;
		property OnGetModuleClass: TThGetModuleClassEvent read FOnGetModuleClass
			write SetOnGetModuleClass;
		property Url;
		property UsesVcl: Boolean read FUsesVcl write SetUsesVcl;
	end;
	//
	TThPageResource = class(TThResource)
	private
		FContent: TThContentBase;
		FOnBeforeOutput: TNotifyEvent;
	protected
		function GetContentText: string;
		procedure SetContent(const Value: TThContentBase);
		procedure SetOnBeforeOutput(const Value: TNotifyEvent);
	protected
		procedure DoBeforeOutput;
		procedure GenerateOutput(inAction: TThAction); virtual;
		procedure ProcessInput(inAction: TThAction); virtual;
		function TransitPage(inAction: TThAction): Boolean; virtual;
		procedure UnassignComponent(AComponent: TComponent); override;
	public
		procedure Fulfill(inAction: TThAction); override;
		property ContentText: string read GetContentText;
	published
		property Content: TThContentBase read FContent write SetContent;
		property OnBeforeFulfill;
		property OnBeforeOutput: TNotifyEvent read FOnBeforeOutput
			write SetOnBeforeOutput;
		property Session;
		property Url;
	end;
}
	//
	TThImageListResource = class(TThResource)
	private
		FImageList: TImageList;
	protected
		procedure SetImageList(const Value: TImageList);
	public
		constructor Create(inOwner: TComponent); override;
		destructor Destroy; override;
		procedure Fulfill(inAction: TThAction); override;
		function GetImageUrl(inIndex: Integer): string; virtual;
		function GetJpegStream(inIndex: Integer): TStream;
	published
		property ImageList: TImageList read FImageList write SetImageList;
		property Url;
	end;

//procedure ThDispatchAction(inOwner: TComponent; inAction: TThAction);

implementation

uses
	Masks, StrUtils, ThVclUtils, ThPathUtils{, ThComponentPool,
	ThActionWrangler, ThWebDataDictionary, ThTemplateContent,
	ThStreamContent}, ThGraphicUtils;

{
	procedure ThDispatchAction(inOwner: TComponent; inAction: TThAction);
	begin
		with TThComponentIterator.Create(inOwner) do
		try
			while Next(TThResource) and not inAction.Handled do
				TThResource(Component).FulfillRequest(inAction);
		finally
			Free;
		end;
	end;
}
{
	procedure ThDispatchAction(inOwner: TComponent; inAction: TThAction);
	begin
			with TThResourceIterator.Create(inOwner) do
			try
				while Next and not inAction.Handled do
					Resource.FulfillRequest(inAction);
			finally
				Free;
			end;
	end;
}


{
	procedure ThDispatchAction(inOwner: TComponent; inAction: TThAction);
	var
		d: TThResourceDispatcher;
		r: TThResource;
	begin
		d := TThResourceDispatcher(
			ThFindComponentByClass(inOwner, TThResourceDispatcher));
		if d <> nil then
			d.DispatchAction(inAction)
		else begin
			r := TThResource(ThFindComponentByClass(inOwner, TThResource));
			if r <> nil then
				r.FulfillRequest(inAction);
		end;
}
{
			with TThResourceDispatcher.Create(inOwner) do
			try
				DispatchAction(inAction);
			finally
				Free;
			end;
}
{
	end;
}

{ TThResource }

function TThResource.Requested(inAction: TThAction): Boolean;
begin
	Result := AnsiStartsStr(Prefix, inAction.Document);
end;

procedure TThResource.PopUrl(inAction: TThAction);
begin
	inAction.Document := Copy(inAction.Document, Length(Prefix) + 1, MAXINT);
end;

procedure TThResource.FulfillRequest(inAction: TThAction);
begin
	try
		if Requested(inAction) then
		begin
			PopUrl(inAction);
			DoBeforeFulfill(inAction);
			inAction.UseResource(Self);
{
			PopUrl(inAction);
			OpenSession(inAction);
			DoBeforeFulfill(inAction);
			Fulfill(inAction);
			CloseSession;
}
		end;
	except
		on E: Exception do
			inAction.Response.ContentText := E.Message;
	end;
end;

procedure TThResource.SetUrl(const Value: string);
begin
	FUrl := Value;
	Prefix := '/' + Value;
end;

procedure TThResource.DoBeforeFulfill(inAction: TThAction);
begin
	if Assigned(FOnBeforeFulfill) then
		FOnBeforeFulfill(Self, inAction);
end;

procedure TThResource.SetSession(const Value: TThSession);
begin
	ChangeComponentProp(TComponent(FSession), Value);
end;

procedure TThResource.UnassignComponent(AComponent: TComponent);
begin
	inherited;
	if FSession = AComponent then
		FSession := nil;
end;

{
procedure TThResource.OpenSession(inAction: TThAction);
begin
	if Session <> nil then
		Session.OpenSession(inAction);
end;

procedure TThResource.CloseSession;
begin
	if Session <> nil then
		Session.CloseSession;
end;
}

function TThResource.GetPrefix: string;
begin
	Result := FPrefix;
end;

{ TThResourceList }

constructor TThResourceList.Create(inContainer: TComponent);
begin
	Container := inContainer;
end;

	function CompareResources(Item1, Item2: Pointer): Integer;
	begin
		Result := Length(TThResource(Item2).Url) - Length(TThResource(Item1).Url);
	end;

procedure TThResourceList.ListResources;
begin
	Clear;
	if Container <> nil then
		with TThComponentIterator.Create(Container) do
		try
			while Next(TThResource) do
				Add(Component);
			Sort(CompareResources);
		finally
			Free;
		end;
end;

procedure TThResourceList.SetContainer(const Value: TComponent);
begin
	FContainer := Value;
	ListResources;
end;

{ TThResourceIterator }

constructor TThResourceIterator.Create(inList: TThResourceList);
begin
	ResourceList := inList;
end;

function TThResourceIterator.Eof: Boolean;
begin
	Result := (ResourceList = nil) or (Index >= ResourceList.Count);
end;

function TThResourceIterator.GetResource: TThResource;
begin
	Result := TThResource(ResourceList[Index - 1]);
end;

{ TThResourceDispatcher }

destructor TThResourceDispatcher.Destroy;
begin
	Clear;
	inherited;
end;

procedure TThResourceDispatcher.Clear;
begin
	FreeAndNil(FResourceList);
end;

procedure TThResourceDispatcher.NeedResourceList;
begin
	if FResourceList = nil then
		FResourceList := TThResourceList.Create(Owner);
end;

function TThResourceDispatcher.GetResourceList: TThResourceList;
begin
	NeedResourceList;
	Result := FResourceList;
end;

function TThResourceDispatcher.CreateIterator: TThResourceIterator;
begin
	Result := TThResourceIterator.Create(ResourceList);
end;

{
procedure TThResourceDispatcher.DispatchAction(inAction: TThAction);
begin
	with CreateIterator do
	try
		while not inAction.Handled and Next do
			Resource.FulfillRequest(inAction);
	finally
		Free;
	end;
end;
}

{ TThFileResourceBase }

function TThFileResourceBase.GetPath: string;
begin
	Result := ThAppendPath(Folder, Filename);
end;

{
procedure TThFileResourceBase.Fulfill(inAction: TThAction);
begin
	if FileExists(Path) then
	begin
		inAction.Response.ContentStream :=
			TFileStream.Create(Path, fmOpenRead, fmShareDenyNone);
		inAction.Response.ContentType := ContentType;
		inAction.Handled := true;
	end;
end;
}

function TThFileResourceBase.GetFilename: string;
begin
	Result := FFilename;
end;

procedure TThFileResourceBase.SetContentType(const Value: string);
begin
	FContentType := Value;
end;

procedure TThFileResourceBase.SetFilename(const Value: string);
begin
	FFilename := Value;
end;

procedure TThFileResourceBase.SetFolder(const Value: string);
begin
	FFolder := Value;
end;

{ TThFolderResource }

{
procedure TThFolderResource.Fulfill(inAction: TThAction);
begin
	if (inAction.Document <> '') then
		Filename := ThUrlToPath(inAction.Document)
	else
		Filename := inAction.Request.Params.Values['file'];
	inherited;
end;
}

{ TThModuleResource }

{
constructor TThModuleResource.Create(inOwner: TComponent);
begin
	inherited;
	FSection := TCriticalSection.Create;
end;

destructor TThModuleResource.Destroy;
begin
	FSection.Free;
	inherited;
end;

function TThModuleResource.DoGetModuleClass: TThModuleClass;
begin
	if Assigned(FOnGetModuleClass) then
		Result := OnGetModuleClass(Self)
	else
		Result := nil;
end;

procedure TThModuleResource.FulfillByModule(inAction: TThAction);
var
	c: TThModuleClass;
	m: TComponent;
begin
	c := DoGetModuleClass;
	if c <> nil then
	begin
		m := TurboPool[c].CheckOut;
		if m <> nil then
//		m := c.Create(Self);
			try
				ThDispatchAction(m, inAction);
			finally
				TurboPool[c].CheckIn(m);
//			m.Free;
			end;
	end;
end;

procedure TThModuleResource.SyncFulfill;
begin
	FulfillByModule(Action);
end;

procedure TThModuleResource.VclFulfill(inAction: TThAction);
begin
	Section.Enter;
	try
		Action := inAction;
		inAction.Thread.Synchronize(SyncFulfill);
	finally
		Section.Leave;
	end;
end;

procedure TThModuleResource.Fulfill(inAction: TThAction);
begin
	if UsesVcl then
		VclFulfill(inAction)
	else
		FulfillByModule(inAction);
end;

procedure TThModuleResource.SetOnGetModuleClass(
	const Value: TThGetModuleClassEvent);
begin
	FOnGetModuleClass := Value;
end;

procedure TThModuleResource.SetUsesVcl(const Value: Boolean);
begin
	FUsesVcl := Value;
end;
}

{ TThPageResource }

//function TThPageResource.TransitPage(inAction: TThAction): Boolean;
//var
//	p: TThPageResource;
//begin
{
	with inAction do
		Result := (Document <> '') and (Document <> PageName)
			and ThFindPageResource(Owner, Document, p);
	if Result then
		p.DispatchAction(inAction)
}
//	Result := false;
//end;

{
procedure TThPageResource.Fulfill(inAction: TThAction);
begin
	if not TransitPage(inAction) then
		if (inAction.Document = '') then
		begin
		//if not AnsiEndsStr('/', inAction.Document) then
			inAction.Response.Redirect(inAction.Request.Document + '/');
			inAction.Handled := true;
		end
		else begin
			ProcessInput(inAction);
			DoBeforeOutput;
			GenerateOutput(inAction);
		end;
end;

procedure TThPageResource.ProcessInput(inAction: TThAction);
begin
	if inAction.Request.Params.Count > 0 then
		with TThComponentIterator.Create(Owner) do
		try
			while Next(TThWebDataDictionary) do
				TThWebDataDictionary(Component).
					ContentFromParams(inAction.Request.Params);
		finally
			Free;
		end;
	ThUpdateTags(Owner);
end;

procedure TThPageResource.GenerateOutput(inAction: TThAction);
var
	s: TStream;
	d, t: string;
begin
	with inAction do
		if not Handled then
		begin
			Handled := true;
			if (Document = '') or (Document = Url) then
//			if (Document = '') or (Document = PageName) then
				Response.ContentText := ContentText
			else begin
				// Strip leading '/'
				//d := Copy(Document, 2, MAXINT);
				d := Document;
				if ThFindContentStream(Owner, d, s) then
					Response.ContentStream := s
				else if ThFindContent(Owner, d,	t) then
					Response.ContentText := t
				else
					Response.ContentText := ContentText;
			end;
		end;
end;

procedure TThPageResource.SetContent(const Value: TThContentBase);
begin
	ChangeComponentProp(TComponent(FContent), Value);
end;

procedure TThPageResource.UnassignComponent(AComponent: TComponent);
begin
	inherited;
	if AComponent = FContent then
		FContent := nil;
end;

function TThPageResource.GetContentText: string;
begin
	if Content <> nil then
		Result := Content.ContentText
	else
		Result := '';
end;

procedure TThPageResource.DoBeforeOutput;
begin
	if Assigned(OnBeforeOutput) then
		OnBeforeOutput(Self);
end;

procedure TThPageResource.SetOnBeforeOutput(const Value: TNotifyEvent);
begin
	FOnBeforeOutput := Value;
end;
}

{ TThPictureResourceBase }

constructor TThPictureResourceBase.Create(inOwner: TComponent);
begin
	inherited;
	FPicture := TPicture.Create;
end;

destructor TThPictureResourceBase.Destroy;
begin
	FPicture.Free;
	inherited;
end;

{
procedure TThPictureResourceBase.Fulfill(inAction: TThAction);
begin
	with inAction.Response do
	begin
		ContentStream := TMemoryStream.Create;
		FPicture.Graphic.SaveToStream(ContentStream);
		ContentStream.Position := 0;
	end;
	inAction.Handled := true;
end;
}

procedure TThPictureResourceBase.SetPicture(const Value: TPicture);
begin
	FPicture.Assign(Value);
end;

{ TThImageListResource }

constructor TThImageListResource.Create(inOwner: TComponent);
begin
	inherited;
	FImageList := TImageList.Create(Self);
	FImageList.SetSubComponent(true);
end;

destructor TThImageListResource.Destroy;
begin
	FImageList.Free;
	inherited;
end;

procedure TThImageListResource.SetImageList(const Value: TImageList);
begin
	//FImageList := Value;
end;

function TThImageListResource.GetJpegStream(inIndex: Integer): TStream;
var
	b: TBitmap;
begin
	Result := TMemoryStream.Create;
	b := TBitmap.Create;
	try
		ImageList.GetBitmap(inIndex, b);
		{$if false}
		b.SaveToStream(Result);
		Result.Position := 0;
		{$else}
		ThBitmapToJpgStream(b, Result);
		{$ifend}
	finally
		b.Free;
	end;
end;

procedure TThImageListResource.Fulfill(inAction: TThAction);
var
	ii: Integer;
begin
	if ImageList <> nil then
	begin
		ii := StrToIntDef(Copy(inAction.Document, 2, MAXINT), 0);
		inAction.ServeStream(GetJpegStream(ii), 'image/jpg');
	end;
end;

function TThImageListResource.GetImageUrl(inIndex: Integer): string;
begin
	Result := Format('%s/%d', [ Url, inIndex ]);
end;

end.
