unit LrProfiles;

interface

{$WARN SYMBOL_PLATFORM OFF}

uses
	SysUtils, Classes, LrPersistComponent;

type
	TLrProfiles = class;
	TLrProfileIdent = string;
	//
	TLrProfile = class(TCollectionItem)
	private
		FIdent: TLrProfileIdent;
		FName: string;
		FProfiles: TLrProfiles;
	protected
		function GetName: string;
		function RandomIdent: TLrProfileIdent;
		procedure GuaranteeUniqueIdent;
	public
		constructor Create(inCollection: TCollection); override;
		procedure AssignProfile(inProfile: TLrProfile); virtual;
		property Profiles: TLrProfiles read FProfiles;
	published
		property Ident: TLrProfileIdent read FIdent write FIdent;
		property Name: string read GetName write FName;
	end;
	//
	TLrProfileClass = class of TLrProfile;
	//
	TLrProfiles = class(TOwnedCollection)
	protected
		function GetProfiles(inIndex: Integer): TLrProfile;
		procedure SetProfiles(inIndex: Integer; const Value: TLrProfile);
	public
		constructor Create(AOwner: TPersistent; ItemClass: TLrProfileClass);
		function AddProfile: TLrProfile;
		function FindProfile(const inIdent: TLrProfileIdent): TLrProfile;
	public
		property Profiles[inIndex: Integer]: TLrProfile read GetProfiles
			write SetProfiles; default;
	end;
	//
	ELrProfileException = class(Exception)
	end;
	//
	TLrProfileMgr = class(TLrPersistComponent)
	private
		FProfiles: TLrProfiles;
		FDefaultIdent: string;
	protected
		function GetCount: Integer;
		function GetDefaultIdent: string;
		function GetDefaultProfile: TLrProfile;
		procedure InitDefaultProfile;
		procedure SetProfiles(const Value: TLrProfiles);
	public
		constructor Create(ItemClass: TLrProfileClass;
			const inFilename: string = ''); reintroduce;
		destructor Destroy; override;
		function GetProfileByIdent(const inIdent: TLrProfileIdent): TLrProfile;
		function GetProfileByName(const inName: string): TLrProfile;
		procedure ProfileNamesToStrings(inStrings: TStrings);
		procedure RemoveProfile(inProfile: TLrProfile);
	public
		property Count: Integer read GetCount;
		property DefaultProfile: TLrProfile read GetDefaultProfile;
	published
		property DefaultIdent: string read GetDefaultIdent
			write FDefaultIdent;
		property Profiles: TLrProfiles read FProfiles
			write SetProfiles;
	end;

implementation

uses
	StrUtils;

{ TLrProfile }

constructor TLrProfile.Create(inCollection: TCollection);
begin
	inherited;
	FProfiles := TLrProfiles(inCollection);
	GuaranteeUniqueIdent;
end;

function TLrProfile.RandomIdent: TLrProfileIdent;
begin
	Result := IntToHex(Random($FFFF), 4);
end;

procedure TLrProfile.GuaranteeUniqueIdent;
var
	id: TLrProfileIdent;
begin
	if Collection <> nil then
	begin
		Ident := '';
		repeat
			id := RandomIdent;
		until (Profiles.FindProfile(id) = nil);
		Ident := id;
	end;
end;

procedure TLrProfile.AssignProfile(inProfile: TLrProfile);
begin
	Ident := inProfile.FIdent;
	Name := inProfile.FName;
end;

function TLrProfile.GetName: string;
begin
	Result := FName;
	if Result = '' then
		Result := '(untitled)';
end;

{ TLrProfiles }

constructor TLrProfiles.Create(AOwner: TPersistent; ItemClass: TLrProfileClass);
begin
	inherited Create(AOwner, ItemClass);
end;

function TLrProfiles.FindProfile(const inIdent: TLrProfileIdent): TLrProfile;
var
	i: Integer;
begin
	for i := 0 to Count - 1 do
	begin
		Result := Profiles[i];
		if (Result.Ident = inIdent) then
			exit;
	end;
	Result := nil;
end;

function TLrProfiles.AddProfile: TLrProfile;
begin
	Result := TLrProfile(Add);
end;

function TLrProfiles.GetProfiles(inIndex: Integer): TLrProfile;
begin
	Result := TLrProfile(Items[inIndex]);
end;

procedure TLrProfiles.SetProfiles(inIndex: Integer; const Value: TLrProfile);
begin
	Items[inIndex].Assign(Value);
end;

{ TLrProfileMgr }

constructor TLrProfileMgr.Create(ItemClass: TLrProfileClass;
	const inFilename: string);
begin
	inherited Create(nil);
	FProfiles := TLrProfiles.Create(Self, ItemClass);
	LoadFromFile(inFilename);
end;

destructor TLrProfileMgr.Destroy;
begin
	Save;
	Profiles.Free;
	inherited;
end;

procedure TLrProfileMgr.InitDefaultProfile;
begin
	if (Profiles.Count > 0) then
		DefaultIdent := Profiles[0].Ident;
end;

function TLrProfileMgr.GetDefaultIdent: TLrProfileIdent;
begin
	if (FDefaultIdent = '') then
		InitDefaultProfile;
	Result := FDefaultIdent;
end;

function TLrProfileMgr.GetDefaultProfile: TLrProfile;
begin
	Result := GetProfileByIdent(DefaultIdent);
end;

procedure TLrProfileMgr.SetProfiles(const Value: TLrProfiles);
begin
	FProfiles.Assign(Value);
end;

procedure TLrProfileMgr.ProfileNamesToStrings(inStrings: TStrings);
var
	i: Integer;
begin
	for i := 0 to Profiles.Count - 1 do
		inStrings.Add(Profiles[i].Name);
end;

function TLrProfileMgr.GetProfileByIdent(const inIdent: string): TLrProfile;
var
	i: Integer;
begin
	for i := 0 to Profiles.Count - 1 do
	begin
		Result := Profiles[i];
		if Result.Ident = inIdent then
			exit;
	end;
	raise ELrProfileException.Create('ProfileByIdent: Bad Ident');
end;

function TLrProfileMgr.GetProfileByName(const inName: string): TLrProfile;
var
	i: Integer;
begin
	for i := 0 to Profiles.Count - 1 do
	begin
		Result := Profiles[i];
		if Result.Name = inName then
			exit;
	end;
	raise ELrProfileException.Create('ProfileByName: Bad Name');
end;

procedure TLrProfileMgr.RemoveProfile(inProfile: TLrProfile);
begin
	if (inProfile.Collection = Profiles) then
	begin
		if (FDefaultIdent = inProfile.Ident) then
			DefaultIdent := '';
		Profiles.Delete(inProfile.Index);
	end;
end;

function TLrProfileMgr.GetCount: Integer;
begin
	Result := Profiles.Count;
end;

end.
