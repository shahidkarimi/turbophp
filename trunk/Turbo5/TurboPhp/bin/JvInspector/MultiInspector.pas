unit MultiInspector;

interface

uses
	SysUtils, Classes, Dialogs, ExtDlgs, Forms, Graphics, TypInfo,
	JvInspector;

type
	TJvInspectorMultiClassItem = class(TJvInspectorClassItem)
	protected
		procedure CreateMembers; override;
	end;
	//
	TJvPropList = class(TList)
	private
		FLastMatch: PPropInfo;
		FMerged: TList;
	protected
		function GetProps(inIndex: Integer): PPropInfo;
		function CompareProp(inPropA, inPropB: PPropInfo): Boolean;
	public
		constructor Create;
		destructor Destroy; override;
		function FindMatch(inProp: PPropInfo): Boolean;
		procedure AssignMerged;
		procedure MergeLastMatch;
		property Props[inIndex: Integer]: PPropInfo read GetProps; default;
	end;
	//
	TInstanceArray = array of TObject;
	//
	TJvInspectorMultiPropData = class(TJvInspectorPropData)
		class function ItemRegister: TJvInspectorRegister; override;
		class function New(const AParent: TJvCustomInspectorItem;
			const Instances: TList;
			const TypeKinds: TTypeKinds = tkProperties): TJvInspectorItemInstances;
	protected
		FInstances: TInstanceArray;
		FProps: array of PPropInfo;
	protected
		procedure SetAsFloat(const Value: Extended); override;
		procedure SetAsInt64(const Value: Int64); override;
		procedure SetAsMethod(const Value: TMethod); override;
		procedure SetAsOrdinal(const Value: Int64); override;
		procedure SetAsString(const Value: string); override;
	public
		function CreateInstanceList: TList;
	end;
	//
	TJvInspectorMultiPropDataFactory = class
		class function GetJvPropList(const AInstance: TObject;
			const TypeKinds: TTypeKinds = tkProperties): TJvPropList;
		class procedure ManufactureItems(const AParent: TJvCustomInspectorItem;
			const Instances: TList);
	private
		FInstances: TInstanceArray;
		FPropLists: array of TJvPropList;
	protected
		function CreateItem(const AParent: TJvCustomInspectorItem;
			inPropIndex: Integer): TJvCustomInspectorItem;
		procedure MergeProps;
		procedure SetObjects(inList: TList);
	public
		procedure CreateItems(const AParent: TJvCustomInspectorItem);
	end;

implementation

uses
	JvResources;

var
	GlobalMultiPropItemReg: TJvInspectorRegister = nil;

procedure RegisterMultiPropDataTypeKinds;
begin
	if TJvCustomInspectorData.ItemRegister = nil then
		raise EJvInspectorReg.CreateRes(@RsEJvInspNoGenReg);
	with TJvInspectorMultiPropData.ItemRegister do
		Add(
			TJvInspectorTypeKindRegItem.Create(TJvInspectorMultiClassItem, tkClass));
end;

{ TJvInspectorMultiClassItem }

procedure TJvInspectorMultiClassItem.CreateMembers;
begin
	if Data.IsInitialized and (Data.AsOrdinal <> 0) then
	begin
		Inspector.BeginUpdate;
		try
			DeleteMembers;
			TJvInspectorMultiPropData.New(Self,
				TJvInspectorMultiPropData(Data).CreateInstanceList
			);
			FLastMemberInstance := TObject(Data.AsOrdinal);
		finally
			Inspector.EndUpdate;
		end;
	end;
end;

{ TJvPropList }

constructor TJvPropList.Create;
begin
	FMerged := TList.Create;
end;

destructor TJvPropList.Destroy;
begin
	FMerged.Free;
	inherited;
end;

function TJvPropList.GetProps(inIndex: Integer): PPropInfo;
begin
	Result := PPropInfo(Items[inIndex]);
end;

function TJvPropList.CompareProp(inPropA, inPropB: PPropInfo): Boolean;
begin
	Result := (inPropA.Name = inPropB.Name)
		and (inPropA.PropType^ = inPropB.PropType^);
end;

function TJvPropList.FindMatch(inProp: PPropInfo): Boolean;
var
	i: Integer;
begin
	Result := false;
	for i := 0 to Pred(Count) do
	begin
		Result := CompareProp(inProp, Props[i]);
		if Result then
		begin
			FLastMatch := Props[i];
			break;
		end;
	end;
end;

procedure TJvPropList.MergeLastMatch;
begin
	FMerged.Add(FLastMatch);
end;

procedure TJvPropList.AssignMerged;
begin
	Assign(FMerged);
	FMerged.Clear;
end;

{ TJvInspectorMultiPropData }

class function TJvInspectorMultiPropData.ItemRegister: TJvInspectorRegister;
begin
	if GlobalMultiPropItemReg = nil then
	begin
		GlobalMultiPropItemReg :=
			TJvInspectorRegister.Create(TJvInspectorMultiPropData);
		// register
		RegisterMultiPropDataTypeKinds;
	end;
	Result := GlobalMultiPropItemReg;
end;

procedure TJvInspectorMultiPropData.SetAsFloat(const Value: Extended);
var
	i: Integer;
begin
	CheckWriteAccess;
	if Prop.PropType^.Kind = tkFloat then
		for i := 0 to Pred(Length(FInstances)) do
			SetFloatProp(FInstances[i], FProps[i], Value)
	else
		raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorFloat]);
	InvalidateData;
	Invalidate;
end;

procedure TJvInspectorMultiPropData.SetAsInt64(const Value: Int64);
var
	i: Integer;
begin
	CheckWriteAccess;
	if Prop.PropType^.Kind = tkInt64 then
		for i := 0 to Pred(Length(FInstances)) do
			SetInt64Prop(FInstances[i], FProps[i], Value)
	else
		raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorInt64]);
	InvalidateData;
	Invalidate;
end;

procedure TJvInspectorMultiPropData.SetAsMethod(const Value: TMethod);
var
	i: Integer;
begin
	CheckWriteAccess;
	if Prop.PropType^.Kind = tkMethod then
		for i := 0 to Pred(Length(FInstances)) do
			SetMethodProp(FInstances[i], FProps[i], Value)
	else
		raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorTMethod]);
	InvalidateData;
	Invalidate;
end;

procedure TJvInspectorMultiPropData.SetAsOrdinal(const Value: Int64);
var
	i: Integer;
begin
	CheckWriteAccess;
	if Prop.PropType^.Kind in [tkInteger, tkChar, tkEnumeration, tkSet,
		tkWChar, tkClass] then
	begin
		if GetTypeData(Prop.PropType^).OrdType = otULong then
			for i := 0 to Pred(Length(FInstances)) do
				SetOrdProp(FInstances[i], FProps[i], Cardinal(Value))
		else
			for i := 0 to Pred(Length(FInstances)) do
				SetOrdProp(FInstances[i], FProps[i], Value)
	end
	else
		raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorOrdinal]);
	InvalidateData;
	Invalidate;
end;

procedure TJvInspectorMultiPropData.SetAsString(const Value: string);
var
	i: Integer;
begin
	CheckWriteAccess;
	if Prop.PropType^.Kind in [tkString, tkLString, tkWString] then
			for i := 0 to Pred(Length(FInstances)) do
				SetStrProp(FInstances[i], FProps[i], Value)
	else
		raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorString]);
	InvalidateData;
	Invalidate;
end;

function TJvInspectorMultiPropData.CreateInstanceList: TList;
var
	i: Integer;
begin
	Result := TList.Create;
	for i := 0 to Pred(Length(FInstances)) do
		Result.Add(TObject(GetOrdProp(FInstances[i], FProps[i])));
end;

class function TJvInspectorMultiPropData.New(
	const AParent: TJvCustomInspectorItem; const Instances: TList;
	const TypeKinds: TTypeKinds): TJvInspectorItemInstances;
begin
	TJvInspectorMultiPropDataFactory.ManufactureItems(AParent, Instances);
	Result := nil;
	Instances.Free;
end;

{ TJvInspectorMultiPropDataFactory }

class procedure TJvInspectorMultiPropDataFactory.ManufactureItems(
	const AParent: TJvCustomInspectorItem; const Instances: TList);
begin
	with TJvInspectorMultiPropDataFactory.Create do
	try
		SetObjects(Instances);
		CreateItems(AParent);
	finally
		Free;
	end;
end;

class function TJvInspectorMultiPropDataFactory.GetJvPropList(
	const AInstance: TObject; const TypeKinds: TTypeKinds): TJvPropList;
var
	i: Integer;
	propCount: Integer;
	propList: PPropList;
begin
	Result := TJvPropList.Create;
	propCount := GetPropList(AInstance.ClassInfo, TypeKinds, nil);
	GetMem(propList, propCount * SizeOf(PPropInfo));
	try
		GetPropList(AInstance.ClassInfo, TypeKinds, propList);
		for i := 0 to Pred(propCount) do
			Result.Add(propList[i]);
	finally
		FreeMem(propList);
	end;
end;

procedure TJvInspectorMultiPropDataFactory.SetObjects(inList: TList);
var
	i: Integer;
	p: TJvPropList;
begin
	SetLength(FPropLists, inList.Count);
	SetLength(FInstances, inList.Count);
	for i := 0 to Pred(inList.Count) do
	begin
		FInstances[i] := inList[i];
		p := GetJvPropList(FInstances[i]);
		FPropLists[i] := p;
	end;
	if inList.Count > 1 then
		MergeProps;
end;

procedure TJvInspectorMultiPropDataFactory.MergeProps;
var
	c, i, j: Integer;
	p: TJvPropList;
	match: Boolean;
begin
	c := Length(FPropLists);
	p := FPropLists[0];
	for i := 0 to Pred(p.Count) do
	begin
		match := true;
		p.FLastMatch := p.Props[i];
		for j := 1 to Pred(c) do
		begin
			match := FPropLists[j].FindMatch(p.Props[i]);
			if not match then
				break;
		end;
		if match then
			for j := 0 to Pred(c) do
				FPropLists[j].MergeLastMatch;
	end;
	for i := 0 to Pred(c) do
		FPropLists[i].AssignMerged;
end;

function TJvInspectorMultiPropDataFactory.CreateItem(
	const AParent: TJvCustomInspectorItem;
	inPropIndex: Integer): TJvCustomInspectorItem;
var
	pprop: PPropInfo;
	data: TJvInspectorMultiPropData;
	i, c: Integer;
	regItem: TJvCustomInspectorRegItem;
begin
	c := Length(FPropLists);
	if (c < 1) or (inPropIndex > FPropLists[0].Count) or (inPropIndex < 0) then
		raise EJvInspectorData.CreateRes(@RsEJvAssertPropInfo);
	//
	pprop := FPropLists[0][inPropIndex];
	if pprop = nil then
		raise EJvInspectorData.CreateRes(@RsEJvAssertPropInfo);
	//
	data := TJvInspectorMultiPropData.CreatePrim(pprop.Name, pprop.PropType^);
	data.Instance := FInstances[0];
	data.Prop := pprop;
	//
	data.FInstances := FInstances;
	//
	c := Length(FPropLists);
	SetLength(Data.FProps, c);
	for i := 0 to Pred(c) do
		data.FProps[i] := FPropLists[i][inPropIndex];
	//
	//Data := TJvInspectorPropData(DataRegister.Add(Data));
	//
	if data <> nil then
	begin
		regItem := TJvInspectorPropData.TypeInfoMapRegister.FindMatch(Data);
		if (RegItem <> nil) and (RegItem is TJvInspectorTypeInfoMapperRegItem) then
			data.TypeInfo := TJvInspectorTypeInfoMapperRegItem(RegItem).NewTypeInfo;
		Result := data.NewItem(AParent);
	end
	else
		Result := nil;
end;

procedure TJvInspectorMultiPropDataFactory.CreateItems(
	const AParent: TJvCustomInspectorItem);
var
	i: Integer;
	p: TJvPropList;
begin
	if Length(FPropLists) > 0 then
	begin
		p := FPropLists[0];
		if p.Count = 0 then
			p.Free
		else for i := 0 to Pred(p.Count) do
			CreateItem(AParent, i)
	end;
end;

initialization
finalization
	FreeAndNil(GlobalMultiPropItemReg);
end.

