unit TpInterfaces;

interface

uses
	Classes;

type
	ITpIncludeLister = interface
	['{8F14670D-7729-4456-9B6D-FAE2BEC3132F}']
		procedure ListPhpIncludes(inIncludes: TStringList);
	end;
	//
	ITpConfigWriter = interface
	['{0447F19B-86C4-4FCA-A95C-C00FBCC370B6}']
		procedure WriteConfig(const inFolder: string);
	end;
	//
	ITpJsIncludeLister = interface
	['{713E08FA-0211-4666-9172-D5D64FF7FE38}']
		procedure ListJsIncludes(inIncludes: TStringList);
	end;
	//
	ITpJsWriter = interface
	['{713E08FA-0211-4666-9172-D5D64FF7FE38}']
		procedure WriteJavaScript(inScript: TStringList);
	end;

implementation

end.
