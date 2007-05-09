unit ThInterfaces;

interface

uses
	Classes, ThTag, ThStructuredHtml, ThJavaScript;

type
	IThHtmlSource = interface
	['{F11847E2-D3A5-4680-9807-CA6B0FCDE81A}']
		procedure CellTag(inTag: TThTag);
		function GetHtml: string;
		property Html: string read GetHtml;
	end;
	//
	IThPublishable = interface
	['{266304E2-7196-4443-92F4-47F435A40CD1}']
		procedure Publish(inHtml: TThStructuredHtml);
	end;
	//
	IThFormInput = interface
	['{D086C74C-B1B6-435F-823C-88A0997DC23E}']
	end;
	//
	IThStyleSource = interface
	['{4FADD829-EF84-4D8A-B40E-E0B0F6367401}']
		procedure PublishStyles(inStyles: TStringList);
	end;
	//
	IThJavaScriptable = interface
	['{E5242113-35C4-42AD-88C9-8E0081D52ABE}']
		function GetJavaScript: TThJavaScriptEvents;
	end;

implementation

end.
