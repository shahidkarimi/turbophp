unit ThHeaderComponent;

interface

uses
	SysUtils, Classes, ThComponent, ThInterfaces, ThStructuredHtml, ThTag;

type
	TThHeaderComponent = class(TThComponent, IThPublishable)
	protected
		procedure Tag(inTag: TThTag); virtual;
	public
		procedure Publish(inHtml: TThStructuredHtml); virtual;
	end;

implementation

{ TThHeaderComponent }

procedure TThHeaderComponent.Tag(inTag: TThTag);
begin
	//
end;

procedure TThHeaderComponent.Publish(inHtml: TThStructuredHtml);
begin
	with TThTag.Create('meta') do
	try
		Tag(ThisTag);
		inHtml.Headers.Add(Html);
	finally
		Free;
	end;
end;

end.
