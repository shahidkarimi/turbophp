unit Registration;

interface

procedure Register;

implementation

uses
	Classes, ComCtrls, ExtCtrls,

	//PhpCodeDesigner,

	//LrTable,

	//TbhContent, TbhTemplateContent,
	//TbhStreamContent, TbhChartContent,
	//TbhWebVariable, TbhWebDataDictionary, TbhValidators,
	//TbhComponents, TbhUserComponent, TbhControls,
	//TbhContentCell,
	//TbhVisualContent,
	//TbhActionWrangler, TbhResource,
	//TbhSessionWrangler,

	ThStyleSheet,

	ThWebControl, ThLabel, ThPanel, ThSpacer, ThImage,
	ThButton, ThImageButton, ThInput, ThCheckBox, ThSelect, ThTextArea,
	ThTabs, ThPageControl,

	ThCellPanel, ThGraphicButton,

	TpWebVariable, TpFileList, TpGenericComponent, TpGenericControl,
	TpAuthenticator,

	TpImageList,
	TpLabel, TpImage, TpPanel,
	TpForm, TpButton, TpImageButton, TpInput, TpCheckbox, TpSelect, TpTextArea,
	TpTable, TpRepeater, TpPageControl, TpModule,

	TpDb, TpDbListSource, TpDbText, TpDbEdit, TpDbTextArea,
	TpMySql, TpODBC,

	TpClassInfo,

	ColorProperty, CollectionProp, ConnectionProperty, DbProperties,
	PhpEventProperty, StyleClassProperty, TpPictureProperty,
	ListSourceProperty, TpComponentProperty 
	;

{.$R F:\Turbo4\TurboHtml.4\TbhContentPalette.res}
{.$R F:\Turbo4\TurboHtml.4\ThPaletteIcons.res}
{$R PaletteIcons.res}

procedure Register;
begin
	//RegisterComponents('Test', [ TPageControl ]);
	//
	RegisterComponents('Main', [
		TTpPanel, TTpLabel, TTpText, TTpImage, TThSpacer,
		TTpTable, TTpRepeater, TTpPageControl,
		TTpModule, TTpGenericControl
	]);
	//
	RegisterComponents('Forms', [
		TTpForm, TTpButton, TTpImageButton, TTpInput, TTpCheckbox, TTpRadio,
		TTpTextArea, TTpSelect, TTpListBox
	]);
	//
	with TurboClassInfo do
	begin
		Add('TTpPanel=Container Panel');
		Add('TTpLabel=Text Label');
		Add('TTpText=Multi-line Text');
		Add('TTpImage=Image');
		Add('TThSpacer=Spacer');
		Add('TTpForm=Form');
		Add('TTpButton=Button');
		Add('TTpImageButton=Image Button');
		Add('TTpInput=Text Input');
		Add('TTpCheckbox=Checkbox');
		Add('TTpRadio=Radio Button');
		Add('TTpTextArea=Text Area Input');
		Add('TTpSelect=Select');
		Add('TTpListBox=List Box');
		Add('TTpTable=Dynamic Table');
		Add('TTpRepeater=Repeater');
		Add('TTpPageControl=Page Control (experimental)');
		Add('TTpModule=Module (experimental)');
		Add('TTpGenericControl=Generic Control');
	end;
	//
	RegisterComponents('System', [
		TThStyleSheet,
		TTpAuthenticator, TTpDbAuthenticator,
		TTpWebVariable, TTpFileList,
		TTpGenericComponent
		//TTpImageList
	]);
	//
	with TurboClassInfo do
	begin
		Add('TThStyleSheet=Style Sheet');
		Add('TTpAuthenticator=User Authenticator (Generic)');
		Add('TTpDbAuthenticator=User Authenticator (Database)');
		Add('TTpWebVariable=Input Variable');
		Add('TTpFileList=File Lister');
		Add('TTpGenericComponent=Generic Component');
	end;
	//
	RegisterComponents('DB', [
		TTpDbText, TTpDbEdit, TTpDbTextArea
	]);
	//
	with TurboClassInfo do
	begin
		Add('TTpDbText=DB Text');
		Add('TTpDbEdit=DB Text Input');
		Add('TTpDbTextArea=DB Text Area');
	end;
	//
	RegisterComponents('MySql', [
		TTpMySql, TTpMySqlTable, TTpMySqlQuery, TTpMySqlDbList, TTpMySqlTableList
	]);
	//
	with TurboClassInfo do
	begin
		Add('TTpMySql=MySql Connection');
		Add('TTpMySqlTable=MySql Table');
		Add('TTpMySqlQuery=MySql Query');
		Add('TTpMySqlDbList=MySql Database List');
		Add('TTpMySqlTableList=MySql Table List');
	end;
	//
	RegisterComponents('ODBC', [
		TTpODBC, TTpODBCTable, TTpODBCQuery, TTpODBCDbList, TTpODBCTableList
	]);
	//
	with TurboClassInfo do
	begin
		Add('TTpODBC=ODBC Connection');
		Add('TTpODBCTable=ODBC Table');
		Add('TTpODBCQuery=ODBC Query');
		Add('TTpODBCDbList=ODBC Database List');
		Add('TTpODBCTableList=ODBC Table List');
	end;
	//
{
	RegisterComponents('Static', [
		TThPanel,	TThLabel, TThText, TThImage,
		TThButton, TThTextArea, TThCheckBox, TThRadio, TThImageButton,
		TThGraphicButton,
		//TThTabs
		TThCellPanel
	]);
}
	//
	RegisterClass(TPanel);
	//
	//
//	RegisterComponents('Test', [
//		TLrTable
//	]);
	//
//	RegisterComponents('Content', [
//		TTbhContent, TTbhContentStrings, TTbhFileContent,
//		TTbhTemplateContent, TTbhTaggedContent, TTbhBlock,
//		TTbhRepeater,
//		TTbhFileStreamContent, TTbhFolderContent, TTbhPictureContent,
//		TTbhChartContent, TTbhImageContent, TTbhTableContent,
//		TTbhUserComponent,
//		TTbhWebVariable, TTbhWebDataDictionary,
//		TTbhNotBlankValidator, TTbhRegExValidator, TTbhEmailValidator,
//		TTbhNumberValidator,
//			TTbhActionWrangler, TTbhResourceDispatcher,
//			TTbhFileResource, TTbhFolderResource, TTbhPictureResource,
//			TTbhModuleResource, TTbhPageResource,
//			TTbhSessionWrangler, TTbhCookieSession, TTbhSessionFileWrangler,
//		TTbhVisualContent, TTbhContentCell
//	]);
	//
	RegisterCollectionPropertyEditor;
	RegisterColorPropertyEditor;
	RegisterConnectionStringPropertyEditor;
	RegisterDbProperties;
	RegisterPhpEventPropertyEditor;
	RegisterPicturePropertyEditor;
	RegisterStyleClassProperty;
	RegisterListSourceProperty;
	RegisterComponentProperty;
{
	RegisterImageListPropertyEditor;
	RegisterPhpEventPropertyEditor;
	RegisterColorPropertyEditor;
	RegisterStyleClassProperty;
}
end;

end.
