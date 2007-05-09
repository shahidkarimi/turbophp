unit htReg;

interface

procedure Register;

implementation

uses
	Classes,
	htColumns, htRows, htLayout, htAutoTable, htPanel,
	htLabel, htImage,
	htDb;

{$R TurboHtml5Palette.res}

procedure Register;
begin
	RegisterComponents('TurboHtml5', [
		ThtColumns, ThtRows, ThtLayout, ThtAutoTable,
		ThtPanel,	ThtLabel, ThtImage
	]);
	RegisterComponents('TurboHtml5 DB', [
		ThtDb, ThtDbTable
	]);
end;

end.
