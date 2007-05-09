unit TpDbConnectionStrings;

interface

uses
	SysUtils, Classes;

function BuildMySqlConnectionString(
	const inHost, inDatabase, inUserName, inPassword: string): string;

implementation

	function Param(const inName, inParam: string): string;
	begin
		if (inParam <> '') then
			Result := inName + '=' + inParam + ';'
		else
			Result := '';
	end;

function BuildMySqlConnectionString(
	const inHost, inDatabase, inUserName, inPassword: string): string;
begin
	Result := Param('Driver', '{MySQL ODBC 3.51 Driver}');
	if inHost = '' then
	begin
		Result := Result
			+ Param('Option', '16834');
	end
	else begin
		Result := Result
			+ Param('Server', inHost)
			+ Param('Port', '3306')
			+ Param('Option', '131072')
			;
	end;
	Result := Result
		+ Param('Database', inDatabase)
		+ Param('USER', inUserName)
		+ Param('PASSWORD', inPassword)
		;
end;

end.
