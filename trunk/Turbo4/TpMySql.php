<?php

# +----------------------------------------------------------------------+
# | TurboPhp 4 - TpMySql
# | Version 4.0 Beta                                                     |
# +----------------------------------------------------------------------+
# | Copyright (c) 2004 Least-Resistance Software                         |
# +----------------------------------------------------------------------+
# | This source file is subject to the terms of the TurboPhp license,    |
# | that is bundled with this package in the file LICENSE.TXT, and is    |
# | available at through the world-wide-web at                           |
# | http://www.turbophp.com/turbophp/license.txt                         |
# +----------------------------------------------------------------------+

include_once('TpDb.php');

TpRegisterClass('mysql', 'TTpMySql');
TpRegisterClass('mysqlquery', 'TTpMySqlQuery');

function TpMySqlResultsToArray(&$inResults)
{
   $rows = array();
   while ($row = mysql_fetch_row($inResults))
     $rows[] = $row[0];
	return $rows;
}

class TTpMySql extends TTpDbConnect
{
	function DoConnect()
	{
		if ($this->Persistent)
			$connect = 'mysql_pconnect';
		else
			$connect = 'mysql_connect';
		$this->Connection = $connect($this->Host, $this->UserName, $this->Password);
		if ($this->App->Debug)
		{
			echo $this->DebugId()."DoConnect ";
			echo ($this->Connection ? 'successful' : 'failed') . "<br>";
		}
	}
	function DoSelectDb()
	{
		if ($this->App->Debug)
			echo $this->DebugId()."DoSelectDb [$this->Database]<br>";
		return mysql_select_db($this->Database);
	}
	function Close()
	{
		mysql_close($this->Connection);
		parent::Close();
	}
  function Generate()
  {
    //$this->DoEvent($this->OnGenerate);
	  //$this->Connect();
	  //$this->SelectDatabase();
		return '';
  }
}

class TTpMySqlQuery extends TTpQuery
{
	function ExecuteQuery()
	{
		if ($this->App->Debug)
			echo $this->DebugId().": query = [" .
				$this->InterpolatedQuery() . "] ... ";
	  $this->Result = mysql_query($this->InterpolatedQuery());
		if ($this->Result)
			$this->Row = mysql_fetch_array($this->Result);
		else
			$this->Row = false;
		if ($this->App->Debug)
			echo ($this->Result ? 'query successful<br>' : 'query failed<br>');

	}
	function NextMysqlRow()
	{
		$this->Row = mysql_fetch_array($this->Result);
	}
	function Rewind()
	{
		parent::Rewind();
		if ($this->Result && (mysql_num_rows($this->Result) > 0))
		{
			if ($this->App->Debug)
				echo "Rewinding ... rows: ".mysql_num_rows($this->Result)."<br>";
			mysql_data_seek($this->Result, 0);
		} else if ($this->App->Debug)
			echo "Rewind failed (bad result)<br>";
	}
	function Next()
	{
		parent::Next();
		if ($this->Result)
			$this->NextMysqlRow();
		return !$this->EOF();
	}
}

?>
