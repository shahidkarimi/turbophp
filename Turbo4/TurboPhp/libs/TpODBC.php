<?php

/**
 *  TurboPhp 4 - TpODBC
 *  Version 4.1 Beta
 *
 *  Copyright (c) 2004 Least-Resistance Software
 *
 *  This source file is subject to the terms of the TurboPhp license,
 *  that is bundled with this package in the file LICENSE.TXT, and is
 *  available at through the world-wide-web at
 *  http://www.turbophp.com/turbophp/license.txt
 *
 * @package TurboPhpLib
 */

/**
 */
include_once('TpDb.php');

/**
 * Returns the first column of the input ODBC result array.
 */
function TpODBCResultsToArray(&$inResults)
{
   $rows = array();
   while ($row = ODBC_fetch_row($inResults))
     $rows[] = $row[0];
  return $rows;
}

/**
 * ODBC database connection.
 * @package TurboPhpLib
 */
class TTpODBC extends TTpDbConnect
{
  var $CursorType;
  /**
   * @access private
   */
  function Init()
  {
    parent::Init();
    $this->CursorType = 0; //SQL_CUR_DEFAULT;
  }
  /**
   * @access private
   */
  function DoConnect()
  {
    if ($this->Persistent)
      $connect = 'odbc_pconnect';
    else
      $connect = 'odbc_connect';
    $this->DebugFlag = true;
    $this->Debug("DSN: [$this->Host]");
    $this->Connection = $connect($this->Host, $this->UserName, $this->Password);
      //, $this->CursorType);
    $this->Debug($this->Connection ? 'successful' : 'failed');
  }
  /**
   * @access private
   */
  //function DoSelectDb()
  //{
    //$this->Debug("Database: [$this->Database]");
    //return ODBC_select_db($this->Database);
  //}
  function Close()
  {
    odbc_close($this->Connection);
    parent::Close();
  }
  /**
   * @access private
   */
  function CreateNewQuery()
  {
    $q = new TTpODBCQuery();
    $q->Page = &$this->Page;
    $q->DebugFlag = true; //$this->DebugFlag;
    return $q;
  }
}

/**
 * ODBC query.
 * @package TurboPhpLib
 */
class TTpODBCQuery extends TTpQuery
{
  var $Rewinding;
  /**
   * @access private
   */
  function DoExecute()
  {
    $this->Row = false;
    $this->Result = odbc_do($this->Db->Connection, $this->InterpolatedQuery());
    if ($this->Page->Debug)
    {
      $q = trim(str_replace(
          array("\n", "\r"), array(" ", ""), $this->InterpolatedQuery()));
      $this->Debug(($this->Result ? 'successful' : 'failed') . " query [$q]");
    }
  }
  /**
   * Get the number of columns in the result set.
   * @return integer Number of columns.
   */
  function GetColCount()
  {
    return ($this->Result ? odbc_num_fields($this->Result) : 0);
  }
  /**
   * Get the number of rows in the result set.
   * @return integer Number of rows.
   */
  function GetRowCount()
  {
    return ($this->Result ? odbc_num_rows($this->Result) : 0);
  }
  /**
   * Get the field name for column $inIndex.
   * @return string Field name.
   */
  function GetFieldName($inIndex)
  {
    return ($this->Result ? odbc_field_name($this->Result, $inIndex + 1) : '');
  }
  /**
   * @access private
   */
  function DoNextRow()
  {
    if ($this->Rewinding)
      $this->Row = odbc_fetch_array($this->Result, 1);
    else
      $this->Row = odbc_fetch_array($this->Result);
    $this->Rewinding = false;
  }
  /**
   * @access private
   */
  function Rewind()
  {
    parent::Rewind();
    $this->Rewinding = true;
  }
}

/**
 * List of ODBC databases.
 * @package TurboPhpLib
 */
class TTpODBCDbList extends TTpODBCQuery
{
  /*
   * @access private
   */
  function DoExecute()
  {
    $this->Result = odbc_tables($this->Db->Connection, '%');
  }
}

/**
 * List of ODBC tables.
 * @package TurboPhpLib
 */
class TTpODBCTableList extends TTpODBCQuery
{
  /*
   * @access private
   */
  function DoExecute()
  {
    //parent::DoExecute();
    $this->Result = odbc_tables($this->Db->Connection);
  }
}

?>
