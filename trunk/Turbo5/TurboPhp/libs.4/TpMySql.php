<?php

/**
 *  TurboPhp 4 - TpMySql
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
 * Returns the first column of the input MySql result array.
 */
function TpMySqlResultsToArray(&$inResults)
{
   $rows = array();
   while ($row = mysql_fetch_row($inResults))
     $rows[] = $row[0];
  return $rows;
}

/**
 * MySql database connection.
 * @package TurboPhpLib
 */
class TTpMySql extends TTpDbConnect
{
  /**
   * @access private
   */
  function DoConnect()
  {
    if ($this->Persistent)
      $connect = 'mysql_pconnect';
    else
      $connect = 'mysql_connect';
    $this->Connection = $connect($this->Host, $this->UserName, $this->Password);
    //if ($this->Connection == false)
    //  $this->Error = 'Connection failed.';
    $this->Debug($this->Connection ? 'successful' : 'failed');
  }
  /**
   * @access private
   */
  function DoSelectDb()
  {
    $this->Debug("Database: [$this->Database]");
    return mysql_select_db($this->Database, $this->Connection);
  }
  /**
   * @access private
   */
  function Close()
  {
    mysql_close($this->Connection);
    parent::Close();
  }
  /**
   * @access private
   */
  function CreateNewQuery()
  {
    $q = new TTpMySqlQuery();
    $q->Page = &$this->Page;
    $q->Db = &$this;
    //$q->DebugFlag = true; //$this->DebugFlag;
    return $q;
  }
}

/**
 * MySql query.
 * @package TurboPhpLib
 */
class TTpMySqlQuery extends TTpQuery
{
  /**
   * @access private
   */
  function DoExecute()
  {
    $this->Result =
      mysql_query($this->InterpolatedQuery(), $this->Db->Connection);
  }
  /**
   * Get the number of columns in the result set.
   * @return integer Number of columns.
   */
  function GetColCount()
  {
    return ($this->Result ? mysql_numfields($this->Result) : 0);
  }
  /**
   * Get the number of rows in the result set.
   * @return integer Number of rows.
   */
  function GetRowCount()
  {
    return ($this->Result ? mysql_num_rows($this->Result) : 0);
  }
  /**
   * Returns the value of mysql_error for the attached connection.
   * @return string MySql error message.
   */
  function GetError()
  {
    mysql_error($this->Db->Connection);
  }
  /**
   * Get the field name for column $inIndex.
   * @return string Field name.
   */
  function GetFieldName($inIndex)
  {
    return ($this->Result ?
      mysql_field_name($this->Result, $inIndex) : 'error: ' . $this->GetError());
  }
  /**
   * @access private
   */
  function DoNextRow()
  {
    $this->Row = mysql_fetch_array($this->Result);
  }
  /**
   * @access private
   */
  function Rewind()
  {
    if ($this->GetRowCount() > 0)
      mysql_data_seek($this->Result, 0);
    else
      $this->Debug("Rewind failed (bad result)");
    parent::Rewind();
  }
}

/**
 * List of MySql databases.
 * @package TurboPhpLib
 */
class TTpMySqlDbList extends TTpMySqlQuery
{
  /**
   * @access private
   */
  function Init()
  {
    parent::Init();
    $this->SQL = 'SHOW DATABASES';
  }
}

/**
 * List of MySql tables.
 * @package TurboPhpLib
 */
class TTpMySqlTableList extends TTpMySqlQuery
{
  /**
   * @access private
   */
  function Init()
  {
    parent::Init();
    $this->SQL = 'SHOW TABLES';
  }
}

?>
