<?php

/**
 *  TurboPhp 4 - TpDb
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

/**#@+
 * @access private
 */

define('P_QUERY_PARAM', '#{%([^}]*)}#Usm');

/**#@-*/

/**
 * Generic database connection object.
 * @package TurboPhpLib
 */
class TTpDbConnect extends TTpSilentObj
{
  /**
   * Path to configuration file.
   * Database access data (server address, login, password) is stored
   * separately so:
   * 1. Configuration files can be stored off the browsable folder tree.
   * 2. Database connections can be altered by replacing or editing the configuration file.
   * 3. Source code can be shared without regard to database security details.
   * @var string
   */
  var $ConfigFile;
  /**
   * Abstract connection object.
   * Represents a database connection identifier or handle.
   * @var mixed
   */
  var $Connection;
  /**
   * True if connections will be initiated manually.
   * By default database connections are automatically opened
   * after application object parsing. If $ConnectManually is
   * true, the connection will instead need to be opened by
   * an explicit call to the {@link Connect} function.
   * @var boolean
   */
  var $ConnectManually;
  /**
   * Name of the connected database.
   * @var string
   */
  var $Database;
  /**
   * Most recent error message.
   * @var string
   */
  var $Error;
  /**
   * Address of the database server.
   * Normally loaded from configuration file.
   * @var string
   */
  var $Host;
  /**
   * Event fired just before openeing the connection.
   * Normally loaded from configuration file.
   * @var string
   */
  var $OnConnect;
  /**
   * Password for database access.
   * Normally loaded from configuration file.
   * @var string
   */
  var $Password;
  /**
   * Specifies whether the connection to the database should be persistent.
   * When the connection is opened, this property is checked to determine if
   * the connection should be opened in persistent mode.
   * @var boolean
   */
  var $Persistent;
  /**
   * User name or login for database access.
   * Normally loaded from configuration file.
   * @var string
   */
  var $UserName;
  /**
   * @access private
   */
  function Init()
  {
    parent::Init();
    $this->Host = '';
  }
  /**
   * @access private
   */
  function StoreAttr($inName, $inValue)
  {
    if ($inName == 'tphost')
      $this->Host = $inValue;
    else if ($inName == 'tpuser')
      $this->UserName = $inValue;
    else if ($inName == 'tppassword')
      $this->Password = $inValue;
    else if ($inName == 'tpdatabase')
      $this->Database = $inValue;
    else
      parent::StoreAttr($inName, $inValue);
  }
  /**
   * @access private
   */
  function AfterParse()
  {
    global $tpSupportPath;
    if ($this->ConfigFile == '')
      $this->ConfigFile = $tpSupportPath . "$this->Name.conf.php";
    if (!$this->ConnectManually)
      $this->AutoConnect();
  }
  /**
   * @access private
   */
  function LoadConfig()
  {
    if (file_exists($this->ConfigFile))
    {
      $conf = parse_ini_file($this->ConfigFile);
      $this->Host = TpSafeArrayGet('Host', $conf);
      $this->UserName = TpSafeArrayGet('User', $conf);
      $this->Password = TpSafeArrayGet('Password', $conf);
    }
  }
  /**
   * @access private
   */
  function DoSelectDb()
  {
    //
  }
  /**
   * Select the database {@link $Database}.
   * Connection must be open to select a databse.
   */
  function SelectDatabase($inDatabase = '')
  {
    if ($inDatabase <> '')
      $this->Database = $inDatabase;
    if ($this->Connection && ($this->Database <> ''))
      return $this->DoSelectDb();
    else if ($this->Page->Debug)
    {
      if (!$this->Connection)
        $this->Debug(" no connection available.");
      if ($this->Database == '')
        $this->Debug(" no database selected (property is empty)");
    }
    return false;
  }
  /**
   * Open the database server connection, if it's not already open.
   * Before opening a database connection, the {@link OnConnect} event
   * is fired, and then an attempt is made to load the configuration file.
   * See also {@link $ConfigFile} and {@link $Persistent}.
   *
   * Note: before access, a database should be selected on the
   * connection. See {@link $Database} and {@link SelectDatabase}.
   */
  function Connect()
  {
    if (!$this->Connection)
    {
      $this->DoEvent($this->OnConnect);
      $this->LoadConfig();
      $this->DoConnect(); // abstract: must be provided in subclass
    }
    return $this->Connection;
  }
  /**
   * Open the database connection and select the databse.
   * Called after application object parsing unless {@link $ConnectManually}
   * is true.
   */
  function AutoConnect()
  {
    $this->Connect();
    $this->SelectDatabase();
  }
  /**
   * Connect to the database server, halting the application on error.
   */
  function ConnectOrDie($inMessage = '')
  {
    if ($inMessage == '')
      $inMessage = 'Could not connect: ';
    $this->Connect() or die($inMessage . mysql_error() . '<br>');
  }
  /**
   * Close the database server connection.
   */
  function Close()
  {
    $this->Connection = false;
  }
}

/**
 * Generic source for data sets.
 * @package TurboPhpLib
 */
class TTpDataSource extends TTpSilentObj
{
  /**
   * Reference to the linked DB connection object.
   * @var object;
   */
  var $Db;
  /**
   * @access private
   */
  /**
   * Query result. Actual type and value depends on the type of data source.
   * @var mixed
   */
  var $Result;
  /**
   * A row of data. Usually a hash.
   * @var array
   */
  var $Row;
  /**
   * Index of the last row of data returned.
   * @var integer
   */
  var $RowIndex;
  /**
   * Default field for accessing list data.
   * @see GetItems
   * @var string
   */
  var $Field;
  /**
   * Event fired before data is accessed.
   * @var string
   */
  var $OnBeforeExecute;
  /**
   * Event fired if there is an error with data access.
   * @var string
   */
  var $OnFailure;
  /**
   * Event fired if data was accessed successfully.
   * @var string
   */
  var $OnSuccess;
  /**
   * @access private
   */
  function Init()
  {
    parent::Init();
    $this->Row = false;
    $this->Field = 0;
  }
  /**
   * @access private
   */
  function AfterParse()
  {
    $this->MarshallObject('Db');
  }
  /**
   * Hook function for executing a data query.
   * Subclasses extend this function to provide specific functionality.
   */
  function DoExecute()
  {
    // Abstract. Must be provided by subclass.
  }
  /**
   * Perform data acess.
   * Fires {@link OnBeforeExecute} then calls {@link ExecuteQuery}.
   * Subsequently fires {@link OnFailure} or {@link OnSuccess} depending
   * on the value of {@link Result}.
   * This function is called automatically if data access
   * functions are invoked and data is not yet available.
   * Specifically {@link Next}, {@link Rewind} and {@link GetField}
   * will call Execute if needed.
   */
  function Execute()
  {
    $this->Row = false;
    $this->RowIndex = -1;
    $this->DoEvent($this->OnBeforeExecute);
    $this->DoExecute();
    if (!$this->Result)
      $this->DoEvent($this->OnFailure);
    else
      $this->DoEvent($this->OnSuccess);
  }
  /**
   * @access private
   */
  function NeedData()
  {
    if (!$this->Result)
      $this->Execute();
  }
  /**
   * True if the end of data has been reached.
   * @return boolean
   */
  function EOF()
  {
    return !$this->Row;
  }
  /**
   * Rewinds the row pointer to the first row in the result set.
   * Calls {@link Execute} if needed.
   */
  function Rewind()
  {
    $this->Row = false;
    $this->RowIndex = -1;
    $this->NeedData();
  }
  /**
   * @abstract
   * @access private
   */
  function DoNextRow()
  {
  }
  /**
   * Fill {@link $Row} with the next row of data.
   * Calls {@link Execute} if needed.
   */
  function Next()
  {
    $this->NeedData();
    if ($this->Result)
    {
      $this->DoNextRow();
      $this->RowIndex++;
    }
    return !$this->EOF();
  }
  /**
   * Get field data from the current row.
   * Calls {@link Execute} if needed.
   * @return Value of key $inFieldId in the array {@link $Row}, or false
   * if the key does not exist.
   */
  function GetField($inFieldId)
  {
    $this->NeedData();
    if ($this->RowIndex < 0)
      $this->Next();
    if (!$this->Row)
      return mysql_error();
    else
    {
      if (array_key_exists($inFieldId, $this->Row))
        return $this->Row[$inFieldId];
      else
        return $inFieldId;
    }
  }
  /**
   * Add an entire column of the result data to $outItems.
   * The column identified by the {@link $Field} property is copied.
   * By default, the first column of the data set is used.
   *
   * This function implements the {@link TpListSource} functionality
   * for data sources.
   */
  function GetItems(&$outItems)
  {
    $this->Rewind();
    while ($this->Next())
      $outItems[] = @$this->Row[$this->Field];
  }
}

/**
 * Generic SQL query object.
 * @package TurboPhpLib
 */
class TTpQuery extends TTpDataSource
{
  /**
   * SQL to be executed.
   * @var string
   */
  var $SQL;
  /**
   * Name, value pairs of SQL parameters.
   * The {@link $SQL} for this control may contain parameters (macros) of the
   * form {%<param_name>}. These parameters are replaced by values from the
   * $Params array.
   *
   * Example 1: the SQL contains a line like
   *  where Cost > {%MinCost}
   * and the $Params array has
   *  "MinCost" => "1000"
   * the executed SQL will read
   *  where Cost > 1000
   *
   * Example 2: the SQL contains a line like
   *  order by '{%SortField}'
   * and the $Params array has
   *  "SortField" => "Name"
   * the executed SQL will read
   *  order by 'Name'
   *
   * @var array
   */
  var $Params;
  /**
   * @access private
   */
  function Init()
  {
    parent::Init();
    $this->Params = array();
  }
  /**
   * @access private
   */
  function AfterParse()
  {
    parent::AfterParse();
    $this->SQL = base64_decode($this->SQL);
  }
  /**
   * @access private
   */
  function InterpolateParam(&$matches)
  {
    $obj = $this->Page->GetObject($matches[1]);
    if ($obj)
      $p = $obj->GetDefaultValue();
    else
      $p = TpSafeArrayGet($matches[1], $this->Params);
    return $this->Interpolate($p);
  }
  /**
   * @access private
   */
  function Interpolate(&$inText)
  {
    return preg_replace_callback(
      P_QUERY_PARAM,
      array(&$this, 'InterpolateParam'),
      $inText
    );
  }
  /**
   * @access private
   */
  function InterpolatedQuery()
  {
    return trim($this->Interpolate($this->SQL));
  }
  function Execute()
  {
    parent::Execute();
    if ($this->Page->Debug)
    {
      $q = trim(str_replace(
          array("\n", "\r"), array(" ", ""), $this->InterpolatedQuery()));
      $this->Debug(($this->Result ? 'successful' : 'failed') . " query [$q]");
    }
  }
}

/**
 * Label whose contents are obtained from a data source.
 * @package TurboPhpLib
 */
class TTpDbText extends TTpObj
{
  /**
   * Reference to an attached {@link TTpDataSource}.
   * @var object
   */
  var $DataSource;
  /**
   * Name of the field to use.
   * @var string
   */
  var $Field;
  /**
   * @access private
   */
  function AfterParse()
  {
    if ($this->Page->Debug)
    {
      if (!$this->DataSource)
        $this->Debug("parsed empty DataSource [$this->DataSource]");
      $s = $this->DataSource;
    }
    //
    $this->MarshallObject('DataSource');
    //
    if ($this->Page->Debug)
    {
      if (!$this->DataSource)
        $this->Debug("failed to get DataSource object [$s]");
      else
        $this->Debug("DataSource object [$s] marshalled.");
    }
  }
  /**
   * @access private
   */
  function BeforeGenerate()
  {
    parent::BeforeGenerate();
    if ($this->DataSource)
    {
      $this->Content = $this->DataSource->GetField($this->Field);
      //$this->Debug("Content = [" . $this->Content . "]");
      if ($this->Content == '')
      {
        //$this->Debug("DBText Content is blank");
        $this->Debug(($this->DataSource->Result ? 'Result ok' : 'Result bad'));
      }
    }
    else
      $this->Debug("Bad DataSource");
    //echo "[$this->Content]<br>;
  }
/*
  function Dump()
  {
    parent::Dump();
    echo "DataSource: [".$this->DataSource->Name."]<br>";
    echo "Field: [$this->Field]<br>";
  }
*/
}

/**
 * Input object whose contents are obtained from a data source.
 * @package TurboPhpLib
 */
class TTpDbInput extends TTpInput
{
  /**
   * Reference to an attached {@link TTpDataSource}.
   * @var object
   */
  var $DataSource;
  /**
   * Name of the field to use.
   * @var string
   */
  var $Field;
  /**
   * @access private
   */
  function AfterParse()
  {
    parent::AfterParse();
    $this->MarshallObject('DataSource');
  }
  /**
   * @access private
   */
  function GetData()
  {
    if ($this->DataSource)
      return $this->DataSource->GetField($this->Field);
    else
    {
      $this->Debug("Bad DataSource");
      return '';
    }
  }
}

/**
 * Single line edit-box whose contents are obtained from a data source.
 * @package TurboPhpLib
 */
class TTpDbEdit extends TTpDbInput
{
  /**
   * @access private
   */
  function BeforeGenerate()
  {
    if ($this->DataSource)
    {
      $this->Value = $this->DataSource->GetField($this->Field);
      $this->Debug("Content = [$this->Value], Field = [$this->Field]");
    } else
      $this->Debug("Bad DataSource");
    parent::BeforeGenerate();
  }
}

/**
 * Multi-line edit-box whose contents are obtained from a data source.
 * @package TurboPhpLib
 */
class TTpDbTextArea extends TTpDbInput
{
  /**
   * @access private
   */
  function StoreValue()
  {
    $this->Content = $this->NewValue;
  }
  /**
   * @access private
   */
  function GetDefaultValue()
  {
    return $this->Content;
  }
  /**
   * @access private
   */
  function BeforeGenerate()
  {
    parent::BeforeGenerate();
    if ($this->DataSource)
      $this->Content = $this->DataSource->GetField($this->Field);
    else
      $this->Debug("Bad DataSource");
  }
}

/*
class TTpDbListSource extends TTpListSource
{
  var $DataSource;
  var $Field;
  function AfterParse()
  {
    //echo "[$this->DataSource]<br>";
    //echo ($this->Page->IsObject($this->DataSource) ? 'Found' : 'Not found')."<br>";
    if (is_string($this->DataSource))
      $this->DataSource = &$this->Page->GetObject($this->DataSource);

// BAD
//    if (is_string($this->DataSource))
//      $this->DataSource &= $this->Page->GetObject($this->DataSource);

// Good
//    if (is_string($this->DataSource))
//      if ($this->Page->IsObject($this->DataSource))
//        $this->DataSource = &$this->Page->Objects[$this->DataSource];

//    echo "[".$this->DataSource->Name."]<br>";
  }
  function GetItems(&$outItems)
  {
    $outItems = array();
    if ($this->DataSource)
    {
      $this->DataSource->Rewind();
      while ($this->DataSource->Next())
      {
        $field = $this->DataSource->GetField($this->Field);
        //if ($this->NumericKeys)
        //$outItems[] = $this->DataSource->GetField($this->Field);
        $outItems[$field] = $field;
      }
    }
    else
      $this->Debug("Bad DataSource");
  }
}
*/

?>
