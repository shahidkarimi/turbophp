<?php

# +----------------------------------------------------------------------+
# | TurboPhp 4 - TpDb
# | Version 4.0 Beta                                                     |
# +----------------------------------------------------------------------+
# | Copyright (c) 2004 Least-Resistance Software                         |
# +----------------------------------------------------------------------+
# | This source file is subject to the terms of the TurboPhp license,    |
# | that is bundled with this package in the file LICENSE.TXT, and is    |
# | available at through the world-wide-web at                           |
# | http://www.turbophp.com/turbophp/license.txt                         |
# +----------------------------------------------------------------------+

TpRegisterClass('dbtext', 'TTpDbText');
TpRegisterClass('dbedit', 'TTpDbEdit');
TpRegisterClass('dbtextarea', 'TTpDbTextArea');
TpRegisterClass('dblistsource', 'TTpDbListSource');

define('P_QUERY_PARAM', '#{%([^}]*)}#Usm');

class TTpDbConnect extends TTpSilentObj
{
  var $ConfigFile;
  var $Connection;
  var $ConnectManually;
  var $Database;
  var $Host;
  var $OnConnect;
  var $Password;
  var $Persistent;
  var $UserName;

  function Init()
  {
    parent::Init();
    $this->Host = '';
  }
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
  function AfterParse()
  {
    global $tpSupportPath;
    if ($this->ConfigFile == '')
      $this->ConfigFile = $tpSupportPath . "$this->Name.conf.php";
    if (!$this->ConnectManually)
      $this->AutoConnect();
  }
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
  function SelectDatabase()
  {
    if ($this->Connection && $this->Database <> '')
      return $this->DoSelectDb(); // abstract: must be provided in subclass
    else if (!$this->App->Debug)
      return false;
    else if ($this->Connection)
      echo $this->DebugId()." no database selected (property is empty).<br>";
    else
      echo $this->DebugId()." no connection available.<br>";
  }
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
  function AutoConnect()
  {
    $this->Connect();
    $this->SelectDatabase();
  }
  function ConnectOrDie($inMessage = '')
  {
    if ($inMessage == '')
      $inMessage = 'Could not connect: ';
    $this->Connect() or die($inMessage . mysql_error() . '<br>');
  }
  function Close()
  {
    $this->Connection = false;
  }
  function Dump()
  {
    parent::Dump();
    echo "Host: [$this->Host]<br>";
    echo "User: [$this->UserName]<br>";
    echo "Password: <hidden><br>";
    echo "Persistent: [$this->Persistent]<br>";
    echo "Database: [$this->Database]<br>";
  }
}

class TTpDataSource extends TTpSilentObj
{
  var $Result;
  var $Row;
  var $OnBeforeExecute;
  var $OnFailure;
  var $OnSuccess;
  function Init()
  {
    parent::Init();
    $this->Row = false;
  }
  function ExecuteQuery()
  {
    // Abstract. Must be provided by subclass.
  }
  function Execute()
  {
    if ($this->App->Debug)
      echo $this->DebugId()."Execute()<br>";
    $this->DoEvent($this->OnBeforeExecute);
    $this->ExecuteQuery();
    if (!$this->Row)
      $this->DoEvent($this->OnFailure);
    else
      $this->DoEvent($this->OnSuccess);
  }
  function NeedData()
  {
    if ($this->App->Debug)
      echo $this->DebugId()."NeedData() ...";
    if (!$this->Result)
    {
      if ($this->App->Debug)
        echo "calling Execute()<br>";
      $this->Execute();
    }
    else if ($this->App->Debug)
      echo "using cache.<br>";
  }
  function EOF()
  {
    return !$this->Row;
  }
  function Rewind()
  {
    $this->NeedData();
  }
  function Next()
  {
    $this->NeedData();
  }
  function GetField($inFieldId)
  {
    $this->NeedData();
    if (!$this->Row)
      return '(bad row)';
    else
      return TpSafeArrayGet($inFieldId, $this->Row);
  }
}

class TTpQuery extends TTpDataSource
{
  var $SQL;
  var $Params;
  function Init()
  {
    parent::Init();
    $this->Params = array();
  }
  function AfterParse()
  {
    parent::AfterParse();
    $this->SQL = base64_decode($this->SQL);
  }
  function InterpolateParam(&$matches)
  {
    $obj = $this->App->GetObject($matches[1]);
    if ($obj)
      $p = $obj->GetDefaultValue();
    else
      $p = TpSafeArrayGet($matches[1], $this->Params);
    return $this->Interpolate($p);
  }
  function Interpolate(&$inText)
  {
    return preg_replace_callback(
      P_QUERY_PARAM,
      array(&$this, 'InterpolateParam'),
      $inText
    );
  }
  function InterpolatedQuery()
  {
    return $this->Interpolate($this->SQL);
  }
  function Dump()
  {
    parent::Dump();
    echo "SQL: [$this->SQL]<br>";
    if (count($this->Params) > 0)
    {
      print_r($this->Params);
      echo "<br>";
    }
  }
}

class TTpDbText extends TTpObj
{
  var $DataSource;
  var $Field;
  function AfterParse()
  {
    if ($this->App->Debug)
    {
      if (!$this->DataSource)
        echo $this->DebugId()."AfterParse(): parsed empty DataSource [$this->DataSource]<br>";
      $s = $this->DataSource;
    }
    //
    if (is_string($this->DataSource))
      $this->DataSource = &$this->App->GetObject($this->DataSource);
    //
    if ($this->App->Debug)
    {
      if (!$this->DataSource)
        echo $this->DebugId()."AfterParse(): failed to get DataSource object [$s]<br>";
      else
        echo $this->DebugId()."AfterParse(): DataSource object [$s => ".$this->DataSource->Name."]<br>";
      $this->App->DumpObjects();
    }
  }
  function BeforeGenerate()
  {
    if ($this->App->Debug)
      echo $this->DebugId()."BeforeGenerate(): DataSource object [$this->DataSource]<br>";
    parent::BeforeGenerate();
    if ($this->DataSource)
      $this->Content = $this->DataSource->GetField($this->Field);
    else if ($this->App->Debug)
      echo $this->DebugId()."Bad DataSource<br>";
  }
  function Dump()
  {
    parent::Dump();
    echo "DataSource: [".$this->DataSource->Name."]<br>";
    echo "Field: [$this->Field]<br>";
  }
}

class TTpDbInput extends TTpInput
{
  var $DataSource;
  var $Field;
  function AfterParse()
  {
    parent::AfterParse();
    if ($this->App->Debug)
      echo $this->DebugId()."DataSource = $this->DataSource<br>";
    if (is_string($this->DataSource))
      $this->DataSource = &$this->App->GetObject($this->DataSource);
  }
  function GetData()
  {
    if ($this->DataSource)
      return $this->DataSource->GetField($this->Field);
    else if ($this->App->Debug)
      echo $this->DebugId()."Bad DataSource<br>";
    return '';
  }
  function Dump()
  {
    parent::Dump();
    echo "DataSource: [".$this->DataSource->Name."]<br>";
    echo "Field: [$this->Field]<br>";
  }
}

class TTpDbEdit extends TTpDbInput
{
  function BeforeGenerate()
  {
    parent::BeforeGenerate();
    if ($this->DataSource)
      $this->Value = $this->DataSource->GetField($this->Field);
    else if ($this->App->Debug)
      echo $this->DebugId()."Bad DataSource<br>";
  }
}

class TTpDbTextArea extends TTpDbInput
{
  function StoreValue()
  {
    $this->Content = $this->NewValue;
  }
  function GetDefaultValue()
  {
    return $this->Content;
  }
  function BeforeGenerate()
  {
    parent::BeforeGenerate();
    if ($this->DataSource)
      $this->Content = $this->DataSource->GetField($this->Field);
    else if ($this->App->Debug)
      echo $this->DebugId()."Bad DataSource<br>";
  }
}

class TTpDbListSource extends TTpListSource
{
  var $DataSource;
  var $Field;
  function AfterParse()
  {
    //echo "[$this->DataSource]<br>";
    //echo ($this->App->IsObject($this->DataSource) ? 'Found' : 'Not found')."<br>";
    if (is_string($this->DataSource))
      $this->DataSource = &$this->App->GetObject($this->DataSource);

// BAD
//    if (is_string($this->DataSource))
//      $this->DataSource &= $this->App->GetObject($this->DataSource);

// Good
//    if (is_string($this->DataSource))
//      if ($this->App->IsObject($this->DataSource))
//        $this->DataSource = &$this->App->Objects[$this->DataSource];

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
      if ($this->App->Debug)
      {
        echo $this->DebugId()."GetItems:<br><pre>";
        print_r($outItems);
        echo "</pre><br>";
      }
    }
    else if ($this->App->Debug)
      echo $this->DebugId()."GetItems: bad DataSource<br>";
  }
  function Dump()
  {
    parent::Dump();
    echo "DataSource: [".$this->DataSource->Name."]<br>";
    echo "Field: [$this->Field]<br>";
  }
}

?>
