<?php

# +----------------------------------------------------------------------+
# | TurboPhp 4 - TpLib                                                   |
# | Version 4.0 Beta                                                     |
# +----------------------------------------------------------------------+
# | Copyright (c) 2004 Least-Resistance Software                         |
# +----------------------------------------------------------------------+
# | This source file is subject to the terms of the TurboPhp license,    |
# | that is bundled with this package in the file LICENSE.TXT, and is    |
# | available at through the world-wide-web at                           |
# | http://www.turbophp.com/turbophp/license.txt                         |
# +----------------------------------------------------------------------+

//=============================================================================
// BEGIN LIBRARY
//=============================================================================

define('TP_NAME', 'tpname');
define('TP_CLASS', 'tpclass');
define('TP_HIDDEN', 'tphidden');

define('P_QVALUE', '(?:")([^"]*)(?:")');
define('P_VALUE', '[^"\s>]*');
define('P_WS', '\s*');
define('P_NAME', '[^=\s]*');
define('P_ATTR', '/'.P_WS.'('.P_NAME.')'.P_WS.'='.P_WS.'('.P_QVALUE.'|'.P_VALUE.')/');

define('P_TOKEN', '([^\s/>]*?)');
define('P_ATTRS', '[^>/]*');
define('P_SATTRS', '([^>]*[^/>]+|[^>]*)');

define('P_TPATTRS', TP_NAME .'="([^"]*)" '. TP_CLASS .'="([^"]*)"');
define('P_CONTENT', '((?:<\1>.*?</\1.>)*|.*)');
define('P_OBJECT_TAG', '<'. P_TOKEN . P_WS . P_SATTRS . P_TPATTRS . P_SATTRS . '>');
define('P_END_TAG', '(</\1>)');

define('P_DUAL_TAG', P_OBJECT_TAG . P_CONTENT . P_END_TAG);
define('P_MONO_TAG', '<'. P_TOKEN . P_WS . P_SATTRS . P_TPATTRS . P_SATTRS . '/>');
define('P_OBJECT', '#(?:' . P_DUAL_TAG . '|' . P_MONO_TAG . ')#Usm');

define('P_STYLE', '#[\"; ]*([^\s:]*)\s*:\s*([^;]*);#');

define('M_DUALTAG', 6);

define('M_ELEMENT', 1);
define('M_ATTRIBUTES1', 2);
define('M_TP_NAME', 3);
define('M_TP_CLASS', 4);
define('M_ATTRIBUTES2', 5);
define('M_CONTENT', 6);
define('M_END_TAG', 7);
define('M_ELEMENT2', 8);
define('M_ATTRIBUTES3', 9);
define('M_TP_NAME2', 10);
define('M_TP_CLASS2', 11);
define('M_ATTRIBUTES4', 12);

define('P_BLOCK', '#<!--BEGIN ([^->]*) -->(.*)<!--END \1 -->#Usm');
define('P_MACRO', '#{%([^}]*)}#Usm');
define('TP_MACSTART', '{%');
define('TP_MACEND', '}');

define('TPONCLICK', 'tponclick');
define('TPONSUBMIT', 'tponsubmit');

define('P_INPUTCHARS', '/[a-zA-Z0-9\s\r\n\t\_\-\~\!\@\#\$\%\^\&\*\(\)\|\+\=\{\}\[\]\,\.\;\:\'\"\/\?]*/');
define('P_CLEANCHARS', '/[a-zA-Z0-9\s\_\-\(\)\+\=\{\}\[\]\,\.\;\:]*/');

# +----------------------------------------------------------------------+
# | Utility Functions                                                    |
# +----------------------------------------------------------------------+

function TpSafeArrayGet($inNeedle, &$inArray)
{
  if (array_key_exists($inNeedle, $inArray))
    return $inArray[$inNeedle];
  else
    return '';
}

function TpFilterInput(&$inText)
{
  if (preg_match(P_INPUTCHARS, $inText, $matches))
    return $matches[0];
  else
    return '';
}

function TpFilterRequest()
{
  foreach ($_REQUEST as $name => $value)
    $_REQUEST[$name] = TpFilterInput($value);
}

function TpCleanInput(&$inText)
{
  if (preg_match(P_CLEANCHARS, $inText, $matches))
    return $matches[0];
  else
    return '';
}

# +----------------------------------------------------------------------+
# | TpClassRegistry                                                      |
# +----------------------------------------------------------------------+

if (!isset($TpClasses))
  $TpClasses = array('default' => 'TTpObj');

function TpRegisterClass($inTpClass, $inClass)
{
  global $TpClasses;
  $TpClasses[$inTpClass] = $inClass;
}

# +----------------------------------------------------------------------+
# | TpTemplate                                                           |
# +----------------------------------------------------------------------+

class TTpTemplate
{
  var $Contents;
  var $Blocks;
  function LoadTemplate($inFilename)
  {
    $handle = fopen($inFilename, 'r');
    $this->Contents = fread($handle, filesize ($inFilename));
    fclose($handle);
  }
}

# +----------------------------------------------------------------------+
# | TpApp                                                                |
# +----------------------------------------------------------------------+

class TTpApp
{
  var $Blocks;
  var $Debug;
  var $Handled;
  var $Objects;
  var $OnBeforeInput;
  var $OnBeforeClicks;
  var $OnGenerate;
  var $Template;
  var $TemplateFile;
  var $WasButtonClick;

  function TTpApp($inFile)
  {
    $this->TemplateFile = $inFile;
    $this->Template = new TTpTemplate();
    $this->Objects = array();
    $this->Blocks = array();
    $this->RegisterClasses();
  }

  function RegisterClasses()
  {
    TpRegisterClass('default', 'TTpObj');
    TpRegisterClass('label', 'TTpObj');
    TpRegisterClass('form', 'TTpForm');
    TpRegisterClass('button', 'TTpButton');
    TpRegisterClass('input', 'TTpInput');
    TpRegisterClass('textarea', 'TTpTextArea');
    TpRegisterClass('checkbox', 'TTpCheckBox');
    TpRegisterClass('radio', 'TTpRadio');
    TpRegisterClass('repeater', 'TTpRepeater');
    TpRegisterClass('table', 'TTpTable');
    TpRegisterClass('select', 'TTpSelect');
    TpRegisterClass('ttpwebvar', 'TTpWebVariable');
  }

  function DoEvent($inEvent)
  {
    if ($inEvent <> '')
      return $inEvent(&$this);
  }

  function LoadTemplate()
  {
    $this->Template->LoadTemplate($this->TemplateFile);
  }

  function AddObject(&$inObj)
  {
    $inObj->App = &$this;
    $GLOBALS[$inObj->Name] = &$inObj;
    $this->Objects[$inObj->Name] = &$inObj;
  }

  function ManufactureObject($inClass)
  {
    global $TpClasses;
    if (class_exists($inClass))
      $class = $inClass;
    else if (array_key_exists($inClass, $TpClasses))
      $class = $TpClasses[$inClass];
    else
      $class = $TpClasses['default'];
    return new $class();
  }

  function ExtractSingleTagObject(&$matches)
  {
    if ($matches[M_TP_NAME2] == '')
      return NULL;
    $obj = &$this->ManufactureObject($matches[M_TP_CLASS2]);
    $obj->Class = $matches[M_TP_CLASS2];
    $obj->Name = $matches[M_TP_NAME2];
    $obj->Elt = $matches[M_ELEMENT2];
    $obj->Content = '';
    $obj->EndTag = '';
    $obj->ParseAttrs($matches[M_ATTRIBUTES3]);
    $obj->ParseAttrs($matches[M_ATTRIBUTES4]);
    return $obj;
  }

  function ExtractDualTagObject(&$matches)
  {
    if ($matches[M_TP_NAME] == '')
      return NULL;
    $obj = &$this->ManufactureObject($matches[M_TP_CLASS]);
    $obj->Class = $matches[M_TP_CLASS];
    $obj->Name = $matches[M_TP_NAME];
    $obj->Elt = $matches[M_ELEMENT];
    $obj->Content = $this->ExtractMacros($matches[M_CONTENT]);
    if (count($matches) > M_END_TAG)
      $obj->EndTag = $matches[M_END_TAG];
    $obj->ParseAttrs($matches[M_ATTRIBUTES1]);
    $obj->ParseAttrs($matches[M_ATTRIBUTES2]);
    return $obj;
  }

  function ExtractFields(&$inFields)
  {
    $result = array();
    $v = get_object_vars($this);
    foreach($inFields as $key => $value)
    {
      $t = substr($key, 0, 2);
      $n = substr($key, 2);
      if (($t == 'tp') && (array_key_exists($n, $v)))
        $this->$n = $value;
      else
        $result[$key] = $value;
    }
    return $result;
  }

  function ExtractApp(&$inObj)
  {
    $this->Attributes = $this->ExtractFields($inObj->Attributes);
    $this->OnGenerate = $inObj->OnGenerate;
    if ($this->OnGenerate == '')
      $this->OnGenerate = $inObj->GetAttribute('tponrun');
  }

  function ExtractObject(&$matches)
  {
    if ($matches[M_TP_NAME] <> '')
      $obj = &$this->ExtractDualTagObject($matches);
    else
      $obj = &$this->ExtractSingleTagObject($matches);
    if ($obj == NULL)
      return $matches[0];
    else if ($obj->Name == 'app')
    {
      $this->extractApp($obj);
//      $this->OnGenerate = $obj->OnGenerate;
//      if ($this->OnGenerate == '')
//        $this->OnGenerate = $obj->GetAttribute('tponrun');
      return '';
    }
    else
    {
      $this->AddObject($obj);
      return TP_MACSTART . $obj->Name . TP_MACEND;
    }
  }

  function ExtractObjects(&$inText)
  {
    return preg_replace_callback(P_OBJECT, array(&$this, 'ExtractObject'),
      $inText);
  }

  function ExtractBlock(&$matches)
  {
    $this->Blocks[$matches[1]] = $this->ExtractMacros($matches[2]);
    return TP_MACSTART . $matches[1] . TP_MACEND;
  }

  function ExtractBlocks(&$inText)
  {
    return preg_replace_callback(P_BLOCK, array(&$this, 'ExtractBlock'),
      $inText);
  }

  function ExtractMacros(&$inText)
  {
    return $this->ExtractObjects($this->ExtractBlocks($inText));
  }

  function IsObject(&$inName)
  {
    return array_key_exists($inName, $this->Objects);
  }

  function IsBlock(&$inName)
  {
    return array_key_exists($inName, $this->Blocks);
  }

  function &GetObject($inName)
  {
    if ($this->IsObject($inName))
    {
      $obj = &$this->Objects[$inName];
      return $obj;
    }
    else
      return false;
  }

  function InterpolateObject(&$matches)
  {
    if ($this->IsObject($matches[1]))
      $r = $this->Objects[$matches[1]]->Generate();
    else if ($this->IsBlock($matches[1]))
      $r = $this->Blocks[$matches[1]];
    else
      return $matches[1];
    return $this->Interpolate($r);
  }

  function Interpolate(&$inText)
  {
    return preg_replace_callback(P_MACRO, array(&$this, 'InterpolateObject'),
      $inText);
  }

  function DoForAllObjects($inMethod)
  {
    $keys = array_keys($this->Objects);
    foreach ($keys as $key)
    {
      $this->Objects[$key]->$inMethod();
    }
  }

  function AfterParse()
  {
    $this->DoForAllObjects('AfterParse');
//    $keys = array_keys($this->Objects);
//    foreach ($keys as $key)
//      $this->Objects[$key]->AfterParse();
  }

  function ParseTemplate()
  {
    $this->LoadTemplate();
    $this->Template->Contents = $this->ExtractMacros($this->Template->Contents);
    $this->AfterParse();
  }

  function DoRequestForAllObjects($inMethod)
  {
    if (count($_REQUEST) > 0)
    {
      $keys = array_keys($this->Objects);
      foreach ($keys as $key)
      {
        $this->Objects[$key]->$inMethod();
        if ($this->Handled)
          break;
      }
    }
  }

  function ProcessInputs()
  {
    $this->DoEvent($this->OnBeforeInput);
    $this->DoForAllObjects('BeforeInput');
    $this->DoRequestForAllObjects('ProcessInput');
  }

  function ProcessClicks()
  {
    $this->WasButtonClick = false;
    $this->DoEvent($this->OnBeforeClicks);
    $this->DoForAllObjects('BeforeClicks');
    $this->DoRequestForAllObjects('ProcessClicks');
    $this->ProcessDefaultClick();
  }

  function ProcessRequest()
  {
    $this->ProcessInputs();
    $this->ProcessClicks();
  }

  function ProcessDefaultClick()
  {
    if (array_key_exists('tpsubmit', $_REQUEST) && !$this->WasButtonClick)
    {
      $n = $_REQUEST['tpsubmit'];
      if ($this->IsObject($n))
      {
        $b = $this->Objects[$n]->DefaultButton;
        if ($this->IsObject($b))
          $this->Objects[$b]->Click();
      }
    }
  }

  function Generate()
  {
    $this->ProcessRequest();
    $this->DoForAllObjects('BeforeGenerate');
    $this->DoEvent($this->OnGenerate);
    if (!$this->Handled)
      return $this->Interpolate($this->Template->Contents);
    else
      return '';
  }

  function Run()
  {
    $this->ParseTemplate();
    return $this->Generate();
  }

  function DumpRequest()
  {
    echo "Request:<pre>";
    print_r($_REQUEST);
    echo "</pre>";
  }

  function DumpObjects()
  {
    echo '<br><br>==object dump ('.count($this->Objects).')==<br><br>';
    $i = 0;
    foreach ($this->Objects as $obj)
    {
      echo '['.$i++.']<br>';
      $obj->Dump();
      echo "<br>";
    }
  }

  function DumpBlocks()
  {
    echo '<br><br>==block dump ('.count($this->Blocks).')==<br><br>';
    $i = 0;
    foreach ($this->Blocks as $name => $blk)
    {
      echo '['.$i++.': '.$name.']<br><pre>';
      echo htmlspecialchars($blk);
      echo "</pre><br>";
    }
  }
}

# +----------------------------------------------------------------------+
# | TpObj                                                                |
# +----------------------------------------------------------------------+

class TTpObj
{
  var $App;
  var $Name;
  var $Class;
  var $Elt;
  var $Attributes;
  var $Style;
  var $Styles;
  var $Content;
  var $EndTag;
  var $Hidden;
  var $OnGenerate;

  function TTpObj()
  {
    $this->Init();
  }

  function Init()
  {
    $this->Attributes = array();
    $this->Styles = array();
  }

  function Assign(&$inObj)
  {
    $this->App = &$inObj->App;
    $this->Name = $inObj->Name;
    $this->Class = $inObj->Class;
    $this->Elt = $inObj->Elt;
    $this->Attributes = $inObj->Attributes;
    $this->Style = $inObj->Style;
    $this->Styles = $inObj->Styles;
    $this->Content = $inObj->Content;
    $this->EndTag = $inObj->EndTag;
    $this->Hidden = $inObj->Hidden;
    $this->OnGenerate = $inObj->OnGenerate;
  }

  function DebugId()
  {
    return "($this->Class) $this->Name: ";
  }

  function Debug($inMsg)
  {
    TpDbgMsg($this->DebugId() . $inMsg . "[" . $this->StackTrace() ."]");
  }

  function StackTrace()
  {
    $s = '';
    $bt = debug_backtrace();
    $c = count($bt);
    if ($c > 2)
      $s = $bt[2]["function"];
    for ($i = 3; $i < $c; $i++)
      $s = $s.'.'.$bt[$i]["function"];
    return $s;
  }

  function Dbg()
  {
    echo $this->StackTrace();
//    $bt = debug_backtrace();
//    foreach ($bt as $frame)
//      echo "Function: ".$frame["function"]."<br>";
//    echo "File: ".$bt[1]["file"]."<br>";
//    echo "Line: ".$bt[1]["line"]."<br>";
//    echo "Line: ".$bt[1]["line"]."<br>";
//    echo "<pre>";
//    var_dump($bt);
//    echo "</pre>";
  }

  function DoEvent($inEvent)
  {
    if ($inEvent <> '')
      return $inEvent(&$this);
    else
      return 0;
  }

  function ExtractStyles($inStyles)
  {
    $result = array();
    if (preg_match_all(P_STYLE, $inStyles, $matches, PREG_SET_ORDER))
      foreach ($matches as $match)
        $result[$match[1]] = $match[2];
    return $result;
  }

  function ExtractAttrs($inAttrs)
  {
    $result = array();
    if (preg_match_all(P_ATTR, $inAttrs, $matches, PREG_SET_ORDER))
    {
      foreach ($matches as $match)
      {
        $name = $match[1];
        if (count($match) > 3)
          $value = $match[3];
        else
          $value = $match[2];
        $result[$name] = $value;
      }
    }
    return $result;
  }

  function StoreAttr($inName, $inValue)
  {
    if ((substr($inName, 0, 2) == 'tp') &&
      array_key_exists(substr($inName, 2), get_object_vars($this)))
    {
      $inName = substr($inName, 2);
      $this->$inName = $inValue;
    }
    else if ($inName == TP_NAME)
      $this->Name = $inValue;
    else if ($inName == TP_CLASS)
      $this->Class = $inValue;
    else if ($inName ==  TP_HIDDEN)
      $this->Hidden = true;
    else if ($inName == 'tpongenerate')
      $this->OnGenerate = $inValue;
    else if ($inName == 'style')
      $this->Styles = $this->ExtractStyles($inValue);
    else
      $this->Attributes[$inName] = $inValue;
  }

  function StoreAttrs(&$inArray)
  {
     foreach ($inArray as $name => $value)
       $this->StoreAttr($name, $value);
  }

  function ParseAttrs(&$inAttrs)
  {
    $this->StoreAttrs($this->ExtractAttrs($inAttrs));
  }

  function AfterParse()
  {
  }

  function BeforeInput()
  {
  }

  function ProcessInput()
  {
  }

  function BeforeClicks()
  {
  }

  function ProcessClicks()
  {
  }

  function GetDefaultValue()
  {
    return $this->Content;
  }

  function GetAttribute($inAttr)
  {
    return TpSafeArrayGet($inAttr, $this->Attributes);
  }

  function GetStyle($inStyle)
  {
    return TpSafeArrayGet($inStyle, $this->Styles);
  }

  function DoStylize(&$inStyle, &$inValue)
  {
    $this->Styles[$inStyle] = $inValue;
  }

  function Stylize($inStyle, $inValue = '')
  {
    if (is_array($inStyle))
    {
      foreach ($inStyle as $style => $value)
        $this->DoStylize($style, $value);
    } else
      $this->DoStylize($inStyle, $inValue);
  }

  function ArrayToAttrs(&$inArray)
  {
    $result = '';
    foreach ($inArray as $name => $value)
      $result .= ' ' . $name . '="' . $value. '"';
    return $result;
  }

  function GetHtmlAttrs()
  {
    return $this->ArrayToAttrs($this->Attributes);
  }

  function ArrayToStyles(&$inArray)
  {
    $result = '';
    foreach ($inArray as $name => $value)
      if ($value != '')
        $result .= ' ' . $name . ': ' . $value. ';';
    return trim($result);
  }

  function GetHtmlStyle()
  {
    $style = $this->ArrayToStyles($this->Styles);
    if ($style != "")
      return ' style="' . $style . '"';
    else
      return '';
  }

  function GetStartTag()
  {
    return '<'.$this->Elt.$this->GetHtmlAttrs().$this->GetHtmlStyle().
     ($this->EndTag == '' ? '/>' : '>');
  }

  function GetContent()
  {
    return $this->Content;
  }

  function GetHtml()
  {
    return $this->GetStartTag().$this->GetContent().$this->EndTag;
  }

  function BeforeGenerate()
  {
    $this->DoEvent($this->OnGenerate);
  }

  function Generate()
  {
    $this->BeforeGenerate();
    if ($this->Hidden)
      return '';
    else
      return $this->GetHtml();
  }

  function Dump()
  {
    if ($this->Hidden)
      echo "(hidden) ";
    echo "$this->Name: ".get_class($this)." [$this->Class]<br>";
    if ($this->Elt <> '')
      echo "Elt: $this->Elt<br>";
    if (count($this->Attributes) > 0)
    {
      print_r($this->Attributes);
      echo "<br>";
    }
    if (count($this->Styles) > 0)
    {
      print_r($this->Styles);
      echo "<br>";
    }
    if ($this->Content <> '')
      echo "Content: ".htmlspecialchars($this->Content)."<br>";
    if ($this->OnGenerate <> '')
      echo "OnGenerate: ".htmlspecialchars($this->OnGenerate)."<br>";
  }
}

# +----------------------------------------------------------------------+
# | Objects                                                              |
# +----------------------------------------------------------------------+

class TTpSilentObj extends TTpObj
{
  function Generate()
  {
     return '';
  }
}

class TTpListSource extends TTpSilentObj
{
  // Abstract. Must be provided by subclass.
  //function GetItems(&$outItems)
  //{
  //}
}

class TTpWebVariable extends TTpSilentObj
{
  var $Value;
  var $Form;
  function GetDefaultValue()
  {
    return $this->Value;
  }
  function ProcessInput()
  {
    if ($this->Name != '')
      if (array_key_exists($this->Name, $_REQUEST))
        $this->Value = $_REQUEST[$this->Name];
  }
  function Dump()
  {
    parent::Dump();
    echo "Value: $this->Value<br>";
  }
}

class TTpFileList extends TTpSilentObj
{
  var $File;
  var $Files;
  var $Folder;
  var $FullPath;
  var $OnFilter;
  function Generate()
  {
    $this->ListFiles();
  }
  function ListFiles()
  {
    $prefix = ($this->FullPath ? $this->Folder : '');
    $this->Files = array();
    if (is_dir($this->Folder))
    {
      if ($dh = opendir($this->Folder))
      {
          while ($this->File = readdir($dh))
            if (($this->File != '.') && ($this->File != '..'))
            {
              $this->DoEvent($this->OnFilter);
              if ($this->File <> '')
                $this->Files[] = $prefix.$this->File;
            }
          closedir($dh);
      }
    }
  }
  function Dump()
  {
    parent::Dump();
    echo "Folder: $this->Folder<br>";
    echo "FullPath: $this->FullPath<br>";
    echo "OnFilter: $this->OnFilter<br>";
  }
}

# +----------------------------------------------------------------------+
# | TpControls                                                           |
# +----------------------------------------------------------------------+

class TTpControl extends TTpObj
{
  function SearchRequest($inMethod)
  {
    $key = $this->GetAttribute("name");
    if ($key != '')
      if (array_key_exists($key, $_REQUEST))
        $this->$inMethod($_REQUEST[$key]);
  }
}

class TTpForm extends TTpControl
{
  var $DefaultButton;
}

class TTpInput extends TTpControl
{
  var $Encoded;
  var $NewValue;
  var $OnSubmit;
  var $Successful;
  var $Value;
  function StoreAttr($inName, $inValue)
  {
    if ($inName == TPONSUBMIT)
      $this->OnSubmit = $inValue;
    else
      parent::StoreAttr($inName, $inValue);
  }
  function AfterParse()
  {
    if (array_key_exists('value', $this->Attributes))
      $this->Value = $this->Attributes['value'];
    if (!array_key_exists('name', $this->Attributes))
      $this->Attributes['name'] = $this->Name;
  }
  function DecodedValue($inValue)
  {
    if ($this->Encoded)
      return base64_decode(urldecode($inValue));
    else
      return $inValue;
  }
  function EncodedValue($inValue)
  {
    if ($this->Encoded)
      return urlencode(base64_encode($inValue));
    else
      return $inValue;
  }
  function SetValueAttribute()
  {
    $this->Attributes['value'] = $this->EncodedValue($this->Value);
  }
  function StoreValue()
  {
    $this->Value = $this->NewValue;
    $this->SetValueAttribute();
  }
  function Submit($inValue)
  {
    $this->NewValue = $this->DecodedValue($inValue);
    $this->DoEvent($this->OnSubmit);
    $this->StoreValue();
    $this->Successful = true;
  }
  function ProcessInput()
  {
    $this->Successful = false;
    $this->SearchRequest('Submit');
  }
  function GetStartTag()
  {
    $this->SetValueAttribute();
    return parent::GetStartTag();
  }
  function GetDefaultValue()
  {
    return $this->Value;
  }
  function Dump()
  {
    parent::Dump();
    echo "OnSubmit: $this->OnSubmit<br>";
  }
}

class TTpTextArea extends TTpInput
{
  function StoreValue()
  {
    $this->Content = $this->NewValue;
  }
  function GetDefaultValue()
  {
    return $this->Content;
  }
}

class TTpClickable extends TTpInput
{
  var $OnClick;
  function StoreAttr($inName, $inValue)
  {
    if ($inName == TPONCLICK)
      $this->OnClick = $inValue;
    else
      parent::StoreAttr($inName, $inValue);
  }
  function Click()
  {
    $this->DoEvent($this->OnClick);
  }
  function ProcessClicks()
  {
    if ($this->Successful)
      $this->Click();
  }
  function Dump()
  {
    parent::Dump();
    echo "OnClick: $this->OnClick<br>";
  }
}

class TTpButton extends TTpClickable
{
  function Click()
  {
    $this->App->WasButtonClick = true;
    parent::Click();
  }
}

class TTpCheckBox extends TTpClickable
{
  var $Checked;
  function AfterParse()
  {
    if (array_key_exists('checked', $this->Attributes))
    {
      unset($this->Attributes['checked']);
      $this->Checked = true;
    }
  }
  function ProcessClicks()
  {
    if ($this->Successful <> $this->Checked)
    {
      $this->Checked = $this->Successful;
      $this->Click();
    }
  }
  function GetHtmlAttrs()
  {
    return parent::GetHtmlAttrs().($this->Checked ? ' checked="checked"' : '');
  }
}

class TTpRadio extends TTpCheckBox
{
  function SetValueAttribute()
  {
    $this->Attributes['value'] = $this->Name;
  }
  function SearchRequest($inMethod)
  {
    $name = $this->GetAttribute('name');
    $value = $this->GetAttribute('value');
    foreach ($_REQUEST as $req_name => $req_value)
    {
      if ($req_name == $name && $req_value == $value)
      {
        $this->$inMethod($name);
        break;
      }
    }
  }
}

class TTpSelect extends TTpClickable
{
  var $Options;
  var $Selected;
  var $Source;
  function Init()
  {
    parent::Init();
    $this->Options = array();
  }
  function ExtractOptions()
  {
    // Grab up the option tags
    preg_match_all("|<option[^<]*?|Ui", $this->Content, $options);
    // Content is now dynamic
    $this->Content = '';
    // Parse options into array
    $this->Options = array();
    foreach ($options[0] as $key => $option)
    {
      // Grab the option content
      preg_match_all("/(?:[^>]*?)>([^<]*?)/Ui", $option, $contents);
      $content = $contents[1][0];
      // Grab the option attributes
      $a = $this->ExtractAttrs($option);
      if (array_key_exists("value", $a))
        $value = $a["value"];
      else
        $value = $content;
      if (array_key_exists("selected", $a))
        $this->Selected = $value;
      // Store the option data
      $this->Options[$value] = $content;
    }
  }
  function AfterParse()
  {
    parent::AfterParse();
    $this->ExtractOptions();
    if (is_string($this->Source))
      $this->Source = &$this->App->GetObject($this->Source);
  }
  function StoreValue()
  {
    $this->Selected = $this->NewValue;
  }
  function ProcessInput()
  {
    $lastSelected = $this->Selected;
    parent::ProcessInput();
    $this->GetSourceOptions();
    if ($this->Selected <> $lastSelected)
      $this->Click();
  }
  function IsGoodSelection()
  {
    return array_key_exists($this->Selected, $this->Options);
  }
  function SetDefaultSelection()
  {
    $keys = array_keys($this->Options);
    if (count($keys) > 0)
      $this->Selected = $keys[0];
    else
      $this->Selected = '';
  }
  function GetSelectedValue()
  {
    if (!$this->IsGoodSelection())
      $this->SetDefaultSelection();
    if ($this->IsGoodSelection())
      return $this->Options[$this->Selected];
    else
      return "";
  }
  function GetDefaultValue()
  {
    return $this->GetSelectedValue();
  }
  function GetHtmlOptions()
  {
    $result = '';
    if (!$this->IsGoodSelection())
      $this->SetDefaultSelection();
    foreach ($this->Options as $key => $value)
    {
      $a = array('value' => $key);
      if ($this->Selected == $key)
        $a['selected'] = 'selected';
      $result .= '<option'.$this->ArrayToAttrs($a).'>'.$value."</option>\n";
    }
    return $result;
  }
  function GetSourceOptions()
  {
    if ($this->Source)
      $this->Source->GetItems($this->Options);
  }
  function BeforeClicks()
  {
    $this->GetSourceOptions();
  }
  function GetContent()
  {
    return $this->GetHtmlOptions();
  }
  function Dump()
  {
    parent::Dump();
    echo "Selected: $this->Selected<br>";
    if ($this->Source)
      echo "Source: ".$this->Source->Name."<br>";
    echo "Options: <pre>";
    print_r($this->Options);
    echo "</pre><br>";
  }
}

class TTpRepeater extends TTpControl
{
  var $Index;
  var $Count;
  var $Stop;
  var $OnRepeat;

  function Init()
  {
    parent::Init();
    $this->Count = 4;
  }

  function StoreAttr($inName, $inValue)
  {
    if ($inName == 'tponrepeat')
      $this->OnRepeat = $inValue;
    else
      parent::StoreAttr($inName, $inValue);
  }

  function ShouldContinue()
  {
    return (!$this->Stop &&
      ($this->Count < 1 ? false : $this->Index < $this->Count));
  }

  function Repeat()
  {
    $this->Index++;
    if ($this->ShouldContinue())
      $this->DoEvent($this->OnRepeat);
    return $this->ShouldContinue();
  }

  function GetHtml()
  {
    $this->Stop = false;
    $this->Index = -1;
    $result = '';
    while ($this->Repeat())
      $result .= $this->App->Interpolate($this->Content);
    return $result;
  }
}

class TTpTable extends TTpControl
{
  var $Rows, $Cols;
  var $OnCell;
  var $OnRow;
  var $Row, $Col;
  var $RowSpan, $ColSpan;
  var $HeaderCell;
  var $CellAttributes;
  var $RowAttributes;
  var $Cells;

  function Init()
  {
    parent::Init();
    $this->RowAttributes = array();
    $this->CellAttributes = array();
  }

  function StoreAttr($inName, $inValue)
  {
    switch ($inName)
    {
      case 'tprows':
        $this->Rows = $inValue;
        break;
      case 'tpcols':
        $this->Cols = $inValue;
        break;
      case 'tponcell':
        $this->OnCell = $inValue;
        break;
      default:
        parent::StoreAttr($inName, $inValue);
        break;
    }
  }

  function ClearCells()
  {
    if ($this->Rows != 0 && $this->Cols != 0)
    {
      $f = array_fill(0, $this->Rows, false);
      $this->Cells = array_fill(0, $this->Cols, $f);
    }
  }

  function MarkSpannedCells()
  {
    for ($j=0; $j<$this->RowSpan; $j++)
      for ($i=0; $i<$this->ColSpan; $i++)
        $this->Cells[$this->Col + $i][$this->Row + $j] = true;
  }

  function GetSpanAttributes()
  {
    $result = '';
    if ($this->ColSpan > 1)
      $result .= ' colspan="'.$this->ColSpan.'"';
    if ($this->RowSpan > 1)
      $result .= ' rowspan="'.$this->RowSpan.'"';
    return $result;
  }

  function GetFixedHtml()
  {
    $result = '';
    $this->ClearCells();
    for ($this->Row=0; $this->Row < $this->Rows; $this->Row++)
    {
      $this->DoEvent($this->OnRow);
      $result .= '<tr'. $this->ArrayToAttrs($this->RowAttributes) . ">\n";
      for ($this->Col=0; $this->Col < $this->Cols; $this->Col++)
      {
        if (!$this->Cells[$this->Col][$this->Row])
        {
          $this->ColSpan = 1;
          $this->RowSpan = 1;
          $this->DoEvent($this->OnCell);
          $this->MarkSpannedCells();
          $t = ($this->HeaderCell ? 'th' : 'td');
          $a = $this->GetSpanAttributes().$this->ArrayToAttrs($this->CellAttributes);
          $result .= "<$t$a>".$this->App->Interpolate($this->Content)."</$t>";
        }
      }
      $result .= "\n</tr>\n";
    }
    return $result;
  }

  function GetContent()
  {
    return $this->GetFixedHtml();
  }

/*
  function GetScrollingHtml()
  {
    $result = '';
    $this->ClearCells();
    for ($this->Row=0; ($this->Row < $this->Rows) && ($this->Row == 0); $this->Row++)
    {
      $this->DoEvent($this->OnRow);
      $result .= '<tr'. $this->ArrayToAttrs($this->RowAttributes) . ">\n";
      for ($this->Col=0; $this->Col < $this->Cols; $this->Col++)
      {
         $this->ColSpan = 1;
         $this->RowSpan = 1;
         $this->DoEvent($this->OnCell);
         $this->MarkSpannedCells();
         $t = ($this->HeaderCell ? 'th' : 'td');
         $a = $this->GetSpanAttributes().$this->ArrayToAttrs($this->CellAttributes);
         $result .= "<$t$a>".$this->App->Interpolate($this->Content)."</$t>";
      }
      $result .= "\n</tr>\n";
    }
    $html = $this->GetStartTag().$result.$this->EndTag;
    //
    $result = '';
    for ($this->Row=1; $this->Row < $this->Rows; $this->Row++)
    {
      $this->DoEvent($this->OnRow);
      $result .= '<tr'. $this->ArrayToAttrs($this->RowAttributes) . ">\n";
      for ($this->Col=0; $this->Col < $this->Cols; $this->Col++)
      {
        if (!$this->Cells[$this->Col][$this->Row])
        {
          $this->ColSpan = 1;
          $this->RowSpan = 1;
          $this->DoEvent($this->OnCell);
          $this->MarkSpannedCells();
          $t = ($this->HeaderCell ? 'th' : 'td');
          $a = $this->GetSpanAttributes().$this->ArrayToAttrs($this->CellAttributes);
          $result .= "<$t$a>".$this->App->Interpolate($this->Content)."</$t>";
        }
      }
      $result .= "\n</tr>\n";
    }
    $result = $this->GetStartTag().$result.$this->EndTag;
    $result = '<div style="width:600px; height:400px; overflow: scroll;">'
            . $result .'</div>';
    return $html.$result;
  }
*/
}

?>
