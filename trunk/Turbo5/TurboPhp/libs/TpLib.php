<?php

/**
 *  TurboPhp 4 - TpLib
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
define('TPCLASS', 'tpClass');

define('P_STYLE', '#[\"; ]*([^\s:]*)\s*:\s*([^;]*);#');

define('P_BLOCK', '#<!--BEGIN ([^->]*) -->(.*)<!--END \1 -->#Usm');
define('P_MACRO', '#{%([^}]*)}#Usm');

define('TP_MACSTART', '{%');
define('TP_MACEND', '}');

define('P_INPUTCHARS', '/[a-zA-Z0-9\s\r\n\t\_\-\~\!\@\#\$\%\^\&\*\(\)\|\+\=\{\}\[\]\,\.\;\:\'\"\/\?]*/');
define('P_CLEANCHARS', '/[a-zA-Z0-9\s\_\-\(\)\+\=\{\}\[\]\,\.\;\:]*/');

define('TPMODULEFLAG', 'TpModularize');
define('TPPORTABLES', 'TpPortables');

/**#@-*/

/**
 * Get array element without warnings if element doesn't exist.
 * @param $inNeedle Array key to search
 * @param $inArray Array to search
 * @return mixed $inArray[$inNeedle] or '' if $inNeedle is not a key of
 * $inArray.
 */
function TpSafeArrayGet($inNeedle, &$inArray)
{
  if (array_key_exists($inNeedle, $inArray))
    return $inArray[$inNeedle];
  else
    return '';
}

/**
 * Filter characters from input text.
 * Returns left-most part of $inText that contains basic characters only.
 * Use this function to strip away odd characters or special bytes from user
 * input. Less restrictive than {@link TpCleanInput}.
 * @param string $inText String to be filtered.
 * @return string Filtered string.
 */
function TpFilterInput(&$inText)
{
  if (preg_match(P_INPUTCHARS, $inText, $matches))
    return $matches[0];
  else
    return '';
}

/**
 * Filter characters from $_REQUEST values.
 * Performs {@link TpFilterInput} on each member of $_REQUEST array.
 */
function TpFilterRequest()
{
  foreach ($_REQUEST as $name => $value)
    $_REQUEST[$name] = TpFilterInput($value);
}

/**
 * Clean characters from input text.
 * Returns left-most part of $inText that contains basic characters only.
 * Use this function to strip away odd characters or special bytes from user
 * input. More restrictive than {@link TpFilterInput}.
 * @param string $inText String to be cleaned.
 * @return string Cleaned string.
 */
function TpCleanInput(&$inText)
{
  if (preg_match(P_CLEANCHARS, $inText, $matches))
    return $matches[0];
  else
    return '';
}

/**
 * Stores data parsed from a request variable.
 * Request variable (GET, POST, and COOKIE) data are parsed into
 * TTpRequestVariable objects.
 * Information encoded into the variable text, such as hit-coordinates and
 * control indices are parsed into fields.
 * See {@link TTpPage.Inputs}.
 * @package TurboPhpLib
 */
class TTpRequestVariable
{
  var $Name;
  var $Index;
  var $Coord;
  var $Value;
  var $RawName;
  function TTpRequestVariable($inName, $inValue)
  {
    $this->RawName = $inName;
    $this->Value = $inValue;
    //
    $end = strrchr($inName, '_');
    if (($end == '_x') || ($end == '_y'))
    {
      $this->Coord = substr($end, -1);
      $inName = substr($inName, 0, -2);
    }
    //
    $parts = explode('__', $inName);
    $this->Name = $parts[0];
    if (count($parts) > 1)
      $this->Index = (integer)$parts[1];
    else
      $this->Index = 0;
  }
}

/**
 * Page object.
 * Implements the TurboPhp page framework, including parsing the input
 * template, handling input, and generating HTML.
 * The behaviour of the page is customized by attaching event handlers.
 * @package TurboPhpLib
 */
class TTpPage extends TTpHtmlParser
{
  /**
   * Array of named macro expansions.
   * When generating HTML, encountered macros of the form {%MacroName} are
   * replaced by matching blocks from this array. E.g. {%MacroName}
   * is replaced by <var>$Blocks['MacroName']</var>.
   * @var array
   */
  var $Blocks;
  /**
   * Enable debugging output.
   * If false, debugging output is suppressed.
   * @var boolean
   */
  var $Debug;
  /**
   * Control debugging output. See also {@link Debug}.
   * @var integer
   */
  var $DebugFlags;
  /**
   * Halt generation.
   * If true, the page returns from the {@link Generate} function without
   * further processing.
   * @var boolean
   */
  var $Handled;
  /**
   * Array of named objects.
   * TurboPhp objects managed by the page are stored in this array.
   * All members of this array must derive from {@link TTpObj}.
   *
   * <b>Warning:</b> be careful accessing this array; objects must always be
   * accessed by reference. Use methods {@link IsObject} or {@link GetObject}
   * instead.
   *
   * @var array
   * @access protected
   */
  var $Objects;
  /**
   * Name of OnBeforeInput event function.
   * This event is fired before any input is processed.
   *
   * @var string
   */
  var $OnBeforeInput;
  /**
   * Name of OnBeforeClicks event function.
   * This event is fired before any clicks are processed.
   *
   * @var string
   */
  var $OnBeforeClicks;
  /**
   * Name of OnGenerate event function.
   * This event is fired before output is generated.
   *
   * @var string
   */
  var $OnGenerate;
  /**
   * Array of request input objects.
   * Array of {@link TTpRequestVariable} objects containing information
   * parsed from the $_REQUEST array.
   *
   * @var array
   */
  var $Inputs;
  /* *
   * {@link TTpTemplate} object
   *
   * @access private
   * @var string
   */
  //var $Template;
  /**
   * Name of template file.
   *
   * @var string
   */
  var $TemplateFile;
  /**
   * Flag used to track button clicks on form input.
   * If form input was received but no button was clicked,
   * the form will invoke it's DefaultButton click.
   *
   * @access private
   * @var boolean
   */
  var $WasButtonClick;
  /**
   * Flag indicating this page is being used as a module.
   *
   * @var boolean
   */
  var $IsModule;
  /**
   * Number of portable header objects for merging modules.
   *
   * @access private
   * @var integer
   */
  var $PortableCount;
  var $LastObj;

  /**
   * @param string Value to store in {@link TemplateFile}.
   */
  function TTpPage($inFile)
  {
    //if (isset($GLOBALS[TPMODULEFLAG]))
    $this->IsModule = @$GLOBALS[TPMODULEFLAG];
    $this->TemplateFile = $inFile;
    $this->Blocks = array();
    $this->Objects = array();
    $this->Inputs = array();
//    $this->PortableCount = 0;
//    $this->IsModule = false;
//    $this->RegisterClasses();
//    $this->IsModule = true;
  }

  /**
   * Fires a named event function.
   * If <var>$inEvent</var> is not empty, it is invoked as a function.
   * A reference to the page is passed as a parameter.
   *
   * @access private
   * @return Zero if the parameter is empty, otherwise value returned
   * by the event function.
   */
  function DoEvent($inEvent)
  {
    if ($inEvent <> '')
      return $this->$inEvent(&$this);
    else
      return 0;
  }

  /**
   * Test for existence of a named macro block.
   * @param string Name of block to look for.
   * @return boolean True if the block exists in this page.
   */
  function IsBlock($inName)
  {
    return array_key_exists($inName, $this->Blocks);
  }

  /**
   * Get the value of a named macro block.
   * @param string Name of block to look for.
   * @return object Returns the value of the named block if it exists,
   * '' otherwise.
   */
  function GetBlock($inName)
  {
    if ($this->IsBlock($inName))
      return $this->Blocks[$inName];
    else
      return '';
  }

  /**
   * Test for existence of a named TurboPhp object.
   * @param string Name of object to look for.
   * @return boolean True if the object exists in this page.
   */
  function IsObject($inName)
  {
    return array_key_exists($inName, $this->Objects);
  }

  /**
   * Get a reference to the named TurboPhp object.
   * @param string Name of object to look for.
   * @return TTpObj Returns a reference to the named object if it exists,
   * false otherwise.
   */
  function &GetObject($inName)
  {
    if ($this->IsObject($inName))
      return $this->Objects[$inName];
    else
    {
//      echo "Searching [$inName] for module ownership<br>";
      if (preg_match('#([^\.]*)\.(.*)#', $inName, $matches))
      {
//        echo "Looking for module [$matches[1]]<br>";
        if ($this->IsObject($matches[1]))
        {
          $obj = &$this->Objects[$matches[1]];
//          echo "Looking for object [$matches[1].$matches[2]]<br>";
          if ($obj->ModulePage->IsObject($matches[2]))
          {
//            echo "Succesfully marshalled object [$matches[1].$matches[2]]<br>";
            return $obj->ModulePage->Objects[$matches[2]];
          }
        }
      }
    }
    return false;
  }

  /**
   * @access private
   */
  function AddObject(&$inObj)
  {
    $this->LastObj = &$inObj;
    $inObj->Page = &$this;
    if ($inObj->Name)
    {
      //$GLOBALS[$inObj->Name] = &$inObj;
      $this->Objects[$inObj->Name] = &$inObj;
      // Experimental
      $n = $inObj->Name;
      $this->$n = &$inObj;
    }
  }

  /**
   * @access private
   */
  function ExtractFields(&$inFields)
  {
    $result = array();
    $v = get_object_vars($this);
    foreach ($inFields as $key => $value)
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

  /**
   * @access private
   */
  function ExtractPageProperties(&$inObj)
  {
    $this->Attributes = $this->ExtractFields($inObj->Attributes);
    if ($this->OnGenerate == '')
      $this->OnGenerate = @$inObj->Attributes['tponrun'];
  }

  function GetClassName(&$inObj)
  {
    global $TpClasses;
    $class = @$inObj->Attributes['tpClass'];
    if ($class == '')
      $class = @$inObj->Attributes['tpclass'];
    //if (array_key_exists($class, $TpClasses))
    //  $class = $TpClasses[$class];
    if (class_exists($class) == false)
      $class = 'TTpObj';
    return $class;
  }

  function GetObjectName(&$inTag)
  {
    $result = @$inTag->Attributes['tpname'];
    if ($result == '')
      $result = @$inTag->Attributes['tpName'];
    if ($result == '')
      $result = @$inTag->Attributes['name'];
    return $result;
  }

  function &ManufactureObject(&$inTag)
  {
    $class = $this->GetClassName(&$inTag);
    $name = $this->GetObjectName(&$inTag);
//    if ($name == '')
//    {
//      echo "<pre>";
//      print_r($inTag->Attributes);
//      echo "</pre>";
//    }
    if ($name == '')
      return NULL;
    else if ($class == 'TTpPage')
    {
      $this->ExtractPageProperties(&$inTag);
      return NULL;
    }
    else
    {
      $obj = new $class();
      $obj->Class = $class;
      $obj->Name = $name;
      return $obj;
    }
  }

  /* *
   * @access private
   */
  function ExtractObject(&$inTag)
  {
    $obj = $this->ManufactureObject(&$inTag);
    if ($obj == NULL)
    {
      //echo "NO object ... extracting children<br>";
      return $this->ExtractChildObjects($inTag);
    }
    else
    {
      //echo "object ok...<br>";
      $obj->AssignAttributes($inTag);
      $obj->Element = $inTag->Element;
      $result = TP_MACSTART . $obj->Name . TP_MACEND;
      if ($inTag->EndTag)
      {
        $obj->Content = $this->ExtractChildObjects($inTag) . $inTag->EndTag->Pre;
        $obj->EndTag = $inTag->EndTag->GetStartTag();
      }
      else
      {
        //echo "promoting children<br>";
        $result .= $this->ExtractChildObjects($inTag);
      }
      $this->AddObject(&$obj);
      return $result;
    }
  }

  /* *
   * @access private
   */
  function ExtractSpecialObject(&$inTag, $inName)
  {
     //echo "Extracting special object [$inName]...<br>";
     $inTag->ParseAttrs($inTag->RawAttrs);
     $inTag->Attributes['tpName'] = $inName;
     return $this->ExtractObject($inTag);
  }

  /* *
   * @access private
   */
  function ExtractModuleObjects(&$inTag)
  {
    //echo "Extracting module objects...[$inTag->Element]<br>";
    if ($inTag->Element == '!doctype')
      $result = $this->ExtractSpecialObject(&$inTag, 'TpDoctype');
    else if ($inTag->Element == 'html')
      $result = $this->ExtractSpecialObject(&$inTag, 'TpHtml');
    else if ($inTag->Element == 'head')
    {
      $result = $this->ExtractSpecialObject(&$inTag, 'TpHead');
      $this->LastObj->Content .= TP_MACSTART . TPPORTABLES . TP_MACEND;
      $this->Blocks[TPPORTABLES] = '';
    }
    else if ($inTag->Element == 'body')
      $result = $this->ExtractSpecialObject(&$inTag, 'TpBody');
    else if (($inTag->Element == 'link') || ($inTag->Element == 'script')
      /*|| ($inTag->Element == 'style')*/
      /*|| ($inTag->Element == 'meta')*/
      )
    {
      $n = 'TpPortable' . ++$this->PortableCount;
      $result = $this->ExtractSpecialObject(&$inTag, $n);
    }
    else
      return '';
    return $inTag->Pre . $result;
  }

  /* *
   * @access private
   */
  function ExtractTagObjects(&$inTag)
  {
//    if ($this->IsModule)
    {
      $result = $this->ExtractModuleObjects(&$inTag);
      if ($result)
        return $inTag->Pre . $result;
    }
    //echo "Extracting tag objects...[$inTag->Element]<br>";
    if (strpos($inTag->RawAttrs, TPCLASS) === false)
    {
      //echo "adding to result...<br>";
      $result =
            $inTag->GetStartTag()
          . $this->ExtractChildObjects(&$inTag)
          . $inTag->GetEndTag()
          ;
    }
    else
    {
      //echo "extracting as object...<br>";
      $inTag->ParseAttrs($inTag->RawAttrs);
      $result = $this->ExtractObject(&$inTag);
    }
    return $inTag->Pre . $result;
  }

  /* *
   * @access private
   */
  function ExtractChildObjects(&$inTag)
  {
    $result = '';
    $c = count($inTag->Tags);
    for ($i = 0; $i < $c; $i++)
      $result .= $this->ExtractTagObjects($inTag->Tags[$i]);
    return $result;
  }

  /* *
   * @access private
   */
  function ExtractObjects()
  {
    $this->Text = $this->ExtractTagObjects(&$this->Root);
  }

  /**
   * @access private
   */
  function InterpolateObject(&$inName)
  {
    if ($this->IsObject($inName))
      $object = $this->Objects[$inName]->Generate();
    else if ($this->IsBlock($inName))
      $object = $this->Blocks[$inName];
    else
      return $inName;
    return $this->Interpolate($object);
  }

  /**
   * @access private
   */
  function InterpolateMatchObject(&$matches)
  {
    return $this->InterpolateObject(&$matches[1]);
  }

  /**
   * Expand all object and block macros in the input text.
   * Macro patterns of the form {%Macro} in the input text are
   * matched against object names, then block names.
   *
   * Macros matching objects are replaced by text provided by the object's
   * {@link TTpObj.Generate()} function.
   *
   * Block macros are replaced by the stored block value.
   *
   * Note: Expanded macro patterns are interpolated recursively.
   *
   * @param string Reference to input text.
   * @return string Input text with objects and macros recursively expanded.
   */
  function Interpolate(&$inText)
  {
    return preg_replace_callback(
      P_MACRO, array(&$this, 'InterpolateMatchObject'), $inText);
  }

  /**
   * Call a method for each object in the {@link Objects} array.
   * @param string Name of method to invoke.
   * @access private
   */
  function DoForAllObjects($inMethod)
  {
    $keys = array_keys($this->Objects);
    foreach ($keys as $key)
      $this->Objects[$key]->$inMethod();
  }

  /**
   * Perform tasks after parsing a template file.
   * Calls AfterParse for each object in the {@link Objects} array.
   * @access private
   */
  function AfterParse()
  {
    $this->DoForAllObjects('AfterParse');
  }

  /**
   * Parse the template file.
   * The template file is read and objects are extracted.
   */
  function ParseTemplate()
  {
    $this->ParseFile($this->TemplateFile);
    $this->ExtractObjects();
    $this->AfterParse();
  }

  /**
   * @access private
   */
  function HaveRequest()
  {
    return (count($_REQUEST) > 0);
  }

  /**
   * @access private
   */
  function DoRequestForAllObjects($inMethod)
  {
    if ($this->HaveRequest())
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

  /**
   * @access private
   */
  function ParseInputs()
  {
    foreach ($_REQUEST as $name => $value)
    {
      $input = new TTpRequestVariable($name, $value);
      $this->Inputs[] = $input;
//      echo "Input matching ($name => $value): ";
//      echo "<pre>";
//      print_r($input);
//      echo "</pre>";
    }
  }

  /**
   * @access private
   */
  function ProcessInputs()
  {
    $this->ParseInputs();
    $this->DoEvent($this->OnBeforeInput);
    $this->DoForAllObjects('BeforeInput');
    $this->DoRequestForAllObjects('ProcessInput');
    $this->DoForAllObjects('AfterInput');
  }

  /**
   * @access private
   */
  function ProcessClicks()
  {
    $this->WasButtonClick = false;
    $this->DoEvent($this->OnBeforeClicks);
    $this->DoForAllObjects('BeforeClicks');
    $this->DoRequestForAllObjects('ProcessClicks');
    $this->ProcessDefaultClick();
    $this->DoForAllObjects('AfterClicks');
  }

  /**
   * @access private
   */
  function ProcessRequest()
  {
    $this->ProcessInputs();
    $this->ProcessClicks();
  }

  /**
   * @access private
   */
  function GetPageQualifier()
  {
    return get_class($this) . '_';
  }

  /**
   * @access private
   */
  function ProcessDefaultClick()
  {
    // Special form-identifying input
    $form_var = $this->GetPageQualifier() . 'tpsubmit';
    if (array_key_exists($form_var, $_REQUEST))
    {
      $n = $_REQUEST[$form_var];
      if ($this->IsObject($n))
      {
        if (!$this->WasButtonClick)
        {
          $b = $this->Objects[$n]->DefaultButton;
          if ($this->IsObject($b))
            $this->Objects[$b]->Click();
        }
        $this->Objects[$n]->Submit();
      }
    }
  }

  /**
   * Redirect the user's browser to the input url. All other processing
   * is stopped.
   */
  function Redirect($inUrl)
  {
    if ($inUrl <> '')
    {
      header("Location: $inUrl");
      $this->Handled = true;
    }
  }

  /**
   * Process input request and returns generated output.
   * Fires events as needed.
   */
  function Generate()
  {
    $this->ProcessRequest();
    $this->DoForAllObjects('BeforeGeneratePage');
    $this->DoEvent($this->OnGenerate);
    if (!$this->Handled)
    {
      if ($this->IsModule)
        return $this->Interpolate($this->TpBody->Content);
      else
        return $this->Interpolate($this->Text);
    }
    else
      return '';
  }

  /**
   * Run the TurboPhp application.
   * Process all input and generate output.
   * Fires events as needed.
   */
  function Run()
  {
    $this->ParseTemplate();
    if (!$this->IsModule)
    {
      echo $this->Generate();
      if ($this->Debug)
        $this->DebugDump();
    }
  }

  /**
   * Dump request datat to debug output.
   */
  function DebugDump()
  {
    //echo "Debug Dump $this->DebugFlags<br>";
    TpDbgStart();
    if ($this->DebugFlags & 0x02)
      $this->DumpRequest();
    if ($this->DebugFlags & 0x01)
      $this->DumpObjects();
    if ($this->DebugFlags & 0x04)
      $this->DumpBlocks();
    if ($this->DebugFlags & 0x08)
      $this->DumpTags();
    TpDbgEnd();
  }

  /**
   * Dump request datat to debug output.
   */
  function DumpRequest()
  {
    echo "{!}Request<br>";
    print_r($_REQUEST);
    echo "<br>{-}<br>";
  }

  /**
   * Dump all objects to debug output.
   */
  function DumpObjects()
  {
    $c = count($this->Objects);
    echo "{!}Objects ($c)<br>";
    $this->DoForAllObjects("Dump");
    echo "{-}<br>";
  }

  /**
   * Dump all blocks to debug output.
   */
  function DumpBlocks()
  {
    echo "{!}Blocks (".count($this->Blocks).")<br>";
    $keys = array_keys($this->Blocks);
    foreach ($keys as $key)
    {
      echo "{+}$key<br>";
      echo $this->Blocks[$key]."<br>";
      //echo htmlspecialchars($blk);
      echo "{-}<br>";
    }
    echo "{-}<br>";
  }

  function DumpTags()
  {
    echo "{!}Tags<br>";
    $this->Root->DumpLevel(999);
    echo "{-}<br>";
  }
}

# +----------------------------------------------------------------------+
# | TpObj                                                                |
# +----------------------------------------------------------------------+

/**
 * Base class for TurboPhp objects.
 * A TTpObj is created for each marked-up HTML element in the template.
 * @package TurboPhpLib
 */
class TTpObj
{
  /**
   * Reference to the owning {@link TTpPage} object.
   * @var TTpPage
   */
  var $Page;
  /**
   * Name of this object.
   * @var string
   */
  var $Name;
  /**
   * Name of the TurboPhp class for this object.
   * <b>Note</b>: the TurboPhp class is not necessarily the same as the actual
   * PHP class.
   * @access private
   * @var string
   */
  var $Class;
  /* *
   * Enable debugging output.
   * If false, debugging output is suppressed for this object.
   * @var boolean
   */
//  var $DebugFlag;
  /* *
   * Element for this control's HTML tag.
   * Every TTpObject corresponds to an HTML tag  (e.g. td, div, input, etc.).
   * $Elt contains the tag element.
   * @var string
   */
  var $Element;
  /**
   * Array of HTML attributes (name/value pairs).
   * Attributes are encoded using XHTML standards (i.e. attribute names
   * are all lowercase).
   *
   * Change attributes by setting array values, e.g.,
   * <code>Attributes['bgcolor'] = 'green';</code>
   * Clear an attribute by
   * <code>Attributes['bgcolor'] = '';</code>
   * or
   * <code>unset($object->Attributes['bgcolor']);</code>
   * Attributes that don't have values should be encoded XHTML style, e.g.
   * <code>Attributes['readonly'] = 'readonly'</code>
   * <b>Note</b>: avoid setting the <i>style</i> attribute using Attributes
   * array. Instead, put style entities into the {@link Styles} array.
   * @var array
   */
  var $Attributes;
  /**
   * The contents of the <i>style</i> attribute as parsed from the template.
   * @access private
   * @var string
   */
  var $Style;
  /**
   * Array of style attributes (name/value pairs).
   * Change attributes by setting array values, e.g.,
   * <code>Styles['background-color'] = 'green';</code>
   * Clear an attribute by
   * <code>Styles['background-color'] = '';</code>
   * or
   * <code>unset($object->Styles['background-color']);</code>
   * @var array
   */
  var $Styles;
  /**
   * The content of the HTML tag.
   * Holds the text that will be output between
   * the element tags: <i><tag><b>content</b></tag></i>
   * @var string
   */
  var $Content;
  /**
   * @access private
   * @var string
   */
  var $EndTag;
  /**
   * True to hide the object.
   * If true, this object's HTML is not written
   * to the output.
   * @var boolean
   */
  var $Hidden;
  /**
   * Name of OnGenerate event function.
   * This event is fired from {@link Generate} before output
   * is generated for this object.
   * This event may be fired multiple times if the object is used more than
   * once in the page. For example, a label object in a {@link TTpRepeater}
   * will be generated once for each repeat.
   * @var string
   */
  var $OnGenerate;
  var $RawAttrs;
  /**
   * Calls the {@link Init} method.
   * Subclasses generally perform initialization
   * duties in an overriden {@link Init} method instead of in constructors
   * to avoid object referencing problems.
   */
  function TTpObj()
  {
    $this->Init();
  }

  /**
   * Perform initialization tasks.
   * Subclasses generally perform initialization
   * duties in redefined Init methods instead of constructors
   * to avoid object referencing problems.
   * @access private
   */
  function Init()
  {
    $this->Attributes = array();
    $this->Styles = array();
  }

  /**
   * Assign the attributes of <var>$inObj</var> to this object.
   * @param TTpObj Reference to object whose attributes are to be copied.
   */
  function Assign(&$inObj)
  {
    echo "Assigning [" . get_class($inObj) ."]<br>";
    $this->Page = &$inObj->Page;
    $this->Class = $inObj->Class;
    $this->Name = $inObj->Name;
    $this->Element = $inObj->Elementt;
    $this->Attributes = $inObj->Attributes;
    $this->Style = $inObj->Style;
    $this->Styles = $inObj->Styles;
    $this->Content = $inObj->Content;
    $this->EndTag = $inObj->EndTag;
    $this->Hidden = $inObj->Hidden;
    $this->OnGenerate = $inObj->OnGenerate;
  }

  /**
   * @access private
   */
  function DebugId()
  {
    return "($this->Class) $this->Name: ";
  }

  /**
   * Output a single line message to the debugging console.
   * @param string Message to output.
   * @param boolean True to prepend a full stack trace to the debug output.
   * Otherwise only the calling function is identified.
   */
  function Debug($inMsg, $inFullTrace = false)
  {
    //if (($this->App->Debug) && ($this->DebugFlag))
    if ($this->Page->Debug)
    {
      TpDbgMsg($this->DebugId() . "[" . $this->StackTrace($inFullTrace) ."]");
      TpDbgMsg(" ==> $inMsg");
    }
  }

  /**
   * @access private
   */
  function StackTrace($inFullTrace = false)
  {
    $s = '';
    $bt = debug_backtrace();
    $c = count($bt);
    if ($c > 2)
      $s = $bt[2]["function"];
    if ($inFullTrace)
      for ($i = 3; $i < $c; $i++)
        $s = $s.'/'.$bt[$i]["function"];
    return $s;
  }

  /**
   * Fires a named event function.
   * If <var>$inEvent</var> is not empty, it is invoked as a function.
   * A reference to the page is passed as a parameter.
   *
   * @access private
   * @return Zero if the parameter is empty, otherwise returns the value
   * of the event function.
   */
  function DoEvent($inEvent)
  {
    if ($inEvent <> '')
      return $this->Page->$inEvent(&$this);
    else
      return 0;
  }

  /**
   * @access private
   */
  function ExtractStyles($inStyles)
  {
    $result = array();
    if (preg_match_all(P_STYLE, $inStyles, $matches, PREG_SET_ORDER))
      foreach ($matches as $match)
        $result[$match[1]] = $match[2];
    return $result;
  }

  /**
   * @access private
   */
  function IsVar($inName)
  {
    return array_key_exists($inName, get_object_vars($this));
  }

  /**
   * @access private
   */
  function StoreVar($inName, $inValue)
  {
    if (!$this->IsVar($inName))
      return false;
    $this->$inName = $inValue;
    return true;
  }

  /**
   * @access private
   */
  function AssignAttribute(&$inName, &$inValue)
  {
/*
    if ($inName == 'name')
      $this->Name = $inValue;
    else
    if ($inName == TPCLASS)
      $this->Class = $inValue;
    else if ($inName == TP_NAME)
      $this->Name = $inValue;
    else if ($inName ==  TP_HIDDEN)
      $this->Hidden = true;
    else if ($inName == 'tpongenerate')
      $this->OnGenerate = $inValue;
    else */
    if ($inName == 'style')
      $this->Styles = $this->ExtractStyles($inValue);
    else if ((substr($inName, 0, 2)=='tp') && $this->StoreVar(substr($inName, 2), $inValue))
        ;
    else
      $this->Attributes[$inName] = $inValue;
    //echo "Name: [$this->Name]<br>";
  }

  /**
   * Assign the attributes of <var>$inTag</var> to this object.
   * @param TTpTag Reference to tag whose attributes are to be copied.
   */
  function AssignAttributes(&$inTag)
  {
    $this->RawAttrs = $inTag->RawAttrs;
    foreach ($inTag->Attributes as $name => $value)
      $this->AssignAttribute($name, $value);
  }

  /**
   * Assign properties of <var>$inTag</var> to this object.
   * @param TTpTag Reference to tag whose properties are to be copied.
   */
  function AssignTag(&$inTag)
  {
    $this->Element = $inTag->Element;
    $this->AssignAttributes($inTag);
    $this->EndTag = $inTag->GetEndTag();
  }

  /**
   * @access private
   */
  function MarshallObject($inField)
  {
    $name = $this->$inField;
    if (is_string($name))
      $this->$inField = &$this->Page->GetObject($name);
    else
      $this->$inField = false;
    if ($this->$inField)
      $this->Debug("Marshalled [" . $name ."] .");
    else
      $this->Debug("FAILED to marshall [" . $name ."] .");
  }

  /**
   * @access private
   */
  function AfterParse()
  {
  }

  /**
   * @access private
   */
  function BeforeInput()
  {
  }

  /**
   * @access private
   */
  function ProcessInput()
  {
  }

  /**
   * @access private
   */
  function AfterInput()
  {
  }

  /**
   * @access private
   */
  function BeforeClicks()
  {
  }

  /**
   * @access private
   */
  function ProcessClicks()
  {
  }

  /**
   * @access private
   */
  function AfterClicks()
  {
  }

  /**
   * @access private
   */
  function BeforeGeneratePage()
  {
  }

  /**
   * Get whatever data is considered the <i>value</i> of this object.
   * For TTpObj, this method returns the object's {@link Content}.
   * Subclasses may override to return other data.
   * @return The default value of this control.
   */
  function GetDefaultValue()
  {
    return $this->Content;
  }

  /**
   * @access private
   */
  function GetAttribute($inAttr)
  {
    return TpSafeArrayGet($inAttr, $this->Attributes);
  }

  /**
   * @access private
   */
  function GetStyle($inStyle)
  {
    return TpSafeArrayGet($inStyle, $this->Styles);
  }

  /**
   * @access private
   */
  function DoStylize(&$inStyle, &$inValue)
  {
    $this->Styles[$inStyle] = $inValue;
  }

  /**
   * @access private
   * @deprecated
   */
  function Stylize($inStyle, $inValue = '')
  {
    if (is_array($inStyle))
    {
      foreach ($inStyle as $style => $value)
        $this->DoStylize($style, $value);
    } else
      $this->DoStylize($inStyle, $inValue);
  }

  /**
   * @access private
   */
  function ArrayToAttrs(&$inArray)
  {
    $result = '';
    foreach ($inArray as $name => $value)
      $result .= ' ' . $name . '="' . $value. '"';
    return $result;
  }

  /**
   * @access private
   */
  function GetHtmlAttrs()
  {
    return $this->ArrayToAttrs($this->Attributes) . $this->RawAttrs;
  }

  /**
   * @access private
   */
  function ArrayToStyles(&$inArray)
  {
    $result = '';
    foreach ($inArray as $name => $value)
      if ($value != '')
        $result .= ' ' . $name . ': ' . $value. ';';
    return trim($result);
  }

  /**
   * @access private
   */
  function GetHtmlStyle()
  {
    $style = $this->ArrayToStyles($this->Styles);
    if ($style != "")
      return ' style="' . $style . '"';
    else
      return '';
  }

  /**
   * @access private
   */
  function GetStartTag()
  {
    return '<'.$this->Element.$this->GetHtmlAttrs().$this->GetHtmlStyle().
     ($this->EndTag == '' ? '/>' : '>');
  }

  /**
   * @access private
   */
  function GetContent()
  {
    return $this->Content;
  }

  /**
   * @access private
   */
  function GetHtml()
  {
    return $this->GetStartTag().$this->GetContent().$this->EndTag;
  }

  /**
   * @access private
   */
  function BeforeGenerate()
  {
    $this->DoEvent($this->OnGenerate);
  }

  /**
   * @access private
   */
  function Generate()
  {
    $this->BeforeGenerate();
    if ($this->Hidden)
      return '';
    else
      return $this->GetHtml();
  }

  /**
   * Dump property information to the standard output.
   */
  function DumpVar(&$inVar, &$inValue)
  {
    if (is_object($inValue))
    {
      echo "[$inVar] => ";
      echo ($inVar == 'Page' ? get_class($inValue) : $inValue->Name);
      echo "<br>";
    }
    else
      switch ($inVar)
      {
        case 'EndTag':
          break;
        //
        default:
          if (is_array($inValue))
          {
            if (count($inValue) > 0)
            {
              echo "{+}[$inVar]<br>";
              print_r($inValue);
              echo "<br>";
              echo "{-}<br>";
            }
          }
          else
          {
            echo "[$inVar] =>";
            if (is_string($inValue) && (strlen($inValue) > 100))
              echo "(long text)";
            else
              print_r($inValue);
            echo "<br>";
          }
          break;
      }
  }

  /**
   * Dump object information to the standard output.
   */
  function Dump()
  {
    echo "{+}$this->Name: ".get_class($this)."<br>";
    //echo "$this->Name: ".get_class($this)." [$this->Class]<br>";
    $vars = get_object_vars($this);
    foreach ($vars as $var => $value)
      if ($value)
        $this->DumpVar(&$var, &$value);
    echo "{-}<br>";
  }
}

# +----------------------------------------------------------------------+
# | Objects                                                              |
# +----------------------------------------------------------------------+

/**
 * Anchor tag (link)
 * @package TurboPhpLib
 */
class TTpAnchor extends TTpObj
{
  // nothing to specialize
};

/**
 * Text output.
 * @package TurboPhpLib
 */
class TTpLabel extends TTpObj
{
  var $Anchor;
  var $Caption;
  /**
   * @access private
   */
  function AfterParse()
  {
    parent::AfterParse();
    // Our Content is typically the Caption
    $this->Caption = $this->Content;
    // Check if the Content is an anchor
    $n = substr($this->Content, 2, -1);
    if ($n && $this->Page->IsObject($n))
    {
      $obj = &$this->Page->Objects[$n];
      if (is_a($obj, "ttpanchor"))
      {
        // Store the label's anchor
        $this->Anchor = &$obj;
        // Caption is now the anchor's content
        $this->Caption = $this->Anchor->Content;
      }
    }
    // Erase designed content
    $this->Content = '';
  }
  /**
   * @access private
   */
  function GetContent()
  {
    // Use Caption as default Content
    if ($this->Content == '')
      $result = $this->Caption;
    // If user has customized Content, use the customized Content
    else
      $result = $this->Content;
    // Erase customized Content, so the default is Caption once again
    $this->Content == '';
    // If using an Anchor
    if ($this->Anchor)
    {
      // The content of this control is output by the anchor instead
      $this->Anchor->Content = $result;
      // Our content will be generated by the Anchor (clumsy)
      $result = TP_MACSTART . $this->Anchor->Name . TP_MACEND;
    }
    // return content
    return $result;
  }
}

/**
 * TTpObj subclass that generates no output.
 * @package TurboPhpLib
 */
class TTpSilentObj extends TTpObj
{
  /**
   * @return string Empty string.
   */
  function Generate()
  {
     return '';
  }
}

/**
 * Generic source for list data.
 * @package TurboPhpLib
 */
class TTpListSource extends TTpSilentObj
{
  // Add items to an array.
  // @abstract
  // @param Reference to array to which the list source should add items.
  function GetItems(&$outItems)
  {
    // Must be provided by subclass.
  }
}

/**
 * Object which represents a web variable.
 * Provides TTpObj functionality for web data that isn't handled by
 * an input object.
 * @package TurboPhpLib
 */
class TTpWebVariable extends TTpSilentObj
{
  /**
   * Value of the variable.
   * @var mixed
   */
  var $Value;
  /**
   * Name of the form object to which this variable belongs.
   * @var string
   */
  var $Form;

  /**
   * @return mixed The {@link Value} field of this control.
   */
  function GetDefaultValue()
  {
    return $this->Value;
  }

  /**
   * Copy the {@link Value} of this control from the $_REQUEST array.
   */
  function ProcessInput()
  {
    if ($this->Name != '')
      if (array_key_exists($this->Name, $_REQUEST))
        $this->Value = $_REQUEST[$this->Name];
  }
}

/**
 * Test if a path identifies a directory.
 * @param string Path to test.
 * @return boolen True if input path is a directory.
 */
function tpIsDir($inFilename)
{
  $fs = stat($inFilename);
  return ($fs['mode'] & 040000 == 040000);
}

/**
 * Collect information about items in a file system directory.
 * Builds an array of records about entries in a given
 * file-system directory.
 *
 * Set the {@link $Folder} field to the pathname to examine.
 *
 * The list is automatically built when {@link Generate} is called, or
 * the list can be built at any time by calling the {@link ListFiles} method.
 *
 * The {@link $Files} field is an array of file names
 * (or complete paths if {@link $FullPath} is true)
 * in the directory.
 *
 * The {@link $Infos} field is an array of information records.
 * Both the $Files and $Infos arrays contain {@link $Count} records.
 *
 * The list can be sorted by calling the {@link Sort} method and specifying
 * one of the info record field names (see {@link $Infos} documentation).
 * @package TurboPhpLib
 */
class TTpFileList extends TTpListSource
{
  /**
   * Number of files in the list.
   * @var integer
   */
  var $Count;
  /**
   * Name of current file for use by the {@link OnFilter} event.
   * @var string
   */
  var $File;
  /** Array of file names
   * (or complete paths if {@link $FullPath} is true)
   * in the directory.
   * @var array
   */
  var $Files;
  /**
   * Information about the current file.
   * See {@link $Infos} for members.
   * @var array
   */
  var $Info;
  /**
   * Array of information records.
   * Each record is an array with the following fields:
   * - <b>name</b> => name of the file
   * - <b>size</b> => size of the file in bytes
   * - <b>time</b> => modification date for the file
   * - <b>dir</b> => true if the file is a directory
   * - <b>ext</b> => file's extension
   * E.g., the extension of the third entry in the directory is
   * accessible as:
   * <code>$this->Infos[2]['ext']</code>
   */
  var $Infos;
  /**
   * Folder to enumerate.
   * @var string
   */
  var $Folder;
  /**
   * If true, then the {@link Files} array contains full pathnames instead of
   * just filenames.
   * @var boolean
   */
  var $FullPath;
  /**
   * List filter event.
   * Called once for each item found while enumerating the
   * folder. The event handler can examine the {@link $File} property for the
   * name of the current file. If the current file should be filtered out of
   * the list, the handler sets $File to the empty string ('').
   * @var string
   */
  var $OnFilter;
  /**
   * @access private
   */
  function Generate()
  {
    $this->ListFiles();
  }
  /**
   * Enumerate the files.
   */
  function ListFiles()
  {
    $this->Infos = array();
    if (is_dir($this->Folder))
    {
      if ($dh = opendir($this->Folder))
      {
        clearstatcache();
        while ($this->File = readdir($dh))
          if (($this->File != '.') && ($this->File != '..'))
          {
            $this->Path = $this->Folder.$this->File;
            $this->Info = array(
               "name" => $this->File,
               "size" => filesize($this->Path),
               "time" => filemtime($this->Path),
               "dir" => tpIsDir($this->Path),
               "ext" => tpSafeArrayGet('extension', pathinfo($this->File))
            );
            $this->DoEvent($this->OnFilter);
            if ($this->File <> '')
              $this->Infos[] = $this->Info;
          }
        closedir($dh);
      }
    }
    $this->FilesFromInfos();
    $this->Count = count($this->Files);
  }
  /**
   * @access private
   */
  function FilesFromInfos()
  {
    $this->Files = array();
    $prefix = ($this->FullPath ? $this->Folder : '');
    foreach ($this->Infos as $info)
      $this->Files[] = $prefix.$info["name"];
  }
  /**
   * @access private
   */
  function CompareAsc($a, $b)
  {
    $av = $a[$this->CompareField];
    $bv = $b[$this->CompareField];
    if (is_string($av))
      return strcmp($av, $bv);
    else
      return $bv - $av;
  }
  /**
   * @access private
   */
  function CompareDesc($a, $b)
  {
    return CompareAsc($b, $a);
  }
  /**
   * Sort the item arrays by $inField.
   * See {@link $Infos} for a list of
   * acceptable field values.
   * @param string
   */
  function Sort($inField)
  {
    if ($inField == '')
      $inField = 'time';
    $this->CompareField = $inField;
    usort($this->Infos, array($this, "CompareAsc"));
    $this->FilesFromInfos();
  }
  function GetItems(&$outItems)
  {
    $outItems = array_merge($outItems, $this->Files);
  }
}

# +----------------------------------------------------------------------+
# | Controls                                                           |
# +----------------------------------------------------------------------+

/**
 * Container of input objects. Represents an HTML <form> object.
 * @package TurboPhpLib
 */
class TTpForm extends TTpObj
{
  /**
   * Name of the default button.
   * This named object will receive a simulated click if the
   * form was submitted by any means other than a button click.
   * @var string
   */
  var $DefaultButton;
  /**
   * Event fired when the form has been submitted.
   * @var string
   */
  var $OnSubmit;
  /**
   * @access private
   */
  function Submit()
  {
    // Submit is called by the Page itself;
    // i.e. not part of the normal input processing.
    $this->DoEvent($this->OnSubmit);
  }
}

/**
 * An object that takes input. Typically an edit box.
 * @package TurboPhpLib
 */
class TTpInput extends TTpObj
{
  /**
   * If true, the input value is base64 encoded when communicating
   * with the client.
   * @var boolean
   */
  var $Encoded;
  /**
   * Identifier for the current instance of this control.
   * If there is more than one instance of this control on a form
   * (for example, if the control is in a repeater), the $Index
   * can be used to identify which instance is firing an event.
   *
   * Do not set this property directly. Use the {@link SetIndex} method.
   *
   * @var mixed
   */
  var $Index;
  /**
   * The value before input was submitted.
   * The {@link OnSubmit} event can compare this
   * datum to the new {@ink $Value} and make adjustments.
   * @var mixed
   */
  var $OldValue;
  /**
   * Event fired when the input control submits a value.
   * The submitted value is available in {@link $NewValue}.
   * After OnSubmit is fired, $NewValue is copied to $Value.
   * @var string
   */
  var $OnSubmit;
  /**
   * True if the input control was successful (i.e. if it submitted a value).
   * @var boolean
   */
  var $Successful;
  /**
   * The value of the input.
   * Typically the string contents of an edit box.
   * @var mixed
   */
  var $Value;
  /**
   * Indexed values of the input.
   * Array of values stored by index.
   * Typically the string contents of an edit box.
   * @var mixed
   */
  var $Values;
  /**
   * @access private
   */
  function Init()
  {
    parent::Init();
    $this->Values = array();
  }
  /**
   * @access private
   */
  function AfterParse()
  {
    if (array_key_exists('value', $this->Attributes))
      $this->Value = $this->Attributes['value'];
    if (! array_key_exists('name', $this->Attributes))
      $this->Attributes['name'] = $this->Name;
  }
  /**
   * @access private
   */
  function DecodedValue($inValue)
  {
    if ($this->Encoded)
      return base64_decode(urldecode($inValue));
    else
      return $inValue;
  }
  /**
   * @access private
   */
  function EncodedValue($inValue)
  {
    if ($this->Encoded)
      return urlencode(base64_encode($inValue));
    else
      return $inValue;
  }
  /**
   * @access private
   */
  function GetIndexedName()
  {
    if ($this->Index)
      return $this->Name . "__" . $this->Index;
    else
      return $this->Name;
  }
  /**
   * @access private
   */
  function GetPageQualifier()
  {
    return $this->Page->GetPageQualifier();
  }
  /**
   * @access private
   */
  function GetRequestName()
  {
    return $this->GetPageQualifier() . $this->Name;
  }
  /**
   * @access private
   */
  function SearchRequest($inMethod)
  {
    $pname = $this->GetRequestName();
    foreach ($this->Page->Inputs as $input)
      if ($input->Name == $pname)
        $this->$inMethod(&$input);
  }
  /**
   * @access private
   */
  function Submit(&$inInput)
  {
    $this->OldValue = $this->Value;
    $this->Index = $inInput->Index;
    $this->Value = $this->DecodedValue($inInput->Value);
    $this->DoEvent($this->OnSubmit);
    $this->Values[$this->Index] = $this->Value;
    $this->Successful = true;
  }
  /**
   * @access private
   */
  function ProcessInput()
  {
    $this->Successful = false;
    $this->SearchRequest('Submit');
  }
  /**
   * @access private
   */
  function GetQualifiedName()
  {
    return $this->GetPageQualifier() . $this->GetIndexedName();
  }
  /**
   * @access private
   */
  function SetNameAttribute()
  {
    $this->Attributes['name'] = $this->GetQualifiedName();
  }
  /**
   * @access private
   */
  function SetValueAttribute()
  {
    $this->Attributes['value'] = $this->EncodedValue($this->Value);
  }
  /**
   * Identify an instance of an input control.
   * When a control is generate multiple times
   * (for example, inside a repeater) use
   * SetIndex to give each instance of the control
   * a unique identifier.
   *
   * SetIndex will fire an OnInput event if the specified index corresponds
   * to a value in the current request.
   *
   * @param Integer Index for this instance.
   */
  function SetIndex($inIndex)
  {
    $this->Index = $inIndex;
    if (array_key_exists($inIndex, $this->Values))
      $this->Value = $this->Values[$inIndex];
  }
  /**
   * @access private
   */
  function BeforeGenerate()
  {
    parent::BeforeGenerate();
    $this->SetValueAttribute();
    $this->SetNameAttribute();
  }
  function GetDefaultValue()
  {
    return $this->Value;
  }
}

/**
 * Represents an HTML <textarea> object.
 * @package TurboPhpLib
 */
class TTpTextArea extends TTpInput
{
  /**
   * @access private
   */
  function AfterParse()
  {
    parent::AfterParse();
    $this->Value = $this->Content;
  }
  /**
   * @access private
   */
  function GetContent()
  {
    return $this->Value;
  }
}

/**
 * Input object that also wants clicks.
 * After all successful input objects have fired {@link TTpInput.OnSubmit} events,
 * clickable objects are given an opportunity to fire {@link OnClick} events.
 * @package TurboPhpLib
 */
class TTpClickable extends TTpInput
{
  /**
   * Event fired when an input object has been clicked (e.g. a button).
   */
  var $OnClick;
  /**
   * @access private
   */
  function Click()
  {
    $this->DoEvent($this->OnClick);
  }
  /**
   * @access private
   */
  function ProcessClicks()
  {
    if ($this->Successful)
      $this->Click();
  }
}

/**
 * Represents a button object.
 * @package TurboPhpLib
 */
class TTpButton extends TTpClickable
{
  /**
   * @access private
   */
  function Click()
  {
    $this->Page->WasButtonClick = true;
    parent::Click();
  }
}

/**
 * Represents a button object.
 * @package TurboPhpLib
 */
class TTpImageButton extends TTpButton
{
  /**
   * @access private
   */
  function Submit(&$inInput)
  {
    if ($inInput->Coord == 'x')
      parent::Submit(&$inInput);
  }
}

/**
 * Represents a control that can be checked.
 * @package TurboPhpLib
 */
class TTpBooleanBox extends TTpClickable
{
  /**
   * Checked state of the control.
   * @var boolean
   */
  var $Checked;
  /**
   * @access private
   */
  function AfterParse()
  {
    parent::AfterParse();
    if (array_key_exists('checked', $this->Attributes))
    {
      unset($this->Attributes['checked']);
      $this->Checked = true;
    }
  }
  /**
   * @access private
   */
  function GetHtmlAttrs()
  {
    return parent::GetHtmlAttrs().($this->Checked ? ' checked="checked"' : '');
  }
}

/**
 * Represents a checkbox object.
 * @package TurboPhpLib
 */
class TTpCheckBox extends TTpBooleanBox
{
  var $Successes;
  /**
   * @access private
   */
  function Init()
  {
    parent::Init();
    $this->Successes = array();
  }
  /**
   * @access private
   */
  function Submit($inValue)
  {
    parent::Submit($inValue);
    $this->Successes[] = $this->Index;
    $this->Checked = true;
  }
  /**
   * @access private
   */
  function ProcessClicks()
  {
    foreach ($this->Successes as $success)
    {
      $this->Index = $success;
      $this->Click();
    }
  }
  /**
   * @access private
   */
  function BeforeGenerate()
  {
    if ($this->Page->HaveRequest())
      $this->Checked = in_array($this->Index, $this->Successes);
    parent::BeforeGenerate();
  }
  /* *
   * @access private
   */
/*
  function SetIndex($inIndex)
  {
    parent::SetIndex($inIndex);
    if ($this->Page->HaveRequest())
      $this->Checked = in_array($inIndex, $this->Successes);
  }
*/
}

/**
 * Represents a radio-button object.
 * @package TurboPhpLib
 */
class TTpRadio extends TTpBooleanBox
{
  var $CheckedIndex;
  var $Group;
  /**
   * @access private
   */
  function AfterParse()
  {
    parent::AfterParse();
    $this->Group = @$this->Attributes['name'];
    $this->CheckedIndex = -1;
  }
  /**
   * @access private
   */
  function GetRequestName()
  {
    return $this->GetPageQualifier() . $this->Group;
  }
  /**
   * @access private
   */
  function Submit(&$inInput)
  {
    $input = new TTpRequestVariable($inInput->Value, '');
    if ($input->Name == $this->Name)
    {
      $this->CheckedIndex = $input->Index;
      parent::Submit(&$input);
    }
  }
  /**
   * @access private
   */
  function ProcessClicks()
  {
    if ($this->Successful)
    {
      $this->Index = $this->CheckedIndex;
      if (!$this->Checked)
      {
        $this->Checked = true;
        $this->Click();
      }
    }
     else $this->Checked = false;
  }
  /**
   * @access private
   */
  function SetNameAttribute()
  {
    $this->Attributes['name'] = $this->GetRequestName();
  }
  /**
   * @access private
   */
  function SetValueAttribute()
  {
    $this->Attributes['value'] = $this->GetIndexedName();
  }
   /**
   * @access private
   */
  function SetIndex($inIndex)
  {
    if ($this->Page->HaveRequest())
      $this->Checked = ($inIndex == $this->CheckedIndex);
    else
      $this->Checked = ($inIndex == 0);
    parent::SetIndex($inIndex);
  }
}

/**
 * Represents a select object (aka combo-box or menu).
 * @package TurboPhpLib
 */
class TTpSelect extends TTpClickable
{
  /**
   * Array of options to display.
   * The {@link $Selected} property always refers to a key
   * of this array. The key could be an integer index, or a string if
   * if $Options is a hashed array.
   * @var array
   */
  var $Options;
  /**
   * Key of the selected item in the {@link Options} array.
   * Read this property to discover the selected value. Write this property
   * to change the selection.
   * To read/write the selected {@var value} use the {@link GetSelectedValue}
   * or {@link SetSelectedValue} methods.
   * @var mixed
   */
  var $Selected;
  /**
   * Reference to a {@link TTpListSource} object which will supply the option
   * array.
   * @var string
   */
  var $Source;
  /**
   * @access private
   */
  function Init()
  {
    parent::Init();
    $this->Options = array();
  }
  /**
   * @access private
   */
  function ExtractOptions()
  {
    // Clear options
    $this->Options = array();
    // Grab up the option tags
    preg_match_all("|<option[^<]*?|Ui", $this->Content, $options);
    // Parse options into array
    foreach ($options[0] as $key => $option)
    {
      // Grab the option content
      preg_match_all("/(?:[^>]*?)>([^<]*?)/Ui", $option, $contents);
      $content = $contents[1][0];
      // Grab the option attributes
      $a = TpExtractAttrs(&$option);
      //
      if (array_key_exists("value", $a))
        $value = $a["value"];
      else
        $value = $content;
      if (array_key_exists("selected", $a))
        $this->Selected = $value;
      // Store the option data
      $this->Options[$value] = $content;
      //
/*
      // This code stores indexed options by default,
      // instead of hashed options as above.
      if (array_key_exists("value", $a))
        $value = $a["value"];
      else
        $value = ''; //count($this->Options); //$content;
      if (array_key_exists("selected", $a))
        $this->Selected = count($this->Options); //$value;
      // Store the option data
      if ($value == '')
        $this->Options[] = $content;
      else
        $this->Options[$value] = $content;
*/
    }
  }
  /**
   * @access private
   */
  function AfterParse()
  {
    parent::AfterParse();
    $this->ExtractOptions();
    $this->MarshallObject('Source');
  }
  /**
   * @access private
   */
  function Submit(&$inInput)
  {
    parent::Submit(&$inInput);
    $this->Selected = $this->Value;
  }
  /**
   * @access private
   */
  function GetSourceOptions()
  {
    if ($this->Source)
    {
      // Stores options by index
      //$this->Options = array();
      //$this->Source->GetItems($this->Options);
      //
      // Stores options by hash
      $list = array();
      $this->Source->GetItems($list);
      // Store the option data
      $this->Options = array();
      foreach ($list as $value)
        $this->Options[$value] = $value;
    }
  }
  /**
   * @access private
   */
  function ProcessInput()
  {
    $lastSelected = $this->Selected;
    parent::ProcessInput();
    $this->GetSourceOptions();
    if ($this->Selected <> $lastSelected)
      $this->Click();
  }
  /**
   * @access private
   */
  function IsGoodSelection()
  {
    return array_key_exists($this->Selected, $this->Options);
  }
  /**
   * @access private
   */
  function SetDefaultSelection()
  {
    $keys = array_keys($this->Options);
    if (count($keys) > 0)
      $this->Selected = $keys[0];
    else
      $this->Selected = '';
  }
  /**
   * Returns the value of the selected option.
   * Where the {@link $Selected} property is a key into the {@link Options}
   * array, this function returns instead the <b>value</b> for that key.
   * This function validates {@link $Selected} by setting it to the default
   * value if it is improper.
   * @return string
   */
  function GetSelectedValue()
  {
    if (!$this->IsGoodSelection())
      $this->SetDefaultSelection();
    if ($this->IsGoodSelection())
      return $this->Options[$this->Selected];
    else
      return "";
  }
  /**
   * Sets the value of the selected option.
   * Where the {@link $Selected} property is a key into the {@link Options}
   * array, this function returns instead the <b>value</b> for that key.
   * This function validates {@link $Selected} by setting it to the default
   * value if it is improper.
   * @return string
   */
  function SetSelectedValue($inValue)
  {
    foreach ($this->Options as $key => $value)
      if ($value == $inValue)
      {
        $this->Selected = $key;
        break;
      }
  }
  /**
   * @access private
   */
  function GetDefaultValue()
  {
    return $this->GetSelectedValue();
  }
  /**
   * @access private
   */
  function GetHtmlOptions()
  {
    if (!$this->IsGoodSelection())
      $this->SetDefaultSelection();
    $result = '';
    foreach ($this->Options as $key => $value)
    {
      $a = array('value' => $key);
      if ($this->Selected == $key)
        $a['selected'] = 'selected';
      $result .= '<option'.$this->ArrayToAttrs($a).'>'.$value."</option>\n";
    }
    return $result;
  }
  /**
   * @access private
   */
  function BeforeClicks()
  {
    $this->GetSourceOptions();
  }
  /**
   * @access private
   */
  function GetContent()
  {
    return $this->GetHtmlOptions();
  }
}

/**
 * A container which can output itself multiple times.
 * A repeater will generate itself as a series of rows until {@link $Count}
 * iterations have been completed or {@link $Stop} becomes true.
 * The {@link $OnRepeat} event is fired once before each iteration.
 * Controls inside the repeater will fire {@link OnGenerate} events every
 * iteration.
 * The {@link $Index} property indicates the current iteration
 * (zero based, i.e., the first iteration has an $Index of zero).
 * @package TurboPhpLib
 */
class TTpRepeater extends TTpObj
{
  /**
   * Current iteration number (zero based).
   * @var Integer
   */
  var $Index;
  /**
   * Number of iterations to repeat.
   * If set to zero, the repeater will continue until {@link $Stop} is true.
   * @var Integer
   */
  var $Count;
  /**
   * Set true to stop the repeater.
   * The repeater will iterate until there have been {@link $Count} interations
   * or until $Stop becomes true. If {@link $Count} is zero, the repeater
   * will repeat indefinitely until $Stop is true.
   * @var Boolean
   */
  var $Stop;
  /**
   * Event that is fired before each repeat.
   * The $OnRepeat event is fired once before each iteration.
   *
   * The {@link $Index} property indicates the current iteration
   * (zero based, that is, the first iteration has $Index = zero).
   *
   * Set {@link $Stop} to true inside this event to stop generation immediately.
   *
   * {@link $Stop} will already be true if
   * {@link $Index} = {@link $Count} and {@link $Count} > 0.
   * In this case, you may set {@link $Stop} to false to continue repeating
   *
   * Note that controls inside the repeater will fire {@link OnGenerate} events
   * each iteration.
   */
  var $OnRepeat;
  /**
   * @access private
   */
  function Init()
  {
    parent::Init();
    $this->Count = 4;
  }
  /**
   * @access private
   */
  function GetChild(&$matches)
  {
    if ($this->Page->IsObject($matches[1]))
    {
      $obj = &$this->Page->Objects[$matches[1]];
      //echo "Found repeater child $obj->Name...<br>";
      $this->Children[] = &$obj;
      $this->LocateChildren($obj->Content);
    }
    return $matches[0];
  }
  /**
   * @access private
   */
  function LocateChildren(&$inText)
  {
    return preg_replace_callback(P_MACRO, array(&$this, 'GetChild'), $inText);
  }
  /**
   * @access private
   */
  function AfterParse()
  {
    parent::AfterParse();
    $this->LocateChildren($this->Content);
  }
  /**
   * @access private
   */
  function ShouldContinue()
  {
    return (!$this->Stop &&
      ($this->Count < 1 ?
        ($this->OnRepeat <> '') : $this->Index < $this->Count));
  }
  /**
   * @access private
   */
  function IndexChildren()
  {
    $c = count($this->Children);
    for ($i = 0; $i < $c; $i++)
    {
      //echo "Repeater child ".$this->Children[$i]->Name." is of class ".get_class($this->Children[$i])."<br>";
      if (is_a($this->Children[$i], "TTpInput"))
      {
        //echo "Repeater child ".$this->Children[$i]->Name." is an input.<br>";
        $this->Children[$i]->SetIndex($this->Index);
      }
    }
  }
  /**
   * @access private
   */
  function Repeat()
  {
    $this->Index++;
    if ($this->ShouldContinue())
    {
      $this->IndexChildren();
      $this->DoEvent($this->OnRepeat);
    }
    return $this->ShouldContinue();
  }
  /**
   * @access private
   */
  function GetContent()
  {
    $this->Stop = false;
    $this->Index = -1;
    $result = '';
    while ($this->Repeat())
      $result .= $this->Page->Interpolate($this->Content);
    return $result;
  }
  /**
   * Dump property information to the standard output.
   */
  function DumpVar(&$inVar, &$inValue)
  {
    if ($inVar == 'Children')
      echo "Children[]<br>";
    else
      parent::DumpVar(&$inVar, &$inValue);
  }
}

/**
 * A scrolling container.
 * @package TurboPhpLib
 */
class TTpScroller extends TTpObj
{
  /**
   * @access private
   */
  function GetContent()
  {
    return
      '<div style="width: 100%; height: 100%; overflow: auto;">'
        . parent::GetContent() .
          '</div>';
  }
}

/**
 * A dynamic table.
 * A table whose layout and content are specified at runtime (i.e. using PHP).
 *
 * Set the number of cells with the {@link $Rows} and {@link $Cols} properties.
 *
 * Each time a cell is generated, the {@link $OnCell} event is fired.
 * The row number is held in {@link $Row} and the col number in {@link $Col}
 * (zero-based).
 * Use the {@link $OnCell} event to specify the {@link $RowSpan} and
 * {@link $ColSpan} for the cell, to setup {@link $CellAttributes},
 * and assign {@link $Content}.
 * @package TurboPhpLib
 */
class TTpTable extends TTpObj
{
  /**
   * Number of rows to generate.
   * @var integer
   */
  var $Rows;
  /**
   * Number of columns to generate.
   * @var integer
   */
  var $Cols;
  /**
   * Event that fires before a cell is generated.
   * The row number of the current cell is available in {@link $Row}
   * and the column number in {@link $Col} (zero-based).
   * Use this event event to specify the {@link $RowSpan} and
   * {@link $ColSpan} for the cell, to setup {@link $CellAttributes},
   * and assign {@link $Content}.
   * @var string
   */
  var $OnCell;
  /**
   * Event that fires before a row is generated.
   * The row number of the current row is available in {@link $Row}
   * (zero-based).
   * Use this event event to specify {@link $RowAttributes} or to
   * calculate row-based values.
   * @var string
   */
  var $OnRow;
  /**
   * Index of the current row (first row has index zero).
   * @var integer
   */
  var $Row;
  /**
   * Index of the current column (first column has index zero).
   * @var integer
   */
  var $Col;
  /**
   * Number of rows the current cell will span. Generally set inside
   * an {@link OnCell} event handler.
   * @var integer
   */
  var $RowSpan;
  /**
   * Number of columns the current cell will span. Generally set inside
   * an {@link OnCell} event handler.
   * @var integer
   */
  var $ColSpan;
  /**
   * Set to true to cause the current cell to be a header cell
   * (i.e. to have a <th> tag instead of <td> tag).
   * @var boolean
   */
  var $HeaderCell;
  /**
   * Array of name, value HTML attribute pairs for the current cell.
   * Example:
   * $this->CellAttributes['color'] = 'green';
   * @var array
   */
  var $CellAttributes;
  /**
   * Array of name, value HTML attribute pairs for the current row.
   * Example:
   * $this->RowAttributes['color'] = 'blue';
   * @var array
   */
  var $RowAttributes;
  /**
   * @access private
   */
  var $Cells;
  /**
   * @access private
   */
  function Init()
  {
    parent::Init();
    $this->RowAttributes = array();
    $this->CellAttributes = array();
  }
  /**
   * @access private
   */
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
  /**
   * @access private
   */
  function ClearCells()
  {
    if ($this->Rows != 0 && $this->Cols != 0)
    {
      $f = array_fill(0, $this->Rows, false);
      $this->Cells = array_fill(0, $this->Cols, $f);
    }
  }
  /**
   * @access private
   */
  function MarkSpannedCells()
  {
    for ($j=0; $j<$this->RowSpan; $j++)
      for ($i=0; $i<$this->ColSpan; $i++)
        $this->Cells[$this->Col + $i][$this->Row + $j] = true;
  }
  /**
   * @access private
   */
  function GetSpanAttributes()
  {
    $result = '';
    if ($this->ColSpan > 1)
      $result .= ' colspan="'.$this->ColSpan.'"';
    if ($this->RowSpan > 1)
      $result .= ' rowspan="'.$this->RowSpan.'"';
    return $result;
  }
  /**
   * @access private
   */
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
          $result .= "<$t$a>".$this->Page->Interpolate($this->Content)."</$t>";
        }
      }
      $result .= "\n</tr>\n";
    }
    return $result;
  }
  /**
   * @access private
   */
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
         $result .= "<$t$a>".$this->Page->Interpolate($this->Content)."</$t>";
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
          $result .= "<$t$a>".$this->Page->Interpolate($this->Content)."</$t>";
        }
      }
      $result .= "\n</tr>\n";
    }
    $result = $this->GetStartTag().$result.$this->EndTag;
    $result = '<div style="width:100%; height:250px; overflow: auto;">'
            . $result .'</div>';
    return $html.$result;
  }
  /**
   * @access private
   */
  function GetContent()
  {
    return $this->GetFixedHtml();
    //return $this->GetScrollingHtml();
  }
}

/**
 * A single sheet inside a {@link TTpPageControl}.
 * @package TurboPhpLib
 */
class TTpSheet extends TTpObj
{
  /**
   * @access private
   */
  function AfterParse()
  {
    parent::AfterParse();
    $this->Styles['display'] = 'none';
  }
}

/**
 * A control that can display one of several overlapping blocks of content.
 * Specify which page is displayed by setting the {@link ActiveIndex} property.
 * @package TurboPhpLib
 */
class TTpPageControl extends TTpObj
{
  /**
   * The index of the page that will be displayed.
   * @var Integer One-based, e.g. first page is index 1.
   */
  var $ActiveIndex;
  /**
   * @access private
   */
  function Init()
  {
    parent::Init();
    $this->ActiveIndex = 1;
  }
  /**
   * @access private
   */
  function GetSheetNamePrefix()
  {
    return $this->Name . 'Sheet_';
  }
  /**
   * @access private
   */
  function GetContent()
  {
    $as = $this->GetSheetNamePrefix() . (string)$this->ActiveIndex;
    $GLOBALS[$as]->Styles['display'] = '';
    return parent::GetContent();
  }
}

/**
 * A page module.
 * @package TurboPhpLib
 */
class TTpModule extends TTpObj
{
  var $ModulePage;
  /**
   * @var string Name of module to include.
   */
  var $ModuleName;
  /**
   * @access private
   */
  function AfterParse()
  {
    $obj = substr($this->ModuleName, 0, -4);
    //echo "Looking for object [$obj]<br>";
    //echo "Including [$this->ModuleName]<br>";
    $GLOBALS[TPMODULEFLAG] = true;
    include $this->ModuleName;
    $this->ModulePage = &$$obj;
    //echo 'Marshalled module of class: ' . get_class($this->ModulePage) . '<br>';
  }
  /**
   * @access private
   */
  function BeforeGeneratePage()
  {
    //echo "BeforeGeneratePage<br>";
    $this->Page->Blocks[TPPORTABLES] .= "<!-- module: $this->Name -->\n";
    foreach ($this->ModulePage->Objects as $key => $value)
    {
      //echo "Testing [$key] for portability...<br>";
      if (strncmp($key, "TpPortable", 10) == 0)
      {
        //echo "Attaching content from $this->Name.$key to header block.<br>";
        $this->Page->Blocks[TPPORTABLES] .=
          $this->ModulePage->InterpolateObject($key) . "\n";
      }
    }
  }
  /**
   * @access private
   */
  function GetContent()
  {
    //$GLOBALS[TPMODULEFLAG] = true;
    //echo "Getting TTpModule Content [$this->ModuleFilename]<br>";
    //ob_start();
    //include $this->ModuleName;
    //$this->Content = ob_get_contents();
    $this->Content = $this->ModulePage->Generate();
    //$this->Content = "Getting TTpModule Content [$this->ModuleFilename]";
    //ob_end_clean();
    return parent::GetContent();
  }
}

?>
