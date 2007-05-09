<?php

/**
 *  TurboPhp - TpParser
 *  Version 4.1 Beta
 *
 *  Copyright (c) 2004-2005 Least-Resistance Software
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
define('P_WS', '\s*');

define('P_SLASH', '(/*)');
define('P_TOKEN', '([^\s]*)');
define('P_ATTRS', '(.*)');

define('P_TAG', '#'. P_SLASH . P_TOKEN . P_WS . P_ATTRS . '#sm');

define('P_ANAME', '[^=\s]*');
define('P_VALUE', '[^"\s>]*');
define('P_QVALUE', '(?:")([^"]*)(?:")');

define('P_ATTR', '/'.P_WS.'('.P_ANAME.')'.P_WS.'='.P_WS.'('.P_QVALUE.'|'.P_VALUE.')/');
/**#@-*/

/**
 * @access private
 */
function &TpExtractAttrs(&$inAttrs)
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

/**
 * @access private
 * @package TurboPhpLib
 */
class TTpTag
{
  var $Attributes;
  var $Markup;
  var $Element;
  var $EndTag;
  var $IsEnd;
  var $Level;
  var $Pre;
  var $RawAttrs;
  var $Tags;
  /**
   * @access private
   */
  function TTpTag()
  {
    $this->Init();
  }
  /**
   * @access private
   */
  function Init()
  {
    $this->Attributes = array();
    $this->Level = 0;
    $this->Tags = array();
  }
  /**
   * @access private
   */
  function EachTagDo($inMethod)
  {
    for ($i = 0; $i < count($this->Tags); $i++)
      $this->Tags[$i]->$inMethod();
  }
  /**
   * @access private
   */
  function Parse()
  {
    //echo "Parsing Markup: [$this->Markup]<br>";
    preg_match(P_TAG, $this->Markup, $matches);
    if (count($matches) > 1)
    {
      $this->IsEnd = ($matches[1] <> '');
      $this->Element = strtolower($matches[2]);
      $this->RawAttrs = $matches[3] . (count($matches) < 4 ? $matches[4] : '');
      //$this->ParseAttrs($this->RawAttrs);
    }
  }
  /**
   * @access private
   */
  function ExtractAttr($matches)
  {
    $c = count($matches);
    if ($c > 1)
    {
      $name = $matches[1];
      if ($c > 3)
        $value = $matches[3];
      else if ($c > 2)
        $value = $matches[2];
      $this->Attributes[$name] = $value;
      return '';
    }
    return $matches[0];
  }
  /**
   * @access private
   */
  function ExtractAttrs(&$inAttrs)
  {
    return preg_replace_callback(P_ATTR, array(&$this, 'ExtractAttr'),
      $inAttrs);
  }
  /**
   * @access private
   */
  function ParseAttrs(&$inRawAttrs)
  {
    $inRawAttrs = $this->ExtractAttrs($inRawAttrs);
    if ($inRawAttrs)
      $inRawAttrs = ' ' . trim($inRawAttrs);
  }
  /**
   * @access private
   */
  function GetHtmlAttrs()
  {
    if ($this->RawAttrs)
      return ' ' . $this->RawAttrs;
    else
      return '';
  }
  /**
   * @access private
   */
  function GetStartTag()
  {
    if ($this->Markup)
      return '<' . $this->Markup . '>';
    else
      return '';
  }
  /**
   * @access private
   */
  function GetEndTag()
  {
    if ($this->EndTag)
      return $this->EndTag->Pre . $this->EndTag->GetStartTag();
    else
      return '';
  }
  /**
   * @access private
   */
/*
  function GenerateHtml()
  {
    echo $this->Pre;
    echo $this->GetStartTag();
    $this->EachTagDo("GenerateHtml");
    echo $this->GetEndTag();
  }
*/
  /**
   * @access private
   */
  function DumpLevel($inLevel, $inIndent = true)
  {
    if ($this->Markup)
    {
      if ($this->EndTag)
        echo "{+}";
      echo "<$this->Markup><br>";
    }
    if ($this->Level >= $inLevel)
      echo "[detail]<br>";
    else
      for ($i=0; $i<count($this->Tags); $i++)
      {
        if ($this->EndTag)
          $this->Tags[$i]->Level = $this->Level + 1;
        else
          $this->Tags[$i]->Level = $this->Level;
        $this->Tags[$i]->DumpLevel($inLevel, ($this->EndTag <> false));
      }
    if ($this->Markup)
    {
      if ($this->EndTag)
        echo "{-}<br>";
      //if ($this->EndTag)
      //  echo "<".$this->EndTag->Markup."><br>";
    }
  }
}

/**
 * @access private
 * @package TurboPhpLib
 */
class TTpHtmlParser
{
  var $Count;
  var $I;
  var $PopTag;
  var $Root;
  var $Text;

  function LoadFile($inFilename)
  {
     $handle = fopen($inFilename, 'r');
     $this->Text = fread($handle, filesize($inFilename));
     fclose($handle);
  }

  function NotDelimiter($inDelim)
  {
    return (($this->I < $this->Count) && ($this->Text{$this->I} <> $inDelim));
  }

  function GetDelimited($inDelim)
  {
    while ($this->NotDelimiter($inDelim))
    {
      //echo htmlspecialchars($this->Text{$this->I});
      $this->I++;
    }
  }

  function GetToken($inDelim)
  {
    $s = $this->I;
    while ($this->NotDelimiter($inDelim))
    {
      if ($this->Text{$this->I++} == '"')
      {
          //echo htmlspecialchars('[->'.$this->Text{$this->I - 1});
          $this->GetDelimited('"');
          $this->I++;
          //echo htmlspecialchars($this->Text{$this->I - 1}.'<-]');
          //echo '<br>';
      }
    }
    return substr($this->Text, $s, $this->I++ - $s);
  }

  function GetTag(&$inTag)
  {
    $inTag->Pre = $this->GetToken('<');
    $inTag->Markup = $this->GetToken('>');
    if ($inTag->Markup)
      $inTag->Parse();
  }

  function &ManufactureObject(&$inTag)
  {
    return $inTag;
  }

  function MatchTag(&$inTag)
  {
    //$s = '';
    while ($this->I < $this->Count)
    {
      $tag = new TTpTag();
      $tag->Level = $inTag->Level + 1;
      $this->GetTag(&$tag);
      if ($tag->IsEnd)
      {
        if ($tag->Element == $inTag->Element)
        {
          $inTag->EndTag = &$tag;
          //$s .= "ending [$inTag->Level: $inTag->Element] with [/" . $inTag->EndTag->Level . ": " . $inTag->EndTag->Element ."]...<br>";
        }
        else
        {
          //$s .= "looking for [/$inTag->Level: $inTag->Element] found [/$tag->Level: $tag->Element]...POP<br>";
          $this->PopTag = &$tag;
        }
        //return $s;
        return;
      }
      else
      {
        //$obj = $this->ManufactureObject(&$tag);
        $obj = &$tag;
        if ($obj != NULL)
        {
          //echo "looking for [/$inTag->Level: $inTag->Element] found [$tag->Level: $tag->Element]...<br>";
          //echo "adding [$tag->Level: $tag->Element] to [$inTag->Level: $inTag->Element]...<br>";
          $this->MatchTag(&$obj);
          $inTag->Tags[] = $obj;
          if ($this->PopTag)
          {
            //$s .= "comparing [/$inTag->Element] and [/" . $this->PopTag->Element . "]...<br>";
            if ($inTag->Element == $this->PopTag->Element)
            {
              $this->PopTag->Level = $inTag->Level + 1;
              $inTag->EndTag = $this->PopTag;
              $this->PopTag = false;
              //$s .= "ending [$inTag->Level: $inTag->Element] with [/" . $inTag->EndTag->Level . ": " . $inTag->EndTag->Element ."]...<br>";
            }
            //else $s .= "POP<br>";
            //return $s;
            return;
          }
        }
      }
    }
//    return $s;
  }

  function Parse()
  {
    $this->I = 0;
    $this->Root = new TTpTag();
    $this->Count = strlen($this->Text);
    //echo $this->MatchTag($this->Root);
    $this->MatchTag($this->Root);
  }

  function ParseFile($inFilename)
  {
    $this->LoadFile($inFilename);
    $this->Parse();
  }
}

?>
