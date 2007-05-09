<?php

/**
 *  More Controls for TurboPhp
 *  Version 1.0 Beta
 *
 *  Copyright (c) 2004 Least-Resistance Software
 *
 *  This source file is subject to the terms of the TurboPhp license,
 *  that is bundled with this package in the file LICENSE.TXT, and is
 *  available at through the world-wide-web at
 *  http://www.turbophp.com/turbophp/license.txt
 *
 * @package TurboMoreLib
 */

/**
 * @package TurboMoreLib
 */
class TTpFmtLabel extends TTpLabel
{
  var $FormatString;
  var $OnFormat;
  function GetContent()
  {
    $s = $this->DoEvent($this->OnFormat);
    if ($s !== 0)
      $this->Content = $s;
    else
    {
      if ($this->FormatString == '')
        $this->FormatString = '%s';
      $this->Content = sprintf($this->FormatString, $this->Caption);
    }
    return parent::GetContent();
  }
}

/**
 * @package TurboMoreLib
 */
class TTpDateLabel extends TTpLabel
{
  var $Filename;
  var $FormatString;
  var $OnFormat;
  var $Time;
  function GetTime()
  {
    if (file_exists($this->Filename))
      $this->Time = filemtime($this->Filename);
    if ($this->Time)
      return $this->Time;
    else
      return time();
  }
  function GetContent()
  {
    if ($this->FormatString == '')
      $this->FormatString = 'r';
    $this->Content = date($this->FormatString, $this->GetTime());
    return parent::GetContent();
  }
}

/**
 * @package TurboMoreLib
 */
class TTpSizeLabel extends TTpLabel
{
  var $Filename;
  var $Size;
  function GetSize()
  {
    if (file_exists($this->Filename))
      $this->Size = filesize($this->Filename);
    return $this->Size;
  }
  function GetContent()
  {
    $s = $this->GetSize();
    if ($s > 512 * 1024 * 1024)
      $this->Content = sprintf("%0.2f Gb", $s / (1024 * 1024 * 1024));
    else if ($s > 512 * 1024)
      $this->Content = sprintf("%0.2f Mb", $s / (1024 * 1024));
    else if ($s > 512)
      $this->Content = sprintf("%0.2f Kb", $s / 1024);
    else
      $this->Content = sprintf("%d bytes", $s);
    return parent::GetContent();
  }
}


?>
