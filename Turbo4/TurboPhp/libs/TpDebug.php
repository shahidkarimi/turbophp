<?php

/**
 *  TurboPhp 4 - TpDebug
 *  Version 4.1 Beta
 *
 *  Routines to access the TurboPhp debugging display (via FTP).
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
function TpDbgConnect()
{
  global $TpDbg;
  if (!isset($TpDbg))
  {
    $TpDbg = ftp_connect("localhost", 999, 20);
    if ($TpDbg)
      ftp_login($TpDbg, "PHP", "TURBO");
   else
      echo "TurboPhp: Could not connect to debug server.<br>";
  }
  return $TpDbg;
}

function TpDbgSend($inMsg)
{
  global $TpDbg;
  if ($inMsg != '')
    @ftp_site($TpDbg, $inMsg);
}

function TpDbgMsg($inMsg)
{
  if (TpDbgConnect())
    TpDbgSend($inMsg);
}

function TpDbgLines($inLines)
{
  if (TpDbgConnect())
  {
    $inLines = str_replace(array("\n", "\r"), array("<br>", ""), $inLines);
    $sa = explode("<br>", $inLines);
    foreach ($sa as $line)
      TpDbgMsg($line);
  }
}

function TpDbgStart()
{
  ob_start();
}

function TpDbgEnd()
{
  $s = ob_get_contents();
  ob_end_clean();
  TpDbgLines($s);
}

?>
