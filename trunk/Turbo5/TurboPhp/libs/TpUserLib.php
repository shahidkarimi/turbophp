<?php

/**
 *  TurboPhp 4 - User Management Library
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
define("USERS_TBL", "UsersTbl");
define("F_USERID", "UserId");
define("F_LOGIN", "Login");
define("F_PASSWORD", "Password");

define("AUTH_TBL", "AuthTbl");
define("F_TOKEN", "Token");
define("F_TIME", "Time");

define("S_AUTH_TOKEN", "Auth");

define("E_BAD_LOGIN", "Bad login");
define("E_BAD_PASSWORD", "Bad password");
define("E_ALREADY_LOGGEDIN", "User already logged in");
/**#@-*/

/**
 * Create an SQL insert command.
 * Converts a table name and an array of name-value pairs
 * into an SQL insert statment.
 * @param $inTable string Table name for the query.
 * @param $inData array Data to insert, array of (fieldname => value) records.
 * @return string SQL insert command.
 */
function tpGetInsertSql($inTable, $inData)
{
  $p = implode(",", array_keys($inData));
  $v = "'" . implode("','", array_values($inData)) . "'";
  return sprintf("INSERT INTO %s (%s) VALUES (%s)", $inTable, $p, $v);
}

/**
 * Destroy the PHP session.
 * 1. Erases the session cookie.
 * 2. Unsets the $_SESSION array.
 * 3. Invokes session_destroy().
 */
function tpUnsession()
{
  // Destroy the session cookie
  setcookie(session_name(), '', 0, '/');
  // Unset all of the session variables.
  $_SESSION = array();
  // Finally, destroy the session.
  session_destroy();
  //session_regenerate_id();
}

/**
 * User authentication object.
 * @package TurboPhpLib
 */
class TTpAuthenticator extends TTpSilentObj
{
  var $Auto;
  //
  var $LoginInput;
  var $PasswordInput;
  //
  var $OnError;
  var $OnFailure;
  var $OnSuccess;
  var $OnLogout;
  var $OnQueryFailure;
  //
  var $SuccessUrl;
  var $FailureUrl;
  var $LogoutUrl;
  //
  var $Ok;
  var $ErrorCode;
  //
  var $UserId;
  var $Passcode;
  var $Login;

  // Authentication Functions

  function Init()
  {
    parent::Init();
    $this->DebugFlag = true;
  }

  function Redirect($inUrl)
  {
    $this->Page->Redirect($inUrl);
  }

  function AfterParse()
  {
    parent::AfterParse();
    $this->MarshallObject('LoginInput');
    $this->MarshallObject('PasswordInput');
  }

  function BeforeInput()
  {
    if ($this->Auto)
      $this->Authenticate();
  }

  function NeedSession()
  {
    if (!isset($_SESSION))
      session_start();
  }

  function Clear()
  {
    $this->Ok = false;
    $this->Login = '';
    $this->UserId = '';
    $this->Passcode = '';
    if (isset($_SESSION))
      unset($_SESSION[S_AUTH_TOKEN]);
  }

  function Error($inErrorCode)
  {
    $this->ErrorCode = $inErrorCode;
    $this->DoEvent($this->OnError);
  }

  function GetSessionToken()
  {
    $this->NeedSession();
    return TpSafeArrayGet(S_AUTH_TOKEN, $_SESSION);
  }

  /**
   * @abstract
   */
  function QueryUserByUserId()
  {
    return false;
  }

  function Authenticate()
  {
    $this->Ok = false;
    $this->UserId = $this->GetSessionToken();
    if ($this->QueryUserByUserId())
      $this->AuthSuccess();
    else
      $this->AuthFailure();
    return $this->Ok;
  }

  function AuthFailure()
  {
    //echo "AUth Fail<Br>";
    $this->Clear();
    $this->DoEvent($this->OnFailure);
    $this->Redirect($this->FailureUrl);
  }

  function AuthSuccess()
  {
    $this->Ok = true;
    $this->DoEvent($this->OnSuccess);
    $this->Redirect($this->SuccessUrl);
  }

  function Logout()
  {
    $this->Clear();
    $this->DoEvent($this->OnLogout);
    if ($this->LogoutUrl <> '')
      $this->Redirect($this->LogoutUrl);
    else
      $this->Redirect($this->FailureUrl);
  }

  // Login Functions

  /**
   * @abstract
   */
  function DoCreateUser(&$inLogin, &$inPassword)
  {
    return false;
  }

  function GetInputLoginValues(&$inLogin, &$inPassword)
  {
    if (($inLogin == '') && $this->LoginInput)
      $inLogin = $this->LoginInput->Value;
    //
    if (($inPassword == '') && $this->PasswordInput)
      $inPassword = $this->PasswordInput->Value;
  }

  function CreateUser($inLogin = '', $inPassword = '')
  {
    $this->GetInputLoginValues($inLogin, $inPassword);
    return $this->DoCreateUser($inLogin, $inPassword);
  }

  function VerifyPassword($inPassword)
  {
    $this->Debug($inPassword);
    $this->Debug(crypt($inPassword, $this->Passcode));
    $this->Debug($this->Passcode);
    return (crypt($inPassword, $this->Passcode) == $this->Passcode);
  }

  /**
   * @abstract
   */
  function QueryUserByLogin($inLogin)
  {
    return false;
  }

  function StoreClientToken()
  {
    $this->NeedSession();
    $_SESSION[S_AUTH_TOKEN] = $this->UserId;
  }

  function Login($inLogin = '', $inPassword = '')
  {
    $this->Clear();
    $this->GetInputLoginValues($inLogin, $inPassword);
    if (!$this->QueryUserByLogin($inLogin))
    {
      $this->Debug('Bad login.');
      $this->Error(E_BAD_LOGIN);
    }
    else
    {
      if (!$this->VerifyPassword($inPassword))
      {
        $this->Debug('Bad Password.');
        $this->Error(E_BAD_PASSWORD);
      }
      else
      {
        $this->Login = $inLogin;
        $this->StoreClientToken();
        $this->Ok = true;
      }
    }
    if ($this->Ok)
      $this->LoginSuccess();
    else
      $this->LoginFailure();
    return $this->Ok;
  }

  function LoginFailure()
  {
    $this->Ok = false;
    $this->DoEvent($this->OnFailure);
    $this->Redirect($this->FailureUrl);
  }

  function LoginSuccess()
  {
    $this->Ok = true;
    $this->DoEvent($this->OnSuccess);
    $this->Redirect($this->SuccessUrl);
  }
}

/**
 * User authentication via database.
 * @package TurboPhpLib
 */
class TTpDbAuthenticator extends TTpAuthenticator
{
  var $Db;
  var $Query;
  var $UsersTbl;
  var $UserIdField;
  var $LoginField;
  var $PasswordField;

  // Authentication Functions

  function Init()
  {
    parent::Init();
    $this->UsersTbl = USERS_TBL;
    $this->UserIdField = F_USERID;
    $this->LoginField = F_LOGIN;
    $this->PasswordField = F_PASSWORD;
  }

  function CreateQueryObject()
  {
     $this->Query = $this->Db->CreateNewQuery();
     $this->Query->Class = 'TpAuthQuery';
     $this->Query->Name = $this->Name . 'Query';
     $this->Query->OnFailure = $this->OnQueryFailure;
  }

  function AfterParse()
  {
    parent::AfterParse();
    $this->MarshallObject('Db');
    if ($this->Db)
      $this->CreateQueryObject();
  }

  function QueryUserByUserId()
  {
    if ($this->UserId == '')
      return false;
    $this->Query->SQL = sprintf(
        "SELECT %s FROM %s WHERE %s='%s'",
          $this->LoginField, $this->UsersTbl, $this->UserIdField, $this->UserId
      );
    $this->Query->Execute();
    if (!$this->Query->Next())
      return false;
    else
    {
      $this->Login = $this->Query->GetField($this->LoginField);
      return true;
    }
  }

  function GetCreateUserTableSQL()
  {
$sql =<<<ESQL
  CREATE TABLE `%s` (
    `%s` int(11) NOT NULL auto_increment,
    `%s` varchar(64) NOT NULL default '',
    `%s` varchar(64) NOT NULL default '',
    PRIMARY KEY  (`%2\$s`),
    UNIQUE KEY `%3\$s` (`%3\$s`)
  );
ESQL;
    return sprintf($sql,
      $this->UsersTbl,
      $this->UserIdField, $this->LoginField, $this->PasswordField,
      ''
    );
  }

  // Login Functions

  function CreateUserTable()
  {
      $this->Query->SQL = $this->GetCreateUserTableSQL();
      $this->Query->Execute();
  }

  function DoCreateUser(&$inLogin, &$inPassword)
  {
    $this->Query->SQL = tpGetInsertSql(
        $this->UsersTbl,
        array(
              $this->LoginField => $inLogin,
              $this->PasswordField => crypt($inPassword)
        )
      );
    $this->Query->Execute();
    return ($this->Query->Result != false);
  }

  function QueryUserByLogin($inLogin)
  {
    $this->Query->SQL = sprintf(
        "SELECT %s, %s FROM %s WHERE BINARY %s='%s'",
        $this->UserIdField, $this->PasswordField,
        $this->UsersTbl,
        $this->LoginField, $inLogin
      );
    $this->Query->Execute();
    if (!$this->Query->Next())
      return false;
    else
    {
      $this->UserId = $this->Query->GetField($this->UserIdField);
      $this->Passcode = $this->Query->GetField($this->PasswordField);
      return true;
    }
  }
}

?>
