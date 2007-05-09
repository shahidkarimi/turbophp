object CodeEditForm: TCodeEditForm
  Left = 337
  Top = 376
  Width = 592
  Height = 431
  Caption = 'CodeEditForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 584
    Height = 21
    AutoSize = True
    ButtonHeight = 19
    ButtonWidth = 52
    Caption = 'ToolBar1'
    Flat = True
    HideClippedButtons = True
    List = True
    ShowCaptions = True
    TabOrder = 0
    Wrapable = False
    object CollapseButton: TToolButton
      Left = 0
      Top = 0
      AutoSize = True
      Caption = 'Collapse'
      ImageIndex = 0
      Style = tbsCheck
      OnClick = CollapseButtonClick
    end
  end
  object Edit: TEasyEdit
    Left = 0
    Top = 21
    Width = 584
    Height = 376
    Cursor = crIBeam
    Align = alClient
    AutoCorrectDelims = ' .,;:'
    BlockSelMode = bsBoth
    CodeTemplates = <>
    CollapseOptions = [coCollapseButtonOnGutter, coPaintCollapseLines, coShowCollapsedHints]
    Colors.InActiveSelBackColor = clBtnFace
    DefaultPage.Header.RightText = '\[date]'
    DefaultPage.Header.Font.Charset = DEFAULT_CHARSET
    DefaultPage.Header.Font.Color = clBtnShadow
    DefaultPage.Header.Font.Height = -11
    DefaultPage.Header.Font.Name = 'MS Sans Serif'
    DefaultPage.Header.Font.Style = []
    DefaultPage.Footer.CenterText = 'page \[page] of \[pages]'
    DefaultPage.Footer.Font.Charset = DEFAULT_CHARSET
    DefaultPage.Footer.Font.Color = clBtnShadow
    DefaultPage.Footer.Font.Height = -11
    DefaultPage.Footer.Font.Name = 'MS Sans Serif'
    DefaultPage.Footer.Font.Style = []
    EditSource = Source
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Gutter.Brush.Color = clBtnFace
    Gutter.Width = 48
    LineDivider.Options = []
    LineDivider.Brush.Color = clBtnShadow
    LineBreak = lbCRLF
    LineNumbers.Options = [lnPaintOnGutter]
    LineNumbers.Visible = True
    LineNumbers.Alignment = lnRight
    LineNumbers.LeftIndent = 16
    LineNumbers.StartFrom = 1
    LineStyles = <>
    Margin.Pen.Color = clBtnShadow
    Options = [eoPaintGutter, eoPaintMargin, eoOverwriteBlocks, eoEnableDragging, eoEnableSearchHighlight, eoEnableSelection, eoSelectBeyondEol, eoForceCutCopy, eoLineStyleBeyondEol, eoInvertLineStyle, eoThumbTracking, eoTripleClickLine]
    PageNumFont.Charset = DEFAULT_CHARSET
    PageNumFont.Color = clWindowText
    PageNumFont.Height = -11
    PageNumFont.Name = 'MS Sans Serif'
    PageNumFont.Style = []
    PopupWindow.Height = 300
    PopupWindow.Width = 200
    PopupWindow.SizeAble = True
    PopupWindow.AdjustSize = False
    PopupWindow.CloseOnSingleClick = True
    Ruler.Options = []
    Ruler.Units = erMilimeters
    ScrollBars = ssBoth
    TabOrder = 1
    TabStop = True
    WantTabs = True
    CodeParamChars = '.'
    OnSourceChanged = EditSourceChanged
    OnAutoComplete = EditAutoComplete
    OnEnter = EditEnter
    OnExit = EditExit
  end
  object PhpParser: TEasyEditorParser
    Rules.Strings = (
      '//Language: PHP                      '
      ''
      '// states'
      'State=snormal'
      'State=sstring1'
      'State=sstring2'
      'State=sstring3'
      'State=sstring4'
      'State=sstring5'
      'State=scomment1'
      'State=scomment2'
      'State=scomment3'
      'State=scomment4'
      'State=shtmltag'
      'State=shtmlPHP'
      'State=smember'
      '// tokens'
      'Token=tnone'
      'Token=tstring'
      'Token=tcomment'
      'Token=tident'
      'Token=tinteger'
      'Token=tfloat'
      'Token=tresword'
      'Token=tassembler'
      'Token=turl'
      'Token=twhitespace'
      'Token=tresword1'
      'Token=tsuperglobal'
      'Token=tindirect'
      'Token=tmember'
      'Token=tvariable'
      '//delims'
      'Delimiters=;.,:'#39'"{}[]()?!@#%^&*-+=|\/<>'
      ''
      ''
      '//Common'
      'snormal <                          shtmltag  tresword1'
      
        'shtmltag [^\>^\'#39'^\"^\0-9^#32^\=][^\>^\'#39'^\"^#32^\=]* shtmltag  tr' +
        'esword1'
      'shtmltag >                         snormal   tresword1'
      ''
      ''
      '//white space'
      'snormal #32*                      snormal   twhitespace'
      ''
      ''
      '//strings'
      'shtmltag   \'#39'                     sstring1   tstring'
      'sstring1   [^\'#39']*                 sstring1   tstring'
      'sstring1   \'#39'                     shtmltag   tstring'
      'sstring1   \'#39'\'#39'                   sstring1   tstring'
      'sstring1   $                      shtmltag   tstring'
      ''
      'shtmltag    \"                    sstring2   tstring'
      'sstring2   [^\"]*                 sstring2   tstring'
      'sstring2   \"                     shtmltag   tstring'
      'sstring2   \"\"                   sstring2   tstring'
      'sstring2   $                      shtmltag   tstring'
      ''
      ''
      '// numbers'
      'shtmltag [0-9][0-9]*              shtmltag  tinteger'
      'shtmltag \#[0-9A-F]+              shtmltag  tinteger'
      'shtmltag [1-9][0-9]*\.[0-9]*                    shtmltag  tfloat'
      
        'shtmltag [1-9][0-9]*{\.[0-9]+}|e{[\+\-]}|[0-9]+ shtmltag   tfloa' +
        't'
      ''
      ''
      '//comments'
      'shtmltag   <\!--                    scomment1 tcomment'
      'scomment1 [^\-]*                   scomment1 tcomment'
      'scomment1  -->                     shtmltag   tcomment'
      ''
      '//idents'
      'shtmlPHP   [a-z_A-Z][a-z_A-Z0-9]*  shtmlPHP   tident'
      'shtmlPHP   [\$][a-z_A-Z][a-z_A-Z0-9]*  shtmlPHP   tvariable'
      ''
      '// reswords'
      'shtmlPHP '#39'.'#39'        shtmlPHP tresword'
      'shtmlPHP '#39'break'#39'        shtmlPHP tresword'
      'shtmlPHP '#39'case'#39'         shtmlPHP tresword'
      'shtmlPHP '#39'class'#39'        shtmlPHP tresword'
      'shtmlPHP '#39'continue'#39'     shtmlPHP tresword'
      'shtmlPHP '#39'default'#39'      shtmlPHP tresword'
      'shtmlPHP '#39'define'#39'       shtmlPHP tresword'
      'shtmlPHP '#39'do'#39'           shtmlPHP tresword'
      'shtmlPHP '#39'else'#39'         shtmlPHP tresword'
      'shtmlPHP '#39'elseif'#39'       shtmlPHP tresword'
      'shtmlPHP '#39'endfor'#39'       shtmlPHP tresword'
      'shtmlPHP '#39'endif'#39'        shtmlPHP tresword'
      'shtmlPHP '#39'endswitch'#39'    shtmlPHP tresword'
      'shtmlPHP '#39'endwhile'#39'     shtmlPHP tresword'
      'shtmlPHP '#39'extends'#39'      shtmlPHP tresword'
      'shtmlPHP '#39'for'#39'          shtmlPHP tresword'
      'shtmlPHP '#39'function'#39'     shtmlPHP tresword'
      'shtmlPHP '#39'global'#39'       shtmlPHP tresword'
      'shtmlPHP '#39'if'#39'           shtmlPHP tresword'
      'shtmlPHP '#39'include'#39'       shtmlPHP tresword'
      'shtmlPHP '#39'include_once'#39'       shtmlPHP tresword'
      'shtmlPHP '#39'int'#39'          shtmlPHP tresword'
      'shtmlPHP '#39'new'#39'          shtmlPHP tresword'
      'shtmlPHP '#39'old_function'#39' shtmlPHP tresword'
      'shtmlPHP '#39'pval'#39'         shtmlPHP tresword'
      'shtmlPHP '#39'require'#39'       shtmlPHP tresword'
      'shtmlPHP '#39'require_once'#39'       shtmlPHP tresword'
      'shtmlPHP '#39'return'#39'       shtmlPHP tresword'
      'shtmlPHP '#39'static'#39'       shtmlPHP tresword'
      'shtmlPHP '#39'string'#39'       shtmlPHP tresword'
      'shtmlPHP '#39'switch'#39'       shtmlPHP tresword'
      'shtmlPHP '#39'this'#39'       shtmlPHP tresword'
      'shtmlPHP '#39'var'#39'          shtmlPHP tresword'
      'shtmlPHP '#39'void'#39'         shtmlPHP tresword'
      'shtmlPHP '#39'while'#39'        shtmlPHP tresword'
      ''
      '//superglobals'
      'shtmlPHP   [\$]GLOBALS  shtmlPHP   tsuperglobal'
      'shtmlPHP   [\$]REQUEST  shtmlPHP   tsuperglobal'
      ''
      '//members'
      'shtmlPHP   [\$[a-z_A-Z][a-z_A-Z0-9]*->  smember   tindirect'
      'smember [a-z_A-Z][a-z_A-Z0-9]*  shtmlPHP   tmember'
      ''
      '// numbers'
      'shtmlPHP [0-9][0-9]*              shtmlPHP  tinteger'
      'shtmlPHP 0x[0-9A-F]+              shtmlPHP  tinteger'
      'shtmlPHP [1-9][0-9]*\.[0-9]*                    shtmlPHP  tfloat'
      
        'shtmlPHP [1-9][0-9]*{\.[0-9]+}|e{[\+\-]}|[0-9]+ shtmlPHP   tfloa' +
        't'
      ''
      '//strings'
      'shtmlPHP  \'#39'                     sstring3   tstring'
      'sstring3  [^\'#39']*                 sstring3   tstring'
      'sstring3  \'#39'                     shtmlPHP   tstring'
      'sstring3  \'#39'\'#39'                   sstring3   tstring'
      'sstring3   \\                    sstring3   tstring'
      'sstring3   \\\'#39'                  sstring3   tstring'
      'sstring3  $                      shtmlPHP   tstring'
      ''
      'shtmlPHP  \"                     sstring4   tstring'
      'sstring4  [^\"]*                 sstring4   tstring'
      'sstring4  \"                     shtmlPHP   tstring'
      'sstring4  \"\"                   sstring4   tstring'
      'sstring4   \\                    sstring4   tstring'
      'sstring4   \\\"                  sstring4   tstring'
      'sstring4  $                      shtmlPHP   tstring'
      ''
      'shtmlPHP    \`                    sstring5   tstring'
      'sstring5   [^\`]*                 sstring5   tstring'
      'sstring5   \`                     shtmlPHP   tstring'
      'sstring5   \`\`                   sstring5   tstring'
      'sstring5   \\                     sstring5   tstring'
      'sstring5   \\\`                   sstring5   tstring'
      'sstring5   $                      shtmlPHP   tstring'
      ''
      '//comments'
      'shtmlPHP   //                      scomment2  tcomment'
      'scomment2  [#1-#255]*              shtmlPHP   tcomment'
      'scomment2 $                        shtmlPHP   tcomment'
      ''
      'shtmlPHP   \#                      scomment3  tcomment'
      'scomment3  [#1-#255]*              shtmlPHP   tcomment'
      'scomment3 $                        shtmlPHP   tcomment'
      ''
      'shtmlPHP   /\*                     scomment4  tcomment'
      'scomment4  [^\*]*                  scomment4  tcomment'
      'scomment4  \*[^/]                  scomment4  tcomment'
      'scomment4  [\*]*/                  shtmlPHP   tcomment'
      ''
      '//PHP'
      'snormal  <?                        shtmlPHP  tresword'
      'shtmlPHP ?>                        snormal   tresword'
      ''
      'snormal  <%                        shtmlPHP  tresword'
      'shtmlPHP %>                        snormal   tresword'
      ''
      'snormal  <script#32language="php">   shtmlPHP  tresword'
      'shtmlPHP </script>                   snormal   tresword')
    Styles = <
      item
        FontColor = clBlack
        Name = 'tnone'
        Description = 'None'
      end
      item
        FontColor = clFuchsia
        Name = 'tstring'
        Description = 'String'
      end
      item
        FontColor = clGray
        FontStyle = [fsItalic]
        Name = 'tcomment'
        Description = 'Comment'
      end
      item
        FontColor = clBlack
        Name = 'tident'
        Description = 'Ident'
      end
      item
        FontColor = clMaroon
        Name = 'tinteger'
        Description = 'Integer'
      end
      item
        FontColor = clMaroon
        Name = 'tfloat'
        Description = 'Float'
      end
      item
        FontColor = clBlack
        FontStyle = [fsBold]
        Name = 'tresword'
        Description = 'Resword'
      end
      item
        FontColor = clGreen
        Name = 'tassembler'
        Description = 'Assembler'
      end
      item
        FontColor = clBlue
        FontStyle = [fsUnderline]
        Name = 'turl'
        Description = 'URL'
      end
      item
        FontColor = clBlack
        Name = 'twhitespace'
        Description = 'White Space'
      end
      item
        FontColor = clTeal
        FontStyle = [fsBold, fsItalic]
        Name = 'tresword1'
        Description = 'Resword 1'
      end
      item
        FontColor = clGreen
        FontStyle = [fsBold]
        Name = 'tsuperglobal'
      end
      item
        FontColor = clNavy
        Name = 'tindirect'
      end
      item
        FontColor = 33023
        Name = 'tmember'
      end
      item
        FontColor = clNavy
        Name = 'tvariable'
      end
      item
        FontColor = clHighlightText
        Color = clHighlight
        Name = 'Active Selection'
        Description = 'Active Selection'
      end
      item
        Color = clBtnFace
        Name = 'Inactive Selection'
        Description = 'Inactive Selection'
      end
      item
        FontColor = clLime
        Color = clBlack
        Name = 'Search Match'
        Description = 'Search Match'
      end
      item
        FontColor = clBtnFace
        Color = -1
        Name = 'Disabled Color'
        Description = 'Disabled Color'
      end
      item
        FontColor = clBtnFace
        Color = -1
        Name = 'ReadOnly Color'
        Description = 'ReadOnly Color'
      end>
    ColorMapping = cmDefault
    Left = 460
    Top = 96
  end
  object Source: TEasyEditSource
    Options = [srBackUnindents, srUseTab, srCursorThroughTabs, srOptimalFill, srAllowUndo, srGroupUndo, srBeyondEol, srAllowHiddenLines, srHighlightUrls, srLeaveTabs]
    Parser = PhpParser
    Strings.Strings = (
      '<?php'
      ''
      '$GLOBALS, $REQUEST, $request, request, globals, $this'
      ''
      'require_once('#39'TpTemplate.php'#39');'
      'require_once('#39'TpObj.php'#39');'
      'require_once('#39'TpControls.php'#39');'
      ''
      'define('#39'P_TOKEN'#39', '#39'(\S*)'#39');'
      '//define('#39'P_WS'#39', '#39'\s*'#39');'
      'define('#39'P_ATTRS'#39', '#39'([^>]*)'#39');'
      'define('#39'M_END_TAG'#39', 6);'
      ''
      'class TTpApp '
      '{'
      '  var $Template;'
      '  var $TemplateFile;'
      '  var $Blocks;'
      '  var $Objects;'
      '  var $Handled;'
      '    '
      '  function TTpApp()'
      '  {'
      '    $this->Template = new TTpTemplate();'
      '  }'
      '  '
      '  function LoadTemplate()'
      '  {'
      '    $this->Template->LoadTemplate($this->TemplateFile);'
      '  }'
      '  '
      '  function ExtractObject(&$matches) '
      '  { '
      '    //echo "[".htmlspecialchars($matches[0])."]<br>";'
      '    if ($matches[M_TP_NAME] == '#39#39') '
      '      return $matches[0];'
      '    switch ($matches[M_TP_CLASS]) '
      '    {'
      '      case '#39'button'#39':'
      '        $obj = new TTpButton(); '
      '        break;'
      '      case '#39'label'#39':'
      '        $obj = new TTpObj(); '
      '        break;'
      '      case '#39'repeater'#39':'
      '        $obj = new TTpRepeater(); '
      '        break;'
      '      default:'
      '        $obj = new TTpObj(); '
      '        break;'
      '    }'
      '    $obj->Name = $matches[M_TP_NAME];'
      '    $obj->Elt = $matches[M_ELEMENT];'
      '    $obj->Content = $matches[M_CONTENT];'
      '    $obj->EndTag = $matches[M_END_TAG];'
      '    $obj->ParseAttrs($matches[M_ATTRIBUTES]);'
      '    $this->Objects[$obj->Name] = &$obj;'
      '    //$GLOBALS[$name] = &$obj;'
      '    //echo "Added object [$name]<br>";'
      '    return '#39'{%'#39'.$obj->Name.'#39'}'#39';'
      '  } '
      ''
      '  function ExtractObjects()'
      '  {'
      '    $pattern = P_OBJECT;'
      
        '    $this->Template->Contents = preg_replace_callback($pattern, ' +
        'array(&$this, '#39'ExtractObject'#39'), $this->Template->Contents);'
      '  }'
      '  '
      '  function IsAppObject(&$inName)'
      '  {'
      '    return array_key_exists($inName, $this->Objects);'
      '  }'
      '  '
      '  function InterpolateObject(&$matches) '
      '  { '
      
        '    //echo "Looking for [$matches[1]] in $this->Objects (".count' +
        '($this->Objects).")<br>";'
      '    if ($this->IsAppObject($matches[1]))'
      '      $r = $this->Objects[$matches[1]]->GetHtml();'
      '    else if ($this->IsAppBlock($matches[1]))'
      '      $r = $this->Blocks[$matches[1]];'
      '    else    '
      '      $r = $matches[1];'
      '    //echo "$this->Objects (".count($this->Objects).")<br>";'
      '    return $r;'
      '  } '
      ''
      '  function InterpolateObjects(&$inText) '
      '  {'
      '    $pattern = '#39'/{%([^}]*)}/Usm'#39';'
      
        '    return preg_replace_callback($pattern, array(&$this, '#39'Interp' +
        'olateObject'#39'), $inText);'
      '  }'
      ''
      '  function ExtractBlock(&$matches)'
      '  {'
      '    //echo "Matched a block [$matches[1]] -> [$matches[3]]<br>";'
      '    $this->Blocks[$matches[1]] = $matches[2];'
      '    return '#39#39';'
      '  }'
      ''
      '  function ExtractBlocks()'
      '  {'
      '    //echo "Extracting blocks<br>";'
      '    $this->Blocks = array();'
      '    $pattern = '#39'/{%--BEGIN ([^}]*)}(.*){%--END \1}/Usm'#39';'
      '    //$pattern = "/{%--BEGIN ([^}]*)}(.*){%--END ([^}]*)}/Usm";'
      
        '    $this->Template->Contents = preg_replace_callback($pattern, ' +
        'array(&$this, '#39'ExtractBlock'#39'), $this->Template->Contents);'
      '  }'
      '  '
      '  function IsAppBlock(&$inName)'
      '  {'
      '    return array_key_exists($inName, $this->Blocks);'
      '  }'
      '  '
      '  /*  '
      '  function DoForObjects(&$inMethod)'
      '  {'
      '    if (is_array($this->Objects))'
      '    {'
      '      $keys = array_keys($this->Objects);'
      '      foreach ($keys as $key) {'
      '        $ctrl =& $this->Objects[$key];'
      '          $this->$inMethod($ctrl);'
      '      }'
      '    }'
      '  }'
      '  */'
      ''
      '  function ProcessRequest()'
      '  { '
      '    $keys = array_keys($this->Objects);'
      '    foreach ($keys as $key) '
      '    {'
      '      $obj =& $this->Objects[$key];'
      '      $obj->ProcessRequest(&$this);'
      '      if ($this->Handled)'
      '        break;'
      '    }'
      '  }'
      '  '
      '  function Run()'
      '  {'
      '    $this->LoadTemplate();'
      '    $this->ExtractObjects();    '
      '    //$this->ProcessRequest();'
      '    if (!$this->Handled) '
      '    {'
      
        '      $this->Template->Contents = $this->InterpolateObjects($thi' +
        's->Template->Contents);'
      '      echo $this->Template->Contents;'
      '    }'
      '  }'
      ' '
      '  function DumpRequest()'
      '  {'
      '    echo "Request:<pre>";'
      '    print_r($_REQUEST);'
      '    echo "</pre>";'
      '  }'
      ''
      '  function DumpObjects()'
      '  { '
      
        '    echo '#39'<br><br>==object dump ('#39'.count($this->Objects).'#39')==<br' +
        '><br>'#39';'
      '    $i = 0;'
      '    foreach ($this->Objects as $obj) '
      '    {'
      '      echo '#39'['#39'.$i++.'#39']<br>'#39';'
      '      $obj->Dump();'
      '      echo "<br>";'
      '    }'
      '  }'
      '}'
      ''
      '?>')
    TabStops = '2'
    Left = 376
    Top = 148
  end
  object ChangeTimer: TTimer
    Enabled = False
    Interval = 2000
    OnTimer = ChangeTimerTimer
    Left = 376
    Top = 96
  end
  object JsParser: TEasyEditorParser
    Rules.Strings = (
      '//Language: Java Script'
      '//Copyright (c) 1992-2002 Altium Limited             '
      '//All rights reserved.                               '
      '//http://www.dream-com.com                           '
      '//contact@dream-com.com                              '
      ''
      '// states'
      'State=snormal,CaseSensitive'
      'State=sstring1'
      'State=sstring2'
      'State=scomment1'
      'State=scomment2'
      '// tokens'
      'Token=tnone'
      'Token=tstring'
      'Token=tcomment'
      'Token=tident'
      'Token=tinteger'
      'Token=tfloat'
      'Token=tresword'
      'Token=tassembler'
      'Token=turl'
      'Token=twhitespace'
      '//delims'
      'Delimiters=;.,:'#39'"{}[]()?!@#$%^&*-+=|\/'
      ''
      '// reswords'
      'snormal '#39'break'#39'        snormal tresword'
      'snormal '#39'case'#39'         snormal tresword'
      'snormal '#39'catch'#39'        snormal tresword'
      'snormal '#39'class'#39'        snormal tresword'
      'snormal '#39'const'#39'        snormal tresword'
      'snormal '#39'continue'#39'     snormal tresword'
      'snormal '#39'debugger'#39'     snormal tresword'
      'snormal '#39'default'#39'      snormal tresword'
      'snormal '#39'delete'#39'       snormal tresword'
      'snormal '#39'do'#39'           snormal tresword'
      'snormal '#39'else'#39'         snormal tresword'
      'snormal '#39'enum'#39'         snormal tresword'
      'snormal '#39'export'#39'       snormal tresword'
      'snormal '#39'extends'#39'      snormal tresword'
      'snormal '#39'false'#39'        snormal tresword'
      'snormal '#39'finally'#39'      snormal tresword'
      'snormal '#39'for'#39'          snormal tresword'
      'snormal '#39'function'#39'     snormal tresword'
      'snormal '#39'if'#39'           snormal tresword'
      'snormal '#39'import'#39'       snormal tresword'
      'snormal '#39'in'#39'           snormal tresword'
      'snormal '#39'new'#39'          snormal tresword'
      'snormal '#39'null'#39'         snormal tresword'
      'snormal '#39'return'#39'       snormal tresword'
      'snormal '#39'super'#39'        snormal tresword'
      'snormal '#39'switch'#39'       snormal tresword'
      'snormal '#39'this'#39'         snormal tresword'
      'snormal '#39'throw'#39'        snormal tresword'
      'snormal '#39'true'#39'         snormal tresword'
      'snormal '#39'try'#39'          snormal tresword'
      'snormal '#39'typeof'#39'       snormal tresword'
      'snormal '#39'var'#39'          snormal tresword'
      'snormal '#39'void'#39'         snormal tresword'
      'snormal '#39'while'#39'        snormal tresword'
      'snormal '#39'with'#39'         snormal tresword'
      ' // numbers'
      'snormal [0-9][0-9]*                            snormal  tinteger'
      'snormal 0X[0-9A-F]+                            snormal  tinteger'
      'snormal [1-9][0-9]*\.[0-9]*                    snormal  tfloat'
      'snormal [1-9][0-9]*{\.[0-9]+}|e{[\+\-]}|[0-9]+ snormal  tfloat'
      '//idents'
      'snormal   [a-z_A-Z][a-z_A-Z0-9]*  snormal   tident'
      ''
      '//white space'
      'snormal #32*                      snormal   twhitespace'
      ''
      ''
      '//strings'
      'snormal    \'#39'                     sstring1   tstring'
      'sstring1   [^\'#39']*                 sstring1   tstring'
      'sstring1   \'#39'                     snormal    tstring'
      'sstring1   \'#39'\'#39'                   sstring1   tstring'
      'sstring1   \\                     sstring1   tstring'
      'sstring1   \\\'#39'                   sstring1   tstring'
      'sstring1   $                      snormal    tstring'
      ''
      '//strings'
      'snormal     \"                    sstring2   tstring'
      'sstring2   [^\"]*                 sstring2   tstring'
      'sstring2   \"                     snormal    tstring'
      'sstring2   \"\"                   sstring2   tstring'
      'sstring2   \\                     sstring2   tstring'
      'sstring2   \\\"                   sstring2   tstring'
      'sstring2   $                      snormal    tstring'
      ''
      ''
      '//comments'
      'snormal   //                      scomment1 tcomment'
      'scomment1 [#1-#255]*              snormal   tcomment'
      'scomment1 $                       snormal   tcomment'
      ''
      'snormal   /\*                     scomment2 tcomment'
      'scomment2 [^\*]*                  scomment2 tcomment'
      'scomment2 \*[^/]                  scomment2 tcomment'
      'scomment2 [\*]*/                  snormal   tcomment')
    Styles = <
      item
        FontColor = clBlack
        Name = 'tnone'
        Description = 'None'
      end
      item
        FontColor = clFuchsia
        Name = 'tstring'
        Description = 'String'
      end
      item
        FontColor = clGray
        FontStyle = [fsItalic]
        Name = 'tcomment'
        Description = 'Comment'
      end
      item
        FontColor = clBlack
        Name = 'tident'
        Description = 'Ident'
      end
      item
        FontColor = clMaroon
        Name = 'tinteger'
        Description = 'Integer'
      end
      item
        FontColor = clMaroon
        Name = 'tfloat'
        Description = 'Float'
      end
      item
        FontColor = clBlack
        FontStyle = [fsBold]
        Name = 'tresword'
        Description = 'Resword'
      end
      item
        FontColor = clGreen
        Name = 'tassembler'
        Description = 'Assembler'
      end
      item
        FontColor = clBlue
        FontStyle = [fsUnderline]
        Name = 'turl'
        Description = 'URL'
      end
      item
        FontColor = clBlack
        Name = 'twhitespace'
        Description = 'White Space'
      end
      item
        FontColor = clHighlightText
        Color = clHighlight
        Name = 'Active Selection'
        Description = 'Active Selection'
      end
      item
        Color = clBtnFace
        Name = 'Inactive Selection'
        Description = 'Inactive Selection'
      end
      item
        FontColor = clLime
        Color = clBlack
        Name = 'Search Match'
        Description = 'Search Match'
      end
      item
        FontColor = clBtnFace
        Color = -1
        Name = 'Disabled Color'
        Description = 'Disabled Color'
      end
      item
        FontColor = clBtnFace
        Color = -1
        Name = 'ReadOnly Color'
        Description = 'ReadOnly Color'
      end>
    ColorMapping = cmDefault
    Left = 464
    Top = 188
  end
  object HtmlParser: TEasyEditorParser
    Rules.Strings = (
      '//Language: HTML'
      '//Copyright (c) 1992-2002 Altium Limited             '
      '//All rights reserved.                               '
      '//http://www.dream-com.com                           '
      '//contact@dream-com.com                              '
      ''
      '// states'
      'State=snormal'
      'State=sstring1'
      'State=sstring2'
      'State=scomment1'
      'State=shtmltag'
      '// tokens'
      'Token=tnone'
      'Token=tstring'
      'Token=tcomment'
      'Token=tident'
      'Token=tinteger'
      'Token=tfloat'
      'Token=tresword'
      'Token=tassembler'
      'Token=turl'
      'Token=twhitespace'
      'Token=tresword1'
      '//delims'
      'Delimiters=;.,:'#39'"{}[]()?!@#$%^&*-+=|\/'
      ''
      ''
      '//reswords'
      'snormal   &lt    snormal tresword1'
      'snormal   &gt    snormal tresword1'
      'snormal   &amp   snormal tresword1       '
      'snormal   &quot  snormal tresword1'
      'snormal   &nbsp  snormal tresword1'
      ''
      '//white space'
      'snormal #32*                      snormal   twhitespace'
      ''
      ''
      '//strings'
      'shtmltag   \'#39'                     sstring1   tstring'
      'sstring1   [^\'#39']*                 sstring1   tstring'
      'sstring1   \'#39'                     shtmltag   tstring'
      'sstring1   \'#39'\'#39'                   sstring1   tstring'
      'sstring1   $                      shtmltag   tstring'
      ''
      '//strings'
      'shtmltag    \"                    sstring2   tstring'
      'sstring2   [^\"]*                 sstring2   tstring'
      'sstring2   \"                     shtmltag   tstring'
      'sstring2   \"\"                   sstring2   tstring'
      'sstring2   $                      shtmltag   tstring'
      ''
      '//Common'
      'snormal <                          shtmltag  tresword'
      'shtmltag [^\>^\'#39'^\"^#32^\=]* shtmltag  tresword'
      'shtmltag >                         snormal   tresword'
      ''
      '//comments'
      ''
      'snormal   <\!--                    scomment1 tcomment'
      'scomment1 [^\-]*                   scomment1 tcomment'
      'scomment1  -->                     snormal   tcomment'
      ''
      '// numbers'
      'shtmltag [0-9][0-9]*              shtmltag  tinteger'
      'shtmltag \#[0-9A-F]+              shtmltag  tinteger'
      'shtmltag [1-9][0-9]*\.[0-9]*                    shtmltag  tfloat'
      
        'shtmltag [1-9][0-9]*{\.[0-9]+}|e{[\+\-]}|[0-9]+ shtmltag   tfloa' +
        't')
    Styles = <
      item
        FontColor = clBlack
        Name = 'tnone'
        Description = 'None'
      end
      item
        FontColor = clMaroon
        Name = 'tstring'
        Description = 'String'
      end
      item
        FontColor = clNavy
        FontStyle = [fsItalic]
        Name = 'tcomment'
        Description = 'Comment'
      end
      item
        FontColor = clBlack
        Name = 'tident'
        Description = 'Ident'
      end
      item
        FontColor = clBlack
        Name = 'tinteger'
        Description = 'Integer'
      end
      item
        FontColor = clBlack
        Name = 'tfloat'
        Description = 'Float'
      end
      item
        FontColor = clBlack
        FontStyle = [fsBold]
        Name = 'tresword'
        Description = 'Resword'
      end
      item
        FontColor = clGreen
        Name = 'tassembler'
        Description = 'Assembler'
      end
      item
        FontColor = clBlue
        FontStyle = [fsUnderline]
        Name = 'turl'
        Description = 'URL'
      end
      item
        FontColor = clBlack
        Name = 'twhitespace'
        Description = 'White Space'
      end
      item
        FontColor = clTeal
        FontStyle = [fsBold, fsItalic]
        Name = 'tresword1'
        Description = 'Resword 1'
      end
      item
        FontColor = clHighlightText
        Color = clHighlight
        Name = 'Active Selection'
        Description = 'Active Selection'
      end
      item
        Color = clBtnFace
        Name = 'Inactive Selection'
        Description = 'Inactive Selection'
      end
      item
        FontColor = clLime
        Color = clBlack
        Name = 'Search Match'
        Description = 'Search Match'
      end
      item
        FontColor = clBtnFace
        Color = -1
        Name = 'Disabled Color'
        Description = 'Disabled Color'
      end
      item
        FontColor = clBtnFace
        Color = -1
        Name = 'ReadOnly Color'
        Description = 'ReadOnly Color'
      end>
    ColorMapping = cmDefault
    Left = 462
    Top = 141
  end
end
