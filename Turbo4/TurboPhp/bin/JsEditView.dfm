object JsEditForm: TJsEditForm
  Left = 363
  Top = 402
  Width = 575
  Height = 499
  Caption = 'JsEditForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object dfsSplitter1: TdfsSplitter
    Left = 0
    Top = 0
    Width = 8
    Height = 465
    Cursor = crDefault
    Align = alLeft
    Color = 15531519
    MinSize = 1
    ParentColor = False
    Maximized = False
    Minimized = False
    ButtonCursor = crDefault
    ButtonWidth = 50
    ButtonHighlightColor = clBtnFace
  end
  object Edit: TEasyEdit
    Left = 8
    Top = 0
    Width = 559
    Height = 465
    Cursor = crIBeam
    Align = alClient
    AutoCorrectDelims = ' .,;:'
    BlockSelMode = bsBoth
    CodeTemplates = <>
    CollapseOptions = [coCollapseButtonOnGutter, coPaintCollapseLines, coPaintCollapsedButtons, coShowCollapsedHints]
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
    LineDivider.Options = []
    LineDivider.Brush.Color = clBtnShadow
    LineBreak = lbCRLF
    LineNumbers.Options = [lnPaintOnGutter]
    LineNumbers.Visible = True
    LineNumbers.Alignment = lnRight
    LineStyles = <>
    Margin.Pen.Color = clBtnShadow
    Options = [eoPaintGutter, eoPaintMargin, eoOverwriteBlocks, eoEnableDragging, eoEnableSearchHighlight, eoEnableSelection, eoSelectBeyondEol, eoForceCutCopy, eoLineStyleBeyondEol, eoInvertLineStyle, eoThumbTracking, eoTripleClickLine]
    PageNumFont.Charset = DEFAULT_CHARSET
    PageNumFont.Color = clWindowText
    PageNumFont.Height = -11
    PageNumFont.Name = 'MS Sans Serif'
    PageNumFont.Style = []
    PopupWindow.Height = 300
    PopupWindow.Width = 400
    PopupWindow.SizeAble = True
    Ruler.Options = []
    Ruler.Units = erMilimeters
    ScrollBars = ssBoth
    TabOrder = 0
    TabStop = True
    WantTabs = True
    CodeParamChars = '.'
    OnSourceChanged = EditSourceChanged
  end
  object Source: TEasyEditSource
    Options = [srBackUnindents, srUseTab, srCursorThroughTabs, srOptimalFill, srAllowUndo, srGroupUndo, srBeyondEol, srHighlightUrls, srLeaveTabs]
    Parser = JsParser
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
    Left = 56
    Top = 88
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
    Left = 56
    Top = 144
  end
  object ChangeTimer: TTimer
    Enabled = False
    Interval = 5000
    OnTimer = ChangeTimerTimer
    Left = 56
    Top = 36
  end
end
