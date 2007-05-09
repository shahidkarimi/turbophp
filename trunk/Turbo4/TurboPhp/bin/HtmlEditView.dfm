object HtmlEditForm: THtmlEditForm
  Left = 388
  Top = 44
  Width = 662
  Height = 657
  Caption = 'HtmlEditForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Edit: TEasyEdit
    Left = 0
    Top = 0
    Width = 654
    Height = 623
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
    Parser = HtmlParser
    Strings.Strings = (
      
        '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "' +
        'DTD/xhtml1-transitional.dtd">'
      '<html><head>'
      '<style type="text/css"><!--'
      'body {background-color: #ffffff; color: #000000;}'
      'body, td, th, h1, h2 {font-family: sans-serif;}'
      'pre {margin: 0px; font-family: monospace;}'
      '.p {text-align: left;}'
      'img {float: right; border: 0px;}'
      
        'hr {width: 600px; align: center; background-color: #cccccc; bord' +
        'er: 0px; height: 1px;}'
      '//--></style>'
      '<title>phpinfo()</title></head>'
      '<body><div class="center">'
      '<table border="0" cellpadding="3" width="600">'
      '<tr class="h"><td>'
      
        '<a href="http://www.php.net/"><img border="0" src="/phpinfo.php?' +
        '=PHPE9568F34-D428-11d2-A769-00AA001ACF42" alt="PHP Logo" /></a><' +
        'h1 class="p">PHP Version 4.3.2</h1>'
      '</td></tr>'
      '</table><br />'
      '<table border="0" cellpadding="3" width="600">'
      '<tr class="v"><td>'
      
        '<a href="http://www.zend.com/"><img border="0" src="/phpinfo.php' +
        '?=PHPE9568F35-D428-11d2-A769-00AA001ACF42" alt="Zend logo" /></a' +
        '>'
      
        'This program makes use of the Zend Scripting Language Engine:<br' +
        ' />Zend Engine v1.3.0, Copyright (c) 1998-2003 Zend Technologies'
      
        '    with DBG v2.11.26, (C) 2000,2001,2002,2003, by Dmitri Dmitri' +
        'enko'
      '</td></tr>'
      '</table><br />'
      '<hr />'
      
        '<h1><a href="/phpinfo.php?=PHPB8B5F2A0-3C92-11d3-A3A9-4C7B08C100' +
        '00">PHP Credits</a></h1>'
      '<hr />'
      '<h1>Configuration</h1>'
      '<h2>PHP Core</h2>'
      '<table border="0" cellpadding="3" width="600">'
      
        '<tr class="h"><th>Directive</th><th>Local Value</th><th>Master V' +
        'alue</th></tr>'
      
        '<tr><td class="e">arg_separator.input</td><td class="v">&amp;</t' +
        'd><td class="v">&amp;</td></tr>'
      
        '<tr><td class="e">arg_separator.output</td><td class="v">&amp;</' +
        'td><td class="v">&amp;</td></tr>'
      
        '<tr><td class="e">asp_tags</td><td class="v">Off</td><td class="' +
        'v">Off</td></tr>')
    TabStops = '2'
    Left = 432
    Top = 40
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
        FontColor = clBlack
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
        FontColor = 5963867
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
        FontColor = clNavy
        FontStyle = [fsBold]
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
    Left = 432
    Top = 92
  end
end
