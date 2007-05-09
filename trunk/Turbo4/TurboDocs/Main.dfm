object Form3: TForm3
  Left = 293
  Top = 110
  Width = 681
  Height = 591
  Caption = 'Form3'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object EasyEdit1: TEasyEdit
    Left = 0
    Top = 21
    Width = 673
    Height = 536
    Cursor = crIBeam
    Align = alClient
    AutoCorrectDelims = ' .,;:'
    BlockSelMode = bsBoth
    CodeTemplates = <
      item
      end>
    CollapseOptions = [coPaintCollapseLines, coPaintCollapsedButtons, coShowCollapsedHints]
    Colors.InActiveSelBackColor = clBtnFace
    DefaultPage.Header.RightText = '\[date]'
    DefaultPage.Header.Font.Charset = DEFAULT_CHARSET
    DefaultPage.Header.Font.Color = clBtnShadow
    DefaultPage.Header.Font.Height = -11
    DefaultPage.Header.Font.Name = 'MS Shell Dlg 2'
    DefaultPage.Header.Font.Style = []
    DefaultPage.Footer.CenterText = 'page \[page] of \[pages]'
    DefaultPage.Footer.Font.Charset = DEFAULT_CHARSET
    DefaultPage.Footer.Font.Color = clBtnShadow
    DefaultPage.Footer.Font.Height = -11
    DefaultPage.Footer.Font.Name = 'MS Shell Dlg 2'
    DefaultPage.Footer.Font.Style = []
    EditSource = EasyEditSource1
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Gutter.Brush.Color = clBtnFace
    LineDivider.Options = []
    LineDivider.Brush.Color = clBtnShadow
    LineBreak = lbCRLF
    LineNumbers.Options = []
    LineStyles = <>
    Margin.Pen.Color = clBtnShadow
    Options = [eoPaintGutter, eoPaintMargin, eoOverwriteBlocks, eoEnableDragging, eoEnableSearchHighlight, eoEnableSelection, eoSelectBeyondEol, eoForceCutCopy, eoLineStyleBeyondEol, eoInvertLineStyle]
    PageNumFont.Charset = DEFAULT_CHARSET
    PageNumFont.Color = clWindowText
    PageNumFont.Height = -11
    PageNumFont.Name = 'MS Shell Dlg 2'
    PageNumFont.Style = []
    PopupWindow.Height = 300
    PopupWindow.Width = 400
    PopupWindow.SizeAble = True
    Ruler.Options = []
    Ruler.Units = erMilimeters
    ScrollBars = ssBoth
    TabOrder = 0
    TabStop = True
    CodeParamChars = '.'
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 673
    Height = 21
    AutoSize = True
    ButtonHeight = 19
    ButtonWidth = 33
    Caption = 'ToolBar1'
    Flat = True
    List = True
    ShowCaptions = True
    TabOrder = 1
    Wrapable = False
    object ToolButton1: TToolButton
      Left = 0
      Top = 0
      Caption = 'Test'
      ImageIndex = 0
      OnClick = ToolButton1Click
    end
  end
  object EasyEditorParser1: TEasyEditorParser
    Rules.Strings = (
      '//Language: Delphi'
      '//Copyright (c) 1992-2002 Altium Limited             '
      '//All rights reserved.                               '
      '//http://www.dream-com.com                           '
      '//contact@dream-com.com                              '
      ''
      '// states'
      'State=snormal'
      'State=sasm'
      'State=sproperty'
      'State=sexternal'
      'State=sstring'
      'State=scomment1'
      'State=scomment2'
      'State=scomment3'
      'State=sasmcomment1'
      'State=sasmcomment2'
      'State=sasmcomment3'
      '// tokens'
      'Token=tNone'
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
      'snormal '#39'absolute'#39'       snormal tresword'
      'snormal '#39'abstract'#39'       snormal tresword'
      'snormal '#39'and'#39'            snormal tresword'
      'snormal '#39'array'#39'          snormal tresword'
      'snormal '#39'as'#39'             snormal tresword'
      '//snormal '#39'asm'#39'            snormal tresword'
      'snormal '#39'assembler'#39'      snormal tresword'
      'snormal '#39'automated'#39'      snormal tresword'
      'snormal '#39'begin'#39'          snormal tresword'
      'snormal '#39'break'#39'          snormal tresword'
      'snormal '#39'case'#39'           snormal tresword'
      'snormal '#39'cdecl'#39'          snormal tresword'
      'snormal '#39'class'#39'          snormal tresword'
      'snormal '#39'const'#39'          snormal tresword'
      'snormal '#39'constructor'#39'    snormal tresword'
      'snormal '#39'continue'#39'       snormal tresword'
      'snormal '#39'default'#39'        snormal tresword'
      'snormal '#39'destructor'#39'     snormal tresword'
      'snormal '#39'dispid'#39'         snormal tresword'
      'snormal '#39'dispinterface'#39'  snormal tresword'
      'snormal '#39'div'#39'            snormal tresword'
      'snormal '#39'do'#39'             snormal tresword'
      'snormal '#39'downto'#39'         snormal tresword'
      'snormal '#39'dynamic'#39'        snormal tresword'
      'snormal '#39'else'#39'           snormal tresword'
      'snormal '#39'end'#39'            snormal tresword'
      'snormal '#39'except'#39'         snormal tresword'
      'snormal '#39'exit'#39'           snormal tresword'
      'snormal '#39'export'#39'         snormal tresword'
      'snormal '#39'exports'#39'        snormal tresword'
      'snormal '#39'external'#39'       snormal tresword'
      'snormal '#39'far'#39'            snormal tresword'
      'snormal '#39'file'#39'           snormal tresword'
      'snormal '#39'finalization'#39'   snormal tresword'
      'snormal '#39'finally'#39'        snormal tresword'
      'snormal '#39'for'#39'            snormal tresword'
      'snormal '#39'forward'#39'        snormal tresword'
      'snormal '#39'function'#39'       snormal tresword'
      'snormal '#39'goto'#39'           snormal tresword'
      'snormal '#39'if'#39'             snormal tresword'
      'snormal '#39'implementation'#39' snormal tresword'
      'snormal '#39'in'#39'             snormal tresword'
      'snormal '#39'index'#39'          snormal tresword'
      'snormal '#39'inherited'#39'      snormal tresword'
      'snormal '#39'initialization'#39' snormal tresword'
      'snormal '#39'inline'#39'         snormal tresword'
      'snormal '#39'interface'#39'      snormal tresword'
      'snormal '#39'is'#39'             snormal tresword'
      'snormal '#39'label'#39'          snormal tresword'
      'snormal '#39'library'#39'        snormal tresword'
      'snormal '#39'message'#39'        snormal tresword'
      'snormal '#39'mod'#39'            snormal tresword'
      'snormal '#39'near'#39'           snormal tresword'
      'snormal '#39'nil'#39'            snormal tresword'
      'snormal '#39'nodefault'#39'      snormal tresword'
      'snormal '#39'not'#39'            snormal tresword'
      'snormal '#39'object'#39'         snormal tresword'
      'snormal '#39'of'#39'             snormal tresword'
      'snormal '#39'or'#39'             snormal tresword'
      'snormal '#39'out'#39'            snormal tresword'
      'snormal '#39'overload'#39'       snormal tresword'
      'snormal '#39'override'#39'       snormal tresword'
      'snormal '#39'packed'#39'         snormal tresword'
      'snormal '#39'pascal'#39'         snormal tresword'
      'snormal '#39'private'#39'        snormal tresword'
      'snormal '#39'procedure'#39'      snormal tresword'
      'snormal '#39'program'#39'        snormal tresword'
      'snormal '#39'property'#39'       snormal tresword'
      'snormal '#39'protected'#39'      snormal tresword'
      'snormal '#39'public'#39'         snormal tresword'
      'snormal '#39'published'#39'      snormal tresword'
      'snormal '#39'raise'#39'          snormal tresword'
      'snormal '#39'read'#39'           snormal tresword'
      'snormal '#39'readonly'#39'       snormal tresword'
      'snormal '#39'record'#39'         snormal tresword'
      'snormal '#39'register'#39'       snormal tresword'
      'snormal '#39'reintroduce'#39'    snormal tresword'
      'snormal '#39'repeat'#39'         snormal tresword'
      'snormal '#39'resident'#39'       snormal tresword'
      'snormal '#39'resourcestring'#39' snormal tresword'
      'snormal '#39'safecall'#39'       snormal tresword'
      'snormal '#39'set'#39'            snormal tresword'
      'snormal '#39'shl'#39'            snormal tresword'
      'snormal '#39'shr'#39'            snormal tresword'
      'snormal '#39'stdcall'#39'        snormal tresword'
      'snormal '#39'stored'#39'         snormal tresword'
      'snormal '#39'string'#39'         snormal tresword'
      'snormal '#39'stringresource'#39' snormal tresword'
      'snormal '#39'then'#39'           snormal tresword'
      'snormal '#39'threadvar'#39'      snormal tresword'
      'snormal '#39'to'#39'             snormal tresword'
      'snormal '#39'try'#39'            snormal tresword'
      'snormal '#39'type'#39'           snormal tresword'
      'snormal '#39'unit'#39'           snormal tresword'
      'snormal '#39'until'#39'          snormal tresword'
      'snormal '#39'uses'#39'           snormal tresword'
      'snormal '#39'var'#39'            snormal tresword'
      'snormal '#39'virtual'#39'        snormal tresword'
      'snormal '#39'while'#39'          snormal tresword'
      'snormal '#39'with'#39'           snormal tresword'
      'snormal '#39'write'#39'          snormal tresword'
      'snormal '#39'writeonly'#39'      snormal tresword'
      'snormal '#39'xor'#39'            snormal tresword'
      ''
      '// numbers'
      'snormal [0-9][0-9]*                            snormal  tinteger'
      'snormal \#[0-9A-F]+                            snormal  tinteger'
      
        'snormal \#\$[0-9A-F]+                            snormal  tinteg' +
        'er'
      'snormal \$[0-9A-F]+                            snormal  tinteger'
      'snormal [1-9][0-9]*\.[0-9]*                    snormal  tfloat'
      'snormal [1-9][0-9]*{\.[0-9]+}|e{[\+\-]}|[0-9]+ snormal  tfloat'
      ''
      '//idents'
      'snormal   [a-z_A-Z][a-z_A-Z0-9]*  snormal   tident'
      '//white space'
      '//snormal #32*                      snormal   twhitespace'
      ''
      '//comments'
      'snormal   //                      scomment1 tcomment '
      'scomment1 [#1-#255]*              snormal   tcomment'
      'scomment1 $                       snormal   tcomment'
      'snormal   \{                      scomment2 tcomment '
      'scomment2 [^\}]*                  scomment2 tcomment'
      'scomment2 \}                      snormal   tcomment'
      ''
      'snormal   (\*                     scomment3 tcomment '
      'scomment3 [^\*]*                  scomment3 tcomment'
      'scomment3 \*[^)]                  scomment3 tcomment'
      'scomment3 [\*]*)                  snormal   tcomment'
      ''
      ''
      '//strings'
      'snormal   \'#39'                      sstring   tstring'
      'sstring   [^\'#39']*                  sstring   tstring'
      'sstring   \'#39'                      snormal   tstring'
      'sstring   \'#39'\'#39'                    sstring   tstring'
      'sstring   $                       snormal   tstring'
      ''
      '// asm'
      'snormal      '#39'asm'#39'                sasm         tresword'
      'sasm         [a-z_A-Z0-9;,@-.]*   sasm         tassembler'
      'sasm         '#39'end'#39'                snormal      tresword'
      'sasm         //                   sasmcomment1 tcomment '
      'sasmcomment1 [#1-#255]*           sasm         tcomment'
      'sasmcomment1 $                    sasm         tcomment'
      'sasm         \{                   sasmcomment2 tcomment '
      'sasmcomment2 [^\}]*               sasmcomment2 tcomment'
      'sasmcomment2 \}                   sasm         tcomment'
      ''
      'sasm         (\*                  sasmcomment3 tcomment '
      'sasmcomment3 [^\*]*               sasmcomment3 tcomment'
      'sasmcomment3 \*[^)]               asmscomment3 tcomment'
      'sasmcomment3 [\*]*)               sasm         tcomment'
      ''
      '// numbers'
      'sasm [0-9][0-9]*                            sasm  tinteger'
      'sasm \$[0-9A-F]+                            sasm  tinteger'
      'sasm [1-9][0-9]*\.[0-9]*                    sasm  tfloat'
      'sasm [1-9][0-9]*{\.[0-9]+}|e{[\+\-]}|[0-9]+ sasm  tfloat'
      ''
      ''
      '// urls'
      'snormal   www\.[a-z_A-Z0-9@\-.]*   snormal turl'
      'snormal   mailto:[a-z_A-Z0-9@\-.]*   snormal turl'
      '')
    Styles = <
      item
        FontColor = clBlack
        Name = 'tNone'
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
    Left = 364
    Top = 56
  end
  object EasyEditSource1: TEasyEditSource
    Options = [srAutoIndent, srBackUnindents, srAllowUndo, srGroupUndo, srBeyondEol, srHighlightUrls]
    Parser = EasyEditorParser1
    TabStops = '2'
    Left = 256
    Top = 56
  end
end
