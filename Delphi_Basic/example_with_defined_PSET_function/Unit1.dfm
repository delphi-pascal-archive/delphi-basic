object Form1: TForm1
  Left = 226
  Top = 115
  BorderStyle = bsDialog
  Caption = 'Delphi Basic'
  ClientHeight = 500
  ClientWidth = 1012
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 590
    Height = 422
    Align = alClient
    TabOrder = 0
    object Memo2: TMemo
      Left = 1
      Top = 1
      Width = 588
      Height = 420
      Align = alClient
      Color = clBlue
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clYellow
      Font.Height = -19
      Font.Name = 'Terminal'
      Font.Style = []
      Lines.Strings = (
        '[basic]'
        ''
        'let mov1 = 0 '
        'let mov2 = 0 '
        'let mov4 = 0 '
        'let mov11 = 0'
        ''
        'for y = 0 to 399'
        ''
        '    let mov11 = mov11 + 1'
        '    let mov1 = mov1 + 9'
        '    let mov2 = mov1'
        '    let mov22 = mov11'
        ''
        '    for x = 0 to 399'
        '        let mov4 = mov4 + 19'
        '        let mov2 = mov2 + 15'
        '        let mov22 = mov22 + 1'
        
          '        let r = abs(sin(mov1/100)+sin(mov2/100)+sin((mov1+mov4)/' +
          '600))*(255/3)'
        '        let g = abs(sin((mov22)/50) + sin(mov11/100))*50'
        '        let b = abs(sin(x*y/100000)*200)'
        '        let a = PSET(x,y,r,g,b)'
        '    endf'
        'endf'
        ''
        '[/basic]')
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 0
    end
  end
  object Panel2: TPanel
    Left = 590
    Top = 0
    Width = 422
    Height = 422
    Align = alRight
    BorderWidth = 10
    Color = clBlack
    TabOrder = 1
    object Image1: TImage
      Left = 11
      Top = 11
      Width = 400
      Height = 400
      Align = alClient
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 422
    Width = 1012
    Height = 78
    Align = alBottom
    TabOrder = 2
    object Run: TButton
      Left = 8
      Top = 8
      Width = 201
      Height = 65
      Caption = 'do it, baby'
      TabOrder = 0
      OnClick = RunClick
    end
    object Memo1: TMemo
      Left = 216
      Top = 1
      Width = 799
      Height = 76
      TabOrder = 1
    end
  end
end
