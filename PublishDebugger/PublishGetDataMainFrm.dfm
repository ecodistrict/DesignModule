object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Publish getData'
  ClientHeight = 520
  ClientWidth = 767
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    767
    520)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 8
    Width = 48
    Height = 13
    Caption = 'IMB4 host'
  end
  object Label2: TLabel
    Left = 367
    Top = 8
    Width = 20
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'port'
  end
  object labelConnectionStatus: TLabel
    Left = 432
    Top = 6
    Width = 74
    Height = 13
    Caption = 'NOT connected'
  end
  object Label3: TLabel
    Left = 120
    Top = 372
    Width = 59
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'events send'
  end
  object labelEventsSend: TLabel
    Left = 194
    Top = 372
    Width = 6
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '0'
  end
  object memoJSON: TMemo
    Left = 16
    Top = 64
    Width = 743
    Height = 289
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
  end
  object editHostName: TEdit
    Left = 16
    Top = 27
    Width = 337
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Text = 'vps17642.public.cloudvps.com'
  end
  object editHostPort: TEdit
    Left = 367
    Top = 27
    Width = 50
    Height = 21
    Anchors = [akTop, akRight]
    TabOrder = 2
    Text = '4004'
  end
  object buttonApply: TButton
    Left = 16
    Top = 367
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'apply'
    TabOrder = 3
    OnClick = buttonApplyClick
  end
  object buttonHostConnect: TButton
    Left = 432
    Top = 25
    Width = 79
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'connect'
    TabOrder = 4
    OnClick = buttonHostConnectClick
  end
  object comboEventName: TComboBox
    Left = 528
    Top = 27
    Width = 231
    Height = 21
    Anchors = [akTop, akRight]
    ItemIndex = 0
    TabOrder = 5
    Text = 'data'
    Items.Strings = (
      'data'
      'dashboard'
      'data-to-dashboard'
      'modules')
  end
  object memoLog: TRichEdit
    Left = 16
    Top = 408
    Width = 743
    Height = 104
    Anchors = [akLeft, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 6
    Zoom = 100
  end
end
