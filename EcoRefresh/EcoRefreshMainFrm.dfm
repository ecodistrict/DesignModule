object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Ecodistrict refresh'
  ClientHeight = 567
  ClientWidth = 508
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    508
    567)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 147
    Width = 38
    Height = 13
    Caption = 'Refresh'
  end
  object Label2: TLabel
    Left = 8
    Top = 8
    Width = 79
    Height = 13
    Caption = 'IMB remote host'
  end
  object Label3: TLabel
    Left = 328
    Top = 8
    Width = 20
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'port'
  end
  object labelConnectionStatus: TLabel
    Left = 408
    Top = 8
    Width = 63
    Height = 13
    Caption = 'disconnected'
  end
  object Label4: TLabel
    Left = 8
    Top = 56
    Width = 38
    Height = 13
    Caption = 'Case ID'
  end
  object Label5: TLabel
    Left = 8
    Top = 104
    Width = 36
    Height = 13
    Caption = 'User ID'
  end
  object Label6: TLabel
    Left = 8
    Top = 336
    Width = 47
    Height = 13
    Caption = 'Response'
  end
  object Label7: TLabel
    Left = 8
    Top = 251
    Width = 48
    Height = 13
    Caption = 'Variant ID'
  end
  object btnRefreshDMQueries: TButton
    Left = 8
    Top = 163
    Width = 121
    Height = 25
    Caption = 'DM queries'
    TabOrder = 0
    OnClick = btnRefreshDMQueriesClick
  end
  object btnRefreshDIQueries: TButton
    Left = 8
    Top = 203
    Width = 121
    Height = 25
    Caption = 'DI queries'
    TabOrder = 1
    OnClick = btnRefreshDIQueriesClick
  end
  object btnRefreshDIObjectProperties: TButton
    Left = 152
    Top = 203
    Width = 121
    Height = 25
    Caption = 'DI object properties'
    TabOrder = 2
    OnClick = btnRefreshDIObjectPropertiesClick
  end
  object btnRefreshDIMeasuresHistory: TButton
    Left = 296
    Top = 203
    Width = 121
    Height = 25
    Caption = 'DI measures history'
    TabOrder = 3
    OnClick = btnRefreshDIMeasuresHistoryClick
  end
  object btnRefreshDIData: TButton
    Left = 8
    Top = 297
    Width = 121
    Height = 25
    Caption = 'DI data'
    TabOrder = 4
    OnClick = btnRefreshDIDataClick
  end
  object editIMBRemoteHost: TEdit
    Left = 8
    Top = 24
    Width = 305
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 5
    Text = 'vps17642.public.cloudvps.com'
  end
  object editIMBRemotePort: TEdit
    Left = 328
    Top = 24
    Width = 57
    Height = 21
    Anchors = [akTop, akRight]
    TabOrder = 6
    Text = '4004'
  end
  object btnConnect: TButton
    Left = 408
    Top = 24
    Width = 92
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Connect'
    TabOrder = 7
    OnClick = btnConnectClick
  end
  object cmbCaseId: TComboBox
    Left = 8
    Top = 72
    Width = 492
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ItemIndex = 0
    TabOrder = 8
    Text = '57b428a6cef25a0a0d6681ac; Antwerp main scenario'
    Items.Strings = (
      '57b428a6cef25a0a0d6681ac; Antwerp main scenario'
      '5721de16eae0e0c543f5234f; Warsaw main scenario'
      '4B95BE74F9A44DA0908A30B27C3E8C99; Schiedam'
      'ensel')
  end
  object cmbUserId: TComboBox
    Left = 8
    Top = 120
    Width = 492
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ItemIndex = 0
    TabOrder = 9
    Text = '1234'
    Items.Strings = (
      '1234')
  end
  object memoResponse: TMemo
    Left = 8
    Top = 355
    Width = 492
    Height = 204
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 10
  end
  object cmbVariantId: TComboBox
    Left = 8
    Top = 270
    Width = 492
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ItemIndex = 0
    TabOrder = 11
    Items.Strings = (
      ''
      '57b43118cef25a0a0d6681b1'
      '57b431aecef25a0a0d6681b2'
      '57b431dfcef25a0a0d6681b3')
  end
end
