object Form5: TForm5
  Left = 0
  Top = 0
  Caption = 'Form5'
  ClientHeight = 318
  ClientWidth = 656
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PopupMenu = PopupMenu1
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 656
    Height = 81
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitLeft = 120
    ExplicitTop = 48
    ExplicitWidth = 265
    object edtServer: TEdit
      Left = 40
      Top = 16
      Width = 211
      Height = 21
      TabOrder = 0
      Text = 'server002.laifuyun.com:443;czw;888888'
    end
    object edtToken: TEdit
      Left = 40
      Top = 43
      Width = 121
      Height = 21
      TabOrder = 1
    end
    object GetToken: TButton
      Left = 176
      Top = 43
      Width = 75
      Height = 25
      Caption = 'GetToken'
      TabOrder = 2
      OnClick = GetTokenClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 81
    Width = 656
    Height = 237
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 8
    ExplicitHeight = 81
    object Splitter1: TSplitter
      Left = 281
      Top = 0
      Width = 5
      Height = 237
    end
    object memReturn: TMemo
      Left = 286
      Top = 0
      Width = 370
      Height = 237
      Align = alClient
      Lines.Strings = (
        'memReturn')
      ScrollBars = ssBoth
      TabOrder = 0
      ExplicitLeft = 473
      ExplicitTop = -8
      ExplicitWidth = 281
    end
    object memSQL: TMemo
      Left = 0
      Top = 0
      Width = 281
      Height = 237
      Align = alLeft
      Lines.Strings = (
        'Memo1')
      ScrollBars = ssBoth
      TabOrder = 1
    end
  end
  object ActionList1: TActionList
    Left = 320
    Top = 168
    object EditCut1: TEditCut
      Category = 'Edit'
      Caption = 'Cu&t'
      Hint = 'Cut|Cuts the selection and puts it on the Clipboard'
      ImageIndex = 0
      ShortCut = 16472
    end
    object EditCopy1: TEditCopy
      Category = 'Edit'
      Caption = '&Copy'
      Hint = 'Copy|Copies the selection and puts it on the Clipboard'
      ImageIndex = 1
      ShortCut = 16451
    end
    object EditPaste1: TEditPaste
      Category = 'Edit'
      Caption = '&Paste'
      Hint = 'Paste|Inserts Clipboard contents'
      ImageIndex = 2
      ShortCut = 16470
    end
    object EditSelectAll1: TEditSelectAll
      Category = 'Edit'
      Caption = 'Select &All'
      Hint = 'Select All|Selects the entire document'
      ShortCut = 16449
    end
    object EditUndo1: TEditUndo
      Category = 'Edit'
      Caption = '&Undo'
      Hint = 'Undo|Reverts the last action'
      ImageIndex = 3
      ShortCut = 16474
    end
    object EditDelete1: TEditDelete
      Category = 'Edit'
      Caption = '&Delete'
      Hint = 'Delete|Erases the selection'
      ImageIndex = 5
      ShortCut = 46
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 360
    Top = 168
    object Copy1: TMenuItem
      Action = EditCopy1
    end
    object Cut1: TMenuItem
      Action = EditCut1
    end
    object Paste1: TMenuItem
      Action = EditPaste1
    end
  end
end
