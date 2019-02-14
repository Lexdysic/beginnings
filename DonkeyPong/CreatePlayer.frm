VERSION 5.00
Begin VB.Form CreatePlayer 
   BackColor       =   &H00854624&
   BorderStyle     =   0  'None
   Caption         =   "Create Player"
   ClientHeight    =   1365
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   5250
   ForeColor       =   &H00FFFFFF&
   LinkTopic       =   "Form1"
   ScaleHeight     =   1365
   ScaleWidth      =   5250
   ShowInTaskbar   =   0   'False
   StartUpPosition =   1  'CenterOwner
   Begin VB.TextBox Text2 
      BackColor       =   &H00854624&
      ForeColor       =   &H00FFFFFF&
      Height          =   315
      Left            =   1365
      TabIndex        =   1
      Top             =   735
      Width           =   1965
   End
   Begin VB.TextBox Text1 
      BackColor       =   &H00854624&
      ForeColor       =   &H00FFFFFF&
      Height          =   300
      Left            =   1365
      TabIndex        =   0
      Top             =   315
      Width           =   1980
   End
   Begin VB.Label command2 
      BackStyle       =   0  'Transparent
      Height          =   225
      Left            =   3570
      TabIndex        =   5
      Top             =   1155
      Width           =   855
   End
   Begin VB.Label command1 
      BackStyle       =   0  'Transparent
      Height          =   225
      Left            =   4515
      TabIndex        =   4
      Top             =   1155
      Width           =   750
   End
   Begin VB.Image Image1 
      Height          =   1350
      Left            =   0
      Picture         =   "CreatePlayer.frx":0000
      Top             =   0
      Width           =   5250
   End
   Begin VB.Label Label2 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "Battle Cry"
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   9
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   210
      Left            =   3210
      TabIndex        =   3
      Top             =   270
      Width           =   915
   End
   Begin VB.Label Label1 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "Name   "
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   9
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   210
      Left            =   240
      TabIndex        =   2
      Top             =   255
      Width           =   630
   End
End
Attribute VB_Name = "CreatePlayer"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Private Sub Command1_Click() 'Close the win w/o doing anything
    CreatePlayer.Hide
    MainWindow.Show
    MainWindow.Enabled = True
    text1.Text = ""
    text2.Text = ""
End Sub

Private Sub Command2_Click() 'create a new player then close the win
    Dim file, reply As Integer
    Dim filename As String
    Dim textt1 As String
    Dim textt2 As String
    textt1 = text1.Text
    textt2 = text2.Text
    filename = App.Path + "/Profiles/" + textt1 + ".pro"
    file = FreeFile
    '`````````````ERROR CHECK```````````````````````````````
    If text1.Text = "" Or text2.Text = "" Then
        MsgBox "You must fill in all information.", 48, "Error"
        CreatePlayer.Show
        Exit Sub
    End If
    If Dir(filename) = "" Then
        'do nothing
    Else
        reply = MsgBox("That file already exsists, Would you like to write over over it?", vbYesNo, "Error")
        If reply = vbNo Then
            text1.Text = ""
            text2.Text = ""
            Exit Sub
            CreatePlayer.Show
        End If
    End If
    '```````````END ERROR CHECK````````````````````````````
    'write to the file the default info
    Open filename For Output As file
        Print #file, textt1         'name
        Print #file, textt2         'battle cry
        Print #file, "0"            'wins
        Print #file, "0"            'losses
        Print #file, "Flea Picker"  'class
        Print #file, filename       'file directory
    Close file
    Call Command1_Click
End Sub

Private Sub Form_Activate()
    Call Form_Load
End Sub

Private Sub Form_Load()
    MainWindow.Enabled = False
End Sub


