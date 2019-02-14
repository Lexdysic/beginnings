VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Begin VB.Form MainWindow 
   BackColor       =   &H007A4112&
   BorderStyle     =   0  'None
   Caption         =   "Donkey Pong"
   ClientHeight    =   5580
   ClientLeft      =   0
   ClientTop       =   150
   ClientWidth     =   9765
   FillColor       =   &H007A4112&
   FillStyle       =   0  'Solid
   LinkMode        =   1  'Source
   MaxButton       =   0   'False
   ScaleHeight     =   5580
   ScaleWidth      =   9765
   StartUpPosition =   2  'CenterScreen
   Begin VB.TextBox reset 
      BackColor       =   &H00854624&
      ForeColor       =   &H00FFFFFF&
      Height          =   285
      Left            =   1560
      TabIndex        =   12
      Text            =   "4"
      Top             =   4680
      Width           =   375
   End
   Begin MSComDlg.CommonDialog dialog 
      Left            =   5040
      Top             =   105
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "Number of Wins:"
      ForeColor       =   &H00FFFFFF&
      Height          =   195
      Left            =   240
      TabIndex        =   13
      Top             =   4680
      Width           =   1185
   End
   Begin VB.Label sop 
      Alignment       =   1  'Right Justify
      BackStyle       =   0  'Transparent
      Caption         =   "U"
      BeginProperty Font 
         Name            =   "Webdings"
         Size            =   18
         Charset         =   2
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   435
      Left            =   105
      TabIndex        =   11
      Top             =   105
      Width           =   330
   End
   Begin VB.Label userinfo2 
      BackStyle       =   0  'Transparent
      BeginProperty Font 
         Name            =   "Comic Sans MS"
         Size            =   11.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   1875
      Left            =   7245
      TabIndex        =   10
      Top             =   2310
      Width           =   2190
   End
   Begin VB.Label userinfo1 
      BackStyle       =   0  'Transparent
      Caption         =   "           "
      BeginProperty Font 
         Name            =   "Comic Sans MS"
         Size            =   11.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   1890
      Left            =   4935
      TabIndex        =   9
      Top             =   2310
      Width           =   2190
   End
   Begin VB.Label levelname 
      BackStyle       =   0  'Transparent
      Caption         =   "The Lab"
      BeginProperty Font 
         Name            =   "Comic Sans MS"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   330
      Left            =   2520
      TabIndex        =   8
      Top             =   5145
      Width           =   1905
   End
   Begin VB.Label text2 
      BackStyle       =   0  'Transparent
      BeginProperty Font 
         Name            =   "Comic Sans MS"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   330
      Left            =   2415
      TabIndex        =   7
      Top             =   3570
      Width           =   1800
   End
   Begin VB.Label text1 
      BackStyle       =   0  'Transparent
      BeginProperty Font 
         Name            =   "Comic Sans MS"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   330
      Left            =   2415
      TabIndex        =   6
      Top             =   3045
      Width           =   1905
   End
   Begin VB.Label close 
      BackStyle       =   0  'Transparent
      Height          =   330
      Left            =   8505
      TabIndex        =   5
      Tag             =   "close"
      Top             =   0
      Width           =   1275
   End
   Begin VB.Label create 
      BackStyle       =   0  'Transparent
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   345
      Left            =   105
      TabIndex        =   4
      Tag             =   "create"
      Top             =   2520
      Width           =   2070
   End
   Begin VB.Label selectp2 
      BackStyle       =   0  'Transparent
      BeginProperty Font 
         Name            =   "Verdana"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   450
      Left            =   105
      TabIndex        =   3
      Tag             =   "play2"
      Top             =   3465
      Width           =   2160
   End
   Begin VB.Label levelbtn 
      BackStyle       =   0  'Transparent
      BeginProperty Font 
         Name            =   "Verdana"
         Size            =   9
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   420
      Left            =   105
      TabIndex        =   2
      Tag             =   "level"
      Top             =   5040
      Width           =   2160
   End
   Begin VB.Label selectp1 
      BackStyle       =   0  'Transparent
      BeginProperty Font 
         Name            =   "Verdana"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   345
      Index           =   1
      Left            =   120
      TabIndex        =   1
      Tag             =   "selectplay1"
      Top             =   3045
      Width           =   2160
   End
   Begin VB.Label playPong 
      BackStyle       =   0  'Transparent
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   14.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   960
      Left            =   4680
      TabIndex        =   0
      Tag             =   "play"
      Top             =   4515
      Width           =   4305
   End
   Begin VB.Image Image1 
      Height          =   5625
      Left            =   0
      Picture         =   "Donkey Pong Main.frx":0000
      Top             =   0
      Width           =   9750
   End
End
Attribute VB_Name = "MainWindow"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub Form_Activate()
    sounder = True
    LevelInfo.levelimage = "lab.jpg"
End Sub

Private Sub resetp1()
    Dim msg As String
    If Text1.Caption <> "" Then
        msg = player1.name + vbCrLf
        msg = msg + "Wins: " + Format(player1.wins) + vbCrLf
        msg = msg + "Losses: " + Format(player1.losses) + vbCrLf
        msg = msg + "Class: " + player1.Class + vbCrLf
        userinfo1.Caption = msg
    End If
End Sub
Private Sub resetp2()
    Dim msg As String
    If Text2.Caption <> "" Then
        msg = player2.name + vbCrLf
        msg = msg + "Wins: " + str(player2.wins) + vbCrLf
        msg = msg + "Losses: " + str(player2.losses) + vbCrLf
        msg = msg + "Class: " + player2.Class + vbCrLf
        userinfo2.Caption = msg
    End If
End Sub


Private Sub playPong_Click()
    'ERROR CHECK``````````````````````````````````
    If Text1.Caption = "" Or Text2.Caption = "" Then
        MsgBox "You must select players.", 48, "Error"
        Exit Sub
    End If
    If Text1.Caption = Text2.Caption Then
        MsgBox "You can't select the same player twice!", 48, "Error"
        Exit Sub
    End If
    'go into pong
    DonkeyPong.Show
End Sub


Private Sub selectp1_Click(Index As Integer)
    Dim file As Integer
    Dim win, loose, msg, name As String
    With dialog
        .DialogTitle = "Select Character"
        .Filter = "Character Profile(*.pro)|*.pro"
        .filename = ""
        .InitDir = App.Path + "\profiles"
        .ShowOpen
        file = FreeFile
        'if cancel was hit exit
        If .filename = "" Then
            Exit Sub
        End If
        'get info from file and sto ing the player info vars
        Open .filename For Input As file
            Line Input #file, player1.name
            Line Input #file, player1.battlecry
            Line Input #file, win
            Line Input #file, loose
            Line Input #file, player1.Class
            Line Input #file, player1.filename
        Close file
        player1.wins = Val(win)
        player1.losses = Val(loose)
        Text1.Caption = player1.name
        resetp1
    End With
End Sub

Private Sub selectp2_Click()
    Dim file As Integer
    Dim win, loose, msg As String
    With dialog
        .DialogTitle = "Select Character"
        .Filter = "Character Profile(*.pro)|*.pro"
        .filename = ""
        .InitDir = App.Path + "\profiles"
        .ShowOpen
        file = FreeFile
        'if cancel was clicked -- exit
        If .filename = "" Then
            Exit Sub
        End If
        'open and save info to game
        Open .filename For Input As file
            Line Input #file, player2.name
            Line Input #file, player2.battlecry
            Line Input #file, win
            Line Input #file, loose
            Line Input #file, player2.Class
            Line Input #file, player2.filename
        Close file
        player1.wins = Val(win)
        player1.losses = Val(loose)
        Text2.Caption = player2.name
        resetp2
    End With
End Sub

Private Sub create_Click()
    CreatePlayer.Show
End Sub

Private Sub close_Click()
    End
End Sub

Private Sub sop_Click() 'mute button
    If sounder = True Then
        sounder = False
        sop.Caption = "V"
    Else
        sounder = True
        sop.Caption = "U"
    End If
End Sub

Private Sub levelbtn_Click() 'select a level
    Dim file As Integer
    With dialog
        .DialogTitle = "Select Level"
        .Filter = "Level(*.lvl)|*.lvl"
        .filename = ""
        .InitDir = App.Path + "\levels"
        .ShowOpen
        file = FreeFile
        'if cancel was hit -- exit
        If .filename = "" Then
            Exit Sub
        End If
        'open up file and saves the stats to the game
        Open .filename For Input As file
            Line Input #file, LevelInfo.levelimage
            Line Input #file, LevelInfo.name
            Line Input #file, LevelInfo.BallColor
            Line Input #file, LevelInfo.BallBorder
        Close file
        levelname.Caption = LevelInfo.name
    End With
End Sub
