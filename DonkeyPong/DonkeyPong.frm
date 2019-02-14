VERSION 5.00
Begin VB.Form DonkeyPong 
   AutoRedraw      =   -1  'True
   BorderStyle     =   4  'Fixed ToolWindow
   Caption         =   "Donkey Pong"
   ClientHeight    =   5175
   ClientLeft      =   45
   ClientTop       =   210
   ClientWidth     =   11250
   ForeColor       =   &H00FFFFFF&
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   MouseIcon       =   "DonkeyPong.frx":0000
   MousePointer    =   99  'Custom
   ScaleHeight     =   5175
   ScaleWidth      =   11250
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.Timer Timer3 
      Enabled         =   0   'False
      Interval        =   75
      Left            =   735
      Top             =   0
   End
   Begin VB.Timer Timer2 
      Interval        =   1
      Left            =   390
      Top             =   0
   End
   Begin VB.Timer Timer1 
      Enabled         =   0   'False
      Interval        =   50
      Left            =   0
      Top             =   0
   End
   Begin VB.Shape Shape1 
      BackStyle       =   1  'Opaque
      BorderStyle     =   0  'Transparent
      DrawMode        =   7  'Invert
      FillColor       =   &H000080FF&
      Height          =   435
      Left            =   720
      Top             =   1440
      Visible         =   0   'False
      Width           =   420
   End
   Begin VB.Label Label6 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "Label3"
      BeginProperty Font 
         Name            =   "Comic Sans MS"
         Size            =   20.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   570
      Left            =   1440
      TabIndex        =   5
      Top             =   1680
      Visible         =   0   'False
      Width           =   1260
   End
   Begin VB.Label Label5 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "Label3"
      BeginProperty Font 
         Name            =   "Comic Sans MS"
         Size            =   20.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   570
      Left            =   1320
      TabIndex        =   4
      Top             =   2640
      Visible         =   0   'False
      Width           =   1260
   End
   Begin VB.Label Label4 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "Label3"
      BeginProperty Font 
         Name            =   "Comic Sans MS"
         Size            =   20.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   570
      Left            =   1320
      TabIndex        =   3
      Top             =   2160
      Visible         =   0   'False
      Width           =   1260
   End
   Begin VB.Label Label3 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "Label3"
      BeginProperty Font 
         Name            =   "Comic Sans MS"
         Size            =   20.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   570
      Left            =   1260
      TabIndex        =   2
      Top             =   1155
      Visible         =   0   'False
      Width           =   1260
   End
   Begin VB.Image hover 
      Height          =   705
      Left            =   4680
      Picture         =   "DonkeyPong.frx":08CA
      Top             =   2040
      Width           =   2175
   End
   Begin VB.Shape power2 
      FillStyle       =   0  'Solid
      Height          =   270
      Left            =   7665
      Top             =   4830
      Width           =   1485
   End
   Begin VB.Image power1 
      Height          =   270
      Left            =   1590
      Picture         =   "DonkeyPong.frx":1491
      Top             =   4815
      Width           =   1455
   End
   Begin VB.Image power 
      Height          =   270
      Left            =   7665
      Picture         =   "DonkeyPong.frx":1AF5
      Top             =   4830
      Width           =   1455
   End
   Begin VB.Label Label2 
      Alignment       =   1  'Right Justify
      BackColor       =   &H00000000&
      Caption         =   "Label2"
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   7.5
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   9600
      TabIndex        =   1
      Top             =   4800
      Width           =   1455
   End
   Begin VB.Label Label1 
      BackColor       =   &H00000000&
      Caption         =   "Label1"
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   7.5
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   120
      TabIndex        =   0
      Top             =   4800
      Width           =   1350
   End
   Begin VB.Shape Paddle2 
      BorderColor     =   &H00004080&
      FillColor       =   &H002D8AB3&
      FillStyle       =   0  'Solid
      Height          =   1095
      Left            =   10560
      Shape           =   4  'Rounded Rectangle
      Top             =   3720
      Width           =   135
   End
   Begin VB.Shape Paddle1 
      BorderColor     =   &H00004080&
      FillColor       =   &H002D8AB3&
      FillStyle       =   0  'Solid
      Height          =   1095
      Left            =   480
      Shape           =   4  'Rounded Rectangle
      Top             =   3720
      Width           =   135
   End
   Begin VB.Shape Ball 
      BackColor       =   &H00FFFFFF&
      BorderColor     =   &H00000000&
      FillColor       =   &H0000FFFF&
      FillStyle       =   0  'Solid
      Height          =   255
      Left            =   5670
      Shape           =   3  'Circle
      Top             =   2520
      Width           =   255
   End
   Begin VB.Image BackGround 
      Height          =   4770
      Left            =   0
      Picture         =   "DonkeyPong.frx":215B
      Top             =   0
      Width           =   11295
   End
   Begin VB.Shape InfoBoard 
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00000000&
      FillStyle       =   0  'Solid
      Height          =   5235
      Left            =   0
      Top             =   0
      Width           =   11295
   End
End
Attribute VB_Name = "DonkeyPong"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Public counter As Integer

Private Sub Form_KeyDown(KeyCode As Integer, Shift As Integer)
'get any keys that are down
If pause = False Then
    Keys(KeyCode) = True
End If
End Sub

Private Sub Form_KeyUp(KeyCode As Integer, Shift As Integer)
    'get any key releases
    Keys(KeyCode) = False
End Sub
Private Sub Exit_game() 'sub to end each game
    Dim file, pl1wins, pl2wins, pl1lose, pl2lose As Integer
    Dim onewins, onelose, twowins, twolose As String
    Dim filename, class1, class2 As String
    file = FreeFile
    'set up wins losses
    If player1.Points > player2.Points Then
        pl1wins = player1.wins + 1
        pl2lose = player2.losses + 1
    Else
        pl2wins = player2.wins + 1
        pl1lose = player1.losses + 1
    End If
    onewins = Format(p1wins)
    onelose = Format(p1lose)
    twowins = Format(p2wins)
    twolose = Format(p2lose)
    'set up class info
    If player1.wins < 5 Then
        class1 = "Flea Picker"
    ElseIf player1.wins >= 5 And player1.wins < 12 Then
        class1 = "Poo Flinger"
    ElseIf player1.wins >= 12 And player1.wins < 20 Then
        class1 = "Coconut Jockey"
    ElseIf player1.wins >= 20 And player1.wins < 30 Then
        class1 = "Tree Huger"
    ElseIf player1.wins >= 30 And player1.wins < 45 Then
        class1 = "Swinger"
    ElseIf player1.wins >= 45 And player1.wins < 60 Then
        class1 = "Pong Masta"
    ElseIf player1.wins >= 60 And player1.wins < 80 Then
        class1 = "Tech Monkey"
    ElseIf player1.wins >= 80 And player1.wins < 100 Then
        class1 = "King Kong"
    ElseIf player1.wins >= 100 Then
        class1 = "Big Bad Voodoo Daddy"
    End If
    'player2 class set
    If player2.wins < 5 Then
        class2 = "Flea Picker"
    ElseIf player2.wins >= 5 And player2.wins < 12 Then
        class2 = "Poo Flinger"
    ElseIf player2.wins >= 12 And player2.wins < 20 Then
        class2 = "Coconut Jockey"
    ElseIf player2.wins >= 20 And player2.wins < 30 Then
        class2 = "Tree Huger"
    ElseIf player2.wins >= 30 And player2.wins < 45 Then
        class2 = "Swinger"
    ElseIf player2.wins >= 45 And player2.wins < 60 Then
        class2 = "Pong Masta"
    ElseIf player2.wins >= 60 And player2.wins < 80 Then
        class2 = "Tech Monkey"
    ElseIf player2.wins >= 80 And player2.wins < 100 Then
        class2 = "King Kong"
    ElseIf player1.wins >= 100 Then
        class2 = "Big Bad Voodoo Daddy"
    End If
    'write to each file the new info
    filename = App.Path + "/Profiles/" + Trim(player1.name) + ".pro"
    Open filename For Output As file
        Print #file, Trim(player1.name)
        Print #file, Trim(player1.battlecry)
        Print #file, Trim(onewins)
        Print #file, Trim(onelose)
        Print #file, Trim(class1)
        Print #file, Trim(player1.filename)
    Close file
    'player2 info
    filename = App.Path + "/Profiles/" + Trim(player2.name) + ".pro"
    Open filename For Output As file
        Print #file, Trim(player2.name)
        Print #file, Trim(player2.battlecry)
        Print #file, Trim(twowins)
        Print #file, Trim(twolose)
        Print #file, Trim(class2)
        Print #file, Trim(player2.filename)
    Close file
    'reset main vars
    Timer1.Enabled = False
    Timer2.Enabled = True
    DonkeyPong.Hide
    MainWindow.Enabled = True
    MainWindow.Show
    player1.Points = 0
    player2.Points = 0
End Sub
Private Sub message(msg As String)
    Timer2.Enabled = False
    Timer1.Enabled = False
    Randomize
    effect = Int(2 * Rnd) + 1
    If effect = 1 Then
        Label3.Visible = True
        Label4.Visible = True
        Label5.Visible = True
        Label6.Visible = False
        Label3.ForeColor = &HFF0000
        Label4.ForeColor = &HFFFF&
        Label5.ForeColor = &H0
        Label3.Caption = msg
        Label4.Caption = msg
        Label5.Caption = msg
        Label3.Left = 0 - Label3.Width
        Label3.Top = DonkeyPong.Height
        Label4.Left = DonkeyPong.Width
        Label4.Top = DonkeyPong.Height
        Label5.Top = DonkeyPong.Height
        Label5.Left = ((DonkeyPong.Width) / 2) - ((Label5.Width) / 2)
        Shape1.Width = DonkeyPong.Width
        Shape1.Height = DonkeyPong.Height
        Shape1.Left = DonkeyPong.Left - Shape1.Width
    End If
    If effect = 2 Then
        Label3.Visible = True
        Label4.Visible = True
        Label5.Visible = True
        Label6.Visible = True
        Label3.Caption = msg
        Label4.Caption = msg
        Label5.Caption = msg
        Label6.Caption = msg
        Label3.ForeColor = &HFFFFFF
        Label4.ForeColor = &HFFFF&
        Label5.ForeColor = &HFF&
        Label6.ForeColor = &H0
        Label3.Left = DonkeyPong.Width
        Label4.Left = Label3.Left + 100
        Label5.Left = Label4.Left + 100
        Label6.Left = Label5.Left + 100
        Label3.Top = Int(DonkeyPong.Height / 2)
        Label4.Top = Int(DonkeyPong.Height / 2)
        Label5.Top = Int(DonkeyPong.Height / 2)
        Label6.Top = Int(DonkeyPong.Height / 2)
    End If
    Timer3.Enabled = True
    flag = False
End Sub
Private Sub Form_Activate()
    resets = (Val(MainWindow.reset.Text) * 2) + 1
    player1.Points = 0
    player2.Points = 0
    BackGround.Top = 0
    BackGround.Left = 0
    BackGround.Picture = LoadPicture(App.Path + "/levels/" + LevelInfo.levelimage)
    InfoBoard.Top = BackGround.Height
    InfoBoard.Left = 0
    InfoBoard.Width = BackGround.Width
    DonkeyPong.Width = BackGround.Width
    DonkeyPong.Height = BackGround.Height + 725 '(InfoBoard.Height)
    Ball.BorderColor = Val(LevelInfo.BallBorder)
    ballspeed = 120
    power2.Left = power.Left
    power2.Top = power.Top
    power2.Width = power.Width
    power2.Height = power.Height
    power.Top = InfoBoard.Top + 75
    power1.Top = InfoBoard.Top + 75
    power2.Top = InfoBoard.Top + 75
    Label2.Top = InfoBoard.Top + 75
    Label1.Top = InfoBoard.Top + 75
    Paddle1.Top = BackGround.Height - Paddle1.Height
    Paddle1.Left = 500
    Paddle2.Left = BackGround.Width - 500 - Paddle2.Width
    Paddle2.Top = BackGround.Height - Paddle2.Height
    DonkeyPong.Enabled = True
End Sub

Private Sub Form_Load()
    'set up vars for game play
    ballspeed = ballspeed + 10 'make the ball go faster
    counter = 800
    hover.Visible = True
    Timer1.Enabled = False
    Timer2.Enabled = True
    Randomize
    'generate random ball angle
    Dim rand As Integer
    rand = Int(2 * Rnd)
    If rand = 0 Then
        BallDir = Int(40 * Rnd) - 30
    ElseIf rand = 1 Then
        BallDir = Int(40 * Rnd) + 170
    End If
    'generate score board info
    Label1.Caption = Format(player1.name) + ": " + Format(player1.Points)
    Label2.Caption = Format(player2.name) + ": " + Format(player2.Points)
    player1.Jumping = False
    player1.JumpDis = 0
    player1.name = MainWindow.text1.Caption
    player2.Jumping = False
    player2.JumpDis = 0
    player2.name = MainWindow.text2.Caption
    'set the paddles and ball position
    Paddle1.Top = BackGround.Height - Paddle1.Height
    Paddle1.Left = 500
    Paddle2.Left = BackGround.Width - 500 - Paddle2.Width
    Paddle2.Top = BackGround.Height - Paddle2.Height
    Ball.Left = (DonkeyPong.Width / 2) - (Ball.Width / 2)
    Ball.Top = (DonkeyPong.Height / 2)
    hover.Left = (DonkeyPong.Width / 2) - (hover.Width / 2)
    hover.Top = (DonkeyPong.Height / 2) - 500
    'set all keys to depressed
    For i = 1 To 128
        Keys(i) = False
    Next
    'STOP GAME IF MAX RESETS HAS BEEN REACHED~~~~~~~~~~~~~~~~~~~
    If (player1.Points + player2.Points) = resets Then
        Call Exit_game
    End If
End Sub

Private Sub Timer1_Timer()
    '^^^^^^Movement^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^'
    Ball.Left = Round(Ball.Left + (ballspeed * Cos((2 * 3.14 * BallDir) / 360)))
    Ball.Top = Round(Ball.Top - (ballspeed * Sin((2 * 3.14 * BallDir) / 360)))
    'wall check^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^'
    If Ball.Left <= 0 Then
        player2.Points = player2.Points + 1
        Call message(player2.battlecry)
        Call Form_Load
    End If
    If Ball.Left + Ball.Width >= DonkeyPong.Width Then 'Right Wall
        player1.Points = player1.Points + 1
        Call message(player1.battlecry)
        Call Form_Load
    End If
    'ceiling/floor check^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^'
    If Ball.Top <= 50 Then  'Top Wall
        BallDir = 360 + (360 - BallDir)
        Ball.Top = 0
    End If
    If Ball.Top + Ball.Height >= DonkeyPong.Height - (DonkeyPong.Height - BackGround.Height) Then 'Bottom Wall
        Ball.Top = BackGround.Height - Ball.Height
        BallDir = 180 + (180 - BallDir)
    End If
    'paddle check^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^'
    'paddle 1 hit detection
    If boxcheck(Ball.Left, Ball.Top + (Ball.Height / 2), Paddle1.Left - 100, Paddle1.Top, Paddle1.Left + Paddle1.Width, Paddle1.Top + Paddle1.Height) Then
        BallDir = 90 + (90 - BallDir)
        Ball.Left = Paddle1.Left + Paddle1.Width
        If (Ball.Top + Int(Ball.Height / 2)) < Paddle1.Top + (Paddle1.Height / 3) Then
            BallDir = BallDir - 10
        End If
        If (Ball.Top + Int(Ball.Height / 2)) > (Paddle1.Top + Paddle1.Height) - (Paddle1.Height / 3) Then
            BallDir = BallDir + 10
        End If
    End If
    'paddle 2 hit detection
    If boxcheck(Ball.Left + Ball.Width, Ball.Top + (Ball.Height / 2), Paddle2.Left, Paddle2.Top, Paddle2.Left + Paddle2.Width + 100, Paddle2.Top + Paddle2.Height) Then
        BallDir = 90 + (90 - BallDir)
        Ball.Left = Paddle2.Left - Ball.Width
        If (Ball.Top + Int(Ball.Height / 2)) < Paddle2.Top + (Paddle2.Height / 3) Then
            BallDir = BallDir - 10
        End If
        If (Ball.Top + Int(Ball.Height / 2)) > (paddle2top + Paddle2.Height) - (Paddle2.Height / 3) Then
            BallDir = BallDir + 10
        End If
    End If
    
    'P1 Jump set up][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]'
    If Keys(87) = True And player1.Jumping = False Then ' sets jump distance (circle radius)
        If player1.JumpDis < BackGround.Height - Paddle1.Height - 200 Then
            player1.JumpDis = player1.JumpDis + 200
        End If
    End If
    If Keys(87) = False And player1.JumpDis > 0 And player1.Jumping = False Then
        player1.Jumping = True 'when done charging tell let it know to jump
        player1.jumpAngle = 180
        player1.Gravity = (100 - (player1.JumpDis / BackGround.Height * 100)) * 0.09
    End If
    If player1.Jumping = True Then
        Paddle1.Top = BackGround.Height - Paddle1.Height + Round(player1.JumpDis * Sin(((2 * 3.14) / 360) * player1.jumpAngle))
        If player1.jumpAngle < 360 Then
            player1.jumpAngle = player1.jumpAngle + player1.Gravity
        Else
            power1.Width = 0
            player1.JumpDis = 0
            player1.Jumping = False
            Paddle1.Top = BackGround.Height - Paddle1.Height
        End If
    End If
    'P2 jump set up[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]'
    If Keys(79) = True And player2.Jumping = False Then ' sets jump distance (circle radius)
        If player2.JumpDis < BackGround.Height - Paddle2.Height - 200 Then
            player2.JumpDis = player2.JumpDis + 200
        End If
    End If
    If Keys(79) = False And player2.JumpDis > 0 And player2.Jumping = False Then
        player2.Jumping = True 'when done charging tell let it know to jump
        player2.jumpAngle = 180
        player2.Gravity = (100 - (player2.JumpDis / BackGround.Height * 100)) * 0.09
    End If
    If player2.Jumping = True Then
        Paddle2.Top = BackGround.Height - Paddle2.Height + Round(player2.JumpDis * Sin(((2 * 3.14) / 360) * player2.jumpAngle))
        If player2.jumpAngle < 360 Then
            player2.jumpAngle = player2.jumpAngle + player2.Gravity
        Else
            player2.JumpDis = 0
            player2.Jumping = False
            Paddle2.Top = BackGround.Height - Paddle2.Height
        End If
    End If
    
    'misc game play functions[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]'
    If BallDir > 360 Then
        BallDir = BallDir - 360
    End If
    If BallDir < 0 Then
        BallDir = BallDir + 360
    End If
    If Keys(32) = True Then
        Call Form_Load
    End If
    If Keys(27) = True Then
        End
    End If
    If player1.Jumping = False Then
        power1.Width = (player1.JumpDis / (BackGround.Height - Paddle1.Height)) * 1400
    End If
    If player1.Jumping = False Then
        power1.Width = (player1.JumpDis / (BackGround.Height - Paddle1.Height)) * 1400
   End If
   If player2.Jumping = False Then
    power2.Width = power.Width - (player2.JumpDis / (BackGround.Height - Paddle2.Height)) * 1400
   End If
    'ERROR CHECKING[][][][][][][][][][][][][][][][][][][][][][][][][][][]'

End Sub
Private Sub Timer2_Timer()
    'hover ma bob count down set up
    counter = counter - 1
    Select Case counter
        Case 600: hover.Picture = LoadPicture(App.Path + "/photos/hover-ma-bob.gif")
        Case 580: hover.Picture = LoadPicture(App.Path + "/photos/hover-ma-bob-red.gif")
        Case 400: hover.Picture = LoadPicture(App.Path + "/photos/hover-ma-bob.gif")
        Case 380: hover.Picture = LoadPicture(App.Path + "/photos/hover-ma-bob-red.gif")
        Case 200: hover.Picture = LoadPicture(App.Path + "/photos/hover-ma-bob.gif")
        Case 180: hover.Picture = LoadPicture(App.Path + "/photos/hover-ma-bob-red.gif")
        Case 0:  hover.Picture = LoadPicture(App.Path + "/photos/hover-ma-bob-green.gif")
    End Select
    If counter = -200 Then
        Timer2.Enabled = False
        Timer1.Enabled = True
        DonkeyPong.Enabled = True
        hover.Visible = False
        hover.Picture = LoadPicture(App.Path + "/photos/hover-ma-bob-red.gif")
    End If
End Sub


Private Sub Timer3_Timer()
    If effect = 1 Then
        If flag = False Then
            Label3.Left = Label3.Left + 300
            Label3.Top = Label3.Top - 200
            
            Label4.Left = Label4.Left - 300
            Label4.Top = Label4.Top - 200
            
            Label5.Top = Label5.Top - 200
            total = Label3.Top
        End If
        If (Abs(Label3.Top - Label4.Top) < 300) _
        And (Abs(Label3.Top - Label5.Top) < 300) _
        And (Abs(Label3.Left - Label4.Left) < 300) _
        And (Abs(Label3.Left - Label5.Left) < 300) Then
            If (Label3.Top = total) Then
                Shape1.Top = 0
                Shape1.Left = 0
                Shape1.Visible = True
                Label4.Top = Label3.Top
                Label5.Top = Label3.Top
                Label4.Left = Label3.Left
                Label5.Left = Label3.Left
            Else
                Shape1.Visible = False
            End If
            If (Label3.Top < DonkeyPong.Height) Then
            
            flag = True
                Label3.Top = Label3.Top + 225
                Label4.Top = Label4.Top + 225
                Label5.Top = Label5.Top + 225
            End If
        End If
        If (Label3.Top = DonkeyPong.Width) Then
            flag = False
            Timer2.Enabled = True
            Timer3.Enabled = False
        End If
    End If
    If effect = 2 Then
        If Label6.Left > 0 - Label6.Width Then
            Label3.Left = Label3.Left - 350
            Label4.Left = Label4.Left - 300
            Label5.Left = Label5.Left - 250
            Label6.Left = Label6.Left - 200
        Else
            Label3.Left = DonkeyPong.Width
            Label4.Left = Label3.Left + 100
            Label5.Left = Label4.Left + 100
            Label6.Left = Label5.Left + 100
            Timer2.Enabled = True
            Timer3.Enabled = False
        End If
    End If
End Sub
