Attribute VB_Name = "Module1"
Type Player
    name As String * 16
    wins As Integer
    losses As Integer
    battlecry As String
    Class As String
    Points As Integer
    jumpAngle As Double
    JumpDis As Long
    Gravity As Double
    Jumping As Boolean
    filename As String
End Type
Type levelInformation
    levelimage As String
    name As String
    BallColor As String
    BallBorder As String
End Type
Public powerups As powerdrop
Type powerdrop
    wait As Integer
    current As Integer
    type As String
    speed As Byte
    who As String
End Type
Public BallDir As Integer   'ball info
Public ballspeed As Integer
Public resets As Integer
Public Keys(256) As Boolean 'key pressing
Public player1 As Player
Public player2 As Player ' player information
Public sounder As Boolean 'sound boolean
Public flag As Boolean
Public effect As Byte
Public LevelInfo As levelInformation
Declare Function SetDoubleClickTime Lib "user32" (ByVal wCount As Long) As Long
Public soundlist(10), songlist(1) As String

Public Function boxcheck(X, Y, X1, Y1, X2, Y2 As Integer) As Boolean
    If X > X1 And X < X2 And Y > Y1 And Y < Y2 Then
        boxcheck = True
    Else
        boxcheck = False
    End If
End Function

Public Function reflect(base As Integer, ang As Integer) As Integer
        reflect = base + (base - ang)
End Function

Public Function msg(str As String)
    errorwin.Show
    errorwin.Label1.Caption = str
End Function



