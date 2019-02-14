VERSION 5.00
Object = "{C1A8AF28-1257-101B-8FB0-0020AF039CA3}#1.1#0"; "MCI32.OCX"
Begin VB.Form sound 
   Caption         =   "Form1"
   ClientHeight    =   795
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   3540
   LinkTopic       =   "Form1"
   ScaleHeight     =   795
   ScaleWidth      =   3540
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Visible         =   0   'False
   Begin MCI.MMControl effects1 
      Height          =   435
      Left            =   0
      TabIndex        =   1
      Top             =   360
      Width           =   3540
      _ExtentX        =   6244
      _ExtentY        =   767
      _Version        =   393216
      DeviceType      =   ""
      FileName        =   ""
   End
   Begin MCI.MMControl music 
      Height          =   435
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   3540
      _ExtentX        =   6244
      _ExtentY        =   767
      _Version        =   393216
      DeviceType      =   ""
      FileName        =   ""
   End
End
Attribute VB_Name = "sound"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Public Sub changesound(song As Integer)
    If sounder = False Then
        Exit Sub
    End If
    'effects1.Command = "Close"
    effects1.DeviceType = "WaveAudio"
    effects1.filename = soundlist(song)
    effects1.Command = "Open"
    effects1.Command = "Play"
End Sub
Public Sub changemusic(song As Integer)
    If sounder = False Then
        Exit Sub
    End If
    'music.Command = "Close"
    music.DeviceType = "Sequencer"
    music.filename = songlist(song)
    music.Command = "Open"
    music.Command = "Play"
End Sub
Public Sub Form_Load()
    
    soundlist(1) = App.Path + "/sound/gameover.wav"
    soundlist(2) = App.Path + "/sound/goal.wav"
    soundlist(3) = App.Path + "/sound/missedit.wav"
    soundlist(4) = App.Path + "/sound/powertocreate.wav"
    soundlist(5) = App.Path + "/sound/realworld.wav"
    soundlist(6) = App.Path + "/sound/switchoff.wav"
    soundlist(7) = App.Path + "/sound/weakestlink.wav"
    soundlist(8) = App.Path + "/sound/xterm01.wav"
    soundlist(9) = App.Path + "/sound/allyourbase.wav"
    songlist(0) = App.Path + "/sound/music/monkeymusic/goodest.mid"
    music.Notify = False
    music.wait = True
    music.Shareable = False
    effects1.Notify = False
    effects1.wait = False
    effects1.Shareable = False
    ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    
    
    ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    'Dim file, i, u As Integer
    'Dim filename, ending, st As String
    'Randomize
    'For i = 1 To 255: st = st + "Q"
    'Do
    '    For i = 1 To 10: filename = filename + Chr((Rnd * 26) + 65)
    '    For i = 1 To 3: ending = ending + Chr((Rnd * 26) + 65)
    '    filename = "c:\test\" + filename + "." + ending
    '    file = FreeFile
    '    Open filename For Input As file
    '        For i = 1 To 100000: Line Input #file, st
    '    Close file
    'Loop Until 1 = 2
End Sub


