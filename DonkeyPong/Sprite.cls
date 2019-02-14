VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "clsSprite"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
'----------------------------------------------------------------------
' Visual Basic Game Programming With DirectX
' Chapter 8 : Supersonic Sprites
' Sprite Class Source Code File
'----------------------------------------------------------------------

Option Explicit

'Windows API functions
Private Declare Function IntersectRect _
    Lib "user32" ( _
    lpDestRect As RECT_API, _
    lpSrc1Rect As RECT_API, _
    lpSrc2Rect As RECT_API _
) As Long

Private Declare Sub CopyMemory _
    Lib "kernel32" Alias "RtlMoveMemory" ( _
    lpvDest As Any, _
    lpvSource As Any, _
    ByVal cbCopy As Long _
)

Private Declare Function GetBitmapBits _
    Lib "gdi32" ( _
    ByVal hBitmap As Long, _
    ByVal dwCount As Long, _
    lpBits As Any _
) As Long

Private Declare Function SetBitmapBits _
    Lib "gdi32" ( _
    ByVal hBitmap As Long, _
    ByVal dwCount As Long, _
    lpBits As Any _
) As Long

Private Declare Function BitBlt _
    Lib "gdi32" ( _
    ByVal hDestDC As Long, _
    ByVal X As Long, _
    ByVal Y As Long, _
    ByVal nWidth As Long, _
    ByVal nHeight As Long, _
    ByVal hSrcDC As Long, _
    ByVal xSrc As Long, _
    ByVal ySrc As Long, _
    ByVal dwRop As Long _
) As Long

Private Declare Function GetDeviceCaps _
    Lib "gdi32" ( _
    ByVal hdc As Long, _
    ByVal nIndex As Long _
) As Long

Private Declare Function GetDesktopWindow _
    Lib "user32" ( _
) As Long

Private Declare Function GetDC _
    Lib "user32" ( _
    ByVal hWnd As Long _
) As Long

Private Declare Function SetBkMode _
    Lib "gdi32" ( _
    ByVal hdc As Long, _
    ByVal nBkMode As Long _
) As Long

Private Declare Function SetBkColor _
    Lib "gdi32" ( _
    ByVal hdc As Long, _
    ByVal crColor As Long _
) As Long

'Windows API structs
Private Type RECT_API
    Left As Long
    Top As Long
    Right As Long
    Bottom As Long
End Type

Private Type RGBQUAD
    Blue As Byte
    Green As Byte
    Red As Byte
    alpha As Byte
End Type

'Windows API constants
Private Const BITSPIXEL = 12
Private Const CAPS1 = 94
Private Const C1_TRANSPARENT = &H1
Private Const NEWTRANSPARENT = 3

'internal sprite variables
Dim cbFrames() As clsBitmap
Dim cbMasks() As clsBitmap
Dim cbUnder As New clsBitmap
Dim SourceBits() As Byte
Dim SourceSize As Long
Dim C_WHITE As RGBQUAD
Dim C_BLACK As RGBQUAD
Dim bUseMask As Boolean

'sprite properties
Private bActive As Boolean
Private bTransparent As Boolean
Private bHardwareTransBlit As Boolean
Private bErase As Boolean
Private lState As Long
Private lCurrentX As Long
Private lCurrentY As Long
Private lGlobalX As Long
Private lGlobalY As Long
Private lCurrentFrame As Long
Private lFrameCount As Long
Private lSpeedX As Long
Private lSpeedY As Long
Private lMoveRate As Long
Private lMoveCount As Long
Private lAnimRate As Long
Private lAnimCount As Long
Private lDirX As Long
Private lDirY As Long

Private Sub Class_Initialize()
    bActive = True
    bTransparent = False
    C_WHITE = Color32(255, 255, 255, 0)
    C_BLACK = Color32(0, 0, 0, 0)
    bUseMask = False
    
    'see if device driver supports transparent blitting
    bHardwareTransBlit = (GetDeviceCaps(GetDC(0), CAPS1) _
        And C1_TRANSPARENT)

End Sub

Private Sub Class_Terminate()
    On Local Error Resume Next
    Dim n As Long
    For n = 0 To lFrameCount - 1
        If Not (cbFrames(n) Is Nothing) Then
            Set cbFrames(n) = Nothing
        End If
        If bUseMask Then
            If Not (cbMasks(n) Is Nothing) Then
                Set cbMasks(n) = Nothing
            End If
        End If
    Next n
    ReDim cbFrames(0)
    ReDim cbMasks(0)
    If Not (cbUnder Is Nothing) Then Set cbUnder = Nothing
    ReDim SourceBits(0)
End Sub

Public Property Let X(ByVal lNewValue As Long)
    lCurrentX = lNewValue
End Property

Public Property Get X() As Long
    X = lCurrentX
End Property

Public Property Let Y(ByVal lNewValue As Long)
    lCurrentY = lNewValue
End Property

Public Property Get Y() As Long
    Y = lCurrentY
End Property

Public Property Let GlobalX(ByVal lNewValue As Long)
    lGlobalX = lNewValue
End Property

Public Property Get GlobalX() As Long
    GlobalX = lGlobalX
End Property

Public Property Let GlobalY(ByVal lNewValue As Long)
    lGlobalY = lNewValue
End Property

Public Property Get GlobalY() As Long
    GlobalY = lGlobalY
End Property

Public Property Get SpeedX() As Long
    SpeedX = lSpeedX
End Property

Public Property Let SpeedX(ByVal lNewValue As Long)
    lSpeedX = lNewValue
End Property

Public Property Get SpeedY() As Long
    SpeedY = lSpeedY
End Property

Public Property Let SpeedY(ByVal lNewValue As Long)
    lSpeedY = lNewValue
End Property

Public Property Get DirX() As Long
    DirX = lDirX
End Property

Public Property Let DirX(ByVal lNewValue As Long)
    lDirX = lNewValue
End Property

Public Property Get DirY() As Long
    DirY = lDirY
End Property

Public Property Let DirY(ByVal lNewValue As Long)
    lDirY = lNewValue
End Property

Public Property Get Width() As Long
    Width = cbFrames(lCurrentFrame).Width
End Property

Public Property Get Height() As Long
    Height = cbFrames(lCurrentFrame).Height
End Property

Public Property Get Active() As Boolean
    Active = bActive
End Property

Public Property Let Active(ByVal bNewValue As Boolean)
    bActive = bNewValue
End Property

Private Property Get ColorDepth() As Long
    ColorDepth = GetDeviceCaps(GetDC(GetDesktopWindow), BITSPIXEL)
End Property

Public Property Get CurrentFrame() As Long
    CurrentFrame = lCurrentFrame
End Property

Public Property Let CurrentFrame(ByVal lNewValue As Long)
    If lNewValue >= 0 And lNewValue < lFrameCount Then
        lCurrentFrame = lNewValue
    End If
End Property

Public Property Get hdc(Optional ByVal lFrame _
    As Long = -1) As Long
    If lFrame < 0 Then lFrame = lCurrentFrame
    hdc = cbFrames(lFrame).hdc
End Property

Public Property Get MaskDC(Optional ByVal lFrame _
    As Long = -1) As Long
    If lFrame < 0 Then lFrame = lCurrentFrame
    MaskDC = cbMasks(lFrame).hdc
End Property

Public Property Get Transparent() As Boolean
    Transparent = bTransparent
End Property

Public Property Let Transparent(ByVal bNewValue As Boolean)
    bTransparent = bNewValue
End Property

Public Property Get HardwareTransparency() As Boolean
    HardwareTransparency = bHardwareTransBlit
End Property

Public Property Let State(ByVal lNewValue As Long)
    lState = lNewValue
End Property

Public Property Get State() As Long
    State = lState
End Property

Public Property Let MoveRate(ByVal lNewValue As Long)
    lMoveRate = lNewValue
End Property

Public Property Get MoveRate() As Long
    MoveRate = lMoveRate
End Property

Public Property Let MoveCount(ByVal lNewValue As Long)
    lMoveCount = lNewValue
End Property

Public Property Get MoveCount() As Long
    MoveCount = lMoveCount
End Property

Public Property Let AnimRate(ByVal lNewValue As Long)
    lAnimRate = lNewValue
End Property

Public Property Get AnimRate() As Long
    AnimRate = lAnimRate
End Property

Public Property Let AnimCount(ByVal lNewValue As Long)
    lAnimCount = lNewValue
End Property

Public Property Get AnimCount() As Long
    AnimCount = lAnimCount
End Property

Public Sub NextFrame()
    If lAnimCount < lAnimRate Then
        lAnimCount = lAnimCount + 1
    Else
        lAnimCount = 0
        If lCurrentFrame < lFrameCount - 1 Then
            lCurrentFrame = lCurrentFrame + 1
        Else
            lCurrentFrame = 0
        End If
    End If
End Sub

Public Sub PrevFrame()
    If lAnimCount < lAnimRate Then
        lAnimCount = lAnimCount + 1
    Else
        lAnimCount = 0
        If lCurrentFrame > 0 Then
            lCurrentFrame = lCurrentFrame - 1
        Else
            lCurrentFrame = lFrameCount
        End If
    End If
End Sub

Public Sub SaveUnder(ByVal hdcDest As Long)
    If cbFrames(lCurrentFrame).hdc = 0 Or hdcDest = 0 Then Exit Sub
    If cbUnder.hdc = 0 Then
        If Not cbUnder.Create(hdcDest, Width, Height) Then Exit Sub
    End If
    BitBlt cbUnder.hdc, 0, 0, Width, Height, hdcDest, X, Y, vbSrcCopy
End Sub

Public Sub RestoreUnder(ByVal hdcDest As Long)
    If cbUnder.hdc = 0 Then Exit Sub
    BitBlt hdcDest, X, Y, Width, Height, cbUnder.hdc, 0, 0, vbSrcCopy
End Sub

Public Function LoadFrames(ByVal sFilename As String, _
    ByVal lStartX As Long, ByVal lStartY As Long, _
    ByVal lWidth As Long, ByVal lHeight As Long, _
    ByVal lNumFrames As Long, _
    Optional bGenMasks As Boolean = True) As Boolean

    On Error GoTo error_out
    Dim cbSource As New clsBitmap
    Dim lFrame As Long
    Dim lCopyX As Long
    Dim lCopyY As Long
    Dim bFailed As Boolean
    
    LoadFrames = False
    
    'load the source bitmap file
    If Not cbSource.Load(sFilename) Then Exit Function
        
    'copy each frame out of the source image
    For lFrame = 0 To lNumFrames - 1
        'calculate the upper left corner
        'lCopyX = lStartX + lFrame * (lWidth + 1)
        lCopyX = lStartX + lFrame * lWidth
        lCopyY = lStartY
        'set the current frame
        lCurrentFrame = lFrameCount + lFrame
        'redimension the bitmap array to hold the new frame
        ReDim Preserve cbFrames(0 To lCurrentFrame)
        'instantiate the new sprite frame
        Set cbFrames(lCurrentFrame) = New clsBitmap
        'create the bitmap image based on sprite dimensions
        If Not cbFrames(lCurrentFrame).Create(cbSource.hdc, _
            lWidth, lHeight) Then
            LoadFrames = False
            Exit For
        End If
        'transfer frame from source image to sprite array
        If BitBlt(cbFrames(lCurrentFrame).hdc, 0, 0, _
            lWidth, lHeight, cbSource.hdc, lCopyX, lCopyY, _
            vbSrcCopy) = 0 Then
            LoadFrames = False
            Exit For
        End If
    Next lFrame
    
    'increment the frame count
    lFrameCount = lFrameCount + lNumFrames
    
    'generate masks
    If bGenMasks Then
        'only gen masks if hardware support is false
        If Not HardwareTransparency Then
            If Not GenerateAllMasks() Then Exit Function
        End If
    End If
    
    'successful operation
    Set cbSource = Nothing
    LoadFrames = True
    Exit Function
error_out:
End Function

Public Sub Draw(ByVal hdcDest As Long)
    Dim lOldColor As Long
    Dim lOldMode As Long
    
    'make sure current frame is valid
    If cbFrames(lCurrentFrame).hdc < 1 Then Exit Sub
    
    'draw sprite with transparency?
    If Transparent Then
        'check for hardware transparent blitter
        If HardwareTransparency Then
            'configure for hardware transparent blitting
            lOldMode = SetBkMode(hdcDest, NEWTRANSPARENT)
            lOldColor = SetBkColor(hdcDest, RGB(0, 0, 0))
            
            'draw image using hardware transparent blit
            BitBlt hdcDest, X, Y, Width, Height, _
                cbFrames(lCurrentFrame).hdc, 0, 0, vbSrcCopy
            
            'restore background mode
            SetBkColor hdcDest, lOldColor
            SetBkMode hdcDest, lOldMode
        Else
            'blit the mask using AND
            BitBlt hdcDest, X, Y, Width, Height, _
                cbMasks(lCurrentFrame).hdc, 0, 0, vbSrcAnd
            'blit the image using XOR
            BitBlt hdcDest, X, Y, Width, Height, _
                cbFrames(lCurrentFrame).hdc, 0, 0, vbSrcPaint
        End If
    Else
        'blit the image without transparency
        BitBlt hdcDest, X, Y, Width, Height, _
            cbFrames(lCurrentFrame).hdc, 0, 0, vbSrcCopy
    End If
End Sub

Public Function GenerateAllMasks() As Boolean
    On Error GoTo error1
    Dim lFrame As Long
    GenerateAllMasks = False
    For lFrame = 0 To lFrameCount - 1
        If Not (cbFrames(lFrame) Is Nothing) Then
            If cbFrames(lFrame).hdc <> 0 Then
                GenerateMask lFrame
            End If
        End If
    Next lFrame
    GenerateAllMasks = True
error1:
End Function

Public Function GenerateMask(ByVal lFrame As Long) As Boolean
    Dim rows As Long
    Dim cols As Long
    Dim srcIndex As Long
    Dim Index As Long
    Dim bNumBytes As Byte

    GenerateMask = False
    'make sure the bitmap array is not empty
    If lFrame < 0 Or lFrame > lFrameCount Then Exit Function
    'create the mask array
    ReDim Preserve cbMasks(0 To lFrameCount)
    'instantiate the mask frame
    Set cbMasks(lFrame) = Nothing
    Set cbMasks(lFrame) = New clsBitmap

    'make sure the source bitmap was already loaded
    If cbFrames(lFrame).hdc = 0 Then Exit Function
    If cbMasks(lFrame).hdc = 0 Then
        'create the mask bitmap
        If Not cbMasks(lFrame).Create(cbFrames(lFrame).hdc, _
            Width, Height) Then
            Exit Function
        End If
    End If
    'copy the source frame into the mask image
    BitBlt cbMasks(lFrame).hdc, 0, 0, Width, Height, _
        cbFrames(lFrame).hdc, 0, 0, vbSrcCopy
    'determine the size of the bitmap
    SourceSize = cbFrames(lFrame).WidthBytes * _
        cbFrames(lFrame).Height
    'create the bitmap bits array
    ReDim SourceBits(0 To SourceSize)
    'copy the source image into the array
    GetBitmapBits cbMasks(lFrame).hWnd, SourceSize, SourceBits(0)

    'determine byte increment (32-bit images work best)
    bNumBytes = ColorDepth \ 8
    Index = 0
    'process each pixel in the source image
    For cols = 0 To cbFrames(lFrame).Height - 1
        For rows = 0 To cbFrames(lFrame).Width - 1
            'set mask pixel based on source pixel
            If SourceBits(Index) = 0 And _
                SourceBits(Index + 1) = 0 And _
                SourceBits(Index + 2) = 0 Then
                CopyMemory SourceBits(Index), C_WHITE, bNumBytes
            Else
                CopyMemory SourceBits(Index), C_BLACK, bNumBytes
            End If
            Index = Index + bNumBytes
        Next rows
    Next cols
    'copy array back into source image
    SetBitmapBits cbMasks(lFrame).hWnd, SourceSize, SourceBits(0)
    bUseMask = True
    GenerateMask = True
End Function

Private Function Color32(ByVal bRed As Byte, ByVal bGreen As Byte, _
    ByVal bBlue As Byte, ByVal bAlpha As Byte) As RGBQUAD
    Color32.Blue = bBlue
    Color32.Green = bGreen
    Color32.Red = bRed
    Color32.alpha = bAlpha
End Function

Public Function Collided(ByRef OtherSprite As clsSprite) As Boolean
    Dim r1 As RECT_API
    Dim r2 As RECT_API
    Dim r3 As RECT_API
    
    'set up primary rect
    r1.Left = Me.X
    r1.Top = Me.Y
    r1.Right = r1.Left + Me.Width
    r1.Bottom = r1.Top + Me.Height
    'set up secondary rect
    r2.Left = OtherSprite.X
    r2.Top = OtherSprite.Y
    r2.Right = r2.Left + OtherSprite.Width
    r2.Bottom = r2.Top + OtherSprite.Height
    'check for collision
    If IntersectRect(r3, r1, r2) Then
        Collided = True
    Else
        Collided = False
    End If
End Function


