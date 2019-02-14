VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "clsBitmap"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
'----------------------------------------------------------------------
' Visual Basic Game Programming With DirectX
' Chapter 7 : Break Out The Bitmaps
' Bitmap Class Source Code File
'----------------------------------------------------------------------
Option Explicit
Option Base 0

Private Declare Function LoadImage _
    Lib "user32" Alias "LoadImageA" ( _
    ByVal hInst As Long, _
    ByVal Filename As String, _
    ByVal un1 As Long, _
    ByVal Width As Long, _
    ByVal Height As Long, _
    ByVal opmode As Long) As Long
    
Private Declare Function SetTextColor _
    Lib "gdi32" ( _
    ByVal hdc As Long, _
    ByVal crColor As Long _
) As Long

Private Declare Function TextOutA _
    Lib "gdi32" ( _
    ByVal hdc As Long, _
    ByVal X As Long, _
    ByVal Y As Long, _
    ByVal lpString As String, _
    ByVal nCount As Long _
) As Long

Private Declare Function SetBkMode _
    Lib "gdi32" ( _
    ByVal hdc As Long, _
    ByVal nBkMode As Long _
) As Long

Private Declare Function CreatePen _
    Lib "gdi32" ( _
    ByVal nPenStyle As Long, _
    ByVal nWidth As Long, _
    ByVal crColor As Long _
) As Long

Private Declare Function CreateCompatibleDC _
    Lib "gdi32" (ByVal hdc As Long) As Long
    
Private Declare Function CreateCompatibleBitmap _
    Lib "gdi32" (ByVal hdc As Long, _
    ByVal nWidth As Long, _
    ByVal nHeight As Long) As Long
    
Private Declare Function SelectObject _
    Lib "gdi32" (ByVal hdc As Long, _
    ByVal hObject As Long) As Long

Private Declare Function GetDeviceCaps _
    Lib "gdi32" (ByVal hdc As Long, _
    ByVal nIndex As Long) As Long
    
Private Declare Function GetDesktopWindow _
    Lib "user32" () As Long
    
Private Declare Function GetDC _
    Lib "user32" (ByVal hwnd As Long) As Long
    
Private Declare Function DeleteDC _
    Lib "gdi32" (ByVal hdc As Long) As Long
    
Private Declare Function DeleteObject _
    Lib "gdi32" (ByVal hObject As Long) As Long

Private Declare Function GetObjectW _
    Lib "gdi32" ( _
    ByVal hObject As Long, _
    ByVal nCount As Long, _
    lpObject As Any _
) As Long

Private Declare Function GetObjectA _
    Lib "gdi32" ( _
    ByVal hObject As Long, _
    ByVal nCount As Long, _
    lpObject As Any _
) As Long

Private Declare Function Ellipse _
    Lib "gdi32" ( _
    ByVal hdc As Long, _
    ByVal X1 As Long, _
    ByVal Y1 As Long, _
    ByVal X2 As Long, _
    ByVal Y2 As Long _
) As Long

Private Declare Function Polyline _
    Lib "gdi32" ( _
    ByVal hdc As Long, _
    lpPoint As POINT, _
    ByVal nCount As Long _
) As Long

Private Declare Function SetPixel _
    Lib "gdi32" ( _
    ByVal hdc As Long, _
    ByVal X As Long, _
    ByVal Y As Long, _
    ByVal crColor As Long _
) As Long

Private Declare Function MoveTo _
    Lib "gdi32" Alias "MoveToEx" ( _
    ByVal hdc As Long, _
    ByVal X As Long, _
    ByVal Y As Long, _
    lpPoint As POINT _
) As Long

Private Declare Function LineTo _
    Lib "gdi32" ( _
    ByVal hdc As Long, _
    ByVal X As Long, _
    ByVal Y As Long _
) As Long

Private Declare Function GetVersionEx _
    Lib "kernel32" Alias "GetVersionExA" ( _
    lpVersionInformation As OSVERSIONINFO _
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

Private Type OSVERSIONINFO
    dwOSVersionInfoSize As Long
    dwMajorVersion As Long
    dwMinorVersion As Long
    dwBuildNumber As Long
    dwPlatformId As Long
    szCSDVersion As String * 128
End Type

Private Type BITMAP_STRUCT
    bmType As Long
    bmWidth As Long
    bmHeight As Long
    bmWidthBytes As Long
    bmPlanes As Integer
    bmBitsPixel As Integer
    bmBits As Long
End Type

Private Type POINT
    X As Long
    Y As Long
End Type

Private Const BITSPIXEL = 12
Private Const LR_LOADFROMFILE = &H10

'class variables
Dim hdcBitmap As Long
Dim hBitmap As Long
Dim bmp As BITMAP_STRUCT
Dim pta(4) As POINT
Private pt As POINT
Dim hPen As Long
Dim hOldPen As Long
Dim ret As Long

Private Sub Class_Terminate()
    'delete the bitmap device context
    If hdcBitmap <> 0 Then DeleteDC hdcBitmap
    'delete the bitmap handle
    If hBitmap <> 0 Then DeleteObject hBitmap
End Sub

Public Property Get Width() As Long
    Width = bmp.bmWidth
End Property

Public Property Get Height() As Long
    Height = bmp.bmHeight
End Property

Public Property Get BitsPerPixel() As Long
    BitsPerPixel = bmp.bmBitsPixel
End Property

Public Property Get Planes() As Long
    Planes = bmp.bmPlanes
End Property

Public Property Get TypeNum() As Long
    TypeNum = bmp.bmType
End Property

Public Property Get WidthBytes() As Long
    WidthBytes = bmp.bmWidthBytes
End Property

Public Property Get hdc() As Long
    hdc = hdcBitmap
End Property

Public Property Get hwnd() As Long
    hwnd = hBitmap
End Property

Public Sub Draw(ByVal X As Long, ByVal Y As Long, _
    ByVal hdcDest As Long)
    BitBlt hdcDest, X, Y, Width, Height, hdcBitmap, _
        0, 0, vbSrcCopy
End Sub

Public Sub Blt(ByVal hdcDest As Long, _
    ByVal Left As Long, ByVal Top As Long, _
    ByVal Right As Long, ByVal Bottom As Long)

    BitBlt hdcDest, Left, Top, Right - Left, Bottom - Top, _
        hdcBitmap, Left, Top, vbSrcCopy
End Sub

Public Function Create(ByVal hdcDest As Long, _
    ByVal lWidth As Long, ByVal lHeight As Long) As Boolean
    Create = False
    
    'free up memory in case function is called twice
    If hdcBitmap <> 0 Then DeleteDC hdcBitmap

    'create DC compatible with drawing surface
    hdcBitmap = CreateCompatibleDC(hdcDest)
    If hdcBitmap = 0 Then Exit Function

    'free up memory in case function is called twice
    If hBitmap <> 0 Then DeleteObject hBitmap
    
    'create bitmap compatible with DC
    hBitmap = CreateCompatibleBitmap(hdcDest, lWidth, lHeight)
    If hBitmap = 0 Then Exit Function
    
    'associate the bitmap with the new DC
    SelectObject hdcBitmap, hBitmap
    
    'get bitmap info
    If GetWindowsCore = 1 Then
        'Win9x version
        GetObjectA hBitmap, Len(bmp), bmp
    Else
        'WinNT version
        GetObjectW hBitmap, Len(bmp), bmp
    End If
    
    Create = True
End Function

Public Function Load(ByVal Filename$) As Boolean
    On Error GoTo error1
    Load = False
    
    'get the bitmap handle from the file
    hBitmap = LoadImage(0, Filename, 0, 0, 0, LR_LOADFROMFILE)
    
    'get object based on OS version
    If GetWindowsCore = 1 Then
        'Win9x
        GetObjectA hBitmap, Len(bmp), bmp
    Else
        'WinNT
        GetObjectW hBitmap, Len(bmp), bmp
    End If
    
    'make sure bitmap is valid
    If hBitmap = 0 Or bmp.bmBitsPixel = 0 Then
        Exit Function
    End If

    'create a device context to hold the image
    hdcBitmap = CreateCompatibleDC(0)
    If hdcBitmap = 0 Then
        Exit Function
    End If
    
    'select the bitmap into the device context
    SelectObject hdcBitmap, hBitmap
    
    Load = True
error1:
    Exit Function
End Function

Private Sub LoadBitmapIntoPicture(ByVal Filename As String, _
    ByVal pic As PictureBox)
    Dim wid As Long
    Dim hgt As Long

    'get the desired size in pixels
    wid = pic.ScaleX(pic.ScaleWidth, pic.ScaleMode, vbPixels)
    hgt = pic.ScaleY(pic.ScaleHeight, pic.ScaleMode, vbPixels)

    'get the bitmap handle from the file
    hBitmap = LoadImage(ByVal 0&, Filename, 0, _
        wid, hgt, LR_LOADFROMFILE)

    'create a device context to hold the image
    hdcBitmap = CreateCompatibleDC(0)

    'select the bitmap into the device context
    SelectObject hdcBitmap, hBitmap

    'copy the bitmap into picresult
    BitBlt pic.hdc, 0, 0, wid, hgt, hdcBitmap, 0, 0, vbSrcCopy

    'delete the device context and bitmap
    DeleteDC hdcBitmap
    DeleteObject hBitmap
End Sub

Public Sub DrawText(ByVal Left As Long, ByVal Top As Long, _
    ByVal sText As String, ByVal lColor As Long)
    Dim hPen As Long
    Dim hOldPen As Long

    hPen = CreatePen(0, 1, lColor)
    hOldPen = SelectObject(Me.hdc, hPen)

    SetTextColor hdcBitmap, lColor
    SetBkMode hdcBitmap, 1
    TextOutA hdcBitmap, Left, Top, sText, Len(sText)
    SelectObject hdcBitmap, hOldPen
    DeleteObject hPen
End Sub

Public Sub DrawEllipse(ByVal Left As Long, ByVal Top As Long, _
    ByVal Right As Long, ByVal Bottom As Long, ByVal Color As Long)
    hPen = CreatePen(0, 1, Color)
    hOldPen = SelectObject(hdcBitmap, hPen)
    Ellipse hdcBitmap, Left, Top, Right, Bottom
    SelectObject hdcBitmap, hOldPen
    DeleteObject hPen
End Sub

Public Sub DrawRect(ByVal Left As Long, ByVal Top As Long, _
    ByVal Right As Long, ByVal Bottom As Long, ByVal Color As Long)
    hPen = CreatePen(0, 1, Color)
    hOldPen = SelectObject(Me.hdc, hPen)
    pta(0).X = Left
    pta(0).Y = Top
    pta(1).X = Right
    pta(1).Y = Top
    pta(2).X = Right
    pta(2).Y = Bottom
    pta(3).X = Left
    pta(3).Y = Bottom
    pta(4).X = Left
    pta(4).Y = Top
    Polyline Me.hdc, pta(0), 5
    SelectObject Me.hdc, hOldPen
    DeleteObject hPen
End Sub

Public Sub DrawPoint(ByVal X As Long, ByVal Y As Long, _
    ByVal Color As Long, Optional ByVal hdcDest As Long = 0)
    ret = SetPixel(hdcDest, X, Y, Color)
End Sub

Public Sub DrawLine(ByVal Left As Long, ByVal Top As Long, _
    ByVal Right As Long, ByVal Bottom As Long, _
    ByVal Color As Long, Optional ByVal Width As Long = 1)
    hPen = CreatePen(0, Width, Color)
    hOldPen = SelectObject(Me.hdc, hPen)
    MoveTo Me.hdc, Left, Top, pt
    LineTo Me.hdc, Right, Bottom
    SelectObject Me.hdc, hOldPen
    DeleteObject hPen
End Sub

Public Function GetWindowsCore() As String
    Dim win As OSVERSIONINFO
    Dim ret As Long
    win.dwOSVersionInfoSize = Len(win)
    ret = GetVersionEx&(win)
    GetWindowsCore = win.dwPlatformId
End Function


