Public Module String_Validation
    Public Function CheckExists(text As String)
        If text = Nothing Then
            Return False
        Else
            Return True
        End If
    End Function
    Public Function ValidEmail(text As String)
        Dim array() As Char = StringIntoArray(text)
        Dim Atsymbol As Integer
        Dim atsymbolPlace As Integer
        Dim FullStop As Integer
        For i = 0 To text.Length - 1
            If array(i) = "@" Then
                Atsymbol = 1
                atsymbolPlace = i
            End If
            If array(i) = "." And i > atsymbolPlace Then
                FullStop = 1
            End If
        Next
        If FullStop = 1 And Atsymbol = 1 Then
            Return True
        End If
        Return False
    End Function
    Public Function StringIntoArray(Text As String)
        Dim array(Text.Length) As Char
        For i = 1 To Text.Length
            array(i - 1) = Text.Substring(i, 1)
        Next
        Return array
    End Function
    Public Function StringEquals(TextIn As String, TextCheck As String)
        If TextIn = TextCheck Then
            Return True
        Else
            Return False
        End If
    End Function
    Public Function PasswordStrength(Password)
        Dim strength As Integer
        If ValidPassword(Password) = True Then
            If Password.length >= 16 Then strength = strength + 1
            If Password.Any(Function(c) Char.IsSymbol(c)) Then strength = strength + 1
            If strength = 1 Then Return "Strong"
            If strength = 2 Then Return "Very Strong"
            If strength = 0 Then Return "Weak"
        Else Return "Invalid"
        End If
    End Function
    Private Function ValidPassword(Password As String) As Boolean
        If Password.Length < 8 Then Return False
        If Not Password.Any(Function(c) Char.IsDigit(c)) Then Return False
        If Not Password.Any(Function(c) Char.IsLower(c)) Then Return False
        If Not Password.Any(Function(c) Char.IsUpper(c)) Then Return False
        Return True
    End Function
End Module
Public Module MathFunc
    Public Function randomnumber(UBound As Integer, LBound As Integer)
        Randomize()
        Return CInt(Math.Ceiling(Rnd() * UBound)) + LBound - 1
    End Function
    Public Function Increment(Value As Integer, Cap As Integer)
        Value = Value + 1
        If Value > Cap Then Value = 1
        Return Value
    End Function
    Public Function Increment(Value As Integer, Cap As Integer, Reset As Integer)
        Value = Value + 1
        If Value > Cap Then Value = Reset
        Return Value
    End Function
    Public Function BiggestNumber(num1 As Double, num2 As Double)
        If num1 > num2 Then
            Return num1
        Else Return num2
        End If
    End Function
    Public Function CheckGreaterThan(Value As Integer, Cap As Integer)
        If Value > Cap Then Return True
        If Value <= Cap Then Return False
    End Function
    Private Function BubbleSort(data() As Integer, Up As Boolean)
        Dim temp As Integer
        For i = data.Length - 1 To 1 Step -1
            For x = 1 To i
                If Up = False Then
                    If data(x) > data(x + 1) Then
                        temp = data(x)
                        data(x) = data(x + 1)
                        data(x + 1) = temp
                    End If
                Else
                    If data(x) < data(x + 1) Then
                        temp = data(x)
                        data(x) = data(x + 1)
                        data(x + 1) = temp
                    End If
                End If
            Next
        Next
        Return data
    End Function
    Private Function QuickSort(data() As Integer, length As Double, up As Boolean)
        Dim midpoint As Integer = Math.Round((length + 2) / 2)
        Dim Node As Integer = data(midpoint - 1)
        Dim temp2 As Integer = 0
        Dim temp1 As Integer = 0
        Dim Array1(temp1) As Integer
        Dim Array2(temp2) As Integer
        For i = 1 To length
            If up = True Then
                If data(i) <= Node And i <> midpoint - 1 Then
                    temp1 = temp1 + 1
                    ReDim Preserve Array1(temp1)
                    Array1(Array1.Length - 1) = data(i)
                End If
                If data(i) > Node Then
                    temp2 = temp2 + 1
                    ReDim Preserve Array2(temp2)
                    Array2(Array2.Length - 1) = data(i)
                End If
            Else
                If data(i) >= Node And i <> midpoint - 1 Then
                    temp1 = temp1 + 1
                    ReDim Preserve Array1(temp1)
                    Array1(Array1.Length - 1) = data(i)
                End If
                If data(i) < Node Then
                    temp2 = temp2 + 1
                    ReDim Preserve Array2(temp2)
                    Array2(Array2.Length - 1) = data(i)
                End If
            End If
        Next
        If Array1.Length - 1 > 1 Then
            Array1 = QuickSort(Array1, Array1.Length - 1, up)
        End If
        If Array2.Length - 1 > 1 Then
            Array2 = QuickSort(Array2, Array2.Length - 1, up)
        End If
        For i = 1 To Array1.Length - 1
            data(i) = Array1(i)
        Next
        data(Array1.Length) = Node
        For i = 1 To Array2.Length - 1
            data(i + Array1.Length) = Array2(i)
        Next
        Return data
    End Function
    Private Function BinarySearchFnc(data() As Integer, length As Integer, value As Integer)
        Dim midpoint As Integer = Math.Round((length + 2) / 2)
        Dim node As Integer = data(midpoint - 1)
        Dim up As Boolean
        Dim temp As Integer = 0
        Dim SecondArray(temp) As Integer
        If node = value Then Return True
        If length <= 2 Then Return False
        If node < value Then
            up = True
        Else up = False
        End If
        For i = 1 To length
            If up = True Then
                If data(i) > node Then
                    temp = temp + 1
                    ReDim Preserve SecondArray(temp)
                    SecondArray(SecondArray.Length - 1) = data(i)
                End If
            Else
                If data(i) < node Then
                    temp = temp + 1
                    ReDim Preserve SecondArray(temp)
                    SecondArray(SecondArray.Length - 1) = data(i)
                End If
            End If
        Next
        Return BinarySearchFnc(SecondArray, SecondArray.Length - 1, value)
    End Function
End Module
Class ReadWrite
    Private File As String
    Public Sub FileSelect(Input As String)
        File = Input
    End Sub
    Public Function read()
        Console.ReadKey()
        Dim fileReader As System.IO.StreamReader
        fileReader = My.Computer.FileSystem.OpenTextFileReader(File)
        Dim stringReader As String
        stringReader = fileReader.ReadLine()
        Return stringReader
    End Function
    Public Sub write(Input As String)
        Dim FILE_NAME As String = File
        If System.IO.File.Exists(FILE_NAME) = True Then
            Dim objWriter As New System.IO.StreamWriter(FILE_NAME)
            objWriter.Write(Input)
            objWriter.Close()
            MsgBox("Text written to file")
        Else
            MsgBox("File Does Not Exist")
        End If
    End Sub
End Class
Class stack
    Private stackpointer As Integer
    Private stackmax As Integer
    Private stackdata() As Object
    Public Sub New(Stacklength As Integer)
        stackpointer = 0
        stackmax = Stacklength - 1
        ReDim stackdata(Stacklength - 1)
    End Sub
    Public Sub PushOntoStack(Input As Integer)
Input:
        If CheckOutOfRange() = True Then
            stackdata(stackpointer) = Input
            stackpointer = stackpointer + 1
        Else QueueDelete()
            GoTo Input
        End If
    End Sub
    Private Sub SetPointer()
        stackpointer = 0
    End Sub
    Public Function PopOffStack()
        If stackpointer = 0 Then Return Nothing
        stackpointer = stackpointer - 1
        Dim Value As Integer = stackdata(stackpointer)
        Return Value
    End Function
    Private Function CheckOutOfRange()
        If stackpointer - 1 = stackmax Then
            Return False
        Else
            Return True
        End If
    End Function
    Private Sub QueueDelete()
        Dim reset As queue = New queue(stackdata.Length)
        For i = 1 To stackdata.Length
            reset.IntoQueue(PopOffStack())
        Next
        For i = 1 To stackdata.Length - 1
            PushOntoStack(reset.OutOfQueue())
        Next
        stackpointer = stackmax - 1
    End Sub
End Class
Class queue
    Private queuedata() As Object
    Private queuepointer As Integer
    Private queueoutpointer As Integer
    Sub New(size As Integer)
        queuepointer = 0
        queueoutpointer = 0
        ReDim queuedata(size)
    End Sub
    Public Sub Reset()
        queuepointer = 0
        queueoutpointer = 0
    End Sub
    Public Sub IntoQueue(Input As Integer)
        queuedata(queuepointer) = Input
        queuepointer = queuepointer + 1
    End Sub
    Public Function OutOfQueue()
        Dim value As Integer = queuedata(queueoutpointer)
        queueoutpointer = queueoutpointer + 1
        Return value
    End Function
End Class
Class Encrypter
    Implements IDisposable
    Private ArrayString() As String
    Private KeyArray() As Integer
    Public Sub SetKeyArray(length As Integer)
        ReDim KeyArray(length - 1)
        Dim x As Integer
        For x = 0 To length - 1
            KeyArray(x) = MathFunc.randomnumber(127, 0)
        Next
    End Sub
    Public Sub Destroykey() Implements IDisposable.Dispose
        Dim x As Integer
        For x = 0 To KeyArray.Length - 1
            KeyArray(x) = 0
        Next
    End Sub
    Public Function XorGateIN(EncryptString As String)
        Dim x As Integer
        Dim j As Integer
        Dim Cipher As String = ""
        For j = 0 To (EncryptString.Length - 1)
            x = Asc(Mid(EncryptString, j + 1, 1)) Xor KeyArray(j)
            Cipher = Cipher & Chr(x)
        Next
        Return Cipher
    End Function
End Class
Class TripleEncryption
    Dim Encrypter1 As New Encrypter
    Dim Encrypter2 As New Encrypter
    Dim Encrypter3 As New Encrypter
    Public Sub Close()
        Encrypter1.Destroykey()
        Encrypter2.Destroykey()
        Encrypter3.Destroykey()
    End Sub
    Private Sub open(Input)
        Encrypter1.SetKeyArray(Input.Length)
        Encrypter2.SetKeyArray(Input.Length)
        Encrypter3.SetKeyArray(Input.Length)
    End Sub
    Public Function Encrypt(Input As String)
        open(Input)
        Return Encrypter3.XorGateIN(Encrypter2.XorGateIN(Encrypter1.XorGateIN(Input)))
    End Function
    Public Function Decrypt(Input As String)
        Return Encrypter1.XorGateIN(Encrypter2.XorGateIN(Encrypter3.XorGateIN(Input)))
    End Function
End Class

