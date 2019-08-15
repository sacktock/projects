Module Module1
    Class Encrypt
        Private ArrayString() As String
        Private EnString As String
        Private KeyArray() As Integer
        Private StringLength As Integer
        Public Sub New()
            EnString = ""
        End Sub
        Public Function callString()
            Return EnString
        End Function
        Private Sub SetKeyArray()
            ReDim KeyArray(callLenght() - 1)
            Dim x As Integer
            For x = 0 To callLenght() - 1
                KeyArray(x) = GetRandomnumber()
            Next
        End Sub
        Private Sub Destroykey()
            Dim x As Integer
            For x = 0 To callLenght() - 1
                KeyArray(x) = 0
            Next
        End Sub
        Private Function callLenght()
            Return StringLength
        End Function
        Private Function GetRandomnumber()
            Randomize()
            Return (CInt(Math.Ceiling(Rnd() * 127)) + 0)
        End Function
        Public Sub Setstring()
            Console.WriteLine("Please enter Your string")
            EnString = Console.ReadLine
            StringLength = EnString.Length
        End Sub
        Private Function XorGateIN(EncryptString As String, ByVal Keystream() As Integer)
            Dim x As Integer
            Dim j As Integer
            Dim Cipher As String = ""
            For j = 0 To (EncryptString.Length - 1)
                x = Asc(Mid(EncryptString, j + 1, 1)) Xor Keystream(j)
                Cipher = Cipher & Chr(x)
            Next
            Return Cipher
        End Function
        Public Function Encrypter()
            SetKeyArray()
            EnString = XorGateIN(callString(), KeyArray)
            Return EnString
        End Function
        Public Function Decrypter()
            EnString = XorGateIN(callString(), KeyArray)
            Return EnString
            Destroykey()
        End Function
    End Class
    Sub Main()
        Dim String1 As Encrypt = New Encrypt()
        String1.Setstring()
        Console.ReadLine()
        Console.WriteLine(String1.Encrypter())
        Console.ReadLine()

        Console.WriteLine(String1.Decrypter())
        Console.ReadLine()
    End Sub

End Module
