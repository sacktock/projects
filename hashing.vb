Module Index
    Private collisions As Integer = 0
    Public Sub AssignIndex(ByRef Memory() As Object, ByRef input As Object, ByRef OverFlow As Collection)
check:
        If Memory(input.id) IsNot Nothing Then
            collisions += 1
            input.InOverflow = True
            OverFlow.Add(input)
        Else
            Memory(input.id) = input
        End If
    End Sub
    Public Function IndexAvailable(ByRef array() As Object, ByVal index As Integer)
        If array(index) Is Nothing Then Return True
        Return False
    End Function
    Public Function GetCollisions()
        Return collisions
    End Function
End Module

Module Module1
    Dim Hashobj = System.Security.Cryptography.HashAlgorithm.Create()
    Const size As Integer = 15
    Const students As Integer = 12
    Public pointer As Integer = 0
    Dim Overflow As New Collection
    Dim Memory(size) As Object

    Public Function HashingAlgoirthm(ByVal Input As Double, ByVal TimeInput As Double) As Integer
        Const Prime As ULong = 5463458053
        Dim output As Decimal
        Dim TempByte(1) As Byte
        TempByte(0) = Convert.ToByte(Input)
        TempByte(1) = Convert.ToByte(TimeInput Mod 256)
        TempByte = Hashobj.ComputeHash(TempByte)
        output = Convert.ToInt64(TempByte(0)) ^ ((Convert.ToInt64(TempByte(1))) / 29)
        output = output * (9753179)
        output = output + TempByte(1) - TempByte(0) / 11
        output = output / Prime
        If output < 0 Then output = output * (-1)
        output = Math.Truncate(output) Mod (size + 1)
        Return output
    End Function
    Public Sub CreateStudents()
        Dim temp As New Comp_Student("ALex")
        Index.AssignIndex(Memory, temp, Overflow)
    End Sub
    Public Function ReturnObject(ByVal Hashtag As Integer, ByVal DayOfBirth As Integer)
        Dim index As Integer = HashingAlgoirthm(DayOfBirth, Hashtag)
        Dim temp As Object = Memory(index)
        If temp.hashtag = Hashtag And temp.DayofBirth = DayOfBirth Then
            Return temp
        Else
            For i = 1 To Overflow.Count
                If Overflow.Item(i).Hashtag = Hashtag And Overflow.Item(i).DayOfBirth = DayOfBirth Then
                    Return Overflow.Item(i)
                End If
            Next
        End If
        Return Nothing
    End Function
    Sub Main()
        Dim tempStudent As Object
        For i = 0 To students - 1
            CreateStudents()
        Next
        Console.Clear()
        For i = 0 To size - 1
            If Memory(i) Is Nothing Then
                Console.WriteLine(i & ". ")
            Else
                Console.WriteLine(i & ". " & Memory(i).name & " " & Memory(i).hashtag & " " & Memory(i).dayofbirth)
            End If
        Next
        Console.WriteLine("[ There were " & Index.GetCollisions & " collisions ]")
        Console.WriteLine((Overflow.Count / students) * 100 & "% of objects put in the overflow")
        Console.WriteLine("ENTER to display overflow")
        Console.ReadLine()
        For i = 1 To Overflow.Count
            Console.WriteLine(i & ". " & Overflow.Item(i).name & " " & Overflow.Item(i).hashtag & " " & Overflow.Item(i).dayofbirth)
        Next
        tempStudent = FindObject()
        If tempStudent.inoverflow = True Then Console.WriteLine("Found in overload:")
        Console.WriteLine(tempStudent.name & ",  " & tempStudent.DayOfBirth)
        Console.ReadLine()
    End Sub
    Public Function FindObject()
        Console.WriteLine("Find Object(Enter HashTag, DayOfBirth)")
        Console.WriteLine("Hashtag:")
        Dim InHash As Integer = Console.ReadLine
        Console.WriteLine("DayOfBirth:")
        Dim InDay As Integer = Console.ReadLine
        Return ReturnObject(InHash, InDay)
    End Function
End Module
Class Comp_Student
    Public ID As Integer
    Public InOverflow As Boolean
    Public Name As String
    Public DayOfBirth As Double
    Public HashTag As Double
    Public Sub New()
        Console.WriteLine("Enter the students name")
        Name = Console.ReadLine
        Console.WriteLine("Enter " & Name & "'s day of birth")
        DayOfBirth = getInt()
        HashTag = CInt(Math.Ceiling(Rnd() * 10000)) + 1
        ID = HashingAlgoirthm(DayOfBirth, HashTag)
    End Sub
    Public Sub New(ByVal INName As String)
        Name = INName
        Randomize()
        DayOfBirth = CInt(Math.Ceiling(rnd() * 30)) + 1
        HashTag = CInt(Math.Ceiling(Rnd() * 10000)) + 1
        ID = HashingAlgoirthm(DayOfBirth, HashTag)
    End Sub
    Public Function getInt()
        Dim output As Integer
        Try
            output = Console.ReadLine
        Catch ex As Exception
            Console.WriteLine(ex.Message)
        End Try
        Return output
    End Function
End Class
