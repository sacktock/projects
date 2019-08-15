Module Module1
    Function ConvertToBinary(ByVal input As Integer) As String
        Dim output As String = ""
        Dim temp() As Integer = {0}
        Dim counter As Integer = 0
        Do Until input = 0
            ReDim Preserve temp(counter)
            temp(counter) = (input Mod 2)
            input = Math.Truncate(input / 2)
            counter += 1
        Loop
        For x = (temp.Length - 1) To 0 Step -1
            output = output & Convert.ToString(temp(x))
        Next
        Return output
    End Function
    Sub Main()
        Dim input As Integer
        Console.WriteLine("Input the denary number you wish to convert:")
        input = Console.ReadLine
        Console.Clear()
        Console.WriteLine("Binary Number:")
        Console.WriteLine(ConvertToBinary(input))
        Console.ReadLine()
    End Sub
End Module
