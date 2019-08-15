Module Module1

    Sub Main()
        Dim ar(99) As Integer
        Dim rand As Random = New Random

        For i As Integer = 0 To ar.Length - 1
            ar(i) = rand.Next(0, 20)
            Console.Write(ar(i).ToString & ", ")
        Next

        Console.WriteLine("Do merge sort")
        Console.ReadLine()

        MergeSort(ar)

        For i As Integer = 0 To ar.Length - 1
            Console.Write(ar(i).ToString & ", ")
        Next

        Console.WriteLine("Number to do binary search on")

        Dim int As Integer = 10
        Console.ReadLine()

        Console.WriteLine(BinarySearch(ar, int).ToString)

        Console.ReadLine()

    End Sub

    Public Function BinarySearch(ByVal ar() As Integer, ByVal val As Integer) As Boolean
        DoBinarySearch(ar, val, Math.Floor(ar.Length / 2))
    End Function
    Private Function DoBinarySearch(ByVal ar() As Integer, ByVal val As Integer, ByVal pivot As Integer)
        Dim int As Integer = ar(pivot)
        Dim temp() As Integer
        If (int = val) Then
            Return True
        ElseIf (ar.Length = 1) And (int <> val) Then
            Return False
        ElseIf (int < val) Then
            ReDim temp(pivot - 1)
            For i As Integer = 0 To pivot - 1
                temp(i) = ar(i)
            Next
            Return DoBinarySearch(temp, val, Math.Floor(temp.Length / 2))
        ElseIf (int > val) Then
            ReDim temp(ar.Length - 2 - pivot)
            Dim count As Integer = 0
            For i As Integer = pivot + 1 To ar.Length - 1

                temp(count) = ar(i)
                count += 1
            Next
            Return DoBinarySearch(temp, val, Math.Floor(temp.Length / 2))
        End If


    End Function


    Public Sub MergeSort(ByRef ar() As Integer)
        DoMergeSort(ar, 0, ar.Length - 1)
    End Sub

    Private Sub DoMergeSort(ByVal ar() As Integer, ByVal low As Integer, ByVal high As Integer)

        If low >= high Then Return
        Dim length As Integer = high - low + 1
        Dim middle As Integer = Math.Floor((low + high) / 2)
        DoMergeSort(ar, low, middle)
        DoMergeSort(ar, middle + 1, high)
        Dim temp(ar.Length - 1) As Integer
        For i As Integer = 0 To length - 1
            temp(i) = ar(low + i)
        Next
        Dim m1 As Integer = 0
        Dim m2 As Integer = middle - low + 1
        For i As Integer = 0 To length - 1
            If m2 <= high - low Then
                If m1 <= middle - low Then
                    If temp(m1) > temp(m2) Then
                        ar(i + low) = temp(m2)
                        m2 += 1
                    Else
                        ar(i + low) = temp(m1)
                        m1 += 1
                    End If
                Else
                    ar(i + low) = temp(m2)
                    m2 += 1
                End If
            Else
                ar(i + low) = temp(m1)
                m1 += 1
            End If
        Next

    End Sub



End Module
