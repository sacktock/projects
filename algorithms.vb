Public Class Form1
    Dim BoxArray As New Collection
    Private NumberArray() As Integer
    Private Sub SetupCollection()
        BoxArray.Add(TextBox1)
        BoxArray.Add(TextBox2)
        BoxArray.Add(TextBox3)
        BoxArray.Add(TextBox4)
        BoxArray.Add(TextBox5)
        BoxArray.Add(TextBox6)
        BoxArray.Add(TextBox7)
        BoxArray.Add(TextBox8)
        BoxArray.Add(TextBox9)
        BoxArray.Add(TextBox10)
        BoxArray.Add(TextBox11)
        BoxArray.Add(TextBox12)
        BoxArray.Add(TextBox13)
        BoxArray.Add(TextBox14)
        BoxArray.Add(TextBox15)
        BoxArray.Add(TextBox16)
    End Sub
    Public Sub New()
        InitializeComponent()
        SetupCollection()
        For i = 1 To BoxArray.Count
            BoxArray(i).text = "0"
        Next
    End Sub

    Private Sub IntoArray()
        ReDim NumberArray(BoxArray.Count)
        For i = 1 To BoxArray.Count
            NumberArray(i) = BoxArray(i).text
        Next
    End Sub
    Private Function BubbleSort(data() As Integer, length As Integer, Up As Boolean)
        Dim temp As Integer
        For i = length - 1 To 1 Step -1
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
    Private Sub OutOfArray()
        For i = 1 To BoxArray.Count
            BoxArray(i).text = NumberArray(i)
        Next
    End Sub
    Private Sub Sort(type As Boolean)
        IntoArray()
        NumberArray = BubbleSort(NumberArray, NumberArray.Length - 1, type)
        OutOfArray()
    End Sub
    Private Sub BubbleSortDesc_Click(sender As Object, e As EventArgs) Handles BubbleSortDesc.Click
        Sort(True)
    End Sub
    Private Sub BubbleSortAsc_Click(sender As Object, e As EventArgs) Handles BubbleSortAsc.Click
        Sort(False)
    End Sub

    Private Sub RandomNumber_Click(sender As Object, e As EventArgs) Handles RandomNumber.Click
        For i = 1 To BoxArray.Count
            Randomize()
            BoxArray(i).text = CInt(Math.Ceiling(Rnd() * 99)) + 1
        Next
    End Sub

    Private Sub Scramble_Click(sender As Object, e As EventArgs) Handles Scramble.Click
        Dim temp As Integer
        Dim rand As Integer
        For i = 1 To BoxArray.Count
            temp = BoxArray(i).text
            Randomize()
            rand = CInt(Math.Ceiling(Rnd() * BoxArray.Count))
            BoxArray(i).text = BoxArray(rand).text
            BoxArray(rand).text = temp
        Next
    End Sub
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

    Private Sub QuickSortDesc_Click(sender As Object, e As EventArgs) Handles QuickSortDesc.Click
        QSort(False)
    End Sub
    Private Sub QSort(type As Boolean)
        IntoArray()
        NumberArray = QuickSort(NumberArray, NumberArray.Length - 1, type)
        OutOfArray()
    End Sub
    Private Sub QuickSortAsc_Click(sender As Object, e As EventArgs) Handles QuickSortAsc.Click
        QSort(True)
    End Sub
    Private Function BinarySearchFnc(data() As Integer, length As Integer, value As Integer)
        Dim midpoint As Integer = Math.Round((length + 2) / 2)
        Dim node As Integer = data(midpoint - 1)
        Dim up As Boolean
        Dim temp As Integer = 0
        Dim SecondArray(temp) As Integer
        If node = value Then
            Return True
        End If
        If length <= 2 Then
            Return False
        End If
        If node < value Then
            up = True
        Else
            up = False
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
    Private Sub BinarySearch_Click(sender As Object, e As EventArgs) Handles BinarySearch.Click
        search(TextBox17.Text)
    End Sub
    Private Sub search(value)
        IntoArray()
        If BinarySearchFnc(NumberArray, NumberArray.Length - 1, value) = True Then
            MsgBox(value & " is within this list of numbers")
        Else
            MsgBox(value & " is not within this list of numbers")
        End If
    End Sub

    Private Sub RandBin_Click(sender As Object, e As EventArgs) Handles RandBin.Click
        Randomize()
        TextBox18.Text = CInt(Math.Ceiling(Rnd() * 80)) + 100
    End Sub
    Private Sub Pack(size As Integer, type As Boolean)
        IntoArray()
        If type = True Then
            NumberArray = BubbleSort(NumberArray, NumberArray.Length - 1, True)
        End If
        MsgBox("It takes " & FirstFitBinPacking(NumberArray, NumberArray.Length - 1, size) & " bins to pack this list of numbers in bins the size " & size & " units")
    End Sub
    Private Function FirstFitBinPacking(data() As Integer, length As Integer, size As Integer)
        Dim Bins As Integer = 1
        Dim binspace(Bins) As Integer
        Dim packed As Boolean
        Dim x As Integer = 1
        For i = 1 To length
            Do Until packed = True
                If x > Bins Then
                    Bins = Bins + 1
                    ReDim Preserve binspace(Bins)
                End If
                binspace(x) = binspace(x) + data(i)
                If binspace(x) > size Then
                    binspace(x) = binspace(x) - data(i)
                    x = x + 1
                    ReDim Preserve binspace(Bins)
                    binspace(Bins) = data(i)
                Else
                    packed = True
                End If
            Loop
            x = 1
            packed = False
        Next
        Return Bins
    End Function

    Private Sub FirstFit_Click(sender As Object, e As EventArgs) Handles FirstFit.Click
        Pack(TextBox18.Text, False)
    End Sub
    Private Function FindSmallest(data() As Integer)
        Return data.Min
    End Function
    Private Sub PrimsAlogorithm()
        Dim ArrayA(5) As Integer
        Dim ArrayB(5) As Integer
        Dim ArrayC(5) As Integer
        Dim ArrayD(5) As Integer
        Dim ArrayE(5) As Integer
        Dim ArrayF(5) As Integer




    End Sub
    Private Sub FirstFitD_Click(sender As Object, e As EventArgs) Handles FirstFitD.Click
        Pack(TextBox18.Text, True)
    End Sub
End Class
