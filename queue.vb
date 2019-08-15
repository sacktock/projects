Module Module1

    Sub Main()

    End Sub
    Public Class Queue
        Private QueueData() As Char
        Private QueueRearPointer As Integer = -1
        Private QueueFrontPointer As Integer

        Public Sub AddData(ByVal input As Char)
            If QueueRearPointer = 9 Then QueueRearPointer = 0
            QueueRearPointer += 1
            QueueData(QueueRearPointer) = input
        End Sub
        Public Function RemoveData()
            Dim value As Char = QueueData(QueueFrontPointer)
            QueueFrontPointer += 1
            Return value
            If QueueFrontPointer = 9 Then QueueFrontPointer = 0
        End Function

    End Class
End Module
