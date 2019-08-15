Public Class Form1
    Private Board(2, 2) As Char
    Private Win As Boolean = False
    Private RecentPress(2, 2) As Boolean
    Private Function WinOrNot()
        Return Win
    End Function
    Private Sub IntoArray()
        Board(0, 0) = Button1.Text
        Board(0, 1) = Button2.Text
        Board(0, 2) = Button3.Text
        Board(1, 0) = Button4.Text
        Board(1, 1) = Button5.Text
        Board(1, 2) = Button6.Text
        Board(2, 0) = Button7.Text
        Board(2, 1) = Button8.Text
        Board(2, 2) = Button9.Text
    End Sub

    Private Function PlayerChar()
        Return PlayerTurn.Text
    End Function

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        If WinOrNot() = False Then
            Button3.Text = PlayerChar()
            EndTurn()
        End If
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        If WinOrNot() = False Then
            Button1.Text = PlayerChar()
            EndTurn()
        End If
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        If WinOrNot() = False Then
            Button2.Text = PlayerChar()
            EndTurn()
        End If
    End Sub

    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click
        If WinOrNot() = False Then
            Button4.Text = PlayerChar()
            EndTurn()
        End If
    End Sub

    Private Sub Button5_Click(sender As Object, e As EventArgs) Handles Button5.Click
        If WinOrNot() = False Then
            Button5.Text = PlayerChar()
            EndTurn()
        End If
    End Sub

    Private Sub Button6_Click(sender As Object, e As EventArgs) Handles Button6.Click
        If WinOrNot() = False Then
            Button6.Text = PlayerChar()
            EndTurn()
        End If
    End Sub

    Private Sub Button7_Click(sender As Object, e As EventArgs) Handles Button7.Click
        If WinOrNot() = False Then
            Button7.Text = PlayerChar()
            EndTurn()
        End If
    End Sub

    Private Sub Button8_Click(sender As Object, e As EventArgs) Handles Button8.Click
        If WinOrNot() = False Then
            Button8.Text = PlayerChar()
            EndTurn()
        End If
    End Sub

    Private Sub Button9_Click(sender As Object, e As EventArgs) Handles Button9.Click
        If WinOrNot() = False Then
            Button9.Text = PlayerChar()
            EndTurn()
        End If
    End Sub
    Private Sub EndTurn()
        IntoArray()
        CheckWin()
        SwapPlayer()
    End Sub
    Private Function NumberInbounds(number As Integer, add As Integer)
        Dim value As Integer
        If number < 4 Then
            value = numberinbounds(number, 4, add)
        End If
        If number > 4 And number < 7 Then
            value = numberinbounds(number, 7, add)
        End If
        If number > 6 Then
            value = numberinbounds(number, 9, add)
        End If
        Return value
    End Function
    Private Function numberinbounds(number As Integer, upperbound As Integer, add As Integer)
        If number + add > upperbound Then
            Return number - add
        Else
            Return add
        End If
    End Function
    Private Sub CheckWin()
        Dim WinCondition As String = CheckWinner(PlayerChar())
        If WinCondition <> False Then
            MsgBox("Player " & PlayerChar() & " Wins!")
            Win = True
        End If
    End Sub
    Private Function CheckWinner(character As Char)
        For x = 0 To 2
            If Board(x, 0) = character And Board(x, 1) = character And Board(x, 2) = character Then
                Return True
            End If
            If Board(0, x) = character And Board(1, x) = character And Board(2, x) = character Then
                Return True
            End If
        Next
        If Board(0, 0) = character And Board(1, 1) = character And Board(2, 2) = character Then
            Return True
        End If
        If Board(0, 2) = character And Board(1, 1) = character And Board(2, 0) = character Then
            Return True
        End If
        Return False
    End Function
    Private Sub SwapPlayer()
        If PlayerChar() = "O" Then
            PlayerTurn.Text = "X"
        ElseIf PlayerChar() = "X" Then
            PlayerTurn.Text = "O"
        End If
    End Sub
    Private Sub NewGame_Click(sender As Object, e As EventArgs) Handles NewGame.Click
        Clearboard()
        PlayerTurn.Text = RandomStart()

        Win = False
    End Sub
    Private Sub Clearboard()
        Button1.Text = ""
        Button2.Text = ""
        Button3.Text = ""
        Button4.Text = ""
        Button5.Text = ""
        Button6.Text = ""
        Button7.Text = ""
        Button8.Text = ""
        Button9.Text = ""
    End Sub
    Private Function RandomStart()
        Randomize()
        Dim num As Integer = CInt(Math.Ceiling(Rnd() * 2)) + 0
        If num = 1 Then
            Return "O"
        Else
            Return "X"
        End If

    End Function
End Class
