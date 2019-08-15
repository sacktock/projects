Module Module1
    Dim Save As stack = New stack()
    Class ReadWrite
        Private File As String
        Public Sub New(Input As String)
            File = Input
        End Sub
        Public Function GetFile()
            Return File
        End Function
        Public Sub write(moves As Integer, playername1 As String, playername2 As String)
            If System.IO.File.Exists(File) = True Then
                Dim objWriter = New System.IO.StreamWriter(File)
                For x = 0 To 7
                    For y = 0 To 7
                        objWriter.Write(Save.OutOfStack(x, y, 0) & ";")
                    Next
                Next
                objWriter.Write(moves & ";")
                objWriter.Write(playername1 & ";")
                objWriter.Write(playername2 & ";")
                objWriter.Close()
            End If
        End Sub

        Public Sub Close()
            File = ""
        End Sub
    End Class
    Class stack
        Private stackpointer As Integer = -1
        Private stackmax As Integer = 4
        Private stackdata(7, 7, stackmax) As Char
        Dim reset As queue = New queue()
        Public Sub IntoStack(data(,) As Char)
            stackpointer = stackpointer + 1
Save:
            If CheckOutOfRange() = True Then
                For x = 0 To 7
                    For y = 0 To 7
                        stackdata(x, y, stackpointer) = data(x, y)
                    Next
                Next

            Else
                reset.IntoQueue(stackdata, stackmax)
                QueueDelete()
                GoTo Save
            End If
        End Sub
        Public Sub SetPointer()
            stackpointer = -1
        End Sub

        Public Function OutOfStack(x, y, undo)
            Dim position As Integer = stackpointer + undo
            Return stackdata(x, y, position)
        End Function
        Private Function CheckOutOfRange()
            If stackpointer = stackmax + 1 Then
                Return False
            Else
                Return True
            End If
        End Function
        Private Sub QueueDelete()
            Dim queuepointer As Integer = 0
            stackpointer = stackmax
            For i = 0 To stackmax - 1
                For x = 0 To 7
                    For y = 0 To 7
                        stackdata(x, y, stackpointer) = reset.OutOfQueue(x, y, queuepointer)
                    Next
                Next
                stackpointer = stackpointer - 1
                queuepointer = queuepointer + 1
            Next
            stackpointer = stackmax - 1
        End Sub
    End Class
    Class queue
        Private queuedata(,,) As Char
        Public Sub New()

        End Sub
        Public Sub IntoQueue(data(,,) As Char, max As Integer)
            ReDim queuedata(7, 7, max)
            Dim queuepointer As Integer = 0
            Dim stackpointer As Integer = max
            For i = 0 To max
                For x = 0 To 7
                    For y = 0 To 7
                        queuedata(x, y, queuepointer) = data(x, y, stackpointer)
                    Next
                Next
                queuepointer = queuepointer + 1
                stackpointer = stackpointer - 1
            Next
        End Sub
        Public Function OutOfQueue(x, y, dimension)
            Return queuedata(x, y, dimension)
        End Function
    End Class
    Class draughts
        Const File As String = "H:\My Documents\swag.txt"
        Private player1, player2 As String
        Private board(7, 7) As Char
        Private moves As Integer
        Private movecoordinates(1) As Integer
        Private SelectCoordinates(1) As Integer
        Private CheckCoordinates(1, 7) As Integer


        Private Sub loadboard(undo As Integer)
            For x = 0 To 7
                For y = 0 To 7
                    board(x, y) = Save.OutOfStack(x, y, undo)
                Next
            Next
        End Sub
        Private Sub Saveboard()
            Save.IntoStack(board)
        End Sub

        Private Sub SetupBoard()
            Dim temp As Integer = 1
            DefaultBoard()
            For x = 0 To 2
                For y = 0 To 7 Step 2
                    board(x, y + temp) = "O"
                Next
                If x = 0 Then
                    temp = 0
                ElseIf x = 1 Then
                    temp = 1
                End If
            Next
            temp = 0
            For x = 5 To 7
                For y = 0 To 7 Step 2
                    board(x, y + temp) = "X"
                Next
                If x = 5 Then
                    temp = 1
                ElseIf x = 6 Then
                    temp = 0
                End If
            Next
        End Sub
        Private Sub DefaultBoard()
            For x = 0 To 7
                For y = 0 To 7
                    board(x, y) = " "
                Next
            Next
        End Sub
        Public Sub DisplayBoard()
            Console.ForegroundColor = ConsoleColor.White
            Dim counter As Integer = 1
            Console.Write(" ")
            For y = 0 To 7
                Console.Write("" & y + 1 & " ")
            Next
            Console.WriteLine()
            For x = 0 To 7
                Console.BackgroundColor = ConsoleColor.Black
                Console.ForegroundColor = ConsoleColor.White
                Console.Write("" & x + 1 & "")
                For y = 0 To 7
                    Console.ForegroundColor = ConsoleColor.Black
                    counter = counter + 1
                    If counter Mod 2 = 0 Then
                        Console.BackgroundColor = ConsoleColor.Black
                    Else
                        Console.BackgroundColor = ConsoleColor.White
                    End If
                    Console.Write("" & board(x, y) & " ")
                Next
                counter = counter + 1
                Console.WriteLine()
            Next
            Console.ForegroundColor = ConsoleColor.White
            Console.BackgroundColor = ConsoleColor.Black
        End Sub
        Public Sub New()
            SetupBoard()

        End Sub
        Public Sub StartGame()
            If LoadSave() = False Then
                Console.WriteLine("please enter player1s name")
                player1 = Console.ReadLine
                Console.WriteLine("please eneter player2s name")
                player2 = Console.ReadLine
                If 1 = 1 Then
                    Console.WriteLine(player1 & " starts")
                    moves = 2
                Else
                    Console.WriteLine(player2 & " starts")
                    moves = 3
                End If
            End If
            Saveboard()
        End Sub
        Private Sub Turn()
            Dim Counter As Integer = 0
            Dim Player As Integer
            If moves Mod 2 = 0 Then
                Console.WriteLine(player1 & "'s turn")
                Player = 1
            Else
                Console.WriteLine(player2 & "'s turn")
                Player = 2
            End If
EnterPiece:
            SelectPiece(getplayerchar(Player))
enter:
            If GetCoords() = False Then
                If Counter = 3 Then
                    Console.WriteLine("Please select another peice")
                    Counter = 0
                    ClearUp()
                    GoTo EnterPiece
                End If
                Counter = Counter + 1
                GoTo enter
            End If
            MovePiece(movecoordinates(0), movecoordinates(1), getplayerchar(Player))
            moves = moves + 1
        End Sub
        Private Sub UndoMove()
            Dim answer As String
            Console.WriteLine("Do you want to Undo your last move?")
            answer = Console.ReadLine
            If answer.ToUpper = ("Y") Then
                loadboard(-1)
                moves = moves - 1
                Console.Clear()
                DisplayBoard()
                RedoMove()
                Saveboard()
            End If
        End Sub
        Private Sub RedoMove()
            Dim answer As String
            Console.WriteLine("Do you want to Redo your last move?")
            answer = Console.ReadLine
            If answer.ToUpper = ("Y") Then
                loadboard(0)
                moves = moves + 1
            End If

        End Sub
        Private Sub SelectPiece(playerchar)
            Console.WriteLine("Enter the coordinates of the piece you want to move:")
enterpiece:
            SelectCoordinates(1) = Console.ReadLine
            SelectCoordinates(0) = Console.ReadLine
            If CheckCoordOutOfBounds(SelectCoordinates(0), SelectCoordinates(1)) = False Then
                Console.WriteLine("Please eneter valid coodinates")
                GoTo enterpiece
            End If
            If checkPieceExists(SelectCoordinates(0), SelectCoordinates(1), playerchar) = False Then
                Console.WriteLine("Please eneter valid coodinates")
                GoTo enterpiece
            End If
        End Sub
        Private Function GetCoords()
            Console.WriteLine("Enter your move (Coordinates):")

            movecoordinates(1) = Console.ReadLine
            movecoordinates(0) = Console.ReadLine
            If CheckCoordOutOfBounds(movecoordinates(0), movecoordinates(1)) = False Then
                Console.WriteLine("These coordinates are not valid")
                Return False
            End If
            If CheckValidMove() = False Then
                Console.WriteLine("This move is not valid")
                Return False
            End If
            Return True
        End Function
        Private Function CheckValidMove()
            Dim Check As Boolean
            If moves Mod 2 = 0 Then
                If CheckPieceMove(movecoordinates(0), movecoordinates(1), -1, -1, " ") = True Then
                    If Firstcheck(1) = True Then
                        Check = True
                    ElseIf SecondCheck(1) = True Then
                        Check = True
                    End If
                Else
                    Check = False
                End If
            End If
            If moves Mod 2 = 1 Then
                If CheckPieceMove(movecoordinates(0), movecoordinates(1), -1, -1, " ") = True Then
                    If Firstcheck(-1) = True Then
                        Check = True
                    ElseIf SecondCheck(-1) = True Then
                        Check = True
                    End If
                Else
                    Check = False
                End If
            End If
            Return Check
        End Function
        Private Sub jumppiece(value, sign)
            If moves Mod 2 = 0 Then
                If CheckPieceMove(SelectCoordinates(0) + value, SelectCoordinates(1) + sign, -1, -1, "X") Then
                    deletePiece(SelectCoordinates(0) + value, SelectCoordinates(1) + sign, -1)
                End If
            ElseIf moves Mod 2 = 1 Then
                If CheckPieceMove(SelectCoordinates(0) + value, SelectCoordinates(1) + sign, -1, -1, "O") Then
                    deletePiece(SelectCoordinates(0) + value, SelectCoordinates(1) + sign, -1)
                End If
            Else
            End If
        End Sub
        Private Function SecondCheck(sign As Integer)
            If (SelectCoordinates(0) + sign * 2) = movecoordinates(0) And (SelectCoordinates(1) + 2) = movecoordinates(1) Then
                If CheckPieceMove(SelectCoordinates(0) + 1, SelectCoordinates(1) + sign, -1, -1, " ") Then
                    Return False
                Else
                    jumppiece(sign, 1)
                    Return True
                End If
            ElseIf (SelectCoordinates(0) + sign * 2) = movecoordinates(0) And (SelectCoordinates(1) - 2) = movecoordinates(1) Then
                If CheckPieceMove(SelectCoordinates(0) - 1, SelectCoordinates(1) + sign, -1, -1, " ") Then
                    Return False
                Else
                    jumppiece(sign, -1)
                    Return True
                End If
            Else
                Return False
            End If
        End Function
        Private Function CheckPieceMove(x, y, number1, number2, paramchar)
            If board(x + number1, y + number2) = paramchar Then
                Return True
            Else
                Return False
            End If
        End Function
        Private Function CheckCoordOutOfBounds(x, y)
            If x < 1 Or x > 8 Then
                Return False
            ElseIf y < 1 Or y > 8 Then
                Return False
            Else
                Return True
            End If
        End Function
        Private Function Firstcheck(sign As Integer)
            If (SelectCoordinates(0) + sign) = movecoordinates(0) And (SelectCoordinates(1) + 1) = movecoordinates(1) Then
                Return True
            ElseIf (SelectCoordinates(0) + sign) = movecoordinates(0) And (SelectCoordinates(1) - 1) = movecoordinates(1) Then
                Return True
            Else
                Return False
            End If
        End Function
        Private Function checkPieceExists(x, y, playerchar)
            If board(x - 1, y - 1) = playerchar Then
                Return True
            Else
                Return False
            End If
        End Function
        Private Sub deletePiece(x, y, number)
            board(x + number, y + number) = " "
        End Sub
        Private Sub MovePiece(x, y, Playerchar)
            board(x - 1, y - 1) = Playerchar
            deletePiece(SelectCoordinates(0), SelectCoordinates(1), -1)
        End Sub
        Public Sub Game()
            StartGame()
            Dim finish As Boolean = False
            Do Until finish = True
                DisplayBoard()
                Turn()
                ClearUp()
                Saveboard()
                UndoMove()
                ClearUp()
                ExitSave()
                Console.Clear()
                finish = Playerwin(1)
                finish = Playerwin(2)

            Loop
        End Sub
        Private Function getplayerchar(number)
            If number = 1 Then
                Return "O"
            Else
                Return "X"
            End If
        End Function
        Private Function Playerwin(player)
            Dim playerchar As Char = getplayerchar(player)
            For x = 0 To 7
                For y = 0 To 7
                    If board(x, y) = playerchar Then
                        Return False
                    End If
                Next
            Next
            If player = 1 Then
                Console.WriteLine("Player 2 wins!")
            Else
                Console.WriteLine("Player 1 wins!")
            End If
            Return True
        End Function
        Private Function randomnumber(n)
            Randomize()
            Return CInt(Math.Ceiling(Rnd() * n)) + 1
        End Function
        Private Sub ExitSave()
            Dim answer As String
            Console.WriteLine("Do you want to Exit and Save?")
            answer = Console.ReadLine()
            If answer.ToUpper = ("Y") Then
                IntoCSV()
                MsgBox("Game Saved")
                Stop
            End If
        End Sub
        Private Sub IntoCSV()
            Dim Intotext As ReadWrite = New ReadWrite(File)
            Intotext.write(moves, player1, player2)
        End Sub
        Private Function LoadSave()
            Dim answer As String
            Console.WriteLine("Do You want to load a Save?")
            answer = Console.ReadLine
            If answer.ToUpper = "Y" Then
                Dim TextFileReader As New Microsoft.VisualBasic.FileIO.TextFieldParser(File)
                TextFileReader.TextFieldType = FileIO.FieldType.Delimited
                TextFileReader.SetDelimiters(";")
                Dim arrayFile() As String = (TextFileReader.ReadFields)
                Dim counter As Integer = 0
                For x = 0 To 7
                    For y = 0 To 7
                        If arrayFile(counter) = Nothing Then
                            board(x, y) = " "
                        Else
                            board(x, y) = arrayFile(counter)
                        End If
                        counter = counter + 1
                    Next
                Next
                moves = arrayFile(64)
                player1 = arrayFile(65)
                player2 = arrayFile(66)
                Return True
            End If
            Return False
        End Function
        Public Sub ClearUp()
            Console.Clear()
            DisplayBoard()
        End Sub
    End Class
    Sub Main()
        Dim game As draughts = New draughts()
        game.Game()
        Console.ReadLine()
    End Sub
End Module

