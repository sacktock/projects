Class QueueOfTiles
    Protected Contents() As Char
    Protected Rear As Integer
    Protected MaxSize As Integer
    Protected Pointer As Integer
    Protected Ignore As Char = " "

    Public Sub New(ByVal MaxSize As Integer)
        Randomize()
        Rear = -1
        Me.MaxSize = MaxSize
        ReDim Contents(Me.MaxSize - 1)
        For Count = 0 To Me.MaxSize - 1
            Contents(Count) = ""
            Add() ' add random character to fill array
        Next
        Pointer = 0
    End Sub

    Public Function IsEmpty() As Boolean
        If Rear = -1 Then
            Return True
        Else
            Return False
        End If
    End Function

    Public Sub SetQueue(ByVal characters As String)
        If characters.Length < MaxSize Then

            Rear = characters.Length - 1
            ReDim Contents(characters.Length - 1)

        Else

        End If
        Pointer = 0
        Rear = Contents.Length - 1
        For i As Integer = 0 To Contents.Length - 1
            Contents(i) = characters.Chars(i)
        Next
    End Sub

    Public Function GetString() As String
        Dim out As String = ""
        For i As Integer = 0 To Contents.Length - 1
            out = out + Contents(i)
        Next
        Return out
    End Function

    Public Function Remove() As Char
        Dim RandNo As Integer
        Dim Item As Char
        If IsEmpty() Then
            Return Nothing
        Else
            Item = Contents(Pointer) ' get first item in the array
            RandNo = Int(Rnd() * 26) ' random integer between 1 and 26
            Contents(Pointer) = Chr(65 + RandNo) ' gives the character associated with ascii value
            If Pointer = Rear Then
                Pointer = 0
            Else
                Pointer += 1
            End If
            Return Item
        End If
    End Function

    Protected Sub Add()
        Dim RandNo As Integer
        Dim done As Boolean = False
        Do While done = False
            If Rear < MaxSize - 1 Then ' if array is full do nothing
                Dim Letters As String = "aaaaaabbbbccccddddeeeeeeeffffgggghhhhiiiiiijjkkllmmmnnnoooooopppqrrrssstttuuuuuuvwxyz_" ' different distribution of letters
                RandNo = Int(Rnd() * Letters.Length) ' random integer between 1 and 26
                Rear += 1 ' increment rear

                If Ignore <> Char.ToUpper(Letters.Chars(RandNo)) Then
                    Contents(Rear) = Char.ToUpper(Letters.Chars(RandNo))
                    Ignore = Char.ToUpper(Letters.Chars(RandNo)) ' gives the character associated with ascii value
                    done = True
                End If
            End If
        Loop
    End Sub

    Public Sub Show()
        Dim Counter As Integer = Pointer
        If Rear <> -1 Then
            Console.WriteLine()
            Console.Write("The contents of the queue are: ")

            For Each Item In Contents
                Console.Write(Contents(Counter))
                If Counter = Rear Then
                    Counter = 0
                Else
                    Counter += 1
                End If
            Next
            Console.WriteLine()
        End If
    End Sub
End Class

Public Class CircularQueue
    Protected Contents() As Char
    Protected Rear As Integer
    Protected MaxSize As Integer
    Protected Pointer As Integer
    Protected Ignore As Char = " "

    Public Sub New(ByVal MaxSize As Integer)
        Me.MaxSize = MaxSize
        ReDim Contents(Me.MaxSize - 1)
        Rear = 0
        Pointer = 0
    End Sub

    Private Function GetSize()
        Return (MaxSize - Pointer) - (MaxSize - Rear)
    End Function

    Public Function IsEmpty() As Boolean
        If Rear = 0 Then
            Return True
        Else
            Return False
        End If
    End Function

    Public Function GetString() As String
        Dim out As String = ""
        For i As Integer = 0 To Contents.Length - 1
            out = out + Contents(i)
        Next
        Return out
    End Function

    Public Function Remove() As Char
        If Not IsEmpty() Then
            Dim Item As Char = Contents(Pointer)

            Pointer += 1
            If Pointer > MaxSize Then
                Pointer = 0
            End If

            Return Item
        Else
            Return Nothing
        End If
    End Function
    Protected Sub Add()
        Dim RandNo As Integer
        If GetSize() < MaxSize Then ' if array is full do nothing
            Dim Letters As String = "aaaaaabbbbccccddddeeeeeeeffffgggghhhhiiiiiijjkkllmmmnnnoooooopppqrrrssstttuuuuuuvwxyz_" ' different distribution of letters
            RandNo = Int(Rnd() * Letters.Length) ' random integer between 1 and 26
            Contents(Rear) = Char.ToUpper(Letters.Chars(RandNo))

            Rear += 1
            If Rear > MaxSize Then
                Rear = 0
            End If

        End If
    End Sub

    Public Sub Show()
        Dim Counter As Integer = Pointer
        If IsEmpty() Then
            Console.WriteLine()
            Console.Write("The contents of the queue are: ")

            For Each Item In Contents
                Console.Write(Contents(Counter))
                If Counter = Rear Then
                    Counter = 0
                Else
                    Counter += 1
                End If
            Next
            Console.WriteLine()
        End If
    End Sub
End Class

Public Class Player
    Dim PlayerName As String
    Dim PlayerScore As Integer
    Dim TilesPlayed As Integer

    Public Function GetPlayerName() As String
        Return PlayerName
    End Function

    Public Function GetPlayerScore() As Integer
        Return PlayerScore
    End Function

    Public Function GetTilesPlayed() As Integer
        Return TilesPlayed
    End Function

    Public Sub New(ByVal PlayerName As String, ByVal PlayerScore As Integer, ByVal TilesPlayer As Integer)
        Me.PlayerName = PlayerName
        Me.PlayerScore = PlayerScore
        Me.TilesPlayed = TilesPlayed
    End Sub
End Class


Module Module1
    Sub Main()
        Dim Word As String
        Dim LoadGame As Boolean = False
        Dim MaxHandSize As Integer
        Dim MaxTilesPlayed As Integer
        Dim NoOfEndOfTurnTiles As Integer
        Dim StartHandSize As Integer
        Dim Choice As String
        Dim AllowedWords As New List(Of String)
        Dim RudeWords As New List(Of String)
        Dim TileDictionary As New Dictionary(Of Char, Integer)()
        Console.WriteLine("++++++++++++++++++++++++++++++++++++++")
        Console.WriteLine("+ Welcome to the WORDS WITH AQA game +")
        Console.WriteLine("++++++++++++++++++++++++++++++++++++++")
        Console.WriteLine()
        Console.WriteLine()
        LoadWords(AllowedWords, "C:\Users\alexw\Desktop\aqawords.txt")
        LoadWords(RudeWords, "C:\Users\alexw\Desktop\rudewords.txt")
        RudeWords.Sort()
        TileDictionary = CreateTileDictionary()
        MaxHandSize = 20
        MaxTilesPlayed = 50
        NoOfEndOfTurnTiles = 3
        StartHandSize = 15
        Choice = ""
        While Choice <> "9" ' 9 leaves the game
            DisplayMenu()
            Console.Write("Enter your choice: ")
            Choice = Console.ReadLine()
            If Choice = "1" Then
                PlayGame(AllowedWords, TileDictionary, True, StartHandSize, MaxHandSize, MaxTilesPlayed, NoOfEndOfTurnTiles, RudeWords, False, False)
            ElseIf Choice = "2" Then
                PlayGame(AllowedWords, TileDictionary, False, 15, MaxHandSize, MaxTilesPlayed, NoOfEndOfTurnTiles, RudeWords, False, False)
            ElseIf Choice = "3" Then
                Console.Write("Word:")
                Word = Console.ReadLine().ToUpper
                AddWordToList(Word, AllowedWords, RudeWords)
            ElseIf Choice = "4" Then
                PlayGame(AllowedWords, TileDictionary, False, StartHandSize, MaxHandSize, MaxTilesPlayed, NoOfEndOfTurnTiles, RudeWords, True, False)
            ElseIf Choice = "5" Then
                DisplayHighScore()
            ElseIf Choice = "6" Then
                'play vs computer
            End If
        End While
    End Sub

    Function CreateTileDictionary() As Dictionary(Of Char, Integer)
        Dim TileDictionary As New Dictionary(Of Char, Integer)()
        For Count = 0 To 25 ' creates dictionary of characters and their corresponding values
            If Array.IndexOf({0, 4, 8, 13, 14, 17, 18, 19}, Count) > -1 Then
                TileDictionary.Add(Chr(65 + Count), 1) ' characters with a value of 1
            ElseIf Array.IndexOf({1, 2, 3, 6, 12, 15, 20}, Count) > -1 Then
                TileDictionary.Add(Chr(65 + Count), 2) ' characters with a value of 2
            ElseIf Array.IndexOf({5, 7, 10, 21, 24}, Count) > -1 Then
                TileDictionary.Add(Chr(65 + Count), 3) ' characters with a value of 3
            ElseIf Array.IndexOf({11, 22, 25}, count) > -1 Then
                TileDictionary.Add(Chr(65 + Count), 4) ' characters with a value of 4
            Else
                TileDictionary.Add(Chr(65 + Count), 5) ' characters with a value of 5
            End If
        Next
        TileDictionary.Add("_", 6)
        Return TileDictionary
    End Function

    Sub DisplayTileValues(ByVal TileDictionary As Dictionary(Of Char, Integer), ByRef AllowedWords As List(Of String))
        Console.WriteLine()
        Console.WriteLine("TILE VALUES")
        Console.WriteLine()
        For Each Tile As KeyValuePair(Of Char, Integer) In TileDictionary
            Console.WriteLine("Points for " & Tile.Key & ": " & Tile.Value)
        Next
        Console.WriteLine()
    End Sub

    Function GetStartingHand(ByRef TileQueue As QueueOfTiles, ByVal StartHandSize As Integer) As String
        Dim Hand As String
        Hand = ""
        For Count = 0 To StartHandSize - 1 ' remove tile from tile queue for each index in players hand
            Hand += TileQueue.Remove()
        Next
        Return Hand
    End Function

    Sub LoadWords(ByRef AllowedWords As List(Of String), ByVal Path As String)
        Try
            Dim FileReader As New System.IO.StreamReader(Path)
            While FileReader.EndOfStream <> True
                AllowedWords.Add(FileReader.ReadLine().Trim().ToUpper()) ' reads each line of text file and sets to upper, trims spaces
            End While
            FileReader.Close()
        Catch
            Console.WriteLine(".txt file is missing")
            AllowedWords.Clear()
            End
        End Try
    End Sub

    Function CheckWordIsInTiles(ByVal Word As String, ByVal PlayerTiles As String) As Boolean
        For Count = 0 To Len(Word) - 1 ' checks each character in the word
            If PlayerTiles.Contains(Word(Count)) Then ' if character is found then set that character to nothing
                PlayerTiles = Replace(PlayerTiles, Word(Count), "", , 1) ' this makes sure there are no repeats
            Else
                If PlayerTiles.Contains("_") Then ' check if blank tile is found
                    PlayerTiles = Replace(PlayerTiles, "_", "", , 1)
                Else
                    Return False ' if character is not found then invalid word
                End If
            End If
        Next
        Return True
    End Function

    Function CheckWordIsInList(ByVal Word As String, ByRef AllowedWords As List(Of String)) As Boolean
        Dim StartIndex As Integer = 0
        Dim EndIndex As Integer = AllowedWords.Count - 1

        If AllowedWords.Count = 0 Then
            Return False
        End If

        While (StartIndex < EndIndex)  ' binary search against each word
            Dim Index As Integer = Math.Round((EndIndex + StartIndex) / 2)
            Dim PivotWord As String = AllowedWords(Index)
            Dim Comparison As Integer = String.Compare(Word, PivotWord)
            If Comparison = 0 Then
                Return True
            ElseIf Comparison < 0 Then
                EndIndex = Index - 1
            ElseIf Comparison > 0 Then
                StartIndex = Index + 1
            End If
        End While
        Return False
    End Function

    Sub AddEndOfTurnTiles(ByRef TileQueue As QueueOfTiles, ByRef PlayerTiles As String, ByVal NewTileChoice As String, ByVal Choice As String)
        Dim NoOfEndOfTurnTiles As Integer
        If NewTileChoice = "1" Then
            NoOfEndOfTurnTiles = Len(Choice)
        ElseIf NewTileChoice = "2" Then
            NoOfEndOfTurnTiles = 3
        Else
            NoOfEndOfTurnTiles = Len(Choice) + 3
        End If
        For Count = 0 To NoOfEndOfTurnTiles - 1 ' default is 0 end of turn tiles
            PlayerTiles += TileQueue.Remove()
        Next
    End Sub

    Sub FillHandWithTiles(ByRef TileQueue As QueueOfTiles, ByRef PlayerTiles As String, ByVal MaxHandSize As Integer)
        While Len(PlayerTiles) <= MaxHandSize
            PlayerTiles += TileQueue.Remove()
        End While
    End Sub

    Function GetScoreForWord(ByVal Word As String, ByVal TileDictionary As Dictionary(Of Char, Integer)) As Integer
        Dim Score As Integer
        Score = 0
        For Count = 0 To Len(Word) - 1
            Score += TileDictionary(Word(Count)) ' gets score of each character in word
        Next
        If Len(Word) > 7 Then
            Score += 20
        ElseIf Len(Word) > 5 Then
            Score += 5
        ElseIf Len(Word) <= 3 Then
            Score -= 1
        End If
        Return Score
    End Function

    Sub UpdateAfterAllowedWord(ByVal Word As String, ByRef PlayerTiles As String, ByRef PlayerScore As Integer, ByRef PlayerTilesPlayed As Integer, ByVal TileDictionary As Dictionary(Of Char, Integer), ByRef AllowedWords As List(Of String))
        PlayerTilesPlayed += Len(Word)
        For Each Letter In Word
            PlayerTiles = Replace(PlayerTiles, Letter, "", , 1) ' replace player tiles from tile queue
        Next
        PlayerScore += GetScoreForWord(Word, TileDictionary) ' add score
    End Sub

    Sub UpdateScoreWithPenalty(ByRef PlayerScore As Integer, ByVal PlayerTiles As String, ByVal TileDictionary As Dictionary(Of Char, Integer))
        For Count = 0 To Len(PlayerTiles) - 1
            PlayerScore -= (6 - TileDictionary(PlayerTiles(Count))) ' subract value of each character in players hand from player score
        Next
    End Sub

    Function GetChoice()
        Dim Choice As String
        Console.WriteLine()
        Console.WriteLine("Either:")
        Console.WriteLine("     enter the word you would like to play OR")
        Console.WriteLine("     press 1 to display the letter values OR")
        Console.WriteLine("     press 2 to swap all your tiles OR")
        Console.WriteLine("     press 4 to view the tile queue OR")
        Console.WriteLine("     press 7 to view your tiles again OR")
        Console.WriteLine("     press 9 to shuffle your tiles OR")
        Console.WriteLine("     press 0 to fill hand and stop the game.")
        Console.Write("> ")
        Choice = Console.ReadLine()
        Console.WriteLine()
        Choice = Choice.ToUpper()
        Return Choice
    End Function

    Function GetNewTileChoice()
        Dim NewTileChoice As String
        NewTileChoice = ""
        While Array.IndexOf({"1", "2", "3", "4"}, NewTileChoice) = -1 ' while new tile choice does not equal 1, 2, 3, 4
            Console.WriteLine("Do you want to:")
            Console.WriteLine("     replace the tiles you used (1) OR")
            Console.WriteLine("     get three extra tiles (2) OR")
            Console.WriteLine("     replace the tiles you used and get three extra tiles (3) OR")
            Console.WriteLine("     get no new tiles (4)?")
            Console.Write("> ")
            NewTileChoice = Console.ReadLine()
        End While
        Return NewTileChoice
    End Function

    Sub DisplayTilesInHand(ByVal PlayerTiles As String, ByVal TileDictionary As Dictionary(Of Char, Integer))
        Console.WriteLine()
        Console.Write("Your current hand: ")
        For i As Integer = 0 To PlayerTiles.Length - 1
            Console.Write(PlayerTiles(i) & "(" & TileDictionary.Item(PlayerTiles(i)) & ") ")
        Next
        Console.WriteLine()
    End Sub

    Sub ShuffleHand(ByRef PlayerTiles As String)
        Dim Temp As Char
        Dim RandNo1 As Integer
        Dim RandNo2 As Integer
        Dim Chars() As Char = PlayerTiles.ToCharArray
        For i As Integer = 0 To 30
            RandNo1 = Int(Rnd() * Chars.Length)
            RandNo2 = Int(Rnd() * PlayerTiles.Length)
            Temp = Chars(RandNo1)
            Chars(RandNo1) = Chars(RandNo2)
            Chars(RandNo2) = Temp
        Next
        PlayerTiles = ""
        For i As Integer = 0 To Chars.Length - 1
            PlayerTiles = PlayerTiles + Chars(i)
        Next
        Console.WriteLine()
        Console.WriteLine("Your new hand: " & PlayerTiles)
    End Sub

    Sub HaveTurn(ByVal PlayerName As String, ByRef PlayerTiles As String, ByRef PlayerTilesPlayed As String, ByRef PlayerScore As Integer, ByVal TileDictionary As Dictionary(Of Char, Integer), ByRef TileQueue As QueueOfTiles, ByRef AllowedWords As List(Of String), ByVal MaxHandSize As Integer, ByVal NoOfEndOfTurnTiles As Integer, ByRef RudeWords As List(Of String), ByRef PlayerWords As List(Of String))
        Dim NewTileChoice As String
        Dim Choice As String ' choice stores choice or word
        Dim ValidChoice As Boolean
        Dim ValidWord As Boolean
        Dim RudeWord As Boolean
        Console.WriteLine()
        Console.WriteLine(PlayerName & " it is your turn.")
        DisplayTilesInHand(PlayerTiles, TileDictionary)
        NewTileChoice = "2"
        ValidChoice = False
        While Not ValidChoice ' repeat until valid choice
            Choice = GetChoice()
            If Choice = "1" Then
                DisplayTileValues(TileDictionary, AllowedWords)
            ElseIf Choice = "4" Then
                TileQueue.Show()
            ElseIf Choice = "7" Then
                DisplayTilesInHand(PlayerTiles, TileDictionary)
            ElseIf Choice = "9" Then
                ShuffleHand(PlayerTiles)
            ElseIf Choice = "0" Then
                FillHandWithTiles(TileQueue, PlayerTiles, MaxHandSize)
                ValidChoice = True
            ElseIf Choice = "2" Then
                Dim temp As String = ""
                For i As Integer = 0 To PlayerTiles.Count - 1
                    temp = temp + TileQueue.Remove()
                Next
                PlayerTiles = temp
                ValidChoice = False
            Else
                If Choice.Length = 0 Then ' empty word is not valid
                    ValidWord = False
                Else
                    RudeWord = CheckWordIsInList(Choice, RudeWords)
                    ValidWord = CheckWordIsInTiles(Choice, PlayerTiles) And Not RudeWord ' if word is not in tiles then not valid
                End If
                If ValidWord Then
                    ValidWord = CheckWordIsInList(Choice, AllowedWords)
                    If ValidWord Then
                        Console.WriteLine()
                        Console.WriteLine("Valid word. " & Choice & " scores " & GetScoreForWord(Choice, TileDictionary).ToString & " points")
                        Console.WriteLine()
                        PlayerWords.Add(Choice)
                        UpdateAfterAllowedWord(Choice, PlayerTiles, PlayerScore, PlayerTilesPlayed, TileDictionary, AllowedWords) ' update score and tiles
                        NewTileChoice = GetNewTileChoice() ' get new tiles
                    End If
                End If
                If Not ValidWord Then
                    Console.WriteLine()
                    Console.WriteLine("Not a valid attempt, you lose your turn.")
                    Console.WriteLine()
                    If RudeWord Then
                        Console.WriteLine("You entered a rude word minus 50 pts")
                        PlayerScore -= 50
                    End If
                End If
                If NewTileChoice <> "4" Then
                    AddEndOfTurnTiles(TileQueue, PlayerTiles, NewTileChoice, Choice)
                End If
                Console.WriteLine()
                Console.WriteLine("Your word was: " & Choice)
                Console.WriteLine("Your new score is: " & PlayerScore)
                Console.WriteLine("You have played " & PlayerTilesPlayed & " tiles so far in this game.")
                ValidChoice = True
            End If
        End While
    End Sub

    Sub DisplayWinner(ByVal PlayerOneScore As Integer, ByVal PlayerTwoScore As Integer, ByRef PlayerOneWords As List(Of String), ByRef PlayerTwoWords As List(Of String), ByRef TileDictionary As Dictionary(Of Char, Integer))
        Console.WriteLine()
        Console.WriteLine("**** GAME OVER! ****")
        Console.WriteLine()
        Console.WriteLine("Player One your score is " & PlayerOneScore)
        If PlayerOneWords.Count <> 0 Then Console.WriteLine("Valid Words Played:")
        For i As Integer = 0 To PlayerOneWords.Count - 1
            Console.WriteLine(i + 1 & ". " & PlayerOneWords(i) & " Score: " & GetScoreForWord(PlayerOneWords(i), TileDictionary))
        Next
        Console.WriteLine("Player Two your score is " & PlayerTwoScore)
        If PlayerTwoWords.Count <> 0 Then Console.WriteLine("Valid Words Played:")
        For i As Integer = 0 To PlayerTwoWords.Count - 1
            Console.WriteLine(i + 1 & ". " & PlayerTwoWords(i) & " Score: " & GetScoreForWord(PlayerTwoWords(i), TileDictionary))
        Next
        If PlayerOneScore > PlayerTwoScore Then
            Console.WriteLine("Player One wins!")
            SaveHighScore(PlayerOneScore, "Player One")
        ElseIf PlayerTwoScore > PlayerOneScore Then
            Console.WriteLine("Player Two wins!")
            SaveHighScore(PlayerTwoScore, "Player Two")
        Else
            Console.WriteLine("It is a draw!")
        End If
        Console.WriteLine()
    End Sub

    Sub PlayGame(ByRef AllowedWords As List(Of String), ByVal TileDictionary As Dictionary(Of Char, Integer), ByVal RandomStart As Boolean, ByVal StartHandSize As Integer, ByVal MaxHandSize As Integer, ByVal MaxTilesPlayed As Integer, ByVal NoOfEndOfTurnTiles As Integer, ByRef RudeWords As List(Of String), ByVal LoadGame As Boolean, ByVal PlayAI As Boolean)

        Dim PlayerOneScore As Integer
        Dim PlayerTwoScore As Integer
        Dim PlayerOneTilesPlayed As Integer
        Dim PlayerTwoTilesPlayed As Integer
        Dim PlayerOneTiles As String = ""
        Dim PlayerTwoTiles As String = ""
        Dim TileQueue As New QueueOfTiles(20)
        Dim PlayerTurn As Integer
        Dim PlayerOneWords As New List(Of String)
        Dim PLayerTwoWords As New List(Of String)
        PlayerOneScore = 50
        PlayerTwoScore = 50
        PlayerOneTilesPlayed = 0
        PlayerTwoTilesPlayed = 0

        If RandomStart Then
            PlayerOneTiles = GetStartingHand(TileQueue, StartHandSize)
            PlayerTwoTiles = GetStartingHand(TileQueue, StartHandSize)
            PlayerTurn = 1
        ElseIf LoadGame Then
            LoadGameSave(PlayerOneTiles, PlayerTwoTiles, TileQueue, PlayerOneScore, PlayerTwoScore, PlayerOneTilesPlayed, PlayerTwoTilesPlayed, PlayerTurn)
        Else
            PlayerOneTiles = "BTAHANDENONSARJ"
            PlayerTwoTiles = "CELZXIOTNESMUAA"
            PlayerTurn = 1
        End If
        While (PlayerOneTilesPlayed <= MaxTilesPlayed And PlayerTwoTilesPlayed <= MaxTilesPlayed And Len(PlayerOneTiles) < MaxHandSize And Len(PlayerTwoTiles) < MaxHandSize) Or PlayerTurn = 2
            If PlayerTurn = 1 Then
                HaveTurn("Player One", PlayerOneTiles, PlayerOneTilesPlayed, PlayerOneScore, TileDictionary, TileQueue, AllowedWords, MaxHandSize, NoOfEndOfTurnTiles, RudeWords, PlayerOneWords)
                Console.WriteLine()
                Console.Write("Press Enter to continue")
                Console.ReadLine()
                PlayerTurn = 2
            Else
                HaveTurn("Player Two", PlayerTwoTiles, PlayerTwoTilesPlayed, PlayerTwoScore, TileDictionary, TileQueue, AllowedWords, MaxHandSize, NoOfEndOfTurnTiles, RudeWords, PLayerTwoWords)
                Console.WriteLine()
                Console.Write("Press Enter to continue")
                Console.ReadLine()
                PlayerTurn = 1
            End If
            SaveGame(PlayerOneTiles, PlayerTwoTiles, TileQueue.GetString, PlayerOneScore, PlayerTwoScore, PlayerOneTilesPlayed, PlayerTwoTilesPlayed, PlayerTurn)
        End While
        UpdateScoreWithPenalty(PlayerOneScore, PlayerOneTiles, TileDictionary)
        UpdateScoreWithPenalty(PlayerTwoScore, PlayerTwoTiles, TileDictionary)
        DisplayWinner(PlayerOneScore, PlayerTwoScore, PlayerOneWords, PLayerTwoWords, TileDictionary)
    End Sub
    Sub AddWordToList(ByVal Word As String, ByRef AllowedWords As List(Of String), ByRef RudeWords As List(Of String))
        Dim Rudeword As Boolean = CheckWordIsInList(Word, RudeWords)
        Dim Exists As Boolean = CheckWordIsInList(Word, AllowedWords)
        If Rudeword Then
            Console.WriteLine()
            Console.WriteLine("Word is a rude word")
            Console.WriteLine()
            Console.WriteLine("Word not added")
            Console.WriteLine()
        ElseIf Exists Then
            Console.WriteLine()
            Console.WriteLine("Word already exists in the game")
            Console.WriteLine()
            Console.WriteLine("Word not added")
            Console.WriteLine()
        Else
            AllowedWords.Add(Word)
            AllowedWords.Sort()
            Dim words() As String = AllowedWords.ToArray
            IO.File.WriteAllLines("C:\Users\alexw\Desktop\aqawords.txt", words)

            Console.WriteLine()
            Console.WriteLine("Word Successfully added")
            Console.WriteLine()
        End If
    End Sub

    Sub SaveGame(ByVal PlayerOneTiles As String, ByVal PlayerTwoTiles As String, ByVal TileQueue As String, ByVal PlayerOneScore As Integer, ByVal PlayerTwoScore As Integer, ByVal PlayerOneTilesPlayed As Integer, ByVal PlayerTwoTilesPlayed As Integer, ByVal PlayerTurn As Integer)
        Console.WriteLine()
        Console.WriteLine("Do you want to save the game (Y/N)?")
        Dim Choice As String = Console.ReadLine()
        If Choice.ToUpper = "Y" Then
            Dim Lines() As String = {PlayerOneTiles, PlayerTwoTiles, TileQueue, PlayerOneScore.ToString, PlayerTwoScore.ToString, PlayerOneTilesPlayed.ToString, PlayerTwoTilesPlayed.ToString, PlayerTurn.ToString}
            IO.File.WriteAllLines("C:\Users\alexw\Desktop\aqawordsave.txt", Lines)
            Console.WriteLine()
            Console.WriteLine("Game Successfully saved")
            Console.WriteLine()
            Console.ReadLine()
            End
        Else
            Console.WriteLine()
            Console.WriteLine("Game Continued")
            Console.WriteLine()
        End If
    End Sub

    Sub LoadGameSave(ByRef PlayerOneTiles As String, ByRef PlayerTwoTiles As String, ByRef TileQueue As QueueOfTiles, ByRef PlayerOneScore As Integer, ByRef PlayerTwoScore As Integer, ByRef PlayerOneTilesPlayed As Integer, ByRef PlayerTwoTilesPlayed As Integer, ByRef PlayerTurn As Integer)
        Try
            Dim FileReader As New System.IO.StreamReader("C:\Users\alexw\Desktop\aqawordsave.txt")
            PlayerOneTiles = FileReader.ReadLine().Trim().ToUpper()
            PlayerTwoTiles = FileReader.ReadLine().Trim().ToUpper()
            TileQueue.SetQueue(FileReader.ReadLine().Trim().ToUpper())
            PlayerOneScore = Integer.Parse(FileReader.ReadLine().Trim())
            PlayerTwoScore = Integer.Parse(FileReader.ReadLine().Trim())
            PlayerOneTilesPlayed = Integer.Parse(FileReader.ReadLine().Trim())
            PlayerTwoTilesPlayed = Integer.Parse(FileReader.ReadLine().Trim())
            PlayerTurn = Integer.Parse(FileReader.ReadLine().Trim())
            FileReader.Close()
        Catch ex As Exception
            Console.WriteLine("Save file could not be found")
        End Try
    End Sub
    Private Class HighScore
        Private PlayerName As String
        Private PlayerScore As Integer

        Public Function GetPlayerName() As String
            Return PlayerName
        End Function

        Public Function GetPlayerScore() As Integer
            Return PlayerScore
        End Function

        Public Sub New(ByVal PlayerName As String, ByVal PlayerScore As Integer)
            Me.PlayerName = PlayerName
            Me.PlayerScore = PlayerScore
        End Sub
    End Class

    Sub SaveHighScore(ByRef PlayerScore As Integer, ByRef PlayerName As String)
        Dim Lines As New Collection
        Dim Temp As New Collection
        Dim Write(0) As String
        Dim FileReader As New System.IO.StreamReader("C:\Users\alexw\Desktop\aqawordshighscore.txt")
        Dim count As Integer = 0

        Do Until FileReader.EndOfStream = True
            Lines.Add(New HighScore(FileReader.ReadLine().Trim(), Integer.Parse(FileReader.ReadLine().Trim()))) ' reads each line of text file and sets to upper, trims spaces
        Loop

        Dim index As Integer = Lines.Count + 1

        For i As Integer = Lines.Count To 1 Step -1
            If PlayerScore > Lines.Item(i).GetPlayerScore() Then
                index = i
            End If
        Next

        If index < 10 Then
            For i As Integer = 1 To index - 1
                Temp.Add(New HighScore(Lines.Item(i).GetPlayerName(), Lines.Item(i).GetPlayerScore()))
            Next

            Temp.Add(New HighScore(PlayerName, PlayerScore)) ' fix this so keys are different

            For i As Integer = index To Lines.Count
                Temp.Add(New HighScore(Lines.Item(i).GetPlayerName(), Lines.Item(i).GetPlayerScore()))
            Next

            If Lines.Count = 11 Then
                Lines.Remove(11)
            End If
        End If
        Lines = Temp

        For i As Integer = 1 To Lines.Count
            ReDim Preserve Write(count)
            Write(count) = Lines.Item(i).GetPlayerName()
            count += 1
            ReDim Preserve Write(count)
            Write(count) = Lines.Item(i).GetPlayerScore()
            count += 1
        Next

        FileReader.Close()

        Try
            IO.File.WriteAllLines("C:\Users\alexw\Desktop\aqawordshighscore.txt", Write)
            FileReader.Close()
        Catch ex As Exception
            Console.WriteLine("High score couldn't be recorded")
        End Try
    End Sub

    Sub DisplayHighScore()
        Try
            Dim Lines As New List(Of String)
            Dim FileReader As New System.IO.StreamReader("C:\Users\alexw\Desktop\aqawordshighscore.txt")
            Dim count As Integer = 0
            While FileReader.EndOfStream <> True
                count += 1
                Lines.Add(count.ToString & ". " & FileReader.ReadLine().Trim() & " acheived a score of: " & FileReader.ReadLine.Trim()) ' reads each line of text file and sets to upper, trims spaces
            End While
            Console.WriteLine()
            Console.WriteLine("////////////////////")
            Console.WriteLine("HIGH SCORE TABLE")
            Console.WriteLine("////////////////////")
            Console.WriteLine()

            If Lines.Count = 0 Then
                Console.WriteLine("NO HIGHSCORES")
            Else
                For i As Integer = 0 To Lines.Count - 1
                    Console.WriteLine(Lines.Item(i))
                Next
            End If
        Catch ex As Exception
            Console.WriteLine()
            Console.WriteLine("High score table couldn't be found")
            Console.WriteLine()
        End Try

        Console.WriteLine("Press enter to continue...")
        Console.ReadLine()
    End Sub

    Sub DisplayMenu()
        Console.WriteLine()
        Console.WriteLine("=========")
        Console.WriteLine("MAIN MENU")
        Console.WriteLine("=========")
        Console.WriteLine()
        Console.WriteLine("1. Play game with random start hand")
        Console.WriteLine("2. Play game with training start hand")
        Console.WriteLine("3. Add a word to AQAWords")
        Console.WriteLine("4. Load a recently saved game")
        Console.WriteLine("5. Display highscore table")
        Console.WriteLine("6. Play game against the computer")
        Console.WriteLine("9. Quit")
        Console.WriteLine()
    End Sub
End Module
