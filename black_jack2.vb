Module Module1

    Class BlackJack
        Private Deck(52) As String
        Private deckpointer As Integer
        Private Players As Integer
        Private PlayerHand(,) As String
        Private PlayerTotal() As Integer
        Private PlayCardCount() As Integer
        Private PlayerTurn As Integer
        Private SplitTrue As Boolean
        Public Sub New()
            Dim counter As Integer = 0
            Dim input As Char
            For suit = 1 To 4
                For number = 1 To 13
                    counter = counter + 1
                    If number = 1 Or number > 10 Then
                        input = ConvertCard(number)
                        Deck(counter) = input & " Of " & GetSuit(suit)
                    Else
                        Deck(counter) = number & " Of " & GetSuit(suit)
                    End If
                Next
            Next
            Console.WriteLine("How Many Players Are Playing:")
            Players = Console.ReadLine()
            ReDim PlayerTotal(Players)
            ReDim PlayerHand(Players, 5)
            deckpointer = 1
            ReDim PlayCardCount(Players)
            shuffle()
        End Sub
        Private Function ConvertCard(number)
            Dim output As Char
            If number = 11 Then
                output = "J"
            ElseIf number = 12 Then
                output = "Q"
            ElseIf number = 13 Then
                output = "K"
            ElseIf number = 1 Then
                output = "A"
            Else
                Return Convert.ToChar(number)
            End If
            Return output
        End Function
        Public Function GetSuit(suit)
            If suit = 1 Then
                Return "Hearts"
            ElseIf suit = 2 Then
                Return "Diamonds"
            ElseIf suit = 3 Then
                Return "Clubs"
            ElseIf suit = 4 Then
                Return "Spades"
            End If
        End Function
        Public Function GetCard()
            deckpointer = Increment(deckpointer, 52, 1)
            Return Deck(deckpointer)
        End Function
        Public Function Increment(value, parameter, reset)
            If value = parameter Then
                value = reset
            Else
                value = value + 1
            End If
            Return value
        End Function
        Public Sub PlayHand()
            PlayerTurn = 1
            For x = 1 To Players
                DrawHand(x)
                Console.ReadLine()
            Next
            Do Until PlayerTurn = Players + 1
                Turn()
                PlayerTurn = Increment(PlayerTurn, Players + 1, 1)
            Loop
            Console.Clear()
            Console.WriteLine("Player " & CheckWinner() & " wins!")
        End Sub
        Private Sub DrawHand(Player)
            Console.Clear()
            PlayerHand(Player, 1) = GetCard()
            Console.WriteLine("Player " & Player & " receives " & PlayerHand(Player, 1))
            PlayerHand(Player, 2) = GetCard()
            Console.WriteLine("Player " & Player & " receives " & PlayerHand(Player, 2))
        End Sub

        Private Function CheckWinner()
            Dim Winner As Integer = 1
            Dim number As Integer = PlayerTotal(1)
            For x = 1 To Players - 1
                If HighestNumber(number, PlayerTotal(x + 1)) = number Then
                    number = number
                Else
                    number = PlayerTotal(x + 1)
                    Winner = x + 1
                End If
            Next
            For x = 1 To Players - 1
                If PlayCardCount(x) >= 5 Then
                    Winner = x
                End If
            Next
            Return Winner
        End Function
        Private Function HighestNumber(num1, num2)
            If num1 > num2 Then
                Return num1
            ElseIf num2 > num1 Then
                Return num2
            Else
                Return num2
            End If
        End Function
        Private Function Twist(cardnumber As Integer)
            Dim card As String = GetCard()
            Console.Clear()
            Console.WriteLine("You receive the " & card)
            Console.ReadLine()
            Console.Clear()
            Return card
        End Function
        Private Sub shuffle()
            For x = 1 To 52
                SwapCards()
            Next
        End Sub
        Private Sub SwapCards()
            Dim randnumber As Integer
            Dim Store As String
            Dim randnumber2 As Integer
            Dim Store2 As String
            Randomize()
            randnumber = CInt(Math.Ceiling(Rnd() * 52))
            Store = Deck(randnumber)
            Randomize()
            randnumber2 = CInt(Math.Ceiling(Rnd() * 52))
            Store2 = Deck(randnumber2)
            Deck(randnumber) = Store2
            Deck(randnumber2) = Store
        End Sub
        Private Function LargerNumber(num1 As Integer, num2 As Integer)
            If num1 > num2 Then
                Return num1
            Else
                Return num2
            End If
        End Function

        Private Sub split(card1 As String, card2 As String)
            Dim Splithand(2, 5) As String
            Dim SplitTotal(2) As Integer
            Dim SplitCardCount(2) As Integer
            Dim cardnumber() As Integer = {1, 1, 1}
            Dim choice As Char
            Dim hand As Integer
            Splithand(1, 1) = card1
            Splithand(2, 1) = card2
            Do Until hand = 3
                Console.WriteLine("Which hand do you want to access?")
                Console.Clear()
                hand = SplitMenu()
                If hand = 3 Then
                    GoTo EndTurn
                End If
                ClearUp(cardnumber(hand), Splithand, hand)
                choice = Menu()
                If CheckBust(FindTotal(cardnumber(hand), hand, Splithand)) = True Then
                    If choice = "1" Then
                        cardnumber(hand) = cardnumber(hand) + 1
                        Splithand(hand, cardnumber(hand)) = Twist(cardnumber(hand))
                    ElseIf choice = "2" Then
                        Console.WriteLine("You finish this hand with a total of " & FindTotal(cardnumber(hand), hand, Splithand))
                        SplitTotal(hand) = FindTotal(cardnumber(hand), hand, Splithand)
                        SplitCardCount(hand) = cardnumber(hand)
                        Console.ReadLine()
                    ElseIf choice = "3" Then
                        If FindTotal(cardnumber(hand), hand, Splithand) = 14 Or FindTotal(cardnumber(hand), hand, Splithand) = 13 And cardnumber(hand) = 2 Then
                            Splithand(hand, 1) = GetCard()
                            Splithand(hand, 2) = GetCard()
                            Console.ReadLine()
                        Else
                            Console.WriteLine("You cannot burn with this hand")
                            Console.ReadLine()
                        End If
                    ElseIf choice = "4" Then

                        Console.WriteLine("You cannot split twice")
                        Console.ReadLine()
                    Else

                    End If
                Else
                    Console.WriteLine("You are Bust")
                    Console.ReadLine()
                    SplitTotal(hand) = 0
                    SplitCardCount(PlayerTurn) = 0
                    choice = "2"
                End If
EndTurn:
            Loop

            PlayerTotal = LargerNumber(SplitTotal(1), SplitTotal(2))
            PlayCardCount = LargerNumber(SplitCardCount(1), SplitCardCount(2))
        End Sub
        Private Sub Turn()
            Dim cardnumber As Integer = 2
            Dim choice As Char
            Console.Clear()
            Console.WriteLine("It is Player " & PlayerTurn & "'s turn")
            DisplayHand(cardnumber, PlayerHand, PlayerTurn)
            Do Until choice = "2"
                If CheckBust(FindTotal(cardnumber, PlayerTurn, PlayerHand)) = True Then
                    ClearUp(cardnumber, PlayerHand, PlayerTurn)
                    choice = Menu()
                    If choice = "1" Then
                        cardnumber = cardnumber + 1
                        PlayerHand(PlayerTurn, cardnumber) = Twist(cardnumber)
                    ElseIf choice = "2" Then
                        Console.WriteLine("You end your turn with a total of " & FindTotal(cardnumber, PlayerTurn, PlayerHand))
                        PlayerTotal(PlayerTurn) = FindTotal(cardnumber, PlayerTurn, PlayerHand)
                        PlayCardCount(PlayerTurn) = cardnumber
                        Console.ReadLine()
                    ElseIf choice = "3" Then
                        If FindTotal(cardnumber, PlayerTurn, PlayerHand) = 14 Or FindTotal(cardnumber, PlayerTurn, PlayerHand) = 13 And cardnumber = 2 Then
                            DrawHand(PlayerTurn)
                            Console.ReadLine()
                        Else
                            Console.WriteLine("You cannot burn with this hand")
                            Console.ReadLine()
                        End If
                    ElseIf choice = "4" Then
                        If ConvertToInteger(PlayerTurn, 1, PlayerHand) = ConvertToInteger(PlayerTurn, 2, PlayerHand) And cardnumber = 2 Then
                            split(PlayerHand(PlayerTurn, 1), PlayerHand(PlayerTurn, 2))
                            choice = "2"
                        Else
                            Console.WriteLine("You cannot split with this hand")
                            Console.ReadLine()
                            Stop
                        End If
                    Else

                    End If
                Else
                    Console.WriteLine("You are Bust")
                    Console.ReadLine()
                    PlayerTotal(PlayerTurn) = 0
                    PlayCardCount(PlayerTurn) = 0
                    choice = "2"
                End If
            Loop
        End Sub
        Private Sub ClearUp(cardnumber, data(,), dimension1)
            Console.Clear()
            DisplayHand(cardnumber, data, dimension1)
        End Sub
        Private Function CheckBust(total)
            If total > 21 Then
                Return False
            Else
                Return True
            End If
        End Function
        Private Sub DisplayHand(cardnumber, data(,), Dimension1)
            Console.WriteLine("Your Hand:")
            For x = 1 To cardnumber
                Console.WriteLine(data(Dimension1, x))
            Next
        End Sub

        Private Function FindTotal(cardnumber, playerturn, data(,))
            Dim total As Integer = 0
            For x = 1 To cardnumber
                total = total + ConvertToInteger(playerturn, x, data)
            Next
            Return total
        End Function
        Private Function ConvertToInteger(playerturn, value, data(,))
            Dim substring As String
            Dim number As Integer
            substring = (data(playerturn, value).Substring(0, 2))
            number = ConvertCardSymbol(substring)
            Return number
        End Function
        Private Function ConvertCardSymbol(symbol As String)
            If symbol = "K " Or symbol = "Q " Or symbol = "J " Then
                Return 10
            ElseIf symbol = "A " Then
                Dim value As Integer
                Console.WriteLine("What Value do you want your ace to be (1 or 11)?")
                Do Until value = 1 Or value = 11
                    value = Console.ReadLine
                    If value <> 1 And value <> 11 Then
                        Console.WriteLine("Not a valid value please re enter")
                    End If
                Loop
                Return value
            Else
                Return Convert.ToInt16(symbol)
            End If
        End Function
        Public Function Menu()
            Console.WriteLine("Twist - 1")
            Console.WriteLine("Stick - 2")
            Console.WriteLine("Burn - 3")
            Console.WriteLine("Split - 4")
            Return Console.ReadLine
        End Function
        Public Function SplitMenu()
            Console.WriteLine("AccessHand 1 - 1")
            Console.WriteLine("Accesshand 2 - 2")
            Console.WriteLine("End Turn - 3")
            Return Console.ReadLine
        End Function
    End Class

    Sub Main()

        Dim Game As BlackJack = New BlackJack()

        Game.PlayHand()

        Console.ReadLine()


    End Sub


End Module
