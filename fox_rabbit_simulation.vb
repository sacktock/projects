Module Module1
    Private Function GetValidInteger()
        Dim ValidData As Integer
        Try
            ValidData = CInt(Console.ReadLine)
        Catch ex As Exception
            Console.WriteLine(ex.Message)
        End Try
        Return ValidData
    End Function
    Public Function GetMenuOption(ByVal NumberOfOptions As Integer)

        Dim MOption As Integer
reenter:
        MOption = GetValidInteger()
        If MOption < NumberOfOptions + 1 And MOption > 0 Then
            Return MOption
        Else
            Console.WriteLine("Option entered is not a valid option")
            GoTo reenter
        End If
    End Function
    Sub Main()
        Dim MenuOption As Integer
        Dim LandscapeSize As Integer
        Dim InitialWarrenCount As Integer
        Dim InitialFoxCount As Integer
        Dim InitialBearCount As Integer
        Dim InitialTreeCount As Integer
        Dim Variability As Integer
        Dim FixedInitialLocations As Boolean
        Do
            Console.WriteLine("Predator Prey Simulation Main Menu")
            Console.WriteLine()
            Console.WriteLine("1. Run simulation with default settings")
            Console.WriteLine("2. Run simulation with custom settings")
            Console.WriteLine("3. Exit")
            Console.WriteLine()
            Console.Write("Select option: ")
            MenuOption = GetMenuOption(3)
            If MenuOption = 1 Or MenuOption = 2 Then
                If MenuOption = 1 Then
                    LandscapeSize = 15
                    InitialWarrenCount = 5
                    InitialFoxCount = 5
                    InitialBearCount = 3
                    InitialTreeCount = 10
                    Variability = 0
                    FixedInitialLocations = True
                Else
                    Console.Write("Landscape Size: ")
                    LandscapeSize = GetValidInteger()
                    Console.Write("Initial number of warrens: ")
                    InitialWarrenCount = GetValidInteger()
                    Console.Write("Initial number of foxes: ")
                    InitialFoxCount = GetValidInteger()
                    Console.Write("Initial number of Bears: ")
                    InitialBearCount = GetValidInteger()
                    Console.Write("Initial number of trees: ")
                    InitialTreeCount = GetValidInteger()
                    Console.Write("Randomness variability (percent): ")
                    Variability = GetValidInteger()
                    FixedInitialLocations = False
                End If
                Dim Sim As New Simulation(LandscapeSize, InitialWarrenCount, InitialFoxCount, InitialBearCount, Variability, FixedInitialLocations, InitialTreeCount)
            End If
        Loop While MenuOption <> 3
        Console.ReadKey()
    End Sub
    Class Tree
        Public Size As desc
        Private Age As Integer
        Private LifeSpan As Integer = 20
        Private alive As Boolean = True
        Dim rnd As New Random()
        Enum desc
            Big
            Small
        End Enum
        Public Function IsAlive()
            If alive = True Then
                Return True
            Else
                Return False
            End If
        End Function
        Public Sub New()
            Size = desc.Small
            alive = True
        End Sub
        Public Sub AdvanceTimePeriod()
            If AgeingVariability() Then
                Age += 1
            End If

            If Age >= LifeSpan / 2 Then
                Size = desc.Big
            End If
            If Age >= LifeSpan Then
                alive = False
            End If
        End Sub
        Private Function AgeingVariability() As Boolean
            Const Variability As Integer = 0.025
            If rnd.Next(0, 100) < Variability * 100 Then
                Return False
            Else
                Return True
            End If

        End Function
        Public Function ReproduceThisPeriod() As Boolean

            Dim ReproductionProbability As Double = 0.1
            If rnd.Next(0, 100) < ReproductionProbability * 100 Then
                Return True
            Else
                Return False
            End If
        End Function
    End Class
    Class Location
        Public Fox As Fox
        Public Warren As Warren
        Public Bear As Bear
        Public Tree As Tree

        Public Sub New()
            Fox = Nothing
            Warren = Nothing
            Bear = Nothing
            Tree = Nothing
        End Sub
    End Class

    Class Simulation
        Private Landscape(,) As Location
        Private TimePeriod As Integer = 0
        Private WarrenCount As Integer = 0
        Private FoxCount As Integer = 0
        Private BearCount As Integer = 0
        Private ShowDetail As Boolean = False
        Private LandscapeSize As Integer
        Private Variability As Integer
        Private Shared Rnd As New Random()

        Public Sub New(ByVal LandscapeSize As Integer, ByVal InitialWarrenCount As Integer, ByVal InitialFoxCount As Integer, ByVal InitialBearCount As Integer, ByVal Variability As Integer, ByVal FixedInitialLocations As Boolean, ByVal InitialTreeCount As Integer)
            Dim MenuOption As Integer
            Dim x As Integer
            Dim y As Integer
            Dim ViewRabbits As String
            Me.LandscapeSize = LandscapeSize
            Me.Variability = Variability
            Landscape = New Location(LandscapeSize, LandscapeSize) {}
            CreateLandscapeAndAnimals(InitialWarrenCount, InitialFoxCount, InitialBearCount, FixedInitialLocations, InitialTreeCount)
            DrawLandscape()
            Do
                Console.WriteLine()
                Console.WriteLine("1. Advance to next time period showing detail")
                Console.WriteLine("2. Advance to next time period hiding detail")
                Console.WriteLine("3. Inspect fox")
                Console.WriteLine("4. Inspect warren")
                Console.WriteLine("5. Inspect Bear")
                Console.WriteLine("6. Exit")
                Console.WriteLine()
                Console.Write("Select option: ")
                MenuOption = GetMenuOption(6)
                If MenuOption = 1 Then
                    TimePeriod += 1
                    ShowDetail = True
                    AdvanceTimePeriod()
                End If
                If MenuOption = 2 Then
                    TimePeriod += 1
                    ShowDetail = False
                    AdvanceTimePeriod()
                End If
                If MenuOption = 3 Then
                    x = InputCoordinate("x")
                    y = InputCoordinate("y")
                    If Not Landscape(x, y).Fox Is Nothing Then
                        Landscape(x, y).Fox.Inspect()
                    End If
                End If
                If MenuOption = 4 Then
                    x = InputCoordinate("x")
                    y = InputCoordinate("y")
                    If Not Landscape(x, y).Warren Is Nothing Then
                        Landscape(x, y).Warren.Inspect()
                        Console.Write("View individual rabbits (y/n)?")
                        ViewRabbits = Console.ReadLine()
                        If ViewRabbits = "y" Then
                            Landscape(x, y).Warren.ListRabbits()
                        End If
                    End If
                End If
                If MenuOption = 5 Then
                    x = InputCoordinate("x")
                    y = InputCoordinate("y")
                    If Not Landscape(x, y).Bear Is Nothing Then
                        Landscape(x, y).Bear.Inspect()
                    End If
                End If

            Loop While (WarrenCount > 0 Or FoxCount > 0 Or BearCount > 0) And MenuOption <> 6
            Console.ReadKey()
        End Sub

        Private Function InputCoordinate(ByVal CoordinateName As Char) As Integer
            Dim Coordinate As Integer
            Console.Write("  Input " & CoordinateName & " coordinate: ")
            Coordinate = CInt(Console.ReadLine())
            Return Coordinate
        End Function

        Private Sub AdvanceTimePeriod()
            Dim NewFoxCount As Integer = 0
            Dim NewBearCount As Integer = 0
            If ShowDetail Then
                Console.WriteLine()
            End If
            For x = 0 To LandscapeSize - 1
                For y = 0 To LandscapeSize - 1
                    If Not Landscape(x, y).Warren Is Nothing Then
                        If ShowDetail Then
                            Console.WriteLine("Warren at (" & x & "," & y & "):")
                            Console.Write("  Period Start: ")
                            Landscape(x, y).Warren.Inspect()
                        End If
                        If FoxCount > 0 Then
                            FoxesEatRabbitsInWarren(x, y)
                        End If
                        If Landscape(x, y).Warren.NeedToCreateNewWarren() Then
                            CreateNewWarren()
                        End If
                        Landscape(x, y).Warren.AdvanceGeneration(ShowDetail)
                        If ShowDetail Then
                            Console.Write("  Period End: ")
                            Landscape(x, y).Warren.Inspect()
                            Console.ReadKey()
                        End If
                        If Landscape(x, y).Warren.WarrenHasDiedOut() Then
                            Landscape(x, y).Warren = Nothing
                            WarrenCount -= 1
                        End If
                    End If
                    If Not Landscape(x, y).Fox Is Nothing Then
                        If ShowDetail Then
                            Console.WriteLine("Fox at (" & x & "," & y & "):")
                            Console.Write("  Period Start: ")
                            Landscape(x, y).Fox.Inspect()
                        End If
                        If BearCount > 0 Then
                            BearsEatFox(x, y)
                        End If
                    End If
                    If Not Landscape(x, y).Tree Is Nothing Then
                        If Landscape(x, y).Tree.IsAlive = False Then
                            Landscape(x, y).Tree = Nothing
                        End If
                    End If
                Next
            Next
            For x = 0 To LandscapeSize - 1
                For y = 0 To LandscapeSize - 1
                    If Not Landscape(x, y).Fox Is Nothing Then
                        If ShowDetail Then
                            Console.WriteLine("Fox at (" & x & "," & y & "): ")
                        End If
                        Landscape(x, y).Fox.AdvanceGeneration(ShowDetail)
                        If Landscape(x, y).Fox.CheckIfDead() Then
                            Landscape(x, y).Fox = Nothing
                            FoxCount -= 1
                        Else
                            If Landscape(x, y).Fox.ReproduceThisPeriod() Then
                                If ShowDetail Then
                                    Console.WriteLine("  Fox has reproduced. ")
                                End If
                                NewFoxCount += 1
                            End If
                            If ShowDetail Then
                                Landscape(x, y).Fox.Inspect()
                            End If
                            Landscape(x, y).Fox.ResetFoodConsumed()
                        End If
                    End If
                    If Not Landscape(x, y).Bear Is Nothing Then
                        If ShowDetail Then
                            Console.WriteLine("Bear at (" & x & "," & y & "): ")
                        End If
                        Landscape(x, y).Bear.AdvanceGeneration(ShowDetail)
                        If Landscape(x, y).Bear.CheckIfDead() Then
                            Landscape(x, y).Bear = Nothing
                            BearCount -= 1
                        Else
                            If Landscape(x, y).Bear.ReproduceThisPeriod() Then
                                If ShowDetail Then
                                    Console.WriteLine(" Bear has reproduced. ")
                                End If
                                NewFoxCount += 1
                            End If
                            If ShowDetail Then
                                Landscape(x, y).Bear.Inspect()
                            End If
                            Landscape(x, y).Bear.ResetFoodConsumed()
                        End If
                    End If
                    If Not Landscape(x, y).Tree Is Nothing Then
                        Landscape(x, y).Tree.AdvanceTimePeriod()
                        If Landscape(x, y).Tree.IsAlive = True Then
                            If Landscape(x, y).Tree.ReproduceThisPeriod() Then
                                CreateNewTree()

                            End If
                        End If
                    End If
                Next
            Next
            If NewFoxCount > 0 Then
                If ShowDetail Then
                    Console.WriteLine("New foxes born: ")
                End If
                For f = 0 To NewFoxCount - 1
                    CreateNewFox()
                Next
            End If
            If NewFoxCount > 0 Then
                If ShowDetail Then
                    Console.WriteLine("New Bears born: ")
                End If
                For f = 0 To NewBearCount - 1
                    CreateNewBear()
                Next
            End If
            If ShowDetail Then
                Console.ReadKey()
            End If
            DrawLandscape()
            Console.WriteLine()

        End Sub
        Enum desc
            Big
            Small
        End Enum
        Private Sub CreateLandscapeAndAnimals(ByVal InitialWarrenCount As Integer, ByVal InitialFoxCount As Integer, ByVal InitialBearCount As Integer, ByVal FixedInitialLocations As Boolean, InitialTreeCount As Integer)
            For x = 0 To LandscapeSize - 1
                For y = 0 To LandscapeSize - 1
                    Landscape(x, y) = New Location()
                Next
            Next
            If FixedInitialLocations Then
                Landscape(1, 1).Warren = New Warren(Variability, 38)
                Landscape(2, 8).Warren = New Warren(Variability, 80)
                Landscape(9, 7).Warren = New Warren(Variability, 20)
                Landscape(10, 3).Warren = New Warren(Variability, 52)
                Landscape(13, 4).Warren = New Warren(Variability, 67)
                WarrenCount = 5
                Landscape(2, 10).Fox = New Fox(Variability)
                Landscape(6, 1).Fox = New Fox(Variability)
                Landscape(8, 6).Fox = New Fox(Variability)
                Landscape(11, 13).Fox = New Fox(Variability)
                Landscape(12, 4).Fox = New Fox(Variability)
                FoxCount = 5
                Landscape(3, 4).Bear = New Bear(Variability)
                Landscape(5, 1).Bear = New Bear(Variability)
                Landscape(10, 7).Bear = New Bear(Variability)
                BearCount = 3
                For x = 0 To InitialTreeCount - 1
                    CreateNewTree()
                Next
            Else
                For w = 0 To InitialWarrenCount - 1
                    CreateNewWarren()
                Next
                For f = 0 To InitialFoxCount - 1
                    CreateNewFox()
                Next
                For e = 0 To InitialBearCount - 1
                    CreateNewBear()
                Next
                For x = 0 To InitialTreeCount - 1
                    CreateNewTree()
                Next
            End If
        End Sub

        Private Sub CreateNewWarren()
            Dim x As Integer
            Dim y As Integer
            Do
                x = Rnd.Next(0, LandscapeSize)
                y = Rnd.Next(0, LandscapeSize)
            Loop While Not Landscape(x, y).Warren Is Nothing
            If ShowDetail Then
                Console.WriteLine("New Warren at (" & x & "," & y & ")")
            End If
            Landscape(x, y).Warren = New Warren(Variability)
            WarrenCount += 1
        End Sub
        Private Sub CreateNewBear()
            Dim x As Integer
            Dim y As Integer
            Do
                x = Rnd.Next(0, LandscapeSize)
                y = Rnd.Next(0, LandscapeSize)
            Loop While Not Landscape(x, y).Bear Is Nothing
            If ShowDetail Then
                Console.WriteLine("  New Bear at (" & x & "," & y & ")")
            End If
            Landscape(x, y).Bear = New Bear(Variability)
            BearCount += 1
        End Sub
        Private Sub CreateNewTree()
            Dim x As Integer
            Dim y As Integer
            Do
                x = Rnd.Next(0, LandscapeSize)
                y = Rnd.Next(0, LandscapeSize)
            Loop While Not Landscape(x, y).Tree Is Nothing
            Landscape(x, y).Tree = New Tree
        End Sub
        Private Sub CreateNewFox()
            Dim x As Integer
            Dim y As Integer
            Do
                x = Rnd.Next(0, LandscapeSize)
                y = Rnd.Next(0, LandscapeSize)
            Loop While Not Landscape(x, y).Fox Is Nothing
            If ShowDetail Then
                Console.WriteLine("  New Fox at (" & x & "," & y & ")")
            End If
            Landscape(x, y).Fox = New Fox(Variability)
            FoxCount += 1
        End Sub

        Private Sub FoxesEatRabbitsInWarren(ByVal WarrenX As Integer, ByVal WarrenY As Integer)
            Dim FoodConsumed As Integer
            Dim PercentToEat As Integer
            Dim Dist As Double
            Dim RabbitsToEat As Integer
            Dim RabbitCountAtStartOfPeriod As Integer = Landscape(WarrenX, WarrenY).Warren.GetRabbitCount()
            For FoxX = 0 To LandscapeSize - 1
                For FoxY = 0 To LandscapeSize - 1
                    If Not Landscape(FoxX, FoxY).Fox Is Nothing Then
                        Dist = DistanceBetween(FoxX, FoxY, WarrenX, WarrenY)
                        If Dist <= 3.5 Then
                            PercentToEat = 20
                        ElseIf Dist <= 7 Then
                            PercentToEat = 10
                        Else
                            PercentToEat = 0
                        End If
                        RabbitsToEat = CInt(Math.Round(CDbl(PercentToEat * RabbitCountAtStartOfPeriod / 100)))
                        FoodConsumed = Landscape(WarrenX, WarrenY).Warren.EatRabbits(RabbitsToEat)
                        Landscape(FoxX, FoxY).Fox.GiveFood(FoodConsumed)
                        If ShowDetail Then
                            Console.WriteLine("  " & FoodConsumed & " rabbits eaten by fox at (" & FoxX & "," & FoxY & ").")
                        End If
                    End If
                Next
            Next
        End Sub
        Private Sub BearsEatFox(ByVal FoxX As Integer, ByVal FoxY As Integer)
            Dim FoodConsumed As Integer
            Dim Dist As Double
            Dim FoxesToEat As Integer = FoxCount
            Dim FoxKilled As Boolean = False
            Dim FoxCountAtStartOfPeriod As Integer = FoxCount
            For BearX = 0 To LandscapeSize - 1
                For BearY = 0 To LandscapeSize - 1
                    If Not Landscape(BearX, BearY).Bear Is Nothing Then
                        Dist = DistanceBetween(BearX, BearY, FoxX, FoxY)
                        If Dist <= 4 And Landscape(BearX, BearY).Bear.BearIsFull = False Then
                            FoxKilled = True
                        End If
                        FoodConsumed = 5
                        Landscape(BearX, BearY).Bear.GiveFood(FoodConsumed)
                        If FoxKilled = True Then
                            Landscape(FoxX, FoxY).Fox.FoxEatenByBear(ShowDetail)
                            If ShowDetail Then
                                Console.WriteLine("  " & FoodConsumed / 5 & " fox(es) eaten by an Bear at (" & BearX & "," & BearY & ").")
                            End If
                        End If
                    End If
                Next
            Next
        End Sub

        Private Function DistanceBetween(ByVal x1 As Integer, ByVal y1 As Integer, ByVal x2 As Integer, ByVal y2 As Integer) As Double
            Return Math.Sqrt(Math.Pow(x1 - x2, 2) + Math.Pow(y1 - y2, 2))
        End Function

        Private Sub DrawLandscape()
            Console.WriteLine()
            Console.WriteLine("TIME PERIOD: " & TimePeriod)
            Console.WriteLine()
            Console.BackgroundColor = ConsoleColor.Black
            Console.Write("      ")
            For x = 0 To LandscapeSize - 1
                If x < 10 Then
                    Console.Write(" ")
                End If
                Console.Write(x & " |  ")
            Next
            Console.WriteLine()
            For x = 0 To LandscapeSize * 4 + 3
                Console.Write("-")
            Next
            Console.WriteLine()
            For y = 0 To LandscapeSize - 1
                If y < 10 Then
                    Console.Write(" ")
                End If
                Console.Write(" " & y & "|")
                For x = 0 To LandscapeSize - 1
                    If Not Landscape(x, y).Warren Is Nothing Then
                        If Landscape(x, y).Warren.GetRabbitCount() < 10 Then
                            Console.Write(" ")
                        End If
                        Console.BackgroundColor = ConsoleColor.DarkBlue
                        Console.Write(Landscape(x, y).Warren.GetRabbitCount())
                        Console.BackgroundColor = ConsoleColor.Black
                    Else
                        Console.Write("  ")
                    End If
                    If Not Landscape(x, y).Fox Is Nothing Then
                        Console.BackgroundColor = ConsoleColor.DarkRed
                        Console.Write("F")
                        Console.BackgroundColor = ConsoleColor.Black
                    Else
                        Console.Write(" ")
                    End If
                    If Not Landscape(x, y).Tree Is Nothing Then
                        Console.BackgroundColor = ConsoleColor.DarkGreen
                        If Landscape(x, y).Tree.Size = desc.Small Then
                            Console.Write("t")
                        ElseIf Landscape(x, y).Tree.Size = desc.Big Then
                            Console.Write("T")
                        End If
                        Console.BackgroundColor = ConsoleColor.Black
                    Else
                        Console.Write(" ")
                    End If
                    If Not Landscape(x, y).Bear Is Nothing Then
                        Console.BackgroundColor = ConsoleColor.DarkMagenta
                        Console.Write("B")
                        Console.BackgroundColor = ConsoleColor.Black
                    Else
                        Console.Write(" ")
                    End If
                    Console.Write("|")
                Next

                Console.WriteLine()
            Next
            Console.BackgroundColor = ConsoleColor.Black
        End Sub
    End Class

    Class Warren
        Private Const MaxRabbitsInWarren As Integer = 99
        Private Rabbits() As Rabbit
        Private RabbitCount As Integer = 0
        Private PeriodsRun As Integer = 0
        Private AlreadySpread As Boolean = False
        Private Variability As Integer
        Private Shared Rnd As New Random()

        Public Sub New(ByVal Variability As Integer)
            Me.Variability = Variability
            Rabbits = New Rabbit(MaxRabbitsInWarren) {}
            RabbitCount = CInt(CalculateRandomValue(CInt(MaxRabbitsInWarren / 4), Variability))
            For r = 0 To RabbitCount - 1
                Rabbits(r) = New Rabbit(Variability)
            Next
        End Sub

        Public Sub New(ByVal Variability As Integer, ByVal RabbitCount As Integer)
            Me.Variability = Variability
            Me.RabbitCount = RabbitCount
            Rabbits = New Rabbit(MaxRabbitsInWarren) {}
            For r = 0 To RabbitCount - 1
                Rabbits(r) = New Rabbit(Variability)
            Next
        End Sub

        Private Function CalculateRandomValue(ByVal BaseValue As Integer, ByVal Variability As Integer) As Double
            Return BaseValue - (BaseValue * Variability / 100) + (BaseValue * Rnd.Next(0, (Variability * 2) + 1) / 100)
        End Function

        Public Function GetRabbitCount() As Integer
            Return RabbitCount
        End Function

        Public Function NeedToCreateNewWarren() As Boolean
            If RabbitCount = MaxRabbitsInWarren And Not AlreadySpread Then
                AlreadySpread = True
                Return True
            Else
                Return False
            End If
        End Function

        Public Function WarrenHasDiedOut() As Boolean
            If RabbitCount = 0 Then
                Return True
            Else
                Return False
            End If
        End Function

        Public Sub AdvanceGeneration(ByVal ShowDetail As Boolean)
            PeriodsRun += 1
            If RabbitCount > 0 Then
                KillByOtherFactors(ShowDetail)
            End If
            If RabbitCount > 0 Then
                KillGenderNeutral(ShowDetail)
            End If
            If RabbitCount > 0 Then
                AgeRabbits(ShowDetail)
            End If
            If RabbitCount > 0 And RabbitCount <= MaxRabbitsInWarren Then
                If ContainsMales() Then
                    MateRabbits(ShowDetail)
                End If
            End If
            If RabbitCount = 0 And ShowDetail Then
                Console.WriteLine("  All rabbits in warren are dead")
            End If
        End Sub

        Public Function EatRabbits(ByVal RabbitsToEat As Integer) As Integer
            Dim DeathCount As Integer = 0
            Dim RabbitNumber As Integer
            If RabbitsToEat > RabbitCount Then
                RabbitsToEat = RabbitCount
            End If
            While DeathCount < RabbitsToEat
                RabbitNumber = Rnd.Next(0, RabbitCount)
                If Not Rabbits(RabbitNumber) Is Nothing Then
                    Rabbits(RabbitNumber) = Nothing
                    DeathCount += 1
                End If
            End While
            CompressRabbitList(DeathCount)
            Return RabbitsToEat
        End Function

        Private Sub KillByOtherFactors(ByVal ShowDetail As Boolean)
            Dim DeathCount As Integer = 0
            For r = 0 To RabbitCount - 1
                If Rabbits(r).CheckIfKilledByOtherFactor() Then
                    Rabbits(r) = Nothing
                    DeathCount += 1
                End If
            Next
            CompressRabbitList(DeathCount)
            If ShowDetail Then
                Console.WriteLine("  " & DeathCount & " rabbits killed by other factors.")
            End If
        End Sub
        Private Sub KillGenderNeutral(ByVal ShowDetail As Boolean)
            Dim DeathCount As Integer
            For r = 0 To RabbitCount - 1
                If Rabbits(r).IsGenderNonBinary() Then
                    If RabbitCount > 0 Then
                        Rabbits(r) = Nothing
                        DeathCount += 1
                    End If
                End If
            Next
            CompressRabbitList(DeathCount)
            If ShowDetail Then
                Console.WriteLine("  " & DeathCount & " rabbits died because they didn't idtentify as either male or female")
            End If
        End Sub

        Private Sub AgeRabbits(ByVal ShowDetail As Boolean)
            Dim DeathCount As Integer = 0
            For r = 0 To RabbitCount - 1
                Rabbits(r).CalculateNewAge()
                If Rabbits(r).CheckIfDead() Then
                    Rabbits(r) = Nothing
                    DeathCount += 1
                End If
            Next
            CompressRabbitList(DeathCount)
            If ShowDetail Then
                Console.WriteLine("  " & DeathCount & " rabbits die of old age.")
            End If
        End Sub

        Private Sub MateRabbits(ByVal ShowDetail As Boolean)
            Dim Mate As Integer = 0
            Dim Babies As Integer = 0
            Dim CombinedReproductionRate As Double
            For r = 0 To RabbitCount - 1
                If Rabbits(r).IsFemale() And RabbitCount + Babies < MaxRabbitsInWarren Then
                    Do
                        Mate = Rnd.Next(0, RabbitCount)
                    Loop While Mate = r Or Rabbits(Mate).IsFemale()
                    CombinedReproductionRate = (Rabbits(r).GetReproductionRate() + Rabbits(Mate).GetReproductionRate()) / 2
                    If CombinedReproductionRate >= 1 Then
                        Rabbits(RabbitCount + Babies) = New Rabbit(Variability, CombinedReproductionRate)
                        Babies += 1
                    End If
                End If
            Next
            RabbitCount = RabbitCount + Babies
            If ShowDetail Then
                Console.WriteLine("  " & Babies & " baby rabbits born.")
            End If
        End Sub

        Private Sub CompressRabbitList(ByVal DeathCount As Integer)
            If DeathCount > 0 Then
                Dim ShiftTo As Integer = 0
                Dim ShiftFrom As Integer = 0
                While ShiftTo < RabbitCount - DeathCount
                    While Rabbits(ShiftFrom) Is Nothing
                        ShiftFrom += 1
                    End While
                    If ShiftTo <> ShiftFrom Then
                        Rabbits(ShiftTo) = Rabbits(ShiftFrom)
                    End If
                    ShiftTo += 1
                    ShiftFrom += 1
                End While
                RabbitCount = RabbitCount - DeathCount
            End If
        End Sub

        Private Function ContainsMales() As Boolean
            Dim Males As Boolean = False
            For r = 0 To RabbitCount - 1
                If Not Rabbits(r).IsFemale() Then
                    Males = True
                End If
            Next
            Return Males
        End Function

        Public Sub Inspect()
            Console.WriteLine("Periods Run " & PeriodsRun & " Size " & RabbitCount)
        End Sub

        Public Sub ListRabbits()
            If RabbitCount > 0 Then
                For r = 0 To RabbitCount - 1
                    Rabbits(r).Inspect()
                Next
            End If
        End Sub
    End Class

    Class Animal
        Protected NaturalLifespan As Double
        Protected ID As Integer
        Protected Shared NextID As Integer = 1
        Protected Age As Integer = 0
        Protected ProbabilityOfDeathOtherCauses As Double
        Protected IsAlive As Boolean
        Protected Shared Rnd As New Random()

        Public Sub New(ByVal AvgLifespan As Integer, ByVal AvgProbabilityOfDeathOtherCauses As Double, ByVal Variability As Integer)
            NaturalLifespan = AvgLifespan * CalculateRandomValue(100, Variability) / 100
            ProbabilityOfDeathOtherCauses = AvgProbabilityOfDeathOtherCauses * CalculateRandomValue(100, Variability) / 100
            IsAlive = True
            ID = NextID
            NextID += 1
        End Sub

        Public Overridable Sub CalculateNewAge()
            Age += 1
            If Age >= NaturalLifespan Then
                IsAlive = False
            End If
        End Sub

        Public Overridable Function CheckIfDead() As Boolean
            Return Not IsAlive
        End Function

        Public Overridable Sub Inspect()
            Console.Write("  ID " & ID & " ")
            Console.Write("Age " & Age & " ")
            Console.Write("LS " & NaturalLifespan & " ")
            Console.Write("Pr dth " & Math.Round(ProbabilityOfDeathOtherCauses, 2) & " ")
        End Sub

        Public Overridable Function CheckIfKilledByOtherFactor() As Boolean
            If Rnd.Next(0, 100) < ProbabilityOfDeathOtherCauses * 100 Then
                IsAlive = False
                Return True
            Else
                Return False
            End If
        End Function

        Protected Overridable Function CalculateRandomValue(ByVal BaseValue As Integer, ByVal Variability As Integer) As Double
            Return BaseValue - (BaseValue * Variability / 100) + (BaseValue * Rnd.Next(0, (Variability * 2) + 1) / 100)
        End Function
    End Class
    Class Bear
        Inherits Animal
        Private FoodUnitsNeeded As Integer = 5
        Private FoodUnitsConsumedThisPeriod As Integer = 0
        Private Const DefaultLifeSpan As Integer = 14
        Private Const DefaultProbabilityDeathOtherCauses As Double = 0.1
        Public Function BearIsFull() As Boolean
            If FoodUnitsConsumedThisPeriod >= 15 Then
                Return True
            Else
                Return False
            End If
        End Function
        Public Sub New(ByVal Variability As Integer)
            MyBase.New(DefaultLifeSpan, DefaultProbabilityDeathOtherCauses, Variability)
            FoodUnitsNeeded = CInt(5 * MyBase.CalculateRandomValue(100, Variability) / 100)
        End Sub
        Public Sub ResetFoodConsumed()
            FoodUnitsConsumedThisPeriod = FoodUnitsConsumedThisPeriod - 5
        End Sub
        Public Overrides Sub Inspect()
            MyBase.Inspect()
            Console.Write("Food needed " & FoodUnitsNeeded & " ")
            Console.Write("Food eaten " & FoodUnitsConsumedThisPeriod & " ")
            Console.WriteLine()
        End Sub
        Public Sub AdvanceGeneration(ByVal ShowDetail As Boolean)
            If FoodUnitsConsumedThisPeriod = 0 Then
                IsAlive = False
                If ShowDetail Then
                    Console.WriteLine("  Bear dies as has eaten no food this period.")
                End If
            Else
                If CheckIfKilledByOtherFactor() Then
                    IsAlive = False
                    If ShowDetail Then
                        Console.WriteLine("  Bear killed by other factor.")
                    End If
                Else
                    If FoodUnitsConsumedThisPeriod < FoodUnitsNeeded Then
                        CalculateNewAge()
                        If ShowDetail Then
                            Console.WriteLine("  Bear ages further due to lack of food.")
                        End If
                    End If
                    CalculateNewAge()
                    If Not IsAlive Then
                        If ShowDetail Then
                            Console.WriteLine("  Bear has died of old age.")
                        End If
                    End If
                End If
            End If
        End Sub
        Public Function ReproduceThisPeriod() As Boolean
            Const ReproductionProbability As Double = 0.4
            If Rnd.Next(0, 100) < ReproductionProbability * 100 Then

                Return True
            Else
                Return False
            End If
        End Function
        Public Sub GiveFood(ByVal FoodUnits As Integer)
            FoodUnitsConsumedThisPeriod = FoodUnitsConsumedThisPeriod + FoodUnits
        End Sub
    End Class

    Class Fox
        Inherits Animal
        Private FoodUnitsNeeded As Integer = 5
        Private FoodUnitsConsumedThisPeriod As Integer = 0
        Private Const DefaultLifespan As Integer = 10
        Private Const DefaultProbabilityDeathOtherCauses As Double = 0.1
        Public Sub FoxEatenByBear(ByVal Showdetail As Boolean)
            IsAlive = False
            If Showdetail Then
                Console.WriteLine(" Fox eaten bY Bear")
            End If
        End Sub
        Public Sub New(ByVal Variability As Integer)
            MyBase.New(DefaultLifespan, DefaultProbabilityDeathOtherCauses, Variability)
            FoodUnitsNeeded = CInt(10 * MyBase.CalculateRandomValue(100, Variability) / 100)
        End Sub

        Public Sub AdvanceGeneration(ByVal ShowDetail As Boolean)
            If FoodUnitsConsumedThisPeriod = 0 Then
                IsAlive = False
                If ShowDetail Then
                    Console.WriteLine("  Fox dies as has eaten no food this period.")
                End If
            Else
                If CheckIfKilledByOtherFactor() Then
                    IsAlive = False
                    If ShowDetail Then
                        Console.WriteLine("  Fox killed by other factor.")
                    End If
                Else
                    If FoodUnitsConsumedThisPeriod < FoodUnitsNeeded Then
                        CalculateNewAge()
                        If ShowDetail Then
                            Console.WriteLine("  Fox ages further due to lack of food.")
                        End If
                    End If
                    CalculateNewAge()
                    If Not IsAlive Then
                        If ShowDetail Then
                            Console.WriteLine("  Fox has died of old age.")
                        End If
                    End If
                End If
            End If
        End Sub

        Public Sub ResetFoodConsumed()
            FoodUnitsConsumedThisPeriod = FoodUnitsConsumedThisPeriod - 10
        End Sub

        Public Function ReproduceThisPeriod() As Boolean
            Const ReproductionProbability As Double = 0.15
            If Rnd.Next(0, 100) < ReproductionProbability * 100 Then
                Return True
            Else
                Return False
            End If
        End Function

        Public Sub GiveFood(ByVal FoodUnits As Integer)
            FoodUnitsConsumedThisPeriod = FoodUnitsConsumedThisPeriod + FoodUnits
        End Sub

        Public Overrides Sub Inspect()
            MyBase.Inspect()
            Console.Write("Food needed " & FoodUnitsNeeded & " ")
            Console.Write("Food eaten " & FoodUnitsConsumedThisPeriod & " ")
            Console.WriteLine()
        End Sub
    End Class

    Class Rabbit
        Inherits Animal
        Enum Genders
            Male
            Female
            NonBinary
        End Enum
        Private ReproductionRate As Double
        Private Const DefaultReproductionRate As Double = 1.2
        Private Const DefaultLifespan As Integer = 4
        Private Const DefaultProbabilityDeathOtherCauses As Double = 0.05
        Private Gender As Genders

        Public Sub New(ByVal Variability As Integer)
            MyBase.New(DefaultLifespan, DefaultProbabilityDeathOtherCauses, Variability)
            ReproductionRate = DefaultReproductionRate * MyBase.CalculateRandomValue(100, Variability) / 100
            Dim rand As Integer = Rnd.Next(0, 100)
            If rand < 45 Then
                Gender = Genders.Male
            ElseIf rand >= 45 And rand <= 90 Then
                Gender = Genders.Female
            Else
                Gender = Genders.NonBinary
            End If
        End Sub

        Public Sub New(ByVal Variability As Integer, ByVal ParentsReproductionRate As Double)
            MyBase.New(DefaultLifespan, DefaultProbabilityDeathOtherCauses, Variability)
            ReproductionRate = ParentsReproductionRate * MyBase.CalculateRandomValue(100, Variability) / 100
            Dim rand As Integer = Rnd.Next(0, 100)
            If rand < 45 Then
                Gender = Genders.Male
            ElseIf rand >= 45 And rand <= 90 Then
                Gender = Genders.Female
            Else
                Gender = Genders.NonBinary
            End If
        End Sub

        Public Overrides Sub Inspect()
            MyBase.Inspect()
            Console.Write("Rep rate " & Math.Round(ReproductionRate, 1) & " ")
            If Gender = Genders.Female Then
                Console.WriteLine("Gender Female")
            ElseIf Gender = Genders.Male Then
                Console.WriteLine("Gender Male")
            Else
                Console.WriteLine("Gender Non-Binary")
            End If
        End Sub

        Public Function IsFemale() As Boolean
            If Gender = Genders.Female Then
                Return True
            Else
                Return False
            End If
        End Function
        Public Function IsGenderNonBinary()
            If Gender = Genders.NonBinary Then
                Return True
            Else
                Return False
            End If
        End Function
        Public Function GetReproductionRate() As Double
            Return ReproductionRate
        End Function
    End Class
End Module

