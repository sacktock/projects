Class Bank_Account
    Private Account_Number As Integer
    Private Bank_Balance As Double
    Public Function GetBankBalance()
        Return Bank_Balance
    End Function
    Public Sub Set_Bank_Balance(AmountAdded As Double)
        Bank_Balance = Bank_Balance + AmountAdded
    End Sub
    Public Sub New()
        Randomize()
        Account_Number = CInt(Math.Ceiling(Rnd() * 9999999999)) + 10000000000
    End Sub
End Class
Class Debit_Card
    Inherits Bank_Account
    Public Sub Buy(MoneySpent As Double)
        Set_Bank_Balance(-1 * MoneySpent)
    End Sub
End Class
Class Credit_Card
    Inherits Bank_Account
    Private CardBalance As Double
    Public Sub LoadCard(AmountToLoad As Double)
        Set_Bank_Balance(AmountToLoad * -1)
        CardBalance = AmountToLoad
    End Sub
    Public Sub Buy(MoneySpent)
        CardBalance = CardBalance - MoneySpent
    End Sub
End Class

Module Module1
    Sub Main()
    End Sub
End Module
