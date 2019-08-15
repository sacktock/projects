Public Class Form1
    Dim Data As New Data_Connection("C:\Users\alexw\OneDrive\Documents\Rental.mdb")

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load

    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click

        TextBox1.Text = Data.Read("Customers", 0, 1)
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        Data.WriteStringNewRecord(TextBox2.Text, "Customers", "CustomerName")
    End Sub
End Class
Class Data_Connection
    Private File As String
    Public Sub New(InputFile)
        File = InputFile
    End Sub
    Public Function Read(ByVal Table As String, ByVal row As Integer, ByVal Item As Integer)
        Dim cn As New OleDb.OleDbConnection
        Dim da As OleDb.OleDbDataAdapter
        Dim ds As New DataSet
        Dim sql As String

        cn.ConnectionString() = "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" & File

        cn.Open()

        sql = "SELECT * FROM " & Table

        da = New OleDb.OleDbDataAdapter(sql, cn)

        da.Fill(ds, Table)

        Return ds.Tables(Table).Rows(row).Item(Item)
    End Function
    Public Sub WriteStringNewRecord(ByVal Input As String, ByVal Table As String, Record As String)
        Dim cn As New OleDb.OleDbConnection
        Dim obj_cmd As OleDb.OleDbCommand
        Dim ds As New DataSet
        Dim sql As String

        cn.ConnectionString = "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" & File
        cn.Open()

        sql = "INSERT INTO " & Table & " (" & Record & ") VALUES('" & Input & "');"
        obj_cmd = New OleDb.OleDbCommand(sql, cn)
        obj_cmd.ExecuteNonQuery()
        cn.Close()
        MsgBox("New " & Record & " Saved")
    End Sub
End Class