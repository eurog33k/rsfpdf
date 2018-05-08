#tag Class
Protected Class Example2
Inherits fpdf
	#tag Method, Flags = &h1
		Protected Sub Footer()
		  //Posición: a 1,5 cm del final
		  self.SetY(-15)
		  
		  //Arial italic 8
		  self.SetFont("Arial","I",8)
		  
		  //Número de página
		  self.Cell(0,10,"Page " + str(self.PageNo()) + "/{nb}",0,0,"C")
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub Header()
		  
		  //Logo - must be the full absolute path to the image file.
		  Dim logopath As String
		  logopath = GetFolderItem("").Child("demoinfo").Child("logo_rpdf.png").ShellPath
		  self.Image(logopath,10,8)
		  
		  //Arial bold 15
		  self.SetFont("Arial","B",15)
		  
		  //Movernos a la derecha
		  self.Cell(80)
		  
		  //Título
		  self.Cell(30,10,"Title",1,0,"C")
		  
		  //Salto de línea
		  self.Ln(20)
		End Sub
	#tag EndMethod


	#tag ViewBehavior
		#tag ViewProperty
			Name="Index"
			Visible=true
			Group="ID"
			InitialValue="-2147483648"
			Type="Integer"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Left"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Name"
			Visible=true
			Group="ID"
			Type="String"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Super"
			Visible=true
			Group="ID"
			Type="String"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Top"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
