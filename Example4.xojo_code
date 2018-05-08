#tag Class
Protected Class Example4
Inherits fpdf
	#tag Method, Flags = &h1
		Protected Function AcceptPageBreak() As boolean
		  //Método que acepta o no el salto automático de página
		  
		  if(me.col < 2) then
		    
		    //Go to next column
		    me.SetCol(me.col+1)
		    
		    //reset Y0
		    me.SetY(me.y0)
		    
		    //go to the next page
		    return false
		    
		  else
		    
		    //Back to first column
		    me.SetCol(0)
		    
		    //Page Break
		    return true
		    
		  end if
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub ChapterBody(charfile as string)
		  //Abrir fichero de texto
		  Dim f As FolderItem
		  Dim t As TextInputStream
		  dim txt as string
		  
		  f = new FolderItem
		  f = GetFolderItem("demoinfo").child(charfile)
		  
		  if f = nil then return // Could not locate file
		  
		  t = f.OpenAsTextFile
		  t.Encoding = Encodings.ASCII
		  txt = t.readall
		  t.close
		  
		  //Font
		  me.SetFont("Times","",12)
		  
		  //Print text in 6cm column
		  me.MultiCell(60,5,txt)
		  me.Ln()
		  
		  //Cita en itálica
		  me.SetFont("","I")
		  me.Cell(0,5,"(end of file)")
		  
		  //Volver a la primera columna
		  me.SetCol(0)
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub ChapterTitle(num as integer, label as string)
		  dim title as string
		  
		  title = "Chapter " + str(num) + " : " + label
		  
		  //Title
		  me.SetFont("Arial","",12)
		  me.SetFillColor(200,220,255)
		  me.Cell(0,6,title,0,1,"L",1)
		  me.Ln(4)
		  
		  //Guardar ordenada
		  me.y0=me.GetY()
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub Footer()
		  //Pie de página
		  
		  me.SetY(-15)
		  me.SetFont("Arial","I",8)
		  me.SetTextColor(128)
		  me.Cell(0,10,"Page " + str(me.PageNo), 0, 0,"C")
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub Header()
		  dim w as double
		  
		  me.SetFont("Arial","B",15)
		  
		  w = me.GetStringWidth(me.title) + 6
		  
		  me.SetX( (210 - w) / 2)
		  
		  me.SetDrawColor(0,80,180)
		  
		  me.SetFillColor(230,230,0)
		  
		  me.SetTextColor(220,50,50)
		  
		  me.SetLineWidth(1)
		  
		  me.Cell(w,9,me.title, 1, 1,"C", 1)
		  
		  me.Ln(10)
		  
		  //Guardar ordenada
		  me.y0 = me.GetY()
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub PrintChapter(num as integer, title as string, file as string)
		  //Add Chapter
		  
		  me.AddPage()
		  me.ChapterTitle(num,title)
		  me.ChapterBody(file)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub SetCol(col as integer)
		  dim x as double
		  
		  //Establecer la posición de una columna dada
		  me.col = col
		  x = 10 + col * 65
		  me.SetLeftMargin(x)
		  me.SetX(x)
		  
		  
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h21
		#tag Note
			//Current Column
		#tag EndNote
		Private col As Integer
	#tag EndProperty

	#tag Property, Flags = &h21
		#tag Note
			//Ordenada de Comienzo de Columna
		#tag EndNote
		Private y0 As Integer
	#tag EndProperty


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
