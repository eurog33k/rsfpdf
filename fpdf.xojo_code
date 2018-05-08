#tag Class
Protected Class fpdf
	#tag Method, Flags = &h1
		Protected Function AcceptPageBreak() As boolean
		  //Accept automatic page break or not
		  return me.AutoPageBreak
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub AddFont(Family as string, optional Style as string = "", optional File as string = "")
		  dim FontKey as string
		  dim FontInfo as new Collection
		  
		  //Add a TrueType or Type1 font
		  
		  family = Family.Lowercase
		  if file = "" then file = Family.ReplaceAll(" ","") + Style.Lowercase
		  
		  if Family = "arial" then Family = "helvetica"
		  
		  Style = Style.Uppercase
		  if Style = "IB" then style = "BI"
		  
		  fontKey = Family + Style
		  
		  if Fonts.Item(FontKey) <> nil then
		    me.Error("Font already added: " + family + " " + style)
		    return
		  end if
		  
		  if me.LoadCharTable(File) = false then
		    me.Error("Could not include font definition file")
		    return
		  end if
		  
		  'fontinfo.Add me.Fonts.Count + 1, "i"
		  'fontinfo.Add type, "type"
		  '
		  'fontinfo.Add name, "name"
		  'fontinfo.Add desc, "desc"
		  'fontinfo.add
		  'fontinfo.Add me.CoreFonts.Item(fontkey), "name"
		  'fontinfo.Add -100, "up"
		  'fontinfo.Add 50, "ut"
		  'fontinfo.Add me.CharWidths.Item(fontkey), "cw"
		  '
		  'me.fonts.Add collection(fontinfo), fontkey
		  '
		  '
		  '$this->fonts[$fontkey]=array('i'=>$i,'type'=>$type,'name'=>$name,'desc'=>$desc,'up'=>$up,'ut'=>$ut,'cw'=>$cw,'enc'=>$enc,'file'=>$file);
		  'if($diff)
		  '{
		  '//Search existing encodings
		  '$d=0;
		  '$nb=count($this->diffs);
		  'for($i=1;$i<=$nb;$i++)
		  '{
		  'if($this->diffs[$i]==$diff)
		  '{
		  '$d=$i;
		  'break;
		  '}
		  '}
		  'if($d==0)
		  '{
		  '$d=$nb+1;
		  '$this->diffs[$d]=$diff;
		  '}
		  '$this->fonts[$fontkey]['diff']=$d;
		  '}
		  'if($file)
		  '{
		  'if($type=='TrueType')
		  '$this->FontFiles[$file]=array('length1'=>$originalsize);
		  'else
		  '$this->FontFiles[$file]=array('length1'=>$size1,'length2'=>$size2);
		  '}
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function AddLink() As String
		  dim n as integer
		  dim detLink as new Collection
		  
		  //Create a new internal link
		  n = me.links.Count + 1
		  
		  detLink.Add nil, "link"
		  
		  me.links.Add detLink, str(n)
		  
		  return str(n)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub AddPage(optional orientation as string = "")
		  dim family,style as string
		  dim size,lw as double
		  dim cf as Boolean
		  
		  dim dc,fc,tc as string
		  
		  //Start a new page
		  
		  if(me.state = 0) then me.Open()
		  
		  family = me.FontFamily
		  style  = me.FontStyle
		  if me.Underline then style = me.FontStyle + "U"
		  size = me.FontSizePt
		  lw = me.LineWidth
		  dc = me.DrawColor
		  fc = me.FillColor
		  tc = me.TextColor
		  cf = me.ColorFlag
		  
		  if(me.page > 0) then
		    
		    //Page footer
		    me.InFooter = true
		    me.Footer()
		    me.InFooter = false
		    
		    //Close page
		    me.endpage()
		  end if
		  
		  //Start new page
		  me.beginpage(orientation)
		  
		  //Set line cap style to square
		  me.out("2 J")
		  
		  //Set line width
		  me.LineWidth = lw
		  me.out( fNum(lw * me.k,"0.00") + " w" )
		  
		  //Set font
		  if(family <> "") then me.SetFont(family,style,size)
		  
		  //Set colors
		  me.DrawColor = dc
		  
		  if(dc <> "0 G") then me.out(dc)
		  
		  me.FillColor=fc
		  
		  if(fc <> "0 g") then me.out(fc)
		  
		  me.TextColor = tc
		  me.ColorFlag = cf
		  
		  //Page header
		  me.Header()
		  
		  //Restore line width
		  if(me.LineWidth <> lw) then
		    me.LineWidth = lw
		    me.out( fNum(lw * me.k,"0.00") + " w" )
		  end if
		  
		  //Restore font
		  if(family <> "") then me.SetFont(family,style,size)
		  
		  //Restore colors
		  if(me.DrawColor <> dc) then
		    me.DrawColor = dc
		    me.out(dc)
		  end if
		  
		  if(me.FillColor <> fc) then
		    me.FillColor=fc
		    me.out(fc)
		  end if
		  
		  me.TextColor = tc
		  me.ColorFlag = cf
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub AliasNbPages(optional alias as string = "{nb}")
		  //Define an alias for total number of pages
		  me.AliasNbPages = alias
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub BeginPage(optional orientation as string = "")
		  me.page = me.page + 1
		  
		  me.Pages.append ""
		  
		  me.state=2
		  me.x = me.lMargin
		  me.y = me.tMargin
		  me.FontFamily = ""
		  
		  //Page orientation
		  if(orientation = "") then
		    
		    orientation = me.DefOrientation
		    
		  else
		    
		    orientation = orientation.Uppercase.Left(1)
		    
		    if(orientation <> me.DefOrientation) then
		      me.OrientationChanges.Add true, str(me.Page)
		    end if
		    
		  end if
		  
		  if(orientation <> me.CurOrientation) then
		    
		    //Change orientation
		    if(orientation = "P") then
		      
		      me.wPt = me.fwPt
		      me.hPt = me.fhPt
		      me.w   = me.fw
		      me.h   = me.fh
		      
		    else
		      
		      me.wPt = me.fhPt
		      me.hPt = me.fwPt
		      me.w   = me.fh
		      me.h   = me.fw
		      
		    end if
		    
		    me.PageBreakTrigger = me.h - me.bMargin
		    me.CurOrientation = orientation
		    
		    
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Cell(w as double, optional h as double = 0, optional txt as string = "", optional border as variant = 0, optional ln as integer = 0, optional align as string = "", optional fill as integer = 0, optional link as string = "")
		  dim x,y,ws,k,dx,sw as double
		  dim s,op as string
		  
		  //Output a cell
		  k = me.k
		  
		  if(me.y + h > me.PageBreakTrigger AND not me.InFooter and me.AcceptPageBreak() ) then
		    
		    //Automatic page break
		    x  = me.x
		    ws = me.ws
		    
		    if( ws > 0) then
		      
		      me.ws=0
		      me.out("0 Tw")
		      
		    end if
		    
		    me.AddPage(me.CurOrientation)
		    me.x = x
		    
		    if(ws > 0) then
		      me.ws = ws
		      me.out( fNum( ws * k, "0.000") + " Tw")
		    end if
		    
		  end if
		  
		  if( w = 0 ) then w = me.w - me.rMargin - me.x
		  
		  s = ""
		  
		  if ( fill = 1 or border = 1) then
		    
		    if( fill = 1) then
		      
		      if border = 1 then
		        op = "B"
		      else
		        op = "f"
		      end if
		      
		    else
		      op = "S"
		    end if
		    
		    s = fNum(me.x * k,"0.00") + " " + _
		    fNum((me.h - me.y) * k,"0.00")  + " " + _
		    fNum(w * k,"0.00") + " " + _
		    fNum( - (h * k),"0.00") + " re " + _
		    op + " "
		    
		  end if
		  
		  if(not border.IsNumeric) then
		    
		    x = me.x
		    y = me.y
		    
		    if border.StringValue.InStr("L") > 0 then
		      s = s + fNum(x * k,"0.00") + " " + _
		      fNum((me.h - y) * k,"0.00") + " m " + _
		      fNum(x * k,"0.00") + " " + _
		      fNum((me.h - (y + h)) * k,"0.00") + " l S "
		    end if
		    
		    if border.StringValue.InStr("T") > 0 then
		      s = s + fNum(x * k,"0.00") + " " + _
		      fNum((me.h - y) * k,"0.00") + " m " + _
		      fNum((x + w) * k,"0.00") + " " + _
		      fNum((me.h - y) * k,"0.00") + " l S "
		      
		    end if
		    
		    if border.StringValue.InStr("R") > 0 then
		      s = s + fNum((x+w)*k,"0.00") + " " + _
		      fNum((me.h-y)*k,"0.00") + " m " + _
		      fNum((x+w)*k,"0.00") + " " + _
		      fNum((me.h-(y+h))*k,"0.00") + " l S "
		      
		    end if
		    
		    if border.StringValue.InStr("B") > 0 then
		      s = s + fNum(x*k,"0.00") + " " + _
		      fNum((me.h-(y+h))*k,"0.00") + " m " + _
		      fNum((x+w)*k,"0.00") + " " + _
		      fNum((me.h-(y+h))*k,"0.00") + " l S "
		      
		    end if
		    
		  end if
		  
		  if(txt <> "") then
		    
		    select case align
		      
		    case "R"
		      sw = me.GetStringWidth(txt)
		      dx = w - me.cMargin - sw
		    case "C"
		      sw = me.GetStringWidth(txt)
		      dx = (w - sw) / 2
		    case else
		      dx = me.cMargin
		    end select
		    
		    
		    if (me.ColorFlag) then
		      s = s + "q " + me.TextColor + " "
		    end if
		    
		    s = s + "BT " + _
		    fNum( (me.x + dx) * k,"0.00") + " " + _
		    fNum( (me.h - (me.y + 0.5 * h + 0.3 * me.FontSize)) * k,"0.00") + " Td " + _
		    me.textstring(txt) + " Tj ET"
		    
		    if ( me.underline = true ) then
		      s = s + " " + me.dounderline(me.x + dx, me.y + 0.5 * h + 0.3 * me.FontSize, txt)
		    end if
		    
		    if(me.ColorFlag) then
		      s = s + " Q"
		    end if
		    
		    if(link <> "") then
		      me.Link(me.x + dx, me.y + 0.5 * h - 0.5 * me.FontSize, me.GetStringWidth(txt),me.FontSize,link)
		    end if
		    
		  end if
		  
		  if (s <> "" ) then me.out(s)
		  
		  me.lasth = h
		  
		  if(ln > 0) then
		    
		    //Go to next line
		    me.y = me.y + h
		    
		    if(ln = 1) then me.x = me.lMargin
		    
		  else
		    
		    me.x = me.x + w
		    
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Close()
		  //Terminate document
		  
		  if (me.state = 3) then return
		  
		  if (me.page = 0) then me.AddPage()
		  
		  //Page footer
		  me.InFooter=true
		  me.Footer()
		  me.InFooter=false
		  
		  //Close page
		  me.endpage()
		  
		  //Close document
		  me.enddoc()
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(optional orientation as string = "P", optional unit as string = "mm", optional format as string = "A4", optional pWidth as double = 0, optional pHeigth as double = 0)
		  dim margin as Double
		  
		  //Some checks
		  //me._dochecks();
		  
		  //Initialization of properties
		  me.page=0
		  me.n=2
		  me.buffer=""
		  redim me.pages(0)
		  
		  me.PageLinks = new Dictionary
		  me.offsets = new Collection
		  me.OrientationChanges = new Collection
		  me.CurrentFont = new Collection
		  me.CharWidths = new Collection
		  me.fonts = new Collection
		  
		  me.state=0
		  
		  me.FontFiles=new Collection
		  me.diffs=array(-1)
		  me.Images = new Collection
		  me.Links = new Collection
		  me.InFooter=false
		  me.lasth=0
		  me.FontFamily=""
		  me.FontStyle=""
		  me.FontSizePt=12
		  me.underline=false
		  me.DrawColor="0 G"
		  me.FillColor="0 g"
		  me.TextColor="0 g"
		  me.ColorFlag=false
		  me.ws=0
		  
		  //Standard fonts
		  me.CoreFonts = new Collection
		  me.CoreFonts.Add "Courier","courier"
		  me.CoreFonts.Add "Courier-Bold","courierB"
		  me.CoreFonts.Add "Courier-Oblique","courierI"
		  me.CoreFonts.Add "Courier-BoldOblique","courierBI"
		  me.CoreFonts.Add "Helvetica","helvetica"
		  me.CoreFonts.Add "Helvetica-Bold","helveticaB"
		  me.CoreFonts.Add "Helvetica-Oblique","helveticaI"
		  me.CoreFonts.Add "Helvetica-BoldOblique","helveticaBI"
		  me.CoreFonts.Add "Times-Roman","times"
		  me.CoreFonts.Add "Times-Bold","timesB"
		  me.CoreFonts.Add "Times-Italic","timesI"
		  me.CoreFonts.Add "Times-BoldItalic","timesBI"
		  me.CoreFonts.Add "Symbol","symbol"
		  me.CoreFonts.Add "ZapfDingbats","zapfdingbats"
		  
		  //gdf: build chartable strings used in LoadCharTable
		  me.kCTs = New Dictionary
		  me.kCTs.Value("courier") = kCT_courier
		  me.kCTs.Value("helvetica") = kCT_helvetica
		  me.kCTs.Value("helveticab") = kCT_helveticab
		  me.kCTs.Value("helveticabi") = kCT_helveticabi
		  me.kCTs.Value("helveticai") = kCT_helveticai
		  me.kCTs.Value("symbol") = kCT_symbol
		  me.kCTs.Value("times") = kCT_times
		  me.kCTs.Value("timesb") = kCT_timesb
		  me.kCTs.Value("timesbi") = kCT_timesbi
		  me.kCTs.Value("timesi") = kCT_timesi
		  me.kCTs.Value("zapfdingbats") = kCT_zapfdingbats
		  
		  //gdf: set default encoding
		  me.InternalEncoding = Encodings.UTF8
		  
		  //Scale factor
		  select case unit.Lowercase
		    
		  case "pt"
		    me.k = 1
		  case"mm"
		    me.k = 72 / 25.4
		  case "cm"
		    me.k = 72 / 2.54
		  case "in"
		    me.k = 72
		  case else
		    me.Error("Incorrect unit: " + unit)
		    
		  end select
		  
		  
		  //Page format
		  
		  if( format <> "" ) then
		    
		    select case format.Lowercase
		      
		    case "a3"
		      me.fwPt = 841.89
		      me.fhPt = 1190.55
		      
		    case "a4"
		      me.fwPt = 595.28
		      me.fhPt = 841.89
		      
		    case "a5"
		      me.fwPt = 420.94
		      me.fhPt = 595.28
		      
		    case "letter"
		      me.fwPt = 612
		      me.fhPt = 792
		      
		    case "legal"
		      me.fwPt = 612
		      me.fhPt = 1008
		      
		    case else
		      me.Error("Unknown page format: " + str(format))
		      
		    end select
		    
		  else
		    
		    me.fwPt = pWidth * me.k
		    me.fhPt = pHeigth * me.k
		    
		  end if
		  
		  me.fw = me.fwPt / me.k
		  me.fh = me.fhPt / me.k
		  
		  
		  //Page orientation
		  orientation = orientation.Lowercase
		  
		  select case orientation.Lowercase
		    
		  case "p", "portrait"
		    
		    me.DefOrientation="P"
		    me.wPt=me.fwPt
		    me.hPt=me.fhPt
		    
		  case "l", "landscape"
		    
		    me.DefOrientation="L"
		    me.wPt=me.fhPt
		    me.hPt=me.fwPt
		    
		  case else
		    
		    me.Error("Incorrect orientation: " + orientation)
		    
		  end select
		  
		  me.CurOrientation = me.DefOrientation
		  me.w = me.wPt / me.k
		  me.h = me.hPt / me.k
		  
		  margin = 28.35 / me.k
		  
		  //Page margins (1 cm)
		  me.SetMargins(margin,margin)
		  
		  //Interior cell margin (1 mm)
		  me.cMargin=margin / 10
		  
		  //Line width (0.2 mm)
		  me.LineWidth = 0.567 / me.k
		  
		  //Automatic page break
		  me.SetAutoPageBreak(true, 2 * margin)
		  
		  //Full width display mode
		  me.SetDisplayMode("fullwidth")
		  
		  //Enable compression
		  'me.SetCompression(True)
		  me.SetCompression(False)
		  
		  //Set default PDF version number
		  me.PDFVersion="1.3"
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Function dounderline(x as double, y as double, txt as string) As string
		  dim up,ut,w as double
		  dim s as string
		  
		  //Underline text
		  up = me.CurrentFont.item("up")
		  ut = me.CurrentFont.item("ut")
		  w = me.GetStringWidth(txt) + me.ws * substr_count(txt," ")
		  
		  s = fNum(x * me.k,"0.00") + " " + _
		  fNum((me.h - (y - up / 1000 * me.FontSize)) * me.k,"0.00") + " " + _
		  fNum(w * me.k,"0.00") + " " + _
		  fNum(- ut / 1000 * me.FontSizePt,"0.00") + " re f"
		  
		  return s
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub enddoc()
		  dim i as integer
		  
		  me.putheader()
		  me.putpages()
		  me.putresources()
		  
		  //Info
		  me.newobj()
		  me.out("<<")
		  me.putinfo()
		  me.out(">>")
		  me.out("endobj")
		  
		  //Catalog
		  me.newobj()
		  me.out("<<")
		  me.putcatalog()
		  me.out(">>")
		  me.out("endobj")
		  
		  //Cross-ref
		  me.out("xref")
		  me.out("0 " + str(me.n + 1) )
		  me.out("0000000000 65535 f ")
		  
		  for i = 1 to me.n
		    me.out( format( me.offsets.item(str(i)), "0000000000" ) + " 00000 n " )
		  next
		  
		  //Trailer
		  me.out("trailer")
		  me.out("<<")
		  me.puttrailer()
		  me.out(">>")
		  me.out("startxref")
		  me.out(str(me.buffer.Len))
		  me.out("%%EOF")
		  me.state = 3
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub endpage()
		  //End of page contents
		  me.state=1
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub Error(msg as string)
		  MsgBox msg
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Function escape(s as string) As string
		  //Add \ before \, ( and )
		  
		  s = s.ReplaceAll(chr(92), chr(92) + chr(92))
		  s = s.ReplaceAll(")", chr(92) + ")")
		  s = s.ReplaceAll("(", chr(92) + "(")
		  
		  return s
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Function fNum(mNumber as double, optional mFormat as string = "0.00") As string
		  dim myDecPoint as string
		  dim myRetVar as string
		  
		  dim i as integer
		  
		  myDecPoint = format(1.1,"0.0").mid(2,1)
		  
		  myRetVar = format(mNumber, "-" + mFormat).ReplaceAll(myDecPoint,".")
		  
		  return myRetVar
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Footer()
		  
		  //To be implemented in your own inherited class
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function GetImageSize(file as string) As collection
		  dim a as FolderItem
		  dim f as BinaryStream
		  
		  dim data as string
		  
		  dim marker as UInt8
		  
		  dim length, bits, height, width, channels as UInt16
		  
		  dim r as Collection
		  
		  a = GetFolderItem(file, FolderItem.PathTypeShell)
		  f = a.OpenAsBinaryFile
		  marker = jpegnextmarker(f)
		  
		  if marker <> &hD8 then return nil
		  
		  while true
		    
		    marker = jpegnextmarker(f)
		    
		    select case marker
		      
		    case &hc0,&hc1,&hc2,&hc3, &hc5,&hc6,&hc7, &hc9,&hca,&hcb, &hcd,&hce,&hcf
		      
		      'length = f.ReadShort
		      length = f.ReadUInt16
		      
		      r = new Collection
		      'r.Add f.readbyte, "bits"
		      r.Add f.readUint8, "bits"
		      'r.Add f.readshort, "height"
		      r.Add f.ReadUInt16, "height"
		      'r.Add f.readshort, "width"
		      r.Add f.ReadUInt16, "width"
		      'r.Add f.Readbyte, "channels"
		      r.Add f.ReadUint8, "channels"
		      
		      return r
		      
		    case &hd8,&hd9
		      
		      return r
		      
		    case else
		      
		    end select
		    
		  wend
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Function GetStringWidth(s as string) As double
		  dim cw as new Collection
		  dim i as Integer
		  dim w as double
		  
		  //Get width of a string in the current font
		  cw = collection(CurrentFont.Item("cw"))
		  
		  w = 0
		  
		  for i = 1 to s.Len
		    w = w + cw.Item( str(s.mid(i,1).asc) )
		  next i
		  
		  return w * me.FontSize / 1000
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetX() As Double
		  return me.x
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetY() As Double
		  return me.y
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GreyScale(p As Picture) As Picture
		  if p = nil then return nil 'raise exception...
		  
		  Dim w As Integer = p.Width
		  Dim h As Integer = p.Height
		  
		  dim surf As RGBSurface = p.RGBSurface
		  
		  if surf = nil then return nil 'raise exception...
		  
		  dim greyColor(255) As Color //precompute the 256 grey colors
		  for i As integer = 0 to 255
		    greyColor(i) = RGB(i, i, i)
		  next
		  
		  dim X, Y, intensity As integer, c As Color
		  For X = 0 To w
		    For Y = 0 To h
		      c = surf.Pixel(X, Y)
		      intensity = c.Red * 0.30 + c.Green * 0.59 + c.Blue * 0.11
		      surf.Pixel(X, Y) = greyColor(intensity) //lookup grey
		    Next
		  Next
		  
		  Return p
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Function gzcompress(s as string) As string
		  // Heres goes the gzip compression algoritm or plugin call
		  return zlibCompress(s)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Header()
		  
		  //To be implemented in your own inherited class
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Image(file as string, x as double, y as double, optional w as double = 0, optional h as double = 0, optional type as string = "", optional link as string = "", isMask As Boolean=False, maskImg As Integer=0)
		  Dim iRet As Integer
		  iret=Image(file,x,y,w,h,type,link, ismask, MaskImg)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Image(file as string, x as double, y as double, optional w as double = 0, optional h as double = 0, optional type as string = "", optional link as string = "", isMask As Boolean=False, maskImg As Integer=0) As Integer
		  //Put an image on the page
		  dim info as collection
		  dim pos as integer
		  
		  if ( me.Images.Item(file) = nil) then
		    
		    //First use of image, get info
		    if(type = "") then
		      
		      pos = file.InStr(".")
		      
		      if(pos = 0) then
		        me.Error("Image file has no extension and no type was specified: " + file)
		        return -1
		      end if
		      
		      type = file.Mid(pos+1)
		      
		    end if
		    
		    type = type.Lowercase
		    
		    select case type
		    case "png"
		      info = me.parsepng(file)
		      if info<>nil and info.Item(1)="alpha" Then
		        ImagePngWithAlpha(file,x,y,w,h,link)
		        Return -1
		      end if
		    case "jpg","jpeg"
		      info = me.parsejpg(file)
		    case else
		      me.Error("Unsupported image type: " + type)
		      return -1
		    end select
		    
		    if isMask Then
		      if tmpfiles<>nil and tmpfiles.HasKey(file) then
		        if info.Item("cs")="" Then
		          info.Add "DeviceGray", "cs"
		        end if
		      end if
		      if info.Item("cs")<>"DeviceGray" then
		        me.Error "Mask must be a gray scale image"
		      end if
		      if PDFVersion<>"1.4" then
		        PDFVersion="1.4"
		      end if
		    end if
		    info.add me.Images.Count + 1, "i"
		    if maskImg>0 Then
		      info.Add maskImg, "masked"
		    end if
		    me.Images.Add info, file
		    
		  else
		    info = collection(me.Images.Item(file))
		  end if
		  
		  //Automatic width and height calculation if needed
		  
		  if(w = 0 and  h = 0) then
		    
		    //Put image at 72 dpi
		    w = info.item("w") / me.k //pixeltomm
		    h = info.item("h") / me.k
		  end if
		  
		  if(w = 0) then w = h * info.item("w") / info.item("h")
		  
		  if(h = 0) then h = w * info.item("h") / info.item("w")
		  
		  if isMask=False Then
		    me.out("q " + fNum(w * me.k,"0.00") + " 0 0 " + fnum(h * me.k,"0.00") + " " + fnum(x * me.k,"0.00") + " " + fNum((me.h-(y+h)) * me.k,"0.00") + " cm /I" + info.item("i").StringValue + " Do Q")
		  end if
		  if(link <> "") then me.Link(x,y,w,h,link)
		  
		  Return info.Item("i")
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub ImagePngWithAlpha(file as string, x as double, y as double, optional w as double = 0, optional h as double = 0, optional type as string = "", optional link as string = "")
		  #If UseMBS Then
		    Dim tmp_alpha As FolderItem
		    Dim tmp_alpha2 As FolderItem
		    Dim tmp_plain As FolderItem
		    Dim strKey As String
		    '$tmp_alpha = tempnam('.', 'mska');
		    Dim iCountFiles As Integer
		    if tmpfiles=nil then
		      tmpfiles=New Dictionary
		    end if
		    iCountFiles=tmpfiles.Count
		    strKey="mska" + Format(iCountFiles,"0000") + ".png"
		    tmp_alpha=GetFolderItem(strKey)
		    if tmp_alpha=nil then
		      exit sub
		    end if
		    strKey="mska" + Format(iCountFiles,"0000") + "b.png"
		    tmp_alpha2=GetFolderItem(strKey)
		    if tmp_alpha2=nil then
		      exit sub
		    end if
		    tmpfiles.value(tmp_alpha.ShellPath)=iCountFiles
		    'tmpfiles.Append tmp_alpha.Name
		    '$this->tmpFiles[] = $tmp_alpha;
		    '$tmp_plain = tempnam('.', 'mskp');
		    iCountFiles=tmpfiles.Count
		    strKey="mskp" + Format(iCountFiles,"0000") + ".png"
		    tmp_plain=GetFolderItem(strKey)
		    if tmp_plain= nil then
		      Exit Sub
		    end if
		    tmpfiles.value(tmp_plain.ShellPath)=iCountFiles
		    'tmpfiles.Append tmp_plain.Name
		    '$this->tmpFiles[] = $tmp_plain;
		    
		    'list($wpx, $hpx) = getimagesize($file);
		    'Dim a As Collection
		    'Dim wpx As Integer
		    'Dim hpx As Integer
		    'a=GetImageSize(file)
		    'wpx=a.Item("width")
		    'hpx=a.Item("height")
		    
		    '$img = imagecreatefrompng($file);
		    Dim img As Picture
		    Dim imgFile As FolderItem
		    imgFile=GetFolderItem(file,FolderItem.PathTypeShell)
		    if imgFile=nil then
		      me.Error("Unexpected error opening alpha png") 
		      exit sub
		    end if
		    img=Picture.Open(imgFile)
		    '$alpha_img = imagecreate( $wpx, $hpx );
		    
		    // generate gray scale pallete
		    'for($c=0;$c<256;$c++)
		    'ImageColorAllocate($alpha_img, $c, $c, $c);
		    '
		    '// extract alpha channel
		    '$xpx=0;
		    'while ($xpx<$wpx){
		    '$ypx = 0;
		    'while ($ypx<$hpx){
		    '$color_index = imagecolorat($img, $xpx, $ypx);
		    '$col = imagecolorsforindex($img, $color_index);
		    'imagesetpixel($alpha_img, $xpx, $ypx, $this->_gamma( (127-$col['alpha'])*255/127) );
		    '++$ypx;
		    '}
		    '++$xpx;
		    '}
		    Dim alpha_img As Picture
		    alpha_img=img.CopyMask
		    alpha_img=InvertPicture(alpha_img)
		    'alpha_img=GreyScale(alpha_img)
		    'imagepng($alpha_img, $tmp_alpha);
		    'alpha_img.Save(tmp_alpha, Picture.SaveAsPNG)
		    dim p As new PNGWriterMBS
		    if p.OpenWriteDestination(tmp_alpha) then
		      if p.SetGrayPicture(alpha_img) then // set picture to write
		        if p.SetHeader(false, -1) then // setup file header
		          if p.SetGamma(0) then // and default gamma
		            if p.WriteInfo then // write file header
		              if p.WriteRows then // write pixels
		                if p.WriteEnd then // and write file end
		                  p = nil // cleanup
		                  'f.Launch
		                end if
		              end if
		            end if
		          end if
		        end if
		      end if
		    end if
		    'imagedestroy($alpha_img);
		    alpha_img=nil
		    
		    // extract image without alpha channel
		    '$plain_img = imagecreatetruecolor ( $wpx, $hpx );
		    Dim plain_img As New Picture(img.Width, img.Height,32)
		    'imagecopy($plain_img, $img, 0, 0, 0, 0, $wpx, $hpx );
		    plain_img.Graphics.DrawPicture(img,0,0)
		    'imagepng($plain_img, $tmp_plain);
		    'plain_img.Save(tmp_plain, Picture.SaveAsPNG)
		    p = new PNGWriterMBS
		    if p.OpenWriteDestination(tmp_plain) then
		      if p.SetRGBPicture(plain_img) then // set picture to write
		        if p.SetHeader(false, -1) then // setup file header
		          if p.SetGamma(0) then // and default gamma
		            if p.WriteInfo then // write file header
		              if p.WriteRows then // write pixels
		                if p.WriteEnd then // and write file end
		                  p = nil // cleanup
		                  'f.Launch
		                end if
		              end if
		            end if
		          end if
		        end if
		      end if
		    end if
		    'imagedestroy($plain_img);
		    plain_img=nil
		    
		    //first embed mask image (w, h, x, will be ignored)
		    '$maskImg = $this->Image($tmp_alpha, 0,0,0,0, 'PNG', '', true); 
		    Image(tmp_alpha.ShellPath, 0, 0, 0, 0, "PNG", "", True)
		    //embed image, masked with previously embedded mask
		    '$this->Image($tmp_plain,$x,$y,$w,$h,'PNG',$link, false, $maskImg);
		    Image(tmp_plain.ShellPath, x, y, w, h, "PNG", link, False, me.Images.Count)
		  #Endif
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function InvertPicture(p As Picture) As Picture
		  Const kMaxMapOffset = 255
		  Dim map(kMaxMapOffset) As Integer
		  For i As Integer = 0 To kMaxMapOffset
		    map(i) = kMaxMapOffset - i
		  Next
		  p.RGBSurface.Transform(map)
		  Return p
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function jpegnextmarker(f as binaryStream) As UInt8
		  dim c as UInt8
		  
		  while true
		    
		    // look for 0xff
		    'while (f.readbyte <> 255)
		    while (f.ReadUint8 <> 255)
		    wend
		    
		    'c = f.readbyte
		    c = f.ReadUInt8
		    
		    if c <> 0 then return c
		    
		  wend
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Line(x1 as double, y1 as double, x2 as double, y2 as double)
		  dim s as string
		  
		  //Draw a line
		  s = fNum(x1 * me.k,"0.00") + " " + _
		  fNum((me.h - y1) * me.k,"0.00") + " m " + _
		  fNum(x2 * me.k,"0.00") + " " + _
		  fNum((me.h - y2) * me.k,"0.00") + " l S"
		  
		  me.out(s)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Link(x as double, y as double, w as double, h as double, link as string)
		  dim detLink as new Collection
		  
		  //Put a link on the page
		  
		  detLink.add x * me.k,"x"
		  detLink.add me.hPt - y * me.k,"y"
		  detlink.add w * me.k,"w"
		  detlink.add h * me.k, "h"
		  detlink.add link, "link"
		  
		  me.PageLinks.Value(me.page) = detLink
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Ln(optional h as double)
		  '*** Line feed; default value is last cell height
		  
		  self.x = self.lMargin
		  
		  if h = 0 then
		    self.y = self.y + lasth
		  else
		    self.y = self.y + h
		  end if
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function LoadCharTable(charfile as string) As boolean
		  Dim t As String
		  dim rowFromFile as string
		  
		  dim chrCod as string
		  dim chrVal as integer
		  
		  dim chrTable as new Collection
		  
		  dim i,tot as integer
		  
		  If Me.kCTs.Lookup(charfile, Nil) = Nil Then return False
		  
		  t = kCTs.Value(charfile).StringValue()
		  tot = CountFields(t, me.kCT__rowdelim)
		  
		  For i = 1 To tot
		    rowFromFile = t.NthField(me.kCT__rowdelim, i)
		    chrCod = rowFromFile.NthField(me.kCT__fielddelim,1)
		    chrVal = rowFromFile.NthField(me.kCT__fielddelim,2).Val
		    chrTable.Add chrVal, chrCod
		  Next
		  
		  CharWidths.Add chrTable, charfile
		  
		  return true
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub MultiCell(w as double, h as double, txt as string, border as variant = 0, align as string = "J", fill as integer = 0)
		  //Output text with automatic or explicit line breaks
		  dim cw as new collection
		  dim s,b,b2,c,letra as string
		  dim i,j,nb,ns,nl,sep,asc_val as integer
		  dim tmp,l,ls,wmax as double
		  
		  cw = collection(CurrentFont.Item("cw"))
		  
		  letra = CurrentFont.Item("name")
		  
		  if w = 0 then w = me.w - me.rMargin - me.x
		  
		  wmax = (w - 2 * me.cMargin) * 1000 / me.FontSize
		  
		  s = txt.ReplaceAll(chr(13),"")
		  nb = s.len
		  if nb > 0 and s.Right(1) = chr(10) then nb = nb - 1
		  
		  b = ""
		  
		  if(border) then
		    
		    if( vartype(border) = 2 and border.IntegerValue = 1) then
		      border="LTRB"
		      b="LRT"
		      b2="LR"
		    else
		      b2 = ""
		      if border.StringValue.instr("L") > 0 then b2 = b2 + "L"
		      if border.StringValue.InStr("R") > 0 then b2 = b2 + "R"
		      if border.StringValue.InStr("T") > 0 then b2 = b2 + "T"
		      b = b2
		    end if
		    
		  end if
		  
		  sep=-1
		  i=1
		  j=1
		  
		  l=0
		  ns=0
		  nl=1
		  
		  while i <= nb
		    
		    //Get next character
		    c = s.mid(i,1)
		    
		    //Explicit line break
		    if c = chr(10) then
		      
		      if(me.ws > 0) then
		        me.ws = 0
		        me.out("0 Tw")
		      end if
		      
		      me.Cell(w,h, s.mid(j, i-j), b, 2, align, fill)
		      
		      i = i + 1
		      sep = -1
		      j = i
		      l=0
		      ns=0
		      nl = nl + 1
		      
		      if(border and nl = 2) then b = b2
		      continue
		    end if
		    
		    
		    if(c = " ") then
		      sep=i
		      ls=l
		      ns = ns + 1
		    end if
		    
		    asc_val = c.Asc
		    if asc_val > 255 then asc_val = 64 // IF COMES OTHER THAN STANDARD ASCII OVERRIDE TO ASC 64
		    
		    tmp = cw.Item( str(asc_val) )
		    l = l + tmp
		    
		    
		    //Automatic line break
		    If(l > wmax) Then
		      
		      If(sep = -1) Then
		        
		        If(i=j) Then i = i + 1
		        
		        If(Me.ws > 0) Then
		          Me.ws = 0
		          Me.out("0 Tw")
		        End If
		        
		        Me.Cell(w,h,s.mid(j,i-j),b,2,align,fill)
		        
		      Else
		        
		        If(align ="J") Then
		          
		          If (ns > 1) Then
		            Me.ws = (wmax - ls) / 1000 * Me.FontSize / (ns-1)
		          Else
		            Me.ws = 0
		          End If
		          
		          Me.out(Me.fNum(Me.ws * Me.k,"0.000") + " Tw")
		          
		        End if
		        
		        me.Cell(w,h, s.mid(j,sep-j),b,2,align,fill)
		        i = sep + 1
		        
		      End If
		      
		      sep = -1
		      j = i
		      l= 0
		      ns = 0
		      nl = nl + 1
		      
		      If(border And nl = 2) Then b=b2
		      
		    else
		      
		      i = i + 1
		      
		    End If
		    
		  wend
		  
		  
		  //Last chunk
		  if (me.ws>0) then
		    me.ws=0
		    me.out("0 Tw")
		  end if
		  
		  if( vartype(border) = 8 and border.StringValue.InStr("B") > 0)  then b = b + "B"
		  me.cell(w,h,s.mid(j,i-j),b,2,align,fill)
		  me.x = me.lMargin
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub newobj()
		  //Begin a new object
		  
		  me.n = me.n + 1
		  me.offsets.add me.buffer.Len, str(me.n)
		  me.out(str(me.n) + " 0 obj")
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Open()
		  me.state = 1
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub out(s as string)
		  //gdf: Convert the text string encoding
		  
		  s = s.ConvertEncoding(me.InternalEncoding)
		  
		  //Add a line to the document
		  if (me.state = 2) then
		    me.pages(me.page) = me.pages(me.page) + s +  chr(10)
		  else
		    me.buffer = me.buffer + s +  chr(10)
		  end if
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Output(optional name as string = "", optional dest as string = "F")
		  Dim f As FolderItem
		  Dim t as BinaryStream
		  
		  //Output PDF to some destination
		  //Finish document if necessary
		  
		  if(me.state < 3) then me.Close()
		  
		  //Normalize parameters
		  if(name = "") then
		    name="doc.pdf"
		    dest="F"
		  end if
		  
		  
		  select case dest
		    
		    //Save to local file
		    
		  case "F"
		    f = GetFolderItem(name)
		    t = f.CreateBinaryFile("pdf")
		    t.Write me.buffer
		    t.close
		    f.Launch
		    
		  case "S"
		    //Return as a string
		    'return me.buffer
		    
		  case else
		    me.Error("Incorrect output destination: " + dest)
		    'return false
		    
		  end select
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function PageNo() As Integer
		  //Get current page number
		  return me.page
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Function parseJPG(file as string) As collection
		  dim f as FolderItem
		  dim t as BinaryStream
		  
		  dim a as new Collection
		  dim r as new Collection
		  
		  dim bpc as integer
		  dim colspace as string
		  dim data as string
		  
		  a = GetImageSize(file)
		  
		  if(a = nil) then
		    me.Error("Missing or incorrect image file: " + file)
		    return nil
		  end if
		  
		  
		  select case a.item("channels").IntegerValue
		    
		  case 3
		    colspace="DeviceRGB"
		  case 4
		    colspace="DeviceCMYK"
		  case else
		    colspace="DeviceGray"
		    
		  end select
		  
		  bpc = 8
		  if (a.item("bits") <> nil) then bpc = a.item("bits").IntegerValue
		  
		  f = GetFolderItem(file, FolderItem.PathTypeShell)
		  t = f.OpenAsBinaryFile
		  while not t.EOF
		    data = data + t.Read(4)
		  wend
		  
		  r.Add a.item("width").IntegerValue, "w"
		  r.Add a.item("height").IntegerValue, "h"
		  r.Add bpc, "bpc"
		  r.add colspace, "cs"
		  r.Add "DCTDecode", "f"
		  r.add data, "data"
		  
		  return r
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Function parsePNG(file as string) As collection
		  dim colspace,parms,pal,data,type as string
		  dim w,h,n as double
		  dim trns as Collection
		  
		  dim bpc,ct as Integer
		  
		  dim r as new Collection
		  Dim f As FolderItem
		  Dim t as BinaryStream
		  Dim strRead As String
		  Dim strCompare As String
		  
		  dim nulo as string
		  
		  f = GetFolderItem(file,  FolderItem.PathTypeShell)
		  t = f.OpenAsBinaryFile
		  
		  strRead=t.Read(8)
		  strCompare=chrb(137) + "PNG" + chr(13) + chr(10) + chr(26) + chr(10)
		  
		  //Check signature
		  'if( t.Read(8) <> chr(137) + "PNG" + chr(13) + chr(10) + chr(26) + chr(10)) then
		  if strRead <> strCompare then
		    me.Error("Not a PNG file: " + file)
		    return r
		  end if
		  
		  //Read header chunk
		  nulo = t.read(4)
		  
		  if(t.read(4) <> "IHDR") then
		    me.Error("Incorrect PNG file: " + file)
		    return r
		  end if
		  
		  w    = t.ReadInt32
		  h    = t.readint32
		  
		  bpc  = t.read(1).Asc
		  
		  if(bpc > 8) then
		    me.Error("16-bit depth not supported: " + file)
		    return r
		  end if
		  
		  ct = t.read(1).asc
		  
		  select case ct
		    
		  case 0
		    colspace ="DeviceGray"
		  case 2
		    colspace = "DeviceRGB"
		  case 3
		    colspace="Indexed"
		  case else
		    'me.Error("Alpha channel not supported: " + file)
		    r.Add "alpha"
		    return r
		    
		  end select
		  
		  if(t.read(1).asc <> 0) then
		    me.Error("Unknown compression method: " + file)
		    return r
		  end if
		  
		  if(t.read(1).asc <> 0) then
		    me.Error("Unknown filter method: " + file)
		    return r
		  end if
		  
		  if(t.read(1).asc <> 0) then
		    me.Error("Interlacing not supported: " + file)
		    return r
		  end if
		  
		  nulo = t.Read(4)
		  
		  parms = "/DecodeParms <</Predictor 15 /Colors "
		  
		  if (ct = 2) then
		    parms = parms + "3"
		  else
		    parms = parms + "1"
		  end if
		  
		  parms = parms + " /BitsPerComponent " + str(bpc) + " /Columns " + str(w) + ">>"
		  
		  //Scan chunks looking for palette, transparency and image data
		  pal  = ""
		  trns = new collection
		  data = ""
		  
		  do
		    
		    nulo = str(t.readint32)
		    n = nulo.CDbl
		    
		    type = t.read(4)
		    
		    select case type
		      
		    case "PLTE"
		      pal = t.read(n)
		      nulo = t.Read(4)
		      
		    case "tRNS"
		      
		      //Read transparency info
		      nulo = t.read(n)
		      
		      if(ct = 0) then
		        
		        trns.Add str(nulo.left(1).asc)
		        
		        
		      elseif(ct = 2) then
		        
		        trns.Add str(nulo.mid(1,1).asc)
		        trns.Add str(nulo.mid(3,1).asc)
		        trns.Add str(nulo.mid(5,1).asc)
		      else
		        if (nulo.inStr(chr(0)) > 0) then
		          trns.Add str(nulo.inStr(chr(0)))
		        end if
		      end if
		      
		      nulo = t.read(4)
		      
		    case "IDAT"
		      
		      //Read image data block
		      data = data + t.read(n)
		      nulo = t.read(4)
		      
		    case "IEND"
		      
		    case else
		      
		      nulo = t.read(n + 4)
		      
		    end select
		    
		  loop until(t.EOF)
		  
		  t.close
		  
		  if(colspace = "Indexed" AND  pal = "") then
		    me.Error("Missing palette in " + file)
		    return r
		  end if
		  
		  r.Add w, "w"
		  r.add h, "h"
		  r.add colspace, "cs"
		  r.add bpc, "bpc"
		  r.add "FlateDecode", "f"
		  r.add parms, "parms"
		  r.add pal, "pal"
		  r.add trns, "trns"
		  r.add data, "data"
		  
		  return r
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub putcatalog()
		  me.out("/Type /Catalog")
		  me.out("/Pages 1 0 R")
		  
		  select case me.ZoomMode
		    
		  case "fullpage"
		    me.out("/OpenAction [3 0 R /Fit]")
		    
		  case "fullwidth"
		    me.out("/OpenAction [3 0 R /FitH null]")
		    
		  case "real"
		    me.out("/OpenAction [3 0 R /XYZ null null 1]")
		    
		    '// POR AHORA NO!
		    '
		    'case else
		    '
		    'if IsNumeric(me.ZoomMode) then
		    'me.out("/OpenAction [3 0 R /XYZ null null " + str( me.ZoomMode.clCDbl / 100) + "]")
		    'else
		    'me.out("/OpenAction [3 0 R /XYZ null null 1]");
		    'end if
		    
		  end select
		  
		  select case me.LayoutMode
		  case "single"
		    me.out("/PageLayout /SinglePage")
		  case "continuous"
		    me.out("/PageLayout /OneColumn")
		  case "two"
		    me.out("/PageLayout /TwoColumnLeft")
		  end select
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub putfonts()
		  dim nf,i,j as Integer
		  dim font as new Collection
		  dim cw as new Collection
		  dim s,type,name,file as string
		  
		  nf = me.n
		  
		  for i = 1 to ubound(me.diffs)
		    
		    //Encodings
		    me.newobj()
		    me.out("<</Type /Encoding /BaseEncoding /WinAnsiEncoding /Differences [" + str(me.diffs(i)) + "]>>")
		    me.out("endobj")
		    
		  next
		  
		  
		  for i = 1 to fonts.Count
		    
		    collection(fonts.Item(i)).Add me.n+1,"n"
		    
		    
		    font = collection(fonts.Item(i))
		    
		    //Font objects
		    type = font.Item("type")
		    name = font.Item("name")
		    
		    if(type = "core") then
		      
		      //Standard font
		      me.newobj()
		      me.out("<</Type /Font")
		      me.out("/BaseFont /" + name)
		      me.out("/Subtype /Type1")
		      
		      if(name <> "Symbol" and name <> "ZapfDingbats") then
		        me.out("/Encoding /WinAnsiEncoding")
		      end if
		      
		      me.out(">>")
		      me.out("endobj")
		      
		    elseif(type = "Type1" or type = "TrueType") then
		      
		      //Additional Type1 or TrueType font
		      me.newobj()
		      me.out("<</Type /Font")
		      me.out("/BaseFont /" + name)
		      me.out("/Subtype /" + type)
		      me.out("/FirstChar 32 /LastChar 255")
		      me.out("/Widths " + str(me.n + 1) + " 0 R")
		      me.out("/FontDescriptor " + str(me.n + 2) + " 0 R")
		      
		      if(font.Item("enc") <> nil) then
		        
		        if(font.item("diff") <> nil) then
		          me.out("/Encoding " + str(nf + font.item("diff")) + " 0 R" )
		        else
		          me.out("/Encoding /WinAnsiEncoding")
		        end if
		        
		      end if
		      
		      me.out(">>")
		      me.out("endobj")
		      
		      //Widths
		      me.newobj()
		      
		      cw = font.Item("cw")
		      
		      s = "["
		      for j = 32 to 254
		        s = s + chr(cw.Item(str(j)) ) + " "
		      next j
		      s = s + "]"
		      
		      me.out(s)
		      me.out("endobj")
		      
		      //Descriptor
		      me.newobj()
		      s = "<</Type /FontDescriptor /FontName /" + name
		      
		      'foreach($font['desc'] as $k=>$v)
		      '$s.=' /'.$k.' '.$v;
		      
		      file = font.Item("file").StringValue
		      if(file) <> "" then
		        s = " /FontFile"
		        if font.Item("type") <> "Type1" then s = s + "2"
		        s = s + " " + collection(FontFiles.item(file)).Item("n").stringvalue + " 0 R"
		      end if
		      
		      me.out(s + ">>")
		      me.out("endobj")
		      
		      
		    end if
		    
		  next i
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub putheader()
		  me.out("%PDF-" + me.PDFVersion)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub putimages()
		  dim filter as string
		  dim info, cTrns as collection
		  dim file,pal,trns as string
		  dim i,j as integer
		  
		  info = new Collection
		  cTrns = new Collection
		  
		  if me.Compress then filter = "/Filter /FlateDecode "
		  
		  for j = 1 to me.Images.Count
		    
		    me.newobj()
		    
		    collection(Images.Item(j)).Add me.n, "n"
		    
		    info = collection(me.Images.Item(j))
		    
		    me.out("<</Type /XObject")
		    me.out("/Subtype /Image")
		    me.out("/Width " + str(info.item("w").Int32Value) )
		    me.out("/Height " + str(info.item("h").int32value) )
		    
		    if (info.Item("masked")<>"") then
		      me.out("/SMask " + Cstr(me.n-1) + " 0 R")
		    end if
		    
		    if(info.item("cs") = "Indexed") then
		      
		      me.out("/ColorSpace [/Indexed /DeviceRGB" + " " + str( info.item("pal").StringValue.len / 3 - 1 ) + " " + str( me.n + 1 ) + " 0 R]")
		      
		    else
		      
		      me.out("/ColorSpace /" + info.item("cs").StringValue)
		      if(info.item("cs").StringValue = "DeviceCMYK") then me.out("/Decode [1 0 1 0 1 0 1 0]")
		      
		    end if
		    
		    me.out("/BitsPerComponent " + info.item("bpc").StringValue )
		    
		    if(info.item("f") <> nil) then me.out("/Filter /" + info.item("f"))
		    
		    if(info.item("parms") <> nil) then me.out(info.item("parms"))
		    
		    cTrns = collection(info.Item("trns"))
		    
		    if(cTrns <> nil and cTrns.Count > 0) then
		      
		      trns = ""
		      
		      for i = 1 to cTrns.Count
		        
		        trns = trns + cTrns.Item(i)
		        me.out("/Mask [" + trns + "]")
		        
		      next i
		      
		    end if
		    
		    me.out("/Length " + str(info.item("data").StringValue.Len) + ">>")
		    me.putstream(info.item("data").StringValue)
		    me.out("endobj")
		    
		    // Palette
		    if( info.Item("cs").StringValue = "Indexed") then
		      
		      me.newobj()
		      
		      if me.compress then
		        pal = gzcompress(info.Item("pal"))
		      else
		        pal = info.item("pal")
		      end if
		      
		      me.out("<<" + filter + "/Length " + str(pal.len) + ">>")
		      me.putstream(pal)
		      me.out("endobj")
		      
		    end if
		    
		  next j
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub putinfo()
		  dim miDate as new date
		  dim stDate as string
		  
		  stDate = replaceall(replaceall(replaceall(str(miDate.SQLDateTime),"-",""),":","")," ","")
		  
		  me.out("/Producer " + me.textstring("rsFPDF v" + me.rsFPDFVersion + " by roblthegreat") )
		  
		  if(me.title <> "") then me.out("/Title " + me.textstring(me.title))
		  
		  if(me.subject <> "") then me.out("/Subject " + me.textstring(me.subject))
		  
		  if(me.author <> "") then me.out("/Author " + me.textstring(me.author))
		  
		  if(me.keywords <> "") then me.out("/Keywords " + me.textstring(me.keywords))
		  
		  if(me.creator <> "") then me.out("/Creator " + me.textstring(me.creator))
		  
		  me.out("/CreationDate " + me.textstring("D:" + stDate))
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub putpages()
		  dim n, nb as Integer
		  dim wPt,hPt as double
		  dim annots, filter,rect, p as string
		  dim kids as string
		  dim cLinks as new Collection
		  dim pl as new Collection
		  dim lnk as new Collection
		  
		  nb = me.page
		  
		  if(me.AliasNbPages <> "") then
		    
		    //Replace number of pages
		    for n = 1 to nb
		      me.pages(n) = me.pages(n).ReplaceAll(me.AliasNbPages,str(nb))
		    next
		    
		  end if
		  
		  if(me.DefOrientation = "P") then
		    wPt = me.fwPt
		    hPt = me.fhPt
		  else
		    wPt = me.fhPt
		    hPt = me.fwPt
		  end if
		  
		  if me.Compress then filter = "/Filter /FlateDecode "
		  
		  for n = 1 to nb
		    
		    //Page
		    me.newobj()
		    me.out("<</Type /Page")
		    me.out("/Parent 1 0 R")
		    
		    if( me.OrientationChanges.Item( str(n) ) ) then
		      me.out("/MediaBox [0 0 " + fNum(hPt,"0.00") + " " + fNum(wPt,"0.00") + "]")
		    end if
		    
		    me.out("/Resources 2 0 R")
		    
		    if(me.PageLinks.Count > 0) then
		      
		      //Links
		      
		      pl = Collection(me.PageLinks.Lookup(n,nil))
		      if pl<>nil then
		        annots = "/Annots ["
		        
		        'for i as integer = 0 to cLinks.Count
		        
		        'pl = cLinks.item(i)
		        
		        rect = fNum( pl.item("x"), "0.00") + " " + _
		        fNum( pl.item("y"), "0.00") + " " + _
		        fNum( pl.item("x") + pl.item("w"), "0.00") + " " + _
		        fNum( pl.item("y") - pl.item("h"), "0.00")
		        
		        annots = annots + "<</Type /Annot /Subtype /Link /Rect [" + rect + "] /Border [0 0 0] "
		        
		        if(not pl.item("link").IsArray ) then
		          
		          annots = annots + "/A <</S /URI /URI " + me.textstring(pl.item("link").StringValue) + ">>>>"
		          
		        else
		          
		          lnk = Collection( me.links.Item( pl.item("link").StringValue ) )
		          
		          if me.OrientationChanges.Item( lnk.Item("page").StringValue ) then
		            h = wPt
		          else
		            h = hPt
		          end if
		          
		          annots = annots + "/Dest [" + _
		          str(1 + 2 * lnk.item("page").IntegerValue ) + _
		          " 0 R /XYZ 0 " + _
		          fNum(h - lnk.item("y").DoubleValue * me.k,"0.00") + _
		          " null]>>"
		          
		        end if
		        
		        'next
		        
		        me.out(annots + "]")
		      end if
		    end if
		    
		    me.out("/Contents " + str(me.n + 1) + " 0 R>>")
		    me.out("endobj")
		    
		    //Page content
		    
		    if me.Compress then
		      p = gzcompress( me.pages(n) )
		    else
		      p = me.pages(n)
		    end if
		    
		    me.newobj()
		    me.out("<<" + filter + "/Length " + str(p.Len) + ">>")
		    me.putstream(p)
		    me.out("endobj")
		    
		  next
		  
		  //Pages root
		  me.offsets.add me.buffer.len,"1"
		  
		  me.out("1 0 obj")
		  me.out("<</Type /Pages")
		  
		  kids = "/Kids ["
		  
		  for n = 0 to nb - 1
		    kids = kids + str(3 + 2 * n) + " 0 R "
		  next n
		  
		  me.out(kids + "]")
		  me.out("/Count " + str(nb))
		  me.out( "/MediaBox [0 0 " + fNum(wPt,"0.00") + " " + fNum(hPt,"0.00") + "]")
		  me.out(">>")
		  me.out("endobj")
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub putresourcedict()
		  dim i as Integer
		  dim f as new Collection
		  
		  me.out("/ProcSet [/PDF /Text /ImageB /ImageC /ImageI]")
		  me.out("/Font <<")
		  
		  for i = 1 to me.Fonts.Count
		    f = me.Fonts.Item(i)
		    me.out("/F" + f.Item("i").StringValue + " " + f.item("n").StringValue + " 0 R")
		  next i
		  
		  me.out(">>")
		  me.out("/XObject <<")
		  me.putxobjectdict()
		  me.out(">>")
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub putresources()
		  me.putfonts()
		  me.putimages()
		  
		  //Resource dictionary
		  me.offsets.add me.buffer.Len, "2"
		  me.out("2 0 obj")
		  me.out("<<")
		  me.putresourcedict()
		  me.out(">>")
		  me.out("endobj")
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub putstream(s as string)
		  me.out("stream")
		  me.out(s)
		  me.out("endstream")
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub puttrailer()
		  
		  me.out("/Size " + str(me.n + 1) )
		  
		  me.out("/Root " + str(me.n + 0) + " 0 R")
		  
		  me.out("/Info " + str(me.n - 1) + " 0 R")
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub putxobjectdict()
		  dim i as Integer
		  dim im as new Collection
		  
		  if Images.Count = 0 then return
		  
		  for i = 1 to Images.Count
		    
		    im = collection(images.Item(i))
		    
		    me.out("/I" + im.item("i").StringValue + " " + im.item("n").StringValue  + " 0 R")
		    
		  next i
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Rect(x as double, y as double, w as double, h as double, optional style as string = "")
		  dim s as string
		  
		  //Draw a rectangle
		  s = fNum(x * me.k,"0.00") + " " + _
		  fNum((me.h - y) * me.k,"0.00") + " " + _
		  fNum(w * me.k,"0.00") + " " + _
		  fNum(-h * me.k,"0.00") + " re "
		  
		  select case style
		    
		  case "F"
		    s = s + "f"
		    
		  case "FD", "DF"
		    s = s + "B"
		    
		  case else
		    s = s + "S"
		    
		  end select
		  
		  me.out(s)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetAuthor(Author as string)
		  me.author = author
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetAutoPageBreak(auto as boolean, optional margin as integer = 0)
		  //Set auto page break mode and triggering margin
		  me.AutoPageBreak = auto
		  me.bMargin = margin
		  me.PageBreakTrigger = me.h - margin
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetCompression(compress as boolean)
		  
		  me.compress = compress
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetCreator(Creator as string)
		  //Creator of document
		  me.creator = creator
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetDisplayMode(Zoom as string, optional Layout as string = "continuous")
		  //Set display mode in viewer
		  
		  if (zoom ="fullpage" or zoom = "fullwidth" or zoom = "real" or zoom = "default") then
		    
		    me.ZoomMode = zoom
		    
		  else
		    
		    me.Error("Incorrect zoom display mode: " + zoom)
		    
		  end if
		  
		  if (layout = "single" or layout = "continuous" or layout = "two" or layout = "default") then
		    
		    me.LayoutMode = layout
		    
		  else
		    
		    me.Error("Incorrect layout display mode: " + layout)
		    
		  end if
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetDrawColor(r as integer, optional g as integer = - 1, optional b as integer = - 1)
		  
		  if( (r = 0 and g = 0 and b = 0) or g = -1 ) then
		    
		    me.DrawColor = fNum(r / 255,"0.000") + " G"
		    
		  else
		    
		    me.DrawColor = fNum(r / 255,"0.000") + " " + _
		    fNum(g / 255,"0.000") + " " + _
		    fNum(b / 255,"0.000") + " RG"
		    
		  end if
		  
		  if (me.page > 0) then me.out(me.DrawColor)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetEnconding(enc as textEncoding)
		  //gdf: set the string encoding used internally by rsFPDF
		  //note that FPDF does not support UTF, but this method allows
		  //to convert to other encodings, such as WindowsLatin1
		  //(Windows 1252, with Euro sign) or
		  //ISOLatin9 (ISO 8859-15, with Euro sign)
		  me.InternalEncoding = enc
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetFillColor(r as integer, optional g as integer = - 1, optional b as integer = - 1)
		  
		  if( (r = 0 and g = 0 and b = 0) or g = -1 ) then
		    
		    me.FillColor = fNum(r / 255,"0.000") + " G"
		    
		  else
		    
		    me.FillColor = fNum(r / 255,"0.000") + " " + _
		    fNum(g / 255,"0.000") + " " + _
		    fNum(b / 255,"0.000") + " rg"
		    
		  end if
		  
		  me.ColorFlag = not (me.FillColor = me.TextColor)
		  
		  if(me.page > 0) then me.out(me.FillColor)
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetFont(family as string, optional style as string = "", optional size as integer = 0)
		  //Select a font; size given in points
		  
		  dim fontkey,file as string
		  dim fontinfo as new Collection
		  
		  family = family.Lowercase
		  
		  select case family
		    
		  case ""
		    family = me.FontFamily
		    
		  case "arial"
		    family = "helvetica"
		    
		  case "symbol", "zapfdingbats"
		    Style = ""
		    
		  end select
		  
		  Style = Style.Uppercase
		  
		  if( Style.InStr("U") > 0) then
		    me.Underline = true
		    Style = Style.Replace("U","")
		    
		  else
		    
		    me.Underline = false
		    
		  end if
		  
		  if(Style = "IB") then Style = "BI"
		  
		  if(Size =0) then Size = me.FontSizePt
		  
		  //Test if font is already selected
		  if (me.FontFamily = family AND me.FontStyle = Style AND me.FontSizePt = Size) then return
		  
		  //Test if used for the first time
		  fontkey = family + style
		  
		  
		  if( me.Fonts.Item(fontkey) = nil ) then
		    
		    //Check if one of the standard fonts
		    if( me.CoreFonts.Item(fontkey) <> nil ) then
		      
		      if( me.CharWidths.Item(fontkey) = nil ) then
		        
		        // Load metric file
		        file = family
		        
		        if(family = "times" or family = "helvetica") then file = file + style.Lowercase
		        
		        if ( me.LoadCharTable(file) = false) then me.Error("Could not include font file")
		        
		        if( me.CharWidths.Item(file) = nil ) then me.Error("Could not include font metric file")
		        
		      end if
		      
		      fontinfo.Add me.Fonts.Count + 1, "i"
		      fontinfo.Add "core", "type"
		      fontinfo.Add me.CoreFonts.Item(fontkey), "name"
		      fontinfo.Add -100, "up"
		      fontinfo.Add 50, "ut"
		      fontinfo.Add me.CharWidths.Item(fontkey), "cw"
		      
		      me.fonts.Add fontinfo, fontkey
		      
		    else
		      
		      me.Error("Undefined font: " + family + " " + style)
		      
		    end if
		    
		  end if
		  
		  // Select it
		  me.FontFamily = family
		  me.FontStyle = style
		  me.FontSizePt = size
		  me.FontSize = size / me.k
		  
		  me.CurrentFont = collection(me.fonts.Item(fontkey))
		  
		  if( me.page > 0 ) then me.out("BT /F" + me.CurrentFont.Item("i") + " " + fNum(me.FontSizePt,"0.00") + " Tf ET")
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetFontSize(size as integer)
		  dim s as string
		  
		  //Set font size in points
		  if (me.FontSizePt = size) then return
		  
		  me.FontSizePt = size
		  me.FontSize   = size / me.k
		  
		  if(me.page > 0) then
		    s = "BT /F" + me.CurrentFont.Item("i") + " " + fNum(me.FontSizePt,"0.00") + "Tf ET"
		    me.out(s)
		  end if
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetKeywords(keywords as string)
		  //Keywords of document
		  me.keywords = keywords
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetLeftMargin(margin as double)
		  //Set left margin
		  
		  me.lMargin = margin
		  if(me.page > 0 AND me.x < margin) then me.x = margin
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetLineWidth(width as Double)
		  //Set line width
		  
		  me.LineWidth = width
		  
		  if(me.page > 0 )  then
		    
		    me.out( fNum(width * me.k,"0.00") + " w" )
		    
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetLink(link as string, y as double = - 1, page as integer = - 1)
		  dim detLink as new Collection
		  
		  //Set destination of internal link
		  if(y = -1) then y = me.y
		  
		  if(page = -1) then page = me.page
		  
		  detLink = Collection(me.links.Item(link))
		  
		  detLink.add array(page,y), link
		  
		  //detLink.Add page, "page"
		  //detLink.add y, "y"
		  //me.links.Add detLink, link
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetMargins(left as double, top as double, optional right as double = - 1)
		  
		  //Set left, top and right margins
		  
		  me.lMargin=left
		  me.tMargin=top
		  
		  if( right = -1) then right = left
		  
		  me.rMargin = right
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetRightMargin(margin as double)
		  
		  //Set right margin
		  me.rMargin = margin
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetSubject(subject as string)
		  //Subject of document
		  me.subject = subject
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetTextColor(r as integer, optional g as integer = - 1, optional b as integer = - 1)
		  
		  if( (r = 0 and g = 0 and b = 0) or g = -1 ) then
		    
		    me.textcolor = fNum(r / 255,"0.000") + " G"
		    
		  else
		    
		    me.textcolor = fNum(r / 255,"0.000") + " " + _
		    fNum(g / 255,"0.000") + " " + _
		    fNum(b / 255,"0.000") + " rg"
		    
		  end if
		  
		  me.ColorFlag = not (me.FillColor = me.TextColor)
		  
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetTitle(title as string)
		  
		  //Title of document
		  me.title = title
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetTopMargin(margin as double)
		  //Set top margin
		  me.tMargin = margin
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetX(x as double)
		  //Set x position
		  
		  if(x >= 0) then
		    me.x = x
		  else
		    me.x = me.w + x
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetXY(x as double, y as double)
		  //Set x and y positions
		  //Don't Alter order 'cause SetY reset Position (Thanks Gilberto De Faveri)
		  me.setY(y)
		  me.setX(x)
		  
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetY(y as double)
		  //Set y position and reset x
		  
		  me.x = me.lMargin
		  
		  if(y >= 0) then
		    me.y = y
		  else
		    me.y = me.h + y
		  end if
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function str_Repeat(strString As String, iAmount As Integer) As String
		  Dim strRet As String
		  For i As Integer=1 to iAmount
		    strRet=strRet + strString
		  Next 
		  Return strRet
		End Function
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Function substr_count(cadena as string, caracter as string) As integer
		  dim i as double
		  dim j as double
		  
		  j = 0
		  
		  for i = 1 to cadena.Len
		    if cadena.mid(i,1) = caracter then j = j + 1
		  next i
		  
		  return j
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Text(x as double, y as double, txt as string)
		  dim s as string
		  
		  //Output a string
		  s = "BT " + _
		  fNum(x * me.k,"0.00") + " " + _
		  fNum((me.h - y) * me.k,"0.00") + " " + _
		  "Td " + me.textstring (txt) + " Tj ET"
		  
		  
		  if(me.underline AND txt <> "") then s = s + " " + me.dounderline(x,y,txt)
		  
		  if(me.ColorFlag) then s = s + "q " + me.TextColor + " " + s + " Q"
		  
		  me.out(s)
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Function textstring(s as string) As string
		  //Format a text string
		  return "(" + me.Escape(s) + ")"
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Write(ph as double, txt as string, optional link as string = "")
		  // THIS PIECE OF CODE WAS PORTED BY DAN HARDING .. THANKS DAN
		  
		  Dim cw as new Collection
		  Dim wTemp as double
		  Dim wmax as double
		  Dim s as string
		  Dim nb as integer
		  Dim sep as integer
		  Dim i as integer
		  Dim j as integer
		  Dim l as integer
		  Dim nl as integer
		  Dim c as string
		  
		  //Output text in flowing mode
		  
		  cw = collection(CurrentFont.Item("cw"))
		  wTemp = me.w - me.rMargin - me.x
		  wmax = (wTemp - 2 * me.cMargin) * 1000 / me.FontSize
		  s = txt.ReplaceAll(chr(13), "")
		  nb = s.Len
		  sep = -1
		  i = 0
		  j = 0
		  l = 0
		  nl = 1
		  
		  do
		    
		    if i >= nb then exit
		    
		    //Get next character
		    c = s.Mid(i + 1, 1)
		    
		    if c = chr(10) then // Explicit line break
		      
		      me.Cell(wTemp, ph, s.Mid(j + 1, i - j), 0, 2, "", 0, link)
		      i = i + 1
		      sep = -1
		      j = i
		      l = 0
		      
		      if nl = 1 then
		        me.x = me.lMargin
		        wTemp = me.w - me.rMargin - me.x
		        wmax = (wTemp - 2 * me.cMargin) * 1000 / me.FontSize
		      end if
		      
		      nl = nl + 1
		      
		      Continue
		      
		    end if
		    
		    if c = " " then sep = i
		    
		    l = l + cw.Item(str(c.asc))
		    
		    if l > wmax then //Automatic line break
		      
		      if sep = -1 then
		        
		        if me.x > me.lMargin then //Move to next line
		          me.x = me.lMargin
		          me.y = me.y + ph
		          wTemp = me.w - me.rMargin - me.x
		          wmax = ( w - 2 * me.cMargin ) * 1000 / me.FontSize
		          i = i + 1
		          nl = nl + 1
		          Continue
		        end if
		        
		        if i = j then i = i + 1
		        
		        me.Cell(wTemp, ph, s.Mid(j + 1, i - j), 0, 2, "", 0, link)
		      else
		        me.Cell(wTemp, ph, s.Mid(j + 1, sep - j), 0, 2, "", 0, link)
		        i = sep + 1
		      end if
		      
		      sep = -1
		      j = i
		      l = 0
		      
		      if nl = 1 then
		        me.x = me.lMargin
		        wTemp = me.w - me.rMargin - me.x
		        wmax = ( wTemp - 2 * me.cMargin) * 1000 / me.FontSize
		      end if
		      
		      nl = nl + 1
		    else
		      
		      i = i + 1
		    end if
		    
		  loop
		  
		  //Last chunk
		  if i <> j then
		    me.Cell( l /1000 * me.FontSize, ph, s.Mid(j + 1), 0, 0, "", 0, link)
		  end if
		  
		End Sub
	#tag EndMethod


	#tag Note, Name = LICENSE
		
		RPDF es FREEWARE al igual que FPDF (el proyecto en el cual está basada esta clase). 
		No hay limitaciones de uso. Puede usarlo libre y gratuitamente en su aplicación (comercial o no), con o sin modificaciones.
		
		RPDF is FREEWARE like FPDF (the proyect where this class is based on).
		There is no use limitations. It can be used in free or commercial applications, with/out modifications.
		
		
		2007-2008 by diego2k
		
		Contact me: diego2k[at]gmail[dot]com  
		
		
		TODO:
		   - True Type embedding and complete support
		   - Improbe Links support
		
		Release 08.03.11
		+ JPG Image support
		+ Links Support
		+ Add GZIP Support throught ZLIB (Thanks Gilberto De Faveri)
		+ Add Write method (Thanks to Dan Harding)
		+ Add SetEncoding to change the internal RPDF text encoding (Thanks to Gilberto De Faveri)
		* Eliminate the need of have Font folder (Thanks Gilberto De Faveri)
		* Fixed SetXY method (Thanks Roberto Tremonti)
		* Fixed some font metrics (Thanks Roberto Tremonti)
		* Fixed bug on multicell method (Thanks to Gilberto De Faveri)
		* h and fontsize changed from private property to protected
		
		Release 08.02.22
		+ I Change the number release version to fit the date so it make sense :D
		+ Add Multicell Method
		* changed lasth property to double for Inch support  (thanks Dan Harding)
		* Fixed setfillcolor
		* Fixed settextcolor
		* Fixed Cell Method. Now it call to acceptpagebreak() method instead of evaluate the property.
		* Fixed Font Metrics of some fonts definition that was wrong
		
		Thanks to people who contribute to the project:
		* FPDF (http://www.fpdf.org/)
		* Luis Melgratti
		* Peter Stys 
		* Jamesee
		* Forisco
		* Computercoder
		* Dan Harding
		* Gilberto De Faveri (http://www.omnidea.it/)
		* Roberto Tremonti
	#tag EndNote


	#tag Property, Flags = &h1
		#tag Note
			// Alias for total number of pages
		#tag EndNote
		Protected AliasNbPages As String = "{nb}"
	#tag EndProperty

	#tag Property, Flags = &h1
		#tag Note
			// Author
		#tag EndNote
		Protected Author As String
	#tag EndProperty

	#tag Property, Flags = &h1
		Protected AutoPageBreak As Boolean = True
	#tag EndProperty

	#tag Property, Flags = &h1
		#tag Note
			//page break margin
		#tag EndNote
		Protected bMargin As Double
	#tag EndProperty

	#tag Property, Flags = &h1
		#tag Note
			//buffer holding in-memory PDF
		#tag EndNote
		Protected buffer As String
	#tag EndProperty

	#tag Property, Flags = &h1
		Protected CharWidths As Collection
	#tag EndProperty

	#tag Property, Flags = &h1
		#tag Note
			//cell margin
		#tag EndNote
		Protected cMargin As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		#tag Note
			//indicates whether fill and text colors are different
		#tag EndNote
		Private ColorFlag As Boolean = True
	#tag EndProperty

	#tag Property, Flags = &h21
		#tag Note
			//compression flag
		#tag EndNote
		Private Compress As Boolean = True
	#tag EndProperty

	#tag Property, Flags = &h21
		#tag Note
			//array of standard font names
		#tag EndNote
		Private CoreFonts As Collection
	#tag EndProperty

	#tag Property, Flags = &h21
		#tag Note
			// Creator
		#tag EndNote
		Private Creator As String
	#tag EndProperty

	#tag Property, Flags = &h21
		#tag Note
			//current orientatio
		#tag EndNote
		Private CurOrientation As String
	#tag EndProperty

	#tag Property, Flags = &h21
		Private CurrentFont As Collection
	#tag EndProperty

	#tag Property, Flags = &h21
		#tag Note
			//default orientation
		#tag EndNote
		Private DefOrientation As String = "P"
	#tag EndProperty

	#tag Property, Flags = &h21
		Private diffs() As int32
	#tag EndProperty

	#tag Property, Flags = &h21
		#tag Note
			//commands for drawing color
		#tag EndNote
		Private DrawColor As String
	#tag EndProperty

	#tag Property, Flags = &h21
		#tag Note
			//dimensions of page format in user unit
		#tag EndNote
		Private fh As double
	#tag EndProperty

	#tag Property, Flags = &h21
		#tag Note
			//dimensions of page format in points
		#tag EndNote
		Private fhPt As double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private FillColor As String
	#tag EndProperty

	#tag Property, Flags = &h21
		Private FontFamily As String
	#tag EndProperty

	#tag Property, Flags = &h21
		Private FontFiles As Collection
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Fonts As collection
	#tag EndProperty

	#tag Property, Flags = &h1
		Protected FontSize As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private FontSizePt As Integer
	#tag EndProperty

	#tag Property, Flags = &h21
		Private FontStyle As String
	#tag EndProperty

	#tag Property, Flags = &h21
		#tag Note
			//dimensions of page format in user unit
		#tag EndNote
		Private fw As double
	#tag EndProperty

	#tag Property, Flags = &h21
		#tag Note
			//dimensions of page format in points
		#tag EndNote
		Private fwPt As double
	#tag EndProperty

	#tag Property, Flags = &h1
		#tag Note
			//current dimensions of page in user unit
		#tag EndNote
		Protected h As double
	#tag EndProperty

	#tag Property, Flags = &h21
		#tag Note
			//current dimensions of page in points
		#tag EndNote
		Private hPt As double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Images As collection
	#tag EndProperty

	#tag Property, Flags = &h21
		#tag Note
			//flag set when processing footer
		#tag EndNote
		Private InFooter As Boolean = False
	#tag EndProperty

	#tag Property, Flags = &h21
		#tag Note
			//gdf: string encoding used internally by rsFPDF
		#tag EndNote
		Private InternalEncoding As TextEncoding
	#tag EndProperty

	#tag Property, Flags = &h21
		#tag Note
			//scale factor (number of points in user unit)
		#tag EndNote
		Private k As double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private kCTs As Dictionary
	#tag EndProperty

	#tag Property, Flags = &h21
		#tag Note
			// Keywords
		#tag EndNote
		Private Keywords As String
	#tag EndProperty

	#tag Property, Flags = &h21
		Private lasth As double
	#tag EndProperty

	#tag Property, Flags = &h21
		#tag Note
			// Layout display mode
		#tag EndNote
		Private LayoutMode As String = "continuous"
	#tag EndProperty

	#tag Property, Flags = &h21
		Private LineWidth As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private links As Collection
	#tag EndProperty

	#tag Property, Flags = &h1
		#tag Note
			//left margin
		#tag EndNote
		Protected lMargin As double
	#tag EndProperty

	#tag Property, Flags = &h21
		#tag Note
			//current object number
		#tag EndNote
		Private n As Integer
	#tag EndProperty

	#tag Property, Flags = &h21
		#tag Note
			//array of object offsets
		#tag EndNote
		Private offsets As Collection
	#tag EndProperty

	#tag Property, Flags = &h21
		#tag Note
			//array indicating orientation changes
		#tag EndNote
		Private OrientationChanges As Collection
	#tag EndProperty

	#tag Property, Flags = &h21
		#tag Note
			// current page number
		#tag EndNote
		Private Page As Integer = 0
	#tag EndProperty

	#tag Property, Flags = &h21
		Private PageBreakTrigger As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private PageLinks As dictionary
	#tag EndProperty

	#tag Property, Flags = &h21
		#tag Note
			//array containing pages
		#tag EndNote
		Private pages() As String
	#tag EndProperty

	#tag Property, Flags = &h21
		Private PDFVersion As String = "1.3"
	#tag EndProperty

	#tag Property, Flags = &h1
		#tag Note
			//right margin
		#tag EndNote
		Protected rMargin As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private rsFPDFVersion As String = "11.07.31"
	#tag EndProperty

	#tag Property, Flags = &h21
		#tag Note
			//current document state
		#tag EndNote
		Private state As Integer
	#tag EndProperty

	#tag Property, Flags = &h21
		#tag Note
			// Subject
		#tag EndNote
		Private Subject As String
	#tag EndProperty

	#tag Property, Flags = &h21
		Private TextColor As string = "0 G"
	#tag EndProperty

	#tag Property, Flags = &h1
		#tag Note
			// Title
		#tag EndNote
		Protected Title As String
	#tag EndProperty

	#tag Property, Flags = &h1
		#tag Note
			//top margin
		#tag EndNote
		Protected tMargin As double
	#tag EndProperty

	#tag Property, Flags = &h0
		tmpfiles As Dictionary
	#tag EndProperty

	#tag Property, Flags = &h1
		Protected Underline As Boolean = False
	#tag EndProperty

	#tag Property, Flags = &h1
		#tag Note
			//current dimensions of page in user unit
		#tag EndNote
		Protected w As double
	#tag EndProperty

	#tag Property, Flags = &h1
		#tag Note
			//current dimensions of page in points
		#tag EndNote
		Protected wPt As double
	#tag EndProperty

	#tag Property, Flags = &h1
		#tag Note
			// Word spacing
		#tag EndNote
		Protected ws As Double = 0
	#tag EndProperty

	#tag Property, Flags = &h1
		#tag Note
			//current position in user unit for cell positioning
		#tag EndNote
		Protected x As Double
	#tag EndProperty

	#tag Property, Flags = &h1
		#tag Note
			//current position in user unit for cell positioning
		#tag EndNote
		Protected y As Double
	#tag EndProperty

	#tag Property, Flags = &h1
		#tag Note
			// Zoom display mode
		#tag EndNote
		Protected ZoomMode As String = "fullpage"
	#tag EndProperty


	#tag Constant, Name = kCT_courier, Type = String, Dynamic = False, Default = \"0;600:1;600:2;600:3;600:4;600:5;600:6;600:7;600:8;600:9;600:10;600:11;600:12;600:13;600:14;600:15;600:16;600:17;600:18;600:19;600:20;600:21;600:22;600:23;600:24;600:25;600:26;600:27;600:28;600:29;600:30;600:31;600:32;600:33;600:34;600:35;600:36;600:37;600:38;600:39;600:40;600:41;600:42;600:43;600:44;600:45;600:46;600:47;600:48;600:49;600:50;600:51;600:52;600:53;600:54;600:55;600:56;600:57;600:58;600:59;600:60;600:61;600:62;600:63;600:64;600:65;600:66;600:67;600:68;600:69;600:70;600:71;600:72;600:73;600:74;600:75;600:76;600:77;600:78;600:79;600:80;600:81;600:82;600:83;600:84;600:85;600:86;600:87;600:88;600:89;600:90;600:91;600:92;600:93;600:94;600:95;600:96;600:97;600:98;600:99;600:100;600:101;600:102;600:103;600:104;600:105;600:106;600:107;600:108;600:109;600:110;600:111;600:112;600:113;600:114;600:115;600:116;600:117;600:118;600:119;600:120;600:121;600:122;600:123;600:124;600:125;600:126;600:127;600:128;600:129;600:130;600:131;600:132;600:133;600:134;600:135;600:136;600:137;600:138;600:139;600:140;600:141;600:142;600:143;600:144;600:145;600:146;600:147;600:148;600:149;600:150;600:151;600:152;600:153;600:154;600:155;600:156;600:157;600:158;600:159;600:160;600:161;600:162;600:163;600:164;600:165;600:166;600:167;600:168;600:169;600:170;600:171;600:172;600:173;600:174;600:175;600:176;600:177;600:178;600:179;600:180;600:181;600:182;600:183;600:184;600:185;600:186;600:187;600:188;600:189;600:190;600:191;600:192;600:193;600:194;600:195;600:196;600:197;600:198;600:199;600:200;600:201;600:202;600:203;600:204;600:205;600:206;600:207;600:208;600:209;600:210;600:211;600:212;600:213;600:214;600:215;600:216;600:217;600:218;600:219;600:220;600:221;600:222;600:223;600:224;600:225;600:226;600:227;600:228;600:229;600:230;600:231;600:232;600:233;600:234;600:235;600:236;600:237;600:238;600:239;600:240;600:241;600:242;600:243;600:244;600:245;600:246;600:247;600:248;600:249;600:250;600:251;600:252;600:253;600:254;600:255;600:", Scope = Private
	#tag EndConstant

	#tag Constant, Name = kCT_helvetica, Type = String, Dynamic = False, Default = \"0;278:1;278:2;278:3;278:4;278:5;278:6;278:7;278:8;278:9;278:10;278:11;278:12;278:13;278:14;278:15;278:16;278:17;278:18;278:19;278:20;278:21;278:22;278:23;278:24;278:25;278:26;278:27;278:28;278:29;278:30;278:31;278:32;278:33;278:34;355:35;556:36;556:37;889:38;667:39;191:40;333:41;333:42;389:43;584:44;278:45;333:46;278:47;278:48;556:49;556:50;556:51;556:52;556:53;556:54;556:55;556:56;556:57;556:58;278:59;278:60;584:61;584:62;584:63;556:64;1015:65;667:66;667:67;722:68;722:69;667:70;611:71;778:72;722:73;278:74;500:75;667:76;556:77;833:78;722:79;778:80;667:81;778:82;722:83;667:84;611:85;722:86;667:87;944:88;667:89;667:90;611:91;278:92;278:93;278:94;469:95;556:96;333:97;556:98;556:99;500:100;556:101;556:102;278:103;556:104;556:105;222:106;222:107;500:108;222:109;833:110;556:111;556:112;556:113;556:114;333:115;500:116;278:117;556:118;500:119;722:120;500:121;500:122;500:123;334:124;260:125;334:126;584:127;350:128;556:129;350:130;222:131;556:132;333:133;1000:134;556:135;556:136;333:137;1000:138;667:139;333:140;1000:141;350:142;611:143;350:144;350:145;222:146;222:147;333:148;333:149;350:150;556:151;1000:152;333:153;1000:154;500:155;333:156;944:157;350:158;500:159;667:160;278:161;333:162;556:163;556:164;556:165;556:166;260:167;556:168;333:169;737:170;370:171;556:172;584:173;333:174;737:175;333:176;400:177;584:178;333:179;333:180;333:181;556:182;537:183;278:184;333:185;333:186;365:187;556:188;834:189;834:190;834:191;611:192;667:193;667:194;667:195;667:196;667:197;667:198;1000:199;722:200;667:201;667:202;667:203;667:204;278:205;278:206;278:207;278:208;722:209;722:210;778:211;778:212;778:213;778:214;778:215;584:216;778:217;722:218;722:219;722:220;722:221;667:222;667:223;611:224;556:225;556:226;556:227;556:228;556:229;556:230;889:231;500:232;556:233;556:234;556:235;556:236;278:237;278:238;278:239;278:240;556:241;556:242;556:243;556:244;556:245;556:246;556:247;584:248;611:249;556:250;556:251;556:252;556:253;500:254;556:255;500:", Scope = Private
	#tag EndConstant

	#tag Constant, Name = kCT_helveticab, Type = String, Dynamic = False, Default = \"0;278:1;278:2;278:3;278:4;278:5;278:6;278:7;278:8;278:9;278:10;278:11;278:12;278:13;278:14;278:15;278:16;278:17;278:18;278:19;278:20;278:21;278:22;278:23;278:24;278:25;278:26;278:27;278:28;278:29;278:30;278:31;278:32;278:33;333:34;474:35;556:36;556:37;889:38;722:39;238:40;333:41;333:42;389:43;584:44;278:45;333:46;278:47;278:48;556:49;556:50;556:51;556:52;556:53;556:54;556:55;556:56;556:57;556:58;333:59;333:60;584:61;584:62;584:63;611:64;975:65;722:66;722:67;722:68;722:69;667:70;611:71;778:72;722:73;278:74;556:75;722:76;611:77;833:78;722:79;778:80;667:81;778:82;722:83;667:84;611:85;722:86;667:87;944:88;667:89;667:90;611:91;333:92;278:93;333:94;584:95;556:96;333:97;556:98;611:99;556:100;611:101;556:102;333:103;611:104;611:105;278:106;278:107;556:108;278:109;889:110;611:111;611:112;611:113;611:114;389:115;556:116;333:117;611:118;556:119;778:120;556:121;556:122;500:123;389:124;280:125;389:126;584:127;350:128;556:129;350:130;278:131;556:132;500:133;1000:134;556:135;556:136;333:137;1000:138;667:139;333:140;1000:141;350:142;611:143;350:144;350:145;278:146;278:147;500:148;500:149;350:150;556:151;1000:152;333:153;1000:154;556:155;333:156;944:157;350:158;500:159;667:160;278:161;333:162;556:163;556:164;556:165;556:166;280:167;556:168;333:169;737:170;370:171;556:172;584:173;333:174;737:175;333:176;400:177;584:178;333:179;333:180;333:181;611:182;556:183;278:184;333:185;333:186;365:187;556:188;834:189;834:190;834:191;611:192;722:193;722:194;722:195;722:196;722:197;722:198;1000:199;722:200;667:201;667:202;667:203;667:204;278:205;278:206;278:207;278:208;722:209;722:210;778:211;778:212;778:213;778:214;778:215;584:216;778:217;722:218;722:219;722:220;722:221;667:222;667:223;611:224;556:225;556:226;556:227;556:228;556:229;556:230;889:231;556:232;556:233;556:234;556:235;556:236;278:237;278:238;278:239;278:240;611:241;611:242;611:243;611:244;611:245;611:246;611:247;584:248;611:249;611:250;611:251;611:252;611:253;556:254;611:255;556:", Scope = Private
	#tag EndConstant

	#tag Constant, Name = kCT_helveticabi, Type = String, Dynamic = False, Default = \"0;278:1;278:2;278:3;278:4;278:5;278:6;278:7;278:8;278:9;278:10;278:11;278:12;278:13;278:14;278:15;278:16;278:17;278:18;278:19;278:20;278:21;278:22;278:23;278:24;278:25;278:26;278:27;278:28;278:29;278:30;278:31;278:32;278:33;333:34;474:35;556:36;556:37;889:38;722:39;238:40;333:41;333:42;389:43;584:44;278:45;333:46;278:47;278:48;556:49;556:50;556:51;556:52;556:53;556:54;556:55;556:56;556:57;556:58;333:59;333:60;584:61;584:62;584:63;611:64;975:65;722:66;722:67;722:68;722:69;667:70;611:71;778:72;722:73;278:74;556:75;722:76;611:77;833:78;722:79;778:80;667:81;778:82;722:83;667:84;611:85;722:86;667:87;944:88;667:89;667:90;611:91;333:92;278:93;333:94;584:95;556:96;333:97;556:98;611:99;556:100;611:101;556:102;333:103;611:104;611:105;278:106;278:107;556:108;278:109;889:110;611:111;611:112;611:113;611:114;389:115;556:116;333:117;611:118;556:119;778:120;556:121;556:122;500:123;389:124;280:125;389:126;584:127;350:128;556:129;350:130;278:131;556:132;500:133;1000:134;556:135;556:136;333:137;1000:138;667:139;333:140;1000:141;350:142;611:143;350:144;350:145;278:146;278:147;500:148;500:149;350:150;556:151;1000:152;333:153;1000:154;556:155;333:156;944:157;350:158;500:159;667:160;278:161;333:162;556:163;556:164;556:165;556:166;280:167;556:168;333:169;737:170;370:171;556:172;584:173;333:174;737:175;333:176;400:177;584:178;333:179;333:180;333:181;611:182;556:183;278:184;333:185;333:186;365:187;556:188;834:189;834:190;834:191;611:192;722:193;722:194;722:195;722:196;722:197;722:198;1000:199;722:200;667:201;667:202;667:203;667:204;278:205;278:206;278:207;278:208;722:209;722:210;778:211;778:212;778:213;778:214;778:215;584:216;778:217;722:218;722:219;722:220;722:221;667:222;667:223;611:224;556:225;556:226;556:227;556:228;556:229;556:230;889:231;556:232;556:233;556:234;556:235;556:236;278:237;278:238;278:239;278:240;611:241;611:242;611:243;611:244;611:245;611:246;611:247;584:248;611:249;611:250;611:251;611:252;611:253;556:254;611:255;556:", Scope = Private
	#tag EndConstant

	#tag Constant, Name = kCT_helveticai, Type = String, Dynamic = False, Default = \"0;278:1;278:2;278:3;278:4;278:5;278:6;278:7;278:8;278:9;278:10;278:11;278:12;278:13;278:14;278:15;278:16;278:17;278:18;278:19;278:20;278:21;278:22;278:23;278:24;278:25;278:26;278:27;278:28;278:29;278:30;278:31;278:32;278:33;278:34;355:35;556:36;556:37;889:38;667:39;191:40;333:41;333:42;389:43;584:44;278:45;333:46;278:47;278:48;556:49;556:50;556:51;556:52;556:53;556:54;556:55;556:56;556:57;556:58;278:59;278:60;584:61;584:62;584:63;556:64;1015:65;667:66;667:67;722:68;722:69;667:70;611:71;778:72;722:73;278:74;500:75;667:76;556:77;833:78;722:79;778:80;667:81;778:82;722:83;667:84;611:85;722:86;667:87;944:88;667:89;667:90;611:91;278:92;278:93;278:94;469:95;556:96;333:97;556:98;556:99;500:100;556:101;556:102;278:103;556:104;556:105;222:106;222:107;500:108;222:109;833:110;556:111;556:112;556:113;556:114;333:115;500:116;278:117;556:118;500:119;722:120;500:121;500:122;500:123;334:124;260:125;334:126;584:127;350:128;556:129;350:130;222:131;556:132;333:133;1000:134;556:135;556:136;333:137;1000:138;667:139;333:140;1000:141;350:142;611:143;350:144;350:145;222:146;222:147;333:148;333:149;350:150;556:151;1000:152;333:153;1000:154;500:155;333:156;944:157;350:158;500:159;667:160;278:161;333:162;556:163;556:164;556:165;556:166;260:167;556:168;333:169;737:170;370:171;556:172;584:173;333:174;737:175;333:176;400:177;584:178;333:179;333:180;333:181;556:182;537:183;278:184;333:185;333:186;365:187;556:188;834:189;834:190;834:191;611:192;667:193;667:194;667:195;667:196;667:197;667:198;1000:199;722:200;667:201;667:202;667:203;667:204;278:205;278:206;278:207;278:208;722:209;722:210;778:211;778:212;778:213;778:214;778:215;584:216;778:217;722:218;722:219;722:220;722:221;667:222;667:223;611:224;556:225;556:226;556:227;556:228;556:229;556:230;889:231;500:232;556:233;556:234;556:235;556:236;278:237;278:238;278:239;278:240;556:241;556:242;556:243;556:244;556:245;556:246;556:247;584:248;611:249;556:250;556:251;556:252;556:253;500:254;556:255;500:", Scope = Private
	#tag EndConstant

	#tag Constant, Name = kCT_symbol, Type = String, Dynamic = False, Default = \"0;250:1;250:2;250:3;250:4;250:5;250:6;250:7;250:8;250:9;250:10;250:11;250:12;250:13;250:14;250:15;250:16;250:17;250:18;250:19;250:20;250:21;250:22;250:23;250:24;250:25;250:26;250:27;250:28;250:29;250:30;250:31;250:32;250:33;333:34;713:35;500:36;549:37;833:38;778:39;439:40;333:41;333:42;500:43;549:44;250:45;549:46;250:47;278:48;500:49;500:50;500:51;500:52;500:53;500:54;500:55;500:56;500:57;500:58;278:59;278:60;549:61;549:62;549:63;444:64;549:65;722:66;667:67;722:68;612:69;611:70;763:71;603:72;722:73;333:74;631:75;722:76;686:77;889:78;722:79;722:80;768:81;741:82;556:83;592:84;611:85;690:86;439:87;768:88;645:89;795:90;611:91;333:92;863:93;333:94;658:95;500:96;500:97;631:98;549:99;549:100;494:101;439:102;521:103;411:104;603:105;329:106;603:107;549:108;549:109;576:110;521:111;549:112;549:113;521:114;549:115;603:116;439:117;576:118;713:119;686:120;493:121;686:122;494:123;480:124;200:125;480:126;549:127;0:128;0:129;0:130;0:131;0:132;0:133;0:134;0:135;0:136;0:137;0:138;0:139;0:140;0:141;0:142;0:143;0:144;0:145;0:146;0:147;0:148;0:149;0:150;0:151;0:152;0:153;0:154;0:155;0:156;0:157;0:158;0:159;0:160;750:161;620:162;247:163;549:164;167:165;713:166;500:167;753:168;753:169;753:170;753:171;1042:172;987:173;603:174;987:175;603:176;400:177;549:178;411:179;549:180;549:181;713:182;494:183;460:184;549:185;549:186;549:187;549:188;1000:189;603:190;1000:191;658:192;823:193;686:194;795:195;987:196;768:197;768:198;823:199;768:200;768:201;713:202;713:203;713:204;713:205;713:206;713:207;713:208;768:209;713:210;790:211;790:212;890:213;823:214;549:215;250:216;713:217;603:218;603:219;1042:220;987:221;603:222;987:223;603:224;494:225;329:226;790:227;790:228;786:229;713:230;384:231;384:232;384:233;384:234;384:235;384:236;494:237;494:238;494:239;494:240;0:241;329:242;274:243;686:244;686:245;686:246;384:247;384:248;384:249;384:250;384:251;384:252;494:253;494:254;494:255;0:", Scope = Private
	#tag EndConstant

	#tag Constant, Name = kCT_times, Type = String, Dynamic = False, Default = \"0;250:1;250:2;250:3;250:4;250:5;250:6;250:7;250:8;250:9;250:10;250:11;250:12;250:13;250:14;250:15;250:16;250:17;250:18;250:19;250:20;250:21;250:22;250:23;250:24;250:25;250:26;250:27;250:28;250:29;250:30;250:31;250:32;250:33;333:34;408:35;500:36;500:37;833:38;778:39;180:40;333:41;333:42;500:43;564:44;250:45;333:46;250:47;278:48;500:49;500:50;500:51;500:52;500:53;500:54;500:55;500:56;500:57;500:58;278:59;278:60;564:61;564:62;564:63;444:64;921:65;722:66;667:67;667:68;722:69;611:70;556:71;722:72;722:73;333:74;389:75;722:76;611:77;889:78;722:79;722:80;556:81;722:82;667:83;556:84;611:85;722:86;722:87;944:88;722:89;722:90;611:91;333:92;278:93;333:94;469:95;500:96;333:97;444:98;500:99;444:100;500:101;444:102;333:103;500:104;500:105;278:106;278:107;500:108;278:109;778:110;500:111;500:112;500:113;500:114;333:115;389:116;278:117;500:118;500:119;722:120;500:121;500:122;444:123;480:124;200:125;480:126;541:127;350:128;500:129;350:130;333:131;500:132;444:133;1000:134;500:135;500:136;333:137;1000:138;556:139;333:140;889:141;350:142;611:143;350:144;350:145;333:146;333:147;444:148;444:149;350:150;500:151;1000:152;333:153;980:154;389:155;333:156;722:157;350:158;444:159;722:160;250:161;333:162;500:163;500:164;500:165;500:166;200:167;500:168;333:169;760:170;276:171;500:172;564:173;333:174;760:175;333:176;400:177;564:178;300:179;300:180;333:181;500:182;453:183;250:184;333:185;300:186;310:187;500:188;750:189;750:190;750:191;444:192;722:193;722:194;722:195;722:196;722:197;722:198;889:199;667:200;611:201;611:202;611:203;611:204;333:205;333:206;333:207;333:208;722:209;722:210;722:211;722:212;722:213;722:214;722:215;564:216;722:217;722:218;722:219;722:220;722:221;722:222;556:223;500:224;444:225;444:226;444:227;444:228;444:229;444:230;667:231;444:232;444:233;444:234;444:235;444:236;278:237;278:238;278:239;278:240;500:241;500:242;500:243;500:244;500:245;500:246;500:247;564:248;500:249;500:250;500:251;500:252;500:253;500:254;500:255;500:", Scope = Private
	#tag EndConstant

	#tag Constant, Name = kCT_timesb, Type = String, Dynamic = False, Default = \"0;250:1;250:2;250:3;250:4;250:5;250:6;250:7;250:8;250:9;250:10;250:11;250:12;250:13;250:14;250:15;250:16;250:17;250:18;250:19;250:20;250:21;250:22;250:23;250:24;250:25;250:26;250:27;250:28;250:29;250:30;250:31;250:32;250:33;333:34;555:35;500:36;500:37;1000:38;833:39;278:40;333:41;333:42;500:43;570:44;250:45;333:46;250:47;278:48;500:49;500:50;500:51;500:52;500:53;500:54;500:55;500:56;500:57;500:58;333:59;333:60;570:61;570:62;570:63;500:64;930:65;722:66;667:67;722:68;722:69;667:70;611:71;778:72;778:73;389:74;500:75;778:76;667:77;944:78;722:79;778:80;611:81;778:82;722:83;556:84;667:85;722:86;722:87;1000:88;722:89;722:90;667:91;333:92;278:93;333:94;581:95;500:96;333:97;500:98;556:99;444:100;556:101;444:102;333:103;500:104;556:105;278:106;333:107;556:108;278:109;833:110;556:111;500:112;556:113;556:114;444:115;389:116;333:117;556:118;500:119;722:120;500:121;500:122;444:123;394:124;220:125;394:126;520:127;350:128;500:129;350:130;333:131;500:132;500:133;1000:134;500:135;500:136;333:137;1000:138;556:139;333:140;1000:141;350:142;667:143;350:144;350:145;333:146;333:147;500:148;500:149;350:150;500:151;1000:152;333:153;1000:154;389:155;333:156;722:157;350:158;444:159;722:160;250:161;333:162;500:163;500:164;500:165;500:166;220:167;500:168;333:169;747:170;300:171;500:172;570:173;333:174;747:175;333:176;400:177;570:178;300:179;300:180;333:181;556:182;540:183;250:184;333:185;300:186;330:187;500:188;750:189;750:190;750:191;500:192;722:193;722:194;722:195;722:196;722:197;722:198;1000:199;722:200;667:201;667:202;667:203;667:204;389:205;389:206;389:207;389:208;722:209;722:210;778:211;778:212;778:213;778:214;778:215;570:216;778:217;722:218;722:219;722:220;722:221;722:222;611:223;556:224;500:225;500:226;500:227;500:228;500:229;500:230;722:231;444:232;444:233;444:234;444:235;444:236;278:237;278:238;278:239;278:240;500:241;556:242;500:243;500:244;500:245;500:246;500:247;570:248;500:249;556:250;556:251;556:252;556:253;500:254;556:255;500:", Scope = Private
	#tag EndConstant

	#tag Constant, Name = kCT_timesbi, Type = String, Dynamic = False, Default = \"0;250:1;250:2;250:3;250:4;250:5;250:6;250:7;250:8;250:9;250:10;250:11;250:12;250:13;250:14;250:15;250:16;250:17;250:18;250:19;250:20;250:21;250:22;250:23;250:24;250:25;250:26;250:27;250:28;250:29;250:30;250:31;250:32;250:33;389:34;555:35;500:36;500:37;833:38;778:39;278:40;333:41;333:42;500:43;570:44;250:45;333:46;250:47;278:48;500:49;500:50;500:51;500:52;500:53;500:54;500:55;500:56;500:57;500:58;333:59;333:60;570:61;570:62;570:63;500:64;832:65;667:66;667:67;667:68;722:69;667:70;667:71;722:72;778:73;389:74;500:75;667:76;611:77;889:78;722:79;722:80;611:81;722:82;667:83;556:84;611:85;722:86;667:87;889:88;667:89;611:90;611:91;333:92;278:93;333:94;570:95;500:96;333:97;500:98;500:99;444:100;500:101;444:102;333:103;500:104;556:105;278:106;278:107;500:108;278:109;778:110;556:111;500:112;500:113;500:114;389:115;389:116;278:117;556:118;444:119;667:120;500:121;444:122;389:123;348:124;220:125;348:126;570:127;350:128;500:129;350:130;333:131;500:132;500:133;1000:134;500:135;500:136;333:137;1000:138;556:139;333:140;944:141;350:142;611:143;350:144;350:145;333:146;333:147;500:148;500:149;350:150;500:151;1000:152;333:153;1000:154;389:155;333:156;722:157;350:158;389:159;611:160;250:161;389:162;500:163;500:164;500:165;500:166;220:167;500:168;333:169;747:170;266:171;500:172;606:173;333:174;747:175;333:176;400:177;570:178;300:179;300:180;333:181;576:182;500:183;250:184;333:185;300:186;300:187;500:188;750:189;750:190;750:191;500:192;667:193;667:194;667:195;667:196;667:197;667:198;944:199;667:200;667:201;667:202;667:203;667:204;389:205;389:206;389:207;389:208;722:209;722:210;722:211;722:212;722:213;722:214;722:215;570:216;722:217;722:218;722:219;722:220;722:221;611:222;611:223;500:224;500:225;500:226;500:227;500:228;500:229;500:230;722:231;444:232;444:233;444:234;444:235;444:236;278:237;278:238;278:239;278:240;500:241;556:242;500:243;500:244;500:245;500:246;500:247;570:248;500:249;556:250;556:251;556:252;556:253;444:254;500:255;444:", Scope = Private
	#tag EndConstant

	#tag Constant, Name = kCT_timesi, Type = String, Dynamic = False, Default = \"0;250:1;250:2;250:3;250:4;250:5;250:6;250:7;250:8;250:9;250:10;250:11;250:12;250:13;250:14;250:15;250:16;250:17;250:18;250:19;250:20;250:21;250:22;250:23;250:24;250:25;250:26;250:27;250:28;250:29;250:30;250:31;250:32;250:33;333:34;420:35;500:36;500:37;833:38;778:39;214:40;333:41;333:42;500:43;675:44;250:45;333:46;250:47;278:48;500:49;500:50;500:51;500:52;500:53;500:54;500:55;500:56;500:57;500:58;333:59;333:60;675:61;675:62;675:63;500:64;920:65;611:66;611:67;667:68;722:69;611:70;611:71;722:72;722:73;333:74;444:75;667:76;556:77;833:78;667:79;722:80;611:81;722:82;611:83;500:84;556:85;722:86;611:87;833:88;611:89;556:90;556:91;389:92;278:93;389:94;422:95;500:96;333:97;500:98;500:99;444:100;500:101;444:102;278:103;500:104;500:105;278:106;278:107;444:108;278:109;722:110;500:111;500:112;500:113;500:114;389:115;389:116;278:117;500:118;444:119;667:120;444:121;444:122;389:123;400:124;275:125;400:126;541:127;350:128;500:129;350:130;333:131;500:132;556:133;889:134;500:135;500:136;333:137;1000:138;500:139;333:140;944:141;350:142;556:143;350:144;350:145;333:146;333:147;556:148;556:149;350:150;500:151;889:152;333:153;980:154;389:155;333:156;667:157;350:158;389:159;556:160;250:161;389:162;500:163;500:164;500:165;500:166;275:167;500:168;333:169;760:170;276:171;500:172;675:173;333:174;760:175;333:176;400:177;675:178;300:179;300:180;333:181;500:182;523:183;250:184;333:185;300:186;310:187;500:188;750:189;750:190;750:191;500:192;611:193;611:194;611:195;611:196;611:197;611:198;889:199;667:200;611:201;611:202;611:203;611:204;333:205;333:206;333:207;333:208;722:209;667:210;722:211;722:212;722:213;722:214;722:215;675:216;722:217;722:218;722:219;722:220;722:221;556:222;611:223;500:224;500:225;500:226;500:227;500:228;500:229;500:230;667:231;444:232;444:233;444:234;444:235;444:236;278:237;278:238;278:239;278:240;500:241;500:242;500:243;500:244;500:245;500:246;500:247;675:248;500:249;500:250;500:251;500:252;500:253;444:254;500:255;444:", Scope = Private
	#tag EndConstant

	#tag Constant, Name = kCT_zapfdingbats, Type = String, Dynamic = False, Default = \"0;0:1;0:2;0:3;0:4;0:5;0:6;0:7;0:8;0:9;0:10;0:11;0:12;0:13;0:14;0:15;0:16;0:17;0:18;0:19;0:20;0:21;0:22;0:23;0:24;0:25;0:26;0:27;0:28;0:29;0:30;0:31;0:32;278:33;974:34;961:35;974:36;980:37;719:38;789:39;790:40;791:41;690:42;960:43;939:44;549:45;855:46;911:47;933:48;911:49;945:50;974:51;755:52;846:53;762:54;761:55;571:56;677:57;763:58;760:59;759:60;754:61;494:62;552:63;537:64;577:65;692:66;786:67;788:68;788:69;790:70;793:71;794:72;816:73;823:74;789:75;841:76;823:77;833:78;816:79;831:80;923:81;744:82;723:83;749:84;790:85;792:86;695:87;776:88;768:89;792:90;759:91;707:92;708:93;682:94;701:95;826:96;815:97;789:98;789:99;707:100;687:101;696:102;689:103;786:104;787:105;713:106;791:107;785:108;791:109;873:110;761:111;762:112;762:113;759:114;759:115;892:116;892:117;788:118;784:119;438:120;138:121;277:122;415:123;392:124;392:125;668:126;668:127;0:128;390:129;390:130;317:131;317:132;276:133;276:134;509:135;509:136;410:137;410:138;234:139;234:140;334:141;334:142;0:143;0:144;0:145;0:146;0:147;0:148;0:149;0:150;0:151;0:152;0:153;0:154;0:155;0:156;0:157;0:158;0:159;0:160;0:161;732:162;544:163;544:164;910:165;667:166;760:167;760:168;776:169;595:170;694:171;626:172;788:173;788:174;788:175;788:176;788:177;788:178;788:179;788:180;788:181;788:182;788:183;788:184;788:185;788:186;788:187;788:188;788:189;788:190;788:191;788:192;788:193;788:194;788:195;788:196;788:197;788:198;788:199;788:200;788:201;788:202;788:203;788:204;788:205;788:206;788:207;788:208;788:209;788:210;788:211;788:212;894:213;838:214;1016:215;458:216;748:217;924:218;748:219;918:220;927:221;928:222;928:223;834:224;873:225;828:226;924:227;924:228;917:229;930:230;931:231;463:232;883:233;836:234;836:235;867:236;867:237;696:238;696:239;874:240;0:241;874:242;760:243;946:244;771:245;865:246;771:247;888:248;967:249;888:250;831:251;873:252;927:253;970:254;918:255;0:", Scope = Private
	#tag EndConstant

	#tag Constant, Name = kCT__fielddelim, Type = String, Dynamic = False, Default = \";", Scope = Private
	#tag EndConstant

	#tag Constant, Name = kCT__rowdelim, Type = String, Dynamic = False, Default = \":", Scope = Private
	#tag EndConstant


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
