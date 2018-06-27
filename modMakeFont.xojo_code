#tag Module
Protected Module modMakeFont
	#tag Method, Flags = &h0
		Function array_fillDouble(iStart as Integer, iNumberOfElements As Integer, d As Double) As Double()
		  Dim dReturn() As Double
		  Dim i As Integer
		  For i=0 to iNumberOfElements-1
		    dReturn.Append d
		  Next
		  Return dReturn 
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function array_fillVar(iStart as Integer, iNumberOfElements As Integer, d As Variant) As Variant()
		  Dim dReturn() As Variant
		  Dim i As Integer
		  For i=0 to iNumberOfElements-1
		    dReturn.Append d
		  Next
		  Return dReturn
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function ByteReplace(mbIn As MemoryBlock, iValToReplace As UInt8, strReplace As String) As MemoryBlock
		  Dim mbOut As New MemoryBlock(mbIn.Size)
		  Dim i As Integer
		  Dim iOutCount As Integer
		  For i=0 to mbIn.Size-1
		    if mbIn.Byte(i)<>iValToReplace Then
		      mbOut.Byte(iOutCount)=mbIn.Byte(i)
		      iOutCount=iOutCount+1
		    else
		      if strReplace<>"" then
		        mbOut.Byte(iOutCount)=Asc(strReplace)
		        iOutCount=iOutCount+1
		      end if
		    end if
		  Next
		  mbOut.Size=iOutCount
		  Return mbOut
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Checksum(strS As String, optional bSavestrS As Boolean=False) As String
		  Dim n As Integer
		  Dim iHigh As Integer
		  Dim iLow As Integer
		  Dim i As Integer
		  Dim iPart1 As Integer
		  Dim iPart2 As Integer
		  Dim iPart3 As Integer
		  Dim iPart4 As Integer
		  Dim iAsc1 As Integer
		  Dim iAsc2 As Integer
		  Dim mb As MemoryBlock
		  
		  mb=strS
		  
		  n = len(strS)
		  iHigh = 0
		  iLow = 0
		  for i=0 to n-1 step 4
		    iPart1 = bitwise.ShiftLeft( mb.Byte(i), 8 )
		    iPart2 = mb.Byte(i+1)
		    iHigh = iHigh + iPart1 + iPart2
		    iPart3=bitwise.ShiftLeft( mb.Byte(i+2) ,8 )
		    iPart4 =  mb.Byte(i+3)
		    iLow = iLow + iPart3 + iPart4
		  next
		  return pack("nn", iHigh+(Bitwise.ShiftRight(iLow,16)), iLow)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Error(strText As String)
		  'Dim iResult As Integer
		  'iResult=MsgBox (strText, 16, "Error")
		  System.DebugLog strText
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function file(f As FolderItem) As string()
		  Dim strReturn() As String
		  Dim strTemp As String
		  Dim t As TextInputStream
		  Dim strEOL As String
		  
		  if f=nil or f.Exists=False Then Return strReturn
		  
		  t = TextInputStream.Open(f)
		  if t<>nil then
		    strTemp=t.ReadAll
		    t.close
		  else
		    Warning("The selected file is not a text file.")
		    Return strReturn
		  end if
		  
		  strEOL=GetEOLFromString(strTemp)
		  
		  'strReturn=Split(strTemp,EndOfLine)
		  strReturn=Split(strTemp,strEOL)
		  Return strReturn
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function file_exists(strFilename As String) As Boolean
		  Dim f As FolderItem
		  f=GetFolderItem("")
		  f=f.Child(strFilename)
		  if f=nil or f.Exists=False Then
		    Return False
		  end if
		  Return True
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetEOLFromString(strSource As String) As String
		  Dim strEOL As String
		  Dim strTmp As String
		  strEOL=chr(13)+chr(10) //CR +LF
		  strTmp=ReplaceAll(strSource,strEOL,"")
		  if len(strTmp)<>len(strSource) then
		    Return strEOL
		  end if
		  strEOL=chr(13) //CR
		  strTmp=ReplaceAll(strSource,strEOL,"")
		  if len(strTmp)<>len(strSource) then
		    Return strEOL
		  end if
		  strEOL=chr(10) //CR +LF
		  strTmp=ReplaceAll(strSource,strEOL,"")
		  if len(strTmp)<>len(strSource) then
		    Return strEOL
		  end if
		  Return ""
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetFont(strFontfileName As String, bSaveOut As Boolean, FontFile As FolderItem, Optional strEnc As String="", Optional bEmbed As Boolean=True, Optional bSubset As Boolean=True) As Dictionary
		  If strFontfileName="" Then Return Nil
		  
		  myFontFileName=strFontfileName
		  
		  If strEnc<>"" Then
		    myEncoding=GetInternetTextEncoding(strEnc)
		  Else
		    myEncoding=GetInternetTextEncoding("WindowsLatin1") //cp1252
		    strEnc="cp1252"
		  End If
		  
		  myEmbed=bEmbed
		  
		  mySubset=bSubset
		  
		  'Dim bSuccess As Boolean
		  Dim dicInfo As Dictionary
		  DicInfo=GoMakeFont(myFontFileName,strEnc,myEmbed,mySubset,bSaveOut,FontFile)
		  
		  'if bSuccess=False Then
		  'MsgBox "oops"
		  'end if
		  
		  Return dicInfo
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function getFontFile(psFileName As String) As Folderitem
		  
		  if psFileName = "" then Return Nil
		  
		  dim fFolder As FolderItem
		  #if TargetWin32 then
		    fFolder = SpecialFolder.Fonts.Child(psFileName)
		    if fFolder <> Nil and fFolder.Exists then
		      Return fFolder
		    else
		      Return Nil
		    end if
		  #elseif TargetMacOS
		    
		    fFolder = SpecialFolder.Fonts.Child(psFileName)
		    if fFolder <> Nil and fFolder.Exists then
		      Return fFolder
		    else
		      fFolder = SpecialFolder.Library.Child("Fonts").Child(psFileName)
		      if fFolder <> Nil and fFolder.Exists then
		        Return fFolder
		      else
		        fFolder = SpecialFolder.UserLibrary.Child("Fonts").Child(psFileName)
		        if fFolder <> Nil and fFolder.Exists then
		          Return fFolder
		        else
		          Return Nil
		        end if
		      end if
		    end if
		    
		  #else
		    Return Nil
		  #endif
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetFontFileName(strFontName As String, bBold As Boolean, bItalic As Boolean) As String
		  Dim strReturn As String
		  
		  #if TargetWin32 then
		    
		    dim l as FileListMBS
		    Dim i, c As Integer
		    Dim dicWindowsFontFamily As Dictionary
		    Dim iWindowsFamilyFontCount As Integer
		    Dim dicFont As Dictionary
		    Dim dicTmp As Dictionary
		    Dim strFontFileName As String
		    Dim strFontNameFromFile As String
		    Dim myTTFInfo As ttfParser
		    Dim strFontFamily As String
		    Dim strFirstFileName As String
		    Dim strCurrentFileName As String
		    
		    if dicWindowsFonts<>nil then
		      if dicWindowsFonts.HasKey(strFontName) then
		        dicWindowsFontFamily=dicWindowsFonts.Value(strFontName)
		        iWindowsFamilyFontCount=dicWindowsFontFamily.Count
		        for i=0 to iWindowsFamilyFontCount-1
		          dicFont=Dictionary(dicWindowsFontFamily.Value(dicWindowsFontFamily.key(i)))
		          strCurrentFileName=dicWindowsFontFamily.key(i).StringValue
		          if strFirstFileName="" then
		            strFirstFileName=strCurrentFileName
		          end if
		          if dicFont.Value("bold").BooleanValue=bBold and dicFont.Value("italic").BooleanValue=bItalic then
		            strReturn=dicWindowsFontFamily.key(i).StringValue
		            exit For
		          end if
		        next
		        if strReturn="" then
		          strReturn=strFirstFileName
		        end if
		      end if
		    else
		      l=new FileListMBS(SpecialFolder.Fonts,"*.ttf")
		      c=l.Count-1
		      for i=0 to c
		        strFontFileName=l.Name(i)
		        if Right(strFontFileName,4)=".ttf" then //mbs also give us .ttc files on mac
		          myTTFInfo=GetFontInfo(strFontFileName)
		          if myTTFInfo<>nil then
		            if myTTFInfo.bParseError=False then
		              if myTTFInfo.strFamilyName<>strFontFamily then
		                if dicWindowsFontFamily<>nil then
		                  if dicWindowsFonts=Nil then
		                    dicWindowsFonts=new Dictionary
		                  end if
		                  dicWindowsFonts.Value(strFontFamily)=dicWindowsFontFamily
		                end if
		                strFontFamily=myTTFInfo.strFamilyName
		                if dicWindowsFonts<>nil and dicWindowsFonts.HasKey(strFontFamily) then
		                  dicWindowsFontFamily=dicWindowsFonts.Value(strFontFamily)
		                else
		                  dicWindowsFontFamily =  new Dictionary
		                end if
		                dicWindowsFontFamily.Value(strFontFileName) = new Dictionary("bold" : myTTFInfo.bBold, "italic" : myTTFInfo.bItalic)
		              else
		                dicWindowsFontFamily.Value(strFontFileName) = new Dictionary("bold" : myTTFInfo.bBold, "italic" : myTTFInfo.bItalic)
		              end if
		              if myTTFInfo.strFamilyName=strFontName then
		                strCurrentFileName=strFontFileName
		                if strFirstFileName="" then
		                  strFirstFileName=strCurrentFileName
		                end if
		              end if
		              if myTTFInfo.strFamilyName=strFontName and myTTFInfo.bBold=bBold and myTTFInfo.bItalic=bItalic then
		                strReturn=strFontFileName
		              end if
		            end if
		          end if
		        end if
		      next
		      if strReturn="" Then
		        strReturn=strFirstFileName
		      end if
		    end if
		  #elseif TargetMacOS
		    
		    dim f as NSFontMBS = NSFontMBS.fontWithName(strFontName, 12)
		    dim strFirstFileName As String
		    if f<>nil then
		      dim fontFamily as string = f.familyName
		      dim FontManager as new NSFontManagerMBS
		      dim fonts() as Variant = FontManager.availableMembersOfFontFamily(fontFamily)
		      
		      Dim strCurrentFileName As String
		      
		      for each item as Variant in fonts
		        dim a() as Variant = item
		        
		        dim PostScriptFontName as string = a(0)
		        dim NameExtension as string = a(1)
		        dim weight as Double = a(2)
		        dim traits as Double = a(3)
		        
		        dim font as NSFontMBS = NSFontMBS.fontWithName(PostScriptFontName, 12)
		        
		        dim isBold as Boolean = FontManager.fontHasTraits(font.fontName, FontManager.NSBoldFontMask)
		        dim isItalic as Boolean = FontManager.fontHasTraits(font.fontName, FontManager.NSItalicFontMask)
		        
		        dim file as FolderItem = font.file
		        strCurrentFileName=file.Name
		        
		        if strFirstFileName="" then
		          strFirstFileName=strCurrentFileName
		        end if
		        
		        if isBold=bBold and isItalic=bItalic then
		          strReturn=strCurrentFileName
		          exit for
		        end if
		        'listbox1.AddRow PostScriptFontName, NameExtension, str(weight), str(traits), file.name
		      next
		    end if
		    if strReturn="" then
		      strReturn=strFirstFileName
		    end if
		  #endif
		  Return strReturn
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetFontInfo(FontFileName As String) As TTFParser
		  'Dim strFontName As String
		  Dim myTTFInfo As ttfParser
		  try
		    myTTFInfo = new TTFParser(FontfileName)
		    if myTTFInfo<>nil then
		      myTTFInfo.ParseOffsetTable
		      if myTTFInfo.dicTables<>nil then
		        myTTFInfo.ParseName
		        myTTFInfo.ParseOS2
		        'strFontName=ttf.strPostScriptName
		        'strFamily=ttf.strFamilyName
		        'strSubFamily=ttf.strSubFamilyName
		        if myTTFInfo.bParseError=True then
		          myTTFInfo=nil
		        end if
		      else
		        myTTFInfo=nil
		      end if
		    end if
		  catch err As RuntimeException
		    myTTFInfo=nil
		    system.DebugLog "Er is een fout opgetreden bij het ophalen van de fontinfo voor font " + FontFileName
		  end try
		  Return myTTFInfo
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetInfoFromTrueType(strFontfileName As String, bEmbed As Boolean, bSubset As Boolean, map() As Variant, Optional fontfile As FolderItem) As Dictionary
		  'Dim keys() As Variant
		  'Dim k As Variant
		  Dim v As Variant
		  Dim dicInfo As New Dictionary
		  if map.Ubound=-1 Then
		    Return nil
		  end if
		  // Return information from a TrueType font
		  try
		    if fontfile<>nil then
		      ttf = new TTFParser(fontfile)
		    else
		      ttf = new TTFParser(strFontfileName)
		    end if
		    if ttf<>nil then
		      ttf.Parse
		    end if
		  catch err As RuntimeException
		    Error(err.Message)
		  end try
		  if bEmbed and ttf<>nil Then
		    if not ttf.bEmbeddable Then
		      Error("Font license does not allow embedding")
		      Return nil
		    end if
		    if bSubset Then
		      Dim iChars() As UInt16
		      'foreach($map as $v)
		      'keys=map.Keys
		      for each v in map
		        'if($v['name']!='.notdef')
		        if Dictionary(v).Value("name").StringValue<>".notdef" Then
		          '$chars[] = $v['uv'];
		          iChars.Append Dictionary(v).Value("uv").UInt32Value
		        end if
		      next
		      ttf.Subset(iChars)
		      dicInfo.Value("Data") = ttf.Build
		    else
		      'dicInfo.Value("Data") = file_get_contents(strFontfileName)
		    end if
		    dicInfo.Value("OriginalSize") = len(dicInfo.Value("Data").StringValue)
		  end if
		  
		  //http://libpdfxx.sourceforge.net/doc/classpdf_1_1font_1_1CDescriptor.html
		  //this function translate EM units (used in CFace) into PDF glyph spaces. glyph space is 1/1000 of text space, which is equal to user space (1/72 inchs) by default.
		  //
		  //00177 {
		  //00178     double temp = em_unit * 1000.0 / face.UnitsPerEM( ) ;
		  //00179     
		  //00180     // round off!
		  //00181     return static_cast<int>( em_unit > 0 ? temp + 0.5 : temp - 0.5 ) ;
		  //00182 }
		  
		  Dim k As Double
		  k = 1000/ttf.iUnitsPerEm
		  
		  Dim widths() As Double
		  
		  Dim id As UInt16
		  Dim w As UInt16
		  dicInfo.Value("FontName") = ttf.strPostScriptName
		  dicInfo.Value("Bold") = ttf.bBold
		  dicInfo.Value("ItalicAngle") = ttf.iItalicAngle
		  dicInfo.Value("IsFixedPitch") = ttf.bIsFixedPitch
		  dicInfo.Value("AvgCharWidth") =  ttf.iAvgCharWidth
		  dicInfo.Value("MaxWidth") = ttf.iAdvanceWidthMax
		  dicInfo.Value("FirstCharIndex") = ttf.iFirstCharIndex
		  dicInfo.Value("LastCharIndex") = ttf.iLastCharIndex
		  dicInfo.Value("Ascender") = round(k*ttf.iTypoAscender)
		  dicInfo.Value("Descender") = round(k*ttf.iTypoDescender)
		  dicInfo.Value("Leading") = round(k*ttf.iTypoLeading)
		  dicInfo.Value("UnderlineThickness") = round(k*ttf.iUnderlineThickness)
		  dicInfo.Value("UnderlinePosition") = round(k*ttf.iUnderlinePosition)
		  dicInfo.Value("FontBBox") = array(round(k*ttf.iXMin), round(k*ttf.iYMin), round(k*ttf.iXMax), round(k*ttf.iYMax))
		  dicInfo.Value("xHeight") = round(k*ttf.ixHeight) 
		  if ttf.iCapHeight<>0 Then
		    dicInfo.Value("CapHeight") = round(k*ttf.iCapHeight)
		  else
		    dicInfo.Value("CapHeight") = 718
		  end if
		  dicInfo.Value("MissingWidth") = round(k*Dictionary(ttf.dicglyphs.Value(0)).Value("w"))
		  dicInfo.Value("uv")=MakeUnicodeArray(map)
		  widths = array_fillDouble(0, 256, dicInfo.Value("MissingWidth").DoubleValue)
		  
		  Dim tmpDictionary As Dictionary
		  Dim i As Integer
		  'for each c As Variant in map
		  for i=0 to map.Ubound
		    v=Dictionary(map(i))
		    
		    if Dictionary(v).Value("name")<>".notdef" Then
		      if ttf.iCharsIsSet(Dictionary(v).Value("uv")) Then
		        
		        id = ttf.iChars(Dictionary(v).Value("uv"))
		        w = Dictionary(ttf.dicGlyphs.Value(id)).Value("w")
		        widths(i) = round(k*w)
		        
		      else
		        Warning("Character " + Dictionary(v).Value("name") + " is missing")
		      end if
		      
		    end if
		    
		  next
		  dicInfo.Value("Widths") = widths
		  return dicInfo
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetInfoFromType1(strFontfileName As String, bEmbed As Boolean, map As Dictionary) As String
		  // Return information from a Type1 font
		  Dim f As FolderItem
		  if bEmbed Then
		    f = GetFolderItem(strFontfileName) //, 'rb');
		    if f=nil or f.Exists=False Then
		      Error("Can't open font file")
		      Return ""
		    Else
		      '// Read first segment
		      '$a = unpack('Cmarker/Ctype/Vsize', fread($f,6));
		      'if($a['marker']!=128)
		      'Error('Font file is not a valid binary Type1');
		      '$size1 = $a['size'];
		      '$data = fread($f, $size1);
		      '// Read second segment
		      '$a = unpack('Cmarker/Ctype/Vsize', fread($f,6));
		      'if($a['marker']!=128)
		      'Error('Font file is not a valid binary Type1');
		      '$size2 = $a['size'];
		      '$data .= fread($f, $size2);
		      'fclose($f);
		      '$info['Data'] = $data;
		      '$info['Size1'] = $size1;
		      '$info['Size2'] = $size2;
		      'end if
		      'end if
		    end if
		  end if
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GoMakeFont(strFontfileName As String, strEnc As String, bEmbed As Boolean=True, bSubset As Boolean=True, bSaveOut As Boolean, FontFile As FolderItem) As Dictionary
		  if get_magic_quotes_runtime=True then
		    set_magic_quotes_runtime=True
		  end if
		  
		  auto_detect_line_endings=True
		  
		  If FontFile<>nil and FontFile.Exists=True Then
		    'ok
		  else
		    FontFile=getFontFile(strFontfileName)
		    If FontFile=nil or FontFile.Exists=false Then
		      Error("Font file not found: " + strFontfileName)
		      Return Nil
		    End If
		  End If
		  
		  Dim strExtension As String
		  Dim strType As String
		  strExtension=Lowercase(Right(strFontfileName,3))
		  
		  if strExtension="ttf" or strExtension="otf" Then
		    strType = "TrueType"
		  elseif strExtension="pfb" Then
		    strType = "Type1"
		  else
		    Error("Unrecognized font file extension: " + strExtension)
		    Return Nil
		  end if
		  
		  Dim map() As Variant
		  Dim dicInfo As Dictionary
		  map=LoadMap(strEnc)
		  if map.Ubound=-1 then
		    Return nil
		  end if
		  'if strType="TrueType" Then
		  dicInfo=GetInfoFromTrueType(strFontfileName, bEmbed, bSubset, map, fontfile)
		  'else
		  'strInfo=GetInfoFromType1(strFontfileName, bEmbed, map)
		  'end if
		  if bSaveOut Then
		    Dim strBasename As String
		    Dim strfile As String
		    //todo: check if the filename entering is just the name and not a full path
		    //$basename = substr(basename($fontfile), 0, -4);
		    strbasename = left(strFontfileName, len(strFontfileName) -4)
		    
		    if bEmbed Then
		      'if(function_exists("gzcompress"))
		      
		      strfile = strbasename + ".z"
		      'SaveToFile(strfile, gzcompress(dicInfo.Value("Data"), "b")
		      Dim iErr As Integer
		      Dim strCompressedData As String
		      strCompressedData=CompressZLibMBS(dicInfo.Value("Data"),6,iErr)
		      if iErr=0 Then
		        SaveToFile(strfile, strCompressedData, "b")
		        dicInfo.Value("File") = strfile
		        Message("Font file compressed: " + strfile)
		        
		      else
		        
		        dicInfo.Value("File") = strbasename
		        bsubset = False
		        Notice("Font file could not be compressed (zlib extension not available)")
		        
		      end if
		    end if
		    MakeDefinitionFile(strbasename+".php", strtype, strenc, bembed, bsubset, map, dicInfo)
		    Message("Font definition file generated: " + strbasename + ".php")
		  end if
		  Return dicInfo
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function LoadMap(strEnc As String) As Variant()
		  Dim map() As Variant
		  Dim fi As FolderItem
		  Dim stra() As String
		  Dim e() As String
		  Dim c As Integer
		  Dim uv As Integer
		  Dim name As String
		  Dim strFileName As String
		  Dim bGoOn As Boolean
		  Dim strName As String
		  Dim strWindowsFileName As String
		  
		  'fFile=GetFolderItem("").Child(Lowercase(strEnc) + ".map")
		  strFileName = Lowercase(strEnc) + ".map"
		  'fFile = Xojo.IO.SpecialFolder.GetResource(strFileName.ToText)
		  
		  bGoOn = True
		  #if DebugBuild
		    bGoOn = False
		    strWindowsFileName = ""
		    #if TargetWindows
		      fi = App.ExecutableFile
		      If Not IsNull(fi) And fi.Exists Then
		        strWindowsFileName = SeperateString(fi.Name, 1, ".")
		      End If
		    #endif
		    fi = App.ExecutableFile.Parent
		    If strWindowsFileName.Len > 0 Then
		      If Not IsNull(fi.Child(strWindowsFileName + " Resources")) And _
		        fi.Child(strWindowsFileName + " Resources").Exists And _
		        fi.Child(strWindowsFileName + " Resources").Directory Then
		        bGoOn = True
		        fi = fi.Child(strWindowsFileName + " Resources")
		      ElseIf Not IsNull(fi.Child("Resources")) And _
		        fi.Child("Resources").Exists And _
		        fi.Child("Resources").Directory Then
		        bGoOn = True
		        fi = fi.Child("Resources")
		      End If
		    End If
		    While Not bGoOn
		      If Not IsNull(fi) And fi.Exists And fi.Directory Then
		        If Not IsNull(fi.Child(strFileName)) And fi.Child(strFileName).Exists Then
		          bGoOn = True
		        Else
		          fi = fi.Parent
		        End If
		      Else
		        fi = Nil 
		        bGoOn = True
		      End If
		    Wend
		    
		    If IsNull(fi) Then
		      bGoOn = False
		    End If
		  #elseif TargetWindows
		    fi = App.ExecutableFile
		    If Not IsNull(fi) And fi.Exists Then
		      strName = SeperateString(fi.Name, 1, ".")
		      fi = fi.Parent
		    Else
		      bGoOn = False
		    End If
		    If bGoOn And Not IsNull(fi) And fi.Exists And fi.Directory Then
		      If Not IsNull(fi.Child(strName + " Resources")) And _
		        fi.Child(strName + " Resources").Exists And _
		        fi.Child(strName + " Resources").Directory Then
		        fi = fi.Child(strName + " Resources")
		      ElseIf Not IsNull(fi.Child("Resources")) And _
		        fi.Child("Resources").Exists And _
		        fi.Child("Resources").Directory Then
		        fi = fi.Child("Resources")
		      Else
		        bGoOn = False
		      End If
		    End If
		  #else
		    fi = App.ExecutableFile
		    If Not IsNull(fi) And fi.Exists Then
		      fi = fi.Parent
		    Else
		      bGoOn = False
		    End If
		    If Not IsNull(fi) And fi.Exists Then
		      fi = fi.Parent
		    Else
		      bGoOn = False
		    End If
		    If bGoOn And Not IsNull(fi) And fi.Exists And fi.Directory Then
		      If Not IsNull(fi.Child("Resources")) And _
		        fi.Child("Resources").Exists And _
		        fi.Child("Resources").Directory Then
		        fi = fi.Child("Resources")
		      Else
		        bGoOn = False
		      End If
		    End If
		  #endif
		  
		  If bGoOn And Not IsNull(fi.Child(strFileName)) And fi.Child(strFileName).Exists Then
		    fi = fi.Child(strFileName)
		  Else
		    bGoOn = False
		  End If
		  
		  If bGoOn Then
		    stra = file(fi)
		    
		    If UBound(stra)=-1 Then
		      Error("Encoding not found: " + strEnc)
		      Return map
		    End If
		    map=array_fillVar(0,256,New Dictionary("uv" : -1, "name" : ".notdef"))
		    For Each strTmp As String In stra
		      try
		        e=Split(RTrim(strTmp)," ")
		        if e.Ubound<>-1 then
		          c = Val("&h" + mid(e(0),2))
		          uv = Val("&h" + mid(e(1),3))
		          name = e(2)
		          map(c) = New Dictionary("uv" : uv, "name" : name)
		        end if
		      catch
		        'break
		      end try
		    Next
		  End If
		  
		  #if DebugBuild
		    If map.Ubound < 0 Then
		      Break
		      'Encoding not found
		    End If
		  #endif
		  
		  Return map
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub MakeDefinitionFile(strFileName as String, strtype As String, strenc as string, bEmbed As Boolean, bSubSet As Boolean, map() As variant, dicInfo As Dictionary)
		  Dim strS As String
		  Dim diff As String
		  strS = "<?php" + EndOfLine.UNIX
		  strS = strS + "$type = '" + strType + "';" + EndOfLine.UNIX
		  strS = strS + "$name = '" + VarToString(dicInfo.Value("FontName")) + "';" + EndOfLine.UNIX
		  strS = strS + "$desc = " + MakeFontDescriptor(dicInfo) + ";" + EndOfLine.UNIX
		  strS = strS + "$up = " + VarToString(dicInfo.Value("UnderlinePosition")) + ";" + EndOfLine.UNIX
		  strS = strS + "$ut = " + VarToString(dicInfo.Value("UnderlineThickness")) + ";" + EndOfLine.UNIX
		  strS = strS + "$cw = " + MakeWidthArray(VariantArrayToUInt16(dicInfo.Value("Widths"))) + ";" + EndOfLine.UNIX
		  strS = strS + "$enc = '" + strenc + "';" + EndOfLine.UNIX
		  diff = MakeFontEncoding(map)
		  if diff<>"" Then
		    strS = strS + "$diff = '" + diff + "';" + EndOfLine.UNIX
		  end if
		  'strS = strS + "$uv = " + MakeUnicodeArray(map) +  ";" + EndOfLine.UNIX
		  strS = strS + "$uv = " + VarToString(dicInfo.Value("uv")) +  ";" + EndOfLine.UNIX
		  if bEmbed Then
		    
		    strS = strS + "$file = '" + VarToString(dicInfo.Value("File")) + "';" + EndOfLine.UNIX
		    
		    if strType="Type1" Then
		      strS = strS + "$size1 = " + VarToString(dicInfo.Value("Size1")) + ";" + EndOfLine.UNIX
		      strS = strS + "$size2 = " + VarToString(dicInfo.Value("Size2")) + ";" + EndOfLine.UNIX
		    else
		      strS = strS + "$originalsize = " + VarToString(dicInfo.Value("OriginalSize")) + ";" + EndOfLine.UNIX
		      if bSubset Then
		        strS = strS + "$subsetted = true;" + EndOfLine.UNIX
		      end if
		      
		    end if
		  end if
		  strS = strS + "?>" + EndOfLine.UNIX
		  SaveToFile(strFileName, strS, "t")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function MakeFontDescriptor(dicinfo As Dictionary) As String
		  Dim fd As String
		  Dim flags As Integer
		  Dim fbb() As String
		  Dim stemv As String
		  // Ascent
		  fd = "array('Ascent'=>" + VarToString(dicInfo.Value("Ascender"))
		  // Descent
		  fd = fd + ",'Descent'=>" + VarToString(dicInfo.Value("Descender"))
		  // CapHeight
		  if dicInfo.Haskey("CapHeight") Then
		    fd = fd + ",'CapHeight'=>" + VarToString(dicInfo.Value("CapHeight"))
		  else
		    fd = fd + ",'CapHeight'=>" + VarToString(dicInfo.Value("Ascender"))
		  end if
		  // Flags
		  flags = 0
		  if dicInfo.Value("IsFixedPitch") Then
		    flags = flags + Bitwise.ShiftLeft(1,0)
		  end if
		  flags = flags + Bitwise.ShiftLeft(1,5)
		  if dicInfo.Value("ItalicAngle").IntegerValue<>0 Then
		    flags = flags + Bitwise.ShiftLeft(1,6)
		  end if
		  fd = fd + ",'Flags'=>" + VarToString(flags)
		  // FontBBox
		  fbb = VariantArrayToString(dicinfo.Value("FontBBox"))
		  fd = fd + ",'FontBBox'=>'[" + fbb(0) + " " + fbb(1) + " " + fbb(2) + " " + fbb(3) + "]'"
		  // ItalicAngle
		  fd = fd + ",'ItalicAngle'=>" + VarToString(dicInfo.Value("ItalicAngle"))
		  // StemV
		  if dicInfo.Haskey("StdVW") Then
		    stemv = dicInfo.Value("StdVW")
		  elseif dicInfo.Value("Bold") Then
		    stemv = Cstr(120)
		  else
		    stemv = Cstr(70)
		  end if
		  fd = fd + ",'StemV'=>" + stemv
		  // MissingWidth
		  fd = fd + ",'MissingWidth'=>" + VarToString(dicInfo.Value("MissingWidth")) + ")"
		  return fd
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function MakeFontEncoding(map() As Variant) As String
		  // Build differences from reference encoding
		  Dim ref() As Variant
		  Dim s As String
		  Dim last As Integer
		  Dim c As Integer
		  ref = LoadMap("cp1252")
		  s = ""
		  last = 0
		  for c=32 to 255
		    
		    if Dictionary(map(c)).Value("name")<>Dictionary(ref(c)).Value("name") Then
		      
		      if c<>last+1 Then
		        s = s + Cstr(c) + " "
		      end if
		      last = c
		      s = s + "/" + Dictionary(map(c)).Value("name") + " "
		      
		    end if
		  next
		  return rtrim(s)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function MakeUnicodeArray(map() As Variant) As String
		  // Build mapping to Unicode values
		  Dim ranges() As Variant 
		  Dim range() As Int16
		  Dim tmprange() As UInt16
		  Dim uv As Int16
		  Dim v As Dictionary
		  Dim bIsSetRange As Boolean
		  Dim bIsSetS As Boolean
		  Dim i As UInt16
		  Dim c As Int16
		  Dim s As String
		  Dim nb As UInt16
		  
		  for i=0 to map.Ubound
		    c=i
		    v=Dictionary(map(c))
		    uv = v.Value("uv")
		    if uv<>-1 Then
		      if bIsSetRange Then
		        if c=range(1)+1 and uv=range(3)+1 Then
		          range(1) = range(1) + 1
		          range(3) = range(3) + 1
		        else
		          ranges.Append range
		          range = array(c, c, uv, uv)
		        end if
		      else
		        range = array(c, c, uv, uv)
		        bIsSetRange=True
		      end if
		    end if
		  next
		  
		  ranges.Append range
		  
		  for each myrange As Variant In ranges
		    tmprange=VariantArrayToUInt16(myrange)
		    if tmprange.Ubound<>-1 then
		      if bIsSetS Then
		        s = s + ","
		      else
		        s = "array("
		        bIsSetS=True
		      end if
		      s = s + Cstr(tmprange(0)) + "=>"
		      nb = tmprange(1)-tmprange(0)+1
		      if nb>1 Then
		        s = s + "array(" + Cstr(tmprange(2)) + "," + Cstr(nb) + ")"
		      else
		        s = s + Cstr(tmprange(2))
		      end if
		    else
		      Break
		    end if
		  next
		  if s<>"" then
		    s = s + ")"
		  end if
		  return s
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function MakeWidthArray(widths() As Uint16) As String
		  Dim s As String
		  Dim c As Integer
		  s = "array(" + EndOfLine.UNIX + chr(9)
		  for c=0 to 255
		    if chr(c)="'" Then
		      s = s + "'\''"
		    elseif chr(c)="\" Then
		      s = s + "'\\'"
		    elseif c>=32 and c<=126 Then
		      s = s + "'" + chr(c) + "'"
		    else
		      s = s + "chr(" + Cstr(c) + ")"
		    end if
		    s = s + "=>" + Cstr(widths(c))
		    if c<255 Then
		      s =  s + ","
		    end if
		    if ((c+1) mod 22=0) Then
		      s = s + EndOfLine.UNIX + chr(9)
		    end if
		  next
		  s = s + ")"
		  return s
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Message(strText As String, Optional strSeverity As String)
		  Dim strEcho As String
		  if strSeverity<>"" then
		    strEcho=strSeverity + ": "
		  end if
		  strEcho=strEcho + strText
		  System.DebugLog strEcho
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Notice(strText As String)
		  If IsNull(App.CurrentThread) Then
		    Dim iResult As Integer
		    iResult = MsgBox (strText, 64, "Notice")
		  Else
		    System.DebugLog "Notice Make Font: " + strText
		  End If
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Pack(strFormat As String, ParamArray iVals As Integer) As String
		  Dim strFormats() As String
		  Dim i As Integer
		  Dim iMbLength As Integer
		  Dim uiTmp As UInt16
		  Dim iTmp As Integer
		  Dim iPos As Integer
		  
		  if strFormat="" or iVals.Ubound=-1 Then Return ""
		  For  i=1 to len(strFormat)
		    strFormats.Append mid(strFormat,i,1)
		    if StrComp(strFormats(i-1),"n",0)=0 Then
		      iMbLength=iMbLength + 2
		    ElseIf StrComp(strFormats(i-1),"N",0)=0 Then
		      iMbLength=iMbLength + 4
		    end if
		  Next
		  Dim packet As New MemoryBlock(iMbLength)
		  packet.LittleEndian=False
		  For i=0 to strFormats.Ubound
		    if StrComp(strFormats(i),"n",0)=0 Then
		      uiTmp=ivals(i)
		      packet.Int16Value(iPos)=uiTmp
		      iPos=iPos+2
		    ElseIf strComp(strFormats(i),"N",0)=0 Then
		      iTmp=iVals(i)
		      packet.Int32Value(iPos)=iTmp
		      iPos=iPos+4
		    end if
		  Next
		  Return packet
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SaveToFile(strFileName As String, strToSave As String, strType As String)
		  Dim f As FolderItem
		  Dim stream As BinaryStream
		  Dim t As TextOutputStream
		  f = GetFolderItem("")
		  f=f.Child(strFileName)
		  If f <> Nil Then
		    if strType="b" Then
		      stream = BinaryStream.Create(f, True) // Overwrite if exists
		      stream.Write(strToSave)
		      stream.Close
		    Else
		      t=TextOutputStream.Create(f)
		      t.WriteLine(strToSave)
		      t.Close
		    End If
		  End If
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub set_magic_quotes_runtime(Assigns bValue As Boolean)
		  magic_quotes_runtime=bValue
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function str_pad(strInput As String, iLength As Integer, strPadString As String) As String
		  Dim i As Integer
		  Dim iStart As Integer
		  iStart=len(strInput)
		  For i=iStart+1 to iLength
		    strInput=strInput + strPadString
		  Next
		  Return strInput
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function substr_replace(strSource As String, strReplacement As String, iStart As Integer, iLength As Integer) As String
		  Dim mbSource As MemoryBlock
		  Dim mbReplace As MemoryBlock
		  Dim i As Integer
		  mbSource=strSource
		  mbReplace=strReplacement
		  'strSource=Left(strSource,iStart-1) + strReplacement + right(strSource, len(strSource) - (istart+iLength))
		  for i=1 to iLength
		    mbSource.Byte(iStart+i-1)=mbReplace.Byte(i-1)
		  next
		  Return mbSource
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function UnPack(strFormat As String, strToUnpack As String) As UInt16()
		  Dim iReturn() As UInt16
		  
		  if strFormat<>"n2" or strToUnpack="" Then Return iReturn
		  
		  Dim packet As MemoryBlock
		  packet=strToUnpack
		  packet.LittleEndian=False
		  if strFormat="n2" Then
		    iReturn.Append packet.UInt16Value(0)
		    iReturn.Append packet.UInt16Value(2)
		  end if
		  
		  Return iReturn
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function VariantArrayToInt16(value As Variant) As Int16()
		  Dim dValues() As Double
		  Dim iValues() As Int16
		  Dim iEnd As Integer
		  Dim i As Integer
		  Dim iType As Integer
		  
		  If value.IsArray Then
		    iType = value.ArrayElementType
		    If iType = Variant.TypeDouble Then
		      dValues = value
		      iEnd = UBound(dValues)
		      For i = 0 To iEnd
		        iValues.Append dValues(i)
		      Next
		    ElseIf iType = Variant.TypeInt32 Or iType = Variant.TypeInt64 Then
		      iValues = value
		    End If
		  End If
		  
		  Return iValues
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function VariantArrayToString(value As Variant) As String()
		  Dim dDoubleValues() As Double
		  Dim varValues() As String
		  Dim iEnd As Integer
		  Dim i As Integer
		  
		  If value.IsArray Then
		    If value.ArrayElementType = Variant.TypeDouble Then
		      dDoubleValues = value
		      iEnd = uBound(dDoubleValues)
		      For i = 0 to iEnd
		        varValues.Append VarToString(dDoubleValues(i))
		      Next
		    End If
		  End If
		  
		  Return varValues
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function VariantArrayToUInt16(value As Variant) As UInt16()
		  Dim dValues() As Double
		  Dim iValues() As UInt16
		  Dim iEnd As Integer
		  Dim i As Integer
		  Dim iType As Integer
		  
		  If value.IsArray Then
		    iType = value.ArrayElementType
		    If iType = Variant.TypeDouble Then
		      dValues = value
		      iEnd = UBound(dValues)
		      For i = 0 To iEnd
		        iValues.Append dValues(i)
		      Next
		    ElseIf iType = Variant.TypeInt32 Or iType = Variant.TypeInt64 Then
		      iValues = value
		    End If
		  End If
		  
		  Return iValues
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function VarToString(VarValue As Variant) As String
		  Dim iVarType As Integer
		  
		  iVarType = VarValue.Type
		  
		  If iVarType = Variant.TypeNil Then
		    Return VarValue.StringValue
		  ElseIf iVarType = Variant.TypeString Then
		    Return VarValue
		  ElseIf iVarType = Variant.TypeDouble Or iVarType = Variant.TypeInt32 Or iVarType = Variant.TypeInt64 Then
		    Return Format(VarValue.DoubleValue,"-#####")
		  Else
		    Break
		  End If
		  
		  Return ""
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Warning(strText As String)
		  If IsNull(App.CurrentThread) Then
		    Dim iResult As Integer
		    iResult = MsgBox (strText, 48, "Warning")
		  Else
		    System.DebugLog "Warning Make Font: " + strText
		  End If
		  
		End Sub
	#tag EndMethod


	#tag Note, Name = makefont.php
		
		<?php
		/*******************************************************************************
		* Utility to generate font definition files                                    *
		*                                                                              *
		* Version: 1.3                                                                 *
		* Date:    2015-11-29                                                          *
		* Author:  Olivier PLATHEY                                                     *
		*******************************************************************************/
		
		require('ttfparser.php');
		
		function Message($txt, $severity='')
		{
		if(PHP_SAPI=='cli')
		{
		if($severity)
		echo "$severity: ";
		echo "$txt\n";
		}
		else
		{
		if($severity)
		echo "<b>$severity</b>: ";
		echo "$txt<br>";
		}
		}
		
		function Notice($txt)
		{
		Message($txt, 'Notice');
		}
		
		function Warning($txt)
		{
		Message($txt, 'Warning');
		}
		
		function Error($txt)
		{
		Message($txt, 'Error');
		exit;
		}
		
		function LoadMap($enc)
		{
		$file = dirname(__FILE__).'/'.strtolower($enc).'.map';
		$a = file($file);
		if(empty($a))
		Error('Encoding not found: '.$enc);
		$map = array_fill(0, 256, array('uv'=>-1, 'name'=>'.notdef'));
		foreach($a as $line)
		{
		$e = explode(' ', rtrim($line));
		$c = hexdec(substr($e[0],1));
		$uv = hexdec(substr($e[1],2));
		$name = $e[2];
		$map[$c] = array('uv'=>$uv, 'name'=>$name);
		}
		return $map;
		}
		
		function GetInfoFromTrueType($file, $embed, $subset, $map)
		{
		// Return information from a TrueType font
		try
		{
		$ttf = new TTFParser($file);
		$ttf->Parse();
		}
		catch(Exception $e)
		{
		Error($e->getMessage());
		}
		if($embed)
		{
		if(!$ttf->embeddable)
		Error('Font license does not allow embedding');
		if($subset)
		{
		$chars = array();
		foreach($map as $v)
		{
		if($v['name']!='.notdef')
		$chars[] = $v['uv'];
		}
		$ttf->Subset($chars);
		$info['Data'] = $ttf->Build();
		}
		else
		$info['Data'] = file_get_contents($file);
		$info['OriginalSize'] = strlen($info['Data']);
		}
		$k = 1000/$ttf->unitsPerEm;
		$info['FontName'] = $ttf->postScriptName;
		$info['Bold'] = $ttf->bold;
		$info['ItalicAngle'] = $ttf->italicAngle;
		$info['IsFixedPitch'] = $ttf->isFixedPitch;
		$info['Ascender'] = round($k*$ttf->typoAscender);
		$info['Descender'] = round($k*$ttf->typoDescender);
		$info['UnderlineThickness'] = round($k*$ttf->underlineThickness);
		$info['UnderlinePosition'] = round($k*$ttf->underlinePosition);
		$info['FontBBox'] = array(round($k*$ttf->xMin), round($k*$ttf->yMin), round($k*$ttf->xMax), round($k*$ttf->yMax));
		$info['CapHeight'] = round($k*$ttf->capHeight);
		$info['MissingWidth'] = round($k*$ttf->glyphs[0]['w']);
		$widths = array_fill(0, 256, $info['MissingWidth']);
		foreach($map as $c=>$v)
		{
		if($v['name']!='.notdef')
		{
		if(isset($ttf->chars[$v['uv']]))
		{
		$id = $ttf->chars[$v['uv']];
		$w = $ttf->glyphs[$id]['w'];
		$widths[$c] = round($k*$w);
		}
		else
		Warning('Character '.$v['name'].' is missing');
		}
		}
		$info['Widths'] = $widths;
		return $info;
		}
		
		function GetInfoFromType1($file, $embed, $map)
		{
		// Return information from a Type1 font
		if($embed)
		{
		$f = fopen($file, 'rb');
		if(!$f)
		Error('Can\'t open font file');
		// Read first segment
		$a = unpack('Cmarker/Ctype/Vsize', fread($f,6));
		if($a['marker']!=128)
		Error('Font file is not a valid binary Type1');
		$size1 = $a['size'];
		$data = fread($f, $size1);
		// Read second segment
		$a = unpack('Cmarker/Ctype/Vsize', fread($f,6));
		if($a['marker']!=128)
		Error('Font file is not a valid binary Type1');
		$size2 = $a['size'];
		$data .= fread($f, $size2);
		fclose($f);
		$info['Data'] = $data;
		$info['Size1'] = $size1;
		$info['Size2'] = $size2;
		}
		
		$afm = substr($file, 0, -3).'afm';
		if(!file_exists($afm))
		Error('AFM font file not found: '.$afm);
		$a = file($afm);
		if(empty($a))
		Error('AFM file empty or not readable');
		foreach($a as $line)
		{
		$e = explode(' ', rtrim($line));
		if(count($e)<2)
		continue;
		$entry = $e[0];
		if($entry=='C')
		{
		$w = $e[4];
		$name = $e[7];
		$cw[$name] = $w;
		}
		elseif($entry=='FontName')
		$info['FontName'] = $e[1];
		elseif($entry=='Weight')
		$info['Weight'] = $e[1];
		elseif($entry=='ItalicAngle')
		$info['ItalicAngle'] = (int)$e[1];
		elseif($entry=='Ascender')
		$info['Ascender'] = (int)$e[1];
		elseif($entry=='Descender')
		$info['Descender'] = (int)$e[1];
		elseif($entry=='UnderlineThickness')
		$info['UnderlineThickness'] = (int)$e[1];
		elseif($entry=='UnderlinePosition')
		$info['UnderlinePosition'] = (int)$e[1];
		elseif($entry=='IsFixedPitch')
		$info['IsFixedPitch'] = ($e[1]=='true');
		elseif($entry=='FontBBox')
		$info['FontBBox'] = array((int)$e[1], (int)$e[2], (int)$e[3], (int)$e[4]);
		elseif($entry=='CapHeight')
		$info['CapHeight'] = (int)$e[1];
		elseif($entry=='StdVW')
		$info['StdVW'] = (int)$e[1];
		}
		
		if(!isset($info['FontName']))
		Error('FontName missing in AFM file');
		if(!isset($info['Ascender']))
		$info['Ascender'] = $info['FontBBox'][3];
		if(!isset($info['Descender']))
		$info['Descender'] = $info['FontBBox'][1];
		$info['Bold'] = isset($info['Weight']) && preg_match('/bold|black/i', $info['Weight']);
		if(isset($cw['.notdef']))
		$info['MissingWidth'] = $cw['.notdef'];
		else
		$info['MissingWidth'] = 0;
		$widths = array_fill(0, 256, $info['MissingWidth']);
		foreach($map as $c=>$v)
		{
		if($v['name']!='.notdef')
		{
		if(isset($cw[$v['name']]))
		$widths[$c] = $cw[$v['name']];
		else
		Warning('Character '.$v['name'].' is missing');
		}
		}
		$info['Widths'] = $widths;
		return $info;
		}
		
		function MakeFontDescriptor($info)
		{
		// Ascent
		$fd = "array('Ascent'=>".$info['Ascender'];
		// Descent
		$fd .= ",'Descent'=>".$info['Descender'];
		// CapHeight
		if(!empty($info['CapHeight']))
		$fd .= ",'CapHeight'=>".$info['CapHeight'];
		else
		$fd .= ",'CapHeight'=>".$info['Ascender'];
		// Flags
		$flags = 0;
		if($info['IsFixedPitch'])
		$flags += 1<<0;
		$flags += 1<<5;
		if($info['ItalicAngle']!=0)
		$flags += 1<<6;
		$fd .= ",'Flags'=>".$flags;
		// FontBBox
		$fbb = $info['FontBBox'];
		$fd .= ",'FontBBox'=>'[".$fbb[0].' '.$fbb[1].' '.$fbb[2].' '.$fbb[3]."]'";
		// ItalicAngle
		$fd .= ",'ItalicAngle'=>".$info['ItalicAngle'];
		// StemV
		if(isset($info['StdVW']))
		$stemv = $info['StdVW'];
		elseif($info['Bold'])
		$stemv = 120;
		else
		$stemv = 70;
		$fd .= ",'StemV'=>".$stemv;
		// MissingWidth
		$fd .= ",'MissingWidth'=>".$info['MissingWidth'].')';
		return $fd;
		}
		
		function MakeWidthArray($widths)
		{
		$s = "array(\n\t";
		for($c=0;$c<=255;$c++)
		{
		if(chr($c)=="'")
		$s .= "'\\''";
		elseif(chr($c)=="\\")
		$s .= "'\\\\'";
		elseif($c>=32 && $c<=126)
		$s .= "'".chr($c)."'";
		else
		$s .= "chr($c)";
		$s .= '=>'.$widths[$c];
		if($c<255)
		$s .= ',';
		if(($c+1)%22==0)
		$s .= "\n\t";
		}
		$s .= ')';
		return $s;
		}
		
		function MakeFontEncoding($map)
		{
		// Build differences from reference encoding
		$ref = LoadMap('cp1252');
		$s = '';
		$last = 0;
		for($c=32;$c<=255;$c++)
		{
		if($map[$c]['name']!=$ref[$c]['name'])
		{
		if($c!=$last+1)
		$s .= $c.' ';
		$last = $c;
		$s .= '/'.$map[$c]['name'].' ';
		}
		}
		return rtrim($s);
		}
		
		function MakeUnicodeArray($map)
		{
		// Build mapping to Unicode values
		$ranges = array();
		foreach($map as $c=>$v)
		{
		$uv = $v['uv'];
		if($uv!=-1)
		{
		if(isset($range))
		{
		if($c==$range[1]+1 && $uv==$range[3]+1)
		{
		$range[1]++;
		$range[3]++;
		}
		else
		{
		$ranges[] = $range;
		$range = array($c, $c, $uv, $uv);
		}
		}
		else
		$range = array($c, $c, $uv, $uv);
		}
		}
		$ranges[] = $range;
		
		foreach($ranges as $range)
		{
		if(isset($s))
		$s .= ',';
		else
		$s = 'array(';
		$s .= $range[0].'=>';
		$nb = $range[1]-$range[0]+1;
		if($nb>1)
		$s .= 'array('.$range[2].','.$nb.')';
		else
		$s .= $range[2];
		}
		$s .= ')';
		return $s;
		}
		
		function SaveToFile($file, $s, $mode)
		{
		$f = fopen($file, 'w'.$mode);
		if(!$f)
		Error('Can\'t write to file '.$file);
		fwrite($f, $s);
		fclose($f);
		}
		
		function MakeDefinitionFile($file, $type, $enc, $embed, $subset, $map, $info)
		{
		$s = "<?php\n";
		$s .= '$type = \''.$type."';\n";
		$s .= '$name = \''.$info['FontName']."';\n";
		$s .= '$desc = '.MakeFontDescriptor($info).";\n";
		$s .= '$up = '.$info['UnderlinePosition'].";\n";
		$s .= '$ut = '.$info['UnderlineThickness'].";\n";
		$s .= '$cw = '.MakeWidthArray($info['Widths']).";\n";
		$s .= '$enc = \''.$enc."';\n";
		$diff = MakeFontEncoding($map);
		if($diff)
		$s .= '$diff = \''.$diff."';\n";
		$s .= '$uv = '.MakeUnicodeArray($map).";\n";
		if($embed)
		{
		$s .= '$file = \''.$info['File']."';\n";
		if($type=='Type1')
		{
		$s .= '$size1 = '.$info['Size1'].";\n";
		$s .= '$size2 = '.$info['Size2'].";\n";
		}
		else
		{
		$s .= '$originalsize = '.$info['OriginalSize'].";\n";
		if($subset)
		$s .= "\$subsetted = true;\n";
		}
		}
		$s .= "?>\n";
		SaveToFile($file, $s, 't');
		}
		
		function MakeFont($fontfile, $enc='cp1252', $embed=true, $subset=true)
		{
		// Generate a font definition file
		if(get_magic_quotes_runtime())
		@set_magic_quotes_runtime(false);
		ini_set('auto_detect_line_endings', '1');
		
		if(!file_exists($fontfile))
		Error('Font file not found: '.$fontfile);
		$ext = strtolower(substr($fontfile,-3));
		if($ext=='ttf' || $ext=='otf')
		$type = 'TrueType';
		elseif($ext=='pfb')
		$type = 'Type1';
		else
		Error('Unrecognized font file extension: '.$ext);
		
		$map = LoadMap($enc);
		
		if($type=='TrueType')
		$info = GetInfoFromTrueType($fontfile, $embed, $subset, $map);
		else
		$info = GetInfoFromType1($fontfile, $embed, $map);
		
		$basename = substr(basename($fontfile), 0, -4);
		if($embed)
		{
		if(function_exists('gzcompress'))
		{
		$file = $basename.'.z';
		SaveToFile($file, gzcompress($info['Data']), 'b');
		$info['File'] = $file;
		Message('Font file compressed: '.$file);
		}
		else
		{
		$info['File'] = basename($fontfile);
		$subset = false;
		Notice('Font file could not be compressed (zlib extension not available)');
		}
		}
		
		MakeDefinitionFile($basename.'.php', $type, $enc, $embed, $subset, $map, $info);
		Message('Font definition file generated: '.$basename.'.php');
		}
		
		if(PHP_SAPI=='cli')
		{
		// Command-line interface
		ini_set('log_errors', '0');
		if($argc==1)
		die("Usage: php makefont.php fontfile [encoding] [embed] [subset]\n");
		$fontfile = $argv[1];
		if($argc>=3)
		$enc = $argv[2];
		else
		$enc = 'cp1252';
		if($argc>=4)
		$embed = ($argv[3]=='true' || $argv[3]=='1');
		else
		$embed = true;
		if($argc>=5)
		$subset = ($argv[4]=='true' || $argv[4]=='1');
		else
		$subset = true;
		MakeFont($fontfile, $enc, $embed, $subset);
		}
		?>
		
	#tag EndNote


	#tag Property, Flags = &h0
		auto_detect_line_endings As Boolean = False
	#tag EndProperty

	#tag Property, Flags = &h0
		dicWindowsFonts As Dictionary
	#tag EndProperty

	#tag Property, Flags = &h0
		magic_quotes_runtime As Boolean = False
	#tag EndProperty

	#tag Property, Flags = &h0
		myEmbed As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		myEncoding As TextEncoding
	#tag EndProperty

	#tag Property, Flags = &h0
		myFontFileName As String
	#tag EndProperty

	#tag Property, Flags = &h0
		mySubset As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		ttf As ttfParser
	#tag EndProperty


	#tag Constant, Name = get_magic_quotes_runtime, Type = Boolean, Dynamic = False, Default = \"True", Scope = Public
	#tag EndConstant


	#tag ViewBehavior
		#tag ViewProperty
			Name="Name"
			Visible=true
			Group="ID"
			Type="String"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Index"
			Visible=true
			Group="ID"
			InitialValue="-2147483648"
			Type="Integer"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Super"
			Visible=true
			Group="ID"
			Type="String"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Left"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Top"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
		#tag EndViewProperty
		#tag ViewProperty
			Name="mySubset"
			Group="Behavior"
			Type="Boolean"
		#tag EndViewProperty
		#tag ViewProperty
			Name="auto_detect_line_endings"
			Group="Behavior"
			InitialValue="False"
			Type="Boolean"
		#tag EndViewProperty
		#tag ViewProperty
			Name="magic_quotes_runtime"
			Group="Behavior"
			InitialValue="False"
			Type="Boolean"
		#tag EndViewProperty
		#tag ViewProperty
			Name="myEmbed"
			Group="Behavior"
			Type="Boolean"
		#tag EndViewProperty
		#tag ViewProperty
			Name="myFontFileName"
			Group="Behavior"
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
	#tag EndViewBehavior
End Module
#tag EndModule
