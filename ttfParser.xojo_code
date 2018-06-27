#tag Class
Protected Class ttfParser
	#tag Method, Flags = &h0
		Sub AddGlyph(id As Integer)
		  Dim keys() As Variant
		  Dim k As Variant
		  Dim myDictionary As Dictionary
		  Dim tmpDictionary As Dictionary
		  if dicGlyphs.HasKey(id) Then
		    tmpDictionary=Dictionary(dicGlyphs.Value(id))
		    if Dictionary(dicGlyphs.Value(id)).HasKey("ssid")=False Then
		      Dictionary(dicGlyphs.Value(id)).Value("ssid")=iSubsettedGlyphs.Ubound+1
		      iSubsettedGlyphs.Append id
		      If Dictionary(dicGlyphs.Value(id)).HasKey("components")=True Then
		        myDictionary=Dictionary(Dictionary(dicGlyphs.Value(id)).Value("components"))
		        keys=myDictionary.Keys()
		        for each k in keys
		          AddGlyph(myDictionary.Value(k))
		        next
		      end if
		    end if
		  end if
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Build() As String
		  BuildCmap
		  BuildHhea
		  BuildHmtx
		  BuildLoca
		  BuildGlyf
		  BuildMaxp
		  BuildPost
		  return BuildFont
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub BuildCmap()
		  if iSubsettedChars.Ubound=-1 Then
		    return
		  end if
		  
		  Dim iCharsTmp() As UInt16
		  Dim iSegments() As Variant
		  Dim iSegment() As UInt16
		  Dim i As Integer
		  Dim iSegCount As Integer
		  Dim n As Integer
		  Dim iStartCount() As Integer
		  Dim iEndCount() As Integer
		  Dim iIdDelta() As Integer
		  Dim iIdRangeOffset() As Integer
		  Dim strGlyphIdArray As String
		  Dim iStart As Integer
		  Dim iEnd As Integer
		  Dim iSegmentTmp() As UInt16
		  Dim c As Integer
		  Dim iSsid As UInt16
		  Dim iEntrySelector As Integer
		  Dim iSearchRange As Integer
		  Dim iRangeShift As Integer
		  Dim strCmap As String
		  Dim strData As String
		  Dim glyphTmp As Dictionary
		  Dim strTmp As String
		  Dim iCharNr As Integer
		  Dim iUInt16Max As UInt16
		  
		  // Divide charset in contiguous segments
		  iCharsTmp = iSubsettedChars
		  iCharsTmp.Sort
		  iSegment = Array(iCharsTmp(0), iCharsTmp(0))
		  for i=1 to iCharsTmp.Ubound
		    if iCharsTmp(i) > iSegment(1) + 1 Then
		      iSegments.Append iSegment
		      iSegment = Array(iCharsTmp(i), iCharsTmp(i))
		    else
		      iSegment(1)=iSegment(1)+1
		    end if
		  next
		  iSegments.Append iSegment
		  'iSegments.Append Array(&hFFFF, &hFFFF)
		  iUInt16Max = &hFFFF
		  iSegment = Array(iUInt16Max, iUInt16Max)
		  iSegments.Append iSegment
		  iSegCount = iSegments.Ubound+1
		  
		  // Build a Format 4 subtable
		  strGlyphIdArray=""
		  for i=0 to iSegCount-1
		    iSegmentTmp=iSegments(i)
		    iStart=iSegmentTmp(0)
		    iEnd=iSegmentTmp(1)
		    'list($start, $end) = iSegments(i)
		    iStartCount.Append iStart
		    iEndCount.Append iEnd
		    if iStart<>iEnd Then
		      // Segment with multiple chars
		      iIdDelta.Append 0
		      iIdRangeOffset.Append len(strGlyphIdArray) + (iSegCount-i)*2
		      for c=iStart to iEnd
		        iCharNr = iChars(c)
		        if dicGlyphs.HasKey(iCharNr)=False then
		          Break
		        end if
		        glyphTmp=Dictionary(dicGlyphs.Value(iCharNr))
		        if glyphTmp.HasKey("ssid")=False Then
		          Break
		        end if
		        iSsid = glyphTmp.Value("ssid")
		        strTmp = pack("n", iSsid)
		        strGlyphIdArray = strGlyphIdArray + strTmp
		      next
		    else
		      // Segment with a single char
		      if iStart<&hFFFF Then
		        iCharNr = iChars(iStart)
		        if dicGlyphs.HasKey(iCharNr)=False then
		          Break
		        end if
		        glyphTmp=Dictionary(dicGlyphs.Value(iCharNr))
		        if glyphTmp.HasKey("ssid")=False Then
		          Break
		        end if
		        iSsid = glyphTmp.Value("ssid")
		      else
		        iSsid = 0
		      end if
		      iIdDelta.Append iSsid - iStart
		      iIdRangeOffset.Append 0
		    end if
		  next
		  
		  iEntrySelector = 0
		  n = iSegCount
		  while n<>1
		    n = Bitwise.ShiftRight(n,1)
		    iEntrySelector=iEntrySelector + 1
		  wend
		  iSearchRange = Bitwise.ShiftLeft(1,iEntrySelector)*2
		  iRangeShift = 2* iSegCount - iSearchRange
		  strCmap = pack("nnnn", 2*iSegCount, iSearchRange, iEntrySelector, iRangeShift)
		  for each ival As Integer In iEndCount
		    strCmap = strCmap + pack("n", ival)
		  next
		  strCmap = strCmap + pack("n", 0) // reservedPad
		  for each ival As Integer In iStartCount
		    strCmap = strCmap + pack("n", ival)
		  next
		  for each ival As Integer In iIdDelta
		    strCmap = strCmap + pack("n", ival)
		  next
		  for each ival As Integer In iIdRangeOffset
		    strCmap = strCmap + pack("n", ival)
		  next
		  strCmap = strCmap + strGlyphIdArray
		  
		  strData = pack("nn", 0, 1) // version, numTables
		  strData = strData + pack("nnN", 3, 1, 12) // platformID, encodingID, offset
		  strData = strData + pack("nnn", 4, 6 + len(strCmap), 0) // format, length, language
		  strData = strData + strCmap
		  SetTable("cmap", strData)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function BuildFont() As String
		  Dim strTags() As String
		  Dim iNumTables As Integer
		  Dim iOffset As Integer
		  Dim iEntrySelector As Integer
		  Dim iSearchRange As Integer
		  Dim iRangeShift As Integer
		  Dim strOffsetTable As String
		  Dim table As Dictionary
		  Dim n As Integer
		  Dim arValues() As String
		  Dim strS As String
		  Dim a() As UInt16
		  Dim iHigh As Integer
		  Dim iLow As Integer
		  Dim strCheckSumAdjustment As String
		  Dim strFont As String
		  Dim keys() As Variant
		  Dim a1 As Integer
		  Dim a2 As Integer 
		  
		  arValues=array("cmap", "cvt", "fpgm", "glyf", "head", "hhea", "hmtx", "loca", "maxp", "name", "post", "prep")
		  for each strTag As String In arValues
		    if len(strTag)<4 then
		      strTag=str_pad(strTag,4," ")
		    end if
		    if dicTables.HasKey(strTag) Then
		      strTags.Append strTag
		    end if
		  next
		  iNumTables = strTags.Ubound+1
		  iOffset = 12 + 16*iNumTables
		  for each strTag As String in strTags
		    if Dictionary(dicTables.Value(strTag)).HasKey("data")=False Then
		      LoadTable(strTag)
		    end if
		    Dictionary(dicTables.Value(strTag)).Value("offset")= iOffset
		    iOffset = iOffset + len(Dictionary(dicTables.Value(strTag)).Value("data"))
		  next
		  
		  // Build offset table
		  iEntrySelector = 0
		  n = iNumTables
		  while n<>1
		    n=Bitwise.ShiftRight(n,1)
		    iEntrySelector= iEntrySelector + 1
		  wend
		  iSearchRange = 16*Bitwise.ShiftLeft(1,iEntrySelector)
		  iRangeShift = 16*iNumTables - iSearchRange
		  strOffsetTable = pack("nnnnnn", 1, 0, iNumTables, iSearchRange, iEntrySelector, iRangeShift)
		  for each strTag As string In strTags
		    table = dicTables.Value(strTag)
		    strOffsetTable = strOffsetTable + strTag + table.Value("checkSum") + pack("NN", table.Value("offset"), table.Value("length"))
		  next
		  
		  // Compute checkSumAdjustment (0xB1B0AFBA - font checkSum)
		  strS = CheckSum(strOffsetTable)
		  for each strTag As String In strTags
		    strS = strS + Dictionary(dicTables.Value(strTag)).Value("checkSum")
		  next
		  a = unpack("n2", CheckSum(strS))
		  iHigh = &hB1B0 + (a(0) xor &hFFFF)
		  iLow = &hAFBA + (a(1) xor &hFFFF) + 1
		  strCheckSumAdjustment = pack("nn", iHigh+ Bitwise.ShiftRight(iLow,16), iLow)
		  Dictionary(dicTables.Value("head")).Value("data") = substr_replace(Dictionary(dicTables.Value("head")).Value("data"), strCheckSumAdjustment, 8, 4)
		  
		  strFont = strOffsetTable
		  for each strTag As String In strTags
		    strFont = strFont + Dictionary(dicTables.Value(strTag)).Value("data")
		  next
		  
		  return strFont
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub BuildGlyf()
		  Dim strData As String
		  Dim iTableOffset As Integer
		  Dim dicGlyph As Dictionary
		  Dim myDictionary As Dictionary
		  Dim strGlyph_Data As String
		  Dim iSsid As Integer
		  Dim keys() As Variant
		  Dim k As Variant
		  Dim iCid As Integer
		  Dim iOffset As Integer
		  Dim ilength As Integer
		  Dim iStrLength As Integer
		  
		  iTableOffset = Dictionary(dicTables.Value("glyf")).Value("offset").IntegerValue
		  strData = ""
		  for each id As Integer in iSubsettedGlyphs
		    dicGlyph = dicGlyphs.Value(id)
		    'fseek($this->f, $tableOffset+$glyph['offset'], SEEK_SET);
		    ReadStream.Position= iTableOffset + dicGlyph.Value("offset").IntegerValue
		    ilength=dicGlyph.Value("length")
		    strGlyph_data = Read(ilength)
		    if dicGlyph.Haskey("components") Then
		      // Composite glyph
		      myDictionary=dicGlyph.Value("components")
		      keys=myDictionary.Keys()
		      for each k in keys
		        iSsid = Dictionary(dicGlyphs.Value(myDictionary.Value(k))).Value("ssid").IntegerValue
		        strGlyph_data = substr_replace(strGlyph_data, pack("n",iSsid), k, 2)
		      next
		    end if
		    strData = strData + strGlyph_data
		    iStrLength=len(strData)
		  next
		  SetTable("glyf", strData)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub BuildHhea()
		  Dim strData As String
		  LoadTable("hhea")
		  iNumberOfHMetrics =iSubsettedGlyphs.Ubound+1
		  strData = substr_replace(Dictionary(dicTables.Value("hhea")).Value("data"), pack("n",iNumberOfHMetrics), 4+15*2, 2)
		  SetTable("hhea", strData)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub BuildHmtx()
		  Dim strData As String
		  Dim dicGlyph As Dictionary
		  strData = ""
		  for each id As Integer In iSubsettedGlyphs
		    if dicGlyphs.HasKey(id)=False then
		      Break
		    end if
		    dicGlyph = dicGlyphs.Value(id)
		    if dicGlyph.HasKey("w")=False or dicGlyph.HasKey("lsb")=False then
		      Break
		    end if
		    strData = strData  + pack("nn", dicGlyph.Value("w"), dicGlyph.Value("lsb"))
		  next
		  SetTable("hmtx", strData)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub BuildLoca()
		  Dim strData As String
		  Dim iOffset As Integer
		  strData = ""
		  iOffset = 0
		  for each id As integer in iSubsettedGlyphs
		    if iIndexToLocFormat=0 Then
		      strData = strData + pack("n", iOffset/2)
		    else
		      strData = strData + pack("N", iOffset)
		    end if
		    if dicGlyphs.HasKey(id)=False Then
		      Break
		    end if
		    iOffset = iOffset + Dictionary(dicGlyphs.Value(id)).Value("length").IntegerValue
		  next
		  if iIndexToLocFormat=0 Then
		    strData = strData + pack("n", iOffset/2)
		  else
		    strData = strData + pack("N", iOffset)
		  end if
		  SetTable("loca", strData)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub BuildMaxp()
		  Dim iNumGlyphs As Integer
		  Dim strData As String
		  LoadTable("maxp")
		  iNumGlyphs = iSubsettedGlyphs.Ubound+1
		  strData = substr_replace(Dictionary(dicTables.Value("maxp")).Value("data"), pack("n",iNumGlyphs), 4, 2)
		  SetTable("maxp", strData)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub BuildPost()
		  Dim iNumberOfGlyphs As Integer
		  Dim iNumNames As Integer
		  Dim strNames As String
		  Dim strName As Variant
		  Dim strData As String
		  Seek("post")
		  if bGlyphNames Then
		    
		    // Version 2.0
		    iNumberOfGlyphs = iSubsettedGlyphs.Ubound + 1
		    iNumNames = 0
		    strNames = ""
		    strData = Read(2*4+2*2+5*4)
		    strData = strData + pack("n", iNumberOfGlyphs)
		    for each id As UInt16 In iSubsettedGlyphs
		      strName = Dictionary(dicGlyphs.Value(id)).Value("name")
		      if Vartype(strName)=8 Then // is_string($name))
		        strData = strData + pack("n", 258+iNumNames)
		        strNames = strNames + chr(len(strName)) + strName
		        iNumNames=iNumNames + 1
		      else
		        strData = strData + pack("n", strName)
		      end if
		    next
		    strData = strData + strNames
		  else
		    // Version 3.0
		    Skip(4)
		    strData = chr(0) + chr(3) + chr(0) + chr(0) //"\x00\x03\x00\x00";
		    strData = strData + Read(4+2*2+5*4)
		  end if
		  SetTable("post", strData)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(FontFile As FolderItem)
		  f=FontFile
		  if f=nil or f.Exists=False Then
		    ParseError("Can't open file")
		    Exit Sub
		  end if
		  ReadStream = BinaryStream.Open(f, False)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(strFontFileName As String)
		  'f=GetFolderItem(strFontFileName)
		  f=getFontFile(strFontFileName)
		  if f=nil or f.Exists=False Then
		    ParseError("Can't open file: " + strFontFileName)
		    Exit Sub
		  end if
		  ReadStream = BinaryStream.Open(f, False)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Destructor()
		  If f<>Nil Then f=nil
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function embeddable() As Boolean
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub LoadTable(strTag As String)
		  Dim iLength As Integer
		  Dim n As Integer
		  Seek(strTag)
		  iLength = Dictionary(dicTables.Value(strTag)).Value("length")
		  n = iLength mod 4
		  if n>0 Then
		    iLength = iLength + 4 - n
		  end if
		  Dictionary(dicTables.value(strTag)).Value("data") = Read(iLength)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Parse()
		  ParseOffsetTable
		  ParseHead
		  ParseHhea
		  ParseMaxp
		  ParseHmtx
		  ParseLoca
		  ParseGlyf
		  ParseCmap
		  ParseName
		  ParseOS2
		  ParsePost
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub ParseCmap()
		  Dim iNumTables As Integer
		  Dim iOffset31 As Integer
		  Dim i As Integer
		  Dim iPlatformID As UInt16
		  Dim iEncodingID As UInt16
		  Dim iFormat As UInt16
		  Dim iOffset As UInt32
		  Dim iStartCount() As UInt16
		  Dim iEndCount() As UInt16
		  Dim iIdDelta() As Int16
		  Dim iIdRangeOffset() As UInt16
		  Dim iSegCount As UInt16
		  Dim iC As UInt16
		  Dim iC1 As UInt16
		  Dim iC2 As UInt16
		  Dim iD As Int16
		  Dim iRo As UInt16
		  Dim iGid As UInt16
		  
		  Seek("cmap")
		  Skip(2) // version
		  
		  iNumTables = ReadUShort
		  iOffset31 = 0
		  for i=0 to iNumTables-1
		    
		    iPlatformID = ReadUShort
		    iEncodingID = ReadUShort
		    iOffset = ReadULong
		    if (iPlatformID=3 and iEncodingID=1) Then
		      iOffset31 = iOffset
		    end if
		  next
		  if iOffset31=0 Then
		    ParseError("No Unicode encoding found")
		    Exit Sub
		  end if
		  Readstream.Position=CLong(Dictionary(dicTables.Value("cmap")).Value("offset")) + iOffset31
		  iFormat = ReadUShort
		  if(iFormat<>4) Then
		    ParseError("Unexpected subtable format: " + Cstr(iFormat))
		  end if
		  Skip(2*2) // length, language
		  iSegCount = ReadUShort/2
		  ReDim iEndCount(iSegCount-1)
		  ReDim iStartCount(iSegCount-1)
		  ReDim iIdDelta(iSegCount-1)
		  ReDim iIdRangeOffset(iSegCount-1)
		  Skip(3*2) // searchRange, entrySelector, rangeShift
		  for i=0 to iSegCount-1
		    iEndCount(i) = ReadUShort()
		  next
		  Skip(2) // reservedPad
		  for i=0 to iSegCount-1
		    iStartCount(i) = ReadUShort
		  next
		  for i=0 to iSegCount-1
		    iIdDelta(i) = ReadShort
		  next
		  iOffset = ReadStream.Position
		  for i=0 to iSegCount-1
		    iIdRangeOffset(i) = ReadUShort
		  next
		  
		  for i=0 to iSegCount-1
		    iC1 = iStartCount(i)
		    iC2 = iEndCount(i)
		    iD = iIdDelta(i)
		    iRo = iIdRangeOffset(i)
		    if iRo>0 Then
		      ReadStream.Position=iOffset + 2 * i + iRo
		    end if
		    for iC=iC1 to iC2
		      if iC=&hFFFF Then
		        exit For iC
		      end if
		      if iRo>0 Then
		        iGid = ReadUShort
		        if iGid>0 Then
		          iGid = iGid + iD
		        end if
		      else
		        iGid = iC+iD
		      end if
		      if iGid>=65536 Then
		        iGid = iGid - 65536
		      end if
		      if iGid>0 Then
		        if iChars.Ubound<iC Then 
		          ReDim iChars(iC)
		          Redim iCharsIsSet(iC)
		        end if
		        iChars(iC) = iGid
		        iCharsIsSet(iC) = True
		      end if
		    next
		  next
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub ParseError(strText As String)
		  'Dim iResult As Integer
		  'iResult=MsgBox (strText, 16, "Error")
		  System.DebugLog strText
		  bParseError=True
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub ParseGlyf()
		  Dim iTableOffset As Integer
		  Dim i As Integer
		  Dim iOffset As Integer
		  Dim iFlags As UInt16
		  Dim iIndex As UInt16
		  Dim iSkip As Integer
		  Dim dicGlyph As Dictionary
		  Dim a As Dictionary
		  Dim dicTable As Dictionary
		  dicTable=Dictionary(dicTables.Value("glyf"))
		  itableOffset = CLong(dicTable.Value("offset"))
		  'foreach($this->glyphs as &$glyph)
		  for i=0 to dicGlyphs.Count-1
		    dicGlyph=dicGlyphs.Value(dicGlyphs.key(i))
		    if Clong(dicGlyph.Value("length"))>0 Then
		      Readstream.Position=iTableOffset+CLong(dicGlyph.Value("offset"))
		      if ReadShort<0 Then
		        // Composite glyph
		        Skip(4*2) // xMin, yMin, xMax, yMax
		        iOffset = 5*2
		        a=new Dictionary
		        do
		          iFlags = ReadUShort
		          iIndex = ReadUShort
		          a.Value(iOffset+2) = iIndex
		          if (iflags and 1)=1 Then// ARG_1_AND_2_ARE_WORDS
		            iSkip = 2*2
		          else
		            iSkip = 2
		          end if
		          if (iflags and 8)=8 Then // WE_HAVE_A_SCALE
		            iSkip = iSkip + 2
		          elseif (iFlags and 64)=64 Then // WE_HAVE_AN_X_AND_Y_SCALE
		            iSkip = iSkip + 2*2
		          elseif (iFlags and 128)=128 Then // WE_HAVE_A_TWO_BY_TWO
		            iSkip = iSkip + 4*2
		          end if
		          Skip(iSkip)
		          iOffset = iOffset + 2*2 + iSkip
		        loop until (iFlags and 32)<>32 // MORE_COMPONENTS
		        dicGlyph.Value("components") = a
		        Dicglyphs.Value(dicGlyphs.key(i)) = dicGlyph
		      End if
		    End If
		  next
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub ParseHead()
		  Seek("head")
		  Skip(3*4) // version, fontRevision, checkSumAdjustment
		  Dim iMagicNumber As Uint32
		  iMagicNumber = ReadULong()
		  
		  if iMagicNumber<>&h5F0F3CF5 Then
		    ParseError("Incorrect magic number")
		    exit Sub
		  end if
		  
		  Skip(2) // flags
		  iUnitsPerEm = ReadUShort
		  Skip(2*8) // created, modified
		  iXMin = ReadShort
		  iYMin = ReadShort
		  iXMax = ReadShort
		  iYMax = ReadShort
		  Skip(3*2) // macStyle, lowestRecPPEM, fontDirectionHint
		  iIndexToLocFormat = ReadShort
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub ParseHhea()
		  Seek("hhea")
		  Skip(4+3*2)
		  iAdvanceWidthMax = ReadUShort
		  'Skip(4+15*2)
		  Skip(11*2)
		  iNumberOfHMetrics = ReadUShort
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub ParseHmtx()
		  Dim i As Integer
		  Dim iAdvanceWidth As UInt16
		  Dim iLsb As Int16
		  Seek("hmtx")
		  //glyphs = array();
		  dicGlyphs= new Dictionary
		  for i=0 to iNumberOfHMetrics-1
		    iAdvanceWidth = ReadUShort
		    iLsb = ReadShort
		    dicGlyphs.value(i) = New Dictionary("w" : iAdvanceWidth, "lsb" : iLsb)
		  next
		  for i = iNumberOfHMetrics to iNumGlyphs-1
		    iLsb = ReadShort
		    dicglyphs.Value(i) = New Dictionary("w" : iAdvanceWidth, "lsb" : iLsb)
		  next
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub ParseLoca()
		  Seek("loca")
		  Dim iOffsets() As UInt32
		  Dim i As Integer
		  if iIndexToLocFormat=0 Then
		    // Short format
		    for i=0 to iNumGlyphs
		      iOffsets.Append 2*ReadUShort
		    Next
		  else
		    
		    // Long format
		    for i=0 to iNumGlyphs
		      iOffsets.Append ReadULong
		    next
		  end if
		  
		  for i=0 to iNumGlyphs-1
		    Dictionary(dicGlyphs.Value(i)).Value("offset") = iOffsets(i)
		    Dictionary(dicGlyphs.Value(i)).Value("length") = iOffsets(i+1) - iOffsets(i)
		  next
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub ParseMaxp()
		  Seek("maxp")
		  Skip(4)
		  iNumGlyphs = ReadUShort
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub ParseName()
		  'Dim iTableOffset As Integer
		  'Dim iCount As UInt16
		  'Dim iStringOffset As UInt16
		  'Dim iNameID As UInt16
		  'Dim iLength As UInt16
		  'Dim iOffset As UInt16
		  'Dim strS As String
		  'Dim rg As New RegEx
		  '
		  'Dim i As Integer
		  'Seek("name")
		  'iTableOffset = CLong(Dictionary(dicTables.Value("name")).Value("offset"))
		  'strPostScriptName = ""
		  'Skip(2) // format
		  'iCount = ReadUShort
		  'iStringOffset = ReadUShort()
		  'for i=0 to iCount-1
		  'Skip(3*2) // platformID, encodingID, languageID
		  'iNameID = ReadUShort
		  'iLength = ReadUShort
		  'iOffset = ReadUShort
		  'if iNameID=6 Then
		  '// PostScript name
		  '//fseek($this->f, $tableOffset+$stringOffset+$offset, SEEK_SET);
		  'ReadStream.Position=iTableOffset + iStringOffset + iOffset
		  'strS = DefineEncoding(Read(iLength), Encodings.UTF8)
		  'strS = ByteReplace(strS, 0, "")
		  ''$s = preg_replace('|[ \[\](){}<>/%]|', '', $s);
		  'rg.SearchPattern="|[ \[\](){}<>/%]|"
		  'rg.ReplacementPattern=""
		  'rg.Options.ReplaceAllMatches=true
		  'strS=rg.Replace(strS)
		  'strPostScriptName = strS
		  'exit for i
		  'end if
		  'next
		  'if strPostScriptName="" Then
		  'Error("PostScript name not found")
		  'end if
		  Dim iTableOffset As Integer
		  Dim iCount As UInt16
		  Dim iStringOffset As UInt16
		  Dim iNameID As UInt16
		  Dim iLength As UInt16
		  Dim iOffset As UInt16
		  Dim strS As String
		  Dim rg As New RegEx
		  Dim iNextNameRecordPosition As UInt64
		  
		  Dim i As Integer
		  Seek("name")
		  iTableOffset = CLong(Dictionary(dicTables.Value("name")).Value("offset"))
		  strPostScriptName = ""
		  Skip(2) // format
		  iCount = ReadUShort
		  iStringOffset = ReadUShort()
		  for i=0 to iCount-1
		    Skip(3*2) // platformID, encodingID, languageID
		    iNameID = ReadUShort
		    iLength = ReadUShort
		    iOffset = ReadUShort
		    iNextNameRecordPosition=ReadStream.Position
		    if iNameID=6 or iNameID=1 or iNameID=2 Then
		      // PostScript name
		      //fseek($this->f, $tableOffset+$stringOffset+$offset, SEEK_SET);
		      ReadStream.Position=iTableOffset + iStringOffset + iOffset
		      strS = DefineEncoding(Read(iLength), Encodings.UTF8)
		      strS = ByteReplace(strS, 0, "")
		      '$s = preg_replace('|[ \[\](){}<>/%]|', '', $s);
		      rg.SearchPattern="|[ \[\](){}<>/%]|"
		      rg.ReplacementPattern=""
		      rg.Options.ReplaceAllMatches=true
		      strS=rg.Replace(strS)
		      Select case iNameID
		      case 6
		        strPostScriptName = strS
		      case 1
		        strFamilyName=strS
		      case 2
		        strSubFamilyName=strS
		      End Select
		      if strPostScriptName<>"" and strFamilyName<>"" and strSubFamilyName<>"" then
		        exit for i
		      else
		        ReadStream.Position=iNextNameRecordPosition
		      end if
		    end if
		  next
		  if strPostScriptName="" Then
		    ParseError("PostScript name not found")
		  end if
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub ParseOffsetTable()
		  Dim strVersion As String
		  Dim iNumTables As Uint16
		  Dim i As Integer
		  Dim strTag As String
		  Dim strChecksum As String
		  Dim iOffset As Int32
		  Dim iLength As Int32
		  
		  strVersion = Read(4)
		  if strVersion="OTTO" Then
		    ParseError("OpenType fonts based on PostScript outlines are not supported")
		    exit Sub
		  end if
		  
		  if strVersion<>Chr(&h00) + chr(&h01) + chr(&h00) + chr(&h00) Then//"\x00\x01\x00\x00" Then
		    ParseError("Unrecognized file format")
		    exit Sub
		  end if
		  
		  iNumTables = ReadUShort
		  Skip(3*2) // searchRange, entrySelector, rangeShift
		  '$this->tables = array();
		  dicTables= new Dictionary
		  
		  for i=0 to iNumTables-1
		    
		    strTag = Read(4)
		    strCheckSum = Read(4)
		    iOffset = ReadULong
		    iLength = ReadULong
		    Self.dicTables.Value(strtag) = New Dictionary("offset" : iOffset, "length" : iLength, "checkSum" : strCheckSum)
		  next
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub ParseOS2()
		  Dim iVersion As UInt16
		  Dim iFsType As UInt16
		  Dim iFsSelection As UInt16
		  
		  //https://www.microsoft.com/typography/otspec/os2.htm
		  
		  Seek("OS/2")
		  iVersion = ReadUShort
		  iAvgCharWidth = ReadShort // xAvgCharWidth
		  Skip(2*2) // usWeightClass, usWidthClass
		  iFsType = ReadUShort
		  bEmbeddable = (iFsType<>2) and (iFsType and &h200)=0
		  Skip(11*2+10+4*4+4)
		  iFsSelection = ReadUShort
		  bitalic = ((iFsSelection and 1)<>0)
		  bBold = ((iFsSelection and 32)<>0)
		  //Skip(2*2) // usFirstCharIndex, usLastCharIndex
		  iFirstCharIndex = ReadUShort //usFirstCharIndex
		  iLastCharIndex =  ReadUShort //usLastCharIndex
		  if iLastCharIndex>255 then
		    iLastCharIndex=255
		  end if
		  iTypoAscender = ReadShort
		  'iTypoDescender = -1 * ReadShort
		  iTypoDescender = ReadShort
		  //http://stackoverflow.com/questions/811608/how-do-i-calculate-pdf-leading-from-ttf
		  //https://www.microsoft.com/typography/otspec/recom.htm
		  iTypoLeading = ReadShort
		  if iVersion>=2 Then
		    'Skip(3*2+2*4+2)
		    Skip(2*2+2*4)
		    ixHeight = ReadShort
		    iCapHeight = ReadShort
		  else
		    ixHeight = 0
		    iCapHeight = 0
		  end if
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub ParsePost()
		  Dim iVersion As UInt32
		  Dim iGlyphNameIndex() As UInt16
		  Dim strNames() As String
		  Dim iNumNames As Integer
		  Dim i As Integer
		  Dim iIndex As UInt16
		  Dim iLen As Integer
		  
		  Seek("post")
		  iVersion = ReadULong
		  iItalicAngle = ReadShort
		  Skip(2) // Skip decimal part
		  iUnderlinePosition = ReadShort
		  iUnderlineThickness = ReadShort
		  bIsFixedPitch = (ReadULong<>0)
		  if iVersion=&h20000 Then
		    // Extract glyph names
		    Skip(4*4) // min/max usage
		    Skip(2) // numberOfGlyphs
		    iNumNames = 0
		    for i=0 to iNumGlyphs-1
		      iIndex = ReadUShort
		      iGlyphNameIndex.Append iIndex
		      if(iIndex>=258 and iIndex-257>iNumNames) Then
		        iNumNames = iIndex-257
		      end if
		    next
		    
		    for i=0 to iNumNames-1
		      iLen = Asc(Read(1))
		      strNames.Append Read(iLen)
		    next
		    
		    Dim igni As Integer
		    Dim ind As Integer
		    igni=iGlyphNameIndex.Ubound
		    
		    'for each ind As UInt16 In iGlyphNameIndex
		    for ind=0 to igni '-1
		      iIndex=iGlyphNameIndex(ind)
		      if iIndex>=258 Then
		        Dictionary(dicGlyphs.Value(dicGlyphs.key(ind))).Value("name") = strNames(iIndex-258)
		      else
		        Dictionary(dicGlyphs.Value(dicGlyphs.key(ind))).Value("name") = iIndex
		      end if
		    next
		    bGlyphNames = true
		  else
		    bGlyphNames = false
		  end if
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Read(iChars As Integer) As String
		  If iChars>0 Then
		    Return ReadStream.Read(iChars)
		  Else
		    Return ""
		  End If
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function ReadShort() As Int16
		  Dim a As Int16
		  a = ReadStream.ReadInt16 //unpack('nn', fread($this->f,2)); //n unsigned short (always 16 bit, big endian byte order)
		  'if a>=&h8000 Then
		  'a=a-65536
		  'end if
		  return a
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function ReadULong() As uInt32
		  Dim a As UInt32 
		  a = ReadStream.ReadUInt32 //unpack('NN', fread($this->f,4)); N unsigned long (always 32 bit, big endian byte order)
		  return a
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function ReadUShort() As uInt16
		  Dim a As UInt16 
		  a = ReadStream.ReadUInt16 //unpack('nn', fread($this->f,2)); //n unsigned short (always 16 bit, big endian byte order)
		  return a
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Seek(strTag As String)
		  if dicTables.HasKey(strTag)=False Then
		    ParseError("Table not found: " + strTag)
		    Exit Sub
		  end if
		  //fseek($this->f, $this->tables[$tag]['offset'], SEEK_SET);
		  Dim dicTmp As Dictionary
		  dicTmp=dicTables.Value(strTag)
		  if dicTmp.haskey("offset")=False Then
		    ParseError("offset not found: " + strTag)
		    Exit Sub
		  end if
		  ReadStream.Position=Clong(dicTmp.Value("offset"))
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetTable(strTag As String, strData As String)
		  Dim iLength As Integer
		  Dim n As Integer
		  Dim mb as new MemoryBlock(1)
		  Dim strChecksum As String
		  mb.Byte(0)=&h00
		  iLength = len(strData)
		  n = iLength mod 4
		  if n>0 Then
		    strData = str_pad(strData, iLength+4-n, mb)
		  end if
		  Dictionary(dicTables.Value(strTag)).Value("data")=strData
		  Dictionary(dicTables.Value(strTag)).Value("length")=iLength
		  strChecksum=Checksum(strData)
		  Dictionary(dicTables.Value(strTag)).Value("checksum")=strChecksum
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Skip(iChars As Integer)
		  Dim strRead As String
		  strRead=ReadStream.Read(iChars)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Subset(iCharsParam() As UInt16)
		  AddGlyph(0)
		  Dim i As Integer
		  for i=0 to iCharsParam.Ubound
		    if iCharsIsSet(iCharsParam(i))=True Then
		      iSubsettedChars.Append iCharsParam(i)
		      AddGlyph(iChars(iCharsParam(i)))
		    end if
		  next
		End Sub
	#tag EndMethod


	#tag Note, Name = Freetype Code Library Reference
		
		The FreeType Engine
		
		Core Library Reference
		
		
		-----------------------------------
		
		
		Table of Contents:
		
		Introduction
		
		I. Types
		
		II. Functions
		
		III. Error codes
		
		
		--------------------
		
		
		Introduction:
		
		This  reference  presents the  types,  functions  and error  codes
		defined in the high-level API header file "freetype.h".  Note that
		all symbols defined  in this file are prefixed  by "TT_", to avoid
		name conflicts with other packages at link time.
		
		Some of its  parts are also dedicated to  the extension that comes
		by default with this distribution of the library, which is kerning
		support.
		
		----------------------------------------------------------------------------
		
		I. Types:
		
		Here is  the list of  all the types  defined in the  core FreeType
		API.  Their exact definition can be found in the file "freetype.h"
		which should be included by every client application.
		
		
		TT_Bool
		
		Can be either TRUE or FALSE.
		
		..................................................................
		
		TT_Fixed
		
		A  signed 16.16  fixed  float value  used  to specify  transform
		coefficients and other important data.
		
		..................................................................
		
		TT_FWord
		
		A signed  16 bits value used  to express a  distance measured in
		the font's original EM units.  These are also called 'FUnits' in
		the TrueType specification.
		
		..................................................................
		
		TT_UFWord
		
		Unsigned FWord.
		
		..................................................................
		
		TT_String
		TT_Char
		TT_Byte
		
		These types represent various 8 bits integer types (for strings,
		signed, and unsigned values, respectively).
		
		..................................................................
		
		TT_Short
		TT_UShort
		TT_Long
		TT_ULong
		
		These four  types are  aliases for 16  bits integer  (signed and
		unsigned) and 32 bits one (signed and unsigned).
		
		..................................................................
		
		TT_F2Dot14
		
		A  2.14 fixed  float integer  used  for unary  vectors and  some
		scaling coefficients.  Their layout is:
		
		s :  1 -- sign bit
		m :  1 -- mantissa bit
		f : 14 -- unsigned fractional value
		
		where  's:m' is  the 2-bit  signed  integer value  to which  the
		always positive fractional part 'f' should be added.
		
		..................................................................
		
		TT_F26Dot6
		
		The  26.6 fixed  float format  used to  define  fractional pixel
		coordinates.  Here, 1 unit = 1/64 pixel.
		
		..................................................................
		
		TT_Pos
		
		This  type  is  used  to  store  point  coordinates,  either  in
		fractional  pixels (26.6 fixed  floats) or  in EM  units (simple
		integers).
		
		The meaning of  the value depends on the  context.  For example,
		all  distances  relative to  a  scaled  glyph  are expressed  in
		fractional pixels (including bearings, advances, etc).  However,
		the same distances are in notional font units when the glyph was
		loaded unscaled.
		
		..................................................................
		
		TT_UnitVector
		
		A simple  structure used to  store a unit vector.   The vector's
		coordinates are expressed in fixed float format (2.14).
		
		struct
		{
		TT_F2Dot14  x;
		TT_F2Dot14  y;
		}
		
		..................................................................
		
		TT_Vector
		
		A  simple  structure  used   to  store  a  single  vector.   Its
		coordinates are expressed in fixed float format (26.6).
		
		struct
		{
		TT_Pos  x;
		TT_Pos  y;
		}
		
		..................................................................
		
		TT_Matrix
		
		A  simple structure  used to  store  a single  2x2 matrix.   Its
		coefficients are  expressed in  16.16 fixed float  format.  This
		matrix is  used to perform  linear transformations on  the glyph
		outline, such as slanting or rotation.
		
		struct
		{
		TT_Fixed  xx, xy;
		TT_Fixed  yx, yy;
		};
		
		The computation performed is:
		
		x' = xx * x  +  xy * y
		y' = yx * x  +  yy * y
		
		..................................................................
		
		TT_BBox
		
		A  simple   type  to   hold  a  glyph's   bbox.   Used   by  the
		TT_Get_Outline_BBox() API.
		
		struct
		{
		TT_Pos  xMin, yMin;
		TT_Pos  xMax, yMax;
		}
		
		..................................................................
		
		TT_Outline
		
		Outlines are now full-class citizens, with their own API.
		
		This   structure  is   used  to   describe  a   vectorial  glyph
		representation to the rasterizer.   It is made of several fields
		described below.  Note however that:
		
		***** THIS  STRUCTURE  MAY  CHANGE   IN  THE  FUTURE.   We  thus
		***** encourage you to use  the outlines APIs described below to
		***** process   your   outlines,  i.e.    create/copy/translate/
		***** transform them as well as rendering bitmaps and pixmaps.
		
		***** THE STRUCTURE CHANGED BETWEEN 1.0 and 1.1!
		
		Now that you've been warned, the fields are:
		
		- An array of points:
		
		The  'n_points'  field  gives  the  number of  points  in  the
		outline,  while  their coordinates  are  found  in the  single
		vector array 'points'.  The  'flag' array holds for each point
		a flag indicating its type.
		
		Currently, only  the first bit  (bit 0, the  least significant
		bit) of each byte is  meaningful to the rasterizer.  When set,
		it indicates that the point  is _on_ the curve.  When not set,
		the point is  said to be _off_ the curve.   It's then a Bezier
		control point.
		
		For  more information  about point  states, read  the TrueType
		specification or the scan-line documentation "raster.txt".
		
		- An array of contours' end-point indexes:
		
		The 'n_contours' field gives the number of contours, while the
		'contours'  array holds  the  indexes of  each contour's  last
		point.  Note that the first contour always begin at point 0.
		
		Hence, contours[0]  holds the index  of the last point  of the
		first contour.   The second  contour starting at  point number
		'contours[0]+1' and ending a point number 'contours[1]'.
		
		** IMPORTANT NOTE: **
		*********************
		
		The last table entry _must_  always give the total number of
		points used to draw the contours, i.e.:
		
		contours[n_contours-1] == n_points
		
		If  this value is  bigger than  'n_points' when  calling the
		scan-line converter,  the component will  immediately return
		an error (TT_Err_Too_Many_Points).  If the value is smaller,
		only the points contained  in the described contours will be
		used in the conversion process.
		
		- An owner field:
		
		This  flag  should  **NEVER**  be  changed by  the  user.   It
		indicates whether the pointer fields own the arrays they refer
		to (when the flag is set),  or if they simply alias them (flag
		unset).
		
		- A high precision flag:
		
		When  this boolean  is  set (i.e.   not  zero), the  scan-line
		converter  uses  a higher  precision  to  compute segment  and
		Bezier coordinates (more  precisely, it uses 1/1024 precision,
		instead of the normal 1/64).  This is of course slower but can
		be important for glyphs rendered at small sizes.
		
		- A second pass flag:
		
		When this  boolean is set, the scan-line  converter performs a
		second  sweep on  the bitmap/pixmap  detect  vertical drop-out
		controls.  Only horizontal drop-outs are detected in the first
		pass.  This  is slower, but  important for glyphs  rendered at
		small sizes.
		
		- A dropout mode:
		
		Used to specify the method to apply for drop-out control (also
		called 'continuity testing'  in other environments).  The mode
		value  must be  one  of  the values  defined  by the  TrueType
		specification.
		
		The recent  modes 4  and 5 introduced  in the  newest TrueType
		specification (Version 1.6) are fully supported.
		
		An invalid value (i.e., other than  0, 1, 2, 4, or 5) is taken
		as no dropout control (equivalent to mode 0).
		
		NOTE 1:
		
		The outline returned  by TT_Get_Glyph_Outline() only alias the
		data that is part of  a glyph container (see below).  However,
		it is  possible to create  and process your own  outlines with
		the newest  API functions TT_New_Outline(), TT_Done_Outline(),
		TT_Copy_Outline(), TT_Translate_Outline(), etc.
		
		TT_Done_Outline() will  only discard an outline's  array if it
		owns them.
		
		NOTE 2:
		
		The outlines created by TT_New_Outline() are _not_ released by
		the  engine  on  TT_Done_FreeType(),  they must  be  discarded
		explicitly by the user who has created them!
		
		NOTE 3:
		
		The   glyph   loader   sets   the   fields   'high_precision',
		'dropout_mode' and 'second_pass' automatically.
		
		NOTE 4:
		
		This structure was called TT_Glyph_Outline in beta versions of
		FreeType.
		
		..................................................................
		
		TT_Glyph_Metrics
		
		A structure  used to return simple glyph  metrics.  These values
		are expressed in fractional  pixels (26.6 format) if scaling was
		active, and in FUnits otherwise.
		
		The main idea was to accomodate vertical text layouts by getting
		rid  of the  two explicit  "leftSideBearing"  and "advanceWidth"
		names.
		
		Though  the library  only processes  horizontal metrics  for the
		moment, the fields meaning vary with the text layout:
		
		bearingX: also  known as the "left side  bearing".  This value
		gives the horizontal  distance from the pen position
		to the glyph's bbox xmin.
		
		bearingY: also  known as the  "top side bearing", this  is the
		vertical distance  from the baseline  to the glyph's
		bbox ymax.
		
		struct
		{
		TT_BBox  bbox;  /* the glyph's bbox */
		
		TT_Pos   bearingX;  /* left-side bearing */
		TT_Pos   bearingY;  /* top-side bearing  */
		
		TT_Pos   advance;   /* advance width or height */
		};
		
		** IMPORTANT NOTE **
		
		Because  of the convention  used by  the TrueType  engine, the
		outlines generated  at glyph-load time are all  placed so that
		the pen is at position  (0,0).  This means that you don't need
		to increase  the pen position by  "bearingX" and/or "bearingY"
		before  writing a glyph.   Text output  can be  performed with
		simple lines like:
		
		for (glyphs in text)
		{
		TT_Load_Glyph( ... );
		TT_Get_Glyph_Outline( glyph, &outline );
		TT_Translate_Outline( outline,
		cur_pos_x * 64, cur_pos_y * 64 );
		
		TT_Get_Outline_Bitmap( outline, bitmap );
		/* blit bitmap to surface */
		
		cur_pos_x += (metrics.advance + 32) / 64
		}
		
		See the file 'test/ftstring.c' for concrete examples.
		
		NOTE 2:
		
		This structure has changed from the beta version of FreeType.
		
		..................................................................
		
		TT_Big_Glyph_Metrics
		
		This structure,  though still unused,  will be used to  return a
		same glyph's metrics in both horizontal and vertical layouts.
		
		struct
		{
		TT_BBox  bbox;          /* the glyph's bbox */
		
		TT_Pos   horiBearingX;  /* horizontal left-side bearing */
		TT_Pos   horiBearingY;  /* horizontal top-side bearing  */
		
		TT_Pos   vertBearingX;  /* vertical left-side bearing */
		TT_Pos   vertBearingY;  /* vertical top-side bearing  */
		
		TT_Pos   horiAdvance;   /* horizontal advance */
		TT_Pos   vertAdvance;   /* vertical advance   */
		}
		
		..................................................................
		
		TT_Instance_Metrics
		
		A structure used to return instance (point size) metrics.
		
		struct
		{
		int       pointSize;
		/* point size in points (1 point = 1/72 inch) */
		
		TT_UShort  x_ppem;  /* horizontal pixels per EM square */
		TT_UShort  y_ppem;  /* vertical pixels per EM square   */
		
		TT_Fixed   x_scale; /* 16.16 scale for EM -> frac pixels */
		TT_Fixed   y_scale; /* 16.16 scale for EM -> frac pixels */
		
		TT_UShort  x_resolution; /* device hor. res. in dpi  */
		TT_UShort  y_resolution; /* device vert. res. in dpi */
		};
		
		The fields  'x_scale' and  'y_scale' can be  used by  clients to
		convert from notional units to fractional pixels, e.g.:
		
		frac_distance = (em_distance * x_scale) / 0x10000;
		
		..................................................................
		
		TT_Raster_Map
		
		This structure is  used to describe a target  bitmap (or pixmap)
		to the scan-line  converter.  It _must_ be set  up by the client
		application.
		
		- The  'rows' field  contains the  total number  of rows  in the
		bitmap
		
		- The 'width' field gives the number of pixels per row (a bit or
		a byte, depending on the map's nature).
		
		- The  'cols' field gives  the number  of columns,  i.e., bytes,
		taken by each row in the map buffer.
		
		** IMPORTANT **: the 'cols' field  must be a multiple of 4 for
		pixmaps!
		
		Typically, its value should  be '(width+7)/8' for bitmaps, and
		'(width+3) & -4' for pixmaps.
		
		- The 'flow' field gives the map's vertical orientation.
		
		For example, if  the first bytes of the  bitmap buffer pertain
		to its upper row, the flow is said to be going 'down', and the
		field should  take the  value 'TT_Flow_Down'.  If  these bytes
		pertain to  its lowest  row, the flow  is going 'up',  and the
		value is 'TT_Flow_Up'.
		
		As an example, the PC video modes use a 'down' flow, where the
		first VRAM  byte corresponds to the upper  and leftmost corner
		of the screen.
		
		- The 'bitmap' field is a typeless pointer to the map's buffer.
		
		- The 'size' field  contains the buffer's size in  bytes.  It is
		usually computed as follows:
		
		size = rows * cols;
		
		NOTE 1:
		
		For  bitmaps, the  leftmost-pixel  is related  to the  highest
		(i.e.  most significant) bit  of its byte.  There is currently
		no support for the opposite convention found in some systems.
		
		(It can  be easily added if  you really need it,  just ask the
		development team.)
		
		struct
		{
		int     rows;    /* number of rows                    */
		int     cols;    /* number of columns (bytes) per row */
		int     width;   /* number of pixels per line         */
		int     flow;    /* bitmap orientation                */
		
		void*   bitmap;  /* bit/pixmap buffer                 */
		long    size;    /* bit/pixmap size in bytes          */
		} TT_Raster_Map;
		
		
		NOTE 2:
		
		The functions TT_Get_Outline_Bitmap() or TT_Get_Glyph_Bitmap()
		are  used  to  render   bitmaps  into  a  TT_Raster_Map.   The
		convention  used  is 0  for  the  background,  and 1  for  the
		foreground.  The glyph is simply 'or-ed' to the bitmap buffer.
		
		NOTE 3:
		
		The        functions        TT_Get_Outline_Pixmap()        and
		TT_Get_Glyph_Pixmap()  are  used  to  render  pixmaps  into  a
		TT_Raster_Map.   Note that  pixels  are drawn  in  spans of  4
		successive  bytes,  when needed.   This  means  that you  must
		ALWAYS  pass  a  clean   pixmap  buffer  to  these  functions.
		Otherwise, garbage could accumulate!
		
		..................................................................
		
		TT_Header
		
		This structure  is used to  hold the font's header.   Its layout
		and meaning  are defined in  the TrueType specification,  in the
		'head' section.
		
		..................................................................
		
		TT_Horizontal_Header
		
		This  structure is used  to hold  the font's  horizontal header.
		Its   layout   and  meaning   are   defined   in  the   TrueType
		specification, in the 'hhead' section.
		
		..................................................................
		
		TT_OS2
		
		This  structure is  used to  hold  the font's  OS/2 table.   Its
		layout and meaning are defined in the TrueType specification, in
		the 'OS/2' section.
		
		..................................................................
		
		TT_Postscript
		
		This structure is used to hold the font's PostScript table.  Its
		layout and meaning are defined in the TrueType specification, in
		the 'post' section.
		
		..................................................................
		
		TT_Face_Properties
		
		This structure  is used to  return an opened  face's properties.
		These are:
		
		- The total  number of  glyphs in the  font, given by  the field
		'num_Glyphs'.
		
		- The  maximum number  of points  for the  font's  glyphs.  This
		value  is  used to  allocate  the  points  tables of  a  glyph
		container's outline.  It can  be fairly large (like 256 points
		for Roman fonts).
		
		- The maximum  number of contours  for the font's  glyphs.  This
		value  is used  to allocate  the  contours tables  of a  glyph
		container's outline.  It can be fairly large (over 16, even in
		Roman fonts).
		
		- The number  of character mappings and name  records within the
		font.  These  values can still  be retrieved through  the APIs
		TT_Get_CharMapCount()  and  TT_Get_Num_Names(),  though  these
		have been _seriously_ deprecated.
		
		- The number of associated faces.  This number is always 1 for a
		normal TrueType font file.   However, when the face object was
		opened  from  a TrueType  collection,  it  contains the  total
		number of embedded fonts.
		
		- Pointers to  the face's  header, horizontal header,  OS/2, and
		PostScript tables.
		
		struct
		{
		TT_UShort  num_Glyphs;        /* number of glyphs in face */
		TT_UShort  max_Points; /* max. numb. of points in a glyph */
		TT_Short   max_Contours;
		/* maximum number of contours in a glyph */
		
		TT_ULong  num_Faces;
		/* 1 for normal TrueType files, and the */
		/* number of embedded faces for TT      */
		/* collections                          */
		
		TT_Header*             header;   /* TrueType header table */
		TT_Horizontal_Header*  horizontal;
		/* TrueType horizontal header */
		TT_Vertical_Header*    vertical;
		/* TrueType vertical header   */
		TT_OS2*                os2;        /* TrueType OS/2 table */
		TT_Postscript*         postscript;
		/* TrueType PostScript table */
		} TT_Face_Properties;
		
		- Note that  the "vertical" field is  set to NULL  when the font
		file does not contain any vertical metrics.
		
		..................................................................
		
		TT_Stream
		
		This handle type  defines a stream used to  access a font file's
		data.   A  client application  should  never  deal with  streams
		directly,  but  some engine  extensions  may  later  need it  to
		support more advanced features like font embedding.
		
		..................................................................
		
		TT_Face
		
		This type defines a handle used to reference a face object.  The
		objects are never accessed  directly by a client application; it
		can only  obtain handles to new  objects, and use  them to query
		specific information or processes.
		
		See also:
		
		TT_Open_Face(), TT_Open_Collection(), TT_Close_Face(),
		TT_Get_Face_Properties(), etc.
		
		..................................................................
		
		TT_Instance
		
		This type defines a handle  used to reference an instance object
		(also called a 'pointsize'  in other type engines).  An instance
		is always  created from  a valid face  object, and  is destroyed
		with it by the engine.
		
		See also:
		
		TT_New_Instance(), TT_Close_Instance(),
		TT_Set_Instance_Pointsize(), TT_Set_Instance_Resolutions(),
		etc.
		
		..................................................................
		
		TT_Glyph
		
		This type defines  a handle used to reference  a glyph container
		object.  A glyph  container is an object owning  tables sized to
		the font's  maximum profile  to hold any  glyph of a  given font
		file.
		
		It  contains an  outline, some  metrics,  as well  as some  data
		related  to the  way it  should  be processed  by the  scan-line
		converter.
		
		Note  that a  glyph  container doesn't  contain  any bitmap  nor
		pixmap!
		
		See also:
		
		TT_New_Glyph(), TT_Close_Glyph(), TT_Get_Glyph_Metrics(),
		TT_New_Outline(), TT_Get_Glyph_Outline(),
		TT_Get_Glyph_Bitmap(), TT_Get_Glyph_Pixmap()
		
		..................................................................
		
		TT_Error
		
		This is the type of all error codes returned by the API.  Nearly
		all functions return an error code, set to 0 in case of success.
		
		A list of all error codes is given in section III.
		
		..................................................................
		
		TT_Engine
		
		For the sake of re-entrancy,  which isn't supported fully by the
		engine,  it is  possible  to distinguish  'engines' to  separate
		several running instances of the library.  For example, it could
		be used as a DLL shared by several client applications.
		
		Each  client program  must  begin by  creating  its own  engine,
		through a  call to TT_Init_FreeType().  The engine  must also be
		passed as the first argument of the following functions:
		
		TT_Open_Face()
		TT_Open_Collection()
		TT_Set_Raster_Palette()
		TT_Get_Outline_Bitmap()
		TT_Get_Outline_Pixmap()
		TT_Done_FreeType()
		
		Note  that any  FreeType object  pertains to  one  single engine
		(there is no sharing). Closing an engine with TT_Done_FreeType()
		will delete all the objects  that have been allocated within its
		instance.
		
		..................................................................
		
		This distribution  comes with an extension used  to support access
		to a  font's kerning information.   The extension's types  and API
		are defined in the file "ftxkern.h"
		
		--------------------------------------------------------------------
		--------------------------------------------------------------------
		
		
		
		II. Functions:
		
		Here is a list of the core library's API.
		
		NOTE:
		
		A function's default result is an error code of type TT_Error; a
		list of error codes is given in section III below.
		
		Some functions return other types, in which case the result type
		is written with its description.
		
		..................................................................
		
		TT_FreeType_Version( int*  major, int*  minor );
		
		Queries the major and minor version of the library.
		
		..................................................................
		
		TT_Init_FreeType( TT_Engine*  engine );
		
		Creates and initializes  a new engine.  Returns a  handle to the
		engine in the '*engine' variable.
		
		This  call  must  be  performed  before any  other  function  of
		FreeType is  invoked.  The engine  handle must be passed  to the
		following functions:
		
		TT_Open_Face()
		TT_Open_Collection()
		TT_Set_Raster_Palette()
		TT_Done_FreeType()
		
		..................................................................
		
		TT_Done_FreeType( TT_Engine  engine );
		
		Finalizes  and destroys  an  engine.  This  call destroys  _all_
		objects that were previously created and used with the engine.
		
		..................................................................
		
		TT_Open_Face( TT_Engine  engine,
		TT_Text*   fontPathName,
		TT_face*   face );
		
		This  call opens  a font  file, located  by  'fontPathName', and
		returns a handle  to the newly corresponding face  object in the
		handle '*face'.  The object is part of the 'engine' instance.
		
		Example:
		
		error = TT_Open_Face( engine, "c:\ttf\wingding.ttf", &face );
		if ( error )
		fprintf( stderr, "could not open face\n" );
		
		NOTE 1:
		
		The font file can be  a TrueType collection; in this case, the
		engine will always  open the first embedded font  found in the
		file.
		
		NOTE 2:
		
		'TT_Text'  is   usually  defined   as  'char'  by   a  typedef
		declaration.  It may  be a 16bit quantity (or  even wider) for
		some operating systems; see ttconfig.h for details.
		
		..................................................................
		
		TT_Open_Collection( TT_Engine  engine,
		TT_Text*   collectionPathName,
		TT_ULong   fontIndex,
		TT_Face*   face );
		
		This  call opens  one  of the  font  files found  in a  TrueType
		collection.   The  file  is  selected  through  the  'fontIndex'
		argument.  The first file has index 0.
		
		Note  that to  know  a collection's  number  of embedded  fonts,
		you'll have to:
		
		1 - open the first collection font with TT_Open_Face().
		
		2 - query the face's properties through
		TT_Get_Face_Properties().
		
		The number of embedded faces is then 'properties->num_Faces'.
		
		Example:
		
		TT_Face             face;
		TT_Face_Properties  properties;
		
		error = TT_Open_Face( engine, "c:\ttf\sample.ttc", &face );
		if ( error ) { ...error .. }
		/* Open first embedded collection font */
		
		error = TT_Get_Face_Properties( face, &properties );
		if ( error ) { ...error .. }
		/* Get face metrics */
		
		printf( "There are %d fonts in this collection\n",
		properties->num_Faces );
		
		TT_Close_Face( face );
		
		error = TT_Open_Collection( engine, "c:\ttf\sample.ttc", 1,
		&face );
		if ( error ) { ...error .. }
		/* Open second font in collection */
		
		NOTE 1:
		
		If  the file  isn't a  collection, 'fontIndex'  must  be zero.
		Otherwise, an error will be produced.
		
		NOTE 2:
		
		'TT_Text'  is   usually  defined   as  'char'  by   a  typedef
		declaration.  It may  be a 16bit quantity (or  even wider) for
		some operating systems; see ttconfig.h for details.
		
		..................................................................
		
		TT_Set_Raster_Palette( TT_Engine  engine,
		char*      palette );
		
		Sets the gray-level palette for  an engine.  The palette is used
		to  create pixmaps  through the  TT_Get_Glyph_Pixmap() function.
		It is an array of five bytes, following the convention:
		
		palette[0] = background (white)
		palette[1] = light
		palette[2] = medium
		palette[3] = dark
		palette[4] = foreground (black)
		
		..................................................................
		
		TT_Get_Face_Properties( TT_Face              face,
		TT_Face_Properties*  properties );
		
		Returns  the  'face'  object's  '*properties'.   This  structure
		contains  various data,  like  the total  number  of glyphs  and
		pointers to some mandatory TrueType tables.
		
		(See the definition of TT_Face_Properties in section I.)
		
		..................................................................
		
		TT_Set_Face_Pointer( TT_Face  face,
		void*    data );
		
		For  convenience  purposes, each  face  object  has a  "generic"
		pointer which value is unused by the engine, but that can be set
		freely by client applications through this function.
		
		Do what you want with it; it's here to give you a chance to link
		a face object to your own structures and data.
		
		..................................................................
		
		void*  TT_Get_Face_Pointer( TT_Face  face );
		^^^^
		Returns    a    face     object's    generic    pointer.     See
		TT_Set_Face_Pointer() above.
		
		..................................................................
		
		TT_Flush_Face( TT_Face  face );
		
		Closes a  given face object's file handler  or descriptor.  This
		is  useful to save  system resources  if your  application opens
		dozens  or even  hundreds of  fonts.  The  face object  is still
		valid, and its file will  be re-opened automatically on the next
		request which requires disk access.
		
		..................................................................
		
		TT_Close_Face( TT_Face  face );
		
		Closes a  given 'face' object.  This function  will also destroy
		all  the face's  child instances.   The face's  glyphs  won't be
		destroyed, however.
		
		..................................................................
		
		TT_New_Instance( TT_Face       face,
		TT_Instance*  instance );
		
		Creates a new  instance object related to the  'face' object.  A
		handle to the newly created instance is returned in 'instance'.
		
		The default  instance resolution is  96dpi in both  vertical and
		horizontal direction; the default point size is 10pt.
		
		..................................................................
		
		TT_Set_Instance_Resolutions( TT_Instance  instance,
		TT_UShort    xResolution,
		TT_UShort    yResolution );
		
		Sets the  target device resolutions  for a given  instance.  The
		values are expressed  in dots per inch (dpi).   A value of 96dpi
		is typical for  an SVGA display, 72dpi for  a Macintosh one, and
		300 to  6000dpi for printers.   Default value (before a  call to
		this function) is 96dpi.
		
		..................................................................
		
		TT_Set_Instance_CharSize( TT_Instance  instance,
		TT_F26Dot6   charsize );
		
		Sets the point size for a given instance.  The size is expressed
		in fractional  (26.6) 'points', where  1 point = 1/72  inch. The
		default value is 10pt (before a call to this function).
		
		For example, to use a char size of 10pt, call the function with:
		
		TT_Set_Instance_CharSize( instance, 10 * 64 );
		
		Fractional char sizes are thus possible.
		
		..................................................................
		
		TT_Set_Instance_CharSizes( TT_Instance  instance,
		TT_F26Dot6   charWidth,
		TT_F26Dot6   charHeight );
		
		Sets  an  instance's glyph  width  and  height independently  in
		fractional  (26.6) points.   Similar  to Set_Instance_CharSize()
		with  the  exception  that  the horizontal  and  vertical  glyph
		dimensions can differ.
		
		..................................................................
		
		TT_Set_Pixel_Sizes( TT_Instance  instance,
		TT_UShort    pixelWidth,
		TT_UShort    pixelHeight,
		TT_F26Dot6   pointSize );
		
		This function  can be used  to specify directly the  pixel sizes
		and  point size  of a  given instance,  independently  of device
		resolutions.  This is not the  recommended way to do it, but can
		be used for debugging or simplicity in some special cases.
		
		Note that you _must_ provide a point size!
		
		..................................................................
		
		TT_Set_Instance_Transform_Flags( TT_Instance  instance,
		TT_Bool      rotated,
		TT_Bool      stretched );
		
		Sets the transform flags for  a given instance.  These flags are
		passed to the interpreter each time a glyph is loaded within the
		instance.  Their  role is to notify the  glyph hinting mechanism
		that the resulting  glyph will be transformed in  a special way.
		Setting  one of these  flags to  true usually  disables hinting,
		though this behaviour varies with each font file.
		
		NOTE:
		
		The  glyph   loader  doesn't  perform  the   rotation  or  the
		stretching itself; this must  be done explicitly by the client
		application.  Use the function TT_Transform_Outline() for that
		purpose.
		
		..................................................................
		
		TT_Get_Instance_Metrics( TT_Instance           instance,
		TT_Instance_Metrics*  imetrics );
		
		This call returns a given instance's current metrics.  They are
		returned  in the  'imetrics' structure,  which  contains, among
		other  things,  the  current   point  size,  ppem,  and  device
		resolution (horizontal and vertical).
		
		..................................................................
		
		TT_Set_Instance_Pointer( TT_Instance  instance,
		void*        data );
		
		For convenience  purposes, each instance object  has a "generic"
		pointer which value is unused by the engine, but that can be set
		freely by client applications through this function.
		
		Do what you want with it, it's here to give you a chance to link
		a face object to your own structures and data.
		
		..................................................................
		
		void*  TT_Get_Instance_Pointer( TT_Instance  instance )
		^^^^
		
		This function  returns an instance object's  generic pointer set
		through TT_Set_Instance_Pointer().
		
		..................................................................
		
		TT_Done_Instance( TT_Instance  instance );
		
		Closes a given instance  object, destroying its associated data.
		Note that this is performed  automatically when a face is closed
		on all its child  instances.  However, explicit deallocation can
		help in freeing the memory used by the application earlier.
		
		..................................................................
		
		TT_New_Glyph( TT_Face    face,
		TT_Glyph*  glyph );
		
		Creates  a  new glyph  container  for  the  glyphs of  the  font
		described by the  'face' handle.  A pointer to  the container is
		returned in 'glyph'.  The face is said to be the glyph's parent.
		
		NOTE:
		
		A glyph is destroyed with its parent face object.  However, it
		is possible to delete it explicitly with TT_Done_Glyph().
		
		..................................................................
		
		TT_Done_Glyph( TT_Glyph  glyph );
		
		Discards a glyph container.  This is also done automatically for
		all glyphs when closing its parent face object.
		
		..................................................................
		
		TT_Load_Glyph( TT_Instance  instance,
		TT_Glyph     glyph,
		TT_UShort    glyphIndex,
		TT_UShort    loadFlags );
		
		Loads and  processes (scales  and/or hints) a  glyph at  a given
		'instance' into the 'glyph' container.
		
		Note  that 'glyph' and  'instance' must  have the  _same_ parent
		face object, or an error message will be returned.
		
		'glyph_index'  is the  glyph's index  as found  in  the TrueType
		file.  It is  _not_ a character code (see  the charmap functions
		below).
		
		'load_flags' is  an integer that specifies  which operations are
		to be performed on  the loaded glyph.  The following values/bits
		are used:
		
		TTLOAD_SCALE_GLYPH
		
		Indicates that  the glyph must  be scaled to  the instance's
		resolution.   The pixel  coordinates returned  in  the glyph
		outline  structure   (see  below)  are   then  expressed  in
		fractional  pixels  represented  in  the  26.6  fixed  point
		floating format defined by Apple as 'F26Dot6'.
		
		If  scaling is  disabled,  the coordinates  returned in  the
		outline  structure  are  integer  font  units,  also  called
		'FUnits' by the TrueType specification.
		
		TTLOAD_HINT_GLYPH
		
		This flag is only valid  when scaling is on.  It informs the
		loader that the glyph  must be hinted (i.e., grid-fitted for
		optimal display).  Note that  hinting will occur only if the
		instance's  transformations   and  metrics  allow   it  (for
		example, most font programs disable hinting automatically in
		case of rotation or stretching).
		
		When  loading a hinted  glyph, the  metrics computed  by the
		loader,   including   the  bounding   box,   will  also   be
		grid-fitted.
		
		NOTE:
		
		You can  also use the constant TTLOAD_DEFAULT,  which is simply
		the union of  TTLOAD_SCALE_GLYPH and TTLOAD_HINT_GLYPH for most
		'typical' loads.
		
		..................................................................
		
		TT_Get_Glyph_Outline( TT_Glyph     glyph,
		TT_Outline*  outline );
		
		This  call returns  the  glyph's 'outline'.   This  is a  simple
		structure which  contains pointers to the data  used to describe
		an   outline  to   the  rasterizer.    See  the   definition  of
		TT_Outline in section I.
		
		..................................................................
		
		TT_Get_Glyph_Metrics( TT_Glyph           glyph,
		TT_Glyph_Metrics*  metrics );
		
		Extracts  the glyph's metrics  and copy  them to  the '*metrics'
		structure.  Its format is described in section I.
		
		When the glyph  has been loaded without scaling,  the values are
		expressed in FUnits (integers relative to the original font grid
		called the EM Square).
		
		When  the glyph  has been  loaded _with_  scaling, which  is the
		default, the  values are expressed  in fractional pixels  in the
		26.6 fixed  point float  format called F26Dot6  (where 1  unit =
		1/64th of a pixel).
		
		When the  glyph has  been loaded with  hinting, the  metrics are
		also  grid-fitted,  including  the  bounding box.   To  get  the
		un-fitted bbox,  just call TT_Get_Outline_BBox()  on the glyph's
		outline.
		
		NOTE:
		
		BBox fitting occurs according to the following scheme:
		
		#define  FLOOR( x )    ( (x) & -64 )
		#define  CEILING( x )  ( ( (x) + 63 ) & -64 )
		
		xMin = FLOOR( xMin );
		yMin = FLOOR( yMin );
		xMax = CEILING( xMax );
		yMax = CEILING( yMax );
		
		This means that the outline's  width and height in pixels can be
		computed simply from the fitted bbox, as:
		
		(xMax-xMin)/64   and   (yMax-yMin)/64
		
		..................................................................
		
		TT_Get_Glyph_Bitmap( TT_Glyph        glyph,
		TT_Raster_Map*  bitmap,
		TT_F26Dot6      xOffset,
		TT_F26Dot6      yOffset );
		
		This call converts  the vectorial glyph representation contained
		in the object handled by 'glyph' into a bitmap.
		
		The  target  bitmap  is   described  by  the  'bitmap'  pointer.
		Clipping will  be done  if necessary.  You  can also  specify an
		offset to be applied  before the scan-line conversion; 'xOffset'
		and 'yOffset'  must be expressed  in fractional pixels  (where 1
		unit = 1/64th pixel).
		
		NOTE 1:
		
		Choosing non integer pixel  offsets, i.e., values of 'xOffset'
		and  'yOffset' that  are not  multiples of  64, will  ruin the
		hinting  performed  by  the  interpreter, and  result  in  bad
		rendering at small sizes.
		
		NOTE 2:
		
		The glyph's  point coordinates  must be scaled  before calling
		this  function.  Never call  this function  with a  glyph that
		were loaded with no scaling!
		
		..................................................................
		
		TT_Get_Glyph_Pixmap( TT_Glyph        glyph,
		TT_Raster_Map*  pixmap,
		TT_F26Dot6      xOffset,
		TT_F26Dot6      yOffset );
		
		This call converts  the vectorial glyph representation contained
		in  the  object handled  by  'glyph'  into  a pixmap  (i.e.,  an
		8-bit/pixel map).  The result  is an anti-aliased version of the
		glyph (a.k.a. font-smoothing).
		
		The target  pixmap is described  by the 'pixmap'  pointer.  Note
		that  its  width  _must_ be  a  multiple  of  4.  For  a  pixmap
		definition and description, see Section I.
		
		As  with TT_Get_Glyph_Bitmap(),  you can  specify offsets  to be
		applied before  the rendering  ('xOffset' and 'yOffset'  must be
		expressed in fractional pixel coordinates).
		
		NOTE 1:
		
		You  don't   need  to  supply  a  temporary   bitmap  for  the
		anti-aliaser.  The rasterizer uses  its own scheme to optimize
		memory uses.
		
		NOTE 2:
		
		The glyph's  point coordinates  must be scaled  before calling
		this function.  This means that  you should never call it with
		a glyph which has been loaded without scaling!
		
		NOTE 3:
		
		The  pixmap passed  to this  function should  always  be EMPTY
		before the call.  If not, garbage will accumulate!
		
		..................................................................
		
		TT_New_Outline( TT_UShort    numPoints,
		TT_UShort    numContours,
		TT_Outline*  outline );
		
		Creates a new outline  object.  This function creates the arrays
		necessary to hold 'numPoints' points and 'numContours' contours,
		and set 'outline's pointers to them.
		
		The new outline owns the arrays, and they will be destroyed with
		it through TT_Done_Outline().
		
		NOTE 1:
		
		Outlines  created   with  this  function   are  called  "user"
		outlines,   in  contrast   with  the   outlines   returned  by
		TT_Get_Glyph_Outline(),  which   fields  refer  to   the  data
		contained within  a glyph object (i.e., these  outlines do not
		own the arrays they refer to).
		
		NOTE 2:
		
		User  outlines  aren't  tracked  by the  engine,  which  means
		they're not  destroyed by  a TT_Done_FreeType().  You  have to
		explicitly  discard them  through  TT_Done_Outline() to  avoid
		memory leaks.
		
		..................................................................
		
		TT_Done_Outline( TT_Outline*  outline );
		
		Deletes an outline's  data.  Note that you need  not destroy the
		outlines returned by  TT_Get_Glyph_Outline(), only those created
		by TT_New_Outline().
		
		..................................................................
		
		TT_Copy_Outline( TT_Outline*  source,
		TT_Outline*  target );
		
		Copies the content  of the 'source' outline into  the content of
		the 'target'  outline.  The two outlines must  have been created
		with   the  same   dimensions  (num_points   and  num_contours),
		otherwise this function will return an error code.
		
		..................................................................
		
		TT_Transform_Outline( TT_Glyph_Outline*  outline,
		TT_Matrix*         matrix );
		
		Applies a simple transformation matrix on a given outline.  This
		will  multiply each  point coordinate  vector by  a  2x2 matrix,
		which coefficients are written in the 16.16 fixed float format.
		
		Rotation can be performed with this function.
		
		NOTE:
		
		This function takes  an outline, and not a  glyph handle, as a
		parameter.  This  'feature' lets you  apply transformations on
		your own copies of glyphs.
		
		..................................................................
		
		TT_Translate_Outline( TT_Glyph_Outline*  outline,
		TT_Pos             xOffset,
		TT_Pos             yOffset );
		
		Applies a simple translation on a given outline.
		
		NOTE:
		
		This function takes  an outline, and not a  glyph handle, as a
		parameter.  This 'feature' lets  you apply translation to your
		own copies of glyphs.
		
		..................................................................
		
		TT_Get_Outline_Bitmap( TT_Outline*     outline,
		TT_Raster_Map*  bitmap );
		
		Renders an outline  into a bitmap.  The latter  must be setup by
		the  user before  the  call (i.e.,  it  is not  created by  this
		function, instead it must be provided by the user).
		
		..................................................................
		
		TT_Get_Outline_Pixmap( TT_Outline*     outline,
		TT_Raster_Map*  pixmap );
		
		Renders an outline  into a pixmap.  The latter  must be setup by
		the  user before  the  call (i.e.,  it  is not  created by  this
		function, instead it must be provided by the user).
		
		NOTE:
		
		The pixmap passed  to this function must always  be EMPTY before
		the call.  Otherwise, garbage may accumulate!
		
		..................................................................
		
		TT_Get_Outline_BBox( TT_Outline*  outline,
		TT_BBox*     bbox );
		
		Returns an outline's bounding box in the 'bbox' structure.  Note
		that the returned coordinates are not grid fitted!
		
		NOTE:
		
		The current release of  FreeType (1.0) does compute the bounding
		box for  the outline's control  points, and not the  "exact" box
		based on Bezier arcs extrema.   Hence, the bbox returned by this
		function  may be  slightly larger  than necessary  if  the glyph
		doesn't have  control points at its  extrema, or if  it has been
		rotated.
		
		..................................................................
		
		void  TT_Transform_Vector( TT_Pos*     x,
		^^^^                       TT_Pos*     y,
		TT_Matrix*  matrix );
		
		Applies a 2x2 matrix to a vector.
		
		..................................................................
		
		TT_Matrix_Multiply( TT_Matrix*  a,
		TT_Matrix*  b );
		
		Multiplies  one   matrix  with   another  --  it   will  compute
		b := a * b.
		
		..................................................................
		
		TT_Matrix_Invert( TT_Matrix*  matrix );
		
		Inverts a  matrix.  In case  of failure, returns the  error code
		TT_Err_Divide_By_Zero if the matrix cannot be inverted.
		
		..................................................................
		
		int  TT_Get_CharMapCount( TT_Face  face );
		^^^
		
		Gets the  number of character  mappings present in  the TrueType
		file described  by the 'face' handle.  Returns -1 if the  handle
		is invalid.
		
		IMPORTANT NOTE: ********
		
		This function  is deprecated.  Get the number of  character maps
		from  the  `num_CharMaps' field  in  the  structure  returned by
		TT_Get_Face_Property() instead.
		
		..................................................................
		
		TT_Get_CharMap_ID( TT_Face     face,
		TT_UShort   charmapIndex,
		TT_UShort*  platformID,
		TT_UShort*  encodingID );
		
		Returns the  platform ID  and platform-specific encoding  ID for
		the charmap  numbered 'charmapIndex' in the  'face' object.  The
		total  number of  character mapping  tables is  returned  by the
		TT_Get_CharMap_Count() function described above.
		
		..................................................................
		
		TT_Get_CharMap( TT_Face      face,
		TT_UShort    charmapIndex,
		TT_CharMap*  charMap );
		
		Returns a handle for  the character map number 'charmapIndex' of
		'face'.   The handle  is placed  in '*charMap'  and can  be used
		later for fast lookup with the TT_Char_Index() API.
		
		Charmap  objects  are automatically  destroyed  when their  face
		object is destroyed.
		
		..................................................................
		
		TT_UShort  TT_Char_Index( TT_CharMap  charMap,
		^^^^^^^^^                 TT_UShort   charCode );
		
		Applies a  charMap to  translate a charCode  into a  glyph index
		that can  be used to  load and address  a glyph in  the TrueType
		file.  In case of error, the undefined glyph (0) is returned.
		
		The charmap handle can be obtained with TT_Get_CharMap().
		
		..................................................................
		
		int  TT_Get_Name_Count( TT_Face  face );
		
		Gets the  number of name strings  found in a  face's name table.
		This function will return -1 if the face handle is invalid.
		
		IMPORTANT NOTE: ********
		
		This function  is deprecated.   Get the  number of  name strings
		from  the  `num_Names'  field  in   the  structure  returned  by
		TT_Get_Face_Property() instead.
		
		..................................................................
		
		TT_Get_Name_ID( TT_Face     face,
		TT_UShort   nameIndex,
		TT_UShort*  platformID,
		TT_UShort*  encodingID,
		TT_UShort*  languageID,
		TT_UShort*  nameID );
		
		Returns the  ID of  a given name  string, indexed by  the number
		'nameIndex' in  a given face.  The  name index ranges  from 0 to
		the value returned by TT_Get_Name_Count() minus one.
		
		Each string has a platformID, encodingID, languageID and nameID,
		as defined by the TrueType specification.
		
		The platformID is typically in  the 0..3 range.  Some font files
		have  unusual name  table entries;  these can  be  detected from
		their platformID which is larger than 3.
		
		..................................................................
		
		TT_Get_Name_String( TT_Face      face,
		TT_UShort    nameIndex,
		TT_String**  stringPtr,
		TT_UShort*   length );
		
		Returns  a  name string's  address  and  length.   Note that  an
		invalid name table entry always returns NULL for 'stringPtr' and
		a zero length.
		
		NOTE:
		
		The  string belongs  to  the  face object  and  should not  be
		written to or freed by the client application.
		
		..................................................................
		
		TT_Init_Kerning_Extension( TT_Engine  engine );
		
		Initializes the kerning extension for a given engine.  This must
		be called  just after the  engine creation, and before  any face
		object allocation.  Example:
		
		TT_Init_FreeType( &engine );
		TT_Init_Kerning_Extension( engine );
		
		..................................................................
		
		TT_Get_Kerning_Directory( TT_Face      face,
		TT_Kerning*  directory );
		
		Queries the  kerning directory  found in a  face object.   If no
		kerning  table  is  found   in  the  TrueType  file,  the  error
		TT_Err_Table_Is_Missing will be returned.
		
		You  can  access  the  subtables  through the  pointers  of  the
		directory.  However,  by default,  the directory is  only loaded
		when a face object is created.  You must load the subtables that
		interest you with a call to TT_Load_Kerning_Table().
		
		The  layout of  all kerning  structures is  defined in  the file
		"lib/extend/apikern.h".  Both  formats (0 and 2)  are exposed by
		this API.
		
		NOTE:
		
		This function must be  called after the kerning extension were
		initialized.
		
		..................................................................
		
		TT_Get_Font_Data( TT_face   face,
		TT_Long   tag,
		TT_Long   offset,
		void*     buffer,
		TT_Long*  length );
		
		Gets font  or table data.   Similar to the GetFontData()  API of
		the  Windows world.   You  can use  the  macro MAKE_TT_TAG()  to
		generate TrueType table tags from character descriptions, like
		
		MAKE_TT_TAG( 'e','b','l','c' )
		
		..................................................................
		
		TT_Load_Kerning_Table( TT_Face    face,
		TT_UShort  kernIndex );
		
		Loads the kerning subtable number 'kern_index' into memory.  The
		subtable can  be accessed through  the pointers provided  by the
		kerning     directory,    obtained     from     a    call     to
		TT_Get_Kerning_Directory().
		
		Note that the interpretation of  the kerning data is left to the
		client  application.  Read the  TrueType specification  for more
		information on kerning encoding.
		
		NOTE:
		
		This function must be  called after the kerning extension were
		initialized.
		
		..................................................................
		
		TT_Init_Post_Extension( TT_Engine  engine );
		
		Initializes the PostScript name extension to load the PostScript
		glyph names given in the 'post' table.  This must be called just
		after  creation  of  the  engine,  and before  any  face  object
		allocation.  See description of TT_Get_PS_Name() for an example.
		
		..................................................................
		
		TT_Load_PS_Names( TT_Face   face,
		TT_Post*  post );
		
		Loads the PostScript glyph names into memory.  This must be done
		before  TT_Get_PS_Name() is  called.  In  case of  error, either
		TT_Err_Invalid_Post_Table or TT_Err_Invalid_Post_Table_Format is
		returned.  See description of TT_Get_PS_Name() for an example.
		
		..................................................................
		
		TT_Get_PS_Name( TT_Face      face,
		TT_UShort    index,
		TT_String**  PSname );
		
		Get  the PostScript  glyph  name  for a  given  glyph index.   A
		pointer to the name is returned in 'PSname'.  Example:
		
		...
		TT_Post  post;
		char*    PSname;
		
		...
		TT_Init_Post_Extension( engine );
		TT_Load_PS_Names( face, &post );
		
		...
		TT_Get_PS_Name( face, index, &PSname );
		
		
		NOTE:
		
		You must not alter the PostScript glyph name string returned
		by PSname.
		
		
		--------------------------------------------------------------------
		--------------------------------------------------------------------
		
		
		III. Error Messages:
		
		Most functions return an error  code, typed to TT_Error.  A return
		value of zero indicates no error.  The error values are defined in
		the file 'freetype.h'.
		
		
		Error   Unprefixed               Error
		Code    Macro Name               Description
		------------------------------------------------------------------
		
		0x0000  Ok                       Successful function call.
		Always 0!
		
		----------------- high-level API error codes ---------------------
		
		The following  error codes are  returned by the high-level  API to
		indicate an invalid client request.
		
		0x0001  Invalid_Face_Handle      An invalid face object handle was
		passed to an API function.
		
		0x0002  Invalid_Instance_Handle  An invalid instance object handle
		was passed to an API function.
		
		0x0003  Invalid_Glyph_Handle     An invalid glyph container handle
		was passed to an API function.
		
		0x0004  Invalid_CharMap_Handle   An invalid charmap handle was
		passed to an API function.
		
		0x0005  Invalid_Result_Address   An output parameter (a result)
		was given a NULL address in an
		API call.
		
		0x0006  Invalid_Glyph_Index      An invalid glyph index was passed
		to one API function.
		
		0x0007  Invalid_Argument         An invalid argument was passed to
		one API function. Usually, this
		means a simple out-of-bounds
		error.
		
		0x0008  Could_Not_Open_File      The pathname passed doesn't point
		to an existing or accessible
		file.
		
		0x0009  File_Is_Not_Collection   Returned by TT_Open_Collection
		when trying to open a file which
		isn't a collection.
		
		0x000A  Table_Missing            A mandatory TrueType table is
		missing from the font file.
		Denotes a broken font file.
		
		0x000B  Invalid_Horiz_Metrics    The font's HMTX table is broken.
		Denotes a broken font.
		
		0x000C  Invalid_CharMap_Format   A font's charmap entry has an
		invalid format.  Some other
		entries may be valid though.
		
		0x000D  Invalid_PPem             Invalid PPem values specified,
		i.e. you're accessing a scaled
		glyph without having called
		TT_Set_Instance_CharSize() or
		TT_Set_Instance_PixelSizes().
		
		0x0010  Invalid_File_Format      The file isn't a TrueType font or
		collection.
		
		0x0020  Invalid_Engine           An invalid engine handle was
		passed to one of the API
		functions.
		
		0x0021  Too_Many_Extensions      The client application is trying
		to initialize too many
		extensions.  The default max
		extensions number is 8.
		
		0x0022  Extensions_Unsupported   This build of the engine doesn't
		support extensions
		
		0x0023  Invalid_Extension_Id     This error indicates that the
		client application is trying to
		use an extension that has not
		been initialized yet.
		
		0x0080  Max_Profile_Missing      The max profile table is missing
		from the font file.
		=> broken font file
		
		0x0081  Header_Table_Missing     The font header table is missing
		from the font file.
		=> broken font file
		
		0x0082  Horiz_Header_Missing     The horizontal header is missing.
		
		0x0083  Locations_Missing        The locations table is missing.
		
		0x0084  Name_Table_Missing       The name table is missing.
		
		0x0085  CMap_Table_Missing       The character encoding tables are
		missing.
		
		0x0086  Hmtx_Table_Missing       The Hmtx table is missing.
		
		0x0087  OS2_Table_Missing        The OS/2 table is missing.
		
		0x0088  Post_Table_Missing       The PostScript table is missing.
		
		----------------- memory component error codes -------------------
		
		0x0100  Out_Of_Memory            An operation couldn't be
		performed due to memory
		exhaustion.
		
		----------------- file component error codes ---------------------
		
		0x0200  Invalid_File_Offset      Trying to seek to an invalid
		portion of the font file.
		Denotes a broken file.
		
		0x0201  Invalid_File_Read        Trying to read an invalid portion
		of the font file.  Denotes a
		broken file.
		
		0x0202  Invalid_Frame_Access     Trying to frame an invalid
		portion of the font file.
		Denotes a broken file.
		
		----------------- glyph loader error codes -----------------------
		
		These errors  are produced  by the glyph  loader.  They  denote an
		invalid glyph record within the font file.
		
		0x0300  Too_Many_Points          The glyph has too many points to
		be valid for its font file.
		
		0x0301  Too_Many_Contours        The glyph has too many contours
		to be valid for its font file.
		
		0x0302  Invalid_Composite_Glyph  A composite glyph's description
		is broken.
		
		0x0303  Too_Many_Ins             The glyph has too many
		instructions to be valid for its
		font file.
		
		----------------- byte-code interpreter error codes --------------
		
		These  error   codes  are  produced  by   the  TrueType  byte-code
		interpreter.  They usually indicate a broken font file or a broken
		glyph within a font.
		
		0x0400  Invalid_Opcode           Found an invalid opcode in a
		TrueType byte-code stream.
		
		0x0401  Too_Few_Arguments        An opcode was invoked with too
		few arguments on the stack.
		
		0x0402  Stack_Overflow           The interpreter's stack has been
		filled up and operations can't
		continue.
		
		0x0403  Code_Overflow            The byte-code stream runs out of
		its valid bounds.
		
		0x0404  Bad_Argument             A function received an invalid
		argument.
		
		0x0405  Divide_By_Zero           A division by 0 operation was
		queried by the interpreter
		program.
		
		0x0406  Storage_Overflow         The program tried to access data
		outside of its storage area.
		
		0x0407  Cvt_Overflow             The program tried to access data
		outside of its control value
		table.
		
		0x0408  Invalid_Reference        The program tried to reference an
		invalid point, zone or contour.
		
		0x0409  Invalid_Distance         The program tried to use an
		invalid distance.
		
		0x040A  Interpolate_Twilight     The program tried to interpolate
		twilight points.
		
		0x040B  Debug_Opcode             The now invalid 'debug' opcode
		was found in the byte-code
		stream.
		
		0x040C  ENDF_In_Exec_Stream      A misplaced ENDF was encountered
		in the byte-code stream.
		
		0x040D  Out_Of_CodeRanges        The program tried to allocate too
		much code ranges (this is really
		an engine internal error that
		should never happen).
		
		0x040E  Nested_DEFS              Nested function definitions
		encountered.
		
		0x040F  Invalid_CodeRange        The program tried to access an
		invalid code range.
		
		0x0410  Invalid_Displacement     The program tried to use an
		invalid displacement.
		
		0x0411  Execution_Too_Long       In order to get rid of "poison"
		fonts, the interpreter produces
		this error when more than a
		million opcodes have been
		interpreted in a single glyph
		program.  This detects infinite
		loops softly.
		
		----------------- internal failure error codes -------------------
		
		These error codes are produced  if an incoherent library state has
		been detected.   All of these reflect  a severe bug  in the engine
		(or a severe  memory corruption due to massive  overwrites by your
		application into the library's data)!
		
		If you  do encounter a  font that makes  one of the  test programs
		produce such an error, please report it!
		
		0x0500  Nested_Frame_Access
		0x0501  Invalid_Cache_List
		0x0502  Could_Not_Find_Context
		0x0503  Unlisted_Object
		
		----------------- scan-line converter error codes ----------------
		
		These  error codes are  produced  by  the raster  component.  They
		indicate that   an outline structure  was incoherently  set up, or
		that you're trying to render a horribly complex glyph.
		
		They should be _extremely_ rare, however.
		
		0x0600  Raster_Pool_Overflow     Render pool overflow. This should
		never happen in this release.
		
		0x0601  Raster_Negative_Height   A negative height was produced.
		
		0x0602  Raster_Invalid_Value     The outline data wasn't set
		properly. Check that:
		points >= endContours[contours]
		
		0x0603  Raster_Not_Initialized   You did not call
		TT_Init_FreeType()!
		
		----------------- engine extensions error codes ------------------
		
		The engine's extensions also provide their own error codes, within
		their own group:
		
		0x0A00  Invalid_Kerning_Table_Format
		A kerning subtable format was
		found invalid in this font.
		
		0x0A01  Invalid_Kerning_Table    A kerning table contains illegal
		glyph indices.
		
		0x0B00  Invalid_Post_Table_Format
		The post table format specified
		in the font is invalid.
		
		0x0B01  Invalid_Post_Table       The post table contains illegal
		entries.
		
		--- end of apiref.txt ---
		
	#tag EndNote

	#tag Note, Name = ttfparser.php
		
		<?php
		/*******************************************************************************
		* Class to parse and subset TrueType fonts                                     *
		*                                                                              *
		* Version: 1.1                                                                 *
		* Date:    2015-11-29                                                          *
		* Author:  Olivier PLATHEY                                                     *
		*******************************************************************************/
		
		class TTFParser
		{
		protected $f;
		protected $tables;
		protected $numberOfHMetrics;
		protected $numGlyphs;
		protected $glyphNames;
		protected $indexToLocFormat;
		protected $subsettedChars;
		protected $subsettedGlyphs;
		public $chars;
		public $glyphs;
		public $unitsPerEm;
		public $xMin, $yMin, $xMax, $yMax;
		public $postScriptName;
		public $embeddable;
		public $bold;
		public $typoAscender;
		public $typoDescender;
		public $capHeight;
		public $italicAngle;
		public $underlinePosition;
		public $underlineThickness;
		public $isFixedPitch;
		
		function __construct($file)
		{
		$this->f = fopen($file, 'rb');
		if(!$this->f)
		$this->Error('Can\'t open file: '.$file);
		}
		
		function __destruct()
		{
		if(is_resource($this->f))
		fclose($this->f);
		}
		
		function Parse()
		{
		$this->ParseOffsetTable();
		$this->ParseHead();
		$this->ParseHhea();
		$this->ParseMaxp();
		$this->ParseHmtx();
		$this->ParseLoca();
		$this->ParseGlyf();
		$this->ParseCmap();
		$this->ParseName();
		$this->ParseOS2();
		$this->ParsePost();
		}
		
		function ParseOffsetTable()
		{
		$version = $this->Read(4);
		if($version=='OTTO')
		$this->Error('OpenType fonts based on PostScript outlines are not supported');
		if($version!="\x00\x01\x00\x00")
		$this->Error('Unrecognized file format');
		$numTables = $this->ReadUShort();
		$this->Skip(3*2); // searchRange, entrySelector, rangeShift
		$this->tables = array();
		for($i=0;$i<$numTables;$i++)
		{
		$tag = $this->Read(4);
		$checkSum = $this->Read(4);
		$offset = $this->ReadULong();
		$length = $this->ReadULong(4);
		$this->tables[$tag] = array('offset'=>$offset, 'length'=>$length, 'checkSum'=>$checkSum);
		}
		}    
		
		function ParseHead()
		{
		$this->Seek('head');
		$this->Skip(3*4); // version, fontRevision, checkSumAdjustment
		$magicNumber = $this->ReadULong();
		if($magicNumber!=0x5F0F3CF5)
		$this->Error('Incorrect magic number');
		$this->Skip(2); // flags
		$this->unitsPerEm = $this->ReadUShort();
		$this->Skip(2*8); // created, modified
		$this->xMin = $this->ReadShort();
		$this->yMin = $this->ReadShort();
		$this->xMax = $this->ReadShort();
		$this->yMax = $this->ReadShort();
		$this->Skip(3*2); // macStyle, lowestRecPPEM, fontDirectionHint
		$this->indexToLocFormat = $this->ReadShort();
		}
		
		function ParseHhea()
		{
		$this->Seek('hhea');
		$this->Skip(4+15*2);
		$this->numberOfHMetrics = $this->ReadUShort();
		}
		
		function ParseMaxp()
		{
		$this->Seek('maxp');
		$this->Skip(4);
		$this->numGlyphs = $this->ReadUShort();
		}
		
		function ParseHmtx()
		{
		$this->Seek('hmtx');
		$this->glyphs = array();
		for($i=0;$i<$this->numberOfHMetrics;$i++)
		{
		$advanceWidth = $this->ReadUShort();
		$lsb = $this->ReadShort();
		$this->glyphs[$i] = array('w'=>$advanceWidth, 'lsb'=>$lsb);
		}
		for($i=$this->numberOfHMetrics;$i<$this->numGlyphs;$i++)
		{
		$lsb = $this->ReadShort();
		$this->glyphs[$i] = array('w'=>$advanceWidth, 'lsb'=>$lsb);
		}
		}
		
		function ParseLoca()
		{
		$this->Seek('loca');
		$offsets = array();
		if($this->indexToLocFormat==0)
		{
		// Short format
		for($i=0;$i<=$this->numGlyphs;$i++)
		$offsets[] = 2*$this->ReadUShort();
		}
		else
		{
		// Long format
		for($i=0;$i<=$this->numGlyphs;$i++)
		$offsets[] = $this->ReadULong();
		}
		for($i=0;$i<$this->numGlyphs;$i++)
		{
		$this->glyphs[$i]['offset'] = $offsets[$i];
		$this->glyphs[$i]['length'] = $offsets[$i+1] - $offsets[$i];
		}
		}
		
		function ParseGlyf()
		{
		$tableOffset = $this->tables['glyf']['offset'];
		foreach($this->glyphs as &$glyph)
		{
		if($glyph['length']>0)
		{
		fseek($this->f, $tableOffset+$glyph['offset'], SEEK_SET);
		if($this->ReadShort()<0)
		{
		// Composite glyph
		$this->Skip(4*2); // xMin, yMin, xMax, yMax
		$offset = 5*2;
		$a = array();
		do
		{
		$flags = $this->ReadUShort();
		$index = $this->ReadUShort();
		$a[$offset+2] = $index;
		if($flags & 1) // ARG_1_AND_2_ARE_WORDS
		$skip = 2*2;
		else
		$skip = 2;
		if($flags & 8) // WE_HAVE_A_SCALE
		$skip += 2;
		elseif($flags & 64) // WE_HAVE_AN_X_AND_Y_SCALE
		$skip += 2*2;
		elseif($flags & 128) // WE_HAVE_A_TWO_BY_TWO
		$skip += 4*2;
		$this->Skip($skip);
		$offset += 2*2 + $skip;
		}
		while($flags & 32); // MORE_COMPONENTS
		$glyph['components'] = $a;
		}
		}
		}
		}
		
		function ParseCmap()
		{
		$this->Seek('cmap');
		$this->Skip(2); // version
		$numTables = $this->ReadUShort();
		$offset31 = 0;
		for($i=0;$i<$numTables;$i++)
		{
		$platformID = $this->ReadUShort();
		$encodingID = $this->ReadUShort();
		$offset = $this->ReadULong();
		if($platformID==3 && $encodingID==1)
		$offset31 = $offset;
		}
		if($offset31==0)
		$this->Error('No Unicode encoding found');
		
		$startCount = array();
		$endCount = array();
		$idDelta = array();
		$idRangeOffset = array();
		$this->chars = array();
		fseek($this->f, $this->tables['cmap']['offset']+$offset31, SEEK_SET);
		$format = $this->ReadUShort();
		if($format!=4)
		$this->Error('Unexpected subtable format: '.$format);
		$this->Skip(2*2); // length, language
		$segCount = $this->ReadUShort()/2;
		$this->Skip(3*2); // searchRange, entrySelector, rangeShift
		for($i=0;$i<$segCount;$i++)
		$endCount[$i] = $this->ReadUShort();
		$this->Skip(2); // reservedPad
		for($i=0;$i<$segCount;$i++)
		$startCount[$i] = $this->ReadUShort();
		for($i=0;$i<$segCount;$i++)
		$idDelta[$i] = $this->ReadShort();
		$offset = ftell($this->f);
		for($i=0;$i<$segCount;$i++)
		$idRangeOffset[$i] = $this->ReadUShort();
		
		for($i=0;$i<$segCount;$i++)
		{
		$c1 = $startCount[$i];
		$c2 = $endCount[$i];
		$d = $idDelta[$i];
		$ro = $idRangeOffset[$i];
		if($ro>0)
		fseek($this->f, $offset+2*$i+$ro, SEEK_SET);
		for($c=$c1;$c<=$c2;$c++)
		{
		if($c==0xFFFF)
		break;
		if($ro>0)
		{
		$gid = $this->ReadUShort();
		if($gid>0)
		$gid += $d;
		}
		else
		$gid = $c+$d;
		if($gid>=65536)
		$gid -= 65536;
		if($gid>0)
		$this->chars[$c] = $gid;
		}
		}
		}
		
		function ParseName()
		{
		$this->Seek('name');
		$tableOffset = $this->tables['name']['offset'];
		$this->postScriptName = '';
		$this->Skip(2); // format
		$count = $this->ReadUShort();
		$stringOffset = $this->ReadUShort();
		for($i=0;$i<$count;$i++)
		{
		$this->Skip(3*2); // platformID, encodingID, languageID
		$nameID = $this->ReadUShort();
		$length = $this->ReadUShort();
		$offset = $this->ReadUShort();
		if($nameID==6)
		{
		// PostScript name
		fseek($this->f, $tableOffset+$stringOffset+$offset, SEEK_SET);
		$s = $this->Read($length);
		$s = str_replace(chr(0), '', $s);
		$s = preg_replace('|[ \[\](){}<>/%]|', '', $s);
		$this->postScriptName = $s;
		break;
		}
		}
		if($this->postScriptName=='')
		$this->Error('PostScript name not found');
		}
		
		function ParseOS2()
		{
		$this->Seek('OS/2');
		$version = $this->ReadUShort();
		$this->Skip(3*2); // xAvgCharWidth, usWeightClass, usWidthClass
		$fsType = $this->ReadUShort();
		$this->embeddable = ($fsType!=2) && ($fsType & 0x200)==0;
		$this->Skip(11*2+10+4*4+4);
		$fsSelection = $this->ReadUShort();
		$this->bold = ($fsSelection & 32)!=0;
		$this->Skip(2*2); // usFirstCharIndex, usLastCharIndex
		$this->typoAscender = $this->ReadShort();
		$this->typoDescender = $this->ReadShort();
		if($version>=2)
		{
		$this->Skip(3*2+2*4+2);
		$this->capHeight = $this->ReadShort();
		}
		else
		$this->capHeight = 0;
		}
		
		function ParsePost()
		{
		$this->Seek('post');
		$version = $this->ReadULong();
		$this->italicAngle = $this->ReadShort();
		$this->Skip(2); // Skip decimal part
		$this->underlinePosition = $this->ReadShort();
		$this->underlineThickness = $this->ReadShort();
		$this->isFixedPitch = ($this->ReadULong()!=0);
		if($version==0x20000)
		{
		// Extract glyph names
		$this->Skip(4*4); // min/max usage
		$this->Skip(2); // numberOfGlyphs
		$glyphNameIndex = array();
		$names = array();
		$numNames = 0;
		for($i=0;$i<$this->numGlyphs;$i++)
		{
		$index = $this->ReadUShort();
		$glyphNameIndex[] = $index;
		if($index>=258 && $index-257>$numNames)
		$numNames = $index-257;
		}
		for($i=0;$i<$numNames;$i++)
		{
		$len = ord($this->Read(1));
		$names[] = $this->Read($len);
		}
		foreach($glyphNameIndex as $i=>$index)
		{
		if($index>=258)
		$this->glyphs[$i]['name'] = $names[$index-258];
		else
		$this->glyphs[$i]['name'] = $index;
		}
		$this->glyphNames = true;
		}
		else
		$this->glyphNames = false;
		}
		
		function Subset($chars)
		{
		/*        $chars = array_keys($this->chars);
		$this->subsettedChars = $chars;
		$this->subsettedGlyphs = array();
		for($i=0;$i<$this->numGlyphs;$i++)
		{
		$this->subsettedGlyphs[] = $i;
		$this->glyphs[$i]['ssid'] = $i;
		}*/
		
		$this->AddGlyph(0);
		$this->subsettedChars = array();
		foreach($chars as $char)
		{
		if(isset($this->chars[$char]))
		{
		$this->subsettedChars[] = $char;
		$this->AddGlyph($this->chars[$char]);
		}
		}
		}
		
		function AddGlyph($id)
		{
		if(!isset($this->glyphs[$id]['ssid']))
		{
		$this->glyphs[$id]['ssid'] = count($this->subsettedGlyphs);
		$this->subsettedGlyphs[] = $id;
		if(isset($this->glyphs[$id]['components']))
		{
		foreach($this->glyphs[$id]['components'] as $cid)
		$this->AddGlyph($cid);
		}
		}
		}
		
		function Build()
		{
		$this->BuildCmap();
		$this->BuildHhea();
		$this->BuildHmtx();
		$this->BuildLoca();
		$this->BuildGlyf();
		$this->BuildMaxp();
		$this->BuildPost();
		return $this->BuildFont();
		}
		
		function BuildCmap()
		{
		if(!isset($this->subsettedChars))
		return;
		
		// Divide charset in contiguous segments
		$chars = $this->subsettedChars;
		sort($chars);
		$segments = array();
		$segment = array($chars[0], $chars[0]);
		for($i=1;$i<count($chars);$i++)
		{
		if($chars[$i]>$segment[1]+1)
		{
		$segments[] = $segment;
		$segment = array($chars[$i], $chars[$i]);
		}
		else
		$segment[1]++;
		}
		$segments[] = $segment;
		$segments[] = array(0xFFFF, 0xFFFF);
		$segCount = count($segments);
		
		// Build a Format 4 subtable
		$startCount = array();
		$endCount = array();
		$idDelta = array();
		$idRangeOffset = array();
		$glyphIdArray = '';
		for($i=0;$i<$segCount;$i++)
		{
		list($start, $end) = $segments[$i];
		$startCount[] = $start;
		$endCount[] = $end;
		if($start!=$end)
		{
		// Segment with multiple chars
		$idDelta[] = 0;
		$idRangeOffset[] = strlen($glyphIdArray) + ($segCount-$i)*2;
		for($c=$start;$c<=$end;$c++)
		{
		$ssid = $this->glyphs[$this->chars[$c]]['ssid'];
		$glyphIdArray .= pack('n', $ssid);
		}
		}
		else
		{
		// Segment with a single char
		if($start<0xFFFF)
		$ssid = $this->glyphs[$this->chars[$start]]['ssid'];
		else
		$ssid = 0;
		$idDelta[] = $ssid - $start;
		$idRangeOffset[] = 0;
		}
		}
		$entrySelector = 0;
		$n = $segCount;
		while($n!=1)
		{
		$n = $n>>1;
		$entrySelector++;
		}
		$searchRange = (1<<$entrySelector)*2;
		$rangeShift = 2*$segCount - $searchRange;
		$cmap = pack('nnnn', 2*$segCount, $searchRange, $entrySelector, $rangeShift);
		foreach($endCount as $val)
		$cmap .= pack('n', $val);
		$cmap .= pack('n', 0); // reservedPad
		foreach($startCount as $val)
		$cmap .= pack('n', $val);
		foreach($idDelta as $val)
		$cmap .= pack('n', $val);
		foreach($idRangeOffset as $val)
		$cmap .= pack('n', $val);
		$cmap .= $glyphIdArray;
		
		$data = pack('nn', 0, 1); // version, numTables
		$data .= pack('nnN', 3, 1, 12); // platformID, encodingID, offset
		$data .= pack('nnn', 4, 6+strlen($cmap), 0); // format, length, language
		$data .= $cmap;
		$this->SetTable('cmap', $data);
		}
		
		function BuildHhea()
		{
		$this->LoadTable('hhea');
		$numberOfHMetrics = count($this->subsettedGlyphs);
		$data = substr_replace($this->tables['hhea']['data'], pack('n',$numberOfHMetrics), 4+15*2, 2);
		$this->SetTable('hhea', $data);
		}
		
		function BuildHmtx()
		{
		$data = '';
		foreach($this->subsettedGlyphs as $id)
		{
		$glyph = $this->glyphs[$id];
		$data .= pack('nn', $glyph['w'], $glyph['lsb']);
		}
		$this->SetTable('hmtx', $data);
		}
		
		function BuildLoca()
		{
		$data = '';
		$offset = 0;
		foreach($this->subsettedGlyphs as $id)
		{
		if($this->indexToLocFormat==0)
		$data .= pack('n', $offset/2);
		else
		$data .= pack('N', $offset);
		$offset += $this->glyphs[$id]['length'];
		}
		if($this->indexToLocFormat==0)
		$data .= pack('n', $offset/2);
		else
		$data .= pack('N', $offset);
		$this->SetTable('loca', $data);
		}
		
		function BuildGlyf()
		{
		$tableOffset = $this->tables['glyf']['offset'];
		$data = '';
		foreach($this->subsettedGlyphs as $id)
		{
		$glyph = $this->glyphs[$id];
		fseek($this->f, $tableOffset+$glyph['offset'], SEEK_SET);
		$glyph_data = $this->Read($glyph['length']);
		if(isset($glyph['components']))
		{
		// Composite glyph
		foreach($glyph['components'] as $offset=>$cid)
		{
		$ssid = $this->glyphs[$cid]['ssid'];
		$glyph_data = substr_replace($glyph_data, pack('n',$ssid), $offset, 2);
		}
		}
		$data .= $glyph_data;
		}
		$this->SetTable('glyf', $data);
		}
		
		function BuildMaxp()
		{
		$this->LoadTable('maxp');
		$numGlyphs = count($this->subsettedGlyphs);
		$data = substr_replace($this->tables['maxp']['data'], pack('n',$numGlyphs), 4, 2);
		$this->SetTable('maxp', $data);
		}
		
		function BuildPost()
		{
		$this->Seek('post');
		if($this->glyphNames)
		{
		// Version 2.0
		$numberOfGlyphs = count($this->subsettedGlyphs);
		$numNames = 0;
		$names = '';
		$data = $this->Read(2*4+2*2+5*4);
		$data .= pack('n', $numberOfGlyphs);
		foreach($this->subsettedGlyphs as $id)
		{
		$name = $this->glyphs[$id]['name'];
		if(is_string($name))
		{
		$data .= pack('n', 258+$numNames);
		$names .= chr(strlen($name)).$name;
		$numNames++;
		}
		else
		$data .= pack('n', $name);
		}
		$data .= $names;
		}
		else
		{
		// Version 3.0
		$this->Skip(4);
		$data = "\x00\x03\x00\x00";
		$data .= $this->Read(4+2*2+5*4);
		}
		$this->SetTable('post', $data);
		}
		
		function BuildFont()
		{
		$tags = array();
		foreach(array('cmap', 'cvt ', 'fpgm', 'glyf', 'head', 'hhea', 'hmtx', 'loca', 'maxp', 'name', 'post', 'prep') as $tag)
		{
		if(isset($this->tables[$tag]))
		$tags[] = $tag;
		}
		$numTables = count($tags);
		$offset = 12 + 16*$numTables;
		foreach($tags as $tag)
		{
		if(!isset($this->tables[$tag]['data']))
		$this->LoadTable($tag);
		$this->tables[$tag]['offset'] = $offset;
		$offset += strlen($this->tables[$tag]['data']);
		}
		//        $this->tables['head']['data'] = substr_replace($this->tables['head']['data'], "\x00\x00\x00\x00", 8, 4);
		
		// Build offset table
		$entrySelector = 0;
		$n = $numTables;
		while($n!=1)
		{
		$n = $n>>1;
		$entrySelector++;
		}
		$searchRange = 16*(1<<$entrySelector);
		$rangeShift = 16*$numTables - $searchRange;
		$offsetTable = pack('nnnnnn', 1, 0, $numTables, $searchRange, $entrySelector, $rangeShift);
		foreach($tags as $tag)
		{
		$table = $this->tables[$tag];
		$offsetTable .= $tag.$table['checkSum'].pack('NN', $table['offset'], $table['length']);
		}
		
		// Compute checkSumAdjustment (0xB1B0AFBA - font checkSum)
		$s = $this->CheckSum($offsetTable);
		foreach($tags as $tag)
		$s .= $this->tables[$tag]['checkSum'];
		$a = unpack('n2', $this->CheckSum($s));
		$high = 0xB1B0 + ($a[1]^0xFFFF);
		$low = 0xAFBA + ($a[2]^0xFFFF) + 1;
		$checkSumAdjustment = pack('nn', $high+($low>>16), $low);
		$this->tables['head']['data'] = substr_replace($this->tables['head']['data'], $checkSumAdjustment, 8, 4);
		
		$font = $offsetTable;
		foreach($tags as $tag)
		$font .= $this->tables[$tag]['data'];
		
		return $font;
		}
		
		function LoadTable($tag)
		{
		$this->Seek($tag);
		$length = $this->tables[$tag]['length'];
		$n = $length % 4;
		if($n>0)
		$length += 4 - $n;
		$this->tables[$tag]['data'] = $this->Read($length);
		}
		
		function SetTable($tag, $data)
		{
		$length = strlen($data);
		$n = $length % 4;
		if($n>0)
		$data = str_pad($data, $length+4-$n, "\x00");
		$this->tables[$tag]['data'] = $data;
		$this->tables[$tag]['length'] = $length;
		$this->tables[$tag]['checkSum'] = $this->CheckSum($data);
		}
		
		function Seek($tag)
		{
		if(!isset($this->tables[$tag]))
		$this->Error('Table not found: '.$tag);
		fseek($this->f, $this->tables[$tag]['offset'], SEEK_SET);
		}
		
		function Skip($n)
		{
		fseek($this->f, $n, SEEK_CUR);
		}
		
		function Read($n)
		{
		return $n>0 ? fread($this->f, $n) : '';
		}
		
		function ReadUShort()
		{
		$a = unpack('nn', fread($this->f,2));
		return $a['n'];
		}
		
		function ReadShort()
		{
		$a = unpack('nn', fread($this->f,2));
		$v = $a['n'];
		if($v>=0x8000)
		$v -= 65536;
		return $v;
		}
		
		function ReadULong()
		{
		$a = unpack('NN', fread($this->f,4));
		return $a['N'];
		}
		
		function CheckSum($s)
		{
		$n = strlen($s);
		$high = 0;
		$low = 0;
		for($i=0;$i<$n;$i+=4)
		{
		$high += (ord($s[$i])<<8) + ord($s[$i+1]);
		$low += (ord($s[$i+2])<<8) + ord($s[$i+3]);
		}
		return pack('nn', $high+($low>>16), $low);
		}
		
		function Error($msg)
		{
		throw new Exception($msg);
		}
		}
		?>
		
	#tag EndNote


	#tag Property, Flags = &h0
		bBold As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		bEmbeddable As Boolean
	#tag EndProperty

	#tag Property, Flags = &h1
		Protected bGlyphNames As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		bIsFixedPitch As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		bItalic As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		bParseError As Boolean = False
	#tag EndProperty

	#tag Property, Flags = &h0
		dicGlyphs As Dictionary
	#tag EndProperty

	#tag Property, Flags = &h0
		dicTables As Dictionary
	#tag EndProperty

	#tag Property, Flags = &h1
		Protected F As FolderItem
	#tag EndProperty

	#tag Property, Flags = &h0
		iAdvanceWidthMax As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		iAvgCharWidth As Int16
	#tag EndProperty

	#tag Property, Flags = &h0
		iCapHeight As Int16
	#tag EndProperty

	#tag Property, Flags = &h0
		iChars() As UInt16
	#tag EndProperty

	#tag Property, Flags = &h0
		iCharsIsSet() As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		iFirstCharIndex As UInt16
	#tag EndProperty

	#tag Property, Flags = &h1
		Protected iIndexToLocFormat As Int16
	#tag EndProperty

	#tag Property, Flags = &h0
		iItalicAngle As Int16
	#tag EndProperty

	#tag Property, Flags = &h0
		iLastCharIndex As UInt16
	#tag EndProperty

	#tag Property, Flags = &h1
		Protected iNumberOfHMetrics As Integer
	#tag EndProperty

	#tag Property, Flags = &h1
		Protected iNumGlyphs As Integer
	#tag EndProperty

	#tag Property, Flags = &h1
		Protected iSubsettedChars() As UInt16
	#tag EndProperty

	#tag Property, Flags = &h1
		Protected iSubsettedGlyphs() As UInt16
	#tag EndProperty

	#tag Property, Flags = &h0
		iTypoAscender As Int16
	#tag EndProperty

	#tag Property, Flags = &h0
		iTypoDescender As Int16
	#tag EndProperty

	#tag Property, Flags = &h0
		iTypoLeading As Int16
	#tag EndProperty

	#tag Property, Flags = &h0
		iUnderlinePosition As Int16
	#tag EndProperty

	#tag Property, Flags = &h0
		iUnderlineThickness As Int16
	#tag EndProperty

	#tag Property, Flags = &h0
		iUnitsPerEm As uInt16
	#tag EndProperty

	#tag Property, Flags = &h0
		ixHeight As Int16
	#tag EndProperty

	#tag Property, Flags = &h0
		iXMax As Int16
	#tag EndProperty

	#tag Property, Flags = &h0
		iXMin As Int16
	#tag EndProperty

	#tag Property, Flags = &h0
		iYMax As Int16
	#tag EndProperty

	#tag Property, Flags = &h0
		iYMin As Int16
	#tag EndProperty

	#tag Property, Flags = &h0
		ReadStream As BinaryStream
	#tag EndProperty

	#tag Property, Flags = &h0
		strFamilyName As String
	#tag EndProperty

	#tag Property, Flags = &h0
		strPostScriptName As String
	#tag EndProperty

	#tag Property, Flags = &h0
		strSubFamilyName As String
	#tag EndProperty


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
			Name="iUnitsPerEm"
			Group="Behavior"
			Type="uInt16"
		#tag EndViewProperty
		#tag ViewProperty
			Name="strPostScriptName"
			Group="Behavior"
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="iXMin"
			Group="Behavior"
			Type="Int16"
		#tag EndViewProperty
		#tag ViewProperty
			Name="iYMin"
			Group="Behavior"
			Type="Int16"
		#tag EndViewProperty
		#tag ViewProperty
			Name="iXMax"
			Group="Behavior"
			Type="Int16"
		#tag EndViewProperty
		#tag ViewProperty
			Name="iYMax"
			Group="Behavior"
			Type="Int16"
		#tag EndViewProperty
		#tag ViewProperty
			Name="bEmbeddable"
			Group="Behavior"
			Type="Boolean"
		#tag EndViewProperty
		#tag ViewProperty
			Name="bBold"
			Group="Behavior"
			Type="Boolean"
		#tag EndViewProperty
		#tag ViewProperty
			Name="iTypoAscender"
			Group="Behavior"
			Type="Int16"
		#tag EndViewProperty
		#tag ViewProperty
			Name="iTypoDescender"
			Group="Behavior"
			Type="Int16"
		#tag EndViewProperty
		#tag ViewProperty
			Name="iCapHeight"
			Group="Behavior"
			Type="Int16"
		#tag EndViewProperty
		#tag ViewProperty
			Name="iItalicAngle"
			Group="Behavior"
			Type="Int16"
		#tag EndViewProperty
		#tag ViewProperty
			Name="iUnderlinePosition"
			Group="Behavior"
			Type="Int16"
		#tag EndViewProperty
		#tag ViewProperty
			Name="iUnderlineThickness"
			Group="Behavior"
			Type="Int16"
		#tag EndViewProperty
		#tag ViewProperty
			Name="bIsFixedPitch"
			Group="Behavior"
			Type="Boolean"
		#tag EndViewProperty
		#tag ViewProperty
			Name="iAvgCharWidth"
			Group="Behavior"
			Type="Int16"
		#tag EndViewProperty
		#tag ViewProperty
			Name="iFirstCharIndex"
			Group="Behavior"
			Type="UInt16"
		#tag EndViewProperty
		#tag ViewProperty
			Name="iLastCharIndex"
			Group="Behavior"
			Type="UInt16"
		#tag EndViewProperty
		#tag ViewProperty
			Name="ixHeight"
			Group="Behavior"
			Type="Int16"
		#tag EndViewProperty
		#tag ViewProperty
			Name="iAdvanceWidthMax"
			Group="Behavior"
			Type="Integer"
		#tag EndViewProperty
		#tag ViewProperty
			Name="iTypoLeading"
			Group="Behavior"
			Type="Int16"
		#tag EndViewProperty
		#tag ViewProperty
			Name="strFamilyName"
			Group="Behavior"
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="bItalic"
			Group="Behavior"
			Type="Boolean"
		#tag EndViewProperty
		#tag ViewProperty
			Name="strSubFamilyName"
			Group="Behavior"
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="bParseError"
			Group="Behavior"
			InitialValue="False"
			Type="Boolean"
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
