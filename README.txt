Note tha I am no longer maintaining this code, as I am no longer using RealBasic or Xojo.

RSFPDF is a fork of 'RPDF', a public domain PDF library for Real Studio / Real Basic.

Like RPDF, RSPDF is a port of the php PDF library, fpdf (http://www.fpdf.org).  However it appears that RPDF has been abandoned with the last release made in March 2008.  RSPDF is an effort to pickup where its predecessor left off.  It will be maintained and updated for more recent versions of Real Studio.

The license will remain the same as RPDF and FPDF.  RSPDF (as was RPDF) is FREEWARE like FPDF (the project where this class is based on).
There is no use limitations. It can be used in free or commercial applications, with/out modifications.

My immediate To Do List:
Bring the code in sync with fpdf 1.7 (Released June 2011)

================
Below are the notes from the original author of RPDF.

2007-2008 by diego2k

Contact me: diego2k[at]gmail[dot]com  


TODO:
   - True Type embedding and complete support
   - Improbe Links support

Release 18.05.08
+ PNG Image Support

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
* Dirk Cleenwerck (PNG Support)