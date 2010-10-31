curl http://www.w3.org/TR/REC-xml/ > files/REC-xml.html

tidy --asxml files/REC-xml.html > files/REC-xml.xml

cat files/REC-xml.xml >> files/2x.xml
cat files/REC-xml.xml >> files/2x.xml
cat files/2x.xml >> files/4x.xml
cat files/2x.xml >> files/4x.xml
cat files/4x.xml >> files/8x.xml
cat files/4x.xml >> files/8x.xml
cat files/8x.xml >> files/16x.xml
cat files/8x.xml >> files/16x.xml
cat files/16x.xml >> files/32x.xml
cat files/16x.xml >> files/32x.xml
cat files/32x.xml >> files/64x.xml
cat files/32x.xml >> files/64x.xml

