curl http://www.w3.org/TR/REC-xml/ > files/REC-xml.html

tidy -asxml -wrap 100000 files/REC-xml.html > files/0x1.xml

cat files/0x1.xml  >> files/0x2.xml
cat files/0x1.xml  >> files/0x2.xml
cat files/0x2.xml  >> files/0x4.xml
cat files/0x2.xml  >> files/0x4.xml
cat files/0x4.xml  >> files/0x8.xml
cat files/0x4.xml  >> files/0x8.xml
cat files/0x8.xml  >> files/0x10.xml
cat files/0x8.xml  >> files/0x10.xml
cat files/0x10.xml >> files/0x20.xml
cat files/0x10.xml >> files/0x20.xml
cat files/0x20.xml >> files/0x40.xml
cat files/0x20.xml >> files/0x40.xml
cat files/0x40.xml >> files/0x80.xml
cat files/0x40.xml >> files/0x80.xml
cat files/0x80.xml >> files/0x100.xml
cat files/0x80.xml >> files/0x100.xml
