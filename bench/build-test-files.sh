curl http://www.w3.org/TR/REC-xml/ > files/REC-xml.html

tidy -asxml -wrap 100000 files/REC-xml.html > files/0x01.xml

cat files/0x01.xml >> files/0x02.xml
cat files/0x01.xml >> files/0x02.xml
cat files/0x02.xml >> files/0x04.xml
cat files/0x02.xml >> files/0x04.xml
cat files/0x04.xml >> files/0x08.xml
cat files/0x04.xml >> files/0x08.xml
cat files/0x08.xml >> files/0x10.xml
cat files/0x08.xml >> files/0x10.xml
cat files/0x10.xml >> files/0x20.xml
cat files/0x10.xml >> files/0x20.xml
cat files/0x20.xml >> files/0x40.xml
cat files/0x20.xml >> files/0x40.xml

