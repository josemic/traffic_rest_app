The DTD has been converted into an XSD using Visual Studio.

From the DTD the erlsom data structures have been created and is now used by the 
parser to parse the records.

DTD validation of an xml file against the schema.
xmllint --valid  update.xml -dtdvalid update.dtd
