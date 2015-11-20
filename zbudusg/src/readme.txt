Zonbudusg prompts for a zonebudget listing file, disu file, 
budget file, title, and a zone input file.  The zone input 
file starts with one line for the number of nodes in the 
model.  Then a zone code is read for each node using the 1-D 
integer array reader.  Arrays in the disu file must be read 
using CONSTANT, INTERNAL, or OPEN/CLOSE because Zonbudusg 
does not read the name file to get the file units for external 
files.


