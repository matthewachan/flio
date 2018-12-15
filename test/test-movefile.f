string filename = 'myfile.txt';
string newname = 'renamedfile.txt';
file f = filename;

move(filename, newname);

remove(newname);
